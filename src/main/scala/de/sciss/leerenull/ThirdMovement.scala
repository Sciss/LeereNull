/*
 *  PDF.scala
 *  (LeereNull)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.leerenull

import de.sciss.kontur.session.BasicTimeline
import de.sciss.strugatzki.aux.{ProcessorCompanion, Processor}
import actors.Actor
import util.control.ControlThrowable
import de.sciss.strugatzki.FeatureSegmentation.Break
import de.sciss.strugatzki.{Strugatzki, FeatureCorrelation, FeatureSegmentation, FeatureExtraction, Span}
import java.io.{FileInputStream, FileOutputStream, File}
import de.sciss.strugatzki.FeatureCorrelation.Match
import collection.immutable.{IndexedSeq => IIdxSeq}

object ThirdMovement extends ProcessorCompanion {
   type PayLoad = Unit

   def folder = new File( LeereNull.baseFolder, "third_move" )

   object Strategy {
      def apply( bal: Double ) : Strategy = {
         require( bal >= 0.0 && bal <= 1.0 )
         new Strategy { val balance = bal }
      }

      case object Imitation extends Strategy { val balance = 0.0 }
      case object Ecology   extends Strategy { val balance = 1.0 }
   }
   sealed trait Strategy {
      def balance: Double
   }

   final case class Settings( timeline: BasicTimeline, tlSpan: Span, layer: File, layerOffset: Long,
                              materialFolder: File, numChannels: Int, startStrategy: Strategy,
                              stopStrategy: Strategy, startDur: (Long, Long), stopDur: (Long, Long),
                              startWeight: Float, stopWeight: Float, maxOverlap: Float,
                              connectionWeight: Float, strategyWeight: Float )

   private case object AbortException extends ControlThrowable

   def apply( settings: Settings )( observer: Observer ) : ThirdMovement =
      new ThirdMovement( settings, observer )
}

class ThirdMovement private( settings: ThirdMovement.Settings, protected val observer: ThirdMovement.Observer )
extends NullGoodies with Processor {
   import ThirdMovement._

   protected val companion = ThirdMovement

   @volatile private var failure          = Option.empty[ Throwable ]
   @volatile private var success          = Option.empty[ AnyRef ]
   @volatile private var progressFactor   = 1.0f

   private val monitor = new AnyRef

   private def handleProcessOption[ A ]( perc: Float, po: Option[ Processor ]) : Option[ A ] = {
      po match {
         case Some( p ) =>
            Some( handleProcess[ A ]( perc, p ))

         case None =>
            if( checkAborted ) throw AbortException
            progress( perc )
            None
      }
   }

   private def handleProcess[ A ]( perc: Float, p: Processor ) : A = {
      failure        = None
      success        = None
      progressFactor = perc
      p.start()
      while( true ) {
         if( checkAborted ) throw AbortException
         failure.foreach( throw _ )
         success match {
            case Some( res ) => return res.asInstanceOf[ A ]
            case None =>
               monitor.synchronized {
                  monitor.wait( 500 )
               }
         }
      }
      sys.error( "Never gets here" )
   }

   private def succeeded( res: AnyRef ) {
      success = Some( res )
      monitor.synchronized( monitor.notifyAll() )
   }

   private def failed( t: Throwable ) {
      failure = Some( t )
      monitor.synchronized( monitor.notifyAll() )
   }

   private def progressed( i: Int ) {
      val p = i.toFloat / 100 * progressFactor
      progress( p )
   }

   protected def body() : Result = {
      try {
         process()
         Success( () )
      }
      catch {
         case AbortException => Aborted
      }
   }

   private def process() {
      val (metaFile, extrOption) = metaFileForLayer( settings.layer )
      handleProcessOption[ Unit ]( 0.1f, extrOption )

      val spanLen             = settings.tlSpan.length
      val numChannels         = settings.numChannels

      val segmCfg             = FeatureSegmentation.SettingsBuilder()
      segmCfg.corrLen         = 88200L // have one second on each side
      segmCfg.databaseFolder  = LeereNull.databaseFolder // hold the normalization data
      segmCfg.metaInput       = metaFile
      val minSpc              = math.min( settings.startDur._1, settings.stopDur._1 ) / 6
      segmCfg.minSpacing      = minSpc // 22050L // 44100L -- no, smaller because we want to use overlap eventually
      segmCfg.numBreaks       = (spanLen / segmCfg.minSpacing).toInt + 1
      segmCfg.span            = Some( Span( settings.layerOffset, settings.layerOffset + spanLen ))
      segmCfg.temporalWeight  = 0.75f  // XXX could be configurable
      val segmProc = FeatureSegmentation( segmCfg ) {
         case FeatureSegmentation.Success( _segm ) => succeeded( _segm )
         case FeatureSegmentation.Progress( i )    => progressed( i )
         case FeatureSegmentation.Aborted          => Act ! Aborted
         case FeatureSegmentation.Failure( e )     => failed( e )
      }

      val segms      = handleProcess[ IndexedSeq[ Break ]]( 0.2f, segmProc ).map( _.pos ).sorted // XXX already sorted?
      val numSegm    = segms.size
      if( numSegm == 0 ) return
      
      var lastPos       = 0L
      var lastSegmLen   = 0L
      var lastIdx       = 0
      val rnd           = new util.Random()
      // tracks the matches per channel
      var lastMatch     = Option.empty[ IIdxSeq[ Match ]]
      val gagaDur       = (settings.startDur._1 + settings.startDur._2 + settings.stopDur._1 + settings.stopDur._2) / 4
      while( lastPos < spanLen ) {
         val maxOvl  = (settings.maxOverlap.toDouble * lastSegmLen + 0.5).toLong
         var idx     = lastIdx
         while( (idx > 0 &&) ((lastPos - segms( idx )) <= maxOvl) ) idx -= 1
         val startIdx = math.min( lastIdx, idx + 1 )
         val startPos = segms( startIdx )

//         val w       = math.max( 0.0, math.min( 1.0, ((minStop + maxStop) / 2 - startPos).toDouble / spanLen ))
         val w       = math.max( 0.0, math.min( 1.0, (gagaDur - startPos).toDouble / spanLen ))

         val minDur  = ((settings.startDur._1 * (1 - w)) + (settings.stopDur._1 * w) + 0.5).toLong
         val maxDur  = ((settings.startDur._2 * (1 - w)) + (settings.stopDur._2 * w) + 0.5).toLong
         val temp    = ((settings.startWeight * (1 - w)) + (settings.stopWeight * w)).toFloat

         val minStop = startPos + minDur
         val maxStop = startPos + maxDur

         idx = startIdx + 1; while( idx < numSegm && (segms( idx ) <= minStop )) idx += 1
         val minIdx  = idx - 1
         idx = minIdx + 1; while( (idx < numSegm) && (segms( idx ) <= maxStop )) idx += 1
         val maxIdx  = idx - 1
         if( minIdx <= maxIdx ) {
            val stopIdx = minIdx + rnd.nextInt( maxIdx - minIdx + 1 )
            val plainSpan  = Span( segms( startIdx ), segms( stopIdx ))
            val layerSpan  = Span( plainSpan.start + settings.layerOffset, plainSpan.stop + settings.layerOffset )

            val corrCfg    = FeatureCorrelation.SettingsBuilder()
            corrCfg.databaseFolder = settings.materialFolder
            val normFile   = new File( settings.materialFolder, Strugatzki.NORMALIZE_NAME )
            if( !normFile.exists() ) {
               val sourceFile = new File( LeereNull.databaseFolder, Strugatzki.NORMALIZE_NAME )
               copyFile( sourceFile, normFile )
            }
            corrCfg.maxBoost     = 20  // +26 dB
            corrCfg.minPunch     = plainSpan.length   // XXX is this actually used when punchOut == None?
            corrCfg.maxPunch     = plainSpan.length   // XXX is this actually used when punchOut == None?
            corrCfg.metaInput    = metaFile
            corrCfg.minSpacing   = 4410L  // 100 ms
            corrCfg.numMatches   = math.min( 4096, numChannels * numChannels * 100 )
            corrCfg.numPerFile   = corrCfg.numMatches
            corrCfg.punchIn      = FeatureCorrelation.Punch( layerSpan, temp )

            val corrProc = FeatureCorrelation( corrCfg ) {
               case FeatureCorrelation.Success( _segm )  => succeeded( _segm )
               case FeatureCorrelation.Progress( i )     => progressed( i )
               case FeatureCorrelation.Aborted           => Act ! Aborted
               case FeatureCorrelation.Failure( e )      => failed( e )
            }

            val perc    = (0.7 * w + 0.2).toFloat
            val corrs   = handleProcess[ IndexedSeq[ Match ]]( perc, corrProc )
            val connW   = if( lastMatch.isDefined ) settings.connectionWeight else 0f
            val w1      = if( connW > 0f ) {
               corrs.map { m =>
                  IIdxSeq.tabulate( numChannels ) { ch =>
                     m.sim // XXX TODO
                  }
               }
            } else {
               corrs.map { m => IIdxSeq.fill( numChannels )( m.sim )}
            }
         }
      }
   }

   private def copyFile( source: File, dest: File ) {
      val sourceCh   = new FileInputStream( source ).getChannel
      val destCh     = new FileOutputStream( dest ).getChannel
      destCh.transferFrom( sourceCh, 0, sourceCh.size() )
   }
   
   private def metaFileForLayer( layer: File ) : (File, Option[ FeatureExtraction ]) = {
      val featureDir = new File( folder, "feature" )
      val metaFile = extrMetaFile( plainName( layer ), featureDir )
      if( metaFile.exists() ) {
         (metaFile, None)
      } else {
         val metaDir = metaFile.getParentFile
         if( !metaDir.exists() ) metaDir.mkdirs()
         val extrCfg = FeatureExtraction.SettingsBuilder()
         extrCfg.audioInput     = layer
         val ff                  = featureFile( plainName( layer ),featureDir )
         extrCfg.featureOutput  = ff
         extrCfg.metaOutput     = Some( metaFile )
//         settings.numCoeffs      = default
//         settings.fftSize        = default
//         settings.fftOverlap     = default
         val proc = FeatureExtraction( extrCfg ) {
            case FeatureExtraction.Success( _ )    => succeeded( ().asInstanceOf[ AnyRef ])
            case FeatureExtraction.Progress( i )   => progressed( i )
            case FeatureExtraction.Aborted         => Act ! Aborted  // indirection
            case FeatureExtraction.Failure( e )    => failed( e )
         }
//         proc.start()
         (metaFile, Some( proc ))
      }
   }

   protected val Act = new Actor {
      def act() {
         ProcT.start()
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
                  aborted()
               case res: Progress =>
                  observer( res )
               case res @ Aborted =>
                  result = res
               case res: Failure =>
                  result = res
               case res: Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }
}