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
import java.io.File
import de.sciss.strugatzki.{FeatureSegmentation, FeatureExtraction, Span}
import de.sciss.strugatzki.aux.{ProcessorCompanion, Processor}
import actors.Actor
import util.control.ControlThrowable
import de.sciss.strugatzki.FeatureSegmentation.Break

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
                              stopStrategy: Strategy )

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
      handleProcessOption[ Unit ]( 0.333f, extrOption )

      val segmCfg             = FeatureSegmentation.SettingsBuilder()
      segmCfg.corrLen         = 88200L // have one second on each side
      segmCfg.databaseFolder  = LeereNull.databaseFolder // hold the normalization data
      segmCfg.metaInput       = metaFile
      segmCfg.minSpacing      = 22050L // 44100L -- no, smaller because we want to use overlap eventually
      segmCfg.numBreaks       = (settings.tlSpan.length / segmCfg.minSpacing).toInt + 1
      segmCfg.span            = Some( Span( settings.layerOffset, settings.layerOffset + settings.tlSpan.length ))
      segmCfg.temporalWeight  = 0.75f  // XXX could be configurable
      val segmProc = FeatureSegmentation( segmCfg ) {
         case FeatureSegmentation.Success( _segm ) => succeeded( _segm )
         case FeatureSegmentation.Progress( i )    => progressed( i )
         case FeatureSegmentation.Aborted          => Act ! Aborted
         case FeatureSegmentation.Failure( e )     => failed( e )
      }

      val segm = handleProcess[ IndexedSeq[ Break ]]( 0.667f, segmProc )

      segm.foreach( println )
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