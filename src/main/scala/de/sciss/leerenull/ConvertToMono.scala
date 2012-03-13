/*
 *  ConvertToMono.scala
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

import java.io.File
import de.sciss.strugatzki.aux.{Processor, ProcessorCompanion}
import collection.immutable.{IndexedSeq => IIdxSeq}
import actors.Actor
import de.sciss.kontur.gui.TrackList
import de.sciss.kontur.session.{Session, BasicTimeline, MatrixDiffusion, AudioTrack, AudioRegion, AudioFileElement}
import annotation.tailrec
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec, AudioFile}
import swing.Swing

object ConvertToMono extends ProcessorCompanion with KonturGoodies with GUIGoodies with NullGoodies with PasteCompanion {
   type PayLoad = (IIdxSeq[ AudioFileElement ], IIdxSeq[ (AudioTrack, IIdxSeq[ AudioRegion ])])

   var VERBOSE = false

   def perform( doc: Session, tl: BasicTimeline, trl: TrackList ) {
      openFileDialog( "Convert to Mono: Render folder", new File( LeereNull.bounceFolder, "mono" )) foreach { f =>
         render( doc, tl, trl, f.getParentFile )
      }
   }

   def render( doc: Session, tl: BasicTimeline, trl: TrackList, dir: File ) {
      // don't filter the muted ones at this point, because the process will do it,
      // and then they will be automatically removed after the render finishes.
      val arsIn0     = collectAudioRegions({ case x => x })( tl ).sortBy( _._2.span.start ).toIndexedSeq
      val arsRemove  = arsIn0.filter( _._2.audioFile.numChannels > 1 )
      val arsIn      = arsIn0.groupBy( _._1 ).mapValues( _.map( _._2 )).toIndexedSeq

      val dlg  = progressDialog( "Convert to Mono" )
      val proc = ConvertToMono( dir, arsIn ) {
         case ConvertToMono.Success( (newFiles, newRegions) ) =>
            dlg.stop()
            val reg1 = newRegions.flatMap {
               case (at, ars) => ars.map( ar => (at, ar))
            }
            Swing.onEDT( pasteResult( doc, tl, arsRemove, reg1, newFiles ))

         case ConvertToMono.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()

         case ConvertToMono.Aborted =>
            dlg.stop()

         case ConvertToMono.Progress( i ) => dlg.progress = i
      }
      dlg.start( proc )
   }

   def apply( bncDir: File, in: IIdxSeq[ (AudioTrack, IIdxSeq[ AudioRegion ])])( observer: ConvertToMono.Observer ) : ConvertToMono =
      new ConvertToMono( observer, bncDir, in )
}
class ConvertToMono( protected val observer: ConvertToMono.Observer, bncDir: File, in: IIdxSeq[ (AudioTrack, IIdxSeq[ AudioRegion ])])
extends Processor {
   import ConvertToMono._

   protected val companion = ConvertToMono

   private def mix( srcOff: Int, dst: Array[ Float ], dstOff: Int, len: Int )( src: Array[ Float ]) {
      var i = 0; while( i < len ) {
         dst( i + dstOff ) += src( i + srcOff )
      i += 1 }
   }

   private def mul( mul: Float )( buf: Array[ Float ]) {
      if( mul == 1 ) return
      var i = 0; while( i < buf.length ) {
         buf( i ) = buf( i ) * mul
      i += 1 }
   }

   private type Key = (Seq[ Float ], File, Long, Long)

   protected def body() : Result = {
//      var map = Map.empty[ (Seq[ Float ], File), AudioFileElement ]

      val inMap: Map[ (AudioTrack, AudioRegion), Key ] = in.flatMap({
         case (at, ars) =>
            at.diffusion match {
               case Some( m: MatrixDiffusion ) if m.numInputChannels > 1 =>
                  val mat = m.matrix.toSeq.map( _.sum )   // Matrix2D equals currently missing
                  val arsF = ars.filter( _.audioFile.numChannels > 1 )
                  arsF.map { ar =>
                     ((at, ar), (mat, ar.audioFile.path, ar.offset, ar.offset + ar.span.length))
                  }
               case _ => IIdxSeq.empty
            }
      })( collection.breakOut )

      val keys = inMap.values.toSet
      val num = keys.size
      val outMap: Map[ Key, AudioFileElement ] = keys.zipWithIndex.map({
         case (key @ (chanGains, inF, fileStart, fileStop), idx) =>
            @tailrec def newFileName( prefix: String, cnt: Int, suffix: String ) : File = {
               val test = new File( bncDir, prefix + cnt+ suffix )
               if( !test.exists() ) test else newFileName( prefix, cnt + 1, suffix )
            }
            val outF = newFileName( plainName( inF ) + "-M", 1, ".aif" )
            assert( !outF.exists() )
            val afIn = AudioFile.openRead( inF )
            try {
               val afOut = AudioFile.openWrite( outF, AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, 1, afIn.sampleRate ))
               try {
                  val bufSz      = 8192
                  val inBuf      = afIn.buffer( bufSz )
                  val inBufGain  = inBuf zip chanGains
                  val inBufRem   = inBuf.drop( 1 )
                  val inBuf0     = inBuf( 0 )
                  val outBuf     = Array( inBuf0 )
                  var rem        = fileStop - fileStart
                  afIn.seek( fileStart )
                  while( rem > 0 ) {
                     val chunk = math.min( rem, bufSz ).toInt
                     afIn.read( inBuf, 0, chunk )
                     inBufGain.foreach {
                        case (chanBuf, gain) => mul( gain )( chanBuf )
                     }
                     inBufRem.foreach( mix( 0, inBuf0, 0, chunk ))
                     afOut.write( outBuf, 0, chunk )
                     rem -= chunk
                  }
               } finally {
                  afOut.cleanUp()
               }
            } finally {
               afIn.cleanUp()
            }

            val afe = AudioFileElement( outF, afIn.numFrames, 1, afIn.sampleRate )

            progress( (idx + 1).toFloat / num )
            if( checkAborted ) return Aborted

            (key, afe)
      })( collection.breakOut )

      val map2: IIdxSeq[ (AudioTrack, AudioRegion) ] = inMap.map({
         case (keyIn @ (at, arIn), keyOut) =>
            val afe = outMap( keyOut )
            val arOut = arIn.copy( audioFile = afe, offset = 0L )
            (at, arOut)
      })( collection.breakOut )

      val map3 = map2.groupBy( _._1 ).mapValues( _.map( _._2 )).toIndexedSeq

      val newFiles = outMap.values.toIndexedSeq

      Success( (newFiles, map3) )
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
