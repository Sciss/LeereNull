/*
 *  ConvertToMono.scala
 *  (LeereNull)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.leerenull

import java.io.File
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{Processor, ProcessorFactory}

import collection.immutable.{IndexedSeq => Vec}
import de.sciss.kontur.gui.TrackList
import de.sciss.kontur.session.{Session, BasicTimeline, MatrixDiffusion, AudioTrack, AudioRegion, AudioFileElement}
import annotation.tailrec
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec, AudioFile}
import scala.util.{Failure, Success}
import swing.Swing

object ConvertToMono extends ProcessorFactory with KonturGoodies with GUIGoodies with NullGoodies with PasteCompanion {
  type Product = (Vec[AudioFileElement], Vec[(AudioTrack, Vec[AudioRegion])])

   var VERBOSE = false

   def perform( doc: Session, tl: BasicTimeline, trl: TrackList ): Unit = {
      openFileDialog( "Convert to Mono: Render folder", new File( LeereNull.bounceFolder, "mono" )) foreach { f =>
         render( doc, tl, trl, f.getParentFile )
      }
   }

   def render( doc: Session, tl: BasicTimeline, trl: TrackList, dir: File ): Unit = {
      // don't filter the muted ones at this point, because the process will do it,
      // and then they will be automatically removed after the render finishes.
      val arsIn0     = collectAudioRegions({ case x => x })( tl ).sortBy( _._2.span.start ).toIndexedSeq
      val arsRemove  = arsIn0.filter( _._2.audioFile.numChannels > 1 )
      val arsIn      = arsIn0.groupBy( _._1 ).mapValues( _.map( _._2 )).toIndexedSeq

      val dlg  = progressDialog( "Convert to Mono" )
      val proc = ConvertToMono(Config(dir, arsIn))
      proc.addListener {
        case Processor.Result(_, Success((newFiles, newRegions))) =>
          dlg.stop()
          val reg1 = newRegions.flatMap {
            case (at, ars) => ars.map(ar => (at, ar))
          }
          Swing.onEDT(pasteResult(doc, tl, arsRemove, reg1, newFiles))

        case Processor.Result(_, Failure(Processor.Aborted())) =>
          dlg.stop()

        case Processor.Result(_, Failure(e)) =>
            dlg.stop()
            e.printStackTrace()

         case prog @ Processor.Progress(_, _) => dlg.progress = prog.toInt
      }
      dlg.start( proc )
   }

  case class Config(bncDir: File, in: Vec[(AudioTrack, Vec[AudioRegion])])

  type Repr = ConvertToMono

  protected def prepare(config: Config): Prepared =
    new ConvertToMono(config.bncDir, config.in)
}

class ConvertToMono(bncDir: File, in: Vec[(AudioTrack, Vec[AudioRegion])])
  extends ProcessorImpl[ConvertToMono.Product, ConvertToMono] {

  import ConvertToMono._

   protected val companion = ConvertToMono

   private def mix( srcOff: Int, dst: Array[ Float ], dstOff: Int, len: Int )( src: Array[ Float ]): Unit = {
      var i = 0; while( i < len ) {
         dst( i + dstOff ) += src( i + srcOff )
      i += 1 }
   }

   private def mul( mul: Float )( buf: Array[ Float ]): Unit = {
      if( mul == 1 ) return
      var i = 0; while( i < buf.length ) {
         buf( i ) = buf( i ) * mul
      i += 1 }
   }

   private type Key = (Seq[ Float ], File, Long, Long)

  protected def body(): Product = {
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
               case _ => Vec.empty
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

            progress = (idx + 1).toFloat / num
            checkAborted()

            (key, afe)
      })( collection.breakOut )

      val map2: Vec[ (AudioTrack, AudioRegion) ] = inMap.map({
         case (keyIn @ (at, arIn), keyOut) =>
            val afe = outMap( keyOut )
            val arOut = arIn.copy( audioFile = afe, offset = 0L )
            (at, arOut)
      })( collection.breakOut )

      val map3 = map2.groupBy( _._1 ).mapValues( _.map( _._2 )).toIndexedSeq

      val newFiles = outMap.values.toIndexedSeq

      (newFiles, map3)
   }

  //   protected val Act = new Actor {
  //      def act(): Unit = {
  //         ProcT.start()
  //        var result: Product = null
  //         loopWhile( result == null ) {
  //            react {
  //               case Abort =>
  //                  ProcT.aborted = true
  //                  aborted()
  //               case res: Progress =>
  //                  observer( res )
  //               case res @ Aborted =>
  //                  result = res
  //               case res: Failure =>
  //                  result = res
  //               case res: Success =>
  //                  result = res
  //            }
  //         } andThen { observer( result )}
  //      }
  //   }
}
