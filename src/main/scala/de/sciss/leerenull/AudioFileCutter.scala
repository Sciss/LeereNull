/*
 *  AudioFileCutter.scala
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

import de.sciss.processor.ProcessorFactory
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.span.Span
import de.sciss.synth
import java.io.File
import synth.io.{AudioFileType, AudioFile}

object AudioFileCutter extends ProcessorFactory {
  type Repr     = AudioFileCutter
  type Product  = Unit

  case class Config(in: File, out: File, span: Span)

  protected def prepare(config: Config): Prepared =
    new AudioFileCutter(config.in, config.out, config.span)
}

final class AudioFileCutter private(val in: File, val out: File, val span: Span)
  extends ProcessorImpl[Unit, AudioFileCutter] {

   import AudioFileCutter._

  protected def body(): Unit = {
    val afIn = AudioFile.openRead(in)
    val afOut = AudioFile.openWrite(out, afIn.spec.copy(fileType = AudioFileType.AIFF, byteOrder = None, numFrames = span.length))
    var rem = span.length
    afIn.seek(span.start)
    val b = afIn.buffer(4096)
    var lastProg = 0
    while (rem > 0L) {
      val len = math.min(rem, 4096).toInt
      afIn.read(b, 0, len)
      afOut.write(b, 0, len)
      rem -= len
      val prog = ((1.0 - (rem.toDouble / span.length)) * 100).toInt
      if (prog != lastProg) {
        lastProg = prog
        // Act ! Progress( prog )
        progress = prog
      }
      checkAborted()
    }
    afIn.close()
    afOut.close()
  }

  //   private object Act extends Actor {
  //      def act(): Unit = {
  //         ProcT.start()
  //         var result : Result = null
  //         loopWhile( result == null ) {
  //            react {
  //               case Abort =>
  //                  ProcT.synchronized {
  //                     ProcT.aborted = true
  ////                     if( ProcT.p != null ) ProcT.p.destroy()
  //                  }
  //               case res @ Progress( _ ) =>
  //                  observer( res )
  //               case res @ Aborted =>
  //                  result = res
  //               case res @ Failure( _ ) =>
  //                  result = res
  //               case res @ Success =>
  //                  result = res
  //            }
  //         } andThen { observer( result )}
  //      }
  //   }
}