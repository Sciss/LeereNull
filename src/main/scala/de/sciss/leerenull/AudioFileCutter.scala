/*
 *  AudioFileCutter.scala
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

import de.sciss.span.Span
import de.sciss.synth
import java.io.File
import actors.Actor
import synth.io.{AudioFileType, AudioFile}

import scala.util.control.NonFatal

object AudioFileCutter {
   def apply( in: File, out: File, span: Span )( observer: PartialFunction[ ProgressOrResult, Unit ]) =
      new AudioFileCutter( in, out, span, observer )

   sealed trait ProgressOrResult
   final case class Progress( percent: Int ) extends ProgressOrResult
   sealed trait Result extends ProgressOrResult
   case object Success extends Result
   final case class Failure( t: Throwable ) extends Result
   case object Aborted extends Result
}
final class AudioFileCutter private ( val in: File, val out: File, val span: Span,
                                        observer: PartialFunction[ AudioFileCutter.ProgressOrResult, Unit ]) {
   import AudioFileCutter._

//   Act.start()

   def abort(): Unit = { Act ! Abort }
   def start(): Unit = { Act.start() }

   private object Abort

   private object ProcT extends Thread {
      var aborted: Boolean = false

      override def run(): Unit = {
         Act ! (try {
            if( procBody() ) Success else Aborted
         } catch {
            case NonFatal(e) => Failure( e )
         })
      }

      private def shouldAbort : Boolean = this.synchronized { aborted }

      private def procBody() : Boolean = {
         val afIn       = AudioFile.openRead( in )
         val afOut      = AudioFile.openWrite( out, afIn.spec.copy( fileType = AudioFileType.AIFF, byteOrder = None, numFrames = span.length ))
         var rem        = span.length
         afIn.seek( span.start )
         val b          = afIn.buffer( 4096 )
         var lastProg   = 0
         while( rem > 0L ) {
            val len  = math.min( rem, 4096 ).toInt
            afIn.read( b, 0, len )
            afOut.write( b, 0, len )
            rem -= len
            val prog = ((1.0 - (rem.toDouble / span.length)) * 100).toInt
            if( prog != lastProg ) {
               lastProg = prog
               Act ! Progress( prog )
            }
            if( shouldAbort ) return false
         }
         afIn.close
         afOut.close
         true
      }
   }

   private object Act extends Actor {
      def act(): Unit = {
         ProcT.start()
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.synchronized {
                     ProcT.aborted = true
//                     if( ProcT.p != null ) ProcT.p.destroy()
                  }
               case res @ Progress( _ ) =>
                  observer( res )
               case res @ Aborted =>
                  result = res
               case res @ Failure( _ ) =>
                  result = res
               case res @ Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }
}