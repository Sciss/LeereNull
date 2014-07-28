/*
 *  FScape.scala
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

import de.sciss.fscape.FScapeJobs
import java.io.File
import de.sciss.processor.{GenericProcessor, Processor}
import de.sciss.processor.impl.ProcessorImpl

import swing.Swing

object FScape extends GUIGoodies {
//   sealed trait ProgressOrResult
//   final case class Progress( i: Int ) extends ProgressOrResult
//   sealed trait Result extends ProgressOrResult
//   case object Success extends Result
//   case object Failure extends Result

   private lazy val jobs = {
      val res = FScapeJobs()
      res.connect() { b =>
         if( !b ) message( "FScape could not be connected!" )
      }
      res
   }

   def shift( freq: Double )( in: File, out: File )( fun: Boolean => Unit ): Unit = {
      val spec = FScapeJobs.OutputSpec.aiffFloat
      val doc  = FScapeJobs.Hilbert( in.getAbsolutePath, out.getAbsolutePath, spec = spec,
                                     gain = FScapeJobs.Gain.immediate, freq = freq, antiAlias =  true )
//      jobs.process( "Hilbert", doc, progress = (i: Int) => fun( Progress( i )))( b => fun( if( b ) Success else Failure ))
      process( "Hilbert", doc )( fun )
   }

   /** @param cents  the pitch factor, not time factor! (thus 1200 means octave up / double speed) */
   def resample( cents: Double )( in: File, out: File )( fun: Boolean => Unit ): Unit = {
      val spec = FScapeJobs.OutputSpec.aiffFloat
      val doc  = FScapeJobs.Resample( in.getAbsolutePath, out.getAbsolutePath, spec = spec,
                                      gain = FScapeJobs.Gain.immediate,
                                      rate = (cents / -100).toString + "semi", keepHeader = true )
      process( "Resample", doc )( fun )
   }

   def process( title: String, doc: FScapeJobs.Doc )( fun: Boolean => Unit ): Unit = {
      val dlg = progressDialog( title )
      //      val proc = new {
      //         def start(): Unit = {
      //         }
      //
      //         def abort(): Unit = {
      //            println( "Ooops. Abort not supported" )
      //         }
      //      }
      val proc: ProcessorImpl[Unit, GenericProcessor[Unit]] =
        new ProcessorImpl[Unit, GenericProcessor[Unit]] with GenericProcessor[Unit] {

          protected def body(): Unit =
            jobs.process( title, doc, progress = (i: Int) => Swing.onEDT( dlg.progress = i )) { b =>
              Swing.onEDT {
                dlg.stop()
                fun( b )
              }
            }
        }

     dlg.start(proc)
   }
}