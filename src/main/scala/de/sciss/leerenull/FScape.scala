package de.sciss.leerenull

import de.sciss.fscape.FScapeJobs
import java.io.File
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

   def shift( freq: Double )( in: File, out: File )( fun: Boolean => Unit ) {
      val spec = FScapeJobs.OutputSpec.aiffFloat
      val doc  = FScapeJobs.Hilbert( in.getAbsolutePath, out.getAbsolutePath, spec = spec,
                                     gain = FScapeJobs.Gain.immediate, freq = freq, antiAlias =  true )
//      jobs.process( "Hilbert", doc, progress = (i: Int) => fun( Progress( i )))( b => fun( if( b ) Success else Failure ))
      process( "Hilbert", doc )( fun )
   }

   def resample( cents: Double )( in: File, out: File )( fun: Boolean => Unit ) {
      val spec = FScapeJobs.OutputSpec.aiffFloat
      val doc  = FScapeJobs.Resample( in.getAbsolutePath, out.getAbsolutePath, spec = spec,
                                      gain = FScapeJobs.Gain.immediate,
                                      rate = "GAGA", keepHeader = true, interpolate = false, fltLength = "GAGA" )
      process( "Resample", doc )( fun )
   }

   def process( title: String, doc: FScapeJobs.Doc )( fun: Boolean => Unit ) {
      val dlg = progressDialog( title )
      val proc = new {
         def start() {
            jobs.process( title, doc, progress = (i: Int) => Swing.onEDT( dlg.progress = i )) { b =>
              Swing.onEDT {
                 dlg.stop()
                 fun( b )
              }
            }
         }

         def abort() {
            println( "Ooops. Abort not supported" )
         }
      }
      dlg.start( proc )
   }
}