package de.sciss.leerenull

import de.sciss.fscape.FScapeJobs
import java.io.File

object FScape extends GUIGoodies {
   private lazy val jobs = {
      val res = FScapeJobs()
      res.connect() { b =>
         if( !b ) message( "FScape could not be connected!" )
      }
      res
   }

   def shift( in: File, out: File, freq: Double )( fun: Boolean => Unit ) {
      val spec = FScapeJobs.OutputSpec.aiffFloat
      val doc  = FScapeJobs.Hilbert( in.getAbsolutePath, out.getAbsolutePath, spec = spec,
                                     gain = FScapeJobs.Gain.immediate, freq = freq, antiAlias =  true )
      jobs.process( "Hilbert", doc )( fun )
   }
}