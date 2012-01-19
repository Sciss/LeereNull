/*
 *  FScape.scala
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

   /** @param cents  the pitch factor, not time factor! (thus 1200 means octave up / double speed) */
   def resample( cents: Double )( in: File, out: File )( fun: Boolean => Unit ) {
      val spec = FScapeJobs.OutputSpec.aiffFloat
      val doc  = FScapeJobs.Resample( in.getAbsolutePath, out.getAbsolutePath, spec = spec,
                                      gain = FScapeJobs.Gain.immediate,
                                      rate = (cents / -100).toString + "semi", keepHeader = true )
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