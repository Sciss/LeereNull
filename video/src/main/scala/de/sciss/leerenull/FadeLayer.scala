/*
 *  TitleLayer.scala
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

object FadeLayer {
   def in(    video: Video, startTime: Double, duration: Double ) = new FadeLayer( video, startTime, duration, 1f, 0f )
   def out(   video: Video, startTime: Double, duration: Double ) = new FadeLayer( video, startTime, duration, 0f, 1f )
   def black( video: Video, startTime: Double, duration: Double ) = new FadeLayer( video, startTime, duration, 1f, 1f )
}
class FadeLayer( protected val video: Video, val startTime: Double, val duration: Double, startAlpha: Float, stopAlpha: Float )
extends VideoLayer {
   def stopTime = startTime + duration

   def draw() {
      import video._

      val delta = now - startTime
      if( delta < 0.0 || delta >= duration ) return

      val w = delta / duration
      val alpha = (startAlpha * (1 - w) + stopAlpha * w).toFloat

//      println( "W = " + w + " Alpha = " + alpha )

      if( alpha > 0f ) {
         fill( color( 0f, alpha.toFloat ))
         rect( 0, 0, width, height )
      }
   }
}
