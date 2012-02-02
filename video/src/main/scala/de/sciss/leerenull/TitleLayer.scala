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

import processing.core.PConstants

object TitleLayer {
//   val startTime  = 0.0
//   val duration   = 7.0 // 10.0
//   val fadeIn     = 2.0
//   val fadeOut    = 2.5
   val fontFace   = "Calisto MT" // AkkoRoundedPro-Regular"
//   val fontSize   = 72 // 80
//   val title      = "Leere Null (2)"
//   val offY       = 120

   def apply( video: Video, startTime: Double = 0.0, duration: Double = 7.0, title: String,
              fontSize: Int = 72, offY: Int = 120, fadeIn: Double = 2.0, fadeOut: Double = 2.5 ) : TitleLayer =
      new TitleLayer( video, startTime, duration, title, fontSize, offY, fadeIn, fadeOut )
}
class TitleLayer( protected val video: Video, val startTime: Double, duration: Double, title: String, fontSize: Int,
                  offY: Int, fadeIn: Double, fadeOut: Double )
extends VideoLayer {
   import Video._
   import TitleLayer._
   import PConstants._

   def stopTime = startTime + duration

//   lazy val titleFont = new Font( fontFace, Font.PLAIN, fontSize )
   lazy val titleFont = video.createFont( fontFace, fontSize, true )
//   createFont(String name, float size, boolean smooth)
//   lazy val titleFont = video.loadFont()

   def draw() {
      import video._
      val delta = now - startTime
      if( delta < 0.0 || delta >= duration ) return

      val a1      = if( fadeIn > 0.0 ) (math.min( fadeIn, delta ) / fadeIn) else 1.0
      val a2      = if( fadeOut > 0.0 ) (math.min( fadeOut, duration - delta ) / fadeOut) else 1.0
//println( "in " + a1 + " out " + a2 + " duration - delta = " + (duration - delta) )
      val alpha   = a1 * a2

      textAlign( CENTER )
      fill( 1.0f, alpha.toFloat )
      textFont( titleFont )
      text( title, videoWidth / 2, offY )
   }
}