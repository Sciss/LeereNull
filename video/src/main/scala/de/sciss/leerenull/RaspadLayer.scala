/*
 *  RaspadLayer.scala
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

object RaspadLayer {
   val startIdx   = 1 // 169
   val stopIdx    = 385 // 553 // 999
   val raspadFPS  = 24
//   val startTime  = 0.0
//   val startTime  = TitleLayer.startTime + TitleLayer.duration + 1.0
   val raspadWidth = 640
   val raspadHeight = 480
   val offX       = (Video.videoWidth - raspadWidth) / 2 // 0
   val offY       = (Video.videoHeight - raspadHeight) / 3 // / 2
   val fadeIn     = 2.0
   val fadeOut    = 2.0

   def numFrames  = stopIdx - startIdx
   def duration   = numFrames.toDouble / raspadFPS

   def apply( video: Video, startTime: Double = 0.0 ) : RaspadLayer = new RaspadLayer( video, startTime )
}
class RaspadLayer( protected val video: Video, val startTime: Double ) extends VideoLayer {
   import Video._
   import RaspadLayer.{duration => gDur, _}

   def duration = gDur
   def stopTime = startTime + duration

   def draw() {
      import video._
      val delta = now - startTime
      if( delta < 0.0 || delta >= duration ) return

      val alpha = (math.min( fadeIn, delta ) / fadeIn) * (math.min( fadeOut, duration - delta ) / fadeOut)
//println( alpha )
      val raspadIdx = ((now - startTime) * raspadFPS + 0.5).toInt
//      if( raspadIdx < (raspadStopIdx - raspadStartIdx) ) {
//         println( "AYA " + raspadIdx )
         val img = loadImage( new File( dataFolder, "RaspadExtr " + (raspadIdx + startIdx + 1000).toString.substring( 1 ) + ".png" ).getPath )
         if( alpha < 1.0 ) tint( 1.0f, alpha.toFloat )
         image( img, offX, offY )
         noTint()
//         raspadIdx += 1
//      }
   }
}