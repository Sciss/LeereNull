package de.sciss.leerenull

import java.io.File

object SonogramLayer {
   val pixelsPerSecond = 50
   val offX = 100
   val offY = 200

   def apply( video: Video, name: String, startTime: Double = 0.0 ) : SonogramLayer =
      new SonogramLayer( video, name, startTime )
}

class SonogramLayer( protected val video: Video, name: String, startTime: Double ) extends VideoLayer {
   import SonogramLayer._

   lazy val img      = video.loadImage( new File( Video.dataFolder, name + ".png" ).getPath )
   lazy val duration = img.width.toDouble / pixelsPerSecond

   def stopTime = startTime + duration

   def draw() {
      import video._

      val delta = now - startTime
      if( delta < 0.0 || delta >= duration ) return

//      image( img, offX, offY
      val w = (delta / duration * img.width + 0.5).toInt
      val h = img.height
      copy( img, 0, 0, w, h, offX, offY, w, h )
   }
}