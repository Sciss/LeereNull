package de.sciss.leerenull

import java.io.File
import processing.core.PApplet
import java.awt.RenderingHints

object MovingImage {
   trait Warp {
      def apply( w: Double ) : Double
   }
   case object LinearWarp extends Warp {
      def apply( w: Double ) : Double = w
   }
   case object CosineWarp extends Warp {
      def apply( w: Double ) : Double = (1.0 - math.cos( w * math.Pi )) * 0.5
   }
   // first a then b
   final case class Combine( a: Warp, b: Warp ) extends Warp {
      def apply( w: Double ) : Double = b( a( w ))
   }
   final case class Pow( f: Double ) extends Warp {
      def apply( w: Double ) : Double = math.pow( w, f )
   }

   def apply( video: VideoLike, fileName: String, startTime: Double, duration: Double,
              startX: Double, startY: Double, startZoom: Double,
              stopX: Double, stopY: Double, stopZoom: Double,
              interpX: Warp, interpY: Warp, interpZoom: Warp ) : MovingImage =
      new MovingImage( video, fileName, startTime, duration, startX, startY, startZoom,
                       stopX, stopY, stopZoom, interpX, interpY, interpZoom )
}
class MovingImage( val video: VideoLike, val fileName: String, val startTime: Double, val duration: Double,
                   val startX: Double, val startY: Double, val startZoom: Double,
                   val stopX: Double, val stopY: Double, val stopZoom: Double,
                   val interpX: MovingImage.Warp, val interpY: MovingImage.Warp, val interpZoom: MovingImage.Warp )
extends VideoLayer {
   def stopTime = startTime + duration

   private lazy val img = video.loadImage( new File( Video.dataFolder, fileName ).getPath )

   def draw() {
      import video._

      val delta = now - startTime
      if( delta < 0.0 || delta >= duration ) return

      val w    = delta / duration
      val wx   = interpX( w )
      val wy   = interpY( w )
      val wz   = interpZoom( w )

      val x    = (1 - wx) * startX + wx * stopX
      val y    = (1 - wy) * startY + wy * stopY
      val z    = (1 - wz) * startZoom + wz * stopZoom

      val hints = g2d.getRenderingHints
      try {
         g2d.setRenderingHint( RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC )

         pushMatrix()
         translate( x.toFloat, y.toFloat )
         scale( z.toFloat )
         image( img, 0f, 0f )
         popMatrix()

      } finally {
         g2d.setRenderingHints( hints )
      }
   }
}
