package de.sciss.leerenull

import java.io.File
import de.sciss.kontur.session.AudioRegion
import de.sciss.strugatzki.aux.{Processor, ProcessorCompanion}
import actors.Actor
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
import javax.imageio.ImageIO
import de.sciss.sonogram.{SonogramPaintController, OverviewComplete, SimpleSonogramOverviewManager}
import java.awt.image.{ImageObserver, BufferedImage}
import java.awt.{Color, Component}

object SonogramOutput extends ProcessorCompanion {
   lazy val mgr = new SimpleSonogramOverviewManager()

   type PayLoad = Unit

   def apply( ar: AudioRegion, output: File, gainOffset: Float = 1.0e-6f, gainFactor: Float = 15f, pixelsPerSecond: Int = 50, height: Int = 160 )
            ( observer: Observer ) : SonogramOutput = {
      new SonogramOutput( observer, ar, output, gainOffset, gainFactor, pixelsPerSecond, height )
   }
}
class SonogramOutput( protected val observer: SonogramOutput.Observer,
                      ar: AudioRegion, output: File, gainOffset: Float, gainFactor: Float, pixelsPerSecond: Int = 50, height: Int = 160 )
extends Processor {
   import SonogramOutput._

   protected val companion = SonogramOutput

   protected def body() : Result = {
      val afIn          = AudioFile.openRead( ar.audioFile.path )
      val numChannels   = afIn.numChannels
      val sampleRate    = afIn.sampleRate
      val numFrames     = ar.span.getLength // afIn.numFrames
      val tmpF          = File.createTempFile( "sono", ".aif" )

      try {
         val afOut   = AudioFile.openWrite( tmpF, AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, 1, sampleRate ))
         try {
            val buf           = afIn.buffer( 8192 )
            afIn.seek( ar.offset )
            var read          = 0L
            val buf0          = buf( 0 )
            val gain          = (ar.gain / math.sqrt( numChannels )).toFloat
            while( (read < numFrames) && !checkAborted ) {
               val chunkLen = math.min( 8192, numFrames - read ).toInt
               afIn.read( buf, 0, chunkLen )
               var ch = 1; while( ch < numChannels ) {
                  val buf1 = buf( ch )
                  var i = 0; while( i < chunkLen ) {
                     buf0( i ) += buf1( i )
                  i += 1 }
               ch += 1 }
               var i = 0; while( i < chunkLen ) {
                  buf0( i ) *= gain
               i += 1 }

               afOut.write( buf, 0, chunkLen )

               read += chunkLen
               progress( ((read.toDouble / numFrames) * 0.5).toFloat )
            }
            if( checkAborted ) return Aborted
         } finally {
            afOut.cleanUp()
         }
      } finally {
         afIn.cleanUp()
      }

      val imgW = (numFrames * pixelsPerSecond / sampleRate + 0.5).toInt
      val imgH = height

      val sono       = mgr.fromPath( tmpF )
      try {
         val monitor    = new AnyRef
         var complete   = false
         sono.addListener {
            case OverviewComplete( _ ) =>
               println( "Completed." )
               complete = true
               monitor.synchronized { monitor.notifyAll() }
         }

         println( "Calculating sonogram..." )

         while( !complete ) {
            monitor.synchronized {
               monitor.wait( 1000 )
               if( checkAborted ) return Aborted
            }
         }

         progress( 0.75f )

         val img  = new BufferedImage( imgW, imgH, BufferedImage.TYPE_INT_ARGB )
         val obs  = new Component {}

//         val rnd = new util.Random()
         val ctrl = new SonogramPaintController {
            def adjustGain( amp: Float, pos: Double ) : Float = amp * gainFactor + gainOffset
            def imageObserver : ImageObserver = obs
         }

         val g2   = img.createGraphics()
         try {
            g2.setColor( Color.black )
            g2.fillRect( 0, 0, imgW, imgH )
            sono.paint( 0.0, numFrames.toDouble, g2, 0, 0, imgW, imgH, ctrl )
            ImageIO.write( img, "png", output )
            progress( 1.0f )

         } finally {
            g2.dispose()
         }

      } finally {
         sono.dispose()
      }

      Success( () )
   }

   protected val Act = new Actor {
      def act() {
         ProcT.start()
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
                  aborted()
               case res: Progress =>
                  observer( res )
               case res @ Aborted =>
                  result = res
               case res: Failure =>
                  result = res
               case res: Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }
}