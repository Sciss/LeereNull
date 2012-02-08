package de.sciss.leerenull

import java.io.File
import de.sciss.strugatzki.aux.{Processor, ProcessorCompanion}
import actors.Actor
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
import javax.imageio.ImageIO
import de.sciss.sonogram.{SonogramPaintController, OverviewComplete, SimpleSonogramOverviewManager}
import java.awt.image.{ImageObserver, BufferedImage}
import java.awt.{Color, Component}
import de.sciss.io.Span
import de.sciss.kontur.session.{FadeSpec, AudioRegion}

object SonogramOutput extends ProcessorCompanion {
   lazy val mgr = new SimpleSonogramOverviewManager()

   type PayLoad = Unit

   def apply( ars: IndexedSeq[ AudioRegion ], output: File, gainOffset: Float = 1.0e-6f, gainFactor: Float = 15f, pixelsPerSecond: Int = 59, height: Int = 144 )
            ( observer: Observer ) : SonogramOutput = {
      new SonogramOutput( observer, ars, output, gainOffset, gainFactor, pixelsPerSecond, height )
   }
}
class SonogramOutput( protected val observer: SonogramOutput.Observer,
                      ars: IndexedSeq[ AudioRegion ], output: File, gainOffset: Float, gainFactor: Float, pixelsPerSecond: Int = 59, height: Int = 144 )
extends Processor {
   import SonogramOutput._

   require( ars.size > 0 )

   protected val companion = SonogramOutput

   protected def body() : Result = {
      val afIns         = ars.map( ar => AudioFile.openRead( ar.audioFile.path ))
//      val numChannels   = afIn.numChannels
      val maxNumCh      = afIns.map( _.numChannels ).max
      val sampleRate    = afIns.head.sampleRate
      val tlStart       = ars.map( _.span.start ).min
      val tlStop        = ars.map( _.span.stop  ).max
      val numFrames     = tlStop - tlStart // ar.span.getLength // afIn.numFrames
      val tmpF          = File.createTempFile( "sono", ".aif" )
      val zipped        = afIns.zip( ars )
      val bufSize       = 8192

      def withEachChannel( buf: Array[ Array[ Float ]])( fun: Array[ Float ] => Unit ) {
         var ch = 0; while( ch < buf.length ) {
            fun( buf( ch ))
         ch += 1 }
      }

      def clear( buf: Array[ Float ]) {
         var i = 0; while( i < buf.length ) {
            buf( i ) = 0f
         i += 1 }
      }

      def mix( srcOff: Int, dst: Array[ Float ], dstOff: Int, len: Int )( src: Array[ Float ]) {
         var i = 0; while( i < len ) {
            dst( i + dstOff ) += src( i + srcOff )
         i += 1 }
      }

      def mulAdd( mul: Float, add: Float )( buf: Array[ Float ]) {
         var i = 0; while( i < buf.length ) {
            buf( i ) = buf( i ) * mul + add
         i += 1 }
      }

      def fadeIn( fd: FadeSpec, pos: Long, len: Int )( buf: Array[ Float ]) {
         var i = 0; while( i < len ) {
            val f = ((pos + i).toDouble / fd.numFrames).toFloat
            if( f < 1f ) {
               buf( i ) *= fd.shape.levelAt( math.max( 0f, f ), 0f, 1f )
            }
         i += 1 }
      }

      def fadeOut( fd: FadeSpec, pos: Long, numFrames: Long, len: Int )( buf: Array[ Float ]) {
         var i = 0; while( i < len ) {
            val f = ((pos + i - (numFrames - fd.numFrames)).toDouble / fd.numFrames).toFloat
            if( f > 0f ) {
               buf( i ) *= fd.shape.levelAt( math.min( 1f, f ), 1f, 0f )
            }
         i += 1 }
      }

      try {
         val afOut   = AudioFile.openWrite( tmpF, AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, 1, sampleRate ))
         try {
//            val buf           = afIn.buffer( 8192 )
            val bufIn         = AudioFile.buffer( maxNumCh, bufSize )
            val bufOut        = afOut.buffer( bufSize )
//            afIn.seek( ar.offset )
            var tlPos         = tlStart // 0L
            val bufOut0       = bufOut( 0 )
//            val gain          = (ar.gain / math.sqrt( numChannels )).toFloat
            while( (tlPos < tlStop) && !checkAborted ) {
               val chunkLen   = math.min( bufSize, tlStop - tlPos ).toInt
               val chunkSpan  = new Span( tlPos, tlPos + chunkLen )
               clear( bufOut0 )
               zipped.foreach { case (af, ar) =>
                  val iSpan      = ar.span.intersection( chunkSpan )
                  val iSpanLen   = iSpan.getLength.toInt
                  if( iSpanLen > 0 ) {
                     val delta = iSpan.start - ar.span.start
                     val afPos = delta + ar.offset
                     if( af.position != afPos ) af.seek( afPos )
                     af.read( bufIn, 0, iSpanLen )
                     ar.fadeIn.foreach { fd =>
                        withEachChannel( bufIn )( fadeIn( fd, delta, iSpanLen ))
                     }
                     ar.fadeOut.foreach { fd =>
                        withEachChannel( bufIn )( fadeOut( fd, delta, ar.span.getLength, iSpanLen ))
                     }
                     val mul = (ar.gain * gainFactor / math.sqrt( af.numChannels )).toFloat
                     withEachChannel( bufIn )( mulAdd( mul, gainOffset ))
                     withEachChannel( bufIn )( mix( 0, bufOut0, (iSpan.start - chunkSpan.start).toInt, iSpanLen ))
                  }
               }

               afOut.write( bufOut, 0, chunkLen )

               tlPos += chunkLen
               progress( (((tlPos - tlStart).toDouble / numFrames) * 0.5).toFloat )
            }
            if( checkAborted ) return Aborted
         } finally {
            afOut.cleanUp()
         }
      } finally {
         afIns.foreach( _.cleanUp() )
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
//         val fadePaint = SonogramFadePaint( obs, ar, gainFactor )
         val ctrl = new SonogramPaintController {
            def adjustGain( amp: Float, pos: Double ) : Float = amp // * fadePaint.sonogramGain( pos / ar.span.getLength ) + gainOffset
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