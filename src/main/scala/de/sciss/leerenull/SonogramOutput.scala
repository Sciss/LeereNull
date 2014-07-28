/*
 *  SonogramOutput.scala
 *  (LeereNull)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.leerenull

import java.io.File
import de.sciss.processor.{Processor, ProcessorFactory}
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.sonogram.{OverviewManager, PaintController}
import de.sciss.span.Span

import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
import javax.imageio.ImageIO
import java.awt.image.{ImageObserver, BufferedImage}
import java.awt.{Color, Component}
import de.sciss.kontur.session.{FadeSpec, AudioRegion}

object SonogramOutput extends ProcessorFactory {
   lazy val mgr: OverviewManager = OverviewManager()

   type Product = Unit

  case class Config(ars: IndexedSeq[AudioRegion], output: File, gainOffset: Float = 1.0e-6f, gainFactor: Float = 15f,
                    pixelsPerSecond: Int = 59, height: Int = 144)

  protected def prepare(config: Config): Prepared =
    new SonogramOutput(config.ars, config.output, config.gainOffset, config.gainFactor, config.pixelsPerSecond,
      config.height )

  type Repr = SonogramOutput
}

class SonogramOutput(ars: IndexedSeq[AudioRegion], output: File, gainOffset: Float, gainFactor: Float, pixelsPerSecond: Int = 59, height: Int = 144)
  extends ProcessorImpl[SonogramOutput.Product, SonogramOutput] {

  import SonogramOutput._

  require(ars.size > 0)

   protected val companion = SonogramOutput

   protected def body() : Product = {
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

      def withEachChannel( buf: Array[ Array[ Float ]])( fun: Array[ Float ] => Unit ): Unit = {
         var ch = 0; while( ch < buf.length ) {
            fun( buf( ch ))
         ch += 1 }
      }

      def clear( buf: Array[ Float ]): Unit = {
         var i = 0; while( i < buf.length ) {
            buf( i ) = 0f
         i += 1 }
      }

      def mix( srcOff: Int, dst: Array[ Float ], dstOff: Int, len: Int )( src: Array[ Float ]): Unit = {
         var i = 0; while( i < len ) {
            dst( i + dstOff ) += src( i + srcOff )
         i += 1 }
      }

      def mulAdd( mul: Float, add: Float )( buf: Array[ Float ]): Unit = {
         var i = 0; while( i < buf.length ) {
            buf( i ) = buf( i ) * mul + add
         i += 1 }
      }

      def fadeIn( fd: FadeSpec, pos: Long, len: Int )( buf: Array[ Float ]): Unit = {
         var i = 0; while( i < len ) {
            val f = ((pos + i).toDouble / fd.numFrames).toFloat
            if( f < 1f ) {
               buf( i ) *= fd.shape.levelAt( math.max( 0f, f ), 0f, 1f )
            }
         i += 1 }
      }

      def fadeOut( fd: FadeSpec, pos: Long, numFrames: Long, len: Int )( buf: Array[ Float ]): Unit = {
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

            while (tlPos < tlStop) {
              checkAborted()
              val chunkLen  = math.min(bufSize, tlStop - tlPos).toInt
              val chunkSpan = Span(tlPos, tlPos + chunkLen)
               clear( bufOut0 )
               zipped.foreach { case (af, ar) =>
                  ar.span.intersect(chunkSpan) match {
                    case iSpan @ Span(_, _) =>
                      val iSpanLen   = iSpan.length.toInt
                     val delta = iSpan.start - ar.span.start
                     val afPos = delta + ar.offset
                     if( af.position != afPos ) af.seek( afPos )
                     af.read( bufIn, 0, iSpanLen )
                     ar.fadeIn.foreach { fd =>
                        withEachChannel( bufIn )( fadeIn( fd, delta, iSpanLen ))
                     }
                     ar.fadeOut.foreach { fd =>
                        withEachChannel( bufIn )( fadeOut( fd, delta, ar.span.length, iSpanLen ))
                     }
                     val mul = (ar.gain * gainFactor / math.sqrt( af.numChannels )).toFloat
                     withEachChannel( bufIn )( mulAdd( mul, gainOffset ))
                     withEachChannel( bufIn )( mix( 0, bufOut0, (iSpan.start - chunkSpan.start).toInt, iSpanLen ))

                    case _ =>
                  }
               }

               afOut.write( bufOut, 0, chunkLen )

               tlPos += chunkLen
               progress = (((tlPos - tlStart).toDouble / numFrames) * 0.5).toFloat
            }

         } finally {
            afOut.cleanUp()
         }
      } finally {
         afIns.foreach( _.cleanUp() )
      }

      val imgW = (numFrames * pixelsPerSecond / sampleRate + 0.5).toInt
      val imgH = height

     val sonoJob = OverviewManager.Job(tmpF)
      val sono = mgr.acquire(sonoJob)
      try {
         val monitor    = new AnyRef
         var complete   = false
         sono.addListener {
            case Processor.Result(_, _) =>
               println( "Completed." )
               complete = true
               monitor.synchronized { monitor.notifyAll() }
         }

         println( "Calculating sonogram..." )

         while( !complete ) {
            monitor.synchronized {
               monitor.wait( 1000 )
              checkAborted()
            }
         }

         progress = 0.75f

         val img  = new BufferedImage( imgW, imgH, BufferedImage.TYPE_INT_ARGB )
         val obs  = new Component {}

//         val rnd = new util.Random()
//         val fadePaint = SonogramFadePaint( obs, ar, gainFactor )
         val ctrl = new PaintController {
            def adjustGain( amp: Float, pos: Double ) : Float = amp // * fadePaint.sonogramGain( pos / ar.span.getLength ) + gainOffset
            def imageObserver : ImageObserver = obs
         }

         val g2   = img.createGraphics()
         try {
            g2.setColor( Color.black )
            g2.fillRect( 0, 0, imgW, imgH )
            sono.paint( 0.0, numFrames.toDouble, g2, 0, 0, imgW, imgH, ctrl )
            ImageIO.write( img, "png", output )
            progress = 1.0f

         } finally {
            g2.dispose()
         }

      } finally {
        mgr.release(sono)
         // sono.dispose()
      }
   }

  //   protected val Act = new Actor {
  //      def act(): Unit = {
  //         ProcT.start()
  //         var result : Product = null
  //         loopWhile( result == null ) {
  //            react {
  //               case Abort =>
  //                  ProcT.aborted = true
  //                  aborted()
  //               case res: Progress =>
  //                  observer( res )
  //               case res @ Aborted =>
  //                  result = res
  //               case res: Failure =>
  //                  result = res
  //               case res: Success =>
  //                  result = res
  //            }
  //         } andThen { observer( result )}
  //      }
  //   }
}