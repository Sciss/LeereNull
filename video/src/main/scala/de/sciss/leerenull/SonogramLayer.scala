package de.sciss.leerenull

import java.io.File
import collection.immutable.{IndexedSeq => IIdxSeq}
import processing.core.{PConstants, PImage}

object SonogramLayer {
   val pixelsPerSecond  = 59 // 50
   val trackHeight      = 144 // 160 // 180
   val trackYOff        = (Video.videoHeight - (trackHeight * 5)) / 2 // 0 // 200
   val trackXOff        = 72 // 100

   def apply( video: VideoLike, instr: IIdxSeq[ Instruction ], startTime: Double = 0.0 ) : SonogramLayer =
      new SonogramLayer( video, instr, startTime )

//   trait InstrLike {
//      def imageID: Int
//      def trackIdx: Int
//      def spanStart: Double
//      def spanStop:  Double
//      def trackStart: Double
//      def presentationDuration: Double
//      def presentationFadeIn: Double
//      def presentationFadeOut: Double
//      def next: Seq[ Instr ]
//   }
//
//   case class Instr( imageID: Int, trackIdx: Int, spanStart: Double, spanStop: Double, trackStart: Double,
//                     presentationDuration: Double, presentationFadeIn: Double, presentationFadeOut: Double )

   object Region {
      def apply( imageFile: File ) : Region = {
         val n = imageFile.getName
         require( n.startsWith( "i" ) && n.endsWith( ".png" ))
         val imageID = n.dropRight( 4 )
         val i = imageID.indexOf( '_' )
         val page = imageID.substring( 1, i ).toInt - 1
         val k = imageID.lastIndexOf( '_' )
         val j = imageID.lastIndexOf( '_', k - 1 )
         val spanStart = imageID.substring( j + 1, k ).toLong
         val spanStop  = imageID.substring( k + 1 ).toLong
         val m = imageID.indexOf( "_trk" ) + 4
         val p = imageID.indexOf( '_', m )
         val trackIdx = imageID.substring( m, p ).toInt - 1
         val q0 = imageID.indexOf( "_foff" )
         val q = q0 + 5
         val foff = if( q0 >= 0 ) imageID.substring( q, imageID.indexOf( '_', q )).toLong else 0L
         new Region( imageID, page, trackIdx, spanStart, spanStop, foff )
      }
   }
   case class Region( imageID: String, page: Int, trackIdx: Int, spanStart: Long, spanStop: Long, fileOffset: Long ) {
      var pred = Option.empty[ Region ]
      var succ = IndexedSeq.empty[ Region ]
//      var trackIdx = if( page == 0 ) 1 else 0  // XXX

//      def isSlice = pred.isDefined
   }

   case class Instruction( imageID: String, startTime: Double, stopTime: Double, startTrackIdx: Int, stopTrackIdx: Int,
                           startSpanStart: Double, stopSpanStart: Double, startSpanStop: Double, stopSpanStop: Double,
                           fadeIn: Double, fadeOut: Double, startGain: Double, stopGain: Double,
                           startTrackStart: Double, stopTrackStart: Double ) {

      def atEnd = copy( startTime = stopTime, startTrackIdx = stopTrackIdx,
                        startSpanStart = stopSpanStart, startSpanStop = stopSpanStop,
                        fadeIn = 0.0, startGain = stopGain, startTrackStart = stopTrackStart )

      def shiftSpan( delta: Double ) = copy( startTime = startTime + delta, stopTime = stopTime + delta )
   }

   object Recorder {
      def apply() : Recorder = new Impl

      private final class Impl extends Recorder {
         var timeOffset: Double = 0.0
         var stack = List.empty[ Instruction ]

         def appear( _imageID: String, gain: Double, trackIdx: Int, trackStart: Double, spanStart: Double, spanStop: Double, _fadeIn: Double ) {
            val dur = _fadeIn
            make1( _imageID, gain, trackIdx, trackStart, spanStart, spanStop, spanStop, _fadeIn, dur )
         }

         def unroll( _imageID: String, gain: Double, trackIdx: Int, trackStart: Double, spanStart: Double, spanStop: Double ) {
            val dur = spanStop - spanStart
            make1( _imageID, gain, trackIdx, trackStart, spanStart, spanStart, spanStop, 0.0, dur )
         }

         private def make1( _imageID: String, gain: Double, trackIdx: Int, trackStart: Double, spanStart: Double,
                            _startSpanStop: Double, _stopSpanStop: Double, _fadeIn: Double, dur: Double ) {
//            val dur = _stopSpanStop - spanStart
            require( dur > 0.0 )
            val in = Instruction(
               imageID        = _imageID,
               startTime      = timeOffset,
               stopTime       = timeOffset + dur,
               startTrackIdx  = trackIdx,
               stopTrackIdx   = trackIdx,
               startSpanStart = spanStart,
               stopSpanStart  = spanStart,
               startSpanStop  = _startSpanStop,
               stopSpanStop   = _stopSpanStop,
               fadeIn         = _fadeIn,
               fadeOut        = 0.0,
               startGain      = gain,
               stopGain       = gain,
               startTrackStart= trackStart,
               stopTrackStart = trackStart
            )
            stack       = in :: stack
            timeOffset  = in.stopTime
         }

         def advance( delta: Double ) {
            timeOffset += delta
            require( timeOffset >= 0.0 )
         }

         def prolong( delta: Double ) {
            require( delta >= 0.0 )
            stack match {
               case head :: tail =>
                  val in0 = head.atEnd
//                  val in = head.shiftSpan( delta )
                  val in = in0.copy( stopTime = in0.startTime + delta, fadeOut = 0.0 )
//                  require( in.startTime >= 0.0 )
                  stack = in :: stack // tail
                  timeOffset = in.stopTime
               case _ =>
            }
         }

         def branch( body: => Unit ) : Double = {
            val oldTimeOffset = timeOffset
            val oldSz = stack.size
            body
            val numAdded = stack.size - oldSz
            if( numAdded > 0 && oldSz > 0 ) {
               val i = stack.toIndexedSeq
               stack = (i( numAdded ) +: (i.take( numAdded ) ++ i.drop( numAdded + 1 ))).toList
            }

//            stack match {
//               case head :: tail => stack = tail; body; stack = head :: stack
//               case _ => body
//            }
            val res = timeOffset
            timeOffset = oldTimeOffset
            res
         }

         def dissolve( transitDur: Double ) {
            require( transitDur > 0.0 )
            stack match {
               case head :: _ =>
                  val in0     = head.atEnd
                  val in      = in0.copy( stopTime = in0.startTime + transitDur, fadeOut = transitDur )
                  stack       = in :: stack
                  timeOffset  = in.stopTime

               case _ => needInstruction()
            }
         }

         case class InstructionX( imageID: Int, startTime: Double, stopTime: Double, startTrackIdx: Int, stopTrackIdx: Int,
                                 startSpanStart: Double, stopSpanStart: Double, startSpanStop: Double, stopSpanStop: Double,
                                 fadeIn: Double, fadeOut: Double, startGain: Double, stopGain: Double,
                                 startTrackStart: Double, stopTrackStart: Double )

         def animate( transitDur: Double, deltaGain: Double, deltaTrackIdx: Int, deltaTrackStart: Double,
                   deltaSpanStart: Double, deltaSpanStop: Double ) {
            require( transitDur > 0.0 )
            stack match {
               case head :: _ =>
                  val in0  = head.atEnd
                  val in   = in0.copy( stopTime = in0.startTime + transitDur,
                                       stopTrackIdx = in0.startTrackIdx + deltaTrackIdx,
                                       stopSpanStart = in0.startSpanStart + deltaSpanStart,
                                       stopSpanStop = in0.startSpanStop + deltaSpanStop,
                                       fadeOut = 0.0,
                                       stopGain = in0.startGain + deltaGain,
                                       stopTrackStart = in0.startTrackStart + deltaTrackStart )
                  require( in.stopSpanStart < in.stopSpanStop )
                  stack = in :: stack
                  timeOffset = in.stopTime

               case _ => needInstruction()
            }
         }

         def crop( transitDur: Double, spanStart: Double, spanStop: Double ) {
            require( spanStop > spanStart )
            stack match {
               case head :: tail =>
                  val in0 = head.atEnd
//                  val oldTimeOffset = timeOffset
//                  dissolve( transitDur )
                  val in1: Instruction = if( transitDur == head.fadeOut ) head else {
                     head.copy( fadeOut = transitDur, stopTime = head.stopTime + (transitDur - head.fadeOut) )
                  }
                  val startTime = in1.stopTime - transitDur
                  val trackStart = in0.startTrackStart + spanStart - in0.stopSpanStart
                  val in2 = in0.copy( startTime = startTime, stopTime = startTime + transitDur,
                                      startSpanStart = spanStart, stopSpanStart = spanStart,
                                      startSpanStop = spanStop, stopSpanStop = spanStop,
                                      fadeIn = transitDur, fadeOut = 0.0,
                                      startTrackStart = trackStart, stopTrackStart = trackStart )
                  stack = in2 :: in1 :: tail
                  timeOffset = in2.stopTime

               case _ => needInstruction()
            }
         }

         @inline private def needInstruction() = sys.error( "No current instruction" )

         def build = {
            val res = stack.reverseIterator.toIndexedSeq
//            assert( res == res.sortBy( _.startTime ))
            res
         }
      }
   }

   trait Recorder {
      def appear( imageID: String, gain: Double, trackIdx: Int, trackStart: Double, spanStart: Double, spanStop: Double, fadeIn: Double )
      def unroll( imageID: String, gain: Double, trackIdx: Int, trackStart: Double, spanStart: Double, spanStop: Double )
      def branch( body: => Unit ) : Double
      def crop( transitDur: Double, spanStart: Double, spanStop: Double )
      def animate( transitDur: Double, deltaGain: Double = 0.0, deltaTrackIdx: Int = 0, deltaTrackStart: Double = 0.0,
                deltaSpanStart: Double = 0.0, deltaSpanStop: Double = 0.0 )
      def dissolve( transitDur: Double )
      def prolong( delta: Double )
      def advance( delta: Double )

      def build: IIdxSeq[ Instruction ]
   }


//   case class Unroll( presentationStart: Double, imageID: Int, trackIdx: Int, trackStart: Double ) {
//      def thenSlice( fadeDur: Double, slices: (Double, Double) ) : Seq[ Slice ]
//   }

   private var imageMap = Map.empty[ String, PImage ]

   def cachedImage( video: VideoLike, id: String ) : PImage = {
      imageMap.get( id ) match {
         case Some( img ) => img
         case _ =>
            val img = video.loadImage( new File( Video.dataFolder, /* "Sono_" + */ id + ".png" ).getPath )
            imageMap += id -> img
            img
      }
   }
}

class SonogramLayer( protected val video: VideoLike, instr: IIdxSeq[ SonogramLayer.Instruction ], val startTime: Double )
extends VideoLayer {
   import SonogramLayer._

//   lazy val img      = video.loadImage( new File( VideoLike.dataFolder, name + ".png" ).getPath )
   lazy val duration = instr.map( _.stopTime ).max // img.width.toDouble / pixelsPerSecond

   def stopTime = startTime + duration

   def timeToPix( secs: Double ) : Int = /* trackXOff + */ (secs * pixelsPerSecond + 0.5).toInt
   def trackIdxToPix( idx: Double ) : Int = trackYOff + (idx * trackHeight + 0.5).toInt

   def draw() {
      import video._

      val delta = now - startTime
      if( delta < 0.0 || delta >= duration ) return

      instr.filter( in => in.startTime <= delta && in.stopTime > delta ).foreach { in =>
         val img     = cachedImage( video, in.imageID )
         val dur     = in.stopTime - in.startTime
         val delta2  = delta - in.startTime
         val wStop   = math.max( 0.0, math.min( 1.0, delta2 / dur ))
         val wStart  = 1.0 - wStop

         val wStartSin = (1.0 - math.cos( wStart * math.Pi )) * 0.5
         val wStopSin  = (1.0 - math.cos( wStop  * math.Pi )) * 0.5

         val a1      = if( in.fadeIn > 0.0 ) (math.min( in.fadeIn, delta2 ) / in.fadeIn) else 1.0
         val a2      = if( in.fadeOut > 0.0 ) (math.min( in.fadeOut, dur - delta2 ) / in.fadeOut) else 1.0
//if( a2 < 1.0 ) println( "JUHU " + a2 )
         val alpha   = a1 * a2
         if( alpha < 1.0 ) tint( 1.0f, alpha.toFloat )

         val tx      = trackXOff + timeToPix( wStartSin * in.startTrackStart + wStopSin * in.stopTrackStart )
         val ty      = trackIdxToPix( wStartSin * in.startTrackIdx + wStopSin * in.stopTrackIdx )
         val ix      = timeToPix( wStart * in.startSpanStart + wStop * in.stopSpanStart )
         val iw      = math.min( img.width, timeToPix( wStart * in.startSpanStop + wStop * in.stopSpanStop )) - ix
         val h       = img.height

//         copy( img, ix, 0, iw, h, tx, ty, iw, h )
//         video.clip( tx, ty, iw, h )
//         image( img, tx - ix, ty )
//         video.noClip()

         if( ix != 0 || iw != img.width ) {
            val fuckYou = createImage( iw, h, PConstants.ARGB )
            fuckYou.copy( img, ix, 0, iw, h, 0, 0, iw, h )
            image( fuckYou, tx, ty )
            fuckYou.delete()
         } else {
            image( img, tx, ty )
         }

         noTint()
      }
//
////      image( img, offX, offY
//      val w = (delta / duration * img.width + 0.5).toInt
//      val h = img.height
//      copy( img, 0, 0, w, h, offX, offY, w, h )
   }
}