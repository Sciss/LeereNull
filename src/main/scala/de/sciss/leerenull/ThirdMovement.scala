/*
 *  ThirdMovement.scala
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

import de.sciss.processor.Processor.Aborted
import de.sciss.processor.{Processor, ProcessorFactory}
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.span.Span
import de.sciss.strugatzki.impl.MathUtil
import actors.Actor
import scala.util.{Failure, Success}
import de.sciss.strugatzki.{Strugatzki, FeatureCorrelation, FeatureSegmentation, FeatureExtraction}
import java.io.{FileInputStream, FileOutputStream, File}
import de.sciss.synth.io.AudioFile
import FeatureSegmentation.Break
import FeatureCorrelation.Match
import collection.immutable.{LongMap, IndexedSeq => Vec}
import java.awt.EventQueue
import xml.{XML, NodeSeq}

object ThirdMovement extends ProcessorFactory {
   type Product = Unit

  var verbose = false

   def folder        = new File( LeereNull.baseFolder, "third_move" )
   def featureFolder = new File( folder, "feature" )

   object Strategy {
      def apply( name: String ) : Strategy = name match {
         case Imitation.name   => Imitation
         case Ecology.name     => Ecology
         case NImitation.name  => NImitation
         case NEcology.name    => NEcology
      }

      /**
       * Imitation between all channels
       */
      case object Imitation  extends Strategy {
         val name = "imitation"
         val isImitative = true
         val isLocal = false
      }
      /**
       * Distinction between all channels
       */
      case object Ecology extends Strategy {
         val name = "ecology"
         val isImitative = false
         val isLocal = false
      }
      /**
       * Imitation between neighbouring channels
       */
      case object NImitation extends Strategy {
         val name = "n-imitation"
         val isImitative = true
         val isLocal = true
      }
      /**
       * Distinction between neighbouring channels
       */
      case object NEcology extends Strategy {
         val name = "n-ecology"
         val isImitative = false
         val isLocal = true
      }

      def seq = Seq[ Strategy ]( Imitation, Ecology, NImitation, NEcology )
   }
   sealed trait Strategy {
      def name: String
      def isImitative: Boolean
      final def isEcological: Boolean = !isImitative
      def isLocal: Boolean
      final def isGlobal: Boolean = !isLocal
   }

   sealed trait SettingsLike {
      def tlSpan: Span
      def layer: File
      def layerOffset: Long
      def materialFolder: File
      def numChannels: Int
      def strategy: Strategy
      def startDur: (Long, Long)
      def stopDur: (Long, Long)
      def startWeight: Float
      def stopWeight: Float
      def maxOverlap: Float
      def connectionWeight: Float
      def strategyWeight: Float
   }

   object SettingsBuilder {
      def apply() = new SettingsBuilder
   }
   final class SettingsBuilder private() extends SettingsLike {
      var tlSpan           = Span( 0, 441000L )
      var layer            = new File( "layer.aif" )
      var layerOffset      = 0L
      var materialFolder   = new File( "material" )
      var numChannels      = 4
      var strategy         = Strategy.Imitation: Strategy
      var startDur         = (66150L, 132300L)
      var stopDur          = (88400L, 176800L)
      var startWeight      = 0.75f
      var stopWeight       = 0.25f
      var maxOverlap       = 0.333f
      var connectionWeight = 0.5f
      var strategyWeight   = 0.5f

      def build : Settings = Settings(
         tlSpan, layer, layerOffset, materialFolder, numChannels, strategy, startDur, stopDur,
         startWeight, stopWeight, maxOverlap, connectionWeight, strategyWeight
      )

      def read( settings: Settings ): Unit = {
         tlSpan            = settings.tlSpan
         layer             = settings.layer
         layerOffset       = settings.layerOffset
         materialFolder    = settings.materialFolder
         numChannels       = settings.numChannels
         strategy          = settings.strategy
         startDur          = settings.startDur
         stopDur           = settings.stopDur
         startWeight       = settings.startWeight
         stopWeight        = settings.stopWeight
         maxOverlap        = settings.maxOverlap
         connectionWeight  = settings.connectionWeight
         strategyWeight    = settings.strategyWeight
      }
   }

   object Settings {
      implicit def fromBuilder( sb: SettingsBuilder ) : Settings = sb.build
      def fromXMLFile( file: File ) : Settings = fromXML( XML.loadFile( file ))
      def fromXML( xml: NodeSeq ) : Settings = {
         val sb               = SettingsBuilder()
         sb.tlSpan            = Span( (xml \ "tlSpan" \ "start").text.toLong, (xml \ "tlSpan" \ "stop").text.toLong )
         sb.layer             = new File( (xml \ "layer").text )
         sb.layerOffset       = (xml \ "layerOffset").text.toLong
         sb.materialFolder    = new File( (xml \ "materialFolder").text )
         sb.numChannels       = (xml \ "numChannels").text.toInt
         sb.strategy          = Strategy( (xml \ "strategy").text )
         sb.startDur          = ((xml \ "startDur" \ "min").text.toLong, (xml \ "startDur" \ "max").text.toLong)
         sb.stopDur           = ((xml \ "stopDur"  \ "min").text.toLong, (xml \ "stopDur"  \ "max").text.toLong)
         sb.startWeight       = (xml \ "startWeight").text.toFloat
         sb.stopWeight        = (xml \ "stopWeight").text.toFloat
         sb.maxOverlap        = (xml \ "maxOverlap").text.toFloat
         sb.connectionWeight  = (xml \ "connectionWeight").text.toFloat
         sb.strategyWeight    = (xml \ "strategyWeight").text.toFloat
         sb.build
      }
   }
   final case class Settings( tlSpan: Span, layer: File, layerOffset: Long,
                              materialFolder: File, numChannels: Int, strategy: Strategy,
                              startDur: (Long, Long), stopDur: (Long, Long),
                              startWeight: Float, stopWeight: Float, maxOverlap: Float,
                              connectionWeight: Float, strategyWeight: Float )
   extends SettingsLike {
      def toXML =
<ueberzeichnung>
   <tlSpan><start>{tlSpan.start}</start><stop>{tlSpan.stop}</stop></tlSpan>
   <layer>{layer.getPath}</layer>
   <layerOffset>{layerOffset}</layerOffset>
   <materialFolder>{materialFolder.getPath}</materialFolder>
   <numChannels>{numChannels}</numChannels>
   <strategy>{strategy.name}</strategy>
   <startDur><min>{startDur._1}</min><max>{startDur._2}</max></startDur>
   <stopDur><min>{stopDur._1}</min><max>{stopDur._2}</max></stopDur>
   <startWeight>{startWeight}</startWeight>
   <stopWeight>{stopWeight}</stopWeight>
   <maxOverlap>{maxOverlap}</maxOverlap>
   <connectionWeight>{connectionWeight}</connectionWeight>
   <strategyWeight>{strategyWeight}</strategyWeight>
</ueberzeichnung>
   }

  type Updater = Vec[(Long, Match)] => Unit

   /**
    * @param   settings the settings that control how the material generation is perfored
    * @param   updater  a function which is called _on the event dispatch thread_ each time a new
    *                   piece of material has been generated for each of the required channels
    *
    * @return  the generating process which must then be started using `start()`.
    */
//   def apply(settings: Settings, updater: Updater)(observer: Observer): ThirdMovement with Processor.Prepared =
//     new ThirdMovement(settings, observer, updater)

  case class Config(settings: Settings, updater: Updater)

  type Repr = ThirdMovement

  protected def prepare(config: Config): Prepared = new ThirdMovement(config.settings, config.updater)
}

class ThirdMovement private(settings: ThirdMovement.Settings,
                            updater: ThirdMovement.Updater)
  extends NullGoodies with ProcessorImpl[ThirdMovement.Product, ThirdMovement] {
   import ThirdMovement._

   protected val companion = ThirdMovement

   @volatile private var failure          = Option.empty[ Throwable ]
   @volatile private var success          = Option.empty[ AnyRef ]
   @volatile private var progressFactor   = 1.0f

   private val monitor = new AnyRef

  private def handleProcessOption[A](perc: Float, po: Option[Processor.Prepared]): Option[A] = {
    po match {
      case Some(p) =>
        Some(handleProcess[A](perc, p))

      case None =>
        checkAborted()
        progress = perc
        None
    }
  }

  private def handleProcess[A](perc: Float, p: Processor.Prepared): A = {
      failure        = None
      success        = None
      progressFactor = perc
      p.start()
      while( true ) {
         checkAborted()
         failure.foreach( throw _ )
         success match {
            case Some( res ) =>
//               if( verbose ) println( p.toString + " results " + res )
               return res.asInstanceOf[ A ]
            case None =>
               monitor.synchronized {
                  monitor.wait( 500 )
               }
         }
      }
      sys.error( "Never gets here" )
   }

   private def succeeded( res: AnyRef ): Unit = {
      success = Some( res )
      monitor.synchronized( monitor.notifyAll() )
   }

   private def failed( t: Throwable ): Unit = {
      failure = Some( t )
      monitor.synchronized( monitor.notifyAll() )
   }

  private def progressed(i: Int): Unit = {
    val p = i.toFloat / 100 * progressFactor
    progress = p
  }

  protected def body(): Product = {
    val (metaFile, extrOption) = metaFileForLayer(settings.layer)
    handleProcessOption[Unit](0.05f, extrOption)

      val tlStart             = settings.tlSpan.start
      val spanLen             = settings.tlSpan.length
      val layStart            = settings.layerOffset
      val layStop             = layStart + spanLen
      val numChannels         = settings.numChannels
      val extrIn              = FeatureExtraction.Config.fromXMLFile( metaFile )
      val stepSize            = extrIn.fftSize / extrIn.fftOverlap
      val numCoeffs           = extrIn.numCoeffs
      val imitativeStrategy   = settings.strategy.isImitative
      val localStrategy       = settings.strategy.isLocal

      def fullToFeat( n: Long ) = ((n + (stepSize >> 1)) / stepSize).toInt
//      def featToFull( i: Int )  = i.toLong * stepSize

//      val connSize            = fullToFeat( 44100L )
      val connTempW           = 0.5f   // XXX could be configurable
      val stratTempW          = 0.25f  // XXX could be configurable

      val segmCfg             = FeatureSegmentation.Config()
      segmCfg.corrLen         = 88200L // have one second on each side
      segmCfg.databaseFolder  = LeereNull.databaseFolder // hold the normalization data
      segmCfg.metaInput       = metaFile
      val minSpc              = math.min( settings.startDur._1, settings.stopDur._1 ) / 6
      segmCfg.minSpacing      = minSpc // 22050L // 44100L -- no, smaller because we want to use overlap eventually
      segmCfg.numBreaks       = (spanLen / segmCfg.minSpacing).toInt + 1
      segmCfg.span            = Span( layStart, layStop )
      segmCfg.temporalWeight  = 0.75f  // XXX could be configurable
      val segmCfgB            = segmCfg.build

      println( "\n:::::::::: Layer Segmentation ::::::::::\n" )
      println( segmCfgB.pretty )

      val segmProc = FeatureSegmentation( segmCfgB )
      segmProc.addListener {
         case Processor.Result(_, Success(_segm)) => succeeded( _segm )
         case prog @ Processor.Progress(_, _)     => progressed(prog.toInt)
         case Processor.Result(_, Failure(Aborted())) => abort()
         case Processor.Result(_, Failure( e ))   => failed( e )
      }

      val segms      = layStart +: handleProcess[ IndexedSeq[ Break ]]( 0.1f, segmProc ).map( _.pos ).sorted // XXX already sorted?
      val numSegm    = segms.size
      if( numSegm == 0 ) return

      if( verbose ) {
         println( "\n:::::::::: " + (if( numSegm <= 5 ) "All " else "First 5 of ") + numSegm + " segments ::::::::::\n" )
         segms.take( 5 ).foreach( m => println( m ))
      }

      var lastSpan      = Span( layStart, layStart )
      var lastSegmLen   = 0L
      var lastStartIdx  = 0
      var lastStopIdx   = 0
      val rnd           = new util.Random()
      // tracks the matches per channel
      var lastMatch     = Option.empty[ Vec[ Match ]]
      val gagaDur       = (settings.startDur._1 + settings.startDur._2 + settings.stopDur._1 + settings.stopDur._2) / 4
      var sameStartIdx  = 0
      while( lastSpan.stop < layStop ) {
         val maxOvl  = (settings.maxOverlap.toDouble * lastSegmLen + 0.5).toLong
         var idx     = lastStopIdx
         while( (idx > 0 &&) ((lastSpan.stop - segms( idx )) <= maxOvl) ) idx -= 1
//         val startIdx = math.min( lastIdx, idx + 1 )
         val startIdx = {
            val res = math.max( lastStartIdx, idx + 1 )
            if( res == lastStartIdx ) {
               sameStartIdx += 1
               if( sameStartIdx == 3 ) {
                  sameStartIdx = 0
                  res + 1
               } else res
            } else res
         }
         val startPos = segms( startIdx )

//         val w       = math.max( 0.0, math.min( 1.0, ((minStop + maxStop) / 2 - startPos).toDouble / spanLen ))
         val w       = math.max( 0.0, math.min( 1.0, (gagaDur + startPos - layStart).toDouble / spanLen ))

         val minDur  = ((settings.startDur._1 * (1 - w)) + (settings.stopDur._1 * w) + 0.5).toLong
         val maxDur  = ((settings.startDur._2 * (1 - w)) + (settings.stopDur._2 * w) + 0.5).toLong
         val temp    = ((settings.startWeight * (1 - w)) + (settings.stopWeight * w)).toFloat

         val minStop = startPos + minDur
         val maxStop = startPos + maxDur

         idx = startIdx + 1; while( idx < numSegm && (segms( idx ) <= minStop )) idx += 1
         val minIdx  = math.max( startIdx + 1, idx - 1 )
         idx = minIdx + 1; while( (idx < numSegm) && (segms( idx ) <= maxStop )) idx += 1
         val maxIdx  = idx - 1

         var chunkOk = false

         if( minIdx <= maxIdx ) {
            val stopIdx    = minIdx + rnd.nextInt( maxIdx - minIdx + 1 )
//            val plainSpan  = Span( segms( startIdx ), segms( stopIdx ))
//            val layerSpan  = Span( plainSpan.start + settings.layerOffset, plainSpan.stop + settings.layerOffset )
            val layerSpan  = Span( segms( startIdx ), segms( stopIdx ))

            val corrCfg    = FeatureCorrelation.Config()
            corrCfg.databaseFolder = settings.materialFolder
            val normFile   = new File( settings.materialFolder, Strugatzki.NormalizeName )
            if( !normFile.exists() ) {
               val sourceFile = new File( LeereNull.databaseFolder, Strugatzki.NormalizeName)
               copyFile( sourceFile, normFile )
            }
            corrCfg.maxBoost     = 20  // +26 dB
            corrCfg.minPunch     = layerSpan.length   // XXX is this actually used when punchOut == None?
            corrCfg.maxPunch     = layerSpan.length   // XXX is this actually used when punchOut == None?
            corrCfg.metaInput    = metaFile
            corrCfg.minSpacing   = 4410L  // 100 ms
//            corrCfg.numMatches   = math.min( 4096, numChannels * numChannels * 100 )
            corrCfg.numMatches   = math.min( 1024, numChannels * 100 )
            corrCfg.numPerFile   = corrCfg.numMatches
            corrCfg.punchIn      = FeatureCorrelation.Punch( layerSpan, temp )
            val corrCfgB = corrCfg.build

            if( verbose ) {
               println( "\n:::::::::: Basic Correlation ::::::::::\n" )
               println( corrCfgB.pretty )
            }

            val corrProc = FeatureCorrelation( corrCfgB )
            corrProc.addListener {
               case Processor.Result(_, Success( _segm ))  => succeeded( _segm )
               case prog @ Processor.Progress(_, _)     => progressed(prog.toInt)
               case Processor.Result(_, Failure(Aborted())) => abort()
               case Processor.Result(_, Failure( e ))      => failed( e )
            }

            val perc    = (0.9 * w + 0.1).toFloat
            val corrs   = handleProcess[ IndexedSeq[ Match ]]( perc, corrProc ).filterNot( _.sim.isNaN )
            val numMatches = corrs.size

            if( verbose ) {
               println( "\n:::::::::: " + (if( numMatches <= 5 ) "All " else "First 5 of ") + numMatches + " matches ::::::::::\n" )
               corrs.take( 5 ).foreach( m => println( m.pretty + "\n" ))
            }

            // account for connectivity
            val w1      = lastMatch match {
               case Some( lms ) if settings.connectionWeight > 0f =>
                  corrs.map { nm =>
                     val nmFeat     = featureFile( plainName( nm.file ), featureFolder )
                     val nextAF     = AudioFile.openRead( nmFeat )

                     val res = Vec.tabulate( numChannels ) { ch =>
                        val lm         = lms( ch )

                        val connFull   = math.min( nm.punch.length, lm.punch.length )
                        val lmFeat     = featureFile( plainName( lm.file ), featureFolder )
                        val nStop0     = fullToFeat( nm.punch.start + connFull )
                        val nStart     = fullToFeat( nm.punch.start )
                        val lStop      = fullToFeat( lm.punch.stop )
                        val lStart0    = fullToFeat( lm.punch.stop - connFull )
                        val numF       = math.min( nStop0 - nStart, lStop - lStart0 )
//                        val nStop      = nStart + numF
                        val lStart     = lStop - numF
                        val lastAF     = AudioFile.openRead( lmFeat )
                        require( lastAF.numChannels == numCoeffs + 1 )
                        require( nextAF.numChannels == numCoeffs + 1 )
                        val lBufT      = lastAF.buffer( numF )
                        val lBufS      = lBufT.drop(1)
                        val nBufT      = nextAF.buffer( numF )
                        val nBufS      = nBufT.drop(1)
                        lastAF.seek( lStart )
                        nextAF.seek( nStart )
                        lastAF.read( lBufT )
                        nextAF.read( nBufT )

                        // XXX to-do: apply boosts?

                        lastAF.close()
                        val (lMeanT, lStdDevT) = MathUtil.stat( lBufT, 0, numF, 0, 1 )
                        val (lMeanS, lStdDevS) = MathUtil.stat( lBufS, 0, numF, 0, numCoeffs )
                        val (nMeanT, nStdDevT) = MathUtil.stat( nBufT, 0, numF, 0, 1 )
                        val (nMeanS, nStdDevS) = MathUtil.stat( nBufS, 0, numF, 0, numCoeffs )
                                                
                        var maxCorr = 0f
                        var off = 0; while( off < numF ) {
                           val tempCorr = if( connTempW > 0f ) {
                              MathUtil.correlate( lBufT, lMeanT, lStdDevT, numF, 1, nBufT, nMeanT, nStdDevT, off, 0 )
                           } else 0f

                           val specCorr = if( connTempW < 1f ) {
                             MathUtil.correlate( lBufS, lMeanS, lStdDevS, numF, numCoeffs, nBufS, nMeanS, nStdDevS, off, 0 )
                           } else 0f

                           val connCorr = (tempCorr * connTempW) + (specCorr * (1 - connTempW))
                           if( connCorr > maxCorr ) maxCorr = connCorr

                        off += 1 }  // XXX if too slow, we can increase the step size

                        (nm.sim * (1 - settings.connectionWeight)) + (maxCorr * settings.connectionWeight)
                     }

                     nextAF.close()
                     res
                  }
               case _ => corrs.map { nm => Vec.fill( numChannels )( nm.sim )}
            }

            // account for strategy

            // with w1 now:
            //
            // m1    m2 ...
            // --------
            // c11  c21 ...
            // c21  c22 ...
            // ...  ...
            //

            // for w2
            //
            // c11 -> mean( max( xcorr( c2... )), max( xcorr( c3... )), ... )
            // while in each sub-step first calculating the 'best possible outcome' and stop
            // if that's below the best match so far.

            // c11 x c22 x c33 x c44
            // c11 x c22 x c33 x c45...c4x
            // c11 x c23 x c34 x c43
            // c11 x c23 x c34 x c45...c4x

            if( (settings.strategyWeight > 0f) && (numMatches > numChannels) ) {
               var bestCorr   = 0.0
               var bestSeq    = Vec.empty[ Int ]
               var xMap       = LongMap.empty[ Float ]

               def weightFun( values: Vec[ Float ]) : Float = {
                  // we could change this to give extra penalty
                  // to particularly low values. for now, just
                  // the average will do.
                  values.sum / values.size
               }

               def xCalc( a: Int, b: Int ) : Float = {
                  val ma      = corrs( a )
                  val mb      = corrs( b )
                  val aFeat   = featureFile( plainName( ma.file ), featureFolder )
                  val bFeat   = featureFile( plainName( mb.file ), featureFolder )
                  val aAF     = AudioFile.openRead( aFeat )
                  val bAF     = AudioFile.openRead( bFeat )
                  val aStop0  = fullToFeat( ma.punch.stop )
                  val bStop0  = fullToFeat( mb.punch.stop )
                  val aStart  = fullToFeat( ma.punch.start )
                  val bStart  = fullToFeat( mb.punch.start )
                  val numF    = math.min( aStop0 - aStart, bStop0 - bStart )
                  if( numF == 0 ) return 0f
                  val bufSize = math.min( 4096, numF )
                  val aBufT   = aAF.buffer( bufSize )
                  val bBufT   = bAF.buffer( bufSize )
                  val aBufS   = aBufT.drop( 1 )
                  val bBufS   = bBufT.drop( 1 )
                  var remain  = numF
                  var c       = 0.0
                  aAF.seek( aStart )
                  bAF.seek( bStart )
                  while( remain > 0 ) {
                     val chunk = math.min( remain, bufSize )
                     aAF.read( aBufT, 0, chunk )
                     bAF.read( bBufT, 0, chunk )

                     // XXX to-do: apply boosts?

                     val (aMeanT, aStdDevT) = MathUtil.stat(aBufT, 0, chunk, 0, 1 )
                     val (aMeanS, aStdDevS) = MathUtil.stat( aBufS, 0, chunk, 0, numCoeffs )
                     val (bMeanT, bStdDevT) = MathUtil.stat( bBufT, 0, chunk, 0, 1 )
                     val (bMeanS, bStdDevS) = MathUtil.stat( bBufS, 0, chunk, 0, numCoeffs )
                     val tempCorr = if( stratTempW > 0f ) {
                       MathUtil.correlate( aBufT, aMeanT, aStdDevT, chunk, 1, bBufT, bMeanT, bStdDevT, 0, 0 )
                     } else 0f
                     val specCorr = if( stratTempW < 1f ) {
                       MathUtil.correlate( aBufS, aMeanS, aStdDevS, chunk, numCoeffs, bBufS, bMeanS, bStdDevS, 0, 0 )
                     } else 0f
                     val stratCorr0 = (tempCorr * stratTempW) + (specCorr * (1 - stratTempW))
                     val stratCorr  = if( imitativeStrategy ) stratCorr0 else 1f - stratCorr0
                     c += stratCorr * chunk
                     remain -= chunk
                  }
                  aAF.close()
                  bAF.close()
                  (c / numF).toFloat
               }

               def xChanCorr( aIdx: Int, bIdx: Int ) : Float = {
                  require( aIdx != bIdx )
                  val i    = math.min( aIdx, bIdx )
                  val j    = math.max( aIdx, bIdx )
                  val key  = (i.toLong << 32) | j
                  xMap.get( key ) match {
                     case Some( value ) => value
                     case None =>
                        val value0  = xCalc( i, j )
                        val value   = if( value0.isNaN ) 0f else value0
                        xMap += ((key, value))
                        value
                  }
               }

               val stratW     = settings.strategyWeight

               // "The number of connections in a full mesh = n(n - 1) / 2."
               // versus Ring: numChannels
               val xTotalNum  = if( localStrategy ) numChannels else {
                  numChannels * (numChannels + 1) / 2
               }  // number of cross correlations between channels

               def bestPrognosis( baseDone: Vec[ Float ], xDone: Vec[ Float ]) : Float = {
                  val chansMissing  = numChannels - baseDone.size
                  val xMissing      = xTotalNum - xDone.size
                  val baseValues    = baseDone ++ Vec.fill( chansMissing )( 1f )
                  val xValues       = xDone ++ Vec.fill( xMissing )( 1f )
                  weightFun( baseValues ) * (1f - stratW) + weightFun( xValues ) * stratW
               }

               var progDone = 0
               val progDoneNum = numMatches * numChannels // numMatches

               def recurse( taken: Vec[ Int ], baseDone: Vec[ Float ], xDone: Vec[ Float ], numDone: Int ): Unit = {
                  val chan          = taken.size
                  require( chan == baseDone.size )
                  checkAborted()

                  var i = 0; while( i < numMatches ) {
                     val numDone1 = numDone + (i + 1)
                     if( verbose ) {
                        val progDone1  = numDone1 * 10 / progDoneNum
                        while( progDone < progDone1 ) {
                           progDone += 1
                           println( progDone )
                        }
                     }

                     if( !taken.contains( i )) {
                        val base       = w1( i )( chan )
                        val baseDone1  = baseDone :+ base
                        if( bestPrognosis( baseDone1, xDone ) > bestCorr ) {
                           var xDone1  = xDone
                           var prog    = 0f
                           var ok      = true
                           var k = if( localStrategy ) taken.size - 1 else 0
                           while( k < taken.size && ok ) {
                              val x = xChanCorr( taken( k ), i )
                              xDone1 :+= x
                              prog = bestPrognosis( baseDone1, xDone1 )
                              ok = prog > bestCorr
                           k += 1 }

                           if( ok ) {
                              val taken1 = taken :+ i
                              if( taken1.size == numChannels ) {
                                 bestCorr = prog   // not a prognosis any more
                                 bestSeq  = taken1
                              } else {
                                 // go into next recursion...
                                 recurse( taken1, baseDone1, xDone1, numDone1 )
                              }
                           }
                        }
                     }
                  i += 1 }
               }

               if( verbose ) {
                  val numCombi = if( localStrategy ) {
                     numMatches.toLong * numChannels - (2 * numChannels)
                  } else {
                     // XXX correct?
                     var prod = 1L; var i = 0; while( i < numChannels ) { prod *= numMatches - i; i += 1 }
                     prod
                  }
                  println( "\nFinding best combination... (out of " + numCombi + ")" )
               }

               val xDone0 = Vec.empty
               var j = 0; while( j < numMatches ) {
                  val base = w1( j )( 0 )
                  val baseDone0 = Vec( base )
                  if( bestPrognosis( baseDone0, xDone0 ) > bestCorr ) {
                     recurse( Vec( j ), baseDone0, xDone0, j + 1 )
                  }
               j += 1 }

               val w2 = bestSeq

               if( verbose ) {
                  println( "\nResult : " + bestSeq )
               }

               val w3 = w2.map( corrs( _ ))

               lastSegmLen    = w3.map( _.punch.length ).min
               lastSpan       = layerSpan
               lastStartIdx   = startIdx
               lastStopIdx    = stopIdx
               lastMatch      = Some( w3 )   // without the adjustments?

               // now adjust matches according to segmentation bounds in the match
               val basicOffset = tlStart + (layerSpan.start - layStart)
               val w4 = w3 map { m =>
   //               val mFeat               = featureFile( plainName( m.file ), folder )
                  val mMeta               = extrMetaFile( plainName( m.file ), featureFolder )
                  val mSegCfg             = FeatureSegmentation.Config()
                  mSegCfg.corrLen         = 44100L // have 0.5 seconds on each side
                  mSegCfg.databaseFolder  = LeereNull.databaseFolder // hold the normalization data
                  mSegCfg.metaInput       = mMeta
                  mSegCfg.minSpacing      = 0L
                  mSegCfg.numBreaks       = 1
                  mSegCfg.temporalWeight  = 0.75f  // XXX could be configurable

                  def findAdjust( span: Span ) : Option[ Long ] = {
                     mSegCfg.span         = span
                     val mSegProc         = FeatureSegmentation( mSegCfg )
                     mSegProc.addListener {
                        case Processor.Result(_, Success( _segm )) => succeeded( _segm )
                        case prog @ Processor.Progress(_, _)    => progressed(prog.toInt)
                        case Processor.Result(_, Failure(Aborted())) => abort()
                        case Processor.Result(_, Failure(e))     => failed( e )
                     }
                     handleProcess[ IndexedSeq[ Break ]]( perc, mSegProc ).map( _.pos ).headOption
                  }

                  val mSegStartStart   = math.max( 0L, m.punch.start - 66150L )
                  val mSegStartStop    = math.min( (m.punch.start + m.punch.stop) / 2, m.punch.start + 44100L ) + 22050L
                  val mStart0          = findAdjust( Span( mSegStartStart, mSegStartStop )).getOrElse( m.punch.start )

                  val mSegStopStart    = math.max( 0L, math.max( (m.punch.start + m.punch.stop) / 2, m.punch.stop - 44100L ) - 22050L )
                  val mFileLen         = AudioFile.readSpec( m.file ).numFrames
                  val mSegStopStop     = math.min( mFileLen, m.punch.stop + 66150L )
                  val mStop            = findAdjust( Span( mSegStopStart, mSegStopStop )).getOrElse( m.punch.stop )

                  val actualOffset0    = basicOffset + mStart0 - m.punch.start
                  val (actualOffset, mStart) = if( actualOffset0 >= 0 ) (actualOffset0, mStart0) else {
                     (0L, mStart0 - actualOffset0)
                  }

                  val mAdjusted = m.copy( punch = Span( mStart, mStop ))

                  (actualOffset, mAdjusted)
               }

               chunkOk = true
               defer { updater( w4 )}
            }
         }

         if( !chunkOk ) {
            lastSegmLen = 4410
//            lastSpan    = Span( lastSpan.stop, lastSpan.stop + lastSegmLen )
            lastStartIdx += 1
            lastStopIdx  += 1
//            lastSpan     = if( lastStartIdx < numSegm ) segms( lastStartIdx ) else spanLen
            lastSpan    = Span( if( lastStartIdx < numSegm ) segms( lastStartIdx ) else layStop,
                                if( lastStopIdx  < numSegm ) segms( lastStopIdx )  else layStop )
            lastMatch   = None
         }
      }
   }

   private def defer( thunk: => Unit ): Unit =
      EventQueue.invokeLater( new Runnable { def run(): Unit = thunk })

   private def copyFile( source: File, dest: File ): Unit = {
      val sourceCh   = new FileInputStream( source ).getChannel
      val destCh     = new FileOutputStream( dest ).getChannel
      destCh.transferFrom( sourceCh, 0, sourceCh.size() )
   }

  private def metaFileForLayer(layer: File): (File, Option[FeatureExtraction with Processor.Prepared]) = {
      val metaFile = extrMetaFile( plainName( layer ), featureFolder )
      if( metaFile.exists() ) {
         (metaFile, None)
      } else {
         val metaDir = metaFile.getParentFile
         if( !metaDir.exists() ) metaDir.mkdirs()
         val extrCfg = FeatureExtraction.Config()
         extrCfg.audioInput     = layer
         val ff                  = featureFile( plainName( layer ), featureFolder )
         extrCfg.featureOutput  = ff
         extrCfg.metaOutput     = Some( metaFile )
//         settings.numCoeffs      = default
//         settings.fftSize        = default
//         settings.fftOverlap     = default
        val proc = FeatureExtraction(extrCfg)
        proc.addListener {
            case Processor.Result(_, Success(_))    => succeeded( ().asInstanceOf[ AnyRef ])
            case prog @ Processor.Progress(_, _)   => progressed(prog.toInt)
            case Processor.Result(_, Failure(Aborted())) => abort()
            case Processor.Result(_, Failure( e ))    => failed( e )
         }
//         proc.start()
         (metaFile, Some( proc ))
      }
   }

  //  protected val Act = new Actor {
  //    def act(): Unit = {
  //      ProcT.start()
  //      var result: Result = null
  //      loopWhile(result == null) {
  //        react {
  //          case Abort =>
  //            ProcT.aborted = true
  //            aborted()
  //          case res: Progress =>
  //            observer(res)
  //          case res@Aborted =>
  //            result = res
  //          case res: Failure =>
  //            result = res
  //          case res: Success =>
  //            result = res
  //        }
  //      } andThen {
  //        observer(result)
  //      }
  //    }
  //  }
}