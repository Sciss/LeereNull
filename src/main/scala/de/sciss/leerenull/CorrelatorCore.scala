/*
 *  CorrelatorCore.scala
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

import de.sciss.leerenull.CorrelatorSelector.Search
import eu.flierl.grouppanel.GroupPanel
import de.sciss.strugatzki.{FeatureCorrelation, Span, FeatureExtraction}
import FeatureCorrelation.{Match, SettingsBuilder => CSettingsBuilder}
import FeatureExtraction.{Settings => ESettings}
import de.sciss.app.AbstractCompoundEdit
import de.sciss.kontur.session.{MatrixDiffusion, AudioTrack, AudioFileElement, FadeSpec, AudioRegion, Session, BasicTimeline}
import java.io.File
import de.sciss.synth.io.AudioFile
import xml.{Node, NodeSeq}

object CorrelatorCore extends GUIGoodies with KonturGoodies with NullGoodies {
   object Transform {
      def fromXML( n: NodeSeq ) : Transform = {
         (n \ "type").text match {
            case "shift"      => TransformShift.fromXML( n )
            case "resample"   => TransformResample.fromXML( n )
            case ""           => TransformNone
            case "none"       => TransformNone
         }
      }
   }
   sealed trait Transform {
      def fscapeOption: Option[ (File, File) => (Boolean => Unit) => Unit ]
      def fileID: String
      def timeScale : Double
      def inverse: Transform
      final def toXML : Node = <transform>{toInnerXML}</transform>
      protected def toInnerXML : NodeSeq
   }
   case object TransformNone extends Transform {
      def fscapeOption = None
      def inverse = this
      def fileID = ""
      def timeScale = 1.0
      protected def toInnerXML = <type>none</type>
   }
   object TransformShift {
      def fromXML( n: NodeSeq ) : TransformShift = {
         TransformShift( (n \ "amount").text.toDouble )
      }
   }
   /** @param amount shift amount in Hertz */
   final case class TransformShift( amount: Double ) extends Transform {
      def fileID = "_Hlb" + amount.toInt
      def inverse = copy( amount = -amount )
      def fscapeOption = Some( FScape.shift( amount ) _ )
      def timeScale = 1.0
      protected def toInnerXML = <type>shift</type><amount>{amount}</amount>
   }
   object TransformResample {
      def fromXML( n: NodeSeq ) : TransformResample = {
         TransformResample( (n \ "amount").text.toDouble )
      }
   }
   /** @param amount resampling amount in semintone cents */
   final case class TransformResample( amount: Double ) extends Transform {
      def fileID = "_Rsmp" + amount.toInt
      def inverse = copy( amount = -amount )
      def fscapeOption = Some( FScape.resample( amount ) _ )
      def timeScale = math.pow( 2, amount / -1200 )
      protected def toInnerXML = <type>resample</type><amount>{amount}</amount>
   }

   def makeMatchEditor( search: Search, idx: Int )( implicit doc: Session ) {
      search.transform.inverse.fscapeOption match {
         case Some( fsc ) =>
            val m          = search.matches( idx )
            val spec       = AudioFile.readSpec( m.file )
            val wholeSpan  = Span( 0L, spec.numFrames )
            val truncSpan  = Span( math.max( wholeSpan.start, m.punch.start - 176400L ), math.min( wholeSpan.stop, m.punch.stop + 176400L ))
            val trunc      = truncSpan != wholeSpan
            val fName      = plainName( m.file ) + (if( trunc ) "_" + m.punch.start + "_" + m.punch.stop else "") +
               search.transform.fileID + ".aif"
            val fTrns      = new File( LeereNull.bounceFolder, fName )

            def trnsDone() {
               val m2      = m.copy( file = fTrns, punch = m.punch.shift( -truncSpan.start ))
               val s2      = search.copy( matches = search.matches.patch( idx, IndexedSeq( m2 ), 1 ))
               makeMatchEditor2( s2, idx )
            }

            def runTrns( f: File ) {
//               FScape.shift( f, fTrns, -freq ) { b =>
//                  if( b ) trnsDone()
//               }
               fsc( f, fTrns )( if( _ ) trnsDone() )
            }

            if( fTrns.isFile ) trnsDone() else {
               if( trunc ) {
                  val truncFile = File.createTempFile( "trunc", ".aif" )
                  truncFile.deleteOnExit()
                  val dlg = progressDialog( "Extracting span" )
                  val cutter = AudioFileCutter( m.file, truncFile, truncSpan ) {
                     case AudioFileCutter.Success =>
                        dlg.stop()
                        runTrns( truncFile )
                     case AudioFileCutter.Failure( e ) =>
                        dlg.stop()
                        e.printStackTrace()
                     case AudioFileCutter.Progress( i ) => dlg.progress = i
                  }
                  dlg.start( cutter )

               } else runTrns( m.file )
            }

         case None => makeMatchEditor2( search, idx )
      }
   }

   private def makeMatchEditor2( search: Search, idx: Int )( implicit doc: Session ) {
      val tls     = doc.timelines
      val set     = search.settings
//      val itrns   = search.transform.inverse

      def frames( afe: AudioFileElement, secs: Double ) = (secs * afe.sampleRate + 0.5).toLong
      def fromInputRate( sp: Span ) =
         Span( (sp.start / search.transform.timeScale + 0.5).toLong,
               (sp.stop  / search.transform.timeScale + 0.5).toLong )

      val m       = search.matches( idx )
      val mPunch  = fromInputRate( m.punch )
      val meta    = ESettings.fromXMLFile( set.metaInput )

      var ar1Off     = -1L
      var incorpOff  = -1L
      var splitPos   = -1L
      var afe1 : AudioFileElement = null

      val vowels  = "aeiouAEIOU".toSet
      def regionName( id: String, afe: AudioFileElement, pre: String = "$" ) = {
         val n1      = plainName( afe.path ) // .filter( _.isLetterOrDigit )
         var n2      = n1; while( n2.size > 20 ) {
            val i = n2.indexWhere( vowels.contains( _ ))
            if( i >= 0 ) n2 = n2.substring( 0, i ) + n2.substring( i + 1 )
            else n2 = n2.take( 20 )
         }
         pre + n2 + "_" + id
      }

      implicit val tl = tls.tryEdit[ BasicTimeline ]( "Add Matcher Timeline" ) { ce0: AbstractCompoundEdit =>
         implicit val ce = ce0   // all because IDEA sucks
         afe1    = provideAudioFile( meta.audioInput )

         var arsStereo  = IndexedSeq.empty[ AudioRegion ]
         var arsLeft    = IndexedSeq.empty[ AudioRegion ]
         var arsRight   = IndexedSeq.empty[ AudioRegion ]

         val pi      = set.punchIn
         val piSpan  = fromInputRate( pi.span )
         val fOff1   = math.max( 0L, piSpan.start - frames( afe1, 10 ))
         val start1  = 0L
         val pre1    = piSpan.start - fOff1
         val maxLen1 = set.punchOut.map( p => fromInputRate( p.span ).start ).getOrElse( afe1.numFrames ) - fOff1
         val len1    = math.min( maxLen1, piSpan.stop - fOff1 + frames( afe1, 1 ))
         val stop1   = start1 + len1
         val fadeIn1 = FadeSpec( math.min( pre1, frames( afe1, 1 )))
         val fadeOut1= FadeSpec( math.min( len1 - pre1, frames( afe1, 1 )))
         val ar1     = AudioRegion( Span( start1, stop1 ), regionName( "pre", afe1, pre = "" ), afe1, fOff1,
            fadeIn = Some( fadeIn1 ), fadeOut = Some( fadeOut1 ))
         arsStereo :+= ar1

         ar1Off      = start1 - fOff1
         incorpOff   = search.offset - ar1Off
         splitPos    = ar1.span.stop + incorpOff

         set.punchOut.foreach { po =>
            val poSpan     = fromInputRate( po.span )
            val fOff2      = math.max( fOff1 + len1, poSpan.start - frames( afe1, 1 ))
            val pre2       = poSpan.start - fOff2
            // start1 + pre1 = timeline spot where punch begins
            val start2     = start1 + pre1 + mPunch.length - pre2
            val maxLen2    = afe1.numFrames - fOff2
            val len2       = math.min( maxLen2, pre2 + poSpan.length + frames( afe1, 10 ))
            val stop2      = start2 + len2
            val fadeIn2    = FadeSpec( math.min( pre2, frames( afe1, 1 )))
            val fadeOut2   = FadeSpec( math.min( len2 - pre2, frames( afe1, 1 )))
            val ar2        = AudioRegion( Span( start2, stop2 ), regionName( "post", afe1, pre = "" ), afe1, fOff2,
               fadeIn = Some( fadeIn2 ), fadeOut = Some( fadeOut2 ))
            arsStereo :+= ar2
         }

         def matchRegions( m: Match, pre: String ) : IndexedSeq[ AudioRegion ] = {
            val afe2    = provideAudioFile( m.file )
            val fOff3   = mPunch.start
            val start3  = start1 + pre1
            val maxLen3 = afe2.numFrames - fOff3
            // merge if gain difference is less than 6 dB
            val split3  = set.punchOut.isDefined && (math.max( m.boostOut, m.boostIn ) / math.min( m.boostOut, m.boostIn )) > 2
            val boost3  = if( !split3 && set.punchOut.isDefined ) math.sqrt( m.boostOut * m.boostIn ).toFloat else m.boostIn
            val fdt3    = mPunch.length / 4
            val len3    = if( split3 ) (mPunch.length / 2) + fdt3 else math.min( maxLen3, mPunch.length + frames( afe2, 1 ))
            val stop3   = start3 + len3
            val fade3   = FadeSpec( fdt3 )
            val arStart = AudioRegion( Span( start3, stop3 ), regionName( if( split3 ) "pin" else "punch", afe2, pre = pre ),
               afe2, fOff3,
               gain = boost3, fadeIn = Some( fade3 ), fadeOut = Some( fade3 ))
//println( "ar3 : " + ar3 + " ; OFFSET = " + fOff3 + " ; m.punch = " + m.punch )

            set.punchOut match {
               case Some( po ) if( split3 ) =>
                  val start4  = stop3 - fdt3
                  val fOff4   = fOff3 + start4 - start3
                  val len4    = math.min( afe2.numFrames - fOff4, mPunch.length - (start4 - start3) + frames( afe2, 1 ))
                  val stop4   = start4 + len4
                  val fade4   = FadeSpec( fdt3 )
                  val boost4  = m.boostOut
                  val arStop  = AudioRegion( Span( start4, stop4 ), regionName( "pout", afe2, pre = pre ), afe2, fOff4,
                     gain = boost4, fadeIn = Some( fade4 ), fadeOut = Some( fade4 ))
                  IndexedSeq( arStart, arStop )

               case _ => IndexedSeq( arStart )
            }
         }

         search.master match {
            case Some( m2 ) =>
               arsLeft  ++= matchRegions( m,  "$L_" )
               arsRight ++= matchRegions( m2, "$R_" )

            case None =>
               arsStereo ++= matchRegions( m, "$_" )
         }

         implicit val tl = BasicTimeline.newEmpty( doc )
         tl.span  = Span( 0L, (arsStereo ++ arsLeft ++ arsRight).map( _.span.stop ).max )
         tl.name  = uniqueName( tls, "$Matcher" )
         tls.editInsert( ce, tls.size, tl )
         var trackMap = Map.empty[ AudioTrack, IndexedSeq[ AudioRegion ]]
         arsStereo.foreach { ar =>
            val tr = placeStereo( ar, diffPrefix = "$", more = trackMap )
            trackMap += tr -> (trackMap.getOrElse( tr, IndexedSeq.empty ) :+ ar)
         }
         arsLeft.foreach { ar =>
            val tr = placeLeft( ar, diffPrefix = "$", more = trackMap )
            trackMap += tr -> (trackMap.getOrElse( tr, IndexedSeq.empty ) :+ ar)
         }
         arsRight.foreach { ar =>
            val tr = placeRight( ar, diffPrefix = "$", more = trackMap )
            trackMap += tr -> (trackMap.getOrElse( tr, IndexedSeq.empty ) :+ ar)
         }
         tl
      }

      val tlf = TimelineFrame2 { f =>
//         println( "Bye..." )
//         f.dispose()
      }

      val butFlipChans = button( "Flip left/right" ) { b =>
         val (trL, trR) = tl.tracks.toList.collect({
            case at: AudioTrack if( at.name.contains( "-L" ) || at.name.contains( "-R" )) => (at, at.diffusion)
         }).collect({
            case (at, Some( m: MatrixDiffusion )) => (at, m)
         }).partition( _._1.name.contains( "-L" ))

         if( trL.nonEmpty || trR.nonEmpty ) tls.joinEdit( "Flip chans" ) { implicit ce =>
            def gugu( at: AudioTrack, m: MatrixDiffusion, in: String, out: String, diff: Int => MatrixDiffusion ) {
               val nameOld = at.name
               val i       = nameOld.indexOf( "-" + in )
               val nameNew = nameOld.substring( 0, i + 1 ) + out + nameOld.substring( i + 2 )
               at.editRename( ce, nameNew )
               val d       = diff( m.numInputChannels )
               at.editDiffusion( ce, Some( d ))
            }

            trL.foreach {
               case (at, m) => gugu( at, m, "L", "R", provideRightDiffusion( diffPrefix = "$" ) _ )
            }
            trR.foreach {
               case (at, m) => gugu( at, m, "R", "L", provideLeftDiffusion( diffPrefix = "$" ) _ )
            }
         }
      }
      butFlipChans.visible = search.master.isDefined

      val butIncorporate = button( "Incorporate" ) { b =>
         nonSyntheticTimelines.headOption.foreach { tl0 =>
            // that is, collect all regions beginning with "$", remove this prefix,
            // apply offset, and paste them to the main timeline
            val arsMap = collectAudioRegions({
               case (_, ar) if( ar.name.startsWith( "$" )) =>
                  val i = ar.name.indexOf( '_' )
                  val diff  = ar.name.substring( 1, i )
                  val arNew = ar.copy( name = ar.name.substring( i + 1 ), span = ar.span.shift( incorpOff ))
                  (diff, arNew)
            }).groupBy( _._1 ).mapValues( _.map( _._2 )) // hell, can this be more messy?
            if( arsMap.nonEmpty ) {
               implicit val tl   = tl0
               tl.joinEdit[ Unit ]( "Incorporate" ) { ce0: AbstractCompoundEdit =>
                  implicit val ce = ce0   // sucky IDEA
                  set.punchOut.foreach { po =>
                     val poSpan        = fromInputRate( po.span )
                     val piSpan        = fromInputRate( set.punchIn.span )
                     val splitDelta    = mPunch.length - (poSpan.start - piSpan.start)   // korrekt?
                     val splitThresh   = (0.3 * tl.rate + 0.5).toLong // secsToFrames( 0.3 )
//println( "pos " + splitPos + " ; delta " + splitDelta )
                     if( splitDelta != 0L ) insertTimelineSpan( splitPos, splitDelta ) {
                        case (at, ar) =>
                           val start   = ar.span.start
                           val stop    = ar.span.stop
                           val mid     = (start + stop) >> 1
                           if( splitPos < mid ) {
                              if( splitPos - start > splitThresh ) InsertSpan.Split
                              else InsertSpan.Move
                           } else {
                              if( stop - splitPos > splitThresh ) InsertSpan.Split
                              else InsertSpan.Ignore
                           }
                     }
                  }
                  var trackMap = Map.empty[ AudioTrack, IndexedSeq[ AudioRegion ]]
                  arsMap.foreach {
                     case ("", ars)  => ars.foreach { ar =>
                        val tr = placeStereo( ar, more = trackMap )
                        trackMap += tr -> (trackMap.getOrElse( tr, IndexedSeq.empty ) :+ ar)
                     }
                     case ("L", ars) => ars.foreach { ar =>
                        val tr = placeLeft( ar, more = trackMap )
                        trackMap += tr -> (trackMap.getOrElse( tr, IndexedSeq.empty ) :+ ar)
                     }
                     case ("R", ars) => ars.foreach { ar =>
                        val tr = placeRight( ar, more = trackMap )
                        trackMap += tr -> (trackMap.getOrElse( tr, IndexedSeq.empty ) :+ ar)
                     }
                  }
               }
            }
         }
      }
//println( "incorpOff " + incorpOff + " : tl.rate " + tl.rate )
      val lbIncorporate = label( "Regions will be offset by " + timeString( incorpOff, tl.rate ))

//      var panelChildren = IndexedSeq[ Component ]( butIncorporate, lbIncorporate )

      val butSearchSplit = button( "New search for this punch length" ) { b =>
         // Match( sim: Float, file: File, punch: Span, boostIn: Float, boostOut: Float )
         // Search: offset
         // Settings( databaseFolder: File, metaInput: File, punchIn: Punch, punchOut: Option[Punch],
         //           minPunch: Long, maxPunch: Long, normalize: Boolean, maxBoost: Float, numMatches: Int,
         //           numPerFile: Int, minSpacing: Long )
         val arIn       = AudioRegion( Span( search.offset, search.offset + afe1.numFrames ), afe1.name, afe1, 0L )
         val copy       = CSettingsBuilder( search.settings )
         copy.minPunch  = m.punch.length  // in input rate!
         copy.maxPunch  = m.punch.length  // in input rate!
         CorrelatorSetup.makeSetup( arIn, copy, search.metas, Some( m ), search.transform )
      }
      butSearchSplit.visible = search.master.isEmpty

      val panel = new GroupPanel {
         theHorizontalLayout is Sequential( butIncorporate, lbIncorporate, butSearchSplit, butFlipChans )
         theVerticalLayout is Parallel( Baseline )( butIncorporate, lbIncorporate, butSearchSplit, butFlipChans )
      }

//      val bp = new BorderPanel {
//         add( panel, BorderPanel.Position.North )
//         add( scroll, BorderPanel.Position.South )
//      }

      tlf.bottomPanel = Some( panel )
      tlf.pack() // AndSetMinimum()
   }
}