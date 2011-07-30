/*
 *  CorrelatorCore.scala
 *  (LeereNull)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
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
 *
 *
 *  Changelog:
 */

package de.sciss.leerenull

import de.sciss.leerenull.CorrelatorSelector.Search
import de.sciss.strugatzki.{Span, FeatureExtraction}
import eu.flierl.grouppanel.GroupPanel
import de.sciss.kontur.session.{AudioTrack, AudioFileElement, FadeSpec, AudioRegion, Session, BasicTimeline}

object CorrelatorCore extends GUIGoodies with KonturGoodies with NullGoodies {
   def makeMatchEditor( search: Search, idx: Int )( implicit doc: Session ) {
      val tls     = doc.timelines
      val set     = search.settings

      def frames( afe: AudioFileElement, secs: Double ) = (secs * afe.sampleRate + 0.5).toLong

      val m       = search.matches( idx )
      val meta    = FeatureExtraction.Settings.fromXMLFile( set.metaInput )

      var ar1Off     = -1L
      var incorpOff  = -1L
      var splitPos   = -1L

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

      implicit val tl = tls.tryEdit( "Add Matcher Timeline" ) { implicit ce =>
         val afe1    = provideAudioFile( meta.audioInput )

         val pi      = set.punchIn
         val fOff1   = math.max( 0L, pi.span.start - frames( afe1, 10 ))
         val start1  = 0L
         val pre1    = pi.span.start - fOff1
         val maxLen1 = set.punchOut.map( p => p.span.start ).getOrElse( afe1.numFrames ) - fOff1
         val len1    = math.min( maxLen1, pi.span.stop - fOff1 + frames( afe1, 1 ))
         val stop1   = start1 + len1
         val fadeIn1 = FadeSpec( math.min( pre1, frames( afe1, 1 )))
         val fadeOut1= FadeSpec( math.min( len1 - pre1, frames( afe1, 1 )))
         val ar1     = AudioRegion( Span( start1, stop1 ), regionName( "pre", afe1, pre = "" ), afe1, fOff1,
            fadeIn = Some( fadeIn1 ), fadeOut = Some( fadeOut1 ))

         ar1Off      = start1 - fOff1
         incorpOff   = search.offset - ar1Off
         splitPos    = ar1.span.stop + incorpOff

         val ar2O    = set.punchOut.map { po =>
            val fOff2      = math.max( fOff1 + len1, po.span.start - frames( afe1, 1 ))
            val pre2       = po.span.start - fOff2
            // start1 + pre1 = timeline spot where punch begins
            val start2     = start1 + pre1 + m.punch.length - pre2
            val maxLen2    = afe1.numFrames - fOff2
            val len2       = math.min( maxLen2, pre2 + po.span.length + frames( afe1, 10 ))
            val stop2      = start2 + len2
            val fadeIn2    = FadeSpec( math.min( pre2, frames( afe1, 1 )))
            val fadeOut2   = FadeSpec( math.min( len2 - pre2, frames( afe1, 1 )))
            val ar2        = AudioRegion( Span( start2, stop2 ), regionName( "post", afe1, pre = "" ), afe1, fOff2,
               fadeIn = Some( fadeIn2 ), fadeOut = Some( fadeOut2 ))
            ar2
         }

         val afe2    = provideAudioFile( m.file )
         val fOff3   = m.punch.start
         val start3  = start1 + pre1
         val maxLen3 = afe2.numFrames - fOff3
         // merge if gain difference is less than 6 dB
         val split3  = set.punchOut.isDefined && (math.max( m.boostOut, m.boostIn ) / math.min( m.boostOut, m.boostIn )) > 2
         val boost3  = if( !split3 && set.punchOut.isDefined ) math.sqrt( m.boostOut * m.boostIn ).toFloat else m.boostIn
         val fdt3    = m.punch.length / 4
         val len3    = if( split3 ) (m.punch.length / 2) + fdt3 else math.min( maxLen3, m.punch.length + frames( afe2, 1 ))
         val stop3   = start3 + len3
         val fade3   = FadeSpec( fdt3 )
         val ar3     = AudioRegion( Span( start3, stop3 ), regionName( if( split3 ) "pin" else "punch", afe2 ),
            afe2, fOff3,
            gain = boost3, fadeIn = Some( fade3 ), fadeOut = Some( fade3 ))
//println( "ar3 : " + ar3 + " ; OFFSET = " + fOff3 + " ; m.punch = " + m.punch )

         val ar4O    = set.punchOut match {
            case Some( po ) if( split3 ) =>
               val start4  = stop3 - fdt3
               val fOff4   = fOff3 + start4 - start3
               val len4    = math.min( afe2.numFrames - fOff4, m.punch.length - (start4 - start3) + frames( afe2, 1 ))
               val stop4   = start4 + len4
               val fade4   = FadeSpec( fdt3 )
               val boost4  = m.boostOut
               val ar4     = AudioRegion( Span( start4, stop4 ), regionName( "pout", afe2 ), afe2, fOff4,
                  gain = boost4, fadeIn = Some( fade4 ), fadeOut = Some( fade4 ))
               Some( ar4 )

            case _ => None
         }

         implicit val tl = BasicTimeline.newEmpty( doc )
         tl.span  = Span( 0L, math.max( math.max( ar1.span.stop, ar2O.map( _.span.stop ).getOrElse( 0L )), ar3.span.stop ))
         tl.name  = uniqueName( tls, "$Matcher" )
         tls.editInsert( ce, tls.size, tl )
         var trackSet = IndexedSeq.empty[ AudioTrack ]
         trackSet :+= placeStereo( ar1, "$", more = trackSet )
         ar2O.foreach( r => trackSet :+= placeStereo( r, "$", more = trackSet ))
         trackSet :+= placeStereo( ar3, "$", more = trackSet )
         ar4O.foreach( r => trackSet :+= placeStereo( r, "$", more = trackSet ))
         tl
      }

      val tlf = TimelineFrame2 { f =>
//         println( "Bye..." )
//         f.dispose()
      }

      val butIncorporate = button( "Incorporate" ) { b =>
         nonSyntheticTimelines.headOption.foreach { tl0 =>
            // that is, collect all regions beginning with "$", remove this prefix,
            // apply offset, and paste them to the main timeline
            val ars = collectAudioRegions {
               case (_, ar) if( ar.name.startsWith( "$" )) =>
                  ar.copy( name = ar.name.substring( 1 ), span = ar.span.shift( incorpOff ))
            }
            if( ars.nonEmpty ) {
               implicit val tl   = tl0
               tl.joinEdit( "Incorporate" ) { implicit ce =>
                  set.punchOut.foreach { po =>
                     val splitDelta    = m.punch.length - (po.span.start - set.punchIn.span.start)   // korrekt?
                     val splitThresh   = secsToFrames( 0.3 )
//println( "pos " + splitPos + " ; delta " + splitDelta )
                     insertTimelineSpan( splitPos, splitDelta ) {
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
                  var trackSet = IndexedSeq.empty[ AudioTrack ]
                  ars.foreach( r => trackSet :+= placeStereo( r, more = trackSet ))
               }
            }
         }
      }
      val lbIncorporate = label( "Regions will be offset by " + timeString( incorpOff ))

      val panel = new GroupPanel {
         theHorizontalLayout is Sequential( butIncorporate, lbIncorporate )
         theVerticalLayout is Parallel( Baseline )( butIncorporate, lbIncorporate )
      }

//      val bp = new BorderPanel {
//         add( panel, BorderPanel.Position.North )
//         add( scroll, BorderPanel.Position.South )
//      }

      tlf.bottomPanel = Some( panel )
      tlf.pack() // AndSetMinimum()
   }
}