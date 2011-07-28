package de.sciss.leerenull

import java.awt.BorderLayout
import de.sciss.leerenull.CorrelatorSelector.Search
import eu.flierl.grouppanel.GroupPanel
import swing.BorderPanel
import de.sciss.strugatzki.{Span, FeatureExtraction}
import de.sciss.kontur.session.{AudioFileElement, FadeSpec, AudioRegion, Session, BasicTimeline}

object CorrelatorCore extends GUIGoodies with KonturGoodies with NullGoodies {
   def makeMatchEditor( search: Search, idx: Int )( implicit doc: Session ) {
      val tls     = doc.timelines

      def frames( afe: AudioFileElement, secs: Double ) = (secs * afe.sampleRate + 0.5).toLong

      val m       = search.matches( idx )
      val meta    = FeatureExtraction.Settings.fromXMLFile( search.settings.metaInput )
      val afe1    = provideAudioFile( meta.audioInput )
      val pi      = search.settings.punchIn
      val start1  = math.max( 0L, pi.span.start - frames( afe1, 10 ))
      val pre1    = pi.span.start - start1
      val stop1   = math.min( afe1.numFrames, pi.span.stop + frames( afe1, 1 ))
      val fadeIn1 = FadeSpec( math.min( pre1, frames( afe1, 1 )))
      val fadeOut1= FadeSpec( math.min( stop1 - pi.span.stop, frames( afe1, 1 )))
      val ar1     = AudioRegion( Span( 0, stop1 - start1 ), "pre_roll", afe1, start1,
         fadeIn = Some( fadeIn1 ), fadeOut = Some( fadeOut1 ))

      implicit val tl = tls.tryEdit( "Add Matcher Timeline" ) { implicit ce =>
         implicit val tl = BasicTimeline.newEmpty( doc )
//         tl.span  = ar0.span
         tl.name  = uniqueName( tls, "$Matcher" )
         tls.editInsert( ce, tls.size, tl )
         placeStereo( ar1, "$" )
         tl
      }

      /* val tlf = */ TimelineFrame2 { f =>
         println( "Bye..." )
         f.dispose()
      }

//      val panel = new GroupPanel {
//         theHorizontalLayout is Sequential( butSelectMatch )
//         theVerticalLayout is Parallel( Baseline )( butSelectMatch )
//      }
//
//      val bp = new BorderPanel {
//         add( panel, BorderPanel.Position.North )
//         add( scroll, BorderPanel.Position.South )
//      }
//
//      tlf.bottomPanel = Some( bp )
//      tlf.pack() // AndSetMinimum()
   }
}