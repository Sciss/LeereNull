/*
 *  CorrelatorSetup.scala
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

import eu.flierl.grouppanel.GroupPanel
import de.sciss.strugatzki.{FeatureExtraction, FeatureCorrelation, Span => SSpan}
import java.io.File
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec}
import de.sciss.io.Span
import de.sciss.kontur.session.{AudioTrack, Track, SessionUtil, BasicTimeline, Session, AudioRegion}
import swing.{ProgressBar, Swing, Dialog}
import javax.swing.{JOptionPane, SwingUtilities}
import de.sciss.common.BasicWindowHandler
import FeatureCorrelation.{Match, Punch, SettingsBuilder => CSettingsBuilder}
import FeatureExtraction.{SettingsBuilder => ESettingsBuilder}

object CorrelatorSetup extends GUIGoodies with KonturGoodies with NullGoodies {
   def bounceAndExtract( tracks: List[ Track ], span: Span )( implicit doc: Session, timeline: BasicTimeline ) {
      val numChannels = tracks.collect({ case at: AudioTrack if( at.diffusion.isDefined ) => at.diffusion.get })
         .foldLeft( 0 )( (maxi, diff) => math.max( maxi, diff.numOutputChannels ))

      val id         = "bnc" + span.start
      val f          = stampedFile( LeereNull.bounceFolder, id, ".aif" )
      val spec       = AudioFileSpec( AudioFileType.AIFF, SampleFormat.Float, numChannels, timeline.rate )
      var canCancel : { def cancel() : Unit } = null

      val ggProgress = new ProgressBar
      val ggCancel = button( "cancel" ) { b =>
         canCancel.cancel()
      }
      val options = Array[ AnyRef]( ggCancel.peer )
      val op = new JOptionPane( ggProgress.peer, JOptionPane.INFORMATION_MESSAGE, 0, null, options )

      def fDispose() {
         val w = SwingUtilities.getWindowAncestor( op )
         if( w != null ) w.dispose()
      }

      var done = false
      try {
         canCancel  = SessionUtil.bounce( doc, timeline, tracks, span, f, spec, {
            case "done" => { done = true; fDispose() }
            case ("progress", i: Int) => ggProgress.value = i
         })
         BasicWindowHandler.showDialog( op, null, "Bounce" )
         if( done ) {
            // now extract
            extract( f ) { (meta, success) =>
               if( success ) Swing.onEDT {
                  val afe  = provideAudioFile( f ) // let it create the ce
                  val ar   = AudioRegion( span, plainName( f ), afe, 0L )
                  makeSetup( ar, defaultSettings( meta ), None )
               }
            }
         } else {
            canCancel.cancel()
         }
      }
      catch { case e: Exception =>
         fDispose()
      }
   }

   def prepareCorrelator( ar: AudioRegion )( implicit doc: Session ) {
      val afPath  = ar.audioFile.path
      val afName  = afPath.getName
      val plain   = plainName( afPath )
      val dbMeta  = dbMetaFile( plain )
      val exMeta  = extrMetaFile( plain )
      if( dbMeta.isFile ) {
         makeSetup( ar, defaultSettings( dbMeta ), None )
      } else if( exMeta.isFile ) {
         makeSetup( ar, defaultSettings( exMeta ), None )
      } else {
         val message = "<html>The audio file associated with the selected region<br>" +
            "<tt>" + afName + "</tt><br>is not in the feature database.<br>" +
            "<B>Extract features now?</B></html>"
         val res = Dialog.showConfirmation( null, message, "Meta data", Dialog.Options.OkCancel, Dialog.Message.Question )
         if( res == Dialog.Result.Ok ) {
            extract( afPath ) { (meta, success) =>
               if( success ) Swing.onEDT( makeSetup( ar, defaultSettings( meta ), None ))
            }
         }
      }
   }

   def extract( afPath: File )( whenDone: (File, Boolean) => Unit ) {
      val settings            = new ESettingsBuilder
      settings.audioInput     = afPath
      val plain               = plainName( afPath )
      settings.featureOutput  = featureFile( plain )
      val meta                = extrMetaFile( plain )
      settings.metaOutput     = Some( meta )

      val dlg = progressDialog( "Extracting features..." )
      val fe = FeatureExtraction( settings ) {
         case FeatureExtraction.Success =>
            dlg.stop()
            whenDone( meta, true )
         case FeatureExtraction.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()
            whenDone( meta, false )
         case FeatureExtraction.Aborted =>
            dlg.stop()
            whenDone( meta, false )
         case FeatureExtraction.Progress( i ) => dlg.progress = i
      }
      dlg.start( fe )
   }

   def defaultSettings( meta: File ) : CSettingsBuilder = {

      // grmphfffffff
      def secsToFrames( d: Double ) = (d * 44100.0 + 0.5).toLong

      val sb            = new CSettingsBuilder
      sb.punchIn        = Punch( SSpan( 0L, 0L ), 0.5f )    // our indicator that punchIn hasn't been set yet
      sb.databaseFolder = LeereNull.databaseFolder
      sb.metaInput      = meta

      sb.numMatches     = 10
      sb.numPerFile     = 2
      sb.maxBoost       = dbamp( 18 ).toFloat

      sb.minPunch       = secsToFrames( 2.0 )
      sb.maxPunch       = secsToFrames( 3.0 )

      sb
   }

   def makeSetup( ar: AudioRegion, settings: CSettingsBuilder, master: Option[ Match ])( implicit doc: Session ) {
      val tls  = doc.timelines
      val ar0  = ar.move( -ar.span.start )
      implicit val tl = tls.tryEdit( "Add Extractor Timeline" ) { implicit ce =>
         implicit val tl = BasicTimeline.newEmpty( doc )
         tl.span  = ar0.span
         tl.name  = uniqueName( tls, "$Extractor" )
         tls.editInsert( ce, tls.size, tl )
         placeStereo( ar0, diffPrefix = "$" )
         tl
      }

//      val settings            = new FeatureCorrelation.SettingsBuilder
//      settings.punchIn        = FeatureCorrelation.Punch( SSpan( 0L, 0L ))    // our indicator that punchIn hasn't been set yet
//      settings.databaseFolder = LeereNull.databaseFolder
//      settings.metaInput      = meta

      val tlf = TimelineFrame2 { f =>
//         println( "Wooha" )
//         f.dispose()
      }

      implicit val tlv = tlf.timelineView

      def punchInText( p: Punch ) = {
         if( p.span.isEmpty ) "<not set>" else timeString( p.span )
      }

      def punchOutText( p: Option[ Punch ]) = {
         p match {
            case Some( po ) => timeString( po.span )
            case None       => "<not set>"
         }
      }

      val lbPunchIn  = label( punchInText(  settings.punchIn  ), Some( 160 ))
      val lbPunchOut = label( punchOutText( settings.punchOut ), Some( 160 ))
      val lbWeightIn = label( percentString( settings.punchIn.temporalWeight ), Some( 32 ))
      val initOutWeight = settings.punchOut.map( _.temporalWeight ).getOrElse( 0.5f )
      val lbWeightOut= label( percentString( initOutWeight ), Some( 32 ))
      val ggWeightIn = decimalSlider( "spect", "temp", initial = settings.punchIn.temporalWeight ) { value =>
         settings.punchIn = settings.punchIn.copy( temporalWeight = value.toFloat )
         lbWeightIn.text = percentString( value )
      }
//      ggWeightIn.decimal = settings.punchIn.temporalWeight
      val ggWeightOut = decimalSlider( "spect", "temp", initial = initOutWeight ) { value =>
         settings.punchOut.foreach { po =>
            settings.punchOut = Some( po.copy( temporalWeight = value.toFloat ))
            lbWeightOut.text = percentString( value )
         }
      }
//      ggWeightOut.decimal = initOutWeight
      val lbMaxBoost  = label( decibelString( ampdb( settings.maxBoost )), Some( 40 ))
      val ggMaxBoost = decimalSlider( "Max boost:", "", initial = ampdb( settings.maxBoost ) * 32 ) { value =>
         val db  = value * 32
         val amp = ampdb( db )
         settings.maxBoost = amp.toFloat
         lbMaxBoost.text = decibelString( db )
      }

      val arDelta = ar.offset

      val butToIn = button( "→ In" ) { b =>
         val sp = selSpan.shift( arDelta )
         if( !sp.isEmpty ) {
            settings.punchIn  = settings.punchIn.copy( span = sp )
            lbPunchIn.text    = punchInText( settings.punchIn ) // str
         }
      }
      val butToOut = button( "→ Out" ) { b =>
         val sp = selSpan.shift( arDelta )
         if( !sp.isEmpty ) {
            settings.punchOut  = Some( Punch(
               sp, settings.punchOut.map( _.temporalWeight ).getOrElse( ggWeightOut.decimal.toFloat )))
            lbPunchOut.text    = punchOutText( settings.punchOut ) // timeString( sp )
         }
      }
      val butFromIn = button( "⬅ In" ) { b =>
         val sp = settings.punchIn.span
         if( !sp.isEmpty ) selSpan = sp
      }
      val butFromOut = button( "⬅ Out" ) { b =>
         settings.punchOut.foreach { po =>
            val sp = po.span
            if( !sp.isEmpty ) selSpan = sp
         }
      }

      val ggNumMatches = integerField( "# Matches:", 1, 1000, initial = settings.numMatches ) { i =>
         settings.numMatches = i
      }
      val ggNumPerFile = integerField( "# Per File:", 1, 1000, initial = settings.numPerFile ) { i =>
         settings.numPerFile = i
      }

      val ggMinPunch = timeField( "Min dur:", 0.0, 60.0, framesToSecs( settings.minPunch )) { secs =>
         settings.minPunch = secsToFrames( secs )
      }
      val ggMaxPunch = timeField( "Max dur:", 0.0, 60.0, framesToSecs( settings.maxPunch )) { secs =>
         settings.maxPunch = secsToFrames( secs )
      }

      val butSearch = button( "Start searching..." ) { b =>
         if( !settings.punchIn.span.isEmpty && settings.punchOut.isDefined ) {
            CorrelatorSelector.beginSearch( ar.span.start - ar.offset, settings, master )
         }
      }

      val panel = new GroupPanel {
         linkHorizontalSize( butToIn, butToOut, butFromIn, butFromOut )
         linkHorizontalSize( ggMinPunch, ggMaxPunch )
         linkHorizontalSize( ggNumMatches, ggNumPerFile )
         theHorizontalLayout is Sequential(
            Parallel( butToIn, butToOut ),
            Parallel( butFromIn, butFromOut ),
            Parallel( lbPunchIn, lbPunchOut ),
            Parallel( ggWeightIn, ggWeightOut ),
            Parallel( lbWeightIn, lbWeightOut ),
            Parallel( ggMinPunch, ggMaxPunch ),
            Parallel( ggNumMatches, ggNumPerFile ),
            Parallel( Sequential( ggMaxBoost, lbMaxBoost ), butSearch )
         )
         theVerticalLayout is Sequential(
            Parallel( Baseline )( butToIn,  butFromIn,  lbPunchIn,  ggWeightIn,  lbWeightIn,  ggMinPunch, ggNumMatches, ggMaxBoost, lbMaxBoost ),
            Parallel( Baseline )( butToOut, butFromOut, lbPunchOut, ggWeightOut, lbWeightOut, ggMaxPunch, ggNumPerFile, butSearch )
         )
      }

      tlf.bottomPanel = Some( panel )
      tlf.packAndSetMinimum()
   }
}