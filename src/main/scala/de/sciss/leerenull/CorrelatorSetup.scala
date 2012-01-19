/*
 *  CorrelatorSetup.scala
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

import eu.flierl.grouppanel.GroupPanel
import java.io.File
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec}
import de.sciss.io.Span
import javax.swing.{JOptionPane, SwingUtilities}
import de.sciss.common.BasicWindowHandler
import de.sciss.kontur.session.{MatrixDiffusion, AudioTrack, Track, SessionUtil, BasicTimeline, Session, AudioRegion}
import collection.breakOut
import swing.{ProgressBar, Swing, Dialog}
import xml.XML
import de.sciss.strugatzki.{FeatureExtraction, FeatureCorrelation, Span => SSpan}
import FeatureCorrelation.{Match, Punch, SettingsBuilder => CSettingsBuilder}
import FeatureExtraction.{ChannelsBehavior, Settings => ESettings, SettingsBuilder => ESettingsBuilder}

object CorrelatorSetup extends GUIGoodies with KonturGoodies with NullGoodies {
   def bounceAndExtract( tracks: List[ Track ], span: Span, transform: CorrelatorCore.Transform )( implicit doc: Session, timeline: BasicTimeline ) {
      val atracks = tracks.collect({ case at: AudioTrack if( at.diffusion.isDefined ) => at })
      val numChannels = atracks.map( _.diffusion.get ).foldLeft( 0 )( (maxi, diff) => math.max( maxi, diff.numOutputChannels ))
      val diffs: Set[ MatrixDiffusion ] = atracks.map( _.diffusion )
         .collect({ case Some( m: MatrixDiffusion ) if( m.numOutputChannels > 1 ) => m })( breakOut )
//      val hasFirst   = diffs.find( m => m.matrix.toSeq.find( p => p.head > 0f && p.last == 0f ).isDefined ).isDefined
//      val hasLast    = diffs.find( m => m.matrix.toSeq.find( p => p.head == 0f && p.last > 1f ).isDefined ).isDefined
      val hasFirst   = diffs.exists( _.matrix.toSeq.forall( _.last == 0f ))
      val hasLast    = diffs.exists( _.matrix.toSeq.forall( _.head == 0f ))

      val id         = "bnc" + span.start
      val fNorm      = stampedFile( LeereNull.bounceFolder, id, ".aif" )
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
         canCancel  = SessionUtil.bounce( doc, timeline, tracks, span, fNorm, spec, {
            case "done" => { done = true; fDispose() }
            case ("progress", i: Int) => ggProgress.value = i
         })
         BasicWindowHandler.showDialog( op, null, "Bounce" )

         def runExtract( fAudio: File, fExtrSource: File ) {
            // now extract
            var plainsBehaviors: List[ (String, ChannelsBehavior) ] = (plainName( fExtrSource ), ChannelsBehavior.Mix) :: Nil
            if( hasFirst ) plainsBehaviors ::= (plainName( fExtrSource ) + "-F", ChannelsBehavior.First)
            if( hasLast )  plainsBehaviors ::= (plainName( fExtrSource ) + "-L", ChannelsBehavior.Last)
            var metas = IndexedSeq.empty[ ESettings ]

            def iter( b: List[ (String, ChannelsBehavior) ]) {
               b match {
                  case (plain, behavior) :: tail =>
                     extract( fExtrSource, plain, behavior ) { (meta, success) =>
                        if( success ) {
                           val sb         = ESettingsBuilder( meta )
                           sb.audioInput  = fAudio // !
                           val meta2      = saveMeta( sb, plain )
                           metas :+= meta2
                           iter( tail )
                        }
                     }
                  case _ =>
                     Swing.onEDT {
                        val afe     = provideAudioFile( fAudio ) // let it create the ce
                        val ar      = AudioRegion( span, plainName( fAudio ), afe, 0L )
//                        val metas   = metaFiles.map( ESettings.fromXMLFile( _ ))
                        makeSetup( ar, defaultSettings, metas, None, transform )
                     }
               }
            }
            iter( plainsBehaviors )
         }

         if( done ) {
            transform.fscapeOption match {
               case Some( fsc ) =>
                  val fTrns = new File( LeereNull.bounceFolder, plainName( fNorm ) + transform.fileID + ".aif" )
//                  FScape.shift( fNorm, fTrns, freq ) { b =>
//                     if( b ) runExtract( fNorm, fTrns )
//                  }
                  fsc( fNorm, fTrns )( if( _ ) runExtract( fNorm, fTrns ))
               case None =>
                  runExtract( fNorm, fNorm )
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
      val afPath     = ar.audioFile.path
      val afName     = afPath.getName
      val plain      = plainName( afPath )
      val metaFiles  = IndexedSeq( plain, plain + "-F", plain + "-L" ).map( n => (dbMetaFile( n ), extrMetaFile( n)))
         .collect {
            case (f, _) if( f.isFile ) => f
            case (_, f) if( f.isFile ) => f
         }
      val metas      = metaFiles.map( ESettings.fromXMLFile( _ ))
      if( metas.nonEmpty ) {
         makeSetup( ar, defaultSettings, metas, None, CorrelatorCore.TransformNone )
      } else {
         val message = "<html>The audio file associated with the selected region<br>" +
            "<tt>" + afName + "</tt><br>is not in the feature database.<br>" +
            "<B>Extract features now?</B></html>"
         val res = Dialog.showConfirmation( null, message, "Meta data", Dialog.Options.OkCancel, Dialog.Message.Question )
         if( res == Dialog.Result.Ok ) {
            val plain = plainName( afPath )
            extract( afPath, plain, ChannelsBehavior.Mix /* XXX */) { (meta, success) =>
               if( success ) {
                  val meta2 = saveMeta( ESettingsBuilder( meta ), plain )
                  Swing.onEDT( makeSetup( ar, defaultSettings, IndexedSeq( meta2 ), None, CorrelatorCore.TransformNone ))
               }
            }
         }
      }
   }

   def saveMeta( sb: ESettingsBuilder, plain: String ) : ESettings = {
      val metaFile   = extrMetaFile( plain )
      sb.metaOutput  = Some( metaFile )
      val meta       = sb.build
      val xml        = meta.toXML
      XML.save( metaFile.getAbsolutePath, xml, "UTF-8", true, null )
      meta
   }

   def extract( afPath: File, plain: String, behavior: ChannelsBehavior )( whenDone: (ESettings, Boolean) => Unit ) {
      val sb               = new ESettingsBuilder
      sb.audioInput        = afPath
      sb.featureOutput     = featureFile( plain )
//      val meta             = extrMetaFile( plain )
//      sb.metaOutput        = Some( meta )
      sb.channelsBehavior  = behavior
      val settings         = sb.build

      val dlg = progressDialog( "Extracting features..." )
      val fe = FeatureExtraction( settings ) {
         case FeatureExtraction.Success =>
            dlg.stop()
            whenDone( settings, true )
         case FeatureExtraction.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()
            whenDone( settings, false )
         case FeatureExtraction.Aborted =>
            dlg.stop()
            whenDone( settings, false )
         case FeatureExtraction.Progress( i ) => dlg.progress = i
      }
      dlg.start( fe )
   }

   def defaultSettings : CSettingsBuilder = {

      // grmphfffffff
      def secsToFrames( d: Double ) = (d * 44100.0 + 0.5).toLong

      val sb            = new CSettingsBuilder
      sb.punchIn        = Punch( SSpan( 0L, 0L ), 0.5f )    // our indicator that punchIn hasn't been set yet
      sb.databaseFolder = LeereNull.databaseFolder
//      sb.metaInput      = meta

      sb.numMatches     = 10
      sb.numPerFile     = 2
      sb.maxBoost       = dbamp( 18 ).toFloat

      sb.minPunch       = secsToFrames( 2.0 )
      sb.maxPunch       = secsToFrames( 3.0 )

      sb
   }

   def makeSetup( ar: AudioRegion, settings: CSettingsBuilder, metas: IndexedSeq[ ESettings ], master: Option[ Match ],
                  transform: CorrelatorCore.Transform )
                ( implicit doc: Session ) {

      val inputRate = ar.audioFile.sampleRate * transform.timeScale
      def secsToFrames( d: Double ) = (d * inputRate + 0.5).toLong
      def framesToSecs( n: Long ) = n / inputRate
      def toInputRate( sp: SSpan ) =
         SSpan( (sp.start * transform.timeScale + 0.5).toLong, (sp.stop * transform.timeScale + 0.5).toLong )
      def fromInputRate( sp: SSpan ) =
         SSpan( (sp.start / transform.timeScale + 0.5).toLong, (sp.stop / transform.timeScale + 0.5).toLong )

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
         if( p.span.isEmpty ) "<not set>" else timeString( p.span, inputRate )
      }

      def punchOutText( p: Option[ Punch ]) = {
         p match {
            case Some( po ) => timeString( po.span, inputRate )
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

      lazy val butToIn = button( "→ In" ) { b =>
         val sp = toInputRate( selSpan.shift( arDelta ))
         if( !sp.isEmpty ) {
            settings.punchIn  = settings.punchIn.copy( span = sp )
            lbPunchIn.text    = punchInText( settings.punchIn ) // str
            butSearch.enabled = true
         }
      }
      lazy val butToOut = button( "→ Out" ) { b =>
         val sp = toInputRate( selSpan.shift( arDelta ))
         if( !sp.isEmpty ) {
            settings.punchOut  = Some( Punch(
               sp, settings.punchOut.map( _.temporalWeight ).getOrElse( ggWeightOut.decimal.toFloat )))
            lbPunchOut.text    = punchOutText( settings.punchOut ) // timeString( sp )
         }
      }
      lazy val butFromIn = button( "⬅ In" ) { b =>
         val sp = settings.punchIn.span
         if( !sp.isEmpty ) selSpan = fromInputRate( sp )
      }
      lazy val butFromOut = button( "⬅ Out" ) { b =>
         settings.punchOut.foreach { po =>
            val sp = po.span
            if( !sp.isEmpty ) selSpan = fromInputRate( sp )
         }
      }

      lazy val ggNumMatches = integerField( "# Matches:", 1, 1000, initial = settings.numMatches ) { i =>
         settings.numMatches = i
      }
      lazy val ggNumPerFile = integerField( "# Per File:", 1, 1000, initial = settings.numPerFile ) { i =>
         settings.numPerFile = i
      }

      lazy val ggMinPunch = timeField( "Min dur:", 0.0, 60.0, framesToSecs( settings.minPunch )) { secs =>
         settings.minPunch = secsToFrames( secs )
      }
      lazy val ggMaxPunch = timeField( "Max dur:", 0.0, 60.0, framesToSecs( settings.maxPunch )) { secs =>
         settings.maxPunch = secsToFrames( secs )
      }

      lazy val searchInputMeta = settings.metaInput

      def metaLabel( m: ESettings ) = {
         val n    = plainName( m.featureOutput )
         if( n.endsWith( "-F" )) "First" else if( n.endsWith( "-L" )) "Last" else "Mix"
      }

      lazy val metas0 = metas.sortBy( metaLabel( _ ) match {
         case "Mix"   => 0
         case "First" => 1
         case "Last"  => 2
      })

      lazy val ggMeta = combo( metas0 )() { meta =>
         val n    = plainName( meta.featureOutput )
         val str  = metaLabel( meta )
         if( searchInputMeta != null && plainName( searchInputMeta ) == n ) {
            "<html><i>" + str + "</i></html>"
         } else str
      }
//      if( metas0.size > 1 && metas0.head.metaOutput == Option( searchInputMeta )) ggMeta.selection.index = 1

      lazy val butSearch = button( "Start searching..." ) { b =>
         if( !settings.punchIn.span.isEmpty /* && settings.punchOut.isDefined */ ) {
            ggMeta.selection.item.metaOutput match {
               case Some( meta ) =>
                  settings.metaInput = meta
                  CorrelatorSelector.beginSearch( ar.span.start - ar.offset, settings, metas, master, transform )
               case None => message( "? No meta file available ?" )
            }
         }
      }
      butSearch.enabled = !settings.punchIn.span.isEmpty

      lazy val panel = new GroupPanel {
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
            Parallel( Sequential( ggMaxBoost, lbMaxBoost ), Sequential( ggMeta, butSearch ))
         )
         theVerticalLayout is Sequential(
            Parallel( Baseline )( butToIn,  butFromIn,  lbPunchIn,  ggWeightIn,  lbWeightIn,  ggMinPunch, ggNumMatches, ggMaxBoost, lbMaxBoost ),
            Parallel( Baseline )( butToOut, butFromOut, lbPunchOut, ggWeightOut, lbWeightOut, ggMaxPunch, ggNumPerFile, ggMeta, butSearch )
         )
      }

      tlf.bottomPanel = Some( panel )
      tlf.packAndSetMinimum()
   }
}