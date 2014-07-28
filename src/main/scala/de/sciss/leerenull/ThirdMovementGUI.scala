/*
 *  ThirdMovementGUI.scala
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

import de.sciss.app.AbstractWindow
import de.sciss.kontur.gui.{TimelineView, AppWindow}
import java.awt.BorderLayout
import de.sciss.processor.Processor
import de.sciss.processor.Processor.Aborted
import de.sciss.span.Span
import de.sciss.swingplus.GroupPanel
import de.sciss.synth.io.AudioFile
import java.io.File
import ThirdMovement.Strategy
import scala.util.{Success, Failure}
import xml.XML
import de.sciss.strugatzki.FeatureCorrelation.Match
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.kontur.util.Matrix2D
import de.sciss.kontur.session.{FadeSpec, AudioTrack, Session, AudioRegion, BasicTimeline}

object ThirdMovementGUI extends GUIGoodies with KonturGoodies with NullGoodies {
   def makeWindow( tl: BasicTimeline, tlv: TimelineView, doc: Session, settings: ThirdMovement.Settings ): Unit = {
      val a = new AppWindow( AbstractWindow.REGULAR ) {
         setTitle( "Überzeichnung : " + tl.name )
         setResizable( false )
      }

      val sb = ThirdMovement.SettingsBuilder()
      sb.read( settings )

      val sr = tl.rate
      implicit val tlv0 = tlv

//      def layerOffset: Long

      lazy val lbSpan      = label( timeString( sb.tlSpan, sr ))
      lazy val butToSpan   = button( "→ Span" ) { b =>
         selSpan match {
           case sp @ Span(_, _) =>
            sb.tlSpan      = sp
            lbSpan.text    = timeString( sp, sr )
//            butSearch.enabled = true
           case _ =>
         }
      }

      val lbLayer    = label( "Layer file:" )
      val txtLayer   = textField( sb.layer.getPath, 48 ) { txt =>
         sb.layer    = new File( txt )
      }
      val butLayer   = button( "\u21EA" ) { b =>
         openFileDialog( "Select layer file", sb.layer, { AudioFile.identify( _ ).isDefined }, a.getWindow ).foreach { f =>
            sb.layer       = f
            txtLayer.text  = f.getPath
         }
      }

      val lbMaterial    = label( "Material folder:" )
      val txtMaterial   = textField( sb.materialFolder.getPath, 48 ) { txt =>
         sb.materialFolder = new File( txt )
      }
      val butMaterial   = button( "\u21EA" ) { b =>
         openFileDialog( "Select material folder (a file inside that folder)", new File( sb.materialFolder, "<dir>" ),
            parent = a.getWindow ).foreach { f =>
            val dir = f.getParentFile
            sb.materialFolder = dir
            txtMaterial.text  = dir.getPath
         }
      }

      def secsToFrames( d: Double ) = (d * sr + 0.5).toLong
      def framesToSecs( n: Long ) = n / sr

      val ggNumChannels = integerField( "Num. channels:", 2, 64, sb.numChannels )( sb.numChannels = _ )
      val lbStrategy    = label( "Strategy:" )
      val ggStrategy    = combo[ Strategy, String ]( Strategy.seq )( s => { sb.strategy = s; println( s )})( _.name )
      ggStrategy.selection.item = sb.strategy
      val lbStartWeight = label( "Start weight:" )
      val ggStartWeight = decimalSlider( "Temp", "Spect", sb.startWeight, w = 216 )( d => sb.startWeight = d.toFloat )
      val lbStopWeight  = label( "Stop weight:" )
      val ggStopWeight  = decimalSlider( "Temp", "Spect", sb.stopWeight, w = 216 )( d => sb.stopWeight = d.toFloat )
      val lbConnWeight  = label( "Connection weight:" )
      val ggConnWeight  = decimalSlider( "0%", "100%", sb.connectionWeight, w = 216 )( d => sb.connectionWeight = d.toFloat )
      val lbMaxOverlap  = label( "Max. overlap:" )
      val ggMaxOverlap  = decimalSlider( "0%", "100%", sb.maxOverlap, w = 216 )( d => sb.maxOverlap = d.toFloat )
      val lbStratWeight = label( "Strategy weight:" )
      val ggStratWeight = decimalSlider( "0%", "100%", sb.strategyWeight, w = 216 )( d => sb.strategyWeight = d.toFloat )
      val ggStartMinDur = timeField( "Start min. duration:", 0.1, 100.0, framesToSecs( sb.startDur._1 )) { d =>
         sb.startDur    = sb.startDur.copy( _1 = secsToFrames( d ))
      }
      val ggStartMaxDur = timeField( "Start max. duration:", 0.1, 100.0, framesToSecs( sb.startDur._2 )) { d =>
         sb.startDur    = sb.startDur.copy( _2 = secsToFrames( d ))
      }
      val ggStopMinDur  = timeField( "Stop min. duration:", 0.1, 100.0, framesToSecs( sb.stopDur._1 )) { d =>
         sb.stopDur     = sb.stopDur.copy( _1 = secsToFrames( d ))
      }
      val ggStopMaxDur  = timeField( "Stop max. duration:", 0.1, 100.0, framesToSecs( sb.stopDur._2 )) { d =>
         sb.stopDur     = sb.stopDur.copy( _2 = secsToFrames( d ))
      }
      val ggLayerOff    = timeField( "Layer offset:", 0.0, 1000.0, framesToSecs( sb.layerOffset )) { d =>
         sb.layerOffset = secsToFrames( d )
      }
      
      val ggAutoSave = checkBox( "Autosave (every 15 mins.)" )()
      ggAutoSave.selected = true

      lazy val ggSearch = button( "Start searching..." ) { b =>
         var err = Option.empty[ String ]
         if( sb.tlSpan.isEmpty ) err = Some( "Timeline span is empty" )
         if( AudioFile.identify( sb.layer ).isEmpty ) err = Some( "Layer file not recognized" )
         if( !sb.materialFolder.isDirectory ) err = Some( "Material folder not recognized" )
         val autoSave = if( ggAutoSave.selected ) {
            val dir = new File( ThirdMovement.folder, "autosave" )
            if( !dir.exists ) {
               if( !dir.mkdirs() ) err = Some( "Could not create autosave folder" )
            }
            val testFile = File.createTempFile( "test", "test", dir )
            if( !testFile.exists ) err = Some( "Autosave folder is not writable" )
            testFile.delete()
            Some( dir )
         } else None
         
         err match {
            case Some( txt ) => message( txt )
            case _ =>
               val newSettings = sb.build
               if( newSettings != settings ) {
                  saveSettings( newSettings )
               }
ThirdMovement.verbose = true
               beginSearch( tl, doc, newSettings, autoSave )
         }
      }

      // a bit ugly, but who cares now?
      lazy val panel = new GroupPanel {
//         linkHorizontalSize( butToIn, butToOut, butFromIn, butFromOut )
//         linkHorizontalSize( ggMinPunch, ggMaxPunch )
//         linkHorizontalSize( ggNumMatches, ggNumPerFile )
         horizontal = Seq(
            Par( Seq(
               butToSpan,
               lbSpan
            ), Seq(
               Par( lbLayer, lbMaterial, lbStrategy, lbStartWeight, lbStopWeight, lbConnWeight, lbStratWeight, lbMaxOverlap ),
               Par( txtLayer, txtMaterial, ggStrategy, ggStartWeight, ggStopWeight, ggConnWeight, ggStratWeight, ggMaxOverlap ),
               Par( butLayer, butMaterial )
            ), Seq(
               Par( ggStartMinDur, ggStopMinDur, ggLayerOff, ggNumChannels ),
               Par( ggStartMaxDur, ggStopMaxDur )
            ), Seq( ggSearch, ggAutoSave ))
         )
         vertical = Seq(
           Par( Baseline )( butToSpan, lbSpan ),
           Par( Baseline )( lbLayer, txtLayer, butLayer ),
           Par( Baseline )( lbMaterial, txtMaterial, butMaterial ),
           Par( Baseline )( lbStrategy, ggStrategy ),
           Par( Baseline )( lbStartWeight, ggStartWeight ),
           Par( Baseline )( lbStopWeight, ggStopWeight ),
           Par( Baseline )( lbConnWeight, ggConnWeight ),
           Par( Baseline )( lbStratWeight, ggStratWeight ),
           Par( Baseline )( lbMaxOverlap, ggMaxOverlap ),
           Par( Baseline )( ggStartMinDur, ggStartMaxDur ),
           Par( Baseline )( ggStopMinDur, ggStopMaxDur ),
            ggLayerOff,
            ggNumChannels,
           Par( Baseline )( ggSearch, ggAutoSave )
         )
      }

      val cp = a.getContentPane
      cp.add( panel.peer, BorderLayout.CENTER )

      a.pack()
      a.setLocationRelativeTo( null )
      a.setVisible( true )
   }

   def beginSearch( tl: BasicTimeline, doc: Session, settings: ThirdMovement.Settings, autoSave: Option[ File ]): Unit = {
//      if( verbose ) println( settings )

      implicit val _doc = doc
      implicit val _tl  = tl

      val rnd     = new util.Random()
      val fadeLen = 882 // 20 ms
      val fadeIn  = Some( FadeSpec( fadeLen ))
      val fadeOut = Some( FadeSpec( fadeLen ))

      var lastSave   = System.currentTimeMillis()

      def update( batch: Vec[ (Long, Match) ]): Unit = {
         tl.tryEdit( "Add segments" ) { implicit ed =>
            var tracks = Map.empty[ AudioTrack, IndexedSeq[ AudioRegion ]]
println( "\n::::::: Insertions :::::::\n" )
            batch.zipWithIndex.foreach { case ((pos, m @ Match( _, file, fileSpan, gain0, _ )), ch) =>
println( "  at " + pos + " file span " + fileSpan )
               val af      = provideAudioFile( file )
               val tlSpan  = Span( pos, pos + fileSpan.length )
               val gain    = if( gain0.isNaN ) 1f else gain0
               val ar      = AudioRegion( tlSpan, plainName( file ) + "@" + fileSpan.start, af, fileSpan.start,
                                          gain, fadeIn, fadeOut )
               val mat     = Matrix2D.fromSeq( if( af.numChannels == 1 ) {
                  Seq( Seq.tabulate( settings.numChannels )( outCh => if( outCh == ch ) 1f else 0f ))
               } else {
                  val (g1, g2) = if( rnd.nextBoolean() ) {
                     // favour left (first) channel
                     (1f, 0.35f)
                  } else {
                     // favour right (last) channel
                     (0.35f, 1f)
                  }

                  Seq.tabulate( af.numChannels ) { inCh =>
                     Seq.tabulate( settings.numChannels )( outCh =>
                        if( outCh == ch ) {
                           if( inCh == 0 ) g1 else if( inCh == af.numChannels - 1 ) g2 else 0f
                        } else 0f
                     )
                  }
               })
               val pre     = "Ch-" + (ch+1) + " "
               val diff    = provideDiffusion( mat, prefix = pre )
               val at      = placeWithDiff( diff, ar, more = tracks, trackPrefix = pre )
               tracks     += ((at, tracks.getOrElse( at, IndexedSeq.empty ) :+ ar))
            }
         }

         autoSave.foreach { autoSaveDir =>
            val now = System.currentTimeMillis()
            if( (now - lastSave) > (15 * 60 * 1000L) ) {
               val f = stampedFile( autoSaveDir, "kontur-autosave", ".xml" )
               doc.save( f )
               lastSave = now
            }
         }
      }

      val dlg  = progressDialog( "Correlating with database" )
     val proc = ThirdMovement(ThirdMovement.Config(settings, update))
     proc.addListener {
         case Processor.Result(_, Success(_)) =>
            dlg.stop()

         case Processor.Result(_, Failure(Aborted())) =>
           dlg.stop()

         case Processor.Result(_, Failure(e)) =>
            dlg.stop()
            e.printStackTrace()

         case prog @ Processor.Progress(_, _) => dlg.progress = prog.toInt
      }
      dlg.start( proc )
   }

   def saveSettings( settings: ThirdMovement.Settings ): Unit = {
      val id   = plainName( settings.layer ) + "@" + settings.layerOffset
      val f = stampedFile( LeereNull.ueberzeichnungFolder, id, ".xml" )
      XML.save( f.getAbsolutePath, settings.toXML, "UTF-8", xmlDecl = true)
   }
}