/*
 *  ThirdMovementGUI.scala
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

import de.sciss.kontur.session.BasicTimeline
import de.sciss.app.AbstractWindow
import de.sciss.kontur.gui.{TimelineView, AppWindow}
import eu.flierl.grouppanel.GroupPanel
import java.awt.BorderLayout
import de.sciss.synth.io.AudioFile
import java.io.File
import ThirdMovement.Strategy
import xml.XML
import de.sciss.strugatzki.FeatureCorrelation.Match
import collection.immutable.{IndexedSeq => IIdxSeq}

object ThirdMovementGUI extends GUIGoodies with KonturGoodies with NullGoodies {
   def makeWindow( tl: BasicTimeline, tlv: TimelineView, settings: ThirdMovement.Settings ) {
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
         val sp = selSpan
         if( !sp.isEmpty ) {
            sb.tlSpan      = sp
            lbSpan.text    = timeString( sp, sr )
//            butSearch.enabled = true
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
      val ggStrategy    = combo[ Strategy, String ]( Strategy.seq )( sb.strategy = _ )( _.name )
      val lbStartWeight = label( "Start weight:" )
      val ggStartWeight = decimalSlider( "Temp", "Spect", sb.startWeight, w = 216 )( d => sb.startWeight = d.toFloat )
      val lbStopWeight  = label( "Stop weight:" )
      val ggStopWeight  = decimalSlider( "Temp", "Spect", sb.startWeight, w = 216 )( d => sb.stopWeight = d.toFloat )
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

      lazy val ggSearch = button( "Start searching..." ) { b =>
         var err = Option.empty[ String ]
         if( sb.tlSpan.isEmpty ) err = Some( "Timeline span is empty" )
         if( AudioFile.identify( sb.layer ).isEmpty ) err = Some( "Layer file not recognized" )
         if( !sb.materialFolder.isDirectory ) err = Some( "Material folder not recognized" )

         err match {
            case Some( txt ) => message( txt )
            case _ =>
               val newSettings = sb.build
               if( newSettings != settings ) {
                  saveSettings( newSettings )
               }
ThirdMovement.verbose = true
               beginSearch( tl, newSettings )
         }
      }

      // a bit ugly, but who cares now?
      lazy val panel = new GroupPanel {
//         linkHorizontalSize( butToIn, butToOut, butFromIn, butFromOut )
//         linkHorizontalSize( ggMinPunch, ggMaxPunch )
//         linkHorizontalSize( ggNumMatches, ggNumPerFile )
         theHorizontalLayout is Sequential(
            Parallel( Sequential(
               butToSpan,
               lbSpan
            ), Sequential(
               Parallel( lbLayer, lbMaterial, lbStrategy, lbStartWeight, lbStopWeight, lbConnWeight, lbStratWeight, lbMaxOverlap ),
               Parallel( txtLayer, txtMaterial, ggStrategy, ggStartWeight, ggStopWeight, ggConnWeight, ggStratWeight, ggMaxOverlap ),
               Parallel( butLayer, butMaterial )
            ), Sequential(
               Parallel( ggStartMinDur, ggStopMinDur, ggLayerOff, ggNumChannels ),
               Parallel( ggStartMaxDur, ggStopMaxDur )
            ), ggSearch )
         )
         theVerticalLayout is Sequential(
            Parallel( Baseline )( butToSpan, lbSpan ),
            Parallel( Baseline )( lbLayer, txtLayer, butLayer ),
            Parallel( Baseline )( lbMaterial, txtMaterial, butMaterial ),
            Parallel( Baseline )( lbStrategy, ggStrategy ),
            Parallel( Baseline )( lbStartWeight, ggStartWeight ),
            Parallel( Baseline )( lbStopWeight, ggStopWeight ),
            Parallel( Baseline )( lbConnWeight, ggConnWeight ),
            Parallel( Baseline )( lbStratWeight, ggStratWeight ),
            Parallel( Baseline )( lbMaxOverlap, ggMaxOverlap ),
            Parallel( Baseline )( ggStartMinDur, ggStartMaxDur ),
            Parallel( Baseline )( ggStopMinDur, ggStopMaxDur ),
            ggLayerOff,
            ggNumChannels,
            ggSearch
         )
      }

      val cp = a.getContentPane
      cp.add( panel.peer, BorderLayout.CENTER )

      a.pack()
      a.setLocationRelativeTo( null )
      a.setVisible( true )
   }

   def beginSearch( tl: BasicTimeline, settings: ThirdMovement.Settings ) {
//      if( verbose ) println( settings )

      def update( batch: IIdxSeq[ (Long, Match) ]) {
         batch.foreach( println )
      }

      val dlg  = progressDialog( "Correlating with database" )
      val proc = ThirdMovement( settings, update _ ) {
         case ThirdMovement.Success( _ ) =>
            dlg.stop()

         case ThirdMovement.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()

         case ThirdMovement.Aborted =>
            dlg.stop()

         case ThirdMovement.Progress( i ) => dlg.progress = i
      }
      dlg.start( proc )
   }

   def saveSettings( settings: ThirdMovement.Settings ) {
      val id   = plainName( settings.layer ) + "@" + settings.layerOffset
      val f = stampedFile( LeereNull.ueberzeichnungFolder, id, ".xml" )
      XML.save( f.getAbsolutePath, settings.toXML, "UTF-8", true, null )
   }
}