/*
 *  LeereNull.scala
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

import de.sciss.gui.{MenuItem, MenuGroup}
import java.util.Properties
import java.io.{File, FileInputStream}
import eu.flierl.grouppanel.GroupPanel
import collection.breakOut
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import de.sciss.kontur.io.SonagramOverview
import de.sciss.kontur.gui.{TrailViewEditor, DefaultTrackComponent}
import swing.{Swing, ButtonGroup, Dialog}
import de.sciss.kontur.Kontur
import de.sciss.kontur.session.{MatrixDiffusion, Diffusion, AudioTrack, AudioRegion}

object LeereNull extends Runnable with GUIGoodies with KonturGoodies with NullGoodies {
   lazy val (baseFolder, databaseFolder, extractorFolder, searchFolder, bounceFolder, ueberzeichnungFolder) = {
      val file = new File( "leerenull-settings.xml" )
      val prop = new Properties()
      val is = new FileInputStream( file )
      prop.loadFromXML( is )
      is.close()
      val base             = new File( prop.getProperty( "base" ))
      val database         = new File( base, "feature" )
      val extractor        = new File( base, "extract" )
      val search           = new File( base, "search" )
      val bounce           = new File( base, "bounce" )
      val ueberzeichnung   = new File( base, "ueberzeichnung" )
      (base, database, extractor, search, bounce, ueberzeichnung)
   }

   def main( args: Array[ String ]) {
      Swing.onEDT( run() )
   }

   def run() {
      SonagramOverview.maxSize = 16384

      val app  = new Kontur( Array() )
      val mf   = app.getMenuFactory
      val mg   = new MenuGroup( "leerenull", "Leere Null" )
      val miExtractor = new MenuItem( "leerenull.extractor", action( "Extractor...", "control E" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val tl0  = tl
               implicit val tlv0 = tlv
               implicit val trl0 = trl

               val span = selSpan
// DISABLE THIS SHORTCUT, AS WE CANNOT SPECIFY SHIFT OR RESAMPLE
//               cutTheCheese( selectedAudioRegions, span ) match {
//                  case IndexedSeq( ar ) => CorrelatorSetup.prepareCorrelator( ar )
//                  case _ =>
////                     val message = "<html>Multiple regions are selected. To proceed,<br>" +
////                        "an intermediate bounced representation is needed.<br>" +
////                        "<B>Go ahead and bounce?</B></html>"
                     val lbInfo = label( "<html>Feature Extraction: To proceed,<br>" +
                               "an intermediate bounced representation is needed.<br>" +
                               "<B>Go ahead and bounce?</B></html>"
                     )
                     def actionBut() {
                        ggShiftAmount.enabled      = bg.selected == Some( radShift )
                        ggResampleAmount.enabled   = bg.selected == Some( radResample )
                        bg.selected match {
                           case Some( `radShift` )    => ggShiftAmount.requestFocus()
                           case Some( `radResample` ) => ggResampleAmount.requestFocus()
                           case _                     =>
                        }
                     }
                     lazy val radNoTrans    = radioButton( "No Transform" )( actionBut() )
                     lazy val radShift      = radioButton( "Freq Shift" )( actionBut() )
                     lazy val radResample   = radioButton( "Resample" )( actionBut() )
                     lazy val bg = new ButtonGroup( radNoTrans, radShift, radResample )
                     lazy val ggShiftAmount = integerField( "Amount (Hz):", 0, 11025, 0 )()
                     ggShiftAmount.enabled = false
                     lazy val ggResampleAmount = integerField( "Amount (Cent):", -4800, 4800, 0 )()
                     ggResampleAmount.enabled = false
                     bg.select( radNoTrans )
                     val p = new GroupPanel {
                        linkVerticalSize( radNoTrans, radShift, radResample, ggShiftAmount, ggResampleAmount )
                        linkHorizontalSize( radNoTrans, radShift, radResample )
                        linkHorizontalSize( ggShiftAmount, ggResampleAmount )
                        theHorizontalLayout is Parallel( lbInfo, radNoTrans,
                           Sequential( radShift, ggShiftAmount ), Sequential( radResample, ggResampleAmount ))
                        theVerticalLayout is Sequential( lbInfo, radNoTrans,
                           Parallel( Baseline )( radShift, ggShiftAmount ),
                           Parallel( Baseline )( radResample, ggResampleAmount ))
                     }
                     val res = Dialog.showConfirmation( null, p.peer, "Extract", Dialog.Options.OkCancel, Dialog.Message.Question )
                     if( res == Dialog.Result.Ok ) {
                        val tracks  = trl.toList.filter( _.selected ).map( _.track )
//                        val shift   = if( radShift.selected ) Some( ggShiftAmount.integer.toDouble ) else None
                        val transform  = bg.selected match {
                           case Some( `radShift` )    => CorrelatorCore.TransformShift( ggShiftAmount.integer.toDouble )
                           case Some( `radResample` ) => CorrelatorCore.TransformResample( ggResampleAmount.integer.toDouble )
                           case _                     => CorrelatorCore.TransformNone
                        }
                        CorrelatorSetup.bounceAndExtract( tracks, span, transform )
                     }
//               }
            }
         }
      })
      val miLoadSearch = new MenuItem( "leerenull.loadsearch", action( "Load Search...", "control O" ) {
         currentDoc.foreach { implicit doc =>
            openFileDialog( "Load Search", searchFolder, filter = _.getName.endsWith( ".xml" )).foreach { file =>
               val search = CorrelatorSelector.Search.fromXMLFile( file )
               CorrelatorSelector.makeSelector( search )
            }
         }
      })
      val miSelStartToRegionEnd = new MenuItem( "leerenull.selstarttoregionend", action( "Set Selection Start to Selected Region's Stop" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val tl0  = tl
               implicit val tlv0 = tlv
               implicit val trl0 = trl
               val span = selSpan
               selectedAudioRegions.headOption.foreach { ar =>
                  val pos = ar.span.stop
                  if( span.contains( pos )) {
//println( "span = " + span + " ; ar.span = " + ar.span )
                     tlv.joinEdit( "Set span" ) { implicit ce =>
                        tlv.editSelect( ce, span.replaceStart( pos ))
                     }
                  }
               }
            }
         }
      })
      val miCleanUpOverlaps = new MenuItem( "leerenull.cleanupoverlaps", action( "Move Overlapping Regions To Free Track Space" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               MoveOverlappingRegions.perform( doc, tl, tlv, trl )
            }
         }
      })
      val miOptimizeTracksCapacities = new MenuItem( "leerenull.optimizetrackscapacities",
         action( "Optimize Selected Tracks' Capacities" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val tl0  = tl
               implicit val tlv0 = tlv
               implicit val trl0 = trl
               val span = selSpan
               tl.joinEdit( "Optimize Tracks Capacities" ) { implicit ce =>
                  type Gaga      = (AudioTrack, Option[ Seq[ Seq[ Float ]]])
                  var taken      = Set.empty[ Gaga ]
                  val withDiffs: Set[ Gaga ] =
                     selTracks.map( at => (at, at.diffusion) ).collect({
                        case (at, Some( m: MatrixDiffusion )) => (at, Some( m.matrix.toSeq ))
                        case (at, None) => (at, Option.empty[ Seq[ Seq[ Float ]]])
                     })( breakOut )
                  withDiffs.foreach { case tup @ (at, matO) =>
                     val ars  = at.trail.getRange( span )
                     val coll = (withDiffs -- taken - tup).filter { case (at2, matO2) =>
                        (matO2 == matO) && ars.forall( (ar: AudioRegion) => at2.trail.getRange( ar.span ).isEmpty )
                     }
                     val at2O = coll.toSeq.sortBy( _._1.name ).headOption
                     at2O.foreach { case tup2 @ (at2, _) =>
                        taken += tup
                        taken += tup2   // we only do that because the trail view isn't updating during the edit!
                        val tveO = trl.getElement( at ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
                        tveO.foreach( _.editDeselect( ce, ars: _* ))
                        at.trail.editRemove( ce, ars: _* )
                        at2.trail.editAdd( ce, ars: _* )
                        val tve2O = trl.getElement( at2 ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
                        tve2O.foreach( _.editSelect( ce, ars: _* ))
                     }
                  }
               }
            }
         }
      })
      val miPanSelectedRegions = new MenuItem( "leerenull.panselregions", action( "Pan Selected Regions..." ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val tl0  = tl
               implicit val tlv0 = tlv
               implicit val trl0 = trl
//               val span    = selSpan
//               val tracks  = selTracks
               val ggNumChannels = integerField( "Channels:", 2, 1024, 2 )()
               val lbBal   = label( percentString( 0 ), Some( 40 ))
               val slidBal = decimalSlider( "left", "right", 0.5, w = 144 ) { d =>
                  lbBal.text = percentString( d * 2 - 1 )
               }
               val p = new GroupPanel {
                  theHorizontalLayout is Parallel( ggNumChannels, Sequential( slidBal, lbBal ))
                  theVerticalLayout is Sequential( ggNumChannels, Parallel( Baseline )( slidBal, lbBal ))
               }
               val res = Dialog.showConfirmation( null, p.peer, "Extract", Dialog.Options.OkCancel, Dialog.Message.Question )
               if( res == Dialog.Result.Ok ) {
                  tl.joinEdit( "Pan Selected Regions" ) { implicit ce =>
                     val outChans   = ggNumChannels.integer
                     val pan        = (slidBal.decimal * 2 - 1).toFloat
                     var moreDiffs  = Set.empty[ Diffusion ]
//                     var moreTracks = Set.empty[ AudioTrack ]
                     var trackMap  = Map.empty[ AudioTrack, IndexedSeq[ AudioRegion ]]
                     val trackPrefix= "T" + percentString( pan, 0 ) + "-"
                     val tracks     = selTracks.toSet
                     val span       = selSpan
                     val arsMap     = collectAudioRegions({
                        case tup @ (at, ar) if( ar.span.overlaps( span ) && tracks.contains( at )) => tup
                     }).groupBy( _._1 ).mapValues( _.map( _._2 ))
                     arsMap.foreach { case (at, ars) =>
                        val tveO = trl.getElement( at ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
                        tveO.foreach( _.editDeselect( ce, ars: _* ))
                        at.trail.editRemove( ce, ars: _* )
                     }
                     arsMap.values.flatten.foreach { ar =>
                        val inChans = ar.audioFile.numChannels
                        val mat     = pannedMatrix( inChans, outChans, pan )
                        val diff    = provideDiffusion( mat, more = moreDiffs.toIndexedSeq )
                        val diffSeq = diff.matrix.toSeq
                        assert( mat.toSeq == diffSeq )
                        moreDiffs  += diff
                        val track   = provideAudioTrackSpace( ar.span, { at =>
                           /* val ok1 = */ at.diffusion match {
                              case Some( m: MatrixDiffusion ) if( m.matrix.toSeq == diffSeq ) => true
                              case None => true
                              case _ => false
                           }
//                           if( ok1 ) {
//                              !regionMap.getOrElse( at, IndexedSeq.empty ).exists( _.span.overlaps( ar.span ))
//                           } else false
                        }, more = trackMap, prefix = trackPrefix )
//                        moreTracks += track
                        trackMap += track -> (trackMap.getOrElse( track, IndexedSeq.empty ) :+ ar)
                        if( track.diffusion.isEmpty ) {
                           track.editDiffusion( ce, Some( diff ))
                        }
                        track.trail.editAdd( ce, ar )
                     }
                  }
               }
            }
         }
      })
      val miSelectWrongChans = new MenuItem( "leerenull.selectwrongchans",
         action( "Select Regions With Incompatible Track Diffusions" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val tl0  = tl
               implicit val tlv0 = tlv
               implicit val trl0 = trl
//               val span = selSpan
               tl.joinEdit( "Select Regions" ) { implicit ce =>
                  val arsMap = collectAudioRegions({ case tup => tup }).groupBy( _._1 ).mapValues( _.map( _._2 ))
                  arsMap.foreach { case (at, ars) =>
                     val tveO = trl.getElement( at ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
                     tveO.foreach( _.editDeselect( ce, ars: _* ))
                  }
                  val flt = arsMap.map({ case (at, ars) =>
                     at.diffusion match {
                        case Some( diff ) =>
                           val tNum = diff.numInputChannels
                           at -> ars.filter( _.audioFile.numChannels != tNum )
                        case None => (at, IndexedSeq.empty[ AudioRegion ])
                     }
                  }).filter( _._2.nonEmpty )
                  var num = 0
                  flt.foreach { case (at, ars) =>
                     val tveO = trl.getElement( at ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
                     tveO.foreach( _.editSelect( ce, ars: _* ))
                     num += ars.size
                  }
                  println( "Invalid regions: " + num )
               }
            }
         }
      })
      val miOptimizeFanatically = new MenuItem( "leerenull.optimizefanatically",
         action( "Fanatically Optimize Track Capacities", "control shift P" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               FanaticallyOptimize.perform( doc, tl, tlv, trl )
            }
         }
      })
      val miExportPDF = new MenuItem( "leerenull.exportpdf", action( "Export Timeline as PDF..." ) {
         currentDoc.foreach { implicit doc =>
            findTimelineFrame.foreach { tlf =>
               val defaultName = tlf.timelineView.timeline.name + ".pdf"
               saveFileDialog( "Export Timeline as PDF", new File( sys.props( "user.home" ), defaultName )).foreach { file =>
                  val view = tlf.tracksPanel.getViewport.getView
//                  val view = PDF.wrapTimeline( tlf, 1024, 64 )
//                  view.span = tlf.tracksPanel.timelineView.span
//                  val oldVerbose = SonagramOverview.verbose
                  val old = DefaultTrackComponent.forceFullPaint
                  DefaultTrackComponent.forceFullPaint = true
//                  SonagramOverview.verbose = true
                  try {
                     println( "Exporting...")
                     PDF.create( file, view, true, 0 )
                     println( "Done." )
                  } finally {
                     DefaultTrackComponent.forceFullPaint = old
//                     SonagramOverview.verbose = oldVerbose
                  }
               }
            }
         }
      })
      val miExportPNG = new MenuItem( "leerenull.exportpng", action( "Export Timeline as PNG..." ) {
         currentDoc.foreach { implicit doc =>
            findTimelineFrame.foreach { tlf =>
               val defaultName = tlf.timelineView.timeline.name + ".png"
               saveFileDialog( "Export Timeline as PNG", new File( sys.props( "user.home" ), defaultName )).foreach { file =>
                  val view = tlf.tracksPanel.getViewport.getView
                  val old  = DefaultTrackComponent.forceFullPaint
                  DefaultTrackComponent.forceFullPaint = true
                  try {
                     println( "Exporting..." )
                     val bImg = new BufferedImage( view.getWidth, view.getHeight, BufferedImage.TYPE_INT_ARGB )
//println( "width = " + bImg.getWidth + ", height = " + bImg.getHeight )
                     try {
                        val bg = bImg.createGraphics()
                        try {
                           view.paint( bg )
                        } finally {
                           bg.dispose()
                        }
                        ImageIO.write( bImg, "png", file )
                     } finally {
                        bImg.flush()
                     }
                     println( "Done." )
                  } finally {
                     DefaultTrackComponent.forceFullPaint = old
                  }
               }
            }
         }
      })

      // third movement
      val miLoadUeberzeichnung = new MenuItem( "leerenull.loadueber", action( "Load Überzeichnung..." ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               openFileDialog( "Load Überzeichnung", ueberzeichnungFolder, filter = _.getName.endsWith( ".xml" )).foreach { file =>
                  val settings = ThirdMovement.Settings.fromXMLFile( file )
                  ThirdMovementGUI.makeWindow( tl, tlv, doc, settings )
               }
            }
         }
      })
      val miNewUeberzeichnung = new MenuItem( "leerenull.newueber", action( "New Überzeichnung..." ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
//               implicit val tl0  = tl
//               implicit val tlv0 = tlv
//               implicit val trl0 = trl
//
//               val span       = selSpan
               val settings   = ThirdMovement.SettingsBuilder()
               ThirdMovementGUI.makeWindow( tl, tlv, doc, settings )
            }
         }
      })
      val miCreateSonogram = new MenuItem( "leerenull.sonogram", action( "Export Region as Sonogram...", "control G"  ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val _tl  = tl
               implicit val _tlv = tlv
               implicit val _trl = trl
               val ars = selectedAudioRegions
               if( ars.nonEmpty ) {
                  val ar0 = ars.head
                  val start = ars.map( _.span.start ).min
                  val stop  = ars.map( _.span.stop ).max
                  val defaultName = {
                     val names = ars.map( _.name )
                     val pre = names.foldLeft( ar0.name )( (red, name) => red.take( red.zip( name ).prefixLength( tup => tup._1 == tup._2 )))
                     val n = if( pre == "" ) "???" else pre
                     val i  = n.lastIndexOf( '.' )
                     if( i >= 0 ) n.substring( 0, i ) else n
                  } + "_" + start + "_" + stop + ".png"
                  saveFileDialog( "Save Sonogram", new File( ueberzeichnungFolder, defaultName )).foreach { file =>
                     val dlg  = progressDialog( "Creating Sonogram" )
                     val proc = SonogramOutput( ars, file ) {
                        case SonogramOutput.Success( _ ) =>
                           dlg.stop()

                        case SonogramOutput.Failure( e ) =>
                           dlg.stop()
                           e.printStackTrace()

                        case SonogramOutput.Aborted =>
                           dlg.stop()

                        case SonogramOutput.Progress( i ) => dlg.progress = i
                     }
                     dlg.start( proc )
                  }
               }
            }
         }
      })
      val miIncorporateBounce = new MenuItem( "leerenull.incbounce", action( "Incorporate Bounce..." ) {
         IncorporateBounce.showGUI()
      })
      val miConvertToMono = new MenuItem( "leerenull.converttomono", action( "Convert all tracks to mono..." ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               ConvertToMono.perform( doc, tl, trl )
            }
         }
      })
      val miSelectTracksWithSameDiffusion = new MenuItem( "leerenull.trackswithsamediff", action( "Select tracks with same diffusion", "control shift D" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               SelectTracksWithSameDiffusion.perform( trl )
            }
         }
      })
      val miMergeTimelines = new MenuItem( "leerenull.mergetimelines", action( "Merge a timeline into another..." ) {
         MergeTimelines.showGUI()
      })

      mg.add( miExtractor )
      mg.add( miLoadSearch )
      mg.add( miSelStartToRegionEnd )
      mg.addSeparator()
      mg.add( miCleanUpOverlaps )
      mg.add( miPanSelectedRegions )
      mg.addSeparator()
      mg.add( miOptimizeTracksCapacities )
      mg.add( miSelectWrongChans )
      mg.add( miOptimizeFanatically )
      mg.add( miSelectTracksWithSameDiffusion )
      mg.addSeparator()
      mg.add( miExportPDF )
      mg.add( miExportPNG )
      mg.add( miCreateSonogram )

      mg.addSeparator()
      mg.add( miLoadUeberzeichnung )
      mg.add( miNewUeberzeichnung )

      mg.addSeparator()
      mg.add( miIncorporateBounce )
      mg.add( miConvertToMono )
      mg.add( miMergeTimelines )

      mf.add( mg )
   }
}
