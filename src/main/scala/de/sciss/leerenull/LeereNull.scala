/*
 *  LeereNull.scala
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

import de.sciss.kontur.{Main => Kontur}
import de.sciss.gui.{MenuItem, MenuGroup}
import de.sciss.kontur.gui.{TimelineFrame}
import de.sciss.app.AbstractWindow
import java.awt.{BorderLayout, EventQueue}
import de.sciss.kontur.session.{AudioRegion, BasicTimeline, Session}
import eu.flierl.grouppanel.GroupPanel
import javax.swing.{JDialog, JOptionPane, JFrame, JLabel, JPanel, WindowConstants}
import java.util.Properties
import java.io.{File, FileInputStream}
import de.sciss.strugatzki.{FeatureExtraction, Span => SSpan, FeatureCorrelation}
import swing.{Swing, Dialog, FlowPanel, ProgressBar, Label}

object LeereNull extends Runnable with GUIGoodies with KonturGoodies {
   var databaseFolder: File = null

   def main( args: Array[ String ]) {
      val file = new File( "leerenull-settings.xml" )
      databaseFolder = if( file.isFile ) {
         val prop = new Properties()
         val is = new FileInputStream( file )
         prop.loadFromXML( is )
         is.close()
         new File( prop.getProperty( "database" ))
      } else new File( sys.props( "user.home" ), "leerenullen" )
      EventQueue.invokeLater( this )
   }

   def run() {
      val app  = new Kontur( Array() )
      val mf   = app.getMenuFactory
      val mg   = new MenuGroup( "leerenull", "Leere Null" )
      val miExtractor = new MenuItem( "leerenull.extractor", action( "Extractor...", "control E" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val tl0  = tl
               implicit val tlv0 = tlv
               implicit val trl0 = trl

               cutTheCheese( selectedAudioRegions, selSpan ) match {
                  case IndexedSeq( ar ) => prepareCorrelator( ar )
                  case _ => message( "Must have exactly one audio region selected" )
               }
            }
         }
      })
      mf.add( mg )
      mg.add( miExtractor )
   }

   def prepareCorrelator( ar: AudioRegion )( implicit doc: Session ) {
      val afPath  = ar.audioFile.path
      val afName  = afPath.getName
      val plain   = plainName( afPath )
      val meta    = metaFile( plain )
      if( meta.isFile ) {
         makeCorrelator( ar, meta )
      } else {
         val message = "<html>The audio file associated with the selected region<br>" +
            "<tt>" + afName + "</tt><br>is not in the feature database.<br>" +
            "<B>Extract features now?</B></html>"
         val res = Dialog.showConfirmation( null, message, "Meta data", Dialog.Options.OkCancel, Dialog.Message.Question )
         if( res == Dialog.Result.Ok ) {
            extract( afPath ) { success =>
               if( success ) Swing.onEDT( makeCorrelator( ar, meta ))
            }
         }
      }
   }

   def plainName( f: File ) : String = {
      val n  = f.getName
      val i  = n.lastIndexOf( '.' )
      val n1 = if( i >= 0 ) n.substring( 0, i ) else n
      if( n1.endsWith( "_feat" )) n1.dropRight( 5 ) else n1
   }

   def metaFile( plain: String )    : File = new File( databaseFolder, plain + "_feat.xml" )
   def featureFile( plain: String ) : File = new File( databaseFolder, plain + "_feat.aif" )

   def extract( afPath: File )( whenDone: Boolean => Unit ) {
      val settings            = new FeatureExtraction.SettingsBuilder
      settings.audioInput     = afPath
      val plain               = plainName( afPath )
      settings.featureOutput  = featureFile( plain )
      settings.metaOutput     = Some( metaFile( plain ))

      val dlg = progressDialog( "Extracting features..." )
      val fe = FeatureExtraction( settings ) {
         case FeatureExtraction.Success =>
            dlg.stop()
            whenDone( true )
         case FeatureExtraction.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()
            whenDone( false )
         case FeatureExtraction.Aborted =>
            dlg.stop()
            whenDone( false )
         case FeatureExtraction.Progress( i ) => dlg.progress = i
      }
      dlg.start( fe )
   }

   def makeCorrelator( ar: AudioRegion, meta: File )( implicit doc: Session ) {
      val tls  = doc.timelines
      val ar0  = ar.move( -ar.span.start )
      implicit val tl = tls.tryEdit( "Add Extractor Timeline" ) { implicit ce =>
         implicit val tl = BasicTimeline.newEmpty( doc )
         tl.span  = ar0.span
         tl.name  = uniqueName( tls, "$Extractor" )
         tls.editInsert( ce, tls.size, tl )
         placeStereo( ar0, "$" )
         tl
      }

      val settings            = new FeatureCorrelation.SettingsBuilder
      settings.punchIn        = FeatureCorrelation.Punch( SSpan( 0L, 0L ))    // our indicator that punchIn hasn't been set yet
      settings.databaseFolder = databaseFolder
      settings.metaInput      = meta

      val tlf = new TimelineFrame( doc, tl )
      tlf.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
      tlf.addListener( new AbstractWindow.Listener {
         def windowClosing( e: AbstractWindow.Event ) {
            println( "Wooha" )
            tlf.dispose()
         }

         def windowClosed(p1: AbstractWindow.Event) {}
         def windowDeactivated(p1: AbstractWindow.Event) {}
         def windowDeiconified(p1: AbstractWindow.Event) {}
         def windowOpened(p1: AbstractWindow.Event) {}
         def windowActivated(p1: AbstractWindow.Event) {}
         def windowIconified(p1: AbstractWindow.Event) {}
      })

      implicit val tlv = tlf.timelineView

      val lbPunchIn  = label( "<not set>", Some( 160 ))
      val lbPunchOut = label( "<not set>", Some( 160 ))
      val lbWeightIn = label( "50%", Some( 32 ))
      val lbWeightOut= label( "50%", Some( 32 ))
      val ggWeightIn = decimalSlider( "spect", "temp", initial = 0.5 ) { value =>
         settings.punchIn = settings.punchIn.copy( temporalWeight = value.toFloat )
         lbWeightIn.text = percentString( value )
      }
      ggWeightIn.decimal = 0.5
      val ggWeightOut = decimalSlider( "spect", "temp", initial = 0.5 ) { value =>
         settings.punchOut.foreach { po =>
            settings.punchOut = Some( po.copy( temporalWeight = value.toFloat ))
            lbWeightOut.text = percentString( value )
         }
      }
      ggWeightOut.decimal = 0.5
//      val lbNumMatches = label( "# Matches:" )
//      val lbNumPerFile = label( "# Per File:" )
      settings.maxBoost = dbamp( 18 ).toFloat
      val lbMaxBoost  = label( "18 dB", Some( 40 ))
      val ggMaxBoost = decimalSlider( "Max boost:", "", initial = 18.0/32 ) { value =>
         val db  = value * 32
         val amp = ampdb( db )
         settings.maxBoost = amp.toFloat
         lbMaxBoost.text = decibelString( db )
      }

//      def selSpan2 = {
//         val res = tlf.timelineView.selection.span
//println( res )
//         res
//      }

      val arDelta = ar.offset

      val butToIn = button( "→ In" ) { b =>
         val sp = selSpan.shift( arDelta )
         if( !sp.isEmpty ) {
            settings.punchIn  = settings.punchIn.copy( span = sp )
//println( "aqui" )
            val str = timeString( sp )
            lbPunchIn.text    = str
//println( "Jo chucka" + str )
         }
      }
      val butToOut = button( "→ Out" ) { b =>
         val sp = selSpan.shift( arDelta )
         if( !sp.isEmpty ) {
            settings.punchOut  = Some( FeatureCorrelation.Punch(
               sp, settings.punchOut.map( _.temporalWeight ).getOrElse( ggWeightOut.decimal.toFloat )))
            lbPunchOut.text    = timeString( sp )
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

      settings.numMatches = 10
      val ggNumMatches = integerField( "# Matches:", 1, 1000, initial = 10 ) { i =>
         settings.numMatches = i
      }
      settings.numPerFile = 2
      val ggNumPerFile = integerField( "# Per File:", 1, 1000, initial = 2 ) { i =>
         settings.numPerFile = i
      }

      settings.minPunch = secsToFrames( 2.0 )
      settings.maxPunch = secsToFrames( 3.0 )
      val ggMinPunch = timeField( "Min dur:", 0.0, 60.0, 2.0 ) { secs =>
         settings.minPunch = secsToFrames( secs )
      }
      val ggMaxPunch = timeField( "Max dur:", 0.0, 60.0, 3.0 ) { secs =>
         settings.maxPunch = secsToFrames( secs )
      }

      val butSearch = button( "Start searching..." ) { b =>
         if( !settings.punchIn.span.isEmpty && settings.punchOut.isDefined ) {
            beginSearch( settings )
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

      val cp = tlf.getContentPane
//      val cp1 = new JPanel( new BorderLayout )
//      tlf.setContentPane( cp1 )
//      cp1.add( cp, BorderLayout.CENTER )
//      cp1.add( panel.peer, BorderLayout.SOUTH )
////      tlf.revalidate()
      cp.add( panel.peer, BorderLayout.SOUTH )

      tlf.pack()
      tlf.getWindow match {
         case f: JFrame => f.setMinimumSize( f.getSize )
      }
   }

   def beginSearch( settings: FeatureCorrelation.Settings ) {
      println( settings )

      val dlg  = progressDialog( "Correlating with database" )
      val fc   = FeatureCorrelation( settings ) {
         case FeatureCorrelation.Success( res ) =>
            dlg.stop()
            println( "Done. " + res.size + " entries:" )
            res.foreach { m =>
               println(  "\nFile      : " + m.file.getAbsolutePath +
                         "\nSimilarity: " + (m.sim * 100) +
                         "\nSpan start: " + m.punch.start +
                         "\nBoost in  : " + ampdb( m.boostIn ))
               if( settings.punchOut.isDefined ) {
                  println( "Span stop : " + m.punch.stop +
                         "\nBoost out : " + ampdb( m.boostOut ))
               }
            }

         case FeatureCorrelation.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()

         case FeatureCorrelation.Aborted =>
            dlg.stop()

         case FeatureCorrelation.Progress( i ) => dlg.progress = i
      }
      dlg.start( fc )
   }
}