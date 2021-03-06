/*
 *  CorrelatorCore.scala
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

import de.sciss.kontur.session.Session
import de.sciss.processor.Processor
import de.sciss.processor.Processor.Aborted
import de.sciss.swingplus.GroupPanel
import collection.breakOut
import javax.swing.table.DefaultTableModel
import scala.util.{Failure, Success}
import swing.{BorderPanel, ScrollPane, Table, Swing}
import java.util.{Locale, Date}
import java.io.File
import xml.{NodeSeq, XML}
import java.text.DateFormat
import de.sciss.app.AbstractWindow
import de.sciss.kontur.gui.AppWindow
import java.awt.BorderLayout
import de.sciss.strugatzki.{FeatureExtraction, FeatureCorrelation}
import FeatureCorrelation.{Config => CSettings, Match}
import FeatureExtraction.{Config => ESettings}

object CorrelatorSelector extends GUIGoodies with KonturGoodies with NullGoodies {
   var verbose    = false
   var autosave   = true

   object Search {
      private val dateFormat = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.FULL, Locale.US )
      def fromXMLFile( file: File ) : Search = fromXML( XML.loadFile( file ))
      def fromXML( xml: NodeSeq ) : Search = {
         val date       = dateFormat.parse( (xml \ "date").text )
         val offset     = {xml \ "offset"}.text.toLong
         val settings   = CSettings.fromXML( xml \ "settings" )
         val matches: IndexedSeq[ Match ] = ((xml \ "matches") \ "match").map( Match.fromXML( _ ))( breakOut )
         val master     = {
            val e = xml \ "master"
            if( e.isEmpty ) None else Some( Match.fromXML( e ))
         }
         val metas      = {
            val res: IndexedSeq[ ESettings ] = (xml \ "metas" \ "feature").map( ESettings.fromXML( _ ))( breakOut )
            if( res.nonEmpty ) res else {
               IndexedSeq( ESettings.fromXMLFile( settings.metaInput ))
            }
         }
//         val shift = {
//            val e = (xml \ "shift").text
//            if( e == "" ) None else Some( e.toDouble )
//         }
         val transform = CorrelatorCore.Transform.fromXML( xml \ "transform" )
         Search( date, offset, settings, matches, metas, master, transform )
      }
   }
   final case class Search( creation: Date, offset: Long, settings: CSettings, matches: IndexedSeq[ Match ],
                            metas: IndexedSeq[ ESettings ], master: Option[ Match ],
                            transform: CorrelatorCore.Transform ) {
      def toXML = <search>
  <date>{Search.dateFormat.format( creation )}</date>
  <offset>{offset}</offset>
  <settings>{settings.toXML.child}</settings>
  <matches>{matches.map(_.toXML)}</matches>
  <metas>{metas.map( _.toXML )}</metas>
  {master match { case Some( m ) => <master>{m.toXML.child}</master>; case None => Nil }}
  <transform>{transform.toXML.child}</transform>
</search>
   }

   /**
    * @param   offset   the offset of the search input with respect to its
    *                   appearance in the main timeline
    */
   def beginSearch( offset: Long, settings: CSettings, metas: IndexedSeq[ ESettings ],
                    master: Option[ Match ], transform: CorrelatorCore.Transform )
                  ( implicit doc: Session ): Unit = {
      if( verbose ) println( settings )

      val dlg  = progressDialog( "Correlating with database" )
      val tim  = new Date()
      val fc   = FeatureCorrelation( settings )
      fc.addListener {
         case Processor.Result(_, Success(res)) =>
            dlg.stop()
            if( verbose ) {
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
            }
            val search = Search( tim, offset, settings, res, metas, master, transform )
            if( autosave ) saveSearch( search )
            Swing.onEDT( makeSelector( search ))

         case Processor.Result(_, Failure(Aborted())) =>
           dlg.stop()

         case Processor.Result(_, Failure(e)) =>
            dlg.stop()
            e.printStackTrace()

         case prog @ Processor.Progress(_, _) => dlg.progress = prog.toInt
      }
      dlg.start( fc )
   }

   def saveSearch( search: Search ): Unit = {
      val id   = plainName( search.settings.metaInput ).filter( _.isLetterOrDigit ).take( 16 )
//      val df   = new SimpleDateFormat( "yyMMdd'_'HHmmss'_" + id + ".xml'", Locale.US )
//      val f    = new File( LeereNull.searchFolder, df.format( search.creation ))
      val f = stampedFile( LeereNull.searchFolder, id, ".xml", search.creation )
      XML.save( f.getAbsolutePath, search.toXML, "UTF-8", xmlDecl = true)
   }

   def makeSelector( search: Search )( implicit doc: Session ): Unit = {
//      val tls  = doc.timelines
//      implicit val tl = tls.tryEdit( "Add Correlator Timeline" ) { implicit ce =>
//         implicit val tl = BasicTimeline.newEmpty( doc )
////         tl.span  = ar0.span
//         tl.name  = uniqueName( tls, "$Correlator" )
//         tls.editInsert( ce, tls.size, tl )
////         placeStereo( ar0, "$" )
//         tl
//      }

      def rowStrings( m: Match ) : Seq[ String ] = Seq(
         percentString( m.sim ), plainName( m.file ), timeString( m.punch, 44100 / search.transform.timeScale ),
         decibelString( ampdb( m.boostIn )), decibelString( ampdb( m.boostOut ))
      )

      val rowData: Array[ Array[ AnyRef ]] = search.matches.map( m => {
         Array[ AnyRef ]( rowStrings( m ): _* )
      })( breakOut )
      val columnNames   = Array[ AnyRef ]( "Sim", "File", "Span", "Gain In", "Gain Out" )
      val table         = new Table {
         peer.putClientProperty( "JComponent.sizeVariant", "small" )
         showGrid  = true
         peer.getTableHeader.setVisible( true )
         peer.setModel( new DefaultTableModel( rowData, columnNames ) {
            override def isCellEditable( row: Int, col: Int ) = false
         })
         val cm = peer.getColumnModel
         Seq( 32, 112, 144, 36, 36 ).zipWithIndex.foreach { case (w, idx) =>
            cm.getColumn( idx ).setPreferredWidth( w )
         }

//         listenTo( selection )
         selection.elementMode   = Table.ElementMode.Row
         selection.intervalMode  = Table.IntervalMode.Single
      }
      val scroll = new ScrollPane( table ) {
         horizontalScrollBarPolicy  = ScrollPane.BarPolicy.Never
         verticalScrollBarPolicy    = ScrollPane.BarPolicy.Always
      }

//      val tlf = TimelineFrame2 { f =>
//         println( "Bye..." )
//         f.dispose()
//      }

      val a = new AppWindow( AbstractWindow.REGULAR ) {
         setTitle( "Search results" )
         setLocationRelativeTo( null )
      }

      val lbInfo = label( "<html>Search conducted for " + plainName( search.settings.metaInput ) + " at " +
         search.creation + (search.master match {
         case Some( m ) => rowStrings( m ).mkString( "<br>Master: ", " ", "" )
         case None => ""
      }) + "</html>" )

      val butSelectMatch = button( "Select match" ) { b =>
         table.selection.rows.headOption.foreach { row =>
//            println( search.matches( row ))
            CorrelatorCore.makeMatchEditor( search, row )
         }
      }

      val panel = new GroupPanel {
         horizontal = Seq( lbInfo, butSelectMatch )
         vertical = Par( Baseline )( lbInfo, butSelectMatch )
      }

      val bp = new BorderPanel {
         add( panel, BorderPanel.Position.North )
         add( scroll, BorderPanel.Position.Center )
      }

      val cp = a.getContentPane
      cp.add( bp.peer, BorderLayout.CENTER )
      a.pack()
      a.setVisible( true )
//      tlf.bottomPanel = Some( bp )
//      tlf.pack() // AndSetMinimum()
   }
}