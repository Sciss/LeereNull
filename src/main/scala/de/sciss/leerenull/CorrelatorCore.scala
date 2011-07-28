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

import de.sciss.strugatzki.FeatureCorrelation
import de.sciss.kontur.session.{BasicTimeline, Session}
import collection.breakOut
import javax.swing.table.{DefaultTableModel, TableModel}
import swing.{GridPanel, BorderPanel, ScrollPane, Table, Swing}
import eu.flierl.grouppanel.GroupPanel

object CorrelatorCore extends GUIGoodies with KonturGoodies with NullGoodies {
   def beginSearch( settings: FeatureCorrelation.Settings )( implicit doc: Session ) {
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
            Swing.onEDT( makeCorrelator( res ))

         case FeatureCorrelation.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()

         case FeatureCorrelation.Aborted =>
            dlg.stop()

         case FeatureCorrelation.Progress( i ) => dlg.progress = i
      }
      dlg.start( fc )
   }

   def makeCorrelator( matches: IndexedSeq[ FeatureCorrelation.Match ])( implicit doc: Session ) {
      val tls  = doc.timelines
      implicit val tl = tls.tryEdit( "Add Correlator Timeline" ) { implicit ce =>
         implicit val tl = BasicTimeline.newEmpty( doc )
//         tl.span  = ar0.span
         tl.name  = uniqueName( tls, "$Correlator" )
         tls.editInsert( ce, tls.size, tl )
//         placeStereo( ar0, "$" )
         tl
      }

      val rowData: Array[ Array[ AnyRef ]] = matches.map( m => {
         Array[ AnyRef ]( percentString( m.sim ), plainName( m.file ), timeString( m.punch ),
            decibelString( ampdb( m.boostIn )), decibelString( ampdb( m.boostOut )))
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

      val tlf = TimelineFrame2 { f =>
         println( "Bye..." )
         f.dispose()
      }

      val butSelectMatch = button( "Select match" ) { b =>
         table.selection.rows.headOption.foreach { row =>
            println( matches( row ))
         }
      }

      val panel = new GroupPanel {
         theHorizontalLayout is Sequential( butSelectMatch )
         theVerticalLayout is Parallel( Baseline )( butSelectMatch )
      }

      val bp = new BorderPanel {
         add( panel, BorderPanel.Position.North )
         add( scroll, BorderPanel.Position.South )
      }

      tlf.bottomPanel = Some( bp )
      tlf.pack() // AndSetMinimum()
   }
}