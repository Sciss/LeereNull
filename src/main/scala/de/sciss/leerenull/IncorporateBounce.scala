/*
 *  IncorporateBounce.scala
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

import de.sciss.app.AbstractApplication
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.JOptionPane
import swing.{BorderPanel, ListView}
import java.io.File
import eu.flierl.grouppanel.GroupPanel
import de.sciss.common.BasicWindowHandler
import swing.event.SelectionChanged
import collection.JavaConversions
import de.sciss.kontur.gui.TimelineFrame
import de.sciss.kontur.session.{AudioRegion, AudioFileElement, BasicTimeline, Session}
import de.sciss.strugatzki.aux.{ProcessorCompanion, Processor}
import actors.Actor

object IncorporateBounce extends ProcessorCompanion with GUIGoodies with KonturGoodies {
   type PayLoad = Unit

   def showGUI() {
      val app  = AbstractApplication.getApplication
      val dh   = app.getDocumentHandler
      val tls  = IIdxSeq.tabulate( dh.getDocumentCount )( dh.getDocument ).flatMap {
         case s: Session =>
            val tls1 = s.timelines.toList.toIndexedSeq
            tls1.collect {
               case btl: BasicTimeline => (s, btl)
            }
         case _ => IIdxSeq.empty[ (Session, BasicTimeline) ]
      }

      if( tls.size < 2 ) {
         println( "There must be at least two timelines" )
         return
      }

      val names = tls.map {
         case (doc, tl) => doc.name.getOrElse( "<untitled>" ) + " : " + tl.name
      }

      var preTL      = tls( 0 )
      var postTL     = tls( 1 )
      var bncAFE     = Option.empty[ AudioFileElement ]
      var bncARs     = IndexedSeq.empty[ AudioRegion ]
      var bncDir     = LeereNull.bounceFolder
      var ok         = false

      val ggOff            = timeField( "Bounce Offset:", 0.0, 600.0, 0.0 )( _ => () )
      val ggFolder         = textField( bncDir.getAbsolutePath, 30 )( str => bncDir = new File( str ))
      val lbFolder         = label( "Synthetic Audio Files Folder:" )
      val ggSelectFolder   = button( "^" ) { b =>
         val res = openFileDialog( "Select folder", new File( ggFolder.text, "Ignore" ))
         res.foreach { f =>
            bncDir = f.getParentFile
            ggFolder.text = bncDir.getAbsolutePath
         }
      }

      def checkPostTimeline() {
         ok = false
         val (_, tl) = postTL
         JavaConversions.asScalaIterator( app.getWindowHandler.getWindows ).collect({
            case tlf: TimelineFrame if( tlf.timelineView.timeline == tl ) => tlf
         }).toList.headOption match {
            case Some( tlf ) =>
               val tlv = tlf.timelineView
               val trl = tlf.tracksPanel
               bncARs = selectedAudioRegions( tl, tlv, trl ).filterNot( _.muted )
               if( bncARs.nonEmpty ) {
                  val afs = bncARs.map( _.audioFile ).toSet.toSeq
                  afs match {
                     case Seq( bnc ) =>
                        bncAFE = Some( bnc )
                        val over = bncARs.sliding( 2, 1 ).collect {
                           case Seq( a, b ) if( a.span overlaps b.span ) => (a, b)
                        }
                        if( over.isEmpty ) {
                           val RegOff = """.+\@(\d{1,2})\'(\d{2})".+""".r
                           bnc.name match {
                              case RegOff( mins, secs ) =>
                                 ggOff.decimal = mins.toInt * 60 + secs.toInt
                              case _ =>
                           }
                           ok = true
                           println( "Selection ok." )

                        } else {
                           println( "Regions in bounce cannot overlap. Found the following overlaps:" )
                           over.take( 10 ).foreach {
                              case (a, b) => println( a.name + " : " + a.span + " -- " + b.name + " : " + b.span )
                           }
                           if( over.size > 10 ) println( "..." )
                        }

                     case _ =>
                        println( "Selection in post timeline must come from exactly one audio file (the bounce)" )
                        println( "However, it currently contains " + afs.size + " files:" )
                        afs.take( 10 ).foreach( e => println( e.name ))
                        if( afs.size > 10 ) println( "..." )
                  }

               } else {
                  println( "Post timeline does not have selected audio regions" )
               }
            case _ => println( "Could not find window belong to timeline " + tl )
         }
      }

      val listPre  = new ListView( names ) {
         listenTo( selection )
         reactions += {
            case SelectionChanged( _ ) => selection.indices.headOption.foreach { idx =>
               preTL = tls( idx )
            }
         }
      }
      val listPost = new ListView( names ) {
         listenTo( selection )
         reactions += {
            case SelectionChanged( _ ) => selection.indices.headOption.foreach { idx =>
               postTL = tls( idx )
               checkPostTimeline()
            }
         }
      }

      listPre.selectIndices(  0 )
      listPost.selectIndices( 1 )

      lazy val pane2 = new GroupPanel {
         theHorizontalLayout is Sequential(
            Parallel( ggOff, Sequential(
               lbFolder, ggFolder, ggSelectFolder )
            )
         )
         theVerticalLayout is Sequential(
            ggOff,
            Parallel( Baseline )( lbFolder, ggFolder, ggSelectFolder )
         )
      }

      val pane = new BorderPanel {
         add( listPre, BorderPanel.Position.West )
         add( listPost, BorderPanel.Position.East )
         add( pane2, BorderPanel.Position.South )
      }

      val op = new JOptionPane( pane.peer, JOptionPane.INFORMATION_MESSAGE, JOptionPane.OK_CANCEL_OPTION )
      BasicWindowHandler.showDialog( op, null, "Incorporate Bounce" ) match {
         case JOptionPane.OK_OPTION =>
            if( ok ) {
               println( "Preparing rendering..." )
               render( bncARs, bncDir, preTL._2 )
            } else {
               println( "Wooop. Settings wrong. Try again." )
            }
         case _ =>
      }
   }

   def render( bncARs: IndexedSeq[ AudioRegion ], bncDir: File, preTL: BasicTimeline ) {
      val dlg  = progressDialog( "Incorporate Bounce" )
      val proc = IncorporateBounce( bncARs, bncDir, preTL ) {
         case IncorporateBounce.Success( _ ) =>
            dlg.stop()

         case IncorporateBounce.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()

         case IncorporateBounce.Aborted =>
            dlg.stop()

         case IncorporateBounce.Progress( i ) => dlg.progress = i
      }
      dlg.start( proc )
   }

   def apply( bncARs: IndexedSeq[ AudioRegion ], bncDir: File, preTL: BasicTimeline )( observer: Observer ) : IncorporateBounce =
      new IncorporateBounce( observer, bncARs, bncDir, preTL )
}
class IncorporateBounce( protected val observer: IncorporateBounce.Observer,
                         bncARs: IndexedSeq[ AudioRegion ], bncDir: File, preTL: BasicTimeline )
extends Processor {
   import IncorporateBounce._

   protected val companion = IncorporateBounce

   protected def body() : Result = {
      Thread.sleep( 3000 )
      Success( () )
   }

   protected val Act = new Actor {
      def act() {
         ProcT.start()
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
                  aborted()
               case res: Progress =>
                  observer( res )
               case res @ Aborted =>
                  result = res
               case res: Failure =>
                  result = res
               case res: Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }
}