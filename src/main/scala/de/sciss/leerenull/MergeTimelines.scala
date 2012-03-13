/*
 *  MergeTimelines.scala
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
import swing.event.SelectionChanged
import javax.swing.JOptionPane
import de.sciss.common.BasicWindowHandler
import swing.{BorderPanel, ListView}
import eu.flierl.grouppanel.GroupPanel
import de.sciss.strugatzki.Span
import de.sciss.kontur.session.{Diffusion, MatrixDiffusion, AudioTrack, Session, BasicTimeline, AudioFileElement, AudioRegion}

object MergeTimelines extends GUIGoodies with KonturGoodies {
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
         case (doc, tl) => doc.displayName + " : " + tl.name
      }

      var srcTL   = tls( 0 )
      var tgtTL   = tls( 1 )
      var ok      = false

      def checkPostTimeline() {
         ok = srcTL != tgtTL
      }

      val listPre  = new ListView( names ) {
         listenTo( selection )
         reactions += {
            case SelectionChanged( _ ) => selection.indices.headOption.foreach { idx =>
               srcTL = tls( idx )
            }
         }
      }
      val listPost = new ListView( names ) {
         listenTo( selection )
         reactions += {
            case SelectionChanged( _ ) => selection.indices.headOption.foreach { idx =>
               tgtTL = tls( idx )
               checkPostTimeline()
            }
         }
      }

      listPre.selectIndices(  0 )
      listPost.selectIndices( 1 )

      val lbSrcPrefix   = label( "Source track name prefix:" )
      val lbSrcSuffix   = label( "Source track name suffix:" )
      val lbTgtPrefix   = label( "Target track name prefix:" )
      val lbTgtSuffix   = label( "Target track name suffix:" )
      val ggSrcPrefix   = textField( "", 8 )()
      val ggSrcSuffix   = textField( "", 8 )()
      val ggTgtPrefix   = textField( "", 8 )()
      val ggTgtSuffix   = textField( "", 8 )()

      lazy val pane2 = new GroupPanel {
         theHorizontalLayout is Sequential(
            Parallel( lbSrcPrefix, lbSrcSuffix ),
            Parallel( ggSrcPrefix, ggSrcSuffix ),
            Parallel( lbTgtPrefix, lbTgtSuffix ),
            Parallel( ggTgtPrefix, ggTgtSuffix )
         )
         theVerticalLayout is Sequential(
            Parallel( Baseline )( lbSrcPrefix, ggSrcPrefix, lbTgtPrefix, ggTgtPrefix ),
            Parallel( Baseline )( lbSrcSuffix, ggSrcSuffix, lbTgtSuffix, ggTgtSuffix )
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
               perform( srcTL._1, srcTL._2, tgtTL._1, tgtTL._2,
                  ggSrcPrefix.text, ggSrcSuffix.text, ggTgtPrefix.text, ggTgtSuffix.text )
            } else {
               println( "Wooop. Settings wrong. Try again." )
            }
         case _ =>
      }
   }

   def perform( srcDoc: Session, srcTL: BasicTimeline, tgtDoc: Session, tgtTL: BasicTimeline,
                srcPre: String, srcSuff: String, tgtPre: String, tgtSuff: String ) {
      require( srcTL != tgtTL )

      val srcTLLen = srcTL.span.length
      val tgtTLLen = tgtTL.span.length

      implicit val ce = tgtTL.editBegin( "Merge timeline" )
      implicit val doc1 = tgtDoc
      var ceOk = false
      try {
         if( srcTLLen > tgtTLLen ) {
            tgtTL.editSpan( ce, Span( srcTL.span.start, srcTL.span.start + tgtTLLen ))
         }
         val srcTracks = srcTL.tracks.toList.collect { case at: AudioTrack => at }
         var newDiffs = IIdxSeq.empty[ Diffusion ]
         var newFiles = IIdxSeq.empty[ AudioFileElement ]
         val trackOff = tgtTL.tracks.size

         if( tgtPre != "" || tgtSuff != "" ) {
            tgtTL.tracks.foreach { t =>
               t.editor.foreach { ed =>
                  ed.editRename( ce, tgtPre + t.name + tgtSuff )
               }
            }
         }

         srcTracks.zipWithIndex.foreach { case (atSrc, idx) =>
            val atTgt = new AudioTrack( tgtDoc )
            if( srcDoc == tgtDoc ) {
               atTgt.diffusion = atSrc.diffusion
            } else atSrc.diffusion match {
               case Some( m: MatrixDiffusion ) =>
                  val diff = provideDiffusion( m.matrix, more = newDiffs )
                  atTgt.diffusion = Some( diff )
                  newDiffs :+= diff
               case _ =>
            }
            val arsIn = atSrc.trail.getAll()
            val arsOut = if( srcDoc == tgtDoc ) arsIn else {
               val files = arsIn.map( _.audioFile.path ).toSet.toSeq
               files.foreach { f =>
                  val prov = provideAudioFile( f, newFiles )
                  newFiles :+= prov
               }
               arsIn.map( ar => ar.copy( audioFile = provideAudioFile( ar.audioFile.path, newFiles )))
            }
            atTgt.trail.add( arsOut: _* )
            atTgt.name = srcPre + atSrc.name + srcSuff
            tgtTL.tracks.editInsert( ce, trackOff + idx, atTgt )
         }

         ceOk = true
      } finally {
         if( ceOk ) tgtTL.editEnd( ce ) else tgtTL.editCancel( ce )
      }
   }
}
