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

      val lbSrcTrkPre   = label( "Source track name prefix:" )
      val lbSrcTrkSuff  = label( "Source track name suffix:" )
      val lbTgtTrkPre   = label( "Target track name prefix:" )
      val lbTgtTrkSuff  = label( "Target track name suffix:" )
      val ggSrcTrkPre   = textField( "", 8 )()
      val ggSrcTrkSuff  = textField( "", 8 )()
      val ggTgtTrkPre   = textField( "", 8 )()
      val ggTgtTrkSuff  = textField( "", 8 )()
      val lbSrcRegPre   = label( "Source region name prefix:" )
      val lbSrcRegSuff  = label( "Source region name suffix:" )
      val lbTgtRegPre   = label( "Target region name prefix:" )
      val lbTgtRegSuff  = label( "Target region name suffix:" )
      val ggSrcRegPre   = textField( "", 8 )()
      val ggSrcRegSuff  = textField( "", 8 )()
      val ggTgtRegPre   = textField( "", 8 )()
      val ggTgtRegSuff  = textField( "", 8 )()

      lazy val pane2 = new GroupPanel {
         theHorizontalLayout is Sequential(
            Parallel( lbSrcTrkPre, lbSrcTrkSuff, lbSrcRegPre, lbSrcRegSuff ),
            Parallel( ggSrcTrkPre, ggSrcTrkSuff, ggSrcRegPre, ggSrcRegSuff ),
            Parallel( lbTgtTrkPre, lbTgtTrkSuff, lbTgtRegPre, lbTgtRegSuff ),
            Parallel( ggTgtTrkPre, ggTgtTrkSuff, ggTgtRegPre, ggTgtRegSuff )
         )
         theVerticalLayout is Sequential(
            Parallel( Baseline )( lbSrcTrkPre,  ggSrcTrkPre,  lbTgtTrkPre,  ggTgtTrkPre  ),
            Parallel( Baseline )( lbSrcTrkSuff, ggSrcTrkSuff, lbTgtTrkSuff, ggTgtTrkSuff ),
            Parallel( Baseline )( lbSrcRegPre,  ggSrcRegPre,  lbTgtRegPre,  ggTgtRegPre  ),
            Parallel( Baseline )( lbSrcRegSuff, ggSrcRegSuff, lbTgtRegSuff, ggTgtRegSuff )
         )
      }

      val pane = new BorderPanel {
         add( listPre, BorderPanel.Position.West )
         add( listPost, BorderPanel.Position.East )
         add( pane2, BorderPanel.Position.South )
      }

      val op = new JOptionPane( pane.peer, JOptionPane.INFORMATION_MESSAGE, JOptionPane.OK_CANCEL_OPTION )
      BasicWindowHandler.showDialog( op, null, "Merge timelines" ) match {
         case JOptionPane.OK_OPTION =>
            if( ok ) {
               perform( srcTL._1, srcTL._2, tgtTL._1, tgtTL._2,
                  ggSrcTrkPre.text, ggSrcTrkSuff.text, ggTgtTrkPre.text, ggTgtTrkSuff.text,
                  ggSrcRegPre.text, ggSrcRegSuff.text, ggTgtRegPre.text, ggTgtRegSuff.text )
            } else {
               println( "Wooop. Settings wrong. Try again." )
            }
         case _ =>
      }
   }

   def perform( srcDoc: Session, srcTL: BasicTimeline, tgtDoc: Session, tgtTL: BasicTimeline,
                srcTrkPre: String, srcTrkSuff: String, tgtTrkPre: String, tgtTrkSuff: String,
                srcRegPre: String, srcRegSuff: String, tgtRegPre: String, tgtRegSuff: String ) {
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

         if( tgtTrkPre != "" || tgtTrkSuff != "" ) {
            tgtTL.tracks.foreach { t =>
               t.editor.foreach { ed =>
                  ed.editRename( ce, tgtTrkPre + t.name + tgtTrkSuff )
               }
            }
         }

         if( tgtRegPre != "" || tgtRegSuff != "" ) {
            tgtTL.tracks.foreach {
               case at: AudioTrack =>
                  val ars = at.trail.getAll()
                  at.trail.editRemove( ce, ars: _* )
                  val arsRen = ars.map( ar => ar.copy( name = tgtRegPre + ar.name + tgtRegSuff ))
                  at.trail.editAdd( ce, arsRen: _* )

               case _ =>
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
               arsIn.map( ar => ar.copy(
                  audioFile = provideAudioFile( ar.audioFile.path, newFiles ),
                  name = srcRegPre + ar.name + srcRegSuff
               ))
            }
            atTgt.trail.add( arsOut: _* )
            atTgt.name = srcTrkPre + atSrc.name + srcTrkSuff
            tgtTL.tracks.editInsert( ce, trackOff + idx, atTgt )
         }

         ceOk = true
      } finally {
         if( ceOk ) tgtTL.editEnd( ce ) else tgtTL.editCancel( ce )
      }
   }
}
