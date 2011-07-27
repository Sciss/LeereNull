/*
 *  KonturGoodies.scala
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

import de.sciss.kontur.gui.{TimelineFrame, BasicTimelineView, TimelineView, BasicTrackList}
import de.sciss.io.Span
import de.sciss.kontur.edit.Editor
import de.sciss.kontur.session.{SessionElement, SessionElementSeq, MatrixDiffusion, Session, AudioTrack, BasicTimeline, AudioRegion}
import de.sciss.kontur.util.Matrix2D
import collection.{breakOut, JavaConversions}
import de.sciss.app.{Application => SApp, AbstractApplication, AbstractCompoundEdit}
import de.sciss.strugatzki.{Span => SSpan}

trait KonturGoodies {
   def app: SApp = AbstractApplication.getApplication

   implicit def editorCanTry( editor: Editor ) = new EditorHasTry( editor )
   final class EditorHasTry( editor: Editor ) {
      def tryEdit[ A ]( name: String )( fun: AbstractCompoundEdit => A ) : A = {
         val ce      = editor.editBegin( name )
         var cancel  = true
         try {
            val res  = fun( ce )
            editor.editEnd( ce )
            cancel   = false
            res
         } finally {
            if( cancel ) editor.editCancel( ce )
         }
      }

      def joinEdit[ A ]( name: String )
                       ( fun: AbstractCompoundEdit => A )
                       ( implicit ceo: Maybe[ AbstractCompoundEdit ]) : A = {
         ceo.option match {
            case Some( ce ) => fun( ce )
            case None => tryEdit( name )( fun )
         }
      }
   }


//      def audioRegionsOverlapping( span: Span )( implicit tl: BasicTimeline ) : IndexedSeq[ AudioRegion ] = {
//         tl.tracks.getStakes( span, true )({
//            case ar: AudioRegion => true
//            case _ => false
//         }).collect({ case ar: AudioRegion => ar })( breakOut ) // ouch
//      }

   def dbamp( d: Double ) = math.pow( 10, d / 20 )
   def ampdb( d: Double ) = math.log10( d ) * 20

   def secsToFrames( d: Double )( implicit tl: BasicTimeline ) = (d * tl.rate + 0.5).toLong
   def framesToSecs( n: Long )( implicit tl: BasicTimeline ) = n / tl.rate

   def selectedAudioRegions( implicit tl: BasicTimeline, tlv: TimelineView, trl: BasicTrackList ) : IndexedSeq[ AudioRegion ] = {
      val tr   = selTracks
      val span = selSpan
      val ars  = tr.flatMap { t =>
         t.trail.getRange( span ).toIndexedSeq
      }
      ars.sortBy( _.span.start )
   }

   def selSpan( implicit tlv: TimelineView ) : Span = {
      val res = tlv.selection.span
      res
   }
   def selSpan_=( sp: Span )( implicit tlv: TimelineView, ce: Maybe[ AbstractCompoundEdit ]) {
      tlv.editor.foreach { ed =>
         ed.joinEdit( "Select span" ) { implicit ce =>
            ed.editSelect( ce, sp )
         }
      }
   }

   def selTracks( implicit tl: BasicTimeline, trl: BasicTrackList ) : IndexedSeq[ AudioTrack ] =
      tl.tracks.toList.collect({
         case at: AudioTrack if( trl.getElement( at ).map( _.selected ).getOrElse( false )) => at
      })( breakOut )

   def currentDoc : Option[ Session ] = Option( app.getDocumentHandler.getActiveDocument.asInstanceOf[ Session ])

   def withTimeline( fun: (BasicTimeline, BasicTimelineView, BasicTrackList) => Unit )( implicit doc: Session ) {
      doc.timelines.toList.filterNot( _.name.startsWith( "$" )).headOption.foreach {
         case tl: BasicTimeline =>
            JavaConversions.asScalaIterator( app.getWindowHandler.getWindows ).collect({
               case tlf: TimelineFrame if( tlf.timelineView.timeline == tl ) => tlf
            }).toList.headOption.foreach { tlf =>
               fun( tl, tlf.timelineView, tlf.tracksPanel )
            }
         case _ =>
      }
   }

   implicit def convertSpan1( sp:  Span ) : SSpan = SSpan( sp.start, sp.stop )
   implicit def convertSpan2( sp: SSpan ) :  Span = new Span( sp.start, sp.stop )

   def cutTheCheese( ars: IndexedSeq[ AudioRegion ], span: Span ) : IndexedSeq[ AudioRegion ] = {
      ars.flatMap { ar =>
         if( ar.span.overlaps( span )) {
            IndexedSeq( if( ar.span.contains( span.start )) {
               ar.split( span.start )._2
            } else {
               ar.split( span.stop )._1
            })
         } else IndexedSeq.empty[ AudioRegion ]
      }
   }

   val acceptAll : AudioTrack => Boolean = _ => true

   def findAudioTrackSpace( span: Span, accept: AudioTrack => Boolean = acceptAll )
                          ( implicit tl: BasicTimeline ) : Option[ AudioTrack ] = {
      tl.tracks.toList.collect({ case at: AudioTrack if( accept( at )) => at })
         .find( _.trail.getRange( span ).isEmpty )
   }

   def provideAudioTrackSpace( span: Span, accept: AudioTrack => Boolean = acceptAll )
                             ( implicit doc: Session, tl: BasicTimeline, ceo: Maybe[ AbstractCompoundEdit ]) : AudioTrack =
      findAudioTrackSpace( span, accept ).getOrElse {
         val ts = tl.tracks
         val ed = ts.editor.get
         ed.joinEdit( "Add track" ) { ce =>
            val at = new AudioTrack( doc )
//         at.diffusion = Some( stereoDiffusion )
            at.name = {
               val set = ts.map( _.name ).toSet
               var i = 0
               var n = ""
               do {
                  i += 1
                  n = "Tr" + i
               } while( set.contains( n ))
               n
            }
            ed.editInsert( ce, ts.size, at )
            at
         }
      }

   def place( ar: AudioRegion, accept: AudioTrack => Boolean = acceptAll )
            ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {

      tl.joinEdit( "Place audio region" ) { implicit ce =>
         val at = provideAudioTrackSpace( ar.span, accept )
         at.trail.editAdd( ce, ar )
         at
      }
   }

   def placeStereo( ar: AudioRegion, prefix: String = "" )
                  ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {
      doc.diffusions.joinEdit( "Place audio region" ) { implicit ce =>
         val d    = provideDiffusion( ar.audioFile.numChannels, 2, prefix )
         val sq   = d.matrix.toSeq
         val at   = place( ar, { at =>
            at.diffusion match {
               case Some( df: MatrixDiffusion ) if( df.matrix.toSeq == sq ) => true
               case _ => false
            }
         })
         if( at.diffusion.isEmpty ) {
            at.diffusion = Some( d )
         }
         at
      }
   }

   def uniqueName( sq: SessionElementSeq[ _ <: SessionElement ], template: String ) : String = {
      var name    = template
      var i       = 0

      while( sq.find( _.name == name ).isDefined ) {
         i    += 1
         name  = template + "[" + i + "]"
      }
      name
   }

   def mixMatrix( inChans: Int, outChans: Int ) : Matrix2D[ Float ] = {
      // ( row, cols ) = (input, output) = InSeq[OutSeq]
      val sq = Seq.tabulate( inChans ) { in =>
//         val inw = if( inChans < 2 ) 1f else (in.toFloat / (inChans - 1))
         Seq.tabulate( outChans ) { out =>
            val outw = if( outChans < 2 ) 1f else (out.toFloat / (outChans - 1))
            val w    = outw * (inChans - 1) // (if( inChans < 2 ) 1 else (inChans - 1))
            math.sqrt( 1f - math.min( 1f, math.abs( w - in ))).toFloat
         }
      }
      Matrix2D.fromSeq( sq )
   }

   def findDiffusion( matrix: Matrix2D[ Float ])( implicit doc: Session ) : Option[ MatrixDiffusion ] = {
      val sq = matrix.toSeq
      doc.diffusions.toList.collect({
         case md: MatrixDiffusion if( md.matrix.toSeq == sq ) => md
      }).headOption
   }

   def provideDiffusion( inChans: Int, outChans: Int, prefix: String = "" )
                       ( implicit doc: Session, ceo: Maybe[ AbstractCompoundEdit ]) : MatrixDiffusion = {
      val m = mixMatrix( inChans, outChans )
      findDiffusion( m ).getOrElse {
         val dfs = doc.diffusions
         dfs.joinEdit( "Add diffusion" ) { ce =>
            val d = new MatrixDiffusion( doc )
            d.matrix = m
            d.name   = prefix + inChans + "->" + outChans
            dfs.editInsert( ce, dfs.size, d )
            d
         }
      }
   }
}