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

import de.sciss.io.Span
import de.sciss.kontur.edit.Editor
import de.sciss.kontur.util.Matrix2D
import collection.{breakOut, JavaConversions}
import de.sciss.app.{Application => SApp, AbstractApplication, AbstractCompoundEdit}
import de.sciss.strugatzki.{Span => SSpan}
import java.io.File
import de.sciss.kontur.gui.{TrailView, TimelineFrame, BasicTimelineView, TimelineView, BasicTrackList}
import de.sciss.kontur.session.{Diffusion, ResizableStake, AudioFileElement, SessionElement, SessionElementSeq, MatrixDiffusion, Session, AudioTrack, BasicTimeline, AudioRegion}
import annotation.switch

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

   def collectAudioRegions[ A ]( fun: PartialFunction[ (AudioTrack, AudioRegion), A ])
                               ( implicit tl: BasicTimeline /*, trl: BasicTrackList */ ) : IndexedSeq[ A ] = {
      val trs = tl.tracks.toList.collect { case atr: AudioTrack => atr }
      val b = IndexedSeq.newBuilder[ A ]
      trs.foreach { tr =>
         tr.trail.visitAll()(  ar => { val tup = (tr, ar); if( fun.isDefinedAt( tup )) b += fun( tup )})
      }
      b.result()
   }

   def nonSyntheticTimelines( implicit doc: Session ) : IndexedSeq[ BasicTimeline ] =
      doc.timelines.toList.collect({ case tl: BasicTimeline if( !tl.name.startsWith( "$" )) => tl })( breakOut )

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

   def findAudioFile( file: File )( implicit doc: Session ) : Option[ AudioFileElement ] =
      doc.audioFiles.find( _.path == file )

   def provideAudioFile( file: File )( implicit doc: Session, ce: Maybe[ AbstractCompoundEdit ]) : AudioFileElement =
      findAudioFile( file ).getOrElse {
         val afe = AudioFileElement.fromPath( doc, file )
         val afs = doc.audioFiles
         afs.joinEdit( "Add audio file" ) { implicit ce =>
            afs.editInsert( ce, afs.size, afe )
         }
         afe
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

   def findAudioTrackSpace( span: Span, accept: AudioTrack => Boolean = acceptAll, more: Seq[ AudioTrack ] = Seq.empty )
                          ( implicit tl: BasicTimeline ) : Option[ AudioTrack ] = {
      (tl.tracks.toList ++ more).collect({ case at: AudioTrack if( accept( at )) => at })
         .find( _.trail.getRange( span ).isEmpty )
   }

   def provideAudioTrackSpace( span: Span, accept: AudioTrack => Boolean = acceptAll, more: Seq[ AudioTrack ] = Seq.empty,
                               prefix: String = "T" )
                             ( implicit doc: Session, tl: BasicTimeline, ceo: Maybe[ AbstractCompoundEdit ]) : AudioTrack =
      findAudioTrackSpace( span, accept, more ).getOrElse {
         val ts = tl.tracks
         val ed = ts.editor.get
         ed.joinEdit( "Add track" ) { ce =>
            val at = new AudioTrack( doc )
//         at.diffusion = Some( stereoDiffusion )
            at.name = {
               val set = (ts.toList ++ more).map( _.name ).toSet
               var i = 0
               var n = ""
               do {
                  i += 1
                  n = prefix + i
               } while( set.contains( n ))
               n
            }
            ed.editInsert( ce, ts.size, at )
            at
         }
      }

   def place( ar: AudioRegion, accept: AudioTrack => Boolean = acceptAll, more: Seq[ AudioTrack ] = Seq.empty,
              prefix: String = "T" )
            ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {

      tl.joinEdit( "Place audio region" ) { implicit ce =>
         val at = provideAudioTrackSpace( ar.span, accept, more, prefix )
         at.trail.editAdd( ce, ar )
         at
      }
   }

   def placeWithDiff( d: MatrixDiffusion, ar: AudioRegion, more: IndexedSeq[ AudioTrack ] = IndexedSeq.empty,
                      trackPrefix: String = "T" )
                  ( implicit doc: Session, tl: BasicTimeline, ce: AbstractCompoundEdit ) : AudioTrack = {
      val sq   = d.matrix.toSeq
      val at   = place( ar, { at =>
         at.diffusion match {
            case Some( df: MatrixDiffusion ) if( df.matrix.toSeq == sq ) => true
            case _ => false
         }
      }, more, trackPrefix )
      if( at.diffusion.isEmpty ) {
         at.diffusion = Some( d )
      }
      at
   }

   def placeStereo( ar: AudioRegion, more: IndexedSeq[ AudioTrack ] = IndexedSeq.empty,
                    diffPrefix: String = "", trackPrefix: String = "T" )
                  ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {
      doc.diffusions.joinEdit( "Place audio region" ) { implicit ce =>
         val d = provideStereoDiffusion( ar.audioFile.numChannels, 2, diffPrefix, more.map( _.diffusion ).collect({ case Some( d ) => d }))
         placeWithDiff( d, ar, more, trackPrefix )
      }
   }

   def placeLeft( ar: AudioRegion, more: IndexedSeq[ AudioTrack ] = IndexedSeq.empty,
                  diffPrefix: String = "", trackPrefix: String = "T-L" )
                ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {
      doc.diffusions.joinEdit( "Place audio region" ) { implicit ce =>
         val numCh = ar.audioFile.numChannels
         val d = provideLeftDiffusion( diffPrefix, more.map( _.diffusion ).collect({ case Some( d ) => d }))( numCh )
         placeWithDiff( d, ar, more, trackPrefix )
      }
   }

   def provideLeftDiffusion( diffPrefix: String = "", more: IndexedSeq[ Diffusion ] = IndexedSeq.empty )( numInChans: Int )
                           ( implicit doc: Session, ce: Maybe[ AbstractCompoundEdit ]) : MatrixDiffusion = {
      val m = (numInChans: @switch) match {
         case 1 => Matrix2D.fromSeq[ Float ]( Seq( Seq( 1f, 0f )))
         case 2 => Matrix2D.fromSeq[ Float ]( Seq( Seq( 1f, 0f ), Seq( 0f, 0f )))
      }
      provideDiffusion( m, diffPrefix, more )
   }

   def placeRight( ar: AudioRegion, more: IndexedSeq[ AudioTrack ] = IndexedSeq.empty,
                   diffPrefix: String = "", trackPrefix: String = "T-R" )
                ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {
      doc.diffusions.joinEdit( "Place audio region" ) { implicit ce =>
         val numCh = ar.audioFile.numChannels
         val d = provideRightDiffusion( diffPrefix, more.map( _.diffusion ).collect({ case Some( d ) => d }))( numCh )
         placeWithDiff( d, ar, more, trackPrefix )
      }
   }

   def provideRightDiffusion( diffPrefix: String = "", more: IndexedSeq[ Diffusion ] = IndexedSeq.empty )( numInChans: Int )
                           ( implicit doc: Session, ce: Maybe[ AbstractCompoundEdit ]) : MatrixDiffusion = {
      val m = (numInChans: @switch) match {
         case 1 => Matrix2D.fromSeq[ Float ]( Seq( Seq( 0f, 1f )))
         case 2 => Matrix2D.fromSeq[ Float ]( Seq( Seq( 0f, 0f ), Seq( 0f, 1f )))
      }
      provideDiffusion( m, diffPrefix, more )
   }

   def insertTimelineSpan( pos: Long, delta: Long )
                         ( decider: (AudioTrack, AudioRegion) => InsertSpan.Action = (t, ar) => {
                            if( pos <= ar.span.start ) InsertSpan.Ignore
                            else if( pos >= ar.span.stop ) InsertSpan.Move
                            else InsertSpan.Split
                         })
                         ( implicit tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) {
      require( (pos >= tl.span.start) && (pos <= tl.span.stop), pos.toString )
//      require( delta >= 0, delta.toString )

//      val affectedSpan = new Span( pos, tl.span.stop )

      tl.joinEdit[ Unit ]( "Insert timeline span" ) { implicit ce: AbstractCompoundEdit =>
         if( delta > 0L ) tl.editSpan( ce, tl.span.replaceStop( tl.span.stop + delta ))
         val map = collectAudioRegions({ case (tr, ar) => (tr.trail, ar, decider( tr, ar ))}).groupBy( _._3 )

         map.getOrElse( InsertSpan.Move, IndexedSeq.empty ).groupBy( _._1 ).foreach {
            case (trail, seq) =>
               val ars = seq.map( _._2 )
               trail.editRemove( ce, ars: _* )
               val arsm = ars.map( _.move( delta ))
               trail.editAdd( ce, arsm: _* )
         }
         map.getOrElse( InsertSpan.Split, IndexedSeq.empty ).groupBy( _._1 ).foreach {
            case (trail, seq) =>
               val ars = seq.map( _._2 )
               trail.editRemove( ce, ars: _* )
               val arss = ars.flatMap { ar =>
                  val (nomove, move) = ar.split( pos )
                  Seq( nomove, move.move( delta ))
               }
               trail.editAdd( ce, arss: _* )
         }
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

   /**
    * @param   bal   balance from -1 (hard left) to 1 (hard right)
    */
   def pannedMatrix( inChans: Int, outChans: Int, bal: Float = 0f ) : Matrix2D[ Float ] = {
      val bal1 = 1f - math.max( 0f, math.min( 1f, bal * 0.5f + 0.5f ))
      val sq = Seq.tabulate( inChans ) { in =>
         Seq.tabulate( outChans ) { out =>
            val outw = if( outChans < 2 ) 1f else (out.toFloat / (outChans - 1))
            val w0   = outw * (inChans - 1)
            val res0 = math.sqrt( 1f - math.min( 1f, math.abs( w0 - in ))).toFloat
            res0 * math.min( 0.5f, math.abs( outw - bal1 )) * 2
         }
      }
      Matrix2D.fromSeq( sq )
   }

   def findDiffusion( matrix: Matrix2D[ Float ], more: IndexedSeq[ Diffusion ] = IndexedSeq.empty )
                    ( implicit doc: Session ) : Option[ MatrixDiffusion ] = {
      val sq = matrix.toSeq
      (doc.diffusions.toList ++ more).collect({
         case md: MatrixDiffusion if( md.matrix.toSeq == sq ) => md
      }).headOption
   }

   def provideStereoDiffusion( inChans: Int, outChans: Int, prefix: String = "", more: IndexedSeq[ Diffusion ] = IndexedSeq.empty )
                       ( implicit doc: Session, ceo: Maybe[ AbstractCompoundEdit ]) : MatrixDiffusion = {
      provideDiffusion( mixMatrix( inChans, outChans ), prefix, more )
   }

   def provideDiffusion( m: Matrix2D[ Float ], prefix: String = "", more: IndexedSeq[ Diffusion ] = IndexedSeq.empty )
                       ( implicit doc: Session, ceo: Maybe[ AbstractCompoundEdit ]) : MatrixDiffusion = {
      findDiffusion( m, more ).getOrElse {
         val dfs = doc.diffusions
         dfs.joinEdit( "Add diffusion" ) { ce =>
            val d = new MatrixDiffusion( doc )
            d.matrix = m
            d.name   = prefix + m.numRows + "->" + m.numColumns
            dfs.editInsert( ce, dfs.size, d )
            d
         }
      }
   }
}