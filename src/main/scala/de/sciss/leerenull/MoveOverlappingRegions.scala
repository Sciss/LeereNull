/*
 *  MoveOverlappingRegions.scala
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

import de.sciss.kontur.gui.{BasicTrackList, TimelineView, TrailViewEditor}
import de.sciss.kontur.session.{Session, BasicTimeline, MatrixDiffusion, AudioRegion, AudioTrack}
import collection.immutable.{IndexedSeq => IIdxSeq}

object MoveOverlappingRegions extends KonturGoodies with NullGoodies {
   def perform( doc: Session, tl: BasicTimeline, tlv: TimelineView, trl: BasicTrackList ) {
      implicit val doc0 = doc
      implicit val tl0  = tl
      implicit val tlv0 = tlv
      implicit val trl0 = trl
      val span = selSpan

      val selARs  = selectedAudioRegions.toSet
      val selMap  = collectAudioRegions { case (at, ar) if !ar.muted && selARs.contains( ar ) => (at, ar) }
//      val ars: IIdxSeq[ AudioRegion ] = selMap.map( _._2 )( collection.breakOut )
      val mapIn   = selMap.groupBy( _._1 ).mapValues( _.map( _._2 ).sortBy( _.span.start ))
      val mapOut  = mapIn.map {
         case (at, ars) =>
            val overs = ars.zip( ars.tails.drop( 1 ).toIndexedSeq ).collect {
               case (ar1, ars1) if ars1.exists( _.span overlaps ar1.span ) => ar1
            }
            (at, overs)
      }
      // first remove overlapping regions
      mapOut.foreach {
         case (at, overs) =>
            val ce = at.editBegin( "Remove overlapping regions" )
            var ceOk = false
            try {
               at.trail.editRemove( ce, overs: _* )
               ceOk = true
            } finally {
               if( ceOk ) at.editEnd( ce ) else at.editCancel( ce )
            }
      }
      // then sort them back in, one by one
      val map2 = mapOut.flatMap {
         case (at, overs) =>
            val matO = at.diffusion match {
               case Some( m: MatrixDiffusion ) => Some( m.matrix.toSeq )
               case _ => None
            }
            val ce = at.editBegin( "Re-insert overlapping region" )
            var ceOk = false
            overs.map { ar =>
               try {
                  val t2 = provideAudioTrackSpace( ar.span, { at =>
                     /* inTxn.get( at ).map( !_.exists( _.span.overlaps( ar.span ))).getOrElse( true ) && */
                     (at.diffusion match {
                        case Some( m: MatrixDiffusion ) => Some( m.matrix.toSeq ) == matO
                        case None => matO.isEmpty
                     })
                  })
                  if( t2.diffusion.isDefined != matO.isDefined ) {
                     assert( matO.isDefined )
                     t2.editDiffusion( ce, at.diffusion )
                  }
                  t2.trail.editAdd( ce, ar )
                  ceOk = true
                  (t2, ar)
               } finally {
                  if( ceOk ) at.editEnd( ce ) else at.editCancel( ce )
               }
            }
      }

      val map3 = map2.groupBy( _._1 ).mapValues( _.map( _._2 ))

      val ce1 = tl.editBegin( "Select moved regions" )
      val ce1Ok = false
      try {
         map3.foreach {
            case (at, ars) =>
               val tve2O = trl.getElement( at ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
               tve2O.foreach( _.editSelect( ce1, ars.toSeq: _* ))
         }
      } finally {
         if( ce1Ok ) tl.editEnd( ce1 ) else tl.editCancel( ce1 )
      }

//      tl.joinEdit( "Clean Up Overlaps" ) { implicit ce =>
//         var trackMap      = Map.empty[ AudioTrack, IndexedSeq[ AudioRegion ]]
////                  var moreTracks = Set.empty[ AudioTrack ]
//         var moreDiffs  = Set.empty[ MatrixDiffusion ]
//         val withDiffs  = selTracks.map( at => (at, at.diffusion) ).collect {
//            case (at, Some( m: MatrixDiffusion )) => (at, Some( m.matrix.toSeq ))
//            case (at, None) => (at, Option.empty[ Seq[ Seq[ Float ]]])
//         }
//         withDiffs.foreach { case (at, matO) =>
//            val hasFirst   = matO match {
//               case Some( seq ) => seq.forall( _.last == 0f )
//               case None => false
//            }
//            val hasLast    = matO match {
//               case Some( seq ) => seq.forall( _.head == 0f )
//               case None => false
//            }
//            val trackPrefix = if( hasFirst ) "T-L" else if( hasLast ) "T-R" else "T"
//
//            var ars = at.trail.getRange( span )
//            var over = IndexedSeq.empty[ AudioRegion ]
//            var foundOverlap = true
//            while( foundOverlap && ars.nonEmpty ) {
//               val ar1  = ars.last
//               val dr   = ars.dropRight( 1 )
//               foundOverlap = dr.exists( _.span.overlaps( ar1.span ))
//               if( foundOverlap ) {
//                  over +:= ar1
//                  ars    = dr
//               }
//            }
//            if( over.nonEmpty ) {
//               val tveO = trl.getElement( at ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
//               tveO.foreach( _.editDeselect( ce, over: _* ))
//               at.trail.editRemove( ce, over: _* )
//               val moveMap = over.map( ar => {
//                  val t2 = provideAudioTrackSpace( ar.span, { at =>
//                     /* inTxn.get( at ).map( !_.exists( _.span.overlaps( ar.span ))).getOrElse( true ) && */
//                     (at.diffusion match {
//                        case Some( m: MatrixDiffusion ) => Some( m.matrix.toSeq ) == matO
//                        case None => matO.isEmpty
//                     })
//                  }, trackMap, trackPrefix )
//                  if( t2.diffusion.isDefined != matO.isDefined ) {
//                     assert( matO.isDefined )
//                     val numInChans = ar.audioFile.numChannels
//                     val d = if( hasFirst ) {
//                        provideLeftDiffusion( more = moreDiffs.toIndexedSeq )( numInChans )
//                     } else if( hasLast ) {
//                        provideRightDiffusion( more = moreDiffs.toIndexedSeq )( numInChans )
//                     } else {
//                        provideStereoDiffusion( numInChans, 2, more = moreDiffs.toIndexedSeq )
//                     }
//                     moreDiffs += d
//                     t2.editDiffusion( ce, Some( d ))
//                  }
////                           moreTracks += t2
//                  trackMap += t2 -> (trackMap.getOrElse( t2, IndexedSeq.empty[ AudioRegion ]) :+ ar)
//                  t2 -> ar
//               }).groupBy( _._1 ).mapValues( _.map( _._2 ))
//               moveMap.foreach { case (at1, ars1) =>
//                  at1.trail.editAdd( ce, ars1: _* )
//                  val tve2O = trl.getElement( at1 ).flatMap[ TrailViewEditor[ AudioRegion ]]( _.trailView.editor.asInstanceOf[ Option[ TrailViewEditor[ AudioRegion ]]])
//                  tve2O.foreach( _.editSelect( ce, ars1: _* ))
//               }
//            }
//         }
//      }
   }
}
