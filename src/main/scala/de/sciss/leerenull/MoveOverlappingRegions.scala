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
import de.sciss.kontur.session.{Session, BasicTimeline, MatrixDiffusion, AudioRegion}

object MoveOverlappingRegions extends KonturGoodies with NullGoodies {
   def perform( doc: Session, tl: BasicTimeline, tlv: TimelineView, trl: BasicTrackList ): Unit = {
      implicit val doc0 = doc
      implicit val tl0  = tl
      implicit val tlv0 = tlv
      implicit val trl0 = trl

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
   }
}
