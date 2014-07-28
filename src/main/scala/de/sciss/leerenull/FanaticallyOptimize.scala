/*
 *  FanaticallyOptimize.scala
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

import de.sciss.kontur.gui.{TimelineView, BasicTrackList}
import de.sciss.kontur.session.{Session, BasicTimeline, AudioTrack}

object FanaticallyOptimize extends KonturGoodies with NullGoodies {
   def perform( doc: Session, tl: BasicTimeline, tlv: TimelineView, trl: BasicTrackList ): Unit = {
      implicit val tl0  = tl
      implicit val tlv0 = tlv
      implicit val trl0 = trl

      val allTracks = tl.tracks.toList.collect({ case at: AudioTrack => at }).toIndexedSeq // .sortBy( _.name )
      val selTracks = allTracks.filter( t => trl.getElement( t ).map( _.selected ).getOrElse( false ))

      selTracks.zipWithIndex.foreach { case (at, idx) =>
         val ars  = at.trail.getAll()
         ars.foreach { ar =>
            val at2O = selTracks.take( idx - 1 ).find( _.trail.getRange( ar.span ).isEmpty )
            at2O.foreach { at2 =>
               tl.joinEdit( "Optimize Tracks Capacities" ) { implicit ce =>
                  at.trail.editRemove( ce, ar )
                  at2.trail.editAdd( ce, ar )
               }
            }
         }
      }
   }
}
