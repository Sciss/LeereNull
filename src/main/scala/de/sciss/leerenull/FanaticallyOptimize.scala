/*
 *  FanaticallyOptimize.scala
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

import de.sciss.kontur.gui.{TimelineView, BasicTrackList}
import de.sciss.kontur.session.{Session, BasicTimeline, AudioTrack}

object FanaticallyOptimize extends KonturGoodies with NullGoodies {
   def perform( doc: Session, tl: BasicTimeline, tlv: TimelineView, trl: BasicTrackList ): Unit = {
      implicit val tl0  = tl
      implicit val tlv0 = tlv
      implicit val trl0 = trl

      val allTracks = tl.tracks.toList.collect({ case at: AudioTrack => at }).toIndexedSeq // .sortBy( _.name )
      val selTracks = allTracks.filter( t => trl.getElement(t).exists(_.selected))

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
