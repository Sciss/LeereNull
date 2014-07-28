/*
 *  PasteCompanion.scala
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

import de.sciss.kontur.session.{Session, BasicTimeline, AudioFileElement, AudioTrack, AudioRegion}
import collection.immutable.{IndexedSeq => IIdxSeq}

trait PasteCompanion {
   protected def VERBOSE : Boolean

   def pasteResult( doc: Session, tl: BasicTimeline, remove: IIdxSeq[ (AudioTrack, AudioRegion) ],
                    insert: IIdxSeq[ (AudioTrack, AudioRegion )],
                    newFiles: IIdxSeq[ AudioFileElement ]): Unit = {

      val removeMap: Map[ AudioTrack, IIdxSeq[ AudioRegion ]] = remove.groupBy( _._1 ).mapValues( _.map( _._2 ))
      val insertMap: Map[ AudioTrack, IIdxSeq[ AudioRegion ]] = insert.groupBy( _._1 ).mapValues( _.map( _._2 ))

      // add new audio files
      if( newFiles.nonEmpty ) {
         val afs = doc.audioFiles
         val ce = afs.editBegin( "Insert audio files" )
         var ceOk = false
         try {
            val off = afs.size
            newFiles.zipWithIndex.foreach {
               case (afe, idx) => afs.editInsert( ce, idx + off, afe )
            }
            ceOk = true
         } finally {
            if( ceOk ) afs.editEnd( ce ) else afs.editCancel( ce )
         }
      }

      if( VERBOSE ) {
         println( ":::: REMOVE ::::" )
         removeMap.foreach { case (at, ars) =>
            println( at.name )
            ars.foreach { ar => println( "  " + ar.name )}
         }
      }

      // remove old regions
      removeMap.foreach { case (at, ars) =>
         val ce = at.editBegin( "Remove old regions in track " + at.name )
         var ceOk = false
         try {
            at.trail.editRemove( ce, ars: _* )
            ceOk = true
         } finally {
            if( ceOk ) at.editEnd( ce ) else at.editCancel( ce )
         }
      }

      if( VERBOSE ) {
         println( ":::: INSERT ::::" )
         insertMap.foreach { case (at, ars) =>
            println( at.name )
            ars.foreach { ar => println( "  " + ar.name )}
         }
      }

      // insert new regions
      insertMap.foreach { case (at, ars) =>
         val ce = at.editBegin( "Insert new regions in track " + at.name )
         var ceOk = false
         try {
            at.trail.editAdd( ce, ars: _* )
            ceOk = true
         } finally {
            if( ceOk ) at.editEnd( ce ) else at.editCancel( ce )
         }
      }
   }
}
