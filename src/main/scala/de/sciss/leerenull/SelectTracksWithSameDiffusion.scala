/*
 *  SelectTracksWithSameDiffusion.scala
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

import de.sciss.kontur.gui.BasicTrackList
import de.sciss.kontur.session.{MatrixDiffusion, AudioTrack}

object SelectTracksWithSameDiffusion extends KonturGoodies {
   def perform( trl: BasicTrackList ) {
      val num     = trl.numElements
      val tracks  = (0 until num).map( trl.getElementAt( _ ).track ).collect {
         case at: AudioTrack => at
      }
      tracks.filter( trl.getElement( _ ).map( _.selected ).getOrElse( false )) match {
         case Seq( one ) =>
            val diff = one.diffusion
            val same = tracks.filter { t => (diff, t.diffusion) match {
               case (None, None) => true
               case (Some( m1: MatrixDiffusion ), Some( m2: MatrixDiffusion )) if m1.matrix.toSeq == m2.matrix.toSeq => true
               case (Some( d1 ), Some( d2 )) if d1 == d2 => true
               case _ => false
            }}
            val toSelect = same.flatMap( trl.getElement )

            val ce = trl.editBegin( "Select tracks" )
            trl.select( toSelect: _* )
            trl.editEnd( ce )

         case _ => println( "Must have exactly one track selected" )
      }
   }
}
