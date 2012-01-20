/*
 *  PDF.scala
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

import de.sciss.kontur.session.BasicTimeline
import de.sciss.strugatzki.Span
import de.sciss.synth.io.AudioFile
import java.io.File

object ThirdMovement {
   object Strategy {
      def apply( bal: Double ) : Strategy = {
         require( bal >= 0.0 && bal <= 1.0 )
         new Strategy { val balance = bal }
      }

      case object Imitation extends Strategy { val balance = 0.0 }
      case object Ecology   extends Strategy { val balance = 1.0 }
   }
   sealed trait Strategy {
      def balance: Double
   }
}

class ThirdMovement {
   import ThirdMovement._

   def generate( tl: BasicTimeline, tlSpan: Span, layer: AudioFile, layerOffset: Long, material: File,
                 numChannels: Int, startStrategy: Strategy, stopStrategy: Strategy ) {

   }
}