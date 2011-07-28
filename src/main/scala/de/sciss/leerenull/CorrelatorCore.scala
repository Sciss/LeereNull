/*
 *  CorrelatorCore.scala
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

import de.sciss.strugatzki.FeatureCorrelation

object CorrelatorCore extends GUIGoodies with KonturGoodies {

   def beginSearch( settings: FeatureCorrelation.Settings ) {
      println( settings )

      val dlg  = progressDialog( "Correlating with database" )
      val fc   = FeatureCorrelation( settings ) {
         case FeatureCorrelation.Success( res ) =>
            dlg.stop()
            println( "Done. " + res.size + " entries:" )
            res.foreach { m =>
               println(  "\nFile      : " + m.file.getAbsolutePath +
                         "\nSimilarity: " + (m.sim * 100) +
                         "\nSpan start: " + m.punch.start +
                         "\nBoost in  : " + ampdb( m.boostIn ))
               if( settings.punchOut.isDefined ) {
                  println( "Span stop : " + m.punch.stop +
                         "\nBoost out : " + ampdb( m.boostOut ))
               }
            }

         case FeatureCorrelation.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()

         case FeatureCorrelation.Aborted =>
            dlg.stop()

         case FeatureCorrelation.Progress( i ) => dlg.progress = i
      }
      dlg.start( fc )
   }
}