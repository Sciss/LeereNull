/*
 *  LeereNull.scala
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

import de.sciss.kontur.{Main => Kontur}
import de.sciss.gui.{MenuItem, MenuGroup}
import java.util.Properties
import java.io.{File, FileInputStream}
import swing.{Dialog, Swing}

object LeereNull extends Runnable with GUIGoodies with KonturGoodies {
   lazy val (baseFolder, databaseFolder, extractorFolder, searchFolder, bounceFolder) = {
      val file = new File( "leerenull-settings.xml" )
      val prop = new Properties()
      val is = new FileInputStream( file )
      prop.loadFromXML( is )
      is.close()
      val base       = new File( prop.getProperty( "base" ))
      val database   = new File( base, "feature" )
      val extractor  = new File( base, "extract" )
      val search     = new File( base, "search" )
      val bounce     = new File( base, "bounce" )
      (base, database, extractor, search, bounce)
   }

   def main( args: Array[ String ]) {
      Swing.onEDT( run() )
   }

   def run() {
      val app  = new Kontur( Array() )
      val mf   = app.getMenuFactory
      val mg   = new MenuGroup( "leerenull", "Leere Null" )
      val miExtractor = new MenuItem( "leerenull.extractor", action( "Extractor...", "control E" ) {
         currentDoc.foreach { implicit doc =>
            withTimeline { (tl, tlv, trl) =>
               implicit val tl0  = tl
               implicit val tlv0 = tlv
               implicit val trl0 = trl

               val span = selSpan
               cutTheCheese( selectedAudioRegions, span ) match {
                  case IndexedSeq( ar ) => CorrelatorSetup.prepareCorrelator( ar )
                  case _ =>
                     val message = "<html>Multiple regions are selected. To proceed,<br>" +
                        "an intermediate bounced representation is needed.<br>" +
                        "<B>Go ahead and bounce?</B></html>"
                     val res = Dialog.showConfirmation( null, message, "Extract", Dialog.Options.OkCancel, Dialog.Message.Question )
                     if( res == Dialog.Result.Ok ) {
                        val tracks = trl.toList.filter( _.selected ).map( _.track )
                        CorrelatorSetup.bounceAndExtract( tracks, span )
                     }
               }
            }
         }
      })
      val miLoadSearch = new MenuItem( "leerenull.loadsearch", action( "Load Search...", "control O" ) {
         currentDoc.foreach { implicit doc =>
            openFileDialog( "Load Search", searchFolder, filter = _.getName.endsWith( ".xml" )).foreach { file =>
               val search = CorrelatorSelector.Search.fromXMLFile( file )
               CorrelatorSelector.makeSelector( search )
            }
         }
      })
      mg.add( miExtractor )
      mg.add( miLoadSearch )
      mf.add( mg )
   }
}