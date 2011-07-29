/*
 *  TimelineFrame2.scala
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

import de.sciss.kontur.gui.TimelineFrame
import de.sciss.app.AbstractWindow
import de.sciss.kontur.session.{Session, BasicTimeline}
import swing.Component
import java.awt.BorderLayout
import javax.swing.{JFrame, WindowConstants}

object TimelineFrame2 {
   def apply( closeFun: TimelineFrame2 => Unit )( implicit doc: Session, tl: BasicTimeline ) =
      new TimelineFrame2( doc, tl, closeFun )
}
final class TimelineFrame2 private ( doc: Session, tl: BasicTimeline, closeFun: TimelineFrame2 => Unit )
extends TimelineFrame( doc, tl ) {
   frame =>

   setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
//   addListener( new AbstractWindow.Listener {
//      def windowClosing( e: AbstractWindow.Event ) {
////         println( "Wooha" )
////         tlf.dispose()
//         closeFun( frame )
//      }
//
//      def windowClosed(p1: AbstractWindow.Event) {}
//      def windowDeactivated(p1: AbstractWindow.Event) {}
//      def windowDeiconified(p1: AbstractWindow.Event) {}
//      def windowOpened(p1: AbstractWindow.Event) {}
//      def windowActivated(p1: AbstractWindow.Event) {}
//      def windowIconified(p1: AbstractWindow.Event) {}
//   })

   private var bottomPanelVar = Option.empty[ Component ]

   def bottomPanel : Option[ Component ] = bottomPanelVar
   def bottomPanel_=( p: Option[ Component ]) {
      bottomPanelVar.foreach { oldp =>
         getContentPane.remove( oldp.peer )
      }
      bottomPanelVar = p
      p.foreach { newp =>
         getContentPane.add( newp.peer, BorderLayout.SOUTH )
      }
   }

   override protected def windowClosing() {
//      println( "DISPOSA!" )
      closeFun( frame )
//      app.getMenuFactory.removeFromWindowMenu( actionShowWindow )
      super.windowClosing()
   }

   def packAndSetMinimum() {
      pack()
      getWindow match {
         case f: JFrame => f.setMinimumSize( f.getSize )
      }
   }
}