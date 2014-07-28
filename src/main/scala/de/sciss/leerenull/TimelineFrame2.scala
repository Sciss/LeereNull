/*
 *  TimelineFrame2.scala
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

import de.sciss.kontur.gui.TimelineFrame
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
   def bottomPanel_=( p: Option[ Component ]): Unit = {
      bottomPanelVar.foreach { oldp =>
         getContentPane.remove( oldp.peer )
      }
      bottomPanelVar = p
      p.foreach { newp =>
         getContentPane.add( newp.peer, BorderLayout.SOUTH )
      }
   }

   override protected def windowClosing(): Unit = {
//      println( "DISPOSA!" )
      closeFun( frame )
//      app.getMenuFactory.removeFromWindowMenu( actionShowWindow )
      super.windowClosing()
   }

   def packAndSetMinimum(): Unit = {
      pack()
      getWindow match {
         case f: JFrame => f.setMinimumSize( f.getSize )
      }
   }
}