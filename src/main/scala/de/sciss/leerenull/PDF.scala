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

import java.io.{FileOutputStream, File}
import com.itextpdf.text.pdf.PdfWriter
import com.itextpdf.text.{Document => IDocument, Rectangle => IRectangle}
import java.awt.Component

object PDF {
   def create( file: File, view: Component, usePrefSize: Boolean = true, margin: Int = 0 ) {
      val viewSz     = if( usePrefSize ) view.getPreferredSize else view.getSize
      val width      = viewSz.width + (margin << 1)
      val height     = viewSz.height + (margin << 1)
      val pageSize	= new IRectangle( 0, 0, width, height )
      val doc		   = new IDocument( pageSize, margin, margin, margin, margin )
      val stream	   = new FileOutputStream( file )
      val writer	   = PdfWriter.getInstance( doc, stream )

      doc.open()
      val cb		   = writer.getDirectContent
      val tp		   = cb.createTemplate( viewSz.width, viewSz.height )
      val g2		   = tp.createGraphics( viewSz.width, viewSz.height /*, fontMapper */ )
//val in = view.getInsets
//g2.translate( -in.left, -in.top )
//g2.translate( margin, margin )
      view.paint( g2 )
      g2.dispose()
      cb.addTemplate( tp, margin, margin )
      doc.close()
   }

//   def wrapTimeline( tlf: TimelineFrame, trackWidth: Int, trackHeight: Int ) = new Component with WrappedTimeline {
//      def numTracks  = tlf.tracksPanel.numElements
//      var span: Span = tlf.tracksPanel.timelineView.timeline.span
//
//      def totalHeight = trackHeight * numTracks + (numTracks - 1)
//
//      override def getPreferredSize = new Dimension( trackWidth, totalHeight )
//
//      override def paint( g: Graphics ) {
//         val g2 = g.asInstanceOf[ Graphics2D ]
//         val clipOrig   = g2.getClip
//         val atOrig     = g2.getTransform
//         g2.clipRect( 0, 0, trackWidth, totalHeight )
//         for( i <- 0 until numTracks ) {
//            val t    = tlf.tracksPanel.getElementAt( i )
////            val span = tlf.tracksPanel.timelineView.timeline.span
////            g2.translate( 0, (i * trackHeight + 1) )
//            t.renderer.trackComponent.paintTrack( g2, 0, 0, trackWidth, trackHeight, span )
//            g2.setTransform( atOrig )
//         }
//         g2.setClip( clipOrig )
//      }
//   }
//
//   trait WrappedTimeline { def span: Span; def span_=( s: Span ) : Unit }
}