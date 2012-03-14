package de.sciss.leerenull

import processing.core.PApplet
import java.awt.Graphics2D

trait VideoLike extends PApplet {
   def now: Double

//   private var g2dVar : Graphics2D = null
//
//   final def g2d : Graphics2D = g2dVar
//
//   override def update( g: Graphics ) {
//      g2dVar = g.asInstanceOf[ Graphics2D ]
//      try {
//         super.update( g )
//      } finally {
//         g2dVar = null
//      }
//   }

   lazy val g2d = getGraphics.asInstanceOf[ Graphics2D ]
}