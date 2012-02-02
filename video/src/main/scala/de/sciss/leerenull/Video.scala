/*
 *  Video.scala
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

import processing.core
import core.{PConstants, PApplet}
import java.io.{PrintStream, File}
import de.sciss.gui.j.LCDPanel
import javax.swing.{BorderFactory, Box, JLabel, JProgressBar, WindowConstants, JFrame}
import java.awt.{Color, Font, Dimension, BorderLayout, EventQueue}

object Video extends App {
   EventQueue.invokeLater( new Runnable { def run() {
      val log = new LogPane
      log.init()
      val logF = new JFrame( "Log" )
      logF.getContentPane.add( log, BorderLayout.CENTER )
//      logF.setSize( 400, 400 )
      logF.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
      logF.pack()
      logF.setVisible( true )
      val ps = new PrintStream( log.outputStream )
      System.setOut( ps )
      System.setErr( ps )
      Console.setOut( log.outputStream )
      Console.setErr( log.outputStream )
      println( "Rendering..." )
//      Console.err.println( "Hello world." )
      new Video
   }})

   val videoWidth       = 1024
   val videoHeight      = 768
   val videoFPS         = 24
   val dataFolder       = new File( "data" )
   val outputFolder     = new File( "data_out" )
//   val totalDuration    = 30.0
//   val totalNumFrames   = (totalDuration * videoFPS + 0.5).toInt
}

class Video extends PApplet {
   import Video._

   val f = {
      val res = new JFrame( "Leere Null" )
      res.getRootPane.putClientProperty( "apple.awt.brushMetalLook", java.lang.Boolean.TRUE )
      res
   }
   private val ggProgress = new JProgressBar( 0, totalNumFrames )
   private val pSecs      = new LCDPanel
   private val ggSecs     = {
      val res = new JLabel( "00:00" )
      res.setBorder( BorderFactory.createEmptyBorder( 1, 6, 1, 6 ))
      res.setFont( new Font( "Gulim", Font.PLAIN, 12 ))
      pSecs.setLayout( new BorderLayout )
      pSecs.add( res, BorderLayout.CENTER )
      pSecs.setMinimumSize( pSecs.getPreferredSize )
      pSecs.setMaximumSize( pSecs.getPreferredSize )
      res
   }
   private val pTop     = {
      val res = Box.createHorizontalBox
      res.setBorder( BorderFactory.createEmptyBorder( 4, 4, 4, 4 ))
// has no effect...
//      res.setBackground( Color.black )
      res.add( ggProgress )
      res.add( Box.createHorizontalStrut( 8 ))
      res.add( pSecs )
      res.add( Box.createHorizontalGlue() )
      res
   }

   // ---- constructor ----

   {
      f.setResizable( false )
      val cp = f.getContentPane
      cp.add( this, BorderLayout.CENTER )
      cp.add( pTop, BorderLayout.NORTH )
      init()
      f.pack()
      f.setLocationRelativeTo( null )
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setVisible( true )
   }

   override def getPreferredSize = new Dimension( videoWidth, videoHeight )

   import core._
//   import video._
   import PConstants._

//   var raspadMovie : Movie = _
//   var outMovie : MovieMaker = _
//   var raspadDuration = 0.0
   var framesWritten = 0

//   lazy val layers         = List( TitleLayer( this ), RaspadLayer( this ))
   lazy val layers         = {
      val title   = TitleLayer( this )
      val raspad  = RaspadLayer( this, title.stopTime )
      val sonoRec = SonogramLayer.Recorder()
      // ...
      val sono    = SonogramLayer( this, sonoRec.build, raspad.stopTime )
      List( title, raspad, sono )
   }
   lazy val totalDuration  = layers.map( _.stopTime ).max
   lazy val totalNumFrames = (totalDuration * videoFPS + 0.5).toInt + 1

   var now           = 0.0

   override def setup() {
//      noLoop()
//      frameRate( 1 )
      frameRate( 100 ) // videoFPS
      size( videoWidth, videoHeight )
      background( 0 )
      colorMode( RGB, 1.0f )
   }

   override def draw() {
      background( 0 )
      now = framesWritten.toDouble / videoFPS

      layers.foreach( _.draw() )

      if( framesWritten < totalNumFrames ) {
         val outFile = new File( outputFolder, "frame" + (framesWritten + 10000).toString.substring( 1 ) + ".png" )
         save( outFile.getPath )

         framesWritten += 1
         ggProgress.setValue( framesWritten )
         val secs = now.toInt
         ggSecs.setText( (100 + (secs / 60)).toString.substring( 1 ) + ":" + ((secs % 60) + 100).toString.substring( 1 ))

      } else {
         noLoop()
         println( "\nDone." )
      }
   }
}