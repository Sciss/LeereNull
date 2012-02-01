package de.sciss.leerenull

import processing.{core, video}
import core.PApplet
import javax.swing.{WindowConstants, JFrame}
import java.awt.{Dimension, BorderLayout, EventQueue}
import java.io.{PrintStream, File}

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
      println( "Hello." )
//      Console.err.println( "Hello world." )
      new Video
   }})

   val videoWidth       = 1024
   val videoHeight      = 768
   val videoFPS         = 24
   val raspadStartIdx   = 169
   val raspadStopIdx    = 999
   val folder           = new File( "data" )
}

class Video extends PApplet {
   import Video._

   val f = new JFrame( "Leere Null" );

   // ---- constructor ----

   f.setResizable( false )
   f.getContentPane.add( this, BorderLayout.CENTER )
   init()
   f.pack()
   f.setLocationRelativeTo( null )
   f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
   f.setVisible( true )

   override def getPreferredSize = new Dimension( videoWidth, videoHeight )

   import core._
   import video._

//   var raspadMovie : Movie = _
   var outMovie : MovieMaker = _
//   var raspadDuration = 0.0
   var raspadIdx = 0 // FUCKING PROCESSING RUNS DRAW BEFORE VAR INIT

   override def setup() {
//      noLoop()
//      frameRate( 1 )
      frameRate( videoFPS )
      size( videoWidth, videoHeight )
      background( 0 )
//      val raspadFile = new File( folder, "RaspadExtr.mov" )
//      raspadMovie    = new Movie( this, raspadFile.getPath )
//      raspadDuration = raspadMovie.duration()
//      raspadMovie.play()
//      raspadMovie.loop()
//      raspadIdx = raspadStartIdx
//      println( "AQUI " + raspadIdx )
   }

//   var frameCount = 0

//   def movieEvent( m: Movie ) {
////      cnt += 1
////      f.setTitle( cnt.toString )
//      println( "MOVIE time = " + m.time() )
//      m.read()
//      redraw()
//   }

   override def draw() {
      val now = frameCount.toDouble / videoFPS
//      println( "NOW = " + now )
//      if( now < raspadDuration ) {
//         raspadMovie.jump( now )
//         raspadMovie.read()
//         image( raspadMovie, 0, 0 )
//      }

//      raspadMovie.available()
//      tint( 255, 20 )
//      image( raspadMovie, 0, 0 )

//      frameCount += 1

      if( raspadIdx < (raspadStopIdx - raspadStartIdx) ) {
//         println( "AYA " + raspadIdx )
         val img = loadImage( new File( folder, "RaspadExtr " + (raspadIdx + raspadStartIdx + 10000).toString.substring( 1 ) + ".png" ).getPath )
         image( img, 0, 0 )
         raspadIdx += 1
      }
   }
}