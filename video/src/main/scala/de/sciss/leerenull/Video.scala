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

   val videoWidth    = 1024
   val videoHeight   = 768
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

   lazy val folder = {
//      val cr = new File( "Contents/Resources" )
//      if( cr.isDirectory ) new File( cr, "data" ) else
         new File( "data" )
   }
   var myMovie : Movie = _

   override def setup() {
      frameRate( 1 )
      size( videoWidth, videoHeight )
//      setPreferredSize( getSize )
      background( 0 )
      val movieFile = new File( folder, "station.mov" )
//f.setTitle( movieFile.getAbsolutePath )
      myMovie = new Movie( this, movieFile.getPath )
      myMovie.loop()
   }

   var cnt = 0

   def movieEvent( m: Movie ) {
//      cnt += 1
//      f.setTitle( cnt.toString )
      m.read()
   }

   override def draw() {
//      fill( 255 )
//      rect( videoWidth / 3, videoHeight / 3, videoWidth / 3, videoHeight / 3 )
      tint( 255, 20 )
//      image( myMovie, mouseX - myMovie.width / 2, mouseY - myMovie.height / 2 )
      image( myMovie, 0, 0 )
   }
}