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
import core.PApplet
import de.sciss.gui.j.LCDPanel
import javax.swing.{BorderFactory, Box, JLabel, JProgressBar, WindowConstants, JFrame}
import java.awt.{Font, Dimension, BorderLayout, EventQueue}
import java.io.{FileFilter, PrintStream, File}

object Video extends App {
   sealed trait Mode { def start: Double }
   final case class Write( start: Double = 0.0 ) extends Mode
   final case class Realtime( start: Double = 0.0 ) extends Mode
   case object Offline extends Mode { val start = 0.0 }

   val mode: Mode = Offline

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
   private val ggProgress = mode match {
      case Offline   => None
      case _         => Some( new JProgressBar( 0, totalNumFrames ))
   }
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
      ggProgress.foreach( res.add )
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

   def dbToAmp( db: Double ) = 20 * math.log10( db )

//   lazy val layers         = List( TitleLayer( this ), RaspadLayer( this ))
   lazy val layers         = {
//      lazy val titleDur = 7.0
//      val title1  = TitleLayer( this,
//         startTime = 0.0,
//         title = "Leere Null 2",
//         fontSize = 72,
//         fadeIn = 2.0,
//         fadeOut = 2.5,
//         duration = titleDur,
//         offY = 120
//      )
//      val title2  = TitleLayer( this,
//         startTime = title1.startTime + 2.5,
//         title = "(Empties)",
//         fontSize = 54,
//         fadeIn = 2.0,
//         fadeOut = 2.5,
//         duration = titleDur - 2.5,
//         offY = 180
//      )
//
//      val raspad  = RaspadLayer( this, title2.stopTime + 1.0 )

      val sonoPageFlips = IndexedSeq( 0, 566933, 911318, 1194431, 1522922 ) :+ 2116800
      val sonoRegions   = dataFolder.listFiles( new FileFilter {
         def accept( f: File ) = {
            val n = f.getName
            n.startsWith( "i" ) && n.endsWith( ".png" )
         }
      }).map( SonogramLayer.Region( _ ))

      var idMap = Map.empty[ Int, SonogramLayer.Region ]
      sonoRegions.foreach { r =>
         val n = r.imageID
         val i = n.indexOf( '_' ) + 1
         val j = n.indexOf( '_', i )
         val id = n.substring( i, j )
         if( id.startsWith( "id" ) && id.charAt( 2 ) != '<' ) {
            val id1 = id.substring( 2 )
            val k = id1.indexOf( '<' )
            val idi = (if( k >= 0 ) id1.substring( 0, k ) else id1).toInt
            idMap += idi -> r
         }
      }
      sonoRegions.foreach { r =>
         val n = r.imageID
         val i = n.indexOf( '_' ) + 1
         val j = n.indexOf( '_', i )
         val id = n.substring( i, j )
         val k = id.indexOf( '<' )
         if( id.startsWith( "id" ) && k >= 0 ) {
//            val idi = id.substring( 0, k ).toInt
            val idFrom = id.substring( k + 1 ).toInt
            val from = idMap( idFrom )
            from.succ :+= r
            r.pred = Some( from )
         }
      }

      val sr = 44100.0
      val sonoCropDur = 0.5
      val sonoMoveDur = 0.5
      val sonoCombiDir = sonoCropDur + sonoMoveDur
//      val sonoAppDur  = 1.0
//      val sonoDissDur = 1.0

      val sonoRec = {
         val rec = SonogramLayer.Recorder()
         import rec._
         sonoRegions.foreach { r =>
            val trkStart   = (r.spanStart - sonoPageFlips( r.page )) / sr
            val spStart    = 0.0
            val spStop     = (r.spanStop - r.spanStart) / sr
            val tStart     = r.spanStart / sr
//            val tDur       = (sonoPageFlips( r.page + 1 ) / sr - (if( r.succ.isEmpty ) 0.0 else sonoCombiDir)) - tStart
            val tDur       = (math.max( r.spanStop, sonoPageFlips( r.page + 1 )) / sr - (if( r.succ.isEmpty ) 0.0 else sonoCombiDir)) - tStart

            branch {
               advance( tStart )
               r.pred match {
                  case Some( pred ) =>
                     if( pred.page == 0 ) { // XXX hardcoded
                        val i = r.imageID.indexOf( "_foff" ) + 5
                        val j = r.imageID.indexOf( '_', i )
                        val foff = r.imageID.substring( i, j ).toLong
                        val trkStartP = foff / sr
                        appear( imageID = r.imageID, gain = 1, trackIdx = pred.trackIdx, trackStart = trkStartP,
                                spanStart = spStart, spanStop = spStop, fadeIn = sonoCropDur )
                        animate( transitDur = sonoMoveDur, deltaTrackIdx = r.trackIdx - pred.trackIdx,
                           deltaTrackStart = trkStart - trkStartP )
                     } else {
                        appear( imageID = r.imageID, gain = 1, trackIdx = pred.trackIdx, trackStart = trkStart,
                                spanStart = spStart, spanStop = spStop, fadeIn = sonoCombiDir )
                     }
                     val pd = tDur - sonoCombiDir
                     if( pd > 0.0 ) prolong( pd )

                  case _ =>
                     unroll( imageID = r.imageID, gain = 1, trackIdx = r.trackIdx, trackStart = trkStart,
                             spanStart = spStart, spanStop = spStop )
                     if( tDur > 0.0 ) prolong( tDur )

               }
               dissolve( if( r.succ.isEmpty ) sonoCombiDir else sonoCropDur )
            }
         }
         rec
      }

//      val sonoRecX = {
//         val r = SonogramLayer.Recorder()
//         import r._
//         unroll( imageID = "raspad", gain = dbToAmp( 0.0 ), trackIdx = 1, trackStart = 0.0, spanStart = 0.0, spanStop = 14.837 /* 15.011 */)
//         val loop1Stop = 14.837
//         val loop2Stop = 22.610
//         branch {
//            val spanStart = 5.622
//            val spanStop  = 7.947
//            val timeDelta = -spanStart
//            crop( transitDur = sonoCropDur, spanStart = spanStart, spanStop = spanStop )
//            animate( transitDur = sonoMoveDur, deltaTrackIdx = -1, deltaTrackStart = timeDelta )
//            prolong( loop2Stop - loop1Stop )
//            dissolve( sonoDissDur )
//         }
//         branch {
//            val spanStart = 7.947
//            val spanStop  = 11.072
//            val timeDelta = 19.691 - loop1Stop - spanStart
//            crop( transitDur = sonoCropDur, spanStart = spanStart, spanStop = spanStop )
//            animate( transitDur = sonoMoveDur, deltaTrackIdx = -1, deltaTrackStart = timeDelta )
//            prolong( loop2Stop - loop1Stop )
//            dissolve( sonoDissDur )
//         }
//         val pedalSpanStart = 16.011
//         advance( pedalSpanStart - loop1Stop )
//         unroll( imageID = "pedale", gain = dbToAmp( 0.0 ), trackIdx = 1, trackStart = pedalSpanStart - loop1Stop, spanStart = 0.0, spanStop = 6.262 )
//prolong( 4.0 )
//         dissolve( 1.0 )
//
////         prolong( 4.0 )
////         branch {
////            crop( 4.0, spanStart = 0.0, spanStop = 4.0 )
////            prolong( 4.0 )
////            animate( 4.0, deltaTrackIdx = -1, deltaTrackStart = 0.0 )
////            dissolve( 4.0 )
////         }
////         branch {
////            crop( 4.0, spanStart = 8.0, spanStop = 15.0 )
////            prolong( 4.0 )
////            animate( 4.0, deltaTrackIdx = -1, deltaTrackStart = -4.0 )
////            dissolve( 4.0 )
////         }
////         advance( 8.0 )
////         dissolve( 8.0 )
//         r
//      }
      // ...

      val sono    = SonogramLayer( this, sonoRec.build, 0.0 ) // raspad.stopTime + 1.0 )
      List( /* title1, title2, raspad, */ sono )
   }
   lazy val totalDuration  = layers.map( _.stopTime ).max
   lazy val totalNumFrames = (totalDuration * videoFPS + 0.5).toInt + 1

   private var _now           = 0.0
   def now = _now + mode.start

   override def setup() {
      mode match {
         case Offline         => noLoop()
         case Realtime( _ )   => frameRate( videoFPS )
         case Write( _ )      => frameRate( 1000 )
      }

      size( videoWidth, videoHeight )
      background( 0 )
      colorMode( RGB, 1.0f )
   }

   override def draw() {
      background( 0 )
      _now = framesWritten.toDouble / videoFPS

      layers.foreach( _.draw() )

      if( framesWritten < totalNumFrames ) {
         mode match {
            case Write( _ ) =>
               val outFile = new File( outputFolder, "frame" + (framesWritten + 10000).toString.substring( 1 ) + ".png" )
               save( outFile.getPath )
            case _ =>
         }

         framesWritten += 1
         ggProgress.foreach( _.setValue( framesWritten ))
         val secs = now.toInt
         ggSecs.setText( (100 + (secs / 60)).toString.substring( 1 ) + ":" + ((secs % 60) + 100).toString.substring( 1 ))

      } else {
         noLoop()
         println( "\nDone." )
      }
   }
}