/*
 *  IncorporateBounce.scala
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

import de.sciss.app.AbstractApplication
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.JOptionPane
import java.io.File
import eu.flierl.grouppanel.GroupPanel
import de.sciss.common.BasicWindowHandler
import swing.event.SelectionChanged
import collection.JavaConversions
import de.sciss.kontur.gui.TimelineFrame
import de.sciss.strugatzki.aux.{ProcessorCompanion, Processor}
import actors.Actor
import de.sciss.strugatzki.Span
import swing.{Swing, BorderPanel, ListView}
import annotation.tailrec
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFile}
import de.sciss.kontur.session.{FadeSpec, AudioTrack, AudioRegion, AudioFileElement, BasicTimeline, Session}

object IncorporateBounce extends ProcessorCompanion with GUIGoodies with KonturGoodies with NullGoodies with PasteCompanion {
   type PayLoad = (IIdxSeq[ AudioFileElement ], IIdxSeq[ (AudioTrack, AudioRegion) ])

   var VERBOSE = false

   def showGUI() {
      val app  = AbstractApplication.getApplication
      val dh   = app.getDocumentHandler
      val tls  = IIdxSeq.tabulate( dh.getDocumentCount )( dh.getDocument ).flatMap {
         case s: Session =>
            val tls1 = s.timelines.toList.toIndexedSeq
            tls1.collect {
               case btl: BasicTimeline => (s, btl)
            }
         case _ => IIdxSeq.empty[ (Session, BasicTimeline) ]
      }

      if( tls.size < 2 ) {
         println( "There must be at least two timelines" )
         return
      }

      val names = tls.map {
         case (doc, tl) => doc.name.getOrElse( "<untitled>" ) + " : " + tl.name
      }

      var preTL      = tls( 0 )
      var postTL     = tls( 1 )
      var bncAFE     = Option.empty[ AudioFileElement ]
      var bncARs     = IndexedSeq.empty[ AudioRegion ]
      var bncDir     = LeereNull.bounceFolder
      var ok         = false

      val ggOff            = timeField( "Bounce Offset:", 0.0, 600.0, 0.0 )( _ => () )
      val ggFolder         = textField( bncDir.getAbsolutePath, 30 )( str => bncDir = new File( str ))
      val lbFolder         = label( "Synthetic Audio Files Folder:" )
      val ggSelectFolder   = button( "^" ) { b =>
         val res = openFileDialog( "Select folder", new File( ggFolder.text, "Ignore" ))
         res.foreach { f =>
            bncDir = f.getParentFile
            ggFolder.text = bncDir.getAbsolutePath
         }
      }

      def checkPostTimeline() {
         ok = false
         val (_, tl) = postTL
         JavaConversions.asScalaIterator( app.getWindowHandler.getWindows ).collect({
            case tlf: TimelineFrame if( tlf.timelineView.timeline == tl ) => tlf
         }).toList.headOption match {
            case Some( tlf ) =>
               val tlv = tlf.timelineView
               val trl = tlf.tracksPanel
               bncARs = selectedAudioRegions( tl, tlv, trl ).filterNot( _.muted )
               if( bncARs.nonEmpty ) {
                  val afs = bncARs.map( _.audioFile ).toSet.toSeq
                  afs match {
                     case Seq( bnc ) =>
                        bncAFE = Some( bnc )
//                        val over = bncARs.sliding( 2, 1 ).collect {
//                           case Seq( a, b ) if( a.span overlaps b.span ) => (a, b)
//                        }
//                        if( over.isEmpty ) {
                           val RegOff = """.+\@(\d{1,2})\'(\d{2})".+""".r
                           bnc.name match {
                              case RegOff( mins, secs ) =>
                                 ggOff.decimal = mins.toInt * 60 + secs.toInt
                              case _ =>
                           }
                           ok = true
                           println( "Selection ok." )

//                        } else {
//                           println( "Regions in bounce cannot overlap. Found the following overlaps:" )
//                           over.take( 10 ).foreach {
//                              case (a, b) => println( a.name + " : " + a.span + " -- " + b.name + " : " + b.span )
//                           }
//                           if( over.size > 10 ) println( "..." )
//                        }

                     case _ =>
                        println( "Selection in post timeline must come from exactly one audio file (the bounce)" )
                        println( "However, it currently contains " + afs.size + " files:" )
                        afs.take( 10 ).foreach( e => println( e.name ))
                        if( afs.size > 10 ) println( "..." )
                  }

               } else {
                  println( "Post timeline does not have selected audio regions" )
               }
            case _ => println( "Could not find window belong to timeline " + tl )
         }
      }

      val listPre  = new ListView( names ) {
         listenTo( selection )
         reactions += {
            case SelectionChanged( _ ) => selection.indices.headOption.foreach { idx =>
               preTL = tls( idx )
            }
         }
      }
      val listPost = new ListView( names ) {
         listenTo( selection )
         reactions += {
            case SelectionChanged( _ ) => selection.indices.headOption.foreach { idx =>
               postTL = tls( idx )
               checkPostTimeline()
            }
         }
      }

      listPre.selectIndices(  0 )
      listPost.selectIndices( 1 )

      lazy val pane2 = new GroupPanel {
         theHorizontalLayout is Sequential(
            Parallel( ggOff, Sequential(
               lbFolder, ggFolder, ggSelectFolder )
            )
         )
         theVerticalLayout is Sequential(
            ggOff,
            Parallel( Baseline )( lbFolder, ggFolder, ggSelectFolder )
         )
      }

      val pane = new BorderPanel {
         add( listPre, BorderPanel.Position.West )
         add( listPost, BorderPanel.Position.East )
         add( pane2, BorderPanel.Position.South )
      }

      val op = new JOptionPane( pane.peer, JOptionPane.INFORMATION_MESSAGE, JOptionPane.OK_CANCEL_OPTION )
      BasicWindowHandler.showDialog( op, null, "Incorporate Bounce" ) match {
         case JOptionPane.OK_OPTION =>
            if( ok ) {
               println( "Preparing rendering..." )
               val off        = (ggOff.decimal * bncAFE.map( _.sampleRate ).getOrElse( 44100.0 ) + 0.5).toLong
               val bncAR1     = bncARs.head
               val bncStart   = bncAR1.span.start - bncAR1.offset
               val delta      = off - bncStart
               val bncARsOff  = if( delta != 0 ) bncARs.map( _.move( delta )) else bncARs
               render( bncARsOff.toIndexedSeq, bncDir, preTL._1, preTL._2 )
            } else {
               println( "Wooop. Settings wrong. Try again." )
            }
         case _ =>
      }
   }

   def render( bncARs: IIdxSeq[ AudioRegion ], bncDir: File, preDoc: Session, preTL: BasicTimeline ) {
      // don't filter the muted ones at this point, because the process will do it,
      // and then they will be automatically removed after the render finishes.
      val preARs = collectAudioRegions({ case x => x })( preTL ).sortBy( _._2.span.start ).toIndexedSeq

      val dlg  = progressDialog( "Incorporate Bounce" )
      val proc = IncorporateBounce( bncARs, bncDir, preTL, preARs ) {
         case IncorporateBounce.Success( (newFiles, newRegions) ) =>
            dlg.stop()
            Swing.onEDT( pasteResult( preDoc, preTL, preARs, newRegions, newFiles ))

         case IncorporateBounce.Failure( e ) =>
            dlg.stop()
            e.printStackTrace()

         case IncorporateBounce.Aborted =>
            dlg.stop()

         case IncorporateBounce.Progress( i ) => dlg.progress = i
      }
      dlg.start( proc )
   }

   def apply( bncARs: IIdxSeq[ AudioRegion ], bncDir: File, preTL: BasicTimeline, preARs: IIdxSeq[ (AudioTrack, AudioRegion) ])
            ( observer: Observer ) : IncorporateBounce =
      new IncorporateBounce( observer, bncARs, bncDir, preTL, preARs )
}
class IncorporateBounce( protected val observer: IncorporateBounce.Observer, bncARs: IIdxSeq[ AudioRegion ],
                         bncDir: File, preTL: BasicTimeline, preARs: IIdxSeq[ (AudioTrack, AudioRegion) ])
extends Processor {
   import IncorporateBounce._

   protected val companion = IncorporateBounce

   private implicit def richSpan[ S <% Span ]( span: S ) : RichSpan = new RichSpan( span )

   // bug in sciss lib intersection for non-overlapping spans
   private final class RichSpan( a: Span ) {
      def ø( b: Span ) : Span = {
         if( a overlaps b ) {
            Span( math.max( a.start, b.start ), math.min( a.stop, b.stop ))
         } else Span( a.start, a.start )
      }

      def notSame( b: Span ) : Boolean = (!a.isEmpty || !b.isEmpty) && ((a.start != b.start) || (a.stop != b.stop))
   }

   private def withEachChannel( buf: Array[ Array[ Float ]])( fun: Array[ Float ] => Unit ) {
      var ch = 0; while( ch < buf.length ) {
         fun( buf( ch ))
      ch += 1 }
   }

   private def fadeIn( fd: FadeSpec, pos: Long, len: Int )( buf: Array[ Float ]) {
      var i = 0; while( i < len ) {
         val f = ((pos + i).toDouble / fd.numFrames).toFloat
         if( f < 1f ) {
            buf( i ) *= fd.shape.levelAt( math.max( 0f, f ), 0f, 1f )
         }
      i += 1 }
   }

   private def fadeOut( fd: FadeSpec, pos: Long, len: Int )( buf: Array[ Float ]) {
      var i = 0; while( i < len ) {
         val f = ((pos + i).toDouble / fd.numFrames).toFloat
         if( f > 0f ) {
            buf( i ) *= fd.shape.levelAt( math.min( 1f, f ), 1f, 0f )
         }
      i += 1 }
   }

   protected def body() : Result = {
      // first, select only those regions which are somehow covered and not muted
      val numIn = preARs.size
      var newFiles = IIdxSeq.empty[ AudioFileElement ]
      val out = preARs.zipWithIndex.flatMap { case ((at, ar), idx) =>
         if( ar.muted ) IIdxSeq.empty else {
            val preSpan = ar.span
            val overs = bncARs.filter( _.span.overlaps( preSpan ))
            val res = overs.map { over =>
               val ar1        = if( over.gain == 1 ) ar else ar.copy( gain = ar.gain * over.gain )
               // see if pre region's fades are affected by the cut
               val postSpan   = over.span ø preSpan
               assert( !postSpan.isEmpty )
               val preFadeIn  = {
                  val len = ar1.fadeIn.map( _.numFrames ).getOrElse( 0L )
                  Span( preSpan.start, preSpan.start + len )
               }
               val preFadeOut  = {
                  val len = ar1.fadeOut.map( _.numFrames ).getOrElse( 0L )
                  Span( preSpan.stop - len, preSpan.stop )
               }
               val postFadeIn    = preFadeIn ø postSpan
               val postFadeOut   = preFadeOut ø postSpan
               // whether we need to bounce because the pre bounce fades are cut
               val needsBounce1  = (!postFadeIn.isEmpty  && (postFadeIn notSame preFadeIn)) ||
                                   (!postFadeOut.isEmpty && (postFadeOut notSame preFadeOut))

               val bncPreFadeIn  = {
                  val len = over.fadeIn.map( _.numFrames ).getOrElse( 0L )
                  Span( over.span.start, over.span.start + len )
               }
               val bncPreFadeOut = {
                  val len = over.fadeOut.map( _.numFrames ).getOrElse( 0L )
                  Span( over.span.stop - len, over.span.stop )
               }
               val bncPostFadeIn    = bncPreFadeIn ø postSpan
               val bncPostFadeOut   = bncPreFadeOut ø postSpan
               // whether we need to bounce because the post bounce fades are cut
               val needsBounce2     = (!bncPostFadeIn.isEmpty  && (bncPostFadeIn notSame bncPreFadeIn)) ||
                                      (!bncPostFadeOut.isEmpty && (bncPostFadeOut notSame bncPreFadeOut))

               // whether we need to bounce because of concurrent pre and post bounce fades
               val needsBounce3  = postFadeIn.overlaps(  bncPostFadeIn ) ||
                                   postFadeIn.overlaps(  bncPostFadeOut ) ||
                                   postFadeOut.overlaps( bncPostFadeIn ) ||
                                   postFadeOut.overlaps( bncPostFadeOut )

               val arOut = if( !(needsBounce1 || needsBounce2 || needsBounce3) ) {  // lucky
                  val ar2 = ar1.copy( span = postSpan )
                  val ar3 = if( bncPostFadeIn.isEmpty )  ar2 else {
                     ar2.copy( fadeIn  = over.fadeIn.map(  _.copy( numFrames = bncPostFadeIn.length )))
                  }
                  val ar4 = if( bncPostFadeOut.isEmpty ) ar3 else {
                     ar3.copy( fadeOut = over.fadeOut.map( _.copy( numFrames = bncPostFadeOut.length )))
                  }
                  ar4

               } else {

                  if( VERBOSE ) {
                     println( "ar " + needsBounce1 + "/" + needsBounce2 + "/" + needsBounce3 + " -> old = " + ar1.span + " -> new = " + postSpan )
                     println( " ; _preIn  = " + preFadeIn + " (" + ar1.fadeIn + ") -> postIn  = " + postFadeIn )
                     println( " ; _preOut = " + preFadeOut + " (" + ar1.fadeOut + ") -> postOut = " + postFadeOut )
                     println( " ; BpreIn  = " + bncPreFadeIn + " (" + over.fadeIn + ") -> postIn  = " + bncPostFadeIn )
                     println( " ; BpreOut = " + bncPreFadeOut + " (" + over.fadeOut + ") -> postOut = " + bncPostFadeOut )
                  }

                  @tailrec def newFileName( prefix: String, cnt: Int, suffix: String ) : File = {
                     val test = new File( bncDir, prefix + cnt + suffix )
                     if( !test.exists() ) test else newFileName( prefix, cnt + 1, suffix )
                  }
                  val inF     = ar.audioFile.path
                  val outF    = newFileName( plainName( inF ) + "Fd", 1, ".aif" )
                  val afIn    = AudioFile.openRead( inF )
                  try {
                     val afOut   = AudioFile.openWrite( outF,
                        afIn.spec.copy( fileType = AudioFileType.AIFF, sampleFormat = SampleFormat.Float, byteOrder = None ))
                     val afOff   = ar1.offset + (postSpan.start - ar1.span.start)
                     afIn.seek( afOff )
                     val bufSz   = 8192
                     val buf     = afIn.buffer( bufSz )
                     var pos     = postSpan.start
                     val stop    = postSpan.stop
                     while( pos < postSpan.stop ) {
                        val chunk = math.min( stop - pos, bufSz ).toInt
                        afIn.read( buf, 0, chunk )
                        if( !preFadeIn.isEmpty ) ar1.fadeIn.foreach { fd =>
                           withEachChannel( buf )( fadeIn( fd, pos - preFadeIn.start, chunk ))
                        }
                        if( !bncPreFadeIn.isEmpty ) over.fadeIn.foreach { fd =>
                           withEachChannel( buf )( fadeIn( fd, pos - bncPreFadeIn.start, chunk ))
                        }
                        if( !preFadeOut.isEmpty ) ar1.fadeOut.foreach { fd =>
                           withEachChannel( buf )( fadeOut( fd, pos - preFadeOut.start, chunk ))
                        }
                        if( !bncPreFadeOut.isEmpty ) over.fadeOut.foreach { fd =>
                           withEachChannel( buf )( fadeOut( fd, pos - bncPreFadeOut.start, chunk ))
                        }
                        afOut.write( buf, 0, chunk )
                        pos += chunk
                     }

                     try {
                        val afeNew  = AudioFileElement( outF, afOut.numFrames, afOut.numChannels, afOut.sampleRate )
                        newFiles :+= afeNew
                        ar1.copy( span = postSpan, fadeIn = None, fadeOut = None, audioFile = afeNew, offset = 0L )
                     } finally {
                        afOut.cleanUp()
                     }
                  } finally {
                     afIn.cleanUp()
                  }
               }
               (at, arOut)
            }

            progress( (idx + 1).toFloat / numIn )
            if( checkAborted ) return Aborted
            res
         }
      }

      Success( (newFiles, out) )
   }

   protected val Act = new Actor {
      def act() {
         ProcT.start()
         var result : Result = null
         loopWhile( result == null ) {
            react {
               case Abort =>
                  ProcT.aborted = true
                  aborted()
               case res: Progress =>
                  observer( res )
               case res @ Aborted =>
                  result = res
               case res: Failure =>
                  result = res
               case res: Success =>
                  result = res
            }
         } andThen { observer( result )}
      }
   }
}