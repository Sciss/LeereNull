package de.sciss.leerenull

import de.sciss.kontur.{Main => Kontur}
import de.sciss.gui.{MenuItem, MenuGroup}
import java.awt.event.ActionEvent
import collection.JavaConversions
import collection.breakOut
import de.sciss.kontur.gui.{BasicTrackList, TrackListEditor, TrackList, TimelineView, BasicTimelineView, TimelineFrame}
import de.sciss.kontur.session.{AudioTrack, AudioRegion, BasicTimeline, Session}
import de.sciss.kontur.edit.Editor
import de.sciss.app.{AbstractWindow, AbstractCompoundEdit}
import java.awt.{BorderLayout, EventQueue}
import javax.swing.{JLabel, JPanel, WindowConstants, JOptionPane, Action, KeyStroke, AbstractAction}

object LeereNull extends Runnable {
   def main( args: Array[ String ]) {
      EventQueue.invokeLater( this )
   }

   def run() {
      val app  = new Kontur( Array() )

      def action( name: String, ks: String = "" )( thunk: => Unit ) = new AbstractAction( name ) {
         if( ks != "" ) {
            putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( ks ))
         }

         def actionPerformed( e: ActionEvent ) {
            thunk
         }
      }

      def currentDoc : Session = app.getDocumentHandler.getActiveDocument.asInstanceOf[ Session ]

      def withTimeline( fun: (Session, BasicTimeline, BasicTimelineView, BasicTrackList) => Unit ) {
         val doc = currentDoc
         if( doc != null ) {
            doc.timelines.toList.filterNot( _.name.startsWith( "$" )).headOption.foreach {
               case tl: BasicTimeline =>
                  JavaConversions.asScalaIterator( app.getWindowHandler.getWindows ).collect({
                     case tlf: TimelineFrame if( tlf.timelineView.timeline == tl ) => tlf
                  }).toList.headOption.foreach { tlf =>
                     fun( doc, tl, tlf.timelineView, tlf.tracksPanel )
                  }
               case _ =>
            }
         }
      }

      def selSpan( implicit tlv: TimelineView ) = tlv.selection.span
      def selTracks( implicit tl: BasicTimeline, trl: BasicTrackList ) : IndexedSeq[ AudioTrack ] =
         tl.tracks.toList.collect({
            case at: AudioTrack if( trl.getElement( at ).map( _.selected ).getOrElse( false )) => at
         })( breakOut )

//      def audioRegionsOverlapping( span: Span )( implicit tl: BasicTimeline ) : IndexedSeq[ AudioRegion ] = {
//         tl.tracks.getStakes( span, true )({
//            case ar: AudioRegion => true
//            case _ => false
//         }).collect({ case ar: AudioRegion => ar })( breakOut ) // ouch
//      }

      def selectedAudioRegions( implicit tl: BasicTimeline, tlv: TimelineView, trl: BasicTrackList ) : IndexedSeq[ AudioRegion ] = {
         val tr   = selTracks
         val span = selSpan
         val ars  = tr.flatMap { t =>
            t.trail.getRange( span ).toIndexedSeq
         }
         ars.sortBy( _.span.start )
      }

      def message( text: String ) {
         JOptionPane.showMessageDialog( null, text )
      }

      val mf   = app.getMenuFactory
      val mg   = new MenuGroup( "leerenull", "Leere Null" )
      val miExtractor = new MenuItem( "leerenull.extractor", action( "Extractor...", "control E" ) {
         withTimeline { (doc, tl, tlv, trl) =>
            implicit val doc0 = doc
            implicit val tl0  = tl
            implicit val tlv0 = tlv
            implicit val trl0 = trl

            selectedAudioRegions match {
               case IndexedSeq( ar ) => makeExtractor( ar )
               case _ => message( "Must have exactly one audio region selected" )
            }
         }
      })
      mf.add( mg )
      mg.add( miExtractor )
   }

   implicit def editorCanTry( editor: Editor ) = new EditorHasTry( editor )
   final class EditorHasTry( editor: Editor ) {
      def tryEdit[ A ]( name: String )( fun: AbstractCompoundEdit => A ) : A = {
         val ce      = editor.editBegin( name )
         var cancel  = true
         try {
            val res  = fun( ce )
            editor.editEnd( ce )
            cancel   = false
            res
         } finally {
            if( cancel ) editor.editCancel( ce )
         }
      }
   }

   def makeExtractor( ar: AudioRegion )( implicit doc: Session ) {
      val tls  = doc.timelines
      val tl   = tls.tryEdit( "Add Extractor Timeline" ) { ce =>
         val tl   = BasicTimeline.newEmpty( doc )
         tl.name  = "$Extractor"
         tls.editInsert( ce, tls.size, tl )
         tl
      }

      val tlf = new TimelineFrame( doc, tl )
      tlf.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
      tlf.addListener( new AbstractWindow.Listener {
         def windowClosing( e: AbstractWindow.Event ) {
            println( "Wooha" )
            tlf.dispose()
         }

         def windowClosed(p1: AbstractWindow.Event) {}
         def windowDeactivated(p1: AbstractWindow.Event) {}
         def windowDeiconified(p1: AbstractWindow.Event) {}
         def windowOpened(p1: AbstractWindow.Event) {}
         def windowActivated(p1: AbstractWindow.Event) {}
         def windowIconified(p1: AbstractWindow.Event) {}
      })

      val cp = tlf.getContentPane
      val cp1 = new JPanel( new BorderLayout )
      tlf.setContentPane( cp1 )
      cp1.add( cp, BorderLayout.CENTER )
      cp1.add( new JLabel( "testin one two" ), BorderLayout.SOUTH )
      tlf.revalidate()
   }
}