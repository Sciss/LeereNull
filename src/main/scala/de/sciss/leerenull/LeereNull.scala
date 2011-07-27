package de.sciss.leerenull

import de.sciss.kontur.{Main => Kontur}
import de.sciss.gui.{MenuItem, MenuGroup}
import java.awt.event.ActionEvent
import collection.JavaConversions
import collection.breakOut
import de.sciss.kontur.gui.{BasicTrackList, TrackListEditor, TrackList, TimelineView, BasicTimelineView, TimelineFrame}
import de.sciss.kontur.edit.Editor
import de.sciss.app.{AbstractWindow, AbstractCompoundEdit}
import java.awt.{BorderLayout, EventQueue}
import javax.swing.{JLabel, JPanel, WindowConstants, JOptionPane, Action, KeyStroke, AbstractAction}
import de.sciss.io.Span
import de.sciss.kontur.util.Matrix2D
import de.sciss.kontur.session.{SessionElement, MatrixDiffusion, SessionElementSeq, AudioTrack, AudioRegion, BasicTimeline, Session}

object LeereNull extends Runnable {
   def main( args: Array[ String ]) {
      EventQueue.invokeLater( this )
   }

   trait MaybeNot { implicit def none[ A ] : Maybe[ A ] = Maybe( None )}
   object Maybe extends MaybeNot {
      implicit def some[ A ]( implicit a: A ) : Maybe[ A ] = Maybe( Some( a ))
      implicit def unwind[ A ]( mb: Maybe[ A ]) : Option[ A ] = mb.option
   }
   final case class Maybe[ A ]( option: Option[ A ])

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

            cutTheCheese( selectedAudioRegions, selSpan ) match {
               case IndexedSeq( ar ) => makeExtractor( ar )
               case _ => message( "Must have exactly one audio region selected" )
            }
         }
      })
      mf.add( mg )
      mg.add( miExtractor )
   }

   def cutTheCheese( ars: IndexedSeq[ AudioRegion ], span: Span ) : IndexedSeq[ AudioRegion ] = {
      ars.flatMap { ar =>
         if( ar.span.overlaps( span )) {
            IndexedSeq( if( ar.span.contains( span.start )) {
               ar.split( span.start )._2
            } else {
               ar.split( span.stop )._1
            })
         } else IndexedSeq.empty[ AudioRegion ]
      }
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

      def joinEdit[ A ]( name: String )
                       ( fun: AbstractCompoundEdit => A )
                       ( implicit ceo: Maybe[ AbstractCompoundEdit ]) : A = {
         ceo.option match {
            case Some( ce ) => fun( ce )
            case None => tryEdit( name )( fun )
         }
      }
   }

   val acceptAll : AudioTrack => Boolean = _ => true

   def findAudioTrackSpace( span: Span, accept: AudioTrack => Boolean = acceptAll )
                          ( implicit tl: BasicTimeline ) : Option[ AudioTrack ] = {
      tl.tracks.toList.collect({ case at: AudioTrack if( accept( at )) => at })
         .find( _.trail.getRange( span ).isEmpty )
   }

   def provideAudioTrackSpace( span: Span, accept: AudioTrack => Boolean = acceptAll )
                             ( implicit doc: Session, tl: BasicTimeline, ceo: Maybe[ AbstractCompoundEdit ]) : AudioTrack =
      findAudioTrackSpace( span, accept ).getOrElse {
         val ts = tl.tracks
         val ed = ts.editor.get
         ed.joinEdit( "Add track" ) { ce =>
            val at = new AudioTrack( doc )
//         at.diffusion = Some( stereoDiffusion )
            at.name = {
               val set = ts.map( _.name ).toSet
               var i = 0
               var n = ""
               do {
                  i += 1
                  n = "Tr" + i
               } while( set.contains( n ))
               n
            }
            ed.editInsert( ce, ts.size, at )
            at
         }
      }

   def place( ar: AudioRegion, accept: AudioTrack => Boolean = acceptAll )
            ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {

      tl.joinEdit( "Place audio region" ) { implicit ce =>
         val at = provideAudioTrackSpace( ar.span, accept )
         at.trail.editAdd( ce, ar )
         at
      }
   }

   def placeStereo( ar: AudioRegion )
                  ( implicit doc: Session, tl: BasicTimeline, ce: Maybe[ AbstractCompoundEdit ]) : AudioTrack = {
      doc.diffusions.joinEdit( "Place audio region" ) { implicit ce =>
         val d    = provideDiffusion( ar.audioFile.numChannels, 2 )
         val sq   = d.matrix.toSeq
         val at   = place( ar, { at =>
            at.diffusion match {
               case Some( df: MatrixDiffusion ) if( df.matrix.toSeq == sq ) => true
               case _ => false
            }
         })
         if( at.diffusion.isEmpty ) {
            at.diffusion = Some( d )
         }
         at
      }
   }

   def uniqueName( sq: SessionElementSeq[ _ <: SessionElement ], template: String ) : String = {
      var name    = template
      var i       = 0

      while( sq.find( _.name == name ).isDefined ) {
         i    += 1
         name  = template + "[" + i + "]"
      }
      name
   }

   def mixMatrix( inChans: Int, outChans: Int ) : Matrix2D[ Float ] = {
      // ( row, cols ) = (input, output) = InSeq[OutSeq]
      val sq = Seq.tabulate( inChans ) { in =>
//         val inw = if( inChans < 2 ) 1f else (in.toFloat / (inChans - 1))
         Seq.tabulate( outChans ) { out =>
            val outw = if( outChans < 2 ) 1f else (out.toFloat / (outChans - 1))
            val w    = outw * (inChans - 1) // (if( inChans < 2 ) 1 else (inChans - 1))
            math.sqrt( 1f - math.min( 1f, math.abs( w - in ))).toFloat
         }
      }
      Matrix2D.fromSeq( sq )
   }

   def findDiffusion( matrix: Matrix2D[ Float ])( implicit doc: Session ) : Option[ MatrixDiffusion ] = {
      val sq = matrix.toSeq
      doc.diffusions.toList.collect({
         case md: MatrixDiffusion if( md.matrix.toSeq == sq ) => md
      }).headOption
   }

   def provideDiffusion( inChans: Int, outChans: Int )
                       ( implicit doc: Session, ceo: Maybe[ AbstractCompoundEdit ]) : MatrixDiffusion = {
      val m = mixMatrix( inChans, outChans )
      findDiffusion( m ).getOrElse {
         val dfs = doc.diffusions
         dfs.joinEdit( "Add diffusion" ) { ce =>
            val d = new MatrixDiffusion( doc )
            d.matrix = m
            d.name   = inChans + "->" + outChans
            dfs.editInsert( ce, dfs.size, d )
            d
         }
      }
   }

   def makeExtractor( ar: AudioRegion )( implicit doc: Session ) {
      val tls  = doc.timelines
      val ar0  = ar.move( -ar.span.start )
      val tl   = tls.tryEdit( "Add Extractor Timeline" ) { implicit ce =>
         implicit val tl = BasicTimeline.newEmpty( doc )
         tl.span  = ar0.span
         tl.name  = uniqueName( tls, "$Extractor" )
         tls.editInsert( ce, tls.size, tl )
         placeStereo( ar0 )
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