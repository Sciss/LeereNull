/*
 *  GUIGoodies.scala
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

import java.util.Locale
import java.text.NumberFormat
import de.sciss.processor.Processor
import de.sciss.span.Span
import de.sciss.util.NumberSpace
import de.sciss.gui.{NumberEvent, NumberListener, NumberField, TimeFormat}
import java.awt.event.{InputEvent, KeyEvent, ActionEvent}
import javax.swing.event.{AncestorEvent, AncestorListener}
import javax.swing.{SwingUtilities, JPanel, JOptionPane, WindowConstants, JDialog, JComponent, AbstractAction, Action => JAction, KeyStroke}
import java.awt.{FileDialog, Component => AWTComponent, Frame => AWTFrame}
import java.io.{FilenameFilter, File}
import scala.concurrent.ExecutionContext
import swing.{TextField, RadioButton, CheckBox, ListView, ComboBox, Swing, ProgressBar, Action, FlowPanel, Slider, Label, Component, Button}
import swing.event.{SelectionChanged, ValueChanged}

trait GUIGoodies {
   def action( name: String, ks: String = "" )( thunk: => Unit ) = new AbstractAction( name ) {
      if( ks != "" ) {
         putValue( JAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke( ks ))
      }

      def actionPerformed( e: ActionEvent ): Unit = {
         thunk
      }
   }

   def message( text: String ): Unit = {
      JOptionPane.showMessageDialog( null, text )
   }

   def button( label: String )( act: Button => Unit ) = new Button {
      but =>
//      text = label
      peer.putClientProperty( "JComponent.sizeVariant", "small" )
      peer.putClientProperty( "JButton.buttonType", "bevel" )
      focusable = false
//      listenTo( this )
//      reactions += {
//         case ButtonClicked( _ ) => act( but )
//      }
      action = Action( label ) { act( but )}
   }

   def combo[ A, B : ListView.Renderer ]( items: Seq[ A ])
                                        ( act:  A => Unit = (_: A) => () )
                                        ( rend: A => B = (a: A) => a.toString ) =
      new ComboBox[ A ]( items ) {
         peer.putClientProperty( "JComponent.sizeVariant", "small" )
         peer.putClientProperty( "JComboBox.isSquare", java.lang.Boolean.TRUE )
         focusable   = false
         renderer    = ListView.Renderer( rend )
         listenTo( selection )
         reactions += {
            case SelectionChanged( _ ) => act( selection.item )
         }
      }

   def openFileDialog( title: String = "Open File", init: File = new File( "" ),
                       filter: File => Boolean = _ => true, parent: AWTComponent = null ) : Option[ File ] =
      fileDialog( FileDialog.LOAD, title, init, filter, parent )

   def saveFileDialog( title: String = "Save File", init: File = new File( "" ),
                       filter: File => Boolean = _ => true, parent: AWTComponent = null ) : Option[ File ] =
      fileDialog( FileDialog.SAVE, title, init, filter, parent )


   private def fileDialog( mode: Int, title: String = "Open File", init: File = new File( "" ),
                       filter: File => Boolean = _ => true, parent: AWTComponent = null ) : Option[ File ] = {
      val f = if( parent != null ) {
         SwingUtilities.getWindowAncestor( parent ) match {
            case fr: AWTFrame => fr
            case _ => null
         }
      } else null

      val dlg = new FileDialog( f, title, mode )
      if( init.isFile || mode == FileDialog.SAVE ) {
         dlg.setDirectory( init.getParent )
         dlg.setFile( init.getName )
      } else {
         dlg.setDirectory( init.getPath )
      }
      dlg.setFilenameFilter( new FilenameFilter {
         def accept( dir: File, name: String ) = filter( new File( dir, name ))
      })
      dlg.setVisible( true )
      val dirName    = dlg.getDirectory
      val fileName   = dlg.getFile
      if( dirName != null && fileName != null ) Some( new File( dirName, fileName )) else None
   }

   def label( txt: String, fixedWidth: Option[ Int ] = None ) = new Label {
      text = if( txt != "" || fixedWidth.isEmpty ) txt else " " // sucky bug ??!!!
      peer.putClientProperty( "JComponent.sizeVariant", "small" )
      fixedWidth.foreach( i => constrainWidth( this, i ))
   }

   def checkBox( txt: String )( fun: Boolean => Unit = (b: Boolean) => () ) = new CheckBox {
      action = Action( txt )( fun( selected ))
      peer.putClientProperty( "JComponent.sizeVariant", "small" )
   }

   def radioButton( txt: String )( fun: => Unit ) = new RadioButton {
      action = Action( txt )( fun )
      peer.putClientProperty( "JComponent.sizeVariant", "small" )
   }

   def textField( txt: String, cols: Int )( fun: String => Unit = (txt: String) => () ) = new TextField( cols ) {
      text = txt
      peer.putClientProperty( "JComponent.sizeVariant", "small" )
      action = Action( "Enter" )( fun( text ))
   }

   trait ProgressDialog {
      def start( p: Processor[_, _] with Processor.Prepared ) : Unit
      var progress : Int
      def stop() : Unit
   }

   def progressDialog( title: String ) = new ProgressDialog {
      var process : Processor[_, _] with Processor.Prepared = null

     import ExecutionContext.Implicits.global

      val pb = new ProgressBar {
         peer.addAncestorListener( new AncestorListener {
           def ancestorAdded  (e: AncestorEvent): Unit = process.start()
           def ancestorMoved  (e: AncestorEvent) = ()
           def ancestorRemoved(e: AncestorEvent) = ()
         })
      }

      def progress = pb.value
      def progress_=( i: Int ): Unit = pb.value = i

      val progressPane = new FlowPanel {
         contents += label( "Processing..." )
         contents += pb
      }

      var dlg: JDialog = null
      val optionAbort = button( "Abort" ) { b =>
         val p = process; if( p != null ) p.abort()
      }

//      new JOptionPane()
      val op = new JOptionPane( progressPane.peer, JOptionPane.INFORMATION_MESSAGE,
         JOptionPane.OK_CANCEL_OPTION, null, Array[ AnyRef ]( optionAbort.peer ), null )
      dlg = op.createDialog( null, title )
      dlg.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )

     def start(p: Processor[_, _] with Processor.Prepared): Unit = {
         Swing.onEDT {
            process = p
            dlg.setVisible( true )
         }
      }

      def stop(): Unit = {
//         Thread.sleep( 4000 )
         Swing.onEDT( dlg.dispose() )
      }
   }

   trait IntegerWidget {
      def integer : Int
      def integer_=( value: Int ) : Unit
   }

   trait DecimalWidget {
      def decimal : Double
      def decimal_=( value: Double ) : Unit
   }

   def integerField( lb: String, min: Int = 0, max: Int = 10, initial: Int = 0 )
                   ( act: Int => Unit = (i: Int) => () ) = new FlowPanel with IntegerWidget {

      val lab  = label( lb )
      val j    = new NumberField( NumberSpace.createIntSpace( min, max ))
      val amap = j.getActionMap
      val imap = j.getInputMap( JComponent.WHEN_FOCUSED )

      override lazy val peer: JPanel =
         new JPanel( new java.awt.FlowLayout( FlowPanel.Alignment.Center.id )) with SuperMixin {
            override def getBaseline( w: Int, h: Int ) : Int = lab.peer.getBaseline( w, h )
         }

      vGap = 0

      def inc( amount: Int ) = new AbstractAction {
         def actionPerformed( e: ActionEvent ): Unit = {
            val value = j.getNumber.intValue + amount
            if( value >= min && value <= max ) {
               j.setNumber( value )
            }
         }
      }

      override def enabled_=( b: Boolean ): Unit = {
         super.enabled = b
         j.setEnabled( b )
      }

      override def requestFocus(): Unit = j.requestFocus()

      amap.put( "leerenull.up", inc( 1 ))
      amap.put( "leerenull.dn", inc( -1 ))
      imap.put( KeyStroke.getKeyStroke( KeyEvent.VK_UP, 0 ), "leerenull.up" )
      imap.put( KeyStroke.getKeyStroke( KeyEvent.VK_DOWN, 0 ), "leerenull.dn" )

      j.putClientProperty( "JComponent.sizeVariant", "small" )
      val list = new NumberListener {
         def numberChanged( e: NumberEvent ): Unit = {
            if( !e.isAdjusting ) act( integer )
         }
      }
      j.addListener( list )
      contents += lab
      contents += Component.wrap( j )
      if( initial != min ) integer = initial

      def integer = j.getNumber.intValue
      def integer_=( value: Int ): Unit = {
         j.removeListener( list )
         try {
            j.setNumber( value )
         } finally {
            j.addListener( list )
         }
      }
   }

   def timeField( lb: String, min: Double = 0.0, max: Double = 60.0, initial: Double = 1.0 )
                ( act: Double => Unit ) = new FlowPanel with DecimalWidget {

      val lab  = label( lb )
      val j    = new NumberField( new NumberSpace( min, max, 0.0 ))
      j.setFlags( NumberField.HHMMSS )
      val amap = j.getActionMap
      val imap = j.getInputMap( JComponent.WHEN_FOCUSED )

      override lazy val peer: JPanel =
         new JPanel( new java.awt.FlowLayout( FlowPanel.Alignment.Center.id )) with SuperMixin {
            override def getBaseline( w: Int, h: Int ) : Int = lab.peer.getBaseline( w, h )
         }

      vGap = 0

      def inc( amount: Double ) = new AbstractAction {
         def actionPerformed( e: ActionEvent ): Unit = {
            val value = math.max( min, math.min( max, j.getNumber.doubleValue + amount ))
//            if( value >= min && value <= max ) {
               j.setNumber( value )
//            }
         }
      }

      def addAction( code: Int, modifiers: Int, amount: Double ): Unit = {
         val id = "leerenull." + (code.toLong << 32) + modifiers
         amap.put( id, inc( amount ))
         imap.put( KeyStroke.getKeyStroke( code, modifiers ), id )
      }

      addAction( KeyEvent.VK_UP, 0, 0.1 )
      addAction( KeyEvent.VK_UP, InputEvent.ALT_MASK, 0.01 )
      addAction( KeyEvent.VK_UP, InputEvent.CTRL_MASK, 1.0 )
      addAction( KeyEvent.VK_DOWN, 0, -0.1 )
      addAction( KeyEvent.VK_DOWN, InputEvent.ALT_MASK, -0.01 )
      addAction( KeyEvent.VK_DOWN, InputEvent.CTRL_MASK, -1.0 )

      j.putClientProperty( "JComponent.sizeVariant", "small" )
      val list = new NumberListener {
         def numberChanged( e: NumberEvent ): Unit = {
            if( !e.isAdjusting ) act( decimal )
         }
      }
      j.addListener( list )
      contents += lab
      contents += Component.wrap( j )
      if( initial != min ) decimal = initial

      def decimal = j.getNumber.doubleValue
      def decimal_=( d: Double ): Unit = {
         j.removeListener( list )
         try {
            j.setNumber( d )
         } finally {
            j.addListener( list )
         }
      }
   }

   def decimalSlider( minLb: String = "min", maxLb: String = "max", initial: Double = 0.0, steps: Int = 1000, w: Int = 72 )
                    ( act: Double => Unit = (d: Double) => () ) = new FlowPanel with DecimalWidget {
      vGap = 0
      val lbMin   = label( minLb )
      val lbMax   = label( maxLb )
      val slid: Slider    = new Slider {
         peer.putClientProperty( "JComponent.sizeVariant", "small" )
         min      = 0
         max      = steps
         listenTo( this )
         reactions += {
            case ValueChanged( _ ) => act( decimal )
         }
      }
      constrainWidth( slid, w )
//      theHorizontalLayout is Sequential( lbMin, slid, lbMax )
//      theVerticalLayout is Parallel( Baseline )( lbMin, slid, lbMax )
      contents ++= Seq( lbMin, slid, lbMax )
      if( initial != 0.0 ) decimal = initial

      def decimal = slid.value.toDouble / steps
      def decimal_=( value: Double ): Unit = {
         val i = (value * steps + 0.5).toInt
         slid.deafTo( slid )
         try {
            slid.value = i
         } finally {
            slid.listenTo( slid )
         }
      }
   }

   def setPreferredWidth( c: Component, w: Int ): Unit = {
      val d = c.preferredSize
      d.width = w
      c.preferredSize = d
   }

   def setMinimumWidth( c: Component, w: Int ): Unit = {
      val d = c.minimumSize
      d.width = w
      c.minimumSize = d
   }

   def setMaximumWidth( c: Component, w: Int ): Unit = {
      val d = c.maximumSize
      d.width = w
      c.maximumSize = d
   }

   def constrainWidth( c: Component, w: Int ): Unit = {
      setPreferredWidth( c, w )
      setMinimumWidth( c, w )
      setMaximumWidth( c, w )
   }

   private val timeFormat = new TimeFormat( 0, "", "", 3, Locale.US )

   def timeString( frames: Long, sr: Double )/*( implicit tl: BasicTimeline )*/ : String = {
//      val sr = 44100.0 // tl.rate
      val secs = frames / sr
      timeFormat.formatTime( secs )
   }

  def timeString(sp: Span, sr: Double) /*( implicit tl: BasicTimeline )*/ : String = {
    timeString(sp.start, sr) + " - " + timeString(sp.stop, sr)
  }

   def percentString( d: Double, numDecimals: Int = 1 ) : String = {
      val nf = NumberFormat.getPercentInstance( Locale.US )
      nf.setMaximumFractionDigits( numDecimals )
      nf.format( d )
   }

   def decibelString( d: Double, numDecimals: Int = 1 ) : String = {
      val nf = NumberFormat.getInstance( Locale.US )
      nf.setMaximumFractionDigits( numDecimals )
      nf.setGroupingUsed( false )
      nf.format( d ) + " dB"
   }
}