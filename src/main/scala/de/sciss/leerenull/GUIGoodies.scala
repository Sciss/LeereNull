/*
 *  GUIGoodies.scala
 *  (LeereNull)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
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
 *
 *
 *  Changelog:
 */

package de.sciss.leerenull

import java.util.Locale
import de.sciss.kontur.session.{BasicTimeline, Session}
import swing.event.{ValueChanged, ButtonClicked}
import java.text.NumberFormat
import de.sciss.util.NumberSpace
import annotation.switch
import de.sciss.gui.{NumberEvent, NumberListener, NumberField, TimeFormat}
import java.awt.event.{InputEvent, KeyEvent, KeyAdapter, ActionEvent}
import de.sciss.strugatzki.{FeatureCorrelation, Span}
import javax.swing.event.{AncestorEvent, AncestorListener}
import swing.{Swing, ProgressBar, Action, FlowPanel, Slider, Label, Component, Button}
import javax.swing.{JPanel, JOptionPane, WindowConstants, JDialog, JComponent, InputMap, AbstractAction, Action => JAction, KeyStroke}

trait GUIGoodies {
   def action( name: String, ks: String = "" )( thunk: => Unit ) = new AbstractAction( name ) {
      if( ks != "" ) {
         putValue( JAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke( ks ))
      }

      def actionPerformed( e: ActionEvent ) {
         thunk
      }
   }

   def message( text: String ) {
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

   def label( txt: String, fixedWidth: Option[ Int ] = None ) = new Label {
      text = if( txt != "" || fixedWidth.isEmpty ) txt else " " // sucky bug ??!!!
      peer.putClientProperty( "JComponent.sizeVariant", "small" )
      fixedWidth.foreach( i => constrainWidth( this, i ))
   }

   trait ProgressDialog {
      type Process = { def start() : Unit; def abort() : Unit }

      def start( p: Process ) : Unit
      def progress : Int
      def progress_=( i: Int ) : Unit
      def stop() : Unit
   }

   def progressDialog( title: String ) = new ProgressDialog {
      var process : Process = null

      val pb = new ProgressBar {
         peer.addAncestorListener( new AncestorListener {
            def ancestorAdded( e: AncestorEvent ) {
               process.start()
            }

            def ancestorMoved( e: AncestorEvent ) {}
            def ancestorRemoved( e: AncestorEvent ) {}
         })
      }

      def progress = pb.value
      def progress_=( i: Int ) { pb.value = i }

      val progressPane = new FlowPanel {
         contents += label( "Processing..." )
         contents += pb
      }

      var dlg: JDialog = null
      val optionAbort = button( "Abort" ) { b =>
         val p = process; if( p != null ) p.abort()
      }

      new JOptionPane()
      val op = new JOptionPane( progressPane.peer, JOptionPane.INFORMATION_MESSAGE,
         JOptionPane.OK_CANCEL_OPTION, null, Array[ AnyRef ]( optionAbort.peer ), null )
      dlg = op.createDialog( null, title )
      dlg.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )

      def start( p: Process ) {
         Swing.onEDT {
            process = p
            dlg.setVisible( true )
         }
      }

      def stop() {
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
                   ( act: Int => Unit ) = new FlowPanel with IntegerWidget {

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
         def actionPerformed( e: ActionEvent ) {
            val value = j.getNumber.intValue + amount
            if( value >= min && value <= max ) {
               j.setNumber( value )
            }
         }
      }

      amap.put( "leerenull.up", inc( 1 ))
      amap.put( "leerenull.dn", inc( -1 ))
      imap.put( KeyStroke.getKeyStroke( KeyEvent.VK_UP, 0 ), "leerenull.up" )
      imap.put( KeyStroke.getKeyStroke( KeyEvent.VK_DOWN, 0 ), "leerenull.dn" )

      j.putClientProperty( "JComponent.sizeVariant", "small" )
      val list = new NumberListener {
         def numberChanged( e: NumberEvent ) {
            if( !e.isAdjusting ) act( integer )
         }
      }
      j.addListener( list )
      contents += lab
      contents += Component.wrap( j )
      if( initial != min ) integer = initial

      def integer = j.getNumber.intValue
      def integer_=( value: Int ) {
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
         def actionPerformed( e: ActionEvent ) {
            val value = math.max( min, math.min( max, j.getNumber.doubleValue + amount ))
//            if( value >= min && value <= max ) {
               j.setNumber( value )
//            }
         }
      }

      def addAction( code: Int, modifiers: Int, amount: Double ) {
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
         def numberChanged( e: NumberEvent ) {
            if( !e.isAdjusting ) act( decimal )
         }
      }
      j.addListener( list )
      contents += lab
      contents += Component.wrap( j )
      if( initial != min ) decimal = initial

      def decimal = j.getNumber.doubleValue
      def decimal_=( d: Double ) {
         j.removeListener( list )
         try {
            j.setNumber( d )
         } finally {
            j.addListener( list )
         }
      }
   }

   def decimalSlider( minLb: String = "min", maxLb: String = "max", initial: Double = 0.0, steps: Int = 1000, w: Int = 72 )
                    ( act: Double => Unit ) = new FlowPanel with DecimalWidget {
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
      def decimal_=( value: Double ) {
         val i = (value * steps + 0.5).toInt
         slid.deafTo( slid )
         try {
            slid.value = i
         } finally {
            slid.listenTo( slid )
         }
      }
   }

   def setPreferredWidth( c: Component, w: Int ) {
      val d = c.preferredSize
      d.width = w
      c.preferredSize = d
   }

   def setMinimumWidth( c: Component, w: Int ) {
      val d = c.minimumSize
      d.width = w
      c.minimumSize = d
   }

   def setMaximumWidth( c: Component, w: Int ) {
      val d = c.maximumSize
      d.width = w
      c.maximumSize = d
   }

   def constrainWidth( c: Component, w: Int ) {
      setPreferredWidth( c, w )
      setMinimumWidth( c, w )
      setMaximumWidth( c, w )
   }

   private val timeFormat = new TimeFormat( 0, "", "", 3, Locale.US )

   def timeString( sp: Span )( implicit tl: BasicTimeline ) : String = {
      val sr = tl.rate
      val startSecs = sp.start / sr
      val stopSecs   = sp.stop / sr
      timeFormat.formatTime( startSecs ) + " - " + timeFormat.formatTime( stopSecs )
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