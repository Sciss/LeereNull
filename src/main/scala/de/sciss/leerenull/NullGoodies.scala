/*
 *  NullGoodies.scala
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

import java.io.File
import java.text.SimpleDateFormat
import java.util.{Date, Locale}

trait NullGoodies {
   def plainName( f: File ) : String = {
      val n  = f.getName
      val i  = n.lastIndexOf( '.' )
      val n1 = if( i >= 0 ) n.substring( 0, i ) else n
      if( n1.endsWith( "_feat" )) n1.dropRight( 5 ) else n1
   }

   def dbMetaFile(   plain: String, dir: File = LeereNull.databaseFolder )  : File  = new File( dir, plain + "_feat.xml" )
   def extrMetaFile( plain: String, dir: File = LeereNull.extractorFolder ) : File  = new File( dir, plain + "_feat.xml" )
   def featureFile(  plain: String, dir: File = LeereNull.databaseFolder )  : File  = new File( dir, plain + "_feat.aif" )

   def stampedFile( folder: File, id: String, ext: String, date: Date = new Date() ) : File = {
      val rem = id + ext
      require( !rem.contains( '\'' ), "Invalid characters " + rem )
      val df = new SimpleDateFormat( "yyMMdd'_'HHmmss'_" + rem + "'", Locale.US )
      new File( folder, df.format( date ))
   }
}