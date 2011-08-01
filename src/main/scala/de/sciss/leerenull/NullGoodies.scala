/*
 *  NullGoodies.scala
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

   def dbMetaFile( plain: String )   : File  = new File( LeereNull.databaseFolder,  plain + "_feat.xml" )
   def extrMetaFile( plain: String ) : File  = new File( LeereNull.extractorFolder, plain + "_feat.xml" )
   def featureFile( plain: String )  : File  = new File( LeereNull.databaseFolder,  plain + "_feat.aif" )

   def stampedFile( folder: File, id: String, ext: String, date: Date = new Date() ) : File = {
      val rem = id + ext
      require( !rem.contains( '\'' ), "Invalid characters " + rem )
      val df = new SimpleDateFormat( "yyMMdd'_'HHmmss'_" + rem + "'", Locale.US )
      new File( folder, df.format( date ))
   }
}