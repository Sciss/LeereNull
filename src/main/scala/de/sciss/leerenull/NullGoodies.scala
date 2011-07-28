package de.sciss.leerenull

import java.io.File

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
}