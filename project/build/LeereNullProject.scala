import sbt._

class LeereNullProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val strugatzki    = "de.sciss" %% "strugatzki" % "0.12"
   val kontur        = "de.sciss" %% "kontur" % "0.15"
   val scalaSwing    = "org.scala-lang" % "scala-swing" % "2.9.0-1"
   val fscapeJobs    = "de.sciss" %% "fscapejobs" % "0.15"
   val itextpdf      = "com.itextpdf" % "itextpdf" % "5.1.1"

   val repo1         = "Clojars Repository" at "http://clojars.org/repo" // this is needed for JSyntaxPane
   val itextRepo     = "itextpdf.com" at "http://maven.itextpdf.com"

   val camelCaseName          = "LeereNull"
   def appBundleName          = camelCaseName + ".app"
   def appBundleContentsPath  = appBundleName / "Contents"
   def appBundleJavaPath      = appBundleContentsPath / "Resources" / "Java"

   private val jarExt                 = ".jar"
   private val jarFilter: FileFilter  = "*" + jarExt

   private def allJarsPath = (publicClasspath +++ buildLibraryJar +++ buildCompilerJar +++ jarPath) ** jarFilter

   def packageAppTask = task {
      val jarsPath               = allJarsPath
      val javaPath               = appBundleJavaPath
      val cleanPaths             = javaPath * jarFilter
      val quiet                  = false
      val versionedNamePattern   = "(.*?)[-_]\\d.*\\.jar".r // thanks to Don Mackenzie

      FileUtilities.clean( cleanPaths.get, quiet, log )

      for( fromPath <- jarsPath.get ) {
         val vName = fromPath.asFile.getName
         if( !vName.contains( "-javadoc" ) && !vName.contains( "-sources" )) {
            val plainName     = vName match {
               case versionedNamePattern( name ) => name + jarExt
               case n => n
            }
            val toPath = javaPath / plainName
            log.log( if(quiet) Level.Debug else Level.Info, "Copying to file " + toPath.asFile )
            FileUtilities.copyFile( fromPath, toPath, log )
         }
      }

      None // what is this for?
   }

   protected def packageAppAction =
      packageAppTask.dependsOn( `package` ) describedAs "Copies all relevant jars into the OS X app bundle."

   lazy val packageApp = packageAppAction
}