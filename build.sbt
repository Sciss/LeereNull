// import AssemblyKeys._

name           := "LeereNull"

version        := "0.14-SNAPSHOT"

organization   := "de.sciss"

scalaVersion   := "2.9.1"

description := "Materials and tools for an algorithmic tape music composition"

homepage := Some( url( "https://github.com/Sciss/LeereNull" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

resolvers += "Clojars Repository" at "http://clojars.org/repo"  // for jsyntaxpane

libraryDependencies ++= Seq(
   "de.sciss" %% "strugatzki" % "0.16",
   "de.sciss" %% "kontur" % "0.18-SNAPSHOT",
   "de.sciss" %% "fscapejobs" % "0.17",
   "com.itextpdf" % "itextpdf" % "5.1.1",
   "de.sciss" %% "sonogramoverview" % "0.17"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- packaging ----

// seq( assemblySettings: _* )

// test in assembly := {}

seq( appbundle.settings: _* )

appbundle.icon := Some( file( "src" ) / "main" / "resources" / "leere_null.png" )
