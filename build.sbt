import AssemblyKeys._

name           := "leerenull"

appbundleName  := "LeereNull"

version        := "0.13-SNAPSHOT"

organization   := "de.sciss"

scalaVersion   := "2.9.1"

libraryDependencies ++= Seq(
   "de.sciss" %% "strugatzki" % "0.15",
   "de.sciss" %% "kontur" % "0.17-SNAPSHOT",
   "de.sciss" %% "fscapejobs" % "0.17",
   "com.itextpdf" % "itextpdf" % "5.1.1"
)

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- packaging ----

seq( assemblySettings: _* )

test in assembly := {}

seq( appbundleSettings: _* )
