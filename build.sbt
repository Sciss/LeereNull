// import AssemblyKeys._

name           := "LeereNull"

version        := "0.15.0-SNAPSHOT"

organization   := "de.sciss"

scalaVersion   := "2.11.2"

description    := "Materials and tools for an algorithmic tape music composition"

homepage       := Some(url(s"https://github.com/Sciss/${name.value}"))

licenses       := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss"     %% "strugatzki"       % "2.4.1",
  "de.sciss"     %% "kontur"           % "1.3.0-SNAPSHOT",
  "de.sciss"     %% "fscapejobs"       % "1.4.1",
  "de.sciss"     %% "sonogramoverview" % "1.7.1",
  "de.sciss"     %% "pdflitz"          % "1.1.0"
)

// retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xfuture")

// ---- packaging ----

// seq( assemblySettings: _* )

// test in assembly := {}

seq(appbundle.settings: _*)

appbundle.icon := Some(file("src") / "main" / "resources" / "leere_null.png")

appbundle.javaOptions += "-Xmx2048m"
