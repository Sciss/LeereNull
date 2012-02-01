seq( appbundle.settings: _* )

name := "LeereNullVideo"

version := "0.10-SNAPSHOT"

organization := "de.sciss"

scalaVersion := "2.9.1"

description := "SPDE Sketch for the introductory video sequence of Leere Null (2)"

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- appbundle ----

appbundle.javaArchs += appbundle.JavaArch_i386    // crappy quicktime runs only in 32 bit

// appbundle.resources += file( "data" )

// appbundle.workingDirectory := Some( file( appbundle.BundleVar_AppPackage ))

appbundle.icon := Some( file( "icon.png" ))

