import sbt._

class LeereNullProject( info: ProjectInfo ) extends DefaultProject( info ) {
   val strugatzki    = "de.sciss" %% "strugatzki" % "0.12"
   val kontur        = "de.sciss" %% "kontur" % "0.15"

   val repo1         = "Clojars Repository" at "http://clojars.org/repo" // this is needed for JSyntaxPane
}