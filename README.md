## Leere Null

### statement

Leere Null is (C)opyright by 2011-2012 Hanns Holger. All rights reserved. It is released under the [GNU General Public License](http://github.com/Sciss/Kontur/blob/master/licenses/Kontur-License.txt). Leere Null is a set up tools and scripts and musical data for an algorithmic electroacoustic music piece of the same title.

### requirements / installation

Builds with sbt 0.11 and compiles against Scala 2.9.1 and Java 1.6. Depends on Kontur, Strugatzki, and FScapeJobs.

### creating an IntelliJ IDEA project

If you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "LeereNull"
    > gen-idea

### download

The current version can be downloaded from [github.com/Sciss/LeereNull](http://github.com/Sciss/LeereNull).

