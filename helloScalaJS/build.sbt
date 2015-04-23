enablePlugins(ScalaJSPlugin)

name := "Scala.js Tutorial"

scalaVersion := "2.11.6"

//fastOptJS
/*
Don't forget to reload the sbt configuration now:
	1. Hit enter to abort the '~fastOptJS' command
	2. Type 'reload'
	3. Start '~fastOptJS' again
*/
//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"
/*replaced with*/
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"
/*
The Scala.js sbt plugin provides 
a mechanism for libraries to 
declare the plain JavaScript libraries they depend on and 
bundle them in a single file. 
All you have to do is activate this and 
then include the file.

In your build.sbt, set:*/
skip in packageJSDependencies := false
/*After 'reload'ing and 
rerunning 'fastOptJS', this will create 
'scala-js-tutorial-jsdeps.js' containing 
all JavaScript libraries next to the main JavaScript file. 
We can then 
simply include this file and 
don't need to 
worry about JavaScript libraries anymore:*/

/*
jQuery (yes, it is included automatically) tries to 
access the 'window' object of the DOM, 
which 
doesn't exist by default in 
the 'Rhino' and 'Node.js' runners. 
To make the DOM available, 
add the following to your build.sbt:*/
jsDependencies += RuntimeDOM
/*If you use 
the 'fastOpt' stage, 
this will 
switch to 
running your code with 'PhantomJS' instead of 'Node.js', 
which you need to install. 
Otherwise, 
in Rhino, 
a fake DOM is automatically made available.*/