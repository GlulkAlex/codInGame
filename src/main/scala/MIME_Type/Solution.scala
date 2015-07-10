package MIME_Type

import MIME_Type.Solution._

/*
Statement
MIME types are
used in numerous internet protocols to
associate a media type
(html, image, video ...) with the content sent.
The MIME type is
generally inferred from the extension of the file to be sent.

You have to
write a program that
makes it possible to
detect the MIME type of a file based on its name.
*/
/*
You are provided with
a table which associates MIME types to file extensions.
You are also given
a list of names of files to
be transferred and
for each one of these files,
you must
find the MIME type to be used.

The `extension` of a `file` is
defined as
the substring which follows the last occurrence, if any, of
the `dot` character within the file name.
If the extension for a given file can
be found in the association table
(case insensitive, e.g. TXT is treated the same way as txt),
then
print the corresponding MIME type.
If it is not possible to
find the MIME type corresponding to a file, or
if the file doesn’t have an extension,
print 'UNKNOWN'.

INPUT:
Line 1:
  Number `N` of elements which
  make up the association table.
Line 2:
  Number `Q` of file names to be analyzed.
N following lines :
  One file extension per line and
  the corresponding MIME type
  (separated by a blank space).
Q following lines :
  One file name per line.

OUTPUT:
For each of the 'Q' filenames,
display on a line the corresponding `MIME type`.
If there is no corresponding type,
then
display `UNKNOWN`.

CONSTRAINTS:
0 < N < 10000
0 < Q < 10000
File extensions are
composed of
a maximum of '10' `alphanumerical` ASCII characters.
MIME types are composed of
a maximum '50' `alphanumerical` and
`punctuation` ASCII characters.
File names are
composed of
a maximum of '256' `alphanumerical` ASCII characters and
`dots` (full stops).
There are
no `spaces` in
the `file` `names`,
`extensions` or
`MIME types`.

EXAMPLE:
  Input:
    2
    4
    html text/html
    png image/png
    test.html
    noextension
    portrait.png
    doc.TXT
  Output:
    text/html
    UNKNOWN
    image/png
    UNKNOWN
 */

/**
 * Created by gluk-alex on 7/9/15.
 */
object Solution {

  import SolutionMain._

  /*lazy*/ val inputFileContent: Array[String] =
    /*getFileContent()
    .toArray*/
  """3
    |10
    |wav audio/x-wav
    |mp3 audio/mpeg
    |pdf application/pdf
    |a
    |a.wav
    |b.wav.tmp
    |test.vmp3
    |pdf
    |mp3
    |report..pdf
    |defaultwav
    |.mp3.
    |final.
    |"""
    .stripMargin
  .split("\n")

  // Number of elements which make up the association table.
  val n: Int =
  /*scala.io.StdIn
  .readInt*/
    /*SolutionMain.inputFileContent(0)
    .toInt*/
  3

  // Number Q of file names to be analyzed.
  val q: Int =
  /*scala.io.StdIn
  .readInt*/
    /*SolutionMain.inputFileContent(1)
    .toInt*/
  10

  /*helper starts*/
  case class ExtensionMIME(ext: String, mimeType: String)

  val extTypeMap: Array[ExtensionMIME] =
    new Array[ExtensionMIME](n)

  /*must find first '.' dot from the right & return string before it*/
  def findExtension(fileName: String = ""): String = {
    /*val dotPosition: Int =
      fileName
      .indexOf(".")*/
    /*val lastDotPosition: Int =
     fileName
     .lastIndexOf(".")*/
    val fileNameComponents: Array[String] =
      fileName
      .split('.')
    val fileNameComponentsLength =
      fileNameComponents.length

    if (
      fileName.isEmpty ||
      /*dotPosition == -1 ||*/
      fileName.endsWith(".") ||
        fileNameComponents.isEmpty ||
        fileNameComponentsLength <= 1
    ) {
      "UNKNOWN"
    } else {
      /*fileName
      .drop(dotPosition)*/
      fileNameComponents(fileNameComponentsLength - 1)
      /*fileNameComponents.last*/
    }
  }

  /*helper ends*/
  for (i <- 0 until n) {
    // ext: file extension
    // mt: MIME type.
    val Array(ext, mt) =
    /*scala.io.StdIn
    .readLine split " "*/
      inputFileContent(i + 2)
      .split(" ")

    /*helper starts*/
    extTypeMap(i) =
      ExtensionMIME(
                     ext = ext.toLowerCase,
                     mimeType = mt)
    /*helper ends*/
  }

  for (i <- 0 until q) {
    val fname =
    // One file name per line.
    /*scala.io.StdIn
    .readLine*/
      inputFileContent(i + 2 + n)

    /*helper starts*/
    val fExt: String =
      findExtension(fname)
      .toLowerCase

    if (
      fExt != "unknown" ||
        extTypeMap.contains(fExt)    ) {
      extTypeMap
      .find(_.ext.toLowerCase == fExt) match {
        case None                             =>
          println("UNKNOWN")
        case Some(ExtensionMIME(_, mimeType)) =>
          println(mimeType)
      }
    } else {
      println("UNKNOWN")
    }
    /*helper ends*/
  }

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  //println("UNKNOWN")
  // For each of the Q filenames,
  // display on a line the corresponding MIME type.
  // If there is no corresponding type, then display UNKNOWN.

}

/*unit test*/
object SolutionMain extends App {
  import MIME_Type.Solution._

  trait Generator[+T] {
    self =>
    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random

    def generate = rand.nextInt()
  }

  def interval(lo: Int, hi: Int): Generator[Int] =
    for {x <- integers} yield lo + x % (hi - lo)

  def getFileContent(
                      filename: String =
                      "Test_1_input.txt",
                      filePath: String =
                      "/home/gluk-alex/Documents/" +
                        "codingameChallenge/src/test/scala/" +
                        "testMIME_type/SimpleExample/"
                        //"testMIME_typeSuit.scala"
                      ): /*Stream*/ Iterator[String] = {
    /*return value*/
    try {
      scala.io.Source
      .fromFile(filePath + filename)
      .getLines()
      //.toStream
    }
    catch {
      case e: Throwable => Iterator(e.getMessage + " " + e.getCause)
    }
    //finally {}
  }

  /*lazy val inputFileContent: Array[String] =
    getFileContent()
    .toArray*/

  def returnMIME_types(
                        filePath: String,
                        filename: String): Vector[String] = {
    /*has its own scope, so name conflict expected*/
    val localInputFileContent: Array[String] =
      getFileContent(filePath, filename)
      .toArray
    // Number of elements which make up the association table.
    val n: Int =
      //self
        //this
      localInputFileContent(0)
      .toInt

    // Number Q of file names to be analyzed.
    val q: Int =
      //this
      localInputFileContent(1)
      .toInt

    val extTypeMap: Array[ExtensionMIME] =
      (for (i <- 0 until n) yield {
        // ext: file extension
        // mt: MIME type.
        val Array(ext, mt) =
          localInputFileContent(i + 2)
          .split(" ")
        /*return value*/
        ExtensionMIME(
                       ext = ext.toLowerCase,
                       mimeType = mt)
      }).toArray

    /*return value*/
    (for (i <- 0 until q) yield {
      val fname =
        localInputFileContent(i + 2 + n)
      val findMatch =
        extTypeMap
        .find(_.ext.toLowerCase == Solution.findExtension(fname).toLowerCase)
      /*return value*/
      findMatch match {
        case None                                      =>
              "UNKNOWN"
        case Some(Solution.ExtensionMIME(_, mimeType)) =>
              mimeType
      }
    }).toVector
  }

  /*
   */
  println(s"findExtension('a.a'):${findExtension("a.a")}")
  println(s"findExtension('a.'):${findExtension("a.")}")
  println(s"findExtension('a'):${findExtension("a")}")
  println(s"findExtension('.'):${findExtension(".")}")
  println(s"findExtension(''):${findExtension("")}")
  println(s"findExtension('.mp3.'):${findExtension(".mp3.")}")

  val endMarker = true
}
