package ASCIIArt

import java.util.Dictionary

import scala.io.Source

/*
task:
In stations and
airports
you often see this type of screen.
Have you ever asked yourself
how it might be possible to
simulate this display on
a good old terminal?
We have (answer): with ASCII art!

ASCII art allows you to
represent forms by
using characters.
To be precise,
in our case,
these `forms` are `words`.
For example,
the word "MANHATTAN" could be
displayed as follows
in ASCII art:
# #  #  ### # #  #  ### ###  #  ###
### # # # # # # # #  #   #  # # # #
### ### # # ### ###  #   #  ### # #
# # # # # # # # # #  #   #  # # # #
# # # # # # # # # #  #   #  # # # #

TODO
​Your mission is to
write a program that can
display a line of text in ASCII art.

INPUT:
Line 1:
  the `width` `L` of
  a `letter` represented in ASCII art.
  All `letters` are the same `width` (monospace).
Line 2:
  the `height` 'H' of
  a `letter` represented in ASCII art.
  All `letters` are the same `height`.
Line 3:
  The line of `text` 'T',
  composed of 'N' ASCII `characters`.
Following Lines:
  the string of `characters` ABCDEFGHIJKLMNOPQRSTUVWXYZ? Represented in ASCII art.

OUTPUT:
The `text` 'T' in ASCII art.
The `characters` 'a' to 'z' are
shown in ASCII art by
their equivalent in upper case.
The `characters` which are
not in the intervals [a-z] or [A-Z] will be
shown as a `question mark` in ASCII art.

CONSTRAINTS :
0 < L < 30
0 < H < 30
0 < N < 200
 */

/**
 * Created by Alex on 26.06.15.
 */
object Solution extends App {
  /*helpers starts*/
  val filename =
  //"Test_1_input.txt"
    "Test_2_input.txt"
  val filePath =
  //E:\Java\Scala\sbt\projects\codInGame\src\main\scala\ASCIIArt\Solution.scala
  //E:\Java\Scala\sbt\projects\codInGame\src\test\scala\testArtASCII\.Test only one letter'E'\Test_1_input.txt
  //"E:\\Java\\Scala\\sbt\\projects\\codInGame\\src\\test\\scala\\testArtASCII\\.Test only one letter'E'\\"
  //E:\Java\Scala\sbt\projects\codInGame\src\test\scala\testArtASCII\.Test'MANHATTAN'\Test_2_input.txt
    "E:\\Java\\Scala\\sbt\\projects\\codInGame\\src\\test\\scala\\testArtASCII\\.Test'MANHATTAN'\\"
  val currFile = Source
    .fromFile(filePath + filename)
  /*val currFileLines = Source
    .fromFile( filePath + filename )
    .getLines()
  val currFileBuffered = Source
    .fromFile( filePath + filename )
    .buffered*/

  def getFileContent(
                      filePath: String,
                      filename: String
                      ): /*Seq*/ Iterator[String] =
  {
    //(
    for (
      line <- Source
        .fromFile(filePath + filename)
        .getLines()
    ) yield line.toString
    /*)
      .toSeq*/
  }

  val fileContent: /*Seq*/ Iterator[String] =
    getFileContent(filePath, filename)

  /*try {
    val fileContent: Seq[String] =
      (for (
        line <- Source
          .fromFile(filePath + filename)
          .getLines()
      ) yield line.toString)
        .toSeq*/
  /*{
    println( "l>" + line )
  }*/
  //} catch {
  //*case ex: Exception => println("Bummer, an exception happened.")//*works
  //case ex: Exception => println(s"Bummer, an $ex happened.") //*works even better
  //}
  //*works

  val alphabetChars: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"

  /*val alphabetMap: Map[Int,Char] =
    Map(
         (1->'a'),
         (2->'b'),
         (3->'c'),
         (4->'d'),
         (5->'e'),
         (6->'f'),
         (7->'g'),
         (8->'h')
       )*/

  def findLetter(
                  letter: String,
                  alphabet: String
                  ): Int =
  {
    alphabet.indexOf(letter)
  }

  def showText(
                text: String,
                alphabet: String,
                letterHeight: Int,
                letterWidth: Int,
                letterMatrix: Array[String],
                textMatrix: Array[String]
                ): Unit =
  {
    for (letter <- text) {
      /*side effect*/
      addLetter(
                 letterNumber = alphabet.indexOf(letter.toUpper),
                 letterHeight = letterHeight,
                 letterWidth = letterWidth,
                 letterMatrix = letterMatrix,
                 textMatrix = textMatrix
               )
    }
    for (row <- textMatrix) {
      /*side effect*/
      println(
               row
             )
    }
  }

  def showLetter(
                  letterNumber: Int,
                  letterHeight: Int,
                  letterWidth: Int,
                  letterMatrix: Array[String]
                  ): Unit =
  {
    if (letterMatrix.nonEmpty) {
      for (row <- letterMatrix) {
        val letter: String =
          if (letterNumber == -1) {
            row.takeRight(letterWidth)
          } else {
            row.drop(letterNumber * letterWidth).take(letterWidth)
          }

        println(s"${ letter }")
        //println(s"${ row.drop(letterNumber * letterWidth).take(letterWidth) }")
        /*println(s"${
          row
            .slice(
              letterNumber * letterWidth,
              letterWidth) }")*/
      }
    } else {
      println("has no letters to print from")
    }
  }

  def addLetter(
                 letterNumber: Int,
                 letterHeight: Int,
                 letterWidth: Int,
                 letterMatrix: Array[String],
                 textMatrix: Array[String]
                 ): Unit =
  {
    if (letterMatrix.nonEmpty) {
      for (row <- letterMatrix.indices /*0 until letterMatrix.length*/ ) {
        val newLetter: String =
          if (letterNumber == -1) {
            letterMatrix(row).takeRight(letterWidth)
          } else {
            letterMatrix(row).drop(letterNumber * letterWidth).take(letterWidth)
          }
        /*letterMatrix(row)
          .slice        (
                          letterNumber * letterWidth,
                          letterWidth)*/

        //if (textMatrix(row).isEmpty) {
        if (textMatrix(row) == null) {
          textMatrix(row) = newLetter
        } else {
          textMatrix(row) += newLetter
        }
      }
    } else {
      println("has no letters to add to")
    }
  }

  /*helpers ends*/

  val l =
  //scala.io.StdIn.readInt
    if (fileContent.hasNext) {
      fileContent.next().toInt
    } else {
      -1
    }
  val h: Int =
  //scala.io.StdIn.readInt
    if (fileContent.hasNext) {
      fileContent.next().toInt
    } else {
      -1
    }
  val t: String =
  //scala.io.StdIn.readLine
    if (fileContent.hasNext) {
      fileContent.next()
    } else {
      ""
    }
  /*helper start*/
  val rowsArray: Array[String] = new Array(h)
  val textArray: Array[String] =
  //new Array(h)
    new Array(h).padTo(h, "")
  /*helper end*/
  for (i <- 0 until h) {
    val row: String =
    //scala.io.StdIn.readLine
      if (fileContent.hasNext) {
        fileContent.next()
      } else {
        ""
      }

    rowsArray(i) = row
    println(s"row:$row,row.size:${ row.size },letters in row:${ row.size / l }")
  }

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  /*unit test*/
  println("answer")
  println(s"l:$l")
  println(s"h:$h")
  println(s"t:$t")
  println(s"textMatrix:${ textArray.headOption }")
  println(s"textMatrix.isEmpty:${ textArray.isEmpty }")
  println(s"textMatrix.isDefinedAt(0):${ textArray.isDefinedAt(0) }")
  println(s"alphabetChars:${ alphabetChars }")
  println(s"alphabetChars.slice(3,4):${ alphabetChars.slice(3, 4) }")
  println(s"alphabetChars.slice(4,3):${ alphabetChars.slice(4, 3) }")
  println(s"alphabetChars.indexOf('!'):${ alphabetChars.indexOf("!") }")
  //println(s"fileContent:$fileContent")
  /*println(s"fileContent line2:${fileContent.take(1)}")
  println(s"fileContent.take(2):${fileContent.take(2)}")
  println(s"fileContent.take(2).head:${fileContent.take(2).head}")
  println(s"fileContent.drop(2):${fileContent.drop(2)}")
  println(s"fileContent.drop(2).head:${fileContent.drop(2).head}")
  println(s"fileContent.hasDefiniteSize:${fileContent.hasDefiniteSize}")
  println(s"fileContent.isEmpty:${fileContent.isEmpty}")
  println(s"fileContent.isTraversableAgain:${fileContent.isTraversableAgain}")*/
  showLetter(
              letterNumber =
                alphabetChars.indexOf(t.headOption.getOrElse("!")),
              letterHeight = h,
              letterWidth = l,
              letterMatrix = rowsArray
            )
  showLetter(
              letterNumber =
                alphabetChars.indexOf("!"),
              letterHeight = h,
              letterWidth = l,
              letterMatrix = rowsArray
            )
  showText(
            text = t,
            alphabet = alphabetChars,
            letterHeight = h,
            letterWidth = l,
            letterMatrix = rowsArray,
            textMatrix = textArray
          )

}
