package Defibrillators

import MIME_Type.SolutionMain._

import math._
import scala.io.StdIn._

/**
 * Created by gluk-alex on 7/11/15.
 */
object Solution extends App {
  /*helper starts*/
  val testPath: String =
    "/home/gluk-alex/Documents/codingameChallenge/src" +
      "/test/scala/" +
      "testDefibrillators/" +
      "CompleteFile1/"
  val testInputArray: Array[String] =
    getFileContent(
                    "Test_3_input.txt",
                    testPath
                  ).toArray
  //"Test_3_input.txt"
  val testOutputSeq: Seq[String] =
    getFileContent(
                    "Test_3_output.txt",
                    testPath
                  ).toSeq
  /*helper ends*/
  val lon =
  //readLine
    testInputArray(0)
  val lat =
  //readLine
    testInputArray(1)
  val n: Int =
  //readInt
    testInputArray(2).toInt

  /*h s*/
  def replaceCommaWithDotToDouble(degreesStr: String): Double = {
    /*return value*/
    degreesStr.replace(',', '.').toDouble
  }

  def calculateDistance(
                         fromLongitude: Double =
                         replaceCommaWithDotToDouble(lon),
                         fromLatitude: Double =
                         replaceCommaWithDotToDouble(lat),
                         toLongitude: Double,
                         toLatitude: Double
                         ): Double = {
    /*val userLongitude: Double =
      lon.replace(',','.').toDouble
    val userLatitude: Double =
      lat.replace(',','.').toDouble*/
    val x: Double =
      (toLongitude - fromLongitude) * cos((toLatitude + fromLatitude) / 2)
    val y: Double =
      (toLatitude - fromLatitude)
    /*return value*/
    sqrt(x * x + y * y) * 6371
  }

  def extractParams(defib: String): (String, Double, Double) = {
    val defibFields: Array[String] =
      defib.split(";")

    /*return*/
    (defibFields(1),
      defibFields(4).replace(',', '.').toDouble,
      defibFields(5).replace(',', '.').toDouble)
  }

  /*val distArr: Array[Double] =
      new Array[Double](n)*/
  case class DefibParam(
                         name: String,
                         longitude: Double,
                         latitude: Double,
                         distance: Double
                         )

  /*h e*/

  /*warper start*/
  //def closestDefibrilator: String = {
  //def closestDefibrilator: (String, Double, Double, Double) = {
  def closestDefibrilator: DefibParam = {
    //var leader: (String, Double) = "" -> 0
    var leader: DefibParam =
      DefibParam("", 0, 0, 0)
    /*h e*/

    for (i <- 0 until n) {
      val defib =
      //readLine
        testInputArray(i + 3)
      /*h s*/
      val (defibrilator, longitude, latitude): (String, Double, Double) =
        extractParams(defib)

      /*distArr(i) =*/
      val dist: Double =
        calculateDistance(
                           toLongitude = longitude,
                           toLatitude = latitude)

      if (leader.name.isEmpty) {
        leader =
          //defibrilator -> dist
          DefibParam(defibrilator, longitude, latitude, dist)
      } else {
        if (leader.distance > dist) {
          leader =
            //defibrilator -> dist
            DefibParam(defibrilator, longitude, latitude, dist)
        }
      }
      /*h e*/
    }

    /*return value*/
    /*name, longit, latit, dist*/
    //(leader._1,leader._2)
    leader
  }

  /*warper end*/

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  //println("answer")
  //println(leader._1)
  println(closestDefibrilator.name)
}
