package temperatures

/*
TODO
Task
In this exercise, you have to
analyze records of temperature to
find the closest to zero.

Write a program that
prints the temperature
closest to 0 among input data.

INPUT:
Line 1: N, the number of temperatures to analyse
Line 2: The N temperatures expressed as
integers ranging from -273 to 5526

OUTPUT:
Display 0 (zero) if
no temperature is provided
Otherwise,
display the temperature closest to 0,
knowing that
if two numbers are equally close to zero,
positive integer has to be considered closest to zero
(for instance,
if the temperatures are -5 to 5, then
display 5)

CONSTRAINTS:
0 ? N < 10000

EXAMPLE:
Input
5
1 -2 -8 4 5
Output
1
 */

import math._
import scala.util._

/**
 * Created by Alex on 20.06.15.
 */
/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {
  // the number of temperatures to analyse
  val n =
    5
  //scala.io.StdIn.readInt
  // the N temperatures expressed as integers ranging from -273 to 5526
  val temps =
    "1 -2 -8 4 5"
  //scala.io.StdIn.readLine

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println("result")
  /*helper starts*/
  val temperatures: Seq[Int] =
    if (temps.isEmpty) {
      Seq.empty
    } else {
      temps.split(' ').map(_.toInt)
    }

  def closestTemperature(temperatures: Seq[Int]): Int = {
    if (temperatures.isEmpty) {
      0
    } else {
      temperatures.minBy(_.abs)
    }
  }

  /*
  Idea:
  >split sequence to positive & negative part then
  >compare minimum + with maximum -
  */
  /*
  cases:
    if empty input=>output: 0
    if input contain 0=>output: 0
    if input contain only positive=>output: minimum
    if input contain only negative=>output: maximum
    if input contain both positive&negative
      cases:
        if abs(maximum) >= minimum =>output: minimum
        if abs(maximum) <= minimum =>output: maximum
   */
  case class TemperatureLeaders(
                                 plusMin: Int,
                                 minusMax: Int, //sound strange
                                 zeroest: Int
                                 )

  def chooseClosestToZero(
                           currentHeadElem: Int,
                           positiveMin: Int,
                           negativeMax: Int
                           ): /*Int*/ TemperatureLeaders = {
    val newPositiveMin =
    /*Assume that `currentHeadElem` != '0'*/
      if ((currentHeadElem > 0) && (currentHeadElem < positiveMin)) {
        currentHeadElem
      } else {
        positiveMin
      }
    val newNegativeMax =
    /*(-)vs(-)*/
      if (
        (currentHeadElem < 0) &&
          (currentHeadElem > negativeMax)
      ) {
        //(-3>-7)
        currentHeadElem
      } else {
        //(-7<=-3)
        negativeMax
      }
    /*? how to compare ?*/
    val newClosestToZero =
    /*!watch out for val type overflow limits!*/
      if (
      /*not default / initialized */
        (newPositiveMin < Int.MaxValue) &&
          abs(newNegativeMax) >= newPositiveMin) {
        newPositiveMin
      } else {
        /*if abs(maximum) <= minimum =>output: maximum*/
        newNegativeMax
      }
    /*return value*/
    TemperatureLeaders(newPositiveMin, newNegativeMax, newClosestToZero)
  }

  def loop(
            inputTempers: Seq[Int],
            //remainingTempers: Seq[Int],
            minPositive: Int = Int.MaxValue,
            /*!watch out for val type overflow limits!*/
            maxNegative: Int = Int.MinValue + 1,
            closestToZero: Int = 0 //for no input at all//or must be set in initialization
            ): Int = {
    if (inputTempers.isEmpty) {
      /*basic case*/
      closestToZero
    } else {
      if (inputTempers.head == 0) {
        /*special basic case*/
        0 //closestToZero
      } else {
        /*recursive step*/
        val TemperatureLeaders(newPositiveMin, newNegativeMax, newClosestToZero) =
          chooseClosestToZero(
                               inputTempers.head: Int,
                               minPositive: Int,
                               maxNegative: Int
                             )

        loop(
              inputTempers.tail,
              newPositiveMin,
              newNegativeMax,
              newClosestToZero
            )
      }
    }
  }

  case class TemperatureComponents(
                                    chill: Option[Int] /*Seq[Int]*/ ,
                                    zero: Option[Int] /*Seq[Int]*/ ,
                                    hot: Option[Int] /*Seq[Int]*/
                                    )

  def getDigitsValue(
                      strWithNegativeHead: String,
                      accumNegative: String = ""
                      ): (String, String) = {
    if (strWithNegativeHead.isEmpty) {
      /*base case*/
      (strWithNegativeHead, accumNegative)
    } else if (strWithNegativeHead.head == ' '/*" "*/) {
      /*special base case*/
      (strWithNegativeHead.tail, accumNegative)
    } else {
      getDigitsValue(
                      strWithNegativeHead.tail: String,
                      accumNegative + strWithNegativeHead.head: String
                    )
    }
  }

  def tempsDecomposition(
                          temps: String,
                          accum: TemperatureComponents = TemperatureComponents(None, None, None)
                          ): TemperatureComponents = {
    if (temps.isEmpty) {
      /*base case*/
      /*return value*/
      accum
    } else {
      /*reccursion*/
      if (temps.head == '-') {
        /*negative value*/
        /*then read until
        'temps.isEmpty' or ' '
         */
        /*
        getNegativeValue
        return
        remaining 'temps'
        and
        digits for negative
        */
        val (nextTemps, negativeDigits) =
          getDigitsValue(
                          temps.tail,
                          accumNegative = "-"
                        )

        tempsDecomposition(
                            temps = nextTemps,
                            TemperatureComponents(
                                                   chill =
                                                     if (
                                                       accum.chill != None &&
                                                         accum.chill.get > negativeDigits.toInt) {
                                                       accum.chill
                                                     } else {
                                                       Some(negativeDigits.toInt)
                                                     },
                                                   zero = accum.zero,
                                                   hot = accum.hot
                                                 )
                          )
      } else {
        if (temps.head == ' ') {
          /*skip, do nothing*/
          tempsDecomposition(
                              /*remains*/
                              temps = temps.tail,
                              /*same*/
                              accum
                            )
        } else {
          /*return value*/
          val (nextTemps, positiveDigits) =
            getDigitsValue(
                            temps.tail,
                            accumNegative = temps.head.toString
                          )

          tempsDecomposition(
                              temps = nextTemps,
                              TemperatureComponents(
                                                     chill = accum.chill,
                                                     zero =
                                                       if (positiveDigits/*.toInt*/ == "0"/*'0'*/) {
                                                         Some(0)
                                                       } else {
                                                         accum.zero
                                                       },
                                                     hot =
                                                       if (
                                                         //accum.hot != None &&
                                                         accum.hot.isDefined &&
                                                           accum.hot.get < positiveDigits.toInt) {
                                                         accum.hot
                                                       } else {
                                                         Some(positiveDigits.toInt)
                                                       }
                                                   )
                            )
        }
      }
    }
  }

  def chooseOneFromThree(tempOptions:TemperatureComponents): Int =tempOptions match {
    case TemperatureComponents(None, None, None) => 0
    case TemperatureComponents(_, Some(z), _) => 0
    /*better / more reliable work with 'None' instead of '_'
    case TemperatureComponents(Some(c), _, _) => c
    case TemperatureComponents(_, _, Some(h)) => h*/
    case TemperatureComponents(Some(c), None, None) => c
    case TemperatureComponents(None, None, Some(h)) => h
    case TemperatureComponents(Some(c), _, Some(h)) => {
      if (c==h) {
        h
      } else {
        if (c*(-1)<h) {
          c
        } else {
          h
        }
      }
    }
    case _ => 0
  }

  /*helper ends*/
  println(s"-7>(-5) ?:${(-7) > (-5)}")
  println(s"-7<(-5) ?:${(-7) < (-5)}")
  println(s"Input:${temperatures}")
  /*println(s"getDigitsValue:${getDigitsValue("234 567")}")
  println(s"getDigitsValue:${getDigitsValue("901 567","-")}")
  println(s"temperatures:${tempsDecomposition("0")}")
  println(s"temperatures:${tempsDecomposition("1")}")
  println(s"temperatures:${tempsDecomposition("-1")}")
  println(s"temperatures:${tempsDecomposition(temps)}")*/
  println(s"Temperature Result:${chooseOneFromThree(tempsDecomposition(temps))}")
  /*initialization*/
  println(
  s"Result:${
    loop(
            temperatures,
              Int.MaxValue,
              /*!watch out for val type overflow limits!*/
              Int.MinValue + 1,
    0
        )
  }"
         )
  println(s"Temperature test1 Result:${chooseOneFromThree(tempsDecomposition("7 5 9 1 4"))}")
  println(s"Temperature test2 Result:${chooseOneFromThree(tempsDecomposition("-273"))}")
  println(s"Temperature test3 Result:${chooseOneFromThree(tempsDecomposition("5526"))}")
  println(s"Temperature test4 Result:${chooseOneFromThree(tempsDecomposition("-15 -7 -9 -14 -12"))}")
  println(s"Temperature test7 Result:${chooseOneFromThree(tempsDecomposition("-10 -10"))}")
}
