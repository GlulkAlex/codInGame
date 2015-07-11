package HorseRacingDuals

import scala.io.StdIn._

/*
Statement
Casablanca’s hippodrome is
organizing a new type of horse racing: duals.
During a dual,
only two horses will participate in the race.

In order for the race to be interesting,
it’s necessary to
try to select two horses with similar strength.
TODO
Write a program which,
using a given number of `strengths`,
identifies
the two closest `strengths` and
shows their `difference` with a positive integer.

INPUT:
Line 1:
  Number `N` of horses
The `N` following lines:
  the strength `Pi` of each horse.
  `Pi` is an integer.

OUTPUT:
  The `difference` 'D'
  between the two closest `strengths`.
  'D' is a positive integer.

CONSTRAINTS :
1 < N  < 100000
0 < Pi ≤ 10000000

EXAMPLE :
Input
3
5
8
9
Output
1

 */

/**
 * Created by gluk-alex on 7/11/15.
 */
object Solution extends App {
  val n =
    readInt
  /*h s*/
  val horsesArray: Array[Int] =
    new Array[Int](n)
  /*h e*/
  for (i <- 0 until n) {
    val pi =
      readInt

    /*h s*/
    horsesArray(i) = pi
    /*h e*/
  }

  /*h s*/
  def closestPair(source: Array[Int]): (Int, Int) = {
    def nextPair(iter: Iterator[Array[Int]]): /*List*/Array[Int] =
      if (iter.hasNext) {
        iter.next
      } else {
        //List()
        Array.emptyIntArray
      }

    /*for correct result must be at least two elements*/
    def loop(
              //pairsRemains: Array[Int] = Array.emptyIntArray,
              pairsRemains: Iterator[Array[Int]] = Iterator.empty,
              first: Int = -1,
              second: Int = -1,
              /*for non empty sorted sequence of positive integers must be
              positive too*/
              minDistance: Int = -1
              ): (Int, Int) = {
      if (pairsRemains.isEmpty) {
        /*return value*/
        (first, second)
      } else {
        /*must be of size two*/
        val currentPair: /*List*/Array[Int] =
        /*reduction to converge eventually*/
          nextPair(pairsRemains)

        /*recursion*/
        if (
          minDistance == -1 ||
            minDistance > currentPair(1) - currentPair(0)
        ) {
          /*new leader*/
        /*} else {
          if (minDistance > currentPair(0) - currentPair(1)) {*/
            /*new leader*/
            loop(
                  pairsRemains = pairsRemains,
                  first = currentPair(0),
                  second = currentPair(1),
                  minDistance = currentPair(1) - currentPair(0)
                )
          } else {
            /*same*/
            loop(
                  pairsRemains = pairsRemains,
                  first = first,
                  second = second,
                  minDistance = minDistance
                )
          //}
        }
      }

    }

    if (source.isEmpty) {
      (-1, -1)
    } else {
      val sortedSource: Array[Int] =
        source.sorted
      val pairsFromSourceIter: Iterator[Array[Int]] =
        sortedSource
        .sliding(2)

      loop(
            pairsRemains =
              //sortedSource,
              pairsFromSourceIter,
            first = -1,
            second = -1,
            minDistance = -1
          )
    }
  }
  /*h e*/

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  //println("answer")
  val (fst, snd) =
    closestPair(horsesArray)

  println(snd - fst)
}
