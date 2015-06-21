package temperaturesTest

//import org.scalacheck._
//import org.scalatest._

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.FunSuite

//import org.scalatest.Assertions._

/**
 * Created by Alex on 20.06.15.
 */
class TemperaturesSuit extends FunSuite {

  import temperatures.Solution

  ignore/*test*/(
        "An empty Set should have size 0"
      ) {
          assert(Set.empty.size == 0)
        }
  ignore/*test*/(
        "A '1'\" should \"be equal '1'"
      ) {
          assert(1 == 1)
        }
  ignore/*test*/(
        "An empty input\" should \"return '0'"
      ) {
          assert(Solution.loop(Seq.empty[Int]) == 0)
        }
  test(
        "closest to zero from '-7, -5, 5'\" should \"be '5'"
      ) {
          info("info is recorded")
          markup("markup is *also* recorded")
          note("notes are sent immediately")
          alert("alerts are also sent immediately")
          //def assertResult(expected: Any, clue: Any)(actual: Any): Unit
          assertResult(
                        //TemperatureLeaders(newPositiveMin, newNegativeMax, newClosestToZero)
                        expected = Solution.TemperatureLeaders(5, -5, 5),
                        clue = "must be (5, -5, 5)"
                      )(
              //chooseClosestToZero( inputTempers.head: Int, minPositive: Int, maxNegative: Int )
              actual = Solution.chooseClosestToZero(-7, 5, -5)
                       )
          /*assert(
                  Solution.chooseClosestToZero(-7, 5, -5) == 0
                )*/
        }
  /*
  TODO
  >It works with -273 alone (250 pts)
  Done>It works with 5526 alone (250 pts)
  >It works when inputs contains only negative numbers: : {-15 -7 -9 -14 -12} -> -7 (250 pts)
  >It works with two negative temperatures that are equal: {-10 -10} -> -10 (250 pts)
  >When two temperatures are as close to 0, then
  the positive wins: {15 -7 9 14 7 12} -> 7 (750 pts)
  >Result is correct with a simple data set: {7 5 9 1 4} -> 1 (500 pts)
   */
  test(
        "closest to zero from (5526) alone should be (5526)"
      ) {
          //def assertResult(expected: Any, clue: Any)(actual: Any): Unit
          assertResult(
                        //TemperatureLeaders(newPositiveMin, newNegativeMax, newClosestToZero)
                        expected = Solution.TemperatureLeaders(5526, Int.MinValue - 1, 5526),
                        clue = "must be (5, -5, 5)"
                      )(
              //chooseClosestToZero( inputTempers.head: Int, minPositive: Int, maxNegative: Int )
              actual = Solution.chooseClosestToZero(5526, Int.MaxValue, Int.MinValue - 1)
                       )
        }
  test(
        "closest to zero from (-273) alone should be (-273)"
      ) {
          //def assertResult(expected: Any, clue: Any)(actual: Any): Unit
          assertResult(
                        //TemperatureLeaders(newPositiveMin, newNegativeMax, newClosestToZero)
                        expected = Solution.TemperatureLeaders(Int.MaxValue, -273, -273),
                        clue = "must be (Int.MaxValue, -273, 273)"
                      )(
              //chooseClosestToZero( inputTempers.head: Int, minPositive: Int, maxNegative: Int )
              actual = Solution.chooseClosestToZero(-273, Int.MaxValue, Int.MinValue)
                       )
        }
  test(
        "It works when inputs contains only negative numbers: {-15 -7 -9 -14 -12} -> -7"
      ) {
          assertResult(
                        expected = -7,
                        clue = "must be -7"
                      )(
              actual =
                Solution
                  .loop(
                    "-15 -7 -9 -14 -12"
                      .split(' ')
                      .map(_.toInt)
                       )
                       )
        }
  test(
        "Result should be correct with a `simple data`"
      ) {
          assertResult(
                        expected = 1,
                        clue = "must be 1"
                      )(
              actual =
                Solution
                  .loop(
                    "7 5 9 1 4"
                      .split(' ')
                      .map(_.toInt)
                       )
                       )
        }
  ignore/*test*/(
        "minimum of an empty Seq should have being equal ?"
      ) {
          assert(Set.empty.size == 0)
          /*
          empty.minBy
          java.lang.UnsupportedOperationException: empty.minBy
           */
          assume(Seq.empty[Int].minBy(_.abs) === -1)
        }
  ignore/*test*/(
        "sort of Set by 'abs' should work"
      ) {
          //assert(Set.empty.size == 0)
          assume(Seq(-1, -2, 3, 4, 5).minBy(_.abs) === -1, "must be = -1")
        }
  test(
        "Complex test case should work"
      ) {
          //assert(Set.empty.size == 0)
          assume(
                  "-5 -4 -2 12 -40 4 2 18 11 5"
                    .split(' ')
                    .map(_.toInt)
                    .minBy(_.abs) === -1, "must be = 2, not (-2)"
                )
        }
}

class TemperaturesSpec extends FlatSpec with Matchers {

  import temperatures.Solution

  "A '1'" should "be equal '1'" ignore {
    1 should be(1)
  }

  "A empty input" should "return '0'" in {
    Solution.loop(Seq.empty[Int]) should be(0)
  }

  "closest to zero from '-5' & '5'" should "be '5'" in {
    Solution.chooseClosestToZero(-7, 5, -5) should be(5)
  }
}
