package testHorseRacingDuals

import MIME_Type.SolutionMain.getFileContent
import HorseRacingDuals.Solution._
import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/11/15.
 */
class TestHorseRacingDualsSuit
  extends FunSuite {

  test(
        "1: 'returnMIME_types' result should match 'Test_3_output.txt'"
      ) {
          val testPath: String =
            "/home/gluk-alex/Documents/codingameChallenge/src" +
              "/test/scala/" +
              "testHorseRacingDuals/" +
              "SimpleCase/"
          //Test_1_input.txt
          val testInputArray: Array[Int] =
            getFileContent(
                            "Test_1_input.txt",
                            testPath
                          )
            .map(_.toInt)
            .toArray
          //Test_1_input.txt
          val testOutputStr: String =
            getFileContent(
                            "Test_1_output.txt",
                            testPath
                          ).next()
          val (fst, snd) =
            closestPair(testInputArray)

          //assert(100000 == 100000)
          assume(
                  testOutputStr.toInt
                    ==
                    snd - fst,
                  "must be equal"
                )
        }
}
