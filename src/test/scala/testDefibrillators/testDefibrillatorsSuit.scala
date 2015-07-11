package testDefibrillators

//import MIME_Type.SolutionMain._

import MIME_Type.SolutionMain.getFileContent
import Defibrillators.Solution._
import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/11/15.
 */
class testDefibrillatorsSuit
  extends FunSuite {

  test(
        "1: 'returnMIME_types' result should match 'Test_3_output.txt'"
      ) {
          val testPath: String =
            "/home/gluk-alex/Documents/codingameChallenge/src" +
              "/test/scala/" +
              "testDefibrillators/" +
              "CompleteFile1/"
          //Test_3_input.txt
          val testOutputStr: String =
            getFileContent(
                            "Test_3_output.txt",
                            testPath
                          ).next()
          val testOutputResultStr: String =
            closestDefibrilator.name

          //assert(100000 == 100000)
          assume(
                  testOutputStr
                    ==
                    testOutputResultStr,
                  "must be equal"
                )
        }
  test(
        "2: 'extractParams' should extract three specific fields from input " +
          "string"
      ) {
          val inputString: String =
            "107;Caisse Primaire d'Assurance Maladie;29 cours Gambetta 34000 " +
              "MONTPELLIER;04 99 52 54 49;3,87110915929521;43,6065196099402"

          //assert(100000 == 100000)
          assume(
                  extractParams(inputString)
                    ==
                    ("Caisse Primaire d'Assurance Maladie",
                      3.87110915929521,
                      43.6065196099402),
                  "must be equal"
                )
        }
  test(
        "3: 'calculateDistance' should work"
      ) {
          val inputString: String =
            "107;Caisse Primaire d'Assurance Maladie;29 cours Gambetta 34000 " +
              "MONTPELLIER;04 99 52 54 49;3,87110915929521;43,6065196099402"
          val (defibrilator, longitude, latitude): (String, Double, Double) =
            extractParams(inputString)

          /*distArr(i) =*/
          val dist: Double =
            calculateDistance(
                               fromLongitude = longitude,
                               fromLatitude = latitude,
                               toLongitude = longitude,
                               toLatitude = latitude
                             )
          //assert(100000 == 100000)
          assume(
                  dist
                    ==
                    43.6065196099402,
                  "must be equal"
                )
        }
}
