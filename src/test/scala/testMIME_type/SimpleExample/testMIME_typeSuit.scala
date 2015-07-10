package testMIME_type.SimpleExample

import org.scalatest.FunSuite

/**
 * Created by gluk-alex on 7/9/15.
 */
class testMIME_typeSuit extends FunSuite {

  import MIME_Type.Solution._
  import MIME_Type.SolutionMain._

  /*

  <OK>load 'input'
  <OK>load 'output'
  <OK>fetch 'input' to 'Solution'
  <OK>compare result with 'output'
   */
  test(
        "1: 'Test_3_input.txt' should have size 15 entries / lines"
      ) {
          //"/home/gluk-alex/Documents/codingameChallenge/src/test/scala
          // /testMIME_type/SimpleExample/Test_1_input.txt"
          val testInput =
            getFileContent(
                            "Test_3_input.txt",
                            "/home/gluk-alex/Documents/codingameChallenge/src" +
                              "/test/scala" +
                              "/testMIME_type/SimpleExample/" +
                              "CorrectDivisionOfTheExtension/"
                          )
          val testInputArray: Array[String] =
            testInput.toArray

          //assert(100000 == 100000)
          assume(
                  testInputArray
                  .length
                    ==
                    15,
                  "must be '15'"
                )
        }

  test(
        "2: 'Test_3_output.txt' should have size 15 entries / lines"
      ) {
          val testPath: String =
            "/home/gluk-alex/Documents/codingameChallenge/src" +
              "/test/scala" +
              "/testMIME_type/SimpleExample/"
          /*val testInput =
            getFileContent(
                            "Test_3_input.txt",
                            testPath +
                              "CorrectDivisionOfTheExtension/"
                          )*/
          val testOutputArray: Array[String] =
            getFileContent(
                            "Test_3_output.txt",
                            testPath +
                              "CorrectDivisionOfTheExtension/"
                          ).toArray
          /*val testInputArray: Array[String] =
            testInput.toArray*/

          //assert(100000 == 100000)
          assume(
                  testOutputArray
                  .length
                    ==
                    10,
                  "must be '10'"
                )
        }
  test(
        "3: 'returnMIME_types' result should match 'Test_3_output.txt'"
      ) {
          val testPath: String =
            "/home/gluk-alex/Documents/codingameChallenge/src" +
              "/test/scala" +
              "/testMIME_type/SimpleExample/"+
              "CorrectDivisionOfTheExtension/"
          val testOutputVector: Vector[String] =
            getFileContent(
                            "Test_3_output.txt",
                            testPath
                          ).toVector
          val testOutputResultVector: Vector[String] =
            returnMIME_types(
                              "Test_3_input.txt",
                                testPath
                            )//.toVector

          //assert(100000 == 100000)
          assume(
                  testOutputVector
                    ==
                    testOutputResultVector,
                  "must be equal"
                )
        }
  test(
        "4: 'returnMIME_types' result should match 'Test_4_output.txt'"
      ) {
          val testPath: String =
            "/home/gluk-alex/Documents/codingameChallenge/src" +
              "/test/scala" +
              "/testMIME_type/SimpleExample/"+
              "ConsiderationOfTheCase/"
          val testOutputVector: Vector[String] =
            getFileContent(
                            "Test_4_output.txt",
                            testPath
                          ).toVector
          val testOutputResultVector: Vector[String] =
            returnMIME_types(
                              "Test_4_input.txt",
                                testPath
                            )//.toVector

          //assert(100000 == 100000)
          assume(
                  testOutputVector
                    ==
                    testOutputResultVector,
                  "must be equal"
                )
        }
  test(
        "5: 'returnMIME_types' result should match 'Test_5_output.txt' (9999 * 2 entries)"
      ) {
          val testPath: String =
            "/home/gluk-alex/Documents/codingameChallenge/src" +
              "/test/scala" +
              "/testMIME_type/SimpleExample/"+
              "LargeDataset/"
          val testOutputVector: Vector[String] =
            getFileContent(
                            "Test_5_output.txt",
                            testPath
                          ).toVector
          val testOutputResultVector: Vector[String] =
            returnMIME_types(
                              "Test_5_input.txt",
                                testPath
                            )//.toVector

          //assert(100000 == 100000)
          assume(
                  testOutputVector
                    ==
                    testOutputResultVector,
                  "must be equal"
                )
        }
  test(
        "6: 'findExtension' result should match 'UNKNOWN' if no extension"
      ) {
          lazy val ext =
            findExtension(".mp3.")

          assume(
                  //MIME_Type.Solution.findExtension("a.a")
                    ext ==
                    "UNKNOWN",
                  "must be 'UNKNOWN'"
                )
        }
  test(
        "7: 'getFileContent' should work when path / file name is wrong "+
          "(No such file or directory)"
      ) {
          lazy val fileIterator =
            getFileContent("a.a")

          /*if (fileIterator.hasNext) {
            println(fileIterator.next())
          }*/
          assume(
                  fileIterator.nonEmpty,
                  "must be 'nonEmpty'"
                )
        }
}