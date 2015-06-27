package ChuckNorris

/*
Chuck Norris' keyboard has 2 keys: white space and 0
 */
/*
task
Binary with '0' and '1' is good, but
binary with only '0', or almost, is even better!
Originally,
this is
a concept designed by Chuck Norris to
send so called `unary messages`.

Here is the `encoding principle`:
The input `message` consists of ASCII characters (7-bit)
The `encoded` output `message` consists of
`blocks` of '0'
A `block` is
separated from another block by
a `space`
Two `consecutive` `blocks` are used to
produce
a series of
same value `bits` (only 1s or 0s):
`First block`:
  it is always '0' or '00'.
  If it is '0', then
  the series contains 1s,
  if not,
  it contains 0s
`Second block`:
  the number of 0s in this `block` is
  the number of
  `bits` in the `series`

Let us take
a simple `example` with
a `message` which consists of
only one `character`: Capital 'C'.
'C' in binary is represented as '1000011', so
with Chuck Norris’ technique this gives:
  0 0 (the first series consists of only a single 1)
  00 0000 ((the second series consists of four 0)
  0 00 (the third consists of two 1)
So 'C' is coded as: '0 0 00 0000 0 00'

Second `example`,
we want to
'encode' the `message` 'CC'
(i.e. the '14' bits '10000111000011') :
  0 0 (one single 1)
  00 0000 (four 0)
  0 000 (three 1)
  00 0000 (four 0)
  0 00 (two 1)
So 'CC' is coded as: '0 0 00 0000 0 000 00 0000 0 00'

TODO
Write a program that
takes an incoming `message` as `input` and
displays as `output`
the message `encoded` using Chuck Norris’ method.

INPUT:
Line 1: the message consisting of 'N' ASCII `characters` (without `carriage return`)

OUTPUT:
The encoded message

CONSTRAINTS :
0 < N < 100

EXAMPLE :
Input
C
Output
0 0 00 0000 0 00
 */

/**
 * Created by Alex on 26.06.15.
 */
object Solution extends App {
  /*helper start*/
  /*for each symbol in message separately*/
  def getCharBinaryForm(
                         letter: Char
                         ): String =
  {
    //val binaryForm: String =
    letter.toInt.toBinaryString
  }

  def encodeWithChuck(
                       binaryMessage: String
                       ): String =
  {
    /*val zero = 0
    val one = 1*/

    /*trait Bit
    class object Zero extends Bit
    class object One extends Bit*/

    /*for each bit in symbol*/
    def readBinarySequence(
                            leadingBit: Int /*Option[Bit]*/ = (-1),
                            stringToRead: String,
                            encodedString: String = ""
                            ): String =
    {
      if (stringToRead.isEmpty) {
        /*basic case*/
        /*return value*/
        encodedString
      } else {
        /*iterative step*/

        val newleadingBit: Int /*Char*/ =
          if (leadingBit /*.isEmpty*/ == -1) {
            /*initial / first value*/
            /*if (stringToRead.head == '1') {
              Some(One)
            } else {
              Some(Zero)
            }*/
            stringToRead.head.asDigit
          } else {
            leadingBit //.toChar
          }

        val (bitSeq, restOfString) =
          stringToRead
            //.span(c => c == newleadingBit)
            .span(_.toString == newleadingBit.toString /*.toChar*/)

        val encodeBlock: String =
          if (newleadingBit == 1 /*'1'*/ ) {
            "0"
          } else {
            "00"
          }
        /*recursion*/
        readBinarySequence(
                            leadingBit = (newleadingBit /*.getNumericValue*/ - 1) * (-1),
                            stringToRead = restOfString,
                            encodedString =
                              if (encodedString.isEmpty) {
                                encodeBlock + " " + "0" * bitSeq.length
                              } else {
                                encodedString + " " + encodeBlock + " " + "0" * bitSeq.length
                              }
                          )
      }
    }
    /*return value*/
    readBinarySequence(
                        leadingBit = (-1),
                        stringToRead = binaryMessage /*binaryForm*/
                        /*letter.toString*/ ,
                        encodedString = ""
                      )
  }

  def encodeMessage(message: String): String = {
    val binaryMessage: String =
      (
      for (symbol <- message) yield symbol.toInt.toBinaryString
      ).mkString("")
    /*return value*/
    //binaryMessage//works
    encodeWithChuck(binaryMessage = binaryMessage)
  }

  /*helper end*/

  val message =
    'C'
  //""
  //scala.io.StdIn.readLine

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println("answer")
  println(s"message:$message")
  //Byte	8 bit signed value. Range from -128 to 127
  println(s"message.toByte:${ message.toByte }")
  //println(s"message.toByte:${message.toBinaryString}")
  println(s"message.charValue():${ message.charValue() }")
  //println(s"message.charValue():${message.charValue().toBinaryString}")
  println(s"message.asDigit():${ message.asDigit }")
  println(s"message.asDigit().toBinaryString:${ message.asDigit.toBinaryString }")
  println(s"'c'.asDigit().toBinaryString:${ 'c'.asDigit.toBinaryString }")
  println(s"'C'.asDigit().toBinaryString:${ 'C'.asDigit.toBinaryString }")
  println(s"'c'.toInt.toBinaryString:${ 'c'.toInt.toBinaryString }")
  println(s"'C'.toInt.toBinaryString:${ 'C'.toInt.toBinaryString }")
  //println(s"'c'.asDigit().toBinaryString:${"c".asDigit.toBinaryString}")
  println(s"message.getNumericValue:${ message.getNumericValue }")
  println(s"message.getNumericValue.toBinaryString:${ message.getNumericValue.toBinaryString }")
  /*println(s"message.until(70,1):${message.until(70,1)}")
  println(s"message.to(80):${message.to(80)}")
  println(s"message.→(2):${message.→(2)}")
  println(s"message.reverseBytes:${message.reverseBytes}")
  println(s"'1'.##:${1.##}")
  println(s"'1'.toBinaryString:${1.toBinaryString}")
  println(s"'3'.toBinaryString:${3.toBinaryString}")
  println(s"'3'.toHexString:${3.toHexString}")
  println(s"'9'.toHexString:${9.toHexString}")
  println(s"'11'.toHexString:${11.toHexString}")
  println(s"'19'.toHexString:${19.toHexString}")
  println(s"'0'*3:${ "0" * 3 }")
  println(s"'00'*7:${ "00" * 7 }")
  println(s"'1000011'.span(_=='1'):${ "1000011".span(_ == '1') }")
  println(s"'1000011'.takeWhile(_=='1'):${ "1000011".takeWhile(_ == '1') }")
  println(s"'1000011'.dropWhile(_=='1'):${ "1000011".dropWhile(_ == '1') }")
  println(s"'000011'.span(_=='0'):${ "000011".span(c => c == '0') }")
  println(s"'000011'.takeWhile(_=='0'):${ "000011".takeWhile(_ == '0') }")
  println(s"'000011'.dropWhile(_=='0'):${ "000011".dropWhile(_ == '0') }")*/
  println(s"''0'' == '0':${ "0" == '0' }")
  println(s"''0'' == 0.toChar:${ "0" == 0.toChar }")
  println(s"''0'' == 0:${ "0" == 0 }")
  println(s"''0'' == 0.toString:${ "0" == 0.toString }")
  println(s"'0' == '0':${ '0' == '0' }")
  println(s"'0' == 0:${ '0' == 0 }")
  println(s"'0'.toString == 0:${ '0'.toString == 0 }")
  println(s"'0'.toString:${ '0'.toString }")
  println(s"'0'.toInt == 0:${ '0'.toInt == 0 }")
  println(s"'0'.toInt:${ '0'.toInt }")
  println(s"'0' == 0.toChar:${ '0' == 0.toChar }")
  println(s"'0' == 0.toString:${ '0' == 0.toString }")
  println(s"'0'.toString == 0.toString:${ '0'.toString == 0.toString }")

  /*println(
           s"encodeWithChuck('C'):${
             encodeWithChuck("C")
           }"
         )*/
  println(
           s"encodeMessage(message='CC'):${
             encodeMessage(message = "CC")
           }"
         )

  /*println(
           s"encodeWithChuck('%'):${
             encodeWithChuck("%")
           }"
         )*/
  println(
           s"encodeMessage(message='%'):${
             encodeMessage(message = "%")
           }"
         )
}
