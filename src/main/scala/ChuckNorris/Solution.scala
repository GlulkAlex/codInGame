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
object Solution /*extends App*/ {
  /*helper start*/
  def stringToBytesASCII(str: String): Array[Byte] = {
    val b: Array[Byte] = new Array(str.length)

    for (
      i <- b.indices
    ) {
      b(i) = str.charAt(i).toByte
    }
    /*return value*/
    b
  }

  /*def stringToBytesUTFCustom(str: String): Array[Byte] = {
    val b: Array[Byte] = new Array(str.length << 1)
    for (
      i <- b.indices
    ) {
      val strChar: Char = str.charAt(i)
      val bpos: Int = i << 1
      b(bpos) = ((strChar & 0xFF00) >> 8).toByte
      b(bpos + 1) = (strChar & 0x00FF).toByte
    }
    /*return value*/
    b
  }*/

  /*
  JavaScript:
  "%".charCodeAt(0).toString(2)
  "100101"
   */

  /*for each symbol in message separately*/
  /*ASCII characters (7-bit)*/
  /*trick is that 'BinaryForm' must be 7 digits long*/
  def getCharBinaryForm(
                         letter: Char
                         ): String =
  {
    val binaryForm: String =
      letter.toInt.toBinaryString

    if (binaryForm.length < 7) {
      "0" * (7 - binaryForm.length) + binaryForm
    } else {
      binaryForm
    }
  }

  def readSymbolBlock(
                       codedMessage: String,
                       blockOf: String = ""
                       ): (Int /*String*/ , String) =
  {
    if (
      codedMessage.isEmpty
    ) {
      (if (blockOf.size > 1) {
        0
      } else {
        1
      }, "")
    } else if (
      codedMessage.head == " " ||
      codedMessage.head == ' '
    ) {
      (if (blockOf.size > 1) {
        0
      } else {
        1
      }, codedMessage.tail)
    } else {
      /*recursion*/
      readSymbolBlock(
                       codedMessage = codedMessage.tail,
                       blockOf = blockOf + codedMessage.head
                     )
    }
  }

  def readBlockSize(
                     codedMessage: String,
                     blockSize: Int = 0
                     ): (Int, String) =
  {
    if (
      codedMessage.isEmpty
    ) {
      (blockSize, "")
    } else if (
      codedMessage.head == " " ||
      codedMessage.head == ' '
    ) {
      (blockSize, codedMessage.tail)
    } else {
      /*recursion*/
      readBlockSize(
                     codedMessage = codedMessage.tail,
                     /*counter*/
                     blockSize = blockSize + 1
                   )
    }
  }

  def decodeChuckEncoding(
                           codedMessage: String,
                           deCodedMessage: String = "",
                           blockOf: /*Int = 0*/ String = "", //'0' or '00'
                           symbolsInBlock: Int = 0 //,
                           //isReadingBlockSymbol: Boolean = true,
                           //isReadingBlockSize: Boolean = false
                           ): String =
  {
    if (codedMessage.isEmpty) {
      /*return value*/
      deCodedMessage
    } else {
      //val (newBlockOf, codedWithoutBlockOf): (Int, String) =
      if (
        symbolsInBlock == 0 &&
        blockOf == ""
      ) {
        val (newBlockOf, codedWithoutBlockOf): (Int, String) =
          readSymbolBlock(
                           codedMessage = codedMessage.tail,
                           blockOf = "" + codedMessage.head
                         )
        /*recursion*/
        decodeChuckEncoding(
                             /*it must converge to prevent endless loop*/
                             codedMessage = codedWithoutBlockOf /*codedMessage.tail*/ ,
                             deCodedMessage = deCodedMessage,
                             blockOf = "" + newBlockOf,
                             symbolsInBlock = 0
                           )
      } else {
        /*to preserve value*/
        //val currentBlockOf = blockOf
        //val currentSymbolsInBlock = symbolsInBlock
        val (newSymbolsInBlock, codedWithoutBlockSize): (Int, String) =
          readBlockSize(
                         codedMessage = codedMessage.tail,
                         blockSize = 1
                       )
        val newDeCodedMessage = deCodedMessage + blockOf * newSymbolsInBlock

        /*recursion*/
        decodeChuckEncoding(
                             /*it must converge to prevent endless loop*/
                             codedMessage = /*codedMessage.tail*/ codedWithoutBlockSize,
                             deCodedMessage = newDeCodedMessage,
                             blockOf = "", //'0' or '00'
                             symbolsInBlock = 0 //,
                           )
      }
    }
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
            stringToRead
              .head
              /*? crucial method ?*/
              .asDigit
            //.getNumericValue
            //.toInt //char code
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
      for (
        symbol <- message
        if !symbol.isControl
      ) yield
      getCharBinaryForm(
                         letter = symbol
                       )
      /*symbol
        .hashCode()
        //.toByte
        //.toChar
        //.toInt
        .toBinaryString*/
      ).mkString("")
    /*return value*/
    //binaryMessage//works
    encodeWithChuck(binaryMessage = binaryMessage)
  }

  /*helper end*/

  val message: String =
  //'%'
    "Chuck Norris' keyboard has 2 keys: 0 and white space."
  //""
  //scala.io.StdIn.readLine

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println("answer")
}

object Main /*Solution*/ extends App {

  import Solution._

  println(s"message:$message")
  //Byte	8 bit signed value. Range from -128 to 127
  //println(s"message.toByte:${ message.toByte }")
  //println(s"message.getDirectionality:${ message.getDirectionality }")
  //println(s"message.isHighSurrogate:${ message.isHighSurrogate }")
  //println(s"message.charValue():${ message.charValue() }")
  //println(s"message.charValue().getNumericValue:${ message.charValue().getNumericValue }")
  //println(s"message.charValue():${message.charValue().toBinaryString}")
  //println(s"message.asDigit():${ message.asDigit }")
  //println(s"message.longValue:${ message.longValue }")
  //println(s"message.intValue():${ message.intValue() }")
  //println(s"message.byteValue():${ message.byteValue }")
  //println(s"message.floatValue():${ message.floatValue() }")
  //println(s"message.doubleValue():${ message.doubleValue() }")
  //println(s"message.abs:${ message.abs }")
  //println(s"message.isUnicodeIdentifierStart:${ message.isUnicodeIdentifierStart }")
  //println(s"message.isUnicodeIdentifierPart:${ message.isUnicodeIdentifierPart }")
  //println(s"message.asDigit().toBinaryString:${ message.asDigit.toBinaryString }")
  //println(s"message.toInt().toBinaryString:${ message.toByte.toBinaryString }")
  //println(s"message.toInt().toBinaryString:${ message.toShort.toBinaryString }")
  //println(s"message.toInt().toBinaryString:${ message.toInt.toBinaryString }")
  //println(s"message.toUpper.toInt().toBinaryString:${ message.toUpper.toInt.toBinaryString }")
  //println(s"message.toInt().toBinaryString:${ message.toDouble.toBinaryString }")
  //println(s"message.toLong.toBinaryString:${ message.toLong.toBinaryString }")
  println(s"'c'.asDigit().toBinaryString:${ 'c'.asDigit.toBinaryString }")
  println(s"'C'.asDigit().toBinaryString:${ 'C'.asDigit.toBinaryString }")
  println(s"'C'.hashCode():${ 'C'.hashCode() }")
  println(s"'%'.hashCode():${ '%'.hashCode }")
  println(s"'%'.asDigit().toBinaryString:${ '%'.asDigit.toBinaryString }")
  println(s"'%'.hashCode.toBinaryString:${ '%'.hashCode.toBinaryString }")
  println(s"'%'.toInt.toBinaryString:${ '%'.toInt.toBinaryString }")
  println(s"'%'.getNumericValue.toBinaryString:${ '%'.getNumericValue.toBinaryString }")
  println(s"'c'.hashCode().toBinaryString:${ 'c'.hashCode().toBinaryString }")
  println(s"'c'.toInt.toBinaryString:${ 'c'.toInt.toBinaryString }")
  println(s"'C'.toInt.toBinaryString:${ 'C'.toInt.toBinaryString }")
  println(s"'1'.toInt:${ '1'.toInt }")
  println(s"'1'.asDigit:${ '1'.asDigit }")
  println(s"'1'.getNumericValue:${ '1'.getNumericValue }")
  println(s"'0'.asDigit:${ '0'.asDigit }")
  println(s"'0'.charValue():${ '0'.charValue() }")
  println(s"'0'.toChar:${ '0'.toChar }")
  println(s"'0'.getNumericValue:${ '0'.getNumericValue }")
  println(s"'0'.toInt:${ '0'.toInt }")
  //println(s"'c'.asDigit().toBinaryString:${"c".asDigit.toBinaryString}")
  //println(s"message.getNumericValue:${ message.getNumericValue }")
  //println(s"message.getNumericValue.toBinaryString:${ message.getNumericValue.toBinaryString }")
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
           s"encodeMessage(message='CC'):\n${
             encodeMessage(message = "CC")
           }\n expected:\n0 0 00 0000 0 000 00 0000 0 00"
         )

  /*println(
           s"encodeWithChuck('%'):${
             encodeWithChuck("%")
           }"
         )*/
  println(
           s"encodeMessage(message='%'):\n${
             encodeMessage(message = "%")
           }\n expected:\n00 0 0 0 00 00 0 0 00 0 0 0"
         )
  println(
           s"decodeChuckEncoding(message='%'):\n${
             readSymbolBlock(codedMessage = "0 0 00 0000 0 000 00 0000 0 00")
           }\n expected:\n(0,?)"
         )
  println(
           s"decodeChuckEncoding(message='%'):\n${
             readSymbolBlock(codedMessage = "00 0000 0 000 00 0000 0 00")
           }\n expected:\n(1,?)"
         )
  println(
           s"decodeChuckEncoding(message='%'):\n${
             readBlockSize(codedMessage = "0000")
           }\n expected:\n(4,'')"
         )
  println(
           s"decodeChuckEncoding(message for 'CC'0 0 00 0000 0 000 00 0000 0 00):\n${
             decodeChuckEncoding(codedMessage = "0 0 00 0000 0 000 00 0000 0 00")
           }\n expected:\n10000111000011"
         )
  println(
           s"decodeChuckEncoding(message for '%'00 0 0 0 00 00 0 0 00 0 0 0):\n${
             decodeChuckEncoding(codedMessage = "00 0 0 0 00 00 0 0 00 0 0 0")
           }\n expected:\n?"
         )
  println(
           s"stringToBytesASCII(str='C'):\n${
             stringToBytesASCII(str = "C").mkString("[", ",", "]")
           }\n expected:\n1000011"
         )
  println(
           s"stringToBytesASCII(str='%'):\n${
             stringToBytesASCII(str = "%").mkString("[", ",", "]")
           }\n expected:\n?"
         )
  /*println(
           s"stringToBytesASCII(str='%'):\n${
             stringToBytesUTFCustom(str = "%")//.mkString
           }\n expected:\n?"
         )*/
  println(
           s"Integer.toBinaryString(37):\n${
             Integer.toBinaryString(37) //.mkString
           }\n expected:\n?"
         )
  println(
           s"42.toBinaryString:\n${
             42.toBinaryString
           }\n expected:\n?"
         )

  val bootomBeackon = true
}
