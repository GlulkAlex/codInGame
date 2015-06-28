package CodeOfTheRings

//package OptimizationChallengeOneDayLong

/*
Middle-Earth – September 21 2940 of the Third Age, 12:01
 */
/*
task description
Welcome to
the first 24 hour `optimization challenge`.
Please note that
the available 24 hours does not reflect
the difficulty of the challenge.
Also
it does not mean that
you will have to code 24 hours in a row.

It is in fact
quite simple to
obtain a 100% score.
However,
we have made it possible for you to
submit a solution several times across the next 24 hours,
so that
you may attempt to
improve your code at any moment.

For this question,
you will be asked to
output a single line as
a solution.
Your `rank` for this challenge will be
evaluated by
the length of your output,
which you will want to `minimize`.

The players with
the shortest output
across all validator test cases
at the end of the 24 hours
will be the winners.
 */
/*
TODO
The program:
The aim of this game is to
help Bilbo escape a forest by
sending him
a sequence of instructions
that will make him
spell out
a given `sentence` using `magic stones`.

Your program receives
a string of `uppercase characters` –
the `magic phrase` Bilbo needs to spell out – and
must `output`
a sequence of characters,
each representing
an `action` for Bilbo to perform.
These actions will have
Bilbo move around in the forest,
interacting with the different stones.

The `forest` (where) Bilbo is trapped works as follows:
The `forest` contains 30 `zones`.
Bilbo can
move `left` and `right` through the `zones`
which are all aligned.
He does this when
he receives
a less-than '<' or
greater-than '>' sign.
The last `zone` is
connected to the first `zone`,
effectively creating a `looping area`.
Each `zone` contains
a magic stone
upon which is
inscribed a `rune` with which
Bilbo can `interact`.

`Runes` work as follows:
Every `rune` is
represented by
a `letter` of the alphabet (A-Z) or
an `empty space`.
All `runes` start out as a `space`.
Bilbo can
change the `value` of the `letter` on the rune by
`rolling` back and forth through the possibilities.
He does this when
he receives
a plus '+' or
minus '-' character.
The `letter` after 'Z' is 'space'.
The `letter` after 'space' is 'A'.
Bilbo can
`trigger` a `rune`.
This will
add the displayed `letter` to
the phrase he is `spelling` out.
He does this when
he receives a dot '.' character.
One `rune` can
be `triggered` several times.

YOU WIN IF
THE `MAGIC PHRASE` IS
`SPELLED OUT` CORRECTLY
AT THE END OF BILBO'S MOVE SEQUENCE.

You lose if:
At the end of Bilbo's move sequence,
the wrong message is displayed.
Bilbo performs
4000 moves or
more in the forest.
You do not
supply Bilbo with
a valid sequence of actions.

Example:

If Bilbo needs to
`spell out` the word 'AB',
you could
give him the instructions '+.+..'
He will:
1)Make the first rune go from 'space' to 'A'.
2)Trigger the 'A'.
3)Make rune go from 'A' to 'B'.
4)Trigger the 'B'.
Alternatively,
you could also
give him the instructions '+.>++'.
to achieve the same result.
Experiment with different tactics!
For expert CodinGamers
(don't bother with this until
you've really tried the rest): Loops.
These actions are also available:
'[': If
the current rune contains a 'space',
skip all instructions up to
the matching closing bracket ']',
otherwise
continue normally.
']': If
the current rune contains a 'space',
continue normally,
otherwise
perform the instructions starting from
the matching opening bracket '['.
This makes it possible to
have Bilbo perform more actions with
less characters for your program to output.

For example,
'AAAAAAAAAAAAAAAAAAAAAAAAAA' (A x26)
can be achieved with
a simple
'+..........................' as well as with
 '+>-[<.>-]'.

Experiment with different techniques,
if you have the guts!
 */
/*
INITIALIZATION INPUT:
Line 1:
one string `magicPhrase`,
the message
you must help Bilbo to `spell out`.
OUTPUT:
A single line containing
a valid `sequence of actions` for Bilbo.

Here are
the `available actions`
you can send to bilbo:
'>': Bilbo `moves` one zone to the `right`.
'<': Bilbo `moves` one zone to the `left`.
'+': Bilbo `rolls` the `letter` on the `rune` of the `zone` he is
in one `letter` (forward)`along` the alphabet.
'-': Bilbo `rolls`
the `letter` on
the `rune` of
the `zone` he is in
one `letter` `back`(ward) through the alphabet.
'.': Bilbo `triggers` the `rune` to
add its `letter` to the magic phrase.
CONSTRAINTS:
`magicPhrase` contains between 1 and 500 characters.
`magicPhrase` is composed of `uppercase letters` (A-Z) and 'spaces'.
Allotted response time to output is <= 2 seconds.
 */

/**
 * Created by Alex on 27.06.15.
 */
object Player extends App {
  val rangeEnd: Int = 'Z'.getNumericValue

  val magicAlphabet: IndexedSeq[Char] /*Vector[Char]NumericRange.Inclusive[Char]*//*Range[Char]*/ =
  //('A' to 'Z') :+ (' ')
    (' ') +: ('A' to 'Z')

  def SpellLettersFrequency(
                             spell: String,
                             lettersFrequencyMap: Map[Char, Int] = Map.empty[Char, Int]
                             ): Map[Char, Int] =
  {
    if (spell.isEmpty) {
      /*return value*/
      lettersFrequencyMap
    } else {
      val letter: Char = spell.head
      val frequency: Int =
        if (lettersFrequencyMap.contains(letter)) {
          lettersFrequencyMap.get(letter).get + 1
        } else {
          1
        }
      /*recursion*/
      SpellLettersFrequency(
                             spell = spell.tail,
                             lettersFrequencyMap + (letter -> frequency)
                           )
    }
  }

  //(' ') `space` is default / initial / starting `rune` value
  /*'A'*/
  var leftLetter: Char = ' '
  /*'B'*/
  var middleLetter: Char = ' '
  /*'C'*/
  var rightLetter: Char = ' '
  var currentLetter: Char = ' '
  var previousLetter: Char = ' '
  /*'+'*/
  var forwardFromPreviousToCurrent: Int = 0
  /*'-'*/
  var backwardFromPreviousToCurrent: Int = 0

  /*
  sequence for any letter must be less then magicAlphabet.length / 2 = 13 + 1
  also
  additional shortcuts / partitioning
  >>special symbol 'space' max controls length '>>>>' = 4
    if using 'space' from both sides then
    max controls length '>>' = 2
  >>letters (backward) from 'space' to 27 / 3 = 9 'S' included
    max controls length (when set up) '<<<--------' = 11
  >>letters (forward) from 'space' to 27 / 3 = 9 'H' included
    max controls length (when set up) '>>>--------' = 11
  */
  trait Rune

  case object LeftRune extends Rune

  case object MiddleRune extends Rune

  case object RightRune extends Rune

  trait RuneLetter

  case class LeftLetter(index: Int) extends RuneLetter

  case class MiddleLetter(index: Int) extends RuneLetter

  case class RightLetter(index: Int) extends RuneLetter

  case class RuneState(
                        currentRune: Rune,
                        leftState: LeftLetter,
                        middleState: MiddleLetter,
                        rightState: RightLetter
                        )

  def spellOutPhrase(
                      phrase: String,
                      /*no commands*/
                      spell: String = "",
                      //previousLetter: Char = ' ',
                      currentRune: Rune = MiddleRune,
                      leftIndex: Int = 0,
                      middleIndex: Int = 0,
                      rightIndex: Int = 0,
                      /*runeState: RuneState =
                      RuneState(
                                 currentRune = MiddleRune,
                                 leftState = LeftLetter(index = 0),
                                 middleState = MiddleLetter(index = 0),
                                 rightState = RightLetter(index = 0)
                               )
                      ,*/
                      //isSpaceUsed: Boolean = false,
                      alphabet: IndexedSeq[Char]
                      ): String =
  {
    if (phrase.isEmpty) {
      spell
    } else {
      /*or hardcode this as '27'*/
      val alphabetLength = alphabet.length
      /*val previousLetterIndex =
        alphabet.indexOf(previousLetter)*/
      val newLetterIndex =
        alphabet.indexOf(phrase.head)
      //var spaceUsed = false
      var newCurrentRune: Rune =
      //MiddleRune
      //runeState.currentRune
        currentRune
      var newLeftIndex = leftIndex
      var newMiddleIndex = middleIndex
      var newRightIndex = rightIndex

      /*if 'spaceUsed' then here defined actual rune position*/
      /*val spaceSymbol: (String, Rune)/*(String,Seq[Rune])*/ =
      /*">.<"*/
      //runeState.currentRune match {
        currentRune match {
          case LeftRune => {
            /*side effect*/
            //newCurrentRune = LeftRune
            /*return*/
            ("<.>", LeftRune)
          }
          case MiddleRune => {
            /*side effect*/
            //newCurrentRune = RightRune
            /*return*/
            (">>.<",RightRune)
          }
          case RightRune => {
            /*side effect*/
            //newCurrentRune = RightRune
            /*return*/
            (">.<",RightRune)
          }
        }*/

      var controlPrefix: String = ""
      val newLetter: String =
        if (
          //newLetterIndex != previousLetterIndex
          newLetterIndex != leftIndex &&
          newLetterIndex != middleIndex &&
          newLetterIndex != rightIndex
        ) {
          /*completely different new letter*/
          if (
            newLetterIndex == 0 &&
            /*(previousLetterIndex > 2 || (alphabetLength - previousLetterIndex) > 2)*/
            /*'3' max for 'MiddleRune'*/
            //(previousLetterIndex > 3 || (alphabetLength - previousLetterIndex) > 3)
            (leftIndex.min(middleIndex).min(rightIndex) > 3 ||
             (alphabetLength - leftIndex.min(middleIndex).min(rightIndex)) > 3)
          ) {
            /*special 'space' if not available more close letters*/

            /*side effect*/
            //spaceUsed = true
            /*set method parameters*/
            /*spaceSymbol._2 match {
              case LeftRune => {
                newLeftIndex = leftIndex
                newMiddleIndex = middleIndex
                newRightIndex = rightIndex
              }
              case MiddleRune => {
                newLeftIndex = leftIndex
                newMiddleIndex = middleIndex
                newRightIndex = rightIndex
              }
              case RightRune => {
                newLeftIndex = leftIndex
                newMiddleIndex = middleIndex
                newRightIndex = rightIndex
              }
            }*/

            /*return value*/
            //spaceSymbol._1
            newLeftIndex = leftIndex
            newMiddleIndex = middleIndex
            newRightIndex = rightIndex

            currentRune match {
              case LeftRune => {
                /*side effect*/
                newCurrentRune = LeftRune
                /*return*/
                "<.>"
              }
              case MiddleRune => {
                /*side effect*/
                newCurrentRune = RightRune
                /*return*/
                ">>.<"
              }
              case RightRune => {
                /*side effect*/
                newCurrentRune = RightRune
                /*return*/
                ">.<"
              }
            }
          } else {
            /*Letter, not a 'space'*/

            newCurrentRune =
              //runeState.currentRune
              currentRune
            val closestIndex: Int = {
              //var minDifference: Int = {
              //minDifference =
              //this                =
              //self: Int                =>
              var stateIndex: Int =
              //runeState.leftState.index
              //newLeftIndex
                newMiddleIndex
              //newRightIndex

              var difference: Int =
              //scala.math.abs(newLetterIndex - runeState.leftState.index)
                scala.math.abs(newLetterIndex - stateIndex)
              /*side effect*/
              newCurrentRune =
                //LeftRune
                MiddleRune

              /*newLeftIndex = leftIndex//newLetterIndex
              newMiddleIndex = newLetterIndex//newMiddleIndex
              newRightIndex = rightIndex//newRightIndex*/

              /*compare with neighbors*/
              if (
              //difference > scala.math.abs(newLetterIndex - runeState.middleState.index)
                difference > (scala.math.abs(newLetterIndex - newRightIndex) + 1) ||
                difference > (alphabetLength - newLetterIndex + newRightIndex + 1)
              ) {
                /*'Right' with '>' clother then 'Middle'*/
                difference =
                  //scala.math.abs(newLetterIndex - runeState.middleState.index)
                  scala.math.abs(newLetterIndex - newRightIndex)
                stateIndex =
                  //runeState.middleState.index
                  newRightIndex

                newCurrentRune =
                  RightRune
              } else if (
              //difference == scala.math.abs(newLetterIndex - runeState.middleState.index)
                difference == scala.math.abs(newLetterIndex - newRightIndex)
              ) {
                /*at the moment 'newCurrentRune' = 'MiddleRune'*/
                if (
                //newCurrentRune == LeftRune &&
                //newCurrentRune == runeState.currentRune
                  newCurrentRune == currentRune
                ) {
                  /*same, & stay put*/
                } else if (
                //newCurrentRune == MiddleRune
                  currentRune == RightRune
                ) {
                  /*side effect*/
                  newCurrentRune =
                    //MiddleRune
                    RightRune

                  /*newLeftIndex = leftIndex//newLetterIndex
                  newMiddleIndex = middleIndex//newMiddleIndex
                  newRightIndex = newLetterIndex//newRightIndex*/
                  /*same*/
                  /*difference =
                    scala.math.abs(newLetterIndex - runeState.middleState.index)*/
                  /*same*/
                  //stateIndex = runeState.middleState.index
                } else {
                  /*same*/
                }
              }

              /*at this moment 'newCurrentRune' is 'MiddleRune' or 'RightRune'*/
              /*only 'LeftRune' left*/
              if (
              //difference > scala.math.abs(newLetterIndex - runeState.rightState.index)
                difference > (scala.math.abs(newLetterIndex - newLeftIndex) + 1) ||
                difference > (alphabetLength - newLetterIndex + newLeftIndex + 1)
              ) {
                /*'Left' with '<' clother then 'Middle' ? or with '<<' then 'Right'? */
                /*useless reassign*/
                difference =
                  //scala.math.abs(newLetterIndex - runeState.rightState.index)
                  scala.math.abs(newLetterIndex - newLeftIndex)
                stateIndex =
                  //runeState.rightState.index
                  newLeftIndex

                newCurrentRune =
                  LeftRune
              } else if (
              //difference == scala.math.abs(newLetterIndex - runeState.rightState.index)
                difference == scala.math.abs(newLetterIndex - newLeftIndex)
              ) {
                /*at this moment 'newCurrentRune' may be 'MiddleRune' or 'RightRune'*/
                if (
                //(newCurrentRune == LeftRune ||
                  (newCurrentRune == RightRune ||
                   newCurrentRune == MiddleRune) &&
                  newCurrentRune == currentRune
                //newCurrentRune == runeState.currentRune
                ) {
                  /*same*/
                } else if (
                //newCurrentRune == RightRune
                  currentRune == LeftRune
                ) {
                  /*side effect*/
                  newCurrentRune =
                    //RightRune
                    LeftRune
                  /*useless reassign*/
                  /*difference =
                    scala.math.abs(newLetterIndex - runeState.rightState.index)*/
                  /*stateIndex = runeState.rightState.index*/

                  /*newLeftIndex = newLetterIndex
                  newMiddleIndex = middleIndex//newMiddleIndex
                  newRightIndex = rightIndex//newRightIndex*/
                } else {
                  /*same*/
                }
              }

              /*side effect*/
              if (
              //newCurrentRune == runeState.currentRune
                newCurrentRune == currentRune
              ) {
                /*same place / rune*/
                controlPrefix = ""
              } else {
                if (
                  (newCurrentRune == MiddleRune &&
                   /*runeState.*/ currentRune == LeftRune) ||
                  (newCurrentRune == RightRune &&
                   /*runeState.*/ currentRune == MiddleRune)
                ) {
                  /*go to right*/
                  controlPrefix = ">"
                } else if (
                  (newCurrentRune == MiddleRune &&
                   /*runeState.*/ currentRune == RightRune) ||
                  (newCurrentRune == LeftRune &&
                   /*runeState.*/ currentRune == MiddleRune)
                ) {
                  /*go to left*/
                  controlPrefix = "<"
                } else if (
                  newCurrentRune == LeftRune &&
                  /*runeState.*/ currentRune == RightRune
                ) {
                  /*go to left*/
                  controlPrefix = "<<"
                } else /*if (
                  newCurrentRune == RightRune &&
                  runeState.currentRune == LeftRune
                )*/ {
                  /*go to right*/
                  controlPrefix = ">>"
                }
              }
              /*side effect*/
              /*set method parameters*/
              newCurrentRune match {
                case LeftRune => {
                  newLeftIndex = newLetterIndex
                  newMiddleIndex = middleIndex
                  newRightIndex = rightIndex
                }
                case MiddleRune => {
                  newLeftIndex = leftIndex
                  newMiddleIndex = newLetterIndex
                  newRightIndex = rightIndex
                }
                case RightRune => {
                  newLeftIndex = leftIndex
                  newMiddleIndex = middleIndex
                  newRightIndex = newLetterIndex
                }
              }

              /*return*/
              //difference
              stateIndex
            }

            /*val closestToNewLetterRune: Rune =
              if (
                newLetterIndex == runeState.leftState.index &&
                newCurrentRune == LeftRune
              ) {
                /*same letter at the same position / rune*/
                /*only '.' needed*/
                LeftRune
              } else if (
                newLetterIndex == runeState.middleState.index &&
                newCurrentRune == MiddleRune
              ) {
                /*same letter at the same position / rune*/
                /*only '.' needed*/
                MiddleRune
              } else if (
                newLetterIndex == runeState.rightState.index &&
                newCurrentRune == MiddleRune
              ) {
                /*same letter at the same position / rune*/
                /*only '.' needed*/
                RightRune
              } else {
                newCurrentRune
              }*/

            /*val closestIndex: Int = closestToNewLetterRune match {
              case LeftRune => runeState.leftState.index
              case MiddleRune => runeState.middleState.index
              case RightRune => runeState.rightState.index
            }*/
            /*actual action, useful result here*/
            //val controlPrefix: String =
            /*if (closestToNewLetterRune == newCurrentRune) {
              ""
            }else*/
            /*if (closestToNewLetterRune == MiddleRune && newCurrentRune == LeftRune) {
                         /*side effect*/
                         newCurrentRune == MiddleRune
                         /*return value*/
                         ">"
                       } else if (closestToNewLetterRune == MiddleRune && newCurrentRune == RightRune) {
                         /*side effect*/
                         newCurrentRune == MiddleRune
                         /*return value*/
                         "<"
                       } else if (closestToNewLetterRune == LeftRune && newCurrentRune == MiddleRune) {
                         /*side effect*/
                         newCurrentRune == LeftRune
                         /*return value*/
                         "<"
                       } else if (closestToNewLetterRune == RightRune && newCurrentRune == MiddleRune) {
                         /*side effect*/
                         newCurrentRune == RightRune
                         /*return value*/
                         ">"
                       } else if (closestToNewLetterRune == LeftRune && newCurrentRune == RightRune) {
                         /*side effect*/
                         newCurrentRune == LeftRune
                         /*return value*/
                         "<<"
                       } else if (closestToNewLetterRune == RightRune && newCurrentRune == LeftRune) {
                         /*side effect*/
                         newCurrentRune == RightRune
                         /*return value*/
                         ">>"
                       } else {
                         ""
                       }*/

            if (newLetterIndex > closestIndex /*previousLetterIndex*/ ) {
              /*'T' char:20 > 'E' char:5*/
              /*'Z' char:26 > 'A' char:1*/
              if ((newLetterIndex - closestIndex /*previousLetterIndex*/) > alphabetLength / 2) {
                /*20 - 5 = 15 > 13 forward*/
                /*5 + 27 - 20 = 13 backward*/
                /*26 - 1 = 25 > 13 forward*/
                /*27 - 26 + 1 = 2 backward*/
                /*move backward*/
                /*and through the 'space'*/
                controlPrefix + ("-" * (alphabetLength - newLetterIndex + closestIndex /*previousLetterIndex*/)) + "."
                /*if (newLetterIndex > alphabetLength / 2) {
                  ("-" * (alphabetLength - previousLetterIndex + newLetterIndex )) + "."
                } else {
                  ("-" * (alphabetLength - newLetterIndex + previousLetterIndex)) + "."
                }*/
              } else {
                /*move forward*/
                controlPrefix + ("+" * (newLetterIndex - closestIndex /*previousLetterIndex*/)) + "."
              }
            } else /*if (newLetterIndex < previousLetterIndex)*/ {
              /*'I' char:9 < 'M' char:13*/
              if ((closestIndex /*previousLetterIndex*/ - newLetterIndex) > alphabetLength / 2) {
                /*move forward*/
                controlPrefix + (
                                "+" * (alphabetLength - (closestIndex /*previousLetterIndex*/ - newLetterIndex))
                                ) + "."
              } else {
                /*13-9=5<13*/
                /*move backward*/
                controlPrefix + ("-" * (closestIndex /*previousLetterIndex*/ - newLetterIndex)) + "."
              }
            }
          }
        } else {
          /*what the case ?*/
          /*one of the runes already has exact same letter*/
          /*Which one it is?*/
          /*same letter*/
          //controlPrefix + "."
          /*must work if pick first matched & ignore the rest*/
          /*do not forget set 'newCurrentRune'*/
          if (currentRune == MiddleRune && middleIndex == newLetterIndex) {
            //newCurrentRune=MiddleRune

            "."
          }else if (currentRune == RightRune && rightIndex == newLetterIndex) {
            /*same*/
            "."
          }else if (currentRune == LeftRune && leftIndex == newLetterIndex) {
            /*same*/
            "."
          }else if (currentRune == MiddleRune && leftIndex == newLetterIndex) {
            newCurrentRune=LeftRune
            "<."
          }else if (currentRune == MiddleRune && rightIndex == newLetterIndex) {
            newCurrentRune=RightRune
            ">."
          }else if (currentRune == RightRune && middleIndex == newLetterIndex) {
            newCurrentRune=MiddleRune
            "<."
          }else if (currentRune == LeftRune && middleIndex == newLetterIndex) {
            newCurrentRune=MiddleRune
            ">."
          }else if (currentRune == LeftRune && rightIndex == newLetterIndex) {
            newCurrentRune=RightRune
            ">>."
          }else /*if (currentRune == RightRune && leftIndex == newLetterIndex)*/ {
            newCurrentRune=LeftRune
            "<<."
          }
        }
      /*recursion*/
      spellOutPhrase(
                      /*make it converge*/
                      phrase = phrase.tail,
                      spell = spell + newLetter,
                      /*previousLetter =
                        if (spaceUsed) {
                          previousLetter
                        } else {
                          phrase.head
                        },*/
                      /*runeState =
                        RuneState(
                                   currentRune =
                                     newCurrentRune
                                   /*if (spaceUsed) {
                                     newCurrentRune
                                   } else {
                                     runeState.currentRune
                                   }*/
                                   ,
                                   leftState = runeState.leftState,
                                   middleState = MiddleLetter(index = previousLetterIndex),
                                   rightState = runeState.rightState
                                 )
                      ,*/
                      currentRune = newCurrentRune,
                      leftIndex = newLeftIndex,
                      middleIndex = newMiddleIndex,
                      rightIndex = newRightIndex,
                      //isSpaceUsed = spaceUsed,
                      alphabet
                    )
    }
  }

  val magicphrase: String =
  //'A'.to('Z'.getNumericValue.toInt,1)
  //'A'.to(30)
    'A'.to('Z')
      .toString()
  /*scala.io.StdIn
    .readLine*/

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println("+.>-.")

  println(s"magicphrase:${ magicphrase }")
  println(s"magicAlphabet:${ magicAlphabet }")
  println(s"magicphrase('A' to 'Z'):${ ('A' to 'Z') }")
  println(s"magicAlphabet find ' '(space) char:${ magicAlphabet.indexOf(' ') }")
  println(s"magicAlphabet find 'A' char:${ magicAlphabet.indexOf('A') }")
  println(s"magicAlphabet find 'M' char:${ magicAlphabet.indexOf('M') }")
  println(s"magicAlphabet find 'I' char:${ magicAlphabet.indexOf('I') }")
  println(s"magicAlphabet find 'Q' char:${ magicAlphabet.indexOf('Q') }")
  println(s"magicAlphabet find 'E' char:${ magicAlphabet.indexOf('E') }")
  println(s"magicAlphabet find 'T' char:${ magicAlphabet.indexOf('T') }")
  println(s"magicAlphabet find 'S' char:${ magicAlphabet.indexOf('S') }")
  println(s"magicAlphabet find 'H' char:${ magicAlphabet.indexOf('H') }")
  println(s"magicAlphabet find 'N' char:${ magicAlphabet.indexOf('N') }")

  println(s"magicAlphabet find 'A' string:${ magicAlphabet.indexOf("A") }")
  println(s"magicAlphabet find 'A' string:${ magicAlphabet.indexOf("A".head) }")
  println(s"magicAlphabet find 'Z' string:${ magicAlphabet.indexOf("Z".head) }")
  println(s"magicAlphabet.length:${ magicAlphabet.length }")
  println(s"'+++++++++++++++++++++++'.length:${ "+++++++++++++++++++++++".length }")
  println(s"(27/2):${ (27 / 2) }")
  println(s"'+'*(27/2):${ '+' * (27 / 2) }")
  println(s"'+'*(27/2):${ "+" * (27 / 2) }")
  println(
           s"SpellLettersFrequency 'MINAS':${
             SpellLettersFrequency(
                                    spell = "MINAS",
                                    lettersFrequencyMap = Map.empty[Char, Int]
                                  )
           }"
         )
  println(
           s"SpellLettersFrequency 'THREE':${
             SpellLettersFrequency(
                                    spell = "THREE",
                                    lettersFrequencyMap = Map.empty[Char, Int]
                                  )
           }"
         )

  /*
  The magic phrase is: MINAS
  +++++++++++++.-----------------------.+++++.--------------.---------.(wrong)
  +++++++++++++.+++++++++++++++++++++++.+++++.++++++++++++++.---------.(right)
  Failure: Bilbo spelled: MQVHZ
   */
  /*
  TODO
  Long Spell (last 24 test)
  my best result: 2599
  The magic phrase is:
  THREE RINGS FOR THE ELVEN KINGS UNDER THE SKY SEVEN FOR THE DWARF LORDS IN THEIR HALLS OF STONE NINE FOR MORTAL MEN DOOMED TO DIE ONE FOR THE DARK LORD ON HIS DARK THRONEIN THE LAND OF MORDOR WHERE THE SHADOWS LIE ONE RING TO RULE THEM ALL ONE RING TO FIND THEM ONE RING TO BRING THEM ALL AND IN THE DARKNESS BIND THEM IN THE LAND OF MORDOR WHERE THE SHADOWS LIE
  Failure: Bilbo has too many actions to perform (infinite loop?).
  Bilbo performs more then
  4000 moves
  (ended with ...SHADOWS LIE ONE RING TO RULE THEM ALL ONE RING TO FIND THEM ONE RING)
  Tip: use loops or move in forest zones
   */
  /*The magic phrase is: UMNE TALMAR RAHTAINE NIXENEN UMIR*/
  /*
  Far away letters
  The magic phrase is:
  G7U21 21-7=14 27-21=6
  GUZ MUG ZOG GUMMOG ZUMGUM ZUM MOZMOZ MOG ZOGMOG GUZMUGGUM
  fail on 'Z M'
  & 'GU' not optimal
   */
  /*
  TODO
  The magic phrase is:
  O OROFARNE LASSEMISTA CARNIMIRIE O ROWAN FAIR UPON YOUR HAIR HOW WHITE THE BLOSSOM LAY
  Failure:
  Bilbo spelled:
  O OROFARNE LASSEM
                   Q AA ZXZVIUIZIE W ZWDXN BXIZ BXWV FOBE HXIE HBJ JHIGE GHE YLOFFOM LXL
   */
  val spell: String =
  //"A"
  //"Z"
  //"AZ"
  //"S"
  //"AS"
    //"UMNE TALMAR R"
  //" E"
  //" T"
  //"GU"
  "Q A"
  //"GUZ M"
  //  "NA"
    //"MINAS"
  //"E T"
  //"Magic Unicorn".toUpperCase
  println(
           s"spellOutPhrase $spell:${
             spellOutPhrase(
                             phrase = spell
                             ,
                             spell = "",
                             //previousLetter = 'R',
                             currentRune = LeftRune,
                             leftIndex = 0,//M
                             middleIndex = 0,//E
                             rightIndex = 0,//A
                             alphabet = magicAlphabet
                           )
           }"
         )

}
