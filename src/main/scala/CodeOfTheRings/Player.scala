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
the `shortest output`
across all validator test cases
at the end of the 24 hours
will be the winners.
 */
/*
TODO
The program:
The aim of this game is to
help Bilbo escape a `forest` by
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
Bilbo move around in the `forest`,
interacting with the different `stones`(runes).

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
a `magic stone`
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
object Player /*extends App*/ {
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
  >>? if sequence of one letter has length greater then '6' then
     may be implement loop controls '[<.>-]' ?
  */
  /*
  new strategy:
   [-3]     -2      -1     [0]       1       2     [3]
    [0]      1       2     [3]       4       5     [6]      [7]
  [space][Letter][Letter][space][Letter][Letter][space][activeRune]
  max `space` length = "<.>" or ">.<" = +3
  max switch to closest letter index = ">>>>" or "<<<<" = +4
  if `spell` consist only one `letter` use 'space0'
   */
  /*content must change effected by encoding to mirror forest state*/
  val runeFrame: Array[Byte] =
  //new Array[Byte](7)
    Array().padTo(len = 8, elem = 0.toByte)

  /*'runeFrame' index pointer*/
  //var activeRune: Byte =
  //0 for forest
  //2 //for 'runeFrame' center ? or '2' or '4' ?
  //val symbolsFrequencyMap =
  def reSetRuneFrame(/*runeFrame: Array[Byte]*/): Array[Byte] /*Unit*/ = {
    /*side effect*/
    /*runeFrame
      /*'map' return new collection not change existing*/
      .map(_ => 0 /*.toByte*/)*/
    for (i <- runeFrame.indices) {
      runeFrame(i) = 0
    }
    //activeRune = 2
    runeFrame(7) = 2
    /*return value*/
    runeFrame
  }

  def getPlusPath(oldIndex: Byte, newIndex: Byte): Byte =
    if (oldIndex > newIndex) {
      (27 - oldIndex + newIndex)
        .toByte
    } else if (oldIndex < newIndex) {
      (newIndex - oldIndex)
        .toByte
    } else {
      0
    }

  def getMinusPath(oldIndex: Byte, newIndex: Byte): Byte =
    if (oldIndex > newIndex) {
      (oldIndex - newIndex)
        .toByte
    } else if (oldIndex < newIndex) {
      (oldIndex + 27 - newIndex)
        .toByte
    } else {
      0
    }

  def getPrefix(
                 /*zone index*/
                 closestIndex: Byte,
                 /*active zone index*/
                 activeRuneZone: Byte
                 ): String =
  {
    /*List(1, 2)|List(1, 4)|List(1, 5)|List(2, 4)|List(2, 5)|List(4, 5)*/
    if (closestIndex == activeRuneZone) {
      /*return value*/
      ""
    } else if (closestIndex < activeRuneZone) {
      /*return value*/
      ">" * (activeRuneZone - closestIndex)
    } else /*if (closestIndex > currentActiveRuneSymbolIndex)*/ {
      /*return value*/
      "<" * (closestIndex - activeRuneZone)
    } /*else if (closestIndex == 1 && currentActiveRuneSymbolIndex == 2) {
      /*return value*/
      ">"//(2-1)*">"
    } else if (closestIndex == 2 && currentActiveRuneSymbolIndex == 1) {
      /*return value*/
      "<"
    } else if (closestIndex == 1 && currentActiveRuneSymbolIndex == 4) {
      /*return value*/
      ">>>"
    } else if (closestIndex == 4 && currentActiveRuneSymbolIndex == 1) {
      /*return value*/
      "<<<"
    } else if (closestIndex == 4 && currentActiveRuneSymbolIndex == 2) {
      /*return value*/
      "<<"
    } else if (closestIndex == 2 && currentActiveRuneSymbolIndex == 4) {
      /*return value*/
      ">>"
    } else if (closestIndex == 2 && currentActiveRuneSymbolIndex == 5) {
      /*return value*/
      ">>>"
    } else if (closestIndex == 5 && currentActiveRuneSymbolIndex == 2) {
      /*return value*/
      "<<<"
    } else if (closestIndex == 5 && currentActiveRuneSymbolIndex == 1) {
      /*return value*/
      "<<<<"
    } else if (closestIndex == 1 && currentActiveRuneSymbolIndex == 5) {
      /*return value*/
      ">>>>"//(5-1)*">"
    } else if (closestIndex == 4 && currentActiveRuneSymbolIndex == 5) {
      /*return value*/
      ">"
    } else /*if (closestIndex == 5 && currentActiveRuneSymbolIndex == 4)*/ {
      /*return value*/
      "<"
    }*/
  }

  /*careful with [Byte] because value may be out of Range from '-128 to 127'*/
  /*test shows that up to 14 + '.' length encoding possible for one letter spell*/
  def encodeSymbol(
                    spellLetter: Char,
                    symbolsFrequencyMap: Map[Char, Int] = Map.empty[Char, Int]
                    ): /*Char*/ String =
  {
    var newControl: String = ""
    var activeRune = runeFrame(7)

    val currentActiveRuneSymbolIndex: Byte =
      runeFrame(activeRune)
    val alphabetLength: Byte =
      27
    val thresholdLength: Byte =
      (27 / 2).toByte
    //magicAlphabet.length
    val newLetterIndex: Byte =
      magicAlphabet
        .indexOf(spellLetter)
        .toByte
    var distanceDifference: Byte =
      scala.math
        .abs(newLetterIndex - currentActiveRuneSymbolIndex)
        .toByte
    var distanceMod: Byte =
      ((newLetterIndex - currentActiveRuneSymbolIndex) % alphabetLength)
        .toByte
    /*val plusPath: Byte =
      if (currentActiveRuneSymbolIndex > newLetterIndex) {
        (alphabetLength - currentActiveRuneSymbolIndex + newLetterIndex)
          .toByte
      } else if (currentActiveRuneSymbolIndex < newLetterIndex) {
        (newLetterIndex - currentActiveRuneSymbolIndex)
          .toByte
      } else {
        0
      }
    val minusPath: Byte =
      if (currentActiveRuneSymbolIndex > newLetterIndex) {
        (currentActiveRuneSymbolIndex - newLetterIndex)
          .toByte
      } else if (currentActiveRuneSymbolIndex < newLetterIndex) {
        (currentActiveRuneSymbolIndex + alphabetLength - newLetterIndex)
          .toByte
      } else {
        0
      }*/

    if (symbolsFrequencyMap.size == 1) {
      /*special test case*/
      /*one symbol only, using initial 'activeRune' & no else*/
      /*side effect*/
      runeFrame(activeRune) = newLetterIndex

      if (currentActiveRuneSymbolIndex == newLetterIndex) {
        /*default 'space'*/
        /*return value*/
        "."
      } else {
        /*not a 'space', not same letter*/
        val plusPath: Byte =
          getPlusPath(currentActiveRuneSymbolIndex, newLetterIndex)
        val minusPath: Byte =
          getMinusPath(currentActiveRuneSymbolIndex, newLetterIndex)

        if (
          plusPath > minusPath
        ) {
          "-" * minusPath + "."
        } else if (
          plusPath < minusPath
        ) {
          "+" * plusPath + "."
        } else {
          "."
        }
      }
    } else {
      /*general case*/
      /*
      TODO
      check & fix it
      because pick not optimal up to '26' length path
       */
      val frameOptions: List[(Byte, Byte, Byte, Byte, Byte)] =
        for (i <- /*Set*/ List(1, 2, 4, 5)) yield (
          i.toByte,
          getMinusPath(runeFrame(i), newLetterIndex),
          getPlusPath(runeFrame(i), newLetterIndex),
          getMinusPath(runeFrame(i), newLetterIndex)
            .min(getPlusPath(runeFrame(i), newLetterIndex)),
          scala.math.abs(activeRune - i).toByte
          )
      val minPathFilter =
        frameOptions
          .min(
            Ordering.by[(Byte, Byte, Byte, Byte, Byte), Byte](_._4)
              )

      val (closestRuneIndex, closestMinusPath, closestPlusPath, _, _): (Byte, Byte, Byte, Byte, Byte) =
      /*(
        /*?set is unordered? but exist 'SortedSet' & 'TreeSet'*/
      for (i <- Set(1, 2, 4, 5)) yield (
        i.toByte,
        getMinusPath(runeFrame(i), newLetterIndex),
        getPlusPath(runeFrame(i), newLetterIndex),
        getMinusPath(runeFrame(i), newLetterIndex)
          .min(getPlusPath(runeFrame(i), newLetterIndex)),
        scala.math.abs(activeRune - i).toByte
        )
      )*/
      /*if shortest distance is same for at least two index then pick closest to `activeRune`*/
        frameOptions
          //.minBy(x=>x)
          .min(
            //Ordering.Byte
            // sort by the 4th element, then 5th
            Ordering[(Byte, Byte)]
              .on((x: (Byte, Byte, Byte, Byte, Byte)) => (x._4, x._5))
              )

      /*'runeFrame' state must confirm / concord newly created 'controls'*/
      if (
        closestRuneIndex == activeRune &&
        (closestMinusPath == 0 ||
         closestPlusPath == 0)) {
        /*same symbol at current zone*/
        /*same 'runeFrame' symbol state*/
        /*return value*/
        "."
      } else if (
        closestRuneIndex == activeRune &&
        newLetterIndex > 0 &&
        (closestMinusPath > 0 &&
         closestPlusPath > 0)) {
        /*side effect*/
        /*'runeFrame' state update*/
        /*activeRune = closestRuneIndex
        runeFrame(7) = activeRune*/
        runeFrame(activeRune) = newLetterIndex

        /*changing in place*/
        if (closestMinusPath > closestPlusPath) {
          /*return value*/
          "+" * closestPlusPath + "."
        } else {
          /*return value*/
          "-" * closestMinusPath + "."
        }
      } else if (
        closestRuneIndex != activeRune &&
        newLetterIndex > 0 &&
        (closestMinusPath == 0 ||
         closestPlusPath == 0)) {
        /*same symbol in the neighbor zone*/
        /*[1][2]_[4][5]*/
        /*.combinations(2)*/
        /*List(1, 2)|List(1, 4)|List(1, 5)|List(2, 4)|List(2, 5)|List(4, 5)*/

        newControl =
          getPrefix(
                     closestRuneIndex: Byte,
                     activeRune: Byte
                   ) + "."

        /*side effect*/
        /*'runeFrame' state update*/
        activeRune = closestRuneIndex
        runeFrame(7) = activeRune
        //runeFrame(activeRune) = newLetterIndex

        /*return value*/
        newControl
      } else if (
        closestRuneIndex != activeRune &&
        newLetterIndex > 0 &&
        (closestMinusPath > 0 &&
         closestPlusPath > 0)) {
        /*must change symbol in the neighbor zone*/
        val control: String =
          getPrefix(
                     closestRuneIndex: Byte,
                     activeRune: Byte
                   ) +
          (if (closestMinusPath > closestPlusPath) {
            "+" * closestPlusPath
          } else {
            "-" * closestMinusPath
          }) + "."

        /*side effect*/
        /*'runeFrame' state update*/
        activeRune = closestRuneIndex
        runeFrame(7) = activeRune
        runeFrame(activeRune) = newLetterIndex

        /*return value*/
        control
      } else if (
        newLetterIndex == 0 &&
        (closestMinusPath >= 3 ||
         closestPlusPath >= 3)
      ) {
        /*case for 'space' '<.>' or '>.<' length '3' max*/
        /*'runeFrame' stays unchanged*/
        if (closestRuneIndex == 2 || closestRuneIndex == 5) {
          /*return value*/
          ">.<"
        } else /*if (closestRuneIndex == 1 || closestRuneIndex == 4)*/ {
          /*return value*/
          "<.>"
        }
      } else /*if (
        newLetterIndex == 0 &&
        (closestMinusPath < 3 ||
         closestPlusPath < 3)
      )*/ {
        /*must change symbol to 'space' somewhere*/
        val control: String =
          getPrefix(
                     closestRuneIndex: Byte,
                     activeRune: Byte
                   ) +
          (if (closestMinusPath > closestPlusPath) {
            "+" * closestPlusPath
          } else {
            "-" * closestMinusPath
          }) + "."

        /*side effect*/
        /*'runeFrame' state update*/
        activeRune = closestRuneIndex
        runeFrame(7) = activeRune
        runeFrame(activeRune) = newLetterIndex

        /*return value*/
        control
      }
      //""
    }
  }

  def encodePhrase(
                    magicPhrase: String,
                    controls: String = "",
                    symbolsFrequencyMap: Map[Char, Int] = Map.empty[Char, Int]
                    ): String =
  {
    if (magicPhrase.isEmpty) {
      /*return value*/
      controls
    } else {
      /*recursion*/
      encodePhrase(
                    /*it must converge*/
                    magicPhrase = magicPhrase.tail,
                    controls =
                      controls +
                      encodeSymbol(
                                    magicPhrase.head,
                                    symbolsFrequencyMap
                                  ),
                    /*same, not changing, just for reference*/
                    symbolsFrequencyMap = symbolsFrequencyMap
                  )
    }
  }

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
                      currentRune: Rune = MiddleRune,
                      leftIndex: Int = 0,
                      middleIndex: Int = 0,
                      rightIndex: Int = 0,
                      alphabet: IndexedSeq[Char]
                      ): String =
  {
    if (phrase.isEmpty) {
      spell
    } else {
      /*or hardcode this as '27'*/
      val alphabetLength = alphabet.length
      val newLetterIndex =
        alphabet.indexOf(phrase.head)
      var newCurrentRune: Rune =
        currentRune
      var newLeftIndex = leftIndex
      var newMiddleIndex = middleIndex
      var newRightIndex = rightIndex

      /*if 'spaceUsed' then here defined actual rune position*/

      var controlPrefix: String = ""
      val newLetter: String =
        if (
          newLetterIndex != leftIndex &&
          newLetterIndex != middleIndex &&
          newLetterIndex != rightIndex
        ) {
          /*completely different new letter*/
          if (
            newLetterIndex == 0 &&
            /*'3' max for 'MiddleRune'*/
            (leftIndex.min(middleIndex).min(rightIndex) > 3 ||
             (alphabetLength - leftIndex.min(middleIndex).min(rightIndex)) > 3)
          ) {
            /*special 'space' if more close letters are not available*/

            /*side effect*/

            /*return value*/
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
              currentRune
            val closestIndex: Int = {
              /*difference may be same but indexes are different*/
              var stateIndex: Int =
                newMiddleIndex

              var difference: Int =
                scala.math.abs(newLetterIndex - stateIndex)
              /*side effect*/
              newCurrentRune =
                MiddleRune

              /*compare with neighbors*/
              if (
                difference > (scala.math.abs(newLetterIndex - newRightIndex) + 1) ||
                difference > (alphabetLength - newLetterIndex + newRightIndex + 1)
              ) {
                /*'Right' with '>' clother then 'Middle'*/
                difference =
                  scala.math.abs(newLetterIndex - newRightIndex)
                stateIndex =
                  newRightIndex

                newCurrentRune =
                  RightRune
              } else if (
                difference == scala.math.abs(newLetterIndex - newRightIndex)
              ) {
                /*at the moment 'newCurrentRune' = 'MiddleRune'*/
                if (
                  newCurrentRune == currentRune
                ) {
                  /*same, & stay put*/
                } else if (
                  currentRune == RightRune
                ) {
                  /*side effect*/
                  newCurrentRune =
                    RightRune

                  /*difference may be same but indexes are different*/
                  stateIndex =
                    newRightIndex
                } else {
                  /*same*/
                }
              }

              /*at this moment 'newCurrentRune' is 'MiddleRune' or 'RightRune'*/
              /*only 'LeftRune' left*/
              if (
                difference > (scala.math.abs(newLetterIndex - newLeftIndex) + 1) ||
                difference > (alphabetLength - newLetterIndex + newLeftIndex + 1)
              ) {
                /*'Left' with '<' clother then 'Middle' ? or with '<<' then 'Right'? */
                /*? useless reassign ?*/
                difference =
                  scala.math.abs(newLetterIndex - newLeftIndex)
                stateIndex =
                  newLeftIndex

                newCurrentRune =
                  LeftRune
              } else if (
                difference == scala.math.abs(newLetterIndex - newLeftIndex)
              ) {
                /*at this moment 'newCurrentRune' may be 'MiddleRune' or 'RightRune'*/
                if (
                  (newCurrentRune == RightRune ||
                   newCurrentRune == MiddleRune) &&
                  newCurrentRune == currentRune
                ) {
                  /*same*/
                } else if (
                  currentRune == LeftRune
                ) {
                  /*side effect*/
                  newCurrentRune =
                    LeftRune
                  /*return value*/
                  stateIndex =
                    newLeftIndex
                } else {
                  /*same*/
                }
              }

              /*side effect*/
              if (
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
              stateIndex
            }

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

            "."
          } else if (currentRune == RightRune && rightIndex == newLetterIndex) {
            /*same*/
            "."
          } else if (currentRune == LeftRune && leftIndex == newLetterIndex) {
            /*same*/
            "."
          } else if (currentRune == MiddleRune && leftIndex == newLetterIndex) {
            newCurrentRune = LeftRune
            "<."
          } else if (currentRune == MiddleRune && rightIndex == newLetterIndex) {
            newCurrentRune = RightRune
            ">."
          } else if (currentRune == RightRune && middleIndex == newLetterIndex) {
            newCurrentRune = MiddleRune
            "<."
          } else if (currentRune == LeftRune && middleIndex == newLetterIndex) {
            newCurrentRune = MiddleRune
            ">."
          } else if (currentRune == LeftRune && rightIndex == newLetterIndex) {
            newCurrentRune = RightRune
            ">>."
          } else /*if (currentRune == RightRune && leftIndex == newLetterIndex)*/ {
            newCurrentRune = LeftRune
            "<<."
          }
        }
      /*recursion*/
      spellOutPhrase(
                      /*make it converge*/
                      phrase = phrase.tail,
                      spell = spell + newLetter,
                      currentRune = newCurrentRune,
                      leftIndex = newLeftIndex,
                      middleIndex = newMiddleIndex,
                      rightIndex = newRightIndex,
                      alphabet
                    )
    }
  }

  /*initialization*/
  reSetRuneFrame()
}

object Main extends App {

  import Player._

  def showPath(
                fromIndex: Byte,
                pathLength: Byte
                ): String =
  {
    magicAlphabet
      .drop(fromIndex)
      .take(
        if (pathLength > (27 - fromIndex)) {
          27 - 0
        } else {
          pathLength
        }
           )
      .mkString("[", "", "]") +
    magicAlphabet
      .take(
        if (pathLength > (27 - fromIndex)) {
          pathLength % (27 - fromIndex)
        } else {
          0
        }
           )
      .mkString("[", "", "]")
  }

  val magicphrase: String =
    'A'.to('Z')
      .toString()
  /*scala.io.StdIn
    .readLine*/

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  println("+.>-.")
  /*Unit test*/
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
  println(s"activeRune:${ runeFrame(7) /*activeRune*/ }")
  reSetRuneFrame()
  println(s"runeFrame:${ runeFrame.mkString("[", "|", "]") }")

  /*
  The magic phrase is: MINAS
  +++++++++++++.-----------------------.+++++.--------------.---------.(wrong)
  +++++++++++++.+++++++++++++++++++++++.+++++.++++++++++++++.---------.(right)
  Failure: Bilbo spelled: MQVHZ
   */
  /*
  Long Spell (last 24 test)
  my best result: (was 2599)
  >>is 1520<<
  The magic phrase is:*/
  val longSpell: String =
    "THREE RINGS FOR THE ELVEN KINGS UNDER THE SKY " +
    "SEVEN FOR THE DWARF LORDS IN THEIR HALLS OF STONE " +
    "NINE FOR MORTAL MEN DOOMED TO DIE " +
    "ONE FOR THE DARK LORD ON HIS DARK THRONEIN THE LAND OF MORDOR " +
    "WHERE THE SHADOWS LIE " +
    "ONE RING TO RULE THEM ALL " +
    "ONE RING TO FIND THEM " +
    "ONE RING TO BRING THEM ALL AND IN THE DARKNESS " +
    "BIND THEM IN THE LAND OF MORDOR WHERE THE SHADOWS LIE"
  /*Failure: Bilbo has too many actions to perform (infinite loop?).
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
    "AZ"
  //"ZA"
  //"BY"
  //"YB"
  //"S"
  //"AS"
  //"UMNE TALMAR R"
  //" E"
  //" T"
  //"GU"
  //"Q A"//spellOutPhrase Q17 A:++++.<.>>>. 13-17=-4
  //  "ISTA" //spellOutPhrase I9STA:++++.++++++++++.+.>>. 13-9=4
  //"GUZ M"
  //  "NA"
  //  "MINAS"
  //"E T"
  //"Magic Unicorn".toUpperCase
  println(
           s"spellOutPhrase $spell:${
             spellOutPhrase(
                             phrase =
                               spell,
                             //spell = "",
                             //currentRune = LeftRune,
                             //leftIndex = 13, //M
                             //middleIndex = 5, //E
                             //rightIndex = 1, //A
                             alphabet = magicAlphabet
                           )
           }"
         )
  //reSetRuneFrame()
  //.compose
  //.andThen
  println(s"runeFrame:${ reSetRuneFrame().mkString("[", "|", "]") }")
  println(
           s"encodePhrase using 1 $spell:${
             encodePhrase(
                           magicPhrase =
                             spell,
                           symbolsFrequencyMap = Map('A' -> 1)
                         )
           }"
         )
  reSetRuneFrame()
  println(s"runeFrame:${ runeFrame.mkString("[", "|", "]") }")
  println(
           s"encodePhrase using all $spell:${
             encodePhrase(
                           magicPhrase =
                             spell,
                           symbolsFrequencyMap =
                             SpellLettersFrequency(spell)
                         )
           }"
         )
  reSetRuneFrame()
  println(s"runeFrame:${ runeFrame.mkString("[", "|", "]") }")
  println(
           s"encodeSymbol 'A':${
             encodeSymbol(
                           spellLetter =
                             'A',
                           symbolsFrequencyMap = Map('A' -> 1)
                         )
           }"
         )
  reSetRuneFrame()
  println(s"runeFrame:${ runeFrame.mkString("[", "|", "]") }")
  println(
           s"encodeSymbol 'M':${
             encodeSymbol(
                           spellLetter =
                             'M',
                           symbolsFrequencyMap = Map('A' -> 1)
                         )
           }"
         )
  reSetRuneFrame()
  println(s"runeFrame:${ runeFrame.mkString("[", "|", "]") }")
  println(
           s"encodeSymbol 'Z' with 1st method:${
             encodeSymbol(
                           spellLetter =
                             'Z',
                           symbolsFrequencyMap = Map('A' -> 1)
                         )
           }"
         )

  println(s"runeFrame:${ reSetRuneFrame().mkString("[", "|", "]") }")
  println(
           s"encodeSymbol 'Z' with newest method:${
             encodeSymbol(
                           spellLetter =
                             'Z',
                           symbolsFrequencyMap =
                             SpellLettersFrequency("AZ")
                         )
           }"
         )

  reSetRuneFrame()
  val encodePhraseLongSpell1 =
    encodePhrase(
                  magicPhrase =
                    longSpell,
                  symbolsFrequencyMap = Map('A' -> 1)
                )
  reSetRuneFrame()
  val encodePhraseLongSpell2 =
    encodePhrase(
                  magicPhrase =
                    longSpell,
                  symbolsFrequencyMap =
                    SpellLettersFrequency(longSpell)
                )
  val spellOutPhraseLongSpell =
    spellOutPhrase(
                    phrase =
                      longSpell,
                    alphabet = magicAlphabet
                  )

  /*println(
           s"encodePhrase $spell:${
             encodePhrase(
                           magicPhrase =
                             spell,
                           symbolsFrequencyMap = Map('A'-> 1)
                         )
           }"
         )*/
  println(
           s"longSpell :\n${
             longSpell
           }"
         )
  println(
           s"encodePhraseLongSpell1.length using 1st strategy= ${ encodePhraseLongSpell1.length }:\n${
              encodePhraseLongSpell1
           }"
         )
  println(
           s"encodePhraseLongSpell2.length using 3rd strategy= ${ encodePhraseLongSpell2.length }:\n${
             encodePhraseLongSpell2
           }"
         )
  println(
           s"${
             spellOutPhraseLongSpell
           }\nspellOutPhraseLongSpell.length using 2nd strategy= ${
             spellOutPhraseLongSpell.length }"
         )

  /*val pPath0to1 = getPlusPath(
                               oldIndex = 0 /*'space'*/ ,
                               newIndex = 1 //'A"
                             )
  val mPath0to1 = getMinusPath(
                               oldIndex = 0 /*'space'*/ ,
                               newIndex = 1 //'A"
                             )

  println(
           s"'getPlusPath' from '0' to '1'$pPath0to1:${
             showPath(
                       fromIndex=0,
                       pathLength=pPath0to1
                     )
           }"
         )
  println(
           s"'getMinusPath' from '0' to '1'$mPath0to1:${
             showPath(
                       fromIndex=0,
                       pathLength=mPath0to1
                     )
           }"
         )
  val pPath0to26 = getPlusPath(
                               oldIndex = 0 /*'space'*/ ,
                               newIndex = 26 //'A"
                             )
  val mPath0to26 = getMinusPath(
                                oldIndex = 0 /*'space'*/ ,
                                newIndex = 26 //'A"
                              )

  println(
           s"'getPlusPath' from '0' to '26'$pPath0to26:${
             showPath(
                       fromIndex=0,
                       pathLength=pPath0to26
                     )
           }"
         )
  println(
           s"'getMinusPath' from '0' to '26'$mPath0to26:${
             showPath(
                       fromIndex=0,
                       pathLength=mPath0to26
                     )
           }"
         )
  val pPath1to12 = getPlusPath(
                                oldIndex = 1 /*'space'*/ ,
                                newIndex = 12 //'A"
                              )
  val mPath1to12 = getMinusPath(
                                 oldIndex = 1 /*'space'*/ ,
                                 newIndex = 12 //'A"
                               )

  println(
           s"'getPlusPath' from '1' to '12'$pPath1to12:${
             showPath(
                       fromIndex=1,
                       pathLength=pPath1to12
                     )
           }"
         )
  println(
           s"'getMinusPath' from '1' to '12'$mPath1to12:${
             showPath(
                       fromIndex=1,
                       pathLength=mPath1to12
                     )
           }"
         )
  val pPath5to17 = getPlusPath(
                                oldIndex = 5 /*'space'*/ ,
                                newIndex = 17 //'A"
                              )
  val mPath5to17 = getMinusPath(
                                 oldIndex = 5 /*'space'*/ ,
                                 newIndex = 17 //'A"
                               )

  println(
           s"'getPlusPath' from '5' to '17'$pPath5to17:${
             showPath(
                       fromIndex=5,
                       pathLength=pPath5to17
                     )
           }"
         )
  println(
           s"'getMinusPath' from '5' to '17'$mPath5to17:${
             showPath(
                       fromIndex=5,
                       pathLength=mPath5to17
                     )
           }"
         )
  val pPath5to18 = getPlusPath(
                                oldIndex = 5 /*'space'*/ ,
                                newIndex = 18 //'A"
                              )
  val mPath5to18 = getMinusPath(
                                 oldIndex = 5 /*'space'*/ ,
                                 newIndex = 18 //'A"
                               )

  println(
           s"'getPlusPath' from '5' to '18'$pPath5to18:${
             showPath(
                       fromIndex=5,
                       pathLength=pPath5to18
                     )
           }"
         )
  println(
           s"'getMinusPath' from '5' to '18'$mPath5to18:${
             showPath(
                       fromIndex=5,
                       pathLength=mPath5to18
                     )
           }"
         )
  val pPath5to19 = getPlusPath(
                                oldIndex = 5 /*'space'*/ ,
                                newIndex = 19 //'A"
                              )
  val mPath5to19 = getMinusPath(
                                 oldIndex = 5 /*'space'*/ ,
                                 newIndex = 19 //'A"
                               )

  println(
           s"'getPlusPath' from '5' to '19'$pPath5to19:${
             showPath(
                       fromIndex=5,
                       pathLength=pPath5to19
                     )
           }"
         )
  println(
           s"'getMinusPath' from '5' to '19'$mPath5to19:${
             showPath(
                       fromIndex=5,
                       pathLength=mPath5to19
                     )
           }"
         )
  val pPath19to5 = getPlusPath(
                                oldIndex = 19 /*'space'*/ ,
                                newIndex = 5 //'A"
                              )
  val mPath19to5 = getMinusPath(
                                 oldIndex = 19 /*'space'*/ ,
                                 newIndex = 5 //'A"
                               )

  println(
           s"'getPlusPath' from '19' to '5'$pPath19to5:${
             showPath(
                       fromIndex=19,
                       pathLength=pPath19to5
                     )
           }"
         )
  println(
           s"'getMinusPath' from '5' to '19'$mPath19to5:${
             showPath(
                       fromIndex=19,
                       pathLength=mPath19to5
                     )
           }"
         )*/

  val testBeacon: Boolean = true
}
