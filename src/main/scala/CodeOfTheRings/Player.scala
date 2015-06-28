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
  trait RunePosition

  case object Left

  case object Middle

  case object Right

  def spellOutPhrase(
                      phrase: String,
                      /*no commands*/
                      spell: String = "",
                      previousLetter: Char = ' ',
                      //isSpaceUsed: Boolean = false,
                      alphabet: IndexedSeq[Char]
                      ): String =
  {
    if (phrase.isEmpty) {
      spell
    } else {
      /*or hardcode this as '27'*/
      val alphabetLength = alphabet.length
      val previousLetterIndex =
        alphabet.indexOf(previousLetter)
      val newLetterIndex =
        alphabet.indexOf(phrase.head)
      var spaceUsed = false

      val spaceSymbol = ">.<"
      val newLetter: String =
        if (newLetterIndex != previousLetterIndex) {
          if (
            newLetterIndex == 0 &&
            (previousLetterIndex>2 || (alphabetLength - previousLetterIndex)>2)
          ) {
            /*'space'*/
            /*side effect*/
            spaceUsed = true
            /*return*/
              spaceSymbol
          } else {
            if (newLetterIndex > previousLetterIndex) {
              /*'T' char:20 > 'E' char:5*/
              /*'Z' char:26 > 'A' char:1*/
              if ((newLetterIndex - previousLetterIndex) > alphabetLength / 2) {
                /*20 - 5 = 15 > 13 forward*/
                /*5 + 27 - 20 = 13 backward*/
                /*26 - 1 = 25 > 13 forward*/
                /*27 - 26 + 1 = 2 backward*/
                /*move backward*/
                /*and through the 'space'*/
                ("-" * (alphabetLength - newLetterIndex + previousLetterIndex)) + "."
                /*if (newLetterIndex > alphabetLength / 2) {
                  ("-" * (alphabetLength - previousLetterIndex + newLetterIndex )) + "."
                } else {
                  ("-" * (alphabetLength - newLetterIndex + previousLetterIndex)) + "."
                }*/
              } else {
                /*move forward*/
                ("+" * ((newLetterIndex - previousLetterIndex))) + "."
              }
            } else /*if (newLetterIndex < previousLetterIndex)*/ {
              /*'I' char:9 < 'M' char:13*/
              if ((previousLetterIndex - newLetterIndex) > alphabetLength / 2) {
                /*move forward*/
                ("+" * (alphabetLength - (previousLetterIndex - newLetterIndex))) + "."
              } else {
                /*13-9=5<13*/
                /*move backward*/
                ("-" * ((previousLetterIndex - newLetterIndex))) + "."
              }
            }
          }
        } else {
          /*same letter*/
          "."
        }
      /*recursion*/
      spellOutPhrase(
                      /*make it converge*/
                      phrase = phrase.tail,
                      spell = spell + newLetter,
                      previousLetter =
                        if (spaceUsed) {
                          previousLetter
                        } else {
                          phrase.head
                        },
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
  println(
           s"spellOutPhrase 'Magic Unicorn':${
             spellOutPhrase(
                             phrase =
                               "E T"
                             //"Magic Unicorn".toUpperCase
                             ,
                             spell = "",
                             previousLetter = ' ',
                             alphabet = magicAlphabet
                           )
           }"
         )

}
