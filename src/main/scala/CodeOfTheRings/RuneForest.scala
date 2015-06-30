package CodeOfTheRings

/*
The `forest` contains 30 `zones`.
The last `zone` is
connected to the first `zone`,
effectively creating a `looping area`.
Each `zone` contains
a `magic stone`
upon which is
inscribed a `rune`
Every `rune` is
represented by
a `letter` of the alphabet (A-Z) or
an `empty space`.
All `runes` start out as a `space`.
a less-than '<' or
greater-than '>' sign.
to move through `zones`:
to move through the `letters`:
a plus '+' or
minus '-' character.
The `letter` after 'Z' is 'space'.
The `letter` after 'space' is 'A'.
'.' `trigger` a `rune`.
This will
add the displayed `letter`
to the spelling phrase
 */

/**
 * Created by Alex on 29.06.15.
 */
object RuneForest /*extends App*/ {

  /*
  `forest` has cycled zones '0' to '29' &
  pointer to `active` `zone` [Byte]
  `zone` has `runeStone`
  `runeStone` has cycled `letters` ' ' + 'A to Z' &
  pointer to `active` `letter` [Byte]

   */
  case class RuneStone(var isActive: Boolean, var letter: Byte)

  val magicAlphabet: IndexedSeq[Char] =
    ' ' +: ('A' to 'Z')
  val runeForestArray: Array[Byte] = new Array[Byte](magicAlphabet.length)
  /*'runeForestArray' index pointer*/
  var activeZone: Byte = 0
  //val zone
  var runeForest: Map[Byte, RuneStone] /*or 'scala.collection.mutable.'*/ =
    (0 to 29)
      .map(i => i.toByte -> RuneStone(false, 0))
      .toMap

  /*set new active `zone` / 'RuneStone'*/
  def traverseZones(
                     direction: Char
                     ): Byte /*Array[Byte]*/ =
  {
    if (runeForestArray.isEmpty) {
      /*return value*/
      //runeForestArray
      -1
    } else {
      /*side effect*/
      activeZone =
        if (direction == '>') {
          ((activeZone + 1) % runeForestArray.length).toByte
        } else /* if (direction == '<')*/ {
          ((activeZone - 1 + runeForestArray.length) % runeForestArray.length).toByte
        }
      /*return value*/
      activeZone
    }
  }

  /*set new active `zone` / 'RuneStone'*/
  def traverseForest(
                      direction: Char,
                      forest: Map[Byte, RuneStone]
                      ): Map[Byte, RuneStone] =
  {
    if (forest.isEmpty) {
      forest
    } else {
      forest
        .find(_._2.isActive) match {
        case None =>
          /*return value*/
          Map.empty[Byte, RuneStone]
        case Some((key, /*value*/ RuneStone(_, letter))) => {
          /*if (direction == '>') {
          } else if (direction == '<') {

          } else {
            /*something unexpected*/
          }*/
          /*return value*/
          forest
            .updated(key, RuneStone(false, letter))
            .updated(
              if (direction == '>') {
                ((key + 1) % forest.size).toByte
              } else {
                ((key - 1 + forest.size) % forest.size).toByte
              }
              ,
              RuneStone(true, letter)
                    )
        }
      }
      /*val (activeZone, activeRuneStone): (Byte, RuneStone) =
       //val activeZone =
         forest
       //.values
       //.find((r)=>r.isActive)
           .find(_._2.isActive)
       .getOrElse(Map.empty[Byte, RuneStone])*/
      //Map(activeZone->activeRuneStone)
      //forest
    }
  }

  def setRuneLetter(
                     direction: Char
                     ): Byte =
  {
    if (magicAlphabet.isEmpty) {
      -1
    } else {
      if (direction == '+') {
        ((runeForestArray(activeZone) + 1) % magicAlphabet.length).toByte
      } else /*if (direction == '-')*/ {
        ((runeForestArray(activeZone) - 1 + magicAlphabet.length) % magicAlphabet.length).toByte
      }
    }
  }

  def setLetter(
                 direction: Char,
                 letterIndex: Byte,
                 alphabet: IndexedSeq[Char]
                 ): Byte =
  {
    if (alphabet.isEmpty) {
      letterIndex
    } else {
      if (direction == '+') {
        ((letterIndex + 1) % alphabet.size).toByte
      } else {
        ((letterIndex - 1 + alphabet.size) % alphabet.size).toByte
      }
    }
  }

  /*active 'runeStone' 'key' needed*/
  def getLetter(
                 letterIndex: Byte,
                 alphabet: IndexedSeq[Char]
                 ): Char /*String*/ =
  {
    if (alphabet.isEmpty) {
      "".toCharArray.head
    } else {
      alphabet(letterIndex)
    }
  }

  def activeZoneWithRuneStone(
                               forest: Map[Byte, RuneStone]
                               ): (Byte, RuneStone) =
  {
    forest
      .find(_._2.isActive) match {
      case None =>
        /*return value*/
        (-1, RuneStone(false, -1))
      case Some((key, RuneStone(_, letter))) => (key, RuneStone(true, letter))
    }
  }

  def spellPhrase(
                   controls: String,
                   spell: String = ""
                   ): String =
  {
    if (controls.isEmpty) {
      spell
    } else {
      val newSpell: String =
        if (controls.head == '.'
        ) {
          spell + magicAlphabet(runeForestArray(activeZone))
        } else {
          spell
        }

      if (
        controls.head == '>' ||
        controls.head == '<'
      ) {
        /*side effect*/
        traverseZones(
                       direction = controls.head
                     )
      } else if (
        controls.head == '+' ||
        controls.head == '-'
      ) {
        /*side effect*/
        runeForestArray(activeZone) =
        setRuneLetter(
                       direction = controls.head
                     )
      } else {
        /*loop cpntrols '[,]' ?*/
      }

      /*return value*/
      spellPhrase(
                   controls = controls.tail,
                   spell = newSpell
                 )
    }
  }

  def spellIt(
               forest: Map[Byte, RuneStone],
               alphabet: IndexedSeq[Char],
               controls: String,
               spell: String = ""
               ): String =
  {
    if (controls.isEmpty) {
      spell
    } else {
      val newForest: Map[Byte, RuneStone] =
        if (
          controls.head == '>' ||
          controls.head == '<'
        ) {
          /*return value*/
          traverseForest(
                          direction = controls.head,
                          forest = forest
                        )
        } else if (
          controls.head == '+' ||
          controls.head == '-'
        ) {
          val (zoneKey, runeValue): (Byte, RuneStone) =
            activeZoneWithRuneStone(
                                     forest = forest
                                   )
          /*return value*/
          forest
            .updated(
              zoneKey,
              RuneStone(
                         runeValue.isActive, //must be 'true'
                         setLetter(
                                    direction = controls.head,
                                    letterIndex = runeValue.letter,
                                    alphabet = alphabet
                                  )
                       )
                    )
        } else {
          forest
        }
      val newSpell: String =
        if (controls.head == '.'
        ) {
          spell + getLetter(
                             letterIndex =
                               activeZoneWithRuneStone(
                                                        forest = forest
                                                      )
                                 ._2.letter,
                             alphabet = alphabet
                           )
        } else {
          spell
        }

      /*return value*/
      spellIt(
               forest = newForest,
               alphabet = alphabet,
               controls = controls.tail,
               spell = newSpell
             )
    }
  }

  /*initialization*/
  runeForestArray
    .map(_ => 0.toByte)

  runeForest =
    runeForest.updated(0, RuneStone(true, 0))
}

object MainUnitTest extends App {
  /*unit test*/
  import RuneForest._

  val controls: String =
    "+.-.+.-.+."
    //"+.>.<.<.>."
    //"+.>...<."
  /*println(s"runeForest: ${ runeForest.filter(_._2.isActive).mkString("|") }")
  println(
           s"traverseForest: ${
             traverseForest(direction = '>', forest = runeForest)
               .filter(_._2.isActive)
               .mkString("|")
           }"
         )
  println(
           s"traverseForest: ${
             traverseForest(direction = '<', forest = runeForest)
               .filter(_._2.isActive)
               .mkString("|")
           }"
         )*/
  /*println(
           s"activeZone : ${
             activeZone
           }"
         )
  println(
           s"runeForestArray : ${
             runeForestArray.mkString("[","|","]")
           }"
         )
  println(
  s"setRuneLetter('-') : ${
    setRuneLetter('-')
  }"
         )
  println(
  s"setRuneLetter('+') : ${
    setRuneLetter('+')
  }"
         )*/
  println(
           s"spellIt $controls: ${
             spellPhrase(
                      controls=controls,
                      spell = ""
             )
           }"
         )

}
