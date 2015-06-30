package testCodeOfTheRings

import org.scalatest.FunSuite

/**
 * Created by Alex on 30.06.15.
 */
class CodeOfTheRingsSuit extends FunSuite {

  import CodeOfTheRings.RuneForest._

  //import CodeOfTheRings.RuneForest.spellPhrase

  import CodeOfTheRings.Player._

  //ignore
  test(
        "1: spelling with 'spellOutPhrase' must match the spell"
      ) {
          val spell: String =
            "MINAS"
          //assert(Set.empty.size == 0)
          assume(
                  spellPhrase(
                               spellOutPhrase(
                                               phrase =
                                                 spell,
                                               //spell = "",
                                               //previousLetter = 'R',
                                               //currentRune = LeftRune,
                                               //leftIndex = 13,//M
                                               //middleIndex = 5,//E
                                               //rightIndex = 1,//A
                                               alphabet =
                                                 CodeOfTheRings.Player.magicAlphabet
                                             )
                             ) === "MINAS",
                  "must be = 'MINAS'"
                )
        }
  //ignore
  test(
        "2: spelling with 'encodePhrase' must match the spell"
      ) {
          val spell: String =
            "MINAS"
          //assert(Set.empty.size == 0)
          assume(
                  spellPhrase(
                               encodePhrase(
                                             magicPhrase =
                                               spell,
                                             symbolsFrequencyMap = Map('A'-> 1)
                                           )
                             ) === "MINAS",
                  "must be = 'MINAS'"
                )
        }

}
