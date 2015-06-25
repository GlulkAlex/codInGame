package descent

import math._
import scala.util._

/**
 * Created by glukalex on 19.06.15.
 */
class Player {

}

/*
description:
The program:
Your mission is
to program the cannons so that
they destroy the mountains before your starship collides with them.

There are 8 mountains.
The starship circles above all the mountains,
going first from left to right, then
from right to left,
and so on and so forth.
With each complete pass,
the starship descends one kilometer
as it is being drawn to the surface by an unknown force.

A complete pass is done in 8 game turns.

You can only fire once per pass located directly below the starship.
Firing on a mountain base will only destroy part of it and
it will sink a random number of kilometers.
 */
/*
Instructions:
INPUT FOR ONE GAME TURN:
Line 1:
2 integers: SX SY
SX is
the horizontal coordinate or
your starship (in kilometers).
It goes from 0
(left of the screen, above first mountain)
to 7 (right of the screen above last mountain)
SY is the current altitude of your ship (in kilometers).
It goes down from 10 (top of the screen)
to 1 (just above ground).
Next 8 lines:
One integer MH(Mountain's heights) per line.
It represents the height of one mountain,
from 9 to 0 (mountain destroyed).
Mountain's heights are provided from left to right.
OUTPUT FOR ONE GAME TURN:
A single line with either:
FIRE (ship is firing its phase cannons)
HOLD (ship is not firing)
CONSTRAINTS:
0 ≤ SX ≤ 7
0 < SY ≤ 10
0 ≤ MH ≤ 9
Response time per turn ≤ 100ms
 */
/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
  /*TODO
  solve puzzle
  DONE
  * */
  // game loop
  while (true) {
    /*helper*/
    val mountains: Array[Int] = new Array(9)
    /*helper end*/
    val Array(sx, sy) =
      for (i <- scala.io.StdIn.readLine split " ") yield i.toInt
    for (i <- 0 until 8) {
      // represents the height of one mountain, from 9 to 0. Mountain heights are provided from left to right.
      val mh: Int = /*readInt*/ scala.io.StdIn.readInt()
    }

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    // either:
    // FIRE (ship is firing its phase cannons) or
    // HOLD (ship is not firing).
    //if (mountains(sx)>=(sy-1) || mountains(sx) == mountains.max) {
    if (mountains(sx) > 0 && mountains(sx) == mountains.max) {
      println("FIRE")
    } else {
      println("HOLD")
    }
  }
}
