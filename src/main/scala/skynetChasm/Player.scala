package skynetChasm

/**
 * Created by glukalex on 19.06.15.
 */

import skynetChasm.Player.Bike

import math._
import scala.util._

/*
Task description:
The program:
The bike moves in a straight line (X axis) and can jump.
Each turn,
it moves forward a number of spaces equal to its speed.
For example,
if X = 1 and speed = 3,
X will be 4 at the end of the turn.
The bike can start with any speed, including being at a stop.
Our engineers have integrated into the virus an SSH tunnel protocol
making it possible to
send a few simple commands to the Moto-Terminators.
Upon connection,
you will receive
the lengths of the `road` before the `gap`,
the length of the `gap` and
the length of the `landing platform`.
After every communication,
you will receive
the current `speed` and `position` on the road of the motorbike.
The virus prototype accepts
four simple commands :
SPEED (+1), SLOW(-1), JUMP (with current speed), WAIT (same speed)
to respectively
accelerate, slow down, jump or keep going.

THE MISSION IS
A SUCCESS IF
THE BIKE COMES AT A STOP ON THE LANDING PLATFORM.

YOU FAIL:
>if the bike falls.
>if the bike did not get to the platform after 50 turns.
>This problem is quite simple.
  As quickly as possible,
  reach the ?minimum?(enough) speed to
  jump the gap and ?(slow down to stop after)?
  you’ll be sure to succeed all the validation tests -
  no matter your initial position.
 */
/*
Algorithm:
 TURN 1
`SPEED`
ACCELERATE TO HAVE ENOUGH MOMENTUM TO JUMP THE GAP.
TURN 2
`JUMP`
THE BIKE MOVED FORWARD 3 SPACES.
TURN 3
`SLOW`
STOP THE BIKE BY SLOWING DOWN.
---
KEEP SLOWING DOWN WITH SLOW UNTIL
THE MOTORBIKE COMES TO A COMPLETE STOP...
 */
/*
Example & restrictions:
INITIALIZATION INPUT:
Line 1: R the length of the road before the gap.
Line 2: G the length of the gap.
Line 3: L the length of the landing platform.
INPUT FOR ONE GAME TURN:
Line 1: S the motorbike's speed.
Line 2: X the position on the road of the motorbike.
OUTPUT FOR ONE GAME TURN:
A single line containing 'one' of 4 keywords:
SPEED SLOW JUMP WAIT.
CONSTRAINTS:
The initial position of the motorbike is always X=0?(more likely it is `1`).
0 ≤ S < 50
0 ≤ X < 100
1 ≤ R ≤ 100
1 ≤ G ≤ 100
1 ≤ L ≤ 100
Response time for one game turn ≤ 150ms
 */
/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
  /*helper*/
  val stopCriteria = false

  /*
  ? `true` to proceed or
  * `false` to continue ?
  * */
  def checkCondition(
                      currentPosX: Int,
                      currentSpeed: Int
                      ): Boolean = {
    if (
    /*fall in chasm*/
      (currentPosX > (r + g + l)) ||
        /*fall in gap*/
        ((currentPosX > r) && (currentPosX < (r + g))) ||
        /*stops in landing pad*/
        ((currentPosX > (r + g)) && (currentSpeed == 0)) ||
        /*stops at the right edge of landing pad*/
        (currentPosX == (r + g + l))
    ) {
      true
    } else {
      false
    }
  }

  trait BikeAction

  case object JUMP extends BikeAction

  case object WAIT extends BikeAction

  case object SLOW extends BikeAction

  case object SPEED extends BikeAction

  case class Bike(posX: Int, speed: Int, action: BikeAction)

  def bikeMoves(
                 //bike: Bike
                 currentPosX: Int,
                 currentSpeed: Int
                 ): Bike = {
    if (currentPosX < r) {
      /*for `jump` 's` must be >= 'g + 1'*/
      if ((currentPosX + currentSpeed) >= r + g) {
        //println("JUMP")
        Bike(currentPosX + currentSpeed, currentSpeed, JUMP)
      } else {
        if ((currentPosX + currentSpeed) == r || currentSpeed == (g + 1)) {
          //println("WAIT")
          Bike(currentPosX + currentSpeed, currentSpeed, WAIT)
        } else if ((currentPosX + currentSpeed) > r && (currentPosX + currentSpeed) < (r + g)) {
          //println("SLOW")
          Bike(currentPosX + currentSpeed - 1, currentSpeed - 1, SLOW)
        } else {
          //println("SPEED")
          Bike(currentPosX + currentSpeed + 1, currentSpeed + 1, SPEED)
        }
      }
    } else if (currentPosX == r) {
      //println("JUMP")
      Bike(currentPosX + currentSpeed, currentSpeed, JUMP)
    } else if (currentPosX >= r + g) {
      //println("SLOW")
      Bike(currentPosX + currentSpeed - 1, currentSpeed - 1, SLOW)
    } else {
      //println("WAIT")
      /*return value*/
      Bike(currentPosX + currentSpeed, currentSpeed, WAIT)
    }
    /*return value*/
    //Bike(bike.posX, bike.speed, bike.action)
  }

  /*
  at any given moment known values is:
    currentPosX & currentSpeed
    and
    calculated / estimated values:

  * */
  def nextTurn(
                currentPosX: Int,
                currentSpeed: Int /*,
                currentAction: BikeAction*/
                ): Unit = {

    if (checkCondition(currentPosX, currentSpeed)) {
      /*stop & exit*/
    } else {
      /*recursion => next*/
      val Bike(nextPosX, nextSpeed, nextAction) =
        bikeMoves(/*Bike(*/ currentPosX, currentSpeed /*, currentAction)*/)
      var expectedSpeed = currentSpeed

      //Console.RESET
      //Console.UNDERLINED
      //Console.GREEN_B
      (1 to r)
      .map(_ => print(s"${Console.RESET; Console.UNDERLINED; Console.GREEN_B}_"))
      //Console.RESET
      //Console.YELLOW_B
      (1 to g)
      .map(_ => print(s"${Console.RESET; Console.YELLOW_B} "))
      //Console.RESET
      //Console.UNDERLINED
      //Console.RED_B
      (1 to l)
      .map(_ => print(s"${Console.UNDERLINED; Console.BLUE_B}_"))
      print(s"${Console.RESET; Console.RED_B}Chasm\n")
      (1 until currentPosX)
      .map(_ => print(">"))
      /*currentAction*/ nextAction match {
        case JUMP  => {
          {
            {
              {
                {
                  //Console.RESET
                  //Console.GREEN
                  //Console.BOLD
                  print(s"${Console.RESET; Console.GREEN; Console.BOLD}${currentSpeed}J")
                  //Console.RESET
                }
              }
            }
          }
        }
        case WAIT  => {
          {
            {
              {
                {
                  /*Console.RESET
            Console.BLUE_B
            Console.BOLD
            print("W")*/
                  print(s"${Console.RESET; Console.BLUE_B; Console.BOLD}${currentSpeed}W")
                  //Console.RESET
                }
              }
            }
          }
        }
        case SLOW  => {
          {
            {
              {
                {
                  expectedSpeed -= 1
                  /*Console.RESET
            Console.BLUE
            Console.BOLD
            print("-")*/
                  print(s"${Console.RESET; Console.BLUE; Console.BOLD}${currentSpeed}-")
                  //Console.RESET
                }
              }
            }
          }
        }
        case SPEED => {
          {
            {
              {
                {
                  expectedSpeed += 1
                  /*Console.RESET
            Console.RED
            Console.BOLD
            print("+")*/
                  print(s"${Console.RESET; Console.RED; Console.BOLD}${currentSpeed}+")
                  //Console.RESET
                }
              }
            }
          }
        }
      }
      (currentPosX until currentPosX + expectedSpeed /*currentSpeed*/)
      .map(_ => print(" "))
      /*Console.RESET
      Console.YELLOW_B
      Console.BOLD*/
      print(s"${Console.RESET; Console.BOLD; Console.RED; Console.BLUE_B}${expectedSpeed}${Console.RESET}\n")
      //Console.RESET
      //println(s"${Console.GREEN}ramp${Console.UNDERLINED;}gap${Console.RESET}landing${Console.RED_B}Chasm")

      nextTurn(nextPosX, nextSpeed /*, nextAction*/)
    }
  }

  /*helper ends*/

  // the length of the road before the gap.
  //21
  val r = 13
  //21//scala.io.StdIn.readInt
  // the length of the gap.
  val g = 2
  //scala.io.StdIn.readInt
  // the length of the landing platform.
  val l = 7 //5//scala.io.StdIn.readInt

  // game loop
  while (stopCriteria /*true*/ ) {
    val s = 6 //scala.io.StdIn.readInt // the motorbike's speed.
    val x = 0 //scala.io.StdIn.readInt // the position on the road of the motorbike.

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    // A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.
    /*
    case x<r =>
    case x>r && x<r+g =>println(s"fail")
    case x==r =>println("JUMP")
    case x+s==r =>println("WAIT")
    case x+s>r && x<r+g =>println(s"fail")
    case x+s-1>r && x<r+g =>println(s"fail")
    case x+s+1>r && x<r+g =>println(s"fail")
    case x>r+g =>println("SLOW")
    case x>r+g+l =>println(s"fail")
    case x+(s-1)>r+g+l =>println(s"fail")
    */

    /*here is confusion between `0` start index & 'r,g,l' length*/
    /*ideally / greedy speed must be just enough to leap*/
    /*one possible option per turn*/
    if ((x + 1 + s) < r && s <= g) {
      /*make sense to speed up if it is still space for jump*/
      /*speed must be greater then gap*/
      println("SPEED")
    } else if ((x + 1) == r || (((x + 1) + s) >= (r + g) && ((x + 1) < r))) {
      /*only if bike before or on ramp edge*/
      /*for `jump` 's` must be >= 'g + 1'*/
      println("JUMP")
    } else if (
             ((x + 1) >= (r + g)) ||
               ((x + 1 + s) > r && ((x + 1 + s) <= (r + g))) ||
               (((x + 1 + s) < r) && (s > (g + 1)))
           ) {
      /*after jump*/
      println("SLOW")
    } else {
      /*speed optimal*/
      println("WAIT")
    }

  }

  //println(s"${Console.GREEN}ramp${Console.UNDERLINED; Console.GREEN_B}gap${Console.RESET}landing${Console
  // .RED_B}Chasm")
  /*action*/
  nextTurn(0, 6 /*, WAIT*/)
  //nextTurn(0, 6, SLOW)
  /*nextTurn(5, 5, SLOW)
  nextTurn(9, 4, WAIT)
  nextTurn(13, 4, JUMP)
  nextTurn(17, 4, SLOW)
  nextTurn(20, 3, SLOW)
  nextTurn(22, 2, SLOW)
  nextTurn(23, 1, SLOW)*/
}
