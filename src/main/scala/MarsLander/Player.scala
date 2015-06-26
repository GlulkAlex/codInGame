package MarsLander

/*
TODO
The program:
In fact
the problems concerns
the landing phase for "Mars Lander",
the landing ship which contains
the Opportunity rover.
Mars Lander is guided by a program, and
right now
the failure rate for landing on the NASA simulator is unacceptable.

Built as a game,
the simulator puts Mars Lander on
a limited zone of Mars sky.
The zone is
7000m wide and
3000m high.
The ship can
get into the `zone` at
a variable `location` and
with a variable `speed` and
`tilt angle`.

Your mission is to
write a new artificial intelligence program that
will enable Mars Lander to
land safely on Mars
without crashing.
The program will have to
go through a series of
increasingly complex simulator tests.

Every second,
depending on
the current flight parameters
(location, speed, fuel ...),
the program must
provide the new desired
`tilt`,
`angle` and
`thrust power` of Mars Lander.

`Angle` goes from -90 degrees to 90 degrees.
`Thrust power` goes from 0 to 4.

The game simulates
a free fall without atmosphere.
Gravity on Mars is 3.711 m/s?.
For a `thrust power` of X,
a `push force` equivalent to X m/s? is generated and
X liters of `fuel` are consumed.
As such,
a `thrust power` of 4 in
an almost vertical position is
needed to compensate for the gravity on Mars.

For a landing to be successful,
the ship must:
>land on `flat ground`
>land in a `vertical position` (`tilt angle` = 0 degrees)
>`vertical speed` must be limited ( ? 40m/s in absolute value)
>`horizontal speed` must be limited ( ? 20m/s in absolute value)

For each test,
there is
a unique area of `flat ground` on the surface of Mars
which is
at least 1000 meters wide.
 */

/*
INITILIZATION INPUT:
Line 1 : the number N of points used to
draw the surface of Mars.
Next N lines:
a couple of integers X Y
providing the coordinates of a ground point.
By linking all the points together in a sequential fashion,
you form the surface of Mars which is
composed of several segments.
For the first point,
X = 0 and
for the last point,
X = 6999
INPUT FOR ONE GAME TURN:
Line 1 :
A single line with 7 integers: X Y HS VS F R P
X,Y are the coordinates of Mars Lander (in meters).
HS and VS are
the `horizontal` and
`vertical speed` of `Mars Lander` (in m/s).
These can be negative
depending on the direction of `Mars Lander`.
F is the remaining quantity of `fuel` in liters.
When there is no more fuel,
the `power` of thrusters falls to zero.
R is the `angle of rotation` of `Mars Lander` expressed in degrees.
P is the thrust power of the landing ship.
OUTPUT FOR ONE GAME TURN:
A single line with 2 integers: R P:
R is 
the desired `rotation angle` for `Mars Lander`. 
Please note that 
for each turn
the actual value of the angle is limited to
the value of the previous turn +/- 15 degrees.
P is
the desired `thrust power`.
0 = off.
4 = maximum power.
Please note that
for each turn
the value of the actual power is limited to
the value of
the previous turn +/- 1.

CONSTRAINTS:
2 <= N < 30
0 <= X < 7000
0 <= Y < 3000
-500 < HS, VS < 500
0 <= F <= 2000
-90 <= R <= 90
0 <= P <= 4
Response time per turn <= 100ms

EXAMPLE :
`Mars Lander` starts with
a `vertical speed` of 'zero',
a `horizontal speed` of -50m/s (going left) and
is `tilted` fully to the left (90 degrees).
Initialization input (out of the infinite loop)
No output expected
6	(N) Surface made of 6 points
(X Y)
0 1500
1000 2000
2000 500	(X Y) Start of flat ground
3500 500	(X Y) End of flat ground
5000 1500
6999 1000
(flat ground -> where Y the same)
Input for turn 1
(X, Y, HS`horizontal speed`, VS`vertical speed`, F`fuel`, R`rotation angle`, P`thrust power`)
5000 2500 -50 0 1000 90 0
Output for turn 1
(R P)
-45 4
Requested rotation to the right, maximum thrust power
Input for turn 2
(X Y HS VS F R P)
4950 2498 -51 -3 999 75 1
Tilt angle changed
only by 15 degrees and
thrust power only by 1
Output for turn 2
(R P)
-45 4
Same request as previous turn
Input for turn 3
(X Y HS VS F R P)
4898 2493 -53 -6 997 60 2
Output for turn 3
(R P)
-45 4
Same request as previous turn
and so on until
`Mars Lander` crashes or lands...
 */

/**
 * Created by Alex on 21.06.15.
 */
object Player extends App {
  // the number of points
  // used to draw the surface of Mars.
  val n =
    6
  //scala.io.StdIn.readInt

  /*solution helper starts*/
  /*testData*/
  val testData1 =
    """
      |0 1500
      |1000 2000
      |2000 500
      |3500 500
      |5000 1500
      |6999 1000
    """.stripMargin
  var surfaceCoordinates =
    List.empty[PlanarCoordinates]
  /*solution helper ends*/

  // land_x: X coordinate of a surface point. (0 to 6999)
  // land_y: Y coordinate of a surface point.
  // By linking all the points together in a sequential fashion,
  // you form the surface of Mars.
  for (i <- /*0*/ n until n) {

    val Array(land_x, land_y) =
      for (
        i <- scala.io.StdIn.readLine split " "
      ) yield i.toInt

    /*solution helper starts*/
    surfaceCoordinates =
      //PlanarCoordinates(land_x, land_y) +: surfaceCoordinates
      /*not prepend, but append, or screwed order then sort needed*/
      //PlanarCoordinates(land_x, land_y) +: surfaceCoordinates
      surfaceCoordinates :+ PlanarCoordinates(land_x, land_y)
    /*solution helper ends*/
  }

  /*helpers start*/
  var newRotAng = 91
  //out of range
  var newPwThrust = -1 //out of range
  /*helpers end*/

  // game loop
  while (true == false) {
    // hs: the horizontal speed (in m/s), can be negative.
    // vs: the vertical speed (in m/s), can be negative.
    // f: the quantity of remaining fuel in liters.
    // r: the rotation angle in degrees (-90 to 90).
    // p: the thrust power (0 to 4).
    val Array(x, y, hs, vs, f, r, p) = for (i <- scala.io.StdIn.readLine split " ") yield i.toInt

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    // R P. R is the desired rotation angle.
    // P is the desired thrust power.
    println("-20 3")

    /*solution helper starts*/
    val landerAction =
      landerNextAction(
                        //getLanderParams(/*landerParams1*/)
                        LanderParams(
                                      x: Int,
                                      y: Int,
                                      hs: Int,
                                      vs: Int,
                                      f: Int,
                                      r: Int,
                                      p: Int
                                    )
                      )

    println(s"${ landerAction.rotAngle } ${ landerAction.powerThrust }")

    if (newRotAng == 91 && newPwThrust == -1
    /*vs == 0 &&
    hs == 50 &&
    r == 90*/
    ) {
      //first turn, more freedom to set new action
      /*defining position relative to flat place*/
      if (
        (
        (((x + vs) > flatGround.landGroundXStart ||
          x > flatGround.landGroundXStart)) &&
        (((x + vs) < flatGround.landGroundXEnd ||
          x < flatGround.landGroundXEnd))
        )
      ) {
        /*above flat landing*/
        /*rotation angle must be 0 -> vertical*/
        newRotAng = 0
        /*free fall ?*/
        newPwThrust = 0
      } else if (((x + vs) < flatGround.landGroundXStart ||
                  x < flatGround.landGroundXStart)) {
        /*need to move right*/
        newRotAng = -45
        /*create some 'hs'*/
        newPwThrust = 3
      } else if (((x + vs) > flatGround.landGroundXEnd) ||
                 (x > flatGround.landGroundXEnd)) {
        /*need to move left*/
        newRotAng = 45
        /*create some 'hs'*/
        newPwThrust = 3
      }


    } else {
      //restricted by previous action -> +/-15 & +/-1
      /*defining position relative to flat place*/
      if (
        (
        (((x + vs) > flatGround.landGroundXStart ||
          x > flatGround.landGroundXStart)) &&
        (((x + vs) < flatGround.landGroundXEnd ||
          x < flatGround.landGroundXEnd))
        )
      ) {
        /*above flat landing*/
        /*rotation angle must be 0 -> vertical*/
        if (newRotAng == 0) {
          /*all ok, do nothing*/
        } else if (newRotAng >= 15) {
          newRotAng =
            turn(
                  turnDirection = LeftTurn,
                  currentAngle = r
                )
        } else if (newRotAng <= -15) {
          newRotAng =
            turn(
                  turnDirection = RightTurn,
                  currentAngle = r
                )
        } else if (
          (
          (newRotAng < 15) ||
          (newRotAng > (-15))
          ) && (newRotAng != 0)
        ) {
          newRotAng = 0
        }
        /*free fall ?*/
      } else if (((x + vs) < flatGround.landGroundXStart ||
                  x < flatGround.landGroundXStart)) {
        /*need to move right*/
        if (newRotAng <= 0) {
          newRotAng =
            turn(
                  turnDirection = RightTurn,
                  currentAngle = r
                )
        }
        /*create some 'hs'*/
      } else if (((x + vs) > flatGround.landGroundXEnd) ||
                 (x > flatGround.landGroundXEnd)) {
        /*need to move left*/
        if (newRotAng >= 0) {
          newRotAng =
            turn(
                  turnDirection = LeftTurn,
                  currentAngle = r
                )
        }
        /*create some 'hs'*/
      }
      /*set thrust, adjust speed*/
      /*
      if no thrust -4 m/s to 'vs' every turn
       each thrust point reduce momentum by 1 m/s
       at full thrust power 'vs' just remains the same
      * */
      if (
      /*fall to fast*/
        vs < -17 &&
        /*reaching the ceiling*/
        (y + vs) < 3000 &&
        newPwThrust < 4
      ) {
        /*change thrust*/
        newPwThrust += 1
      } else if (newPwThrust > 1) {
        /*change thrust*/
        newPwThrust -= 1
      }
    }

    println(s"${ newRotAng } ${ newPwThrust }")

    /*solution helper ends*/
  }

  /*helper starts*/
  val landerParams1 = "5000 2500 -50 0 1000 90 0"

  case class PlanarCoordinates(x: Int, y: Int)

  val testCoordinates1: /*Seq*/ List[PlanarCoordinates] =
  //for (line <- testData1) /*yield*/ {
    (for (line <- testData1.lines
          if (line.nonEmpty && !line(0).isWhitespace)) yield {
      val (x, y) = line.span(p => p != ' ')
      /*println(
               s"x:$x,y:$y"+
               s",line(0):${line(0)},line(0).isControl=?:${line(0).isControl}" +
               s",,line(0).isWhitespace=?:${line(0).isWhitespace}" +
               s",line.charAt(0):${line.charAt(0)},line.charAt(0)=?'\\n':${line.charAt(0)=="\n"}" +
               s",line head:${line.head}=?''${line.head==""}")*/
      /*return value*/
      new PlanarCoordinates(x.toInt, y.stripPrefix(" ").toInt)

      /*line.toString match {
        case " " => println("Space")
        case "\n" => println("New Line")
        case _ => print(line)
      }*/
    })
      /*by default to stream*/
      //.toSeq
      .toList

  /*input Array(land_x, land_y)*/
  /*output landGroundXStart,landGroundXEnd, landGroundY)*/
  case class FlatGroundParams(
                               landGroundXStart: Int,
                               landGroundXEnd: Int,
                               landGroundY: Int,
                               landGroundXCenter: Option[Int]
                               )

  /*
  assume that
  to link all the points together in a sequential fashion
  flat ground coordinates must be also sequential
  */
  def findFlatGround(
                      coordinates: List[PlanarCoordinates],
                      flatStart: Option[Int] = None,
                      flatEnd: Option[Int] = None,
                      flatY: Option[Int] = None
                      ): FlatGroundParams =
  {
    if (coordinates.isEmpty /*|| flatEnd.nonEmpty*/ ) {
      /*basic step*/
      FlatGroundParams(
                        flatStart.getOrElse(-1),
                        flatEnd.getOrElse(-1),
                        flatY.getOrElse(-1),
                        if (flatStart.nonEmpty && flatEnd.nonEmpty) {
                          Some((flatStart.get + flatEnd.get) / 2)
                        } else {
                          None
                        }
                      )
    } else {
      /*recursion*/
      if (flatStart.isEmpty) {
        /*initial / first step*/
        findFlatGround(
                        coordinates.tail,
                        flatStart = Some(coordinates.head.x),
                        flatY = Some(coordinates.head.y)
                      )
      } else {
        if (flatY.get == coordinates.head.y) {
          /*all done return & exit*/
          findFlatGround(
                          Nil,
                          flatStart,
                          flatEnd = Some(coordinates.head.x),
                          flatY
                        )
        } else {
          /*new leader*/
          findFlatGround(
                          coordinates.tail,
                          flatStart = Some(coordinates.head.x),
                          flatY = Some(coordinates.head.y)
                        )
        }
      }
    }
  }

  case class LanderParams(
                           x: Int,
                           y: Int,
                           horizontSpeed: Int,
                           vertSpeed: Int,
                           fuel: Int,
                           rotAngle: Int,
                           powerThrust: Int
                           )

  case class LanderAction(
                           rotAngle: Int,
                           powerThrust: Int
                           )

  val flatGround =
    findFlatGround(testCoordinates1)

  trait TurnDirection

  case object LeftTurn extends TurnDirection

  case object RightTurn extends TurnDirection

  /*angle -> range from (-90) to (+90)*/
  def turn(
            turnDirection: TurnDirection,
            currentAngle: Int
            ): Int =
  {
    turnDirection match {
      case LeftTurn => {
        /*+*/
        if (currentAngle >= 0) {
          if ((currentAngle + 15) > 90) {
            90
          } else {
            currentAngle + 15
          }
        } else {
          /*-90 + 15 = -75*/
          /*-10 + 15 = 5*/
          currentAngle + 15
        }
      }
      case RightTurn => {
        /*-*/
        if (currentAngle < 0) {
          /*-90 - 15 = -105*/
          if ((currentAngle - 15) < (-90)) {
            -90
          } else {
            currentAngle - 15
          }
        } else {
          /*90 - 15 = 75*/
          /*10 - 15 = -5*/
          currentAngle - 15
        }
      }
    }
  }

  def landerNextAction(landerParams: LanderParams): LanderAction = {
    var landerAction =
      LanderAction(
                    landerParams.rotAngle: Int,
                    landerParams.powerThrust: Int
                  )

    /*first turn -> special parameters allowed*/
    val firstTurn: Boolean =
      (
      landerParams.vertSpeed == 0 &&
      landerParams.horizontSpeed == 50 &&
      landerParams.rotAngle == 90
      )

    /*defining position above flat place*/
    if (
      (
      ((landerParams.x + landerParams.horizontSpeed) >= ((flatGround.landGroundXStart + flatGround.landGroundXEnd) / 2)) &&
      (landerParams.x < flatGround.landGroundXEnd)
      ) ||
      (
      ((landerParams.x + landerParams.horizontSpeed) <= (flatGround.landGroundXStart + flatGround.landGroundXEnd) / 2) &&
      (landerParams.x > flatGround.landGroundXStart))
    ) {
      /*TODO*/
      /*tilt must be vertical */
      landerAction = LanderAction(
                                   0,
                                   landerParams.powerThrust: Int
                                 )
    } else if (landerParams.x > flatGround.landGroundXEnd) {
      /*go left, rotAngle > 0*/
      if (landerAction.rotAngle > 0) {
        /*keep current angle ?*/
        /*calculate possible arrival at 'flatGround' ?*/
        /*'horizontSpeed' must be > 0*/
      } else {
        if (firstTurn) {
          /*any value*/
          landerAction = LanderAction(
                                       45,
                                       landerParams.powerThrust: Int
                                     )
        } else {
          /*change with 15 step*/
          landerAction = LanderAction(
                                       landerParams.rotAngle + 15,
                                       landerParams.powerThrust: Int
                                     )
        }
      }
    } else if (landerParams.x < flatGround.landGroundXStart) {
      /*go right, rotAngle < 0*/
      if (landerAction.rotAngle < 0) {
        /*calculate possible arrival at 'flatGround' ?*/
        /*keep current angle*/
      } else {
        landerAction = LanderAction(
                                     -45,
                                     landerParams.powerThrust: Int
                                   )
      }
    } else {
      /*half of the Lander will be smashed*/
    }

    /*evaluating descend speed*/
    /*must be 'reasonable' & vertical not too big over time*/
    /*check for 'flatGround.landGroundY'*/
    if (landerParams.horizontSpeed > 40) {
      /*change tilt*/
      /*to closer to vertical*/
    }
    /*
    Landing in progress...
      X=2500m, Y=227m, HSpeed=0m/s VSpeed=-130m/s
      Fuel=500l, Angle=15 degree, Power=0 (0.0m/s2)
     */
    if (
      landerParams.vertSpeed > 20 &&
      /*reaching the ceiling*/
      landerParams.vertSpeed + landerParams.y < 3000 &&
      landerParams.powerThrust > 1
    ) {
      /*change thrust*/
      landerAction = LanderAction(
                                   landerAction.rotAngle,
                                   landerParams.powerThrust - 1
                                 )
    }
    /*when thrust '4' applied 'vertSpeed' stay the same as previous*/
    if (
      landerParams.vertSpeed < -20 &&
      /*reaching the ceiling*/
      landerParams.vertSpeed + landerParams.y < 3000 &&
      landerParams.powerThrust < 4
    ) {
      /*change thrust*/
      landerAction = LanderAction(
                                   landerAction.rotAngle,
                                   landerParams.powerThrust + 1
                                 )
    }
    if (landerParams.y + landerParams.vertSpeed <= flatGround.landGroundY) {
      /*we are about to crash / land*/
      landerAction = LanderAction(
                                   0,
                                   landerAction.powerThrust
                                 )
    }

    /*return value*/
    landerAction
  }

  def getLanderParams(landerParams: String): LanderParams = {
    val Array(
    x: Int,
    y: Int,
    horizontSpeed: Int,
    vertSpeed: Int,
    fuel: Int,
    rotAngle: Int,
    powerThrust: Int
             ) =
      for (elem <- landerParams.split(" ")) yield elem.toInt

    /*return value*/
    LanderParams(
                  x: Int,
                  y: Int,
                  horizontSpeed: Int,
                  vertSpeed: Int,
                  fuel: Int,
                  rotAngle: Int,
                  powerThrust: Int
                )
  }

  val landerAction = landerNextAction(getLanderParams(landerParams1))
  /*helper ends*/

  /*unit test*/
  //testData1.linesWithSeparators
  /*println(s"'0 1500' gives:${ "0 1500".split(" ").mkString(",") }")*/
  println(s"testCoordinates1:${ testCoordinates1 }")
  println(s"findFlatGround:${ findFlatGround(testCoordinates1) }")

  /*println(s"90 + 15 must be 90:${ (90 + 15) - ((90 + 15) % 90) }")
  println(s"((-90 - 15) % 90):=${ ((-90 - 15) % 90) }")
  println(s"(-90) - 15 must be (-90):${ (-90 - 15) - ((-90 - 15) % 90) }")
  println(s"(-80) - 15 must be (-90):${ (-80 - 15) - ((-80 - 15) % 90) }")
  println(s"(-10) + 15 must be (5):${ (-10 + 15) - ((-10 + 15) % 90) }")*/
  println(s"flatGround middle / center is:${ ((flatGround.landGroundXStart + flatGround.landGroundXEnd) / 2) }")
  println(s"flatGround middle / center is:${ flatGround.landGroundXCenter.get }")

  println(s"${ landerAction.rotAngle } ${ landerAction.powerThrust }")

}
