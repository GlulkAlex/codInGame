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
0 1500	(X Y)
1000 2000	(X Y)
2000 500	(X Y) Start of flat ground
3500 500	(X Y) End of flat ground
5000 1500	(X Y)
6999 1000	(X Y)
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
  // the number of points used to draw the surface of Mars.
  val n =
    scala.io.StdIn.readInt

  for(i <- 0 until n) {
    // land_x: X coordinate of a surface point. (0 to 6999)
    // land_y: Y coordinate of a surface point.
    // By linking all the points together in a sequential fashion,
    // you form the surface of Mars.
    val Array(land_x, land_y) =
      for(i <- scala.io.StdIn.readLine split " ") yield i.toInt
  }

  // game loop
  while(true) {
    // hs: the horizontal speed (in m/s), can be negative.
    // vs: the vertical speed (in m/s), can be negative.
    // f: the quantity of remaining fuel in liters.
    // r: the rotation angle in degrees (-90 to 90).
    // p: the thrust power (0 to 4).
    val Array(x, y, hs, vs, f, r, p) = for(i <- scala.io.StdIn.readLine split " ") yield i.toInt

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    // R P. R is the desired rotation angle.
    // P is the desired thrust power.
    println("-20 3")
  }
}
