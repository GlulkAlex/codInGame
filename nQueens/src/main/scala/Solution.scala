package nQueens

/** @author Alex
  */
object Solution {
  /*helper*/
  trait Direction
  case object N extends Direction
  case object E extends Direction
  case object S extends Direction
  case object W extends Direction
  case object NW extends Direction
  case object NE extends Direction
  case object SE extends Direction
  case object SW extends Direction
  /*helper*/
  case class traversalOpetations( xOp: Int => Int, yOp: Int => Int )

  /*
  Constraints:
  queens
  4<=N<=100
  input array
  1<=A(i)<=N for all i in range(1 to N)
  A(i)=A(j) <=> i=j
  (i.e.
  you can assume that the array contains some
  `permutation` of
  the integers from 1 to N,
  with no repetitions)
  ?so values in array unique?
	 */
  def maxThreats( a: Array[ Int ] ): Int = {
    /*Input:
			 * val a = Array(2,3,1)
			 * ? A.size <=> represent board dimensions (size * size)
			 * A(i) indicates that
			 * queen in row i, column A(i) 
			 * val queensPositions =
			 * Map(
			 * "row1"->"column2",
			 * "row2"->"column3",
			 * "row3"->"column1")
			 * Output:
			 *  'maxThreats return 1'
			 * */
    /*
			 * Threat:
			 * cases:
			 * row# match => 
			 *  1 for 1st match in checked direction
			 *  max possible 2 for each direction
			 * column# match => 
			 *  1 for 1st match in checked direction
			 *  max possible 2 for each direction
			 * diagonal# match => 
			 *  1 for 1st match in checked direction
			 *  max possible 2 for each direction
			 * */

    case class Queen(
      id: Int,
      /*? as `ID` ?*/
      row: Int,
      column: Int,
      /*for each of 8 direction*/
      var threats: Int )

    val quenns =
      for (
        //elem <-a 
        i <- 0 until a.size
      ) yield Queen( i, i, a( i ), 0 )

    val directions =
      Seq( N, NE, E, SE, S, SW, W, NW )
      /*has no direct access to element by index*/
      /*Set(N,NE,E,SE,S,SW,W,NW)
      directions(N)*/

      /*to pass relevant operation to
       * recursive function*/
      def traversalParam0: Direction => Int => Int = ???
      def traversalParam1( direction: Direction ): Int => Int = ???

      //case class traversalOpetations( xOp: Int => Int, yOp: Int => Int )
      /*or make a `Map`*/
      def traversalParam( direction: Direction ): traversalOpetations = {
        direction match {
          /*up*/
          case N => traversalOpetations(
            /*up to '0'*/
            ( xOp: Int ) => xOp - 1,
            ( yOp: Int ) => yOp )
          /*up & right*/
          case NE => traversalOpetations(
            ( xOp: Int ) => xOp - 1,
            ( yOp: Int ) => yOp + 1 )
          /*right*/
          case E => traversalOpetations(
            ( xOp: Int ) => xOp,
            ( yOp: Int ) => yOp + 1 )
          /*down & right*/
          case SE => traversalOpetations(
            /*down to board / dimension size*/
            ( xOp: Int ) => xOp + 1,
            ( yOp: Int ) => yOp + 1 )
          /*down */
          case S => traversalOpetations(
            ( xOp: Int ) => xOp + 1,
            ( yOp: Int ) => yOp )
          /*down & left*/
          case SW => traversalOpetations(
            ( xOp: Int ) => xOp + 1,
            /*back to '0'*/
            ( yOp: Int ) => yOp - 1 )
          /*left*/
          case W => traversalOpetations(
            ( xOp: Int ) => xOp,
            ( yOp: Int ) => yOp - 1 )
          /*up & left*/
          case NW => traversalOpetations(
            ( xOp: Int ) => xOp - 1,
            ( yOp: Int ) => yOp - 1 )
          /*unchanged*/
          case x => traversalOpetations(
            ( xOp: Int ) => xOp,
            ( yOp: Int ) => yOp )
        }
      }

      /*  check for 
       *  `restrictions` for next `index` value
       * if any `condition` is true then stop*/
      /*to invoke `base case`
       * case depend on direction ?*/
      def stopCondition(
        /*array / seq indexes
           * with starting base '0'*/
        xQ: Int,
        yQ: Int,
        boardSize: Int,
        direction: Direction ): Boolean = {
        traversalParam( direction )
          .xOp( xQ ) <= 0 ||
          traversalParam( direction )
          .yOp( yQ ) <= 0 ||
          traversalParam( direction )
          .xOp( xQ ) >= boardSize ||
          traversalParam( direction )
          .yOp( yQ ) >= boardSize ||
          boardSize == 0
      }

      /*one at a time
       * return
       * '0' or '1'*/
      def checkDirection(
        direction: Direction,
        /*current*/
        xQ: Int,
        yQ: Int,
        /*threads accumulator*/
        foundT: Int = 0,
        quennsTotal: Array[ Int ] ): Int = {
        /*base case*/
        /*must be pre-condition
							 * not post-condition*/
        /*all searched nothing left*/
        if ( stopCondition(
          /*row*/
          xQ,
          /*column*/
          yQ,
          boardSize = quennsTotal.size,
          direction
        ) ) {
        	println(s"[Debug]stopCondition("+
        			s"xQ:${xQ},yQ:${yQ},size:${quennsTotal.size},dir:${direction})")
          println(s"[Debug]return:${foundT}")
          /*return value*/
          foundT

          /*can go further*/
          /*recursive*/
          /*actual check*/
        } else {
          /*lookup for next position / step in track*/
          val nextIndices =
            traversalParam( direction )
          val nextRowIndex: Int =
            nextIndices.yOp( yQ )
          val nextColumnIndex: Int =
            nextIndices.yOp( yQ )
          val queenColumnInNextRow = quennsTotal( nextRowIndex )
          //foundT
          /*Q found*/
          if ( queenColumnInNextRow == ( nextColumnIndex + 1 ) ) {
        	  println(s"[Debug]Q found:${foundT}")
            /*return value*/
            1
            /*recursion to next*/
          } else {
        	  println(s"[Debug]recursion to next xQ:${nextRowIndex},yQ:${nextColumnIndex}")
            checkDirection(
              direction, //same
              xQ = nextRowIndex,
              yQ = nextColumnIndex,
              foundT, //same 
              quennsTotal //same
            )
          }
        }
      }

      /*  must return
 *  sum of threats
 *  for current queen
 * recursive
 * until 
 * run out of dimensions*/
      def checkDirections(
        directions: Seq[ Direction ],
        /*current*/
        xQ: Int,
        yQ: Int,
        /*threads accumulator*/
        foundT: Int = 0,
        quennsTotal: Array[ Int ] ): Int = {
        /*base case*/
        if ( quennsTotal.isEmpty ) {
          /*return value*/
          foundT
          /*recursive case*/
        } else {
          /*check 
			 * ? all around direction ?*/
          val queenThreats: Seq[ Int ] = for (
            //direction <-directions
            i <- 0 until directions.size
          ) yield checkDirection(
            direction = directions( i ),
            /*current*/
            xQ: Int,
            yQ: Int,
            /*threads accumulator*/
            foundT = 0,
            quennsTotal: Array[ Int ] )
          /*return value*/
          queenThreats.sum
        }
      }

      /*recursive
		 * until 
		 * run out of dimensions*/
      def check4DiagonalDirections(
        direction: Direction,
        /*current*/
        xQ: Int,
        yQ: Int,
        foundT: Int = 0,
        quennsTotal: IndexedSeq[ Queen ] ): Int = {
        /*base case*/
        if ( quennsTotal.isEmpty ) {
          foundT
          /*recursive case*/
        } else {
          direction match {
            case NW => {
              /*all searched nothing left*/
              if ( ( xQ - 1 ) <= 1 || ( yQ - 1 ) <= 1 ) {
                //if (xQ == quennsTotal.size || yQ == quennsTotal.size) {
                check4DiagonalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ -= 1 & yQ -= 1*/
                val potentialTreat =
                  /*sequence index zero based
									 * so, must not be below '0'*/
                  quennsTotal( xQ - 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ - 1 ) ) {
                  check4DiagonalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4DiagonalDirections(
                    direction, //same
                    xQ = xQ - 1,
                    yQ = yQ - 1,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
            case NE => {
              /*all searched nothing left*/
              if ( ( xQ - 1 ) <= 1 || yQ >= ( quennsTotal.size - 1 ) ) {
                //if (xQ == quennsTotal.size || yQ == quennsTotal.size) {
                check4DiagonalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ -= 1 & yQ += 1*/
                val potentialTreat =
                  quennsTotal( xQ - 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ + 1 ) ) {
                  check4DiagonalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4DiagonalDirections(
                    direction, //same
                    xQ = xQ - 1,
                    yQ = yQ + 1,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
            case SE => {
              /*all searched nothing left*/
              if ( ( xQ + 1 ) >= quennsTotal.size || ( yQ + 1 ) >= quennsTotal.size ) {
                check4DiagonalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ += 1 & yQ += 1*/
                val potentialTreat =
                  quennsTotal( xQ + 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ + 1 ) ) {
                  check4DiagonalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4DiagonalDirections(
                    direction, //same
                    xQ = xQ + 1,
                    yQ = yQ + 1,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
            case SW => {
              /*all searched nothing left*/
              if ( ( xQ + 1 ) >= quennsTotal.size || ( yQ - 1 ) <= 1 ) {
                check4DiagonalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ += 1 & yQ -= 1*/
                val potentialTreat =
                  quennsTotal( xQ + 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ - 1 ) ) {
                  check4DiagonalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4DiagonalDirections(
                    direction, //same
                    xQ = xQ + 1,
                    yQ = yQ - 1,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
          }
        }
      }

      /*recursive
		 * until 
		 * run out of dimensions*/ //Orthoganal
      def check4OrthoganalDirections(
        direction: Direction,
        /*current*/
        xQ: Int,
        yQ: Int,
        foundT: Int = 0,
        quennsTotal: IndexedSeq[ Queen ] ): Int = {
        /*base case*/
        if ( quennsTotal.isEmpty ) {
          foundT
          /*recursive case*/
        } else {
          direction match {
            case N => {
              /*all searched nothing left*/
              if ( ( xQ - 1 ) <= 1 ) {
                //if (xQ == quennsTotal.size || yQ == quennsTotal.size) {
                check4OrthoganalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ -= 1 & yQ -= 1*/
                val potentialTreat =
                  /*sequence index zero based
									 * so, must not be below '0'*/
                  quennsTotal( xQ - 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ - 1 ) ) {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ = xQ - 1,
                    yQ = yQ,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
            case E => {
              /*all searched nothing left*/
              if ( yQ >= ( quennsTotal.size - 1 ) || ( xQ - 1 ) <= 1 ) {
                //if (xQ == quennsTotal.size || yQ == quennsTotal.size) {
                check4OrthoganalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ -= 1 & yQ += 1*/
                val potentialTreat =
                  quennsTotal( xQ - 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ + 1 ) ) {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ = xQ,
                    yQ = yQ + 1,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
            case S => {
              /*all searched nothing left*/
              if ( ( xQ + 1 ) >= quennsTotal.size ) {
                check4OrthoganalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ += 1 & yQ += 1*/
                val potentialTreat =
                  quennsTotal( xQ + 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ + 1 ) ) {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ = xQ + 1,
                    yQ = yQ,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
            case W => {
              /*all searched nothing left*/
              if ( ( yQ - 1 ) <= 1 || ( xQ + 1 ) >= quennsTotal.size ) {
                check4OrthoganalDirections(
                  direction, //same
                  xQ: Int, //same
                  yQ: Int, //same
                  foundT, //same
                  quennsTotal = IndexedSeq.empty[ Queen ] )

                /*actual check*/
              } else {
                /*xQ += 1 & yQ -= 1*/
                val potentialTreat =
                  quennsTotal( xQ + 1 )
                val Queen( _, _, column, _ ) = potentialTreat
                /*Q found*/
                if ( column == ( yQ - 1 ) ) {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ: Int, //same
                    yQ: Int, //same
                    foundT = 1,
                    quennsTotal = IndexedSeq.empty[ Queen ] )
                  /*recursion to next*/
                } else {
                  check4OrthoganalDirections(
                    direction, //same
                    xQ = xQ,
                    yQ = yQ - 1,
                    foundT, //same 
                    quennsTotal //same
                  )
                }
              }
            }
          }
        }
      }

      def threadQueen(
        queensTotal: Array[ Int ] /*IndexedSeq[ Queen ]*/ ,
        queensReamain: Array[ Int ] /*IndexedSeq[ Queen ]*/ ,
        rowIndex: Int = 0,
        globalT: Int = 0 ): Int = {
        /*base case*/
        if ( queensReamain.isEmpty ) {
          globalT
          /*recursive case*/
        } else {
          /*check all diagonal directions
           * for current queen*/

          threadQueen(
            queensTotal, //same
            queensReamain = queensReamain.tail,
            globalT =
              globalT
                .max(
                  checkDirections(
                    directions: Seq[ Direction ],
                    /*current queen*/
                    xQ = rowIndex + 1,
                    yQ = queensTotal( rowIndex + 1 ),
                    /*threads accumulator*/
                    foundT = 0,
                    queensTotal: Array[ Int ] ) ) )
        }
      }

    /*return T
		 * as highest number of 
		 * threads to
		 * any position with Q on it
		 * (max of individual to each Q)
		 * may be from 0 (solved, no Q in danger) to 8
		 * but
		 * if only diagonals allowed then
		 * up to 4 max
		 * */
    threadQueen(
      queensTotal = a/*queens*/,
      queensReamain = a/*queens*/,
      /*localT =*/ 0,
      globalT = 0
    )
  }

  /*unit tesat*/
  def main( args: Array[ String ] ) {
    val traversalParam: Map[ Direction, traversalOpetations ] =
      Map(
        /*up*/
        N -> traversalOpetations(
          ( xOp: Int ) => xOp - 1,
          ( yOp: Int ) => yOp ),
        /*up & right*/
        NE -> traversalOpetations(
          ( xOp: Int ) => xOp - 1,
          ( yOp: Int ) => yOp + 1 ),
        /*right*/
        E -> traversalOpetations(
          ( xOp: Int ) => xOp,
          ( yOp: Int ) => yOp + 1 ),
        /*down & right*/
        SE -> traversalOpetations(
          ( xOp: Int ) => xOp + 1,
          ( yOp: Int ) => yOp + 1 ),
        /*down */
        S -> traversalOpetations(
          ( xOp: Int ) => xOp + 1,
          ( yOp: Int ) => yOp ),
        /*down & left*/
        SW -> traversalOpetations(
          ( xOp: Int ) => xOp + 1,
          ( yOp: Int ) => yOp - 1 ),
        /*left*/
        W -> traversalOpetations(
          ( xOp: Int ) => xOp,
          ( yOp: Int ) => yOp - 1 ),
        /*up & left*/
        NW -> traversalOpetations(
          ( xOp: Int ) => xOp - 1,
          ( yOp: Int ) => yOp - 1 )
      )

      /*  check for restrictions for next index value
       * if any condition is true then stop*/
      def stopCondition(
        /*array / seq indexes
           * with starting base '0'*/
        xQ: Int,
        yQ: Int,
        boardSize: Int,
        direction: Direction ): Boolean = {
        /*apply without get
         * so, no Option return*/
        traversalParam( direction )
          .xOp( xQ ) <= 1 ||
          traversalParam( direction )
          .yOp( yQ ) <= 1 ||
          traversalParam( direction )
          .xOp( xQ ) >= boardSize ||
          traversalParam( direction )
          .yOp( yQ ) >= boardSize ||
          boardSize == 0
      }

    trait Generator[ +T ] {
      self => // an alias for "this".
      /*abstract*/
      def generate: T

      def map[ S ]( f: T => S ): Generator[ S ] =
        new Generator[ S ] {
          def generate = f( self.generate )
        }

      def flatMap[ S ]( f: T => Generator[ S ] ): Generator[ S ] =
        new Generator[ S ] {
          def generate = f( self.generate ).generate
          /*or*/
          //*def generate = f( /*full name*/Generator1.generate ).generate
        }
    }

    /*Some instances:*/
    val integers = new Generator[ Int ] {
      val rand =
        //new java.util.Random
        new scala.util.Random

      def generate = rand.nextInt()
    }

      def intervalInt(
        lo: Int,
        hi: Int ): Generator[ Int ] =
        /*'+1' to include upper bound*/
        for { x <- integers } yield lo + {
          val choise = x % ( hi + 1 - lo )
          /*'abs'*/
          choise match {
            case v if v > 0 => v
            /*to return only positive*/
            case v if v < 0 => -v
            case _          => 0
          }
        }

      def showBoard( queens: Array[ Int ] ): Unit = {
        for {
          row <- 0 until queens.size
          column <- 0 until queens.size
        } {
          if ( ( column + 1 ) == ( queens( row ) ) ) {
            print( "Q" )
          } else {
            print( "X" )
          }
          if ( column == queens.size - 1 ) {
            print( "\n" )
          }
        }
      }
    //var fw = new PrintWriter(sys.env("OUTPUT_PATH"));
    var _a_size = 0;
    _a_size =
      //Console
      /*scala.io.StdIn
        .readInt*/
      intervalInt(
        lo = 2,
        hi = 9 )
        .generate

    val _a = new Array[ Int ]( _a_size )

    /*array generator*/
    for ( _a_i <- 0 until _a_size ) {
      var _a_item: Int =
        //Console
        /*scala.io.StdIn
          .readInt*/
        intervalInt(
          lo = 1,
          hi = _a_size )
          .generate

      _a( _a_i ) = _a_item
    }

    lazy val res = maxThreats( _a )
    //fw.
    println( s"maxThreats for current board:${res}" )

    //fw.close();
    println( _a.mkString( "{", ",", "}" ) )
    showBoard( queens = _a )
    val goNorth = traversalParam.get( N )
    val goNorthEast = traversalParam.get( NE )
    val goEast = traversalParam.get( E )
    val goSouthEast = traversalParam.get( SE )
    val goSouth = traversalParam.get( S )
    val goSouthWest = traversalParam.get( SW )
    val goWest = traversalParam.get( W )
    val goNorthWest = traversalParam.get( NW )

    println( s"goNorth from x2,y2 to x${goNorth.get.xOp( 2 )},y${goNorth.get.yOp( 2 )}" )
    println( s"goNorthEast from x2,y2 to x${goNorthEast.get.xOp( 2 )},y${goNorthEast.get.yOp( 2 )}" )
    println( s"goEast from x2,y2 to x${goEast.get.xOp( 2 )},y${goEast.get.yOp( 2 )}" )
    println( s"goSouthEast from x2,y2 to x${goSouthEast.get.xOp( 2 )},y${goSouthEast.get.yOp( 2 )}" )
    println( s"goSouth from x2,y2 to x${goSouth.get.xOp( 2 )},y${goSouth.get.yOp( 2 )}" )
    println( s"goSouthWest from x2,y2 to x${goSouthWest.get.xOp( 2 )},y${goSouthWest.get.yOp( 2 )}" )
    println( s"goWest from x2,y2 to x${goWest.get.xOp( 2 )},y${goWest.get.yOp( 2 )}" )
    println( s"goNorthWest from x2,y2 to x${goNorthWest.get.xOp( 2 )},y${goNorthWest.get.yOp( 2 )}" )
    println
    println( s"true && false:${true && false}" )
    println( s"true || false:${true || false}" )
    println( s"not (true && false):${!( true && false )}" )
    println( s"not (true || false):${!( true || false )}" )
    println( s"x:${
      traversalParam( NW )
        .xOp( 2 )
    }<=1:${
      traversalParam( NW )
        .xOp( 2 ) <= 1
    }" )
    println( s"y:${
      traversalParam( NW )
        .yOp( 2 )
    }<=1:${
      traversalParam( NW )
        .yOp( 2 ) <= 1
    }" )
    println( s"x:${
      traversalParam( NW )
        .xOp( 2 )
    }>=3:${
      traversalParam( NW )
        .xOp( 2 ) >= 3
    }" )
    println( s"y:${
      traversalParam( NW )
        .yOp( 2 )
    }>=3:${
      traversalParam( NW )
        .yOp( 2 ) >= 3
    }" )
    println( s"y:3==0:${
      3 == 0
    }" )
    println( s"Shall we stop goNorthWest from x2,y2 ${
      stopCondition(
        xQ = 2,
        yQ = 2,
        boardSize = 3,
        direction = NW )
    }" )
    println( s"Shall we stop goNorthWest from x3,y2 ${
      stopCondition(
        xQ = 3,
        yQ = 2,
        boardSize = 3,
        direction = NW )
    }" )
    println( s"Shall we stop goNorthWest from x2,y3 ${
      stopCondition(
        xQ = 2,
        yQ = 3,
        boardSize = 3,
        direction = NW )
    }" )
  }
}  
