package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

/*
Your overall score for this assignment is 6.00 out of 10.00

The code you submitted did not pass all of our tests: your submission achieved a score of
6.00 out of 10.00 in our tests.

In order to find bugs in your code, we advise to perform the following steps:
 - Take a close look at the test output that you can find below: it should point you to
   the part of your code that has bugs.
 - Run the tests that we provide with the handout on your code.
 - The tests we provide do not test your code in depth: they are very incomplete. In order
   to test more aspects of your code, write your own unit tests.
 - Take another very careful look at the assignment description. Try to find out if you
   misunderstood parts of it. While reading through the assignment, write more tests.

Below you can find a short feedback for every individual test that failed.

======== LOG OF FAILED TESTS ========

Your solution achieved a testing score of 12 out of 20.
Below you can see a short feedback for every test that failed, indicating the reason
for the test failure and how many points you lost for each individual test.

An internal error happened while testing your code. Please send your entire feedback message
to one of the teaching assistants. success 12, fail 3, total 20

[Test Description] computeSolutions
[Observed Error] -5.562827743418748 was not -0.1980358747915814 plus or minus 1.0E-5
[Lost Points] 3

Unique identifier of this grade is 5786137a-2e0a-4c37-9c10-3c382de70958. This identifier will uniquely identify your assignment throughout the grading system.
*/
/*
compile to JavaScript with
'webUI/fastOptJS' and
refresh your browser
*/
/** If
  * all 'ten' values are
  * highlighted every time you modify something, then
  * something is wrong with
  * the way you construct your signals.
  */
object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
			(name, expr) <- namedExpressions
    } yield
      name -> Var( eval( expr(), namedExpressions ) )
  }

  def eval(
		expr: Expr, 
		references: Map[String, Signal[Expr]]): Double = {
			
			expr match {
				/*base case*/
				case Literal( v ) => v
				case Ref( r ) =>
					eval(
						getReferenceExpr(
							name = r,
							references = references ),
						/*very subtle, precise & sensitive adjustment*/
						references - r )
				case Plus( a: Expr, b: Expr ) =>
					eval( a, references ) +
						eval( b, references )
				case Minus( a: Expr, b: Expr ) =>
					eval( a, references ) -
						eval( b, references )
				case Times( a: Expr, b: Expr ) =>
					eval( a, references ) *
						eval( b, references )
				case Divide( a, b ) =>
					eval( a, references ) /
						eval( b, references )
				/*if all fails*/
				case _ => Double.NaN
			}
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(
																name: String,
																references: Map[String, Signal[Expr]]) = {
    references
			.get(name)
				.fold[Expr] {
											Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
