package calculator

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
After compiling 
with 'webUI/fastOptJS' and 
refreshing your browser, 
you can 
play with the 'root solver'.*/
object Polynomial {
		
	/*
	the intermediary discriminant, 
	which we call 'delta':
		delta = b^2 - 4 * a * c*/
  def computeDelta(
		a: Signal[Double], 
		b: Signal[Double],
		c: Signal[Double]): Signal[Double] = {
			//ids = List("polyroota", "polyrootb", "polyrootc")
			//id="polyrootdelta"
			/*val discriminant: Double = b() * b() - 4 * a() * c()
			
			new Signal( discriminant )*/
			val discriminant = Var( b() * b() - 4 * a() * c() )
			
			discriminant
  }

	/*
	use 'delta' to 
	compute 
	the set of roots of 
	the polynomial in 'computeSolutions'. 
	Recall that 
	there can be 
	0 (when 'delta' is negative), 
	1 or 
	2 roots to such a polynomial, and 
	that can be 
	computed with the formula:
		( -b +- sqrt('delta') ) / ( 2 * a )
		*/
  def computeSolutions(
		a: Signal[Double], 
		b: Signal[Double],
		c: Signal[Double], 
		delta: Signal[Double]): Signal[Set[Double]] = {
			/*works but 
			halts & freeze on / when input started with '-'
			if '-' added later then OK
			must be some input validation 
			'Double.NaN' in 'CalculatorUI.doubleValueOfInput'*/ 
			Var( 
				if ( delta() < 0 ) {
					Set( 0.0 ) 
				} else if (delta() == 0) {
					val root: Double =  -b() / 2 * a()
					
					Set( root )  				
				} else {
					//& 2 signals ?
					val root1: Double = ( -b() + Math.sqrt( delta() )) / 2 * a()
					val root2: Double = ( -b() - Math.sqrt( delta() )) / 2 * a()
					
					Set( root1, root2 ) 				
				}
			)
			
			/*val root = Var( Set( 0.0 ) ) 
		
			root() = delta() match {
				case d if d < 0 => Set( 0.0 )
				case d if d == 0 => {
					val root: Double =  -b() / 2 * a()
					
					Set( root )
				}
				case _ => {
					val root1: Double = ( -b() + Math.sqrt( delta() )) / 2 * a()
					val root2: Double = ( -b() - Math.sqrt( delta() )) / 2 * a()
					
					Set( root1, root2 )
				}
			}
			
			root*/
  }
}
