package calculator

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
			/*val discriminant: Double = b() * b() - 4 * a() * c()
			
			new Signal( discriminant )*/
			val discriminant = Var( ( b() * b() ) - ( 4 * a() * c() ) ) 
			
			discriminant
  }

	/*
	use 'delta' to 
	compute 
	the set of roots of 
	the polynomial in 'computeSolutions'. 
	Recall that 
	there can be 
	>>0 (when 'delta' is negative), 
	>>1 or 
	>>2 roots to such a polynomial, and 
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
					/*no roots == empty set
					not some value like '0.0'*/
					Set(  ) 
				} else if (delta() == 0) {
					/*parenthesis matters so
					make it double */
					val root: Double =  ( -b() ) / ( 2 * a() )
					
					Set( root )  				
				} else {
					//& 2 signals ?
					/*-5.562827722895522 was not -0.1980358747915814 plus or minus 1.0E-5*/
					/*fixed*/
					//val deltaSQRT: Double =  Math.sqrt( delta() )
					/*must depend on other signals
					in order to change when they changed*/
					val root1: Double = ( ( -b() ) + Math.sqrt( delta() ) ) / ( 2 * a() )
					val root2: Double = ( ( -b() ) - Math.sqrt( delta() ) ) / ( 2 * a() )
					
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
