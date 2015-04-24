package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

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
							/*or .tail ?*/
							references = references ),
						references )
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
