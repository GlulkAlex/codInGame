package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
	
	/*test sample for 'Map'*/
	/*property("updateKey1") = forAll { (k: Int, v: Int, m: Map[ Int, Int ]) =>
    val m1 = m.updated(k, v)
		
    m1(k) == v
  }

	lazy val genMap: Gen[ Map[ Int, Int ] ] = for {
    k <- arbitrary[ Int ]
    v <- arbitrary[ Int ]
    m <- oneOf( 
			//from import Gen._
			const( Map.empty[ Int, Int ] ), 
			genMap )
  } yield m.updated( k, v )
	
  implicit lazy val arbMap: Arbitrary[Map[ Int, Int ]] = Arbitrary(genMap)*/
	
	property("maxOfThree1") = forAll { ( x: Int, y: Int, z: Int ) =>
    val h = insert(z, insert(y, insert(x, empty)))
		
    findMin(deleteMin(deleteMin(h))) == Math.max(Math.max(x, y),z)
  }  		
	
	property("maxOfTwo1") = forAll { ( x: Int, y: Int ) =>
    val h = insert(y, insert(x, empty))
		
    findMin(deleteMin(h)) == Math.max(x, y)
  }  	
	
	/*tie ?*/
  property( "gen1" ) = forAll { ( h: H ) =>
    val m = if ( isEmpty( h ) ) 0 else findMin( h )
    findMin( insert( m, h ) ) == m
  }
	
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a  
	}
	
	property("minOfTwo1") = forAll { ( x: Int, y: Int ) =>
    val h = insert(y, insert(x, empty))
		
    findMin(h) == Math.min(x, y)
  }  	
	
	property("insertAndDelete1") = forAll { a: Int =>
    val h = insert(a, empty)
		
    deleteMin(h) == empty
  }  
	
	property("sortedSequence1") = forAll { ( h: H ) =>
		def heapLoop(
									heapRemains: H, 
									growingSeq: Seq [Int]): Seq [Int] = 
										//if (heapRemains == Nil) {
										//if (heapRemains == empty) {
										//if (heapRemains.isEmpty) {
										if (isEmpty(heapRemains)) {
											growingSeq
										} else {
											val min = findMin(heapRemains)
											
											heapLoop(
																deleteMin(heapRemains), 
																//min +: growingSeq)
																growingSeq :+ min )
										}
										
    val sequence = heapLoop(
															h, 
															Seq.empty[Int])
    val sortedSeq = sequence.sorted
		
		sequence == sortedSeq
  }
	
	property("melding1") = forAll { ( h1: H, h2: H ) =>
    val h3 = meld(h1, h2)
		
    val min1 = findMin(h1)
    val min2 = findMin(h3)
    val min3 = findMin(h3)
		
		min3 == min1 || min3 == min2
  }
	/**TODO:
	* add more properties that fails for
	* 'Bogus#BinomialHeap(s)'
	* from suggested properties:
	*>>If you insert any two elements into an empty heap, 
		finding the minimum of the resulting heap should get 
		the smallest of the two elements back.
	*>>If you insert an element into an empty heap, then 
		delete the minimum, 
		the resulting heap should be empty.
	*>>Given any heap, 
		you should get 
		a sorted sequence of elements when 
		continually finding and deleting minima. 
		(Hint: 
			recursion and helper functions are your friends.)
	*>>Finding a minimum of 
		the melding of any two heaps should return 
		a minimum of one or the other.
	*/
	
	/*mast be
	empty or not
	if not then
	arbitrary size*/
	/*
	type [H] are chosen in the 'QuickCheckSuite'
	from
	BinomialHeap
	to
	Bogus5BinomialHeap
	for
	object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap
	*/
  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[ Int ]
    h <- oneOf( 
			//*from import Gen._
			const( empty ), 
			genHeap )
  } yield insert(v, h)
	
	/*
	To get support for your own type T 
	you need to 
	define an implicit 
	def or val 
	of type Arbitrary[T]. 
	Use the factory method 
	Arbitrary(...) to 
	create 
	the Arbitrary instance. 
	This method takes 
	one parameter of type Gen[T] and 
	returns an instance of Arbitrary[T].
	*/
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
