package calculator

/*to run or re-run
'webUI/fastOptJS'*/
object TweetLength {
  final val MaxTweetLength = 140

	/*from
	http://en.wikipedia.org/wiki/UTF-8
	{Bits of
	code 
	point}	{First
						code 
						point}	{Last
											code 
											point}	{Bytes in
																sequence}	Byte 1		Byte 2		Byte 3		Byte 4
  7				U+0000		U+007F		1						0xxxxxxx
	11			U+0080		U+07FF		2						110xxxxx	10xxxxxx
	16			U+0800		U+FFFF		3						1110xxxx	10xxxxxx	10xxxxxx
	21			U+10000		U+1FFFFF	4						11110xxx	10xxxxxx	10xxxxxx	10xxxxxx
	*/
	/**Note that 
	the remaining number of characters could 
	very well be negative, if 
	the Tweet text 
	currently 
	contains more than 'MaxTweetLength' characters.*/
  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
		/*'tweetText' must be binded to
		'tweetRemainingCharsCount'
		when it changes that
		updates 'tweetRemainingCharsCount' state value*/	
		
		/*need to
		1. return / extract text characters from signal then
		2. update signal with new value or
		create new signal ?*/
		/*store current value*/
		//val t = tweetText()
		//*val num = Var(1)                 
		/*??? binding ???*/
		//*val twice = Signal( num() * 2 )	
		
		val tweet = Var(0) 
		tweet() = MaxTweetLength - tweetLength(tweetText())	
		
		tweet
    //*new Signal(MaxTweetLength - tweetLength(tweetText()))
  }

	/**For better visual feedback, 
	we also want to 
	display 
	the remaining character count in 
	colors indicating how "safe" we are:
	>>If there are 
		15 or more characters left, 
		the color "green"
	>>If there are 
		between 0 and 14 characters left, included, 
		the color "orange"
	>>Otherwise 
		(if the remaining count is negative), 
		the color "red"
	(these are HTML colors).

	Implement 
	the function 'colorForRemainingCharsCount', which 
	uses 
	the signal of 
	remaining char count to 
	compute the signal of color.*/
  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
		/*all signals new or 
		just changes / update state of existent ones ?*/
		/*val color = Var("green") 
		
    color() = remainingCharsCount() match {
			case x if x > 15 => "green"
			case x if x >= 0 && x <= 14 => "orange"
			case _ => "red"
		}
		
		color*/
		new Signal(remainingCharsCount() match {
				case x if x > 15 => "green"
				case x if x >= 0 && x <= 14 => "orange"
				case _ => "red"
			}
		)
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as 
	 *  tweet lengths count 
	 *  the number of Unicode `code points` in the string.
   *  Note that 
	 *  this is still 
	 *  a simplified view of the reality. 
	 *  Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
