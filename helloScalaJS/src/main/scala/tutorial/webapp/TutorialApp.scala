package tutorial.webapp

import scala.scalajs.js.JSApp
/*import org.scalajs.dom
import dom.document*/
//remove the imports for the DOM, and 
//add the import for jQuery:
import org.scalajs.jquery.jQuery
//import scala.scalajs.js.annotation.JSExport

object TutorialApp extends JSApp {
	/*
	'dom' is 
	the root of the JavaScript DOM and 
	corresponds to 
	the global scope of Script (aka the window object). 
	We additionally import 
	'document' (which corresponds to document in JavaScript) 
	for convenience.*/

	/*We now create 
	a method that 
	allows us to 
	append a <p> tag with 
	a given text to a given node:*/
	/*def appendPar(
		targetNode: dom.Node, 	
		text: String): Unit = {
			val parNode = document.createElement("p")
			val textNode = document.createTextNode(text)
			
			parNode.appendChild(textNode)
			targetNode.appendChild(parNode)
	}*/

	/*We still want to 
	get rid of 
	the 'onclick' attribute of our <button>. 
	After removing 
	the attribute, 
	we add 
	the 'setupUI' method, 
	in which we use 'jQuery' to 
	add an event handler to the button. 
	We also 
	move the "Hello World" message into 
	this function.*/
	
	/*execute 'setupUI', 
		once the DOM is loaded*/
	def setupUI(): Unit = {
		jQuery("#click-me-button")
			.click(addClickedMessage _)
		jQuery("body")
			.append("<p>Hello ScalaJS World !</p>")
	}	
	/*
	This step shows 
	how you can add 
	a button and 
	react to 
	events on it by 
	still just using 
	the DOM (we will use jQuery in the next step). 
	We want to 
	add a button that 
	adds another <p> tag to 
	the body when 
	it is clicked.*/

	/*We start by 
	adding 
	a method to 
	'TutorialApp' which 
	will be called when 
	the button is clicked:*/
	/*annotation 
	tells the Scala.js compiler to 
	make that method callable from JavaScript.*/
	/*Since 
	we do not call 'addClickedMessage' from 
	plain JavaScript anymore, 
	we can 
	remove the '@JSExport' annotation 
	(and the corresponding import).*/
	//@JSExport
	def addClickedMessage(): Unit = {
		/*appendPar(
			document.body, 
			"You clicked the button!")*/
		/*replaced with*/	
		/*We can now 
		remove 'appendPar' and 
		replace all calls to it by 
		the simple:*/
		jQuery("body")
			.append("<p>You clicked the button</p>")		
	}

  def main(): Unit = {
		/*output to browser console*/
    println("Hello ScalaJS world!")
		/*output to browser window*/		
		//appendPar(document.body, "Hello ScalaJS World !!!")
		/*jQuery("body")
			.append("<p>Hello ScalaJS World !!!</p>")*/
		/*Finally, 
		we add 
		a last call to jQuery in the main method, 
		in order to 
		execute setupUI, 
		once the DOM is loaded:	*/
		jQuery(setupUI _)
		/*Again, 
		since 
		we are not calling 'setupUI' directly 
		from plain JavaScript, 
		we do not need to export it 
		(even though jQuery will call it).*/
  }
}