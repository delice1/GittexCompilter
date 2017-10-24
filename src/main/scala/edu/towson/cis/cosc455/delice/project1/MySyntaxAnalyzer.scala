package edu.towson.cis.cosc455.delice.project1

import java.lang

class MySyntaxAnalyzer extends SyntaxAnalyzer {

  val TITLES : List[String] = List("\\TITLE")

  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError : Boolean = errorFound

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Error")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = ???

  override def innerItem(): Unit = ???

  override def innerText(): Unit = ???

  override def link(): Unit = ???

  override def italics(): Unit = ???

  override def body(): Unit = ???

  override def bold(): Unit = ???

  override def newline(): Unit = ???
  /*
  {
    if ((NEWLINE contains Compiler.currentToken) || NEWLINE.isEmpty) //right idea, need different syntax
      Compiler.Scanner.getNextToken()
    else
      println("SYNTAX ERROR - A new line was expected h")
  }
  */

  override def title(): Unit = {
    if(TITLES contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else(
      println("SYNTAX ERROR - A title was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    )
  }

  override def variableDefine(): Unit = ???

  override def image(): Unit = ???

  override def variableUse(): Unit = ???

  override def heading(): Unit = ???

  override def listItem(): Unit = ???

  //def isEmpty() : Boolean //added myself.. who knows!
}
