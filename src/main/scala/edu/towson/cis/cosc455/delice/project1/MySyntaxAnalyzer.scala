package edu.towson.cis.cosc455.delice.project1

import java.lang

class MySyntaxAnalyzer extends SyntaxAnalyzer
{
  var parseTree = scala.collection.mutable.Stack[String]()

  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError : Boolean = errorFound

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
        variableDefine()
      }
      else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
        title()
      }
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.isEnd = true
      }
      else {
        println("Syntax error, looking for DOCE " + Compiler.currentToken)
        //System.exit(1)
      }
    }
    else {
      println("Syntax error, DOCB " + Compiler.currentToken )
      //System.exit(1)
    }
  }

  override def paragraph(): Unit = {

  }

  override def innerItem(): Unit = {

  }

  override def innerText(): Unit = {

  }

  override def link(): Unit = {

  }

  override def body(): Unit = {

  }

  override def bold(): Unit = {

  }

  override def newline(): Unit = {
    /*
    {
      if ((NEWLINE contains Compiler.currentToken) || NEWLINE.isEmpty) //right idea, need different syntax
        Compiler.Scanner.getNextToken()
      else
        println("SYNTAX ERROR - A new line was expected h")
    }
    */
  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //REQTEXT ... need to add
      //BRACKETE ... need to add
    }
    else {
      println("SYNTAX ERROR - A title was expected when '" + Compiler.currentToken + "' was found")
      //System.exit(1)
    }
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //REQTEXT
      //EQUAL SIGN
    }
    else {
      println("Syntax error , defb in var define " + Compiler.currentToken)
      //System.exit(1)
    }
    //REQTEXT
    //BRACKETE
    variableDefine()
  }

  override def image(): Unit = {

  }

  override def variableUse(): Unit = {

  }

  override def heading(): Unit = {

  }

  override def listItem(): Unit = {

  }

  //def isEmpty() : Boolean //added myself.. who knows!

}