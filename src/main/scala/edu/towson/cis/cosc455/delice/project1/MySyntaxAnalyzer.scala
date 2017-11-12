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
      println("DOCB " + Compiler.currentToken)
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
        println("gittex defb")
        variableDefine()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
        println("gittex titleb")
        title()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) || Compiler.isText || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
        body()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        println("REACHED DOCE IN GITTEX")
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
    println("MADE IT TO PARAGRAPH")
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
        variableDefine()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) || Compiler.isText){
        println("contains inner text in paragraph")
        innerText()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Syntax error, PARAE " + Compiler.currentToken)
        //System.exit(1)
      }
    }
    else{
      println("Syntax error, PARAB " + Compiler.currentToken)
      //System.exit(1)
    }
  }

  override def innerItem(): Unit = {

  }

  override def innerText(): Unit = {
    println("inner text " + Compiler.currentToken)
    if (Compiler.isText){
      parseTree.push(Compiler.currentToken)
      Compiler.isText = false
      Compiler.Scanner.getNextToken()
      innerText()
    }
  }

  override def link(): Unit = {

  }

  override def body(): Unit = {
    println("IN BODY")
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      println("IN PARAB IN BODY")
      paragraph()
      body()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      newline()
      body()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) || Compiler.isText){
      innerText()
      body()
    }
    println("REACHED END OF BODY")
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
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          println("parse tree in title " + parseTree.reverse)
        }
        else{
          println("syntax error, brackete in title " + Compiler.currentToken)
          //System.exit(1)
        }
      }
      else{
        println("syntax error, reqtext in title " + Compiler.currentToken)
        //System.exit(1)
      }
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