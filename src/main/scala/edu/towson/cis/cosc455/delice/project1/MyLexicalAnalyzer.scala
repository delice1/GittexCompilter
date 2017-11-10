package edu.towson.cis.cosc455.delice.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {

  private var lexLength: Int = _
  private var lexeme: String = "" //changed to string
  private var lexems = new ListBuffer[String]
  private var sourceLine: List[Char] = Nil //changed to list
  private var nextChar: Char = ' '
  private var position: Int = _

  //Lexical analyzer begins here
  def start(line: String): Unit = {
    println("IN START")
    initializeLexems()
    sourceLine = line.toList
    position = 0
    getNextToken()
    caseswitch(nextChar)
  }

  override def addChar(): Unit = {
    println("MADE IT TO ADDCHAR")
    getNonBlank()
    lexLength = 0
    lexeme += nextChar
    println(lexeme)
  }


  //Checks if current char is a space
  def isSpace(c: Char): Boolean = {
    println("MADE IT TO ISSPACE")
    c == ' ' | c == '\t' | c == '\n' | c == '\b' | c == '\f' | c == '\r' //CONSTANTS.WHITESPACE
  }


  //Checks if character in nextChar is a legal lexeme
  private def isLexeme(nextChar: Char): Boolean = {
    println("MADE IT TO ISLEXEME")
    nextChar match {
      case '!' => true
      case '+' => true
      case '[' => true
      case '\\' => true
      case '#' => true
      case '*' => true
      case _ => false
    }
  }


  def caseswitch(c: Char) = {
    println("nextchar in caseswitch is " + nextChar)
    println("c in caseswitch is " + c)
    c match {
      case '!' => image(c)
      case '+' => listitem(c)
      case '[' => link(c)
      case '\\' => newline(c)
      case '#' => heading(c)
      case '*' => bold(c)
      case _ => println("default case reached"); text(c) //somethings wrong with text! need to change!
    }
  }

  override def lookup(candidateToken: String): Boolean = {
    println("I AM IN LOOKUP METHOD")
    println("Candidate token in lookup is '" + candidateToken + "'")
    if (!lexems.contains(candidateToken)) {
      println("Lexical error: " + candidateToken + " not recognized.")
      System.exit(1)
      false
    }
    else {
      println("LOOKUP SUCCESS")
      true
    }
  }

  def getNonBlank(): Unit = {
    while (isSpace(nextChar))
      getChar()
  }


  override def getNextToken(): Unit = {
    println("MADE IT TO GETNEXTTOKEN")
    lexeme = ""
    getNonBlank()
    addChar()
    getChar() //nextChar = getChar() ??

    while (!isSpace(nextChar)) {
      addChar()
      nextChar = getChar() //getChar?
    }
  }

  override def getChar(): Char = {
    println("MADE IT TO GETCHAR")
    if (!sourceLine.isEmpty) {
      nextChar = sourceLine.head
      //need something else i think
      nextChar
    }
    else {
      println("Lexical error " + nextChar)
      System.exit(1)
      '\n'
    }
  }

  //add legal lexemes to language
  private def initializeLexems(): Unit = {
    //val validLexems : List[String] = List (CONSTANTS.DOCB)
    lexems += CONSTANTS.DOCB
    lexems += CONSTANTS.DOCE
    lexems += CONSTANTS.TITLEB
    lexems += CONSTANTS.BRACKETE
    lexems += CONSTANTS.HEADING
    lexems += CONSTANTS.PARAB
    lexems += CONSTANTS.PARAE
    lexems += CONSTANTS.BOLD
    lexems += CONSTANTS.LISTITEMB
    lexems += CONSTANTS.NEWLINE
    lexems += CONSTANTS.LINKB
    lexems += CONSTANTS.ADDRESSB
    lexems += CONSTANTS.ADDRESSE
    lexems += CONSTANTS.IMAGEB
    lexems += CONSTANTS.DEFB
    lexems += CONSTANTS.EQSIGN
    lexems += CONSTANTS.USEB
    lexems ++= CONSTANTS.LETTERS
    lexems ++= CONSTANTS.NUMBERSETC
    lexems ++= CONSTANTS.WHITESPACE
    lexems ++= CONSTANTS.VALIDTEXT

    lexems.toList //convert ListBuffer to List
  }


  //called if token is text
  private def text(c: Char): Unit = {
    println("tell ma i made it to text")
    nextChar = getChar()
    addChar()
  }

  //called if token is image
  private def image(c: Char): Unit = {
    println("Hello1 image")

  addChar()
  getChar()

  if (nextChar == '[') {
    println("made it here")
    addChar()
    getChar()
  }
  lookup(lexeme)

  }


  //called if token is list
  private def listitem(c : Char): Unit = {
    //lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
      addChar()
      getChar()
      lookup(lexeme)
    }
  }

  //called if token is link
  private def link(c : Char): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      addChar()
      getChar()
    }
  }

  //called if token newline, title, paragraph, variable define, or begin/end
  private def newline(c : Char) : Unit = {
    println("current token in newline " + Compiler.currentToken)
    //lookup(Compiler.currentToken)
    //Compiler.currentToken = "\\BEGIN"
    addChar()
    getChar()

    if (!isSpace(nextChar)){
      addChar()
    }

    else{
      println("error- newline error")
    }
  }


  //called if token heading
  private def heading(c : Char) : Unit = {
    addChar()
    getChar()
    lookup(lexeme)
    //lookup(Compiler.currentToken)
    if (Compiler.currentToken == CONSTANTS.HEADING) {
      println("made it")
    }
    else println("Error: looking for text in heading")
    }

  //called if token bold
  private def bold(c : Char) : Unit = {
    //lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      addChar()
      getChar()
      if (nextChar == '*') {
        addChar()
      }
      lookup(lexeme)
    }
  }
}
