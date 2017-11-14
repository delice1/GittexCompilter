package edu.towson.cis.cosc455.delice.project1

import scala.collection.mutable.ListBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {

  //Initial Declarations
  private var lexeme: String = ""
  private var lexems = new ListBuffer[String]
  var sourceLine: List[Char] = Nil
  private var nextChar: Char = ' '

  //Start- Beginning of Lexical Analyzer
  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line.toList
    getNextToken()
  }

  //Adds nextChar to lexeme
  override def addChar(): Unit = {
    lexeme += nextChar
  }

  //Returns true if character is equal to any of the CONSTANTS.WHITESPACE
  private def isSpace(c: Char): Boolean = {
    c == ' ' | c == '\t' | c == '\n' | c == '\b' | c == '\f' | c == '\r' //CONSTANTS.WHITESPACE
  }

  //Returns true if character in nextChar is a legal lexeme
  private def isLexeme(nextChar: Char): Boolean = {
    nextChar match {
      case '!' => true
      case '+' => true
      case '[' => true
      case '\\' => true
      case '#' => true
      case '*' => true
      case '=' | '(' | ')' | ']' => true //EQSIGN, ADDRESSB, ADDRESSE, BRACKETE
      case _ => false
    }
  }

  //Calls appropriate sub-method method depending on character (nextChar) passed in
  private def caseswitch(c: Char) : Unit = {
    c match {
      case '!' => image(c)
      case '+' => listitem(c)
      case '[' => link(c)
      case '\\' => newline(c)
      case '#' => heading(c)
      case '*' => bold(c)
      case '=' | '(' | ')' | ']' => other(c) //EQSIGN, ADDRESSB, ADDRESSE, BRACKETE
      case _ => text(c) //default => text
    }
  }

  //Returns true if candidate token is in list of lexems
  override def lookup(candidateToken: String): Boolean = {
    //println("I AM IN LOOKUP METHOD")
    if (!lexems.contains(candidateToken)) {
      println("Lexical error: " + candidateToken + " not recognized.")
      System.exit(1)
      false
    }
    else {
      //println("LOOKUP SUCCESS")
      true
    }
  }

  //Loops until a space is not found
  private def getNonBlank(): Unit = {
    while (isSpace(nextChar))
      getChar()
  }

  //Calls switch statement to get next token
  override def getNextToken(): Unit = {
    lexeme = ""
    getNonBlank()

    caseswitch(nextChar) //calls sub-method
  }

  //Assigns the next character in sourceLine to be nextChar until the list is empty
  override def getChar(): Char = {
    //println("MADE IT TO GETCHAR")
    if (!sourceLine.isEmpty) {
      nextChar = sourceLine.head
      sourceLine = sourceLine.tail //assigns everything else in list to soureLine
      nextChar
    }
    else {
      println("Lexical error with " + nextChar)
      System.exit(1)
      '\n'
    }
  }

  //Adds pre-defined lexems to mutable list buffer. Converts to immutable list at end.
  private def initializeLexems() : Unit = {
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

    lexems.toList //convert mutable ListBuffer to immutable List
  }

  //Sub method: Text (Default case of switch)
  private def text(c: Char) : Unit = {
    addChar()
    getChar()

    //keeps going until it finds another keyword char
    while (!isLexeme(nextChar)) {
      addChar()
      getChar()
    }
    Compiler.currentToken = lexeme
    Compiler.isText  = true
  }

  //Sub method: Image
  private def image (c : Char) : Unit = {
    addChar()
    getChar()
    if (nextChar == '[') {
      addChar()
      getChar()
    }
    lookup(lexeme)
    Compiler.currentToken = lexeme //assigns lexeme to global variable currentToken
  }

  //Sub method: List
  private def listitem(c: Char) : Unit = {
    addChar()
    getChar()
    lookup(lexeme)
    Compiler.currentToken = lexeme
  }

  //Sub method: Link
  private def link(c : Char) : Unit = {
    addChar()
    getChar()
    lookup(lexeme)
    Compiler.currentToken = lexeme
  }

  //Sub method: Newline, Title, Paragraph, Variable Define, Begin, or End
  private def newline(c : Char) : Unit = {
    addChar()
    while (!isSpace(nextChar) && nextChar != '['){
      if (lexeme.startsWith(CONSTANTS.DOCB)) {
        Compiler.isEnd = true
      }
      getChar()
      if (!isSpace(nextChar)) {
        addChar()
      }
    }
    if (lookup(lexeme)) {
      Compiler.currentToken = lexeme
    }
    else {
      println("Lexical error with " + Compiler.currentToken)
    }
    if (nextChar == '['){
      getChar()
    }
  }

  //Sub method: Heading
  private def heading (c : Char) : Unit = {
    addChar()
    getChar()
    lookup(lexeme)
    Compiler.currentToken = lexeme
  }

  //Sub method: Bold
  private def bold(c : Char) : Unit = {
    addChar()
    getChar()
    lookup(lexeme)
    Compiler.currentToken = lexeme
  }

  //called if token is other
  private def other(c: Char) : Unit = {
    addChar()
    getChar()
    lookup(lexeme)
    Compiler.currentToken = lexeme
  }
}