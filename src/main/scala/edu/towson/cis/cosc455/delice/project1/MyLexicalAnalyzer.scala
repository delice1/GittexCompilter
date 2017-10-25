package edu.towson.cis.cosc455.delice.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  private var lexLength: Int = _
  private var lexeme: Array[Char] = new Array[Char](100)
  private var lexems = List[String]() //changed to List
  private var nextChar: Char = _
  private var sourceLine: String = _
  private var position: Int = _

  //Gets first lexeme
  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line
    position = 0
    nextChar = getChar()
    getNextToken()
  }

  override def addChar(): Unit = {
    if (lexLength <= 98){
      lexeme({lexLength += 1; lexLength-1}) = nextChar
      lexeme(lexLength) = 0
    }
    else {
      println("LEXICAL ERROR- lexem too long")
      if (!isSpace(nextChar)){
        while (!isSpace(nextChar)) getChar
      }
      lexLength = 0
      getNonBlank
      addChar()
    }
  }

  private def getToken (c : Char):Unit = {
    //While c is a space, tab, or new line, loop getChar until it is not a space
    while (c == '\t' || c == '\n' || c == ' ')
      getChar()
    //switch statement for keywords
    c match{
      case '!' => image()
      case '+' => listitem()
      case '[' => link()
      case '\\' => newline()
      case '#' => heading()
      case '*' => bold()
    }
  }

  //called if token is image
  private def image(): Unit = {

  }
  //called if token is list
  private def listitem(): Unit = {

  }
  //called if token is link
  private def link(): Unit = {

  }
  //called if token newline
  private def newline() : Unit = {

  }
  //called if token heading
  private def heading() : Unit = {

  }
  //called if token bold
  private def bold() : Unit = {

  }

  //Checks if current char is a space
  private def isSpace(c: Char): Boolean = {
    c == ' '
  }

  override def lookup(candidateToken: String): Boolean = {
    if (!lexems.contains(candidateToken)) {
      Compiler.Parser.setError()
      println("LEXICAL ERROR-'" + candidateToken + "' not recognized.")
      false
    }
    else{
      true
    }
  }
  private def getNonBlank(): Unit = {
    while (isSpace(nextChar))
      getChar
  }

  override def getNextToken(): Unit = {
    lexLength = 0
    //Ignore spaces and add the first character to token
    getNonBlank
    addChar()
    nextChar = getChar() //originally just getChar

    //Continue gathering characters for token
    while ((nextChar != '\n') && (nextChar != ' ') && (nextChar != '\t')){ //added not equal to tab
      addChar()
      nextChar = getChar() //originally just getChar
    }

    //Convert gathered character array token into a String
    val newToken: String = new String(lexeme)
    if (lookup(newToken.substring(0, lexLength)))
      Compiler.currentToken_$eq(newToken.substring(0,lexLength))
  }

  override def getChar(): Char = {
    //println("testing getChar method")
    //print("pos " + position)
    //println(sourceLine)
    //sourceLine.drop(1)
    //println(", char " + sourceLine.charAt({position+=1; position-1}))
    if (position < sourceLine.length){
      sourceLine.charAt({position+=1; position-1})
    }
    else
      '\n'
  }

  //add legal lexems to language
  private def initializeLexems(): Unit = {
    CONSTANTS.DOCB :: lexems
    CONSTANTS.DOCE :: lexems
    CONSTANTS.TITLEB :: lexems
    CONSTANTS.BRACKETE :: lexems
    CONSTANTS.HEADING :: lexems
    CONSTANTS.PARAB :: lexems
    CONSTANTS.PARAE :: lexems
    CONSTANTS.BOLD :: lexems
    CONSTANTS.LISTITEM :: lexems
    CONSTANTS.NEWLINE :: lexems
    CONSTANTS.LINKB :: lexems
    CONSTANTS.ADDRESSB :: lexems
    CONSTANTS.ADDRESSE :: lexems
    CONSTANTS.IMAGEB :: lexems
    CONSTANTS.DEFB :: lexems
    CONSTANTS.EQSIGN :: lexems
    CONSTANTS.USEB :: lexems
    CONSTANTS.LETTERS :: lexems
    CONSTANTS.NUMBERSETC :: lexems
    CONSTANTS.WHITESPACE :: lexems
    CONSTANTS.VALIDTEXT :: lexems
  }
}
