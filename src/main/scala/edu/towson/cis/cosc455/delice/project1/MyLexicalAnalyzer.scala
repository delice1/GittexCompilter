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
    getChar()
    getNextToken()
  }

  override def addChar(): Unit = {
    if (lexLength <= 98){
      lexeme({lexLength += 1; lexLength -1}) = nextChar
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

  //Checks if current char is a space
  private def isSpace(c: Char): Boolean = c == ' '

  override def lookup(candidateToken: String): Boolean = {
    if (!lexems.contains(candidateToken)) {
      Compiler.Parser.setError() //originally was Compiler.Parser.setError()
      println("LEXICAL ERROR-'" + candidateToken + " ' not recognized.")
      false
    }
    else{
      Compiler.currentToken = candidateToken
      true
    }
  }
  private def getNonBlank(): Unit = {
    while (isSpace(nextChar))
      nextChar = getChar //originally just getChar
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
    if (position < sourceLine.length)
      sourceLine.charAt(position = position + 1)
    else
      '\n'
  }

  //add legal lexems to language
  private def initializeLexems(): Unit = {
    "\BEGIN" :: lexems
    "\END" :: lexems
    "\TITLE" :: lexems
    "]" :: lexems
    "#" :: lexems
    "\PARAB" :: lexems
    "\PARAE" :: lexems
    "*" :: lexems
    "+" :: lexems
    "\\" :: lexems
    "[" :: lexems
    "(" :: lexems
    ")" :: lexems
    "![" :: lexems
    "\DEF" :: lexems
    "=" :: lexems
    "\USE[" :: lexems
    "A" :: lexems
    "B" :: lexems
    "C" :: lexems
    "D" :: lexems
    "E" :: lexems
    "F" :: lexems
    "G" :: lexems
    "H" :: lexems
    "I" :: lexems
    "J" :: lexems
    "K" :: lexems
    "L" :: lexems
    "M" :: lexems
    "N" :: lexems
    "O" :: lexems
    "P" :: lexems
    "Q" :: lexems
    "R" :: lexems
    "S" :: lexems
    "T" :: lexems
    "U" :: lexems
    "V" :: lexems
    "W" :: lexems
    "X" :: lexems
    "Y" :: lexems
    "Z" :: lexems
  }
}
