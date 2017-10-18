package edu.towson.cis.cosc455.delice.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  private var lexLength: Int = _
  private var lexeme: Array[Char] = new Array[Char](100)
  private var lexems: Array[String] = new Array[String](500) //was originally array list
  private var nextChar: Char = _
  private var sourceLine: String = _
  private var position: Int = _

  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line
    position = 0
    getChar
    getNextToken
  }

  override def addChar(): Unit = {
    if (lexLength <= 98){
      lexeme({lexLength += 1; lexLength -1}) = nextChar
      lexeme(lexLength) = 0
    }
    else {
      println("LEXICAL ERROR - The found lexeme is too long")
      if (!isSpace(nextChar)){
        while (!isSpace(nextChar)) getChar
      }
      lexLength = 0
      getNonBlank
      addChar()
    }
  }

  private def isSpace(c: Char): Boolean = c == ' '

  override def lookup(candidateToken: String): Boolean = {
    if (!lexems.contains(candidateToken)){
      Compiler.Parser.setError()
      println("LEXICAL ERROR -'" + candidateToken + " ' is not recognized.")
      false
    }
  }
  private def getNonBlank(): Unit = {
    while (isSpace(nextChar)) getChar
  }

  override def getNextToken(): Unit = {
    lexLength = 0
    //Ignore spaces and add the first character to token
    getNonBlank
    addChar()
    getChar()
    //Continue gathering characters for token
    while ((nextChar != '\n') && (nextChar != ' ')){
      addChar()
      getChar
    }
    //Convert gathered character array token into a String
    val newToken: String = new String(lexeme)
    if (lookup(newToken.substring(0, lexLength)))
      Compiler.currentToken_$eq(newToken.substring(0,lexLength))
  }

  override def getChar(): Char = {
    nextChar = if (position < sourceLine.length)
      sourceLine.charAt({position +=1; position - 1})
    else '\n'
  }

  private def initializeLexems(): Unit = {
    lexems :+= "\BEGIN"
    lexems :+= "\END"


  }
}
