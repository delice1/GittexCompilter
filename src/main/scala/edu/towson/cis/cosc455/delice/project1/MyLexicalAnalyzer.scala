package edu.towson.cis.cosc455.delice.project1

class MyLexicalAnalyzer extends LexicalAnalyzer {
  override def addChar(): Unit = ???

  override def lookup(): Boolean = ???

  override def getNextToken(): Unit = {
    val c  = getChar()
  }

  override def getChar(): Char = ???
}
