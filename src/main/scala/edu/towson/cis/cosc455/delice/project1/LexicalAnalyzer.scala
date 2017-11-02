package edu.towson.cis.cosc455.delice.project1

trait LexicalAnalyzer {
    def addChar() : Unit
    def getChar() : Char
    def getNextToken() : Unit
    def lookup(candidateToken : String) : Boolean
}
