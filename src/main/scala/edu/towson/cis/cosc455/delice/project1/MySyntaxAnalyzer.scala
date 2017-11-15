package edu.towson.cis.cosc455.delice.project1

import java.lang

class MySyntaxAnalyzer extends SyntaxAnalyzer
{
  var parseTree = scala.collection.mutable.Stack[String]()

  override def gittex(): Unit = {
    //DOCB
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //VARIABLEDEFINE
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
        variableDefine()
      }
      //TITLE
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
        title()
      }
      //BODY
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) || Compiler.isText || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
        body()
      }
      //DOCE
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.isEnd = true
        if (Compiler.Scanner.sourceLine.length > 1){
          println("Syntax error, nothing allowed after \\END")
          System.exit(1)
        }
      }
      else {
        println("Syntax error, looking for DOCE " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else {
      println("Syntax error, DOCB " + Compiler.currentToken )
      System.exit(1)
    }
  }

  override def paragraph(): Unit = {
    //PARAB
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //VARIABLE DEFINE
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
        variableDefine()
      }
      //INNER TEXT
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) || Compiler.isText){
        innerText()
      }
      //PARAE
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else{
        println("Syntax error, PARAE " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else{
      println("Syntax error, PARAB " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerItem()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerItem()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerItem()
    }
    if (Compiler.isText){
      parseTree.push(Compiler.currentToken)
      Compiler.isText = false
      Compiler.Scanner.getNextToken()
      innerItem()
    }
  }

  override def innerText(): Unit = {
    //TEXT
    if (Compiler.isText){
      parseTree.push(Compiler.currentToken)
      Compiler.isText = false
      Compiler.Scanner.getNextToken()
      innerText()
    }
    //VARIABLE USE
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerText()
      }
    //HEADING
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      heading()
      innerText()
    }
    //BOLD
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerText()
    }
    //LIST ITEM
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
      listItem()
      innerText()
    }
    //IMAGE
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      image()
      innerText()
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerText()
    }
  }

  override def link(): Unit = {
    //LINK
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //TEXT
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
        //BRACKETE
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          //ADDRESSB
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            //TEXT
            if (Compiler.isText){
              parseTree.push(Compiler.currentToken)
              Compiler.isText = false
              Compiler.Scanner.getNextToken()
              //ADDRESSE
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
                parseTree.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
              else{
                println("syntax error, address e in link " + Compiler.currentToken)
                System.exit(1)
              }
            }
            else{
              println("syntax error, text in link " + Compiler.currentToken)
              System.exit(1)
            }
          }
          else{
            println("syntax error, addressb in link " + Compiler.currentToken)
            System.exit(1)
          }
        }
        else{
          println("syntax error, brackete in link " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else{
        println("syntax error, text in link " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else{
      println("syntax error, linkb in link " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def body(): Unit = {
    //PARAB
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      paragraph()
      body()
    }
    //NEWLINE
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      newline()
      body()
    }
    //INNER TEXT
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) || Compiler.isText){
      innerText()
      body()
    }
  }

  override def bold(): Unit = {
    //BOLD
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //TEXT
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
        //BOLD
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else{
          println("syntax error, bold in bold " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else{
        println("syntax error, text in bold " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else{
      println("syntax error, bold in bold " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def newline(): Unit = {
    //NEWLINE
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else{
      println("syntax error, newline in newline " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def title(): Unit = {
    //TITLEB
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //TEXT
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
        //BRACKETE
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else{
          println("syntax error, brackete in title " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else{
        println("syntax error, reqtext in title " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else {
      println("SYNTAX ERROR - A title was expected when '" + Compiler.currentToken + "' was found")
      System.exit(1)
    }
  }

  override def variableDefine(): Unit = {
    //DEFB
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //TEXT
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
        //EQSIGN
        if (Compiler.currentToken.equals(CONSTANTS.EQSIGN)){
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
          //TEXT
          if (Compiler.isText){
            parseTree.push(Compiler.currentToken)
            Compiler.isText = false
            Compiler.Scanner.getNextToken()
            //BRACKETE
            if (Compiler.currentToken.equals(CONSTANTS.BRACKETE)){
              parseTree.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()
              //VARIABLE DEFINE
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
                variableDefine()
              }
            }
            else{
              println("Brackete required in var def" + Compiler.currentToken)
              System.exit(1)
            }
          }
          else{
            println("text required in var def " + Compiler.currentToken)
            System.exit(1)
          }
        }
        else{
          println("equal sign required in var def " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else{
        println("text required in var def " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else {
      println("Syntax error , defb in var define " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def image(): Unit = {
    //IMAGE
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //TEXT
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
        //BRACKETE
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          //ADDRESSB
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
            parseTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
            //TEXT
            if (Compiler.isText){
              parseTree.push(Compiler.currentToken)
              Compiler.isText = false
              Compiler.Scanner.getNextToken()
              //ADDRESSE
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
                parseTree.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
              else{
                println("syntax error, addresse in image " + Compiler.currentToken)
                System.exit(1)
              }
            }
            else{
              println("syntax error, text in image " + Compiler.currentToken)
              System.exit(1)
            }
          }
          else{
            println("syntax error, addressb in image " + Compiler.currentToken)
            System.exit(1)
          }
        }
        else{
          println("syntax error, brackete in image " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else{
        println("syntax error, text in image " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else{
      println("syntax error, imageb in image " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
        else{
          println("syntax error, brackete in variable use " + Compiler.currentToken)
          System.exit(1)
        }
      }
      else{
        println("syntax error, text in variable use " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else{
      println("syntax error, useb in variable use " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.isText){
        parseTree.push(Compiler.currentToken)
        Compiler.isText = false
        Compiler.Scanner.getNextToken()
      }
      else{
        println("syntax error, text in heading " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else{
      println("syntax error, heading in heading " + Compiler.currentToken)
      System.exit(1)
    }
  }

  override def listItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) || Compiler.isText){
        innerItem()
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
        listItem()
      }
    }
    else{
      println("syntax error, listitemb in listitem " + Compiler.currentToken)
      System.exit(1)
    }
  }
}