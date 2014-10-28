package com.crayon.website.payment

import scala.collection.mutable.ArrayBuffer
import java.io.File
import net.didion.jwnl.data.POS

trait Guard[T] {
  def accept(token: T , prePath: ArrayBuffer[String]): Boolean
}

trait Action[T] {
  def perform(currState: String, token: T): Unit
  def getPreStatesAndNormalize(): ArrayBuffer[String]
}

class FSM[T](val action: Action[T],
  val debug: Boolean = false) {

  val statesMap = scala.collection.mutable.Map[
    String,(String,File,POS)]()
  val transitions = scala.collection.mutable.Map[
    String,ArrayBuffer[(String,Guard[T])]]()
  var currState: String = "START"
  
  def addState(state: String , guard: String, dictOrRegexFile: File , pos: POS): Unit = {
    
    statesMap (state)= (guard , dictOrRegexFile , pos)
    
  }
  
  def addTransition(from: String, to: String,
      guard: Guard[T]): Unit = {
    
    val translist = transitions.getOrElse(from, ArrayBuffer()) 
    translist += ((to, guard))
    transitions(from) = translist
  } 
  
  def transition(token: T): Boolean = {
    val tgas = transitions.getOrElse(currState, List())
      .filter(tga => tga._2.accept(token , action.getPreStatesAndNormalize))
    if (tgas.size == 1) {
      // no ambiguity, just take the path specified
      val tga = tgas.head
      if (debug)
        Console.println("%s -> %s".format(currState, tga._1))
      currState = tga._1
      action.perform(currState, token)
          return true
    } else {
      if (tgas.isEmpty) 
      {
        action.perform(currState, token) 
            return false
      }
      else {
        currState = tgas.head._1
        action.perform(currState, token)
            return true
      }
    }
  }
  
  def run(tokens: List[T]): Unit = tokens.foreach(transition(_))
}

class PFSM[T](action: Action[T], debug: Boolean = false) 
    extends FSM[T](action, debug) {

  val tprobs = scala.collection.mutable.Map[
    ((String,String)),Double]()

  def addTransition(from: String, to: String,
      tprob: Double, guard: Guard[T]): Unit = {
    super.addTransition(from, to, guard)
    tprobs((from, to)) = tprob
  }

  override def transition(token: T): Boolean = {
    val tgas = transitions.getOrElse(currState, List())
      .filter(tga => tga._2.accept(token, action.getPreStatesAndNormalize))
    if (tgas.size == 1) {
      // no ambiguity, just take the path specified
      val tga = tgas.head
      if (debug)
        Console.println("%s -> %s".format(currState, tga._1))
      currState = tga._1
      action.perform(currState, token)
      return true
    } else {
      // choose the most probable transition based
      // on tprobs. Break ties by choosing head as before
      if (tgas.isEmpty) {
        if(token.toString.equals("END"))
            action.perform("ENDWITHOUTMATCH", token)    
        else if(token.toString.split(" ").size < 2)   // don't perform(save to preState) if it is a mismatched phrase
            action.perform("O", token)    
        return false
      }
      else {
        val bestTga = tgas
          .map(tga => (tga, tprobs((currState, tga._1))))
          .sortWith((a, b) => a._2 > b._2) 
          .head._1
        currState = bestTga._1
        action.perform(currState, token)
        return true
      }
    }
  }
  
  override def run(tokens: List[T]): Unit = {
       var i = 0
       while(i < tokens.size){   // if  phrase are matched, jump several steps
           val matchflag = transition(tokens(i))

           val numOfWord =  tokens(i).toString.split(" ").size
         
           if( matchflag && numOfWord == 3 && i+8 < tokens.size )
            	      i += 8
           else if ( matchflag && numOfWord == 2 && i+5 < tokens.size )
            	      i += 5
           else if ( matchflag && numOfWord == 2 && i+4 < tokens.size )
            	      i += 4
           else i += 1
       }
       val endflag ="END".asInstanceOf[T]
       transition(endflag)
  }

}
