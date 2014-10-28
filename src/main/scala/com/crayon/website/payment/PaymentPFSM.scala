package com.crayon.website.payment

import java.io.File
import java.util.regex.Pattern
import scala.Array.canBuildFrom
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import net.didion.jwnl.data.POS

class BoolGuard(val refValue: Boolean , definedPrePath: ArrayBuffer[String]) extends Guard[String] {
  override def accept(token: String , prePath: ArrayBuffer[String]): Boolean = refValue
}
class EndwithMatchGuard(val definedPrePath: ArrayBuffer[String]) extends Guard[String] {
   override def accept(token: String , prePath: ArrayBuffer[String]): Boolean = {
    return "END".equals(token) && definedPrePath.contains(prePath.mkString(" "))
  }
}
class DictGuard(val words: Set[String] , val definedPrePath: ArrayBuffer[String]) extends Guard[String] {
  override def accept(token: String , prePath: ArrayBuffer[String]): Boolean = {
    if(definedPrePath.head.equals("path of same and nearby states"))  return words.contains(token.toLowerCase())  // it means same and nearby states 
    return words.contains(token) && definedPrePath.contains(prePath.mkString(" "))
  }
}
class RegexGuard(val file: File, val definedPrePath: ArrayBuffer[String]) extends Guard[String] {
  val patterns = Source.fromFile(file).getLines()
    .map(line => Pattern.compile(line))
    .toList
  override def accept(token: String, prePath: ArrayBuffer[String]): Boolean = {
    if(definedPrePath.mkString(",").equals(prePath.mkString(",")))
	    patterns.map(pattern => {
	      val matcher = pattern.matcher(token)
	      if (matcher.matches()) return true
	    })
    false
  }
}
class CombinedGuard(val file: File, val pfile: File , val definedPrePath: ArrayBuffer[String]) 
      extends Guard[String] {
  val words = Source.fromFile(file).getLines()
    .map(line => line.toLowerCase().split(" ").toList)
    .flatten
    .toSet
  val patterns = Source.fromFile(pfile).getLines()
    .map(line => Pattern.compile(line))
    .toList
    
  override def accept(token: String, prePath: ArrayBuffer[String]): Boolean = {
    if(!definedPrePath.mkString(",").equals(prePath.mkString(","))) return false
    acceptWord(token) || acceptPattern(token)
  }
  
  def acceptWord(token: String): Boolean = 
    words.contains(token.toLowerCase())
    
  def acceptPattern(token: String): Boolean = {
    patterns.map(pattern => {
      val matcher = pattern.matcher(token)
      if (matcher.matches()) return true
    })
    false
  }
}

class CollectAction(val debug: Boolean) extends Action[String] {
  val stab = new ArrayBuffer[(String,String)]()
      stab += (("","START"))

  override def perform(currState: String, token: String): Unit = {
    if (debug)
      Console.println("setting: %s to %s".format(token, currState))
    stab += ((token,currState))
  }
  override def getPreStatesAndNormalize(): ArrayBuffer[String] = {
    // ignore O first , then ignore same and nearby states, only keep one.
    val states = stab.map(f => f._2).filter(p => !p.equals("O"))
    var preStates = ArrayBuffer[String]()
    var preState = ""
    states.foreach(f =>{
       if(!f.equals(preState)){
          preStates += f
       }
       preState = f
    })
//  println("preStates:"+preStates)
    return preStates
  }
  
  def getSymbolTable(): Map[String,List[String]] = {
    stab.groupBy(kv => kv._1)
      .map(kv => (kv._1, kv._2.map(_._2).toList))
  }
}
class PaymentPFSM(val whwordFILE: File,   val paymentgatewayFILE: File,  val keywordFILE: File,
    val prpFILE: File,   val inFILE: File,  val vbFILE: File, val nnFILE: File,
    val punctuationFILE: File, val wordnetConfig: File, 
    val debug: Boolean = false) {
  
  val wn = new Wordnet(wordnetConfig)
  val proc = new CoreNLPProcessor(internStrings = true)
    
  def parse(s: String): List[(String,String)] = {
    val collector = new CollectAction(debug)
    val fsm = buildFSM(collector, debug)
   
    val doc = proc.mkDocument(s.toLowerCase())
        proc.tagPartsOfSpeech(doc)
        proc.lemmatize(doc)
        doc.clear()
    val lemmas  = doc.sentences.map(f => f.lemmas.get.toList).toList.flatten
    val trigrams = NGram.alltrigrams(lemmas)
    val x = fsm.run(trigrams)
    collector.stab.toList
  }
  
  def buildFSM(collector: CollectAction, 
      debug: Boolean): PFSM[String] = {

    val pfsm = new PFSM[String](collector, debug)
    pfsm.addState("START", null, null, null)
    pfsm.addState("WHWORD", "DictGuard", whwordFILE, null)
    pfsm.addState("PRP", "DictGuard", prpFILE, null)
    pfsm.addState("IN", "DictGuard", inFILE, null)
    pfsm.addState("NN", "DictGuard", nnFILE, POS.NOUN)
    pfsm.addState("KEYWORD", "DictGuard", keywordFILE, POS.NOUN)
    pfsm.addState("VB", "DictGuard", vbFILE, POS.VERB)
    pfsm.addState("PAYMENTGATEWAY", "DictGuard", paymentgatewayFILE, null)
    pfsm.addState("PUNCTUATION", "DictGuard", punctuationFILE, null)
    pfsm.addState("ENDWITHMATCH", "EndGuard", null, null)

    var transitions = scala.collection.mutable.Set[String]()
    transitions.add("WHWORD KEYWORD PAYMENTGATEWAY")
    transitions.add("WHWORD KEYWORD VB PAYMENTGATEWAY")
    transitions.add("WHWORD KEYWORD NN VB PAYMENTGATEWAY")
    transitions.add("PRP VB PAYMENTGATEWAY")
    transitions.add("PAYMENTGATEWAY NN KEYWORD")
    transitions.add("PAYMENTGATEWAY KEYWORD NN")
    transitions.add("VB PAYMENTGATEWAY KEYWORD")
    transitions.add("VB IN PAYMENTGATEWAY")
    transitions.add("KEYWORD NN PAYMENTGATEWAY")
    transitions.add("KEYWORD VB IN PAYMENTGATEWAY")
    transitions.add("KEYWORD PUNCTUATION PAYMENTGATEWAY")
    transitions.add("WHWORD VB PAYMENTGATEWAY")

    val stateMapping = scala.collection.mutable.Map[((String,String)),ArrayBuffer[String]]()   // (from , to) , multi-defianedPreStates

    transitions.foreach(r => {
    	val words = ("START "+r+" ENDWITHMATCH").split(" ")
    	var states = ""
    	for(i <- 0 to words.length-2){
    	   states += " " + words(i)
    	   val definedPreStates = stateMapping.getOrElse((words(i),words(i+1)), ArrayBuffer())
    	   definedPreStates += states.trim()
    	   stateMapping((words(i),words(i+1))) = definedPreStates
    	   //support transitions among multiple same and nearby states, don't care prestates , ignore the first state "START"
    	   if(i>0) stateMapping((words(i),words(i))) = ArrayBuffer("path of same and nearby states")   
    	}
    })

    stateMapping.foreach{ case (k,v) => {
           if(pfsm.statesMap(k._2)._1.equals("DictGuard")){
               val words = convertDictFileWithWordNet(pfsm.statesMap(k._2)._2 , null)
               pfsm.addTransition( k._1, k._2, 1.0,  new DictGuard(words , v))
           }
           else if (pfsm.statesMap(k._2)._1.equals("RegexGuard"))
        	   pfsm.addTransition( k._1, k._2, 1.0,  new RegexGuard(pfsm.statesMap(k._2)._2 , v))
           else if (pfsm.statesMap(k._2)._1.equals("EndGuard"))
               pfsm.addTransition( k._1, k._2, 0.1,  new EndwithMatchGuard(v))
    }}
    
    pfsm
    
  }
  
  def convertDictFileWithWordNet(file: File , pos: POS): Set[String] = {
    if(pos == null)
          Source.fromFile(file).getLines().map(line => line.toLowerCase()).toSet
    else  Source.fromFile(file).getLines()
          .map(line => {wn.lemmaNames(wn.synsets(line.toLowerCase(), pos))}).flatten.toSet
  }
  
}
