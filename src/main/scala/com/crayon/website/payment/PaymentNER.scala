package com.crayon.website.payment

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import java.io.File
import scala.collection.JavaConversions

class PaymentNER() {
  
   val datadir = "resources/paymentNER-fsm"
   val negexdir = "resources/negex"
   val paymentFSM = new PaymentPFSM(
	        new File(datadir, "whword.dict"), 
	        new File(datadir, "paymentgateway.dict"), 
	        new File(datadir, "keyword.dict"), 
	        new File(datadir, "prp.dict"), 
	        new File(datadir, "in.dict"), 
	        new File(datadir, "vb.dict"), 
	        new File(datadir, "nn.dict"), 
	        new File(datadir, "punctuation.dict"),
	        new File("resources/wordnet_properties.xml"),
	        false)
  val negAnnotator = new NegexAnalysis(
            new File(negexdir, "negex_triggers.txt"),
            List("Negated", "Affirmed"))
  val proc = new CoreNLPProcessor(internStrings = true)   
   
  def getPayment(doc: String) : Array[String] = {
      if(doc == null) return Array.empty
      
      val textLowerCase = doc.toLowerCase()
      val sentences = proc.mkDocument(textLowerCase).sentences.map(f => f.getSentenceText)
      var paymentlist = List[String]()
      NGram.trigrams(sentences.toList).foreach(trisentences => {

          val stab = paymentFSM.parse(trisentences.mkString(". "))
	      val paymentgateways = stab.filter(p => p._2.equals("PAYMENTGATEWAY")).map(f => f._1) 
	      if( stab.last._2.equals("ENDWITHMATCH")){
	           val affirmedPayment = paymentgateways.filter(p => {
	             negAnnotator.predict(trisentences.mkString(". "), p, false).equals("Affirmed")
	          })
	          paymentlist = paymentlist :::  affirmedPayment
          }
      })
      paymentlist.toSet.toArray
  }
}