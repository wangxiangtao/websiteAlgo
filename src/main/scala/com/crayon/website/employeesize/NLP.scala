package com.crayon.website.employeesize

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.struct.DirectedGraphEdgeIterator

class NLP()  {
  
   val corenlp = new CoreNLPProcessor(internStrings = true)
   val fastnlp = new FastNLPProcessor(internStrings = true)

   def parseByFastnlp(sentence: String): List[(String,String,String)] = {
       val dependence = fastnlp.annotate(sentence)
	   val mappings = new DirectedGraphEdgeIterator[String](dependence.sentences.head.dependencies.get).map(d => {
	       val startword = dependence.sentences.head.words(d._1)
	       val endword = dependence.sentences.head.words(d._2)
	       (startword,endword, d._3)
	   }).toList 
       mappings
   }
   
   def parseByCorenlp(sentence: String): List[((String,String),(String,String),String)] = {
       val dependence = corenlp.annotate(sentence)
	   val mappings = new DirectedGraphEdgeIterator[String](dependence.sentences.head.dependencies.get).map(d => {
       val startword =  dependence.sentences.head.words(d._1)
       val starttag  =  dependence.sentences.head.tags.get(d._1) 
       val endword =  dependence.sentences.head.words(d._2)
       val endtag  =  dependence.sentences.head.tags.get(d._2) 

	      ((startword,starttag),(endword,endtag), d._3)
	      
	   }).toList 
       mappings
   }
   def splitSentence(text: String): List[(String)] = {
        val doc = corenlp.mkDocument(text)
        doc.clear()
        doc.sentences.toList.map(f => f.getSentenceText)
   }
}

