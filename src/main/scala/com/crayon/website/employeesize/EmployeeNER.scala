package com.crayon.website.employeesize
import scala.io.Source
import java.io.File
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

import java.io.PrintWriter
import java.io.FileWriter
import java.util.Collection
import org.ahocorasick.trie.Trie
class EmployeeNER {
   val proc = new CoreNLPProcessor(internStrings = true)
   val neo4jGraph = new Neo4jLocal()
   val nlp = new NLP
   val trie = new Trie().onlyWholeWords();
   val model = Source.fromFile(new File("resources/employeesizeNER-fsm", "model.train"))
	    				   .getLines().toSet
	  Source.fromFile(new File("resources/employeesizeNER-fsm", "keyword.txt"))
	    	.getLines().map(line => line.toLowerCase()).toSet[String]			
            .foreach(trie.addKeyword(_))
   val wordBag = scala.collection.mutable.Set[(String,String)]()   
   
   
   def train(){
     	 neo4jGraph.deleteAllNodeAndRelation
         val trainingdata = Source.fromFile(new File("resources/employeesizeNER-fsm", "trainingdata.txt"))
	    				   .getLines().map(line => line.toLowerCase()).toList.map( f => {
            val line =  f.toLowerCase().split("\\|\\|")(0).trim()
            val number =  f.toLowerCase().split("\\|\\|")(1).trim()
            val key =  f.toLowerCase().split("\\|\\|")(2).trim()
            (line,number,key)
         }) 
         val trainWriter = new PrintWriter(
             new FileWriter(new File("resources/employeesizeNER-fsm", "model.train")))
         val ruleset = scala.collection.mutable.Set[String]() 
	     trainingdata.foreach(s =>{
	         var tx = neo4jGraph.graphDb.beginTx()
			     nlp.parseByCorenlp(s._1).foreach(f => {
			            neo4jGraph.createNodeIfNotExist(f._1._1,f._1._2)
		               neo4jGraph.createNodeIfNotExist(f._2._1,f._2._2)
		               neo4jGraph.createRelation(f._1._1,f._1._2, f._2._1,f._2._2, f._3)
		         })
		         val path = neo4jGraph.getShortestPath(s._2, s._3)
		         if(!path.trim().equals(""))  ruleset.add(path)
		         neo4jGraph.deleteAllNodeAndRelation
 	         tx.success()
 	         tx.finish()
	     })
	     ruleset.foreach(trainWriter.println(_))
	     trainWriter.flush()
	     trainWriter.close()
         neo4jGraph.shutdown
   }
   
   def test(){
     println(extract("we have 4 offices and 1000 employees in 5 countries"))
     println(extract("we have 4 offices and 2000 employees in 5 countries"))
   }
   
   def extract(s: String) : String = {
     
      val doc = proc.mkDocument(s)
                proc.tagPartsOfSpeech(doc)
           doc.clear()
      doc.sentences.foreach(s => {
         val emits = trie.parseText(s.getSentenceText)
         if(emits.size() != 0 ) {
            val keyword = emits.iterator().next().getKeyword()
            val tags = s.tags.get.toList
            for(i <- 0 to tags.size-1 if tags(i).equals("CD"))
            {   
                val number = s.words(i)
	            var tx = neo4jGraph.graphDb.beginTx()
			    nlp.parseByCorenlp(s.getSentenceText).foreach(f => {
			         if(!wordBag.contains(f._1)){
   			    	    neo4jGraph.createNodeIfNotExist(f._1._1,f._1._2)
   			    	    wordBag.add(f._1)
			         }
			    	 if(!wordBag.contains(f._2)){
			    	     neo4jGraph.createNodeIfNotExist(f._2._1,f._2._2)
			    	     wordBag.add(f._2)
			    	 }
		             neo4jGraph.createRelation(f._1._1,f._1._2, f._2._1,f._2._2, f._3)
		        })
		        val path = neo4jGraph.getShortestPath(number,keyword)
		        neo4jGraph.deleteAllNodeAndRelation
	 	        tx.success()
	 	        tx.finish()
	 	        if(model.contains(path)){
  		            return number
	 	        }
            }
         }
      })
      
      return null
   }
}

