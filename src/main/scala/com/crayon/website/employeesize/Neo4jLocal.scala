package com.crayon.website.employeesize


import org.neo4j.cypher.ExecutionEngine
import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphalgo.impl.util.PathImpl
import org.neo4j.graphdb.DynamicLabel
import org.neo4j.graphdb.PathExpanders
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.helpers.collection.IteratorUtil
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Relationship
class Neo4jLocal (){
	val graphDb = new GraphDatabaseFactory().newEmbeddedDatabase("neo4j-db");
	val engine = new ExecutionEngine( graphDb );
	def createNodeIfNotExist(word: String, tag: String) : Unit = {
	     engine.execute("MERGE (Word { tag: '"+tag+"' , value:'"+word+"'})")
	}
	
	def deleteAllNodeAndRelation() {
	     engine.execute("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r"); 
	}
	
	def returnAll() {
		   println(engine.execute("MATCH (n) OPTIONAL MATCH (n)-[r]-() RETURN n,r"))
	}
	
	def createRelation(startword: String,starttag:String, endword: String,endtag:String,relation: String) {
	        engine.execute("MERGE (startword:Word { tag:'"+starttag+"' , value:'"+startword+"'})"+
		     " MERGE (endword:Word { tag:'"+endtag+"', value:'"+endword+"'}) "+
		     " MERGE (startword)-[r:"+relation+"]->(endword) "); 
	}
	
    def getShortestPath(number: String , key: String) : String = {
		   val result =  engine.execute("MATCH (people:Word { value:'"+key+"' }),(num :Word { value:'"+number+"' }),"+
		    		"p = shortestPath((people)-[*..15]-(num)) RETURN p"); 
		   val column = result.columnAs("p")
		   var shortestPath = ""
  	       var currentNode = ""
		   while(column.hasNext){
  	          val path =  IteratorUtil.asCollection(column.next.asInstanceOf[PathImpl])
  	          path.toArray().toList.foreach(
  	            _  match {
  	               case n: Node => {
	  	                shortestPath += n.getProperty("tag")+" "
	  	                currentNode = n.getProperty("value").toString()
	  	              }
  	               case r: Relationship => {
  	            	  	if(r.getStartNode().getProperty("value").equals(currentNode))
  	            	  	shortestPath += "--> "+r.getType().name()+" --> "
  	            	  	else shortestPath += "<-- "+r.getType().name()+" <-- "
  	               }
  	            }
  	          )
  	       }
		   shortestPath
	}
	def shutdown() {
	  	graphDb.shutdown();
	}
}