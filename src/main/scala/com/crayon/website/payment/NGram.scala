package com.crayon.website.payment

import scala.collection.mutable.ArrayBuffer

object NGram {

	def bigrams(tokens: List[Any]): List[List[Any]] = ngrams(tokens, 2)
	
	def trigrams(tokens: List[Any]): List[List[Any]] = ngrams(tokens, 3)
	
	def ngrams(tokens: List[Any], n: Int): List[List[Any]] = {
			val nwords = tokens.size
			val ngrams = new ArrayBuffer[List[Any]]()
			for (i <- 0 to (nwords - n)) {
			    ngrams += tokens.slice(i, i + n)
			}
			
			if(nwords < n) ngrams += tokens

			ngrams.toList
	}
	
	def alltrigrams(tokens: List[String]) : List[String]= {
	   val ngrams = new ArrayBuffer[String]()

	   for (i <- 0 to tokens.size-1) {
		   if(i < (tokens.size - 2))
		      ngrams += tokens.slice(i, i + 3).mkString(" ")
		   if(i < (tokens.size - 1))
		      ngrams += tokens.slice(i, i + 2).mkString(" ")
		   ngrams += tokens(i)
	   }
	   ngrams.toList
	}
}