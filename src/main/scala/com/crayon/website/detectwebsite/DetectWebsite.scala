package com.crayon.website.detectwebsite

import org.ahocorasick.trie.Trie
import com.crayon.data.website.company.util.Levenshtein

class DetectWebsite {
     
     def matchDataPoint(phone: String, email: String, fax: String, text: String) : String  = {
            if (text == null || text.equals("")) return "no match"
            val normalizedPhone = if(phone != null && phone.length()>5) phone.substring(phone.length()-4) else ""
            val normalizedFax = if(fax != null && fax.length()>5) fax.substring(fax.length()-4) else ""
            val normalizedEmail = if(email != null ) email.trim() else ""
		    val trie = new Trie();
		
		    trie.addKeyword(normalizedPhone);
		    trie.addKeyword(normalizedFax);
		    trie.addKeyword(normalizedEmail);
		    
			val emits = trie.parseText(text);

			if (emits.size() == 0)  return "no match"
            val emit = emits.iterator().next()
            if(emit.getKeyword().equals(normalizedPhone))  "match by phone"
            else if(emit.getKeyword().equals(normalizedFax))  "match by fax"
            else if(emit.getKeyword().equals(email)) {
              println("email:"+email)
              "match by email"
            }
            else {
              println("error:"+emit.getKeyword())
              "Error"
            }
     }
     
     def matchHost ( companyname : String,  website : String) : Boolean = {
            if(website == null || companyname == null) return false
			val websiteWords  = website.toLowerCase().split("\\.")
			var p  = 0f
			for(i <- 0 to 1){
				if(websiteWords(0).length()>2) {
					 val nameWords = companyname.toLowerCase().split("\\s+");
					  p = Levenshtein.getDistancePercentage(nameWords(0), websiteWords(i));
					 if(nameWords.length>1){
						 val y = Levenshtein.getDistancePercentage(nameWords(0)+nameWords(1), websiteWords(i));
						 p = if ( p > y) p else y
					 }
					 if(nameWords.length>2){
						 val y = Levenshtein.getDistancePercentage(nameWords(0)+nameWords(1)+nameWords(2), websiteWords(i));
						 p = if ( p > y) p else y
					 }
					 if(p > 0.7){
	            			 return true;
					 }
				}
			}
			return false;
	}
     
    def matchTitle( companyname : String,  title : String) : Boolean = {
    		 
      
      return false;
    }
}