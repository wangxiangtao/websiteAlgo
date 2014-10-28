package com.crayon.website.address

import java.net.URL
import java.util.regex.Pattern
import scala.collection.JavaConversions.collectionAsScalaIterable
import org.ahocorasick.trie.Trie
import com.crayon.data.website.company.util.TextFilter
import de.l3s.boilerpipe.extractors.KeepEverythingExtractor
import com.crayon.data.website.company.util.StringUtils

class AddressExtractorImpl {
  
  def main(args: Array[String]): Unit = {
     val url = new URL("http://www.purepak.co.uk/contact.html");
     val text = KeepEverythingExtractor.INSTANCE.getText(url);
     val test = "Tel: 01793 512505 Fax: 01634 511994"
     extract(text).foreach(f => println("result:"+ f))  
  } 
  
  def extract(s: String) : Array[String] = {
    
    val lines = s.lines.toList
    val addressSet = scala.collection.mutable.Set[String]()
    var curIndex = 0
    
    while( curIndex < lines.size )  {
           var address = ""
           val curLine = lines(curIndex)
           val preLine = if(curIndex > 0) lines(curIndex-1) else ""
           val postLine = if(curIndex < lines.size-1) lines(curIndex+1) else ""
           val secondPostLine = if(curIndex < lines.size-2) lines(curIndex+2) else ""
           var confidence = countFeatureAndConfidence(curLine)
           if(confidence._1 > 0.5 && confidence._2 >= 0.5){
             address = curLine 
             confidence = countFeatureAndConfidence(postLine)
             if(TextFilter.OnlyNNPorCD(preLine) || (existCountryOrCity(postLine) && confidence._1 > 0.65)){
                address = preLine + " " + address
             }
             confidence = countFeatureAndConfidence(postLine)
             if(existCountryOrCity(postLine) && confidence._1 > 0.3 && confidence._2 >= 0.25){
                address = address + " " + postLine 
                curIndex+=1
                val confidence = countFeatureAndConfidence(secondPostLine)
                if(existCountryOrCity(secondPostLine) && confidence._1 > 0.65){
                  address = address + " " + secondPostLine 
                  curIndex+=1
                }
             } 
           }
           if(address.length() > 10)   addressSet.add(address.trim())
           curIndex+=1
     } 
       
     addressSet.toArray
  }
  
  def countFeatureAndConfidence(s: String) = {
	  
      val cleanText = removeSymbol(s).toLowerCase()
      val originalSize = cleanText.split(" ").size
      val country =  WorldMap.extractCountry(cleanText)
      val city = if(country.size == 0) WorldMap.extractCity(cleanText)
                  else WorldMap.extractCity(cleanText,country)
      val locationMark =  WorldMap.extractLocationMarks(cleanText)
      val StreetUnitFloor =  WorldMap.extractStreetAndUnitAndFloor(cleanText)
      val zipcode =  WorldMap.extractZipCode(cleanText)
      val featureList = country ++ city ++ locationMark ++ StreetUnitFloor ++ zipcode
      val featureTrie = new Trie().removeOverlaps().onlyWholeWords().caseInsensitive()
          featureList.foreach(featureTrie.addKeyword(_))
    
      val removeFeatureText = new StringBuffer();
      featureTrie.tokenize(cleanText).filter(p => !p.isMatch())
      			 .foreach(f => removeFeatureText.append(f.getFragment()))
	  
	  val finalSize = if(removeFeatureText.toString().trim().equals("")) 0 
	  			       else removeFeatureText.toString().trim().split("\\s+").size
      val confidenceForFeatureNum = (originalSize-finalSize)/originalSize.toFloat
      val confidenceForFeatureCategory = (exist(country)+exist(city)+exist(locationMark)
                                         +exist(StreetUnitFloor ++ zipcode))/"4".toFloat
                               
//      println("originalCleanText:"+cleanText)
//      println("remove feature cleanText:"+removeFeatureText)
//      println( (confidenceForFeatureNum, confidenceForFeatureCategory) )
      (confidenceForFeatureNum, confidenceForFeatureCategory)        
  } 
  
  def extractCountry(s: String) : String = {
      val country =  WorldMap.extractCountry(removeSymbol(s))
      if (country.size > 0 ) {
        return country(0)
      }
      else null
  }
  
  def exist (s: List[String]) : Int = {
     if (s.size > 0) return 1 else return 0
  }
  
  def existCountryOrCity(s:String) : Boolean = {
      val country =  WorldMap.extractCountry(s)
      val city =  WorldMap.extractCity(s)
      return (country.size > 0 || city.size > 0)
  }
  
  def removeSymbol(s: String) :String = {
		val m = Pattern.compile("[,\"\\?!:'(){}].").matcher(s)
		val withoutSymbolText = m.replaceAll(" ")
		val withoutSymbolAndWhiteSpaceText = StringUtils.removeWhiteSpace(withoutSymbolText)
	    withoutSymbolAndWhiteSpaceText
  }
}