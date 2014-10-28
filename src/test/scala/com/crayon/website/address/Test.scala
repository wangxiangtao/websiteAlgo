package com.crayon.website.address

import java.net.URL
import org.apache.commons.lang.StringEscapeUtils
import com.crayon.data.website.company.util.JDBCConnectionWrapper
import com.crayon.website.detectwebsite.DetectWebsite

object Main {
//     val jdbcConnection = new JDBCConnectionWrapper("com.mysql.jdbc.Driver",
//                           "jdbc:mysql://ec2-175-41-175-218.ap-southeast-1.compute.amazonaws.com:3306/"+
//                           "HannoverImpulse?useUnicode=true&characterEncoding=UTF-8"
//    		   				, "release4", "release4");
//     val companytable = "company"
//     def main(args: Array[String]): Unit = {
//     val dw = new DetectWebsite
//	 var companies = jdbcConnection.select("select id, website from "+companytable+" where website is not null");
//	 val addressExtactor = new AddressExtractorImpl
//     var update = jdbcConnection.update("update company set location_of_company_worldwide =?,no_of_location_of_company_worldwide=?,"+
//                           "location_in_country=?,no_of_country_worldwide=?,location_page=? where id = ?");
//	 while(companies.next()){
//         val id = companies.getString("id")
//         println(id)
//         var website = companies.getString("website").toLowerCase()
//         if(!website.startsWith("http")) website = "http://"+website
//          var url : URL = null
//		  try {
//			   url = new URL(website)
//		  }
//		  catch {
//		  		case e: Exception => {  println("exception caught: " + e) ;   }
//		  }
//		  
//		  if(url!=null) {
//		      val host = Option(url.getHost()).getOrElse(None);
//		      var crawlData = jdbcConnection.select("select text,url,title from crawl_data where host ='"+host+"'");
//		      var address = scala.collection.mutable.Set[String]()
//	          var urls = scala.collection.mutable.Set[String]()
//		      while(crawlData.next()){
//		         val url =crawlData.getString("url").toLowerCase()
//		         val title =crawlData.getString("title").toLowerCase()
//		         if((url+title).indexOf("contact") != -1){
//		             val add = addressExtactor.extract(crawlData.getString("text")).toSet
//		             if(add.size > 0 ) {
//		               urls.add(url)
//		               address = address ++ add
//		             } 
//		         }
//		      }
//		      if(address.size > 0) {
//		          var country = scala.collection.mutable.Set[String]()
//		          address.foreach(f => {
//		              val c = addressExtactor.extractCountry(f)
//		              if(c != null)
//		              country.add(c)
//		          })
//		          update.setParameter(1, StringEscapeUtils.escapeSql(address.mkString(" || ")))
//		          update.setParameter(2, address.size)
//		          update.setParameter(3, StringEscapeUtils.escapeSql(country.mkString(" || ")))
//		          update.setParameter(4, country.size)
//		          update.setParameter(5, urls.mkString(","))
//		          update.setParameter(6, id)
//		          update.commit()
//			   } 
//		      crawlData.close()
//		  }
//     }
//	 update.close()
//     companies.close()
//
//   }
}