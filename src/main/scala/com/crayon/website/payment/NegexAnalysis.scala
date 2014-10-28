package com.crayon.website.payment

import java.io.File
import java.util.regex.Pattern

import scala.Array.canBuildFrom
import scala.io.Source

case class Offset(val start: Int, val end: Int) {
  def isNone = start == -1 && end == -1
  def None = Offset(-1,-1)
}

class NegexAnalysis(ruleFile: File, 
    responses: List[String]) {

  val rules = sortRules(ruleFile)
  
  def predict(sentence: String, phrase: String, 
      nonStrict: Boolean): String = 
    if (negTagger(sentence, phrase, nonStrict)) 
      responses(0)
    else responses(1)

  def sortRules(ruleFile: File): List[(Pattern,String)] = {

    Source.fromFile(ruleFile)
      .getLines()
      .map(line => {
        val cols = line.split("\t\t")
        (cols(0), cols(1))
      })
      .toList
      .sortWith((a,b) => a._1.length > b._1.length)
      // replace spaces by \\s+ and convert to pattern
      .map(pair => (
        Pattern.compile("\\b(" + pair._1
          .trim()
          .replaceAll("\\s+", "\\\\s+") + ")\\b"), 
            pair._2))
  }
  
  def negTagger(sentence: String, phrase: String, 
      tagPossible: Boolean): Boolean = {
    val normSent = sentence.toLowerCase()
      .replaceAll("\\s+", " ")
    val wordPositions = 0 :: normSent.toCharArray()
      .zipWithIndex
      .filter(ci => ci._1 == ' ')
      .map(ci => ci._2 + 1)
      .toList
    // tag the phrase
    val phrasePattern = Pattern.compile(
      "\\b(" + 
      phrase.replaceAll("\\s+", "\\\\s+") + 
      ")\\b", Pattern.CASE_INSENSITIVE)
    val phraseOffset = offset(normSent, phrasePattern)
    if (phraseOffset.isNone) return false
    // look for CONJ trigger terms
    val conjOffsets = offsets(normSent, "[CONJ]", rules)
    if (conjOffsets.isEmpty) {
      // run through the different rule sets, 
      // terminating when we find a match
      val triggerTypes = if (tagPossible) 
        List("[PREN]", "[POST]", "[PREP]", "[POSP]")
      else List("[PREN]", "[POST]")
      isTriggerInScope(normSent, rules, 
        phraseOffset, wordPositions, triggerTypes)      
    } else {
      val conjOffset = conjOffsets.head
      if (conjOffset.end < phraseOffset.start) {
        val truncSent = normSent.substring(conjOffset.end + 1)
        negTagger(truncSent, phrase, tagPossible)
      } else if (phraseOffset.end < conjOffset.start) {
        val truncSent = normSent.substring(0, conjOffset.start)
        negTagger(truncSent, phrase, tagPossible)
      } else {
        false
      }
    }
  }
  
  def isTriggerInScope(
      sentence: String,
      rules: List[(Pattern,String)],
      phraseOffset: Offset,
      wordPositions: List[Int],
      triggerTypes: List[String]): Boolean = {
    if (triggerTypes.isEmpty) false
    else {
      val currentTriggerType = triggerTypes.head
      val triggerOffsets = offsets(sentence, 
        currentTriggerType, rules)
      val selectedTriggerOffset = firstNonOverlappingOffset(
        phraseOffset, triggerOffsets)
      if (selectedTriggerOffset.isNone)
        // try with the next trigger pattern
        isTriggerInScope(sentence, rules, 
          phraseOffset, wordPositions,
          triggerTypes.tail)
      else {
        if (currentTriggerType.startsWith("[PRE"))
          selectedTriggerOffset.start < 
            phraseOffset.start
        else
          wordDistance(phraseOffset, 
            selectedTriggerOffset, 
            wordPositions) <= 5 &&
            phraseOffset.start < 
            selectedTriggerOffset.start
      }
    }
  }
  
  def wordDistance(phraseOffset: Offset, 
      triggerOffset: Offset,
      wordPositions: List[Int]): Int = {
    if (phraseOffset.start < triggerOffset.start)
      wordPositions
        .filter(pos => pos > phraseOffset.end && 
          pos < triggerOffset.start)
        .size
    else
      wordPositions
        .filter(pos => pos > triggerOffset.end && 
          pos < phraseOffset.start)
        .size
  }
  def offset(sentence: String, 
      pattern: Pattern): Offset = {
    val matcher = pattern.matcher(sentence)
    if (matcher.find()) 
      Offset(matcher.start(), matcher.end())
    else Offset(-1, -1)      
  }
  
  def offsets(sentence: String, ruleType: String,
      rules: List[(Pattern,String)]): List[Offset] = {
    rules.filter(rule => ruleType.equals(rule._2))
      .map(rule => offset(sentence, rule._1))
      .filter(offset => (! offset.isNone))
  }
  
  def firstNonOverlappingOffset(phraseOffset: Offset, 
      triggerOffsets: List[Offset]): Offset = {
    val phraseRange = Range(phraseOffset.start, phraseOffset.end)
    val nonOverlaps = triggerOffsets
      .filter(offset => {
        val offsetRange = Range(offset.start, offset.end)  
        phraseRange.intersect(offsetRange).size == 0
      })
    if (nonOverlaps.isEmpty) Offset(-1,-1) 
    else nonOverlaps.head
  }
}