/**
  * Created by mattkohl on 07/10/2016.
  */

import scala.io.Source


class Stemmer {

  // word to be stemmed.
  var b = ""
  val vowels = "aeiou"


  def isVowel(i: Int): Boolean = (vowels contains b(i)) || (i > 0 && b(i) == 'y' && !isVowel(i-1))
  def isConsonant(i: Int): Boolean = !isVowel(i)

  def add(ch: Char): Unit = b += ch
  def add(w: String): Unit = b = w


  def getNumConsSeqs(s: String): Int = {
    def consSeqsIter(i: Int, prevConsonant: Boolean): Int = {
      if (i > s.length-1) 0
      else if (isConsonant(i)) {
        if (!prevConsonant && i != 0)
          1 + consSeqsIter(i + 1, prevConsonant = true)
        else
          0 + consSeqsIter(i + 1, prevConsonant = true)
      }
      else
        0 + consSeqsIter(i + 1, prevConsonant = false)
    }
    consSeqsIter(i = 0, prevConsonant = false)
  }


  def isVowelInStem(suffix: String): Boolean = 0 until (b.length - suffix.length) exists isVowel


  def isDoubleConsonant: Boolean = {
    val bLength = b.length - 1
    isConsonant(bLength) && (bLength >= 1) && (b(bLength) == b(bLength - 1))
  }


  def isConsonantVowelConsonant(s: String): Boolean = {
    val i = b.length - 1 - s.length
    val ch = b(i)
    (i > 1) && isConsonant(i) && isVowel(i-1) && isConsonant(i-2) && !("wxy" contains ch)
  }


  def wasReplaced(suffix: String, substitution: String, checker: Int => Boolean): Boolean = {
    if (b endsWith suffix) {
      val stem = b.substring(0, b.length - suffix.length)
      val numConsonantSeqs = getNumConsSeqs(stem)
      if (checker(numConsonantSeqs)) {
        b = stem + substitution
      }
      return true
    }
    false
  }


  def processSubList(l: List[(String, String)], checker: Int => Boolean): Boolean = {
    val iter = l.toIterator
    var done = false

    while (!done && iter.hasNext) {
      val v = iter.next
      done = wasReplaced(v._1, v._2, checker)
    }
    done
  }


  def step1(): Unit = {
    var m = getNumConsSeqs(b)

    var subs = List(
      ("sses", "ss"),
      ("ies", "i"),
      ("ss", "ss"),
      ("s", "")
    )
    processSubList(subs, _ >= 0)

    if (!wasReplaced("eed", "ee", _ > 0)) {
      if ((isVowelInStem("ed") && wasReplaced("ed", "", _ >= 0)) || (isVowelInStem("ing") && wasReplaced("ing", "", _ >= 0))) {
        subs = List(
          ("at", "ate"),
          ("bl", "ble"),
          ("iz", "ize")
        )

        if (!processSubList(subs, _ >= 0)) {
          m = getNumConsSeqs(b)
          val last = b(b.length - 1)
          if (isDoubleConsonant && !("lsz" contains last)) {
            b = b.substring(0, b.length - 1)
          }
          else if (m == 1 && isConsonantVowelConsonant("")) {
            b = b + "e"
          }
        }
      }
    }

    isVowelInStem("y") && wasReplaced("y", "i", _ >= 0)
  }


  def step2(): Unit = {

    val subs = List(
      ("ational", "ate"),
      ("tional", "tion"),
      ("enci", "ence"),
      ("anci", "ance"),
      ("izer", "ize"),
      ("bli", "ble"),
      ("alli", "al"),
      ("entli", "ent"),
      ("eli", "e"),
      ("ousli", "ous"),
      ("ization", "ize"),
      ("ation", "ate"),
      ("ator", "ate"),
      ("alism", "al"),
      ("iveness", "ive"),
      ("fulness", "ful"),
      ("ousness", "ous"),
      ("aliti", "al"),
      ("iviti", "ive"),
      ("biliti", "ble"),
      ("logi", "log")
    )
    processSubList(subs, _ > 0)
  }


  def step3(): Unit = {

    val subs = List(
      ("icate", "ic"),
      ("ative", ""),
      ("alize", "al"),
      ("iciti", "ic"),
      ("ical", "ic"),
      ("ful", ""),
      ("ness", "")
    )
    processSubList(subs, _ > 0)
  }


  def step4(): Unit = {

    val subs = List(
      ("al", ""),
      ("ance", ""),
      ("ence", ""),
      ("er", ""),
      ("ic", ""),
      ("able", ""),
      ("ible", ""),
      ("ant", ""),
      ("ement", ""),
      ("ment", ""),
      ("ent", "")
    )
    var result = processSubList(subs, _ > 1)

    if (!result) {
      if (b.length > 4) {
        if (b(b.length - 4) == 's' || b(b.length - 4) == 't') {
          result = wasReplaced("ion", "", _ > 1)
        }
      }
    }

    if (!result) {
      val subs = List(
        ("ou", ""),
        ("ism", ""),
        ("ate", ""),
        ("iti", ""),
        ("ous", ""),
        ("ive", ""),
        ("ize", "")
      )
      processSubList(subs, _ > 1)
    }
  }


  def step5a(): Unit = {
    wasReplaced("e", "", _ > 1)

    if (!isConsonantVowelConsonant("e")) {
      wasReplaced("e", "", _ == 1)
    }
  }


  def step5b(): Unit = {
    val m = getNumConsSeqs(b)
    if (m > 1 && isDoubleConsonant && b.endsWith("l")) {
      b = b.substring(0, b.length - 1)
    }
  }
}


object runIt {
  def main(args: Array[String]): Unit = {

    val source = Source.fromURL(getClass.getResource("/vocabulary.txt"))
    val stemmer = new Stemmer()

    for (line <- source.getLines) {
      val trimmed = line.trim()
      stemmer.add(trimmed)

      if (stemmer.b.length > 2) {
        stemmer.step1()
        stemmer.step2()
        stemmer.step3()
        stemmer.step4()
        stemmer.step5a()
        stemmer.step5b()
      }

      println(stemmer.b)
    }
  }
}
