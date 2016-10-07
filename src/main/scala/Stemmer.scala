/**
  * Created by mattkohl on 07/10/2016.
  */

import scala.io.Source


class Stemmer {

  // word to be stemmed.
  var word = ""
  val vowels = "aeiou"

  def isVowel(i: Int): Boolean = (vowels contains word(i)) || (i > 0 && word(i) == 'y' && isVowel(i-1))

  def isConsonant(i: Int): Boolean = !isVowel(i)


  def add(ch: Char) = word += ch
  def add(w: String) = word = w


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


  def isVowelInStem(s: String): Boolean = 0 until (word.length - s.length) exists isVowel


  def isDoubleConsonant: Boolean = {
    val l = word.length - 1
    isConsonant(l) && (l >= 1) && (word(l) == word(l - 1))
  }


  def isConsonantVowelConsonant(s: String): Boolean = {
    val i = word.length - 1 - s.length
    val ch = word(i)
    (i > 1) && isConsonant(i) && isVowel(i-1) && isConsonant(i-2) && !("wxy" contains ch)
  }


  def wasReplaced(orig: String, replace: String, checker: Int => Boolean): Boolean = {
    if (word endsWith orig) {
      val n = word.substring(0, word.length - orig.length)
      val m = getNumConsSeqs(n)
      if (checker(m)) {
        word = n + replace
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


  def step1() = {
    var m = getNumConsSeqs(word)

    // step 1a
    var vals = List(
      ("sses", "ss"),
      ("ies", "i"),
      ("ss", "ss"),
      ("s", "")
    )
    processSubList(vals, _ >= 0)

    // step 1b
    if (!wasReplaced("eed", "ee", _ > 0)) {
      if ((isVowelInStem("ed") && wasReplaced("ed", "", _ >= 0)) || (isVowelInStem("ing") && wasReplaced("ing", "", _ >= 0))) {
        vals = List(
          ("at", "ate"),
          ("bl", "ble"),
          ("iz", "ize")
        )

        if (!processSubList(vals, _ >= 0)) {
          m = getNumConsSeqs(word)
          val last = word(word.length - 1)
          if (isDoubleConsonant && !("lsz" contains last)) {
            word = word.substring(0, word.length - 1)
          }
          if (m == 1 && isConsonantVowelConsonant("")) {
            word += "e"
          }
        }
      }
    }

    // step 1c
    isVowelInStem("y") && wasReplaced("y", "i", _ >= 0)
  }


  def step2() = {

    val vals = List(
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
    processSubList(vals, _ > 0)
  }


  def step3() = {

    val vals = List(
      ("icate", "ic"),
      ("ative", ""),
      ("alize", "al"),
      ("iciti", "ic"),
      ("ical", "ic"),
      ("ful", ""),
      ("ness", "")
    )
    processSubList(vals, _ > 0)

  }


  def step4() = {

    // step4a
    val vals = List(
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
    var res = processSubList(vals, _ > 1)

    // step4b
    if (!res) {
      if (word.length > 4) {
        if (word(word.length - 4) == 's' || word(word.length - 4) == 't') {
          res = wasReplaced("ion", "", _ > 1)
        }
      }
    }

    // step4c
    if (!res) {
      val vals = List(
        ("ou", ""),
        ("ism", ""),
        ("ate", ""),
        ("iti", ""),
        ("ous", ""),
        ("ive", ""),
        ("ize", "")
      )
      processSubList(vals, _ > 1)
    }
  }


  def step5a() = {
    wasReplaced("e", "", _ > 1)

    if (!isConsonantVowelConsonant("e")) {
      wasReplaced("e", "", _ == 1)
    }
  }


  def step5b() = {
    val m = getNumConsSeqs(word)
    if (m > 1 && isDoubleConsonant && word.endsWith("l")) {
      word = word.substring(0, word.length - 1)
    }
  }
}


object runIt {
  def main(args: Array[String]) = {

    val source = Source.fromURL(getClass.getResource("/vocabulary.txt"))
    val stemmer = new Stemmer()

    for (line <- source.getLines) {
      val l = line.trim()
      stemmer.add(l)

      if (stemmer.word.length > 2) {
        stemmer.step1()
        stemmer.step2()
        stemmer.step3()
        stemmer.step4()
        stemmer.step5a()
        stemmer.step5b()
      }

      println(stemmer.word)
    }
  }
}



