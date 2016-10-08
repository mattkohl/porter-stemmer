import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TestStemmer extends FunSuite {

  val stemmer = new Stemmer()
  stemmer.add("bane")

  test("Is the first character a consonant") {
    assert(stemmer.isConsonant(0) === true)
  }

  test("Is the second character a consonant") {
    assert(stemmer.isConsonant(1) === false)
  }

}
