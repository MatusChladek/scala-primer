package week6

import scala.io.Source

object linuxWords extends App {
  /* read a file of words */
  val in = Source.fromURL("https://lamp.epfl.ch/wp-content/uploads/2019/01/linuxwords.txt")

  /* create a list and filter all words where *all* their characters are not letters (like dashes) */
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))


  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI",
    '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS",
    '8' -> "TUV", '9' -> "WXYZ"
  )

  /** Invert mnem map to give a map from chars 'A'...'Z' to '2'...'9' */
  val charCode: Map[Char, Char] =
    for ((digit, strng) <- mnem; letter <- strng) yield letter -> digit

  println(charCode.filter(_._2 == '3'))

  /** Maps word to digit string it can represent, e.g. "Java" -> "5282" */
  def wordCode(word: String): String = {
    word.toUpperCase().map(charCode)
  }

  println(wordCode("Java"))

  /**
   * A map from digit strings to words that represent them
   * e.g. "5285" -> List("Java", "Lava", Kata",...)
   * Missing number maps to empty List()
   */
  val wordsForNum: Map[String, Seq[String]] = words.groupBy(wordCode).withDefaultValue(Seq())

  /* function that receives a number and finds the words that match it */
  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length // iterate over the number
        word <- wordsForNum(number.take(split)) // get the word before the spilt
        rest <- encode(number.drop(split)) //get the words after the split
      } yield word :: rest
      }.toSet // pass a set to the for

  /* better print of the results */
  def translate(number: String): Set[String] = encode(number).map(_ mkString " ")

  /* test the translate and print results*/
  translate("7225247386").foreach(println)
}
