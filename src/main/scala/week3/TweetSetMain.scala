package week3

import week3.TweetSet._

object TweetSetMain extends App {
  val leaf1 = new NonEmpty(new Tweet("subTweet3", 3), Empty, Empty)
  val leaf2 = new NonEmpty(new Tweet("axubTweet3", 4), Empty, Empty)

  val t1 = new NonEmpty(new Tweet("subTweet13", 3), Empty, leaf1)
  val t2 = new NonEmpty(new Tweet("xubTweet2", 4), leaf2, Empty)
  val t = new NonEmpty(new Tweet("theTweet2", 2), t1, t2)
  println(t)
  println(t.filter(x => x.text.contains("3")))
  println(t.descendingByRetweet)
}