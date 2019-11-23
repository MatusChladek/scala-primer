package week3

object TweetSet {

  class Tweet(val text: String, val retweets: Int) {

    override def toString: String = "Text: " + this.text +" - Retweets: "+ this.retweets+ ";"

  }

  abstract class TweetSet {
    def include(x: Tweet): TweetSet

    def contains(x: Tweet): Boolean

    def isEmpty: Boolean

    /** This method takes a predicate and returns a subset of all the elements
     * in the original set for which the predicate is true.
     */
    def filter(p: Tweet => Boolean): TweetSet

    /**
     * Takes an accumulator set as a second argument.
     * This accumulator contains the ongoing result of the filtering.
     */
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

    def head: Tweet

    def union(that: TweetSet): TweetSet

    type TweetList = List[Tweet]

    def descendingByRetweet: TweetList = {
      val emptyList = new Nil[Tweet]
      this.addMinTweets(emptyList)
    }

    def addMinTweets(acc: TweetList): Cons[Tweet] = {
      val min = this.findMinAcc

      if (min.retweets.equals(-1)) new Cons[Tweet](min, acc)
      else this.remove(min).addMinTweets(new Cons[Tweet](min, acc))
    }

    def remove(x: Tweet): TweetSet

    def findMinAcc: Tweet

    def findMin(acc: Tweet): Tweet

  }

  object Empty extends TweetSet {
    def include(x: Tweet): TweetSet = new NonEmpty(x, Empty, Empty)

    def contains(x: Tweet): Boolean = false

    def isEmpty: Boolean = true

    def filter(p: Tweet => Boolean): TweetSet = this

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

    def head: Tweet = throw new NoSuchElementException("Empty.head")

    def union(that: TweetSet): TweetSet = that

    def remove(x: Tweet): TweetSet = this

    def findMinAcc: Tweet = {
      //            throw new NoSuchElementException("Empty.findMinAcc")
      new Tweet("Empty.findMinAcc", -1)
    }

    def findMin(acc: Tweet): Tweet = {
      acc
    }

    override def toString: String = "."
  }

  /**
   * The tweets are indexed by their text bodies.
   * The bodies of all tweets on the left are lexicographically smaller
   * than elem and all bodies of elements on the right are lexicographically greater.
   */
  class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def include(x: Tweet): TweetSet = {
      if (x.text < elem.text) {
        new NonEmpty(elem, left.include(x), right)
      } else if (x.text > elem.text) {
        new NonEmpty(elem, left, right.include(x))
      } else this
    }

    def contains(x: Tweet): Boolean = {
      if (x.text < elem.text) {
        this.left.contains(x)
      } else if (x.text > elem.text) {
        this.right.contains(x)
      } else true
    }

    def isEmpty: Boolean = false

    def filter(p: Tweet => Boolean): TweetSet = this.filterAcc(p, Empty)

    def head: Tweet = elem

    def union(that: TweetSet): TweetSet = {
      (left.union(right).union(that)).include(elem)
    }

    /**
     * If predicate is true for head add it to accumulator and run filterAcc on union of left and right
     */
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      //      TODO TESTS
      val updated_acc = if (p(this.head)) acc.include(this.head) else acc

      val leftAcc = left.filterAcc(p, updated_acc)
      right.filterAcc(p, leftAcc)

      /**
       * probably slower since union is computed in each recursive call
       */
      //      left.union(right).filterAcc(p, updated_acc)
    }

    /**
     * Returns TweetSet with only one Tweet removed
     */
    def remove(tw: Tweet): TweetSet = {
      if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
      else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
      else left.union(right)
    }

    def findMinAcc: Tweet = {
      findMin(new Tweet("Init", Int.MaxValue))
    }

    def findMin(acc: Tweet): Tweet = {
      if (elem.retweets < acc.retweets) left.union(right).findMin(elem)
      else left.union(right).findMin(acc)
    }

    override def toString: String = "{" + left + elem + right + "}"

  }

}
