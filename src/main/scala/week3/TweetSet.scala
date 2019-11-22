package week3

object TweetSet {

  class Tweet(val text: String, val retweets: Int) {

    override def toString: String = this.text

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
  }

  object Empty extends TweetSet {
    def include(x: Tweet): TweetSet = new NonEmpty(x, Empty, Empty)

    def contains(x: Tweet): Boolean = false

    def isEmpty: Boolean = true

    def filter(p: Tweet => Boolean): TweetSet = this

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

    def head: Tweet = throw new NoSuchElementException("Empty.head")

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

    /**
     * If predicate is true for head add it to accumulator and run filterAcc on first left then right sub tree
     */
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      val updated_acc = if (p(this.head)) acc.include(this.head) else acc

      val leftAcc = left.filterAcc(p, updated_acc)
      right.filterAcc(p, leftAcc)
    }

    override def toString: String = "{" + left + elem + right + "}"

  }

}
