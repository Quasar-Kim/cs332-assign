package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = {
    val reducer = (acc: TweetSet, tw: Tweet) => if (p(tw)) acc.incl(tw) else acc 
    this.reduce[TweetSet](reducer, new Empty())
  }

  /**
   * Reduce the set to a single value by successively applying `reducer` to each
   * elements.
   * @param reducer receives accumulator and current element as arguments, then
   *                returns a new accumulator value.
   * @param acc     initial value of accumulator.
   * @returns       a reduce value.
   */
  def reduce[A](reducer: (A, Tweet) => A, acc: A): A

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   */
   def union(that: TweetSet): TweetSet = {
     val reducer = (acc: TweetSet, tw: Tweet) => acc.incl(tw)
     this.reduce[TweetSet](reducer, that)
   }

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet = {
    val reducer = (acc: Option[Tweet], tweetA: Tweet) => acc match {
      case Some(tweetB) => if (tweetA.retweets > tweetB.retweets) Some(tweetA) else Some(tweetB)
      case None => Some(tweetA)
    }
    this.reduce[Option[Tweet]](reducer, Option.empty) match {
      case Some(tweet) => tweet
      case None => throw new java.util.NoSuchElementException()
    }
  }

  def leastRetweeted: Tweet = {
    val reducer = (acc: Option[Tweet], tweetA: Tweet) => acc match {
      case Some(tweetB) => if (tweetA.retweets < tweetB.retweets) Some(tweetA) else Some(tweetB)
      case None => Some(tweetA)
    }
    this.reduce[Option[Tweet]](reducer, None) match {
      case Some(tweet) => tweet
      case None => throw new java.util.NoSuchElementException()
    }
  }

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   */
  def descendingByRetweet: TweetList = {
    def reducer(acc: (TweetList, TweetSet), tw: Tweet): (TweetList, TweetSet) = {
      val (result, set) = acc
      val leastRetweeted = set.leastRetweeted
      (new Cons(leastRetweeted, result), set.remove(leastRetweeted))
    }
    val (result, _) = this.reduce[(TweetList, TweetSet)](reducer, (Nil, this))
    result
  }

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def reduce[A](reducer: (A, Tweet) => A, acc: A): A = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def reduce[A](reducer: (A, Tweet) => A, acc: A): A = {
    val reducedAcc = reducer(acc, elem)
    val leftReducedAcc = left.reduce[A](reducer, reducedAcc)
    right.reduce[A](reducer, leftReducedAcc)
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
  def reduce[A](reducer: (A, Tweet, Int) => A, acc: A): A = reduceInner[A](reducer, acc, 0)
  def reduceInner[A](reducer: (A, Tweet, Int) => A, acc: A, index: Int): A

  def apply(x: Int): Option[Tweet] =
    reduce[Option[Tweet]]((acc, tweet, i) => acc match {
      case Some(_) => acc
      case None => if (i == x) Some(tweet) else None
    }, None)
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  def reduceInner[A](reducer: (A, Tweet, Int) => A, acc: A, index: Int): A = acc
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  def reduceInner[A](reducer: (A, Tweet, Int) => A, acc: A, index: Int): A =
    tail.reduceInner[A](reducer, reducer(acc, head, index), index + 1)
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(
    (tw: Tweet) => google.exists((keyword: String) => tw.text.contains(keyword))
  )
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(
    (tw: Tweet) => apple.exists((keyword: String) => tw.text.contains(keyword))
  )

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
