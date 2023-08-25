package funsets

import scala.annotation.tailrec

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface:
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = input => input == elem
  
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = elem => s(elem) || t(elem)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = elem => s(elem) && t(elem)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = elem => s(elem) && !t(elem)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = intersect(s, p)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Will iterate from -1000 up to 1000. If all the elements in the set match the predicate, then return true, and false
   * otherwise
   * If a is bigger than the upper bound then return true, as all the elements have matched the predicate
   * Else if an element belongs to the set but does not match the predicate then return false, as not all elements match
   * Else the element belongs to the set and matches the predicate or the element does not belong, continue to the next
   * iteration
   */
  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean =
    @tailrec
    def iter(a: Int): Boolean =
      if a > bound then
        true
      else if s(a) && !p(a) then
        false
      else
        iter(a + 1)
    iter(-bound)

  /**
   * Will iterate from -1000 up to 1000. If at least one element in the set matches the predicate, then return true, and
   * false otherwise
   * forall will return false if at least one element does not match the predicate, so by first negating forall we
   * assure that it will return true if at least one element does not match the predicate, and by also negating the
   * predicate we assure that it will return true if at least one element matches the predicate
   */
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s, elem => !p(elem))

  /**
   * We can interpret map in the following way:
   * A given input number belongs to the mapped set if in the original set exists element such that when the
   * function is applied to it is equal to the input number
   */
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = input => exists(s, elem => input == f(elem))

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String =
    val xs = for i <- (-bound to bound) if contains(s, i) yield i
    xs.mkString("{", ",", "}")

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit =
    println(toString(s))

object FunSets extends FunSets
