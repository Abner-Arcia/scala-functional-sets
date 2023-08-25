package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s1s2: FunSet = (elem: Int) => elem == 1 || elem == 2
    val s1s2s3: FunSet = (elem: Int) => elem == 1 || elem == 2 || elem == 3

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union of 1 and 2 should contain 1")
      assert(contains(s, 2), "Union of 1 and 2 should contain 2")
      assert(!contains(s, 3), "Union of 1 and 2 should not contain 3")
  }

  test("intersect contains all elements that belong to both sets") {
    new TestSets:
      val s = intersect(s1, s1s2)
      assert(contains(s, 1), "Intersection of 1 and 1, 2 should contain 1")
      assert(!contains(s, 2), "Intersection of 1 and 1, 2 should not contain 2")
  }

  test("diff contains all elements that belong to the first set but not to the second one") {
    new TestSets:
      val s = diff(s1s2, s1)
      assert(!contains(s, 1), "Difference of 1, 2 and 1 should not contain 1")
      assert(contains(s, 2), "Difference of 1, 2 and 1 should contain 2")
  }

  test("filter contains all elements that belong to the set and are accepted by the predicate") {
    new TestSets:
      val s = filter(s1s2s3, elem => elem % 2 == 1) // match odd numbers
      assert(contains(s, 1), "Filtering of 1, 2, 3 by odd numbers should contain 1")
      assert(!contains(s, 2), "Filtering of 1, 2, 3 by odd numbers should not contain 2")
      assert(contains(s, 3), "Filtering of 1, 2, 3 by odd numbers should contain 3")
  }

  test("forall returns true if each element that belongs to the set is accepted by the predicate, false otherwise") {
    new TestSets:

      // all elements between 0 and 9 are less than 100
      val lessThan100: Boolean = forall(elem => elem >= 0 && elem < 10, elem => elem < 100)

      // not all elements that are even are positive, they can be negative too
      val positives: Boolean = forall(elem => elem % 2 == 0, elem => elem > 0)

      assert(lessThan100, "All numbers 0 to 9 are less than 100")
      assert(!positives, "Not all even numbers are positive")
  }

  test("exists returns true if there is at least one element that belongs to the set and is accepted by the predicate, false otherwise") {
    new TestSets:

      // none of the elements between 0 and 9 are negative
      val negatives: Boolean = exists(elem => elem >= 0 && elem < 10, elem => elem < 0)

      // in the set of all elements that are even, there is at least a 4
      val fourIsEven: Boolean = exists(elem => elem % 2 == 0, elem => elem == 4)

      assert(!negatives, "There does not exists a negative number in the numbers 0 to 9")
      assert(fourIsEven, "There exists at least a number 4 in the even numbers")
  }

  test("map contains all elements that belong to the set created by applying the given function to each element of the original set") {
    new TestSets:

      // map numbers 1 to 10 to their negatives
      val negatives: FunSet = map(elem => elem >= 1 && elem <= 10, elem => -elem)

      assert(contains(negatives, -1), "The mapping of numbers 1 to 10 to their negatives should contain -1")
      assert(contains(negatives, -10), "The mapping of numbers 1 to 10 to their negatives should contain -10")
      assert(!contains(negatives, 0), "The mapping of numbers 1 to 10 to their negatives should not contain 0")
      assert(!contains(negatives, -11), "The mapping of numbers 1 to 10 to their negatives should not contain -11")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
