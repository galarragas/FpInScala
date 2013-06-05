package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TailTest extends FlatSpec with ShouldMatchers {
  "Tail" should "return empty List when applied to empty list" in {
    tail(List()) should equal(List())
  }

  it should "return empty list when applied to single item list" in {
    tail(List("a")) should equal(List())
  }

  it should "return rest of the list when applied to a multi item list" in {
    tail(List("a", "b", "c")) should equal(List("b", "c"))
  }
}

class DropTest extends FlatSpec with ShouldMatchers {
  "Drop" should "alwais return empty List when applied to empty list" in {
    drop(List(), 2) should equal(List())
  }

  it should "return empty list when dropping same number of items than contained" in {
    drop(List("a", "b", "c"), 3) should equal(List())
  }

  it should "return empty list when dropping more items than contained" in {
    drop(List("a", "b", "c"), 10) should equal(List())
  }

  it should "return rest of the list after removing first n elements" in {
    drop(List("a", "b", "c"), 2) should equal(List("c"))
  }

  it should "return same list when dropping zero elements" in {
    drop(List("a", "b", "c"), 0) should equal(List("a", "b", "c"))
  }
}

class DropWhileTest extends FlatSpec with ShouldMatchers {
  "DropWhile" should "always return empty List when applied to empty list" in {
    dropWhile(List()) (_ => false) should equal(List())
  }

  it should "return empty list when using an always true predicate" in {
    dropWhile(List("a", "b", "c")) (_ => true) should equal(List())
  }

  it should "return same list when using an always false predicate" in {
    dropWhile(List("a", "b", "c")) (_ => false) should equal(List("a", "b", "c"))
  }

  it should "return rest of the list after finding an element NOT matching the predicate" in {
    dropWhile(List("a", "a", "aa", "c", "ccc", "c")) (_.length < 2) should equal(List("aa", "c", "ccc", "c"))
  }
}

class SetHeadTest extends FlatSpec with ShouldMatchers {
  "SetHead" should "return the new element as list when applied to empty list" in {
    setHead[String](List())("a") should equal(List("a"))
  }

  it should "replace the head of the list" in {
    setHead(List("a", "b", "c"))("a1") should equal(List("a1", "b", "c"))
  }
}

class InitTest extends FlatSpec with ShouldMatchers {
  "Init" should "return empty List when applied to empty list" in {
    init(List()) should equal(List())
  }

  it should "return empty list when applied to single item list" in {
    init(List("a")) should equal(Nil)
  }

  it should "return all but last element of the list when applied to a multi item list" in {
    init(List("a", "b", "c")) should equal(List("a", "b"))
  }
}

class LengthTest extends FlatSpec with ShouldMatchers {
  "Length" should "return 0 for empty List" in {
    List.length(List()) should equal(0)
  }

  it should "return 1 when applied to single item list" in {
    List.length(List("a")) should equal(1)
  }

  it should "return the count of all element of the list when applied to a multi item list" in {
    List.length(List("a", "b", "c")) should equal(3)
  }
}

class FoldLeftTest extends FlatSpec with ShouldMatchers {
  "FoldLeft" should "apply to all elements in list" in {
    foldLeft(List(1,2,3), 0)(_ + _) should equal(6)
  }

  it should "use the start value supplied" in {
    foldLeft(List(1,2,3), 1)(_ + _) should equal(7)
  }

  it should "return start value for empty lis" in {
    foldLeft[Int, Int](List(), 1)(_ + _) should equal(1)
  }
}

class ReverseTest extends FlatSpec with ShouldMatchers {
  "Reverse" should "invert list elements" in {
    reverse(List(1,2,3)) should equal(List(3,2,1))
  }

  it should "return empty list applied to Nil" in {
    reverse(List()) should equal(List())
  }
}

class FoldLeft_rTest extends FlatSpec with ShouldMatchers {
  "FoldLeft built using fold right" should "apply to all elements in list" in {
    foldLeft_r(List(1,2,3), 0)(_ + _) should equal(6)
  }

  it should "use the start value supplied" in {
    foldLeft_r(List(1,2,3), 1)(_ + _) should equal(7)
  }

  it should "return start value for empty lis" in {
    foldLeft_r[Int, Int](List(), 1)(_ + _) should equal(1)
  }
}

class AppendWithFold extends FlatSpec with ShouldMatchers {
  "Append With Fold" should "add all elements of second list to first" in {
    append_fold(List(1, 2, 3), List(4, 5, 6, 7)) should equal(List(1, 2, 3, 4, 5, 6, 7))
  }

  it should "return second list if first is empty" in {
    append_fold(List(), List(4, 5, 6, 7)) should equal(List(4, 5, 6, 7))
  }

  it should "return first list if first is empty" in {
    append_fold(List(4, 5, 6, 7), List()) should equal(List(4, 5, 6, 7))
  }
}

class ConcatListOfListTest extends FlatSpec with ShouldMatchers {
  "concatListOfList" should "merge all lists into a single long one" in {
    concatListOfList(List(List(1,2,3), List(4,5,6), List(7,8,9))) should equal(List(1,2,3,4,5,6,7,8,9))
  }

  it should "ignore empty list"  in {
    concatListOfList(List(List(1,2,3), Nil, List(7,8,9))) should equal(List(1,2,3,7,8,9))
  }
}

class AddOneToListTest extends FlatSpec with ShouldMatchers {
  "addOneToList" should "return incremented list" in {
    addOneToList(List(1, 2, 3)) should equal(List(2, 3, 4))
  }
}

class MapTest extends FlatSpec with ShouldMatchers {
  "Map" should "apply function to every element in the list returning list with transformed elements" in {
    map(List(1, 2, 3))(_ + 1) should equal(List(2, 3, 4))
  }
}

class FilterTest extends FlatSpec with ShouldMatchers {
  "Filter" should "return a list with only elements satisfying the predicate" in {
    filter(List(3,2,7,10,7,2,11,1,0))(_>2) should equal(List(3,7,10,7,11))
  }

  it should "return empty list if applied to empty list" in {
    filter(List())(a => true) should equal(List())
  }
}

class FlatMapTest extends FlatSpec with ShouldMatchers {
  "FlatMap" should "apply function to every element in the list returning a single list with concat of results" in {
    flatMap(List('a', 'b', 'c'))(a => List(a, a)) should equal(List('a', 'a', 'b', 'b', 'c', 'c'))
  }
}

class FilterWithFlatMapTest extends FlatSpec with ShouldMatchers {
  "Filter with flat map" should "return a list with only elements satisfying the predicate" in {
    filterWithFlatMap(List(3,2,7,10,7,2,11,1,0))(_>2) should equal(List(3,7,10,7,11))
  }

  it should "return empty list if applied to empty list" in {
    filterWithFlatMap(List())(a => true) should equal(List())
  }
}

class ComposeListsTest extends FlatSpec with ShouldMatchers {
  "composeLists" should "compose every element in the list with the provided function" in {
    composeLists(List(1,2,3), List(4,5,6))(_+_) should equal(List(5,7,9))
  }

  it should "not combine elements in left list if the list is longer than the right provided one" in {
    composeLists(List(1,2,3,7,8), List(4,5,6))(_+_) should equal(List(5,7,9))
  }

  it should "not combine elements in right list if the list is longer than the left provided one" in {
    composeLists(List(1,2,3), List(4,5,6,7,8))(_+_) should equal(List(5,7,9))
  }
}

class HasSubsequenceTest extends FlatSpec with ShouldMatchers {
  "hasSubsequence" should "return true if subsequence" in {
    hasSubsequence(List(1,2,3), List(2,3)) should be(true)
  }

  it should "return false if not subsequence" in {
    hasSubsequence(List(1,2,3), List(2,4)) should be(false)
  }

  it should "return false if not subsequence but might look like" in {
    hasSubsequence(List(1,2,3), List(2,3,4)) should be(false)
  }

  it should "return true if subsequence is an empty list" in {
    hasSubsequence(List(1,2,3), Nil) should be(true)
  }

  it should "return false if subsequence is longer than original" in {
    hasSubsequence(List(1,2,3), List(1,2,3,4)) should be(false)
  }
}