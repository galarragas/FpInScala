package fpinscala.datastructures

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import fpinscala.datastructures.Tree._

class TreeSizeTest extends FlatSpec with ShouldMatchers {
  "Size" should "return 1 for a leaf" in {
    Tree.size(Leaf("ciao")) should equal(1)
  }

  it should "return the 1 + number of not null leaves for a simple branch" in {
    Tree.size(Branch(Leaf("ciao"), Leaf("hello"))) should equal(3)
    Tree.size(Branch(Leaf("ciao"), null)) should equal(2)
  }

  it should "count the total number of nodes for a complex tree" in {
    val theTree =
      Branch(
        Branch(
          Leaf("leaf1"),
          Leaf("leaf2")
        ),
        Branch(
          Branch(
            Leaf("leaf3"),
            null
          ),
          Leaf("Leaf4")
        )
      )
    Tree.size(theTree) should equal(8)
  }
}

class TreeWithFoldSizeTest extends FlatSpec with ShouldMatchers {
  "Size with FOLD" should "return 1 for a leaf" in {
    Tree.sizeWithFold(Leaf("ciao")) should equal(1)
  }

  it should "return the 1 + number of not null leaves for a simple branch" in {
    Tree.sizeWithFold(Branch(Leaf("ciao"), Leaf("hello"))) should equal(3)
    Tree.sizeWithFold(Branch(Leaf("ciao"), null)) should equal(2)
  }

  it should "count the total number of nodes for a complex tree" in {
    val theTree =
      Branch(
        Branch(
          Leaf("leaf1"),
          Leaf("leaf2")
        ),
        Branch(
          Branch(
            Leaf("leaf3"),
            null
          ),
          Leaf("Leaf4")
        )
      )
    Tree.sizeWithFold(theTree) should equal(8)
  }
}

class MaximumTest extends FlatSpec with ShouldMatchers {
  "Maximum" should "return the value of the leaf for a leaf" in {
    maximum(Leaf(5)) should equal(5)
  }

  it should "return the max value of the not null leaves for a simple branch" in {
    maximum(Branch(Leaf(4), Leaf(9))) should equal(9)
    maximum(Branch(null, Leaf(3))) should equal(3)
  }

  it should "return 0 for a branch without leaves" in {
    maximum(Branch(null, null)) should equal(0)
  }

  it should "return the max value of the leaves in the tree" in {
    val theTree =
      Branch(
        Branch(
          Leaf(2),
          Leaf(9)
        ),
        Branch(
          Branch(
            Leaf(3),
            null
          ),
          Branch(
            Leaf(4),
            null
          )
        )
      )
    maximum(theTree) should equal(9)
  }
}

class MaximumWithFoldTest extends FlatSpec with ShouldMatchers {
  "Maximum with FOLD" should "return the value of the leaf for a leaf" in {
    maxWithFold(Leaf(5)) should equal(5)
  }

  it should "return the max value of the not null leaves for a simple branch" in {
    maxWithFold(Branch(Leaf(4), Leaf(9))) should equal(9)
    maxWithFold(Branch(null, Leaf(3))) should equal(3)
  }

  it should "return 0 for a branch without leaves" in {
    maxWithFold(Branch(null, null)) should equal(0)
  }

  it should "return the max value of the leaves in the tree" in {
    val theTree =
      Branch(
        Branch(
          Leaf(2),
          Leaf(9)
        ),
        Branch(
          Branch(
            Leaf(3),
            null
          ),
          Branch(
            Leaf(4),
            null
          )
        )
      )
    maxWithFold(theTree) should equal(9)
  }
}

class DepthTest extends FlatSpec with ShouldMatchers {
  "Depth" should "return 0 for a leaf" in {
    depth(Leaf(5)) should equal(0)
  }

  it should "return 1 for a simple branch" in {
    depth(Branch(Leaf(4), Leaf(9))) should equal(1)
    depth(Branch(null, Leaf(3))) should equal(1)
  }

  it should "return the max distance to leaves in the tree" in {
    val theTree =
      Branch(
        Branch(
          Leaf(2),
          Leaf(9)
        ),
        Branch(
          Branch(
            Leaf(3),
            Branch(
              Leaf(3),
              Branch(
                null,
                Leaf(1)
              )
            )
          ),
          Branch(
            Leaf(4),
            null
          )
        )
      )
    depth(theTree) should equal(5)
  }
}

class DepthWithFoldTest extends FlatSpec with ShouldMatchers {
  "Depth with FOLD" should "return 0 for a leaf" in {
    depthWithFold(Leaf(5)) should equal(0)
  }

  it should "return 1 for a simple branch" in {
    depthWithFold(Branch(Leaf(4), Leaf(9))) should equal(1)
    depthWithFold(Branch(null, Leaf(3))) should equal(1)
  }

  it should "return the max distance to leaves in the tree" in {
    val theTree =
      Branch(
        Branch(
          Leaf(2),
          Leaf(9)
        ),
        Branch(
          Branch(
            Leaf(3),
            Branch(
              Leaf(3),
              Branch(
                null,
                Leaf(1)
              )
            )
          ),
          Branch(
            Leaf(4),
            null
          )
        )
      )
    depthWithFold(theTree) should equal(5)
  }
}

class TreeMapTest extends FlatSpec with ShouldMatchers {
  "Map" should "transform a leaf into another with content from the application of the function to the origin leaf content" in {
    map(Leaf("ciccio"))(_.length) should equal(Leaf(6))
  }

  it should "transform a branch into another one with mapped leaves" in {
    map(Branch(Leaf("uno"), Leaf("zero")))(_.length) should equal(Branch(Leaf(3), Leaf(4)))
  }

  it should "map null leaf to null" in {
    map(Branch(null, Leaf("zero")))(_.length) should equal(Branch(null, Leaf(4)))
  }

  it should "map complex trees" in {
    val theTree =
      Branch(
        Branch(
          Leaf(2),
          Leaf(9)
        ),
        Branch(
          Branch(
            Leaf(3),
            Branch(
              Leaf(3),
              Branch(
                null,
                Leaf(1)
              )
            )
          ),
          Branch(
            Leaf(4),
            null
          )
        )
      )

    val transformed =
      Branch(
        Branch(
          Leaf("2"),
          Leaf("9")
        ),
        Branch(
          Branch(
            Leaf("3"),
            Branch(
              Leaf("3"),
              Branch(
                null,
                Leaf("1")
              )
            )
          ),
          Branch(
            Leaf("4"),
            null
          )
        )
      )
    map(theTree)(_.toString) should equal(transformed)
  }
}

class TreeMapWithFoldTest extends FlatSpec with ShouldMatchers {
  "Map with FOLD" should "transform a leaf into another with content from the application of the function to the origin leaf content" in {
    mapWithFold(Leaf("ciccio"))(_.length) should equal(Leaf(6))
  }

  it should "transform a branch into another one with mapped leaves" in {
    map(Branch(Leaf("uno"), Leaf("zero")))(_.length) should equal(Branch(Leaf(3), Leaf(4)))
  }

  it should "map null leaf to null" in {
    mapWithFold(Branch(null, Leaf("zero")))(_.length) should equal(Branch(null, Leaf(4)))
  }

  it should "map complex trees" in {
    val theTree =
      Branch(
        Branch(
          Leaf(2),
          Leaf(9)
        ),
        Branch(
          Branch(
            Leaf(3),
            Branch(
              Leaf(3),
              Branch(
                null,
                Leaf(1)
              )
            )
          ),
          Branch(
            Leaf(4),
            null
          )
        )
      )

    val transformed =
      Branch(
        Branch(
          Leaf("2"),
          Leaf("9")
        ),
        Branch(
          Branch(
            Leaf("3"),
            Branch(
              Leaf("3"),
              Branch(
                null,
                Leaf("1")
              )
            )
          ),
          Branch(
            Leaf("4"),
            null
          )
        )
      )
    mapWithFold(theTree)(_.toString) should equal(transformed)
  }
}
