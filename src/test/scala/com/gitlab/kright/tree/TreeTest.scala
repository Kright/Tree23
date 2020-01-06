package com.gitlab.kright.tree

import scala.collection.mutable.ArrayBuffer

class TreeTest extends org.scalatest.FunSuite {

  test("empty tree") {
    assert(Tree.empty.size == 0)
    assert(Tree.empty.depth == 0)
    assert(Tree.empty.isEmpty)
    assert(Tree.empty[Int].toArray.sameElements(Array[Int]()))
    assert(Tree.empty[Int].remove(0).toArray.sameElements(Array[Int]()))
  }

  test("one-node tree") {
    assert(Tree(1).size == 1)
    assert(Tree(1).depth == 1)
    assert(Tree(1).nonEmpty)
    assert(Tree(1).toArray.sameElements(Array(1)))
    assert(Tree(1).remove(0).toArray.sameElements(Array(1)))
    assert(Tree(1).remove(1).toArray.sameElements(Array[Int]()))
  }

  test("several insertions") {
    for (range <- Seq(1 to 10, 10 to 1 by -1, (1 to 12).map(_ * 7 % 13))) {
      var tree: Tree[Int] = Tree.empty
      val summ = new ArrayBuffer[Int]()

      for (i <- range) {
        tree = tree.insert(i)
        summ += i

        val expected = summ.distinct.sorted.toArray

        assert(tree.toArray[Int] sameElements expected.sorted,
          s"expected: ${expected.mkString(", ")}, found: ${tree.toArray.mkString(", ")}")

        assert(tree.min == expected.min)
        assert(tree.max == expected.max)
      }
    }
  }

}
