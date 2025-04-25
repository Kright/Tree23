package com.gitlab.kright.tree23

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class TreeSortedSetTest extends AnyFunSuite {

  test("empty set") {
    assert(TreeSortedSet.empty[Int].size == 0)
    assert(TreeSortedSet.empty[Int].isEmpty)
    assert(TreeSortedSet.empty[Int].toArray.sameElements(Array[Int]()))
    assert(TreeSortedSet.empty[Int].excl(0).toArray.sameElements(Array[Int]()))
  }

  test("one-element set") {
    assert(TreeSortedSet(1).size == 1)
    assert(TreeSortedSet(1).nonEmpty)
    assert(TreeSortedSet(1).toArray.sameElements(Array(1)))
    assert(TreeSortedSet(1).excl(0).toArray.sameElements(Array(1)))
    assert(TreeSortedSet(1).excl(1).toArray.sameElements(Array[Int]()))
  }

  test("several insertions") {
    for (range <- Seq(1 to 10, 10 to 1 by -1, (1 to 12).map(_ * 7 % 13))) {
      var tree = TreeSortedSet.empty[Int]
      val summ = new ArrayBuffer[Int]()

      for (i <- range) {
        tree = tree.incl(i)
        summ += i

        val expected = summ.distinct.sorted.toArray
        assertSame(tree, expected)

        assert(tree.min == expected.min)
        assert(tree.max == expected.max)
      }
    }
  }

  def assertSame[T](tree: TreeSortedSet[T], seq: Seq[T], msg: => String = "")(implicit tag: ClassTag[T], ord: Ordering[T]) = {
    val expected = seq.distinct.sorted
    assert(tree.toArray.sameElements(expected),
      s"expected: ${expected.mkString("[", ", ", "]")}, found: ${tree.toArray.mkString("[", ", ", "]")}: $msg")
  }

  test("insertions and removes") {
    def swap(arr: Array[Int], i: Int, j: Int) = {
      val t = arr(i)
      arr(i) = arr(j)
      arr(j) = t
    }

    def check(arr: Array[Int]) = {
      var tree = TreeSortedSet.empty[Int]
      arr.zipWithIndex.foreach { case (elem, index) =>
        tree = tree.incl(elem)
        assertSame(tree, arr.take(index + 1))
      }

      assertSame(tree, arr)
      arr.zipWithIndex.foreach { case (elem, index) =>
        assert(tree.nonEmpty)
        val newTree = tree.excl(elem)
        assertSame(newTree, arr.drop(index + 1), s"$tree => $newTree")
        tree = newTree
      }
      assert(tree.isEmpty)
    }

    def walk(arr: Array[Int], pos: Int): Unit = {
      if (pos < arr.size) {
        for (i <- pos until arr.size) {
          swap(arr, pos, i)
          walk(arr, pos + 1)
          swap(arr, pos, i)
        }
      } else {
        check(arr)
      }
    }

    val elems = (1 to 9).toArray

    walk(elems, 0)
  }

}
