package com.gitlab.kright.tree23

import scala.collection._
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractSet, Set, SortedSet, SortedSetOps, StrictOptimizedSortedSetOps}

class TreeSortedSet[A](val tree: Tree[A])(implicit val ord: Ordering[A])
  extends AbstractSet[A]
    with SortedSet[A]
    with SortedSetOps[A, TreeSortedSet, TreeSortedSet[A]]
    with StrictOptimizedSortedSetOps[A, TreeSortedSet, TreeSortedSet[A]]
    with SortedSetFactoryDefaults[A, TreeSortedSet, Set]
    with DefaultSerializable {

  override def iteratorFrom(start: A): Iterator[A] = iterator.dropWhile(ord.lt(_, start))

  override def incl(elem: A): TreeSortedSet[A] = new TreeSortedSet(tree.insert(elem))

  override def excl(elem: A): TreeSortedSet[A] = new TreeSortedSet(tree.remove(elem))

  override def contains(elem: A): Boolean = tree.contains(elem)

  override def iterator: Iterator[A] = tree.iterator

  override def ordering: Ordering[A] = ord

  override def rangeImpl(from: Option[A], until: Option[A]): TreeSortedSet[A] =
    filter(elem =>
      !from.exists(fr => ord.gt(fr, elem)) &&
        !until.exists(unt => ord.lt(unt, elem))
    )

  override def sortedIterableFactory: SortedIterableFactory[TreeSortedSet] = TreeSortedSet
}

object TreeSortedSet extends SortedIterableFactory[TreeSortedSet] {
  def empty[T: Ordering] = new TreeSortedSet(Tree.empty[T])

  def apply[T: Ordering](t: T) = new TreeSortedSet(Tree(t))

  override def from[E](it: IterableOnce[E])(implicit ord: Ordering[E]): TreeSortedSet[E] = {
    var tree = Tree.empty[E]
    for (elem <- it) {
      tree = tree.insert(elem)
    }
    new TreeSortedSet(tree)
  }

  override def newBuilder[A](implicit ord: Ordering[A]): mutable.Builder[A, TreeSortedSet[A]] =
    new mutable.Builder[A, TreeSortedSet[A]] {
      private var tree = Tree.empty[A]

      override def clear(): Unit = {
        tree = Tree.empty
      }

      override def result(): TreeSortedSet[A] = new TreeSortedSet[A](tree)

      override def addOne(elem: A): this.type = {
        tree = tree.insert(elem)
        this
      }
    }
}
