package com.gitlab.kright

import scala.collection.{AbstractIterator, immutable, mutable}
import scala.reflect.ClassTag


sealed trait Tree[+T] extends immutable.Seq[T] {
  self =>

  def depth: Int

  def length: Int

  override def toArray[U >: T: ClassTag]: Array[U] = {
    val builder = mutable.ArrayBuilder.make[U]
    foreach(builder.addOne)
    builder.result()
  }

  override def foreach[U](func: T => U): Unit =
    this match {
      case Empty =>
      case Tree2(left, value, right) =>
        left.foreach(func)
        func(value)
        right.foreach(func)
      case Tree3(left, midLeftValue, middle, midRightValue, right) =>
        left.foreach(func)
        func(midLeftValue)
        middle.foreach(func)
        func(midRightValue)
        right.foreach(func)
    }

  def insert[U >: T](elem: U)(implicit ord: Ordering[U]): Tree[U] =
    innerInsert(elem) match {
      case Left(tree) => tree
      case Right((left, midValue, right)) => Tree2(left, midValue, right)
    }

  def contains[U >: T](elem: U)(implicit ord: Ordering[U]): Boolean = this match {
    case Empty => false
    case Tree2(left, value, right) =>
      ord.compare(elem, value) match {
        case s if s < 0 => left.contains(elem)
        case s if s == 0 => true
        case s if s > 0 => right.contains(elem)
      }
    case Tree3(left, midLeftValue, middle, midRightValue, right) =>
      ord.compare(elem, midLeftValue) match {
        case s if s < 0 => left.contains(elem)
        case s if s == 0 => true
        case s if s > 0 =>
          ord.compare(elem, midRightValue) match {
            case s if s < 0 => middle.contains(elem)
            case s if s == 0 => true
            case s if s > 0 => right.contains(elem)
          }
      }
  }

  def apply(i: Int): T = {
    assert(i < size)
    assert(i >= 0)
    this match {
      case Empty => ???
      case Tree2(left, value, right) =>
        if (i < left.size)
          left(i)
        else if (i == left.size)
          value
        else
          right(i - left.size - 1)
      case Tree3(left, midLeftValue, middle, midRightValue, right) =>
        if (i < left.size) {
          left(i)
        } else if (i == left.size) {
          midLeftValue
        } else {
          val i2 = i - left.size - 1
          if (i2 < middle.size) {
            middle(i2)
          } else {
            if (i2 == middle.size) {
              midRightValue
            } else {
              right(i - middle.size - 1)
            }
          }
        }
    }
  }

  def iterator: Iterator[T] = {
    new AbstractIterator[T] {
      private var pos = 0

      override def hasNext: Boolean = pos < self.length

      override def next(): T = {
        val elem = self(pos)
        pos += 1
        elem
      }
    }
  }

  private def innerInsert[U >: T](elem: U)(implicit ord: Ordering[U]): Either[Tree[U], (Tree[U], U, Tree[U])] = {
    type TU = Tree[U]

    def tree2(t0: TU, v0: U, t1: TU) =
      Left(Tree2(t0, v0, t1))

    def tree3(t0: TU, v0: U, t1: TU, v1: U, t2: TU) =
      Left(Tree3(t0, v0, t1, v1, t2))

    def tree4(t0: TU, v0: U, t1: TU, v1: U, t2: TU, v2: U, t3: TU) =
      Right(Tree2(t0, v0, t1), v1, Tree2(t2, v2, t3))

    this match {
      case Empty => Right((Empty, elem, Empty))

      case t@Tree2(left, midValue, right) =>
        ord.compare(elem, midValue) match {
          case s if s < 0 =>
            left.innerInsert(elem) match {
              case Left(tree) => tree2(tree, midValue, right)
              case Right((lt, v, rt)) => tree3(lt, v, rt, midValue, right)
            }
          case s if s == 0 => Left(t)
          case s if s > 0 =>
            right.innerInsert(elem) match {
              case Left(tree) => tree2(left, midValue, tree)
              case Right((lt, v, rt)) => tree3(left, midValue, lt, v, rt)
            }
        }

      case t@Tree3(left, midLeftValue, middle, midRightValue, right) =>
        ord.compare(elem, midLeftValue) match {
          case s if s < 0 =>
            left.innerInsert(elem) match {
              case Left(tree) => tree3(tree, midLeftValue, middle, midRightValue, right)
              case Right((lt, v, rt)) => tree4(lt, v, rt, midLeftValue, middle, midRightValue, right)
            }
          case s if s == 0 => Left(t)
          case s if s > 0 =>
            ord.compare(elem, midRightValue) match {
              case s if s < 0 =>
                middle.innerInsert(elem) match {
                  case Left(tree) => tree3(left, midLeftValue, tree, midRightValue, right)
                  case Right((lt, v, rt)) => tree4(left, midLeftValue, lt, v, rt, midRightValue, right)
                }
              case s if s == 0 => Left(t)
              case s if s > 0 =>
                right.innerInsert(elem) match {
                  case Left(tree) => tree3(left, midLeftValue, middle, midRightValue, tree)
                  case Right((lt, v, rt)) => tree4(left, midLeftValue, middle, midRightValue, lt, v, rt)
                }
            }
        }

    }
  }
}

case object Empty extends Tree[Nothing] {
  override def depth: Int = 0

  override def length: Int = 0
}

case class Tree2[T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T] {
  assert(left.depth == right.depth)

  override val depth: Int = left.depth + 1

  override val length: Int = left.size + 1 + right.size
}

case class Tree3[T](left: Tree[T], midLeftValue: T, middle: Tree[T], midRightValue: T, right: Tree[T]) extends Tree[T] {
  assert(left.depth == middle.depth && middle.depth == right.depth)

  override val depth: Int = 1 + left.depth

  override val length: Int = 2 + left.size + middle.size + right.size
}

object Tree {
  def empty[T]: Tree[T] = Empty

  def apply[T](t: T): Tree[T] = Tree2(Empty, t, Empty)
}
