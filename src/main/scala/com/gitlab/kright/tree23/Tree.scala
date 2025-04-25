package com.gitlab.kright.tree23

import scala.collection.{AbstractIterator, immutable, mutable}
import scala.reflect.ClassTag


sealed trait Tree[+T] extends immutable.Seq[T] {
  self =>

  def depth: Int

  def length: Int

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val builder = mutable.ArrayBuilder.make[U]
    foreach(builder.addOne)
    builder.result()
  }

  override def isEmpty: Boolean =
    this match {
      case Empty => true
      case _ => false
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
    assert(i < size, s"wrong index: $i, but size is $size")
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
              right(i2 - middle.size - 1)
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

    def makeRight(t0: TU, v0: U, t1: TU, v1: U, t2: TU, v2: U, t3: TU) =
      Right(Tree2(t0, v0, t1), v1, Tree2(t2, v2, t3))

    this match {
      case Empty => Right((Empty, elem, Empty))

      case t@Tree2(left, midValue, right) => Left {
        ord.compare(elem, midValue) match {
          case s if s < 0 =>
            left.innerInsert(elem) match {
              case Left(tree) => unsafeTree(tree, midValue, right)
              case Right((lt, v, rt)) => unsafeTree(lt, v, rt, midValue, right)
            }
          case 0 => t
          case s if s > 0 =>
            right.innerInsert(elem) match {
              case Left(tree) => unsafeTree(left, midValue, tree)
              case Right((lt, v, rt)) => unsafeTree(left, midValue, lt, v, rt)
            }
        }
      }

      case t@Tree3(left, midLeftValue, middle, midRightValue, right) =>
        ord.compare(elem, midLeftValue) match {
          case s if s < 0 =>
            left.innerInsert(elem) match {
              case Left(tree) => Left(unsafeTree(tree, midLeftValue, middle, midRightValue, right))
              case Right((lt, v, rt)) => makeRight(lt, v, rt, midLeftValue, middle, midRightValue, right)
            }
          case 0 => Left(t)
          case s if s > 0 =>
            ord.compare(elem, midRightValue) match {
              case s if s < 0 =>
                middle.innerInsert(elem) match {
                  case Left(tree) => Left(unsafeTree(left, midLeftValue, tree, midRightValue, right))
                  case Right((lt, v, rt)) => makeRight(left, midLeftValue, lt, v, rt, midRightValue, right)
                }
              case 0 => Left(t)
              case s if s > 0 =>
                right.innerInsert(elem) match {
                  case Left(tree) => Left(unsafeTree(left, midLeftValue, middle, midRightValue, tree))
                  case Right((lt, v, rt)) => makeRight(left, midLeftValue, middle, midRightValue, lt, v, rt)
                }
            }
        }

    }
  }

  override def min[U >: T](implicit ord: Ordering[U]): T =
    this match {
      case Tree2(left, value, _) =>
        if (left.depth > 0) left.min[U] else value
      case Tree3(left, value, _, _, _) =>
        if (left.depth > 0) left.min[U] else value
      case Empty =>
        throw new UnsupportedOperationException("empty.min")
    }

  override def max[U >: T](implicit ord: Ordering[U]): T =
    this match {
      case Tree2(_, value, right) =>
        if (right.depth > 0) right.max[U] else value
      case Tree3(_, _, _, value, right) =>
        if (right.depth > 0) right.max[U] else value
      case Empty =>
        throw new UnsupportedOperationException("empty.max")
    }

  private def unsafeTree[A](t0: Tree[A], v0: A, t1: Tree[A]): Tree[A] =
    Tree2(t0, v0, t1)

  private def unsafeTree[A](t0: Tree[A], v0: A, t1: Tree[A], v1: A, t2: Tree[A]): Tree[A] =
    Tree3(t0, v0, t1, v1, t2)

  private def unsafeTree[A](t0: Tree[A], v0: A, t1: Tree[A], v1: A, t2: Tree[A], v2: A, t3: Tree[A]): Tree[A] =
    Tree2(Tree2(t0, v0, t1), v1, Tree2(t2, v2, t3))

  private def innerFixLeft[A](left: Tree[A], value: A, right: Tree[A]): Tree[A] =
    if (left.depth == right.depth) {
      Tree2(left, value, right)
    } else {
      right match {
        case Tree2(rl, rv, rr) => unsafeTree(left, value, rl, rv, rr)
        case Tree3(rl, rlv, rm, rrv, rr) => unsafeTree(left, value, rl, rlv, rm, rrv, rr)
        case Empty => ??? // impossible
      }
    }

  private def innerFixRight[A](left: Tree[A], value: A, right: Tree[A]): Tree[A] =
    if (left.depth == right.depth) {
      Tree2(left, value, right)
    } else {
      left match {
        case Tree2(ll, lv, lr) => unsafeTree(ll, lv, lr, value, right)
        case Tree3(ll, llv, lm, lrv, lr) => unsafeTree(ll, llv, lm, lrv, lr, value, right)
        case Empty => ??? // impossible
      }
    }

  private def innerRemoveMax[U >: T](implicit ord: Ordering[U]): (T, Tree[T]) = {
    val value = max[U]
    (value, innerRemove[U](value).get)
  }

  private def innerRemoveMin[U >: T](implicit ord: Ordering[U]): (T, Tree[T]) = {
    val value = min[U]
    (value, innerRemove[U](value).get)
  }

  private def innerFixLeft[A](left: Tree[A], midLeftValue: A, middle: Tree[A], midRightValue: A, right: Tree[A]): Tree[A] =
    if (left.depth == middle.depth) {
      unsafeTree(left, midLeftValue, middle, midRightValue, right)
    } else {
      assert(left.depth + 1 == middle.depth)
      middle match {
        case Tree2(ml, mv, mr) =>
          unsafeTree(unsafeTree(left, midLeftValue, ml, mv, mr), midRightValue, right)
        case Tree3(ml, mlv, mm, mrv, mr) =>
          unsafeTree(unsafeTree(left, midLeftValue, ml), mlv, unsafeTree(mm, mrv, mr), midRightValue, right)
        case Empty => ??? // impossible
      }
    }

  private def innerFixMiddle[A](left: Tree[A], midLeftValue: A, middle: Tree[A], midRightValue: A, right: Tree[A]): Tree[A] =
    if (middle.depth == left.depth) {
      unsafeTree(left, midLeftValue, middle, midRightValue, right)
    } else {
      left match {
        case Tree2(ll, lv, lr) =>
          unsafeTree(unsafeTree(ll, lv, lr, midLeftValue, middle), midRightValue, right)
        case Tree3(ll, llv, lm, lrv, lr) =>
          unsafeTree(unsafeTree(ll, llv, lm), lrv, unsafeTree(lr, midLeftValue, middle), midRightValue, right)
        case Empty => ??? // impossible
      }
    }

  private def innerFixRight[A](left: Tree[A], midLeftValue: A, middle: Tree[A], midRightValue: A, right: Tree[A]): Tree[A] =
    if (right.depth == middle.depth) {
      unsafeTree(left, midLeftValue, middle, midRightValue, right)
    } else {
      middle match {
        case Tree2(ml, mv, mr) =>
          unsafeTree(left, midLeftValue, unsafeTree(ml, mv, mr, midRightValue, right))
        case Tree3(ml, mlv, mm, mrv, mr) =>
          unsafeTree(left, midLeftValue, unsafeTree(ml, mlv, mm), mrv, unsafeTree(mr, midRightValue, right))
        case Empty => ??? // impossible
      }
    }

  def remove[U >: T](elem: U)(implicit ord: Ordering[U]): Tree[T] =
    this.innerRemove(elem).getOrElse(this)

  /** return new Tree or None if element isn't in tree */
  private def innerRemove[U >: T](elem: U)(implicit ord: Ordering[U]): Option[Tree[T]] =
    this match {
      case Empty => None
      case t@Tree2(left, value, right) =>
        t.getPos(elem) match {
          case 0 => left.innerRemove(elem).map(innerFixLeft(_, value, right))
          case 1 => Option {
            if (left.isEmpty) {
              right
            } else {
              val (leftMax, newLeft) = left.innerRemoveMax[U]
              innerFixLeft(newLeft, leftMax, right)
            }
          }
          case 2 => right.innerRemove(elem).map(innerFixRight(left, value, _))
        }
      case t@Tree3(left, midLeftValue, middle, midRightValue, right) =>
        t.getPos(elem) match {
          case 0 => left.innerRemove(elem).map(innerFixLeft(_, midLeftValue, middle, midRightValue, right))
          case 1 => Option {
            if (left.isEmpty) {
              unsafeTree(middle, midRightValue, right)
            } else {
              val (leftMax, newLeft) = left.innerRemoveMax[U]
              innerFixLeft(newLeft, leftMax, middle, midRightValue, right)
            }
          }
          case 2 => middle.innerRemove(elem).map(innerFixMiddle(left, midLeftValue, _, midRightValue, right))
          case 3 => Option {
            if (middle.isEmpty) {
              unsafeTree(left, midLeftValue, right)
            } else {
              val (midMax, newMid) = middle.innerRemoveMax[U]
              innerFixMiddle(left, midLeftValue, newMid, midMax, right)
            }
          }
          case 4 => right.innerRemove(elem).map(innerFixRight(left, midLeftValue, middle, midRightValue, _))
        }
    }

  def structureToString: String =
    this match {
      case Empty => "."
      case Tree2(left, value, right) =>
        s"(${left.structureToString}, $value, ${right.structureToString})"
      case Tree3(left, midLeftValue, middle, midRightValue, right) =>
        s"(${left.structureToString}, $midLeftValue, ${middle.structureToString}, $midRightValue, ${right.structureToString})"
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

  private[tree23] def getPos[U >: T](elem: U)(implicit ord: Ordering[U]): Int =
    ord.compare(elem, value) match {
      case s if s < 0 => 0
      case 0 => 1
      case s if s > 0 => 2
    }
}

case class Tree3[T](left: Tree[T], midLeftValue: T, middle: Tree[T], midRightValue: T, right: Tree[T]) extends Tree[T] {
  assert(left.depth == middle.depth && middle.depth == right.depth)

  override val depth: Int = 1 + left.depth

  override val length: Int = 2 + left.size + middle.size + right.size

  private[tree23] def getPos[U >: T](elem: U)(implicit ord: Ordering[U]): Int =
    ord.compare(elem, midLeftValue) match {
      case s if s < 0 => 0
      case 0 => 1
      case s if s > 0 =>
        ord.compare(elem, midRightValue) match {
          case s if s < 0 => 2
          case 0 => 3
          case s if s > 0 => 4
        }
    }
}

object Tree {
  def empty[T]: Tree[T] = Empty

  def apply[T](t: T): Tree[T] = Tree2(Empty, t, Empty)
}
