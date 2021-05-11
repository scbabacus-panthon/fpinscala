package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](node : Tree[A]):Int =
    node match{
    case Branch(l,r) =>size(l) + size(r) + 1
    case Leaf(x) => 1
  }
  def maximum(node : Tree[Int]):Int =
    node match{
      case Branch(l,r) => maximum(l) max maximum(r)
      case Leaf(x) => x
    }

  def depth[A](node : Tree[A]):Int =
    node match{
      case Branch(l,r) => (depth(l) max depth(r)) + 1
      case Leaf(x) => 1
    }

  def map[A,B](node : Tree[A])(f: A => B): Tree[B] =
    node match{
      case Branch(l,r) => Branch(map(l)(f),map(r)(f))
      case Leaf(x) => Leaf(f(x))
    }

  def fold[A,B](node: Tree[A])(g: A=> B)(f: (B,B) => B): B =
    node match{
      case Branch(l,r) => f(fold(l)(f),fold(r)(f))
      case Leaf(x) => g(x)
    }
  def size2[A,B](node : Tree[A]):Int =
    fold(node)(x => 1)(1+_+_)
  def maximum2[A,B](node: Tree[Int]): Int =
    fold(node)(x => x)(_ max _)
  def depth2[A,B](node : Tree[A]):Int =
    fold(node)(x => 0)((A,B) => (A max B) + 1)
  def map2[A,B](node : Tree[A])(f: A => B): Tree[B] =
    fold(node)(x => Leaf(f(x)):Tree[B])(Branch(_,_))




}