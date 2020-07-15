package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    // The answer is more elegant:
    // case Branch(l, r) => 1 (depth(l) max  depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(lf: A => B)(bf: (B, B)  => B): B = t match {
    case Leaf(a) => lf(a)
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def size2[A](t: Tree[A]): Int = 
    fold(t)(_ => 1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int =
  // I don't get why size2 can deduce parameters, while the following cannot:
  // fold(t)(_)(_ max _)
    fold(t)((a: Int) => a)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    //fold(t)((a: Int) => 1)(1 + (_ max _))
    fold(t) (_ => 1) ((b1: Int, b2: Int) => 1 + (b1 max b2))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    def leaf(a: A): Tree[B] = Leaf(f(a))
    fold(t) (leaf) (Branch(_, _))
    // I cannot understand why the following gives me a type mismatch:
    //fold(t) (Leaf(f(_))) (Branch(_, _))
  }
}

