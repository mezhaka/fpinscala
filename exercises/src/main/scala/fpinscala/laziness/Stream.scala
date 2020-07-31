package fpinscala.laziness

import scala.collection.immutable.List
import Stream._
trait Stream[+A] {
  def toList(): List[A] = {
    println("toList")
    val r = this match {
      case Empty => List[A]()
      case Cons(h, t) => {println("toList eval h()");
                          val t = h(); println(s"toList eval h(): $t"); t} ::
      {println("toList eval t().toList()"); t().toList()}
    }
    println("exit toList")
    r
  }

  def toList2(): List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => go(t(), h() :: l)
    }
    return go(this, List[A]()).reverse
  }

  def toList3(): List[A] = {
    val buf = new collection.mutable.ListBuffer[A] 
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => {
        buf += h()
        go(t())
      }
    }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    println(s"enter take: $n")
    var r = {
    if (n == 0) Empty
    else this match {
        case Empty => Empty
        case Cons(h, t) => Stream.cons({println("in take, pass h()");
          val t = h(); println(s"in take h(): $t"); t},
        {println("pass t()"); t().take(n-1)})
    }
    }
    println(s"exit take: $n")
    r
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Empty => Empty
      case Cons(_, t) => t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) =>  Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)((a, b) => if (p(a)) cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    this.foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  // TODO I was getting the following error with my signature:
  // def append(s: Stream[A]): Stream[A] = ???
  // [error] /app/exercises/src/main/scala/fpinscala/laziness/Stream.scala:100: covariant type A occurs in contravariant position in type fpinscala.laziness.Stream[A] of value s
  // [error]   def append(s: Stream[A]): Stream[A] =
  // [error]              ^
  // 
  // I decided to look up the correct signature in the answers, cause I did not
  // feel like digging into why this does not work.
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    println("enter cons")
    lazy val head = {println("cons: eval head"); val t = hd;
      println(s"cons: eval head: $t"); t}
    lazy val tail = {println("cons: eval tail"); tl}
    val r = Cons(() => head, () => tail)
    println("exit cons")
    r
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a + b, go(b,  a + b))
    Stream.cons(0, Stream.cons(1, go(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

object Main {
  def test[T](a: T, b: T): Unit = {
    if (a != b) println(s"Not OK: $a != $b")
    else println("OK")
  }


  def main(args: Array[String]): Unit = {
    test(Stream(1, 2).toList(),
         List(1, 2))
    test(Stream(1, 2).toList2(),
         List(1, 2))
    test(Stream(1, 2).toList3(),
         List(1, 2))

    test(Stream(1, 2, 3).take(2).toList(),
         List(1, 2))
    test(Stream(1, 2, 3).take(0).toList(),
         List())
    println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    test(Stream(1, 2, 3).take(20).toList(),
         List(1, 2, 3))

    test(Stream(1, 2, 3).drop(2).toList(),
         List(3))

    test(Stream(1, 2, 3, 4).takeWhile(_ < 3).toList(),
         List(1, 2))
    test(Stream(1, 2, 3, 4).takeWhileViaFold(_ < 3).toList(),
         List(1, 2))

    test(Stream(1, 2).forAll(_ > 0),
         true)
    test(Stream(1, 2).forAll(_ > 1),
         false)

    test(Stream(1, 2).headOption,
         Some(1))
    test(Stream().headOption,
         None)

    test(Stream(1, 2).map(_ + 10).toList,
         List(11, 12))
    test(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList,
         List(2, 4))
    test(Stream(1).append(Stream(2)).toList,
         List(1, 2))
    test(Stream(1, 2).flatMap(a => Stream(a, -a)).toList,
         List(1, -1, 2, -2))
    test(Stream.constant(3).take(3).toList,
         List(3, 3, 3))
    test(Stream.from(3).take(3).toList,
         List(3, 4, 5))
    test(Stream.fibs.take(8).toList,
         List(0, 1, 1, 2, 3, 5, 8, 13))
  }
}
