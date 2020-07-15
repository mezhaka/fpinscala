package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // I went to read about folds and there was an explanation of this reverse
  // trick.
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(List.reverse(as), z)((b: B, a: A) => f(a, b))

  def foldRight3[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    // I did peek into the answers.  The core thing to come up with this
    // solution (that I was strugling with) is that the result of the fold is a
    // callable function, not the value.  I was trying to come up with an
    // answer that would just be a call to foldLeft, which is not possible.
    // Things got better as soon as I got it that the return value is a
    // function of z.  Then I tried to write down a fold expression for a
    // relatively small list of 3 elements e1..e3:
    // f(e1, f(e2, f(e3, z))) and this is a function of z, which we call g3(z)
    // then define h2..h0:
    // g2(z) = f(e1, f(e2, z))
    // g1(z) = f(e1, z)
    // g0(z) = z -- this is a recursive bottom
    // looking at the definitions of gx I could see that
    // g3(z) = g2(f(e3, z))
    // g2(z) = g1(f(e2, z))
    // g1(z) = g0(f(e1, z))
    // and this demonstrates the recursive structure of each successive g
    // function.
    //
    // Why is it OK to leave out g type in this case?
    foldLeft(as, (b: B) => b) ((g, a) => b => g(f(a, b)) ) (z)


  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(List.reverse(l), z)((a: A, b: B) => f(b, a))

  def foldLeft3[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b) ((a, g) => (b => g(f(b, a))) ) (z)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Int]) = foldLeft(ns, 1)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(oldh, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => 1 + y)

  def length1[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => 1 + acc)

// I was stuck for a while with this stack trace until I realized I need to
// explicitly specify the type of Nil (is this how casting looks in Scala?),
// when passing to foldLeft -- I wonder why could it not be inferred, since Nil
// is a subclass of List[Nothing]...
//  [error] /app/exercises/src/main/scala/fpinscala/datastructures/List.scala:112: type mismatch;
//  [error]  found   : fpinscala.datastructures.Cons[A]
//  [error]  required: fpinscala.datastructures.Nil.type
//  [error]     foldLeft(l, Nil)((acc, h) => Cons(h, acc))
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))
    
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h, t) => Cons(h, t))

  // TODO Is it possible with foldLeft?
  //def append3[A](a1: List[A], a2: List[A]): List[A] =
    //foldLeft(a1, a2)((h, t) => Cons(h, t))

  // This is O(N) in memory...
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def transform[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), transform(t)(f))
  }

  def transform2[A, B](l: List[A])(f: A => B): List[B] =
    // I have again stepped into the same problem: I used Nil so the inferred
    // type was fpinscala.datastructures.Nil.type but later I use
    // fpinscala.datastructures.Cons[B] and I get this error:
    // 
    // [error] /app/exercises/src/main/scala/fpinscala/datastructures/List.scala:134: type mismatch;
    // [error]  found   : fpinscala.datastructures.Cons[B]
    // [error]  required: fpinscala.datastructures.Nil.type
    // [error]     foldRight(l, Nil)((a: A, b: List[B]) => Cons(f(a), b))
    // [
    // foldRight(l, Nil)((a: A, b: List[B]) => Cons(f(a), b))
    foldRight(l, List[B]())((a: A, b: List[B]) => Cons(f(a), b))

  def map[A,B](l: List[A])(f: A => B): List[B] = transform2(l)(f)

  def add1(l: List[Int]): List[Int] =
    transform2(l)(1 + _) 

  def toStr[A](l: List[A]): List[String] =
    transform2(l)(_.toString())

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_, t) => filter(t)(f)
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a: A, b: List[A]) => if (f(a)) Cons(a, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((a: A, b: List[B]) => List.append(f(a), b))

  def filter3[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a: A) => if (f(a)) List(a) else List[A]())

  // I peeked in the solution cause I did not know how to match on several
  // parameters at the same time
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (l1, Nil) => Nil
      case (Nil, l1) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def zipWith2[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    val buf = new collection.mutable.ListBuffer[C]
    @annotation.tailrec
    def go(l1: List[A], l2: List[B]): Unit = (l1, l2) match {
      case (l1, Nil) => Nil
      case (Nil, l1) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => {
        buf += f(h1, h2);
        go(t1, t2)
      }
    }
    go(l1, l2)
    List(buf.toList: _*)
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def go[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, Cons(_, _)) => false
      case (Cons(hl, tl), Cons(hr, tr)) => (hl == hr) && go(tl, tr)
    }
    sup match {
      case Cons(_, t) => go(sup, sub) || hasSubsequence(t, sub)
      case Nil => go(sup, sub)
    }
  }

  // I get compilation error here -- it's not tailrec and I don't get why.
  // @annotation.tailrec 
  def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def go[A](sup: List[A], sub: List[A], c: () => Boolean ): Boolean =
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, Cons(_, _)) => c()
        case (Cons(hl, tl), Cons(hr, tr)) => {
          if (hl != hr) c()
          else go(tl, tr, c)
        }
    }
    sup match {
      case Cons(h, t) => go(sup, sub, () => hasSubsequence2(t, sub))
      case Nil => go(sup, sub, () => false)
    }
  }

  def hasSubsequence3[A](sup: List[A], sub: List[A]): Boolean = {
    println(s"hs $sup  --  $sub")
    @annotation.tailrec
    def go[A](sup: List[A], sub: List[A], c: () => Boolean ): Boolean = {
      println("go")
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, Cons(_, _)) => c()
        case (Cons(hl, tl), Cons(hr, tr)) => {
          if (hl != hr) c()
          else go(tl, tr, c)
        }
    }
    }
    sup match {
      case Cons(h, t) => {
        println("1")
        go(sup, sub, () => {
          println("2")
          hasSubsequence2(t, sub)
        })
      }
      case Nil => go(sup, sub, () => false)
    }
  }


}


object Main {
  def test[T](a: T, b: T): Unit = {
    if (a != b) println(s"Not OK: $a != $b")
    else println("OK")
  }


  def main(args: Array[String]): Unit = {
    test(List.x, 3)
    test(List.tail(List(1, 2, 3)), List(2, 3))
    test(List.setHead(List(1, 2, 3), 10), List(10, 2, 3))
    test(List.drop(List(1, 2, 3, 4), 2), List(3, 4))
    test(List.drop(List(1, 2, 3, 4), 10), List())
    test(List.drop(List(), 10), List())
    test(List.dropWhile(List(-1, -2, -3, 4), (x: Int) => x < 0), List(4))
    test(List.init(List(-1, -2, -3)), List(-1, -2))
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    test(List.length(List(1, 2, 3)), 3)
    test(List.length(List()), 0)
    test(List.foldLeft(List(1, 2, 3), 0)(_ + _), 6)
    test(List.sum3(List(1, 2, 3)), 6)
    test(List.product3(List(1, 2, 3)), 6)
    test(List.length1(List(1, 2, 3)), 3)
    test(List.reverse(List(1, 2, 3)), List(3, 2, 1))
    test(List.append2(List(1), List(2)), List(1, 2))
    test(List.concat(List(List(1, 2), List(3), List(4, 5))),
         List(1, 2, 3, 4, 5))

    val l = List(1, 2, 3)
    // Note this type only corresponds to foldRight, because foldLeft has a
    // different order of the arguments for the reducer
    type FoldRight[A, B] = (List[A], B) => ((A, B) => B)  => B
    def mkRightStr[A](x: A, y: String): String = s"($x + $y)"
    def showFoldRight(f: FoldRight[Int, String]): String =
      f(l, "0")(mkRightStr)

    println(List.foldRight2(l, "0")(mkRightStr))
    val standardRight = showFoldRight(List.foldRight)
    test(standardRight ,
         showFoldRight(List.foldRight2))
    test(standardRight,
         showFoldRight(List.foldRight3))

    type FoldLeft[A, B] = (List[A], B) => ((B, A) => B)  => B
    def mkLeftStr[A](x: String, y: A): String = s"($x + $y)"
    def showFoldLeft(f: FoldLeft[Int, String]): String =
      f(l, "0")(mkLeftStr)
    println(List.foldLeft(l, "0")(mkLeftStr))
    val standardLeft = showFoldLeft(List.foldLeft)
    test(standardLeft,
         showFoldLeft(List.foldLeft2))
    test(standardLeft,
         showFoldLeft(List.foldLeft3))

    // I want:
    // println(showFold(List.foldRight, mkRightStr))
    // println(showFold(List.foldLeft, mkLeftStr))
    //def mkRightStr[A](x: A, y: String): String = s"($x + $y)"
    //def showFold[A, B](f: Fold[Int, String]): String =
      //f(l, "0")(mkRightStr)

    test(List.transform(List(1, 2))(_ + 1), List(2, 3))
    test(List.transform2(List(1, 2))(_ + 1), List(2, 3))
    test(List.add1(List(1, 2)), List(2, 3))
    println(List.toStr(List(1.0, 2.0)))
    test(List.toStr(List(1.0, 2.0)), List("1.0", "2.0"))

    test(List.filter(List(1, 2, 3, 4))(_ % 2 == 0), List(2, 4))
    test(List.filter2(List(1, 2, 3, 4))(_ % 2 == 0), List(2, 4))
    test(List.flatMap(List(1,2,3))(i => List(i,i)), List(1, 1, 2, 2, 3, 3))
    test(List.filter3(List(1, 2, 3, 4))(_ % 2 == 0), List(2, 4))
    test(List.zipWith(List(1, 2), List(3, 4))(_ + _), List(4, 6))
    test(List.zipWith2(List(1, 2), List(3, 4))(_ + _), List(4, 6))

    def testSubsequence(f: (List[Int], List[Int]) => Boolean): Unit = {
      test(f(List(1, 2, 3, 4), List(2, 3)), true)
      test(f(List(1, 2, 3, 4), List(3, 4)), true)
      test(f(List(1, 2, 3, 4), List(1, 2)), true)
      test(f(List(1, 2, 3, 4), List(1)), true)
      test(f(List(1, 2, 3, 4), List(4)), true)
      test(f(List(1, 2, 3, 4), Nil), true)
      test(f(Nil, Nil), true)
      test(f(Nil, List(1)), false)
      test(f(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)), false)
    }
    testSubsequence(List.hasSubsequence)
    testSubsequence(List.hasSubsequence2)

    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    def testSize(f: Tree[Int] => Int): Unit = {
      println("Test Tree.size")
      test(Tree.size(Leaf(1)), 1)
      test(Tree.size(t), 5)
    }
    testSize(Tree.size)
    testSize(Tree.size2)

    test(Tree.maximum(t), 3)
    test(Tree.maximum2(t), 3)

    test(Tree.depth(Leaf(1)), 1)
    test(Tree.depth(t), 3)
    test(Tree.depth2(Leaf(1)), 1)
    test(Tree.depth2(t), 3)

    test(Tree.map(t)(_ + 0), t)
    test(Tree.map2(t)(_ + 0), t)
  }
}
