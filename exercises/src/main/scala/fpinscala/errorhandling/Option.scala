package fpinscala.errorhandling
import scala.collection.immutable.List


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // The way to access `this` is not described in the book, which is
  // kind of incomplete information to accomplish this exercise.
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None

  def flatMap2[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => this
  }

  def orElse2[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

  def filter2(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // I think I've spent about two hours figuring out why my attempts fail.  The
  // key thing was to follow all the types carefully.  One of my last attempts:
  // mean(xs) map (m => (xs map (e => math.pow(e - m, 2)))) map (l => mean(l))
  // I was deconstructing it in chunks and got stuck in the last map evaluation.
  // At some point I carefully followed the types and realized that if I use
  // map in the end, then the return type would be Option[Option[Double]] -- I
  // tried it and it worked, then I tried flatMap instead, just based on it's
  // signature.
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).
    map(m => (xs map (e => math.pow(e - m, 2)))).
    flatMap(l => mean(l))
  }

  def map20[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
     (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(aa), Some(bb)) => Some(f(aa, bb))
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (a => b map (b => f(a, b)))
    // TODO This gives me a compiler error:
    // [error] /app/exercises/src/main/scala/fpinscala/errorhandling/Option.scala:90: missing parameter type for expanded function ((x$2: <error>) => f(x$2, b))
    // [error]     a flatMap (b map (b => f(_, b)))
    //a flatMap (b map (b => f(_, b)))

  //def map2[A,B,C](a: Option[A],
                  //b: Option[B],
                  //c: Option[C])(f: (A, B, C) => D): Option[C] =


  @annotation.tailrec
  def allExist[A](a: List[Option[A]]): Boolean = a match {
    case Nil => true
    case h :: t => h match {
      case Some(aa) => allExist(t)
      case None => false
    }
  }

  def sequenceUnkosher[A](a: List[Option[A]]): Option[List[A]] = {
    if (allExist(a)) Some(a map ((o: Option[A]) => o match { case Some(a) => a }))
    else None
  }

  // This was really hard to come up with.  When I got stuck I was trying to
  // write down the types of intermediate results, but it did not help much.
  // I tried to do something like this:
  // case h :: t => h map ((hh: A) => hh :: sequence(t))
  // but the sequence in this case return the Option[List[A]], but I needed the 
  // List[A].  At some point I went to lie on the floor and wasn't forcing
  // myself to get the answer for some time, then it occurred to me that I
  // could inject another map into the first map function and I came up with
  // the some solution, which was almost correct -- I had to figure out when to
  // use faltMap instead of map.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap ((hh: A) => sequence(t) map ((tt: List[A]) => hh :: tt))
  }

  def sequenceViaFold[A](l: List[Option[A]]): Option[List[A]] =
    // TODO this does not compile withouth explicit casting to Option[List[A]]
    // and I still don't get it when I should do explicit casts.
    // I did not get it that I could use map2 for that -- I only realized it
    // when reading the answers.
    l.foldRight(Some(List[A]()): Option[List[A]]) (
        (a: Option[A], b: Option[List[A]]) =>
          a flatMap ((aa: A) => b map ((bb: List[A]) => aa :: bb))
      )
  
  def sequenceViaFold2[A](l: List[Option[A]]): Option[List[A]] =
  // I wasn't able to make it compile withouth all the type specifiers:
    l.foldRight(Some(List[A]()): Option[List[A]]) (
      (ao: Option[A], bo: Option[List[A]]) =>
        map2(ao, bo) ((a: A, b: List[A]) => a :: b))
  // but the answers show that you can explicitly specify the type and it would
  // make it possible to drop type specifiers further:
  // a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def sequenceViaTraverse[A](l: List[Option[A]]): Option[List[A]] =
    traverse(l)(a => a)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap(b => traverse(t)(f) map (tt => b :: tt))
  }

  def traverseViaMap2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def traverseViaFold[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] =
    l.foldRight[Option[List[B]]] (Some(Nil)) ((a, ll) => map2(f(a), ll)(_ :: _))
}


object Main {
  def test[T](a: T, b: T): Unit = {
    if (a != b) println(s"Not OK: $a != $b")
    else println("OK")
  }

  def main(args: Array[String]): Unit = {
    val hi = Some("hi")
    test(hi map (s => s), Some("hi"))
    test(None map(s => s), None)

    test(hi getOrElse "ho", "hi")
    test(None getOrElse "ho", "ho")
    //test(hi getOrElse2 "ho", "hi")
    //test(None getOrElse2 "ho", "ho")


    def nohi(a: String): Option[String] = {
      if (a == "hi") None
      else Some(a)
    }

    test(hi flatMap nohi, None)
    test(Some("wooga") flatMap nohi, Some("wooga"))
    test(hi flatMap2 nohi, None)
    test(Some("wooga") flatMap2 nohi, Some("wooga"))

    test(None orElse Some("wooga"), Some("wooga"))
    test(Some("zoogga") orElse Some("wooga"), Some("zoogga"))

    test(hi filter (_ == "hi"), hi)
    test(hi filter (_ == "wooga"), None)
    test(hi filter2 (_ == "hi"), hi)
    test(hi filter2 (_ == "wooga"), None)
    
    val l = List(1.0, 2, 3, 4)
    test(Option.variance(l), Some(1.25))
    test(Option.variance(List()), None)

    test(Option.map2(hi, hi)(_ + _), Some("hihi"))
    test(Option.map2(None: Option[String], hi)((s1, s2) => s1 + s2),
         None)
    test(Option.map20(hi, hi)(_ + _),
         Some("hihi"))
    test(Option.map20(None: Option[String], hi)((s1, s2) => s1 + s2),
         None)

    test(Option.allExist(List(hi, hi)),
         true)
    test(Option.allExist(List(hi, hi, None)),
         false)

    def testSeq(f: List[Option[String]] => Option[List[String]]) = {
      test(
        f(List(hi, hi)),
        Some(List("hi", "hi")))
      test(
        f(List(hi, hi, None)),
        None)
    }

    testSeq(Option.sequence)
    testSeq(Option.sequenceUnkosher)
    testSeq(Option.sequenceViaFold)
    testSeq(Option.sequenceViaFold2)
    testSeq(Option.sequenceViaTraverse)

    def testTraverse(f: List[Int] =>
                        (Int => Option[Int]) => Option[List[Int]]): Unit = {
      test(f(List(1, 2))(Some(_)),
           Some(List(1, 2)))
      test(
        f(List(1, 2))(a =>
          if (a % 2 == 0) Some(a)
          else None
        ),
        None
      )
    }
    testTraverse(Option.traverse)
    testTraverse(Option.traverseViaMap2)
    testTraverse(Option.traverseViaFold)

    val v: Int = 4
    test(
      Right(v) map (2*_),
      Right(2*v)
    )
    test(
      Left(v) map ((a: Int) => 2*a),
      Left(v)
    )
    test(
      Right(v) flatMap (Right(_)),
      Right(v)
    )
    test(
      Right(v) flatMap (v => Left("hi")),
      Left("hi")
    )
    test(
      Left(v) flatMap (v => Left("hi")),
      Left(v)
    )
    test(
      Right(v) orElse (Left("hi")),
      Right(v)
    )
    test(
      Left(v) orElse (Right(5)),
      Right(5)
    )

    test(
      Right("1").map2 (Right("2")) (_ + _),
      Right("12")
    )
    test(
      Left("1").map2 (Right("2")) ((l: String, r: String) => l + r),
      Left("1")
    )
    test(
      Right("1").map2 (Left("2")) ((l: String, r: String) => l + r),
      Left("2")
    )
    test(
      Left("1").map2 (Left("2")) ((l: String, r: String) => l + r),
      Left("1")
    )

    test(
      Either.traverse (List(1, 2)) (a =>
          if (a % 2 == 0) Right(a)
          else Left("too odd")
      ),
      Left("too odd")
    )
    test(
      Either.traverse (List(2)) (a =>
          if (a % 2 == 0) Right(a)
          else Left("too odd")
      ),
      Right(List(2))
    )

    test(
      Either.sequence (List(Right(2))),
      Right(List(2))
    )

    test(
      Either.sequence (List(Right(2), Left("hmm..."), Left("wooga"))),
      Left("hmm...")
    )
  }
}
