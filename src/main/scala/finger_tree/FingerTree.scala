package finger_tree

/**
  * 2-3 FingerTree.
  * https://en.wikipedia.org/wiki/Finger_tree
  * https://ja.wikipedia.org/wiki/2-3_%E3%83%95%E3%82%A3%E3%83%B3%E3%82%AC%E3%83%BC%E3%83%84%E3%83%AA%E3%83%BC
  *
  */
sealed trait FingerTree[+A] {

  import FingerTree._

  def isEmpty: Boolean = {
    this == Empty
  }

  /** we can implement this by popL or digit.head */
  def head: Option[A] = this match {
    case Empty                        => None
    case Single(a)                    => Some(a)
    case Deep(One(a), _, _)           => Some(a)
    case Deep(Two(a, _), _, _)        => Some(a)
    case Deep(Three(a, _, _), _, _)   => Some(a)
    case Deep(Four(a, _, _, _), _, _) => Some(a)
  }

  /** we can implement this by popR or digit.last */
  def last: Option[A] = this match {
    case Empty                        => None
    case Single(a)                    => Some(a)
    case Deep(_, _, One(a))           => Some(a)
    case Deep(_, _, Two(_, b))        => Some(b)
    case Deep(_, _, Three(_, _, c))   => Some(c)
    case Deep(_, _, Four(_, _, _, d)) => Some(d)
  }

  def tail: Option[FingerTree[A]] = {
    popL.map(_._2)
  }

  def init: Option[FingerTree[A]] = {
    popR.map(_._1)
  }

  def cons[X >: A](x: X): FingerTree[X] = this match {
    case Empty                         => Single(x)
    case Single(a)                     => Deep(One(x), Empty, One(a))
    case Deep(One(b), m, sf)           => Deep(Two(x, b), m, sf)
    case Deep(Two(b, c), m, sf)        => Deep(Three(x, b, c), m, sf)
    case Deep(Three(b, c, d), m, sf)   => Deep(Four(x, b, c, d), m, sf)
    case Deep(Four(b, c, d, e), m, sf) => Deep(Two(x, b), m.cons(Node3(c, d, e)), sf)
  }

  def snoc[X >: A](x: X): FingerTree[X] = this match {
    case Empty                         => Single(x)
    case Single(a)                     => Deep(One(a), Empty, One(x))
    case Deep(pr, m, One(b))           => Deep(pr, m, Two(b, x))
    case Deep(pr, m, Two(b, c))        => Deep(pr, m, Three(b, c, x))
    case Deep(pr, m, Three(b, c, d))   => Deep(pr, m, Four(b, c, d, x))
    case Deep(pr, m, Four(b, c, d, e)) => Deep(pr, m.snoc(Node3(b, c, d)), Two(e, x))
  }

  /**
    * @return (haed, tail)
    */
  def popL: Option[(A, FingerTree[A])] = this match {
    case Empty                         => None
    case Single(a)                     => Some(a, Empty)
    case Deep(One(b), m, sf)           => Some(b, borrowL(m, sf))
    case Deep(Two(b, c), m, sf)        => Some(b, Deep(One(c), m, sf))
    case Deep(Three(b, c, d), m, sf)   => Some(b, Deep(Two(c, d), m, sf))
    case Deep(Four(b, c, d, e), m, sf) => Some(b, Deep(Three(c, d, e), m, sf))
  }

  /**
    * @return (init, last)
    */
  def popR: Option[(FingerTree[A], A)] = this match {
    case Empty                         => None
    case Single(a)                     => Some(Empty, a)
    case Deep(pr, m, One(b))           => Some(borrowR(pr, m), b)
    case Deep(pr, m, Two(b, c))        => Some(Deep(pr, m, One(b)), c)
    case Deep(pr, m, Three(b, c, d))   => Some(Deep(pr, m, Two(b, c)), d)
    case Deep(pr, m, Four(b, c, d, e)) => Some(Deep(pr, m, Three(b, c, d)), e)
  }

  private def append[B >: A](es: List[B], other: FingerTree[B]): FingerTree[B] = (this, other) match {
    case (pr, Empty) =>
      es.foldLeft[FingerTree[B]](pr) { case (acc, e) => acc.snoc(e) }
    case (pr, Single(f)) =>
      es.foldLeft[FingerTree[B]](pr) { case (acc, e) => acc.snoc(e) }.snoc(f)
    case (Deep(pr, m1, sf1), Deep(pr1, m2, sf)) =>
      val esNode = (sf1.toList ++ es ++ pr1.toList).grouped(3).toList.map {
        case a :: b :: c :: Nil => Node3(a, b, c)
        case a :: b :: Nil      => Node2(a, b)
        case _ :: Nil | Nil => throw new IllegalArgumentException
      }
      val m = m1.append(esNode, m2)
      Deep(pr, m, sf)
  }

  def ++[B >: A](other: FingerTree[B]): FingerTree[B] = {
    append(Nil, other)
  }
}

object FingerTree {
  val empty: FingerTree[Nothing] = Empty

  /**
    * root of tree
    * has 1 ~ 4 elements.
    */
  sealed trait Digit[+A] {
    def toList: List[A] = this match {
      case One(a)           => List(a)
      case Two(a, b)        => List(a, b)
      case Three(a, b, c)   => List(a, b, c)
      case Four(a, b, c, d) => List(a, b, c, d)
    }
  }
  case class One[A](a: A) extends Digit[A]
  case class Two[A](a1: A, a2: A) extends Digit[A]
  case class Three[A](a1: A, a2: A, a3: A) extends Digit[A]
  case class Four[A](a1: A, a2: A, a3: A, a4: A) extends Digit[A]

  /**
    * node of tree(not root)
    * has 2 ~ 3 elements.
    */
  sealed trait Node[+A]
  case class Node2[A](a1: A, a2: A) extends Node[A]
  case class Node3[A](a1: A, a2: A, a3: A) extends Node[A]

  /**
    * finger tree elements
    */
  case object Empty extends FingerTree[Nothing]
  case class Single[A](a: A) extends FingerTree[A]
  case class Deep[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) extends FingerTree[A]

  /**
    * carry down
    */
  private def borrowL[A](m: FingerTree[Node[A]], sf: Digit[A]): FingerTree[A] = {
    if (m.isEmpty) {
      toTree(sf)
    } else {
      val Some((node, m_)) = m.popL
      Deep(toDigit(node), m_, sf)
    }
  }

  /**
    * carry down
    */
  private def borrowR[A](pr: Digit[A], m: FingerTree[Node[A]]): FingerTree[A] = {
    if (m.isEmpty) {
      toTree(pr)
    } else {
      val Some((m_, node)) = m.popR
      Deep(pr, m_, toDigit(node))
    }
  }

  /**
    * convert digit to fingerTree.
    *
    * @note we can  implement by pr.toList.foldLeft(empty) { _.snoc(_) }
    */
  private def toTree[A](pr: Digit[A]): FingerTree[A] = pr match {
    case One(a)           => empty.snoc(a)
    case Two(a, b)        => empty.snoc(a).snoc(b)
    case Three(a, b, c)   => empty.snoc(a).snoc(b).snoc(c)
    case Four(a, b, c, d) => empty.snoc(a).snoc(b).snoc(c).snoc(d)
  }

  /**
    * convert node to digit
    */
  private def toDigit[A](node: Node[A]): Digit[A] = node match {
    case Node2(a, b)    => Two(a, b)
    case Node3(a, b, c) => Three(a, b, c)
  }
}
