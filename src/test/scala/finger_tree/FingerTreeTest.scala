package finger_tree

import org.scalatest.{DiagrammedAssertions, FunSuite}

class FingerTreeTest extends FunSuite with DiagrammedAssertions {

  import FingerTree._

  test("empty") {
    assert(empty === Empty)
  }

  // see https://upload.wikimedia.org/wikipedia/commons/f/f2/2-3_finger_tree%2C_pushing_and_popping_example.svg
  test("scenario") {
    // (updater, expect)
    List[(FingerTree[Int] => FingerTree[Int], FingerTree[Int])](
      // format: off
      (_.snoc(1), Single(1)),
      (_.snoc(2), Deep(One(1), Empty, One(2))),
      (_.snoc(3), Deep(One(1), Empty, Two(2, 3))),
      (_.snoc(4), Deep(One(1), Empty, Three(2, 3, 4))),
      (_.snoc(5), Deep(One(1), Empty, Four(2, 3, 4, 5))),
      (_.snoc(6), Deep(One(1), Single(Node3(2, 3, 4)), Two(5, 6))),
      (_.snoc(7), Deep(One(1), Single(Node3(2, 3, 4)), Three(5, 6, 7))),
      (_.snoc(8), Deep(One(1), Single(Node3(2, 3, 4)), Four(5, 6, 7, 8))),
      (_.snoc(9), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, One(Node3(5, 6, 7))), Two(8, 9))),
      (_.snoc(10), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, One(Node3(5, 6, 7))), Three(8, 9, 10))),
      (_.snoc(11), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, One(Node3(5, 6, 7))), Four(8, 9, 10, 11))),
      (_.snoc(12), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Two(Node3(5, 6, 7), Node3(8, 9, 10))), Two(11, 12))),
      (_.snoc(13), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Two(Node3(5, 6, 7), Node3(8, 9, 10))), Three(11, 12, 13))),
      (_.snoc(14), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Two(Node3(5, 6, 7), Node3(8, 9, 10))), Four(11, 12, 13, 14))),
      (_.snoc(15), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Three(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13))), Two(14, 15))),
      (_.snoc(16), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Three(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13))), Three(14, 15, 16))),
      (_.snoc(17), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Three(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13))), Four(14, 15, 16, 17))),
      (_.snoc(18), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Four(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13), Node3(14, 15, 16))), Two(17, 18))),
      (_.snoc(19), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Four(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13), Node3(14, 15, 16))), Three(17, 18, 19))),
      (_.snoc(20), Deep(One(1), Deep(One(Node3(2, 3, 4)), Empty, Four(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13), Node3(14, 15, 16))), Four(17, 18, 19, 20))),
      (_.snoc(21), Deep(One(1), Deep(One(Node3(2, 3, 4)), Single(Node3(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13))), Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Three(2, 3, 4), Deep(Three(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Two(3, 4), Deep(Three(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(One(4), Deep(Three(Node3(5, 6, 7), Node3(8, 9, 10), Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Three(5, 6, 7), Deep(Two(Node3(8, 9, 10), Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Two(6, 7), Deep(Two(Node3(8, 9, 10), Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(One(7), Deep(Two(Node3(8, 9, 10), Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Three(8, 9, 10), Deep(One(Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Two(9, 10), Deep(One(Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(One(10), Deep(One(Node3(11, 12, 13)), Empty, Two(Node3(14, 15, 16), Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Three(11, 12, 13), Deep(One(Node3(14, 15, 16)), Empty, One(Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Two(12, 13), Deep(One(Node3(14, 15, 16)), Empty, One(Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(One(13), Deep(One(Node3(14, 15, 16)), Empty, One(Node3(17, 18, 19))), Two(20, 21))),
      (_.popL.get._2, Deep(Three(14, 15, 16), Single(Node3(17, 18, 19)), Two(20, 21))),
      (_.popL.get._2, Deep(Two(15, 16), Single(Node3(17, 18, 19)), Two(20, 21))),
      (_.popL.get._2, Deep(One(16), Single(Node3(17, 18, 19)), Two(20, 21))),
      (_.popL.get._2, Deep(Three(17, 18, 19), Empty, Two(20, 21))),
      (_.popL.get._2, Deep(Two(18, 19), Empty, Two(20, 21))),
      (_.popL.get._2, Deep(One(19), Empty, Two(20, 21))),
      (_.popL.get._2, Deep(One(20), Empty, One(21))),
      (_.popL.get._2, Single(21)),
      (_.popL.get._2, Empty),
      // format: on
    ).foldLeft[FingerTree[Int]](empty) {
      case (acc, (updater, expect)) =>
        val updated = updater(acc)
        assert(updated === expect)
        updated
    }
  }

  test("++") {
    def fromCons(range: Range): FingerTree[Int] = range.foldLeft[FingerTree[Int]](empty) {
      case (acc, i) => acc.cons(i)
    }
    def fromSnoc(range: Range): FingerTree[Int] = range.foldLeft[FingerTree[Int]](empty) {
      case (acc, i) => acc.snoc(i)
    }

    val ft1 = fromSnoc(1 to 5)
    val ft2 = fromCons(6 to 10)
    // format: off
    val expect1 = Deep(One(1),Deep(One(Node2(2,3)),Empty,Three(Node2(4,5),Node2(10,9),Node2(8,7))),One(6))
    // format: on
    assert(ft1 ++ ft2 === expect1)

    val ft10 = fromSnoc(11 to 20)
    val ft20 = fromCons(21 to 30)
    // format: off
    val expect2 = Deep(One(11),Deep(One(Node3(12,13,14)),Deep(One(Node2(Node3(15,16,17),Node2(18,19))),Empty,One(Node3(Node2(20,30),Node2(29,28),Node3(27,26,25)))),One(Node3(24,23,22))),One(21))
    // format: on
    assert(ft10 ++ ft20 == expect2)
  }

}
