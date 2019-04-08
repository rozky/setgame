package setgame.model

import cats.data.NonEmptyList

case class Triplet[A](a: A, b: A, c: A) {
  require(a != b)
  require(a != c)
  require(b != c)

  def toNEL: NonEmptyList[A] = NonEmptyList.of(a, b, c)
}

object Triplet {
  def fromList[A](cards: List[A]): Option[Triplet[A]] = {
    cards match {
      case a :: b :: c :: tail => Some(Triplet(cards.head, cards(1), cards(2)))
      case _ => None
    }
  }

  def fromListUnsafe[A](cards: List[A]): Triplet[A] = {
    require(cards.size == 3)

    Triplet(cards.head, cards(1), cards(2))
  }

  def triplesFromListUnsafe[A](cards: List[A]): NonEmptyList[Triplet[A]] = {
    require(cards.size >= 3)
    require(cards.size % 3 == 0)

    val triplets = cards
      .sliding(3, 3)
      .map(Triplet.fromListUnsafe)
      .toList

    NonEmptyList.fromListUnsafe(triplets)
  }
}
