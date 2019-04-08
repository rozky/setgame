package setgame.model

import cats.Eval
import cats.data.{IndexedStateT, NonEmptyList, State}

import scala.util.Random

sealed trait DeckOfCards

object DeckOfCards {
  final case object EmptyDeckOfCards extends DeckOfCards
  final case class NonEmptyDeckOfCards(private[model] val triplets: NonEmptyList[Triplet[Card]]) extends DeckOfCards

  val takeTriplet: IndexedStateT[Eval, NonEmptyDeckOfCards, DeckOfCards, Triplet[Card]] = IndexedStateT {
    case _ @ NonEmptyDeckOfCards(NonEmptyList(head, Nil)) =>
      Eval.now((EmptyDeckOfCards, head))
    case _ @ NonEmptyDeckOfCards(NonEmptyList(head, tail)) =>
      Eval.now((NonEmptyDeckOfCards(NonEmptyList.fromListUnsafe(tail)), head))
  }

  val takeTripletOpt: State[DeckOfCards, Option[Triplet[Card]]] = State {
    case deck @ EmptyDeckOfCards => (deck, None)
    case _ @ NonEmptyDeckOfCards(NonEmptyList(head, Nil)) => (EmptyDeckOfCards, Some(head))
    case _ @ NonEmptyDeckOfCards(NonEmptyList(head, tail)) => (NonEmptyDeckOfCards(NonEmptyList.fromListUnsafe(tail)), Some(head))
  }

  private[model] lazy val fullDeck: NonEmptyDeckOfCards = fromCards(NonEmptyList.fromListUnsafe(Card.allCards))

  private[model] def shuffledFullDeck(): NonEmptyDeckOfCards = fromCards(NonEmptyList.fromListUnsafe(Random.shuffle(Card.allCards)))

  private[model] def fromCards(cards: NonEmptyList[Card]): NonEmptyDeckOfCards = {
    require(cards.size % 3 == 0)

    val triplets = cards
      .toList
      .sliding(3, 3)
      .map(Triplet.fromListUnsafe)
      .toList

    NonEmptyDeckOfCards(NonEmptyList.fromListUnsafe(triplets))
  }
}

