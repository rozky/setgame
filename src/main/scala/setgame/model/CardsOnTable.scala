package setgame.model

import cats.Eval
import cats.data.{IndexedStateT, NonEmptyList, NonEmptyMap}
import cats.implicits._
import setgame.model.CardPosition.CardPosition

sealed trait CardsOnTable {
  def getCardAtPosition(position: CardPosition): Option[Card]
}

object CardsOnTable {
  type RemovedCards = (Option[Card], Option[Card], Option[Card])

  lazy val MaxNumberOfCards: Int = CardPosition.values.size

  sealed trait MissingCardsOnTable extends CardsOnTable

  final case object NoCardsOnTable extends MissingCardsOnTable {
    override def getCardAtPosition(position: CardPosition): Option[Card] = None
  }

  sealed trait SomeCardsOnTable extends CardsOnTable {
    private[model] def cardSlots: NonEmptyMap[CardPosition, Option[Card]]

    override def getCardAtPosition(position: CardPosition): Option[Card] = {
      cardSlots.lookup(position).flatten
    }
  }

  final case class SomeMissingCardsOnTable(override val cardSlots: NonEmptyMap[CardPosition, Option[Card]],
                                           private[model] val emptyPositions: NonEmptyList[Triplet[CardPosition]])
    extends SomeCardsOnTable with MissingCardsOnTable {

    require(cardSlots.length == MaxNumberOfCards)
    // todo - validate empty losts
  }

  final case class AllCardsOnTable(override val cardSlots: NonEmptyMap[CardPosition, Option[Card]]) extends SomeCardsOnTable {
    require(cardSlots.length == MaxNumberOfCards)
  }

  def addCards(cards: Triplet[Card]): IndexedStateT[Eval, MissingCardsOnTable, SomeCardsOnTable, Unit] = IndexedStateT {
    case NoCardsOnTable => Eval.now((CardsOnTable.fromCards(cards.toNEL), ()))
    case SomeMissingCardsOnTable(cardSlots, emptyPositions) => {
      val positionsToFill: Triplet[CardPosition] = emptyPositions.head

      val updatedCards = cardSlots
        .updateWith(positionsToFill.a)(_ => Some(cards.a))
        .updateWith(positionsToFill.b)(_ => Some(cards.b))
        .updateWith(positionsToFill.c)(_ => Some(cards.c))

      NonEmptyList.fromList(emptyPositions.tail) match {
        case Some(emptyPositions) => Eval.now((SomeMissingCardsOnTable(updatedCards, emptyPositions), ()))
        case _ => Eval.now((AllCardsOnTable(updatedCards), ()))
      }
    }
  }

  def removeCards(positions: Triplet[CardPosition]): IndexedStateT[Eval, SomeCardsOnTable, MissingCardsOnTable, Option[Triplet[Card]]] = IndexedStateT { table =>
    val tripletOfCards: Option[Triplet[Card]] = (table.getCardAtPosition(positions.a), table.getCardAtPosition(positions.b), table.getCardAtPosition(positions.c))
      .mapN(Triplet.apply)

    require(tripletOfCards.isDefined, "There no cards at picked slots")

    val nextSlots = table.cardSlots
      .updateWith(positions.a)(_ => None)
      .updateWith(positions.b)(_ => None)
      .updateWith(positions.c)(_ => None)

    val emptySlots: NonEmptyList[Triplet[CardPosition]] = getEmptySlotsUnsafe(nextSlots)
    if (emptySlots.size == 5) {
      Eval.now((NoCardsOnTable, tripletOfCards))
    } else {
      Eval.now((SomeMissingCardsOnTable(nextSlots, emptySlots), tripletOfCards))
    }
  }

  private[model] def fromCards(cards: NonEmptyList[Card]): SomeCardsOnTable = {
    require(cards.size <= MaxNumberOfCards, "Too many cards")
    require(cards.size % 3 == 0, "Number of cards must be multiplier of 3")

    val indexedCards = zipCardsWithPosition(cards)

    if (cards.size == MaxNumberOfCards) {
      AllCardsOnTable(indexedCards)
    } else {
      SomeMissingCardsOnTable(indexedCards, getEmptySlotsUnsafe(indexedCards))
    }
  }

  private[model] def fromIndexedCards(indexedCards: NonEmptyMap[CardPosition, Option[Card]]): CardsOnTable = {
    require(indexedCards.length == MaxNumberOfCards)

    val emptyPositions = indexedCards.toNel.filterNot(e => e._2.isDefined).map(_._1)

    emptyPositions.size match {
      case empty if empty == indexedCards.length => NoCardsOnTable
      case empty if empty == 0 => AllCardsOnTable(indexedCards)
      case _ => SomeMissingCardsOnTable(indexedCards, Triplet.triplesFromListUnsafe(emptyPositions))
    }
  }

  private def zipCardsWithPosition(cards: NonEmptyList[Card]): NonEmptyMap[CardPosition, Option[Card]] = {
    val cardsFilledToMaxSize = cards.map(Some.apply) ++ List.fill(MaxNumberOfCards - cards.size)(Option.empty[Card])

    val positions = NonEmptyList.fromListUnsafe(CardPosition.values.toList.take(cardsFilledToMaxSize.size))

    positions
      .zipWith(cardsFilledToMaxSize) { case (position, card) => (position, card) }
      .toNem
  }

  private[model] def getEmptySlotsUnsafe(slots: NonEmptyMap[CardPosition, Option[Card]]): NonEmptyList[Triplet[CardPosition]] = {
    val emptyPositions = slots.toNel.filterNot(e => e._2.isDefined).map(_._1)
    Triplet.triplesFromListUnsafe(emptyPositions)
  }
}
