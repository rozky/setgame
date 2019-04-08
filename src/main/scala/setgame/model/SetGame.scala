package setgame.model

import cats.Eval
import cats.data.{IndexedStateT, NonEmptyList, NonEmptySet, OptionT, State}
import setgame.model.CardPosition.CardPosition
import setgame.model.CardsOnTable.{NoCardsOnTable, SomeCardsOnTable, SomeMissingCardsOnTable}
import setgame.model.DeckOfCards.NonEmptyDeckOfCards
import setgame.model.PlayerId.PlayerId

sealed trait SetGame {
  def score: GameScore

  def cardsOnTable: CardsOnTable
}

object SetGame {

  final case class FinishedGame(override val score: GameScore) extends SetGame {
    override def cardsOnTable: CardsOnTable = NoCardsOnTable
  }

  // todo - add check to make sure that set is really available
  final case class GameWithSet(override val score: GameScore,
                               override val cardsOnTable: SomeCardsOnTable,
                               private[model] val deck: DeckOfCards) extends SetGame

  def playTurn(player: PlayerId, positions: Triplet[CardPosition]): IndexedStateT[Eval, GameWithSet, SetGame, Unit] = IndexedStateT { game =>

    require(game.score.getPlayerScore(player).isDefined, "This player is not playing")

    val (nextDeck, cardsFromDeckOpt) = DeckOfCards.takeTripletOpt.run(game.deck).value
    val (nextCardsOnTable, pickedCardsOpt) = replacedPickedCards(positions, game.cardsOnTable, cardsFromDeckOpt)

    pickedCardsOpt match {
      case Some(pickedCards) => Eval.now((updateGame(game)(player, pickedCards, nextCardsOnTable, nextDeck), ()))
      case _ => Eval.now((game, ()))
    }
  }

  val showMoreCards: State[SetGame, Unit] = State {
    case game@GameWithSet(score, cardsOnTable @ SomeMissingCardsOnTable(cards, _), deck @ NonEmptyDeckOfCards(_)) =>

      val (nextDeck, cardsFromDeck) = DeckOfCards.takeTriplet.run(deck).value

      val nextCardsOnTable: (SomeCardsOnTable, Unit) = CardsOnTable.addCards(cardsFromDeck).run(cardsOnTable).value

      (GameWithSet(score, nextCardsOnTable._1, nextDeck), ())

    case game => (game, ())
  }

  val getWinner: State[SetGame, NonEmptyList[PlayerId]] = State { game =>
    (FinishedGame(game.score), game.score.winners)
  }

  def newGame(players: NonEmptySet[PlayerId]): GameWithSet = {
    newGame(players, DeckOfCards.shuffledFullDeck()).get
  }

  private[model] def newGame(players: NonEmptySet[PlayerId],
                             deck: NonEmptyDeckOfCards): Option[GameWithSet] = {
    val p = for {
      a <- OptionT(DeckOfCards.takeTripletOpt)
      b <- OptionT(DeckOfCards.takeTripletOpt)
      c <- OptionT(DeckOfCards.takeTripletOpt)
      d <- OptionT(DeckOfCards.takeTripletOpt)
    } yield CardsOnTable.fromCards(a.toNEL concatNel b.toNEL concatNel c.toNEL concatNel d.toNEL)

    p.value.run(deck).value match {
      case (deck, Some(gridOfCards)) => Some(
        GameWithSet(
          GameScore.forPlayers(players),
          gridOfCards,
          deck))
      case _ => None
    }
  }

  private def replacedPickedCards(positions: Triplet[CardPosition],
                                  currentCardsOnTable: SomeCardsOnTable,
                                  nextCardsFromDeck: Option[Triplet[Card]]): (CardsOnTable, Option[Triplet[Card]]) = {
    val replacePickedCardsWithCardsFromDeck = nextCardsFromDeck match {
      case Some(triplet) =>
        for {
          removedCards <- CardsOnTable.removeCards(positions)
          _ <- CardsOnTable.addCards(triplet)
        } yield removedCards
      case _ =>
        for {
          removedCards <- CardsOnTable.removeCards(positions)
        } yield removedCards
    }

    replacePickedCardsWithCardsFromDeck.run(currentCardsOnTable).value

  }

  private def updateGame(currentGame: GameWithSet)
                        (player: PlayerId,
                         pickedCards: Triplet[Card],
                         nextCardsOnTable: CardsOnTable,
                         nextDeck: DeckOfCards): SetGame = {
    if (isSetOfCardsValid(pickedCards)) {
      val nextScore = GameScore.increment(player).runS(currentGame.score).value

      nextCardsOnTable match {
        case nextCardsOnTable: SomeCardsOnTable => currentGame.copy(score = nextScore, cardsOnTable = nextCardsOnTable, deck = nextDeck)
        case _ => FinishedGame(nextScore)
      }
    } else {
      val nextScore = GameScore.decrement(player).runS(currentGame.score).value
      currentGame.copy(score = nextScore)
    }
  }

  private def isSetOfCardsValid(cards: Triplet[Card]): Boolean = {
    allSameOrAllDifferent(cards.a.shape.id, cards.b.shape.id, cards.c.shape.id) &&
      allSameOrAllDifferent(cards.a.color.id, cards.b.color.id, cards.c.color.id) &&
      allSameOrAllDifferent(cards.a.number.id, cards.b.number.id, cards.c.number.id) &&
      allSameOrAllDifferent(cards.a.shading.id, cards.b.shading.id, cards.c.shading.id)
  }

  private def allSame(a: Int, b: Int, c: Int): Boolean = (a & b & c) == a

  private def allDifferent(a: Int, b: Int, c: Int): Boolean = Set(a, b, c).size == 3

  private def allSameOrAllDifferent(a: Int, b: Int, c: Int): Boolean = allSame(a, b, c) || allDifferent(a, b, c)
}
