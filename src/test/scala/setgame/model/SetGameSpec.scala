package setgame.model

import cats.Eval
import cats.data.{IndexedStateT, NonEmptyList, NonEmptyMap, NonEmptySet}
import setgame.BaseSpec
import setgame.Fixtures.Cards._
import setgame.model.CardPosition._
import setgame.model.PlayerId._
import setgame.model.SetGame.{FinishedGame, GameWithSet}

class SetGameSpec extends BaseSpec {
  val players = NonEmptySet.of(Player_1, Player_2)

  "newGame" should "set score of each player to 0 at the begin" in {
    // when
    val game = SetGame.newGame(NonEmptySet.of(Player_1, Player_2))

    // then
    game.score should be(GameScore(NonEmptyMap.of((Player_1, 0), (Player_2, 0))))
  }

  it should "show 12 cards from the top of the deck" in {
    // when
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // then
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_1) should be(Some(CARD_1))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_2) should be(Some(CARD_2))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_3) should be(Some(CARD_3))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_4) should be(Some(CARD_4))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_5) should be(Some(CARD_5))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_6) should be(Some(CARD_6))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_7) should be(Some(CARD_7))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_8) should be(Some(CARD_8))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_9) should be(Some(CARD_9))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_10) should be(Some(CARD_10))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_11) should be(Some(CARD_11))
    game.cardsOnTable.getCardAtPosition(CardPosition.Pos_12) should be(Some(CARD_12))
  }

  "playTurn" should "increase player's score by 1 if submits valid move" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- SetGame.playTurn(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
    } yield ()

    val gameAfter = play.runS(game).value

    // then player score is increased by 1
    gameAfter.score should be(GameScore(NonEmptyMap.of((Player_1, 1), (Player_2, 0))))
  }

  it should "decrease player's score by 1 if submits invalid move" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    val play = for {
      _ <- SetGame.playTurn(Player_2, Triplet(Pos_1, Pos_2, Pos_4))
    } yield ()

    val gameAfter = play.runS(game).value

    // then player score is decreased by 1
    gameAfter.score should be(GameScore(NonEmptyMap.of((Player_1, 0), (Player_2, -1))))
  }

  it should "replace submitted cards with new one if move was valid" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- SetGame.playTurn(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
    } yield ()

    val gameAfter = play.runS(game).value

    // then
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_1) should be(Some(CARD_13))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_2) should be(Some(CARD_14))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_3) should be(Some(CARD_15))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_4) should be(Some(CARD_4))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_5) should be(Some(CARD_5))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_6) should be(Some(CARD_6))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_7) should be(Some(CARD_7))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_8) should be(Some(CARD_8))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_9) should be(Some(CARD_9))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_10) should be(Some(CARD_10))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_11) should be(Some(CARD_11))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_12) should be(Some(CARD_12))
  }

  it should "not change cards on the table if a player submits invalid move" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- SetGame.playTurn(Player_2, Triplet(Pos_1, Pos_2, Pos_4))
    } yield ()

    val gameAfter = play.runS(game).value

    // then
    gameAfter.cardsOnTable should be(game.cardsOnTable)
  }

  it should "fail if there no cards at submitted positions" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- SetGame.playTurn(Player_2, Triplet(Pos_13, Pos_14, Pos_15))
    } yield ()

    val exception = intercept[IllegalArgumentException](play.runS(game).value)

    // then
    exception.getMessage should be("requirement failed: There no cards at picked slots")
  }

  it should "accept multiple valid moves" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_2, Triplet(Pos_7, Pos_8, Pos_9))
    } yield ()

    val gameAfter = play.runS(game).value

    // then
    gameAfter.score should be(GameScore(NonEmptyMap.of((Player_1, 1), (Player_2, 1))))

    // and
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_1) should be(Some(CARD_13))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_2) should be(Some(CARD_14))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_3) should be(Some(CARD_15))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_4) should be(Some(CARD_4))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_5) should be(Some(CARD_5))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_6) should be(Some(CARD_6))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_7) should be(Some(CARD_16))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_8) should be(Some(CARD_17))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_9) should be(Some(CARD_18))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_10) should be(Some(CARD_10))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_11) should be(Some(CARD_11))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_12) should be(Some(CARD_12))
  }

  it should "not replace submitted cards with new one if move was valid but there is no more cards" in {
    val game = SetGame.newGame(players, DeckOfCards.fromCards(Card.getCards(12))).get

    // when
    val play = for {
      _ <- SetGame.playTurn(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
    } yield ()

    val gameAfter = play.runS(game).value

    // then
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_1) should be(None)
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_2) should be(None)
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_3) should be(None)
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_4) should be(Some(CARD_4))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_5) should be(Some(CARD_5))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_6) should be(Some(CARD_6))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_7) should be(Some(CARD_7))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_8) should be(Some(CARD_8))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_9) should be(Some(CARD_9))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_10) should be(Some(CARD_10))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_11) should be(Some(CARD_11))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_12) should be(Some(CARD_12))
  }

  it should "fail if player who is not playing submits a move" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- SetGame.playTurn(Player_3, Triplet(Pos_1, Pos_2, Pos_3))
    } yield ()

    val exception = intercept[IllegalArgumentException](play.runS(game).value)

    // then
    exception.getMessage should be("requirement failed: This player is not playing")
  }

  it should "be possible to play full game" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_4, Pos_5, Pos_6))
      _ <- playTurnUnsafe(Player_1, Triplet(Pos_7, Pos_8, Pos_9))
      _ <- SetGame.playTurn(Player_1, Triplet(Pos_10, Pos_11, Pos_12))
    } yield ()

    val gameAfter = play.runS(game).value

    // then
    gameAfter should be(FinishedGame(GameScore(NonEmptyMap.of((Player_1, 27), (Player_2, 0)))))
  }

  "showMoreCards" should "show more cards if there are free card slots" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- SetGame.showMoreCards
    } yield ()

    val gameAfter = play.runS(game).value

    // then
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_13) should be(Some(CARD_13))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_14) should be(Some(CARD_14))
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_15) should be(Some(CARD_15))
  }

  it should "not show more cards if deck is empty" in {
    val game = SetGame.newGame(players, DeckOfCards.fromCards(Card.getCards(12))).get

    // when
    val play = for {
      _ <- SetGame.showMoreCards
    } yield ()

    val gameAfter = play.runS(game).value

    // then
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_13) should be(None)
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_14) should be(None)
    gameAfter.cardsOnTable.getCardAtPosition(CardPosition.Pos_15) should be(None)
  }

  "getWinner" should "show more cards if there are free card slots" in {
    val game = SetGame.newGame(players, DeckOfCards.fullDeck).get

    // when
    val play = for {
      _ <- SetGame.playTurn(Player_1, Triplet(Pos_1, Pos_2, Pos_3))
      winners <- SetGame.getWinner
    } yield winners

    val (gameAfter, winners) = play.run(game).value

    // then
    gameAfter should be(FinishedGame(GameScore(NonEmptyMap.of((Player_1, 1), (Player_2, 0)))))

    // and
    winners should be(NonEmptyList.of(Player_1))
  }

  def playTurnUnsafe(player: PlayerId, positions: Triplet[CardPosition]): IndexedStateT[Eval, GameWithSet, GameWithSet, Unit] = {
    SetGame.playTurn(player, positions).bimap(_.asInstanceOf[GameWithSet], identity)
  }
}