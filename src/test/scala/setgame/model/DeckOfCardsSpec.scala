package setgame.model

import cats.data.NonEmptyList
import setgame.BaseSpec
import setgame.Fixtures.Cards._
import setgame.model.DeckOfCards.{EmptyDeckOfCards, NonEmptyDeckOfCards}

class DeckOfCardsSpec extends BaseSpec {

  "takeTriplet" should "take single triple from the deck" in {
    // when
    val (deck, triplet) = DeckOfCards.takeTriplet.run(DeckOfCards.fullDeck).value

    // then
    triplet should be(Triplet(CARD_1, CARD_2, CARD_3))

    // and
    deck shouldBe an[NonEmptyDeckOfCards]
  }

  "takeTripletOpt" should "take multiple triples from the deck" in {
    // when
    val p = for {
      triplet1 <- DeckOfCards.takeTripletOpt
      triplet2 <- DeckOfCards.takeTripletOpt
    } yield (triplet1, triplet2)

    val (deck, (triplet1, triplet2)) = p.run(DeckOfCards.fullDeck).value

    // then
    triplet1 should be(Some(Triplet(CARD_1, CARD_2, CARD_3)))
    triplet2 should be(Some(Triplet(CARD_4, CARD_5, CARD_6)))

    // and
    deck shouldBe an[NonEmptyDeckOfCards]
  }

  it should "return no triplet once deck is empty" in {
    // when
    val p = for {
      triplet1 <- DeckOfCards.takeTripletOpt
      triplet2 <- DeckOfCards.takeTripletOpt
      triplet3 <- DeckOfCards.takeTripletOpt
    } yield (triplet1, triplet2, triplet3)

    val (deck, (triplet1, triplet2, triplet3)) = p.run(DeckOfCards.fromCards(NonEmptyList.fromListUnsafe(Card.allCards.take(3)))).value

    // then
    triplet1 should be(Some(Triplet(CARD_1, CARD_2, CARD_3)))
    triplet2 should be(None)
    triplet3 should be(None)

    // and
    deck should be(EmptyDeckOfCards)
  }
}
