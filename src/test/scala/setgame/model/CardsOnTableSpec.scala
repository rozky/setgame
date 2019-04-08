package setgame.model

import cats.data.NonEmptyList
import setgame.BaseSpec
import setgame.Fixtures.Cards._
import setgame.model.CardPosition._
import setgame.model.CardsOnTable.{AllCardsOnTable, MissingCardsOnTable}

class CardsOnTableSpec extends BaseSpec {

  "fromCards" should "create an instance from 3 cards" in {
    // when
    val table = CardsOnTable.fromCards(NonEmptyList.of(CARD_1, CARD_2, CARD_3))

    // then
    table.getCardAtPosition(Pos_1) should be(Some(CARD_1))
    table.getCardAtPosition(Pos_2) should be(Some(CARD_2))
    table.getCardAtPosition(Pos_3) should be(Some(CARD_3))
    CardPosition.values.toList.drop(3).foreach { position =>
      table.getCardAtPosition(position) should be(None)
    }
  }

  it should "fail if number of cards is more than 15" in {
    // when
    val exception = intercept[IllegalArgumentException](CardsOnTable.fromCards(NonEmptyList.fromListUnsafe(List.fill(16)(CARD_1))))

    // then
    exception.getMessage should be("requirement failed: Too many cards")
  }

  "removeCards" should "remove existing cards and return removed cards" in {
    val table = CardsOnTable.fromCards(NonEmptyList.fromListUnsafe(Card.allCards.take(12)))

    // when
    val (updatedTable, removedCards) = CardsOnTable.removeCards(Triplet(Pos_1, Pos_2, Pos_3)).run(table).value

    // then
    updatedTable.getCardAtPosition(Pos_1) should be(None)
    updatedTable.getCardAtPosition(Pos_2) should be(None)
    updatedTable.getCardAtPosition(Pos_3) should be(None)
    updatedTable.getCardAtPosition(Pos_4) should be(Some(CARD_4))
    updatedTable shouldBe a[MissingCardsOnTable]

    // and
    removedCards should be(Some(Triplet(CARD_1, CARD_2, CARD_3)))
  }

  it should "fail if removing card from already empty position" in {
    val table = CardsOnTable.fromCards(NonEmptyList.fromListUnsafe(Card.allCards.take(9)))

    // when
    val exception = intercept[IllegalArgumentException](CardsOnTable.removeCards(Triplet(Pos_8, Pos_9, Pos_10)).run(table).value)

    // then
    exception.getMessage should be("requirement failed: There no cards at picked slots")
  }

  "addCards" should "add cards to table with 3 empty slots" in {
    val table = CardsOnTable.fromCards(NonEmptyList.fromListUnsafe(Card.allCards.take(12))).asInstanceOf[MissingCardsOnTable]

    // when
    val (updatedTable, _) = CardsOnTable.addCards(Triplet(CARD_13, CARD_14, CARD_15)).run(table).value

    // then
    updatedTable.getCardAtPosition(Pos_13) should be(Some(CARD_13))
    updatedTable.getCardAtPosition(Pos_14) should be(Some(CARD_14))
    updatedTable.getCardAtPosition(Pos_15) should be(Some(CARD_15))

    // and
    updatedTable shouldBe a[AllCardsOnTable]
  }

  it should "add cards to table with more than 3 empty slots" in {
    val table = CardsOnTable.fromCards(NonEmptyList.fromListUnsafe(Card.allCards.take(9))).asInstanceOf[MissingCardsOnTable]

    // when
    val (updatedTable, _) = CardsOnTable.addCards(Triplet(CARD_13, CARD_14, CARD_15)).run(table).value

    // then
    updatedTable.getCardAtPosition(Pos_10) should be(Some(CARD_13))
    updatedTable.getCardAtPosition(Pos_11) should be(Some(CARD_14))
    updatedTable.getCardAtPosition(Pos_12) should be(Some(CARD_15))

    // and
    updatedTable shouldBe a[MissingCardsOnTable]
  }
}
