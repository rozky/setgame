package setgame.model

import cats.data.NonEmptyList
import setgame.BaseSpec
import setgame.Fixtures.Cards._
import setgame.model.Triplet.triplesFromListUnsafe

class TripletSpec extends BaseSpec {

  "fromList" should "create triples from list with 3 cards" in {
    // when
    val triplet = Triplet.fromList(List(CARD_1, CARD_2, CARD_3))

    // then
    triplet should be(Some(Triplet(CARD_1, CARD_2, CARD_3)))
  }

  it should "create triples from list list with more than 3 cards" in {
    // when
    val triplet = Triplet.fromList(List(CARD_1, CARD_2, CARD_3, CARD_4))

    // then
    triplet should be(Some(Triplet(CARD_1, CARD_2, CARD_3)))
  }

  it should "not create triple of list contains less than 3 items" in {
    // when
    val triplet = Triplet.fromList(List(CARD_1, CARD_2))

    // then
    triplet should be(None)
  }

  "triplesFromListUnsafe" should "create full triples" in {
    triplesFromListUnsafe(List(CARD_1, CARD_2, CARD_3)) should be(NonEmptyList.of(Triplet(CARD_1, CARD_2, CARD_3)))
    triplesFromListUnsafe(List(CARD_1, CARD_2, CARD_3, CARD_4, CARD_5, CARD_6)) should be(NonEmptyList.of(Triplet(CARD_1, CARD_2, CARD_3), Triplet(CARD_4, CARD_5, CARD_6)))
  }
}
