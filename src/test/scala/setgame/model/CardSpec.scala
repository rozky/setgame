package setgame.model

import setgame.Fixtures.Cards._
import setgame.{BaseSpec, Fixtures}

class CardSpec extends BaseSpec {

  "allCards" should "be correct" in {
    Card.allCards should have size(81)
    Card.allCards(0) should be(CARD_1)
    Card.allCards(1) should be(CARD_2)
    Card.allCards(2) should be(CARD_3)
    Card.allCards(3) should be(CARD_4)
    Card.allCards(4) should be(CARD_5)
    Card.allCards(5) should be(CARD_6)
  }
}
