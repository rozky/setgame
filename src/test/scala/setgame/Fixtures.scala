package setgame

import setgame.model.Card
import setgame.model.Card.ColorFeature.{Purple, Red}
import setgame.model.Card.NumberFeature.{One, Three, Two}
import setgame.model.Card.ShadingFeature.{Outlined, Solid, Striped}
import setgame.model.Card.ShapeFeature.Oval

object Fixtures {
  object Cards {
    val CARD_1 = Card(Oval, Red, One, Solid)
    val CARD_2 = Card(Oval, Red, One, Striped)
    val CARD_3 =  Card(Oval, Red, One, Outlined)
    val CARD_4 = Card(Oval, Red, Two, Solid)
    val CARD_5 = Card(Oval, Red, Two, Striped)
    val CARD_6 = Card(Oval, Red, Two, Outlined)
    val CARD_7 = Card(Oval, Red, Three, Solid)
    val CARD_8 = Card(Oval, Red, Three, Striped)
    val CARD_9 = Card(Oval, Red, Three, Outlined)
    val CARD_10 = Card(Oval, Purple, One, Solid)
    val CARD_11 = Card(Oval, Purple, One, Striped)
    val CARD_12 = Card(Oval, Purple, One, Outlined)
    val CARD_13 = Card(Oval, Purple, Two, Solid)
    val CARD_14 = Card(Oval, Purple, Two, Striped)
    val CARD_15 = Card(Oval, Purple, Two, Outlined)
    val CARD_16 = Card(Oval, Purple, Three, Solid)
    val CARD_17 = Card(Oval, Purple, Three, Striped)
    val CARD_18 = Card(Oval, Purple, Three, Outlined)
  }
}
