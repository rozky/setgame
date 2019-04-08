package setgame.model

import cats.data.NonEmptyList
import setgame.model.Card.ColorFeature.ColorFeature
import setgame.model.Card.NumberFeature.NumberFeature
import setgame.model.Card.ShapeFeature.Shape
import setgame.model.Card.ShadingFeature.ShadingFeature

case class Card(shape: Shape,
                color: ColorFeature,
                number: NumberFeature,
                shading: ShadingFeature)

object Card {
  private[model] val allCards: List[Card] = {
    for {
      shape <- Card.ShapeFeature.values.toList
      color <- Card.ColorFeature.values.toList
      number <- Card.NumberFeature.values.toList
      shading <- Card.ShadingFeature.values.toList
    } yield Card(shape, color, number, shading)
  }

  private[model] def getCards(n: Int): NonEmptyList[Card] = {
    require(n > 0)

    NonEmptyList.fromListUnsafe(allCards.take(n))
  }

  object ShapeFeature extends Enumeration {
    type Shape = Value
    val Oval = Value(1)
    val Squiggles = Value(2)
    val Diamonds = Value(4)
  }

  object ColorFeature extends Enumeration {
    type ColorFeature = Value
    val Red = Value(1)
    val Purple = Value(2)
    val Green = Value(4)
  }

  object NumberFeature extends Enumeration {
    type NumberFeature = Value
    val One = Value(1)
    val Two = Value(2)
    val Three = Value(4)
  }

  object ShadingFeature extends Enumeration {
    type ShadingFeature = Value
    val Solid = Value(1)
    val Striped = Value(2)
    val Outlined = Value(4)
  }
}
