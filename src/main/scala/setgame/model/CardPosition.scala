package setgame.model

import cats.Order

object CardPosition extends Enumeration {
  type CardPosition = Value
  val Pos_1, Pos_2, Pos_3, Pos_4, Pos_5, Pos_6, Pos_7, Pos_8, Pos_9, Pos_10, Pos_11, Pos_12, Pos_13, Pos_14, Pos_15 = Value

  implicit def cardPositionOrder: Order[CardPosition] = Order.from(_.id - _.id)
}
