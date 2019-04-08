package setgame.model

import cats.Order

object PlayerId extends Enumeration {
  type PlayerId = Value
  val Player_1, Player_2,Player_3, Player_4, Player_5, Player_6, Player_7, Player_8, Player_9 = Value

  implicit def playerOrder: Order[PlayerId] = Order.from(_.id - _.id)
}