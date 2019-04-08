package setgame.model

import setgame.BaseSpec

class PlayerIdSpec extends BaseSpec {

  "PlayerId" should "have numeric id" in {
    PlayerId.Player_1.id should be(0)
    PlayerId.Player_2.id should be(1)
    PlayerId.Player_3.id should be(2)
    PlayerId.Player_4.id should be(3)
    PlayerId.Player_5.id should be(4)
    PlayerId.Player_6.id should be(5)
    PlayerId.Player_7.id should be(6)
    PlayerId.Player_8.id should be(7)
    PlayerId.Player_9.id should be(8)
  }
}
