package setgame.model

import cats.data.{NonEmptyList, NonEmptySet}
import setgame.BaseSpec
import setgame.model.PlayerId._

class GameScoreSpec extends BaseSpec {

  "forPlayers" should "create scores for players" in {
    // when
    val scores = GameScore.forPlayers(NonEmptySet.of(Player_1, Player_2))

    // then
    scores.getPlayerScore(Player_1) should be(Some(0))
    scores.getPlayerScore(Player_2) should be(Some(0))
    scores.getPlayerScore(Player_3) should be(None)
  }

  "increment" should "should increment player score" in {
    // when
    val s = for {
      _ <- GameScore.increment(Player_1)
    } yield ()

    val scores = s.runS(GameScore.forPlayers(NonEmptySet.of(Player_1, Player_2))).value

    // then
    scores.getPlayerScore(Player_1) should be(Some(1))
    scores.getPlayerScore(Player_2) should be(Some(0))
    scores.getPlayerScore(Player_3) should be(None)
  }

  it should "should increment multiple player scores multiple times" in {
    // when
    val s = for {
      _ <- GameScore.increment(Player_1)
      _ <- GameScore.increment(Player_2)
      _ <- GameScore.increment(Player_3)
      _ <- GameScore.increment(Player_2)
    } yield ()

    val scores = s.runS(GameScore.forPlayers(NonEmptySet.of(Player_1, Player_2))).value

    // then
    scores.getPlayerScore(Player_1) should be(Some(1))
    scores.getPlayerScore(Player_2) should be(Some(2))
    scores.getPlayerScore(Player_3) should be(None)
  }

  "decrement" should "should decrement player score" in {
    // when
    val s = for {
      _ <- GameScore.decrement(Player_1)
    } yield ()

    val scores = s.runS(GameScore.forPlayers(NonEmptySet.of(Player_1, Player_2))).value

    // then
    scores.getPlayerScore(Player_1) should be(Some(-1))
    scores.getPlayerScore(Player_2) should be(Some(0))
    scores.getPlayerScore(Player_3) should be(None)
  }

  it should "should decrement multiple player scores multiple times" in {
    // when
    val s = for {
      _ <- GameScore.decrement(Player_1)
      _ <- GameScore.decrement(Player_2)
      _ <- GameScore.decrement(Player_3)
      _ <- GameScore.decrement(Player_2)
    } yield ()

    val scores = s.runS(GameScore.forPlayers(NonEmptySet.of(Player_1, Player_2))).value

    // then
    scores.getPlayerScore(Player_1) should be(Some(-1))
    scores.getPlayerScore(Player_2) should be(Some(-2))
    scores.getPlayerScore(Player_3) should be(None)
  }

  "winners" should "get single winner with highest score" in {
    // when
    val s = for {
      _ <- GameScore.increment(Player_1)
      _ <- GameScore.increment(Player_2)
      _ <- GameScore.decrement(Player_3)
      _ <- GameScore.decrement(Player_3)
      _ <- GameScore.increment(Player_2)
    } yield ()

    val winners = s.runS(GameScore.forPlayers(NonEmptySet.of(Player_1, Player_2, Player_3))).value.winners

    // then
    winners should be(NonEmptyList.of(Player_2))
  }

  it should "get multiple winners" in {
    // when
    val s = for {
      _ <- GameScore.decrement(Player_1)
      _ <- GameScore.decrement(Player_3)
      _ <- GameScore.decrement(Player_3)
      _ <- GameScore.decrement(Player_2)
    } yield ()

    val winners = s.runS(GameScore.forPlayers(NonEmptySet.of(Player_1, Player_2, Player_3))).value.winners

    // then
    winners should be(NonEmptyList.of(Player_1, Player_2))
  }
}
