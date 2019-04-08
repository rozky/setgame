package setgame.model

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, State}
import setgame.model.PlayerId.PlayerId

case class GameScore(private val scores: NonEmptyMap[PlayerId, Int]) {
  def getPlayerScore(player: PlayerId): Option[Int] = scores.lookup(player)

  def winners: NonEmptyList[PlayerId] = {
    val winners: List[PlayerId] = scores.toNel
      .toList
      .groupBy(_._2)
      .maxBy(_._1)
      ._2
      .map(_._1)
    NonEmptyList.fromListUnsafe(winners)
  }
}

object GameScore {

  def forPlayers(players: NonEmptySet[PlayerId]): GameScore = {
    GameScore(players.toNonEmptyList.map(p => (p, 0)).toNem)
  }

  def increment(player: PlayerId): State[GameScore, Unit] =  State {
    case GameScore(scores) if scores.contains(player) => (GameScore(scores.updateWith(player)(_ + 1)), ())
    case scores => (scores, ())
  }

  def decrement(player: PlayerId): State[GameScore, Unit] =  State {
    case GameScore(scores) if scores.contains(player) => (GameScore(scores.updateWith(player)(_ - 1)), ())
    case scores => (scores, ())
  }
}
