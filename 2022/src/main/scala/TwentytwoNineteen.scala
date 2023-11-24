package twentytwo.nineteen

import lib.Support

import scala.annotation.tailrec

case class Blueprint(
    id: Int,
    oreBotOre: Int,
    clayBotOre: Int,
    obsBotOre: Int,
    obsBotClay: Int,
    geoBotOre: Int,
    geoBotObs: Int
) {
  val neededOre = Seq(clayBotOre, obsBotOre, geoBotOre).max
}
case class State(
    time: Int,
    orebots: Int,
    claybots: Int,
    obsbots: Int,
    geobots: Int,
    ore: Int,
    clay: Int,
    obs: Int,
    geo: Int
) {
  def mine: State =
    this.copy(time = time + 1, ore = ore + orebots, clay = clay + claybots, obs = obs + obsbots, geo = geo + geobots)
}
object TwentytwoNineteen extends App with Support {
  val testData =
    """
      |Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
      |""".stripMargin.trim

  val inputR =
    """Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".r

  val input = load

  def sum1n(n: Int): Int = n * (n + 1) / 2

  def run(data: String) = {
    val in = stringSeq(data).map {
      case inputR(blueprintId, oreBotOre, clayBotOre, obsBotOre, obsBotClay, geoBotOre, geoBotObs) =>
        Blueprint(
          blueprintId.toInt,
          oreBotOre.toInt,
          clayBotOre.toInt,
          obsBotOre.toInt,
          obsBotClay.toInt,
          geoBotOre.toInt,
          geoBotObs.toInt
        )
    }

    @tailrec def bestGeodeYield(blueprint: Blueprint, q: List[State], best: Int, fullTime: Int): Int = {
      q match {
        case Nil =>
          println(blueprint, best)
          best
        case state :: tail if state.time == fullTime =>
          bestGeodeYield(blueprint, tail, Math.max(best, state.geo), fullTime)
        case state :: tail
            if state.geo + (state.geobots * (fullTime - state.time)) + sum1n(fullTime - state.time) < best =>
          bestGeodeYield(blueprint, tail, best, fullTime)
        case state :: tail =>
          val mined = state.mine
          val nothing = Some(mined)
          val orebot =
            if (state.orebots < blueprint.neededOre && state.ore >= blueprint.oreBotOre)
              Some(mined.copy(orebots = mined.orebots + 1, ore = mined.ore - blueprint.oreBotOre))
            else None
          val claybot =
            if (state.claybots < blueprint.obsBotClay && state.ore >= blueprint.clayBotOre)
              Some(mined.copy(claybots = mined.claybots + 1, ore = mined.ore - blueprint.clayBotOre))
            else None
          val obsbot =
            if (
              state.obsbots < blueprint.geoBotObs && state.ore >= blueprint.obsBotOre && state.clay >= blueprint.obsBotClay
            )
              Some(
                mined.copy(
                  obsbots = mined.obsbots + 1,
                  ore = mined.ore - blueprint.obsBotOre,
                  clay = mined.clay - blueprint.obsBotClay
                )
              )
            else None
          val geobot =
            if (state.ore >= blueprint.geoBotOre && state.obs >= blueprint.geoBotObs)
              Some(
                mined.copy(
                  geobots = mined.geobots + 1,
                  ore = mined.ore - blueprint.geoBotOre,
                  obs = mined.obs - blueprint.geoBotObs
                )
              )
            else None

          val nextState = geobot.map(List(_)).getOrElse(List(obsbot, claybot, orebot, nothing).flatten)

          bestGeodeYield(blueprint, nextState ++ tail, best, fullTime)
      }
    }

    val initialState =
      State(time = 0, orebots = 1, claybots = 0, obsbots = 0, geobots = 0, ore = 0, clay = 0, obs = 0, geo = 0)

    val p1 = in.map(bp => bestGeodeYield(bp, List(initialState), 0, 24) * bp.id).sum
    println(p1)

    val p2 = in.take(3).map(bp => bestGeodeYield(bp, List(initialState), 0, 32)).product
    println(p2)
  }

  println("--- testdata ---")
//  run(testData)
  println("--- real ---")
  run(input)
}
