package bot

import bot.Dir.Dir

import scala.collection.mutable

class MoveComputer(input: Input) {

  case class TileAndPos(tile: Tile, pos: Pos)

  case class TileAndPath(tile: Tile, pos: Pos, path: Path)

  case class MoveAndScore(name: String, move: Dir, score: Float)

  val hero = input.hero
  val game = input.game
  val board = game.board
  val pathFinder = new PathFinder(input.game.board)

  def compute(): Dir = {
    Console.print(hero.pos + " - ")

    val moves = getPossibleMoves
    moves.sortBy(-_.score)
    val best = moves.head
    Console.println(best.name + " - " + best.move)
    best.move
  }

  private def getPossibleMoves: Seq[MoveAndScore] = {
    val moves = mutable.Buffer[MoveAndScore]()

    moves ++= getDrinkOfOpportunityMoves
    moves ++= getGrabMineMoves
    moves ++= getFallbackMoves
  }

  private def getDrinkOfOpportunityMoves: Seq[MoveAndScore] = {
    val pathsToTaverns = scoredPathsTo(_ == Tile.Tavern)

    pathsToTaverns.headOption match {
      case Some(path) if path.path.length == 1 && hero.life < MoveComputer.DRINK_OF_OPPORTUNITY_THRESHOLD =>
        Seq(MoveAndScore(s"Why not take a drink while I'm here?", path.path.firstMove, MoveComputer.DRINK_OF_OPPORTUNITY_SCORE))
      case _ =>
        Nil
    }
  }

  private def getGrabMineMoves: Seq[MoveAndScore] = {
    val pathsToAvailableMines = scoredPathsTo(isAvailableMine, minimumLife = MoveComputer.GRAB_MINE_MINIMUM_LIFE)

    pathsToAvailableMines.map { path =>
      val score = MoveComputer.GRAB_MINE_SCORE - (path.path.cost * MoveComputer.GRAB_MINE_COST_FACTOR) + computeLocationScore(path.pos)
      MoveAndScore(s"Going to mine at ${path.pos}", path.path.firstMove, score)
    }
  }

  private def getFallbackMoves: Seq[MoveAndScore] = {
    scoredPathsTo(_ == Tile.Tavern).map { path =>
      val score = MoveComputer.BORED_DRINK_SCORE + computeLocationScore(path.pos)
      MoveAndScore(s"Go to tavern ${path.pos} since I have nothing to do", path.path.firstMove, score)
    }
  }

  private def scoredPathsTo(matcher: Tile => Boolean,
                            minimumLife: Int = Int.MinValue): Seq[TileAndPath] = {

    val allPaths = allTiles
      .filter(x => matcher(x.tile))
      .flatMap(tile => pathFinder.findPath(hero.pos, tile.pos).map(TileAndPath(tile.tile, tile.pos, _)))

    allPaths
      .filter(hero.life - _.path.cost >= minimumLife)
      .sortBy(_.path.cost)
  }

  private def computeLocationScore(pos: Pos): Float = {
    var score = 0f

    score += computeProximityScore(pos, isAvailableMine) * MoveComputer.LOCATION_MINE_PROXIMITY_FACTOR
    score += computeProximityScore(pos, _ == Tile.Tavern) * MoveComputer.LOCATION_TAVERN_PROXIMITY_FACTOR

    score
  }

  private def computeProximityScore(pos: Pos,
                                    matcher: Tile => Boolean): Float = {

    val allPaths = allTiles
      .filter(x => matcher(x.tile))
      .flatMap(tile => pathFinder.findPath(pos, tile.pos).map(TileAndPath(tile.tile, tile.pos, _)))

    allPaths.map(_.path.cost) match {
      case Nil =>
        0
      case costs =>
        val max = costs.max.toFloat
        costs.map(max / _).sum
    }
  }

  private def allTiles: Seq[TileAndPos] = {
    (0 until board.size).flatMap { y =>
      (0 until board.size).flatMap { x =>
        val pos = Pos(x, y)
        board.at(pos).map(TileAndPos(_, pos))
      }
    }
  }

  private def isAvailableMine(tile: Tile): Boolean = tile match {
    case mine: Tile.Mine => mine.heroId.isEmpty || !mine.heroId.contains(hero.id)
    case _ => false
  }
}

object MoveComputer {
  val MINE_COST = 25
  val MAXIMUM_LIFE = 100

  val DRINK_OF_OPPORTUNITY_SCORE = 10000000f
  val DRINK_OF_OPPORTUNITY_THRESHOLD = 90

  val GRAB_MINE_SCORE = 10000f
  val GRAB_MINE_COST_FACTOR = 5f
  val GRAB_MINE_MINIMUM_LIFE = MINE_COST + 1

  val BORED_DRINK_SCORE = -100000f

  val LOCATION_MINE_PROXIMITY_FACTOR = 5f
  val LOCATION_TAVERN_PROXIMITY_FACTOR = 5f
}
