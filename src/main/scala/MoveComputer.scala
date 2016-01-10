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
      case Some(path) if path.path.length == 1 && hero.life < MoveComputer.THIRST_OF_OPPORTUNITY_THRESHOLD =>
        Seq(MoveAndScore(s"Why not take a drink while I'm here?", path.path.firstMove, Int.MaxValue))
      case _ =>
        Nil
    }
  }

  private def getGrabMineMoves: Seq[MoveAndScore] = {
    val pathsToAvailableMines = scoredPathsTo({
      case mine: Tile.Mine => mine.heroId.isEmpty || !mine.heroId.contains(hero.id)
      case _ => false
    }, minimumLife = MoveComputer.MINIMUM_LIFE_FOR_MINE)

    pathsToAvailableMines.map { path =>
      val score = -path.path.cost
      MoveAndScore(s"Going to mine at ${path.pos}", path.path.firstMove, score)
    }
  }

  private def getFallbackMoves: Seq[MoveAndScore] = {
    scoredPathsTo(_ == Tile.Tavern).headOption match {
      case Some(path) =>
        Seq(MoveAndScore("Go to tavern since I have nothing to do", path.path.firstMove, Float.MinValue))
      case None =>
        Nil
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

  private def allTiles: Seq[TileAndPos] = {
    (0 until board.size).flatMap { y =>
      (0 until board.size).flatMap { x =>
        val pos = Pos(x, y)
        board.at(pos).map(TileAndPos(_, pos))
      }
    }
  }
}

object MoveComputer {
  val MINE_COST = 25
  val MAXIMUM_LIFE = 100

  val THIRST_THRESHOLD = MINE_COST
  val THIRST_OF_OPPORTUNITY_THRESHOLD = 90
  val MINIMUM_LIFE_FOR_MINE = MINE_COST + 1
}
