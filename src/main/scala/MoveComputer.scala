package bot

import bot.Dir.Dir

import scala.collection.mutable

class MoveComputer(input: Input) {

  import MoveComputer._

  case class TileAndPos(tile: Tile, pos: Pos)

  case class TileAndPath(tile: Tile, pos: Pos, path: Path)

  case class MoveAndScore(name: String, move: Dir, score: Float)

  val hero = input.hero
  val game = input.game
  val board = game.board
  val pathFinder = new PathFinder(input.game.board)
  val allTiles = enumerateRelevantTiles

  def compute(): Dir = {
    Console.print(hero.pos + " - ")

    val moves = getPossibleMoves
    moves.sortBy(-_.score)
    moves.headOption match {
      case Some(best) =>
        Console.println(best.name + " - " + best.move)
        best.move
      case _ =>
        Console.println("I can't do nothing!")
        Dir.Stay
    }
  }

  private def getPossibleMoves: Seq[MoveAndScore] = {
    val moves = mutable.Buffer[MoveAndScore]()

    moves ++= getDrinkOfOpportunityMoves
    moves ++= getGrabMineMoves
    moves ++= getAggressiveMoves
    moves ++= getFallbackMoves
  }

  private def getDrinkOfOpportunityMoves: Seq[MoveAndScore] = {
    val pathsToTaverns = pathsOrderedByCost(isTavern)

    pathsToTaverns.headOption match {
      case Some(path) if path.path.length == 1 && hero.life < DRINK_OF_OPPORTUNITY_THRESHOLD =>
        Seq(MoveAndScore(s"Why not take a drink while I'm here?", path.path.firstMove, DRINK_OF_OPPORTUNITY_SCORE))
      case _ =>
        Nil
    }
  }

  private def getGrabMineMoves: Seq[MoveAndScore] = {
    val minimumLife = if (hero.mineCount >= GRAB_MINE_HIGH_ON_MINES_THRESHOLD) {
      GRAB_MINE_MINIMUM_LIFE_WHEN_HIGH_ON_MINES
    } else {
      GRAB_MINE_MINIMUM_LIFE_WHEN_LOW_ON_MINES
    }

    val pathsToAvailableMines = pathsOrderedByCost(isAvailableMine, minimumLifeAtDestination = minimumLife)

    pathsToAvailableMines.map { path =>
      val mine = path.tile.asInstanceOf[Tile.Mine]
      val owner = mine.heroId.map(findEnemy)
      var score = GRAB_MINE_SCORE - (path.path.cost * GRAB_MINE_COST_FACTOR) + computeLocationScore(path.pos)

      if (owner.isDefined) {
        score += GRAB_MINE_STEAL_SCORE
        if (owner.get.gold > hero.gold) score += GRAB_MINE_STEAL_FROM_LEADER_SCORE
      }

      MoveAndScore(s"Going to mine at ${path.pos}", path.path.firstMove, score)
    }
  }

  private def getAggressiveMoves: Seq[MoveAndScore] = {
    val pathsToEnemies = pathsOrderedByCost(isAttractiveEnemy, minimumLifeAtDestination = ATTACK_MINIMUM_LIFE)

    pathsToEnemies.map { path =>
      val score = ATTACK_SCORE - (path.path.cost * ATTACK_COST_FACTOR) + computeLocationScore(path.pos)
      MoveAndScore(s"Going to mine at ${path.pos}", path.path.firstMove, score)
    }
  }

  private def getFallbackMoves: Seq[MoveAndScore] = {
    pathsOrderedByCost(isTavern).map { path =>
      val score = BORED_DRINK_SCORE + computeLocationScore(path.pos)
      MoveAndScore(s"Go to tavern ${path.pos} since I have nothing to do", path.path.firstMove, score)
    }
  }

  private def pathsOrderedByCost(matcher: Tile => Boolean,
                                 minimumLifeAtDestination: Int = Int.MinValue): Seq[TileAndPath] = {

    val allPaths = allTiles
      .filter(x => matcher(x.tile))
      .flatMap(tile => pathFinder.findPath(hero.pos, tile.pos).map(TileAndPath(tile.tile, tile.pos, _)))

    allPaths
      .filter(hero.life - _.path.cost >= minimumLifeAtDestination)
      .sortBy(_.path.cost)
  }

  private def computeLocationScore(pos: Pos): Float = {
    var score = 0f

    score += computeProximityFactor(pos, isAvailableMine) * LOCATION_MINE_PROXIMITY_FACTOR
    score += computeProximityFactor(pos, isTavern) * LOCATION_TAVERN_PROXIMITY_FACTOR
    score += computeProximityFactor(pos, isAttractiveEnemy) * LOCATION_ATTRACTIVE_ENEMY_PROXIMITY_FACTOR
    score += computeProximityFactor(pos, isThreateningEnemy) * LOCATION_THREATENING_ENEMY_PROXIMITY_FACTOR

    score
  }

  // This will return a value tending to 1 if all matching objects are near
  private def computeProximityFactor(pos: Pos,
                                     matcher: Tile => Boolean): Float = {

    val allPaths = allTiles
      .filter(x => matcher(x.tile))
      .flatMap(tile => pathFinder.findPath(pos, tile.pos).map(TileAndPath(tile.tile, tile.pos, _)))

    allPaths
      .map(x => Math.min(x.path.length, board.size))
      .map(_.toFloat)
    match {
      case Nil => 0f
      case lengths =>
        val max = lengths.max
        lengths.map(_ / max).sum / lengths.size
    }
  }

  private def enumerateRelevantTiles: Seq[TileAndPos] = {
    (0 until board.size).flatMap { y =>
      (0 until board.size).flatMap { x =>
        val pos = Pos(x, y)
        board.at(pos).map(TileAndPos(_, pos))
      }
    }
  }

  private def isTavern(tile: Tile): Boolean = tile match {
    case Tile.Tavern => true
    case _ => false
  }

  private def isAvailableMine(tile: Tile): Boolean = tile match {
    case mine: Tile.Mine => mine.heroId.isEmpty || !mine.heroId.contains(hero.id)
    case _ => false
  }

  private def isAttractiveEnemy(tile: Tile): Boolean = tile match {
    case enemy: Tile.Hero if isEnemy(enemy) =>
      val found = findEnemy(enemy.id)
      found.life <= hero.life - ATTACK_MINIMUM_LIFE_DIFFERENCE && found.mineCount >= ATTACK_MINIMUM_MINES
    case _ => false
  }

  private def isThreateningEnemy(tile: Tile): Boolean = tile match {
    case enemy: Tile.Hero if isEnemy(enemy) =>
      val found = findEnemy(enemy.id)
      !found.crashed && found.life >= THREATENING_ENEMY_MINIMUM_LIFE
    case _ => false
  }

  private def isEnemy(enemy: Tile.Hero): Boolean = {
    enemy.id != hero.id
  }

  private def findEnemy(id: Int): Hero = {
    game.heroes.find(_.id == id).get
  }
}

object MoveComputer {
  val MINE_COST = 25
  val MAXIMUM_LIFE = 100

  val WEAK_THRESHOLD = 30

  val DRINK_OF_OPPORTUNITY_SCORE = 10000000f
  val DRINK_OF_OPPORTUNITY_THRESHOLD = 90

  val GRAB_MINE_SCORE = 10000f
  val GRAB_MINE_COST_FACTOR = 5f
  val GRAB_MINE_HIGH_ON_MINES_THRESHOLD = 2
  val GRAB_MINE_MINIMUM_LIFE_WHEN_LOW_ON_MINES = 26
  val GRAB_MINE_MINIMUM_LIFE_WHEN_HIGH_ON_MINES = 45
  val GRAB_MINE_STEAL_SCORE = 1000f
  val GRAB_MINE_STEAL_FROM_LEADER_SCORE = 1000f

  val ATTACK_SCORE = 20000f
  val ATTACK_MINIMUM_MINES = 2
  val ATTACK_COST_FACTOR = 20f
  val ATTACK_MINIMUM_LIFE = 30
  val ATTACK_MINIMUM_LIFE_DIFFERENCE = 20

  val BORED_DRINK_SCORE = -100000f

  val PATH_SCORE_POSITIONS = 5
  val LOCATION_MINE_PROXIMITY_FACTOR = 1000f
  val LOCATION_TAVERN_PROXIMITY_FACTOR = 1000f
  val LOCATION_ATTRACTIVE_ENEMY_PROXIMITY_FACTOR = 2000f
  val LOCATION_THREATENING_ENEMY_PROXIMITY_FACTOR = -5000f

  val THREATENING_ENEMY_MINIMUM_LIFE = 40
}
