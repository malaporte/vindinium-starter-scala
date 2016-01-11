package bot

import bot.Dir.Dir

import scala.collection.mutable

case class Path(from: Pos,
                to: Pos,
                positions: Seq[Pos],
                cost: Int) {

  def firstMove: Dir = {
    positions.headOption match {
      case Some(pos) =>
        from.dirTo(pos)
      case None =>
        Dir.Stay
    }
  }

  def length = positions.length
}

class PathFinder(board: Board) {
  private val cache = mutable.Map[(Pos, Pos), Option[Path]]()

  def findPath(from: Pos, to: Pos): Option[Path] = {
    cache.getOrElse(from -> to, {
      val path = computePath(from, to)
      cache.put(from -> to, path)
      path
    })
  }

  private def computePath(from: Pos, to: Pos): Option[Path] = {
    val open = mutable.Set[Pos](from)
    val closed = mutable.Set[Pos]()
    val g = mutable.Map(from -> 0)
    val f = mutable.Map(from -> heuristic(from, to))
    val cameFrom = mutable.Map[Pos, Pos]()

    while (open.nonEmpty) {
      val current = open.minBy(f(_))

      if (current.neighbors.contains(to)) {
        def reconstructPath(node: Pos): Seq[Pos] = {
          cameFrom.get(node) match {
            case Some(origin) => reconstructPath(origin) :+ node
            case None => Nil
          }
        }

        val path = reconstructPath(current) :+ to
        return Some(Path(from, to, path, g(current)))
      }

      open -= current
      closed += current

      current
        .neighbors
        .filter(x => board.at(x).contains(Tile.Air))
        .filter(!closed.contains(_))
        .foreach { neighbor =>

          val c = g(current) + cost(current, neighbor)

          if (open.contains(neighbor) && c < g(neighbor)) {
            open.remove(neighbor)
          }

          if (closed.contains(neighbor) && c < g(neighbor)) {
            closed.remove(neighbor)
          }

          if (!open.contains(neighbor) && !closed.contains(neighbor)) {
            g.put(neighbor, c)
            f.put(neighbor, c + heuristic(neighbor, to))
            cameFrom.put(neighbor, current)
            open += neighbor
          }
        }
    }

    None
  }

  private def walkableNeighbors(pos: Pos): Set[Pos] = {
    pos.neighbors.filter(x => board.at(x).contains(Tile.Air))
  }

  private def heuristic(from: Pos, to: Pos): Int = {
    Math.abs(from.x - to.x) + Math.abs(from.y - to.y)
  }

  private def cost(from: Pos, to: Pos): Int = {
    1
  }
}

