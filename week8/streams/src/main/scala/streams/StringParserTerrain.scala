package streams

import scala.annotation.tailrec

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     *inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     *inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   *
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   *
   * is represented as
   *
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    def isValid(pos: Pos): Boolean = (
      pos.row < levelVector.length
      && pos.row >= 0
      && pos.col < levelVector(0).length
      && pos.col >= 0
      && levelVector(pos.row)(pos.col) != '-'
    )
    isValid
  }

  def terrainFunctionAlt(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    def valid(pos: Pos) = {
      if (levelVector.isEmpty || pos.row < 0 || pos.col < 0) false
      else {
        pos.row < levelVector.length && pos.col < levelVector(pos.row).length && levelVector(pos.row)(pos.col) != '-'
      }
    }
    valid
  }

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    @tailrec
    def findCharIter(c: Char, levelVector: Vector[Vector[Char]], accLevel: Int): Pos = {
      if (accLevel == levelVector.length) Pos(-1, -1)
      else if (levelVector(accLevel).indexOf(c) >= 0) Pos(accLevel, levelVector(accLevel).indexOf(c))
      else findCharIter(c, levelVector, accLevel + 1)
    }
    findCharIter(c, levelVector, 0)
  }

  // Excellent answer
  def findCharAlt(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val row = levelVector.indexWhere(_ contains c)
    val col = levelVector(row).indexOf(c)
    Pos(row, col)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\r?\n").map(str => Vector(str: _*)).toIndexedSeq: _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}

