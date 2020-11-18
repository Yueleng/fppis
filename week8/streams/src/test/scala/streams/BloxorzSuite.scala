package streams

import org.junit._
import org.junit.Assert.assertEquals

import Bloxorz._

class BloxorzSuite {
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        if (!block.isLegal) println("b1: " + block.b1 + "b2: " + block.b2)
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
      assertEquals(Pos(4,7), goal)
    }

  @Test def `isStanding Test`: Unit =
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isStanding, "Pos(1,1), Pos(1,1) should be standing")
      assert(!Block(Pos(1,1), Pos(2,1)).isStanding, "Pos(1,1), Pos(2,1) should not be standing")
    }

  @Test def `isLegal Test`: Unit =
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isLegal, "Pos(1,1), Pos(1,1) should be legal")
      assert(!Block(Pos(0,2), Pos(0,3)).isStanding, "Pos(0,2), Pos(0,3) should not be legal")
    }

  @Test def `startPos && startBlock Test`: Unit =
    new Level1 {
      assertEquals(startPos, Pos(1,1))
      assertEquals(startBlock, Block(Pos(1,1), Pos(1,1)))
    }

  @Test def `neighbors Test`: Unit =
    new Level1 {
      // is standing
      assertEquals(
        Block(Pos(1,1), Pos(1,1)).neighbors.toSet,
        Set(
          (Block(Pos(1,2), Pos(1,3)), Right),
          (Block(Pos(1,-1), Pos(1,0)), Left),
          (Block(Pos(2,1), Pos(3,1)), Down),
          (Block(Pos(-1,1), Pos(0,1)), Up)
        )
      )

      // not standing
      assertEquals(
        Block(Pos(1,1), Pos(1,2)).neighbors.toSet,
        Set(
          (Block(Pos(1,3), Pos(1,3)), Right),
          (Block(Pos(1,0), Pos(1,0)), Left),
          (Block(Pos(2,1), Pos(2,2)), Down),
          (Block(Pos(0,1), Pos(0,2)), Up)
        )
      )
    }

  @Test def `legalNeighbors Test`: Unit =
    new Level1 {
      // is standing
      assertEquals(
        Block(Pos(1,1), Pos(1,1)).legalNeighbors.toSet,
        Set(
          (Block(Pos(1,2), Pos(1,3)), Right),
          (Block(Pos(2,1), Pos(3,1)), Down)
        )
      )

      // not standing
      assertEquals(
        Block(Pos(1,1), Pos(1,2)).legalNeighbors.toSet,
        Set(
          (Block(Pos(1,3), Pos(1,3)), Right),
          (Block(Pos(1,0), Pos(1,0)), Left),
          (Block(Pos(2,1), Pos(2,2)), Down),
          (Block(Pos(0,1), Pos(0,2)), Up),
        )
      )
    }

  @Test def `done Test`: Unit =
    new Level1 {
      assert(!done(Block(startPos, startPos)), "Start position is not done yet.")
      assert(done(Block(goal, goal)), "Goal position is done")
    }


  @Test def `neighborsWithHistory Test`: Unit =
    new Level1 {
      // startPos
      assertEquals(
        neighborsWithHistory(Block(Pos(1,1), Pos(1,1)), List(Left, Up)).toSet,
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        )
      )

      // edgePos(s)
      assertEquals(
        neighborsWithHistory(Block(Pos(3,1), Pos(3,1)), List(Left, Up)).toSet,
        Set(
          (Block(Pos(3,2),Pos(3,3)), List(Right,Left,Up)),
          (Block(Pos(1,1),Pos(2,1)), List(Up,Left,Up))
        )
      )

      assertEquals(
        neighborsWithHistory(Block(Pos(3,6), Pos(3,7)), List(Left, Up)).toSet,
        Set(
          (Block(Pos(3,8),Pos(3,8)), List(Right,Left,Up)),
          (Block(Pos(2,6),Pos(2,7)), List(Up,Left,Up)),
          (Block(Pos(3,5),Pos(3,5)), List(Left,Left,Up)),
          (Block(Pos(4,6),Pos(4,7)), List(Down,Left,Up))
        )
      )
    }


//  @Test def `newNeighborsOnly Test`: Unit =
//    new Level1 {
//      assert()
//    }



  @Test def `optimal solution for level 1 (5pts)`: Unit =
  new Level1 {
    assertEquals(Block(goal, goal), solve(solution))
  }


  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
