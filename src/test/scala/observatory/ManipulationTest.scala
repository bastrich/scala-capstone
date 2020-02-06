package observatory

import observatory.Manipulation.{average, deviation, makeGrid}
import org.junit.Assert._
import org.junit.Test

trait ManipulationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object

  @Test
  def `make Grid`: Unit = {
    val grid = makeGrid(
      List(
        (Location(90, -180), 0),
        (Location(-89, -179), 10)
      )
    )

    assertEquals(5, grid(GridLocation(0, 0)), 0.2)
  }

  @Test
  def `test average`: Unit = {
    val grid = average(
      List(
        List(
          (Location(90, -180), 0),
          (Location(-89, -179), 10)
        ),
        List(
          (Location(90, -180), 10),
          (Location(-89, -179), 20)
        )
      )
    )

    assertEquals(10, grid(GridLocation(0, 0)), 0.2)
  }

  @Test
  def `test deviation`: Unit = {
    val grid = deviation(
      List(
        (Location(90, -180), 0),
        (Location(-89, -179), 10)
      ),
      _ => 0
    )

    assertEquals(5, grid(GridLocation(0, 0)), 0.2)
  }
}
