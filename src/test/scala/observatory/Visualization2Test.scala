package observatory

import observatory.Visualization2.{bilinearInterpolation, visualizeGrid}
import org.junit.Assert._
import org.junit.Test

trait Visualization2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  // Implement tests for methods of the `Visualization2` object

  @Test
  def `bilinear Interpolation` = {
    assertEquals(10, bilinearInterpolation(CellPoint(0.5, 0.5), 10D, 10D, 10D, 10D), 0.00000001)
    assertEquals(2.5, bilinearInterpolation(CellPoint(0.5, 0.5), 10D, 0D, 0D, 0D), 0.00000001)
    assertEquals(10, bilinearInterpolation(CellPoint(0, 1), 10D, 10D, 10D, 10D), 0.00000001)
  }

  @Test
  def `visualize Grid`: Unit = {
    assertTrue(
      visualizeGrid(_ => 5, List((5, Color(7, 7, 7)), (8, Color(0, 0, 0))), Tile(0, 0, 0))
        .forall { case (_, _, pixel) => pixel.red == 7 && pixel.green == 7 && pixel.blue == 7 }
    )
  }

}
