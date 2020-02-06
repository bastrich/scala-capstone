package observatory

import observatory.Visualization.{interpolateColor, predictTemperature, visualize}
import org.junit.Assert._
import org.junit._

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  @Test
  def `interpolate color`: Unit = {
    val points = List(
      (3D, Color(0, 0, 0)),
      (5D, Color(1, 1, 1)),
      (6D, Color(3, 3, 3)),
      (8D, Color(6, 6, 6))
    )

    assertEquals(Color(2, 2, 2), interpolateColor(points, 5.5D))
  }

  @Test
  def `predict temperature 1`: Unit = {
    val temperatures = List((Location(0.0, 0.0), 10.0))
    val location = Location(88.0, -176.0)
    assertEquals(10, predictTemperature(temperatures, location), 0.001)
  }

  @Test
  def `predict temperature 2`: Unit = {
    val temperatures = List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0))
    val location = Location(45.0, 90.0)
    assertTrue(predictTemperature(temperatures, location) <= 17.0)
  }

  @Test
  def `predict temperature 3`: Unit = {
    val temperatures = List((Location(45.0, -90.0), 0.0), (Location(-45.0, 0.0), -7.092675394838437))
    val location = Location(45.0, 90.0)
    assertEquals(-1.7047676765041955, predictTemperature(temperatures, location), 0.001)
  }

  @Test
  def `test visualize`: Unit = {
    val image = visualize(List((Location(0, 0), 5)), List((5, Color(7, 7, 7))))
    assertEquals(360, image.width)
    assertEquals(180, image.height)

    assertTrue(
      image.forall { case (_, _, pixel) => pixel.red == 7 && pixel.green == 7 && pixel.blue == 7 }
    )
  }

}