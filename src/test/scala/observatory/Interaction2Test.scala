package observatory

import observatory.Interaction2._
import org.junit.Assert._
import org.junit.Test

trait Interaction2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive user interface", 6) _

  // Implement tests for methods of the `Interaction2` object

  @Test
  def `available Layers`: Unit = {
    assertEquals(
      List(
        Layer(LayerName.Temperatures, TemperaturesColorScale, 1975 to 2015),
        Layer(LayerName.Deviations, DeviationsColorScale, 1990 to 2015)
      ),
      availableLayers
    )
  }

  @Test
  def `year Bounds`: Unit = {
    assertEquals(
      1975 to 2015,
      yearBounds(Signal(Layer(LayerName.Temperatures, TemperaturesColorScale, 1975 to 2015)))()
    )
  }

  @Test
  def `year Selection`: Unit = {
    assertEquals(
      1975,
      yearSelection(Signal(Layer(LayerName.Temperatures, TemperaturesColorScale, 1975 to 2015)), Signal(1970))()
    )
  }

  @Test
  def `layer Url Pattern`: Unit = {
    assertEquals(
      "target/temperatures/1980/{z}/{x}-{y}.png",
      layerUrlPattern(Signal(Layer(LayerName.Temperatures, TemperaturesColorScale, 1975 to 2015)), Signal(1980))()
    )
  }

  @Test
  def `test caption`: Unit = {
    assertEquals(
      "Temperatures (1990)",
      caption(Signal(Layer(LayerName.Temperatures, TemperaturesColorScale, 1975 to 2015)), Signal(1990))()
    )
  }
}
