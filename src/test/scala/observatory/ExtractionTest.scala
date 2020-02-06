package observatory

import java.time.LocalDate

import org.junit.Assert._
import org.junit.Test
import Extraction.locateTemperatures
import Extraction.locationYearlyAverageRecords

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  @Test
  def `locate Temperatures`(): Unit = {
    assertEquals(
      List(
        (LocalDate.of(2077, 9, 26), Location(32.950, -65.567), -16.11111111111111D),
        (LocalDate.of(2077, 9, 25), Location(0, 0), -15D)
      ),
      locateTemperatures(2077, "/test-stations.csv", "/2077.csv")
    )
  }

  @Test
  def `location Yearly Average Records`(): Unit = {
    assertEquals(
      List(
        (Location(32.950, -65.567), -16.11111111111111D),
        (Location(0, 0), -15D)
      ),
      locationYearlyAverageRecords(
        List(
          (LocalDate.of(2077, 9, 26), Location(32.950, -65.567), -16.11111111111111D),
          (LocalDate.of(2077, 9, 25), Location(0, 0), -15D)
        )
      )
    )
  }


}
