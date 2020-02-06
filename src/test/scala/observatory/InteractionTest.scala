package observatory

import java.util.concurrent.CopyOnWriteArrayList

import observatory.Interaction.{generateTiles, pixelLocation, tile, tileLocation}
import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable
import scala.collection.concurrent


trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  @Test
  def `tile location`: Unit = {
    assertEquals(Location(0, 0), tileLocation(Tile(1, 1, 1)))
  }

  @Test
  def `pixel Location`: Unit = {
    assertEquals(Location(-10.487811882056686, 7.03125), pixelLocation(10, 15, Tile(1, 1, 1)))
  }

  @Test
  def `test tile`: Unit = {
    assertTrue(
      tile(List((Location(0, 0), 10.0)), List((10.0, Color(255, 0, 0))), Tile(0, 0, 0))
        .forall { case (_, _, pixel) => pixel.red == 255 && pixel.green == 0 && pixel.blue == 0 && pixel.alpha == 127 }
    )
  }

  @Test
  def `generate Tiles`: Unit = {
    val traversed = new CopyOnWriteArrayList[(Year, Tile, String)]()
    generateTiles[String](List((1, "123"), (2, "456")), { case (y, t, d) =>
      traversed.add((y, t, d))
    })

    val expected = List(
      (1, Tile(0, 0, 0), "123"),
      (1, Tile(0, 0, 1), "123"),
      (1, Tile(0, 1, 1), "123"),
      (1, Tile(1, 0, 1), "123"),
      (1, Tile(1, 1, 1), "123"),
      (1, Tile(0, 0, 2), "123"),
      (1, Tile(0, 1, 2), "123"),
      (1, Tile(0, 2, 2), "123"),
      (1, Tile(0, 3, 2), "123"),
      (1, Tile(1, 0, 2), "123"),
      (1, Tile(1, 1, 2), "123"),
      (1, Tile(1, 2, 2), "123"),
      (1, Tile(1, 3, 2), "123"),
      (1, Tile(2, 0, 2), "123"),
      (1, Tile(2, 1, 2), "123"),
      (1, Tile(2, 2, 2), "123"),
      (1, Tile(2, 3, 2), "123"),
      (1, Tile(3, 0, 2), "123"),
      (1, Tile(3, 1, 2), "123"),
      (1, Tile(3, 2, 2), "123"),
      (1, Tile(3, 3, 2), "123"),
      (1, Tile(0, 0, 3), "123"),
      (1, Tile(0, 1, 3), "123"),
      (1, Tile(0, 2, 3), "123"),
      (1, Tile(0, 3, 3), "123"),
      (1, Tile(0, 4, 3), "123"),
      (1, Tile(0, 5, 3), "123"),
      (1, Tile(0, 6, 3), "123"),
      (1, Tile(0, 7, 3), "123"),
      (1, Tile(1, 0, 3), "123"),
      (1, Tile(1, 1, 3), "123"),
      (1, Tile(1, 2, 3), "123"),
      (1, Tile(1, 3, 3), "123"),
      (1, Tile(1, 4, 3), "123"),
      (1, Tile(1, 5, 3), "123"),
      (1, Tile(1, 6, 3), "123"),
      (1, Tile(1, 7, 3), "123"),
      (1, Tile(2, 0, 3), "123"),
      (1, Tile(2, 1, 3), "123"),
      (1, Tile(2, 2, 3), "123"),
      (1, Tile(2, 3, 3), "123"),
      (1, Tile(2, 4, 3), "123"),
      (1, Tile(2, 5, 3), "123"),
      (1, Tile(2, 6, 3), "123"),
      (1, Tile(2, 7, 3), "123"),
      (1, Tile(3, 0, 3), "123"),
      (1, Tile(3, 1, 3), "123"),
      (1, Tile(3, 2, 3), "123"),
      (1, Tile(3, 3, 3), "123"),
      (1, Tile(3, 4, 3), "123"),
      (1, Tile(3, 5, 3), "123"),
      (1, Tile(3, 6, 3), "123"),
      (1, Tile(3, 7, 3), "123"),
      (1, Tile(4, 0, 3), "123"),
      (1, Tile(4, 1, 3), "123"),
      (1, Tile(4, 2, 3), "123"),
      (1, Tile(4, 3, 3), "123"),
      (1, Tile(4, 4, 3), "123"),
      (1, Tile(4, 5, 3), "123"),
      (1, Tile(4, 6, 3), "123"),
      (1, Tile(4, 7, 3), "123"),
      (1, Tile(5, 0, 3), "123"),
      (1, Tile(5, 1, 3), "123"),
      (1, Tile(5, 2, 3), "123"),
      (1, Tile(5, 3, 3), "123"),
      (1, Tile(5, 4, 3), "123"),
      (1, Tile(5, 5, 3), "123"),
      (1, Tile(5, 6, 3), "123"),
      (1, Tile(5, 7, 3), "123"),
      (1, Tile(6, 0, 3), "123"),
      (1, Tile(6, 1, 3), "123"),
      (1, Tile(6, 2, 3), "123"),
      (1, Tile(6, 3, 3), "123"),
      (1, Tile(6, 4, 3), "123"),
      (1, Tile(6, 5, 3), "123"),
      (1, Tile(6, 6, 3), "123"),
      (1, Tile(6, 7, 3), "123"),
      (1, Tile(7, 0, 3), "123"),
      (1, Tile(7, 1, 3), "123"),
      (1, Tile(7, 2, 3), "123"),
      (1, Tile(7, 3, 3), "123"),
      (1, Tile(7, 4, 3), "123"),
      (1, Tile(7, 5, 3), "123"),
      (1, Tile(7, 6, 3), "123"),
      (1, Tile(7, 7, 3), "123"),
      (2, Tile(0, 0, 0), "456"),
      (2, Tile(0, 0, 1), "456"),
      (2, Tile(0, 1, 1), "456"),
      (2, Tile(1, 0, 1), "456"),
      (2, Tile(1, 1, 1), "456"),
      (2, Tile(0, 0, 2), "456"),
      (2, Tile(0, 1, 2), "456"),
      (2, Tile(0, 2, 2), "456"),
      (2, Tile(0, 3, 2), "456"),
      (2, Tile(1, 0, 2), "456"),
      (2, Tile(1, 1, 2), "456"),
      (2, Tile(1, 2, 2), "456"),
      (2, Tile(1, 3, 2), "456"),
      (2, Tile(2, 0, 2), "456"),
      (2, Tile(2, 1, 2), "456"),
      (2, Tile(2, 2, 2), "456"),
      (2, Tile(2, 3, 2), "456"),
      (2, Tile(3, 0, 2), "456"),
      (2, Tile(3, 1, 2), "456"),
      (2, Tile(3, 2, 2), "456"),
      (2, Tile(3, 3, 2), "456"),
      (2, Tile(0, 0, 3), "456"),
      (2, Tile(0, 1, 3), "456"),
      (2, Tile(0, 2, 3), "456"),
      (2, Tile(0, 3, 3), "456"),
      (2, Tile(0, 4, 3), "456"),
      (2, Tile(0, 5, 3), "456"),
      (2, Tile(0, 6, 3), "456"),
      (2, Tile(0, 7, 3), "456"),
      (2, Tile(1, 0, 3), "456"),
      (2, Tile(1, 1, 3), "456"),
      (2, Tile(1, 2, 3), "456"),
      (2, Tile(1, 3, 3), "456"),
      (2, Tile(1, 4, 3), "456"),
      (2, Tile(1, 5, 3), "456"),
      (2, Tile(1, 6, 3), "456"),
      (2, Tile(1, 7, 3), "456"),
      (2, Tile(2, 0, 3), "456"),
      (2, Tile(2, 1, 3), "456"),
      (2, Tile(2, 2, 3), "456"),
      (2, Tile(2, 3, 3), "456"),
      (2, Tile(2, 4, 3), "456"),
      (2, Tile(2, 5, 3), "456"),
      (2, Tile(2, 6, 3), "456"),
      (2, Tile(2, 7, 3), "456"),
      (2, Tile(3, 0, 3), "456"),
      (2, Tile(3, 1, 3), "456"),
      (2, Tile(3, 2, 3), "456"),
      (2, Tile(3, 3, 3), "456"),
      (2, Tile(3, 4, 3), "456"),
      (2, Tile(3, 5, 3), "456"),
      (2, Tile(3, 6, 3), "456"),
      (2, Tile(3, 7, 3), "456"),
      (2, Tile(4, 0, 3), "456"),
      (2, Tile(4, 1, 3), "456"),
      (2, Tile(4, 2, 3), "456"),
      (2, Tile(4, 3, 3), "456"),
      (2, Tile(4, 4, 3), "456"),
      (2, Tile(4, 5, 3), "456"),
      (2, Tile(4, 6, 3), "456"),
      (2, Tile(4, 7, 3), "456"),
      (2, Tile(5, 0, 3), "456"),
      (2, Tile(5, 1, 3), "456"),
      (2, Tile(5, 2, 3), "456"),
      (2, Tile(5, 3, 3), "456"),
      (2, Tile(5, 4, 3), "456"),
      (2, Tile(5, 5, 3), "456"),
      (2, Tile(5, 6, 3), "456"),
      (2, Tile(5, 7, 3), "456"),
      (2, Tile(6, 0, 3), "456"),
      (2, Tile(6, 1, 3), "456"),
      (2, Tile(6, 2, 3), "456"),
      (2, Tile(6, 3, 3), "456"),
      (2, Tile(6, 4, 3), "456"),
      (2, Tile(6, 5, 3), "456"),
      (2, Tile(6, 6, 3), "456"),
      (2, Tile(6, 7, 3), "456"),
      (2, Tile(7, 0, 3), "456"),
      (2, Tile(7, 1, 3), "456"),
      (2, Tile(7, 2, 3), "456"),
      (2, Tile(7, 3, 3), "456"),
      (2, Tile(7, 4, 3), "456"),
      (2, Tile(7, 5, 3), "456"),
      (2, Tile(7, 6, 3), "456"),
      (2, Tile(7, 7, 3), "456")
    )

    assertEquals(expected.size, traversed.size)
    assertTrue(expected.forall(triple => traversed.contains(triple)))
  }
}
