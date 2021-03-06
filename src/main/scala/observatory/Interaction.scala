package observatory

import com.sksamuel.scrimage.Image
import observatory.Visualization.{interpolateColor, predictTemperature}

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = pow(2, tile.zoom)
    Location(toDegrees(atan(sinh(Pi * (1 - 2 * tile.y / n)))), tile.x / n * 360.0 - 180.0)
  }

  def pixelLocation(imagePixelX: Int, imagePixelY: Int, tile: Tile): Location =
    tileLocation(Tile(tile.x * 256 + imagePixelX, tile.y * 256 + imagePixelY, tile.zoom + 8))

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image =
    Image(
      TileSize,
      TileSize,
      Array.tabulate(TileSize * TileSize) { i =>
        interpolateColor(
          colors,
          predictTemperature(temperatures, pixelLocation(i % TileSize, i / TileSize, tile))
        ).pixel()
      }
    )

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit =
    (
      for {
        (year, data) <- yearlyData
        zoom <- 0 to 3
        x <- 0 until pow(2, zoom).toInt
        y <- 0 until pow(2, zoom).toInt
      } yield (year, data, zoom, x, y)
      )
      .par
      .foreach { case (year, data, zoom, x, y) =>
        generateImage(year, Tile(x, y, zoom), data)
      }

}
