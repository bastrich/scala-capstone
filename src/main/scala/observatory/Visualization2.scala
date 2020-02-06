package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.pixelLocation
import observatory.Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) + d10 * point.x * (1 - point.y) + d01 * (1 - point.x) * point.y + d11 * point.x * point.y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image =
    Image(
      TileSize,
      TileSize,
      Array.tabulate(TileSize * TileSize) { i =>
        val pixelLoc = pixelLocation(i % TileSize, i / TileSize, tile)
        val pixelGridLocation = GridLocation(pixelLoc.lat.ceil.toInt, pixelLoc.lon.floor.toInt)
        val pixelTemperature = bilinearInterpolation(
          CellPoint((pixelLoc.lon - pixelGridLocation.lon) / 1D, (pixelGridLocation.lat - pixelLoc.lat) / 1D),
          grid(pixelGridLocation),
          grid(GridLocation(pixelGridLocation.lat - 1, pixelGridLocation.lon)),
          grid(GridLocation(pixelGridLocation.lat, pixelGridLocation.lon + 1)),
          grid(GridLocation(pixelGridLocation.lat - 1, pixelGridLocation.lon + 1))
        )
        val color = interpolateColor(
          colors,
          pixelTemperature
        )

        Pixel(color.red, color.green, color.blue, 127)
      }
    )
}
