package observatory

import com.sksamuel.scrimage.Image
import observatory.Visualization._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @author acdhirr
    * @param point (x, y) coordinates of a point in the grid cell
    *        (CellPoint is a fraction 0 ≤ x ≤ 1 and 0 ≤ y ≤ 1)
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
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

    // prepare xy for readability and some speed gain
    val x = point.x
    val y = point.y
    val xy = x * y

    // Sum surfaces of opposite corners:
    // d00(1-x)(1-y) + d01(1-x)(y) + d10(x)(1-y) + d11(x)(y)
    d00 * (1-y-x+xy) + d01 * (y-xy) + d10 * (x-xy) + d11 * xy
  }

  /**
    * @author acdhirr
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

    val (x0,y0) = tile.offSet

    // maps a temperature to an image pixel (256 x 256)
    val gridTemperatures: Seq[((Int, Int), Temperature)] = (
      for {
        x <- (x0 to x0 + 255).par;
        y <- (y0 to y0 + 255).par;
        location: Location = Tile(x,y,tile.zoom+8).toLocation
        // GridLocation -180 -> 179, -90 -> 89
        ips = interpolationSquare(location)
        d00 = grid(ips.upperLeft)
        d01 = grid(ips.lowerLeft)
        d10 = grid(ips.upperRight)
        d11 = grid(ips.lowerRight)
      } yield {
        (x-x0,y-y0) -> bilinearInterpolation(ips.point,d00,d01,d10,d11)
      }
    ).seq

    coloredTemperatureImage(gridTemperatures,colors,256,127)
  }

  /**
    * Defines a square with 4 corners and a CellPoint with
    * coordinates (0 ≤ x ≤ 1 and 0 ≤ y ≤ 1),
    * relative to the upper left corner of the square.
    *
    * @author acdhirr
    * @param location
    */
  case class interpolationSquare(location: Location) {

    private val iLat = location.lat.floor.toInt
    private val iLon = location.lon.floor.toInt
    private val dLat = location.lat - iLat
    private val dLon = location.lon - iLon

    def upperLeft   = GridLocation(iLat,iLon)
    def lowerLeft   = GridLocation(iLat+1,iLon).wrap
    def upperRight  = GridLocation(iLat,iLon+1).wrap
    def lowerRight  = GridLocation(iLat+1,iLon+1).wrap

    // !Remember: CellPoint uses (x,y) so use (longitude, latitude)!
    def point       = CellPoint(dLon,dLat)

    override def toString: String =
      s"$upperLeft,$lowerLeft,$upperRight,$lowerRight, $point"
  }

}

