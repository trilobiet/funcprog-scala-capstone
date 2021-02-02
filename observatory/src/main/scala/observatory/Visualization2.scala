package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.immutable
import scala.math.pow
import Visualization._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
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

    // prepare xy for readability and some speed up
    val x = point.x
    val y = point.y
    val xy = x * y

    // Sum surfaces of opposite corners:
    // d00(1-x)(1-y) + d01(1-x)(y) + d10(x)(1-y) + d11(x)(y)
    d00 * (1-y-x+xy) + d01 * (y-xy) + d10 * (x-xy) + d11 * xy
  }

  /**
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

    /*
    // tile size 360/2ⁿ (n = zoom level)
    val tileSize =  360/pow(2,tile.zoom)
    // pixel size in degrees is 1/256 tile size
    val pixelSize =  tileSize/256

    println(s"tile size: $tileSize")
    println(s"pixel size: $pixelSize")
    */

    val (x0,y0) = tile.offSet
    // println(s"offset: ($x0,$y0)")

    // maps a temperature to an image pixel (256 x 256)
    val gridTemperatures: Seq[((Int, Int), Temperature)] = (
      for {
        x <- (x0 to x0 + 255).par;
        y <- (y0 to y0 + 255).par;
        location: Location = Tile(x,y,tile.zoom+8).toLocation
        // GridLocation -180 -> 179, -90 -> 89
        iLat = location.lat.toInt
        iLon = location.lon.toInt
        d00 = grid(GridLocation(iLat,iLon))
        d01 = grid(GridLocation(iLat+1,iLon).wrap)
        d10 = grid(GridLocation(iLat,iLon+1).wrap)
        d11 = grid(GridLocation(iLat+1,iLon+1).wrap)
        p = CellPoint(location.lon % 1,location.lat % 1) // use decimal part for interpolation
        // q = println(iLat,iLon,p)
      } yield {
        (x-x0,y-y0) -> bilinearInterpolation(p,d00,d01,d10,d11)
      }
    ).seq

    coloredTemperatureImage(gridTemperatures,colors,256,127)
  }

}
