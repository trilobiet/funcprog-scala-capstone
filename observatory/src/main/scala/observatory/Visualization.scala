package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import Math._
import scala.collection.parallel.ParIterable
import scala.collection.parallel.immutable.ParSeq

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  var lineCounter = 0;

  /**
    * @author acdhirr
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    // Inverse distance weighting https://en.wikipedia.org/wiki/Inverse_distance_weighting
    val p = 2 // power parameter

    val result2 =
      temperatures.par.aggregate(
        (0d, 0d)  // acc start value
      )(
        (acc: (Double, Double), y: (Location, Temperature)) => {
          val idw = inverseDistanceWeight(y._1,location,p) // weight for location y._1
          (acc._1 + idw * y._2, acc._2 + idw) // add weighted temperature left, add weight right
        },
        (a: (Double, Double), b: (Double, Double)) => (a._1 + b._1, a._2 + b._2)  // combine accumulators
      )

    val q = result2._1 / result2._2

    // println(s"predictedTemperature for $location is $q")
    if (location.lon == PI) {
      lineCounter = lineCounter + 1
      println(s"line $lineCounter ready")
    }
    q

    /*
    val weights =
      temperatures.par.map { case(loc,_) => inverseDistanceWeight(loc,location,p) }
    val weightSum = weights.par.sum

    // zip temperatures with corresponding weights
    val weightedTemps: Iterable[(Temperature, Temperature)] =
      temperatures
        .par.map { case(loc,temp) => temp }
        .seq.zip(weights)
      // same as temperatures.par.map(_._2).seq.zip(weights)

    val predictedTemperature = weightedTemps
      .par.map{ case(temp,weight) => weight * temp }
      .seq.sum / weightSum

    println(s"predictedTemperature for $location is $predictedTemperature")

    predictedTemperature

    */

  }

  /**
    * @author acdhirr
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    // Note that the given points are not sorted in a particular order.
    val temperatures = points.toMap

    val lesserList = temperatures.keySet.filter(_ <= value)
    val greaterList = temperatures.keySet.filter(_ >= value)
    val lower = if (lesserList.isEmpty) temperatures.keySet.min else lesserList.max
    val upper = if (greaterList.isEmpty) temperatures.keySet.max else greaterList.min

    val lowerColor = temperatures(lower)
    val upperColor = temperatures(upper)

    val fraction = (value-lower)/(upper-lower)

    val r = lowerColor.red    + fraction * (upperColor.red    - lowerColor.red)
    val g = lowerColor.green  + fraction * (upperColor.green  - lowerColor.green)
    val b = lowerColor.blue   + fraction * (upperColor.blue   - lowerColor.blue)

    Color(r.round.toInt, g.round.toInt, b.round.toInt)
  }

  /**
    * @author acdhirr
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val gtemps: Iterable[(Location, Temperature)] = temperatures.map(t => (deg2rad(t._1),t._2))

    val allTemperatures: ParSeq[(Location, Temperature)] = for {
      lat <- (-90 to 89).par;
      lon <- (-180 to 179).par;
      location = Location(deg2rad(lat),deg2rad(lon))
    } yield (location, predictTemperature(gtemps, location))

    val xyColors: Iterable[((Int, Int), Color)] = allTemperatures.map{
      case(loc,temp) => ( loc2xy(rad2deg(loc)), interpolateColor(colors,temp) )
    }.seq

    val pixels: Array[Pixel] = new Array[Pixel](xyColors.size)

    println(pixels.length)

    xyColors.par.foreach {
      // our Color type ≠ com.sksamuel.scrimage.Color,
      // so we cannot use our Color directly in the constructor!
      case((x,y),color: Color) => pixels(y * 360 + x) = Pixel(color.red,color.green,color.blue,255)
    }

    //xyColors.foreach(println)

    val img = Image(360,180,pixels)

    img

  }

  /**
    * @author acdhirr
    * @param location
    * @return
    */
  def loc2xy(location: Location): (Int,Int) = {

    val width = 360
    val height = width / 2

    /*
      (0,0) = {90,-180}
      +-----------------------+
      |                       |
      |    (180,90) = {0,0}   |
      |           .           |
      |                       |
      |                       |
      +-----------------------+
    */

    // lat = y, lon = x
    // {90,-180}  -> (0,0)
    // {0,0}      -> (180,90)
    // {-90,180}  -> (360,180)
    // {-89,179}  -> (359,179)

    ((round(location.lon+180) % 360).toInt,(round(90-location.lat) % 180).toInt)

  }


  /**
    * @author acdhirr
    * @param loc1
    * @param loc2
    * @param power
    * @return Inverse Distance Weight for two locations
    */
  def inverseDistanceWeight(loc1: Location, loc2: Location, power:Int): Double = {

    if (loc1.lat - loc2.lat > 0.25 && loc1.lon - loc2.lon > 0.25 )
      0
    else {
      val dist = greatCircleDistanceRad(loc1, loc2)
      1 / Math.pow(if (dist < 1) 1 else dist, power)
    }
  }


  /**
    * Calculate the spherical distance in km between 2 points on the globe
    * https://en.wikipedia.org/wiki/Great-circle_distance
    *
    * @author acdhirr
    * @param loc1 in degrees
    * @param loc2 in degrees
    * @return atcf distance between two locations
    */
  def greatCircleDistance(loc1: Location, loc2: Location): Double = {

    val r =  6371  // earth radius (average) in meters  6371.0088
    val lon1 = deg2rad(loc1.lon) // These are quite costly
    val lat1 = deg2rad(loc1.lat)
    val lon2 = deg2rad(loc2.lon)
    val lat2 = deg2rad(loc2.lat)

    val deltaSigma =
      if (loc1.equals(loc2))          0
      else if (loc1.isAntipode(loc2)) PI
      else                            acos( sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(abs(lon1-lon2)) )

    r * deltaSigma
  }

  /**
    * Calculate the spherical distance in km between 2 points on the globe
    * https://en.wikipedia.org/wiki/Great-circle_distance
    *
    * @author acdhirr
    * @param loc1 in radians
    * @param loc2 in radians
    * @return atcf distance between two locations
    */
  def greatCircleDistanceRad(loc1: Location, loc2: Location): Double = {

    val r =  6371  // earth radius (average) in meters  6371.0088

    val deltaSigma =
      if (loc1.equals(loc2))          0
      else if (loc1.isAntipode(loc2)) PI
      else                            acos( sin(loc1.lat) * sin(loc2.lat) + cos(loc1.lat) * cos(loc2.lat) * cos(abs(loc1.lon-loc2.lon)) )

    r * deltaSigma
  }


  /**
    * Convert degrees to radians
    *
    * @author acdhirr
    * @param degrees
    * @return radians
    */
  def deg2rad(degrees: Double): Double =
      degrees * PI / 180

  def rad2deg(radians: Double): Double =
      radians * 180 / PI

  def deg2rad(location: Location): Location =
    Location(deg2rad(location.lat),deg2rad(location.lon))

  def rad2deg(location: Location): Location =
    Location(rad2deg(location.lat),rad2deg(location.lon))

}

