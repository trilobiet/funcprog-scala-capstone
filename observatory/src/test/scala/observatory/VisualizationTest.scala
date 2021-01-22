package observatory

import org.junit.Assert._
import org.junit.Test


trait VisualizationTest extends MilestoneSuite {

  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  val colorScale: Map[Temperature,Color] = Map(
    60d	-> Color(255,255,255),
    32d	-> Color(255,0,0),
    12d	-> Color(255,255,0),
    0d	-> Color(0,255,255),
    -15d	-> Color(0,0,255),
    -27d	-> Color(255,0,255),
    -50d	-> Color(33,0,107),
    -60d	-> Color(0,0,0)
  )


  // Implement tests for the methods of the `Visualization` object
  @Test def `'greatCircleDistance' of antipodes must return ~20015 km`: Unit = {

    val loc1 = Location(52.05249,4.21875).asRadians
    val loc2 = Location(-52.05249,-175.78125).asRadians
    val tl = Visualization.greatCircleDistance(loc1,loc2)
    val expected = 6371 * Math.PI
    assertEquals(s"distance between antipodes should equal to about $expected", expected, tl, 1)
  }

  @Test def `'greatCircleDistance' of equal locations must return 0 meter`: Unit = {

    val loc1 = Location(52.05249,4.21875).asRadians
    val loc2 = Location(52.05249,4.21875).asRadians
    val tl = Visualization.greatCircleDistance(loc1,loc2)
    val expected = 0
    assertEquals(s"distance between equal locations should equal to $expected", expected, tl, 0)

  }

  @Test def `'greatCircleDistance' of Rotterdam-Kaapstad must return ~9660 km`: Unit = {

    val loc1 = Location(52.05249,4.21875).asRadians
    val loc2 = Location(-33.928992,18.417396).asRadians
    val tl = Visualization.greatCircleDistance(loc1,loc2)
    val expected = 9660
    assertEquals(s"distance between Rotterdam and Kaapstad should equal to about $expected", expected, tl, 1)
  }

  @Test def `test interpolated temperature colors`: Unit = {

    val calculatedColors = List(
      Visualization.interpolateColor(colorScale,6),
      Visualization.interpolateColor(colorScale,22),
      Visualization.interpolateColor(colorScale,46),
      Visualization.interpolateColor(colorScale,100),
      Visualization.interpolateColor(colorScale,-7),
      Visualization.interpolateColor(colorScale,-21),
      Visualization.interpolateColor(colorScale,-38),
      Visualization.interpolateColor(colorScale,-55),
      Visualization.interpolateColor(colorScale,-100)
    )

    val expectedColors = List(
      Color(128,255,128),
      Color(255,128,0),
      Color(255,128,128),
      Color(255,255,255),
      Color(0,136,255),
      Color(128,0,255),
      Color(149,0,184),
      Color(17,0,54),
      Color(0,0,0)
    )

    // println(calculatedColors)

    assert(expectedColors.toSeq.equals(calculatedColors.toSeq),"interpolateColor does not return correct colors")
  }

  @Test def `1: predicted Temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa `: Unit = {
    val z = Location(88.0,-176.0)
    val temperatures3 = List(
      (Location(88.0, -177.0), 10.0),
      (Location(88.0, -177.05), 20.0)
    )
    val t = Visualization.predictTemperature(temperatures3, z)
    val td1 = Math.abs(t-10)
    val td2 = Math.abs(t-20)
    // println("predicted " + t)
    assert(td1 < td2, s"Temperature $t should be closer to closest point than farther point")
  }

  @Test def `2: predicted Temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa `: Unit = {
    val z = Location(88.0,-178.0)
    val temperatures3 = List(
      (Location(88.0, -177.0), 10.0),
      (Location(88.0, -177.05), 20.0)
    )
    val t = Visualization.predictTemperature(temperatures3, z)
    val td1 = Math.abs(t-10)
    val td2 = Math.abs(t-20)
    // println("predicted " + t)
    assert(td1 > td2, s"Temperature $t should be closer to closest point than farther point")
  }

  @Test def `3: predicted Temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa `: Unit = {
    val z = Location(87.0,0.0)
    val temperatures3 = List(
      (Location(88.0, 0.0), 10.0),
      (Location(88.05, 0.0), 20.0)
    )
    val t = Visualization.predictTemperature(temperatures3, z)
    val td1 = Math.abs(t-10)
    val td2 = Math.abs(t-20)
    // println("predicted " + t)
    assert(td1 < td2, s"Temperature $t should be closer to closest point than farther point")
  }

  @Test def `4: predicted Temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa `: Unit = {
    val z = Location(89.0,0.0)
    val temperatures3 = List(
      (Location(88.0, 0.0), 10.0),
      (Location(88.05, 0.0), 20.0)
    )
    val t = Visualization.predictTemperature(temperatures3, z)
    val td1 = Math.abs(t-10)
    val td2 = Math.abs(t-20)
    // println("predicted " + t)
    assert(td1 > td2, s"Temperature $t should be closer to closest point than farther point")
  }

  @Test def `xy2loc`: Unit = {

    // lat = y, lon = x
    // {90,-180}  -> (0,0)
    // {0,0}      -> (180,90)
    // {-89,179}  -> (359,179)

    val locUpperLeft = Location(90,-180)
    val locCenter = Location(0,0)
    val locLowerRight = Location(-89,179)

    val expected = List(locUpperLeft,locCenter,locLowerRight)
    val actual = List(Visualization.xy2loc(0,0),Visualization.xy2loc(180,90),Visualization.xy2loc(359,179))

    assert(expected.equals(actual),"xy2loc does not return correct image coordinates")
  }

}
