package observatory

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import java.util.stream
import scala.collection.immutable

trait ManipulationTest extends MilestoneSuite {

  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object
  private val locationsTemperatures = List(
    (new Location(45, -90), -20.0)
    , (new Location(45, 90), -10.0)
    , (new Location(0, 0), 0.0)
    , (new Location(-45, -90), 0.0)
    , (new Location(-45, 90), 20.0)
  )

  @Test def `Grid temperature must equal temperature of continuous map at the same location`: Unit = {

    val f: GridLocation => Temperature = Manipulation.makeGrid(locationsTemperatures)
    // pick any random location (int)
    val actual = f(GridLocation(35,68))
    val expected = Visualization.predictTemperature(locationsTemperatures,Location(35,68))

    assertEquals("blabla",actual,expected,0.00001)
  }


  @Test def `averages of multiple times the same year must for every grid point be the same value as for that grid point in a single year`: Unit = {

    val lt1 = Extraction.locateTemperaturesFromLists(2020, Q(csvStations), Q(csv2021) )
    val lya1 = Extraction.locationYearlyAverageRecords(lt1)

    val temperaturess: Seq[Iterable[(Location, Temperature)]] = List(lya1,lya1,lya1)

    val avg = Manipulation.average(temperaturess)
    val grid = Manipulation.makeGrid(lya1)

    // test must be true for every coordinate in the grid
    val trueForAll = ! {
      for ( x <- -89 to 90; y <- -180 to 179 )
      yield {
        val loc = GridLocation(x, y)
        math.abs(avg(loc)-grid(loc)) < 0.00001
      }
    }.contains(false)

    assertTrue("averages do not everywhere have the expected value",trueForAll)
  }


  @Test def `Test expected temperature deviation for a single GridPoint`: Unit = {

    val lt2030 = locationsTemperatures.filterNot(ld => ld._1 == Location(0,0)) ::: List((Location(0, 0), 10.0))
    // for the test we take just 1 year to calculate the normals from
    val ltNormal = locationsTemperatures
    val temperaturess: Seq[Iterable[(Location, Temperature)]] = List(ltNormal)
    val normals = Manipulation.average(temperaturess)
    val dev: (GridLocation => Temperature) = Manipulation.deviation(lt2030,normals)

    val expected = 10
    val actual = dev(GridLocation(0,0))

    assertEquals("Actual deviation does not match expected value",expected,actual,0.000001)
  }

}
