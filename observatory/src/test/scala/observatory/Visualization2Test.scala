package observatory

import org.junit.Assert.assertEquals
import org.junit.Test

trait Visualization2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  // Implement tests for methods of the `Visualization2` object

  @Test def `'bilinearInterpolation' should return temperature t for any point when all corners have temperature t`: Unit = {

    val p = CellPoint(.25,.75)
    val tActual = Visualization2.bilinearInterpolation(p,30d,30d,30d,30d)
    val tExpected = 30d

    assertEquals("temperature in any point p must be equal to any of its corners when they all have the same temperature", tExpected, tActual, 0.00001)
  }

  @Test def `'bilinearInterpolation' should return temperature (t2-t1)/2 for any point x=0.5 when left corners have temperature t1 and right corners temperature t2`: Unit = {

    val p = CellPoint(.5,.3435879)
    val tActual = Visualization2.bilinearInterpolation(p,10d,10d,20d,20d)
    val tExpected = 15d

    assertEquals("temperature in any point p must be equal to average of left and right side when those sides have uniform temperature", tExpected, tActual, 0.00001)
  }

  @Test def `'bilinearInterpolation' should return higher temperature for point closer to hottest corner`: Unit = {

    val p1 = CellPoint(.2,.2)
    val p2 = CellPoint(.5,.5)
    val p3 = CellPoint(.8,.8)
    val d00 = 30d
    val d01 = 20d
    val d10 = 20d
    val d11 = 10d
    val t1 = Visualization2.bilinearInterpolation(p1,d00,d01,d10,d11)
    val t2 = Visualization2.bilinearInterpolation(p2,d00,d01,d10,d11)
    val t3 = Visualization2.bilinearInterpolation(p3,d00,d01,d10,d11)

    assert(t1 > t2 && t2 > t3, "points closer to hottest corner must have higher temperature")
  }


  @Test def `'bilinearInterpolation' should return average temperature of all 4 corners for a point in the center of the square`: Unit = {

    val p = CellPoint(.5,.5)
    val d00 = 40d
    val d01 = 28d
    val d10 = 20d
    val d11 = 16d
    val tActual = Visualization2.bilinearInterpolation(p,d00,d01,d10,d11)
    val tExpected = (d00+d01+d10+d11)/4 // 26

    assertEquals("temperature in center must be average temperature of all 4 corners", tExpected, tActual, 0.00001)
  }

}
