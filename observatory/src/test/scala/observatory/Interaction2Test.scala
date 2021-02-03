package observatory

import observatory.Interaction2._
import org.junit.Assert._
import org.junit.Test

trait Interaction2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive user interface", 6) _

  private val yearRange = 2000 to 2030

  @Test def `yearSelection must never be less than lower bound`: Unit = {

    val sLayer = Signal( Layer(LayerName.Temperatures,null,yearRange) )
    val sliderValue = Signal(1999)
    assert(yearSelection(sLayer,sliderValue)().equals(2000),"yearSelection out of bounds!")
  }

  @Test def `yearSelection must never be more than upper bound`: Unit = {

    val sLayer = Signal( Layer(LayerName.Temperatures,null,yearRange) )
    val sliderValue = Signal(2031)
    assert(yearSelection(sLayer,sliderValue)().equals(2030),"yearSelection out of bounds!")
  }

}
