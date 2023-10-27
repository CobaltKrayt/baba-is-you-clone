package baba

import infrastructure.ScoreCounter
import org.junit.runner.RunWith
import org.scalatest.{Args, Status, Suites}
import org.scalatestplus.junit.JUnitRunner
import baba.BaseSnakeTestSuite.{BaseTests, MaxGrade}
import baba.basic.{DeathTests, GrowthTests, MovementTests, FullGameTests, NoReverseTest, PlacementTests, WrapAroundTests}
import baba.reverse.ReverseTests

abstract class SnakeTestSuite(suites: SnakeTestSuiteBase*) extends Suites(BaseTests ++ suites: _*) {
  override def run(testName: Option[String], args: Args): Status = {
    val scoreCounter = new ScoreCounter()
    val newArgs =
      args.copy(configMap = args.configMap.updated("scoreCounter", scoreCounter))
    val res = runDirect(testName, newArgs)
    printf("You got %d/%d points!\n", scoreCounter.points, scoreCounter.maxPoints)
    printf("Your grade for the baba exercise will be : %.2f\n", scoreCounter.fraction() * MaxGrade)
    res
  }

  // run without making a new scorecounter
  def runDirect(testName: Option[String], args: Args): Status = {
    super.run(testName, args)
  }
}

object BaseSnakeTestSuite {
  val MaxGrade = 5.5
  val BaseTests: Seq[SnakeTestSuiteBase] = Seq[SnakeTestSuiteBase](
    new PlacementTests,
    new MovementTests,
    new GrowthTests,
    new WrapAroundTests,
    new DeathTests,
    new FullGameTests
  )
}


