package com.bmc.apps

import org.scalatest.FunSuite
import com.bmc.utilities.BasicTrigFunction
import com.bmc.apps.CurvatureApplication._

class TestCurvatureApplication extends FunSuite {
  trait CommonTestObjects{
    val sinx = BasicTrigFunction(Seq(0.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0))
    val sin2x = BasicTrigFunction(Seq(0.0, 0.0, 0.0), Seq(0.0, 0.0, 1.0))
  }

  test("test ex1 sinx and sin2x") {
    new CommonTestObjects {
      //assert(computeOneTerm(compute2LDelta(sinx, sin2x)) === ((13.0*13.0/48.0+9.0/16.0)/2.0))
      assert(computeOneTerm(compute2LDelta(sinx, sin2x)) === 2.0416666666666665)
      assert(computeTwoTerm(compute2Alpha(sinx, sin2x), compute2LBeta(sinx, sin2x)) === 3.toFloat/8)
      assert(computeThreeTerm(compute2Alpha(sinx, sin2x)) === -9.toFloat/8)
      assert(computeFourTerm(compute2LBigB(sinx), compute2LBigB(sin2x)) === 0.0)
      assert(computeUnnormalizedSectionalCurvature(sinx, sin2x) == 1.2916666666666665)
    }
  }

  test("test other examples") {
    new CommonTestObjects {
      val ex2first = BasicTrigFunction(Seq(0.0, 0.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0, 1.0))
      val ex2second = BasicTrigFunction(Seq(0.0, 0.0, 0.0, 0.0), Seq(0.0, 0.0, 1.0, 0.0))
      assert(computeUnnormalizedSectionalCurvature(ex2first, ex2second) === 3.4416666666666664)
      val ex3first = BasicTrigFunction(Seq(0.0, 0.0, 0.0, 1.0), Seq(0.0, 1.0, 0.0, 0.0))
      val ex3second = BasicTrigFunction(Seq(0.0, 0.0, 0.0, 0.0), Seq(0.0, 0.0, 1.0, 0.0))
      assert(computeUnnormalizedSectionalCurvature(ex3first, ex3second) === 10.941666666666666)
      //integration error in thesis for ex4
      val ex4first = BasicTrigFunction(Seq(0.0, 0.0, 0.0, 1.0), Seq(0.0, 1.0, 0.0, 0.0))
      val ex4second = BasicTrigFunction(Seq(0.0, 1.0, 0.0, 0.0), Seq(0.0, 0.0, 1.0, 0.0))
      assert(computeUnnormalizedSectionalCurvature(ex4first, ex4second) === 11.847916666666666)
    }
  }
}
