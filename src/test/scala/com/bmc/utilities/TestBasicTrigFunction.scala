package com.bmc.utilities

import org.scalatest.FunSuite

class TestBasicTrigFunction extends FunSuite {
  trait CommonTestObjects{
    val cosx = BasicTrigFunction(Seq(0.0, 1.0), Seq(0.0, 0.0))
    val minuscosx = BasicTrigFunction(Seq(0.0, -1.0), Seq(0.0, 0.0))
    val sinx = BasicTrigFunction(Seq(0.0,0.0), Seq(0.0, 1.0))
    val minussinx = BasicTrigFunction(Seq(0.0, 0.0), Seq(0.0, -1.0))
    val cosxplussinx = BasicTrigFunction(Seq(0.0, 1.0), Seq(0.0, 1.0))
    val sin2x = BasicTrigFunction(Seq(0.0, 0.0, 0.0), Seq(0.0, 0.0, 1.0))
  }

  test("test mkString"){
    new CommonTestObjects {
      assert(cosx.mkString() === "1.0cos1x")
      assert(sinx.mkString() === "1.0sin1x")
      assert(cosxplussinx.mkString() === "1.0cos1x+1.0sin1x")
    }
  }

  test("test plus"){
    new CommonTestObjects {
      assert(cosx.plus(sinx) === cosxplussinx)
    }
  }

  test("test times"){
    new CommonTestObjects {
      assert(cosxplussinx.minus(cosx) === sinx)
      assert(cosxplussinx.minus(sinx) === cosx)
    }
  }

  test("test minus"){
    new CommonTestObjects {
      assert(cosx.times(-1.0) === minuscosx)
      assert(sinx.times(-1.0) === minussinx)
    }
  }

  test("test takeHilbertTransform"){
    new CommonTestObjects {
      assert(cosx.takeHilbertTransform() === sinx)
      assert(sinx.takeHilbertTransform() === minuscosx)
    }
  }

  test("test takeDerivative"){
    new CommonTestObjects {
      assert(cosx.takeDerivative() === minussinx)
      assert(sinx.takeDerivative() === cosx)
    }
  }

  test("test take2ndDerivative"){
    new CommonTestObjects {
      assert(cosx.takeDerivative().takeDerivative() === minuscosx)
      assert(sinx.takeDerivative().takeDerivative() === minussinx)
      assert(sin2x.takeDerivative().takeDerivative() === BasicTrigFunction(Seq(0.0, 0.0, 0.0), Seq(0.0, 0.0, -4.0)))
      assert(sin2x.takeDerivative().takeDerivative() === sin2x.times(-1.0).times(2).times(2))
    }
  }

  test("test halfDerivative 'Lambda' in the case of Hdot 1/2"){
    new CommonTestObjects {
      assert(cosx.takeDerivative().takeHilbertTransform() === cosx)
      assert(sinx.takeDerivative().takeHilbertTransform() === sinx)
      assert(sin2x.takeDerivative().takeHilbertTransform() === BasicTrigFunction(Seq(0.0, 0.0, 0.0), Seq(0.0, 0.0, 2.0)))
    }
  }

  test("test computeProduct"){
    new CommonTestObjects {
      assert(cosx.computeProduct(sinx) === sinx.computeProduct(cosx))
      assert(minussinx.computeProduct(sinx) === sinx.computeProduct(minussinx))
      assert(cosx.computeProduct(cosx).plus(sinx.computeProduct(sinx)) === BasicTrigFunction(Seq(1.0, 0.0, 0.0, 0.0), Seq(0.0, 0.0, 0.0, 0.0)))
    }
  }

  test("test computeIntegral"){
    new CommonTestObjects {
      assert(cosx.computeIntegral(cosx) === 0.5)
      assert(cosx.computeIntegral(minuscosx) === -0.5)
      assert(cosx.computeIntegral(cosxplussinx) === 0.5)
      assert(sinx.computeIntegral(cosxplussinx) === 0.5)
      assert(cosx.computeIntegral(sinx) === 0.0)
      assert(sinx.computeIntegral(cosx) === 0.0)
      assert(cosxplussinx.computeIntegral(cosxplussinx) === 1.0)
    }
  }
}
