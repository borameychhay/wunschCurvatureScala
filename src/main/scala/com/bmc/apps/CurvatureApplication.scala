package com.bmc.apps

import com.bmc.utilities.BasicTrigFunction
import breeze.linalg._

/*
 This is an application for computing the sectional curvature of
 the infinite dimensional manifold the diffeomorphism group of
 the circle modulo the circle using the H dot 1/2 metric

 The proof of the math can be found in the resources
 */

object CurvatureApplication {

  def main(args: Array[String]): Unit = {
    val length = 10
    val r = scala.util.Random

    computeUnnormalizedSectionalCurvature(
      BasicTrigFunction(Seq.fill(length)(r.nextDouble()), Seq.fill(length)(r.nextDouble())),
      BasicTrigFunction(Seq.fill(length)(r.nextDouble()), Seq.fill(length)(r.nextDouble()))
    )
  }

  def compute2Alpha(u: BasicTrigFunction, v: BasicTrigFunction): BasicTrigFunction = {
    u.takeDerivative().times(-1.0).computeProduct(v)
      .plus(u.computeProduct(v.takeDerivative()))
  }

  def compute2LBeta(u: BasicTrigFunction, v: BasicTrigFunction): BasicTrigFunction = {
    u.times(2.0).takeDerivative().computeProduct(v.takeDerivative().takeHilbertTransform())
      .plus(u.computeProduct(v.takeHilbertTransform().takeDerivative().takeDerivative()))
      .minus(v.times(2.0).takeDerivative().computeProduct(u.takeDerivative().takeHilbertTransform()))
      .minus(v.computeProduct(u.takeHilbertTransform().takeDerivative().takeDerivative()))
  }

  def compute2LDelta(u: BasicTrigFunction, v: BasicTrigFunction): BasicTrigFunction = {
    u.times(2.0).takeDerivative().computeProduct(v.takeDerivative().takeHilbertTransform())
      .plus(u.computeProduct(v.takeHilbertTransform().takeDerivative().takeDerivative()))
      .plus(v.times(2.0).takeDerivative().computeProduct(u.takeDerivative().takeHilbertTransform()))
      .plus(v.computeProduct(u.takeHilbertTransform().takeDerivative().takeDerivative()))
  }

  def compute2LBigB(uOrV: BasicTrigFunction): BasicTrigFunction = {
    uOrV.times(2.0).takeDerivative().computeProduct(uOrV.takeDerivative().takeHilbertTransform())
      .plus(uOrV.computeProduct(uOrV.takeHilbertTransform().takeDerivative().takeDerivative()))
  }

  def computeOneTerm(Ldelta: BasicTrigFunction): Double = Ldelta.takeLambdaInverse().computeIntegral(Ldelta)/4

  def computeTwoTerm(alpha: BasicTrigFunction, Lbeta: BasicTrigFunction): Double = -2.0*alpha.computeIntegral(Lbeta)/4

  def computeThreeTerm(alpha: BasicTrigFunction): Double = -3.0*alpha.computeIntegral(alpha.takeHilbertTransform().takeDerivative())/4

  def computeFourTerm(Bu: BasicTrigFunction, Bv: BasicTrigFunction): Double = -4.0*Bu.takeLambdaInverse().computeIntegral(Bv)/4

  def computeUnnormalizedSectionalCurvature(u: BasicTrigFunction, v: BasicTrigFunction): Double = {
    val alpha = compute2Alpha(u,v)
    val Lbeta = compute2LBeta(u,v)
    val Ldelta = compute2LDelta(u,v)
    val LBu = compute2LBigB(u)
    val LBv = compute2LBigB(v)
    computeOneTerm(Ldelta) + computeTwoTerm(alpha, Lbeta) + computeThreeTerm(alpha) + computeFourTerm(LBu, LBv)
  }
}


