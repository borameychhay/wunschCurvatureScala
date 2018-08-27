package com.bmc.utilities

case class BasicTrigFunction(evenTerms: Seq[Double], oddTerms: Seq[Double]) {

  def mkString(): String = {
    val oddPart: Seq[String] = for(n <- this.oddTerms.indices if oddTerms(n) != 0.0) yield oddTerms(n).toString + "sin" + n.toString + "x"
    val evenPart: Seq[String] = for(n <- this.evenTerms.indices if evenTerms(n) != 0.0) yield evenTerms(n).toString + "cos" + n.toString + "x"
    this match {
      case ofcn: BasicTrigFunction if this.evenTerms.toSet == Set(0.0) => oddPart.mkString("+")
      case efcn: BasicTrigFunction if this.oddTerms.toSet == Set(0.0) => evenPart.mkString("+")
      case _ => evenPart.mkString("+")+ "+" + oddPart.mkString("+")
    }
  }

  def plus(g: BasicTrigFunction): BasicTrigFunction = BasicTrigFunction(addSeqTermByTerm(this.evenTerms, g.evenTerms), addSeqTermByTerm(this.oddTerms, g.oddTerms))

  def times(c: Double): BasicTrigFunction = BasicTrigFunction(multByConstant(this.evenTerms, c), multByConstant(this.oddTerms, c))

  def minus(g: BasicTrigFunction): BasicTrigFunction = this.plus(g.times(-1.0))

  def takeHilbertTransform(): BasicTrigFunction = {
    val newOddTerms: Seq[Double] = for(n <- 1 until this.evenTerms.size) yield this.evenTerms(n)
    val newEvenTerms: Seq[Double] = for(n <- 1 until this.oddTerms.size) yield -this.oddTerms(n)
    BasicTrigFunction(Seq(0.0)++newEvenTerms, Seq(0.0)++newOddTerms)
  }

  def takeDerivative(): BasicTrigFunction = {
    val newOddTerms: Seq[Double] = for(n <- this.evenTerms.indices) yield -n*this.evenTerms(n)
    val newEvenTerms: Seq[Double] = for(n <- this.oddTerms.indices) yield n*this.oddTerms(n)
    BasicTrigFunction(newEvenTerms, newOddTerms)
  }

  def takeLambdaInverse(): BasicTrigFunction = {
    val newOddTerms: Seq[Double] = for(n <- this.evenTerms.indices if n!=0) yield this.oddTerms(n)/n
    val newEvenTerms: Seq[Double] = for(n <- this.oddTerms.indices if n!=0) yield this.evenTerms(n)/n
    BasicTrigFunction(Seq(0.0)++newEvenTerms, Seq(0.0)++newOddTerms)
  }

  def computeProduct(g: BasicTrigFunction): BasicTrigFunction = {
    val cosCos = computeProductMatrix(this.evenTerms, g.evenTerms)
    val sinSin = computeProductMatrix(this.oddTerms, g.oddTerms)
    val cosSin = computeProductMatrix(this.evenTerms, g.oddTerms)
    val sinCos = computeProductMatrix(this.oddTerms, g.evenTerms)
    val newOddTerms: Seq[Double] = addSeqTermByTerm(flattenCosSinMatrix(cosSin), flattenSinCosMatrix(sinCos))
    val newEvenTerms: Seq[Double] = addSeqTermByTerm(flattenCosCosMatrix(cosCos), flattenSinSinMatrix(sinSin))
    BasicTrigFunction(newEvenTerms, newOddTerms)

  }

  //computes the integral of the product of two functions of over 2pi normalized by pi
  def computeIntegral(g: BasicTrigFunction): Double = {
    val oddPart: Seq[Double] = for(n <- 1 until Math.min(this.oddTerms.size, g.oddTerms.size)) yield this.oddTerms(n)*g.oddTerms(n)/2
    val evenPart: Seq[Double] = for(n <- 1 until Math.min(this.evenTerms.size, g.evenTerms.size)) yield this.evenTerms(n)*g.evenTerms(n)/2
    this.oddTerms.head*g.oddTerms.head + oddPart.sum +this.oddTerms.head*g.oddTerms.head + evenPart.sum
  }

}
