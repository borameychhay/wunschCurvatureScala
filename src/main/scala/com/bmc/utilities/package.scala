package com.bmc

package object utilities {

  def computeProductMatrix(firstArray: Seq[Double], secondArray: Seq[Double]): Array[Array[Double]] = {
    val result = Array.ofDim[Double](firstArray.size, secondArray.size)
    for(i<-firstArray.indices) for(j<-secondArray.indices) result(i)(j) += firstArray(i)*secondArray(j)
    result
  }

  def flattenCosCosMatrix(cc: Array[Array[Double]]): Seq[Double] = {
    val result = Array.ofDim[Double](cc.length*2)
    for(i<- cc.indices) for(j<-cc.indices) {
      result(i+j) += 0.5*cc(i)(j)
      result(math.abs(i-j)) += 0.5*cc(i)(j)
    }
    result.toSeq
  }

  def flattenSinSinMatrix(ss: Array[Array[Double]]): Seq[Double] = {
    val result = Array.ofDim[Double](ss.length*2)
    for(i<- ss.indices) for(j<-ss.indices) {
      result(i+j) += -0.5*ss(i)(j)
      result(math.abs(i-j)) += 0.5*ss(i)(j)
    }
    result.toSeq
  }

  def flattenCosSinMatrix(cs: Array[Array[Double]]): Seq[Double] = {
    val result = Array.ofDim[Double](cs.length*2)
    for(i<- cs.indices) for(j<-cs.indices) {
      if(i>j){
        result(i+j) += 0.5*cs(i)(j)
        result(i-j) += -0.5*cs(i)(j)
      }
      if(j>i){
        result(i+j) += 0.5*cs(i)(j)
        result(j-i) += 0.5*cs(i)(j)
      }
      if(i==j){
        result(i+j) += 0.5*cs(i)(j)
      }
    }
    result.toSeq
  }

  def flattenSinCosMatrix(sc: Array[Array[Double]]): Seq[Double] = {
    val result = Array.ofDim[Double](sc.length*2)
    for(i<- sc.indices) for(j<-sc.indices) {
      if(i>j){
        result(i+j) += 0.5*sc(i)(j)
        result(i-j) += 0.5*sc(i)(j)
      }
      if(j>i){
        result(i+j) += 0.5*sc(i)(j)
        result(j-i) += -0.5*sc(i)(j)
      }
      if(i==j){
        result(i+j) += 0.5*sc(i)(j)
      }
    }
    result.toSeq
  }

  //assumes they are the same size
  def addSeqTermByTerm(a: Seq[Double], b: Seq[Double]): Seq[Double]= {
    for(i <- a.indices) yield a(i) + b(i)
  }

  def multByConstant(a: Seq[Double], c: Double): Seq[Double] = a.map(num => num*c)
}
