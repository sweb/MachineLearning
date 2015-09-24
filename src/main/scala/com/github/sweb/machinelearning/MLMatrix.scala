package com.github.sweb.machinelearning

import org.ejml.simple.SimpleMatrix

/**
 * Created by Florian on 23.09.2015.
 * Notes: Array of rows
 */
case class MLMatrix(data: SimpleMatrix) extends MLEsotericMatrix {

  def this(rawData: Array[Array[Double]]) = {
    this(new SimpleMatrix(rawData))
  }

  def apply(row: Int, col:Int): Double = {
    data.get(row, col)
  }

  def transpose(): MLMatrix = {
    MLMatrix(data.transpose())
  }

  def invert(): MLMatrix = {
    MLMatrix(data.invert())
  }

  def mult(otherMatrix: MLEsotericMatrix): MLMatrix = {
    MLMatrix(data.mult(otherMatrix.data))
  }
}

object MLMatrix {
  def apply(rawData: Array[Array[Double]]) = new MLMatrix(rawData)
}

case class MLVector(data: SimpleMatrix) {
  
  def this(rawData: Array[Double]) = {
    this(new SimpleMatrix(Array(rawData)).transpose())
  }
  
  def apply(index: Int): Double = {
    data.get(index)
  }

  def transpose(): MLVector = {
    MLVector(data.transpose())
  }

  def mult(otherMatrix: MLEsotericMatrix): MLVector = {
    MLVector(data.mult(otherMatrix.data))
  }

}

object MLVector {
  def apply(rawData: Array[Double]) = new MLVector(rawData)
}


trait MLEsotericMatrix {

  def data(): SimpleMatrix

  def transpose(): MLEsotericMatrix

  def mult(otherMatrix: MLEsotericMatrix): MLEsotericMatrix
}