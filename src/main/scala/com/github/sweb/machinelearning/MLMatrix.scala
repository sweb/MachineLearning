package com.github.sweb.machinelearning

import org.ejml.simple.SimpleMatrix

/**
 * Created by Florian on 23.09.2015.
 * Notes: Array of rows
 */
case class MLMatrix(data: SimpleMatrix) {

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

  def *(otherMatrix: MLMatrix): MLMatrix = {
    MLMatrix(data.mult(otherMatrix.data))
  }

  def *(otherMatrix: MLVector): MLVector = {
    MLVector(data.mult(otherMatrix.data))
  }

  def -(otherMatrix: MLMatrix): MLMatrix = {
    MLMatrix(data.minus(otherMatrix.data))
  }

  def numberOfRows = data.getMatrix.getNumRows
  def numberOfCols = data.getMatrix.getNumCols
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

  def *(otherMatrix: MLVector): MLVector = {
    MLVector(data.mult(otherMatrix.data))
  }

  def *(otherMatrix: MLMatrix): MLVector = {
    MLVector(data.mult(otherMatrix.data))
  }

  def -(otherMatrix: MLVector): MLVector = {
    MLVector(data.minus(otherMatrix.data))
  }

  def numberOfItems = data.getMatrix.getNumElements

  def getData(): Array[Double] = data.getMatrix.getData

}

object MLVector {
  def apply(rawData: Array[Double]) = new MLVector(rawData)
}