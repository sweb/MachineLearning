package com.github.sweb.machinelearning

import org.ejml.simple.SimpleMatrix

/**
 * Created by Florian on 23.09.2015.
 * Notes: Array of rows
 */
trait MLMatrixLike {
  def data: SimpleMatrix

  protected def internalMult(implicit that: MLMatrixLike): SimpleMatrix = {
    data.mult(that.data)
  }

  def *(implicit that: MLMatrixLike): MLVector = MLVector(internalMult)
}

case class MLMatrix(data: SimpleMatrix) extends MLMatrixLike {

  def this(rawData: Array[Array[Double]]) = this(new SimpleMatrix(rawData))

  def apply(row: Int, col:Int): Double = data.get(row, col)

  def transpose(): MLMatrix = MLMatrix(data.transpose())

  def invert(): MLMatrix = MLMatrix(data.invert())

  def *(implicit that: MLMatrix): MLMatrix = MLMatrix(internalMult)

  def *(that: Double): MLMatrix = MLMatrix(data.scale(that))

  def -(that: MLMatrix): MLMatrix = MLMatrix(data.minus(that.data))

  def numberOfRows = data.getMatrix.getNumRows
  def numberOfCols = data.getMatrix.getNumCols

  def applyToColumn(colId: Int, f: Double => Double): Array[Double] = {
    require(colId < numberOfCols)
    val column: Array[Double] = data.extractVector(false, colId).getMatrix.getData
    column.map(f)
  }

  def to2DArray: Array[Array[Double]] = {
    val nrows = numberOfRows
    val ncols = numberOfCols

    val rowIds = 0 until nrows
    val colIds = 0 until ncols
    rowIds.map(rowId => colIds.map(colId => data.get(rowId * ncols + colId)).toArray).toArray
  }
}

object MLMatrix {
  def apply(rawData: Array[Array[Double]]) = new MLMatrix(rawData)
}

case class MLVector(data: SimpleMatrix) extends MLMatrixLike {
  
  def this(rawData: Array[Double]) = {
    this(new SimpleMatrix(Array(rawData)).transpose())
  }
  
  def apply(index: Int): Double = data.get(index)

  def transpose(): MLVector = MLVector(data.transpose())

  def -(that: MLVector): MLVector = MLVector(data.minus(that.data))

  def numberOfItems = data.getMatrix.getNumElements

  def getData(): Array[Double] = data.getMatrix.getData

}

object MLVector {
  def apply(rawData: Array[Double]) = new MLVector(rawData)
}