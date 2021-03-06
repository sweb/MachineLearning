package com.github.sweb.machinelearning

import org.scalatest._
/**
 * Created by Florian on 23.09.2015.
 */
class MLMatrixSpec extends FlatSpec with Matchers {
  val data = Array(Array(0.0, 1.0), Array(5.0, 2.0))
  val matrix = MLMatrix(data)
  "A matrix" should "take a 2d array to initialize" in {
    matrix(0,0) should be (0.0)
    matrix(0,1) should be (1.0)
    matrix(1,0) should be (5.0)
    matrix(1,1) should be (2.0)
  }

  it should "transpose" in {
    val transposedMatrix = matrix.transpose()
    transposedMatrix(0,0) should be (0.0)
    transposedMatrix(0,1) should be (5.0)
    transposedMatrix(1,0) should be (1.0)
    transposedMatrix(1,1) should be (2.0)
  }

  it should "invert" in {
    val invertedMatrix = matrix.invert()
    invertedMatrix(0,0) should be (-0.4)
    invertedMatrix(0,1) should be (0.2)
    invertedMatrix(1,0) should be (1.0)
    invertedMatrix(1,1) should be (0.0)
  }

  it should "multiply" in {
    val data = Array(Array(2.0, 5.0), Array(1.0, 3.0))
    val baseMatrix = MLMatrix(data)
    val matrix = baseMatrix * baseMatrix.invert()

    matrix(0,0) should be (1.0 +- 0.000001)
    matrix(0,1) should be (0.0 +- 0.000001)
    matrix(1,0) should be (0.0 +- 0.000001)
    matrix(1,1) should be (1.0 +- 0.000001)
  }

  it should "apply a function to a column" in {
    matrix.applyToColumn(0, x => x * 2).toList should be (List(0.0, 10.0))
    matrix.applyToColumn(1, x => x + 1.0).toList should be (List(2.0, 3.0))
  }

  it should "convert back to an Array" in {
    (matrix.to2DArray.toList.map(_.toList)) should be (data.toList.map(_.toList))
  }
}
