package com.github.sweb.machinelearning

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by Florian on 19.12.2015.
  */
class PreprocessorSpec extends FlatSpec with Matchers {

  val data = Array(Array(1.47),
    Array(1.50),
    Array(1.52),
    Array(1.55),
    Array(1.57),
    Array(1.60),
    Array(1.63),
    Array(1.65),
    Array(1.68),
    Array(1.70),
    Array(1.73),
    Array(1.75),
    Array(1.78),
    Array(1.80),
    Array(1.83))

  it should "standardize predictors to have unit variance" in {
    val results = Preprocessor.standardize(MLMatrix(data))._1.data.getMatrix.getData
    results(0) should equal(-1.581 +- 0.001)
    results(1) should equal(-1.318 +- 0.001)
    results(2) should equal(-1.143 +- 0.001)
    results(3) should equal(-0.881 +- 0.001)
    results(4) should equal(-0.706 +- 0.001)
    results(5) should equal(-0.443 +- 0.001)
    results(6) should equal(-0.18 +- 0.001)
    results(7) should equal(-0.005 +- 0.001)
    results(8) should equal(0.256 +- 0.001)
    results(9) should equal(0.431 +- 0.001)
    results(10) should equal(0.694 +- 0.001)
    results(11) should equal(0.869 +- 0.001)
    results(12) should equal(1.132 +- 0.001)
    results(13) should equal(1.307 +- 0.001)
    results(14) should equal(1.569 +- 0.001)
  }

  it should "standardize zero variance and normal predictors to have unit variance" in {
    val data = Array(Array(1.0, 1.47),
      Array(1.0, 1.50),
      Array(1.0, 1.52),
      Array(1.0, 1.55),
      Array(1.0, 1.57),
      Array(1.0, 1.60),
      Array(1.0, 1.63),
      Array(1.0, 1.65),
      Array(1.0, 1.68),
      Array(1.0, 1.70),
      Array(1.0, 1.73),
      Array(1.0, 1.75),
      Array(1.0, 1.78),
      Array(1.0, 1.80),
      Array(1.0, 1.83))

    val results = Preprocessor.standardize(MLMatrix(data))._1.data.getMatrix.getData

    val expected = Array(
      Array(1.0, -1.581),
      Array(1.0, -1.318),
      Array(1.0, -1.143),
      Array(1.0, -0.881),
      Array(1.0, -0.706),
      Array(1.0, -0.443),
      Array(1.0, -0.18),
      Array(1.0, -0.005),
      Array(1.0, 0.256),
      Array(1.0, 0.431),
      Array(1.0, 0.694),
      Array(1.0, 0.869),
      Array(1.0, 1.132),
      Array(1.0, 1.307),
      Array(1.0, 1.569)
    )

    //results.toList should be (expected.flatten.toList)

    results.zip(expected.flatten.toList).map(x => {
      x match {
        case (y, z) => y should equal (z +- 0.001)
      }
    })
  }

  it should "standardize multiple normal predictors to have unit variance" in {
    val data = Array(Array(1.0, 3.0),
      Array(2.0, 5.0),
      Array(3.0, 20.0),
      Array(4.0, 3.5),
      Array(5.0, 15.0),
      Array(6.0, 8.0)
    )

    val expected = Array(Array(-1.33, -0.87),
      Array(-0.80, -0.58),
      Array(-0.26, 1.57),
      Array(0.26, -0.80),
      Array(0.80, 0.85),
      Array(1.33, -0.15))

    val results = Preprocessor.standardize(MLMatrix(data))._1.data.getMatrix.getData

    results.zip(expected.flatten.toList).map(x => {
      x match {
        case (y, z) => y should equal (z +- 0.01)
      }
    })
  }

}
