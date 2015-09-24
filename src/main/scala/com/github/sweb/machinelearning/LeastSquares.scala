package com.github.sweb.machinelearning

/**
 * Implementation of least squares, according to chapter 3.2 of ESL II.
 */
case class LeastSquares(featureMatrix: Array[Array[Double]], observations: Array[Double], addIntercept: Boolean) {
  val processedFeatures = preprocessFeatures(featureMatrix)
  val X = MLMatrix(processedFeatures)
  val y = MLVector(observations)

  lazy val fittedParameters = solve()

  def solve(): Array[Double] = {
    // X has to have full column rank - how to check?
    require(X.data.getMatrix.getNumRows == y.data.getMatrix.getNumRows,
      "Number of feature rows does not equal number of observations")

    val T = X.transpose()

    val beta = ( T.mult(X) ).invert().mult(T).mult(y)

    beta.data.getMatrix().getData()
  }

  def addInterceptColumn(featureMatrix: Array[Array[Double]]): Array[Array[Double]] = {
    featureMatrix.map(row => row.+:(1.0))
  }

  private def preprocessFeatures(featureMatrix: Array[Array[Double]]) = {
    if (addIntercept) addInterceptColumn(featureMatrix) else featureMatrix
  }

  def residualSumOfSquares(parameters: Array[Double]): Double = {
    val beta = MLVector(parameters)
    val rss = (y.minus( (X.mult(beta)) )).transpose().mult( (y.minus(X.mult(beta))) )
    rss.data.getMatrix.getData()(0)
  }

  def predict(featureMatrix: Array[Array[Double]]): Array[Double] = {
    val processedFeatures = preprocessFeatures(featureMatrix)
    val X = MLMatrix(processedFeatures)
    val beta = MLVector(fittedParameters)
    val prediction = X.mult(beta)
    prediction.data.getMatrix.getData
  }

}