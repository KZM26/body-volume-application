package tools

import test.BVATestSuite
import tools.EllipseMaster

class EllipseMasterTest extends BVATestSuite {

  // Test values from http://paulbourke.net/geometry/ellipsecirc/

  it ("Numerical approximation tests") {
    val a = 1
    var b = 1
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    var realVal = IndexedSeq[Double](6.27672765822760, 6.28312071096907, 6.28318466121547, 6.28318530072077, 6.28318530710706, 6.28318530717921, 6.28318530717957, 6.28318530717915)

    var n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.numerical(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }

    b = 2
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    realVal = IndexedSeq[Double](9.67849075560978, 9.68834861548905, 9.68844722449405, 9.68844821058716, 9.68844822044810, 9.68844822054622, 9.68844822054806, 9.68844822054901)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.numerical(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }

    b = 10
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    realVal = IndexedSeq[Double](40.60042321878313, 40.63932399169911, 40.63973762290312, 40.63974175922775, 40.63974180059099, 40.63974180100578, 40.63974180101561, 40.63974180100927)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.numerical(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }

    b = 1000
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    realVal = IndexedSeq[Double](4000.00700622830936, 4000.01160343712263, 4000.01520168268371, 4000.01558399234045, 4000.01558806354569, 4000.01558810436791, 4000.01558810497863, 4000.01558809291328)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.numerical(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }
  }

// TODO: Check ground truth
  /*
  it ("Integral approximation tests") {
    val a = 1
    var b = 1
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    var realVal = IndexedSeq[Double](6.28318530717959, 6.28318530717958, 6.28318530717947, 6.28318530718043, 6.28318530717586, 6.28318530715542, 6.28318530604334, 6.28318532160340)

    var n = 10
    for (i <- realVal.indices){
      val res = EllipseMaster.integral(a, b, n)
      val perc = res/realVal(i)
      val diff = perc - 1
      val abs = math.abs(diff)
      assert(abs < 0.01)
      n *= 10
    }
/*
    b = 2
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    realVal = IndexedSeq[Double](9.68844822056413, 9.68844822054768, 9.68844822054768, 9.68844822054761, 9.68844822054766, 9.68844822054803, 9.68844822054681, 9.68844822054597)

    n = 10
    for (i <- realVal.indices){
      val res = EllipseMaster.integral(a, b, n)
      val perc = res/realVal(i)
      val diff = perc - 1
      val abs = math.abs(diff)
      assert(abs < 0.01)
      n *= 10
    }

    b = 10
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    realVal = IndexedSeq[Double](40.64219391439480, 40.63974180100898, 40.63974180100900, 40.63974180100901, 40.63974180100879, 40.63974180101027, 40.63974180100751, 40.63974180107515)

    n = 10
    for (i <- realVal.indices){
      val res = EllipseMaster.integral(a, b, n)
      val perc = res/realVal(i)
      val diff = perc - 1
      val abs = math.abs(diff)
      assert(abs < 0.01)
      n *= 10
    }

    b = 1000
    // Real values for 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000
    realVal = IndexedSeq[Double](4004.12231000731981, 4000.05272720403855, 4000.01561291779944, 4000.01558810469305, 4000.01558810466850, 4000.01558810488496, 4000.01558810481674, 4000.01558810381857)

    n = 10
    for (i <- realVal.indices){
      val res = EllipseMaster.integral(a, b, n)
      val perc = res/realVal(i)
      val diff = perc - 1
      val abs = math.abs(diff)
      assert(abs < 0.01)
      n *= 10
    }*/
  }
*/
  it ("Anonymous approximation test"){
    val a = 1

    var b = 1
    var realVal = 6.28318530717959
    assert(math.abs(EllipseMaster.anonymous(a, b)/realVal - 1) < 0.01)

    b = 2
    realVal = 9.68303887270669
    assert(math.abs(EllipseMaster.anonymous(a, b)/realVal - 1) < 0.01)

    b = 10
    realVal = 39.92419204913146
    assert(math.abs(EllipseMaster.anonymous(a, b)/realVal - 1) < 0.01)

    b = 1000
    realVal = 3848.93374981198258
    assert(math.abs(EllipseMaster.anonymous(a, b)/realVal - 1) < 0.01)
  }

  it ("Hudson approximation test"){
    val a = 1

    var b = 1
    var realVal = 6.28318530717959
    assert(math.abs(EllipseMaster.hudson(a, b)/realVal- 1) < 0.01)

    b = 2
    realVal = 9.68844734419566
    assert(math.abs(EllipseMaster.hudson(a, b)/realVal- 1) < 0.01)

    b = 10
    realVal = 40.63151007270351
    assert(math.abs(EllipseMaster.hudson(a, b)/realVal- 1) < 0.01)

    b = 1000
    realVal = 3992.68624906240484
    assert(math.abs(EllipseMaster.hudson(a, b)/realVal- 1) < 0.01)
  }

  it ("Ramanujan 1 approximation test"){
    val a = 1

    var b = 1
    var realVal = 6.28318530717959
    assert(math.abs(EllipseMaster.ramanujan1(a, b)/realVal - 1) < 0.01)

    b = 2
    realVal = 9.68842109767129
    assert(math.abs(EllipseMaster.ramanujan1(a, b)/realVal - 1) < 0.01)

    b = 10
    realVal = 40.60552518514097
    assert(math.abs(EllipseMaster.ramanujan1(a, b)/realVal - 1) < 0.01)

    b = 1000
    realVal = 3983.74047795885099
    assert(math.abs(EllipseMaster.ramanujan1(a, b)/realVal - 1) < 0.01)
  }

  it ("Ramanujan 2 approximation test"){
    val a = 1

    var b = 1
    var realVal = 6.28318530717959
    assert(math.abs(EllipseMaster.ramanujan2(a, b)/realVal - 1) < 0.01)

    b = 2
    realVal = 9.68844821613009
    assert(math.abs(EllipseMaster.ramanujan2(a, b)/realVal - 1) < 0.01)

    b = 10
    realVal = 40.63927210018871
    assert(math.abs(EllipseMaster.ramanujan2(a, b)/realVal - 1) < 0.01)

    b = 1000
    realVal = 3998.50189422870017
    assert(math.abs(EllipseMaster.ramanujan2(a, b)/realVal - 1) < 0.01)
  }

  it ("Holder high eccentricity approximation test"){
    val a = 1

    var b = 1
    var realVal = 6.28318530717959
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal - 1) < 0.01)

    b = 2
    realVal = 9.70448163064351
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal - 1) < 0.01)

    b = 10
    realVal = 40.75658566530318
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal - 1) < 0.01)

    b = 1000
    realVal = 4000.06474173655124
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal - 1) < 0.01)
  }

  // TODO: Check ground truth
  /*
  it ("Cantrell approximation test"){
    val a = 1

    var b = 1
    var realVal = 6.28318530717959
    assert(math.abs(EllipseMaster.cantrell(a, b)/realVal - 1) < 0.01)

    b = 2
    realVal = 9.68854806636610
    assert(math.abs(EllipseMaster.cantrell(a, b)/realVal - 1) < 0.01)

    b = 10
    realVal = 40.63266238102069
    assert(math.abs(EllipseMaster.cantrell(a, b)/realVal - 1) < 0.01)

    b = 1000
    realVal = 4000.01691838166516
    assert(math.abs(EllipseMaster.cantrell(a, b)/realVal - 1) < 0.01)
  }
  */
  // TODO: Check ground truth
  /*
  it ("Exact approximation tests") {
    val a = 1
    var b = 1
    // Real values for 10, 50, 250, 1250
    var realVal = IndexedSeq[Double](6.28318530717959, 6.28318530717959, 6.28318530717959, 6.28318530717959)

    var n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.exact(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 5
    }

    b = 2
    // Real values for 10, 50, 250, 1250
    realVal = IndexedSeq[Double](9.69160447929656, 9.68844822218643, 9.68844822054768, 9.68844822054768)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.exact(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 5
    }

    b = 10
    // Real values for 10, 50, 250, 1250
    realVal = IndexedSeq[Double](41.42030490115282, 40.70623873690818, 40.64052842541464, 40.63974180295930)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.exact(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 5
    }

    b = 1000
    // Real values for 10, 50, 250, 1250
    realVal = IndexedSeq[Double](4106.61427461212224, 4020.25802285236841, 4004.01689018024035, 4000.80887965779220)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.exact(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 5
    }
  }
*/
  // TODO: Re-test after changes.
  it ("Bessel approximation tests") {
    val a = 1
    var b = 1
    // Real values for 10, 100, 1000
    var realVal = IndexedSeq[Double](6.28318530717959, 6.28318530717959, 6.28318530717959)

    var n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.bessel(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }

    b = 2
    // Real values for 10, 100, 1000
    realVal = IndexedSeq[Double](9.68844822054742, 9.68844822054768, 9.68844822054768)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.bessel(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }

    b = 10
    // Real values for 10, 100, 1000
    realVal = IndexedSeq[Double](40.63963178516602, 40.63974180100895, 40.63974180100895)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.bessel(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }

    b = 1000
    // Real values for 10, 100, 1000
    realVal = IndexedSeq[Double](3998.65719052395661, 4000.00902882897572, 4000.01558741091912)

    n = 10
    for (i <- realVal.indices){
      assert(math.abs(EllipseMaster.bessel(a, b, n)/realVal(i) - 1) < 0.01)
      n *= 10
    }
  }

  /*
  it ("Holder low eccentricity approximation test"){
    val a = 1

    var b = 1
    var realVal = 6.28318530717959
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal- 1) < 0.01)

    b = 2
    realVal = 9.70448163064351
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal- 1) < 0.01)

    b = 10
    realVal = 40.75658566530318
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal- 1) < 0.01)

    b = 1000
    realVal = 4000.06474173655124
    assert(math.abs(EllipseMaster.holderHigh(a, b)/realVal- 1) < 0.01)
  }
  */

}
