# TODO: test minor keys
test.get.diatonic.index <- function() {
  checkEquals(1, get.diatonic.index(0, 0))
  checkEquals(2, get.diatonic.index(14, 0))
  checkEquals(3, get.diatonic.index(16, 0))
  checkEquals(4, get.diatonic.index(5, 0))
  checkEquals(5, get.diatonic.index(7, 0))
  checkEquals(6, get.diatonic.index(21, 0))
  checkEquals(7, get.diatonic.index(35, 0))

  checkEquals(1, get.diatonic.index(1, 1))
  checkEquals(2, get.diatonic.index(15, 1))
  checkEquals(3, get.diatonic.index(17, 1))
  checkEquals(4, get.diatonic.index(6, 1))
  checkEquals(5, get.diatonic.index(8, 1))
  checkEquals(6, get.diatonic.index(22, 1))
  checkEquals(7, get.diatonic.index(24, 1))

  checkEquals(1, get.diatonic.index(2, 2))
  checkEquals(2, get.diatonic.index(16, 2))
  checkEquals(3, get.diatonic.index(18, 2))
  checkEquals(4, get.diatonic.index(7, 2))
  checkEquals(5, get.diatonic.index(9, 2))
  checkEquals(6, get.diatonic.index(23, 2))
  checkEquals(7, get.diatonic.index(25, 2))

  checkEquals(1, get.diatonic.index(4, 4))
  checkEquals(2, get.diatonic.index(18, 4))
  checkEquals(3, get.diatonic.index(20, 4))
  checkEquals(4, get.diatonic.index(9, 4))
  checkEquals(5, get.diatonic.index(11, 4))
  checkEquals(6, get.diatonic.index(13, 4))
  checkEquals(7, get.diatonic.index(27, 4))
}
