source('noland.R')

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

  checkEquals(1, get.diatonic.index(0, 21))
  checkEquals(2, get.diatonic.index(14, 21))
  checkEquals(3, get.diatonic.index(16, 21))
  checkEquals(4, get.diatonic.index(5, 21))
  checkEquals(5, get.diatonic.index(7, 21))
  checkEquals(6, get.diatonic.index(21, 21))
  checkEquals(7, get.diatonic.index(35, 21))

  checkEquals(1, get.diatonic.index(1, 22))
  checkEquals(2, get.diatonic.index(15, 22))
  checkEquals(3, get.diatonic.index(17, 22))
  checkEquals(4, get.diatonic.index(6, 22))
  checkEquals(5, get.diatonic.index(8, 22))
  checkEquals(6, get.diatonic.index(22, 22))
  checkEquals(7, get.diatonic.index(24, 22))

  checkEquals(1, get.diatonic.index(2, 23))
  checkEquals(2, get.diatonic.index(16, 23))
  checkEquals(3, get.diatonic.index(18, 23))
  checkEquals(4, get.diatonic.index(7, 23))
  checkEquals(5, get.diatonic.index(9, 23))
  checkEquals(6, get.diatonic.index(23, 23))
  checkEquals(7, get.diatonic.index(25, 23))

  checkEquals(1, get.diatonic.index(4, 13))
  checkEquals(2, get.diatonic.index(18, 13))
  checkEquals(3, get.diatonic.index(20, 13))
  checkEquals(4, get.diatonic.index(9, 13))
  checkEquals(5, get.diatonic.index(11, 13))
  checkEquals(6, get.diatonic.index(13, 13))
  checkEquals(7, get.diatonic.index(27, 13))
}

                                        # a few spotchecks
test.get.emission.probs <- function() {
  probs <<- get.emission.probs()
  f <- function(x, y) { get.transition.symbol(x, y) + 1 }

  checkEquals(8.66, probs[1, f( 0,  0)])
  checkEquals(5.51, probs[8, f( 2,  0)])
  checkEquals(1.00, probs[1, f( 2,  0)])
  checkEquals(1.00, probs[2, f( 2,  0)])
  checkEquals(4.72, probs[1, f(21, 16)])
  checkEquals(4.97, probs[8, f(21, 16)])
  checkEquals(4.79, probs[3, f( 9, 16)])
  checkEquals(4.64, probs[1, f(35, 35)])
  checkEquals(5.85, probs[1, f(35,  0)])
  checkEquals(5.12, probs[8, f(21, 21)])
  checkEquals(5.26, probs[8, f( 7, 16)])
  checkEquals(5.85, probs[8, f(30,  7)])

  checkEquals(6.14, probs[22, f( 0,  0)])
  checkEquals(5.51, probs[17, f( 2,  0)])
  checkEquals(1.00, probs[22, f( 2,  0)])
  checkEquals(1.00, probs[23, f( 2,  0)])
  checkEquals(4.72, probs[22, f(21, 16)])
  checkEquals(4.97, probs[17, f(21, 16)])
  checkEquals(4.79, probs[24, f( 9, 16)])
  checkEquals(5.43, probs[22, f(35, 35)])
  checkEquals(5.85, probs[22, f(35,  0)])
  checkEquals(6.60, probs[17, f(21, 21)])
  checkEquals(5.26, probs[17, f( 7, 16)])
  checkEquals(5.85, probs[17, f(30,  7)])
}

test.get.trans.probs <- function() {
  probs <<- get.trans.probs()

  checkEquals( 1.000, probs[ 1,  1])
  checkEquals( 0.511, probs[ 1, 13])
  checkEquals( 0.511, probs[13,  1])
  checkEquals( 0.040, probs[ 3,  1])
  checkEquals( 0.591, probs[ 8,  1])
  checkEquals( 0.511, probs[ 8, 20])
  checkEquals( 0.511, probs[20,  8])
  checkEquals(-0.402, probs[13,  3])
  checkEquals(-0.402, probs[20, 10])
  checkEquals(-0.160, probs[20, 22])
}

test.parse.chord <- function() {
  p <- parse.chord
  checkEquals('A:maj', p('A'))
  checkEquals('A:maj', p('A/3'))
  checkEquals('A#:maj', p('A#/3'))
  checkEquals('A:maj', p('A:sus4(2)'))
  checkEquals('A:maj', p('A:maj7/5'))
  checkEquals('A:maj', p('A:7'))
  checkEquals('N', p('N'))
  checkEquals('F#:maj', p('F#'))
  checkEquals('A:maj', p('A:aug')) # hmm
  checkEquals('A:min', p('A:min'))
  checkEquals('A:min', p('A:min/4'))
  checkEquals('C#:dim', p('C#:dim/b3'))
  checkEquals('Eb:min', p('Eb:min'))
  checkEquals('C#:dim', p('C#:hdim/b3'))
  checkEquals('G:maj', p('G:(1, b3, 4)'))
  checkEquals('C:maj', p('C:9(*3)'))
  checkEquals('C:maj', p('C:(1,5)'))
}

test.get.chord.id <- function() {
  p <- get.chord.id
  checkEquals(0, p('C:maj'))
  checkEquals(12, p('C:min'))
  checkEquals(24, p('C:dim'))
  checkEquals(25, p('C#:dim'))
  checkEquals(3, p('Eb:maj'))
  checkEquals(9, p('A:maj'))
  checkEquals(10, p('Bb:maj'))
  checkEquals(10, p('A#:maj'))
  checkEquals(c(10, 2), p(c('A#:maj', 'D:maj')))
  checkEquals(37, p('N'))
}

test.get.transitions <- function() {
  s <- get.transition.symbol
  checkEquals(c(s(1, 2), s(2, 29), s(29, 4), s(4, 37)), get.transitions(c(1, 2, 29, 4, 37)))
}

run.noland.tests <- function() {
  source('noland_test.R')
  test.suite <- defineTestSuite("noland",
                                dirs = file.path("."),
                                testFileRegexp = '^noland_test.R$')
  runTestSuite(test.suite)
}
