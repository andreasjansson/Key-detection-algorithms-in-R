## Key detection algorithms in R
## Copyright (C) 2011 <andreas@jansson.me.uk>

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.

## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.


## Based on Noland & Sandler: Key Estimation Using a Hidden Markov Model
## The thing that sets this method apart is the use of transitions between
## two consecutive chords as HMM observations, almost like in a second
## order Markov model. And obviously the use of Krumhansl's perceptual
## tests.


# TODO: write comment about not figuring out where to put aug chords

library(HMM)
library(magic)

get.hmm <- function() {
  return(initHMM(get.state.names(), get.symbol.names(), get.start.probs(),
                 get.normalised.trans.probs(), get.normalised.emission.probs()))
}

get.transitions <- function(chords) {
  chords.table <- cbind(chords[1:(length(chords) - 1)], chords[2:length(chords)])
  transitions <- apply(chords.table, 1, function(x) { get.transition.symbol(x[1], x[2]) })
  return(transitions)
}

## indexed from 0
get.state.names <- function() {
  return(0:23)
}

## 1:12 == major chords, 13:24 == minor chords, 25:36 == dim chords, 37 = no chord
## indexed from 0
get.symbol.names <- function() {
  return(0:(37 ^ 2 - 1))
}

get.start.probs <- function() {
  return(rep(1 / 24, 24))
}

get.normalised.trans.probs <- function() {
  probs <- get.trans.probs() + 1
  # normalise
  probs <- t(t(probs) / colSums(probs))
  return(probs)
}

get.normalised.emission.probs <- function() {
  probs <- get.emission.probs()
  # normalise
  probs <- probs / rowSums(probs)
  return(probs)
}

## Return a matrix of correlations between keys, the first 12 columns being
## correlations from the major keys and the last 12 from minor keys.
get.trans.probs <- function() {

  # TODO UPNEXT: make test pass

  cors <- krumhansl.key.profile.correlations
  probs <- matrix(0, 24, 24)
  for(i in 1:24) {

    if(i <= 12) {
      probs[,i] <- c(shift(cors[1:12, 1], i - 1),
                     shift(cors[13:24, 1], i - 1))
    }
    else {
      probs[,i] <- c(shift(cors[1:12, 2], i - 13),
                     shift(cors[13:24, 2], i - 13))
    }
  }
  return(probs)
}

get.emission.probs <- function() {
  # 37 chords, maj min dim + no chord
  ntrans <- 37 ^ 2
  probs <- matrix(0, nrow = 24, ncol = ntrans)

  for(key in 0:23) {
    for(transition in 0:(ntrans - 1)) {
      chords <- get.chords.in.transition(transition)
      chord1index <- get.diatonic.index(chords[1], key)
      chord2index <- get.diatonic.index(chords[2], key)

      # if any of the members of the transition are non-diatonic,
      # set the emission probability to 1
      if(length(c(chord1index, chord2index)) < 2) {
        prob <- 1
      }

      # the probability of staying on the same diatonic chord is taken from
      # Krumhansl's harmonic hierarchy ratings, and boosted by 2
      else if(chord1index == chord2index) {
        if(key < 12) {
          row <- c(1, 3 + 12, 5 + 12, 6, 8, 10 + 12, 12 + 24)[chord1index]
          prob <- krumhansl.harmonic.hierarchy.ratings[row, 1] + 2
        }
        else {
          row <- c(4, 6 + 12, 8 + 12, 9, 11, 1 + 12, 3 + 24)[chord1index]
          prob <- krumhansl.harmonic.hierarchy.ratings[row, 2] + 2
        }
      }

      # the probability of transitioning from one diatonic chord to another
      # diatonic chord is taken from Krumhansl's chord transition ratings
      else {
        prob <- krumhansl.chord.transition.ratings[chord1index, chord2index]        
      }

      probs[key + 1, transition + 1] <- prob
    }
  }
  return(probs)
}

## Return the diatonic index of chord in key, or integer(0) if chord
## is not diatonic.
get.diatonic.index <- function(chord, key) {

  # normalise minor keys
  if(key > 12)
    key <- (key - 9) %% 12

  # major chords
  if(chord < 12) {
    chord <- (chord - key) %% 12
    return(switch(as.character(chord),
                  '0' = 1,
                  '5' = 4,
                  '7' = 5,
                  integer(0)))
  }

  # minor chords
  if(chord < 24) {
    chord <- (chord - 12 - key) %% 12
    return(switch(as.character(chord),
                  '2' = 2,
                  '4' = 3,
                  '9' = 6,
                  integer(0)))
  }

  # diminished chords
  if(chord < 36) {
    chord <- (chord - 24 - key) %% 12
    return(switch(as.character(chord),
                  '11' = 7,
                  integer(0)))
  }

  return(integer(0))
}

## Read a file in Chris Harte's format.
## Returns a vector of of chord ids.
# TODO: fix this to match : syntax in get.chord.id
read.chord.file <- function(filename) {
  data <- read.table(filename, sep = ' ', comment.char = '')
  chords <- data[,3]
  chords <- unlist(lapply(chords, parse.chord))
  return(get.chord.id(chords))
}

get.transition.symbol <- function(chord1, chord2) {
  return(chord1 * 37 + chord2)
}

get.chords.in.transition <- function(transition) {
  chord1 <- floor(transition / 37)
  chord2 <- transition %% 37
  return(c(chord1, chord2))
}

parse.chord <- function(unparsed) {
  # only interested in minor, major, dim chords and [N]o chord
  if(unparsed == 'N')
    return('N')
  root <- gsub('[:/].*$', '', unparsed)
  if(length(grep(':min', unparsed)))
    return(paste(root, 'min', sep = ':'))
  if(length(grep(':h?dim', unparsed)))
    return(paste(root, 'dim', sep = ':'))
  return(paste(root, 'maj', sep = ':'))
}

## Returns an integer from a chord name.
## (vectorised)
get.chord.id <- function(name) {
  chord.names <-
    c('C:maj','C#:maj','Db:maj','D:maj','D#:maj','Eb:maj','E:maj','E#:maj','Fb:maj','F:maj','F#:maj',
      'Gb:maj','G:maj','G#:maj','Ab:maj','A:maj','A#:maj','Bb:maj','B:maj','B#:maj','Cb:maj',
      'C:min','C#:min','Db:min','D:min','D#:min','Eb:min','E:min','E#:min','Fb:min','F:min','F#:min',
      'Gb:min','G:min','G#:min','Ab:min','A:min','A#:min','Bb:min','B:min','B#:min','Cb:min',
      'C:dim','C#:dim','Db:dim','D:dim','D#:dim','Eb:dim','E:dim','E#:dim','Fb:dim','F:dim','F#:dim',
      'Gb:dim','G:dim','G#:dim','Ab:dim','A:dim','A#:dim','Bb:dim','B:dim','B#:dim','Cb:dim',
      'N')
  chord.ids <-
    c(rep(c(0, 1, 1, 2, 3, 3, 4, 5, 4, 5, 6, 6, 7, 8, 8, 9, 10, 10, 11, 0, 11), 3) +
      rep(0:2 * 12, each = 21), 3 * 12 + 1)

  names(chord.ids) <- chord.names
  return(unname(chord.ids[name]))
}

krumhansl.key.profile.correlations <-
  t(matrix(c(
             # Cmaj  Cmin
              1.000,  0.511, # Cmaj 
             -0.500, -0.158, # C#maj
              0.040, -0.402, # Dmaj 
             -0.105,  0.651, # D#maj
             -0.185, -0.508, # Emaj 
              0.591,  0.241, # Fmaj 
             -0.683, -0.369, # F#maj
              0.591,  0.215, # Gmaj 
             -0.185,  0.536, # G#maj
             -0.105, -0.654, # Amaj 
              0.040,  0.237, # A#maj
             -0.500, -0.298, # Bmaj 
              0.511,  1.000, # Cmin 
             -0.298, -0.394, # C#min
              0.237, -0.160, # Dmin 
             -0.654,  0.055, # D#min
              0.536, -0.003, # Emin 
              0.215,  0.339, # Fmin 
             -0.369, -0.673, # F#min
              0.241,  0.339, # Gmin 
             -0.508, -0.003, # G#min
              0.651,  0.055, # Amin 
             -0.402, -0.160, # A#min
             -0.158, -0.394  # Bmin 
             ), nrow = 2))

krumhansl.chord.transition.ratings <-
  t(matrix(c(
           # I     ii    iii   IV    V     vi    vii
             0,    5.10, 4.78, 5.91, 5.94, 5.26, 4.57, # I
             5.69, 0,    4.00, 4.76, 6.10, 4.97, 5.41, # ii
             5.38, 4.47, 0,    4.63, 5.03, 4.60, 4.47, # iii
             5.94, 5.00, 4.22, 0,    6.00, 4.35, 4.79, # IV
             6.19, 4.79, 4.47, 5.51, 0,    5.19, 4.85, # V
             5.04, 5.44, 4.72, 5.07, 5.56, 0,    4.50, # vi
             5.85, 4.16, 4.16, 4.53, 5.16, 4.19, 0     # vii
             ), nrow = 7))

krumhansl.harmonic.hierarchy.ratings <-
  t(matrix(c(
             # C:maj  C:min
             6.66, 5.30, # C:maj 
             4.71, 4.11, # C:#maj
             4.60, 3.83, # D:maj 
             4.31, 4.14, # D:#maj
             4.64, 3.99, # E:maj 
             5.59, 4.41, # F:maj 
             4.36, 3.92, # F:#maj
             5.33, 4.38, # G:maj 
             5.01, 4.45, # G:#maj
             4.64, 3.69, # A:maj 
             4.73, 4.22, # A:#maj
             4.67, 3.85, # B:maj 
             3.75, 5.90, # C:min 
             2.59, 3.08, # C:#min
             3.12, 3.25, # D:min 
             2.18, 3.50, # D:#min
             2.76, 3.33, # E:min 
             3.19, 4.60, # F:min 
             2.13, 2.98, # F:#min
             2.68, 3.48, # G:min 
             2.61, 3.53, # G:#min
             3.62, 3.78, # A:min 
             2.56, 3.13, # A:#min
             2.76, 3.14, # B:min 
             3.27, 3.93, # C:dim 
             2.70, 2.84, # C:#dim
             2.59, 3.43, # D:dim 
             2.79, 3.42, # D:#dim
             2.64, 3.51, # E:dim 
             2.54, 3.41, # F:dim 
             3.25, 3.91, # F:#dim
             2.58, 3.16, # G:dim 
             2.36, 3.17, # G:#dim
             3.35, 4.10, # A:dim 
             2.38, 3.10, # A:#dim
             2.64, 3.18  # B:dim 
             ), nrow = 2))
