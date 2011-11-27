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


## Read a file in Chris Harte's format.
## Returns a vector of of chord ids.
read.chord.file <- function(filename) {
  data <- read.table(filename, sep = ' ')
  chords <- data[,3]

  # only interested in minor and major chords
  # minor == '-', major == ''
  chords <- gsub(':min.*$', '-', chords)
  chords <- gsub(':.*$', '', chords)
  chords <- gsub('/.*$', '', chords)

  return(get.chord.id(chords))
}

## Returns a 2-column matrix of transitions,
## col 1 == from, col 2 == to
get.transitions <- function(chords) {
  return(cbind(chords[1:(length(chords) - 1)], chords[2:length(chords)]))
}

## Returns an integer from a chord name.
## (vectorised)
get.chord.id <- function(name) {
  chord.names <-
    c('C','C#','Db','D','D#','Eb','E','E#','Fb','F','F#','Gb','G','G#','Ab','A','A#','Bb','B','B#','Cb',
      'C-','C#-','Db-','D-','D#-','Eb-','E-','E#-','Fb-','F-','F#-',
      'Gb-','G-','G#-','Ab-','A-','A#-','Bb-','B-','B#-','Cb-',
      'N')
  chord.ids <-
    c(0, 1, 1, 2, 3, 3, 4, 5, 4, 5, 6, 6, 7, 8, 8, 9, 10, 10, 11, 0, 11, 
      12, 13, 13, 14, 15, 15, 16, 17, 16, 17, 18, 18, 19, 20, 20, 21, 22, 22, 23, 12, 23,
      24)

  names(chord.ids) <- chord.names
  return(chord.ids[name])
}

krumhansl.key.profile.correlations <-
  t(matrix(c(
             # Cmaj  Cmin
              1.000,  0.511, # Cmaj 
             −0.500, −0.158, # C#maj
              0.040, −0.402, # Dmaj 
             −0.105,  0.651, # D#maj
             −0.185, −0.508, # Emaj 
              0.591,  0.241, # Fmaj 
             −0.683, −0.369, # F#maj
              0.591,  0.215, # Gmaj 
             −0.185,  0.536, # G#maj
             −0.105, −0.654, # Amaj 
              0.040,  0.237, # A#maj
             −0.500, −0.298, # Bmaj 
              0.511,  1.000, # Cmin 
             −0.298, −0.394, # C#min
              0.237, −0.160, # Dmin 
             −0.654,  0.055, # D#min
              0.536, −0.003, # Emin 
              0.215,  0.339, # Fmin 
             −0.369, −0.673, # F#min
              0.241,  0.339, # Gmin 
             −0.508, −0.003, # G#min
              0.651,  0.055, # Amin 
             −0.402, −0.160, # A#min
             −0.158, −0.394  # Bmin 
             ), nrow = 2))

krumhansl.chord.transition.ratings <-
  t(matrix(c(
           # I    ii    iii   IV     V     vi    vii
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
             # Cmaj  Cmin
             6.66, 5.30, # Cmaj 
             4.71, 4.11, # C#maj
             4.60, 3.83, # Dmaj 
             4.31, 4.14, # D#maj
             4.64, 3.99, # Emaj 
             5.59, 4.41, # Fmaj 
             4.36, 3.92, # F#maj
             5.33, 4.38, # Gmaj 
             5.01, 4.45, # G#maj
             4.64, 3.69, # Amaj 
             4.73, 4.22, # A#maj
             4.67, 3.85, # Bmaj 
             3.75, 5.90, # Cmin 
             2.59, 3.08, # C#min
             3.12, 3.25, # Dmin 
             2.18, 3.50, # D#min
             2.76, 3.33, # Emin 
             3.19, 4.60, # Fmin 
             2.13, 2.98, # F#min
             2.68, 3.48, # Gmin 
             2.61, 3.53, # G#min
             3.62, 3.78, # Amin 
             2.56, 3.13, # A#min
             2.76, 3.14, # Bmin 
             3.27, 3.93, # Cdim 
             2.70, 2.84, # C#dim
             2.59, 3.43, # Ddim 
             2.79, 3.42, # D#dim
             2.64, 3.51, # Edim 
             2.54, 3.41, # Fdim 
             3.25, 3.91, # F#dim
             2.58, 3.16, # Gdim 
             2.36, 3.17, # G#dim
             3.35, 4.10, # Adim 
             2.38, 3.10, # A#dim
             2.64, 3.18  # Bdim 
             ), nrow = 2))
