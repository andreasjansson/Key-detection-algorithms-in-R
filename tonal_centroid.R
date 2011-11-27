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


## based on Harte, Sandler and Gasser: Detecting Harmonic Change in Musical Audio


## Return the tonal centroid for an 12-dimensional chroma vector
tonal.centroid <- function(chroma) {
  1 / magnitude(chroma) * apply(centroid.transform * chroma, 2, sum)
}

centroid.transform <- cbind(1 * sin(0:11 * (7 * pi) / 6),
                   1 * cos(0:11 * (7 * pi) / 6),
                   1 * sin(0:11 * (3 * pi) / 2),
                   1 * cos(0:11 * (3 * pi) / 2),
                   .5 * sin(0:11 * (2 * pi) / 3),
                   .5 * sin(0:11 * (2 * pi) / 3));

magnitude <- function(v) {
  sqrt(sum(v ^ 2))
}
