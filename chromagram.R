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


## Very basic, untuned, chromagram extraction from mp3

library(tuneR)
library(signal)

## Return a matrix where the rows are frames and the columns
## represent bins in a 12 dimensional chroma vector
chromagram.from.mp3 <- function(filename) {
  fs <- 11025
  ws <- 2048
  time <- 30
  
  a1 <- readMP3(filename)
                                        #a1 <- readMP3("c9.mp3")
  print("done reading mp3")

  cfs <- a1@samp.rate
  if(length(a1@left) > cfs * time)
    a1 <- extractWave(a1, to=cfs*time, interact=FALSE)
  a1 <- mono(a1, "left")
  a1 <- normalize(a1, "8")
  a1 <- downsample(a1, fs)
  print("done downsampling")

  s1 <- specgram(a1@left - 127, ws, fs)
  print("done specgram")

  s <- Mod(s1$S)

  # remove quiet partials
  s[s < quantile(s, .5)] <- 0

  # linear low pass filter
  s <- s / ((1:nrow(s)) / 50)

  m <- matrix(0, nrow=12, ncol=ncol(s))
  n <- nrow(s)

  # TODO: adaptive tuning!
  ps <- round((log2((1:n * fs / n) / 440) * 12 + 10)) %% 12

  for(i in 1:ncol(s)) {
    if(i %% 100 == 0)
      print(sprintf("%d / %d", i, ncol(s)))
    
    for(j in 3:(n/2)) { # only 0-5000hz
      p <- ps[j - 1]
      if(p == 0)
        p <- 12
      m[p, i] <- m[p, i] + s[j, i]
    }
  }

  # normalise
  m <- m / max(m)

  return(m)
}

plot.chromagram <- function(chromagram) {
  image(t(chromagram), xaxt="n", yaxt="n")
  axis(2, at=0:11/11, c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"), tick=FALSE)
#  axis(1, at=0:(ncol(m) - 1) / ncol(m), round(s1$t, 1), tick=FALSE)
}
