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
  ws <- 8192
  time <- 300
  bins <- 12 * 5 # must be an odd multiple of 12
  
  a1 <- readMP3(filename)
  print("done reading mp3")
  a1 <- prepare.audio(a1, fs, ws, time)
  print("done downsampling")
  s <- Mod(specgram(a1@left - 127, ws, fs)$S)
  s <- filter.specgram(s, ws, fs)
  print("done specgram")
  m <- spec2bins(s, fs, bins, ws)
#  m <- tune.bins(m)
  print("done binning")
  return(m)
}

tune.bins <- function(m) {
  bins <- nrow(m) / 12

  # shift bins so that 439hz is still a
  m <- ashift(m, c(ceil(bins / 2), 0))
  
  centre <- bins.centre(m)
  m <- ashift(m, c(ceiling(bins / 2) - centre, 0))
  m <- summarise.bins(m)
  return(m)
}

summarise.bins <- function(m) {
  bins <- nrow(m) / 12
  group <- rep(1:12, each = bins)
  m <- rowsum(m, group)
  return(m)
}

bins.centre <- function(m) {
  bins <- nrow(m) / 12
  b <- rowSums(m)
  sums <- unlist(lapply(1:bins, function(x) {
    sum(b[(0:11 * bins + x)])
  }))
  centre <- which(sums == max(sums))
  return(centre)
}

spec2bins <- function(s, fs, bins, ws) {
  m <- matrix(0, nrow=bins, ncol=ncol(s))
  n <- nrow(s)
  min.j <- floor(100 * (ws / 2) / fs)
  max.j <- floor(2000 * (ws / 2) / fs)

  # TODO: UPNEXT: get this right, test with actual audio data
  c0 <- 16.3516
  ps <- ceiling((log2((1:n * fs / n) / c0) * bins)) %% bins

  for(i in 1:ncol(s)) {
    if(i %% 100 == 0)
      print(sprintf("%d / %d", i, ncol(s)))
    
    for(j in min.j:max.j) {
      p <- ps[j]
      if(p == 0)
        p <- bins
      m[p, i] <- m[p, i] + s[j, i]
    }
  }

  # normalise
  m <- m / max(m)
  return(m)
}

prepare.audio <- function(a, fs, ws, time) {
  cfs <- a@samp.rate
  if(length(a@left) > cfs * time)
    a <- extractWave(a, to=cfs*time, interact=FALSE)
  if(a@stereo)
    a <- mono(a, "both")
  a <- normalize(a, "8")
  a@bit <- 8
  a <- downsample(a, fs)
  return(a)
}

filter.specgram <- function(s, ws, fs) {
  min.j <- floor(100 * (ws / 2) / fs)
  max.j <- floor(2000 * (ws / 2) / fs)

  # remove quiet partials
  s[s < quantile(s, .94)] <- 0

  # linear low pass filter
  # s <- s / ((1:nrow(s)) / 50)

  s[1 : (min.j - 1),] <- 0
  s[(max.j + 1) : nrow(s),] <- 0

  return(s)
}

reassemble.specgram <- function(spec, window.size = 8192, fs = 11025) {
  a <- integer(window.size * ncol(spec))
  real.ws <- window.size
  window.size <- window.size / 2
  for(x in 1:(ncol(spec) - 1)) {
    print(x)
    for(y in 1:(window.size / 2 - 1)) {

      if(Mod(spec[y, x]) > 0) {
        frames <- (window.size * (x - 1) + 1):(window.size * x)

        frequency <- y
        wave <- cos(1:real.ws * frequency * 2 * pi / real.ws)[1:length(frames)]

        a[frames] <- a[frames] + wave
      }
    }
  }

  w <- Wave(a, bit = 8, samp.rate = fs)
  w <- normalize(w, "8")
  return(w)
}

plot.chromagram <- function(chromagram) {
  image(t(chromagram), xaxt="n", yaxt="n")
  axis(2, at=0:11/11, c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"), tick=FALSE)
#  axis(1, at=0:(ncol(m) - 1) / ncol(m), round(s1$t, 1), tick=FALSE)
}

plot.bins <- function(m) {
  bins <- nrow(m)
  rc <- rowSums(m)
  names(rc)[(1:12) * (bins / 12)] <- c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b')
  barplot(rc, col=c("red","green","blue", "yellow", "purple", "orange", "pink", colours()[sample(length(colours()), length(colours()))])[1:(bins / 12)])
}
