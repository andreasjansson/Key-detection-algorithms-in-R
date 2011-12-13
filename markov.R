source("chromagram.R")
library(magic)

make.discrete.markov.model <- function(filename = NULL,
                                       discretise = discretise.template.temperlay.major(),
                                       order = 1,
                                       values.count = 12,
                                       chromagram = NULL) {
  if(is.null(chromagram))
    chromagram <- chromagram.from.mp3(filename)

  g.chromagram <<- chromagram
  g.keys <<- c()

  model <- matrix(0, nrow = values.count ^ order, ncol = values.count)
  prev <- integer(order)
  for(i in 1:ncol(chromagram)) {
    chroma <- chromagram[,i]
    value <- discretise(chroma)
    if(i > order) {
      key <- get.key(prev, values.count)
      g.keys <<- c(g.keys, key)
      model[key, value] <- model[key, value] + 1
    }
    prev <- shift(prev)
    prev[1] <- value
  }

  model <- model / rowSums(model)
  model[is.nan(model)] <- 0
  return(model)
}

get.key <- function(values, values.count) {
  values <- values - 1
  sum(values * values.count ^ ((length(values) - 1):0)) + 1
}

discretise.template.temperlay.major <- function() {
  return(discretise.template.pitch.class(c(5, 2, 3.5, 2, 4.5, 4, 2, 4.5, 2, 3.5, 1.5, 4) / 5))
}

discretise.template.flat <- function() {
  return(discretise.template.pitch.class(c(1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1)))
}

discretise.template.major.chords <- function() {
  return(discretise.template.pitch.class(c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0)))
}

discretise.template.pitch.class <- function(template) {
  g.pitch.classes <<- list()
  pcl <- length(template)
  template.matrix <- matrix(rep(template, pcl), nrow = pcl)
  for(i in 1:pcl) {
    template.matrix[,i] <- shift(template.matrix[,i], (i - 1))
  }

  dimnames(template.matrix) <- markov.key.dimnames
  
  return(function(chroma) {
    names(chroma) <- markov.key.dimnames[[1]]
    sums <- colSums(chroma * template.matrix)

    three.max <- sums[sums > rev(sort(sums))[4]]
    three.max.names = unlist(lapply(1:3, function(i) {
      w <- which(three.max[i] == sums)
      return(w[sample(length(w), 1)])
    }))
    names(three.max) <- markov.key.dimnames[[1]][three.max.names]
    print(rev(sort(three.max)))

    max.sum <- which(sums == max(sums))
    g.pitch.classes <<- c(g.pitch.classes, list(max.sum))
    if(length(max.sum) > 1)
      return(sample(max.sum, 1))
    return(max.sum)
  })
}

discretise.chords <- function() {
  major <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0)
  minor <- c(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  dim   <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0)

  template.matrix <- matrix(c(rep(major, 12), rep(minor, 12), rep(dim, 12)), nrow = 12)
  for(i in 1:36) {
    template.matrix[,i] <- shift(template.matrix[,i], (i - 1))
  }
  return(function(chroma) {
    sums <- colSums(chroma * template.matrix)
    max.sum <- which(sums == max(sums))
    g.pitch.classes <<- c(g.pitch.classes, list(max.sum))
    if(length(max.sum) > 1)
      return(sample(max.sum, 1))
    return(max.sum)
  })
}

correlate.models <- function(m1, m2, transpose.m1 = 0, transpose.m2 = 0) {
  return(sum(abs(ashift(m1, rep(- transpose.m1, 2)) -
                 ashift(m2, rep(- transpose.m2, 2)))))
}

plot.correlations <- function(m1, m2) {
  barplot(unlist(lapply(0:11, function(x) { correlate.models(m1, m2, x) })))
}

plot.colSums <- function(m) {
  barplot(colSums(m))
}

binary.correlation <- function(m1, m2) {
  
}

markov.key.dimnames <- list(c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b'), c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b'))

markov.chord.dimnames <- list(c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b',
                                'c-', 'c#-', 'd-', 'd#-', 'e-', 'f-', 'f#-', 'g-', 'g#-', 'a-', 'a#-', 'b-',
                                'c/', 'c#/', 'd/', 'd#/', 'e/', 'f/', 'f#/', 'g/', 'g#/', 'a/', 'a#/', 'b/'),
                              c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b',
                                'c-', 'c#-', 'd-', 'd#-', 'e-', 'f-', 'f#-', 'g-', 'g#-', 'a-', 'a#-', 'b-',
                                'c/', 'c#/', 'd/', 'd#/', 'e/', 'f/', 'f#/', 'g/', 'g#/', 'a/', 'a#/', 'b/'))
                              

