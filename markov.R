source("chromagram.R")
library(magic)

get.best.estimates <- function(chromagram, estimates.per.frame = 3) {
  estimates <- c()
  estimator <- get.local.estimates(estimates.per.frame)
  for(i in 1:ncol(chromagram)) {
    chroma <- chromagram[,i]
    local.estimates <- estimator(chroma)
    if(!all(is.na(local.estimates)))
      estimates <- c(estimates, list(local.estimates))
  }
  return(estimates)
}

get.local.estimates <- function(count) {
  major <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0)
  minor <- c(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  diminished <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0)
  
  cutoff <- 1.5

  templates <- list(major, minor, diminished)
  template.matrix <- matrix(nrow = 12, ncol = 0)

  for(template in templates)
    template.matrix <- cbind(template.matrix, matrix(rep(template, 12), nrow = 12))

  for(i in 1:(12 * length(templates))) {
    template.matrix[,i] <- shift(template.matrix[,i], (i - 1))
  }

  return(function(chroma) {
    sums <- colSums(chroma * template.matrix)

    best <- sums[sums > rev(sort(sums))[count + 1]]
    chords <- unlist(lapply(1:count, function(i) {
      w <- which(best[i] == sums)
      return(w[sample(length(w), 1)])
    }))
    best <- rev(sort(best))

    if(!is.na(best[1]) && best[1] > cutoff)
      return(chords)

    return(NA)
  })
}

# dynamic programming, as described by temperlay
find.best.estimates.path <- function(estimates, estimates.per.frame = 3) {
  best.prev.indices <- matrix(NA, nrow = length(estimates), ncol = estimates.per.frame)
  costs <- matrix(NA, nrow = length(estimates), ncol = estimates.per.frame)
  costs[1,] <- 0

  for(row in 2:length(estimates)) {
    current <- estimates[[row]]
    prev <- estimates[[row - 1]]

    min.cost <- Inf
    min.i.prev <- NA
    for(i.cur in 1:estimates.per.frame) {
      for(i.prev in 1:estimates.per.frame) {
        cost <- costs[row - 1, i.prev] + get.chord.transition.cost(prev[i.prev], current[i.cur])
        if(!is.na(cost) && cost < min.cost) {
          min.cost <- cost
          min.i.prev <- i.prev
        }
      }
      costs[row, i.cur] <- min.cost
      best.prev.indices[row, i.cur] <- min.i.prev
    }
  }

  best.path.indices <- trace.best.path.indices(best.prev.indices, costs)
  best.path <- integer(length(estimates))
  for(row in 1:length(estimates)) {
    best.path[row] <- estimates[[row]][best.path.indices[row]]
  }

  return(best.path)
}

trace.best.path.indices <- function(best.prev.indices, costs) {
  count <- nrow(best.prev.indices)
  path <- integer(count)
  lowest.values <- min(costs[nrow(costs),])
  lowest <- which(costs[nrow(costs),] == lowest.values)[sample(length(lowest.values), 1)]
  path[count] <- lowest

  for(i in count:2) {
    lowest <- best.prev.indices[i, lowest]
    path[i - 1] <- lowest
  }

  return(path)
}

get.chord.transition.cost <- function(from, to) {

  if(is.na(from) || is.na(to))
    return(NA)

  # TODO: UPNEXT: base this on circle of fifths, only to eliminate noise
}

make.discrete.markov.model <- function(filename = NULL,
                                       discretise = discretise.template.temperlay(),
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

discretise.template.temperlay <- function() {
  return(discretise.template.pitch.class(
         list(c(5, 2, 3.5, 2, 4.5, 4, 2, 4.5, 2, 3.5, 1.5, 4) / 5,
              c(5, 2, 3.5, 4.5, 2, 4, 2, 4.5, 3.5, 2, 1.5, 4) / 5),
         c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b',
           'c-', 'c#-', 'd-', 'd#-', 'e-', 'f-', 'f#-', 'g-', 'g#-', 'a-', 'a#-', 'b-')))
}

discretise.chords <- function() {
  major <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0)
  minor <- c(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  dim   <- c(1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0)
  return(discretise.template.pitch.class(list(major, minor, dim),
         c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b',
           'c-', 'c#-', 'd-', 'd#-', 'e-', 'f-', 'f#-', 'g-', 'g#-', 'a-', 'a#-', 'b-',
           'c/', 'c#/', 'd/', 'd#/', 'e/', 'f/', 'f#/', 'g/', 'g#/', 'a/', 'a#/', 'b/')))
}

discretise.template.flat <- function() {
  return(discretise.template.pitch.class(list(c(1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1))))
}

discretise.template.major.chords <- function() {
  return(discretise.template.pitch.class(list(c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0))))
}

discretise.template.pitch.class <- function(templates, class.names) {
  g.pitch.classes <<- list()
  pcl <- length(templates[[1]])
  template.matrix <- matrix(nrow = pcl, ncol = 0)

  for(template in templates)
    template.matrix <- cbind(template.matrix, matrix(rep(template, pcl), nrow = pcl))

  for(i in 1:(pcl * length(templates))) {
    template.matrix[,i] <- shift(template.matrix[,i], (i - 1))
  }

  return(function(chroma) {
    sums <- colSums(chroma * template.matrix)

    three.max <- sums[sums > rev(sort(sums))[4]]
    three.max.names = unlist(lapply(1:3, function(i) {
      w <- which(three.max[i] == sums)
      return(w[sample(length(w), 1)])
    }))
    names(three.max) <- class.names[three.max.names]
    three.max <- rev(sort(three.max))
    if(!is.na(three.max[1]) && three.max[1] > 1.5)
      print(three.max)

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
                              


chord.names <- c('c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b',
                 'c-', 'c#-', 'd-', 'd#-', 'e-', 'f-', 'f#-', 'g-', 'g#-', 'a-', 'a#-', 'b-',
                 'c/', 'c#/', 'd/', 'd#/', 'e/', 'f/', 'f#/', 'g/', 'g#/', 'a/', 'a#/', 'b/')
