source("chromagram.R")
library(magic)

make.discrete.markov.model <- function(filename, discretise = discretise.template.pitch.class(),
                                       values.count = 12) {
  chromagram <- chromagram.from.mp3(filename)

  model <- matrix(0, nrow = values.count, ncol = values.count)
  prev.value <- 0
  for(i in 1:ncol(chromagram)) {
    chroma <- chromagram[,i]
    value <- discretise(chroma)
    print(value)
    if(i > 1) {
      model[prev.value, value] <- model[prev.value, value] + 1
    }
    prev.value <- value
  }

  model <- model / rowSums(model)
  return(model)
}

discretise.template.pitch.class <- function() {
  template.matrix <- matrix(rep(c(1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1), 12), nrow = 12)
  for(i in 1:12) {
    template.matrix[,i] <- shift(template.matrix[,i], (i - 1))
  }
  return(function(chroma) {
    sums <- colSums(chroma * template.matrix)
    return(which(sums == max(sums)))
  })
}
