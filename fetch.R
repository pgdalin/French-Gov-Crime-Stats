# fetch function

fetch <- function(p, d, c) {
  i <- grep(p, d[[c]])
  print(d[i,])
}