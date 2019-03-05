to_list <- function(x, new_rownames = rownames(x), new_colnames = colnames(x)) {
  UseMethod("to_list", x)
}

to_list.matrix <- function(x, new_rownames = rownames(x), new_colnames = colnames(x)) {
  to_list(as.data.frame(x), new_rownames, new_colnames)
}

to_list.data.frame <- function(x, new_rownames = rownames(x), new_colnames = colnames(x)) {
  rownames(x) <- new_rownames
  colnames(x) <- new_colnames

  Map(f = function(rowname){
    as.list(x[rowname,])
  }, rownames(x))
}

to_list.numeric <- function(x, new_rownames = rownames(x), new_colnames = colnames(x)) {
  n <- names(x)

  Map(f = function(name){
    x[[name]]
  }, n)
}
