.onLoad <- function(libname, pkgname) {
  if (!exists(".Random.seed")) {
    set.seed(NULL)
  }
}
