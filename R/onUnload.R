.onUnload <- function (libpath) {
library.dynam.unload("ELLsae", libpath)
  print("Hello Peter! :-)" )
}