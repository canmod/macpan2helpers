## We don't really need this.
## However, it makes sense to Depends: on macpan2, since this package
## doesn't make any sense without it, and we have to import *something*
## from the package to make R CMD check happy (otherwise, we only use
## macpan2 functions in examples so far ...

##' @importFrom macpan2
NULL
