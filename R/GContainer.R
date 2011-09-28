##' @include GComponent.R
NULL

##' Base class for container objects
GContainer <- setRefClass("GContainer",
                            contains="GComponent",
                            fields=list(
                              children="list"
                              ))

                              
