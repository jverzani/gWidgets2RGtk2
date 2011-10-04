##' @include GComponent.R
NULL

##' Base class for widget objects
##'
##' This inherits observable interface
GWidget <- setRefClass("GWidget",
                       contains="GComponentObservable",
                       fields=list(
                         coerce_with="ANY" # function
                       )
                       )

                                   
##' Class to hold widget with items where handlers apply to each item
##' 
GWidgetWithItems <- setRefClass("GWidgetWithItems",
                                contains="GWidget",
                                fields=list(
                                  widgets="list"
                                  ),
                                methods=list(
                                  connect_to_toolkit_signal=function(signal) {
                                    ## override, done when adding items
                                  }
                                ))


