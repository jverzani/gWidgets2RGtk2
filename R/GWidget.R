##' @include GComponent.R
NULL

##' GWidget is the Base class for widget objects
##' @rdname gWidgets2RGtk2-package
GWidget <- setRefClass("GWidget",
                       contains="GComponentObservable",
                       fields=list(
                         coerce_with="ANY" # function
                       )
                       )

                                   
##' GWidgetWithItems is Base class for selection widgets based on a set of items. The key
##' here is the handlers apply to each item, but the handler is
##' assigned to the class member.
##' @rdname gWidgets2RGtk2-package
GWidgetWithItems <- setRefClass("GWidgetWithItems",
                                contains="GWidget",
                                fields=list(
                                  widgets="list"
                                  ),
                                methods=list(
                                  connect_to_toolkit_signal=function(signal,f) {
                                    ## override, done when adding items
                                  }
                                ))


