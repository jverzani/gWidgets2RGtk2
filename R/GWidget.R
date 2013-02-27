##' @include GComponent.R
NULL

##' GWidget is the Base class for widget objects
##' @rdname gWidgets2RGtk2-package
GWidget <- setRefClass("GWidget",
                       contains="GComponentObservable",
                       methods=list(
                         initialize=function(..., coerce.with=NULL) {
                           if(is.null(coerce_with) && !is.null(coerce.with))
                             coerce_with <<- coerce.with
                           callSuper(...)
                         }
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
                                  connect_to_toolkit_signal=function(signal,f, emitter) {
                                    ## override, done when adding items
                                  }
                                ))


getWidget.GWidgetWithItems <- function(obj) getWidget(obj$block)


