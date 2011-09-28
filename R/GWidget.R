##' @include GComponent.R
NULL

##' Base class for widget objects
GWidget <- setRefClass("GWidget",
                       contains="GComponent",
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
                                  add_handler=function(signal, handler, action=NULL) {
                                    "Just adds observer, need to connect widget to call notify_observers"
                                    if(!missing(handler) && is.function(handler)) {
                                      o <- gWidgets2:::observer(.self, handler, action)
                                      invisible(add_observer(o, signal))
                                    }
                                  },
                                  block_handler=function(ID) {
                                    "Block all handlers"
                                    block_observer(ID)
                                  },
                                  unblock_handler=function(ID) {
                                    "unblock all handlers"
                                    unblock_observer(ID)
                                  },
                                  remove_handler=function(ID) {
                                    "remove all handlers"
                                    remove_observer(ID)
                                  }
                       ))


