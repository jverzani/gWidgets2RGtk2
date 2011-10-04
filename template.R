##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.XXX.guiWidgetsToolkitRGtk2 <-  function(toolkit,

                    handler = NULL,action = NULL, container = NULL, ... ) {
  GXXX$new(toolkit,
           
                    handler = handler,action = action, container = container, ...)
}


## XXX
GXXX <- setRefClass("GXXX",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                handler=NULL, action=NULL, container=NULL, ...) {

                                widget <<- XXX

                                initFields(block=widget)

                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              get_value=function( ...) {
                                
                              },
                              set_value=function(value, ...) {
                                
                              },
                              get_index = function(...) {

                              },
                              set_index = function(value,...) {

                              },
                              get_items = function(i, j, ..., drop=TRUE) {

                              },
                              set_items = function(value, i, j, ...) {

                              },
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_clicked(handler, action=action, ...)
                              }
                              ))

