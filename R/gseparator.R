##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gseparator guiWidgetsToolkitRGtk2
## @export .gseparator guiWidgetsToolkitRGtk2
.gseparator.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                         horizontal = TRUE,
                   container = NULL, ... ) {
  GSeparator$new(toolkit, horizontal=horizontal, container = container, ...)
}


GSeparator <- setRefClass("GSeparator",
                          contains="GWidget",
                          methods=list(
                            initialize=function(toolkit,
                              horizontal=TRUE, container=NULL,
                              ...) {
                              
                              if(horizontal)
                                widget <<- gtkHSeparatorNew()
                              else
                                widget <<- gtkVSeparatorNew()
                              
                              initFields(block=widget)
                              add_to_parent(container, .self, ...)
                              
                              callSuper(toolkit)
                            }
                            ))

