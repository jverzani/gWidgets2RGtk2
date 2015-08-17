##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gprogressbar guiWidgetsToolkitRGtk2
##' @S3method .gprogressbar guiWidgetsToolkitRGtk2
.gprogressbar.guiWidgetsToolkitRGtk2 <- function(toolkit, value, container, ...) {
  GProgressBar$new(toolkit, value, container, ...)
}

##' For RGtk2, the Gprogressbar class has the extra reference method
##' \code{set_border}. The \code{border} argument has been deprecated.
##' @rdname gWidgets2RGtk2-package
GProgressBar <- setRefClass("GProgressBar",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit=NULL, value, container, ...) {
                                
                                widget <<- gtkProgressBar()

                                if(!missing(value))
                                  set_value(value)
                                
                                initFields(block=widget)
                                
                                add_to_parent(container, .self, ...)

                                callSuper(toolkit)
                              },
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                if(is.null(value)) {
                                  widget$pulse()
                                } else {
                                  value <- as.numeric(value)
                                  frac <- (value/100) %% 1
                                  widget$setFraction(frac)
                                }
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                as.integer(widget$getFraction() * 100)
                              }
                              ))


