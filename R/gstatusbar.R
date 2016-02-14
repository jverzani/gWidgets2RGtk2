##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gstatusbar guiWidgetsToolkitRGtk2
## @export .gstatusbar guiWidgetsToolkitRGtk2
.gstatusbar.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                text="",
                                                container = NULL, ... ) {
  GStatusBar$new(toolkit,
                 text=text,
                 container = container, ...)
}


GStatusBar <- setRefClass("GStatusBar",
                          contains="GWidget",
                          methods=list(
                            initialize=function(toolkit=NULL,
                              text="", container=NULL, ...) {
                              
                              block <<- gtkStatusbarNew()
                              block$setHasResizeGrip(TRUE)
                              sbl <- block[[1]][[1]]
                              ## use our own label, not statusbars
                              label <- widget <<- gtkLabel()
                              label['xalign'] <- 0.0
                              block[[1]]$remove(block[[1]][[1]])
                              block[[1]]$add(widget)

                              set_value(text)

                              if(!is.null(container))
                                if(!is(container, "GWindow"))
                                  getTopLevel(container)$add_statusbar(.self)
                                else
                                  container$add_statusbar(.self)

                              callSuper(toolkit)
                              },
                              get_value=function( ...) {
                                widget$getLabel()
                              },
                              set_value=function(value, ...) {
                                widget$setLabel(paste(value, collapse=";"))
                              }
                              ))

