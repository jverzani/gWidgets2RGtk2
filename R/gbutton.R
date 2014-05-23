##' @include GWidget.R
NULL


##' toolkit constructor
##' @inheritParams gWidgets2::gbutton
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @seealso The documentation for this is found at \code{\link{gbutton}}.
##' @method .gbutton guiWidgetsToolkitRGtk2
##' @S3method .gbutton guiWidgetsToolkitRGtk2
.gbutton.guiWidgetsToolkitRGtk2 <- function(toolkit, text, handler, action, container, ...) {
  GButton$new(toolkit, text, handler, action, container, ...)
}

##' For RGtk2, the GButton class has the extra reference method
##' \code{set_border}. The \code{border} argument has been deprecated.
##' @rdname gWidgets2RGtk2-package
GButton <- setRefClass("GButton",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit=NULL, text=NULL,  handler, action, container, ...) {
                                
                                widget <<- gtkButton()
                                block <<- gtkEventBox()
                                block$add(widget)
                                toolkit <<- toolkit # otherwise next line fails to find toolkit for dispatch
                                if(!is_empty(text))
                                  set_value(text)


                                
                                initFields(
                                           change_signal="clicked"
                                           )
                                
                                add_to_parent(container, .self, ...)

                                if(is(action, "GAction")) {
                                  #make_action_proxy(action)
                                  gtkaction <- action$widget
                                  ##
                                  widget$setRelatedAction(gtkaction) # connect
                                  widget$setUseActionAppearance(TRUE)
                                  ##
                                  icon <- gtkaction['stock-id']
                                  if(!is.null(icon)) {
                                    image <- gtkaction$createIcon(GtkIconSize[4]) # button size
                                    widget$setImage(image)
                                    widget$image$show()
                                  }
                                  ## tooltip
                                  tip <- gtkaction['tooltip']
                                  if(!is.null(tip))
                                    tooltip(.self) <- tip
                                } else {
                                  handler_id <<- add_handler_changed(handler, action)
                                }
                                callSuper(toolkit)
                              },
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                old_value <- get_value()
                                if(!is_empty(old_value) && !is_empty(value) &&
                                   value == old_value)
                                  return()
                                widget$setLabel(value)
                                set_icon(value)
                                ## signal change, not done by widget
                                invoke_change_handler()
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                widget$getLabel()
                              },
                              set_icon = function(value) {
                                ## Set icon using a stock icon
                                icon <- getStockIconByName(value, toolkit=toolkit)
                                if(!is.null(icon)) {
                                  image <- gtkImageNew()
                                  image$SetFromStock(icon, size="button")
                                  widget$setImage(image)
                                  widget$image$show()
                                }
                              },
                              set_font = function(value) {
                                object <- getWidget(widget)[[1]] # label is first child or something
                                if(is(object, "GtkAlignment"))
                                  object <- object[[1]][[2]] # a real hacke
                                set_rgtk2_font(object, value)
                              },
                              ## Handler: changed -> clicked
                              add_handler_clicked=function(handler, action=NULL, ...) {
                                add_handler_changed(handler, action, ...)
                              },
                              ## Extra methods
                              remove_border=function() {
                                "Remove border by setting relief to none"
                                widget$SetRelief(GtkReliefStyle['none'])
                              }
                              ))


