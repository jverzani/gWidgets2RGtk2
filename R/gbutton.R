##' @include GWidget.R
NULL

##' Toolkit button constructor
##'
##' @inheritParams gWidgets2::gbutton
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gbutton.guiWidgetsToolkitRGtk2 <- function(toolkit, text, handler, action, container, ...) {
  GButton$new(toolkit, text, handler, action, container, ...)
}

##' Button class
##'
##' For RGtk2, the button class has some extra reference methods:
##' \itemize{
##' \item {\code{remove_border} will remove the border around the button. (The \code{border=FALSE} argument is deprecated.)}
##' }
##' @rdname gWidgets2RGtk2-package
GButton <- setRefClass("GButton",
                            contains="GWidget",
                            fields=list(
                              other = "ANY"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, text=NULL,  handler, action, container, ...) {
                                widget <<- gtkButton()
                                toolkit <<- toolkit # otherwise next line fails to find toolkit for dispatch
                                if(!is_empty(text))
                                  set_value(text)


                                
                                initFields(block=widget,
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
                                icon <- getStockIconByName(value, toolkit=toolkit)
                                if(!is.null(icon)) {
                                  image <- gtkImageNew()
                                  image$SetFromStock(icon, size="button")
                                  widget$setImage(image)
                                }
                                widget$setLabel(value)
                                ## signal change, not done by widget
                                invoke_change_handler()
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                widget$getLabel()
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


## ##' exported Subclass of GComponent for users to subclass
## ##'
## ##' @exportClass GButtonRGtk2
## GButtonRGtk2 <- setRefClass("GButtonRGtk2",
##                                contains="GButton")
