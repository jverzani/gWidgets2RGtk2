##' @include GWidget.R
NULL

##' Toolkit button constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gbutton.guiWidgetsToolkitRGtk2 <- function(toolkit, text, handler, action, container, ...) {
  GButton$new(toolkit, text, handler, action, container, ...)
}

##' Button class
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


                                
                                initFields(block=widget)
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
                                icon <- getStockIconByName(value, toolkit=toolkit)
                                if(!is.null(icon)) {
                                  image <- gtkImageNew()
                                  image$SetFromStock(icon, size="button")
                                  widget$setImage(image)
                                }
                                widget$setLabel(value)
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                widget$getLabel()
                              },

                              ## font is a real hack
                              set_font = function(obj, value) {
                                object <- getWidget(obj)[[1]] # label is first child or something
                                if(is(object, "GtkAlignment"))
                                  object <- object[[1]][[2]] # a real hacke
                                font(object) <- value
                                
                              },
                              ## Handler: changed -> clicked
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_clicked(handler, action=action, ...)
                              },
                              remove_border=function() {
                                "Remove border by setting relief to none"
                                widget$SetRelief(GtkReliefStyle['none'])
                              }
                              ))


## ##' exported Subclass of GComponent for users to subclass
## ##'
## ##' @exportClasses GButtonRGtk2
## GButtonRGtk2 <- setRefClass("GButtonRGtk2",
##                                contains="GButton")
