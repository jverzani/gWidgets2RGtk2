##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gradio
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gradio guiWidgetsToolkitRGtk2
##' @S3method .gradio guiWidgetsToolkitRGtk2
.gradio.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                            items,selected=1, horizontal=FALSE, handler=NULL,
                                            action=NULL, container=NULL, ...
                                            ) {

  GRadio$new(toolkit, items, selected, horizontal,
             handler, action, container, ...)
}


## radio button class
GRadio <- setRefClass("GRadio",
                      contains="GWidgetWithItems",
                      methods=list(
                        initialize=function(toolkit, items, selected, horizontal,
                          handler, action, container, ...) {
                          widget <<- NULL
                          widgets <<- list()
                          if(horizontal)
                            block <<- gtkHBox()
                          else
                            block <<- gtkVBox()
                          
                          set_items(value=items)
                          set_index(selected)
                          
                          add_to_parent(container, .self, ...)
                          
                          handler_id <<- add_handler_changed(handler, action)
                          
                          callSuper(toolkit)
                        },
                        get_value=function(drop=TRUE, ...) {
                          get_items(get_index())
                        },
                        set_value=function(value, drop=TRUE, ...) {
                          set_index(pmatch(value, get_items()))
                        },
                        get_index = function(...) {
                          which(sapply(widgets, gtkToggleButtonGetActive))
                        },
                        set_index = function(value, ...) {
                          widgets[[value[1]]]$setActive(TRUE)
                        },
                        get_items = function(i, ...) {
                          items <- sapply(widgets, gtkButtonGetLabel)
                          items[i]
                        },
                        set_items = function(value, i, ...) {
                          ## make widgets
                          radiogp <- gtkRadioButton(label=value[1])
                          sapply(value[-1], gtkRadioButtonNewWithLabelFromWidget, 
                                      group = radiogp)
                          widgets <<- rev(radiogp$getGroup())
                          ## pack in widgets
                          sapply(block$getChildren(), gtkContainerRemove, object=block) # remove old
                          sapply(widgets, gtkBoxPackStart, object=block, padding=2)
                          
                          ## add handler to each button to call back to observers
                          sapply(widgets, gSignalConnect, signal="toggled", f = function(self, w, ...) {
                            if(w$getActive())
                              self$notify_observers(signal="toggled", ...)
                          }, data=.self, user.data.first=TRUE)
                          invisible()
                        },
                        add_handler_changed=function(handler, action=NULL, ...) {
                          add_handler("toggled", handler, action=action, ...)
                        }
                        ))

