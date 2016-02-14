##' @include GWidget.R
NULL

##' Toolkit XXX constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gcheckbox guiWidgetsToolkitRGtk2
## @export .gcheckbox guiWidgetsToolkitRGtk2
.gcheckbox.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                              text, checked = FALSE, use.togglebutton=FALSE, handler = NULL, action = NULL,
                                              container = NULL, ... ) {
  if(use.togglebutton)
    GToggleButton$new(toolkit,
                      text, checked, handler, action, container, ...)
  else
    GCheckbox$new(toolkit,
                  text, checked, handler, action, container, ...)
}

## Checkbox reference class
GCheckbox <- setRefClass("GCheckbox",
                         contains="GWidget",
                         methods=list(
                           initialize=function(toolkit=NULL,
                             text="", checked = FALSE,  handler = NULL, action = NULL,
                             container = NULL, ... ) {

                               if(!is(widget, "GtkWidget")) {
                                   widget <<- gtkCheckButtonNewWithLabel(text)
                                   widget$setActive(checked)
                                   
                                   initFields(block=widget,
                                              change_signal="toggled"
                                              )
                                   add_to_parent(container, .self, ...)
                                   
                                   handler_id <<- add_handler_changed(handler, action)
                               }
                             callSuper(toolkit)
                           },
                           set_value=function(value, index=TRUE, drop=TRUE, ...) {
                             widget$setActive(value)
                             ## invoke_change_handler() # not needed?
                           },
                           get_value=function(index=TRUE, drop=TRUE, ...) {
                             widget$getActive()
                           },
                           get_items = function(i, j, ..., drop=TRUE) {
                             widget[[1]]$getLabel()
                           },
                           set_items = function(value, i, j, ...) {
                             widget[[1]]$setLabel(value)
                           },
                           set_font = function(value) {
                             ## need to set font on label
                             label_widget <- getWidget(widget)$getChildren()[[1]]
                             set_rgtk2_font(label_widget, value)
                           }
                           ## ,
                           ## add_handler_changed=function(handler, action=NULL, ...) {
                           ##   add_handler("toggled", handler, action=action, ...)
                           ## }
                           ))


## Basic toggle button class
GToggleButton <- setRefClass("GToggleButton",
                             contains="GCheckbox",
                             methods=list(
                               initialize=function(toolkit=NULL,
                                 text, checked = FALSE,  handler = NULL, action = NULL,
                                 container = NULL, ... ) {

                                 widget <<- gtkToggleButton()
                                 ## initialize widget
                                 set_value(checked)
                                 set_items(value=text)
                                 
                                 initFields(
                                            block=widget,
                                            change_signal="toggled"
                                           )
                                 
                                 add_to_parent(container, .self, ...)
                                 
                                 handler_id <<- add_handler_changed(handler, action)
                                 callSuper(toolkit)

                              },
                              get_items = function(i, j, ..., drop=TRUE) {
                                widget$getLabel()
                              },
                              set_items = function(value, i, j, ...) {
                                ## use UseStock if in stock icon
                                widget$setLabel(value[1])
                              }
                              ##  ,
                              ## add_handler_changed=function(handler, action=NULL, ...) {
                              ##   add_handler("toggled", handler, action=action, ...)
                              ## }
                              ))

