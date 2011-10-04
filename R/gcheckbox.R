##' @include GWidget.R
NULL

##' Toolkit XXX constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
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

##' Basic check box
GCheckbox <- setRefClass("GCheckbox",
                         contains="GWidget",
                         methods=list(
                           initialize=function(toolkit=NULL,
                             text="", checked = FALSE,  handler = NULL, action = NULL,
                             container = NULL, ... ) {
                             
                             widget <<- gtkCheckButtonNewWithLabel(text)
                             widget$setActive(checked)
                             
                             initFields(block=widget,
                                        change_signal="toggled"
                                        )
                             add_to_parent(container, .self, ...)
                             
                             handler_id <<- add_handler_changed(handler, action)
                             
                             callSuper(toolkit)
                           },
                           set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                widget$setActive(value)
                              },
                           get_value=function(index=TRUE, drop=TRUE, ...) {
                             widget$getActive()
                           },
                           get_items = function(i, j, ..., drop=TRUE) {
                             widget[[1]]$getLabel()
                           },
                           set_items = function(value, i, j, ...) {
                             widget[[1]]$setLabel(value)
                           }
                           ## ,
                           ## add_handler_changed=function(handler, action=NULL, ...) {
                           ##   add_handler("toggled", handler, action=action, ...)
                           ## }
                           ))


##' Basic toggle button
GToggleButton <- setRefClass("GToggleButton",
                             contains="GCheckbox",
                             methods=list(
                               initialize=function(toolkit=NULL,
                                 text, checked = FALSE,  handler = NULL, action = NULL,
                                 container = NULL, ... ) {

                                 widget <<- gtkToggleButtonNewWithLabel(text)
                                 set_items(value=text)

                                 set_value(checked)
                                 
                                 initFields(toolkit=toolkit,
                                            block=widget,
                                            change_signal="toggled"
                                           )
                                 
                                 add_to_parent(container, .self, ...)
                                 
                                 handler_id <<- add_handler_changed(handler, action)
                                 
                                 .self
                              },
                              get_items = function(i, j, ..., drop=TRUE) {
                                widget$getLabel()
                              },
                              set_items = function(i, j, ..., value) {
                                ## use UseStock if in stock icon
                                widget$setLabel(value)
                              }
                              ##  ,
                              ## add_handler_changed=function(handler, action=NULL, ...) {
                              ##   add_handler("toggled", handler, action=action, ...)
                              ## }
                              ))

## ##' exported Subclass of GComponent for users to subclass
## ##'
## ##' @exportClass GCheckBoxRGtk2
## GCheckboxRGtk2 <- setRefClass("GCheckboxRGtk2",
##                                contains="GCheckbox")
