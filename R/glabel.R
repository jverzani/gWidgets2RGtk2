##' @include GWidget.R
NULL

##' Toolkit label constructor
##'
##' @inheritParams gWidgets2::glabel
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.glabel.guiWidgetsToolkitRGtk2 <- function(toolkit, text="", markup=FALSE, editable=FALSE,
                                           handler=NULL, action=NULL, container=NULL,
                                           ...) {
  GLabel$new(toolkit, text, markup, editable, handler, action, container, ...)
}

##' label class for RGtk2
##'
##' The label class for RGtk2 has some extra methods:
##' \itemize{
##' \item{\code{set_angle} Can be used to set the angle for the text. A value of 90 will rotate ccw to read bottom to top}
##' }
##' @rdname gWidgetsRGtk2-package
GLabel <- setRefClass("GLabel",
                            contains="GWidget",
                            fields=list(
                              markup="ANY",
                              editable="logical",
                              edit_widget = "ANY",
                              state="character"
                              ),
                            methods=list(

                              
                              initialize=function(toolkit=NULL, text, markup=FALSE, editable=FALSE, handler, action, container, ...) {

                                widget <<- gtkLabel()
                                widget$setSelectable(TRUE)
                                if(markup)
                                  widget$setUseMarkup(TRUE)

                                ## we put in an event box to catch events for the handler and editable stuff.
                                ## Likely that should just be done away with, but here it is.
                                block <<- gtkEventBoxNew()
                                block$SetVisibleWindow(FALSE)
                                block$add(widget)

                                initFields(
                                           markup=markup,
                                           editable=editable,
                                           change_signal="button-press-event"
                                           )
                                add_to_parent(container, .self, ...)

                                set_value(text)

                                if(editable) {
                                  ## Set up widget to toggle between
                                  state <<- "label"
                                  edit_widget <<- gtkEntryNew()
                                  gSignalConnect(edit_widget, "activate", function(e) {
                                    show_label_widget()
                                  })
                                  ## event box handler
                                  handler <- function(h, ...) {
                                    if(state == "label") {
                                      show_edit_widget()
                                    } else {
                                      show_label_widget()
                                    }
                                  }
                                }
                                
                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },

                              
                              ## set the value
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                value <- paste(value, collapse="\n")
                                if(markup)
                                  widget$setMarkup(value)
                                else
                                  widget$setLabel(value)
                                ## signal change, not done by widget
                                invoke_change_handler()
                              },
                              ## tricky part is for markup
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                value <- widget$getLabel()
                                 if(markup)
                                   value <- gsub("<[^>]*>","",value)
                                value
                              },

                              ## methods for editing
                              show_edit_widget = function() {
                                edit_widget$setText(get_value())
                                block$remove(widget)
                                block$add(edit_widget)
                                state <<- "edit"
                              },
                              show_label_widget = function() {
                                set_value(edit_widget$getText())
                                block$remove(edit_widget)
                                block$add(widget)
                                state <<- "label"
                              },
                              ## Handler
                              handler_widget = function() block, # put on block,not widget
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_clicked(handler, action=action, ...)
                              },
                              add_handler_clicked=function(handler, action=NULL, ...) {
                                widget$setSelectable(FALSE)
                                add_event_handler("button-press-event", handler, action, ...)
                              },


                              ## secret methods
                              set_angle = function(angle) {
                                "Rotate text by angle degrees ccw"
                                widget$setAngle(as.integer(angle)[1])
                              }
                              ))

