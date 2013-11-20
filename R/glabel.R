##' @include GWidget.R
NULL

##' Toolkit label constructor
##'
##' @inheritParams gWidgets2::glabel
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .glabel guiWidgetsToolkitRGtk2
##' @S3method .glabel guiWidgetsToolkitRGtk2
.glabel.guiWidgetsToolkitRGtk2 <- function(toolkit, text="", markup=FALSE, editable=FALSE,
                                           handler=NULL, action=NULL, container=NULL,
                                           ...) {
  GLabel$new(toolkit, text, markup, editable, handler, action, container, ...)
}

##' label class for RGtk2
##'
##' The label class for RGtk2 has the extra method
##' \code{set_angle} to set the angle for the
##' text. A value of 90 will rotate ccw to read bottom to top
##' @rdname gWidgets2RGtk2-package
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
                                gp <- gtkHBox()
                                block$add(gp)
                                gp$packStart(widget)

                                initFields(
                                           markup=markup,
                                           editable=editable,
                                           change_signal="button-press-event"
                                           )
                                add_to_parent(container, .self, ...)

                                set_value(text)
                                if(editable) {
                                  ## don't confuse mouse users
                                  widget$setSelectable(FALSE)
                                  
                                  ## capture events at block
                                  block$setAboveChild(TRUE)
                                  block$setVisibleWindow(FALSE)
                                  
                                  ## Set up widget to toggle between
                                  state <<- "label"
                                  edit_widget <<- gtkEntryNew()
                                  edit_widget$hide()
                                  gp$packStart(edit_widget)
                                  gSignalConnect(edit_widget, "activate", function(e) {
                                    show_label_widget()
                                  })

                                  handler <- function(...) {
                                    if(state == "label") show_edit_widget() else show_label_widget()
                                    FALSE
                                  }
                                  handler_id <<- gSignalConnect(block, "button-press-event", handler)
                                  
                                }
                                
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
                                edit_widget$grabFocus()
                                edit_widget$setText(get_value())
                                widget$hide()
                                edit_widget$show()
                                state <<- "edit"
                              },
                              show_label_widget = function() {
                                set_value(edit_widget$getText())
                                edit_widget$hide()
                                widget$show()
                                state <<- "label"
                                
                              },
                              ## Handler
                              handler_widget = function() block, # put on block,not widget
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_clicked(handler, action=action, ...)
                              },
                              add_handler_clicked=function(handler, action=NULL, ...) {

                                block$addEvents(GdkEventMask["all-events-mask"])
                                
                                add_handler(block, "button-press-event", event_decorator(handler), action)
                              },


                              ## secret methods
                              set_angle = function(angle) {
                                "Rotate text by angle degrees ccw"
                                widget$setAngle(as.integer(angle)[1])
                              }
                              ))

