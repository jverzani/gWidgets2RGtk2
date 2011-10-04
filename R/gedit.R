##' @include GWidget.R
NULL

##' Toolkit gedit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gedit.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                    text = "", width = 25, coerce.with = NULL, initial.msg=initial.msg,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GEdit$new( toolkit, text = text, width = width, coerce.with = coerce.with, initial.msg=initial.msg,
                    handler = handler,action = action, container = container, ...)
}


##' Single line edit class
##'
##' We add a few methods beyond the spec: set_error, clear_error, validate_value,
GEdit <- setRefClass("GEdit",
                            contains="GWidget",
                            fields=list(
                              init_msg="character",
                              init_msg_flag="logical",
                              completion="ANY",
                              validator="ANY"
                              ),
                            methods=list(
                              initialize=function( toolkit=NULL,
                                text = "", width = 25, coerce.with = NULL,
                                initial.msg="",
                                handler = NULL, action = NULL, container = NULL, ...) {

                                widget <<- gtkEntryNew()
                                initFields(block=widget,
                                           coerce_with=coerce.with,
                                           init_msg=initial.msg,
                                           init_msg_flag=FALSE,
                                           completion=NULL,
                                           validator=NULL,
                                           change_signal="activate"
                                           )

                                ## init msg
                                if(nchar(init_msg) > 0) {
                                  id <- gSignalConnect(widget, "focus-in-event", function(...) {
                                    clear_init_txt()
                                  })
                                  gSignalConnect(widget, "focus-out-event", function(...) {
                                    if(nchar(widget$getText()) == 0) {
                                      set_init_txt()
                                    }
                                  })
                                }
                                ## overwrite?
                                if(nchar(text) > 0)
                                  set_value(text)
                                
                                add_to_parent(container, .self, ...)

                                
                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                clear_init_txt()
                                widget$setText(value)
                                widget$activate() # emit signal
                              },
                              get_value=function(index=TRUE, drop=TRUE, ...) {
                                if(!init_msg_flag)
                                  widget$getText()
                                else
                                  ""
                              },
                              set_init_txt=function() {
                                "set initial text, gray out"
                                widget$modifyText(GtkStateType[1], "gray")
                                widget$setText(init_msg)
                                init_msg_flag <<- TRUE
                              },
                              clear_init_txt=function() {
                                "clear out init text, set back to black"
                                widget$modifyText(GtkStateType[1], NULL) # should restore setting
                                if(init_msg_flag)
                                  widget$setText("")
                                init_msg_flag <<- FALSE
                              },
                              get_items=function(i, j, ..., drop=TRUE) {
                                "i for index"
                                if(is.null(completion))
                                  return(character(0))
                                
                                store <- completion$GetModel()
                                nrows <- dim(store)[1]
                                store[i , ]
                              },
                              set_items=function(value, i, j, ...) {
                                if(is.null(completion)) {
                                  completion <<- gtkEntryCompletionNew()
                                   model <- rGtkDataFrame(data.frame(character(1000),stringsAsFactors=FALSE))
                                  completion$SetModel(model)
                                  completion$SetTextColumn(0)           # Columns count from 0 -- not 1

                                  ## set properties
                                  completion$setInlineCompletion(TRUE)
                                  completion$setInlineSelection(TRUE)

                                  widget$SetCompletion(completion)
                                }

                                store <- widget$GetCompletion()$GetModel()
                                nrows <- dim(store)[1]
                                n <- length(value)
                                if(n > nrows)
                                  values <- values[1:nrows]            # truncate
                                if(missing(i))
                                  i <- 1:n
                                store[i , ] <- value
                              },
                              get_visible = function() {
                                widget$getVisibility()
                              },

                              set_visible = function(value) {
                                widget$setInvisibleChar(42L) # asterisk
                                widget$setVisibility(as.logical(value))
                              },

                              get_editable=function() {
                                "Can we actually edit widget?"
                                widget$getEditable()
                              },
                              set_editable = function(value) {
                                widget$setEditable(as.logical(value))
                              },
                              ## Handler: changed -> clicked
                              ## add_handler_changed = function(handler, action=NULL, ...) {
                              ##   if(missing(handler) || is.null(handler))
                              ##     return()
                              ##   f <- function(h, widget, event, ...) {
                              ##     keyval <- event$GetKeyval()
                              ##     if(keyval == GDK_Return) {
                              ##       handler(h, widget, event, ...)
                              ##       return(TRUE)
                              ##     } else {
                              ##       return(FALSE)
                              ##     }
                              ##   }
                              ##   add_handler("activate", f, action=action, ...)
                              ## },
                             

                              ## Extra methods
                              set_validator = function(FUN) {
                                "Set a function to do the validation"
                                validator <<- FUN
                              },
                              validate_input = function() {
                                "Return logical indicating if input is valid"
                                if(is.null(validator))
                                  TRUE
                                else 
                                  validator(get_value())
                              },
                              set_error = function(msg) {
                                "Add error state and message to widget"
                                widget$setIconFromStock("primary", "gtk-no")
                                if(!missing(msg))
                                  widget$setIconTooltipText("primary", "asdfasf")
                              },
                              clear_error = function() {
                                "Clear error message"
                                widget$setIconFromStock("primary", NULL)
                                widget$setIconTooltipText("primary", NULL)
                              }
                              ))

## ##' exported Subclass for users to subclass
## ##'
## ##' @exportClass GEditRGtk2
## GEditRGtk2 <- setRefClass("GEditRGtk2",
##                                contains="GEdit")
