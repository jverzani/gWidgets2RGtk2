##' @include GWidget.R
NULL

## TODO: XXX drophandler -- doubles up!

##' Toolkit gedit constructor
##'
##' @param initial.msg initial.msg
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gedit guiWidgetsToolkitRGtk2
##' @S3method .gedit guiWidgetsToolkitRGtk2
.gedit.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                           text = "", width = 25, coerce.with = NULL, initial.msg=initial.msg,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GEdit$new( toolkit, text = text, width = width, coerce.with = coerce.with, initial.msg=initial.msg,
                    handler = handler,action = action, container = container, ...)
}


##' The GEdit class adds some methods beyond the spec: \code{set_error}, \code{clear_error}, \code{validate_value}
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
                                widget$setWidthChars(width)
                                
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


                                ## hard code drop handler
                                ## otherwise we have problems with doubling up (Can't
                                ## avoid the default call)
#                                add_drop_target(function(h,...) {
#                                  h$obj$set_value("")
#                                  focus(h$obj) <- TRUE
#                                  invoke_change_handler()
#                                })
                                
#                                handler_id <<- add_handler_changed(handler, action)
                                ## change handler on focus out event
                                add_handler_blur(function(...) invoke_change_handler())
                                
                                callSuper(toolkit)
                              },
                              set_value=function(value, index=TRUE, drop=TRUE, ...) {
                                clear_init_txt()
                                widget$selectRegion(0, -1) # select then replace
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
                              set_editable = function(value, j) {
                                widget$setEditable(as.logical(value))
                              },
                              add_drop_target=function(handler, action=NULL, ...) {
                                ## Override default handling here, as we don't know how to bypass
                                ## the default event from happening (should be by gSignalStopEmission)
                                "Specify that widget is a drop target"
                                   gtkDragDestSet(handler_widget(),
                                                  flags="all", 
                                                  targets=widgetTargetTypes,
                                                  actions="copy")

                                ## we connect twice! Once to store current value, once to set
                                gSignalConnect(handler_widget(), "drag-data-received", function(...) {
                                  set_attr('..predropvalue', get_value())
                                }, after=FALSE)
                                      
                                
                                gSignalConnect(handler_widget(), "drag-data-received",
                                               function(h, widget, context, x, y, sel, data.type, event.time) {
                                                 last_time <- .dnd.env[['last.time']]
                                                 print(list("last time",
                                                            last_time=last_time,
                                                            class = class(last_time),
                                                            event.time=event.time))
                                                 
                                                 if(!is.null(last_time) && last_time == event.time) return()
                                                 .dnd.env[['last.time']] <- event.time

                                                 ## our hack
                                                 set_value(get_attr('..predropvalue'))
                                                 set_attr('..predropvalue', "")

                                                 
                                                 target <- context$getTargets()[[3]] # GdkAtom instance
                                                 target <- attr(target, "name")

                                                    ## do different things depending on context
                                                    message("drag data received")
                                                    print(list(context=context,
                                                               x=x,
                                                               y=y,
                                                               sel=sel,
                                                               data.type=data.type,
                                                               event.time=event.time)
                                                          )
                                                    if(target == "TEXT") {
                                                      h$dropdata <- rawToChar(sel$getText())
                                                    } else if(as.integer(target) == TARGET.TYPE.TEXT) {
                                                      h$dropdata <- rawToChar(sel$getText())
                                                    } else if(as.integer(target) == TARGET.TYPE.OBJECT) {
                                                      key <- rawToChar(sel$getText())
                                                      h$dropdata <- .dnd.env[[key]]; 
                                                    }
                                                    handler(h)

                                                    gtkDragFinish(context, TRUE, FALSE, time=event.time)
                                                    ## This fails when dropping onto gedit!!! (gets called twice)
                                                 message("stop emission for insert text")
                                                    try(gSignalStopEmission(widget, "insert-text"), silent=TRUE)
                                                    return(TRUE)
                                                  }, data=list(obj=.self, action=action), after=TRUE, user.data.first=TRUE)
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
                              set_invalid=function(value, msg=NULL) {
                                "Set invalid state with message"
                                if(value)
                                  set_error(msg)
                                else
                                  clear_error()
                                callSuper(value, msg)
                              },
                              set_error = function(msg) {
                                "Add error state and message to widget"
                                widget$setIconFromStock("primary", "gtk-no")
                                if(!missing(msg) && !is.null(msg))
                                  widget$setIconTooltipText("primary", msg)
                              },
                              clear_error = function() {
                                "Clear error message"
                                widget$setIconFromStock("primary", NULL)
                                widget$setIconTooltipText("primary", NULL)
                              }
                              ))

