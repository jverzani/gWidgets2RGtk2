##' @include gtk-misc.R
NULL


## Base classes. These are *not* exported, rather each toolkit implementation needs
## to (mostly) provide these same basic classes:
## GComponent
##   - GWidget
##     - GButton
##     - GLabel
##     - Others matching the constructors
##   -GContainer
##     - GWindow
##     - GGroup
##       - GFrame
##         - GExpandGroup
##     - GLayout
##     - GNotebook
##       - GStacked
##     - GPanedGroup



##' Base Class for widgets and containers
##' 
##' GComponent is a parent class for both GContainer and GWidget and
##' inherits its primary interface from
##' gWidgets2::BasicToolkitInterface.
##' @rdname gWidgets2RGtk2-package
GComponent <- setRefClass("GComponent",
                               contains="BasicToolkitInterface",
                               fields=list(
                                 handler_id="ANY",
                                 .e="environment" # for tag
                                 ),
                               methods=list(
                                 initialize=function(toolkit=guiToolkit(), ...,
                                   expand, fill, anchor, label # should be gobbled prior, but don't want to pass on
                                   ) {
                                   initFields(toolkit=toolkit,
                                              .e=new.env()
                                              )
                                   if(is(handler_id, "uninitializedField"))
                                     handler_id <<- NULL

                                   if(is(default_expand, "uninitializedField"))
                                     default_expand <<- NULL

                                   if(is(default_fill, "uninitializedField"))
                                     default_fill <<- NULL

                                   callSuper(...)
                                 },
                                 show = function() {
                                   cat(sprintf("Object of class %s", class(.self)[1]))
                                 },
                                 ## length
                                 get_length = function(...) {
                                   "Get length of object. Needed for sapply."
                                   1
                                 },
                                 ## visible
                                 get_visible = function() widget$getVisible(),
                                 set_visible = function(value) widget$setVisible(as.logical(value)),
                                 ## focus
                                 get_focus = function() wiget$hasFocus(),
                                 set_focus = function(value) {
                                   "If we can focus, do so, then raise"
                                   if(value) {
                                     if(block$getCanFocus())
                                       block$grabFocus()
                                     block$getWindow()$raise()
                                   }
                                 },
                                 ## enabled 
                                 get_enabled = function() widget$getSensitive(),
                                 set_enabled = function(value) widget$setSensitive(as.logical(value)),
                                 ## tooltip
                                 get_tooltip = function(...) widget$getTooltipText(),
                                 set_tooltip = function(value) widget$setTooltipText(paste(value, collapse="\n")),
                                 ## font
                                 set_font = function(value) {
                                   set_rgtk2_font(getWidget(widget), value)
                                 },
                                 set_rgtk2_font = function(obj, value) {
                                   "Set font on a gtkWidget instance"
                                   if(!is(obj, "GtkWidget"))
                                     stop("Font setting called with object of class", class(obj))
                                   
                                   if(!is.list(value))
                                     value <- sapply(value, identity, simplify=FALSE)
                                   font_desc <- pangoFontDescriptionNew()
                                   for(key in names(value)) {
                                     val <- value[[key]]
                                     switch(key,
                                            "weight"= font_desc$setWeight(PangoWeight[val]),
                                            "style" = font_desc$setStyle(PangoStyle[val]),
                                            "size"  = font_desc$setSize(val * PANGO_SCALE),
                                            "scale" = font_desc$setAbsoluteSize(10 * PangoScale[val] * PANGO_SCALE),
                                            "family" = font_desc$setFamily(val),
                                            "color" = obj$modifyFg(GtkStateType[1], val),
                                            "foreground" = obj$modifyFg(GtkStateType[1], val),
                                            "background" = obj$modifyBg(GtkStateType[1], val),
                                            )
                                   }
                                   obj$modifyFont(font_desc)

                                 },
                                 ## tag
                                 get_attr = function(key) {
                                   if(missing(key))
                                     ls(.e)
                                   else
                                     attr(.e, key)
                                 },
                                 set_attr = function(key, value) {
                                   tmp <- .e
                                   attr(tmp, key) <- value
                                 },
                                 ## still there?
                                 is_extant = function() {
                                   "Is widget still available?"
                                   if(is(block, "<invalid>"))
                                     return(FALSE)
                                   else
                                     TRUE
                                 },
                                 ## size
                                 get_size=function(...) {
                                   alloc <- getBlock(.self)$getAllocation()$allocation
                                   c(width=alloc$width, height=alloc$height)
                                 },
                                 set_size=function(value, ...) {
                                   "Set widget size (size request), value=c(width=-1, height=-1)"
                                   if(is.list(value))
                                     value <- unlist(value) # list is named components
                                   if(length(value) >= 2) {
                                     width <- value[1]; height <- value[2]
                                   } else if(names(value) == "height") {
                                     width <- -1; height <- value
                                   } else {
                                     width <- value; height <- -1
                                   }
                                   getBlock(.self)$SetSizeRequest(width,height)
                                 },
                                 ##
                                 ## Work with containers
                                 ##
                                 set_parent = function(parent) {
                                   "Assign parent to parent property"
                                   parent <<- parent
                                 },
                                 add_to_parent = function(parent, child, expand=NULL, fill=NULL, anchor=NULL, ...) {
                                   "Add a child to parent if it is ia container and non null. Dispatches to add_child method of parent"
                                   
                                   if(missing(parent) || is.null(parent))
                                     return()

                                   ## return here. This is for tcltk compliance
                                   if(is(parent, "GLayout"))
                                     return()
                                   
                                   if(!is(parent, "GContainer") && is.logical(parent) && parent) {
                                     tmp <- gwindow(toolkit=toolkit)
                                     tmp$add_child(child, expand, fill, anchor, ...)
                                     return()
                                   }
                                   if(!is(parent,  "GContainer")) {
                                     message("parent is not a container")
                                     return()
                                   }

                                   parent$add_child(child, expand, fill, anchor, ...)
                                 },
                                 ##
                                 ## Drag and drop
                                 ##
                                 add_drop_source=function(handler, action=NULL, data.type="text", ...) {
                                   "Specify widget is a drag source"
                                  gtkDragSourceSet(handler_widget(),
                                                   start.button.mask=c("button1-mask", "button3-mask"),
                                                   targets=widgetTargetTypes[[data.type]],
                                                   actions="copy")

                                   if(data.type == "text") {
                                     f <- function(h, widget, context, sel, tType, eTime) {
                                       val <- handler(h) # returns text
                                       sel$setText(val) 
                                     }
                                   }
                                   else if(data.type == "object") {
                                     print("Set object")
                                     key <- digest(.self)
                                     f <- function(h, widget, context, sel, tType, eTime) {
                                       val <- handler(h) ## returns an object
                                       .dnd.env[[key]] <- val
                                       sel$setText(key) 
                                     }
                                   }
                                     
                                   
                                   gSignalConnect(handler_widget(), "drag-data-get", f= f,
                                                 data=list(obj=.self, action=action), user.data.first=TRUE)
                                   
                                   if(data.type == "object") {
                                     gSignalConnect(handler_widget(), "drag-end", f=function(key, ...) {
                                       .dnd.env[[key]] <- NULL # clean up
                                     }, data=digest(.self), user.data.first=TRUE)
                                   }
                                 },
                                 add_drop_target=function(handler, action=NULL, ...) {
                                   "Specify that widget is a drop target"
                                   gtkDragDestSet(handler_widget(),
                                                  flags="all", 
                                                  targets=widgetTargetTypes,
                                                  actions="copy")

                                   gSignalConnect(handler_widget(), "drag-data-received",
                                                  function(h, widget, context, x, y, sel, data.type, event.time) {
                                                    target <- context$getTargets()[[3]] # GdkAtom instance
                                                    target <- as.integer(attr(target, "name"))
                                                    ## do different things depending on context
                                                    if(target == TARGET.TYPE.TEXT) {
                                                      h$dropdata <- rawToChar(sel$getText())
                                                    } else if(target == TARGET.TYPE.OBJECT) {
                                                      key <- rawToChar(sel$getText())
                                                      h$dropdata <- .dnd.env[[key]]; 
                                                    }
                                                    ## call handler
                                                    handler(h)
                                                    ## all donw
                                                    gtkDragFinish(context, TRUE, FALSE, time=event.time)
                                                  }, data=list(obj=.self, action=action), user.data.first=TRUE)
                                 },
                                 add_drag_motion=function(handler, action=NULL, ...) {
                                   "Called when motion over widget occurs"
                                   ## avoid add-handler, here we do directly, as above.
                                   gSignalConnect(handler_widget(), "drag-motion", f=function(h, ...) {
                                     handler(h, ...)
                                   }, data=list(obj=.self, action=action), user.data.first=TRUE)
                                 }
                                 
                                 )
                               )

##' GComponentObservable adds the observable interface
GComponentObservable <- setRefClass("GComponentObservable",
                                    fields=list(
                                      change_signal="character", # what signal is default change signal
                                      connected_signals="list"
                                      ),
                                    contains="GComponent",
                                    methods=list(
                                      ## Some decorators for handlers
                                      ## these wrap the handler to satisfy or fill the h object or return value
                                      event_decorator=function(handler) {
                                        "Decorator for basic event"
                                        f <- function(h, ...) {
                                          out <- handler(h, ...)
                                          if(is.atomic(out) && is.logical(out) && out[1])
                                            out[1]
                                          else
                                            FALSE # need logical
                                        }
                                        f
                                      },
                                      key_release_decorator=function(handler) {
                                        f <- function(d, widget, event,...) {
                                          h <- list(obj=d$obj,action=d$action)
                                          h$key <- event$getString() # XXX This is bad -- no locale, ...
                                          state <- event$getState()
                                          if(state == 0)
                                            h$modifier <- NULL
                                          else
                                            h$modifier <- gsub("-mask", "", names(which(state == GdkModifierType)))
                                          handler(h,widget, event,...)
                                        }
                                        event_decorator(f)
                                      },
                                      button_press_decorator = function(handler) {
                                        "Add in position information to 'h' component"
                                        f <- function(h, widget, event, ...) {
                                          ## stuff in some event information
                                          h$x <- event$getX(); h$X <- event$getXRoot()
                                          h$y <- event$getY(); h$Y <- event$getYRoot()
                                          h$state <- gsub("-mask", "", names(which(event$getState() == GdkModifierType)))
                                          h$button <- event$getButton()
                                          handler(h, widget, event, ...)
                                        }
                                        event_decorator(f)
                                      },
                                      ## code for integrating observable interface with RGtk2
                                      handler_widget = function() widget, # allow override for block (e.g., glabel)
                                      is_handler=function(handler) {
                                        "Helper to see if handler is a handler"
                                        !missing(handler) && !is.null(handler) && is.function(handler)
                                      },
                                      ##
                                      ## Adding a handler means to
                                      ## a) create an observer and add an observer for the given signal
                                      ## 
                                      ## b) create a call back which
                                      ## calls the notify observer
                                      ## method when the widget
                                      ## actualy emits the signal
                                      add_handler=function(signal, handler, action=NULL, decorator, emitter) {
                                        "Uses Observable framework for events. Adds observer, then call connect signal method. Override last if done elsewhere"
                                        if(is_handler(handler)) {
                                          if(!missing(decorator))
                                            handler <- decorator(handler)
                                          o <- gWidgets2:::observer(.self, handler, action)
                                          invisible(add_observer(o, signal))
                                          connect_to_toolkit_signal(signal, emitter=emitter)
                                        }
                                      },
                                      add_event_handler=function(handler, action=NULL, ..., decorator) {
                                        add_handler(handler, action=NULL, decorator=.self$event_decorator, ...)
                                      },
                                      

                                      connect_to_toolkit_signal=function(
                                        signal, # which signal (gSignalConnect)
                                        f=function(self, ...) { # notify observer in Gtk callback
                                          self$notify_observers(signal=signal, ...)
                                        },
                                        emitter=.self$handler_widget() # can override here
                                        ) {
                                        "Connect signal of toolkit to notify observer"
                                        ## only connect once
                                        message("connect to toolkit")
                                        if(is.null(connected_signals[[signal, exact=TRUE]]))
                                          gSignalConnect(handler_widget(), signal, f, data=.self, user.data.first=TRUE)
                                        connected_signals[[signal]] <<- TRUE
                                      },
                                      ## initiate a handler (emit signal)
                                      invoke_handler=function(signal, ...) {
                                        "Bypasses gSignalEmit which crashes R for me.
                                        Invoke observers listening to signal"
                                        notify_observers(..., signal=signal)
                                      },
                                      invoke_change_handler=function(...) {
                                        "Generic change handler invoker."
                                        if(!is(change_signal, "uninitializedField") && length(change_signal))
                                          invoke_handler(signal=change_signal, ...)
                                      },
                                      ## block and unblock
                                      block_handlers=function() {
                                        "Block all handlers."
                                        ## default is to block the observers. 
                                        block_observers()
                                      },
                                      block_handler=function(ID) {
                                        "Block a handler by ID"
                                        block_observer(ID)
                                      },
                                      unblock_handlers=function() {
                                        "unblock blocked observer. May need to be called more than once to clear block"
                                        unblock_observers()
                                      },
                                      unblock_handler=function(ID) {
                                        "unblock a handler by ID"
                                        unblock_observer(ID)
                                      },
                                      remove_handlers=function() {
                                        "Remove all observers"
                                        remove_observers()
                                      }, 
                                      remove_handler=function(ID) {
                                        "remove a handler by ID"
                                        remove_observer(ID)
                                      },
                                      
                                      ## basic set of handlers
                                      add_handler_changed=function(handler, action=NULL,...) {
                                        if(!is(change_signal, "uninitializedField") && length(change_signal)) {
                                          add_handler(change_signal, handler, action, ...)
                                        } else {
                                          stop("No change_signal defined for widget")
                                        }
                                      },
                                      ## Defind add_handler_EVENT methods
                                      ## basically passes down to add_handler or add_event_handler as needed
                                      ## by the RGtk2 event we bind the handler to.
                                      ## we have to check is handler is missing or a function when we apply a decorator
                                      add_handler_keystroke=function(handler, action=NULL, ...) {
                                        "Keystroke handler. Defined for all, but might restrict to only gedit, gtext"
                                        add_handler("key-release-event", handler, action, .self$key_release_decorator, ...)
                                      },                                 
                                      add_handler_clicked = function(handler, action=NULL, ...) {
                                        add_handler("clicked", handler, action, ...)
                                      },
                                      add_handler_button_press=function(handler, action=NULL, ...) {
                                        add_handler("button-press-event", handler, action, .self$button_press_decorator, ...)
                                      },
                                      add_handler_focus=function(handler, action=NULL, ...) {
                                        add_handler("focus-in-event", handler, action, .self$event_decorator, ...)
                                      },
                                      add_handler_blur=function(handler, action=NULL, ...) {
                                        add_handler("focus-out-event", handler, action, event_decorator, ...)
                                      },
                                      ## XXX add stibs for others
                                      ##
                                      add_popup_menu = function(menulist, action=NULL, ...) {
                                        if(is(menulist, "list")) 
                                          mb <- gmenu(menulist, popup=TRUE)
                                        else
                                          mb <- menulist
                                        if(!is(mb, "GMenuPopup"))
                                          stop("Pass in popupmenu or list defining one")

                                        f <- function(w, e, ...) {
                                          ## XXX count is wrong! (Fixed count in newest RGtk2)
                                          if(e$button == 1 && e$type == GdkEventType['button-press'] -1L) {
                                            mb$widget$popup(button=e$button, activate.time=e$time)
                                          }
                                          FALSE
                                        }
                                        gSignalConnect(handler_widget(), "button-press-event", f)
                                      },
                                      add_3rd_mouse_popup_menu=function(menulist, action=NULL, ...) {
                                        if(is(menulist, "list")) 
                                          mb <- gmenu(menulist, popup=TRUE)
                                        else
                                          mb <- menulist
                                        print(mb)
                                        if(!is(mb, "GMenuPopup"))
                                          stop("Pass in popupmenu or list defining one")
                                        
                                        f <- function(w, e, ...) {
                                          ## make work wih mac and right mouse!!!
                                          if(isRightMouseClick(e)) {
                                            mb$widget$popup(button=e$button, activate.time=e$time)
                                          }
                                          FALSE
                                        }
                                        gSignalConnect(handler_widget(), "button-press-event", f)
                                      }


                                      ))
