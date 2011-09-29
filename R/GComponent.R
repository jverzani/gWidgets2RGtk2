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
##     - GPanedGroup
##     - GStacked


##' Base Class for widgets and containers
##' 
##' GComponent as parent for GContainer and GWidget.
##' Here we place GtkWidget and GtkObject methods. Container methods in GContainer
##' @importClassesFrom gWidgets2 BasicToolkitInterface
GComponent <- setRefClass("GComponent",
                               contains="BasicToolkitInterface",
                               fields=list(
                                 toolkit="ANY",
                                 widget="ANY",
                                 block="ANY",
                                 parent="ANY", # NULL for gwindow, else parent container
                                 handler_id="ANY",
                                 .e="environment"
                                 ),
                               methods=list(
                                 initialize=function(toolkit=guiToolkit(), ...) {
                                   initFields(toolkit=toolkit,
                                              .e=new.env()
                                              )
                                   if(is(handler_id, "unitializedField"))
                                     handler_id <<- NULL
                                   
                                   callSuper(...)
                                 },
                                 get_length = function(...) {
                                   "Get length of object. Needed for sapply."
                                   1
                                 },
                                 get_visible = function() widget$getVisible(),
                                 set_visible = function(value) widget$setVisible(as.logical(value)),
                                 get_enabled = function() widget$getSensitive(),
                                 set_enabled = function(value) widget$setSensitive(as.logical(value)),
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
                                 is_extant = function() {
                                   "Is widget still available?"
                                   if(is(block, "<invalid>"))
                                     return(FALSE)
                                   else
                                     TRUE
                                 },
                                 get_size=function(...) {
                                   alloc <- block$getAllocation()$allocation
                                   c(width=alloc$width, height=alloc$height)
                                 },
                                 set_size=function(value, ...) {
                                   ## Huh??
                                 },

                                 ## Work with containers
                                 set_parent = function(parent) parent <<- parent,
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

                                 
                                 ## RGtk2 handler code
                                 handler_widget = function() widget, # allow override for block (glabel)
                                 add_handler = function(signal, handler, action=NULL, ...) {
                                   if(missing(handler) || is.null(handler))
                                     return(NULL)
                                   ##
                                   gSignalConnect(handler_widget(), signal, handler, user.data.first=TRUE, data=list(obj=.self, action=action))
                                 },
                                 ## for RGtk2, distinction made here
                                 add_event_handler = function(signal, handler, action=NULL, ...) {
                                   if(missing(handler))
                                     return(NULL)
                                   
                                   FUN <- function(h, ...) {
                                     handler(h, ...)
                                     FALSE # need logical
                                   }
                                   gSignalConnect(handler_widget(), signal, FUN, user.data.first=TRUE, data=list(obj=.self, action=action))                                           },
                                 ## typical signal maps
                                 add_handler_clicked = function(handler, action=NULL, ...) {
                                   add_handler("clicked", handler, action, ...)
                                 },
                                 add_handler_focus=function(handler, action=NULL, ...) {
                                   add_event_handler("focus-in-event", handler, action, ...)
                                 },
                                 add_handler_blur=function(handler, action=NULL, ...) {
                                   add_event_handler("focus-out-event", handler, action, ...)
                                 },
                                 keystroke_handler=function(handler, action=NULL, ...) {
                                   f <- function(d, widget, event,...) {
                                     h <- list(obj=d$obj,action=d$action)
                                     h$key <- event$getString() # XXX This is bad -- no locale, ...
                                     state <- event$getState()
                                     if(state == 0)
                                       h$modifier <- NULL
                                     else
                                       h$modifier <- gsub("-mask", "", names(which(state == GdkModifierType)))
                                     handler(h,...)
                                     return(FALSE) # propogate
                                   }
                                   f
                                 },
                                 add_handler_keystroke=function(handler, action=NULL, ...) {
                                   "Keystroke handler. Defined for all, but only gedit, gtext"
                                   if(missing(handler) || is.null(handler))
                                     return()
                                   add_event_handler("key-release-event", .self$keystroke_handler, action, ...)
                                 },                                 
                                 ##
                                 emit_signal=function(signal, ..., detail=NULL) {
                                   "Emit signal, for svalue<- assignments, others"
                                   gSignalEmit(widget, signal, ..., detail)
                                 },
                                 ##
                                 add_popup_menu = function(menulist, action=NULL, ...) {
                                   XXX("Add popup menu code")
                                 },


                                 ## Work with handlers (block, un, remove)
                                 block_handler=function(ID) {
                                   if(missing(ID))
                                     ID <- handler_id
                                   lapply(ID, gSignalHandlerBlock, obj=widget)
                                 },
                                 unblock_handler=function(ID) {
                                   if(missing(ID))
                                     ID <- handler_id
                                   lapply(ID, gSignalHandlerUnblock, obj=widget)
                                 },
                                 remove_handler=function(ID) {
                                   if(missing(ID))
                                     ID <- handler_id
                                   lapply(ID, gSignalHandlerDisconnect, obj=widget)
                                   if(identical(ID, handler_id))
                                     handler_id <<- NULL # zero out if this one
                                 }

                                 )
                               )

##' exported Subclass of GComponent for users to subclass
##'
##' @exportClasses GComponentRGtk2
GComponentRGtk2 <- setRefClass("GComponentRGtk2",
                               contains="GComponent")
