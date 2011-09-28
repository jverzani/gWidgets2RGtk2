##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gaction.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                             label, tooltip=NULL, icon = NULL, key.accel = NULL,
                                             handler = NULL,action = NULL, parent = NULL, ... ) {
  GAction$new(toolkit,
              label, tooltip=tooltip, icon = icon, key.accel = key.accel,
              handler = handler,action = action, parent = parent, ...)
}


## XXX
GAction <- setRefClass("GAction",
                       contains="GWidget",
                       fields=list(
                         accel_key="ANY"
                         ),
                       methods=list(
                         initialize=function(toolkit=NULL,
                           label="", tooltip=NULL, icon = NULL, key.accel = NULL,
                           handler, action=NULL, parent, ...) {
                           
                           widget <<- gtkAction(name=make.names(label),
                                                label=label,
                                                tooltip=tooltip,
                                                stock.id=icon)
                           
                           initFields(block=widget,
                                      accel_key=key.accel)

                           if(!is.null(parent) && !is.null(handler))
                             add_key_accel(parent, handler)

                           handler_id <<- add_handler_changed(handler, action)
                           
                           callSuper(toolkit)
                         },
                         add_key_accel=function(parent, handler) {
                           ## accel buttons
                           if(!is.null(accel_key) && !is.null(parent)) {
                             toplevel <- getBlock(parent)$toplevel
                             ## mask Shift-1, Control-4 alt-8
                             ## key sprintf("GDK_%s",key)
                             ## flag GtkAccelFlags -- 1
                             if(grepl("^Control", accel_key) ||
                                grepl("^Alt", accel_key) ||
                                grepl("^Shift", accel_key)) {
                               tmp <- strsplit(accel_key, "-")[[1]]
                               modifier <- c(Shift="shift-mask", "Control"="control-mask", Alt="mod1-mask")[tmp[1]]
                               key <- sprintf("GDK_%s", tmp[2])
                             } else {
                               modifier <- "modifier-mask"
                               key <- sprintf("GDK_%s", accel_key)
                             }
                             a <- gtkAccelGroup()
                             toplevel$addAccelGroup(a)
                             a$connect(get(key), modifier, "visible", function(...) {
                               h <- list(action=action)
                               handler(h, ...)
                               TRUE
                             })
                           }
                         },
                         get_value=function( ...) {
                           widget$getLabel()
                         },
                         set_value=function(value, ...) {
                           widget$setLabel(value)
                         },
                         get_tooltip=function(...) {
                           widget['tooltip']
                         },
                         set_tooltip=function(value, ...) {
                           widget$setTooltip(paste(value, "\n"))
                         },
                         add_handler_changed=function(handler, action=NULL, ...) {
                           add_handler("activate", handler, action=action, ...)
                         }
                         ))

