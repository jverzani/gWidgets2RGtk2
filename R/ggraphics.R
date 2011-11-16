##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::ggraphics
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .ggraphics guiWidgetsToolkitRGtk2
##' @S3method .ggraphics guiWidgetsToolkitRGtk2
.ggraphics.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                               width = dpi*6, height = dpi*6, dpi = 75, ps = 12,    
                                               handler = NULL,action = NULL, container = NULL, ... ) {
  GGraphics$new(toolkit,
                width=width, height=height, dpi=dpi, ps=ps,
                handler = handler,action = action, container = container, ...)
}


## 
GGraphics <- setRefClass("GGraphics",
                         contains="GWidget",
                         fields=list(
                           device_number="numeric",
                           rubber_band="environment"
                           ),
                         methods=list(
                           initialize=function(toolkit=NULL,
                             width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12, 
                             handler=NULL, action=NULL, container=NULL, ...) {
                             
                             
                             widget <<- gtkDrawingAreaNew()
                             asCairoDevice(widget, pointsize=ps)
                             widget$AddEvents(GdkEventMask["all-events-mask"])
                             
                             initFields(block=widget)
                             
#                             if(!is.null(width) & !is.null(height))
#                               set_size(c(width=width, height=height))

                             add_widget_events()
                             add_rubber_band()
                             add_right_mouse_menu()
                             add_to_parent(container, .self, ...)
                             
                             handler_id <<- add_handler_changed(handler, action)
                             
                             callSuper(toolkit)
                           },
                           add_widget_events = function() {
                             "Add events to widget to customize behaviours"
                             gSignalConnect(widget, signal="map-event", f = function(w, e, ...) {
                               if(is.null(widget$GetData(".devnum"))) {
                                 asCairoDevice(widget, pointsize=ps) # turn into cairo device
                                 device_number <<- widget$GetData(".devnum")
                               }
                               return(TRUE)             # don't propogate
                             })
                             gSignalConnect(widget, "button-press-event", f=function(w,...) {
                               dev.set(w$getData(".devnum"))
                               return(FALSE)
                             })
                             gSignalConnect(widget, "destroy-event", f=function(w, ...) {
                               dev.off(w$getData(".devnum"))
                               return(FALSE)
                             })
                             
                             gSignalConnect(widget, "realize", f=function(...) {
                               gdkWindowProcessAllUpdates()
                               while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
                             })
                           },
                           draw_rectangle=function(x0, x, y0, y) {
                             "Draw rectangle"
                             x <- c(x0, x); y <- c(y0, y)
                             x0 <- min(x); x <- max(x)
                             y0 <- min(y); y <- max(y)
                             
                             allocation = widget$allocation ## GetAllocation()
                             a.width <- allocation$allocation$width
                             a.height <- allocation$allocation$height 

                             ## background style
                             gcb <- gdkGCNew(widget$window)
                             gcb$copy(widget["style"]$blackGc)
                             gcb$setRgbFgColor(gdkColorParse("gray50")$color)
                             gcb$setLineAttributes(line.width=1, line.style=GdkLineStyle["solid"],
                                                   cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
                             ## foreground style
                             gc <- gdkGCNew(widget$window)
                             gc$copy(widget["style"]$blackGc)
                             gc$setRgbFgColor(gdkColorParse("black")$color)
                             gc$setRgbBgColor(gdkColorParse("gray50")$color)
                             gc$setLineAttributes(line.width=1, line.style=GdkLineStyle["double-dash"],
                                                  cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
                             gc$setDashes(c(8, 4))

                             ## the entire rectangle to clear
                             rect <- as.GdkRectangle(c(x=0, y=0, width=a.width, height=a.height))
                             widget$setData("lastRect", rect)

                             for (i in 1:2) {
                               ## draw in background color first
                               tmp.gc <- if (i == 1) gcb else gc
                               gdkDrawRectangle(widget$window, gc=tmp.gc, filled=FALSE, x=x0, y=y0, width=x-x0, height=y-y0)
                             }
                             
                             gdkWindowProcessAllUpdates()
                             while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
                           },
                           clear_rectangle=function(){
                             "clear rectangle"
                             last <- widget$getData("lastRect")
                             if(!is.null(last)) 
                               widget$window$invalidateRect(last, FALSE)
                             gdkWindowProcessAllUpdates()
                             while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
                           },
                           add_rubber_band=function() {
                             "add rubber bandings. This is from  the excellent playwith package by Felix Andrews"
                             e <- rubber_band <<- environment()
                             
                             ## add environment and values to da
                             e$dragging <- FALSE
                             e$x0 <- e$y0 <- e$x <- e$y <- 0
                             widget$setData("env", e)

                             ## need to bind drag actions: click, motion, release
                             gSignalConnect(widget, "button-press-event", f=function(w, e) {
                               if(isRightMouseClick(e))
                                 return(FALSE)
                               clear_rectangle()
                               
                               a.width <- widget$getAllocation()$width
                               a.height <- widget$getAllocation()$height


                               env <- rubber_band
                               env$x0 <- env$x <- e$x
                               env$y0 <- env$y <- e$y
                               env$dragging <- TRUE
                               return(FALSE)
                             })
                             
                             gSignalConnect(widget, "motion-notify-event", f=function(w, e) {
                               env <- rubber_band
                               ## are we dragging?
                               if(env$dragging) {
                                 clear_rectangle()
                                 env$x <- e$x
                                 env$y <- e$y
                                 ## did we move enough? 10 pixels say
                                 
                                 if(max(abs(env$x - env$x0), abs(env$y - env$y0)) > 10)
                                   draw_rectangle(env$x0, env$x, env$y0, env$y)
                
                               }
                               return(FALSE)
                             })
            
                             gSignalConnect(widget, "button-release-event", f=function(w, e) {
                               if(isRightMouseClick(e))
                                 return(FALSE)
                               env <- rubber_band
                               ## remove draggin
                               env$dragging <- FALSE
                               clear_rectangle()
                               return(FALSE)
                             })
                           },
                           add_right_mouse_menu = function() {
                             "Add menu to right mouse trigger"
                             l <- list()
                             l$copyAction <- gaction("Copy", "Copy current graph to clipboard", icon="copy",
                                                     handler=function(h, ...) copyToClipboard(obj))
                             l$printAction <- gaction("Save", "Save current graph", icon="save",
                                                      handler=function(h,...) {
                                                        fname <- gfile(gettext("Filename to save to"), type="save")
                                                        if(nchar(fname)) {
                                                          if(!file.exists(fname) || gconfirm(gettext("Overwrite file?")))
                                                            set_value(fname)
                                                        }
                                                      })
                             add_3rd_mouse_popup_menu(l)
                           },
                           get_value=function( ...) {
                             
                           },
                           set_visible=function(value, ...) {
                             "Set as current device, raise"
                             if(value) {
                               devnum <- widget$GetData(".devnum")
                               if(!is.null(devnum))
                                 dev.set(devnum)
                               set_focus(TRUE)
                             }
                           },
                           set_value=function(value, ...) {
                             "Save figure to file specfied by value"
                             XXX("Complete, code is a hack")
                           },
                           drawable_to_ndc = function() {
                             ## convert to normalized device coordinates
                             e <- rubber_band
                             x.pixel <- sort(c(e$x0, e$x))
                             y.pixel <- sort(c(e$y0, e$y))
  
                             allocation = widget$allocation ## GetAllocation()
                             da.w <- allocation$allocation$width
                             da.h <- allocation$allocation$height 
                             
                             ndc <- list(x=x.pixel/da.w, y= 1- rev(y.pixel/da.h))
                             return(ndc)
                           },
          
                           add_handler_changed=function(handler, action=NULL, ...) {
                             "Change handler is called after rubber band selection is updated"
                             if(!is_handler(handler)) return()
                             f <- function(h, w, e, ...) {
                               if(!isFirstMouseClick(e))
                                 return(FALSE)
                               coords <- drawable_to_ndc()
                               h$x <- grconvertX(coords$x, from="ndc", to="user")
                               h$y <- grconvertY(coords$y, from="ndc", to="user")
                               handler(h, w, e, ...)
                               return(FALSE)             # propagate
                             }
                             add_handler("button-release-event", f, action=action)
                           },
                           add_handler_clicked=function(handler, action=NULL, ...) {
                             if(!is_handler(handler)) return()
                              f = function(h, w, e,...) {
                                if(!isFirstMouseClick(e))
                                  return(FALSE)
                                
                                ## changes to allocation storage with newer RGtk2
                                allocation <- w$allocation ## GetAllocation()
                                xclick <- e$GetX()
                                yclick <- e$GetY()
                                width <- allocation$allocation$width
                                height <- allocation$allocation$height 
                                
                                x <- xclick/width
                                y <- (height - yclick)/height
                                
                                ## put into usr coordinates
                                h$x <- grconvertX(x, from="ndc", to="user")
                                h$y <- grconvertY(y, from="ndc", to="user")
                                
                                handler(h, w, e, ...)
                                return(FALSE)
                              }
                              add_handler("button-press-event", f, action=action, ...)
                            }
                           ))

