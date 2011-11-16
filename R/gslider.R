##' @include GWidget.R
NULL

##' Toolkit  constructor
##'
##' @inheritParams gWidgets2::gslider
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gslider guiWidgetsToolkitRGtk2
##' @S3method .gslider guiWidgetsToolkitRGtk2
.gslider.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                             from = 0, to = 100, by = 1, value = from, horizontal = TRUE,
                                             handler = NULL, action = NULL, container = NULL, ... ) {
  GSlider$new(toolkit,
              from, to, by, value, horizontal,
              handler,action, container, ...)
}


## glider class
GSlider <- setRefClass("GSlider",
                       contains="GWidget",
                       fields=list(
                         items = "ANY"
                         ),
                       methods=list(
                         initialize=function(toolkit,
                           from, to, by, value, horizontal,
                           handler, action, container, ...) {
                           if(length(from) == 1)
                             x <- seq(from, to, by)
                           else
                             x <- from
                           x <- sort(unique(x))
                           items <<- x
                           
                           if (horizontal)
                             widget <<- gtkHScaleNewWithRange(1L, length(items), 1L)
                           else
                             widget <<- gtkVScaleNewWithRange(1L, length(items), 1L)

                           gSignalConnect(widget, "format-value", function(widget, value, ...) {
                             ## value is index
                             format(items[as.integer(value)], digits=3)
                           })
                           set_value(value[1])
                           
                           initFields(block=widget,
                                      default_expand=TRUE,
                                      default_fill=ifelse(horizontal, "x", "y"),
                                      change_signal="value-changed")
                           
                           add_to_parent(container, .self, ...)

                           handler_id <<- add_handler_changed(handler, action)
                           
                           callSuper(toolkit)
                         },
                         get_value=function(drop=TRUE, ...) {
                           items[get_index()]
                         },
                         set_value=function(value, drop=TRUE, ...) {
                           i <- pmatch(value, items)
                           set_index(i)
                         },
                         get_index = function(...) {
                           widget$getValue()
                         },
                         set_index = function(value,...) {
                           if(!is_empty(value))
                             widget$setValue(value) # widget uses index 1, ..., n
                         },
                         get_items = function(i, ...) {
                           items
                         },
                         set_items = function(value, i, ...) {
                           cur <- get_value()
                           items <<- sort(unique(value))
                           widget$setRange(1, length(value))
                           widget$setIncrements(1L, 1L) # button 1, button 2
                           
                           set_value(cur)
                         }
                         ## ,
                         ## add_handler_changed=function(handler, action=NULL, ...) {
                         ##   add_handler("value-changed", handler, action=action, ...)
                         ## }
                         ))

