##' @include GWidget.R
NULL

##' Toolkit XXX constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gspinbutton.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                 from = 0, to = 10, by = 1, value = from, digits = 0,
                                                 handler = NULL,action = NULL, container = NULL, ... ) {
  GSpinButton$new( toolkit, from , to , by, value, digits,
                  handler = handler, action = action, container = container, ...)
}


## spingbutton class
GSpinButton <- setRefClass("GSpinButton",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit,
                                from = 0, to = 10, by = 1, value = from, digits = 0,
                                handler, action, container, ...) {

                                if(digits == 0 &&  as.logical((by %% 1))) # FALSE if integer o/w T
                                  digits <- abs(floor(log(by,10)))
             
                                adjustment <- gtkAdjustmentNew(value=value, lower=from,
                                                               upper=to,step.incr=by)
                                widget <<- gtkSpinButtonNew(adjustment, (to-from)/by, digits=digits)
                                set_value(value)
                                
                                initFields(block=widget)

                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              get_value=function(drop=TRUE, ...) {
                                widget$getValue()
                              },
                              set_value=function(value, drop=TRUE, ...) {
                                widget$setValue(value)
                              },
                              set_items = function(value, i, ...) {
                                ## check that value is a regular sequence
                                if(length(value) <=1) {
                                  message("Can only assign a vector with equal steps, as produced by seq, say")
                                  return()
                                }
                                if(length(value) > 2 &&
                                   !all.equal(diff(diff(value)), rep(0, length(value) - 2))) {
                                  message("Can only assign a vector with equal steps, as produced by seq, say")
                                  return()
                                }
                                ## get current value, increment
                                cur <- get_value()
                                inc <- head(diff(value), n=1)

                                widget$setRange(min(value), max(value))
                                widget$setIncrements(inc, inc) # button 1, button 2
                                set_value(cur)
                              },
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler("value-changed", handler, action=action, ...)
                              }
                              ))

