##' @include GWidget.R
NULL

##' S3 method for gtimer
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gtimer.guiWidgetsToolkitRGtk2 <- function(toolkit, ms, FUN, data=NULL,  one.shot=FALSE, start=TRUE)
  GTimer$new(toolkit, ms, FUN, one.shot=one.shot, start=start)

##' Timer for gWidgets.
GTimer <- setRefClass("GTimer",
                      fields=list(
                        "oneShot"="logical",
                        "started" = "logical",
                        interval="integer",
                        FUN="ANY",
                        FUN_wrapper="ANY",
                        ID = "ANY"
                        ),
                      methods=list(
                        initialize=function(toolkit=guiToolkit(), ms, FUN=function(...) {}, one.shot=FALSE, start=TRUE) {

                          f <- function(...) {
                            FUN(...)
                            if(one.shot) {
                              stop_timer()
                              FALSE
                            } else {
                              TRUE
                            }
                          }

                          
                          initFields(started=FALSE,
                                     interval=as.integer(ms),
                                     oneShot=one.shot,
                                     FUN=FUN,
                                     FUN_wrapper=f
                                     )
                          
                          if(start) 
                            start_timer()
                          
                          callSuper()
                        },
                        ## Main interface for gtimer:
                        set_interval=function(ms) {
                          "Set the interval. Need to stop and start active timer to implement."
                          interval <<- as.integer(ms)
                        },
                        start_timer = function() {
                          "Start the timer"
                          if(!started) {
                            ID <<- gTimeoutAdd(interval, FUN_wrapper, data = NULL)
                          }
                          started <<- TRUE
                        },
                        stop_timer = function() {
                          "stop the timer"
                          gSourceRemove(ID)
                          started <<- FALSE
                        }))
