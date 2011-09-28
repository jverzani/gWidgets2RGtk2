##' @include GWidget.R
NULL

##' S3 method for gtimer
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gtimer.guiWidgetsToolkitRGtk2 <- function(toolkit, ms, FUN, data=NULL,  one.shot=FALSE, start=TRUE)
  GTimer$new(ms, FUN, one.shot=FALSE, start=TRUE)

##' Timer for gWidgets.
GTimer <- setRefClass("GTimer",
                      fields=list(
                        "oneShot"="logical",
                        "started" = "logical",
                        interval="integer",
                        FUN="ANY",
                        ID = "ANY"
                        ),
                      methods=list(
                        initialize=function(ms, FUN, one.shot=FALSE, start=TRUE) {

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
                                     FUN=f
                                     )
                          
                          if(start) 
                            start_timer()
                          
                          .self
                        },
                        start_timer = function() {
                          if(!started) {
                            ID <<- gTimeoutAdd(interval, FUN, data = NULL)
                          }
                          started <<- TRUE
                        },
                        stop_timer = function() {
                          gSourceRemove(ID)
                          started <<- FALSE
                        }))
