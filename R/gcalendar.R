##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gcalendar
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gcalendar guiWidgetsToolkitRGtk2
##' @S3method .gcalendar guiWidgetsToolkitRGtk2
.gcalendar.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                               text="",
                                               format="%Y-%m-%d",
                                               handler = NULL,action = NULL, container = NULL, ... ) {
  GCalendar$new(toolkit,
                 text=text,
                format=format,
                handler = handler,action = action, container = container, ...)
}
  


## Calendar
GCalendar <- setRefClass("GCalendar",
                         contains="GWidget",
                         fields=list(
                           "format"="character"
                           ),
                         methods=list(
                           initialize=function(toolkit=NULL,
                             text="",
                             format="%Y-%m-%d",
                             handler, action, container, ...) {

                             block <<- gtkHBox()
                             widget <<- gtkEntry()
                             widget$setText(text)
                             button <- gtkButton("Date...")
                             
                             initFields(format=format,
                                        change_signal="activate")

                             block$packStart(widget, expand=TRUE, fill=TRUE)
                             block$packStart(button)

                             calendar_callback <- function(h,...) {
                               ## called when button is clicked
                               ## pop up a calendar, when date selected, copy to entry
                               win <- gtkWindowNew(show=FALSE)
                               cal <- gtkCalendarNew()
                               if(nchar(cur_date <- widget$getText())) {
                                 tmp <- as.numeric(strsplit(cur_date, "-")[[1]])
                                 cal$selectDay(tmp[3])
                                 cal$selectMonth(tmp[2] - 1L, tmp[1])
                               }
                               win$Add(cal)
                               cal$Show();
                               win$Show()

                               cal$AddCallback("day-selected-double-click", function(w,...) {
                                 l <- cal$GetDate()
                                 date_selected <- paste(l$year, l$month+1, l$day,sep="-",collapse="-")
                                 date_selected <- format(as.Date(date_selected,format=format))
                                 set_value(date_selected)
                                 win$Destroy()
                               })
                             }
                             gSignalConnect(button, "clicked", f=calendar_callback)

                             
                             add_to_parent(container, .self, ...)
                             
                             handler_id <<- add_handler_changed(handler, action)
                             
                             callSuper(toolkit)
                           },
                           get_value=function(drop=TRUE, ...) {
                             val <- widget$getText()
                             cur_date <- try(as.Date(val, format=format))
                             if(is.na(cur_date)) 
                               cur_date <- as.Date(NA)
                             if(missing(drop) || is.null(drop) || drop)
                               format(cur_date, format=format)
                             else
                               cur_date
                           },
                           set_value=function(value, ...) {
                             widget$setText(value)
                             invoke_change_handler()
                           }
                           ## ,
                           ## add_handler_changed=function(handler, action=NULL, ...) {
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
                           ##   add_handler(change_signal, f, action=action, ...) 
                           ## }
                           ))

