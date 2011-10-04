##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gimage.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                         filename = "", dirname = "", stock.id=NULL, size = "",
                                         handler = NULL,action = NULL, container = NULL, ... ) {
  GImage$new( toolkit,
             filename=filename, dirname=dirname, stock.id=stock.id, size=size,
             handler = handler,action = action, container = container, ...)
}


## Main class for gimage
GImage <- setRefClass("GImage",
                      contains="GWidget",
                      fields=list(
                        "image_name"="ANY", # name (filename or stock)
                        "stock_size"="integer"  # size if stock (GtkIconSize value)
                        ),
                      methods=list(
                        initialize=function(toolkit=NULL,
                          filename = "", dirname = "", stock.id=NULL, size = "",
                          handler=NULL, action=NULL, container=NULL, ...) {
                          

                          ## get file or stock
                          stock_size <<- 0L
                          if(!is.null(stock.id)) {
                            size <- switch(toupper(size),
                                           "MENU"= GtkIconSize["menu"],
                                           "SMALL_TOOLBAR"= GtkIconSize["small-toolbar"],
                                           "LARGE_TOOLBAR"= GtkIconSize["large-toolbar"],
                                           "BUTTON"= GtkIconSize["button"],
                                           "DND"= GtkIconSize["dnd"],
                                           "DIALOG"= GtkIconSize["dialog"],
                                           GtkIconSize["menu"])
                            image_name <<- getStockIconByName(stock.id, toolkit=toolkit)
                            stock_size <<- size
                          } else {
                            ## piece together name
                            if(nchar(dirname))
                              filename <- sprintf("%s%s%s", dirname, .Platform$file.sep, filename)
                            if(file.exists(filename)) {
                              image_name <<- filename
                            } else {
                              message(sprintf("File %s is not found", filename))
                              stop()
                            }
                          }

                          
                          widget <<- gtkImageNew()
                          block <<- gtkEventBoxNew()
                          block$setVisibleWindow(FALSE)
                          block$add(widget)

                          set_value(image_name)

                          add_to_parent(container, .self, ...)

                          initFields("change_signal"="button-press-event")
                          handler_id <<- add_handler_changed(handler, action)

                          callSuper(toolkit)
                        },
                        get_value=function( ...) {
                          image_name
                        },
                        set_value=function(value, ...) {
                          if(file.exists(value)) {
                            widget$setFromFile(value)
                          } else {
                            ## assume stock
                            value <- getStockIconByName(value, toolkit=toolkit)
                            widget$setFromStock(value, size=stock_size)
                          }
                          image_name <<- value
                        },
                        handler_widget=function() {
                          block
                        },
                        add_handler_changed=function(handler, action=NULL, ...) {
                          "Change handler is button-press-event"
                          add_handler_button_press(handler, action=action, ...)
                        },
                        add_handler_clicked=function(handler, action=NULL, ...) {
                          add_handler_changed(handler, action=action, ...)
                        }
                        ))

