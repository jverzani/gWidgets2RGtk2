##' @include GContainer.R
NULL

##' toolkit constructor for gwindow
##'
##' @inheritParams gWidgets2::gwindow
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gwindow guiWidgetsToolkitRGtk2
##' @S3method .gwindow guiWidgetsToolkitRGtk2
.gwindow.guiWidgetsToolkitRGtk2 <- function(toolkit, title, visible=visible, name, width, height, parent, handler, action,  ...) {
  GWindow$new(toolkit, title, visible=visible, name, width, height, parent, handler, action,  ...)
}

## Main class for gwindow instances
GWindow <- setRefClass("GWindow",
                            contains="GContainer",
                            fields=list(
                              menubar_area="ANY",
                              toolbar_area="ANY",
                              infobar_area="ANY",
                              content_area="ANY",
                              statusbar_area="ANY",
                              statusbar_widget="ANY"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, title="",  visible=TRUE, name=NULL, width=NULL, height=NULL,
                                parent=NULL, handler, action, ...) {

                                widget <<- gtkWindow(show=FALSE)
                                set_value(title)
                                if(is.null(width))
                                  width <- 400L
                                if(is.null(height))
                                  height <- as.integer(0.7 * width)
                                widget$setDefaultSize(width, height)


                                if(!is.null(parent)) {
                                  if(inherits(parent, "GComponent")) {
                                    ## a widget
                                    parent_widget <- getWidget(parent)
                                    if(!inherits(parent_widget,"GtkWindow"))
                                      parent_widget <- getGtkWindow(parent_widget)
                                    widget$setTransientFor(parent_widget)
                                    widget$setPosition(GtkWindowPosition["center-on-parent"])
                                    widget$setDestroyWithParent(TRUE)
                                    ## windows fixes
                                    widget$setSkipTaskbarHint(TRUE)
                                    widget$setSkipPagerHint(TRUE)
                                  } else {
                                    ## check that parent is a numeric pair
                                    if(is.numeric(parent) && length(parent) >= 2) {
                                      widget$Move(as.integer(parent[1]),as.integer(parent[2]))
                                    }
                                  }
                                }

                                
                                initFields(toolkit=toolkit, block=NULL,
                                           menubar_area=gtkHBox(),
                                           toolbar_area=gtkHBox(),
                                           infobar_area=gtkInfoBar(show=FALSE),
                                           content_area=gtkHBox(),
                                           statusbar_area=gtkHBox()
                                           )
                                init_infobar()
                                
                                ## add areas to widget. For now we have simple
                                layout_widget()
                                
                                handler_id <<- add_handler_changed(handler, action)

                                if(visible)
                                  widget$show()
                                
                                callSuper(...)
                              },
                              layout_widget = function() {
                                ## we make a stack of widgets, content_area is the key one
                                tbl <- gtkTable(rows=4, columns=1, homogeneous=FALSE)
                                tbl$SetColSpacings(0)
                                tbl$SetRowSpacings(0)
                                tbl$Attach(menubar_area, 0,1,0,1, xoptions=c("expand", "fill"), yoptions = c("shrink"))
                                tbl$Attach(toolbar_area, 0,1,1,2,  xoptions=c("expand", "fill"), yoptions = c("shrink"))
                                tbl$Attach(infobar_area, 0,1,2,3, xoptions=c("expand", "fill"), yoptions = c("shrink"))
                                tbl$Attach(content_area, 0,1,3,4, xoptions=c("expand", "fill"), yoptions=c("expand", "fill"))
                                tbl$Attach(statusbar_area, 0,1,4,5, xoptions=c("expand", "fill"), yoptions = c("shrink"))
                                
                                ## size grip issue if no statusbar
                                ##content_area['border-width'] <<- 13
                                ## XXX status bar is too short for labels

                                widget$add(tbl)
                              },
                              init_infobar=function() {
                                infobar_area$setNoShowAll(TRUE)
                                infobar_area$setMessageType("warning")            
                                infobar_area$addButton(button.text = "gtk-ok",
                                                   response.id = GtkResponseType['ok'])
                                gSignalConnect(infobar_area, "response", 
                                               function(infobar_area, resp.id) infobar_area$hide())
                                gSignalConnect(infobar_area, "response", 
                                               function(infobar_area, resp.id) infobar_area$hide())
                              },
                              ## Widget methods
                              get_value = function(...) widget$getTitle(),
                              set_value = function(value, ...) widget$setTitle(paste(value, collapse=" ")),
                              set_focus = function(value) {
                                if(value)
                                  widget$present()
                              },
                              get_size = function() {
                                theSize <- widget$GetSize()
                                return(unlist(theSize[2:3]))
                              },
                              update_widget=function(...) {
                                widget$setSizeRequest(-1, -1)
                              },
                              is_extant=function() {
                                !is(widget, "<invalid>")
                              },
                              set_icon=function(stock) {
                                iconfile <- .GWidgetsRGtk2Icons$icons[[stock]]
                                if(!is.null(iconfile))
                                  widget$setIconFromFile(iconfile)
                              },
                              ##
                              ## add methods
                              add_child=function(child, ...) {
                                                                
                                if(missing(child) || is.null(child))
                                  return()

                                ## whoa nelly, must check on type of child here -- not just in add method
                                if(is(child, "GMenuBar")) {
                                  add_menubar(child)
                                } else if(is(child, "GToolBar")) {
                                  add_toolbar(child)
                                } else if(is(child, "GStatusBar")) {
                                  add_statusbar(child)
                                } else {
                                  ## clear out old (only one child allowed)
                                  sapply(content_area$getChildren(), content_area$remove)
                                  ## add. Child can be RGtk2Object or GComponent
                                  content_area$packStart(getBlock(child), expand=TRUE, fill=TRUE)
                                }
                                child_bookkeeping(child)
                              },
                              remove_child=function(child) {
                                child$set_parent(NULL)
                                content_area$remove(getBlock(child))
                              },
                              dispose_window = function() {
                                "close window"
                                widget$destroy()
                              },
                              add_menubar=function(child, ...) {
                                menubar_area$packStart(getBlock(child), expand=TRUE, fill=TRUE)
                              },
                              add_toolbar=function(child, ...) {
                                toolbar_area$packStart(getBlock(child), expand=TRUE, fill=TRUE)
                              },
                              add_statusbar=function(child, ...) {
                                statusbar_widget <<- child # RGtk2 object
                                statusbar_area$packStart(getBlock(child), expand=TRUE, fill=TRUE)
                              },
                              ## set infobar message
                              set_infobar=function(msg, ...) {
                                label <- gtkLabel(msg)
                                curChildren <- infobar_area$getContentArea()$getChildren()
                                if(length(curChildren))
                                  sapply(curChildren, infobar_area$getContentArea()$remove)
                                infobar_area$getContentArea()$packStart(label, expand=TRUE, fill=TRUE)
                                infobar_area$show()
                                ## hide after 4 seconds of mouse click
                                timer <- gtimer(4*1000, FUN=function(...) {
                                  if(is_extant())
                                    infobar_area$hide()
                                }, one.shot=TRUE, toolkit=toolkit)
                              },
                              ## set statusbar message
                              set_statusbar=function(msg, ...) {
                                if(!is(statusbar_widget, "uninitializedField"))
                                  statusbar_widget$set_value(msg)
                              },
                              ## clear statusbar message
                              clear_statusbar=function(...) {
                                if(!is(statusbar_widget, "uninitializedField"))
                                  statusbar_widget$set_value("")
                              },

                              ## handlers
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_destroy(handler, action, ...)
                              },
                              add_handler_destroy=function(handler, action=NULL, ...) {
                                "window manager delete event"
                                add_handler("destroy", handler, action=action, ...)
                              },
                              add_handler_unrealize=function(handler, action, ...) {
                                "Intercept window manager delete event"
                                add_event_handler("delete-event", handler, action=action, ...)
                              }
                              ))


                              
