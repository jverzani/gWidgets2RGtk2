##' @include GContainer.R
NULL

##' toolkit constructor for gwindow
##'
##' @export
##' @rdname gWidgetsRGtk2-undocumented
.gwindow.guiWidgetsToolkitRGtk2 <- function(toolkit, title, visible=visible, name, width, height, parent, handler, action,  ...) {
  GWindow$new(toolkit, title, visible=visible, name, width, height, parent, handler, action,  ...)
}

##' Main class for gwindow instances
GWindow <- setRefClass("GWindow",
                            contains="GContainer",
                            fields=list(
                              menubar_area="ANY",
                              toolbar_area="ANY",
                              infobar_area="ANY",
                              content_area="ANY",
                              statusbar_area="ANY"
                              ),
                            methods=list(
                              initialize=function(toolkit=NULL, title="",  visible=TRUE, name=NULL, width=NULL, height=NULL,
                                parent=NULL, handler, action, ...) {

                                widget <<- gtkWindow(show=visible)
                                set_value(title)
                                initFields(toolkit=toolkit, block=NULL,
                                           menubar_area=gtkHBox(),
                                           toolbar_area=gtkHBox(),
                                           infobar_area=gtkInfoBar(show=FALSE),
                                           content_area=gtkHBox()
                                           )
                                init_infobar()
                                
                                ## add areas to widget. For now we have simple
                                layout_widget()
                                

                                ## process parent (make transient for, location, ....
                                ## size of widget ...
                                ## handler for window close

                                callSuper(...)
                              },
                              layout_widget = function() {
                                ## we make a stack of widgets, content_area is the key one
                                tbl <- gtkTable(rows=4, columns=1, homogeneous=FALSE)
                                tbl$SetColSpacings(0)
                                tbl$SetRowSpacings(0)
                                tbl$Attach(menubar_area, 0,1,0,1, yoptions = c("fill"))
                                tbl$Attach(toolbar_area, 0,1,1,2, yoptions = c("fill"))
                                tbl$Attach(infobar_area, 0,1,2,3, xoptions=c("shrink", "fill"), yoptions = c("shrink"))
                                tbl$AttachDefaults(content_area, 0,1,3,4)
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
                              ##
                              ## add methods
                              add_child=function(child, ...) {
                                if(missing(child) || is.null(child))
                                  return()
                                ## clear out old (only one child allowed)
                                sapply(content_area$getChildren(), content_area$remove)
                                ## add. Child can be RGtk2Object or GComponent
                                content_area$packStart(getBlock(child), expand=TRUE, fill=TRUE)
                                ## bookkeep if possible
                                if(is(child, "GComponent"))
                                  child$set_parent(.self)
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
                                tbl <- widget[[1]]
                                statusbar_area <<- getWidget(child) # RGtk2 object
                                tbl$Attach(getBlock(child), 0,1,5,6, yoptions = c("fill"))
                              },
                              ## set infobar message
                              set_infobar=function(msg, ...) {
                                label <- gtkLabel(msg)
                                curChildren <- infobar_area$getContentArea()$getChildren()
                                if(length(curChildren))
                                  sapply(curChildren, infobar_area$getContentArea()$remove)
                                infobar_area$getContentArea()$packStart(label, expand=TRUE, fill=TRUE)
                                infobar_area$show()
                              },
                              ## set statusbar message
                              set_statusbar=function(msg, ...) {
                                statusbar_area$push(1L, msg)
                              },
                              ## clear statusbar message
                              clear_statusbar=function(msg, ...) {
                                statusbar_area$push(1L, "") # bypass stack
                              }
                              ))


                              
