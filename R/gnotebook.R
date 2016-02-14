##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gnotebook guiWidgetsToolkitRGtk2
## @export .gnotebook guiWidgetsToolkitRGtk2
.gnotebook.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                               tab.pos = 3, 
                                               container = NULL, ... ) {
  GNotebook$new(toolkit, tab.pos, 
                    container = container, ...)
}



GNotebook <- setRefClass("GNotebook",
                            contains="GContainer",
                            methods=list(
                              initialize=function(toolkit=NULL, tab.pos=3, 
                                 container=NULL, ...) {
                                
                                ## To be able to subclass we define widget in separate method
                                if(is(widget, "uninitializedField")) 
                                  make_widget(tab.pos)

                                add_to_parent(container, .self, ...)

                                callSuper(toolkit)
                              },
                              make_widget = function(tab.pos) {
                                widget <<- gtkNotebookNew()
                                widget$SetScrollable(TRUE)
                                
            
                                ## tab placement: 1,2,3,4 -> 3,0,2,1
                                types <- c("bottom","left","top","right")
                                tabposition <- GtkPositionType[types]
                                widget$SetTabPos(tabposition[tab.pos])
                                
                                
                                initFields(block=widget)
                              }, 
                              get_value=function( ...) {
                                widget$getCurrentPage() + 1L
                              },
                              set_value=function(value, ...) {
                                nPages <- widget$GetNPages()
                                widget$SetCurrentPage(min(nPages,as.numeric(value)-1))
                              },
                              get_index = function(...) {
                                get_value()
                              },
                              set_index = function(value,...) {
                                set_value(value)
                              },
                              get_names = function(...) {
                                n <- widget$getNPages()
                                if(n == 0)
                                  return(character(0))
                                sapply(seq_len(n), function(i) {
                                  label <- widget$getTabLabel(getBlock(get_items(i, drop=TRUE)))
                                  label[[1]]$getText()
                                })
                              },
                              set_names = function(value, ...) {
                                n <- widget$getNPages()
                                sapply(seq_len(n), function(i) {
                                  label <- widget$getTabLabel(getBlock(get_items(i, drop=TRUE)))
                                  label[[1]]$setText(value[i])
                                })
                                invisible()
                              },
                              get_items = function(i, j, ..., drop=TRUE) {
                                "Return child at ith spot"
                                items <- children[i]
                                if(drop && length(items) == 1)
                                  items[[1]]
                                else
                                  items
                              },
                              get_length = function(...) {
                                "Nmber of pages"
                                widget$GetNPages()
                              },
                              ##
                              make_label = function(child, label, close.button=FALSE, ...) {
                                ## make a label widget, possibly with close buttons, ...
                                hbox <- gtkHBox()
                                l <- gtkLabel(label)
                                hbox$packStart(l, expand=TRUE, fill=TRUE)
                                if(!is.null(close.button) && close.button) {
                                  evb <- gtkEventBox()
                                  evb$addEvents(GdkEventMask["all-events-mask"])
                                  evb$setVisibleWindow(FALSE)
                                  hbox$packEnd(evb)
                                  img <- gtkImageNew()
                                  img$setFromStock("gtk-close", size=GtkIconSize['small-toolbar'])
                                  evb$add(img)
                                  gSignalConnect(evb, "button-press-event", f=function(data, ...) {
                                    data$widget$remove_child(data$child)
                                  }, data=list(widget=.self, child=child), user.data.first=TRUE)
                                }
                                hbox
                              },
                              add_child=function(child, label="", index=NULL, close.button=FALSE, ...) {
                                label_widget <- make_label(child, label, close.button, ...) ## XXX
                                
                                if(is.null(index) || !is.numeric(index))
                                  page_no <- widget$appendPage(getBlock(child), label_widget)
                                else if(index < 1)
                                  page_no <- widget$prependPage(getBlock(child), label_widget)
                                else
                                  page_no <- widget$insertPage(getBlock(child), label_widget, position=index-1L)
                                set_value(page_no + 1)

                                child_bookkeeping(child)
                                
                              },
                              remove_child = function(child) {
                                ## remove from children
                                children <<- Filter(function(i) !identical(i, child), children)
                                ## remove from widget
                                widget$remove(getBlock(child))
                              },
                              remove_page_by_index=function(i) {
                                child <- get_items(i)
                                remove_child(child)
                              },
                              remove_current_page = function() {
                                remove_page_by_index(get_index())
                              },
                                add_tab_icon = function(page, stock.id, handler=NULL, where="left") {
                                    "Add a stock icon to a tab (by index) with optional handler."
                                    child <- widget$getNthPage(page - 1L)
                                    box <- widget$getTabLabel(child)
                                    icon <- gimage(stock.id=stock.id, handler=handler)
                                    evb <- icon$widget$parent
                                    box$packStart(evb)
                                    if (where == "left")
                                        box$reorderChild(evb, 0L)
                                  
                                },
                                add_tab_tooltip = function(page, tooltip) {
                                    "Add a tooltip to a tab (by index)"
                                    child <- widget$getNthPage(page - 1L)
                                    box <- widget$getTabLabel(child)
                                    sapply(box$getChildren(), function(x) x$setTooltipText(tooltip))
                                },
                              ## handlers
                              add_handler_changed=function(handler, action=NULL, ...) {
                                "A tab changed"
                                decorator <- function(FUN) {
                                  force(FUN)
                                  f <- function(self, w, pageref, i, ...) {
                                    FUN(self, page.no= i + 1L)
                                  }
                                  f
                                }
                                add_handler("switch-page", handler, action=action, decorator=decorator, ...)
                              }
                              ))

