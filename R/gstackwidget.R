##' @include gnotebook.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gstackwidget guiWidgetsToolkitRGtk2
## @export .gstackwidget guiWidgetsToolkitRGtk2
.gstackwidget.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                  container = NULL, ... ) {
  GStackWidget$new(toolkit,
                   container = container, ...)
}



GStackWidget <- setRefClass("GStackWidget",
                            contains="GNotebook",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                 container=NULL, ...) {

                                ## To be able to subclass we define widget in separate method
                                if(is(widget, "uninitializedField")) 
                                  make_widget()

                                add_to_parent(container, .self, ...)

                                callSuper(toolkit, container=container)
                              },
                              make_widget = function() {
                                widget <<- gtkNotebookNew()
                                widget$setShowTabs(FALSE)
                                initFields(block=widget)
                              },
                              get_names=function(...) {},
                              set_names=function(...) {},
                              add_child=function(child,  index=NULL,  ...) {
                                "Similar to GNotebook's, but without label and close button code"
                                
                                if(is.null(index))
                                  page_no <- widget$appendPage(getBlock(child))
                                else if(index < 1)
                                  page_no <- widget$prependPage(getBlock(child))
                                else
                                  page_no <- widget$insertPage(getBlock(child), position=index-1L)
                                set_value(page_no + 1L)

                                child_bookkeeping(child)
                              }
                              ))

