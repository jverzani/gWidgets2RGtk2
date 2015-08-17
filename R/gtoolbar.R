##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gtoolbar guiWidgetsToolkitRGtk2
##' @S3method .gtoolbar guiWidgetsToolkitRGtk2
.gtoolbar.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                              toolbar.list=list(),
                                              style = c("both","icons","text","both-horiz"),
                                              container = NULL,
                                              ... ) {
  GToolBar$new(toolkit,
               toolbar.list=toolbar.list, style=style,
               container=container ,...)
}


## toolbar class
GToolBar <- setRefClass("GToolBar",
                        contains="GWidget",
                        fields=list(
                          toolbar_list="list"
                          ),
                        methods=list(
                          initialize=function(toolkit=NULL,
                            toolbar.list=list(),
                            style = c("both", "icons", "text", "both-horiz"),
                            container = NULL,
                            ...) {
                            
                            widget <<- gtkToolbar()
                            widget$setStyle(match.arg(style))
                            
                            initFields(block=widget,
                                       toolbar_list=list()
                                       )

                            add_toolbar_items(toolbar.list)

                            if(!is.null(container) && is(container, "GWindow"))
                              add_to_parent(container, .self, ...)
                            
                            callSuper(toolkit)
                          },
                          add_toolbar_items=function(items) {
                            "Map a toolbar list, a named list of gaction items or gsepartor items"
                            ## XXX Odd, doesn't seem to like this style -- doesn't find dispatcher.GAction
                            ## dispatcher <- function(item) UseMethod("dispatcher")
                            ## dispatcher.default <- function(item) add_widget_toolitem(item)
                            ## dispatcher.GAction <- function(item) add_gaction_toolitem(item)
                            ## dispatcher.GSeparator <- function(item) add_gseparator_toolitem(item)
                            ## sapply(items, dispatcher)
                            sapply(items, function(item) {
                              ## do dispatch based on class
                              if(is(item, "GAction"))
                                add_gaction_toolitem(item)
                              else if(is(item, "GSeparator"))
                                add_gseparator_toolitem(item)
                              else
                                add_widget_toolitem(item)
                            })
                            widget$show()
                            toolbar_list <<- gWidgets2:::merge.list(toolbar_list, items)
                          },
                          add_gseparator_toolitem=function(obj) {
                            "Helper to add a separator"
                            item <- gtkSeparatorToolItemNew()
                            widget$insert(item, pos=-1)
                          },
                          add_gaction_toolitem=function(obj) {
                            "Helper to add a gaction item"
                            item <- obj$widget$createToolItem()
                            widget$insert(item, pos=-1)
                          },
                          add_widget_toolitem=function(obj) {
                            "Add a widget to the toolbar"
                            item <- gtkToolItemNew()
                            item$add(getBlock(obj))
                            widget$insert(item, pos=-1)
                          },
                          clear_toolbar=function() {
                            "Clear toolbar items"
                            x <- widget$getChildren()
                            sapply(rev(x), widget$remove)
                            widget$hide()
                          },
                          get_value=function( ...) {
                            toolbar_list
                          },
                          set_value=function(value, ...) {
                            "Clear toolbar, add anew"
                            clear_toolbar()
                            add_toolbar_items(value)
                          }
                          ))

