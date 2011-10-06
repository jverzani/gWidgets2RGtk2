##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gtoolbar.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                              toolbar.list=list(),
                                              style = c("both", "icons", "text", "both-horiz"),
                                              container = NULL,
                                              ... ) {
  GToolBar$new(toolkit,
               toolbar.list=toolbar,list, style=style,
               container=container ,...)
}


## XXX
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
                            initFields(block=widget,
                                       toolbar_list=list()
                                       )

                            add_toolbar_items(toolbar.list)

                            add_to_parent(container, .self, ...)
                            
                            callSuper(toolkit)
                          },
                          add_toolbar_items=function(items) {
                            "Map a toolbar list, a named list of gaction items or gsepartor items"
                            dispatcher <- function(obj) UseMethod("dispatcher")
                            dispatcher.GAction <- function(obj) add_gaction_item(obj)
                            dispatcher.GSeparator <- function(obj) add_gseparator(obj)
                            sapply(items, dispatcher)
                            toolbar_list <- merge(toolbar_list, items)
                          },
                          add_gseparator=function(obj) {
                            "Helper to add a separator"
                            item <- gtkSeparatorToolItemNew()
                            widget$insert(item, pos=-1)
                          },
                          add_gaction_item=function(obj) {
                            "Helper to add a gaction item"
                            item <- obj$widget$createToolItem()
                            widget$insert(item, pos=-1)
                          },
                          clear_toolbar=function() {
                            "Clear toolbar items"
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

