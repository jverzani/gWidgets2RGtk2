##' @include GWidget.R
NULL


## constructor here
##' Toolkit constructor
##'
##' @inheritParams gWidgets2::.XXX
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gdf.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                         items = NULL, name = deparse(substitute(items)),
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GDf$new(toolkit,
           items=items, name=name,
           handler = handler, action = action, container = container, ...)
}

## We leverage the GtkDfExtra data frame editor below

##' Reference class for data frame editor
##'
##' @importFrom RGtk2Extras gtkDfEdit
GDf <- setRefClass("GDf",
                   contains="GWidget", 
                   methods=list(
                     initialize=function(toolkit, items, name=deparse(substitute(df)),
                       handler=NULL, action=NULL,
                       container=NULL,
                       ...) {

                       block <<- gtkDfEdit(items, name)
                       widget <<- block$getData(".local")$view # .local is gtkDfEdit thing

                       modify_df_edit()

                       
                       add_to_parent(container, .self, ...)
                       
##                       handler_id <<- add_handler_changed(handler, action)
                       
                       callSuper(toolkit)
                     },
                     modify_df_edit=function() {
                       ## Make changes to the dfedit object
                       
                     },
                     ## The basic gWidgets interface functions
                     ##
                     get_value=function( ...) {
                       sels <- get_index()
                       block[sels[[1]], sels[[2]]]
                     },
                     set_value=function(value, ...) {
                       
                     },
                     get_index = function(...) {
                      gtkDfEditGetSelection(block)
                     },
                     set_index = function(value,...) {
                       
                     },
                     get_items = function(i, j, ..., drop=TRUE) {
                       block[i, j, ..., drop=drop]
                     },
                     set_items = function(value, i, j, ...) {
                       
                     },
                     get_length=function() {
                       get_dim()[2]     # no cols
                     },
                     get_dim=function() {
                       block$getDimension()
                     },
                     
                     get_dimnames=function() {
                       "Get the row and column names"
                       list(block$getRowNames(), block$getColumnNames())
                     },
                     set_dimnames=function(value) {
                       
                     },
                     get_names=function() {
                       get_dimnames()[[2]]
                     },
                     add_handler_changed=function(handler, action=NULL, ...) {
                       signal <- "row-changed"
                       if(is_handler(handler)) {
                         o <- gWidgets2:::observer(.self, handler, action)
                         invisible(add_observer(o, signal))
                       }
                       ## now connect to model (not view, hence no call to add__handler
                       model <- widget$getModel()
                       connect_to_toolkit_signal(signal="row-changed", emitter=model)
                     }
                     ))
