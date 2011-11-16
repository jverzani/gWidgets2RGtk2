##' @include gdf.R
##' @include notebook-widgets.R
NULL


##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gdfnotebook
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gdfnotebook guiWidgetsToolkitRGtk2
##' @S3method .gdfnotebook guiWidgetsToolkitRGtk2
.gdfnotebook.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                 items,
                                                 container = NULL, ... ) {
  GDfNotebook$new(toolkit,
                  items, container = container, ...)
}




GDfNotebook <- setRefClass("GDfNotebook",
                           contains="GNotebookOfPages",
                           methods=list(
                             make_ui=function(container) {
                               g <- ggroup(expand=TRUE, horizontal=FALSE)
                               tb_container <- ggroup(cont=g, spacing=0)
                               add_toolbar(tb_container)
                               widget <<- gnotebook(container=g, expand=TRUE, fill=TRUE)
                               block <<- g$block
                             },
                             add_toolbar=function(tb_container) {
                               gbutton("new", container=tb_container, handler=function(h, ...) {
                                 blank_df <- data.frame(lapply(1:10, function(i) rep("", 100)), stringsAsFactors=FALSE)
                                 names(blank_df) <- sprintf("X%s", 1:10)
                                 add_page(blank_df, "new page")
                               })
                               gbutton("open", container=tb_container, handler=function(h,...) {
                                 ## present data frames in a list
                                 cur_dfs <- Filter(function(x) is.data.frame(get(x, .GlobalEnv)), ls(.GlobalEnv))
                                 if(length(cur_dfs) == 0) {
                                   galert(gettext("No data frames to choose from"), parent=block)
                                   return()
                                 } else if (length(cur_dfs) == 1) {
                                   add_page(get(cur_dfs, .GlobalEnv), cur_dfs)
                                 }
                                 if(length(cur_dfs) >= 2) {
                                   w <- gbasicdialog(gettext("Select a data frame to edit"), parent=block,
                                                     handler=function(h,...) {
                                                       if(length(val <- svalue(tbl))) {
                                                         add_page(get(val, .GlobalEnv), val)
                                                       }
                                                     })
                                   tbl <- gtable(cur_dfs, cont=w)
                                   size(tbl) <- c(300, 300)
                                   visible(w, set=TRUE)
                                 }
                               })
                               gbutton("close", container=tb_container, handler=function(h,...) {
                                 df <- get_cur_page()
                                 if(df$can_undo()) {
                                   if(!gconfirm(gettext("Really close? There are unsaved changes"), parent=block))
                                     return()
                                 }
                                 remove_page(df)
                               })
                               gbutton("save", container=tb_container, handler=function(h,...) {
                                 save_DF()
                               })
                             },
                             add_page=function(new_df, name=deparse(substitute(new_df))) {
                               page <- gdf(new_df, container=widget, label=name, expand=TRUE)
                               pages <<- c(pages, page)
                               nms <<- c(nms, name)
                               set_cur_page(length(pages))
                             },
                             undo = function(...) {
                               get_cur_page()$undo(...)
                             },
                             redo = function(...) {
                               get_cur_page()$undo(...)
                             },
                             save_DF=function() {
                               df <- get_cur_page()
                               df$save_data(nms[get_index_from_page()])
                             }
                             ))

