##' @include ggraphics.R
NULL

##' toolkit implementation
.ggraphicsnotebook.guiWidgetsToolkitRGtk2 <- function(toolkit, width, height, dpi,  container, ...) {
  GGraphicsNew$new(toolkit, width=width, height=height, dpi=dpi, container=container, ...)
}

## basic subclass
GGraphicsNotebook <- setRefClass("GGraphicsNotebook",
                             contains="GNotebookOfPages",
                             methods=list(
                               page_change_handler=function(page.no) {
                                 "Called when page is changed"
                               },
                               add_toolbar=function(tb_container) {
                                 
                               },
                               get_index_from_page=function(page) {
                                 "get page index in the pages list"
                                 which(sapply(pages, function(i) identical(i, page)))
                               },
                               add_page=function(new_df, name=deparse(substitute(new_df))) {
                                 page <- ggraphics(container=widget, label=sprintf("Plot%s",""), expand=TRUE) ## XXX modify name
                                 pages <<- c(pages, page)
                                 nms <<- c(nms, name)
                                 set_cur_page(length(pages))
                               },
                               add_toolbar=function(tb_container) {
                                 
                               },
                               save_plot=function() {
                                 
                               }
                               ))
