##' @include GComponent.R
NULL

GNotebookOfPages <- setRefClass("GNotebookOfPages",
                                contains="GComponent",
                                fields=list(
                                  pages="list",
                                  nms="character",
                                  cur_page="numeric"
                                  ),
                                methods=list(
                                  initialize=function(toolkit=NULL,
                                    container=NULL, ...) {
                                    
                                    if(!interactive()) {
                                      return(callSuper(...))
                                    }
                                    
                                    make_ui(container)
                                    initFields(pages=list(),
                                               nms=character(0),
                                               cur_page=0,
                                               default_expand=TRUE,
                                               default_fill=TRUE,
                                               toolkit=toolkit
                                               )

                                    ## set current page when page is changed
                                    widget$add_handler_changed(handler=function(h,...) {
                                      print(h$page.no)
                                      set_cur_page(h$page.no)
                                      page_change_handler(h$page.no)
                                    })

                                add_to_parent(container, .self, ...)


                                    
                                    callSuper(...)
                              },
                             make_ui=function(container) {
                               g <- ggroup(expand=TRUE, horizontal=FALSE)
                               tb_container <- ggroup(cont=g)
                               add_toolbar(tb_container)
                               widget <<- gnotebook(container=g, expand=TRUE, fill=TRUE)
                               block <<- g$block
                             },
                             page_change_handler=function(page.no) {
                               "Called when page is changed"
                             },
                             add_toolbar=function(tb_container) {
                               XXX("Subclass")
                             },
                             get_index_from_page=function(page) {
                               "get page index in the pages list"
                               which(sapply(pages, function(i) identical(i, page)))
                             },
                             add_page=function(...) {
                               XXX("sublass")
                             },
                             remove_page=function(i) {
                               if(!is.numeric(i))
                                 i <- get_index_from_page(i)
                               widget$remove_page_by_index(i) ## remove from notebook
                               pages[[i]] <<- NULL
                               nms <<- nms[[-i]]
                             },
                             set_cur_page=function(i) {
                               "Set current page to page i, a number or a page reference"
                               if(!is.numeric(i))
                                 i <- get_index_from_page(i)
                               cur_page <<- i
                             },
                             get_cur_page=function() {
                               get_page(cur_page)
                             },
                             get_page=function(i) {
                               pages[[i]]
                             },
                             ## These are passthroughs
                             get_value=function( ...) {
                               get_cur_page()$get_value(...)
                             },
                             set_value=function(value, ...) {
                               get_cur_page()$set_value(value, ...)
                             },
                             get_index = function(...) {
                               get_cur_page()$get_index(...)
                             },
                             set_index = function(value,...) {
                               get_cur_page()$set_index(value, ...)
                             },
                             get_items = function(...) {
                               get_cur_page()$get_items(...)
                             },
                             set_items = function(value, ...) {
                               get_cur_page()$set_items(value, ...)
                             }
                             ))

