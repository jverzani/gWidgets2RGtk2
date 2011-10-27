##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::.gvarbrowser
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gvarbrowser.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                 handler = NULL,action = "summary", container = NULL, ... ) {
  GVarBrowser$new(toolkit,
                  handler = handler,action = action, container = container, ...)
}


##' Class for variable browser.
GVarBrowser <- setRefClass("GVarBrowser",
                            contains="GWidget",
                           fields=list(
                             "model"="ANY",
                             "ws_model"="ANY",
                             "icon_classes"="list"
                             ),
                            methods=list(
                              initialize=function(toolkit=NULL,
                                handler=NULL, action=NULL, container=NULL, ...) {

                                ws_model <<- gWidgets2:::WSWatcherModel$new(toolkit=guiToolkit())
                                o = gWidgets2:::Observer$new(function(...) {update_browser()}, obj=ws_model)
                                ws_model$add_observer(o)

                                model <<- gtkTreeStore(c(key="gchararray", summary="gchararray",
                                                        icon="gchararray", background="gchararray",
                                                        digest="gchararray"))

                                widget <<-  gtkTreeViewNew()
                                widget$setModel(model)

                                
                                block <<- gtkScrolledWindowNew()
                                block$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                                block$add(widget)

                                icon_classes <<- getWithDefault(getOptions("gwidgets2:gvarbrowser_classes"),
                                                               gWidgets2:::gvarbrowser_default_classes)
                                
                                add_view_columns()
                                
                                initFields(block=widget)

                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              add_view_columns=function() {
                                "Add view columns"
                                view.col <- gtkTreeViewColumnNew()

                                cellrenderer <- gtkCellRendererPixbufNew()
                                view.col$PackStart(cellrenderer, FALSE)
                                view.col$AddAttribute(cellrenderer, "stock-id", 2L)
                                view.col$setTitle(gettext("Object"))

                                cellrenderer <- gtkCellRendererText()
                                view.col$PackStart(cellrenderer, TRUE)
                                view.col$AddAttribute(cellrenderer, "text", 0L)
                                view.col$AddAttribute(cellrenderer, "font", 3L)
                                view.col$setTitle(gettext("Description"))
                                widget$insertColumn(view.col, pos=-1)
                                
                                
                                ##  now summary
                                view.col <- gtkTreeViewColumnNew()
                                cellrenderer <- gtkCellRendererText()
                                view.col$PackStart(cellrenderer, TRUE)
                                view.col$AddAttribute(cellrenderer, "text", 1L)
                                widget$insertColumn(view.col, pos=-1)
                              },
                              init_model=function() {
                                "Put in headings Data, Data sets, ..."
                                for(i in c(names(icon_classes), gettext("Others"))) {
                                  parent_iter <- model$append(NULL)      # toplevel item
                                  model$setValue(parent_iter$iter, column=0, value=i)
                                  model$setValue(parent_iter$iter, column=3L, value="bold")
                                }
                              },
                              add_value=function(x, name, iter) {
                                "Add a row to the model"
                                model$setValue(iter$iter, column=0, name)
                                model$setValue(iter$iter, column=1, short_summary(x))
                                model$setValue(iter$iter, column=2, icon_for(x))
                                model$setValue(iter$iter, column=3L, value="")
                                model$setValue(iter$iter, column=4L, value=digest(x))
                                ## recurse if needed
                                if(is.list(x) && !is.null(attr(x, "names"))) {
                                  lapply(names(x), function(i) {
                                    child_iter <- model$append(iter$iter)
                                    add_value(x[[i]], i, child_iter)
                                  })
                                }
                              },
                              update=function() {
                                "Ugly function to update browser"

                                ## helper function
                                modify_children <- function(out_names, out, parent_iter) {
                                  child_iter <- model$iterChildren(parent_iter$iter)
                                  if(flag <- child_iter$retval) {
                                    ## we have children, so we loop over current ones and check
                                    while(flag) {
                                      key <- model$getValue(child_iter$iter, column=0)$value
                                      remove_key <- replace_key <- FALSE
                                      if(!is.null(key)) {
                                        if(key %in% out_names) {
                                          ## "Alread there did it change?"
                                          dgest <- model$getValue(child_iter$iter, column=4L)$value
                                          if(dgest != digest(get(key, .GlobalEnv))) {
                                            replace_key <- TRUE
                                          } else {
                                          }
                                          out_names <- setdiff(out_names, key)
                                        } else {
                                          remove_key <- TRUE          # remove later
                                        }
                                        ## now insert those between
                                        ind <- out_names < key
                                        if(length(ind) && any(ind)) {
                                          add_nms <- out_names[ind]
                                          out_names <- out_names[out_names > key]
                                          for(j in rev(add_nms)) {
                                            iter <- model$insertBefore(parent=parent_iter$iter, sibling=child_iter$iter)
                                            add_value(out[[j]], name=j,  iter=iter)
                                          }
                                        }
                                        if(replace_key) {
                                          ## add in one, then remove
                                          iter <- model$insertBefore(parent=parent_iter$iter, sibling=child_iter$iter)
                                          add_value(out[[key]], name=key, iter=iter)
                                          remove_key <- TRUE
                                        }
                                        if(remove_key) {
                                          ## not there now, we remove
                                          flag <- model$remove(child_iter$iter)
                                        } else {
                                          flag <- model$iterNext(child_iter$iter)
                                        }        
                                      }
                                    }
                                  }
                                  ## now append what is left
                                  if(length(out_names)) {
                                    for(j in out_names) {
                                      child_iter <- model$append(parent=parent_iter$iter)
                                      add_value(out[[j]], name=j,  iter=child_iter)      
                                    }
                                  }
                                }
                                
                                ## Now loop over icon_classes and modify each child
                                for(i in names(icon_classes)) {
                                  
                                  ## Compute names of objects at this level. (From wsmodel)
                                  klasses <- icon_classes[[i]]
                                  out <- ws_model$get_by_function(function(y)  length(Filter(function(x) is(y, x), klasses) > 0))
                                  out_names <- sort(names(out))
                                  parent_iter <- model$getIterFromString(as.character(match(i, names(icon_classes)) - 1L))
                                  modify_children(out_names, out, parent_iter)
                                }
                                ## now do others
                                klasses <- unlist(icon_classes)
                                out <- ws_model$get_by_function(function(y)  !(length(Filter(function(x) is(y, x), klasses) > 0)))
                                out_names <- sort(names(out))
                                parent_iter <- model$getIterFromString(as.character(length(icon_classes)))
                                modify_children(out_names, out, parent_iter)
                                
                              },
                              get_value=function( ...) {
                                
                              },
                              set_value=function(value, ...) {
                                
                              },
                              get_index = function(...) {

                              },
                              set_index = function(value,...) {

                              },
                              get_items = function(i, j, ..., drop=TRUE) {

                              },
                              set_items = function(value, i, j, ...) {

                              },
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler_clicked(handler, action=action, ...)
                              }
                              ))

