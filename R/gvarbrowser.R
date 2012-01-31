##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gvarbrowser
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gvarbrowser guiWidgetsToolkitRGtk2
##' @S3method .gvarbrowser guiWidgetsToolkitRGtk2
.gvarbrowser.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                 handler = NULL,action = "summary", container = NULL, ... ) {
  GVarBrowser$new(toolkit,
                  handler = handler,action = action, container = container, ...)
}


## Class for variable browser.
GVarBrowser <- setRefClass("GVarBrowser",
                            contains="GWidget",
                          fields=list(
                             "model"="ANY",
                             "ws_model"="ANY",
                             "icon_classes"="list",
                             "timer"= "ANY"
                             ),
                            methods=list(
                              initialize=function(toolkit=NULL,
                                handler=NULL, action=NULL, container=NULL, ...) {

                                ws_model <<- gWidgets2:::WSWatcherModel$new(toolkit=guiToolkit())
                                o = gWidgets2:::Observer$new(function(self) {self$update_view()}, obj=.self)
                                ws_model$add_observer(o)

                                
                                model <<- gtkTreeStore(c(key="gchararray", summary="gchararray",
                                                        icon="gchararray", background="gchararray",
                                                        digest="gchararray"))

                                widget <<-  gtkTreeViewNew()
                                widget$setModel(model)
                                widget$setRulesHint(TRUE) # shading
                                widget$getSelection()$setMode(GtkSelectionMode["multiple"])
                                
                                block <<- gtkScrolledWindowNew()
                                block$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                                block$add(widget)

                                icon_classes <<- getWithDefault(getOption("gwidgets2:gvarbrowser_classes"),
                                                               gWidgets2:::gvarbrowser_default_classes)
                                
                                add_view_columns()
                                init_model()
                                add_context_menu()
                                ## drop target. Returns object of class ???
                                add_drop_source(handler=function(h,...) {
                                  l <- list(name=svalue(h$obj),
                                            obj=svalue(h$obj, drop=FALSE)
                                            )
                                  class(l) <- c("gvarbrowser_dropdata", class(l))
                                  l
                                }, action=NULL, data.type="object")

                                
                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                ## this gives a problem, not sure why. Try our own timer.
                                ## ws_model$start_timer() # start me up

                                ## Try our oown timer. Need to check in update view the size and slow down if too large
                                timer <<- gtimer(1000, function(...) .self$ws_model$update_state())
                                
                                update_view()

                                
                                callSuper(toolkit)
                              },
                              start_timer=function() timer$start_timer(),
                              stop_timer=function() timer$stop_timer(),
                              adjust_timer=function(ms) {
                                "Adjust interval to size of workspace"
                                if(missing(ms)) {
                                  n <- length(ls(envir=.GlobalEnv))
                                  ms <- 1000 * floor(log(5 + n, 5))
                                }
                                timer$set_interval(ms)
                              },
                              add_view_columns=function() {
                                "Add view columns"
                                view.col <- gtkTreeViewColumnNew()

                                cellrenderer <- gtkCellRendererPixbufNew()
                                view.col$PackStart(cellrenderer, FALSE)
                                view.col$AddAttribute(cellrenderer, "stock-id", 2L)

                                cellrenderer <- gtkCellRendererText()
                                view.col$PackStart(cellrenderer, TRUE)
                                view.col$AddAttribute(cellrenderer, "text", 0L)
                                view.col$AddAttribute(cellrenderer, "font", 3L)

                                view.col$setTitle(gettext("Object"))
                                widget$insertColumn(view.col, pos=-1)
                                
                                
                                ##  now summary
                                view.col <- gtkTreeViewColumnNew()
                                cellrenderer <- gtkCellRendererText()
                                view.col$PackStart(cellrenderer, TRUE)
                                view.col$AddAttribute(cellrenderer, "text", 1L)
                                view.col$setTitle(gettext("Description"))
                                
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
#                                model$setValue(iter$iter, column=2, icon_for_object(x))
                                model$setValue(iter$iter, column=2, stockIconFromObject(toolkit, x))
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
                              update_view=function(...) {
                                "Ugly function to update browser"
                                stop_timer()
                                adjust_timer()
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
                                ##
                                start_timer()
                              },
                              ## These are from gtree. How to share?
                              walk_back_from_path=function(path) {
                                "Walk the tree back from path"
                                if(is.numeric(path)) {
                                  ## create GtkTreePath
                                  tpath <- paste(path - 1L, collapse=":")
                                  path <- gtkTreePathNewFromString(tpath)
                                }
                                stopifnot(is(path, "GtkTreePath"))
                                
                                iter <- model$getIter(path)
                                walk_back_from_iter(iter)
                              },
                              walk_back_from_iter=function(iter) {
                                "Walk the tree back from iter"
                                vals <- c()
                                while(iter$retval) {
                                  vals <- c(model$getValue(iter$iter,0L)$value, vals)
                                  iter <- model$iterParent(iter$iter)
                                }
                                vals[-1] # drop first
                              },
                              
                              get_value=function(drop=TRUE, ...) {
                                "Get selected values as names. A value may be 'name' or 'lst$name1$name2'"
                                out <- get_items(drop=FALSE)
                                drop <- getWithDefault(drop, TRUE) # may be NULL
                                if(drop) {
                                  out <- lapply(out, function(i) paste(i, collapse="$"))
                                  if(length(out) == 1)
                                    out <- out[[1]]
                                } else {
                                  ## return objects, not values
                                  out <- sapply(out, gWidgets2:::get_object_from_string)
                                }
                                out
                              },
                              set_value=function(value, ...) {
                                "Select and open value given."
                              },
                              get_index = function(...) {
                                "Get index of selected value: path, drop first, shift"
                                sel_model <- widget$getSelection()
                                selected_rows <- sel_model$getSelectedRows()
                                sel_list <- selected_rows$retval # a list of GtkTreePath objects
                                if(length(sel_list) == 0)
                                  return(numeric(0)) # no selection
                                ## we need to drop first and add one
                                out <- lapply(sel_list, function(path) {
                                  tmp <- path$toString()
                                  tmp <- as.numeric(strsplit(tmp, ":")[[1]])
                                  tmp <- tmp[-1] # drop first
                                  tmp + 1
                                })
                                if(length(out) == 1) out <- out[[1]]
                                out
                              },
                              get_items = function(i, j, ..., drop=TRUE) {
                                "Return value without $, but as vector. Not sure, why"
                                sel_model <- widget$getSelection()
                                selected_rows <- sel_model$getSelectedRows()
                                sel_list <- selected_rows$retval # a list of GtkTreePath objects
                                if(length(sel_list) == 0)
                                  return(character(0)) # no selection
                                
                                out <- lapply(sel_list, .self$walk_back_from_path)
                                if(drop && length(out) == 1)
                                  out <- out[[1]]
                                out
                              },
                              set_items = function(value, i, j, ...) {
                                
                              },
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler("row-activated", handler, action=action, ...)
                              },
                              ## context menu popup
                              add_context_menu=function() {
                                ## call back
                                on_button_pressed <- function(view, event, data) {
                                  if(gWidgets2RGtk2:::isRightMouseClick(event)) {
                                    ret <- view$getPathAtPos(event$x, event$y)
                                    if(!ret$retval)
                                      return(FALSE)
                                    
                                    path <- ret$path
                                    out <- walk_back_from_path(path)
                                    if(length(out) == 0)
                                      return(FALSE)

                                    nm <- paste(out, collapse="$")
                                    obj <- gWidgets2:::get_object_from_string(out)
                                    ## popup menu
                                    menu <- gtkMenuNew()
                                    menu$append(gtkMenuItemNewWithLabel(gettext(sprintf("Actions for %s:", nm))))
                                    menu$append(gtkSeparatorMenuItem())

                                    ## rm, only if length 1
                                    if(length(out) == 1) {
                                      menuitem <- gtkMenuItemNewWithLabel(gettext("rm"))
                                      gSignalConnect(menuitem, "activate", function(data) {
                                        rm(list=out, envir=.GlobalEnv)
                                      })
                                      menu$append(menuitem)
                                    }
                                    ## view
                                    menuitem <- gtkMenuItemNewWithLabel(gettext("View"))
                                    gSignalConnect(menuitem, "activate", function(data) {
                                      View(obj)
                                    })
                                    menu$append(menuitem)
                                    ## fix?
                                    if(length(out) == 1) {
                                      menuitem <- gtkMenuItemNewWithLabel(gettext("fix"))
                                      gSignalConnect(menuitem, "activate", function(data) {
                                        fix(obj)
                                      })
                                    }
                                    menu$append(menuitem)
                                    
                                    ## popup menu                                    
                                    menu$popup(NULL, NULL, NULL, NULL,
                                               event$button,
                                               event$time)
                                  }
                                  FALSE
                                }
                                ## attach to button-press and popup-menu
                                gSignalConnect(widget, "button-press-event", on_button_pressed)
                                gSignalConnect(widget, "popup-menu", on_button_pressed)
                                
                              }
                              ))

