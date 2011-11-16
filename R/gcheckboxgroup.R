##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gcheckboxgroup
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gcheckboxgroup guiWidgetsToolkitRGtk2
##' @S3method .gcheckboxgroup guiWidgetsToolkitRGtk2
.gcheckboxgroup.guiWidgetsToolkitRGtk2 <-  function(toolkit=NULL,
                                                    items, checked = FALSE, horizontal = FALSE,
                                                    use.table=FALSE, handler = NULL,
                                                    action = NULL, container = NULL, ... ) {
  if(use.table)
    GCheckboxGroupTable$new(toolkit, items, checked = checked,
                            handler = handler,action = action,  container = container, ...)
  else
    GCheckboxGroup$new(toolkit,
                       items, checked = checked, horizontal = horizontal,
                       handler = handler, action = action, container = container, ...)
}


## checkbox group class
GCheckboxGroup <- setRefClass("GCheckboxGroup",
                            contains="GWidgetWithItems",
                            methods=list(
                              initialize=function(toolkit,
                                items, checked = FALSE, horizontal = FALSE,
                                 handler = NULL,
                                action = NULL, container = NULL, ... ) {

                                if(horizontal)
                                  block <<- gtkHBox()
                                else
                                  block <<- gtkVBox()
                                widget <<- NULL
                                widgets <<- list()
                                
                                set_items(value=items)
                                set_index(checked)
                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              get_value=function(drop=TRUE, ...) {
                                items <- get_items()
                                items[get_index()]
                              },
                              set_value=function(value, drop=TRUE, ...) {
                                items <- get_items()
                                ind <- pmatch(value, items)
                                set_index(ind)
                              },
                              get_index = function(...) {
                                "Return indices, not logical"
                                which(sapply(widgets, function(i) i$getActive()))
                              },
                              set_index=function(value, ...) {
                                block_observer()
                                if(is.logical(value))
                                  value <- rep(value, length=get_length())
                                if(is.numeric(value)) {
                                  value <- is.element(seq_len(get_length()), value)
                                }
                                mapply(gtkToggleButtonSetActive, object=widgets, is.active=value)
                                unblock_observer()
                                notify_observers(signal="toggled")
                                invisible()
                              },
                              get_items = function(i, ...) {
                                items <- sapply(widgets, function(i) i[[1]]$getLabel())[i]
                                setNames(items, NULL) # drop names
                              },
                              set_items = function(value, i, ...) {
                                ## make widgets
                                widgets <<- sapply(value, gtkCheckButtonNewWithLabel)
                                ## layout widgets
                                sapply(block$getChildren(), gtkContainerRemove, object=block) # remove old
                                sapply(widgets, gtkBoxPackStart, object=block, expand=FALSE, padding=1)
                                ## connec widgets
                                sapply(widgets, gSignalConnect, signal="toggled", f=function(self, widget, event, ...) {
                                  self$notify_observers(signal="toggled", ...)
                                }, data=.self, user.data.first=TRUE)
                                invisible()
                              },
                              get_length = function() {
                                length(widgets)
                              },
                              ## Handler: changed -> clicked
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler("toggled", handler, action=action, ...)
                              }
                              ))


## uses table for checkboxes
GCheckboxGroupTable <-  setRefClass("GCheckboxGroupTable",
                            contains="GWidget",
                            methods=list(
                              initialize=function(toolkit,
                                items, checked = FALSE,
                                handler = NULL,
                                action = NULL, container = NULL, ... ) {

                                widget <<- gtkTreeViewNew(TRUE)
                                widget$SetRulesHint(TRUE)      # shade

                                block <<- gtkScrolledWindowNew()
                                block$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                                block$Add(widget)
                                
                                store <- rGtkDataFrame(make_items())
                                widget$setModel(store)
                                widget$setHeadersVisible(FALSE)
                                
                                ## set up the view columns
                                vc <- gtkTreeViewColumnNew()
                                widget$insertColumn(vc, 0)
                                cr <- gtkCellRendererToggle()
                                vc$PackStart(cr, TRUE)
                                cr['activatable'] <- TRUE                  # needed
                                vc$addAttribute(cr, "active", 1)            
                                item.toggled <- function(tbl, cell, path, data) {
                                  store <- tbl$getModel()
                                  row <- as.numeric(path) + 1
                                  store[row,2] <- !store[row, 2]
                                }
                                gSignalConnect(cr, "toggled", item.toggled, data=widget, user.data.first=TRUE)

                                cr <- gtkCellRendererTextNew()
                                vc <- gtkTreeViewColumnNew()
                                vc$PackStart(cr, TRUE)
                                vc$addAttribute(cr, "text", 0)            
                                widget$insertColumn(vc, 1)

                                ## icons, tooltips???

                                set_items(value=items)
                                set_index(checked)
                                
                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              ## helper function
                              make_items = function(items, icons, tooltips, checked=rep(FALSE, length(items))) {
                                if(missing(items) ||
                                   (is.data.frame(items) && nrow(items) == 0) ||
                                   (length(items) == 0)
                                   ) {
                                  out <- data.frame(items=character(0),
                                                    checked=logical(0),
                                                    icons=character(0),
                                                    tooltips=character(0),
                                                    stringsAsFactors=FALSE)
                                } else if(is.data.frame(items)) {
                                  ## check
                                  out <- items
                                  if(ncol(out) == 1) 
                                    out$checked <- as.logical(rep(checked, length=nrow(items)))
                                  if(ncol(out) == 2)
                                    out$icons <- rep("", nrow(items))
                                  if(ncol(out) == 3)
                                    out$tooltip <- rep("", nrow(items))
                                } else {
                                  ## piece together
                                  items <- as.character(items)
                                  
                                  if(missing(icons))
                                    icons <- ""
                                  icons <- rep(icons, length=length(items))
                                  
                                  if(missing(tooltips))
                                    tooltips <- ""
                                  icons <- rep(tooltips, length=length(items))
                                  
                                  checked <- rep(checked, length=length(items))
                                  
                                  out <- data.frame(items=items, checked=checked, icons=icons, tooltips=tooltips,
                                                    stringsAsFactors=FALSE)
                                }
                                return(out)
                              },
                              get_value=function(drop=TRUE, ...) {
                                get_items(get_index())
                              },
                              set_value=function(value,  drop=TRUE, ...) {
                                ind <- match(value, get_items())
                                ind <- ind[!is.na(ind)]
                                set_index(ind)
                              },
                              get_index = function(...) {
                                store <- widget$getModel()
                                vals <- store[,2, drop=TRUE]
                                which(vals)
                              },
                              set_index=function(value, ...) {
                                if(is.numeric(value)) {
                                  value <- is.element(seq_len(get_length()), value)
                                }
                                store <- widget$getModel()
                                store[,2] <- value
                                ## how to get view of model to update? (toggle signal of cell renderer?)
                              },
                              get_items = function(i, ...) {
                                store <- widget$getModel()
                                items <- store[,1, drop=TRUE]
                                items[i]
                              },
                              set_items = function(value, i, ...) {
                                items <- make_items(value)

                                if(missing(i)) {
                                  ## replace the store
                                  newStore <- rGtkDataFrame(items)
                                  widget$setModel(newStore)
                                } else {
                                  if(is.logical(i))
                                    i <- which(i)
                                  
                                  store[i,] <- items
                                }
                                
                              },
                              get_length = function() {
                                "Number of items to choose from"
                                length(get_items())
                              },
                              ## handlers
                              handler_widget=function() {
                                ## put handler on cell renderer, not widget
                                view_column <- widget$getColumn(0)
                                cell_renderer <- view_column$getCellRenderers()[[1]]
                                cell_renderer
                              },
                              add_handler_changed = function(handler, action=NULL, ...) {
                                add_handler("toggled", handler, action, ...)
                              }
                              ))
