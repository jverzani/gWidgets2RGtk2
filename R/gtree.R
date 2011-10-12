##' @include GWidget.R
##' @include gtree.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::.gtree
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gtree.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                           offspring = NULL, offspring.data = NULL,
                                           chosen.col = 1, offspring.col=2, icon.col=NULL, tooltip.col=NULL,
                                           multiple = FALSE,
                                           handler = NULL,action = NULL, container = NULL, ... ) {
  GTree$new(toolkit,
            offspring=offspring, offspring.data=offspring.data,
            chosen.col=chosen.col, offspring.col=offspring.col, icon.col=icon.col, tooltip.col=tooltip.col,
            multiple=multiple,
            handler = handler,action = action, container = container, ...)
}


## XXX
GTree <- setRefClass("GTree",
                     contains="GWidget",
                     fields=list(
                       chosen_col="IntegerOrNULL",
                       offspring_col="IntegerOrNULL",
                       icon_col="IntegerOrNULL",
                       tooltip_col="IntegerOrNULL",
                       offspring_data="ANY",
                       offspring="function"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         offspring = NULL, offspring.data = NULL,
                         chosen.col = 1, offspring.col=2, icon.col=NULL, tooltip.col=NULL,
                         multiple = FALSE,
                         handler=NULL, action=NULL, container=NULL, ...) {

                         widget <<- gtkTreeViewNew()
                         block <<- gtkScrolledWindowNew()
                         block$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                         block$add(widget)

                         if(multiple)
                           set_selection_mode("multiple")

                         
                         ## call offspring to get data frame
                         items <- offspring(c(), offspring.data)

                         ## we want column index, not name
                         .character_to_index <- function(val, x) {
                           if(is.character(val)) {
                             if(is.element(val, x))
                               val <- match(val, x)
                             else
                               val <- NULL
                           }
                           if(is.numeric(val))
                             val <- as.integer(val)
                           val
                         }
                         icon.col <- .character_to_index(icon.col, names(items))
                         tooltip.col <- .character_to_index(tooltip.col, names(items))
                         offspring.col <- .character_to_index(offspring.col, names(items))
                         chosen.col <- .character_to_index(chosen.col, names(items))

                         initFields(chosen_col=chosen.col,
                                    offspring_col=offspring.col,
                                    icon_col = icon.col,
                                    tooltip_col=tooltip.col,
                                    offspring_data=offspring.data,
                                    change_signal="changed",
                                    default_expand=TRUE,
                                    default_fill=TRUE,
                                    toolkit=toolkit # needed here for gmenu call later
                                    )

                      
                         ## we add columns for each column in items, but don't display all of them
                         types <- sapply(items, RtoGObjectConversion)
                         model <- gtkTreeStoreNew(types)
                         model <- gtkTreeModelSortNewWithModel(model)
                         widget$setModel(model)

                         make_columns(items)
                         add_child_items(items, NULL)
                         set_names(names(items)[-unlist(list(offspring_col, icon_col, tooltip_col))])

                         ## Main configuration respond to open event, close event by populating 
                         gSignalConnect(widget, "row-expanded", function(view, iter, path, ...) {
                           sorted_model <- widget$getModel()
                           model <- sorted_model$getModel()                           

                           us_path <- sorted_model$convertPathToChildPath(path)
                           iter <- model$getIter(us_path)

                           path <- walk_back_from_iter(iter)
                           children <- offspring(path, offspring.data)
                           add_child_items(children, iter$iter)
                           ## remove errant offspring
                           child_iter <- model$IterChildren(iter$iter)
                           if(child_iter$retval)
                             model$remove(child_iter$iter)
                         })

                         gSignalConnect(widget, "row-collapsed", function(view, iter, path, ...) {
                           ## get unsorted iter from path
                           sorted_model <- widget$getModel()
                           model <- sorted_model$getModel() # non-sorted

                           uspath <- sorted_model$ConvertPathToChildPath(path)
                           iter <- model$GetIter(uspath)$iter
                           ## get children, remove
                           n <- model$IterNChildren(iter)
                           if(n > 1) { ## n=1 gets removed when expanded
                             for(i in 1:(n-1)) {
                               child_iter <- model$IterChildren(iter)
                               if(child_iter$retval)
                                 model$Remove(child_iter$iter)
                             }
                           }

                         })
                         
                         
                         add_to_parent(container, .self, ...)
                         
                         handler_id <<- add_handler_changed(handler, action)
                         
                         callSuper(toolkit)
                       },
                       ## Stolen from gtable -- inheritance
                       set_selection_mode=function(mode=c("none","single","browse", "multiple", "extended")) {
                         "Helper: Set the selection mode"
                         sel_model <- widget$getSelection()
                         sel_model$setMode(GtkSelectionMode[match.arg(mode)])
                       },
                       make_columns=function(items) {
                         "Make new columns, watching out for icons, tooltips, visible"
                         if(!is.null(icon_col)) {
                           widget$InsertColumn(make_icon_column(), 0L)
                         }
                         if(!is.null(tooltip_col)) {
                           ## use column tooltip_col - 1L for a tooltip
                           x <- seq_along(items);
                           if(!is.null(tooltip_col)) {
                             widget$setTooltipColumn(tooltip_col - 1L)
                           }
                         }
                         ## now add columns, one by one skipping ones we don't represent
                         not_these <- unlist(list(icon_col, tooltip_col, offspring_col))
                         these <- setdiff(seq_along(items), not_these)
                         sapply(these, function(col) {
                           treeview_col <- make_treeview_column(items[,col], col - 1L)
                           widget$insertColumn(treeview_col, pos = -1) # at end
                          })
                        },
                        make_icon_column=function() {
                          "Make column for icons"
                          cellrenderer <- gtkCellRendererPixbufNew()
                          view.col <- gtkTreeViewColumnNew()
                          view.col$PackStart(cellrenderer, TRUE)
                          view.col$AddAttribute(cellrenderer, "stock-id", icon_col - 1L)
                          event_box <- gtkEventBox() # need this for consistency
                          label <- gtkLabel()
                          event_box$add(label)
                          view.col$setWidget(event_box)
                          view.col
                        },
                       get_view_columns=function() {
                         "Helper: get non-icon columns to iterate over"
                         columns <- widget$getColumns()
                         if(!is.null(icon_col))
                            columns <- columns[-1]
                         columns
                       }, 
                       ## tree methods
                       add_child_items=function(children, parent.iter=NULL) {
                         model <- widget$getModel()$getModel()

                         if(nrow(children) == 0)
                           return()

                         has_offspring <- children[,offspring_col]
                         
                         ## load row by row, column by column
                         ## we add columns for offspring, ... as these are needed to count
                         for(i in 1:nrow(children)) {
                           iter <- model$Append(parent=parent.iter)
                           ## now write values for each column
                           for(j in 1:ncol(children)) {
                             model$SetValue(iter$iter, column=j-1, children[i,j])
                           }
                           ## add branch?
                           if(has_offspring[i]) {
                             model$Append(parent=iter$iter)
                           }
                         }
                         
                       },
                       walk_back_from_path=function(path) {
                         "Walk the tree back from path"
                         ## assume path is not from sorted store
                         model <- widget$getModel()$getModel()
                         iter <- model$getIter(path)
                         walk_back_from_iter(iter)
                       },
                       walk_back_from_iter=function(iter) {
                         "Walk the tree back from iter"
                         model <- widget$getModel()$getModel()
                         vals <- c()
                         while(iter$retval) {
                           vals <- c(model$getValue(iter$iter, chosen_col -1L)$value, vals)
                           iter <- model$iterParent(iter$iter)
                         }
                         vals
                       },
                           
                       ## main methods
                       get_value=function(i, drop=TRUE,...) {
                         "Return path (by chosen col)"
                         sel_model <- widget$getSelection()
                         selected_rows <- sel_model$getSelectedRows()
                         sel_list <- selected_rows$retval # a list of GtkTreePath objects
                         if(length(sel_list) == 0)
                           return(character(0)) # no selection

                         sorted_model <- widget$getModel()
                         model <- sorted_model$getModel() # non-sorted
                         
                         out <- lapply(sel_list, function(i) {
                           us_path <- sorted_model$ConvertPathToChildPath(i)
                           walk_back_from_path(us_path)
                         })
                         if(!is.null(drop) && drop)
                           out <- lapply(out, tail, n=1)
                         if(length(out) == 1)
                           out <- out[[1]]
                         out
                       },
                       set_value=function(value, ...) {
                         "open path, set via match"

                         
                       },
                       get_index = function(...) {
                         "get path index as integer vector"
                         sel_model <- widget$getSelection()
                         selected_rows <- sel_model$getSelectedRows()
                         sel_list <- selected_rows$retval # a list of GtkTreePath objects
                         if(length(sel_list) == 0)
                           return(integer(0)) # no selection

                         sorted_model <- widget$getModel()
                         model <- sorted_model$getModel() # non-sorted

                         
                         out <- lapply(sel_list, function(i) {
                           us_path <- sorted_model$ConvertPathToChildPath(i)
                           as.numeric(strsplit(us_path$toString(), ":")[[1]]) + 1L
                         })
                         if(length(out) == 1)
                           out <- out[[1]]
                         out
                       },
                       set_index = function(value,...) {
                         "open to specifed index, if possible"
                       },
                       get_items = function(i, j, ..., drop=TRUE) {
                         "XXX"
                       },
                       set_items = function(value, i, j, ...) {
                         "XXX"
                       },
                       get_names=function() {
                         sapply(get_view_columns(), function(col) col$getWidget()$getWidget()$getLabel())
                       },
                       set_names=function(value) {
                         f <- function(col, nm) {
                           label <- col$getWidget()$getChild()
                           label$setLabel(nm)
                         }
                         mapply(f, get_view_columns(), value)
                       },
                       add_handler_changed=function(handler, action=NULL, ...) {
                         add_handler_clicked(handler, action=action, ...)
                       }
                       ))

