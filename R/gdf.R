##' @include GWidget.R
##' @include gmenu.R
##' @include dialogs.R
##' @include gtable.R
NULL

## TODO
## * handlers
## * column drag and drop
## * size override for passing in column sizes through a list.


##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gdf guiWidgetsToolkitRGtk2
## @export .gdf guiWidgetsToolkitRGtk2
.gdf.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                         items = NULL,
                    handler = NULL,action = NULL, container = NULL, ... ) {
  GDf$new(toolkit,
           items=items, 
           handler = handler, action = action, container = container, ...)
}



##' S3 generic to ensure we don't change data type when assigning into column
##'
##' @param x column values
##' @param value new value
##' @return coerced new value
ensure_type <- function(x, value) UseMethod("ensure_type")
ensure_type.default <- function(x, value) value
ensure_type.character <- function(x, value) as.character(value)
ensure_type.factor <- function(x, value) {x[length(x) + 1] <- value; utils::tail(x, n=1)}
ensure_type.numeric <- function(x, value) as.numeric(value)
ensure_type.integer <- function(x, value) as.integer(value)
ensure_type.logical <- function(x, value) as.logical(value)


my_logical <- function(x) {
    y <- as.character(x)
    y[is.na(y)] <- "NA"
    y <- factor(y)
    class(y) <- c("mylogical", "factor")
    y
}

my_date <- function(x) {
    y <- format(x)
    y[is.na(y)] <- "NA"
    class(y) <- c("mydate", "character")
    y
}

##' make a view column for the given type of variable
##'
##' @param x variable
##' @param self reference to ModelView object
##' @param model_idx 1-based index in model of data to represent
##' @param view_col optional view column to reuse
##' @return a gtkTreeViewColumn with data "n", and "id" to block editable id
add_editable_cell_renderer <- function(x, self, model_idx, view_col)  {

  ## Make column, set editable
  if(missing(view_col))
    view_col <- gtkTreeViewColumnNew()
  else
    view_col$clear()

  make_editable_cell_renderer(x, self, model_idx, view_col)
  
  event_box <- gtkEventBox()
  event_box$SetVisibleWindow(FALSE)
  label <- gtkLabel()
  event_box$add(label)
  event_box$setAboveChild(TRUE)         # gets events to box
  view_col$setWidget(event_box)
  

  view_col
}

## Make cell renderer for above. Dispatches on x
make_editable_cell_renderer <- function(x, self, model_idx, view_col) UseMethod("make_editable_cell_renderer")
make_editable_cell_renderer.default <- function(x, self, model_idx, view_col) {
  cr <- gtkCellRendererText()
  cr['editable'] <- TRUE
  cr$setData("editable", "editable")
  cr$setData("view_col", view_col)      # no lookup otherwise
  view_col$setResizable(TRUE)
  view_col$packStart(cr, TRUE)
  view_col$addAttribute(cr, "text", model_idx - 1L)
  ## make editable
  id <- gSignalConnect(cr,
                       signal="edited",
                       f=function(cr, path, newtext) {
                         visible <- self$get_visible()
                         i <- which(visible)[as.numeric(path) + 1]

                         view_col <- cr$getData("view_col")
                         j <- self$get_column_index(view_col)

                         self$cmd_set_cell(i, j, newtext)
                       })
  view_col$setData("n", model_idx)      # map to model
  view_col$setData("edit.id", id)       # use this to disable editing
}


make_editable_cell_renderer.factor <- function(x, self, model_idx, view_col) {
  cr <- gtkCellRendererCombo()
  model <- rGtkDataFrame(levels(x))
  cr['model'] <- model
  cr['text-column'] <- 0
  cr['editable'] <- TRUE
  cr$setData("editable", "editable")
  cr$setData("view_col", view_col)      # no lookup otherwise
  view_col$setResizable(TRUE)
  view_col$packStart(cr, TRUE)
  view_col$addAttribute(cr, "text", model_idx - 1L)
  ## make editable
  id <- gSignalConnect(cr,
                       signal="edited",
                       f=function(combo, path, newdata) {
                         visible <- self$get_visible()
                         i <- which(visible)[as.numeric(path) + 1]

                         view_col <- cr$getData("view_col")
                         j <- self$get_column_index(view_col)

                         self$cmd_set_cell(i, j, newdata) # toggle
                       })
  view_col$setData("n", model_idx)      # map to model
  view_col$setData("edit.id", id)       # use this to disable editing
}



make_editable_cell_renderer.logical <- function(x, self, model_idx, view_col) {
 
  cr <- gtkCellRendererToggle()
  cr['activatable'] <- TRUE
  cr$setData("editable", "activatable")
  cr$setData("view_col", view_col)      # no lookup otherwise
  
  view_col$setResizable(TRUE)
  view_col$packStart(cr, TRUE)
  view_col$addAttribute(cr, "active", model_idx - 1L)

  ## make editable
  id <- gSignalConnect(cr,
                       signal="toggled",
                       f=function(cr, path, data) {
                         visible <- self$get_visible()
                         i <- which(visible)[as.numeric(path) + 1]

                         view_col <- cr$getData("view_col")
                         j <- self$get_column_index(view_col)

                         self$cmd_set_cell(i, j, !self$get_cell(i, j)) # toggle
                       })
  view_col$setData("n", model_idx)      # map to model
  view_col$setData("edit.id", id)       # use this to disable editing

}



make_editable_cell_renderer.Date <- function(x, self, model_idx, view_col) {
  cr <- gtkCellRendererText()
  cr['editable'] <- TRUE
  cr$setData("editable", "editable")
  cr$setData("view_col", view_col)      # no lookup otherwise
  view_col$setResizable(TRUE)
  view_col$packStart(cr, TRUE)
  view_col$addAttribute(cr, "text", model_idx - 1L)
  view_col$setCellDataFunc(cr, function(vc, cr, model, iter, ...) {
       ## set cell value by formatting
       col <- self$get_column_index(cr$getData("view_col"))
       row <- as.numeric(model$getPath(iter)$toString()) + 1L
       val <- self$get_cell(row, col)
       cr["text"] <- format(val)
   })
  ## make editable
  id <- gSignalConnect(cr,
                       signal="edited",
                       f=function(cr, path, newtext) {
                         visible <- self$get_visible()
                         i <- which(visible)[as.numeric(path) + 1]

                         view_col <- cr$getData("view_col")
                         j <- self$get_column_index(view_col)
                         ## calls ensure_type to format
                         self$cmd_set_cell(i, j, newtext)
                       })
  view_col$setData("n", model_idx)      # map to model
  view_col$setData("edit.id", id)       # use this to disable editing
}

make_editable_cell_renderer.POSIXt <- make_editable_cell_renderer.Date

## This is a bit convoluted due to the command framework. To do
## something, say set a cell value we have 3 methods! One is a
## gWidgets methods (\code{set_items(i,j,value)}), this in turn calls
## a command with undo/redo support (\code{cmd_set_cell}), the
## command relies on the third method to actual set the cell value
## (\code{set_cell(i,j,value)}). To make matters worse, there is an
## issue defining one-off reference classes within a reference class
## when the \code{<<-} operator is involved. As such, we have a
## fourth place things may be defined -- in reference class
## definitions appearing after the one for \code{GDf}. Be warned,
## this is a maintenance issue.

GDfBase <- setRefClass("GDfBase",
                       contains="GWidget",
                       fields=list(
                         model="ANY",
                         store="ANY",
                         freeze_attributes="character", # one of FALSE, TRUE, "row", or "column"
                         cmd_stack="ANY",
                         cell_popup_id="ANY"
                         ),
                       methods=list(
                       set_frame=function(items) {
                         "Change data frame, update view, clear out stack"
                         clear_stack()
                         clear_view_columns()

                         ## if any row is all NA, then we set the class to numeric
                         for (nm in names(items)) {
                           if(all(is.na(items[,nm]))) {
                             items[nm] <- rep(NA_real_, nrow(items))
                           }
                         }

                         ## turn logical into factor -- must turn back
                         inds <- which(sapply(items, function(x) is(x, "logical")))
                         for (i in inds) 
                             items[[i]] <- my_logical(items[[i]])

                         ## turn date into character -- must turn back
                         inds <- which(sapply(items, function(x) is(x, "Date")))
                         for (i in inds) 
                             items[[i]] <- my_date(items[[i]])
                         
                         
                         mod_items <- cbind(`_visible`=rep(TRUE, nrow(items)),
                                            `_deleted`=rep(FALSE, nrow(items)),
                                            `_rownames`=I(rownames(items)),
                                            items)
                         
                         model$setFrame(mod_items)
                         add_view_columns()
                       },
                       save_data=function(nm, where) {
                         assign(nm, get_frame(), where)
                         cmd_stack$clear()
                       },
                       ##
                       ## View column methods
                       ##
                       get_view_column=function(j) {
                         "Return view column in column j (1-based). Use j=0 for rownames column"
                         widget$getColumns()[[j + 1L]] # skip rownams
                       },
                       clear_view_columns=function() {
                         "Clear out all view columns from view"
                         sapply(rev(widget$getColumns()), widget$removeColumn)
                       },
                       add_view_columns=function() {
                         "Add view columns to treeview widget"
                         DF <- as.data.frame(model)

                         ## add row names column
                         view_col<- add_editable_cell_renderer(as.character(DF[[3]]), self=.self, model_idx=3)
                         cr <- view_col$getCellRenderers()[[1]]; cr['font'] <- "Bold"
                         widget$insertColumn(view_col, -1L)
                         ## add remaining columns
                         sapply(seq_along(DF[,-(1:3), drop=FALSE]), function(i) {
                           view_col <- add_editable_cell_renderer(DF[[i + 3L]], self=.self, model_idx=i + 3L)
                           widget$insertColumn(view_col, -1L)
                           if(!freeze_attributes %in% c("TRUE", "column"))
                             add_popup_to_view_col(view_col)
                         })
                         ## set names
                         set_names(names(DF)[-(1:3)]) # remove first 3!
                       },
                       ##
                       ## Get the data
                       ##
                       get_frame=function() {
                         "Get data frame from columns. Skips deleted rows, but returns non-visible ones"
                         columns <- widget$getColumns()[-1] # drop rownames
                         cols <- sapply(columns, function(vc) vc$getData("n"))
                         out <- model[]
                         ## must convert character to Date before subsetting.
                         inds <- which(sapply(out, function(x) is(x, "mylogical")))
                         for (i in inds) 
                             out[[i]] <- as.logical(out[[i]])
                         ## convert mydate to  date
                         inds <- which(sapply(out, function(x) is(x, "mydate")))
                         for (i in inds) 
                             out[[i]] <- as.Date(out[[i]])
                         
                         
                         out <- out[not_deleted(),cols, drop=FALSE]
                         names(out) <- get_names()
                         rownames(out) <- make.unique(get_rownames())
                         
                         out
                       },
                         ## DND
                         ## XXX This needs fleshing out
                         add_dnd_columns=function() {
                           ## Hack to add in drag and drop to columns
                           remove_popup_menu()
                           add_dnd_to_vc <- function(vc) {
                             vc$setClickable(TRUE)
                             btn <- vc$getWidget()$getParent()$getParent()$getParent()
                             label <- vc$getWidget()$getChild() 
                             gtkDragSourceSet(btn,
                                              start.button.mask=c("button1-mask", "button3-mask"),
                                              targets=widgetTargetTypes[["text"]],
                                              actions="copy")
                             gSignalConnect(btn, "drag-data-get", function(data, widget, contet, sel, ty, tm, ...) {
                               sel$setText(data$getLabel(), -1)
                             }, label, user.data.first=TRUE)
                           }

                           QT <- Map(add_dnd_to_vc, widget$getColumns())
                         },
                       ##
                       ## Column methods
                       ##
                       add_column=function(x, nm) {
                         "Add column to end of model, create new view column, set name to nm"
                         n <- get_dim()[2]
                         if(missing(nm))
                           nm <- sprintf("X%s", n)
                         frame <- as.data.frame(model)
                         frame[not_deleted(),ncol(frame) + 1] <- x
                         model$setFrame(frame)
                         model_n <- dim(model)[2]
                         view_col <- add_editable_cell_renderer(x, .self, model_n)
                         widget$insertColumn(view_col, -1)
                         add_popup_to_view_col(view_col)
                         
                         j <- get_column_index(view_col)
                         set_name(j, nm)

                         
                         model_n         # return
                       },
                       insert_column=function(j, model_idx, nm) {
                         "Insert column j with model number model_idx and name name"
                         DF <- as.data.frame(model)
                         view_col <- add_editable_cell_renderer(DF[[model_idx]], .self, model_idx)
                         view_col$getWidget()$getChild()$setLabel(nm)
                         widget$insertColumn(view_col, j)
                         add_popup_to_view_col(view_col)
                       },
                       remove_column=function(j) {
                         "Remove column from view. Keeps data in model. Returns name and index in model to undo"
                         column <- get_view_column(j)
                         old_nm <- get_name(j)
                         model_idx <- column$getData("n") # model_idx is 1-based
                         widget$removeColumn(column)
                         invoke_change_handler()
                         ## return info to reconstruct
                         invisible(list(nm=old_nm, model_idx=model_idx ))
                       },
                       move_column=function(from ,to) {
                         "Move a view column from j to i, shift others over. Does not effect model."
                         columns <- widget$getColumns()[] # includes rownames as 1, so no shift over
                         from_col <- columns[[from + 1L]]
                         to_col <- columns[[to]]
                         widget$moveColumnAfter(from_col, to_col)
                       },
                       hide_column=function(j, value) {
                         "Hide column j (view coordinates). Hiding column j does not remove it and j still refers to this column. That is view coordinates are different from what is in GUI"
                         column <- get_view_column(j)
                         column$setVisible(!as.logical(value))
                         invisible(!value)
                       },
                       unhide_column=function(j) {
                         column <- get_view_column(j)
                         column$setVisible(TRUE)
                       },
                       is_editable=function(j) {
                         column <- get_view_column(j)
                         cr <- column$getCellRenderers()[[1]]
                         cr[cr$getData("editable")]
                       },
                       block_editable_column=function(j) {
                         "Block that column j can be edited"
                         column <- get_view_column(j)
                         cr <- column$getCellRenderers()[[1]]
                         cr[cr$getData("editable")] <- FALSE
                       },
                       unblock_editable_column=function(j) {
                         "unblock that column j can be edited"
                         column <- get_view_column(j)
                         cr <- column$getCellRenderers()[[1]]
                         cr[cr$getData("editable")] <- TRUE
                       },
                       ##
                       ## Row methods
                       ##
                       hide_row=function(i,value=TRUE) {
                         "Hide row i, i refers to non-deleted rows. value if TRUE to hide, FALSE to unhide"
                         i <- map_i(i)
                         model[i,1] <<- !as.logical(value)
                         invisible(!value) # return opposite for command framework
                       },
                       remove_row=function(i, value, model_index=FALSE) {
                         "Delete row"
                         ## we don't actually delete, we just make not visible and deleted.
                         if(!model_index)
                           i <- map_i(i)
                         model[i,1] <<- !as.logical(value)
                         model[i,2] <<- as.logical(value)
                         invisible(i)
                       },
                       unremove_row=function(model_i) {
                         "Un delete row model_i."
                         model[model_i,1] <<- TRUE # show it
                         model[model_i,2] <<- FALSE
                       },
                       insert_row=function(i, value) {
                         "Insert new row after position i, i=0:nrow"
                         ## we insert row into model. How depends on what i is.
                         DF <- as.data.frame(model)
                         if(i == 0) {
                           ## first
                           new_DF <- DF[c(1, seq_len(nrow(DF))), ]
                           new_i <- 1
                         } else if(i >= get_dim()[1]) {
                           i <- get_dim()[1] # in case bigger
                           ## end
                           new_DF <- DF[c(seq_len(nrow(DF)), 1), ]
                           new_i <- nrow(new_DF)
                         } else {
                           ## middle
                           ii <- map_i(i)
                           new_DF <- DF[c(1:ii, 1, (ii+1):nrow(DF)), ]
                           new_i <- ii + 1
                         }
                         ## clear
                         n <- ncol(new_DF)
                         new_DF[new_i,] <- lapply(1:n, function(i) NA)
                         new_DF[new_i, 1:3] <- list(TRUE, FALSE, "")
                         
                         if(!missing(value)) {
                           ind <- sapply(widget$getColumns()[-1], function(vc) vc$getData("n"))
                           new_DF[new_i, ind] <- value
                         } 

                         model$setFrame(new_DF)
                         return(new_i)   # new column in model coordinates
                       },
                       ## names
                       set_view_column_name=function(column, value) {
                         j <- get_column_index(column)
                         old_nm <- get_name(j)
                         column$getWidget()$getChild()$setLabel(value)
                         return(old_nm)
                       },
                       set_name = function(j, value) {
                         "Set view column's name"
                         old_nm <- get_name(j)
                         column <- get_view_column(j)
                         column$getWidget()$getChild()$setLabel(value)
                         invoke_change_handler()
                         return(old_nm)
                       },
                       get_name=function(j) {
                         column <- get_view_column(j)
                         column$getWidget()$getChild()$getLabel()
                       },
                       hide_names=function(value) {
                         "Toggle display of names by logical values"
                         widget$setHeadersVisible(as.logical(value))
                       },
                       set_row_name=function(i, value) {
                         i <- map_i(i)
                         old_value <- model[i, 3L]
                         model[i, 3L] <<- value
                         invisible(old_value)
                       },
                       hide_row_names=function(value) {
                         "Toggle display of row names by logical value"
                         hide_column(0, value)
                       },
                       ##
                       ## mappings between view columns and model
                       ##
                       get_column_index=function(view_col) {
                         "Get view column index from view column"
                         columns <- widget$getColumns()[-1] # no reownames
                         Filter(function(i) identical(columns[[i]], view_col), seq_along(columns)) 
                       },
                       map_j=function(j) {
                         "get column in model from j"
                         column <- get_view_column(j) # skip rownames
                         n <- column$getData("n") # 0-based
                         n
                       },
                       not_deleted=function() {
                         "Return indices of non deleted model rows"
                         which(!model[,2L])
                       },
                       map_i=function(i) {
                         "Return model index from i (possibl sorted, removed, ...)"
                         ## i is in order, but we skip over deleted -- but not filtered
                         not_deleted()[i]
                       },
                       ##
                       ## Values by cell, column
                       ##
                       get_cell=function(i,j) {
                         "Get value in cell i,j using view coordinates"
                         model[map_i(i), map_j(j)]
                       },
                       set_cell=function(i, j, value) {
                         "set cell, i, j in view coordinates (including hidden). Return old_value"
                         n <- map_j(j)
                         old_value <- model[map_i(i), n]
                         model[map_i(i), n] <<- ensure_type(model[,n], value)
                         old_value
                       },
                       get_column_value=function(j) {
                         "Get data in model for jth column in view coordinates"
                         model[not_deleted(), map_j(j)]
                       },
                       ##
                       ## Selection methods. From gtable
                       ##
                       get_selected=function() {
                         "Get selected indices or numeric(0)"
                         sel_model <- widget$getSelection()
                         x <- sapply(sel_model$getSelectedRows()$retval, gtkTreePathToString)
                         if(is.null(x))
                           x <- integer(0)
                         else
                           x <- as.numeric(x) + 1L
                         seq_len(nrow(model))[model[,1]][x] # not deleted
                       },
                       set_selected=function(ind) {
                         "Set selected rows by index"
                         old_ind <- get_selected()
                         ind <- seq_len(nrow(model))[!model[,2]][ind]
                         sel_model = widget$getSelection()
                         block_handlers()
                         sel_model$unselectAll()
                         lapply(ind, function(i) {
                           sel_model$selectPath(gtkTreePathNewFromString(i-1))
                         })
                         
                         unblock_handlers()
                       },
                         set_selectmode=function(mode=c("none", "single", "browse", "multiple")) {
                           sel = widget$getSelection()
                           sel$setMode(match.arg(mode))
                         },
                       ##
                       ## Popup menu methods. From gtable (should be a subclass)
                       ##
                       default_popup_menu=function(view_col) {
                         "Provide default popup menu (passed to gmenu(..., popup=TRUE))"
                         j <- get_column_index(view_col)
                         x <- get_column_value(j)
                         nm <- get_name(j)

                         ## intercept row names
                         types <- c("other", "character", "factor", "numeric", "logical")
                         tmp <- function(x) UseMethod("tmp")
                         tmp.default <- function(x) ""
                         tmp.numeric <- function(x) "numeric"
                         tmp.factor <- function(x) "factor"
                         tmp.character <- function(x) "character"
                         tmp.logical <- function(x) "logical"

                         actions <- list(
                                         gaction("Rename column", handler=function(h,...) {
                                           out <- ginput(gettext("New column names:"), nm, title=gettext("Rename column"), parent=.self)
                                           if(nchar(out))
                                             cmd_set_column_name(j, out)
                                         }),
                                         gseparator(),
                                         gaction("Insert column...", handler=function(h,...) {
                                           ## need x, nm
                                           x <- character(get_dim()[1])
                                           nm <- "Replace me"
                                           cmd_insert_column(x, nm, j)
                                         }),
                                         gaction("Delete column", handler=function(h,...) {
                                           cmd_remove_column(j)
                                         }),
                                         ## gaction("Hide column", handler=function(h,...) {
                                         ##   cmd_hide_column(j)
                                         ## }),
                                         gseparator(),
                                         ## Coerce class of object
                                         gradio(types, selected=getWithDefault(match(tmp(x), types), 1L), handler=function(h,...) {
                                           ind <- svalue(h$obj, index=TRUE)
                                           if(ind > 1) 
                                             cmd_coerce_column(j, get(sprintf("as.%s", types[ind])))
                                         }),
                                         gseparator(),
                                         gaction("Edit factor levels...", handler=function(h, ...) {
                                           collapseFactor <- function(f, parent = NULL) {
                                             out <- character()
                                             w <- gbasicdialog("Edit factor levels", parent = parent,
                                                               handler = function(h,...) {
                                                                 new_f <- relf$get_value()
                                                                 if(!is.factor(new_f))
                                                                     new_f <- factor(new_f)
                                                                 assign("out", new_f, inherits=TRUE)
                                                               })
                                             size(w) <- c(600, 400)
                                             
                                             g <- ggroup(cont = w)
                                             relf <- CollapseFactor$new(f, cont = g)
                                             visible(w, set = TRUE)
                                             out
                                           }
                                           out <- collapseFactor(x)
                                           if(length(out)) {
                                             cmd_replace_column(out, j)
                                           }
                                           
                                         }),
                                         gseparator(),
                                         gcheckbox("Editable", checked=is_editable(j), handler=function(h,...) {
                                           if(svalue(h$obj))
                                             unblock_editable_column(j)
                                           else
                                             block_editable_column(j)
                                         }),
                                         gseparator(),
                                         gaction("Apply function ...", handler=function(h,...) {
                                           w <- gbasicdialog(gettext("Apply a function to values in the column"), parent=.self, handler=function(h,...) {
                                             val <- svalue(txt)
                                             new_x <- eval(parse(text=val), envir=list(x=x))
                                             if(is.vector(new_x))
                                               cmd_insert_column(new_x)
                                             else
                                               galert(gettext("Expression did not return a vector"), parent=.self)
                                           })
                                           txt <- gtext("sapply(x, function(i) {\n\n})", cont=w)
                                           visible(w, TRUE)
                                         })
                                         )
                         enabled(actions[[8]]) <- is.factor(x)
                         actions
                       },
                       add_popup_menu=function(menulist) {
                         f <- function(...) menulist
                         add_popup(f)
                       },
                       add_popup=function(menu_fun=NULL) {
                         "Add a popup menu to the columns. Function should generate list of actions, ..."
                         
                         
                         sapply(widget$getColumns()[-1], function(view.col) {
                           add_popup_to_view_col(view.col)
                         })
                       },
                       add_popup_to_view_col=function(view.col, menu_fun) {
                         if(freeze_attributes %in% c("TRUE", "column")) {
                           return()     # no popup menu if frozen
                         }
                         if(missing(menu_fun))
                           menu_fun <- .self$default_popup_menu

                         view.col$setClickable(TRUE)
                         event_box <- view.col$getWidget()                          
                         btn <- event_box$getParent()$getParent()$getParent()
                         id <- gSignalConnect(btn, "button-press-event", f=function(w, e, data, ...) {
                           if(isRightMouseClick(e)) {
                             popup <- gmenu(menu_fun(data), popup=TRUE, toolkit=toolkit)
                             ## This is a *real* hack, as the following doesn't
                             ## work when I use event_box. NOt sure why
                             ## not, seems like the right combination of
                             ## arguments is given
                             popup$widget$popup(button=e$button, activate.time=e$time)
                           }
                           FALSE
                         }, data=view.col)
                         btn$setData("popup_id", id)
                       },
                       remove_popup_menu=function() {
                         "remove popup menu from column headers"
                         sapply(widget$getColumns(), function(view.col) {
                           view.col$setClickable(FALSE)
                           btn <- view.col$getWidget()$getParent()$getParent()$getParent()
                           if(!is.null(id <- btn$getData("popup_id")))
                             gSignalHandlerDisconnect(btn, id)
                         })
                       },
                       default_cell_popup_menu=function() {
                         ## returns a function of i,j which produces actions for a popup menu
                         ## j=0 when rownames column
                         f <- function(self, i, j) {
                           if(j == 0) {
                             ## rownames popup
                             actions <- list(
                                             gaction("Insert row", handler=function(...) {
                                               self$cmd_insert_row(i)
                                             }),
                                             gaction("Delete row", handler=function(...) {
                                               self$cmd_remove_row(i)
                                             })#,
                                             ## gaction("Hide row", handler=function(...) {
                                             ##   self$cmd_hide_row(i)
                                             ## })
                                             )
                           } else {
                             ## No idea what to put here...
                             actions <- list(
                                             gaction("cell popup")
                                             )
                           }
                         }
                         return(f)
                       },
                       add_cell_popup=function(menu_fun) {
                         "Add cell popup, pass in function to produce menu or uses default"
                         if(missing(menu_fun))
                           menu_fun <- default_cell_popup_menu()

                         id <- gSignalConnect(widget, "button-press-event", f=function(w, e, self) {
                           if(isRightMouseClick(e)) {
                             path <- w$getPathAtPos(e$x, e$y)
                             if(!path$retval) # no path
                               return(FALSE)
                             i <- visible <- self$get_visible()
                             i <- which(visible)[as.numeric(path$path$toString()) + 1]
                             j <- self$get_column_index(path$column)
                             if(length(j) == 0) j <- 0
                             ## Can have popup on cells, but right now don't know what to put there
                             if(j == 0) {
                               popup <- gmenu(menu_fun(self, i, j), popup=TRUE, toolkit=toolkit)
                               popup$widget$popup(button=e$button, activate.time=e$time)
                             }
                           }
                           FALSE
                         }, data=.self)
                         cell_popup_id <<- id
                       },
                       remove_cell_popup=function() {
                         "Remove cell popup"
                         gSignalHandlerDisconnect(widget, cell_popup_id)
                       },
                       ##
                       ## Main GWidget interface. Need gWidgets (svalue) and matrix interface
                       ##
                       get_dim=function() {
                         "size of displayed data"
                         ncols <- length(widget$getColumns()) - 1L # no rownames
                         nrows <- length(model[not_deleted(),1])
                         c(rows=nrows, cols=ncols)
                       },
                       get_length=function() {
                         get_dim()[2]
                       },
                       get_value=function(drop=TRUE, ...) {
                         ind <- get_selected()
                         if(length(ind) == 0)
                           return(NULL)  # ?? what is right?
                         
                         DF <- get_frame()
                         if(drop)
                           DF[ind, 1]
                         else
                           DF[ind,]
                       },
                       set_value=function(value, ...) {

                       },
                       get_index=function(...) {
                         get_selected()
                       },
                       set_index=function(value, ...) {
                         set_selected(value)
                       },
                       get_items=function(i,j, ...,drop=TRUE) {
                         x <- get_frame()
                         x[i,j, ..., drop=drop]
                       },
                       set_items=function(value, i, j, ...) {
                         "Replace part of data: whole thing, by column, by cell. By row?"
### XXX Need checks on i,j bounds not exceeding. Need to call insert_column, insert_row otherwise
                         
                         if(missing(i) && missing(j)) {
                           if(!is.data.frame(value))
                             value <- data.frame(value, stringsAsFactors=FALSE)
                           set_frame(value)
                         } else if(missing(i)) {
                           ## replace column by column
                           if(is.vector(value))
                             cmds <- list(cmd_replace_column(value, j))
                           else
                             cmds <- lapply(seq_along(j), function(i) cmd_replace_column(value[i], j[i], add=FALSE))
                           cmd_stack$add(gWidgets2:::CommandList$new(lst=cmds))
                         } else if(missing(j)) {
                           ## XXX need to add this row by row!
                           value <- as.data.frame(value)
                           sapply(seq_along(value), function(col) .self$set_items(value[,row], i, col))
                         } else {
                           if(length(i) == 1) {
                             value <- rep(value, length=length(j)) # recyle
                             cmd_list <- lapply(seq_along(j), function(jj) cmd_set_cell(i, j[jj], value[jj], add=FALSE))
                           } else if(length(j) == 1) {
                             value <- rep(value, length=length(i)) # recyle
                             cmd_list <- lapply(seq_along(i), function(ii) cmd_set_cell(i[ii], j, value[ii], add=FALSE))                                             } else {
                               ## no recycling
                               if(length(i) != nrow(value) || length(j) != ncol(value))
                                 stop(gettext("value is not of correct dimensions for indices"))
                               cmd_list <- lapply(seq_len(length(i) * length(j)), function(x) NULL)
                               ctr <- 1
                               for(ii in seq_along(i))
                                 for(jj in seq_along(j)) {
                                   cmd_list[[ctr]] <- cmd_set_cell(i[ii], j[jj], value[i[ii], j[jj]], add=FALSE)
                                   ctr <- ctr + 1
                                 }
                             }
                           cmd_stack$add(gWidgets2:::CommandList$new(lst=cmd_list))
                         }
                       },
                       get_names=function() {
                         sapply(widget$getColumns()[-1], function(vc) vc$getWidget()$getChild()$getLabel())
                       },
                       set_names=function(value) {
                         "Set the names of each column"
                         ## modify when using widget, not default
                                        #f <- function(vc, nm) vc$getWidget()$getChild()$setLabel(nm)
                                        #mapply(f, widget$getColumns()[-1], value)
                         cmd_set_column_names(value)
                       },
                       get_rownames=function() {
                         model[not_deleted(), 3]       # fixed
                       },
                       set_rownames=function(value) {
                         if(length(value) != length(not_deleted()))
                           return()      # wrong length
                         old_names <- get_rownames()
                         model[not_deleted(),3] <<- value
                         invisible(old_names)
                       },
                       get_dimnames=function() {
                         list(rownames=get_rownames(), colnames=get_names())
                       },
                       set_dimnames=function(value) {
                         XXX()
                       },
                       get_visible=function() {
                         "Return which rows are visible"
                         model[not_deleted(),1L]
                       },
                       set_visible=function(value) {
                         ## don't update
                         block_handlers()
                         if(length(value) == length(not_deleted()))
                           model[not_deleted(), 1L] <<- as.logical(value)
                         unblock_handlers()
                       },
                       get_editable=function(j) {
                         is_editable(j)
                       },
                       set_editable=function(value, j, ...) {
                         "Make a column editable or not."
                         if(value)
                           unblock_editable_column(j)
                         else
                           block_editable_column(j)
                       },
                       ##
                       ## Handler code
                       ##
                       handler_widget=function() {
                         ## for add_handler_changed
                         model
                       },
                       add_handler_clicked=function(handler, action=NULL, ...) {
                         ## put this on tree view, we used handler_widget for the model
                         signal <- "button-press-event"
                         if(is_handler(handler)) {
                           o <- gWidgets2:::observer(.self, event_decorator(handler), action)
                           invisible(add_observer(o, signal))
                           gSignalConnect(widget, signal, function(...) {
                             .self$notify_observers(signal=signal, ...)
                           })
                           connected_signals[[signal]] <<- TRUE
                         }
                       },
                       add_handler_column_clicked=function(handler, action=NULL, ...) {
                         XXX()
                       },
                       add_handler_column_double_click=function(handler, action=NULL, ...) {
                         XXX()
                       },
                       add_handler_column_right_click=function(handler, action=NULL, ...) {
                         XXX()
                       },
                       ##
                       ## Command infrastructure
                       ##
                       clear_stack=function() cmd_stack$clear(),
                       can_undo=function() cmd_stack$can_undo(),
                       can_redo=function() cmd_stack$can_do(),
                       undo=function() cmd_stack$undo(),
                       redo=function() cmd_stack$redo(),
                       ##
                       ## Commands with undo support
                       ## 
                       cmd_set_cell=function(i, j, value, add=TRUE) {
                         "Set cell i,j (in view indices) to value"
                         if(length(j)) {
                           cmd <- gWidgets2:::Command$new(receiver=.self, meth="set_cell",i=i, j=j, value=value)
                         } else {
                           ## setting row name
                           cmd <- gWidgets2:::Command$new(receiver=.self, meth="set_row_name",i=i, value=value)
                         }
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       ## Column commands. 
                       cmd_insert_column=function(x, nm, j, add=TRUE) {
                         "Insert values from x into j"
                         if(missing(nm))
                           nm <- gettext("Replace me")
                         if(missing(j))
                           j <- get_dim()[2]
                         cmd <- InsertColumn$new(.self, meth="", x=x, nm=nm, j=j)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_replace_column=function(x, j, add=TRUE) {
                         "Replace values in column j with x"
                         cmd <- ReplaceColumn$new(.self, meth="", x=x, j=j)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_remove_column=function(j, add=TRUE) {
                         "remove column j"
                         cmd <- RemoveColumn$new(.self, meth="", j=j)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_hide_column=function(j, add=TRUE) {
                         "hide column j"
                         ## do command
                         cmd <-  gWidgets2:::Command$new(receiver=.self, meth="hide_column", j=j, value=TRUE)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_unhide_column=function(j, add=TRUE) {
                         "Show hidden column j"
                         ## do command
                         cmd <- setRefClass("UnhideColumn",
                                            contains="Command",
                                            methods=list(
                                              do=function() receiver$unhide_column(params$col),
                                              undo=function() receiver$hide_column(params$col)
                                              ))$new(.self, meth="", col=j)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_coerce_column=function(j, coerce_with, add=TRUE) {
                         "Coerce column using coerce_with function, e.g. as.integer or as.character"
                         x <- get_column_value(j)
                         x <- coerce_with(x)
                         cmd <- ReplaceColumn$new(.self, meth="", j=j, x=x)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_set_column_name=function(j, nm, add=TRUE) {
                         cmd <- gWidgets2:::Command$new(receiver=.self, meth="set_name", j=j, value=nm)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_set_column_names=function(value, add=TRUE) {
                         if(length(value) != get_dim()[2])
                           stop(gettext("Wrong length names"))
                         cmds <- lapply(seq_along(value), function(j) cmd_set_column_name(j, value[j], add=FALSE))
                         cmdlist <- gWidgets2:::CommandList$new(lst=cmds)
                         if(add)
                           cmd_stack$add(cmdlist)
                         invisible(cmdlist)
                       },
                       ## row commands
                       cmd_insert_row=function(i, nm, add=TRUE) {
                         if(missing(nm))
                           nm <- ""
                         cmd <- InsertRow$new(.self, meth="", i=i, nm=nm)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_remove_row=function(i, add=TRUE) {
                         cmd <- RemoveRow$new(.self, meth="", i=i)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_hide_row=function(i, add=TRUE) {
                         cmd <- gWidgets2:::Command$new(.self, meth="hide_row", i=i, value=TRUE)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       },
                       cmd_unhide_row=function(i, add=TRUE) {
                         cmd <- gWidgets2:::Command$new(.self, meth="hide_row", i=i, value=FALSE)
                         if(add)
                           cmd_stack$add(cmd)
                         invisible(cmd)
                       }

                       ))


##' For \code{RGtk2}, a GDf object has several methods defined for it
##' that are toolkit specific, but may be useful. For example, the
##' columns may be made editable or non editable
##' (\code{block_editable_column} and \code{unblock_editable_column})
##' (accessed through \code{editable<-}); the headers can be
##' hidden/shown through the method \code{hide_names(boolean)}; the
##' rownames can be hidden/shown through themethod
##' \code{hide_row_names(boolean)}; the popup menus for the headers
##' can be removed (\code{remove_popup_menu}) and customized
##' (\code{add_popup}); similarly the cell popup can be
##' (\code{remove_cell_popup} and \code{add_cell_popup}).
##'
##' Passing in a value \code{freeze_attributes = TRUE} will make it so
##' there are no menu items to resize frame, change variable types,
##' relabel factors, .... Values of \code{"row"} or \code{"column"}
##' will remove popup menus just for the row or columns.
##' 
##' @rdname gWidgets2RGtk2-package
GDf <- setRefClass("GDf",
                   contains="GDfBase",
                   methods=list(
                     initialize=function(toolkit, items, name=deparse(substitute(df)),
                       handler=NULL, action=NULL,
                       container=NULL,
                       ...) {
                       
                       model <<- rGtkDataFrame()
                       store <<- model$filter()
                       store$setVisibleColumn(0L)
                       
                       widget <<- gtkTreeView()
                       widget$setModel(store)

                       ## make pretty XXX shade, ...
                       widget$setRulesHint(TRUE)
                       widget$setGridLines(GtkTreeViewGridLines['both'])
                       ## block is scrolled window
                       block <<- gtkScrolledWindow()
                       block$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                       block$add(widget)

                       ## command stack
                       cmd_stack <<- gWidgets2:::CommandStack$new()

                       ## initialize fields
                       initFields(default_expand=TRUE,
                                  default_fill=TRUE,
                                  change_signal="row-changed",
                                  freeze_attributes = as.character(getWithDefault(list(...)$freeze_attributes, FALSE))
                                  )

                       if(!is.data.frame(items))
                         items <- data.frame(items, stringsAsFactors=FALSE)
                       set_frame(items)

                       ## adjust if we should freeze attributes
                       if(freeze_attributes %in% c("TRUE", "row")) {
                         set_editable(FALSE, 0) # don't edit row names
                       }
                       if(!freeze_attributes %in% c("TRUE", "row")) {
                         ## menus only good once realized
                         gSignalConnect(widget, "realize", f=function(...) .self$add_cell_popup())
                       }

                       

                       
                       add_to_parent(container, .self, ...)
                       
                       ##handler_id <<- add_handler_changed(handler, action)
                       
                       callSuper(toolkit)
                     }
                     ))


## Some commands defined as separate classes. Put here, not inline as
## Issue here with <<- within command
## These are not exported or documented.

ReplaceColumn <- setRefClass("ReplaceColumn",
                             contains="Command",
                             methods=list(
                               do=function() {
                                j <- params$j
                                params$old_n <<- receiver$map_j(j)
                                #
                                x <- params$x
                                params$nm <<- receiver$get_name(j)
                                receiver$add_column(x, params$nm)
                                params$new_n <<- receiver$get_dim()[2] 
                                params$new_index <<- ncol(receiver$model) # new model index
                                receiver$remove_column(j)
                                receiver$move_column(params$new_n - 1L, j)

                               },
                               undo=function() {
                                 DF <- as.data.frame(receiver$model)
                                 n <- params$old_n
                                 add_editable_cell_renderer(DF[[n]], self=receiver, model_idx=n, receiver$get_view_column(params$j))
                                 receiver$set_name(params$j, params$nm)
                               },
                               redo=function() {
                                 DF <- as.data.frame(receiver$model)
                                 n <- params$new_index
                                 add_editable_cell_renderer(DF[[n]], self=receiver, model_idx=n, receiver$get_view_column(params$j))
                                 receiver$set_name(params$j, params$nm)
                               }
                               ))

InsertColumn <- setRefClass("InsertColumn",
                            contains="Command",
                            methods=list(
                              do=function() {
                                n <- receiver$add_column(params$x, params$nm)
                                params$model_idx <<- n
                                receiver$move_column(receiver$get_dim()[2], params$j + 1) # to right
                              },
                              undo=function() {
                                receiver$remove_column(params$j + 1)
                              },
                              redo=function() {
                                ## adds another column, not sure why
                                receiver$insert_column(params$j + 1, params$model_idx, params$nm)
                              }
                              ))

RemoveColumn <- setRefClass("RemoveColumn",
                            contains="Command",
                            fields=list(
                              "model_idx"="numeric"
                              ),
                            methods=list(
                              do=function() {
                                out <- receiver$remove_column(params$j) ## nm, model_idx
                                params$nm <<- out$nm; params$model_idx <<- out$model_idx
                              },
                              undo=function() {
                                receiver$insert_column(params$j, params$model_idx, params$nm)
                              },
                              redo=function() do()
                              ))

InsertRow <- setRefClass("InsertRow",
                         contains="Command",
                         fields=list(
                           row_num="ANY"
                           ),
                         methods=list(
                           do=function() {
                             row_num <<- receiver$insert_row(params$i)
                             receiver$set_row_name(row_num, params$nm)
                           },
                           undo=function() {
                             receiver$remove_row(row_num, TRUE, model_index=TRUE)
                           },
                           redo=function() {
                             receiver$unremove_row(row_num)
                           }
                           ))

RemoveRow <- setRefClass("RemoveRow",
                         contains="Command",
                         fields=list(
                           model_index="ANY"
                           ),
                         methods=list(
                           do=function() {
                             model_index <<- receiver$map_i(params$i)
                             receiver$remove_row(model_index, TRUE, model_index=TRUE)
                           },
                           undo=function() {
                             receiver$unremove_row(model_index)
                           },
                           redo=function() do()
                           ))


## ### Factor editor

## CollapseFactor <- setRefClass("CollapseFactor",
##                               fields = list(
##                                old = "ANY",
##                                widget = "ANY"
##                                ),
##                              methods = list(
##                                initialize = function(fac, cont = gwindow(), ...) {
##                                  old <<- as.character(fac)
##                                  make_gui(cont)
##                                  callSuper()
##                                },
##                                make_gui =  function(cont) {
##                                  group <- gpanedgroup(cont = cont)
##                                  levs <- sort(unique(as.character(old)))
##                                  DF <- data.frame(original = levs,
##                                                   new = levs, stringsAsFactors = FALSE)
##                                         #
##                                  widget <<- tbl <- gtable(DF, cont = group,  multiple = TRUE)
##                                  size(tbl) <- c(300, 200)
##                                         #
##                                  nested_group <- ggroup(cont = group, horizontal = FALSE)
##                                  instructions <- gettext(paste(
##                                    "Select a one or more levels.",
##                                    "Enter a new label in the text box.",
##                                    "If there is one level selected, it will be renamed.",
##                                    "If more, they will be collapsed and renamed.",
##                                    sep="\n"))
##                                         #
##                                  glabel(instructions, cont = nested_group)
## #                                 factor_edit <- gcombobox(levs, selected = 0, editable = TRUE, 
## #                                                        cont = nested_group)
##                                  factor_edit <- gedit("", cont=nested_group)
## #                                 factor_edit[] <- levs
##                                  enabled(factor_edit) <- FALSE
##                                         #
##                                  addHandlerSelectionChanged(widget, function(h,...) {
##                                    ind <- svalue(widget, index = TRUE)
##                                    enabled(factor_edit) <- (length(ind) > 0)
##                                    if (length(ind) > 0) {
##                                      blockHandler(factor_edit)
##                                      svalue(factor_edit) <- svalue(widget)
##                                      unblockHandler(factor_edit)
##                                    }
##                                  })
##                                  ##
##                                  factor_edit_change_handler =  function(h,...) {
##                                    ind <- svalue(tbl, index = TRUE)
##                                    if(length(ind) == 0 || ind == 0L)  {
##                                      return()
##                                    }
##                                         #
##                                    tbl[ind,2] <- svalue(factor_edit)
## #                                   svalue(tbl, index = TRUE) <- 0
##                                    blockHandler(factor_edit)
## #                                   factor_edit[] <- sort(unique(tbl[,2]))
## #                                   svalue(factor_edit) <- ""
##                                    unblockHandler(factor_edit)
##                                  }
##                                  addHandlerChanged(factor_edit, handler=function(h,...) {
##                                    blockHandler(factor_edit)
##                                    factor_edit_change_handler(h,...)
##                                    svalue(tbl, index=TRUE) <- 0
##                                    focus(tbl) <- TRUE
##                                    unblockHandler(factor_edit)
##                                  })
##                                  addHandlerKeystroke(factor_edit, handler =factor_edit_change_handler)
##                                  addHandlerBlur(factor_edit, handler=function(h,...) {
##                                    svalue(tbl, index = TRUE) <- 0
##                                    svalue(h$obj) <- ""
##                                  })
##                                },
##                                get_value = function() {
##                                  "Return factor with new levels"
##                                  old_levels <- widget[,1]
##                                  new_levels <- widget[,2]
##                                  new <- old
##                                  for(i in seq_along(old_levels)) # one pass
##                                    new[new == old_levels[i]] <- new_levels[i]
##                                  factor(new)
##                                }
##                                ))

CollapseFactor <- setRefClass("CollapseFactor",
                              fields = list(
                                old = "ANY",
                                cur_reference_level="ANY",
                                widget = "ANY",
                                cur_child = "ANY"
                                ),
                             methods = list(
                               initialize = function(fac, cont = gwindow(), ...) {
                                 old <<- fac
                                 cur_reference_level <<- levels(fac)[1]

                                 make_gui(cont)
                                 callSuper()
                               },
                               make_gui =  function(cont) {
                                 
                                 directions <- "
Adjust levels of a factor.

One can add a level through the 'Add' button.
Toggle if ordered with checkbox.
Select level to rename, make reference level, or
reorder, as appropriate.
Select levels to collapse.
"
                                 

                                 ## adjust these properties during dialog
                                 


                                 g <- gpanedgroup(container=cont, expand=TRUE)
                                 
                                 lg <- gvbox(container=g, fill="y")
                                 rg <- ggroup(container=g, expand=TRUE, fill="both")
                                 
                                 ## fill left group with table
                                 cur_levels <- gtable(levels(old), container=lg,
                                                      multiple=TRUE,
                                                      expand=TRUE, fill="y")
                                 size(cur_levels) <- c(width=200, height=400)
                                 names(cur_levels) <- "Levels"
                                 cur_levels$remove_popup_menu()

                                 bg <- ggroup(cont=lg)
                                 add_level <- gbutton("add", cont=bg, handler=function(h,...) {
                                   add_level_dialog(g)
                                 })
                                 tooltip(add_level) <- gettext("Add a new level to factor")
                                 
                                 is_ordered <- gcheckbox("Ordered", container=bg, checked=is.ordered(old))
                                 tooltip(is_ordered) <- gettext("Toggle if factor is ordered")
                                 
                                 
                                 ## Fill right group
                                 cur_child <<- gvbox(container=rg, expand=TRUE)
                                 glabel(directions, cont=cur_child, anchor=c(-1,0))
                                 
                                 ## adjust size after
                                 addHandler(g, "map", function(...) {
                                   svalue(g) <- 0.2
                                 })
                                 
                                 
                                 ## show different things based on selection...
                                 none_selected <- function() {
                                   delete(rg, cur_child)
                                   cur_child <<- gvbox(container=rg, expand=TRUE)
                                   glabel("Directions...", cont=cur_child, anchor=c(-1, 0))
                                 }
                                 
                                 one_selected <- function() {
                                   ## if one_is selected
                                   delete(rg, cur_child)
                                   cur_child <<- gvbox(container=rg, expand=TRUE)
                                   
                                   ## offer to relabel
                                   glabel(gettext("Relabel:"), container=cur_child, anchor=c(-1,0))
                                   rename_level <- gedit(svalue(cur_levels),
                                                         container=cur_child)
                                   gseparator(container=cur_child)
                                   
                                   ## give choice of making ordered, or adjusting order
                                   if(svalue(is_ordered)) {
                                     bg <- ggroup(cont=cur_child)
                                     move_up <- gbutton("up", cont=bg, handler=function(h,...) {
                                       ind <- svalue(cur_levels, ind=TRUE)
                                       cur <- cur_levels[]
                                       cur[c(ind-1,ind)] <- cur[c(ind, ind-1)]
                                       ## adjust factor
                                       old <<- factor(old, levels=cur)
                                       ## adjust GUI
                                       cur_levels[] <- cur
                                       svalue(cur_levels) <- ind - 1

                                       selection_changed()
                                     })
                                     move_down <- gbutton("down", cont=bg, handler=function(h,...) {
                                       ind <- svalue(cur_levels, ind=TRUE)
                                       cur <- cur_levels[]
                                       cur[c(ind,ind + 1)] <- cur[c(ind+1, ind)]
                                       ## adjust factor
                                       old <<- factor(old, levels=cur)
                                       ## adjust GUI
                                       cur_levels[] <- cur
                                       svalue(cur_levels) <- ind + 1

                                       selection_changed()
                                     })
                                     tooltip(move_up) <- gettext("Move selected level up in the order")
                                     tooltip(move_down) <- gettext("Move selected level down in the order")
                                     
                                     
                                     cur_ind <- svalue(cur_levels, ind=TRUE)
                                     nlevs <- length(cur_levels[])
                                     
                                     enabled(move_up) <- cur_ind > 1
                                     enabled(move_down) <- cur_ind < nlevs
                                   } else {
                                     ## can make ordered *or* make reference level
                                     bg <- ggroup(container=cur_child)
                                     ref_button <- gbutton("Set as reference", cont=bg, handler=function(h,...) {

                                       ind <- svalue(cur_levels, index=TRUE)
                                       if (ind == 1) return()
                                       cur_reference_level <<- svalue(cur_levels)
                                       ## adjust old
                                       relevel(old, cur_reference_level)
                                       ## adjust GUI
                                       blockHandler(cur_levels)
                                       tmp <- cur_levels[]
                                       tmp[c(1, ind)] <- tmp[c(ind, 1)]
                                       cur_levels[] <- tmp
                                       svalue(cur_levels, index=TRUE) <- 1
                                       unblockHandler(cur_levels)
                                     })
                                     tooltip(ref_button) <- "
For an unordered factor, the top most level is set
as the reference level.Clicking this button will
move the selected level to the top.
"
      
                                   }
                                   
                                   addSpring(cur_child)
                                   
                                   addHandler(rename_level, "key-press-event", handler=function(h, w, e, ...) {
                                     if(e$GetKeyval() != GDK_Return) {
                                       return(FALSE)
                                     }
                                     ind <- svalue(cur_levels, index=TRUE)
                                     new_name <- svalue(h$obj)
                                     ## adjust old
                                     tmp <- old
                                     levels(tmp)[ind] <- new_name
                                     old <<- tmp
                                     ## adjust GUI
                                     blockHandler(rename_level)
                                     tmp <- cur_levels[]
                                     tmp[ind] <- new_name
                                     cur_levels[] <- tmp
                                     svalue(cur_levels, index=TRUE) <- ind
                                     svalue(rename_level) <- ""
                                     unblockHandler(rename_level)
                                     focus(cur_levels) <- TRUE
                                     
                                     return(TRUE)
                                   })
                                 }
                                 
                                 more_than_one_selected <- function() {
                                   delete(rg, cur_child)
                                   cur_child <<- gvbox(container=rg, expand=TRUE)
                                   
                                   glabel("Collapse selected levels to:", container=cur_child, anchor=c(-1,0))
                                   collapse_levels <- gedit("", intial.msg="Collapse levels to...",
                                                            container=cur_child)
                                   addSpring(cur_child)
                                   
                                   addHandlerChanged(collapse_levels, handler=function(h,...) {

                                     ind <- svalue(cur_levels, index=TRUE)
                                     new_val <- svalue(collapse_levels)
                                     
                                     if (length(ind) < 2)
                                       return()
                                     ## adjust old
                                     tmp <- old
                                     levels(tmp)[ind] <- new_val
                                     old <<- tmp
                                     ## adjust GUI

                                     blockHandler(cur_levels);
                                     tmp <- cur_levels[]
                                     tmp[ind] <- new_val
                                     tmp <- tmp[-sort(ind)[-1]]
                                     cur_levels[] <- tmp
                                     unblockHandler(cur_levels)
                                     
                                     svalue(cur_levels, index=TRUE) <- sort(ind)[1]
                                   })
                                 }
                                 
                                 ##
                                 selection_changed <- function(...) {
                                   ind <- svalue(cur_levels, index=TRUE)
                                   if(length(ind) == 0)
                                     none_selected()
                                   else if(length(ind) == 1)
                                     one_selected()
                                   else
                                     more_than_one_selected()
                                 }
                                 
                                 
                                 addHandlerSelectionChanged(cur_levels, handler=function(h,...) {
                                   blockHandler(cur_levels)
                                   on.exit(unblockHandler(cur_levels))
                                   selection_changed()

                                 })
                                 addHandlerChanged(is_ordered, handler=function(h, ...) {
                                   ## adjust factor
                                   old <<- factor(old, ordered=svalue(h$obj))
                                   ## adjust GUI
                                   ind <- svalue(cur_levels, index=TRUE)
                                   if (length(ind)==0 || ind < 1)
                                     ind <- 1
                                   svalue(cur_levels, index=TRUE) <- ind
                                 })
                                 
                                 ##
                                 add_level_dialog <- function(parent) {
                                   ## add a level to current levels
                                   dlg <- gbasicdialog(parent=parent, handler=function(...) {
                                     new_val <- svalue(e)
                                     tmp <- cur_levels[]
                                     if(nchar(new_val) > 0 && !(new_val %in% tmp)) {
                                         tmp <- c(tmp, new_val)
                                         ## adjust factor
                                         xx <- old
                                         levels(xx) <- tmp
                                         old <<- xx
                                         ## add to GUI
                                         blockHandler(cur_levels)

                                         cur_levels[] <- tmp
                                       
                                         unblockHandler(cur_levels)
                                         svalue(cur_levels, index=TRUE) <- length(tmp)
                                     }
                                   })
                                   g <- gvbox(cont=dlg)
                                   glabel("Add a new level to factor ...", cont=g, anchor=c(-1,0))
                                   e <- gedit("", container=g)
                                   visible(dlg, TRUE)
                                 }
                                 
                                 
                                 
                               },
                               get_value = function() {
                                 "Return factor with new levels"
                                 old
                               }
                               ))
