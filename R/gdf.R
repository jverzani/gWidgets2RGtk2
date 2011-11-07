##' @include GWidget.R
##' @include gmenu.R
##' @include dialogs.R
##' @include gtable.R
NULL

## TODO
## * handlers
## * edit factor labels dialog
## * column drag and drop
## * what to do about selection -- it ain't a working
## * size override for passing in column sizes through a list.


##' Toolkit constructor
##'
##' @inheritParams gWidgets2::.XXX
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @note The \code{RGtk2} object has several methods defined for it
##' that are toolkit specific, but may be useful. For example, the
##' columns may be made editable or non editable
##' (\code{block_editable_column} and \code{unblock_editable_column});
##' the headers can be hidden/shown through the method
##' \code{hide_names(boolean)}; the rownames can be hidden/shown
##' through themethod \code{hide_row_names(boolean)}; the popup menus
##' for the headers can be removed (\code{remove_popup_menu}) and
##' customized (\code{add_popup}); similarly the cell popup can be
##' (\code{remove_cell_popup} and \code{add_cell_popup}). 
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
ensure_type.factor <- function(x, value) {x[length(x) + 1] <- value; tail(x, n=1)}
ensure_type.numeric <- function(x, value) as.numeric(value)
ensure_type.integer <- function(x, value) as.integer(value)
ensure_type.logical <- function(x, value) as.logical(value)


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

##' Reference class for data frame editor
##'
##' This is a bit convoluted due to the command framework. To do
##' something, say set a cell value we have 3 methods! One is a
##' gWidgets methods (\code{set_items(i,j,value)}), this in turn calls
##' a command with undo/redo support (\code{cmd_set_cell}), the
##' command relies on the third method to actual set the cell value
##' (\code{set_cell(i,j,value)}). To make matters worse, there is an
##' issue defining one-off reference classes within a reference class
##' when the \code{<<-} operator is involved. As such, we have a
##' fourth place things may be defined -- in reference class
##' definitions appearing after the one for \code{GDf}. Be warned,
##' this is a maintenance issue.
GDf <- setRefClass("GDf",
                   contains="GWidget",
                    fields=list(
                      model="ANY",
                      store="ANY",
                      cmd_stack="ANY",
                      cell_popup_id="ANY"
                      ),
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
                                   change_signal="row-changed")
                        set_frame(items)

                        ## menus only good once realized
                        gSignalConnect(widget, "realize", f=function(...) .self$add_cell_popup())

                        
                        add_to_parent(container, .self, ...)
                        
                        ##handler_id <<- add_handler_changed(handler, action)
                       
                        callSuper(toolkit)
                      },
                      set_frame=function(items) {
                        "Change data frame, update view, clear out stack"
                        clear_stack()
                        clear_view_columns()

                        mod_items <- cbind(`_visible`=rep(TRUE, nrow(items)),
                                           `_deleted`=rep(FALSE, nrow(items)),
                                           `_rownames`=I(rownames(items)),
                                           items)
                        
                        model$setFrame(mod_items)
                        add_view_columns()
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
                        sapply(seq_along(DF[,-(1:3)]), function(i) {
                          view_col <- add_editable_cell_renderer(DF[[i + 3L]], self=.self, model_idx=i + 3L)
                          widget$insertColumn(view_col, -1L)
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
                        out <- model[not_deleted(),cols]
                        names(out) <- get_names()
                        rownames(out) <- make.unique(get_rownames())
                        out
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
                        ## return info to reconstruct
                        invisible(list(nm=old_nm, model_idx=model_idx ))
                      },
                      move_column=function(from ,to) {
                        "Move a view column from j to i, shift others over. Does not effect model."
                        columns <- widget$getColumns()[-1]
                        from_col <- columns[[from]]
                        if(to-1 == 0)
                          to_col <- NULL
                        else
                          to_col <- columns[[to-1]]
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
                        model[i,1] <- !as.logical(value)
                        invisible(!value) # return opposite for command framework
                      },
                      remove_row=function(i, value, model_index=FALSE) {
                        "Delete row"
                        ## we don't actually delete, we just make not visible and deleted.
                        if(!model_index)
                          i <- map_i(i)
                        model[i,1] <- !as.logical(value)
                        model[i,2] <- as.logical(value)
                        invisible(i)
                      },
                      unremove_row=function(model_i) {
                        "Un delete row model_i."
                        model[model_i,1] <- TRUE # show it
                        model[model_i,2] <- FALSE
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
                        model[i, 3L] <- value
                        invisible(old_value)
                      },
                      hide_row_names=function(value) {
                        "Toggle display of row names by logical value"
                        hide_column(1, value)
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
                                        gaction("Hide column", handler=function(h,...) {
                                          cmd_hide_column(j)
                                        }),
                                        gseparator(),
                                        ## Coerce class of object
                                        gradio(types, selected=getWithDefault(match(tmp(x), types), 1L), handler=function(h,...) {
                                          ind <- svalue(h$obj, index=TRUE)
                                          if(ind > 1) 
                                            cmd_coerce_column(j, get(sprintf("as.%s", types[ind])))
                                        }),
                                        gseparator(),
                                        gaction("Edit factor levels...", handler=function(h, ...) {
                                          XXX("Write me")
                                          w <- gbasicdialog(gettext("Edit factor levels"))
                                          levs <- levels(x)
                                          gtable(levs, cont=w)
                                          visible(w, set=TRUE)
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
                        enabled(actions[[9]]) <- is.factor(x)
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
                                            }),
                                            gaction("Hide row", handler=function(...) {
                                              self$cmd_hide_row(i)
                                            })
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
                      get_value=function(...) {

                      },
                      set_value=function(value, ...) {

                      },
                      get_index=function(...) {

                      },
                      set_index=function(value, ...) {

                      },
                      get_items=function(i,j, ...,drop=TRUE) {
                        x <- get_frame()
                        x[i,j, ..., drop=TRUE]
                      },
                      set_items=function(value, i, j, ...) {
                        "Replace part of data: whole thing, by column, by cell. By row?"
                        if(missing(i) && missing(j)) {
                          set_frame(value)
                        } else if(missing(i)) {
                          if(is.vector(value))
                            cmds <- list(cmd_replace_column(j, value))
                          else
                            cmds <- lapply(seq_along(j), function(i) cmd_replace_column(value[i], j[i]))
                          cmd_stack$add(CommandList(lst=cmds))
                        } else if(missing(j)) {
                          ## set by row
                        } else {
                          cmd_set_cell(i, j, value)
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
                        if(length(value) == length(not_deleted()))
                          model[not_deleted(), 1L] <- as.logical(value)
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
                      cmd_set_cell=function(i, j, value) {
                        "Set cell i,j (in view indices) to value"
                        if(length(j)) {
                          cmd <- gWidgets2:::Command$new(receiver=.self, meth="set_cell",i=i, j=j, value=value)
                          cmd_stack$add(cmd)
                        } else {
                          ## setting row name
                          cmd <- gWidgets2:::Command$new(receiver=.self, meth="set_row_name",i=i, value=value)
                          cmd_stack$add(cmd)
                        }
                      },
                      ## Column commands. 
                      cmd_insert_column=function(x, nm, j) {
                        "Insert values from x into j"
                        if(missing(nm))
                          nm <- gettext("Replace me")
                        if(missing(j))
                          j <- get_dim()[2]
                        cmd <- InsertColumn$new(.self, meth="", x=x, nm=nm, j=j)
                        cmd_stack$add(cmd)
                      },
                      cmd_replace_column=function(x, j) {
                        "Replace values in column j with x"
                        cmd <- ReplaceColumn$new(.self, meth="", x=x, j=j)
                        cmd_stack$add(cmd)
                      },
                      cmd_remove_column=function(j) {
                        "remove column j"
                        cmd <- RemoveColumn$new(.self, meth="", j=j)
                        cmd_stack$add(cmd)
                      },
                      cmd_hide_column=function(j) {
                        "hide column j"
                        ## do command
                        cmd <-  gWidgets2:::Command$new(receiver=.self, meth="hide_column", j=j, value=TRUE)
                        cmd_stack$add(cmd)
#                        cmd <- setRefClass("HideColumn",
#                                           contains="Command",
#                                           methods=list(
#                                             do=function() receiver$hide_column(params$col),
#                                             undo=function() receiver$unhide_column(params$col)
#                                             ))$new(.self, meth="", col=j)
#                        cmd_stack$add(cmd)
                      },
                      cmd_unhide_column=function(j) {
                        "Show hidden column j"
                        ## do command
                        cmd <- setRefClass("UnhideColumn",
                                           contains="Command",
                                           methods=list(
                                             do=function() receiver$unhide_column(params$col),
                                             undo=function() receiver$hide_column(params$col)
                                             ))$new(.self, meth="", col=j)
                        cmd_stack$add(cmd)
                      },
                      cmd_coerce_column=function(j, coerce_with) {
                        "Coerce column using coerce_with function, e.g. as.integer or as.character"
                        x <- get_column_value(j)
                        x <- coerce_with(x)
                        cmd <- ReplaceColumn$new(.self, meth="", j=j, x=x)
                        cmd_stack$add(cmd)
                      },
                      cmd_set_column_name=function(j, nm) {
                        cmd <- gWidgets2:::Command$new(receiver=.self, meth="set_name", j=j, value=nm)
                        cmd_stack$add(cmd)
                      },
                      cmd_set_column_names=function(value) {
                        if(length(value) != get_dim()[2])
                          stop(gettext("Wrong length names"))
                        cmds <- lapply(seq_along(value), function(j) cmd_set_column_name(j, value[j]))
                        cmd_stack$add(CommandList$new(lst=cmds))
                      },
                      ## row commands
                      cmd_insert_row=function(i, nm) {
                        if(missing(nm))
                          nm <- ""
                        cmd <- InsertRow$new(.self, meth="", i=i, nm=nm)
                        cmd_stack$add(cmd)
                      },
                      cmd_remove_row=function(i) {
                        cmd <- RemoveRow$new(.self, meth="", i=i)
                        cmd_stack$add(cmd)
                      },
                      cmd_hide_row=function(i) {
                        cmd <- gWidgets2:::Command$new(.self, meth="hide_row", i=i, value=TRUE)
                        cmd_stack$add(cmd)
                      },
                      cmd_unhide_row=function(i) {
                        cmd <- gWidgets2:::Command$new(.self, meth="hide_row", i=i, value=FALSE)
                        cmd_stack$add(cmd)
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



## delete when done
## GDf <- setRefClass("GDf",
##                    contains="GWidget", 
##                    methods=list(
##                      initialize=function(toolkit, items, name=deparse(substitute(df)),
##                        handler=NULL, action=NULL,
##                        container=NULL,
##                        ...) {

##                        block <<- gtkDfEdit(items, name)
##                        widget <<- block$getData(".local")$view # .local is gtkDfEdit thing

##                        modify_df_edit()

                       
##                        add_to_parent(container, .self, ...)
                       
## ##                       handler_id <<- add_handler_changed(handler, action)
                       
##                        callSuper(toolkit)
##                      },
##                      modify_df_edit=function() {
##                        ## Make changes to the dfedit object
                       
##                      },
##                      ## The basic gWidgets interface functions
##                      ##
##                      get_value=function( ...) {
##                        sels <- get_index()
##                        block[sels[[1]], sels[[2]]]
##                      },
##                      set_value=function(value, ...) {
                       
##                      },
##                      get_index = function(...) {
##                       gtkDfEditGetSelection(block)
##                      },
##                      set_index = function(value,...) {
                       
##                      },
##                      get_items = function(i, j, ..., drop=TRUE) {
##                        block[i, j, ..., drop=drop]
##                      },
##                      set_items = function(value, i, j, ...) {
                       
##                      },
##                      get_length=function() {
##                        get_dim()[2]     # no cols
##                      },
##                      get_dim=function() {
##                        block$getDimension()
##                      },
                     
##                      get_dimnames=function() {
##                        "Get the row and column names"
##                        list(block$getRowNames(), block$getColumnNames())
##                      },
##                      set_dimnames=function(value) {
                       
##                      },
##                      get_names=function() {
##                        get_dimnames()[[2]]
##                      },
##                      add_handler_changed=function(handler, action=NULL, ...) {
##                        signal <- "row-changed"
##                        if(is_handler(handler)) {
##                          o <- gWidgets2:::observer(.self, handler, action)
##                          invisible(add_observer(o, signal))
##                        }
##                        ## now connect to model (not view, hence no call to add__handler
##                        model <- widget$getModel()
##                        connect_to_toolkit_signal(signal="row-changed", emitter=model)
##                      }
##                      ))
