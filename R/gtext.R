##' @include GWidget.R
NULL


## Make a tag table to be shared. We use memoise as we only need to make this once.
make_tag_table <- memoise(gtkTextTagTableNew)

##' toolkit implementation
##'
##' @inheritParams gWidgets2::gtext
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gtext guiWidgetsToolkitRGtk2
##' @S3method .gtext guiWidgetsToolkitRGtk2
.gtext.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                    text = NULL, width = NULL, height = 300, font.attr = NULL,
                    wrap = TRUE,
                    handler = NULL, action = NULL, container = NULL,... ) {
  
  GText$new(toolkit,
            text = text, width = width, height = height,
            font.attr = font.attr, wrap = wrap,
            handler = handler, action = action, container = container, ...
            )

}


GText <- setRefClass("GText",
                     contains="GWidget",
                     fields=list(
                       buffer="ANY",
                       tag_table="ANY",
                       font_attr="list"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         text = NULL, width = NULL, height = 300,
                         font.attr = NULL, wrap = TRUE,
                         handler=NULL, action=NULL, container=NULL, ...) {

                         tag_table <<- make_tag_table()
                         buffer <<- gtkTextBufferNew(tag_table)
                         widget <<- gtkTextViewNewWithBuffer(buffer)
                         widget$SetLeftMargin(10)
                         widget$SetRightMargin(10)
                         if(wrap)
                           widget$SetWrapMode(GtkWrapMode['word'])
                         else
                           widget$SetWrapMode(GtkWrapMode['none'])

                         block <<- gtkScrolledWindowNew()
                         block$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                         if(!is.null(width))
                           block$SetSizeRequest(width,height)
                         
                         block$add(widget)
                         widget$show()

                         font_attr <<- getWithDefault(font.attr, list())
                         insert_text(text, where="beginning",  do.newline=FALSE)
                         
                         add_to_parent(container, .self, ...)
                         
                         handler_id <<- add_handler_changed(handler, action)
                         
                         callSuper(toolkit)
                       },
                       get_value=function(drop=FALSE, ...) {
                         "Return text, or selected text if drop=TRUE"
                         if(is.null(drop) || drop == FALSE) {
                           start <- buffer$GetStartIter()$iter
                           end <- buffer$GetEndIter()$iter
                         } else {
                           ## return only **selected** text
                           ## if drop==TRUE
                           bounds <- buffer$GetSelectionBounds()
                           if(bounds$retval == FALSE)
                             return("") # no selectin
                           start <- bounds$start
                           end <- bounds$end
                         }
                         buffer$GetText(start, end) # has embedded "\n"
                       },
                       set_value=function(value, ...) {
                         "Replace all text, pasted together with newline"
                         args <- list(...)
                         drop <- getWithDefault(args$drop, FALSE)
                         bounds <- buffer$GetSelectionBounds()
                         if (drop && bounds$retval) {
                           ## replace selection with new string
                           buffer$insert(bounds$start, value, -1)
                           new_bounds <- buffer$GetSelectionBounds()
                           buffer$delete(new_bounds$start, new_bounds$end)
                         } else {
                           value <- paste(value, collapse="\n")
                           buffer$setText(value)
                         }
                       },
                       get_index = function(...) {
                         stop("Not defined")
                       },
                       set_index = function(value,...) {
                         stop("Not defined")
                       },
                       get_items = function(i, j, ..., drop=TRUE) {
                         stop("Not defined")
                       },
                       set_items = function(value, i, j, ...) {
                         stop("Not defined")
                       },
                       get_tag_name=function(name, value) {
                         "Return tag table name for passing to function"
                         value <- switch(name,
                                         "weight" = PangoWeight[value],
                                         "style"  = PangoStyle[value],
                                         "size"   = if(is.numeric(value)) {
                                           as.integer(value)
                                         } else {
                                           PangoScale[value] * 12
                                         },
                                         "scale"  = PangoScale[value],
                                         value)
                         name <- switch(name,
                                        "color"="foreground",
                                        "size" = "size-points",
                                        name)
                         nm <- sprintf("%s-%s", name, value)
                         if(is.null(tag_table$lookup(nm))) {
                           tt <- gtkTextTagNew(nm) # family-Monospace, say
                           tt[name] <- value
                           tag_table$add(tt)
                         }
                         nm
                       },
                       set_font = function(font.attr) {
                         "Set  font for selection or entire buffer if no selection"

                         font.attr <- sapply(font.attr, identity, simplify=FALSE)
                         if(length(font.attr) == 0)
                           return()

                         bounds <- buffer$GetSelectionBounds()
                         
                         if(bounds$retval == FALSE) {
                           ## if no text selected, we set for entire buffer
                           ## change entire buffer -- new as of 0.64
                           start <- buffer$GetStartIter()$iter
                           end <- buffer$GetEndIter()$iter
                           buffer$removeAllTags(start, end) # remove, the reset below
                         } else {
                           start <- bounds$start
                           end <- bounds$end
                         }

                         for(i in names(font.attr)) {
                           tag_nm <- get_tag_name(i, tolower(font.attr[i]))
                           buffer$ApplyTagByName(tag_nm, start, end)
                         }
                       },
                       insert_text=function(value, where, font.attr=NULL, do.newline,  ...) {
                         "Insert text into buffer. Font.attr is a list with named quantities" 
                         if(is_empty(value))
                           return()
                         
                          iter <- switch(where,
                                         "end"=buffer$GetEndIter()$iter,
                                         "beginning"=buffer$GetStartIter()$iter,
                                         buffer$getIterAtMark(buffer$getInsert())$iter)
            
                         value <- paste(c(value,""), collapse=ifelse(do.newline, "\n", ""))
                         arg_list <- list(object=buffer, iter=iter, text=value)

                         if(is.null(font.attr)) {
                           font.attr <- font_attr
                         } else {
                           font.attr <- sapply(font.attr, identity, simplify=FALSE)
                           font.attr <- gWidgets2:::merge.list(font_attr, font.attr)
                         }
                         if(length(font.attr) > 0) {
                           for(i in names(font.attr)) {
                             arg_list[[length(arg_list) + 1]] <- get_tag_name(i, font.attr[[i]])
                           }
                         }
                         do.call("gtkTextBufferInsertWithTagsByName",arg_list)                           

                         ## scroll to end -- if appended to end
                         if(where == "end") {
                           gdkWindowProcessAllUpdates()
                           while (gtkEventsPending())
                             gtkMainIterationDo(blocking=FALSE)
                           
                           end <- buffer$getEndIter()$iter
                           widget$scrollToIter(end, within.margin = 0, use.align=TRUE)
                         }
                         
                       },
                       add_handler_changed=function(handler, action=NULL, ...) {
                         add_handler_keystroke(handler, action=action, ...)
                       },
                       add_handler_selection_changed=function(handler, action=NULL, ...) {
                         message("No good signal for initiating this. Needs hacking XXX")
                       }
                       ))


  
