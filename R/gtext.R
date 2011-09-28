##' @include GWidget.R
NULL


## font sizes
fontSizes <- c(
               "xx-large"= PANGO_SCALE_XX_LARGE,
               "x-large" = PANGO_SCALE_X_LARGE,
               "large"   = PANGO_SCALE_LARGE,
                 "medium"  = PANGO_SCALE_MEDIUM,
               "small"   = PANGO_SCALE_SMALL,
               "x-small" = PANGO_SCALE_X_SMALL,
               "xx-small" = PANGO_SCALE_XX_SMALL
               )

## Make a tag table to be shared. We use memoise as we only need to make this once.
make_tag_table <- memoise(function() {
  ## font colors
  fontColors <-  sapply(colors(), identity)
  
  ## list of fonts
  font_list <- list(weight=PangoWeight,
                    style=PangoStyle,
                    family=sapply(c("sans", "helvetica", "times", "monospace"), identity),
                    scale=fontSizes,      # not size!
                    foreground=fontColors,
                    background=fontColors)
  
  ## global tag table
  tag_table <- gtkTextTagTableNew()
  ## populate tag table from above
  for(nm in names(font_list)) {
    for(i in names(font_list[[nm]])) {
      tt <- gtkTextTagNew(sprintf("%s-%s", nm, i)) # family-Monospace, say
      tt[nm] <- font_list[[nm]][i]
      tag_table$add(tt)
    }
  }
  
  tag_table
})

##' toolkit implementation
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
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
                       buffer="ANY"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         text = NULL, width = NULL, height = 300,
                         font.attr = NULL, wrap = TRUE,
                         handler=NULL, action=NULL, container=NULL, ...) {

                         buffer <<- gtkTextBufferNew(make_tag_table())
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
                           sw$SetSizeRequest(width,height)
                         
                         block$add(widget)
                         widget$show()

                         set_font(font.attr) # buffer font
                         insert_text(text, where="beginning", font.attr=NULL, do.newline=FALSE)
                         
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
                         value <- paste(value, collapse="\n")
                         buffer$setText(value)
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
                       set_font = function(font.attr) {
                         "Set  font for selection or entire buffer if no selection"

                         font.attr <- sapply(font.attr, identity, simplify=FALSE)
                         if(length(font.attr) == 0)
                           return()

                         tag_table <- buffer$getTagTable()
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
                           if(i == "size")
                             tag_nm <- sprintf("%s-%s", "scale", tolower(font.attr[i]))
                           else
                             tag_nm <- sprintf("%s-%s", i, tolower(font.attr[i]))
                           buffer$ApplyTagByName(tag_nm, start, end)
                         }
                       },
                       insert_text=function(value, where, font.attr=NULL, do.newline,  ...) {
                         "Insert text into buffer. Font.attr is a vector (or list) with named quantities" 
                         if(is_empty(value))
                           return()
                         
                          iter <- switch(where,
                                         "end"=buffer$GetEndIter()$iter,
                                         "beginning"=buffer$GetStartIter()$iter,
                                         buffer$getIterAtMark("insert"))
            
                         value <- paste(c(value,""), collapse=ifelse(do.newline, "\n", ""))
                         arg_list <- list(object=buffer, iter=iter, text=value)

                         if(!is.null(font.attr) && length(font.attr)) {
                           for(i in names(font.attr)) {
                             if(i == "size")
                               arg_list[[length(arg_list) + 1]] <- sprintf("%s-%s", "scale", tolower(font.attr[i]))
                             else
                               arg_list[[length(arg_list) + 1]] <- sprintf("%s-%s", i, tolower(font.attr[i]))
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
                       }
                       ))


  
