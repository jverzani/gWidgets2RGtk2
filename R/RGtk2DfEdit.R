# Written by Tom Taverner <Thomas.Taverner@pnl.gov>
# for the U.S. Department of Energy (PNNL, Richland, WA, USA)
# Website: http://omics.pnl.gov/software
#
# Notice: This computer software was prepared by Battelle Memorial Institute,
# hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with the
# Department of Energy (DOE).  All rights in the computer software are reserved
# by DOE on behalf of the United States Government and the Contractor as
# provided in the Contract.
#
# NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR
# IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
#
# This notice including this sentence must appear on any copies of this computer
# software.

#' A package for an editing data frames for RGtk2. Improves on base edit.data.frame function found in utils
#' @name RGtk2DfEdit-package
#' @docType package

# Update 8-4-10: Transient modal windows to prevent races while editing 
# big frames
# Should be able to specify the environment to update dataframe in
# Need redo stack
# Paste now turns NA into ""

DATA_OBJECTS = c("data.frame", "matrix", "array")

old_warning <- warning

STACK_LIMIT <- 1E8
# Don't bother drawing row selection rectangles if there are more than this
MAX_ROWS_TO_DRAW_SELECTION <- 1000
VERSION_STRING <- "version 0.5.5"

COLUMN_OFFSET <- 1

ToInternalColIdx <- function(x) x+COLUMN_OFFSET    
ToExternalColIdx <- function(x) x-COLUMN_OFFSET

# replace these with "" in pretty_print

SPRINTF_FORMAT <-  "%.4G"

if(.Platform$OS.type == "windows") {
  PLATFORM_OS_TYPE <- "windows"
} else if (.Platform$OS.type == "unix"){
  if (Sys.info()["sysname"] == "Darwin")
    PLATFORM_OS_TYPE <- "mac"
  else{ 
    PLATFORM_OS_TYPE <- "unix"    }
}

# for pasting
if(PLATFORM_OS_TYPE == "windows"){
  NEWLINE_CHAR <- "\r\n"
  HEADER_BOX_MARGIN <- 2 # how to get this?
} else {
  NEWLINE_CHAR <- "\n"
  HEADER_BOX_MARGIN <- 4
}

DO_CAIRO <- TRUE
if (Sys.info()["sysname"] == "Darwin")
  DO_CAIRO <- FALSE
  
DEFAULT_COLNAMES <-  c(LETTERS, sort(as.vector(outer(LETTERS, LETTERS, FUN=paste, sep=""))))

MergeAdjacent <- function(v, warn=T){
      dv <- diff(v)
      if(warn && sum(dv < 1)) 
        warning("Vector is not increasing")
    
      if(length(v)){
        w1 <- which(dv != 1)
        list(start=c(1, w1+1),
          end=c(w1, length(v)), 
          length=1+length(w1))
      } else {
        list(start=integer(0), end=integer(0), length=0)
      }
    }  
  
  # http://www.mail-archive.com/r-help@r-project.org/msg38009.html  
make.call = function(func, my.arg){
  Call <- match.call()
  fn <- deparse(substitute(func))
  Call[[1]] <- as.name(fn)
  Call$func <- NULL
  Call$my.arg <- NULL
  my.formals <- formals(func)
  Call[[2]] <- as.name(deparse(substitute(my.arg)))
  return(Call)
}

findxInParseTree <- function(txt){
  .env00 <- new.env() 
  .env00$flag <- FALSE  
  dfs <- function(ll){
    if(!is.call(ll)) {
      if (length(ll) && as.character(ll) == "x") .env00$flag <- TRUE
      return()
    } else {
      for(ii in 1:length(ll)) dfs(ll[[ii]])
    }
  }
  pt <- as.list(parse(text=txt))
  dfs(pt[[1]])
  return(.env00$flag)
}

  # Remove leading and trailing white space from text and replace all blanks with "NA"
StripBlanks <- function(x){
  x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
  x[x=="NA"|nchar(x) == 0] <- NA 
  return(x)
}

# Keep all our coercions in here
# to.levels means you're turning a factor into its levels, rather than
#   as.integer()
# Coercing a factor to a factor will keep its level ordering.
myDataTypeCoercions <- function(typ, x, to.levels=FALSE){
  rv <- NA
  tryCatch(
    if(to.levels){
      stopifnot(is.factor(x))
      rv <- levels(x)[as.integer(x)]
      rv <- myDataTypeCoercions("character", rv)
    } else if(typ=="integer"){
      rv <- as.integer(x)
    } else if (typ== "logical"){
      rv <- as.logical(x)
    } else if (typ=="numeric") {
      rv <- as.numeric(x)
    } else if ("factor"%in%typ){
      sbx <- StripBlanks(x)    
      if(inherits(x, "factor")){
        theLevels = unique(StripBlanks(levels(x)))
      } else {
        theLevels = unique(sbx, na.last = TRUE)
      }
      rv <- factor(sbx, levels=theLevels)
    } else {
      x <- as.character(x) 
      x[is.na(x)] <- ""
      rv <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)  
      rv[nchar(rv) == 0] <- ""
    }, warning = function(w) return(rv))
  return(rv)
}

GetClasses <- function(x){
 nc <- ncol(x)
 rv <- rep(NA, nc)
 for(jj in 1:nc){
   cc <- x[,jj]
   cx <- class(cc)
     # class might be "ordered factor"   
   if("factor"%in%cx) {   
     cx <- "factor"
   } else if(!is.atomic(cc) || length(cx) != 1) {
     cx <- "character"
   }
   rv[jj] <- cx
 }
 return(rv)
}
  
# Coerce frame2 to theClasses1 column classes 
CoerceDataTypes <- function(frame2, theClasses1, to.levels=FALSE){ 
  stopifnot(is.data.frame(frame2))
  theClasses2 <- GetClasses(frame2)
  if(NA%in%theClasses1) stop("Trying to coerce to NA class") 
  stopifnot(length(theClasses1) == length(theClasses2))
  xx <- theClasses1 != theClasses2  
  xx[is.na(xx)] <- TRUE 
  for(jj in which(xx)){ 
    frame2[,jj] <- myDataTypeCoercions(theClasses1[jj], frame2[,jj], to.levels)
  } # for jj  
  return(frame2) 
}

# We normally coerce when we change cells, but sometimes we might want to paste
# and accept the default data frame coercion
ChangeCells <- function(df, nf, row.idx, col.idx, do.coercion=T){ 
  dmm <- dim(df)
  if(!is.data.frame(nf)){
    nf <- data.frame(nf)
  }                                 
  if(missing(row.idx)) row.idx <- 1:dmm[1] 
  if(missing(col.idx)) col.idx <- 1:dmm[2]
  stopifnot(ncol(nf) == length(col.idx) && nrow(nf) == length(row.idx)) 

  oldf = df[row.idx, col.idx, drop=F]
 
  theClasses <- GetClasses(df) 

  if(do.coercion){
    nf <- CoerceDataTypes(nf, theClasses[col.idx])
  }
  
  idxf <- which(theClasses == "factor")
  idxf <- idxf[idxf%in%col.idx]
  
  if(length(idxf)){
    for(jj in idxf){
      nf.col = which(col.idx==jj)
      xx <- df[[jj]] 
      lvls <- levels(xx) 
      to.model <- as(nf[,nf.col], class(lvls)) 
      to.model[!nchar(to.model)] <- NA  # 3-16-10
        # we're changing the levels
      if((!all(to.model%in%lvls))||(!all(xx[row.idx]%in%xx[-row.idx]))){  
        x <- as.vector(xx) 
        x[row.idx] <- to.model 
        df[,jj] <- myDataTypeCoercions("factor", x)
      } else {
        df[row.idx,jj] <- to.model
      }    
    } 
    cc <- !col.idx%in%idxf 
    df[row.idx, col.idx[cc] ] <- nf[,cc,drop=F]
  } else {
    df[row.idx, col.idx] <- nf
  }

  return(list(df = df,  
    undo = list( 
      func = "ChangeCells", 
      args = list(nf=oldf, row.idx=row.idx, col.idx=col.idx, do.coercion=T) 
    ) 
  )) 
}


SetFactorAttributes <- function(df, col.idx, info){ 
  idx <- col.idx
  theCol <- df[[idx]]
  stopifnot("factor"%in%class(theCol))
  lvls <- levels(theCol)
  old <- list(levels=lvls)
  if(length(lvls) > 1){
    old$contrasts <- contrasts(theCol)
    old$contrast.name <- attr(theCol, "contrast.name")
  }

  if(identical(info$ordered, TRUE)) 
    theCol <- as.ordered(theCol)    
  if(!is.null(info$levels))
    theCol <- factor(theCol, levels=info$levels)
  if(!is.null(info$contrasts))
    contrasts(theCol) <- info$contrasts
  if(!is.null(info$contrast.name)) 
    attr(theCol, "contrast.name") <- info$contrast.name
  
  df[[idx]] <- theCol

  return(list(df = df,  
    undo = list( 
      func = "SetFactorAttributes", 
      args = list(col.idx=idx, info=old) 
    ) 
  )) 
}

  # to.levels: when coercing a factor, use levels(x)[as.numeric(x)]
CoerceColumns <- function(df, theClasses, col.idx, to.levels=FALSE){
  idx <- col.idx
  if(length(theClasses) == 1 && length(idx) > 1) 
    theClasses <- rep(theClasses, length(idx))

  stopifnot(length(idx) == length(theClasses)) 
  stopifnot(max(idx) <= ncol(df))

  old.c <- GetClasses(df)[idx] # the previous class of the column
  df[,idx] <- CoerceDataTypes(df[,idx,drop=F], theClasses, to.levels)

  return(list(df = df,
    undo=list(  
      func = "CoerceColumns", 
      args = list(theClasses = old.c, col.idx=idx)
    ) 
  )) 
} 

ChangeColumnNames <- function(df, theNames, col.idx){
  idx <- col.idx
  stopifnot(length(idx) == length(theNames)) 
  stopifnot(max(idx) <= ncol(df))

  oldNames <- colnames(df)[idx] 
  colnames(df)[idx] <- theNames 
 
  return(list(df = df,
    undo=list(  
      func = "ChangeColumnNames", 
      args = list(theNames = oldNames, col.idx=idx)
    ) 
  )) 
} 
 
ChangeRowNames <- function(df, theNames, row.idx){
  idx <- row.idx
  stopifnot(length(idx) == length(theNames))   
  stopifnot(max(idx) <= nrow(df))
  rdf <- rownames(df)
  oldNames <- rdf[idx] 
  theNames <- make.unique(c(rdf[-idx], theNames))[length(rdf)+1:length(idx)-length(idx)] 
  rownames(df)[idx] <- theNames
  df[idx, 1] <- theNames
  return(list(df = df,
    undo=list(  
      func = "ChangeRowNames", 
      args = list(theNames = oldNames, row.idx=idx)
    ) 
  )) 
} 

# Makes indexes  
InsertIndex <- function(orig, insertions){ 
  all.idx <- 1:orig 
  li <- 1:length(insertions) 
  del.idx <- insertions + li - 1 
  ins.idx <- orig + li 
  for(jj in li) 
    all.idx <- append(all.idx, ins.idx[jj], after=del.idx[jj]-1) 
  return(all.idx) 
} 
# deleting indexed rows and columns 
DeleteRows <- function(df, row.idx){
  idx <- row.idx
  
  new_df <- df[-idx,,drop=F]

    # New code: set this flag to TRUE if the row names are 1:dim(df)[1]
    # and remember that the last rowname is actually " ", so omit it
  row_names_null_flag <- identical(rev(rownames(df))[-1], as.character((dim(df)[1]-1):1))
    # insert the new frame into the old frame at the insertion position
  if(row_names_null_flag && dim(new_df)[1]>1){
    #cat("row names are null\n")
    change.idx <- 1:(dim(new_df)[1]-1)
    rownames(new_df)[change.idx] <- new_df[change.idx,1] <- as.character(change.idx)
  }
        
  list(df = new_df,
    undo = list( 
      func = "InsertRows", 
      args = list(
        nf = df[idx,,drop=F],
        row.idx=idx-1:length(idx)+1)))
}

InsertRows <- function(df, nf, row.idx){
  idx <- row.idx
  ddf <- dim(df)
#  print("InsertRows")
#  print(dim(df))
#  print(dim(nf))
#  print(row.idx)
  stopifnot(ddf[2] == dim(nf)[2] && dim(nf)[1] == length(idx))
  colnames(nf) <- colnames(df)
  xx <- InsertIndex(ddf[1], idx)
  nf <- CoerceDataTypes(nf, GetClasses(df))  
  new_df <- rbind(df, nf)[xx,,drop=F]
  
  row_names_null_flag <- identical(rev(rownames(df))[-1], as.character((dim(df)[1]-1):1))
  if(row_names_null_flag && dim(df)[1]>1){
    change.idx <- 1:(dim(new_df)[1]-1)
    rownames(new_df)[change.idx] <- new_df[change.idx,1] <- as.character(change.idx)
  }    
  new_df[,1] <- rownames(new_df)   
  
  list(df=new_df,  
    undo = list( 
      func = "DeleteRows", 
      args = list(
        row.idx=idx+1:length(idx)-1)))
}

InsertNARows <- function(df, row.idx){
  idx <- row.idx
  ddf <- dim(df)
  xx <- InsertIndex(ddf[1], idx)  
  lidx <- length(idx)
    
  nf <- data.frame(rbind(rep(NA, ddf[2])))[rep(1, lidx),]
  colnames(nf) <- colnames(df)
  rownames(nf) <- make.unique(c(rownames(df), idx+1:lidx-1))[ddf[1]+1:lidx]
  nf[,ddf[2]] <- ""
  nf[,1] <- rownames(nf)
  nf <- CoerceDataTypes(nf, GetClasses(df))
  
  new_df = rbind(df, nf)[xx,,drop=F]

    # New code: set this flag to TRUE if the row names are 1:dim(df)[1]
    # and remember that the last rowname is actually " ", so omit it
  row_names_null_flag <- identical(rev(rownames(df))[-1], as.character((dim(df)[1]-1):1))
    # insert the new frame into the old frame at the insertion position
  if(row_names_null_flag && dim(df)[1]>1){
    #cat("row names are null\n")
    change.idx <- 1:(dim(new_df)[1]-1)
    rownames(new_df)[change.idx] <- new_df[change.idx,1] <- as.character(change.idx)
  }
    
  list(df = new_df,  
    undo = list( 
      func = "DeleteRows", 
      args = list(
        row.idx=idx+1:length(idx)-1)))
}


DeleteColumns <- function(df, col.idx) {
  idx <- col.idx
  list(df = df[,-idx,drop=F],  
    undo = list( 
      func = "InsertColumns", 
      args = list(
        nf = df[,idx,drop=F],
        col.idx=idx-1:length(idx)+1))) 
}

InsertColumns <- function(df, nf, col.idx){
  idx <- col.idx
  ddf <- dim(df)
#  print("InsertColumns")
#  print(ddf)
#  print(dim(nf))
#  print(length(idx))
  stopifnot(ddf[1] == dim(nf)[1] && dim(nf)[2] == length(idx))
  xx <- InsertIndex(ddf[2], idx) 
  list(df = cbind(df, nf)[,xx,drop=F],  
    undo = list( 
      func = "DeleteColumns", 
      args = list(
        col.idx=idx+1:length(idx)-1)))
}

InsertNAColumns <- function(df, col.idx, NA.opt=""){
  idx <- col.idx
  ddf <- dim(df)
  xx <- InsertIndex(ddf[2], idx) 
  lidx <- length(idx)
  nf <- data.frame(X=cbind(rep(NA.opt, ddf[1])),stringsAsFactors=FALSE)[,rep(1, length(idx)),drop=F]
  colnames(nf) <- make.unique(c(colnames(df), DEFAULT_COLNAMES[idx+1:lidx-2]))[ddf[2]+1:lidx]

  list(df = cbind(df, nf)[,xx,drop=F],  
    undo = list( 
      func = "DeleteColumns", 
      args = list(
        col.idx=idx+1:length(idx)-1)))
}

# insert new frame dat
GetTaskPasteIn <- function(theFrame, dat, insert.row, insert.col, 
  do.rownames = F, do.colnames = F, do.coercion=T){ 

  stopifnot(class(dat) == "data.frame")

  theRownames <- NULL 
  theColnames <- NULL 
  if(do.rownames)
    theRownames <- rownames(dat)
  if (do.colnames)
    theColnames <- colnames(dat) 
   
  dd <- dim(dat) 
  dm <- dim(theFrame)
  if (is.null(insert.row) ) insert.row <- 1
  if (is.null(insert.col)) insert.col <- 1
  ins <- c(insert.row, insert.col)
  
  ins.end <- c(ins[1]+dd[1]-1, ins[2]+dd[2]-1) # end of insertion range  

  tasks <- list()
  
  if(dm[1] < ins[1]+dd[1]){ # Rows
    idx <- (dm[1]+1):(ins[1]+dd[1])
    idx <- rep(dm[1], length(idx))
    tasks[[length(tasks)+1]] <- list(func="InsertNARows", args=list(row.idx=idx)) 
  }
  if(dm[2] < ins[2]+dd[2]){ # Columns
    idx <- (dm[2]+1):(ins[2]+dd[2])
    idx <- rep(dm[2], length(idx))
    tasks[[length(tasks)+1]] <- list(func="InsertNAColumns", args=list(col.idx=idx))
  }

  row.idx <- ins[1]:ins.end[1]
  col.idx <- ins[2]:ins.end[2]
  
  if(do.rownames && !is.null(theRownames))
    tasks[[length(tasks)+1]] <- list(func="ChangeRowNames", 
        arg = list(theNames = theRownames, row.idx=row.idx))

  if(do.colnames && !is.null(theColnames))
    tasks[[length(tasks)+1]] <- list(func="ChangeColumnNames", 
        arg = list(theNames = theColnames, col.idx=col.idx))  
  
  tasks[[length(tasks)+1]] <- list(func="ChangeCells", 
    args=list(nf=dat, row.idx=row.idx, col.idx=col.idx, do.coercion=do.coercion))
    
  return(tasks)
} 

# Do a user call
DoUserCall <- function(call_name, fargs, .local){
  handler <- .local$changed.handler[[call_name]]  
  if(!is.null(handler$func) && is.function(handler$func) ){
    fargs <- append(list(obj = .local$group.main), fargs)
    if(!is.null(handler$data))
      fargs <- append(fargs, list(data=handler$data))

    tryCatch({
      do.call(handler$func, fargs)
    }, error = function(e) warning(e))
  }
}
  
# Do a task list
DoTask <- function(.local, df, task, handler=NULL){
  w <- TransientWindow("Updating...", .local)
  on.exit(w$destroy())
  
  undo <- list()
  for(taskItem in task){
    arg <- taskItem$arg
    func.name <- taskItem$func
    arg$df <- df     
            
    # This can fail when it's too big
    rv <- do.call(taskItem$func, arg) 
    df <- rv$df 
    undo[[length(undo)+1]] <- rv$undo
        
    if("col.idx"%in%names(taskItem$arg)) taskItem$arg$col.idx <- ToExternalColIdx(taskItem$arg$col.idx)
    DoUserCall(func.name, taskItem$arg, .local)
    if("col.idx"%in%names(taskItem$arg)) taskItem$arg$col.idx <- ToInternalColIdx(taskItem$arg$col.idx)
    
  }
  return(list(df=df, undo=undo))
}
 

  bgColor <- list(as.GdkColor(c(237,236,235)*256),as.GdkColor(c(235,234,219)*256))[[(PLATFORM_OS_TYPE == "windows")+1]]
  selectedColor <- as.GdkColor(c(198, 213, 253)*256) # Linux
  whiteColor <- as.GdkColor(c(255, 255, 255)*256)
  #selectedColor <- as.GdkColor(c(255,255,255)*256) # Linux
  selectedColumnColor <- as.GdkColor(c(198, 213, 253)*256) # Linux  
  selectedTextColor <- as.GdkColor("black")
  
  myValidInputKeys <- c(GDK_space:GDK_asciitilde, GDK_Delete, GDK_BackSpace)
  myMetaKeys <- c(GDK_Shift_L, GDK_Shift_R, GDK_Control_L, GDK_Control_R)
  myShiftKeys <- c(GDK_Shift_L, GDK_Shift_R)
  myValidNavigationKeys <- c(GDK_Down, GDK_Left, GDK_Up, GDK_Right,
  GDK_Page_Up, GDK_Page_Down, GDK_Return, GDK_ISO_Left_Tab, GDK_Tab, GDK_Home, GDK_End)
  myValidKeys <- c(myValidInputKeys, myMetaKeys, myValidNavigationKeys, GDK_Insert)
  exclude.factor <- NA

#' Convenience function to call data frame editor in its own window
#'
#' @param items A data frame (?) to display graphically
#' @param dataset.name Name for data set
#' @param size (height, width)
#' @param col.width (width)
#' @return An object of class GtkDfEdit for which a few RGtk2-style methods are defined
#' @export
dfedit <- function(items, dataset.name = deparse(substitute(items)), 
  size=c(500, 300), col.width = 64, pretty_print=TRUE, sprintf_format="%.6G"){
  
  if(missing(items)||is.null(items)) {
    items <- data.frame(NULL)
    dataset.name <- "new.data.frame"
  }

  obj <- gtkDfEdit(items, dataset.name, size.request=size, col.width = col.width, pretty_print=pretty_print, sprintf_format=sprintf_format)
  dialog <- gtkDialog(dataset.name, NULL, "modal", "Close", 1,show = FALSE)    
  dialog$setPosition(GtkWindowPosition["center-on-parent"])
  dialog[["vbox"]]$add(obj)
    
  #win <- gtkWindowNew()  
  #win$setTitle(dataset.name)
  dialog$run()
  rv <- invisible(obj$getDataFrame())
  dialog$destroy()
  invisible(rv)
}
   
# Our handler:
#      isText <- theClass == "factor" || theClass == "character"
#	    renderer <- .local$allColumns[[kk-1]]$renderer
#      renderer.set <- renderer['ellipsize-set']
#      if( isText && !renderer.set) {
#	        renderer['ellipsize'] <- PangoEllipsizeMode['end']
#	        renderer['ellipsize-set'] <- TRUE
#      } else if (!isText && renderer.set) {
#	        renderer['ellipsize'] <- PangoEllipsizeMode['none']
#	        renderer['ellipsize-set'] <- FALSE
#      }

DoUndo <- function(.local){  
  if(!length(.local$undoStack)) return(TRUE)
  undo <- .local$undoStack[[length(.local$undoStack)]]
  rv <- DoTask(.local, .local$theFrame, rev(undo), .local$changed.handler)
  UpdateDfEditor(.local, rv$df)
   if(length(.local$undoStack))
     .local$undoStack[[length(.local$undoStack)]] <- NULL
}

TransientWindow <- function(txt, .local){
    w <- gtkWindowNew("", show=F); w$setDecorated(FALSE); w$add(gtkLabelNew(txt))
    w$setPosition(GtkWindowPosition["center-on-parent"])              
    w$setTransientFor(.local$toplevel); w$setModal(TRUE); w$showAll()
    return(w)
}

# restrict means you can't select the last index
GetSelectedRows <- function(tv, .local, restrict=TRUE){
   # block transient interactions with a popup
#  w <- TransientWindow("Getting row selection...", .local)  
#  on.exit(w$destroy())  
  sr <- integer(0)
  #tryCatch({    
    rv <- gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(tv))$retval
    if(length(rv)){
      sr <- sapply(rv, gtkTreePathGetIndices)
      if(is.numeric(sr)) {
        sr <- sr + 1
        if(restrict) sr <- sr[!sr%in%dim(.local$theFrame)[1]]
      } else {
        sr <- integer(0)        
      }
    }
  #},
  #error=function(e) {
  #  warning(e)
  #  integer(0)
  #})
  return(sr)
}
    
# Make a data frame to put into the model.
# We have an extra row for names and a blank at the end.
# Also, data frames with all-NAs turn into logical, which displays badly.
MakeInternalDataFrame <- function(dataset, add.rows=T, add.columns=T, NA_replace = NA_character_){
  
  if(is.null(dim(dataset))) dataset <- cbind(dataset)
  if(!length(rownames(dataset)) && dim(dataset)[1])
      rownames(dataset) <- 1:dim(dataset)[1]
  if(!length(colnames(dataset)) && dim(dataset)[2])
      colnames(dataset) <- DEFAULT_COLNAMES[1:dim(dataset)[2]]
      
   # Fix bug with NA row name, 09-15-2010
  row_names_dataset <- row.names(dataset)
  row.problem.idx <- is.na(row_names_dataset) | duplicated(row_names_dataset)
  if(any(row.problem.idx)){
    dataset <- dataset[!row.problem.idx,,drop=F]
    message(paste("Removed missing or duplicated row names at positions:", paste(which(row.problem.idx), collapse=", ")))
  }  

  if(add.columns){
   theFrame <- data.frame(rows = row.names(dataset), dataset, 
     " " = vector("character", nrow(dataset)), 
     check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    theFrame <- data.frame(dataset, stringsAsFactors=FALSE)
  }
  
    # turn all-NA columns to whatever default we want
#  if(dim(theFrame)[2] > 0) {
#    which.allNA <- which(colSums(is.na(theFrame)) == dim(theFrame)[1])
#    for(jj in which.allNA) theFrame[,jj] <- NA_replace
#  }
#
  theClasses <- GetClasses(theFrame)  
  for(jj in which(theClasses == "character")) 
    theFrame[,jj] <- myDataTypeCoercions("character", theFrame[,jj])
  
  if(add.rows){ 
    if (add.columns){
		  blankRow <- data.frame(rows=" ", rbind(rep(NA, dim(dataset)[2])), " " = "", row.names = " ", stringsAsFactors=F)
   } else {
		blankRow <- data.frame(rbind(rep(NA, dim(dataset)[2])), stringsAsFactors=F)
   }
		blankRow <- CoerceDataTypes(blankRow, theClasses)
		for(jj in which(theClasses == "factor")){
  		theFrame[,jj] <- myDataTypeCoercions("factor", theFrame[,jj])
		  blankRow <- SetFactorAttributes(df=blankRow, col.idx=jj, 
        info=list(levels=levels(theFrame[,jj]), ordered=is.ordered(theFrame[,jj])))$df  
      }
		names(blankRow) <- names(theFrame)
		theFrame <- rbind(theFrame, blankRow)
  }
  theFrame
}

# Make a data frame to extract from the model.
MakeExternalDataFrame <- function(theFrame, .local){

  dataset.class <- .local$dataset.class
  new.frame <- theFrame[-dim(theFrame)[1],-c(1, dim(theFrame)[2]), drop=F]
  tryCatch({
    if(length(dataset.class)==0 || "data.frame"%in%dataset.class){ 
      class(new.frame) <- dataset.class
    } else if(length(dataset.class)==1) {
      new.frame <- as(new.frame, dataset.class)
    }}, error = function(e) warning(paste("Couldn't create data of class", paste(dataset.class, collapse = ", "))))

    # Copy any other attributes over
  dataset.attributes <- .local$dataset.attributes
  copy.attr <- dataset.attributes[!names(dataset.attributes)%in%c('class', 'dim', 'dimnames', 'names', 'row.names')]    
  if(length(copy.attr)) for(ii in 1:length(copy.attr)) attr(new.frame, names(copy.attr)[ii]) <- copy.attr[[ii]]

  return(new.frame)
}

MakeLastPath <- function(df){
    if(dim(df)[1] == 1) return(gtkTreePathNewFromString("0"))
    return(gtkTreePathNewFromString(as.character(dim(df)[1]-2)))
}

quick_entry <- function(msg, handler, data=NULL, win=NULL) {
  dialog <- gtkDialog(msg, NULL, c("destroy-with-parent"), "gtk-ok", 1, "gtk-cancel", 0, show = F)
  #dialog$setDecorated(FALSE)
  entry = gtkEntryNew()        
  entry$setSizeRequest(300, -1)  
  gSignalConnect(entry, "key-press-event", function(obj, evt){
    if(evt[["keyval"]] == GDK_Return) {
      handler(entry$getText(), data)
      dialog$destroy()     
    }
    if(evt[["keyval"]] == GDK_Escape) {
      dialog$destroy()     
    }
    return(FALSE)
    })
   
  if(!is.null(win)) {
     checkPtrType(win, "GtkWindow")
     dialog$setPosition(GtkWindowPosition["center-on-parent"])
     dialog$setTransientFor(win)
  }
  gSignalConnect(dialog, "response", function(dlg, arg1, user.data) {
     if(arg1==1)
       handler(entry$getText(), user.data)
     dialog$destroy()
  }, data=data)                                    
  dialog[["vbox"]]$packStart(entry, TRUE, TRUE, 10)
  dialog$showAll()
}

quick_query <- function(message, handler, data, win=NULL) {
  dialog <- gtkDialog("Query", NULL, c("modal", "destroy-with-parent"), "gtk-ok", 1, "gtk-cancel", 0, show = F)
  label <- gtkLabel(message)     
  if(!is.null(win)) {
     checkPtrType(win, "GtkWindow")
     dialog$setPosition(GtkWindowPosition["center-on-parent"])
     dialog$setTransientFor(win)
  }
  gSignalConnect(dialog, "response", function(dlg, arg1, user.data) {
    if(arg1) handler(data)
  dialog$destroy()
  })
  dialog[["vbox"]]$packStart(label, TRUE, TRUE, 10)
  dialog$showAll()
}

quick_message <- function(message, win=NULL) {
  dialog <- gtkDialog("Message", NULL, c("modal","destroy-with-parent"), "gtk-ok", 1,
                      show = FALSE)
  label <- gtkLabel(message)     
  if(!is.null(win)) {
     checkPtrType(win, "GtkWindow")
     dialog$setPosition(GtkWindowPosition["center-on-parent"])
     dialog$setTransientFor(win)
  }
  gSignalConnect(dialog, "response", gtkWidgetDestroy)  
  dialog[["vbox"]]$packStart(label, TRUE, TRUE, 10)
  dialog$showAll()
}
    
#warning <- quick_message

CtrlLetter <- function(keyval, stat, let)
  keyval == let && (as.flag(stat) & GdkModifierType['control-mask'])
ShiftLetter <- function(keyval, stat, let)
  keyval == let && (as.flag(stat) & GdkModifierType['shift-mask'])  
CtrlShiftLetter <- function(keyval, stat, let)
  keyval == let && (as.flag(stat) & GdkModifierType['control-mask'] & GdkModifierType['shift-mask'])

   # this is all horribly platform dependent
CopyToClipboard <- function(dat, do.rownames=F, do.colnames=F){
   
  write.function <- function(dat, p)
    write.table(dat, p, row.names=F, col.names=F, quote=F, sep="\t")

  if(do.rownames) {
    t.dat <- t(cbind(rownames(dat), dat))
  } else {
    t.dat <- t(dat)      
  }

  dat2 <- paste( apply(t.dat, 2, function(ll) {
      ll[is.na(ll)] <- ""
      paste(ll, collapse="\t")
    }), collapse=NEWLINE_CHAR)
        
  if(do.colnames) {
    dat.cn <- ""
    if(do.rownames) dat.cn <- "\t" 
    dat.cn <- paste(dat.cn, paste(colnames(dat), collapse="\t"), sep="")
    dat2 <- paste(dat.cn, dat2, sep=NEWLINE_CHAR)
  }
  
  if(.Platform$OS.type == "windows") {
    #write.function(dat, "clipboard")
    get("writeClipboard", envir=.GlobalEnv)(dat2)
  } else if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin")
      a <- pipe("pbcopy", "w")
    else {
      if(!length(system('which xclip', intern=T))){
        quick_message("xclip must be installed to copy")
        return(FALSE)
      }
      a <- pipe("xclip -selection c", open="w")
  }
    write.function(dat2, a)
    close(a)
  }
}  

# Where to read from
GetPasteClipboardPipe <- function(){
  if(PLATFORM_OS_TYPE == "windows") {
    p <- "clipboard"
  } else if (PLATFORM_OS_TYPE == "unix"){
    if(!length(system('which xsel', intern=T))){
      quick_message("xsel must be installed to paste")
      return(FALSE)
    }
    p <- pipe("xsel -o -b", open="r")
  } else if (PLATFORM_OS_TYPE == "mac"){
    p <- pipe("pbpaste")
  } else{ 
    stop("Unrecognized platform type")
  }
}

CloseClipboardPipe <- function(p)
  if (PLATFORM_OS_TYPE == "unix" || PLATFORM_OS_TYPE=="mac" )
    close(p)

# Args are arguments to read.table
ReadFromClipboard <- function(...){
     # block transient interactions with a popup
  dat <- NULL
  b <- GetPasteClipboardPipe()
  tryCatch(dat <- read.table(b, ...), 
  error = function(e) print(e),
  finally = CloseClipboardPipe(b))
  return(dat)
}
          
# from gWidgets
gtkMenuPopupHack <- function (object, parent.menu.shell = NULL, parent.menu.item = NULL, 
  func = NULL, data = NULL, button, activate.time) {
  checkPtrType(object, "GtkMenu")
  if (!is.null(parent.menu.shell)) 
      checkPtrType(parent.menu.shell, "GtkWidget")
  if (!is.null(parent.menu.item)) 
      checkPtrType(parent.menu.item, "GtkWidget")
  if (!is.null(func)) 
     func <- as.function(func)
  button <- as.numeric(button)
  activate.time <- as.numeric(activate.time)
  w <- .RGtkCall("S_gtk_menu_popup", object, parent.menu.shell, 
      parent.menu.item, func, data, button, activate.time, 
      PACKAGE = "RGtk2")
  return(invisible(w))
}


###############################################################################
# Factor editor
###############################################################################
##########################################
# Blocking editor

BlockSizeHandler <- function(the.column, data){
  .local <- data$.local
  row.idx = data$row.idx
  col.idx = data$col.idx
  entry.frame <- data.frame(X=the.column)
  task <- list(
   list(func="ChangeCells", 
    arg = list(nf=entry.frame, row.idx=row.idx, col.idx=col.idx))
   )
  DoTaskWrapper(.local, task)
}

 # start.lvl = level name to start cycling at
DoBlockSize <- function(the.column, loc.window, handler, data, start.lvl=NULL){

  .BlockEnv <- new.env()
  .BlockEnv$the.column <- the.column 
  .BlockEnv$data <- data

  UpdateColumn <- function(block.size, dummy){
    total.len <- length(.BlockEnv$the.column)
    .BlockEnv$the.column <-  gl(length(lvls), block.size, total.len, labels=lvls)
    #handler(the.column, data)
  }
  
  if(!is.factor(the.column)) stop("Can't block over non-factors")  
  lvls <- unique(levels(the.column))
  if(!is.null(start.lvl)){
    if(!start.lvl%in%lvls) stop("start.level must be in levels")
    ww <- which(lvls == start.lvl)[1]
    if(ww > 1) lvls <- c(lvls[ww:length(lvls)], lvls[1:(ww-1)])
  }
  UpdateColumn(1)
  
  ok.handler <- function(block.size, dummy){
    handler(.BlockEnv$the.column, .BlockEnv$data) 
  }
 
  MakeSpinDialog(loc.window, title = "Blocking", label="Select Block Size", spin.handler=UpdateColumn, ok.handler=ok.handler, data=NULL)
}

MakeSpinDialog <- function(loc.window, title, label, spin.handler, ok.handler, data=NULL, maximum=100.0, minimum=1.0){
  window2 <- gtkWindowNew(show=F)
  window2$setTitle(title)
  window2$setPosition(GtkWindowPosition["center-on-parent"])  
  window2$setTransientFor(loc.window)
  window2$setModal(TRUE)
  box0 <- gtkVBoxNew(FALSE, 5)
  
  window2$add(box0)
  window2$setResizable(FALSE)
  window2$setPosition(GtkWindowPosition["center-on-parent"])  
  if(!is.null(loc.window)) window2$setTransientFor(loc.window)
  window2$setModal(TRUE)

  box1 <- gtkHBoxNew(FALSE, 5)
  box0$packStart(box1, FALSE, FALSE, 10)
  box1$packStart(gtkLabelNew(label), FALSE, FALSE, 10)
  spinner_adj <- gtkAdjustment(1, 1, maximum, minimum, 5.0)
  spinner <- gtkSpinButton(spinner_adj, 1.0, 0)
  spinner$setValue(1)
  box1$add(spinner)
  spinner$grabFocus()

  box2 <- gtkHBoxNew(FALSE, 5)
  box0$packEnd(box2, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("    OK    ")
  gSignalConnect(spinner, "value-changed", function(...){
    spin.handler(spinner$getValue(), data)
  })
  gSignalConnect(spinner, "key-press-event", function(obj, evt){
    if(evt[["keyval"]] == GDK_Return) {
      spinner$update()
      ok.handler(spinner$getValue(), data) 
      window2$destroy()
    }
    if(evt[["keyval"]] == GDK_Escape) {
      window2$destroy()
    }    
    FALSE
  })
  gSignalConnect(button, "clicked", function(handler, data){
    spinner$update()  
    ok.handler(spinner$getValue(), data)
    window2$destroy()
  })
  
  box2$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("   Cancel   ")
  gSignalConnect(button, "clicked", function(...){
    window2$destroy()
    })
  box2$packEnd(button, FALSE, FALSE, 5)
  window2$show()
}

FactorEditorHandler <- function(the.column, col.idx, data){
  .local <- data
  #print(.FactorEnv$the.column)
#  the.column <- myDataTypeCoercions("factor", the.column)
#  x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
#  x[x=="NA"|nchar(x) == 0] <- NA
  entry.frame <- data.frame(X=the.column) 
  the.contrasts <- NULL
  if(length(levels(the.column)) > 1) the.contrasts <- contrasts(the.column)
  task <- list(list(func="ChangeCells", 
     arg = list(nf=entry.frame, col.idx=col.idx)),
    list(func="SetFactorAttributes", 
    arg = list(col.idx=col.idx, info=list(levels=levels(the.column), 
     contrasts=the.contrasts, contrast.name=attr(the.column, "contrast.name"), ordered=is.ordered(the.column))))
     )
  DoTaskWrapper(.local, task)
}

DoFactorEditor <- function(theFrame, toplevel, col.idx=integer(0), 
  handler=NULL, data=NULL){

  .FactorEnv <- new.env()
  .FactorEnv$col.idx <- col.idx

  UpdateView <- function(){
    contr <- contrast.coding[[which(sapply(rev(grb$getGroup()), gtkToggleButtonGetActive))]]
    x <- .FactorEnv$the.column
    if(ordered.checkbox$getActive() != is.ordered(x))
      x <- factor(x, levels = levels(x), ordered = ordered.checkbox$getActive())
    if(length(levels(x)) > 1) {
      contrasts(x) <- contr
      attr(x, "contrast.name") <- contr
    }
    handler(x, .FactorEnv$col.idx , data)
    .FactorEnv$the.column <- x
  }

  cell.edited <- function(cell, path.string, new.text, data){
    xx <- .FactorEnv$xx      
    if(!nchar(new.text))# || new.text%in%xx) 
      stop("New name must exist")# and be unique")
    checkPtrType(data, "GtkListStore")
    model <- data
    path <- gtkTreePathNewFromString(path.string)
    iter <- model$getIter(path)$iter

    i <- path$getIndices()[[1]]+1

      # editing the level names
    zz <- theFrame[,.FactorEnv$col.idx]
    lzz <- levels(zz)    
    ww <- which(xx[i]==lzz)
    
    if(length(ww)){
      lzz[ww] <- new.text
      levels(zz) <- lzz
      .FactorEnv$the.column <- zz
      UpdateView()            
    }
    xx[i] <- new.text
    model$set(iter, 0, new.text)
    .FactorEnv$xx <- xx
    UpdateLabel()    
  }
    
  add.item <- function(button, data) {
    xx <- .FactorEnv$xx      
     for(k in 1:(length(xx)[1]+1)){
       nl <- paste("Level", k, sep="_")
       if(!nl%in%xx) break
     }
     xx <- c(xx, nl)
     .FactorEnv$xx <- xx     
     iter <- model$append()$iter
     model$set(iter, 0, xx[length(xx)])
            
    .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
     UpdateView()
     UpdateLabel()
   }


  remove.item <- function(widget, data)
  {
     xx <- .FactorEnv$xx   
     checkPtrType(data, "GtkTreeView")
     treeview <- data
     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]]){
        iter <- selected$iter
         
        path <- model$getPath(iter)
        i <- path$getIndices()[[1]]+1
        model$remove(iter)
        
        xx <- xx[-i]
        .FactorEnv$xx <- xx           
        path$prev()
        selection$selectPath(path)        
        .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
        UpdateView()            
        UpdateLabel()        
      }
  }

  move.item.up <- function(widget, data)
  {
     xx <- .FactorEnv$xx
     checkPtrType(data, "GtkTreeView")
     treeview <- data

     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]])
       {
         iter <- selected$iter
           
         path <- model$getPath(iter)
         i <- path$getIndices()[[1]]+1
         if(i == 1) return()
         model$set(iter, 0, xx[i-1])
         path$prev()
         selection$selectPath(path)
         selected <- selection$getSelected()
         iter <- selected$iter
         model$set(iter, 0, xx[i])
         tmp <- xx[i-1]
         xx[i-1] <- xx[i]
         xx[i] <- tmp
         .FactorEnv$xx <- xx
        .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
         UpdateView()
         UpdateLabel()                 
       }
  }

  move.item.down <- function(widget, data)
  {
     xx <- .FactorEnv$xx
     checkPtrType(data, "GtkTreeView")
     treeview <- data

     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]])
       {
         iter <- selected$iter
           
         path <- model$getPath(iter)
         i <- path$getIndices()[[1]]+1
         if(i == length(xx)) return()
         model$set(iter, 0, xx[i+1])
         gtkTreePathNext(path)
         selection$selectPath(path)
         selected <- selection$getSelected()
         iter <- selected$iter
         model$set(iter, 0, xx[i])
         tmp <- xx[i+1]
         xx[i+1] <- xx[i]
         xx[i] <- tmp
         .FactorEnv$xx <- xx         
        .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
         UpdateView()                
         UpdateLabel()              
       }
  }

  edit.item <- function(widget, data) {
     checkPtrType(data, "GtkTreeView")
     treeview <- data

     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]])
       {
         iter <- selected$iter         
         path <- model$getPath(iter)         
         treeview$setCursorOnCell(path,
           treeview$getColumns()[[1]],
           treeview$getColumns()[[1]]$getCellRenderers()[[1]],
           TRUE)
         UpdateLabel()           
       }
  }
   
  contrast.coding <- list(
  "Treatment (default) - contrasts each level with the first.\nThe first level is omitted." = "contr.treatment", 
  "Helmert - contrast the second level with the first, the \nthird with the average of the first two, and so on." = "contr.helmert",  
  "Polynomial - contrasts based on orthogonal polynomials." = "contr.poly", 
  "Sum - sum to zero contrasts." = "contr.sum", 
  "SAS - treatment contrasts with base level set to be the\nlast level of the factor." = "contr.SAS")
  
  UpdateLabel <- function(){
    xx <- .FactorEnv$xx  
    contr <- contrast.coding[[which(sapply(rev(grb$getGroup()), gtkToggleButtonGetActive))]]

    contrF <- paste("Control (first level) is: <b>", xx[1], "</b>", sep="")
    contrL <- paste("Control (last level) is: <b>", rev(xx)[1], "</b>", sep="")
    
    contr.msg <- list(contr.treatment = contrF, contr.helmert = contrL, 
      contr.poly = "L and Q terms (see MASS, p156)", 
      contr.sum = contrL, contr.SAS = contrL)[[contr]]
    
    lab1$setMarkup(contr.msg)
    
  }  

  .FactorEnv$the.column <- theFrame[,.FactorEnv$col.idx]
  .FactorEnv$col.original <- .FactorEnv$the.column  
  contr <- attr(.FactorEnv$the.column, "contrast.name")    

  box4 <- gtkVBoxNew(FALSE, 5)  
  grb <- gtkRadioButtonNewWithLabel(NULL, label=names(contrast.coding)[1])
  grb$setActive(TRUE)
  box4$packStart(grb, FALSE, FALSE, 0)
  for(ii in 2:length(names(contrast.coding))){
    grb <- gtkRadioButtonNewWithLabel(group=grb$getGroup(), 
      label=names(contrast.coding)[ii])
    if(!is.null(contr) && contr==unlist(contrast.coding)[ii]) grb$setActive(TRUE)
    gSignalConnect(grb, "toggled", function(grb, ...) if(grb$getActive()) UpdateLabel())
    box4$packStart(grb, FALSE, FALSE, 0)
  }

  data.factors <- which(GetClasses(theFrame) == "factor")
  if(!length(colnames(theFrame)[data.factors])) stop("No data columns are of type \"factor\"")

  # called with no selection  
  if(!length(col.idx)) {
    col.idx <- data.factors[1]
    .FactorEnv$col.idx <- col.idx
  } else if(length(col.idx==1) && !is.factor(theFrame[,col.idx])) {
    stop(paste("Data column:", col.idx, "is not of type \"factor\""))
  }
    
#  window <- gtkWindowNew(show=F)
  dialog <- gtkDialog("Factor Editor", NULL, "modal", "gtk-ok", 1, "gtk-cancel", 0, show = T)    
  dialog$setPosition(GtkWindowPosition["center-on-parent"])
  dialog$setTransientFor(toplevel) 
      
#  if(!is.null(toplevel)) {
#    window$setTransientFor(toplevel)  
#    window$setModal(TRUE)
#  }  

  dialog$setTitle("Factor Editor")
#  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  dialog[["vbox"]]$add(box0)  
#  window$add(box0)

  fr0 <- gtkFrameNew(label="Column Selection")

  cb1 <- gtkComboBoxNewText()  
  cb1$show()
  cb1["width-request"] <- 75
  
  for (item in colnames(theFrame)[data.factors]) # omit "rows" heading
    cb1$appendText(item)  

  theIdx <- which(data.factors %in% col.idx)-1
  cb1$setActive(theIdx)
  fr0$add(cb1)  
  box0$packStart(fr0, FALSE, FALSE, 5)    
  fr1 <- gtkFrameNew(label="Factor Level Order")
  box0$packStart(fr1, TRUE, TRUE, 5)
  box1 <- gtkHBoxNew(FALSE, 5)
  fr1$add(box1)
  box1.5 <- gtkVBoxNew(FALSE, 0)  
  box1$packStart(box1.5, TRUE, TRUE, 5)

  box1.6 <- gtkHBoxNew(FALSE, 0)  
  box1.5$packStart(box1.6, FALSE, TRUE, 5)  
  lab1 <- gtkLabelNew("")  
  box1.6$packStart(lab1, FALSE, FALSE, 5)
                                            
  MakeModel <- function(){
    col.original <- .FactorEnv$col.original
    if(!is.factor(col.original)) {
      col.original <- as.factor(integer(0))
      #stop("Can't edit non-factors")
      warning("Can't edit non-factors")    
      }
    .FactorEnv$xx <- na.omit(cbind(levels(col.original)))
#print(col.original)
#print(levels(col.original))
    #.FactorEnv$xx <- cbind(levels(col.original))
    model <- gtkListStoreNew("gchararray")
    xx <- .FactorEnv$xx
    sapply(xx, function(x) model$set(model$append()$iter, 0, x))
    UpdateLabel()  
    return(model)
  }
    
  sw <- gtkScrolledWindowNew(NULL, NULL)
  sw$setPolicy("automatic", "automatic")
  box1.5$packStart(sw, TRUE, TRUE, 0)
  treeview <- gtkTreeViewNew()  
  
  model <- MakeModel()  
  treeview$setModel(model)
   
  gSignalConnect(cb1, "changed", function(widget){
      new.idx <- which(widget$getActiveText()==colnames(theFrame))
      .FactorEnv$col.idx <- new.idx
      .FactorEnv$the.column <- theFrame[,new.idx]
      .FactorEnv$col.original <- .FactorEnv$the.column
      model <- MakeModel()
      treeview$setModel(model)
  })   
  
  treeview$setRulesHint(TRUE)
  treeview$setHeadersVisible(FALSE)
  treeview$setEnableSearch(FALSE)  
  treeview$getSelection()$setMode("single")
  renderer <- gtkCellRendererTextNew()
  renderer$setData("column", 0)

  renderer['editable-set'] <- TRUE
  renderer['editable'] <- TRUE
  treeview$insertColumnWithAttributes(-1, "Name", renderer, text = 0)

  sw$setShadowType(as.integer(1))
  sw$add(treeview)
  sw$setSizeRequest(300, -1)
#  window$setResizable(FALSE)
  dialog$setResizable(FALSE)

  gSignalConnect(renderer, "edited", cell.edited, model)

  box2 <- gtkVBoxNew(FALSE, 5)
  box1$packStart(box2, FALSE, FALSE, 5)

  button1 <- gtkButtonNewWithLabel("Move Up")
  box2$packStart(button1, FALSE, FALSE, 0)

  button2 <- gtkButtonNewWithLabel("Move Down")
  box2$packStart(button2, FALSE, FALSE, 0)

  button3 <- gtkButtonNewWithLabel("Edit Name")
  box2$packStart(button3, FALSE, FALSE, 0)

  button4 <- gtkButtonNewWithLabel("Add Level")
  box2$packStart(button4, FALSE, FALSE, 0)

  button5 <- gtkButtonNewWithLabel("Remove Level")
  box2$packStart(button5, FALSE, FALSE, 0)
  
  gSignalConnect(button1, "clicked", move.item.up, data=treeview)
  gSignalConnect(button2, "clicked", move.item.down, data=treeview)
  gSignalConnect(button3, "clicked", edit.item, data=treeview)
  gSignalConnect(button4, "clicked", add.item, model)
  gSignalConnect(button5, "clicked", remove.item, treeview)   
  
  ordered.checkbox <- gtkCheckButtonNewWithLabel(label="Levels Are Ordered")
  ordered.checkbox$setActive(is.ordered(.FactorEnv$the.column))
  box0$packStart(ordered.checkbox)

  expander <- gtkExpanderNew(label="Factor Contrasts (For Experts)")
  fr2 <- gtkFrameNew(label="Contrast Coding")
  #box0$packStart(fr2, FALSE, FALSE, 5)
  box0$packStart(expander, FALSE, FALSE, 5)
  expander$add(fr2)
  fr2$add(box4)

#  box6 <- gtkHBoxNew(FALSE, 5)
   
  UpdateLabel()    
  if(dialog$run() == 1){
    UpdateView()
    dialog$destroy()
  } else {
    .FactorEnv$the.column <- .FactorEnv$col.original  
    UpdateView()
    dialog$destroy()    
  }
  
#  button <- gtkButtonNewWithLabel("  Cancel  ")
#  gSignalConnect(button, "clicked", function(...) {
#   .FactorEnv$the.column <- .FactorEnv$col.original
#    UpdateView()
#    window$destroy()
#    })
#  box6$packEnd(button, FALSE, FALSE, 5)
#  
#  box0$packEnd(box6, FALSE, FALSE, 0)
#  button <- gtkButtonNewWithLabel("    OK    ")
#  gSignalConnect(button, "clicked", function(...){    	
#    UpdateView()
#    window$destroy()
#  })
#  box6$packEnd(button, FALSE, FALSE, 0)
#  
#  window$show()
}

###############################################################################
# End factor editor
###############################################################################

#TableEntryDialog <- function(.localenv){

MakeRadioDialog <- function(title, items, handler=NULL, data=NULL){
  window <- gtkWindowNew(show=F) 
  if(!is.null(data$.local)) {
    window$setPosition(GtkWindowPosition["center-on-parent"])  
    window$setTransientFor(data$.local$toplevel)  
    window$setModal(TRUE)
  }  
  window$setTitle(title)
  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  window$add(box0)
  window$setResizable(FALSE)
  window$resize(window$getSize()$width, 1)

  xx <- list()
  for(jj in 1:length(items)){
    fr1 <- gtkFrameNew(label=names(items[[jj]]))
    xx[[jj]] <- MakeRadiobuttonGroup(items[[jj]][[1]])
    fr1$add(xx[[jj]]$box)
    box0$add(fr1)
  }

  fr2 <- gtkVBoxNew()
  box18000 <- gtkHBoxNew(FALSE, 5)
  fr2$add(box18000)
  button <- gtkButtonNewWithLabel(" Cancel ")
  gSignalConnect(button, "clicked", function(...){
    window$destroy()
  })

  box18000$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("   OK   ")
  box18000$packEnd(button, FALSE, FALSE, 0)
  gSignalConnect(button, "clicked", function(...){
    results<- sapply(xx, function(item) 
        which(rev(sapply(item$grb$getGroup(), gtkToggleButtonGetActive))))
    res <- list()
    for(jj in 1:length(items)) 
      res[[names(items[[jj]])]] <- items[[jj]][[1]][results[jj]]
    if(!is.null(handler)) handler(res, data)
    window$destroy()
  })
  box0$packStart(fr2, FALSE, FALSE, 0)  
  window$show()
}

TABLE_IMPORT_OPTIONS <- list(
    list("Data Type" = c("Numeric Data", "Character Data")), 
    list("Name Options" = c("Row And Column Names", "Row Names Only", "Column Names Only", "No Row Or Column Names"))
    )
 
TABLE_IMPORT_OPTIONS_FUNC <- function(results){
  do.colnames <- F; do.rownames <- F
  if(results$"Name Options"%in%c("Row And Column Names", "Row Names Only"))
    do.rownames <- T
  if(results$"Name Options"%in%c("Row And Column Names", "Column Names Only"))
    do.colnames <- T
  colClasses <- NA
  if(results$"Data Type"%in%c("Numeric Data"))
    colClasses <- "numeric"
  if(results$"Data Type"%in%c("Character Data"))
    colClasses <- "character"
  list(colClasses=colClasses, do.rownames=do.rownames, do.colnames=do.colnames)
}

# This is the dialog for pasting stuff to the whole frame
DoPasteDialog <- function(.local, do.rownames=T, do.colnames=T, colClasses=NULL) 
  MakeRadioDialog(title = "Paste In Data",
    items = TABLE_IMPORT_OPTIONS, 
    data=list(.local=.local), 
    handler=function(results, data){
      .local <- data$.local
      rv <- TABLE_IMPORT_OPTIONS_FUNC(results)
      do.rownames <- rv$do.rownames
      do.colnames <- rv$do.colnames      
      colClasses <- rv$colClasses
        
      dat <- ReadFromClipboard(header=do.colnames,sep="\t", 
        row.names=if(do.rownames) 1 else NULL, colClasses=colClasses, stringsAsFactors=F)
      if(!is.null(dat)){
         task <- GetTaskPasteIn(.local$theFrame, dat, 
           1, 2, do.rownames=do.rownames, do.colnames=do.colnames, do.coercion=F)
         DoTaskWrapper(.local, task)
      }
    })

FILE_IMPORT_OPTIONS <- TABLE_IMPORT_OPTIONS 
ll <- length(FILE_IMPORT_OPTIONS)
FILE_IMPORT_OPTIONS[[ll+1]] <- list("File Type" = c("Comma Separated (csv)", "Tab Separated (txt)"))

FILE_IMPORT_OPTIONS_FUNC <- function(results){
  sep<-""
  if (results$"File Type"%in%"Comma Separated (csv)")
    sep<-","
  if (results$"File Type"%in%"Tab Separated (txt)")
    sep<-""
  list(sep=sep)
}

#
#win <- gtkWindowNew()
#tv <- gtkTextViewNew()
#tv$setEditable(FALSE)
#win$add(tv)
#buffer <- tv$getBuffer()
#xx <- readLines("xx.txt", 2)
#buffer$setText(xx)

# This is the dialog for pasting stuff to the whole frame
DoImportDialog <- function(data) MakeRadioDialog(title = "Table Characteristics",
    items = FILE_IMPORT_OPTIONS,
    data=data, 
    handler=function(results, data){
      .local <- data$.local
      file.name <- data$file.name
      rv <- TABLE_IMPORT_OPTIONS_FUNC(results)
      do.rownames <- rv$do.rownames
      do.colnames <- rv$do.colnames
      colClasses <- rv$colClasses 
      rv <- FILE_IMPORT_OPTIONS_FUNC(results)
      sep <- rv$sep
      
      tryCatch({
        cf <- count.fields(file.name, sep=sep,comment.char="")
        tt <- table(cf)
        cf.msg <- paste(paste("Lines with ", names(tt), " fields: ", tt, sep=""),  collapse="\n")
        msg <- paste("Fields in file:\n", cf.msg)
        # scan first 5 in to get colClasses vector
        if(colClasses != "character" && do.rownames){        
          xx <- read.table(file.name, sep=sep,header=do.colnames, 
               row.names=if(do.rownames) 1 else NULL, 
               stringsAsFactors=F,fill=T,comment.char="", nrows=5)  
          NN <- dim(xx)[2]
          col.vector <- c("character", rep(colClasses, NN))
        }  else {
          col.vector <- "character"
        }
                     
        data = list(.local=.local, do.rownames=do.rownames, 
          do.colnames=do.colnames, 
          sep=sep, file.name=file.name,  col.vector = col.vector)
        quick_query(msg, data, win=.local$toplevel,
          handler=function(data){
            attach(data, warn=F)
            dat <- read.table(file.name, sep=sep,header=do.colnames, 
             row.names=if(do.rownames) 1 else NULL, 
             stringsAsFactors=F,colClasses= col.vector,
             fill=T,comment.char="")
            ReplaceEditWindow(MakeInternalDataFrame(dat), .local)
          }
        )
       }, error = function(e) {
         print(e)
      })
    })

###############################################################################
# Sort dialog
###############################################################################

MakeRadiobuttonGroup <- function(labels, theChoice=1){
  box3 <- gtkVBoxNew()
  grb1 <- gtkRadioButtonNewWithLabel(NULL, label=labels[1])
  if(theChoice == 1) grb1$setActive(TRUE)
  box3$add(grb1)
  for(jj in 2:length(labels)){
    grb1 <- gtkRadioButtonNewWithLabel(group=grb1$getGroup(),label=labels[jj])
    if(jj == theChoice) grb1$setActive(TRUE)
    box3$add(grb1)
  }
  return(list(box=box3, grb=grb1))
}

MakeComboEntry <- function(items, box1){
  box2 <- gtkHBoxNew()
	cb1 <- gtkComboBoxNewText()  
	cb1$show()
	cb1["width-request"] <- 100		
	for (item in items) # omit "rows" heading
	  cb1$appendText(item)  
	cb1$setActive(0)
  box5 <- gtkVBoxNew()		
  box5$packStart(cb1, TRUE, FALSE, 0)    
  box2$packStart(box5, TRUE, TRUE, 10)
  xx1 <- MakeRadiobuttonGroup(c("Ascending", "Descending"))
  box3 <- xx1$box
  grb1 <- xx1$grb
  xx2 <- MakeRadiobuttonGroup(c("Default", "Character", "Numeric"))
  box4 <- xx2$box
  grb2 <- xx2$grb
	box2$packStart(box4, FALSE, FALSE, 0)    
	box2$packStart(box3, FALSE, FALSE, 0)
  box1$packStart(box2, FALSE, FALSE, 0)
  return(list(col=cb1, ord=grb1, typ=grb2, box=box2))
}
  
# Returns the ordering of the table
DoSortDialog <- function(theFrame, handler, .localenv){
  window <- gtkWindowNew(show=F) 
  if(!is.null(.localenv$toplevel)) {
    window$setPosition(GtkWindowPosition["center-on-parent"])    
    window$setModal(TRUE)
    window$setTransientFor(.localenv$toplevel)      
  }
  window$setTitle("Sort Options")
  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  window$add(box0)
  items <- colnames(theFrame)
  fr0 <- gtkFrameNew(label="Sort Key Selection")
  box1 <- gtkVBoxNew(FALSE, 5)
  box0$add(fr0)
  fr0$add(box1)

  .sl <- new.env()
  .sl$theList <- list()
  .sl$theList[[length(.sl$theList)+1]] <- MakeComboEntry(items, box1)
  fr1 <- gtkFrameNew(label="Add/Remove Keys")
  box9000 <- gtkHBoxNew(FALSE, 5)
  fr1$add(box9000)
  button <- gtkButtonNewWithLabel("Remove A Key")
  gSignalConnect(button, "clicked", function(obj, ...){
    if(length(.sl$theList)<2) return(FALSE)
    .sl$theList[[length(.sl$theList)]]$box$destroy()
    .sl$theList[[length(.sl$theList)]] <- NULL
    window$resize(window$getSize()$width, 1)
  })
  box9000$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("Add A Key")
  gSignalConnect(button, "clicked", function(obj, data=.sl){
    .sl$theList[[length(.sl$theList)+1]] <- MakeComboEntry(items, box1)
  })
  box9000$packEnd(button, FALSE, FALSE, 0)
  box0$packStart(fr1, FALSE, FALSE, 0)

  fr2 <- gtkVBoxNew()
  box18000 <- gtkHBoxNew(FALSE, 5)
  fr2$add(box18000)
  button <- gtkButtonNewWithLabel(" Cancel ")
  gSignalConnect(button, "clicked", function(obj, data=.sl){
    window$destroy()
  })
  box18000$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("   OK   ")
  box18000$packEnd(button, FALSE, FALSE, 0)
  gSignalConnect(button, "clicked", function(obj, data=.sl){
		opts <- lapply(.sl$theList, function(item){
				list(col = item$col$getActiveText(),
  				ord = which(rev(sapply(item$ord$getGroup(), gtkToggleButtonGetActive))),
  				typ = which(rev(sapply(item$typ$getGroup(), gtkToggleButtonGetActive))))
		})
		dataset.order <- do.call("order", lapply(opts, function(item){
		  xx <- theFrame[[item$col]]
		  if(item$typ == 1) {
        xrank <- xtfrm(xx)
      } else if (item$typ == 2) {
		    xrank <- xtfrm(as.character(xx))
      } else if (item$typ == 3) {
		    xrank <- xtfrm(as.numeric(xx))
      } else {
        stop("Sort error")
      }
			(c(-1, 1)[(item$ord==1)+1])*xrank
		}))
    handler(dataset.order, .localenv)
    window$destroy()    
  })
  box0$packStart(fr2, FALSE, FALSE, 0)  
  window$setResizable(FALSE)
  window$show()
}
###############################################################################
# End sort dialog
###############################################################################
                

DoTaskWrapper <- function(.local, task, do.undo = TRUE){
  #tryCatch({
    rv <- DoTask(.local, .local$theFrame, task, .local$changed.handler)

    rows.changed <- NULL
      # Special for changecells to minimize edit time
    if(length(task) == 1 && task[[1]]$func == "ChangeCells")
      rows.changed <- task[[1]]$arg$row.idx
    #print("Updating here...")
    #print(dim(.local$model))
    UpdateDfEditor(.local, rv$df, rows.changed)    
    #print("Updating finished")
    #print(dim(.local$model))
      
    if(do.undo){
      .local$undoStack[[length(.local$undoStack)+1]] <- rv$undo         
      
      if(object.size(.local$undoStack) > STACK_LIMIT){
        warning("Stack full")    
        jj <- 0
        while(object.size(.local$undoStack) > STACK_LIMIT && length(.local$undoStack))
          .local$undoStack[[jj <- jj + 1]] <- NULL
        if(object.size(.local$undoStack) > STACK_LIMIT){ 
          warning("This edit is too large to support undo")
          .local$undoStack <- list()
        }
      }
    }
  #}, error = function(e) {
  #  warning("An error has occurred in performing the task")
  #  old_warning(e)
  #})      
}


  # update the scroll in one treeview from another
ViewOnScrollChanged <- function(obj, data){
  sw2 <- data$sw2
  .local <- data$.local
  # Take over the event loop! 
  # See http://wiki.laptop.org/go/PyGTK/Smooth_Animation_with_PyGTK
   while(gtkEventsPending())
    gtkMainIteration()
  gtkAdjustmentSetValue(sw2, gtkAdjustmentGetValue(obj))
  .local$allow.key.flag <- TRUE  # For paging, however, this has a problem...
}


############################################################
# Turns out that scroll_row_timeout has no horizontal autoscroll.
# I based this on gtktreeview::gtk_tree_view_vertical_autoscroll, no offset
HScroll <- function(data){ 
  .local <- data
    # which side of the middle are we?
  view <- .local$view
  sw.ha <- .local$sw.ha
  ptr <- view$getBinWindow()$getPointer()
  vr <- view$getVisibleRect()$visible.rect
  sw.ha.value <- sw.ha$value
  direction <- ifelse (ptr$x - sw.ha.value <= vr$width/2, -1, 1) 

  val <- sw.ha.value + direction*sw.ha[["step_increment"]]
  if(0 <= val && val <= sw.ha[["upper"]] - sw.ha[["page_size"]]) {
    sw.ha$setValue(val)
  } else if (val < 0) {
    sw.ha$setValue(0)
  } else {
    sw.ha$setValue(sw.ha[["upper"]] - sw.ha[["page_size"]])
  }
  TRUE
}

# Bind this to button-release, focus-out and enter-notify
RemoveHScrollTimeout <- function(obj, event, data){
  .local <- data
  try({
    gSourceRemove(.local$hScrollTimeout) 
    .local$doingHScroll <- FALSE
  }, silent=T) 
  FALSE
}

AddHScrollTimeout <- function(obj, event, data){
  .local <- data
  sw.ha <- .local$sw.ha
  view <- .local$view
  if (!.local$doingHScroll){
    ptr <- obj$getBinWindow()$getPointer()
    if (as.flag(ptr$mask) & GdkEventMask['button-press-mask']){
    x <- ptr$x - sw.ha$getValue()
    y <- ptr$y
    vr <- view$getVisibleRect()$visible.rect
    h <- vr$height
    w <- vr$width
    z1 <- y > h/w*x 
    z2 <- y > h - h/w*x
    if((z1 && !z2) || (!z1 && z2)){ 
      .local$hScrollTimeout <- gTimeoutAdd(.local$SCROLL_ROW_TIMEOUT, HScroll, data=.local)
      .local$doingHScroll <- TRUE
    }
    }} #if, if
    TRUE
}
############################################################
# End autoscroll

PaintSelectionOnTimeout <- function(.local){
  # We want to add a timeout to the LAST key pressed.
  try({
    gSourceRemove(.local$do.paint.timeout) 
    .local$do.paint.timeout <- NULL
  }, silent=T)

  .local$do.paint.timeout <- gTimeoutAdd(100, function(){
		.local$do.paint <- TRUE
    .local$rectangles <- list()
     UpdateSelectionRectangle(.local)
		return(FALSE)
	})
}


PaintRowSelectionOnTimeout <- function(.local){
#  # We want to add a timeout to the LAST key pressed.
#  try({
#    gSourceRemove(.local$do.rows.paint.timeout) 
#    .local$do.rows.paint.timeout <- NULL
#  }, silent=T)
#
#  .local$do.rows.paint.timeout <- gTimeoutAdd(100, function(){
#     #print("Updating")
    if(length(GetSelectedColumns(.local))==0){
      gsr <- gtkTreeSelectionGetSelectedRows(.local$ss.rn)$retval
      if(length(gsr)){# && length(gsr) < MAX_ROWS_TO_DRAW_SELECTION){
    		.local$do.paint <- TRUE
        sr <- sapply(gsr, gtkTreePathGetIndices)+1
        cc <- .local$allColumns
        view <- .local$view
        ma <- MergeAdjacent(sort(sr))
        rectangles <- vector("list", ma$length)    
        FIRST_COL <- cc[[1]]$column
        if(length(cc) > 1)
          LAST_COL <- cc[[length(cc)-1]]$column
        else 
          LAST_COL <- FIRST_COL
          
        for(i in 1:ma$length){
          path.start <- gsr[[ma$start[i]]]
          if(ma$start[i] == ma$end[i])
            path.end <- path.start
          else
            path.end <- gsr[[ma$end[i]]]
          rect.start <- gtkTreeViewGetCellArea(view, path.start, FIRST_COL)$rect
          rect.end <- gtkTreeViewGetCellArea(view, path.end, LAST_COL)$rect
          rect <- gdkRectangleUnion(rect.start, rect.end)$dest
          rect$y <- rect$y + .local$sw.view.va$getValue()
          #rect$height <- .local$sw.view.va[["upper"]]
          rectangles[[i]] <- rect
        }
        .local$rectangles <- rectangles        
      } else {
        .local$rectangles <- list()      
      }
     .local$viewGetBinWindow$invalidateRect(NULL, FALSE)      
    }
#		return(FALSE)
#	})
}
  
####
############################################################
MoveCursor <- function(widget, direction, .local, stat=as.integer(0)){
  cursor.info <- gtkTreeViewGetCursor(widget)
  path <- cursor.info$path
  allColumns <- .local$allColumns
  row.idx <- as.integer(gtkTreePathGetIndices(path))+1
  
  #print(row.idx)

  new.idx <- GetColIdx(cursor.info$focus.column)
  ori.idx <- new.idx

  if (direction == "right"){
     if( new.idx < length(allColumns)){
       new.idx <- new.idx+1
     } else {
       insert.dialog(.local, row.idx=row.idx-1, col.idx=new.idx, insert.type ="Columns", win=.local$toplevel)
     }
  } else if (direction == "left" && 1 < new.idx) {
    new.idx <- new.idx-1
  } else if (direction == "down") {
    if (row.idx < dim(.local$theFrame)[1]){
      gtkTreePathNext(path)
    } else {
       insert.dialog(.local, row.idx=row.idx-1, col.idx=new.idx, insert.type ="Rows", win=.local$toplevel)
    }            
  } else if (direction == "up") {
    gtkTreePathPrev(path)
  } else {
  }      
    #column indicators
  if(direction%in%c("up", "down") && stat == as.integer(0)){
    if(.local$do.paint){    # we've been drawing a rectangle
      .local$do.paint <- FALSE
      .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
    }
    UpdateColumnSelection(.local, new.idx)
  }
  selectedColumns.new <- integer(0) 
  if(direction%in%c("left", "right")){
    col.idx <- new.idx
    if (stat == as.integer(0)) { # select only this column
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      }
      .local$start.key.column.select <- NULL
       selectedColumns.new <- col.idx     
       UpdateColumnSelection(.local, selectedColumns.new)
    } else if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      }
        # this had better exist!
        # Found a bug here 082210
      selectedColumns.new <- .local$start.key.column.select:col.idx
      UpdateColumnSelection(.local, selectedColumns.new)
      PaintSelectionOnTimeout(.local)
    } else {
      .local$start.key.column.select <- NULL
    }
  }

  new.col <- allColumns[[new.idx]]$column
  renderer <- allColumns[[new.idx]]$renderer

    # ScrollToCell seems to stop working after we add a new column.
    # This routine is from gtktreeview::gtk_tree_view_scroll_to_cell
  if(direction%in%c("left", "right")){
    cell_rect <- gtkTreeViewGetBackgroundArea(widget, path, new.col)$rect
    vis_rect <- gtkTreeViewGetVisibleRect(widget)$visible.rect
    dest_x <- vis_rect$x
    dest_y <- vis_rect$y
    if (cell_rect$x < vis_rect$x)
      dest_x <- cell_rect$x
    if (cell_rect$x+cell_rect$width > vis_rect$x + vis_rect$width)
      dest_x <- cell_rect$x + cell_rect$width - vis_rect$width
    gtkTreeViewScrollToPoint(widget, dest_x, dest_y)
  }
  #if(!gtkWidgetIsFocus(.local$view)) { #added 4-21-2010
  #    print("MoveCursor: focus is FALSE")   
  #   gtkWidgetGrabFocus(.local$view)
  #  #gtkTreeViewSetCursorOnCell(widget, path, new.col, renderer, TRUE)         
  #   #.local$flash.cursor <- TRUE
  #}  
  #if(!gtkWidgetIsFocus(.local$view)) #added 4-21-2010
  #    print("MoveCursor: focus is STILL FALSE")      
    
  if(.local$flash.cursor){
   force.reset.focus(.local)
#    gtkTreeViewSetCursorOnCell(widget, path, new.col, renderer, TRUE)    
    .local$flash.cursor <- FALSE
  }          

  gtkTreeViewSetCursorOnCell(widget, path, new.col, renderer, FALSE)
#  print("MoveCursor returning")   
  # This is to fix a bug where the window doesn't initially get focus.
  # We pop up a transient window to give focus back to wherever we called from.
  if(!gtkWidgetIsFocus(.local$view)) { #added 4-21-2010 
    force.reset.focus(.local)
  }
  return(TRUE)
}  

force.reset.focus <- function(.local){
#return()
  on.exit({w$destroy(); .local$view$grabFocus()}) # block concurrent user calls with a modal window
  w <- gtkWindowNew("Refocusing...", show=F)
  w$setDecorated(FALSE)
  lab <- gtkLabelNew("Refocusing...")
  w$add(lab)
  w$setPosition(GtkWindowPosition["center-on-parent"])              
  w$setTransientFor(.local$toplevel)    
  w$setModal(TRUE)        
  w$showAll()
  lab$grabFocus()
}

RowNamesClearContents <- function(row.idx, .local){
  if(length(row.idx)==0) return(FALSE)
  nf <- .local$theFrame[row.idx,, drop=F]
  nf[,-1] <- ""
  task <- list(list(func="ChangeCells", 
    arg = list(nf=nf, row.idx=row.idx)))
  DoTaskWrapper(.local, task)
}  

GetColIdx <- function(column){
  #tryCatch(
    as.integer(column["title"])#,
  #  error = function(e) integer(0))
}

# Bug - can still edit last row name
RowNamesKeyPress <- function(widget, event, data) {
  .local <- data
  keyval <- event[["keyval"]] 
  stat <- as.flag(event[["state"]])    
  #print(keyval)
  if (CtrlLetter(keyval, stat, GDK_z)){
    #print("Ctrl-z")
    DoUndo(.local)
    return(TRUE)
  } else if(keyval == GDK_Delete) {
    row.idx <- GetSelectedRows(.local$view.rn, .local)
    RowNamesClearContents(row.idx, .local)
    return(TRUE)
  } else if (CtrlLetter(keyval, stat, GDK_c)) {
     # block transient interactions with a popup
     #print("Ctrl-c")
    gsr <- GetSelectedRows(.local$view.rn, .local)
      w <- TransientWindow("Copying...", .local)
      on.exit(w$destroy())    
    if(!length(gsr) && dim(.local$theFrame)[1] > 1) gsr <- 1:(dim(.local$theFrame)[1]-1)
    CopyToClipboard(.local$theFrame[gsr, 1:(length(.local$allColumns)-1)+COLUMN_OFFSET, drop=F], do.rownames=T, do.colnames=T)
    retval <- TRUE
    .local$allow.key.flag <- TRUE                          
  }
           
    # ignore last row
  cursor.info <- gtkTreeViewGetCursor(widget)    
  if(!keyval%in%myValidNavigationKeys || keyval == GDK_Return && !is.null(cursor.info$path)){
    col.idx <- GetColIdx(cursor.info$focus.column)
    row.idx <- as.integer(gtkTreePathGetIndices(cursor.info$path))+1
    if(row.idx > dim(.local$theFrame)[1]-1) return(TRUE)        
  }
  
  if(keyval%in%myValidNavigationKeys){
    #cat("*")
    if( .local$scrollRowNames) { 
      .local$scrollRowNames <- FALSE
     while(gtkEventsPending())
            gtkMainIteration()
      gtkPropagateEvent(widget, event)
      #gtkTreeSelectionUnselectAll(.local$ss)
      gtkAdjustmentSetValue(.local$sw.view.va, gtkAdjustmentGetValue(.local$sw.rn.va))
      .local$scrollRowNames <- TRUE 
      return(TRUE)
   }
  } 
    return(FALSE)
}


  # Apply command to range [sr, sc]
CommandData <- function(.local, sr, sc) {
  command.dialog <- list(
    title = "Apply Command",
    label = "Apply a command or function to cell selection.\nSelection is stored as variable x.\n",
    txt.stringItem = "", label = "Enter Code (Ctrl-Enter To Run)", multi=T, signal.on.startup = F,
      signal = c("default", "run.it"),    
    do.apply.trueFalseItem = FALSE, label = "Apply Function",
      signal = c("default", "toggle.sensitive", "apply.over"),
      apply.over.radiobuttonItem = c(1,2), item.labels = c("Rows", "Columns"), label = "Over", indent=10,    
      signal = c("default", function(apply.over, position){
        if(get.value(apply.over) == 1) set.value(position, "Right")
        if(get.value(apply.over) == 2) set.value(position, "Bottom")        
      }, "position"),
    insert.trueFalseItem = TRUE, label = "Put Output In Table",
      signal = c("default", "toggle.sensitive", "position"),    
    position.radiobuttonItem = c("Replace", "Bottom", value="Right"), label = "Position to Insert", indent=10,
    otherTable.stringItem = "", label = "Put Output In Another Table", sensitive=F
  )
  
  command <- function(txt, insert, position, do.apply, apply.over, otherTable){
   txt <- sub("^=(.*?)$", "\\1", txt, perl=TRUE)
   try.short.fn <- paste("function(x) {", txt, "}")             
   dat <- NULL
   .e = new.env()   
   .e$output <- NULL
   x <- as.matrix(.local$theFrame[sr, sc+1, drop=F])   
   .e$xx <- NULL
   tryCatch({
     .e$output <- eval(parse(text=txt), envir=.GlobalEnv)
     if(!is.function(.e$output)){ # not a function, maybe something like 1:10                        
        .e$xx <- .e$output
        if(findxInParseTree(txt) && exists("x", envir = .GlobalEnv)) 
          quick_message("  Warning! A variable called 'x' exists already and will be used.  ", win=.local$toplevel)
     }
   }, error = function(e) { # try putting "function(x)" on the front and see if that works
     tryCatch({    
       .e$output <- eval(parse(text=try.short.fn), envir=.GlobalEnv)
     }, error = function(e) quick_message("  Sorry, that didn't make sense to R  ^_^;;  ", win=.local$toplevel))
  })  
  output <- .e$output    
  xx <- .e$xx  
  if(is.function(output)) {
    if(do.apply){
      xx <- apply(x, apply.over, output) 
    } else {
      xx <- eval(make.call(output, x))  
    }
  }
  dat <- NULL
  tryCatch({  
    if(!is.null(xx) && length(xx) && !(is.list(xx) && !is.data.frame(xx))){
      if (position == "Replace"){
          dat <- array(xx, c(length(sr), length(sc)))
          if(is.atomic(dat)) DoTaskWrapper(.local,list(list(func="ChangeCells", args=list(nf=dat, row.idx=sr, col.idx=sc+1))))
      } else if (position == "Right"){
         dat <- t(ragged.cbind(xx))
         new.dat <- array(NA, c(dim(.local$theFrame)[1], dim(dat)[2]))
         new.dat[sr, 1:dim(dat)[2]] <- dat
         #new.dat <- data.frame(dat, stringsAsFactors=F)         
         task <- list(list(func="InsertColumns",          
                 arg = list(nf = new.dat, col.idx=max(sc)+1+rep(1, dim(new.dat)[2]))))
        DoTaskWrapper(.local, task)                       
      } else if (position == "Bottom") {
         dat <- ragged.cbind(xx)
         new.dat <- array(NA, c(dim(dat)[1], dim(.local$theFrame)[2]))
         new.dat[1:dim(dat)[1], sc+1] <- dat
         new.dat <- data.frame(new.dat, stringsAsFactors=F)
         task <- list(list(func="InsertRows",                   
                 arg = list(nf = new.dat, row.idx=max(sr)+rep(1, dim(new.dat)[1]))))
        DoTaskWrapper(.local, task)      
      }
    }
    #}, error = function(e) quick_message("  Output couldn't be put into cells  ", win=.local$toplevel))
    }, error = function(e) cat("Output couldn't be put into cells\n"))    
  }  

  run.dialog(func=command, dlg.list=command.dialog, win=.local$toplevel)
  
 }
    
insert.dialog <- function(.local, row.idx=NULL, col.idx=NULL, insert.type ="Rows", win=NULL) {
  
  choice.list <- list("Columns" = "InsertNAColumns", "Rows" = "InsertNARows")

  dialog <- gtkDialog("Insert Rows/Columns", NULL, c("modal", "destroy-with-parent"), "gtk-ok", 1, "gtk-cancel", 0, show = F)
  #dialog$setDecorated(FALSE)

  do.insert <- function(){                
    choice.idx <- which(sapply(rev(grb$getGroup()), gtkToggleButtonGetActive))
    choice <- names(choice.list)[choice.idx]
    func <- choice.list[[choice.idx]]
    n <- as.integer(spinner$getValue())
    stopifnot(is.integer(n) && n > 0)
    arg <- list()
    if(choice == "Rows"){
      idx <- row.idx
      stopifnot(!is.null(idx))
      theIdx <- rep(idx+1, n)            
      arg$row.idx <- theIdx      
    } else {
      idx <- col.idx                     
      stopifnot(!is.null(idx))      
      theIdx <- rep(idx+1, n)                  
      arg$col.idx <- theIdx
    }
    DoTaskWrapper(.local,list(list(func=func,arg=arg)))
  }

  box0 <- gtkVBoxNew(FALSE, 5)
  
  grb.keypress <- function(obj, evt){
    keyval <- evt[["keyval"]]
    if(keyval == GDK_Return){
      do.insert() 
      dialog$destroy()
      .local$view$grabFocus()
      return(TRUE)
    }
    if(keyval == GDK_Right){
      spinner$spin(GtkSpinType["step-forward"], 1)
      return(TRUE)
    }    
    if(keyval == GDK_Left){
      spinner$spin(GtkSpinType["step-backward"], 1)
      return(TRUE)
    }        
    FALSE
  }

  fr2 <- gtkFrameNew(label="Insert")   
  box4 <- gtkVBoxNew(FALSE, 5)
  grb <- gtkRadioButtonNewWithLabel(NULL, label=names(choice.list)[1])
  gSignalConnect(grb, "key-press-event", grb.keypress)  
  grb$setActive(TRUE)
  box4$packStart(grb, FALSE, FALSE, 0)
  grb <- gtkRadioButtonNewWithLabel(group=grb$getGroup(), label=names(choice.list)[2])
  gSignalConnect(grb, "key-press-event", grb.keypress)
  if(!is.null(insert.type) && insert.type==names(choice.list)[2]) grb$setActive(TRUE)
  box4$packStart(grb, FALSE, FALSE, 0)
  fr2$add(box4)
  box0$add(fr2)


  box1 <- gtkHBoxNew(FALSE, 5)
  box1$packStart(gtkLabelNew("Number To Insert"), FALSE, FALSE, 10)
  spinner_adj <- gtkAdjustment(1, 1, .Machine$integer.max, 1, 10.0)
  spinner <- gtkSpinButton(spinner_adj, 1.0, 0)
  spinner$setValue(1)
  box1$add(spinner)
  spinner$grabFocus()
  box0$add(box1)

  gSignalConnect(spinner, "key-press-event", function(obj, evt){
    if(evt[["keyval"]] == GDK_Return) {
      spinner$update()
      do.insert() 
      dialog$destroy()  
    }
    if(evt[["keyval"]] == GDK_Escape) {
      dialog$destroy()
    }    
    .local$view$grabFocus()    
    FALSE
  })
  
  if(!is.null(win)) {
     checkPtrType(win, "GtkWindow")
     dialog$setPosition(GtkWindowPosition["center-on-parent"])
     dialog$setTransientFor(win)
  }
  gSignalConnect(dialog, "response", function(dlg, arg1, ...) {
     if(arg1==1) do.insert() 
     dialog$destroy()
     .local$view$grabFocus()     
  })                                    
  dialog[["vbox"]]$packStart(box0, TRUE, TRUE, 10)
  dialog$showAll()
}    


ViewKeyPress <- function(widget, event, data) {
  .local <- data
  retval <- TRUE       
  event.time <- event[["time"]]
  keyval <- event[["keyval"]]
  
      # remove click info
  .local$last_click_info <- NULL

  #cat(c(".", "*")[.local$allow.key.flag+1])
  #if( .local$allow.key.flag && event.time > .local$last.time){ #4-21-10
  if( event.time > .local$last.time){  

#  if(.local$allow.key.flag) if(!gtkWidgetIsFocus(.local$view)) { #added 4-21-2010
#      print("ViewKeyPress: focus is FALSE")
#     gtkWidgetGrabFocus(.local$toplevel)      
#     gtkWidgetGrabFocus(.local$view)
#  }  
  
    #tryCatch({        
      .local$last.time <- event.time
      .local$allow.key.flag <- FALSE                            
      stat <- as.flag(event[["state"]]) 
  
      allColumns <- .local$allColumns
      view <- .local$view
      model <- .local$model
      
      # Paging events
    if(keyval%in%c(GDK_Page_Up, GDK_Page_Down, GDK_Home, GDK_End)) {
     if ( (gtkRangeGetValue(.local$vbar) == 0) ||
        (gtkRangeGetValue(.local$vbar) == .local$va[["upper"]] - .local$va[["page_size"]]) ) 
           .local$allow.key.flag <- TRUE
        # Update the paint rectangle, and set a timeout if we're select-paging
      if(stat==0)
        .local$do.paint <- FALSE
      else 
        PaintSelectionOnTimeout(.local)
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
      }                
      if(stat==0 && length(GetSelectedColumns(.local)) > 1){
				cursor.info <- gtkTreeViewGetCursor(widget)
				new.idx <- GetColIdx(cursor.info$focus.column)
        UpdateColumnSelection(.local, new.idx)          
      }
      retval <- FALSE
    } else if(CtrlLetter(keyval, stat, GDK_Up) || CtrlLetter(keyval, stat, GDK_Down)
      || CtrlLetter(keyval, stat, GDK_Left) || CtrlLetter(keyval, stat, GDK_Right)) {
      .local$allow.key.flag <- TRUE            
      retval <- FALSE
    } else if (ShiftLetter(keyval, stat, GDK_Up) || ShiftLetter(keyval, stat, GDK_Down)){
      PaintSelectionOnTimeout(.local)
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      }
      .local$allow.key.flag <- TRUE            
      retval <- FALSE
      #return(FALSE)                          
    } else if (CtrlLetter(keyval, stat, GDK_c) || CtrlLetter(keyval, stat, GDK_C)) {
      namesflag <- CtrlLetter(keyval, stat, GDK_C)
      gsr <- GetSelectedRows(.local$view, .local)
     # block transient interactions with a popup
      w <- TransientWindow("Copying...", .local)
      on.exit(w$destroy())          
      if(!length(gsr) && dim(.local$theFrame)[1] > 1) gsr <- 1:(dim(.local$theFrame)[1]-1)
      CopyToClipboard(.local$theFrame[gsr, GetSelectedColumns(.local)+COLUMN_OFFSET, drop=F], 
        do.rownames=namesflag, do.colnames=namesflag)
      retval <- TRUE
      .local$allow.key.flag <- TRUE                          
      #return(TRUE)
    } else if (CtrlLetter(keyval, stat, GDK_v)){
      #print("Ctrl-v")
      dat <- ReadFromClipboard(row.names=NULL, sep="\t", fill=TRUE, stringsAsFactors=F) # character vector
      row.idx <- GetSelectedRows(.local$view, .local, restrict=FALSE)
      sc <- GetSelectedColumns(.local, restrict=FALSE)
      if(length(sc) == 0 && length(.local$allColumns) == 1) sc <- 1 # 
      if(length(sc) && length(row.idx)){
        col.idx <- min(sc)
        row.idx <- row.idx[1]
        task <- GetTaskPasteIn(.local$theFrame, dat, row.idx, col.idx+COLUMN_OFFSET)
        DoTaskWrapper(.local, task)
      }
      retval <- TRUE      
      .local$allow.key.flag <- TRUE                          
      #return(TRUE)
    } else if (CtrlLetter(keyval, stat, GDK_z)){
      #print("Ctrl-z")
      DoUndo(.local)
      .local$allow.key.flag <- TRUE
      retval <- TRUE
      #return(TRUE)
    } else if (CtrlLetter(keyval, stat, GDK_a)){
      #print("Ctrl-a")
      SelectAll(.local)
      .local$allow.key.flag <- TRUE        
      retval <- TRUE
      #return(TRUE)
    } else if(keyval == GDK_Delete) {
      row.idx <- GetSelectedRows(.local$view, .local)
      col.idx <- GetSelectedColumns(.local) + COLUMN_OFFSET      
      if(length(row.idx) == 0) row.idx <- 1:(dim(.local$theFrame)[1]-1)
      if(length(row.idx)> 0 && length(col.idx) > 0){
        nf <- .local$theFrame[row.idx, col.idx, drop=F]      
        stopifnot(ncol(nf) > 0)  
        nf[,] <- ""
        task <- list(list(func="ChangeCells", 
          arg = list(nf=nf, row.idx=row.idx, col.idx=col.idx)))
        DoTaskWrapper(.local, task)
      }
      .local$allow.key.flag <- TRUE                  
      retval <- TRUE      
      #return(TRUE)   
    } else if(keyval%in%myMetaKeys ||  (stat == GdkModifierType['control-mask'])){
      if(keyval%in%myShiftKeys)
        .local$start.key.column.select <- GetColIdx(widget$getCursor()$focus.column)
      .local$allow.key.flag <- TRUE                
      retval <- TRUE        
      #return(TRUE)
    } else if (keyval%in%myValidNavigationKeys){
      .local$rectangles <- list()      
      if (keyval == GDK_Up || ShiftLetter(keyval, stat, GDK_Return)) {
        MoveCursor(widget, "up", .local, stat)
        retval <- TRUE          
      } else if (keyval == GDK_Down || keyval == GDK_Return) {
        MoveCursor(widget, "down", .local, stat)
        retval <- TRUE          
      } else if(keyval == GDK_Right || keyval == GDK_Tab) {
        MoveCursor(widget, "right", .local, stat)
        retval <- TRUE          
      } else if(keyval == GDK_Left || keyval == GDK_ISO_Left_Tab) {
        MoveCursor(widget, "left", .local, stat)
        retval <- TRUE          
      } else {
        retval <- FALSE
      }
      .local$allow.key.flag <- TRUE                  
    } else if (keyval==GDK_Insert){
        #print("Starting insert")
        cursor.info <- gtkTreeViewGetCursor(widget)
        path <- cursor.info$path          
        col.idx <- GetColIdx(cursor.info$focus.column)
        row.idx <- as.integer(gtkTreePathGetIndices(path))                
        if(!is.null(path)){
          insert.dialog(.local, row.idx, col.idx, insert.type="Rows", win=.local$toplevel)
        }
        .local$allow.key.flag <- TRUE           
        retval <- TRUE                   
    } else if (keyval == GDK_equal && length(sc <- GetSelectedColumns(.local))) {

      if(length(sr <- GetSelectedRows(.local$view, .local))==0) 
          sr <- 1:(dim(.local$theFrame)[1]-1)
      CommandData(.local, sr, sc)
      .local$allow.key.flag <- TRUE        
      retvale <- TRUE
    } else if (keyval%in%myValidInputKeys){ # valid input keys
      cursor.info <- gtkTreeViewGetCursor(widget)
      col.idx <- GetColIdx(cursor.info$focus.column)
      renderer <- allColumns[[col.idx]]$renderer        
      path <- cursor.info$path 
      row.idx <- as.integer(gtkTreePathGetIndices(path))+1
      ddf <- dim(.local$theFrame)
        # ignore last row
      if(row.idx > ddf[1]-1 || col.idx > ddf[2]-2) {
        .local$allow.key.flag <- TRUE        
        retval <- TRUE          
      } else {             
        gtkTreeViewSetCursorOnCell(widget, cursor.info$path,
           cursor.info$focus.column, renderer, TRUE)              
        gtkPropagateEvent(view, event)
        retval <- FALSE
      } # if row.idx
      #.local$allow.key.flag <- TRUE           
    }
    #}, error = function(e){
    #    warning(e)
    #    #cat("E")
    #    .local$allow.key.flag <- TRUE
    #   }) # TryCatch       
  } else {# if allow.key.flag is false or event.time
  }
  if(!keyval%in%myValidKeys) { # otherwise bad things
    .local$allow.key.flag <- TRUE
  }  
  
  return(retval)
}

GetSelectedColumns <- function(.local, restrict=TRUE) {
 rv <- .local$selectedColumns
 if(restrict) rv <- rv[!rv%in%(dim(.local$theFrame)[2]-1)]
 return(rv)
}

RendererEditingStarted <- function(renderer, entry, path, data) {  
  checkPtrType( entry, "GtkEntry")
  col.idx <- data$col.idx 
  .local <- data$.local
  #print(entry)
  if(!.local$do.rendererEditingStarted) return(FALSE)
  
#  frame_text <- as.character(.local$theFrame[as.integer(path)+1, col.idx])
#  gtkEntrySetText(entry, frame_text)
      
  theFrame <- .local$theFrame
  view <- .local$view
    # don't paint rectangles
  .local$do.paint <- FALSE
  entry$setHasFrame(TRUE)
  entry$modifyBase(as.integer(1), as.GdkColor("black"))  
  entry$modifyBase(as.integer(3), as.GdkColor("black"))      
  entry$modifyText(as.integer(1), as.GdkColor("white"))              
  entry$modifyText(as.integer(3), as.GdkColor("white"))   
            
  entry$setAlignment(1)
  
  entry$setEvents(GdkEventMask["all-events-mask"])	  	  
  
  gObjectSetData(entry, "index", col.idx)
  gObjectSetData(entry, "renderer", renderer)
  gObjectSetData(entry, "path.str", path)
  gObjectSetData(entry, "path", gtkTreePathNewFromString(path))
  gObjectSetData(entry, "text", gtkEntryGetText(entry))    
  .local$entry <- entry
    
  # Insert cursor at point where we clicked
  if(0) if(!is.null(.local$last_click_info)) gSignalConnect(entry, "map-event", function(entry, event, data=.local){ 
    print("A")
    insertion_point <- 0
    pixel_size <- entry$getLayout()$getPixelSize()
    click_info <- .local$last_click_info
    col_width <- click_info$column$getWidth()
    text_width <- pixel_size$width
    text_pos <- (click_info$cell.x - col_width + text_width)/(text_width)
    if(length(text_pos) != 1 || text_pos < 0 || text_pos > 1) text_pos <- 0
    insertion_point <- round(text_pos*nchar(entry$getText()))   
    gtkEditableSelectRegion(entry, insertion_point, insertion_point)
    return(FALSE)  
 })
  # 7-4-10 - disable right-click menu, seems to confuse people
  IgnoreRClick <- function(entry, event, data){
    if(event[["button"]] == as.integer(3)) return(TRUE)
    return(FALSE)
  }
  gSignalConnect(entry, "button-press-event", IgnoreRClick)
  gSignalConnect(entry, "button-release-event", IgnoreRClick)  
    # We've trapped the temporary GtkEntry the renderer creates.
  gSignalConnect(entry, "key-press-event", RendererEntryKeyPress, data=.local)
    # you can leave the entry in 2 ways, focusing out or pressing a key from within
  gSignalConnect(entry, "focus-out-event", function(entry, event){
    if(!identical(.local$ignore_entry_focus_out, NULL)) {
      .local$ignore_entry_focus_out <- NULL
      return(FALSE)
    }
    .local$ignore_entry_focus_out <- TRUE
    gtkPropagateEvent(entry, event)            
    EnterTextIntoCell(entry, .local)    
    #print(.local$entry)
    #print("Entry focus out returning")  
    .local$allow.key.flag <- TRUE # unlock
    .local$entry <- NULL                 
      # changed 082210
    return(FALSE)
  }) # focus out event
  
  
 .local$doingEntryIntoCompletion <- FALSE  
 typ <- GetClasses(.local$theFrame)[col.idx]
 if(typ == "factor"){
   create.completion.model <- function(factor.levels) {
     store <- gtkListStoreNew("gchararray")
     sapply(factor.levels, function(x) store$set(store$append()$iter, 0, x))
     return(store)
   }
   factor.levels <- levels(theFrame[,col.idx])
   completion <- gtkEntryCompletionNew()
     # Assign the completion to the entry
   entry$setCompletion(completion)
 
     # Create a tree model and use it as the completion model
   completion.model <- create.completion.model(factor.levels)
   entry$setData("levels", factor.levels)    
   completion$setModel(completion.model)
   completion$setTextColumn(0)
   #completion$setMinimumKeyLength(0)
   completion$setInlineCompletion(TRUE)
   completion$setPopupSingleMatch(FALSE)
   .local$doingEntryIntoCompletion <- TRUE
     # move cursor down if you hit return after you select a match
        
   gSignalConnect(completion, "match-selected", after=T, function(widget, completion.model, iter){
     #print("match selected")
     #entry$setText(gtkTreeModelGetValue(completion.model, iter, 0)$value)
     EnterTextIntoCell(entry, .local)    
     MoveCursor(.local$view, "down", .local)
     return(FALSE)
   })
  entry$setData("completion", completion)
  entry$setData("pos", 0)  # match position
  entry$setData("current.match", character(0))  
  }
  return(FALSE)
}

EnterTextIntoCell <- function(entry, .local){
  w <- TransientWindow("", .local)  
  on.exit(w$destroy())  

  #print("e-out")
  #print(gObjectGetData(entry, "text"))
  if(gObjectGetData(entry, "text") != gtkEntryGetText(entry)){
    #tryCatch({
  	  col.idx <- entry$getData("index")  
  	  row.idx <- as.integer(entry$getData("path.str"))+1      
      keyval <- entry$getData("keyval")
      theText <- entry$getText()          
      nf <- data.frame(theText, stringsAsFactors=F)
      task <- list(list(func="ChangeCells", 
        arg = list(nf=nf, row.idx=row.idx, col.idx=col.idx)))
      DoTaskWrapper(.local, task)
    #}, error = function(e) warning(e) )
    #if(.local$doingEntryIntoCompletion){ #we're entering data into a factor 
    #    if (keyval == GDK_Return)
    #       MoveCursor(view, "down", .local)
    #}        
  } 
}
  

RendererEntryKeyPress <- function(entry, event, data) {
  .local <- data
  #print("RendererEntryKeyPress called")
  retval <- TRUE    
  event.time <- event[["time"]]
  keyval <- event[["keyval"]] 
  #cat(c("A", "B")[.local$allow.renderer.key.flag+1])
  if(1){# .local$allow.renderer.key.flag && event.time >= .local$last.time){
    .local$last.time <- event.time
    retval <- FALSE
    keyval <- event[["keyval"]]
    stat <- as.flag(event[["state"]])
    entry$setData("keyval", keyval)
  	entry$setData("stat", stat)
  	
    view <- .local$view
      # control keys for popup selection
    if(.local$doingEntryIntoCompletion) { # we're in the popup. or something.
      retval <- FALSE
              
      if (keyval %in% c(GDK_Return)){        
          # this is to handle returns on the entry completion
        current.match <- entry$getData("current.match") 
        if(length(current.match)) 
          entry$setText(current.match)
          
        if(nchar(entry$getText()) > 0) {  
          #EnterTextIntoCell(entry, .local)  
          #.local$allow.key.flag <- TRUE # unlock
          #.local$entry <- NULL                      
          MoveCursor(view, "down", .local)
        } 
        
       #.local$allow.key.flag <- TRUE # unlock
       #.local$entry <- NULL              
        #
        retval <- FALSE
      } else if( 0 && keyval == GDK_Escape){
        entry$setText(entry$getData("text"))
        MoveCursor(view, "down", .local)
        retval <- TRUE
      }
        # Keep track of entry completion
        # this is really hacky. first, get the matches in the entry
      ll <-  entry$getData("levels")
        # this may not be perfect...
      matches <- ll[regexpr(tolower(entry$getText()),tolower(ll)) == 1]
      if( length(matches) > 1) {  # because the popup is only for these matches
         # now keep track of the up/down arrow selection...
        if (keyval %in% c(GDK_Up, GDK_Down)){
          xx <- -1 
          if(keyval%in%c(GDK_Down)) 
            xx <- 1          
          xx <- (entry$getData("pos") + xx)%%(length(matches)+1)
        } else {
          xx <- 0        
        }
        #print(xx)
        #print(matches[xx])
        entry$setData("pos", xx)
        entry$setData("current.match", matches[xx])
      }
            
    }  else if (!keyval%in%myValidInputKeys && !keyval%in%myMetaKeys){  
    #print("Exiting")
      # EnterTextIntoCell(entry, .local)  # update the cell
      # .local$allow.key.flag <- TRUE # unlock
      # .local$entry <- NULL      
      if (keyval == GDK_Up || ShiftLetter(keyval, stat, GDK_Return)){
        MoveCursor(view, "up", .local)
      } else if (keyval == GDK_Down || keyval == GDK_Return) {
        MoveCursor(view, "down", .local)
      } else if (keyval == GDK_Left || keyval == GDK_ISO_Left_Tab) {
        MoveCursor(view, "left", .local)
      } else if (keyval == GDK_Right || keyval == GDK_Tab) {
        MoveCursor(view, "right", .local)
      }
      retval <- FALSE
    }
    if(keyval%in%myMetaKeys)
      retval <- TRUE
  } 
  #print(paste("RendererEntryKeyPress returning", retval)) 
  #print(paste("key flag is", .local$allow.key.flag)     )
  #print(.local$view$isFocus()) # Here's the problem....
  return(retval)
}

ColumnSetState <- function(allColumns, ii, state, .local){    
  if(length(ii) != 1 || ii < 1 || length(allColumns) < ii)
	  stop(paste("trying to set column index outside bounds:", ii))	
  #if(ii != length(allColumns))
  gtkWidgetSetState(allColumns[[ii]]$button,
    ifelse(state, as.integer(1), as.integer(0)))
  .local$allColumns <- allColumns
  if(state) {
    .local$selectedColumns <- sort(union(ii, .local$selectedColumns)) 
  } else {
    .local$selectedColumns <- sort(setdiff(.local$selectedColumns, ii)) 
  }
}  

UpdateColumnSelection <- function(.local, selectedColumns.new) {
    allColumns <- .local$allColumns
    selectedColumns <- GetSelectedColumns(.local, restrict=FALSE)
    ll <- length(allColumns)
    if(ll > 1){ # for zero columns, don't select anything
        # Don't activate the last column - it's left blank
      for (ii in setdiff(selectedColumns.new, selectedColumns))
        ColumnSetState(allColumns, ii, TRUE, .local)
      for (ii in setdiff(selectedColumns, selectedColumns.new))
        ColumnSetState(allColumns, ii, FALSE, .local)
    }
}

SelectAll <- function(.local){
  allColumns <- .local$allColumns
  UpdateColumnSelection(.local, 1:length(allColumns))
  if(length(.local$rectangles)){
    .local$rectangles <- list()
    .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
  }
  .local$do.paint <- TRUE
  
   # block transient interactions with a popup
  w <- TransientWindow("Selecting...", .local)
  on.exit(w$destroy())
  
  gtkTreeSelectionSelectAll(gtkTreeViewGetSelection(.local$view))    
  UpdateSelectionRectangle(.local)      
}

RowNamesButtonPress <- function(widget, event, data) {
  .local <- data
   # kill any open entries   
  typ <- event[['type']]
  stat <- event[['state']]
  info <- widget$getPathAtPos(event[["x"]], event[["y"]])
  if(is.null(info$path)) return(TRUE)         

  row.idx <- info$path$getIndices()+1
            
  UpdateColumnSelection(.local, integer(0))
  #if(length(.local$rectangles)){
  #  .local$rectangles <- list()
  #  .local$viewGetBinWindow$invalidateRect(NULL, FALSE)             
  #}
  if (event[["button"]] == as.integer(1)){
    if(row.idx > dim(.local$theFrame)[1]-1) return(TRUE)
    if(typ == as.integer(4)){ # single clicked     

    DoUserCall("RowClicked", list(idx = info$path$getIndices()+1), .local)
    DoUserCall("Selection", list(row.idx=info$path$getIndices()+1, col.idx=1:(dim(.local$theFrame)[2]-2)), .local)    

     # scroll main view in sync
    if( .local$scrollRowNames) { 
      .local$scrollRowNames <- FALSE
      gtkPropagateEvent(widget, event)
      gtkTreeSelectionUnselectAll(.local$ss)
      gtkAdjustmentSetValue(.local$sw.view.va, gtkAdjustmentGetValue(.local$sw.rn.va))
      .local$scrollRowNames <- TRUE 
      return(TRUE)
   }
     
   } else if(typ == as.integer(5)){ # ignore double click
     return(TRUE)
   }      
  } else if (event[["button"]] == as.integer(3)){ # our popup menu
    m <- Row3rdButtonMenu(.local, row.idx)
    gtkMenuPopupHack(m, button = event$GetButton(), 
      activate.time = gdkEventGetTime(event))
    return(TRUE)
  }    
  return(FALSE)
}

ViewButtonPress <- function(widget, event, data) {
  .local <- data
  model <- .local$model

  allColumns <- .local$allColumns
  info <- widget$getPathAtPos(event[["x"]], event[["y"]])
  renderer <- info$column$getCellRenderers()[[1]]

    # store last click info here
  .local$last_click_info <- info
  

  if(is.null(info$path)) return(TRUE)
  row.idx <- info$path$getIndices()+1
  col.idx <- GetColIdx(info$column)
  
  typ <- event[['type']]
  stat <- event[['state']]

  if (event[["button"]] == as.integer(3)){ # our popup menu
  	m <- Cell3rdButtonMenu(.local, row.idx, col.idx)
    gtkMenuPopupHack(m, button = event$GetButton(),
      activate.time = gdkEventGetTime(event))
    return(TRUE)
  } else if (event[["button"]] == as.integer(1)){    
    if(typ == as.integer(4)){ # single clicked
        # we want to keep going if user clicks on column to resize - hard to tell           
      if(is.null(info$path) || (identical(row.idx, 1) && info$cell.x < 5)) {   
        .local$rectangles <- list()
        return(FALSE)
      }
        
      selectedColumns <- GetSelectedColumns(.local)
      if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
        if(is.null(.local$start.column.select)) .local$start.column.select <- col.idx
        selectedColumns <- .local$start.column.select:col.idx          
      } else {      
         selectedColumns <- col.idx
        .local$start.column.select <- col.idx
        gsr <- GetSelectedRows(.local$view, .local)      
        gsc <- GetSelectedColumns(.local)
                 
        if(length(gsr) && length(gsc) && gsr[1] == row.idx && (length(gsr) > 1 || length(gsc) > 1 || col.idx != gsc) ) {
                  
          if(!is.null(.local$entry)) {
            .local$entry$unrealize()
            .local$entry <- NULL
          }     
          .local$do.paint <- TRUE          
          .local$rectangles <- list()          
#          if(1 || length(gsr) > 1) { # if there's a block of selected cells, set to one row
#             cat("*")           
             .local$view$setCursorOnCell(info$path, info$column, renderer, FALSE)                                            
#          }
          UpdateColumnSelection(.local, selectedColumns)                     
          UpdateSelectionRectangle(.local)                     
          return(TRUE)               
        }
      }
      UpdateColumnSelection(.local, selectedColumns) 
    }  else if (typ == as.integer(5)) { # ignore dclick
      return(TRUE)
    }
   }
   
   ddf <- dim(.local$theFrame)
     # disable input for last row and column without killing the cursor
   if (row.idx > ddf[1]-1 || col.idx == ddf[2]-1){
     .local$rectangles <- list()      
     force.reset.focus(.local)     
     #.local$view$setCursorOnCell(info$path, info$column, renderer, TRUE)       
     .local$view$setCursorOnCell(info$path, info$column, renderer, FALSE)       
     return(TRUE)
   }

    # reset the cursor so it's visible  
  .local$flash.cursor <- TRUE
    #  Flash the rectangles off when you click
  .local$do.paint <- TRUE        # 2-12-10
  if(length(.local$rectangles)){
#  print("resetting") 
    .local$rectangles <- list() # 3-2-10
	  widget$getBinWindow()$invalidateRect(NULL, FALSE)
  }
#print("Exiting ViewButtonPress")
  return(FALSE)
}

# Update rectangle from .local$view selected rows.
# Don't draw if only 1 selected square
# Don't update if nothing changed.
UpdateSelectionRectangle <- function(.local){
  widget <- .local$view
  gsr <- widget$getSelection()$getSelectedRows()$retval      	
  #gsr <- GetSelectedRows(.local$view, .local)
  allColumns <- .local$allColumns
  sw.va <- .local$sw.view.va
	selectedColumns <- GetSelectedColumns(.local) 
  path2.index <- path1.index <- integer(0)
  
  
  if(length(selectedColumns) && length(gsr)){
  	path1 <- gsr[[1]]
  	path2 <- gsr[[length(gsr)]]
  	path1.index <- path1$GetIndices()+1
  	path2.index <- path2$GetIndices()+1
  	if(length(path2.index) != 1 || length(path1.index) != 1){
  	  warning("path1 or path2 are length zero")
  	  return(FALSE)
  	}

    if(path2.index == dim(.local$theFrame)[1]) {
      if(path1.index == path2.index) return(FALSE)
      path2$prev()
    }
  	
  	currentVadj <- sw.va$getValue() # this gets called *before* value-changed
	  rect1 <- widget$getCellArea(path1, allColumns[[selectedColumns[1]]]$column)$rect
	  rect1$y <- rect1$y + currentVadj
	  rect2 <- widget$getCellArea(path2, 
      allColumns[[selectedColumns[length(selectedColumns)]]]$column)$rect
	  rect2$y <- rect2$y + currentVadj
	  .local$rectangles[[length(.local$rectangles)+1]] <- 
        gdkRectangleUnion(rect1, rect2)$dest
  	  # contiguous
  	  # we need this in windows but not osx or linux    
  } else {
	  .local$rectangles <- list()
  }

  if(!length(path1.index) || !length(path2.index)) {
     row.idx <- integer(0)
  } else {
     row.idx <- path1.index:path2.index
  }  

  DoUserCall("Selection", list(row.idx=row.idx, col.idx=selectedColumns-COLUMN_OFFSET), .local)    
      
	widget$getBinWindow()$invalidateRect(NULL, FALSE)
  return(FALSE)   
}  

ViewButtonRelease <- function(widget, event, data){
  #if(.local$do.rendererEditingStarted){ # if we've started editing, don't do anything
  .local <- data
  if(1 || is.null(.local$entry)){
    allColumns <- .local$allColumns
    sw.va <- .local$sw.view.va
        
    info <- widget$getPathAtPos(event$x, event$y)
    selectedColumns <- GetSelectedColumns(.local)
    col.idx <- GetColIdx(info$column)
    
      # Guess which way the user's gone outside the columns
    if(!length(col.idx)){      
      view <- .local$view
      sw.ha <- .local$sw.ha
      ptr <- view$getBinWindow()$getPointer()
      vr <- view$getVisibleRect()$visible.rect
      sw.ha.value <- sw.ha$value
      direction <- ifelse (ptr$x - sw.ha.value <= vr$width/2, -1, 1)
      if(direction == 1) col.idx <- length(allColumns)
      if(direction == -1) col.idx <- 1
    }
    
    if(!is.null(.local$start.column.select) && length(col.idx)) {
      selectedColumns <- .local$start.column.select:col.idx
    } else if (length(col.idx)) {
      selectedColumns <- col.idx
    }# otherwise col.idx is NA 
   UpdateColumnSelection(.local, selectedColumns)
     # This may or may not be needed. 4-10-10
   .local$rectangles <- list() # reset our rectangle-drawing
  # 3-2-10
   UpdateSelectionRectangle(.local)     
  }
}

  # Sync column selection with mouse-down
ViewMotionNotify <- function(widget, event){
#  while(gtkEventsPending())
#    gtkMainIteration()  
#  allColumns <- .local$allColumns
  pointer <- gdkWindowGetPointer(event[["window"]])
  pp <- gtkTreeViewGetPathAtPos(widget, event[["x"]], event[["y"]])
  dis <- gtkWidgetGetDisplay(widget)  
  win <- gtkWidgetGetWindow(widget)    
  if ((pp$cell.x-2)^2 + (pp$cell.y-2)^2 < 50){
    gdkWindowSetCursor(win, gdkCursorNewFromName(dis, "cross"))
  } else {
    gdkWindowSetCursor(win, gdkCursorNewFromName(dis, "arrow"))
  }

#  if (as.flag(pointer$mask) & GdkModifierType["button1-mask"]){
#    info <- gtkTreeViewGetPathAtPos(widget, pointer[["x"]], pointer[["y"]])
#    if (info$retval){
#      col.idx <- GetColIdx(info$column)
#      if(is.null(.local$start.select.column)) .local$start.select.column <- col.idx
#		  new.sel <- sort(.local$start.select.column:col.idx)
#		  UpdateColumnSelection(.local, new.sel)     
#   }
#  } 
  return(FALSE)
}

Cell3rdButtonMenu <- function(.local, row.idx, col.idx){
  typ <-  GetClasses(.local$theFrame)[col.idx+COLUMN_OFFSET]	  
  m <- gtkMenu()
  cutItem <- gtkMenuItem("Cut")
  copyItem <- gtkMenuItem("Copy (Ctrl-C)")
  copyWithNamesItem <- gtkMenuItem("Copy With Names (Ctrl-Shift-C)")
  pasteItem <- gtkMenuItem("Paste (Ctrl-V)")
  m$append(cutItem)
  m$append(copyItem)
  m$append(copyWithNamesItem)	  
  m$append(pasteItem)
  lapply(c(cutItem), gtkWidgetSetSensitive, FALSE)
  gSignalConnect(copyItem, "activate", 
    function(...) {
      w <- TransientWindow("Copying...", .local)
      on.exit(w$destroy())    
    CopyToClipboard(theFrame[GetSelectedRows(.local$view, .local), 
    GetSelectedColumns(.local)+COLUMN_OFFSET, drop=F])
    })
  gSignalConnect(copyWithNamesItem, "activate", 
    function(...) {
       w <- TransientWindow("Copying...", .local)
      on.exit(w$destroy())    
     CopyToClipboard(theFrame[GetSelectedRows(.local$view, .local), 
      GetSelectedColumns(.local)+COLUMN_OFFSET, drop=F], do.rownames=T, do.colnames=T)
     })      
  gSignalConnect(pasteItem, "activate", function(...) {         
  dat <- ReadFromClipboard(row.names=NULL, fill=TRUE, sep="\t", stringsAsFactors=F) # character vector
  task <- GetTaskPasteIn(.local$theFrame, dat, row.idx, col.idx+COLUMN_OFFSET)
  DoTaskWrapper(.local, task)
    
})      
  
  m$append(gtkSeparatorMenuItem())
  view <- .local$view	
  theFrame <- .local$theFrame
  editFactorsItem <- gtkMenuItem("Edit Factors...")
  randomizeItem <- gtkMenuItem("Randomize Selected")
  fillItem <- gtkMenuItem("Fill Down")	  
  fillCyclicItem <- gtkMenuItem("Fill In Cycles")
    m$append(editFactorsItem)
  m$append(randomizeItem)
  m$append(fillItem)
  m$append(fillCyclicItem)
  if(typ != "factor")
    lapply(c(editFactorsItem, fillCyclicItem), gtkWidgetSetSensitive, FALSE)
  gSignalConnect(editFactorsItem, "activate", function(...) {
   # DoFactorEditor(theFrame, .local, col.idx + COLUMN_OFFSET))
    DoFactorEditor(theFrame, .local$toplevel, col.idx + COLUMN_OFFSET, 
     FactorEditorHandler, data=.local)
  })
  gSignalConnect(randomizeItem, "activate", function(...){
       sr <- GetSelectedRows(.local$view, .local)                            
       if(length(sr)==0) sr <- 1:(dim(.local$theFrame)[1]-1)
       if(length(sr)){
         entry.frame <- theFrame[sr, GetSelectedColumns(.local)+1, drop=F]
         entry.frame <- entry.frame[sample(1:(dim(entry.frame)[1])),,drop=F]
         task <- list(list(func="ChangeCells", 
            arg = list(nf=entry.frame, row.idx=sr, col.idx=GetSelectedColumns(.local)+1)))
         DoTaskWrapper(.local, task)
       }
     })  
  gSignalConnect(fillItem, "activate", function(...){
       sr <- GetSelectedRows(.local$view, .local)
       sc <- GetSelectedColumns(.local)+1
       if(length(sr)==0) sr <- 1:(dim(.local$theFrame)[1]-1)  # None selected: fill the whole column
       if(length(sr)==1) sr <- sr:(dim(.local$theFrame)[1]-1) # One selected: fill everything below
       if(length(sr) && length(sc)){
           # if the user's selected noncontiguous rows, we're still drawing just one selected rectangle
           # so we want to fill the whole thing anyway           
         sr <- range(sr)[1]:range(sr)[2]
         entry.frame <- theFrame[sr, sc, drop=F]
         for(jj in 1:length(sc))
           entry.frame[,jj] <- entry.frame[1,jj]
         task <- list(list(func="ChangeCells", 
            arg = list(nf=entry.frame, row.idx=sr, col.idx=GetSelectedColumns(.local)+1)))
         DoTaskWrapper(.local, task)
       }
     })  
  gSignalConnect(fillCyclicItem, "activate", function(...) {
       sr <- GetSelectedRows(.local$view, .local)
       cc <- col.idx+COLUMN_OFFSET         
       df1 <- dim(.local$theFrame)[1]
       if(length(sr)==0) sr <- 1:(dim(.local$theFrame)[1]-1)  # None selected: fill the whole column
       if(length(sr)==1) sr <- sr:(dim(.local$theFrame)[1]-1) # One selected: fill everything down              
       if(length(sr) < 2 && df1 > 1) sr <- 1:(df1-1)
                
       if(length(sr)) {
           # if the user's selected noncontiguous rows, we're still drawing just one selected rectangle
           # so we want to fill the whole thing anyway           
         sr <- range(sr)[1]:range(sr)[2]
                
         DoBlockSize(
          theFrame[sr, cc],
          .local$toplevel,
          BlockSizeHandler,
          data =  list(.local=.local, row.idx=sr, col.idx=cc), 
          start.lvl=theFrame[sr[1], cc])
         }
     })
  return(m)
}

# Function to call when the data is sorted
SortHandler <- function(new.order, .local){
  dd <- dim(.local$theFrame)
  if(dd[1] > 1){
    theFrame <- .local$theFrame[new.order, ,drop=F]
    task <- list(
      list(func="ChangeRowNames", 
        arg = list(theNames = rownames(theFrame), row.idx=1:(dd[1]-1))),
      list(func="ChangeCells", 
        arg = list(nf=theFrame, row.idx=1:(dd[1]-1))))
    DoTaskWrapper(.local, task)
  }
}

Corner3rdButtonMenu <- function(.local){	
  theFrame <- .local$theFrame
  m <- gtkMenu()
  cutItem <- gtkMenuItem("Cut")
  copyItem <- gtkMenuItem("Copy")
  pasteItem <- gtkMenuItem("Paste...")
  m$append(cutItem)
  m$append(copyItem)	  
  m$append(pasteItem)
  m$append(gtkSeparatorMenuItem())
  gSignalConnect(copyItem, "activate", function(...) {
      w <- TransientWindow("Copying...", .local)
      on.exit(w$destroy())    
    CopyToClipboard(MakeExternalDataFrame(theFrame, .local), do.rownames=T, do.colnames=T)
    })
  gSignalConnect(pasteItem, "activate", function(...){
    DoPasteDialog(.local)
  })      
  lapply(c(cutItem), 
    gtkWidgetSetSensitive, FALSE)
    
  renameItem <- gtkMenuItem("Rename Dataset")
  gSignalConnect(renameItem, "activate", function(...) {
     obj <- .local$rowname.column.obj$eventbox
     EditHeaderBox(obj, handler = function(obj, event, data){
        .local = data$.local
        box = data$box
        label = data$label
        getText <- obj$getText()
    	  if(nchar(getText) > 0) {
          label$setText(getText)          
      	  #assign(getText, MakeExternalDataFrame(.local$theFrame, .local$dataset.class), envir=.GlobalEnv)
      	  .local$dataset.name <- getText	  
             # 3-13-10
        to.external <- MakeExternalDataFrame(.local$theFrame, .local)
				nam <- .local$dataset.name
				# clean up spaces
				my.assign <- function(nam)
					 eval(parse(text=paste(paste(".GlobalEnv", nam, sep="$"), "<- to.external")))
				tryCatch({
					my.assign(nam)
				}, error = function(e) {  # User called it something strange
					my.assign(deparse(nam))
				})
  
          message(paste("RGtk2DfEdit: Creating dataset", .local$dataset.name, "in global environment."))
        }
        obj$destroy()
        box$packEnd(label, FALSE, FALSE, 0)       
        gtkWidgetSetState(.local$rowname.column.obj$button, as.integer(0))
        FALSE
      }, data=list(.local=.local, data=NULL))            
    }
  )   #..
  m$append(renameItem)  
  m$append(gtkSeparatorMenuItem())  
      
    
  openItem <- gtkMenuItem("Open File...")  
  m$append(openItem)	        
  gSignalConnect(openItem, "activate", function(...){
    tryCatch({	    
      file.name <- file.choose()
      dataset.name <- basename(file.name)
      ww <- which(strsplit(dataset.name, "")[[1]]%in%".")        
      if(length(ww) && rev(ww)[1] > 1)
        dataset.name <- substr(dataset.name, 1, rev(ww)[1]-1)

      if(nchar(dataset.name) > 0){
        DoImportDialog(data=list(file.name=file.name,.local=.local)) 
        ModelChangeDatasetName(.local, dataset.name)
      }
    }, error = function(e){warning(e)} )       
  })      
  saveItem <- gtkMenuItem("Save As CSV...")  
  m$append(saveItem)	        
  gSignalConnect(saveItem, "activate", function(...){
    tryCatch({	    
      theName <- paste(.local$dataset.name, "csv", sep=".")
      theFile <- choose.files(default=theName, multi=F)
      if(length(theFile) > 0 && nchar(theFile))
        write.csv(MakeExternalDataFrame(.local$theFrame, .local), theFile, quote=F)
    }, error = function(e){warning(e)})       
  })      
      
  m$append(gtkSeparatorMenuItem())  
    	  
  sortItem <- gtkMenuItem("Sort...")	  
  gSignalConnect(sortItem, "activate", function(...){
    dd <- dim(.local$theFrame)
    DoSortDialog(.local$theFrame[-dd[1], -dd[2],drop=F], SortHandler, .local)
  })
  m$append(sortItem)
  m$append(gtkSeparatorMenuItem())
  # move dataset 1 column along
  ordinalItem <- gtkMenuItem("Default Rows")
  gSignalConnect(ordinalItem, "activate", function(...) {
    dd1 <- dim(.local$theFrame)[1]
    if(dd1 > 1){
      task <- list(
        list(func="ChangeRowNames", 
          arg = list(theNames = 1:(dd1-1), row.idx=1:(dd1-1))))
      DoTaskWrapper(.local, task)
    }
  })
  m$append(ordinalItem)      
  m$append(gtkSeparatorMenuItem())
  ordinalItem <- gtkMenuItem("Default Columns")
  gSignalConnect(ordinalItem, "activate", function(...) {
    dd2 <- dim(.local$theFrame)[2]
    if(dd2 > 2){
      task <- list(
        list(func="ChangeColumnNames", 
          arg = list(theNames = DEFAULT_COLNAMES[1:(dd2-2)], col.idx=2:(dd2-1))))
      DoTaskWrapper(.local, task)
    }
  })

  m$append(ordinalItem)      
  m$append(gtkSeparatorMenuItem())
  setNameItem <- gtkMenuItem("Edit Dataset Name")
  m$append(setNameItem)	  
  
  lapply(c(setNameItem), gtkWidgetSetSensitive, FALSE)
  
  m$append(gtkSeparatorMenuItem())
  aboutItem <- gtkMenuItem("About...")
  m$append(aboutItem)	  
  gSignalConnect(aboutItem, "activate", function(...){
	  dlg <- gtkAboutDialogNew(show=F)
    dlg["authors"] <- c("Tom Taverner <Thomas.Taverner@pnl.gov>", 
"for the Department of Energy (PNNL, Richland, WA)",
"2010, Battelle Memorial Institute", 
"Contributions from John Verzani", 
"RGtk2 by Michael Lawrence and Duncan Temple Lang",
"cairoDevice by Michael Lawrence")
    dlg["program-name"] <- "RGtk2DfEdit"
    dlg["comments"] <- "A spreadsheet data frame editor for the R environment"
    dlg["copyright"] <- "GPLv2"
    dlg["version"] <- VERSION_STRING
    gtkDialogRun(dlg)
    gtkWidgetDestroy(dlg)
  })
  	  
  return(m)
}


Row3rdButtonMenu <- function(.local, row.idx){	  
  theFrame <- .local$theFrame
  m <- gtkMenu()
  cutItem <- gtkMenuItem("Cut")
  copyItem <- gtkMenuItem("Copy")
  pasteItem <- gtkMenuItem("Paste")
  m$append(cutItem)
  m$append(copyItem)
  m$append(pasteItem)
  m$append(gtkSeparatorMenuItem())
  gSignalConnect(copyItem, "activate", function(...) {
      w <- TransientWindow("Copying...", .local)
      on.exit(w$destroy())      
     CopyToClipboard(theFrame[GetSelectedRows(.local$view.rn, .local),-c(1, dim(theFrame)[2]), drop=F], do.rownames=T)
     })
  gSignalConnect(pasteItem, "activate", function(...) {
    dat <- ReadFromClipboard(row.names=1, sep="\t", stringsAsFactors=F) # character vector
    task <- GetTaskPasteIn(.local$theFrame, dat, row.idx, 2, do.rownames= T)
    DoTaskWrapper(.local, task)
  })                     

  insertItem <- gtkMenuItem("Insert")
  gSignalConnect(insertItem, "activate", function(...) {
     task <- list(list(func="InsertNARows", 
                 arg = list(row.idx=row.idx)))
     DoTaskWrapper(.local, task)
  })
  deleteItem <- gtkMenuItem("Delete")
  gSignalConnect(deleteItem, "activate", function(...){	  
    sr <- GetSelectedRows(.local$view.rn, .local)
    if(length(sr)){
	    task <- list(list(func="DeleteRows", arg = list(row.idx=sr)))
      DoTaskWrapper(.local, task)
    }
  })
  clearItem <- gtkMenuItem("Clear Contents")
  m$append(insertItem)
  m$append(deleteItem)
  m$append(clearItem)
  gSignalConnect(clearItem, "activate", function(...){	  	  
    row.idx <- GetSelectedRows(.local$view.rn, .local)
    RowNamesClearContents(row.idx, .local)
  })

  lapply(c(cutItem), gtkWidgetSetSensitive, FALSE)
  if(!length(GetSelectedRows(.local$view.rn, .local)))	  
	  lapply(c(deleteItem, pasteItem), gtkWidgetSetSensitive, FALSE)
  if(row.idx == dim(theFrame)[1])	  
	  lapply(c(cutItem, copyItem, deleteItem, pasteItem, clearItem, cutItem), 
      gtkWidgetSetSensitive, FALSE)  
      
  m$append(gtkSeparatorMenuItem())
  setAsNamesItem <- gtkMenuItem("To Column Names")
  m$append(setAsNamesItem)	  

  gSignalConnect(setAsNamesItem, "activate", function(...){
    theNames <- as.character(theFrame[row.idx,-1])
    theNames[is.na(theNames)] <- ""	    
    theNames <- make.unique(theNames)	    
    dd2 <- dim(.local$theFrame)[2]
    task <- list(
      list(func="ChangeColumnNames", 
        arg = list(theNames = theNames, col.idx=2:dd2)),
      list(func="DeleteRows", 
        arg = list(row.idx=row.idx))
     )
    DoTaskWrapper(.local, task)      
  })    	      	                	  
  
  return(m)
}


ColumnClearContents <- function(col.idx, .local){
  if(length(col.idx)==0) return(FALSE)
  nf <- .local$model[,col.idx, drop=F]        
  stopifnot(ncol(nf) > 0)  
  nf[,] <- ""
  task <- list(list(func="ChangeCells", 
    arg = list(nf=nf, col.idx=col.idx)))
  DoTaskWrapper(.local, task)    
}

Column3rdButtonMenu <- function(.local, col.idx){	
  theFrame <- .local$theFrame
  typ <- GetClasses(theFrame)[col.idx+COLUMN_OFFSET]
  lastColumn <- col.idx == length(.local$allColumns)  # is this the last column
   
  theColumn <- theFrame[,col.idx+COLUMN_OFFSET,drop=F]
  m <- gtkMenu()
  cutItem <- gtkMenuItem("Cut")
  copyItem <- gtkMenuItem("Copy")
  pasteItem <- gtkMenuItem("Paste")
  m$append(cutItem)
  m$append(copyItem)
  m$append(pasteItem)
  m$append(gtkSeparatorMenuItem())
  gSignalConnect(copyItem, "activate", 
    function(...) {
      w <- TransientWindow("Copying...", .local)
      on.exit(w$destroy())        
      CopyToClipboard(.local$theFrame[-dim(.local$theFrame)[1],GetSelectedColumns(.local)+1,drop=F],
       do.colnames=T)
       })
  gSignalConnect(pasteItem, "activate", function(...) {
    dat <- ReadFromClipboard(header=T, sep="\t", stringsAsFactors=F) # character vector
    task <- GetTaskPasteIn(.local$theFrame, dat, 
      1, col.idx+COLUMN_OFFSET, do.colnames= T)
    DoTaskWrapper(.local, task)
   })

  renameItem <- gtkMenuItem("Rename Column")
  gSignalConnect(renameItem, "activate", function(...) {
      obj <- .local$allColumns[[col.idx]]$eventbox
      EditHeaderBox(obj, handler = function(obj, event, data){
        col.idx = data$data
        box = data$box
        label = data$label
        .local <- data$.local
  
        new.name <- obj$getText()
        task <- list(list(func="ChangeColumnNames", 
         arg = list(theNames = new.name, col.idx=col.idx+COLUMN_OFFSET)))
        obj$destroy()
        box$packEnd(label, FALSE, FALSE, 0)
        if (new.name != colnames(.local$theFrame)[col.idx+COLUMN_OFFSET])
          DoTaskWrapper(.local, task)        
        FALSE
      }, data=list(.local=.local, col.idx=col.idx))
    }
  )   #..
    
  insertItem <- gtkMenuItem("Insert Column")
  deleteItem <- gtkMenuItem("Delete Column")
  clearItem <- gtkMenuItem("Clear Contents") 
  m$append(renameItem)
  m$append(gtkSeparatorMenuItem())    
  m$append(insertItem)
  m$append(deleteItem)
  m$append(clearItem)
  m$append(gtkSeparatorMenuItem())  
  gSignalConnect(clearItem, "activate", function(...) ColumnClearContents(GetSelectedColumns(.local)+COLUMN_OFFSET, .local))
  gSignalConnect(insertItem, "activate", function(...) {
     task <- list(list(func="InsertNAColumns", 
                 arg = list(col.idx=col.idx+COLUMN_OFFSET)))
     DoTaskWrapper(.local, task)                       
  })
  gSignalConnect(deleteItem, "activate", function(...) {
     sc <- GetSelectedColumns(.local)+COLUMN_OFFSET
     if(length(sc)){	    
       task <- list(list(func="DeleteColumns", 
                   arg = list(col.idx=GetSelectedColumns(.local)+COLUMN_OFFSET)))
       DoTaskWrapper(.local, task)	  
     }
 })  
  
  sortItem <- gtkMenuItem("Sort...")	  
  gSignalConnect(sortItem, "activate", function(...) {
    dd <- dim(.local$theFrame)
    DoSortDialog(.local$theFrame[-dd[1], -dd[2],drop=F], SortHandler, .local)
  })
  m$append(sortItem)
  m$append(gtkSeparatorMenuItem())
  
  	  
  lapply(c(cutItem), gtkWidgetSetSensitive, FALSE)
  if(col.idx == length(.local$allColumns)) 
    lapply(c(deleteItem), gtkWidgetSetSensitive, FALSE)

    # If it's a factor, allow coercion to level type or numeric ordering
  if ("factor"%in%typ[[1]]) {
		item <- gtkCheckMenuItem("To Factor Levels")
		levelType <- class(levels(theFrame[,col.idx+COLUMN_OFFSET]))
		if(!is.atomic(levelType)) levelType <- "character" # default to character
		gSignalConnect(item, "button-release-event", function(item, evt, levelType){
	    task <- list(list(func="CoerceColumns", 
        arg = list(theClasses = levelType, col.idx=col.idx+COLUMN_OFFSET, to.levels=TRUE)))
      DoTaskWrapper(.local, task)  		  
			m$popdown()               
			return(TRUE)
		  }, levelType)
		m$append(item) 		
		
 		item <- gtkCheckMenuItem("To Factor Ordering")  		
		gSignalConnect(item, "button-release-event", function(item, evt, levelType){
	    task <- list(list(func="CoerceColumns", 
        arg = list(theClasses = "integer", col.idx=col.idx+COLUMN_OFFSET)))
      DoTaskWrapper(.local, task)  		  
			m$popdown()               
			return(TRUE)
		  }, levelType)
		m$append(item) 		 		
 		              
  }  else {  
    dataTypeNames <- list(Character="character", Integer="integer", Factor="factor", Logical="logical", Numeric="numeric")
    dataTypeItems <- list()
    for(theNewTypeName in names(dataTypeNames)){
  		item <- gtkCheckMenuItem(theNewTypeName)
  		item$setDrawAsRadio(TRUE)
  		dataTypeItems[[length(dataTypeItems)+1]] <- item
  		gSignalConnect(item, "button-release-event", function(item, evt, theNewTypeName){  		
  	    task <- list(list(func="CoerceColumns", 
          arg = list(theClasses = dataTypeNames[[theNewTypeName]], col.idx=GetSelectedColumns(.local)+COLUMN_OFFSET)))
        DoTaskWrapper(.local, task)  		  
  			m$popdown()               
  			return(TRUE)
  		  }, theNewTypeName)
  		if (dataTypeNames[[theNewTypeName]]%in%typ[[1]]) item$setActive(TRUE)  
  		m$append(item)
    }
  }
  m$append(gtkSeparatorMenuItem())
  editFactorsItem <- gtkMenuItem("Edit Factors...")  
  m$append(editFactorsItem)  
  m$append(gtkSeparatorMenuItem())
  abbreviateItem <- gtkMenuItem("Shorten")
  m$append(abbreviateItem)
  setAsNamesItem <- gtkMenuItem("To Row Names")
  m$append(setAsNamesItem)

  gSignalConnect(editFactorsItem, "activate", function(...) 
    DoFactorEditor(theFrame, .local$toplevel, col.idx + COLUMN_OFFSET, 
      FactorEditorHandler, data=.local))  
  if(typ != "factor") editFactorsItem$setSensitive(FALSE)
  
  gSignalConnect(abbreviateItem, "activate", function(...){
     abcol <- data.frame(X=cbind(abbreviate(as.character(theColumn[[1]]), minlength=10)))	     
     task <- list(
        list(func="CoerceColumns", 
          arg = list(theClasses = "character", idx=col.idx+COLUMN_OFFSET)),
         list(func="ChangeCells", 
         arg = list(nf=abcol, col.idx=col.idx+COLUMN_OFFSET))
    )
     DoTaskWrapper(.local, task)               
  })	  
  gSignalConnect(setAsNamesItem, "activate", function(...){
    theNames <- as.character(theColumn[[1]])
    theNames[is.na(theNames)] <- ""	    
    theNames <- make.unique(theNames)
    dd1 <- dim(.local$theFrame)[1]
    task <- list(
      list(func="ChangeRowNames", 
        arg = list(theNames = theNames, row.idx=1:dd1)),
      list(func="DeleteColumns", 
        arg = list(col.idx=col.idx+COLUMN_OFFSET))
    )
    DoTaskWrapper(.local, task)      
  })
  
 if(lastColumn) { # disable everything except for insert
   lapply(m$getChildren(), gtkWidgetSetSensitive, FALSE)
   lapply(list(insertItem), gtkWidgetSetSensitive, TRUE)
  }
  return(m)
}

# just update the dataset name when we change the text
CornerBoxButtonPress <- function (obj, event, data){
  .local <- data$.local
  gtkWidgetGrabFocus(.local$view)
    # get out of editing or selection
  button <- event[['button']]
  typ <- event[['type']]
  stat <- event[['state']]
  col.idx <- data$col.idx
  if (button == as.integer(1)){
   if(typ == as.integer(4)){ # single clicked
     gtkTreeViewGetSelection(.local$view.rn)$unselectAll()
     SelectAll(.local)     
     
   } else if(0 && typ == as.integer(5)){ # double clicked 
    gtkWidgetSetState(.local$rowname.column.obj$button, as.integer(1))
               
    EditHeaderBox(obj, handler = function(obj, event, data){
        .local = data$.local
        box = data$box
        label = data$label
        getText <- obj$getText()
    	  if(nchar(getText) > 0) {
          label$setText(getText)                                                      
      	  #assign(getText, MakeExternalDataFrame(.local$theFrame, .local$dataset.class), envir=.GlobalEnv)
      	  .local$dataset.name <- getText	  
             # 3-13-10
        to.external <- MakeExternalDataFrame(.local$theFrame, .local)
				nam <- .local$dataset.name
				# clean up spaces
				my.assign <- function(nam)
					 eval(parse(text=paste(paste(".GlobalEnv", nam, sep="$"), "<- to.external")))
				tryCatch({
					my.assign(nam)
				}, error = function(e) {  # User called it something strange
					my.assign(deparse(nam))
				})
  
          message(paste("RGtk2DfEdit: Creating dataset", .local$dataset.name, "in global environment."))
        }
        obj$destroy()
        box$packEnd(label, FALSE, FALSE, 0)       
        gtkWidgetSetState(.local$rowname.column.obj$button, as.integer(0))
        FALSE
      }, data=list(.local=.local, data=NULL))
  	} # end double clicked
  	} # end clicked
    if (button == as.integer(3)){ # our popup menu  
  	m <- Corner3rdButtonMenu(.local)
    gtkMenuPopupHack(m, button = event$GetButton(),
        activate.time = gdkEventGetTime(event))
    return(FALSE)
  }
  return(TRUE)
}

ModelChangeDatasetName <- function(.local, theName){
  .local$dataset.name <- theName
  .local$rowname.column.obj$eventbox$getChildren()[[1]]$getChildren()[[1]]$setText(theName)
}
   
# Sets up column header box for editing
# handler is function to call on focus out event
# Passes to handler: list(data=data, box=box, label=label, .local=.local)
EditHeaderBox <- function(obj, handler, data){
  .local <- data$.local
  col.idx <- data$col.idx

  box = obj$getChildren()[[1]]
  label = box$getChildren()[[1]]
  height = box$allocation$height
  box$remove(label)
  entry <- gtkEntryNew()
  entry$setText(label$getText())
  entry$setHasFrame(FALSE)
  #entry$modifyBase(as.integer(1), selectedColumnColor)    
  entry$modifyBase(as.integer(1), as.GdkColor(c(255,255,255)*256))  
  sapply(as.integer(0:1), function(y) entry$modifyText(y, as.GdkColor("black")))    
  entry$modifyBase(as.integer(1), as.GdkColor(c(255,255,255)*256))  
  entry$setAlignment(1)
  entry$setSizeRequest(-1, height-HEADER_BOX_MARGIN) # 1 pixel margins I guess?
  box$packEnd(entry, FALSE, FALSE, 0)
  entry$grabFocus()
  gSignalConnect(entry, "key-press-event", function(obj, event, data=col.idx){
    if (event[["keyval"]]%in%myValidNavigationKeys) .local$view$grabFocus()
    return(FALSE)
  })    
  #gSignalConnect(entry, "button-press-event", function(obj, event, data=col.idx){
  #  return(TRUE)
  #})      
  
  gSignalConnect(entry, "focus-out-event", handler, data=list(data=col.idx, box=box, label=label, .local=.local))
  
  #gSignalConnect(entry, "unrealize", handle)  
  
  #.local$entry <- entry  
  return(TRUE)
}       

# column header clicked
# obj is eventbox
ColumnHeaderButtonPress <- function (obj, event, data){
  .local <- data$.local  
  
  # get out of editing or selection    
  view <- .local$view
  view$grabFocus()    
  view.rn <- .local$view.rn      
  model <- .local$model
  allColumns <- .local$allColumns
  
      # kill any active entries
  if(!is.null(.local$entry)) {
    #tryCatch({
    .local$entry$unrealize()  
    .local$entry <- NULL
    #}, silent=F)
  }
      
  # unselect the main view
  if(length(.local$rectangles)){
    .local$rectangles <- list()
    .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
  }      
  for(tv in c(view, view.rn)){
    pfc <- gtkTreeViewGetCursor(tv)
    gtkTreeSelectionUnselectAll(gtkTreeViewGetSelection(tv))
  }                         
  
  button <- event[['button']]
  typ <- event[['type']]
  stat <- event[['state']]
  col.idx <- data$col.idx
  
    # ignore d-click on last column
  # ignore if it's the end column  
  lastColumn <- col.idx == length(allColumns)

  selectedColumns.new <- integer(0) # tom 092610
    # update: don't let double-click edit.    
  if (0 && !lastColumn && button == as.integer(1) && typ == as.integer(5) && stat == as.integer(0)){ # double clicked
    EditHeaderBox(obj, handler = function(obj, event, data){
      col.idx = data$data
      box = data$box
      label = data$label
      .local <- data$.local

      new.name <- obj$getText()
      task <- list(list(func="ChangeColumnNames", 
       arg = list(theNames = new.name, col.idx=col.idx+COLUMN_OFFSET)))
      obj$destroy()
      box$packEnd(label, FALSE, FALSE, 0)
      if (new.name != colnames(.local$theFrame)[col.idx+COLUMN_OFFSET])
        DoTaskWrapper(.local, task)        
      FALSE
    }, data=list(.local=.local, col.idx=col.idx))
  } else if (button == as.integer(1) && typ == as.integer(4)){ # column clicked  	
    selectedColumns <- GetSelectedColumns(.local)
    if (stat == as.integer(0)) { # select only this column
       selectedColumns.new <- col.idx                       
    # extend shift-selection
    .local$start.column.select <- col.idx
  } else if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
    if(is.null(.local$start.column.select)){
      .local$start.column.select <- col.idx
    }
    selectedColumns.new <- .local$start.column.select:col.idx  
  } else if (as.flag(stat) & GdkModifierType['control-mask']) { # range
     selectedColumns.new <-
      c(union, setdiff)[[col.idx%in%selectedColumns+COLUMN_OFFSET]](selectedColumns, col.idx)
  } else {
    .local$start.column.select <- NULL
  }
  UpdateColumnSelection(.local, selectedColumns.new)  

  DoUserCall("ColumnClicked", list(idx = data$col.idx), .local)
  DoUserCall("Selection", list(row.idx=1:(dim(.local$theFrame)[1]-1), col.idx=GetSelectedColumns(.local)-COLUMN_OFFSET), .local)          
        
  } # clicked
  
  selectedColumns <- GetSelectedColumns(.local)
  if(length(selectedColumns)){
    .local$do.paint <- TRUE
    ma <- MergeAdjacent(sort(selectedColumns))
    rectangles <- vector("list", ma$length)    
    for(i in 1:ma$length){    
      col.start <- allColumns[[selectedColumns[ma$start[i]]]]$column
      if(ma$start[i] == ma$end[i])
        col.end <- col.start
      else
        col.end <- allColumns[[selectedColumns[ma$end[i]]]]$column    
      rect.start <- gtkTreeViewGetCellArea(view, .local$FIRST_PATH, col.start)$rect
      rect.end <- gtkTreeViewGetCellArea(view, .local$LAST_PATH, col.end)$rect
      rect <- gdkRectangleUnion(rect.start, rect.end)$dest
      rect$y <- rect$y + .local$sw.view.va$getValue()
      rect$height <- .local$sw.view.va[["upper"]]
      rectangles[[i]] <- rect
    }
    .local$rectangles <- rectangles
    .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
  }
          
  if (button == as.integer(3)){ # our popup menu
	  m <- Column3rdButtonMenu(.local, col.idx)
    gtkMenuPopupHack(m, button = event$GetButton(),
        activate.time = gdkEventGetTime(event))
    return(FALSE)
   }
  return(TRUE)
}

GetVerticalPosition <- function(.local){
  .local$sw.view.va$getValue()
}

SetVerticalPosition <- function(.local, value){
  .local$sw.view.va$setValue(value)
  .local$sw.rn.va$setValue(value)
}
# replace the entire gtktab
# new.df is the internal DF representation
# want to keep cursor position if it exists
ReplaceEditWindow <- function(theFrame, .local){
  old.v.pos <- GetVerticalPosition(.local)
  #old.h.pos <- .local$sw.ha$getValue()   

  UpdateColumnSelection(.local, integer(0))       
  .local$theFrame <- theFrame
  gtktab.new <- MakeDFEditWindow(.local, theFrame) 
    
  .local$gtktab$destroy()  
  .local$gtktab <- gtktab.new
  .local$group.main$packStart(.local$gtktab, TRUE, TRUE, 0)
  .local$start.column.select <- NULL

  gTimeoutAdd(50, function(){SetVerticalPosition(.local, old.v.pos); return(FALSE)})
}      

NewColumnToggle <- function(model, j, width=64, is.row = F, is.editable=T, pretty_print=F, resizable=T){
	renderer <- gtkCellRendererToggleNew()
	column <- gtkTreeViewColumnNew()  
  column$packStart(renderer)
  column$setTitle(as.character(j-1))
	gtkTreeViewColumnSetFixedWidth(column, width)
	gtkTreeViewColumnSetSizing(column, GtkTreeViewColumnSizing['fixed'])
	gtkTreeViewColumnSetResizable(column, resizable)
	return(list(column=column, renderer=renderer, col.idx=j))
}

# Old code for pretty print:
#          txt <- .Call("R_getGObjectProps", cell, "text", PACKAGE = "RGtk2") # ~5 microseconds
#          if(txt == "1.#QNAN0" || txt == "-1.#IND00") # ~7 microseconds
#            .Call("R_setGObjectProps", cell, list(text="NA"), PACKAGE = "RGtk2")
#          else # ~13 microseconds, 22 if you suppress warnings
#            .Call("R_setGObjectProps", cell, list(text=sprintf(SPRINTF_FORMAT, as.numeric(txt))), PACKAGE = "RGtk2")    

# create a column with the title as the index
NewColumn <- function(model, j, width=64, is.row = F, is.editable=T, pretty_print=F, resizable=T, SPRINTF_FORMAT="%.6G"){
	renderer <- gtkCellRendererTextNew()
	#gtkCellRendererTextSetFixedHeightFromFont(renderer, 1)
	gObjectSetData(renderer, "column", j-1)
	renderer['xalign'] <- 1
	renderer['editable-set'] <- TRUE
	renderer['editable'] <- is.editable
	column <- gtkTreeViewColumnNewWithAttributes(as.character(j-1), renderer, text = j-1)  
	gtkTreeViewColumnSetFixedWidth(column, width)
	gtkTreeViewColumnSetSizing(column, GtkTreeViewColumnSizing['fixed'])
	gtkTreeViewColumnSetResizable(column, resizable)
	if(identical(pretty_print, TRUE)) {
	  cmj <- class(model[,j])
  	if(identical(cmj, "numeric"))
      gtkTreeViewColumnSetCellDataFunc(column, renderer, 
        function(tree.column, cell, tree.model, iter, data){
         x <- .RGtkCall("S_gtk_tree_model_get_value", tree.model, iter, data)$value
         if(is.na(x))
            .Call("R_setGObjectProps", cell, list(text="NA"), PACKAGE = "RGtk2")
         else if(is.numeric(x))
           .Call("R_setGObjectProps", cell, list(text=sprintf(SPRINTF_FORMAT,x)), PACKAGE = "RGtk2")
        }, func.data=as.integer(j-1))
    else
      gtkTreeViewColumnSetCellDataFunc(column, renderer, 
        function(tree.column, cell, tree.model, iter, data){ 
         x <- .RGtkCall("S_gtk_tree_model_get_value", tree.model, iter, data)$value
         if(is.na(x))
            .Call("R_setGObjectProps", cell, list(text="NA"), PACKAGE = "RGtk2")
#          else
#           .Call("R_setGObjectProps", cell, list(text=sprintf(SPRINTF_FORMAT,x)), PACKAGE = "RGtk2")    
        }, func.data=as.integer(j-1))
  }        
    
	return(list(column=column, renderer=renderer, col.idx=j))
}

# Must be called after widget is mapped
MakeButtonAndEventBox <- function(col.obj, label.str, handler, .local){
	label <- gtkLabelNew(label.str)
	box <- gtkHBoxNew()
	#gtkBoxPackEnd(box, label, FALSE, FALSE, 0)
	gtkContainerAdd(box, label)	
	eventbox <- gtkEventBoxNew()
	gtkContainerAdd(eventbox, box)
	view.col <- col.obj$column
	gtkTreeViewColumnSetWidget(view.col, eventbox)
	alignment <- gtkWidgetGetParent(eventbox)
	gtkAlignmentSet(alignment, 0, 1, 1, 1)
	col.idx <- GetColIdx(view.col)
	col.obj$eventbox <- eventbox
  gtkWidgetModifyBg(eventbox, as.integer(1), selectedColumnColor)
	col.obj$button <- gtkWidgetGetParent(gtkWidgetGetParent(alignment))
	gSignalConnect(eventbox, "button-press-event", handler, data=list(col.idx=col.idx, .local=.local))
	return(col.obj)
}

                              
UpdateDfEditor <- function(.local, theFrame, rows.changed=NULL){ 
  if (is.null(rows.changed)){
    if(ncol(theFrame) != ncol(.local$model)){
      ReplaceEditWindow(theFrame, .local)
    } else {
      cn <- colnames(theFrame)
      for(jj in which(cn != colnames(.local$theFrame)))
        ModelChangeColumnName(.local, jj, cn[jj])
      .local$model$setFrame(theFrame)
    }    
    .local$LAST_PATH <- MakeLastPath(theFrame)
  } else {
     .RGtkCall("R_r_gtk_data_frame_set", .local$model, theFrame, as.list(as.integer(rows.changed - 
     1)), F)
  }
  .local$theFrame <- theFrame    
  to.external <- MakeExternalDataFrame(.local$theFrame, .local)
 	#assign(.local$dataset.name, to.external, envir=.GlobalEnv)   
#3-13-10
# Change this to work with embedded lists - different choices for data names  
  # if it's "iris", save it as iris
  # if it's "xx$iris" and "xx" exists and is a list, save it as xx$iris
  # if it's "abc$iris" and "abc" isn't a list, save it as "abc$iris"
    #3-13-10
  nam <- .local$dataset.name
  # clean up spaces
  my.assign <- function(nam)
     eval(parse(text=paste(paste(".GlobalEnv", nam, sep="$"), "<- to.external")))
	tryCatch({
    my.assign(nam)
	}, error = function(e) {  # User called it something strange
    my.assign(deparse(nam))
  })

}

ModelChangeColumnName <- function(.local, idx, new.name)
  .local$allColumns[[idx-1]]$eventbox$getChildren()[[1]]$getChildren()[[1]]$setText(new.name)

 	  # function that takes a NewColumn returned object and adds 
    # eventboxes, buttons, etc
    # MUST BE DONE AFTER VIEW IS MAPPED
InitColumn <- function(.local, col.obj, nam){
  col.obj <- MakeButtonAndEventBox(col.obj, label.str=nam, handler=ColumnHeaderButtonPress, .local=.local)
  gSignalConnect(col.obj$renderer, "editing-started", function(renderer, editable, path, data=list(.local=.local)){
    #.local <- data$.local
    FALSE
  })
  if(0) gSignalConnect(col.obj$renderer, "editing-started", function(renderer, entry, path, data) {
#  print("Here")
#       gSignalConnect(entry, "map-event", function(entry, event, data=.local){ 
#print("A")
#Sys.sleep(1)  
#print("B")
#    insertion_point <- 0
#    pixel_size <- entry$getLayout()$getPixelSize()
#    click_info <- .local$last_click_info
#    col_width <- click_info$column$getWidth()
#    text_width <- pixel_size$width
#    text_pos <- (click_info$cell.x - col_width + text_width)/(text_width)
#    if(length(text_pos) != 1 || text_pos < 0 || text_pos > 1) text_pos <- 0
#    insertion_point <- round(text_pos*nchar(entry$getText()))   
#    gtkEditableSelectRegion(entry, insertion_point, insertion_point)
#    return(TRUE)  
# })      
  }, data=list(col.idx=col.obj$col.idx, .local=.local))
  gSignalConnect(col.obj$renderer, "editing-started", after=T, RendererEditingStarted, data=list(col.idx=col.obj$col.idx, .local=.local))
  col.obj$state.set <- NULL    
  return(col.obj)
}  

InitAllColumns <- function(.local, model, allColumns){
  ds.colnames <- colnames(model)
  for(j in 1:length(allColumns)){
    allColumns[[j]] <- InitColumn(.local, allColumns[[j]], ds.colnames[j+1])
    allColumns[[j]]$col.idx <- j+COLUMN_OFFSET
  }
  # The last column is not editable
  allColumns[[j]]$renderer['editable'] <- FALSE 
  allColumns[[j]]$column$setResizable(FALSE)
  #gtkWidgetModifyBg(allColumns[[j]]$eventbox, as.integer(1), bgColor)    		  
  return(allColumns)
}


MakeAVerticalScrollbar <- function(.local, gtktab, va){
  vbar <- gtkVScrollbarNew(va)
  gtktab$attach(vbar, 1, 2, 0, 1, 0, 5)
    # scroll on this doesn't repaint, kill it
  vbar$setEvents(GdkEventMask["button-motion-mask"])	  
  gSignalConnect(vbar, "scroll-event", function(...) {
    return(TRUE)
  })
  gSignalConnect(vbar, "button-press-event", function(...) {
     if(!gtkWidgetIsFocus(.local$view) && !gtkWidgetIsFocus(.local$view.rn)) .local$view$grabFocus()
     return(FALSE)
  })
  gSignalConnect(vbar, "button-release-event", function(...) {
    .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
    return(FALSE)
  })
  return(vbar)
}

	
ViewExpose <- function(widget, event=NULL, data=NULL){
  .local <- data
  if(.local$do.paint){  
  #cat("*")
    sw.va <- .local$sw.view.va
    currentVadj <- gtkAdjustmentGetValue(sw.va) # this gets called *before* value-changed
    for(r in .local$rectangles){
      r$y <- r$y - currentVadj
      r1 <- r
      if(r1$height > 0 && r1$width > 0){
        if(DO_CAIRO){
          selectedColorRgb <- c(49, 106, 197)
          cr <- gdkCairoCreate(.local$viewGetBinWindow)
          cairoSetSourceRgba(cr, selectedColorRgb[1]/256, 
            selectedColorRgb[2]/256, selectedColorRgb[3]/256, 0.2)      
          cr$rectangle(r1$x, r1$y, r1$width, r1$height)
          cr$clip()
          cr$paint()
        }
        gdkDrawRectangle(.local$viewGetBinWindow, .local$gc.invert, filled = F, r1$x, r1$y, r1$width, r1$height)          
        #rwid = 2
        #gdkDrawRectangle(.local$viewGetBinWindow, .local$gc.invert, filled = T, r1$x+r1$width-rwid, r1$y+r1$height-rwid, 2*rwid, 2*rwid)
      }
    }
  } # display mode
  return(FALSE)
}


DoScrollUpdate <- function(obj, evt, data){
  .local <- data
#  if(!gtkWidgetIsFocus(obj)) gtkWidgetGrabFocus(obj)    
  while(gtkEventsPending())
    gtkMainIteration()
  dir <- evt[["direction"]]
  va <- .local$vbar$getAdjustment()
  if (dir == GdkScrollDirection["down"]) {
    new.val <- min(va$getValue() + va[["step_increment"]], va[["upper"]] - va[["page_size"]])    
    va$setValue(new.val)
  } else if (dir == GdkScrollDirection["up"]) {
    va$setValue(va$getValue() - va[["step_increment"]])
  }
  FALSE
}  

MakeDFEditWindow <- function(.local, theFrame, size.request=c(500, 300), col.width=64){  

  .local$allColumns	<-	NULL
  .local$allow.key.flag	<-	TRUE
  .local$col.width	<-	col.width
  .local$disconnected.scrollID	<-	TRUE
  .local$disconnected.scrollID	<-	TRUE
  .local$do.paint	<- TRUE
  .local$do.rendererEditingStarted	<-	TRUE
  .local$doing.scroll	<-	FALSE
  .local$doingEntryIntoFactor	<-	FALSE
  .local$doingHScroll	<-	FALSE
  .local$FIRST_PATH	<-	gtkTreePathNewFromString(0)
  .local$flash.cursor	<-	TRUE
  .local$last.time	<-	0
  .local$LAST_PATH	<-	NULL
  .local$model	<-	NULL
  .local$SCROLL_ROW_TIMEOUT	<-	150
  .local$scrollID	<-	0

  .local$theFrame <- theFrame
  
  sw.view <- gtkScrolledWindowNew()
  .local$sw.view <- sw.view
  sw.view$setPolicy(GtkPolicyType['never'], GtkPolicyType['never'])
  sw.view$setSizeRequest(size.request[1], size.request[2])
  
  .local$LAST_PATH <- MakeLastPath(theFrame)  
  dataset.name <- .local$dataset.name
  .local$model <- rGtkDataFrame()
  view <- gtkTreeViewNewWithModel(.local$model)  
  
  .local$model$setFrame(.local$theFrame)  # This is causing a memory leak...
        
  gtktab <- gtkTableNew(2,2, FALSE)   
  .local$mapped <- FALSE
  .local$view <- view  
  gtkTreeViewSetFixedHeightMode(view, TRUE)
  allColumns <- vector("list", (dim(theFrame)[2]-1))                    

  view.rn <- gtkTreeViewNewWithModel(.local$model)
  .local$view.rn <- view.rn  
  view.rn$SetFixedHeightMode(TRUE)
  rowname.column.obj <- NewColumn(.local$model, 1, width=10, is.row=T, resizable=F)
  .local$rowname.column.obj <- rowname.column.obj
  gtkTreeViewAppendColumn(view.rn, rowname.column.obj$column)
                      
  for(j in 2:(dim(.local$model)[2])){
    tmp <- NewColumn(.local$model, j, pretty_print=.local$pretty_print, width=col.width, SPRINTF_FORMAT = .local$SPRINTF_FORMAT)
    gtkTreeViewAppendColumn(view, tmp$column)
    allColumns[[j-1]] <- tmp
  }                                              
                                   
  ss <- view$getSelection()
  .local$ss <- ss  
  ss$setMode(as.integer(3)) # multiple
  view$setRubberBanding(TRUE)

  sw.view$add(view)
  sw.view.va <- sw.view$getVadjustment()
  .local$sw.view.va <- sw.view.va   
  sw.view.ha <- sw.view$getHadjustment()
  .local$sw.ha <- sw.view.ha    
  .local$va <- sw.view.va
  
  ss.rn <- view.rn$getSelection()
  # 09-16-10 - this is screwing up memory caching
  gSignalConnect(ss.rn, "changed",     function(treeselection, data){
      w <- TransientWindow("Updating Selection...", .local)
      on.exit(w$destroy())    
      PaintRowSelectionOnTimeout(.local)
    }
  )             

  .local$ss.rn <- ss.rn    
  ss.rn$setMode(as.integer(3)) # multiple
  
  sw.rn <- gtkScrolledWindowNew()
  sw.rn$add(view.rn)
  view.rn$setSizeRequest(-1, 10)
  sw.rn$setPolicy(GtkPolicyType['never'], GtkPolicyType['never'])
  sw.rn.va <- sw.rn$getVadjustment()
  .local$sw.rn.va <- sw.rn.va
  
  if(1){
  gtkWidgetModifyBase(view, GtkStateType['selected'],  whiteColor)
  gtkWidgetModifyBase(view, GtkStateType['active'], whiteColor)
  gtkWidgetModifyText(view, GtkStateType['selected'], as.GdkColor("black"))
  gtkWidgetModifyText(view, GtkStateType['active'], as.GdkColor("black"))
                        
  sapply(list(view, view.rn), function(x){
    sapply(list(GtkStateType['active'],  
        GtkStateType['selected'], GtkStateType['normal']), 
      function(y) gtkWidgetModifyFg(x, y, as.GdkColor("black")))
  })

  gtkWidgetModifyBase(view.rn, GtkStateType['normal'], bgColor)
  gtkWidgetModifyBase(view.rn, GtkStateType['selected'], selectedColor)
  gtkWidgetModifyBase(view.rn, GtkStateType['active'], selectedColor)
  gtkWidgetModifyText(view.rn, GtkStateType['selected'], as.GdkColor("black"))
  gtkWidgetModifyText(view.rn, GtkStateType['active'], as.GdkColor("black"))  
  }
  view.rn$setEnableSearch(FALSE)
  view$setEnableSearch(FALSE)  
  
  #view$setGridLines(as.integer(3))  
  #view.rn$setGridLines(as.integer(3))    

  paned <- gtkHPanedNew()
  paned$add1(sw.rn)
  paned$add2(sw.view)
  paned$setPosition(100)  
  gtktab$attach(paned, 0, 1, 0, 1, 5, 5)
  hbar <- gtkHScrollbarNew(sw.view.ha)
  gtktab$attach(hbar, 0, 1, 1, 2, 5, 0)
  
  vbar <- MakeAVerticalScrollbar(.local, gtktab, sw.view.va)
  .local$vbar <- vbar  
    
  ## we're doing all this after we map
  ## Notebooks map too!

  gSignalConnect(view, "map", after=T, data=.local, function(view, data){   
    .local <- data
    if(.local$mapped) return(TRUE)
    .local$mapped <- TRUE    
    .local$do.paint <- FALSE        
    .local$rowname.column.obj <- MakeButtonAndEventBox(.local$rowname.column.obj, 
      label.str=.local$dataset.name, handler=CornerBoxButtonPress, .local=.local)      
             
    allColumns <- InitAllColumns(.local, .local$model, allColumns)
    .local$allColumns <- allColumns    
  
    gSignalConnect(view,"key-press-event", ViewKeyPress, data=.local)  
    gSignalConnect(view,"button-press-event", ViewButtonPress, data=.local)
    gSignalConnect(view,"button-release-event", after=T, ViewButtonRelease, data=.local)    
    #gSignalConnect(view,"motion-notify-event", ViewMotionNotify)  
    gSignalConnect(view.rn,"key-press-event", RowNamesKeyPress, data=.local)          
    gSignalConnect(view.rn,"button-press-event", RowNamesButtonPress, data=.local)
    
      # tree_view != NULL failed
  	gSignalConnect(view.rn, "focus-in-event", function(...) {gtkTreeSelectionUnselectAll(.local$ss) })
  	gSignalConnect(view, "focus-in-event", function(...) {gtkTreeSelectionUnselectAll(.local$ss.rn) })  	
            
    rows.renderer <- view.rn$getColumns()[[1]]$getCellRenderers()[[1]] 
    .local$rows.renderer <- rows.renderer      
    gSignalConnect(rows.renderer,"edited", function(renderer, path, new.text){ 
      idx <- as.integer(path)+1
      if(new.text != row.names(.local$theFrame)[idx]){      
        task <- list(list(func="ChangeRowNames", 
          arg = list(theNames = new.text, row.idx=idx)))
        DoTaskWrapper(.local, task)
      }
      return(FALSE)
    })

    gSignalConnect(view, "expose-event", after=T, ViewExpose, data=.local)    
    gSignalConnect(view, "scroll-event", after=T, DoScrollUpdate, data=.local)
    gSignalConnect(view.rn, "scroll-event", after=T, DoScrollUpdate, data=.local)    
    
    gSignalConnect(view, "leave-notify-event", AddHScrollTimeout, data=.local)
    lapply(c("enter-notify-event", "button-release-event", "focus-out-event"), 
      function(evt) gSignalConnect(view, evt, RemoveHScrollTimeout, data=.local)) 
      
    .local$viewGetBinWindow <- view$getBinWindow()		      
    .local$gc.invert <- gdkGCNew(.local$viewGetBinWindow) 


    toplevel <- .local$group.main$getToplevel()
    if (toplevel$flags() & GtkWidgetFlags["toplevel"]){
      .local$toplevel <- toplevel
    }  

      # Kill drag and drop, it causes a crash    
      # http://www.mail-archive.com/gtk-app-devel-list@gnome.org/msg11623.html      
    screen <- .local$group.main$getScreen()
    settings <- gtkSettingsGetForScreen(screen)
    settings[["gtk-dnd-drag-threshold"]] <- 10000
    checkPtrType(view, "GtkTreeView")              
    .local$scrollRowNames <- TRUE
    gSignalConnect(sw.view.va, "value-changed",  function(obj, data){
      sw2 <- data$sw2
      .local <- data$.local        
      if(identical(.local$scrollRowNames, TRUE)){
          # Take over the event loop
          # See http://wiki.laptop.org/go/PyGTK/Smooth_Animation_with_PyGTK
        while(gtkEventsPending())
          gtkMainIteration()
        gtkAdjustmentSetValue(sw2, gtkAdjustmentGetValue(obj))
        .local$allow.key.flag <- TRUE  # For paging, however, this has a problem...
      }
    }, data=list(sw2=sw.rn.va, .local=.local))
    
    # for some reason the last column doesn't get alignment set properly
  	alignment <- gtkWidgetGetParent(allColumns[[length(allColumns)]]$eventbox)
  	gtkAlignmentSet(alignment, 0, 1, 1, 1)    
     	
    return(TRUE)  
  }) # end of map event callback
       
  return(gtktab)
} # end MakeDFEditWindow

ConvertToDataObj <- function(items){
  .e <- new.env()
  .e$items <- items
  .e$dataset.attributes <- attributes(items)  

  if(is.ts(items)) {
    items <- data.frame(x=as.numeric(items), check.names=FALSE)
  } else {
    tryCatch(.e$items <- data.frame(.e$items, check.names=FALSE, stringsAsFactors=F), error = function(e)
      tryCatch(.e$items <- as.matrix(.e$items), error = function(e)
         stop(paste("Passed a", paste(class(.e$items), collapse=", "), "but expected a valid data object"))))
  }

  rv <- list(items=.e$items, dataset.attributes = .e$dataset.attributes)
  .e <- NULL
  return(rv)
}

#' Data frame editor for RGtk2
#'
#' @param items A data frame (?) to display graphically
#' @param dataset.name Name for data set
#' @param container An RGtk2 container object to place widget within. (Can this be missing?)
#' @return An object of class GtkDfEdit for which a few RGtk2-style methods are defined
#' @export
gtkDfEdit <- function(items, dataset.name = deparse(substitute(items)), 
  size.request=c(500, 300), col.width = 64, pretty_print=TRUE, sprintf_format = "%.6G",
  dataset.class="data.frame", envir=.GlobalEnv){
   # our local environment
  #print(dim(.local$items))
  .local <- new.env()
  .local$items <- items
  .local$dataset.name <- dataset.name
  .local$dataset.class <- class(items)
  .local$pretty_print <- pretty_print     

  tryCatch(sprintf(sprintf_format, pi), error = function(e) stop(e$message))
  .local$SPRINTF_FORMAT <- sprintf_format
    
  .local$dataset.attributes <- attributes(items)
    
    #3-13-10
      # have we been passed a string referring to an object?
      # Might be "a", or "a$b$c"      
  if(!any(class(items)%in%DATA_OBJECTS)){
    if (is.character(items) && nchar(items)){
			.local$dataset.name <- items
      tryCatch({ # try eval(), then get() just in case it's a name using "$"
				.local$items <- safe.eval(items, envir=envir)
				.local$dataset.class <- class(.local$items)
				.local$dataset.attributes <- attributes(.local$items)
        }, 
        error = function(e) tryCatch({
          .local$items <- safe.eval(items, envir=envir)
  				.local$dataset.class <- class(.local$items)            
  				.local$dataset.attributes <- attributes(.local$items)  				
          }, error = function(e)
            stop("Passed a name that isn't a valid object") ) )
       if(!any(class(.local$items)%in%DATA_OBJECTS)) {
         .local$rv <- ConvertToDataObj(.local$items)
         .local$items <- .local$rv$items; .local$dataset.attributes <- .local$rv$dataset.attributes
       }   
    } else {
      .local$rv <- ConvertToDataObj(items)
      .local$items <- .local$rv$items; .local$dataset.attributes <- .local$rv$dataset.attributes
    } 
   }       
    
  .local$theFrame <- MakeInternalDataFrame(.local$items)  
  .local$items <- NULL
      
  .local$theStack	<-	list()        
  .local$selectedColumns	<- integer(0)                             
            
  .local$gtktab <- MakeDFEditWindow(.local, .local$theFrame, size.request, col.width) 
  
  group.main	<-	gtkVBoxNew()
  .local$group.main <- group.main
  group.main$packStart(.local$gtktab, TRUE, TRUE, 0)
  group.main$setData(".local", .local) # use getData
  class(group.main) <- c("GtkDfEdit", class(group.main))

#  gSignalConnect(group.main, "destroy", function(obj, ...){
#    print("Destroyed")
#    #browser()
#    .local <- obj$getData(".local")
#    .local$model$setFrame()
#    rm(list=ls(env=.local), envir=.local)
#    obj$setData(".local", NULL)
##    print(gc()[2,2])
#  })
  return(group.main)
}

#' get selected row and column indices
#'
#' @param object The RGtk2DfEdit object
#' @return the 1-indexed selected rows
#' @export
gtkDfEditGetSelection <- function(object){
  .local <- object$getData(".local")
  dmm <- object$getDimension()
  columns=GetSelectedColumns(.local)
  rows <- GetSelectedRows(.local$view, .local)
  if(!length(rows) && !length(columns)){
    rows <- GetSelectedRows(.local$view.rn, .local)
    if(length(rows))
      if(dmm[2]) columns <- 1:dmm[2]
  } else if (!length(rows) && length(columns)){
    if(dmm[1]) rows <- 1:dmm[1]
  }
	return(list(rows=rows, columns=columns))
}


## JV ADD METHODS
## Should be able to call these via RGtk2 dispatch: obj$getModel() instead of gtkDfEditGetModel(obj)
#' get Model from object
#'
#' @param object The RGtk2DfEdit object
#' @return the RGtk2DataFrame that is the backend model for the widget
#' @export
gtkDfEditGetModel <- function(object) {
  object$getData(".local")$model
}

#' Return the dimensions (nrow, ncol) of the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the number of rows and columns -- not counting row names
#' @export
gtkDfEditGetDimension <- function(object) {
  dim(object$getModel()) - c(1,2)       # rownames
}

#' Return the columns of the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the column names for the current object
#' @export
gtkDfEditGetColumnNames <- function(object) {
  model <- object$getModel()
  nms <- colnames(model)[-1]
  return(nms)
}

#' Return the row names of the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the row names for the current object
#' @export
gtkDfEditGetRowNames <- function(object) {
  model <- object$getModel()
  nms <- model[,1, drop=TRUE]
 return(nms)
}

#' Return a data frame from the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the data frame with row names and column names
#' @export
gtkDfEditGetDataFrame <- function(object) {
  .local <- object$getData(".local")
  #dimnames(model) <- list(object$getRowNames(), object$getColumnNames())
  return(MakeExternalDataFrame(object$getModel(), .local))
}

#' S3 data extraction method
#'
#' Grabs data frame then passes onto [.data.frame method
#' @method [ RGtkDfEdit
#' @param x The RGtk2DfEdit object
#' @param i Row index
#' @param j Column indext
#' @param drop passed to extraction for data frame
#' @return The extracted entries
#' @export
"[.GtkDfEdit" <- function(x, i, j, drop = TRUE) {
   if(missing(drop))
     if (missing(i)) {
       drop <- TRUE
     } else {
       drop <- length(x$getDimension()[2]) == 1
     }
   df <- x$getDataFrame()
   df[i,j, drop=drop]
}

gtkDfEditSetActionHandler <- function(object, func.name, handler=NULL, data=NULL) {
  l <- object$getData(".local")
  if(!length(l$changed.handler)) l$changed.handler <- list()
  l$changed.handler[[func.name]]$func <- handler
  l$changed.handler[[func.name]]$data <- data
  object$setData(".local", l)
  invisible()
}

# needs missing i, j methods
"[<-.GtkDfEdit" <- function(x, i, j, value) {   
   l <- x$getData(".local")
   task <- list(
     list(func="ChangeCells", 
      arg = list(nf=value, row.idx=i, col.idx=ToInternalColIdx(j), do.coercion=T)))
   DoTaskWrapper(l, task)
   x
}

gtkDfEditDoTask <- function(x, task){
  # Correct col.idx to internal
  if(length(task)){
		for(jj in 1:length(task)){
		  stopifnot(all(c("func", "arg")%in%names(task[[jj]])))
		  if("col.idx"%in%names(task[[jj]]$arg)) 
		    task[[jj]]$arg$col.idx <- ToInternalColIdx(task[[jj]]$arg$col.idx)
      if(task[[jj]]$func=="InsertRows") 
        task[[jj]]$arg$nf <- MakeInternalDataFrame(task[[jj]]$arg$nf, add.rows=F)
      else if(task[[jj]]$func=="InsertColumns") 
        task[[jj]]$arg$nf <- MakeInternalDataFrame(task[[jj]]$arg$nf, add.columns=F)
		}
		DoTaskWrapper(x$getData(".local"), task)
  }
}

gtkDfEditUndo <- function(x){
  DoUndo(x$getData(".local"))
}    


gtkDfEditSort <- function(x){
  l <- x$getData(".local")
  dd <- dim(l$theFrame)
  DoSortDialog(l$theFrame[-dd[1], -dd[2],drop=F], SortHandler, l)
}

gtkDfEditEditFactors <- function(x){
  .local <- x$getData(".local")
  DoFactorEditor(.local$theFrame, .local$toplevel, integer(0), 
  FactorEditorHandler, data=.local)
}

gtkDfEditCommandData <- function(x){
  .local <- x$getData(".local")
  sc <- GetSelectedColumns(.local)
  if(length(sr <- GetSelectedRows(.local$view, .local))==0) 
    sr <- 1:(dim(.local$theFrame)[1]-1)
  CommandData(.local, sr, sc)
}

gtkDfEditGetDatasetName <- function(x){
  x$getData(".local")$dataset.name
}

gtkDfEditSetDatasetName <- function(x, new.name){
  ModelChangeDatasetName(x$getData(".local"), new.name)
}
