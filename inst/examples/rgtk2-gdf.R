library(gWidgets2)
options(guiToolkit="RGtk2")

## Some modifications to gdf available through RGtk2


## like gtable, only used to select values with a checkbox


DF <- cbind(mtcars[1:10, 1:4], Selected=rep(FALSE, 10))
DF <- DF[, c(ncol(DF), 1:(ncol(DF)-1))]

w <- gwindow("Select value by checkbox", visible=FALSE)
tbl <- gdf(DF, cont=w)

## modify
sapply(2:ncol(DF), function(j) editable(tbl, j=j) <- FALSE)
tbl$hide_row_names(TRUE)
tbl$remove_popup_menu()

visible(w) <- TRUE
## select some values
tbl[c(1,3),1] <- TRUE


## get values as logical
tbl[,1]

## get values:
tbl[tbl[,1], -1]                        # drop first column
