## miscellaneous functions

##' toolkit class for RGtk2
##'
##' @importClassesFrom gWidgets2 guiWidgetsToolkit
##' @export
setClass("guiWidgetsToolkitRGtk2",
         contains="guiWidgetsToolkit")

## some special class unions so we can have easier to deal with default
## setClassUnion("IntegerOrNULL", c("integer", "NULL"))
## setClassUnion("CharacterOrNULL", c("character", "NULL"))
## setClassUnion("LogicalOrNULL", c("logical", "NULL"))
## setClassUnion("LogicalCharacterOrNULL", c("logical", "character", "NULL"))


## Pango Scale for converting between name and numeric value
PangoScale <- c(
               "xx-large"= PANGO_SCALE_XX_LARGE,
               "x-large" = PANGO_SCALE_X_LARGE,
               "large"   = PANGO_SCALE_LARGE,
               "medium"  = PANGO_SCALE_MEDIUM,
               "small"   = PANGO_SCALE_SMALL,
               "x-small" = PANGO_SCALE_X_SMALL,
               "xx-small" = PANGO_SCALE_XX_SMALL
               )


##' get index of element of list
##'
##' Like match, but works with list
##' @param lst a list to search through
##' @param ele element of list
##' @return returns index of element or integer(0)
get_index <- function(lst, ele) {
  n <- seq_along(lst)
  n[sapply(lst, function(i) identical(i, ele))]
}
