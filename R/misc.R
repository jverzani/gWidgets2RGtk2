## miscellaneous functions
##' @import gWidgets2
##' @include gWidgets2RGtk2-package.R
NULL

##' toolkit class for RGtk2
##'
##' @name guiWidgetsToolkitRGtk2-class
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

