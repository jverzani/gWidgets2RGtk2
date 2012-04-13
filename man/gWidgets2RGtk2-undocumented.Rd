\name{getWidget.RGtkObject}
\alias{.addStockIcons.guiWidgetsToolkitRGtk2}
\alias{.gaction.guiWidgetsToolkitRGtk2}
\alias{.galert.guiWidgetsToolkitRGtk2}
\alias{.gbasicdialog.guiWidgetsToolkitRGtk2}
\alias{.gbutton.guiWidgetsToolkitRGtk2}
\alias{.gcalendar.guiWidgetsToolkitRGtk2}
\alias{.gcheckbox.guiWidgetsToolkitRGtk2}
\alias{.gcheckboxgroup.guiWidgetsToolkitRGtk2}
\alias{.gcombobox.guiWidgetsToolkitRGtk2}
\alias{.gconfirm.guiWidgetsToolkitRGtk2}
\alias{.gdf.guiWidgetsToolkitRGtk2}
\alias{.gdfnotebook.guiWidgetsToolkitRGtk2}
\alias{.gedit.guiWidgetsToolkitRGtk2}
\alias{.getStockIconByName.guiWidgetsToolkitRGtk2}
\alias{.getStockIcons.guiWidgetsToolkitRGtk2}
\alias{.gexpandgroup.guiWidgetsToolkitRGtk2}
\alias{.gfile.guiWidgetsToolkitRGtk2}
\alias{.gfilebrowse.guiWidgetsToolkitRGtk2}
\alias{.gformlayout.guiWidgetsToolkitRGtk2}
\alias{.gframe.guiWidgetsToolkitRGtk2}
\alias{.ggraphics.guiWidgetsToolkitRGtk2}
\alias{.ggraphicsnotebook.guiWidgetsToolkitRGtk2}
\alias{.ggroup.guiWidgetsToolkitRGtk2}
\alias{.gimage.guiWidgetsToolkitRGtk2}
\alias{.ginput.guiWidgetsToolkitRGtk2}
\alias{.glabel.guiWidgetsToolkitRGtk2}
\alias{.glayout.guiWidgetsToolkitRGtk2}
\alias{.gmenu.guiWidgetsToolkitRGtk2}
\alias{.gmessage.guiWidgetsToolkitRGtk2}
\alias{.gnotebook.guiWidgetsToolkitRGtk2}
\alias{.gpanedgroup.guiWidgetsToolkitRGtk2}
\alias{.gradio.guiWidgetsToolkitRGtk2}
\alias{.gseparator.guiWidgetsToolkitRGtk2}
\alias{.gslider.guiWidgetsToolkitRGtk2}
\alias{.gspinbutton.guiWidgetsToolkitRGtk2}
\alias{.gstackwidget.guiWidgetsToolkitRGtk2}
\alias{.gstatusbar.guiWidgetsToolkitRGtk2}
\alias{.gtable.guiWidgetsToolkitRGtk2}
\alias{.gtext.guiWidgetsToolkitRGtk2}
\alias{.gtimer.guiWidgetsToolkitRGtk2}
\alias{.gtoolbar.guiWidgetsToolkitRGtk2}
\alias{.gtree.guiWidgetsToolkitRGtk2}
\alias{.gvarbrowser.guiWidgetsToolkitRGtk2}
\alias{.gwindow.guiWidgetsToolkitRGtk2}
\alias{.stockIconFromObject.guiWidgetsToolkitRGtk2}
\alias{addToGtkStockIcons}
\alias{getBlock.RGtkObject}
\alias{getWidget.RGtkObject}
\title{method for stopping getWidget}
\usage{
  \method{getWidget}{RGtkObject} (obj)

  \method{getBlock}{RGtkObject} (obj)

  \method{.addStockIcons}{guiWidgetsToolkitRGtk2} (toolkit,
    iconNames, iconFiles, ...)

  \method{.getStockIcons}{guiWidgetsToolkitRGtk2} (toolkit,
    ...)

  \method{.getStockIconByName}{guiWidgetsToolkitRGtk2}
    (toolkit, name, ...)

  addToGtkStockIcons(iconNames, iconFiles)

  \method{.stockIconFromObject}{guiWidgetsToolkitRGtk2}
    (toolkit, obj, ...)

  \method{.gbutton}{guiWidgetsToolkitRGtk2} (toolkit, text,
    handler, action, container, ...)

  \method{.ggroup}{guiWidgetsToolkitRGtk2} (toolkit,
    horizontal = TRUE, spacing = 5,
    use.scrollwindow = FALSE, container = NULL, ...)

  \method{.gframe}{guiWidgetsToolkitRGtk2} (toolkit, text,
    markup, pos, horizontal = TRUE, spacing = 5,
    container = NULL, ...)

  \method{.gexpandgroup}{guiWidgetsToolkitRGtk2} (toolkit,
    text, markup, horizontal = TRUE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gcheckbox}{guiWidgetsToolkitRGtk2} (toolkit,
    text, checked = FALSE, use.togglebutton = FALSE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gcheckboxgroup}{guiWidgetsToolkitRGtk2}
    (toolkit = NULL, items, checked = FALSE,
    horizontal = FALSE, use.table = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gcombobox}{guiWidgetsToolkitRGtk2} (toolkit,
    items, selected = 1, editable = FALSE,
    coerce.with = NULL, handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.gnotebook}{guiWidgetsToolkitRGtk2} (toolkit,
    tab.pos = 3, container = NULL, ...)

  \method{.gpanedgroup}{guiWidgetsToolkitRGtk2} (toolkit,
    horizontal = TRUE, container = NULL, ...)

  \method{.gseparator}{guiWidgetsToolkitRGtk2} (toolkit,
    horizontal = TRUE, container = NULL, ...)

  \method{.gstackwidget}{guiWidgetsToolkitRGtk2} (toolkit,
    container = NULL, ...)

  \method{.gwindow}{guiWidgetsToolkitRGtk2} (toolkit,
    title, visible = visible, name, width, height, parent,
    handler, action, ...)

  \method{.gmenu}{guiWidgetsToolkitRGtk2} (toolkit,
    menu.list = list(), popup = FALSE, container = NULL,
    ...)

  \method{.gstatusbar}{guiWidgetsToolkitRGtk2} (toolkit,
    text = "", container = NULL, ...)

  \method{.gtoolbar}{guiWidgetsToolkitRGtk2} (toolkit,
    toolbar.list = list(),
    style = c("both", "icons", "text", "both-horiz"),
    container = NULL, ...)

  \method{.gslider}{guiWidgetsToolkitRGtk2} (toolkit,
    from = 0, to = 100, by = 1, value = from,
    horizontal = TRUE, handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.gspinbutton}{guiWidgetsToolkitRGtk2} (toolkit,
    from = 0, to = 10, by = 1, value = from, digits = 0,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.glayout}{guiWidgetsToolkitRGtk2} (toolkit,
    homogeneous = FALSE, spacing = 10, container = NULL,
    ...)

  \method{.gradio}{guiWidgetsToolkitRGtk2} (toolkit, items,
    selected = 1, horizontal = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gmessage}{guiWidgetsToolkitRGtk2} (toolkit, msg,
    title = "message",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.gconfirm}{guiWidgetsToolkitRGtk2} (toolkit, msg,
    title = "Confirm",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.ginput}{guiWidgetsToolkitRGtk2} (toolkit, msg,
    text = "", title = "Input",
    icon = c("info", "warning", "error", "question"),
    parent = NULL, ...)

  \method{.gbasicdialog}{guiWidgetsToolkitRGtk2} (toolkit,
    title = "Dialog", parent = NULL, do.buttons = TRUE,
    handler = NULL, action = NULL, ...)

  \method{.galert}{guiWidgetsToolkitRGtk2} (toolkit, msg,
    title = "message", delay = 3, parent = NULL, ...)

  \method{.gcalendar}{guiWidgetsToolkitRGtk2} (toolkit,
    text = "", format = "\%Y-\%m-\%d", handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gfile}{guiWidgetsToolkitRGtk2} (toolkit,
    text = "", type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(),
    multi = FALSE, ...)

  \method{.gfilebrowse}{guiWidgetsToolkitRGtk2} (toolkit,
    text = "", type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(), quote = TRUE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gaction}{guiWidgetsToolkitRGtk2} (toolkit,
    label, tooltip = NULL, icon = NULL, key.accel = NULL,
    handler = NULL, action = NULL, parent = NULL, ...)

  \method{.ggraphics}{guiWidgetsToolkitRGtk2} (toolkit,
    width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gtable}{guiWidgetsToolkitRGtk2} (toolkit, items,
    multiple = FALSE, chosen.col = 1, icon.col = NULL,
    tooltip.col = NULL, handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.gdf}{guiWidgetsToolkitRGtk2} (toolkit,
    items = NULL, handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.gdfnotebook}{guiWidgetsToolkitRGtk2} (toolkit,
    items, container = NULL, ...)

  \method{.gtext}{guiWidgetsToolkitRGtk2} (toolkit,
    text = NULL, width = NULL, height = 300,
    font.attr = NULL, wrap = TRUE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gimage}{guiWidgetsToolkitRGtk2} (toolkit,
    filename = "", dirname = "", stock.id = NULL,
    size = "", handler = NULL, action = NULL,
    container = NULL, ...)

  \method{.glabel}{guiWidgetsToolkitRGtk2} (toolkit,
    text = "", markup = FALSE, editable = FALSE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{.gtimer}{guiWidgetsToolkitRGtk2} (toolkit, ms,
    FUN, data = NULL, one.shot = FALSE, start = TRUE)

  \method{.gtree}{guiWidgetsToolkitRGtk2} (toolkit,
    offspring = NULL, offspring.data = NULL,
    chosen.col = 1, offspring.col = 2, icon.col = NULL,
    tooltip.col = NULL, multiple = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.gvarbrowser}{guiWidgetsToolkitRGtk2} (toolkit,
    handler = NULL, action = "summary", container = NULL,
    ...)

  \method{.gedit}{guiWidgetsToolkitRGtk2} (toolkit,
    text = "", width = 25, coerce.with = NULL,
    initial.msg = initial.msg, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{.ggraphicsnotebook}{guiWidgetsToolkitRGtk2}
    (toolkit, width, height, dpi, container, ...)

  \method{.gformlayout}{guiWidgetsToolkitRGtk2} (toolkit,
    align = "left", spacing = 5, container = NULL, ...)
}
\arguments{
  \item{name}{name of icon}

  \item{obj}{R object to get icon from}

  \item{digits}{digits}

  \item{delay}{delay}

  \item{width}{width of device (pixels)}

  \item{height}{height of device (pixels)}

  \item{dpi}{dots per inch}

  \item{ps}{pointsize}

  \item{initial.msg}{initial.msg}

  \item{toolkit}{used to dispatch into toolkit if a
  separate implementation is made}

  \item{iconNames}{names of icons}

  \item{iconFiles}{path of icons}

  \item{...}{ignored}

  \item{text}{label text. If text matches a stock icon
  name, that is used as well}

  \item{handler}{A handler assigned to the default change
  signal. Handlers are called when some event triggers a
  widget to emit a signal. For each widget some default
  signal is assumed, and handlers may be assigned to that
  through \code{addHandlerChanged} or at construction time.
  Handlers are functions whose first argument, \code{h} in
  the documentation, is a list with atleast two components
  \code{obj}, referring to the object emitting the signal
  and \code{action}, which passes in user-specified data to
  parameterize the function call.}

  \item{action}{User supplied data passed to the handler
  when it is called}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

  \item{use.scrollwindow}{logical. If TRUE, places box
  container in scrollwindow. In this case, a size should be
  set.}

  \item{markup}{does label use markup (toolkit specific)}

  \item{pos}{position of label: 0=left, 1=right, some
  toolkit allow values in between}

  \item{checked}{is button selected}

  \item{use.togglebutton}{Use a toggle button (shows
  depressed) not a check box}

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}

  \item{items}{checkbox labels}

  \item{use.table}{logical. If supported, and \code{TRUE}
  then uses a table widget with scrollbars}

  \item{selected}{integer. Which item (by index) is
  selected. Use -1 for no selection}

  \item{editable}{logical. Is user allowed to edit value}

  \item{coerce.with}{A function of function name to be
  called before selected value is returned by
  \code{svalue}}

  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}

  \item{title}{title for window's title bar. This is the
  main property and is accessed via \code{svalue} or
  \code{svalue<-}.}

  \item{visible}{logical. If code{TRUE} window is drawn
  when constructed. Otherwise, window can be drawn later
  using \code{visible<-}. This value can default to
  \code{FALSE} by setting the option:
  \code{options("gWidgets:gwindow-default-visible-is-false"=TRUE)}.
  There are advantages: windows can draw slowly when adding
  many items. With \pkg{gWidgets2RGtk2}, the
  \code{ggraphics} widget can like to be added to an
  undrawn widget as this avoids sizing issue.}

  \item{parent}{If non-NULL, can be used to suggest default
  location of window. The argument name was changed from
  location to parent. This can be a coordinate pair (x,y)
  with (0,0) the upper left corner, or a gwindow instance.
  In the latter case the location is suggested by the
  location of the current window. This is useful for
  placing dialogs near the parent window.}

  \item{menu.list}{A list defining the menu structure.
  Named sub lists determine the submenu titles and
  structure. The list may have components of class:
  \code{GAction}, mapped to a button; \code{GSeparator},
  mapped to a horizontal separator; \code{GRadio}, mapped
  to linked buttons; or \code{GCheckbox}, mapped to a
  checkbox button.}

  \item{popup}{logical. If true, make a popup window to be
  added through a handler call}

  \item{toolbar.list}{list. A one-level list of
  \code{gaction} items, \code{gseparator} items or possibly
  other widgets. In the latter cases the \code{container}
  argument is not specified prior. (XXX Need to work this
  out with gWidgetstcltk)}

  \item{style}{style for icon or text.}

  \item{from}{If a number of length one then a starting
  point, in which case to, by are passed to \code{seq}.
  Otherwise a sequence of values for which
  sort(unique(from)) will order}

  \item{to}{ending point when from is starting point}

  \item{by}{step size if not specified by \code{from}}

  \item{homogeneous}{are cells all the same size}

  \item{msg}{Character. Message to display.}

  \item{icon}{which icon to display}

  \item{do.buttons}{FALSE to suppress buttons when no
  parent}

  \item{label}{label for action}

  \item{tooltip}{toolktip for actin}

  \item{key.accel}{keyboard accelerator. If given, parent
  must be specified.}

  \item{format}{Date format}

  \item{type}{type of browser: to open a file, to save a
  file or to select a directory}

  \item{initial.filename}{Suggested file name}

  \item{filter}{A filter specifiation. This is toolkit
  specific. Here are some examples: \itemize{ \item{RGtk2}{
  Something like \preformatted{ list("All files" =
  list(patterns = c("*")), "R files" = list(patterns =
  c("*.R", "*.Rdata")), "text files" = list(mime.types =
  c("text/plain")) ) } } \item{tcltk}{} \item{Qt}{} }}

  \item{multi}{Logical. Allow multiple files to be
  selected?}

  \item{quote}{quote output}

  \item{multiple}{logical allow multiple selectino}

  \item{chosen.col}{which value from the row is returned by
  selection}

  \item{icon.col}{NULL or integer. If latter, specifies
  column containing stock icon}

  \item{tooltip.col}{NULL or integer. If latter, specifies
  column containing tooltip}

  \item{font.attr}{font attributes for text buffer. One can
  also specify font attributes for insertion. The font
  attributes are specified with a named vector or list,
  with names and values coming from: \describe{
  \item{weight}{ in c("light", "normal", "bold", "heavy")}
  \item{style}{inc("normal", "oblique", "italic")}
  \item{family}{in c("sans", "helvetica", "times",
  "monospace")} \item{size}{in c("xx-small", "x-small",
  "small", "medium", "large", "x-large", "xx-large")}
  \item{foreground}{a value in colors()}
  \item{background}{a value in colors()} }}

  \item{wrap}{logical do lines wrap}

  \item{filename}{basename of file}

  \item{dirname}{dirname of file}

  \item{stock.id}{stock id of icon (if non NULL)}

  \item{size}{size of icon when a stock id (toolkit
  dependent)}

  \item{ms}{interval in milliseconds}

  \item{FUN}{FUnction to call. Has one argument, data
  passed in}

  \item{data}{passed to function}

  \item{one.shot}{logical. If TRUE, called just once, else
  repeats}

  \item{start}{logical. If FALSE, started by
  \code{start_timer} OO method. (Call
  \code{obj$start_time()}).}

  \item{offspring}{function. A function passed values
  \code{path} and \code{data}, the latter from
  \code{offspring.data}. The path is the current position
  of the parent item using the named keys from the chosen
  column.}

  \item{offspring.data}{Passed to second argument of
  \code{offspring} function. Used to parameterize a
  function call.}

  \item{offspring.col}{integer or column name. Points to
  column containing logical values indicating if a row has
  offspring.}

  \item{align}{alignment of label. Left justify or center
  balance. Leave as "default" for underlying toolkit
  default.}
}
\value{
  NULL
}
\description{
  method for stopping getWidget

  S3 method for stopping getBlock

  add stock icons

  Returns list of stock ids

  return stock id

  helper function

  return stock id from object

  Toolkit constructor

  toolkit constructor for ggroup

  gframe constructor

  toolkit constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  toolkit constructor for gwindow

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  toolkit implementation for gmessage

  toolkit implementation for gconfirm

  toolkit implmentation of ginput

  toolkit implementation

  toolkit implementation of galert

  Toolkit constructor

  Toolkit implementation

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  toolkit implementation

  Toolkit constructor

  Toolkit label constructor

  S3 method for gtimer

  Toolkit constructor

  Toolkit constructor

  Toolkit gedit constructor

  toolkit implementation

  Toolkit constructor
}
\seealso{
  The documentation for this is found at
  \code{\link{gbutton}}.
}

