\name{getWidget.RGtkObject}
\alias{.galert.guiWidgetsToolkitRGtk2}
\alias{.gbasicdialog.guiWidgetsToolkitRGtk2}
\alias{.gconfirm.guiWidgetsToolkitRGtk2}
\alias{.ggroup.guiWidgetsToolkitRGtk2}
\alias{.ginput.guiWidgetsToolkitRGtk2}
\alias{.gmessage.guiWidgetsToolkitRGtk2}
\alias{.gwindow.guiWidgetsToolkitRGtk2}
\alias{getBlock.RGtkObject}
\alias{getWidget.RGtkObject}
\title{method for stopping getWidget}
\usage{
  getWidget.RGtkObject(obj)

  getBlock.RGtkObject(obj)

  .gwindow.guiWidgetsToolkitRGtk2(toolkit, title, visible =
  visible, name, width, height, parent, handler, action,
  ...)

  .ggroup.guiWidgetsToolkitRGtk2(toolkit, horizontal =
  TRUE, spacing = 5, use.scrollwindow = FALSE, container =
  NULL, ...)

  .gmessage.guiWidgetsToolkitRGtk2(toolkit, msg, title =
  "message", icon = c("info", "warning", "error",
  "question"), parent = NULL, ...)

  .gconfirm.guiWidgetsToolkitRGtk2(toolkit, msg, title =
  "Confirm", icon = c("info", "warning", "error",
  "question"), parent = NULL, ...)

  .ginput.guiWidgetsToolkitRGtk2(toolkit, msg, text = "",
  title = "Input", icon = c("info", "warning", "error",
  "question"), parent = NULL, ...)

  .gbasicdialog.guiWidgetsToolkitRGtk2(toolkit, title =
  "Dialog", parent = NULL, do.buttons = TRUE, handler =
  NULL, action = NULL, ...)

  .galert.guiWidgetsToolkitRGtk2(toolkit, msg, title =
  "message", delay = 3, parent = NULL, ...)
}
\arguments{
  \item{toolkit}{}

  \item{msg}{}

  \item{title}{}

  \item{icon}{}

  \item{parent}{}

  \item{...}{}

  \item{toolkit}{}

  \item{msg}{}

  \item{title}{}

  \item{icon}{}

  \item{parent}{}

  \item{...}{}

  \item{toolkit}{}

  \item{msg}{}

  \item{text}{}

  \item{title}{}

  \item{icon}{}

  \item{parent}{}

  \item{...}{}

  \item{toolkit}{}

  \item{title}{}

  \item{parent}{}

  \item{do.buttons}{}

  \item{handler}{}

  \item{action}{}

  \item{...}{}

  \item{toolkit}{}

  \item{msg}{}

  \item{title}{}

  \item{delay}{}

  \item{parent}{}

  \item{...}{}
}
\value{
  NULL

  logical

  character

  logical

  NULL
}
\description{
  method for stopping getWidget

  S3 method for stopping getBlock

  toolkit constructor for gwindow

  toolkit constructor for ggroup

  toolkit implementation for gmessage

  toolkit implementation for gconfirm

  toolkit implmentation of ginput

  toolkit implementation

  toolkit implementation of galert
}

