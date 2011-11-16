\name{add_editable_cell_renderer}
\alias{add_editable_cell_renderer}
\title{make a view column for the given type of variable}
\usage{
  add_editable_cell_renderer(x, self, model_idx, view_col)
}
\arguments{
  \item{x}{variable}

  \item{self}{reference to ModelView object}

  \item{model_idx}{1-based index in model of data to
  represent}

  \item{view_col}{optional view column to reuse}
}
\value{
  a gtkTreeViewColumn with data "n", and "id" to block
  editable id
}
\description{
  make a view column for the given type of variable
}

