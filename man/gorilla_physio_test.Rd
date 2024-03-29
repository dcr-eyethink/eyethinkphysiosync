% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gorilla_physio_test.R
\name{gorilla_physio_test}
\alias{gorilla_physio_test}
\title{Process gorilla data from test phase of physiological sync experiment}
\usage{
gorilla_physio_test(
  data,
  test_task = "post_memory",
  post_task_link = c("item"),
  memory_id = c("item"),
  image_id = "ad_image",
  frag_id = "ad_fragment",
  set_id = "ad_set"
)
}
\arguments{
\item{data}{either task data that has already been filtered to the post task, or an imported data list, that will be filtered with test_task}

\item{test_task}{names of post tasks to analyse here}
}
\value{
td (trial data) and full post test data, with one line per trial
}
\description{
Process gorilla data from test phase of physiological sync experiment
}
