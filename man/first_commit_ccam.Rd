% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first_commit_ccam.R
\name{first_commit_ccam}
\alias{first_commit_ccam}
\title{First Commit to CCAM Database}
\usage{
first_commit_ccam(
  ccam_file,
  ccam_label_file,
  censustract_path,
  cd116_path,
  stateleglower_path,
  statelegupper_path,
  place_path
)
}
\arguments{
\item{ccam_file}{Path to the .rda file of the exisitng CCAM data}

\item{ccam_label_file}{Path to the .csv file containing the labels for the CCAM variables}
}
\value{
None
}
\description{
Use only once to add exisitng CCAM data at the creation of the database.
}
