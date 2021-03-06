% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/More.R
\name{More}
\alias{More}
\title{More}
\usage{
More(query_obj)
}
\arguments{
\item{query_obj}{QueryClass instance-ID}
}
\value{
Boolean
}
\description{
Indicates if there are any other results in the query-result.
}
\examples{
\dontrun{
query_iterate <- Query(Session, "collection('TestDB/Test.xml')")
while (More(query_iterate)) {
    iterResult <- c(iterResult, Next(query_iterate))
  }

print(query_iterate)
## Return "0d" "<Line_1 line=\"1\">Content 1</Line_1>"
          "0d" "<Line_2 line=\"2\">Content 2</Line_2>"

}

}
