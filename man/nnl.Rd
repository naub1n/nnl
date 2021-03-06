% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nnl.R
\name{nnl}
\alias{nnl}
\title{Complete and unique function to extrapolate a SpatialLinesDataFrame A to a SpatialLinesDataFrame B}
\usage{
nnl(
  l_A,
  l_B,
  id_l_A,
  id_l_B,
  id_p_A = "ID_PTS_A",
  id_p_B = "ID_PTS_B",
  bfr_width = 100,
  step = 5,
  rate = 45,
  cut = F,
  p = T,
  ncores = NULL,
  verbose = F
)
}
\arguments{
\item{l_A}{a SpatialLinesDataFrame. Reference lines.}

\item{l_B}{a SpatialLinesDataFrame. Searched lines.}

\item{id_l_A}{a character string. IDs Column name of reference SpatialLinesDataFrame (Lines A)}

\item{id_l_B}{a character string. IDs Column name of searched SpatialLinesDataFrame (Lines B)}

\item{id_p_A}{a character string. Indicates the IDs column name of points created to lines A. Default : "ID_PTS_A"}

\item{id_p_B}{a character string. Indicates the IDs column name of points created to lines B. Default : "ID_PTS_B"}

\item{bfr_width}{an integer. Buffer distance.}

\item{step}{an integer. Define the distance between two points along the line in meters. Defautl = 5.}

\item{rate}{an integer. Between 0 and 100. Define lower limit to select line B. Rate between number of points B on Lines A and totalpoints on Line B.}

\item{cut}{logical. Define if function cut SpatialLinesDataFrame B to a smaller entities.}

\item{p}{logical. Define if function use multithreading. Default = TRUE.}

\item{ncores}{an integer. Number of CPU cores used. If NULL, all cores - 1 are used.}

\item{verbose}{logical. View info for each step.}
}
\value{
a SpatialLinesDataFrame.
}
\description{
Complete and unique function to extrapolate a SpatialLinesDataFrame A to a SpatialLinesDataFrame B
}
\examples{
\dontrun{
url_bresle <- paste0("https://api.sandre.eaufrance.fr/",
                     "coursdeau/v1/amont/G01-0400?pk_vid=aade75889c86a2471576396546bb9c85")
river_bresle <- rgdal::readOGR(url_bresle)

url_example <- "https://raw.githubusercontent.com/naub1n/nnl/master/example/example.geojson"
example <- rgdal::readOGR(url_example)

river_bresle <- sp::spTransform(river_bresle, sp::CRS("+init=epsg:2154"))

extrapolate_example <- nnl(l_A = example,
                           l_B = river_bresle,
                           id_l_A = "ID_EXAMPLE",
                           id_l_B = "CdEntiteHydrographique",
                           cut = T)

plot(river_bresle, col = "blue")
plot(example, col = "green", add = T)
plot(extrapolate_example, col = "red", add = T )
}
}
