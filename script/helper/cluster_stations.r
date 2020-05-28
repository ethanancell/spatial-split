#' Cluster stations that are near one another.
#'
#' Uses complete linkage clustering where each cluster has a maximum similarity
#' score of h between furthest stations in a cluster. dist_adj can be used to
#' scale distances between stations and elev_adj can be used to scale elevation
#' between stations. For example, distance can be scaled so that dist_adj = 4
#' km is a one unit similarity between stations and elevation can be scaled
#' so that elev_adj = 50 is a one unit similarity between stations. Then two
#' stations that are separated by 4 km and have a difference of 50 units of
#' elevation have a total similarity score of 2. h is used to specify a maximum
#' similarity score between stations in a cluster.
#'
#' @param lon A numeric vector of station longitudes.
#' @param lat A numeric vector of station latitudes.
#' @param elev A numeric vector of station elevations.
#' @param dist_adj Constant value in km that scales the dissimilarity score
#' for geographic distance.
#' @param elev_adj Constant value that scales elevation dissimilarity.
#' @param h Maximum similarity score for the cluster analysis.
#' @param method stats::hclust method for clustering. One of "ward.D",
#' "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#' "median" (= WPGMC) or "centroid" (= UPGMC).
#'
#' @return A numeric vector of clusters, where each unique number represents a
#' cluster.
#'
#' @examples
#' # Simple example
#' subset <- snowload2::ghcnd_stations[10500:10600,]
#' cluster_stations(subset$LONGITUDE, subset$LATITUDE, subset$ELEVATION,
#'                  dist_adj = 4, elev_adj = 50, h = 2)
#'
#' # Practical use example
#' library(dplyr)
#' cluster_test <- snowload2::ghcnd_stations %>%
#'   filter(STATE %in% c("UT", "NV")) %>%
#'   group_by(STATE) %>%
#'   mutate(CLUST = cluster_stations(LONGITUDE, LATITUDE, ELEVATION,
#'                                   dist_adj = 4,
#'                                   elev_adj = 50,
#'                                   h = 2)) %>%
#'   ungroup() %>%
#'   mutate(CLUST = sprintf(paste0(STATE, "%04d"), CLUST))
#'
#'
#' @export
cluster_stations <- function(lon, lat, elev, dist_adj, elev_adj, h,
                             method = "complete") {
  if (length(lon) != length(lat) || length(lon) != length(elev)) {
    stop("Lengths of lon, lat, and elev must be the same.")
  }
  if (!is.numeric(lon) || !is.numeric(lat) || !is.numeric(elev)) {
    stop("Vectors lon, lat, and elev must be numeric.")
  }
  if (any(is.na(elev)) || any(is.na(lon)) || any(is.na(lat))) {
    stop("All elev, lon, and lat not have NA values.")
  }

  # Find similarity matrix
  #=============================================================================
  # find distances between all locations in km
  dist <- fields::rdist.earth(matrix(c(lon, lat), ncol = 2), miles = FALSE)

  # find difference in elevation between all locations
  delev <- abs(outer(elev, elev, "-"))

  # Produce "similarity matrix":
  # - distance of dist_adj km produces a score of 1
  # - elevation difference of elev_adj produces a score of 1
  similarity <- stats::as.dist((dist/dist_adj) + (delev/elev_adj))

  # Cluster and return cluster vector
  #=============================================================================
  # Cluster all stations with the farthest neighbors in a cluster
  if (length(lon) > 1) {
    clust <- stats::hclust(similarity, method)
  } else if (length(lon) == 1) {
    return(1)
  } else return(NULL)

  # Cut heirarchical clustering tree at h and return cluster id's
  stats::cutree(clust, h = h)
}
