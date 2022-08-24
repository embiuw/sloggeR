#' \code{estDistance} Estimate distance to sighting
#'
#' @param brH Height of observation platform (in meters)
#' @param obsH Height of observer (in meters)
#' @param angle Angle of depression (pitch) recorded by e.g. SensorLogger
#' @return Estimated distance to sighting (in meters)
#' @details To be added
#' @family Sightings survey data processing
#' @seealso \code{\link{readSL}} to read single observation data in a subdirectory,
#'   \code{\link{getSightings}} to read all observations in a main directory,
#'   \code{\link{annotateSightings}} to manually extract information from voice recordings,
#' @author Martin Biuw
#' @example
#' To be added
#' @export
#'
estDistance <- function(brH=19.2, obsH=1.73, angle=-0.4055861) {
  daHoriz <- function(br=brH, h=obsH, r=6378137) {
    ro <- r+br+h
    d <- sqrt(ro^2-r^2)
    a <- circular::deg(atan2(d, r))
    c(dh=d, ah=a, arev=90-a)
  }

  dH <- daHoriz()

  findDist <- function(br=brH, h=obsH, r=6378137, Arev=90-angle, aHrev=dH[3], aD=dH[1]) {
    totH <- br+h+r
    stepSize <- round(diff(seq(1, totH, length=1000))[1])
    rangeH <- c(1, totH)
    oppH <- aD*sin(circular::rad(aHrev))
    maxAdj <- sqrt(aD^2-oppH^2)
    minAdj <- br+h


    findHyp <- function(x=seq(minAdj, maxAdj, by=0.01)) {
      opp <- x*tan(circular::rad(Arev))
      xx <- totH-x
      sqrt(opp^2+xx^2)
    }

    optAdj <- which.min(abs(r-findHyp()))
    optAdj <- seq(minAdj, maxAdj, by=0.01)[optAdj]
    optOpp <- optAdj*tan(circular::rad(Arev))
    sqrt(optAdj^2+optOpp^2)
  }
  findDist()
}
