#' Test data for fitting the Exemplar-based categorization model
#'
#' Data about categorization in four different conditions.
#' 
#' \itemize{
#'   \item condition. experimental condition (size, angle, criss, diag)
#'   \item angle. feature value of the angle feature
#'   \item size. feature value of the size feature
#'   \item N. How often the feature combination occured.
#'   \item obs_cat. How often a category 1 mode was observed given this feature combination.
#'   \item pobs. Probability that a category 1 mode was observed given this feature combination (obs_cat / N)
#'   \item true_cat. True category label of this feature combination (0 or 1)
#' }
#' @name nosofsky1989
#'
#' @docType data#'
#' @usage data(nosofsky1989)#'
#' @format An object of class \code{"data.frame"}.#'
#' @keywords datasets
#' @references Nosofsky, R. M. (1989). Further tests of an exemplar-similarity approach to relating identification and categorization. Perception & Psychophysics, 45, 279–290. doi:10.3758/BF03204942
#' @source From Table 3 and Figure 2 in Nosofsky, R. M. (1989). Further tests of an exemplar-similarity approach to relating identification and categorization. Perception & Psychophysics, 45, 279–290. doi:10.3758/BF03204942
#' @examples
#' data(nosofsky1989)
NULL