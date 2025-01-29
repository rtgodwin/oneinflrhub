#' Predicted Expected Response for One-Inflated or Truncated Models
#'
#' Calculates the predicted expected response for a model fitted using 
#' \code{\link{oneinfl}} or \code{\link{truncreg}}.
#'
#' @param object An object of class `truncmodel`
#' @param ... Additional arguments, such as `df` and `type`. \code{type} is a character string specifying the type of prediction. Currently, only 
#'   \code{"response"} is supported, which calculates the expected value of the response variable.
#'
#' @return
#' A numeric vector of predicted expected responses for the observations in \code{df}.
#'
#' @details
#' This function computes the expected response based on the fitted model. The computation 
#' differs depending on the distribution. For \code{Poisson (PP)}, predicted values are computed
#' using \code{\link{E_pois_noinfl}}. For \code{Negative Binomial (ZTNB)}, predicted values are
#' computed using \code{\link{E_negbin_noinfl}}.
#' 
#' @seealso
#' \code{\link{oneinfl}} for fitting one-inflated models.
#' \code{\link{truncreg}} for fitting truncated models.
#' \code{\link{E_pois_noinfl}}, \code{\link{E_negbin_noinfl}} for the expected value calculations.
#'
#' @examples
#' # Example usage
#' df <- data.frame(x = rnorm(100), y = rpois(100, lambda = 5))
#' model <- truncreg(y ~ x, df = df, dist = "Poisson")
#' predict(model, df = df, type = "response")
#'
#' @export

predict.truncmodel <- function(object, ...) {
  args <- list(...)
  df <- args$df
  type <- args$type %||% "response"
  
  b <- object$beta
  if (object$dist == "negbin") { a <- object$alpha }
  formula <- object$formula
  cleandata <- makeXZy(formula, df)
  X <- cleandata$X
  
  if (type == "response") {
    if (object$dist == "negbin") {
      return(E_negbin_noinfl(b, a, X))
    }
    if (object$dist == "Poisson") {
      return(E_pois_noinfl(b, X))
    }
  }
}