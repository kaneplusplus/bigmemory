#' @name big.matrix.descriptor-class
#' @docType class
#' @rdname big.matrix.descriptor-class
#' @title Class "big.matrix.descriptor"
#' @description An object of this class contains necessary and sufficient information
#' to ``attach'' a shared or filebacked \code{\link{big.matrix}}.
#' @section Objects from the Class:
#' Objects should not be created by calls of the form \code{new("big.matrix.descriptor", ...)},
#' but should use the \code{\link{describe}} function.
#' @section Slots:
#'   \describe{
#'      \item{\code{description}:}{Object of class \code{"list"}; details omitted.}
#'   }
#' @section Extends:
#' Class \code{"\linkS4class{descriptor}"}, directly.
#' @section Methods:
#' \describe{
#'      \item{attach.resource}{\code{signature(obj = "big.matrix.descriptor")}: ... }
#'      \item{sub.big.matrix}{\code{signature(x = "big.matrix.descriptor")}: ... }
#'  }
#' @references Other types of descriptors are defined in package \pkg{synchronicity}.
#' @author John W. Emerson and Michael J. Kane
#' @note We provide \code{attach.resource} for convenience, but expect most users
#' will prefer \code{\link{attach.big.matrix}}.
#' @seealso See also \code{\link{attach.big.matrix}}.
#' @examples showClass("big.matrix.descriptor")
#' @keywords classes
