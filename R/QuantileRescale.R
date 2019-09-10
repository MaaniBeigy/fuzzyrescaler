#' @title R6 Quantile Rescale
#' @name QuantileRescale
#' @description The R6 class \code{QuantileRescale} transforms data into 0-1 or
#'              0-100 scale with values above (1-p)\% and below p\% percentiles
#'              taken as the nearest endpoints.
#' @param x An \code{R} object. Currently there are methods for numeric
#'              vectors, matrices, and data frames.
#' @param select expression, indicating columns to select from a data frame or
#'              matrix
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds. This class is
#'              missing tolerable (i.e., compatible), in which the
#'              transformation will not remove \code{NAs} if na.rm is FALSE, it
#'              abides the dimension of vectors and retrun NAs beside the truly
#'              transformed values.
#' @param digits integer indicating the number of decimal places to be used.
#' @param probs numeric vector of probabilities with values in \code{[0,1]}.
#' @param max a scalar indicating the maximum endpoint of rescaled variable. The
#'            might be 1, 100, etc.
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#'             algorithms explained in \link[stats]{quantile} to be used.
#' @examples
#' x <- c(
#'    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
#'    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
#' )
#' x_trf <- QuantileRescale$new(x)$transform_x()
#' R6::is.R6(x_trf)
#' @export
#' @import dplyr R6
NULL
#' @importFrom cvcqv SampleQuantiles
NULL
QuantileRescale <- R6::R6Class(
    classname = "QuantileRescale",
    inherit = SampleQuantiles,
    public = list(
        # ---------------- determining defaults for arguments -----------------
        x = NA,
        select = NA,
        na.rm = FALSE,
        digits = 1,
        probs = 0.025,
        max = 1,
        type = 7,
        epsp1 = NA,
        epsm1 = NA,
        n = NA,
        quant1 = NA,
        quant2 = NA,
        transform_x = NA,
        transform_df = NA,
        xtr = 0,
        # --------- determining constructor defaults for arguments ------------
        initialize = function(
            x = NA,
            select = NA,
            na.rm = FALSE,
            digits = 1,
            probs = 0.025,
            max = 1,
            type = 7,
            ...
        ) {
            # ------------------------ check state x --------------------------
            if (missing(x) || is.null(x)) {
                stop("object 'x' not found")
            } else if (!missing(x)) {
                self$x <- x
            }
            # ---------------------- check state select -----------------------
            if (!missing(select)) {
                self$select <- select
            }
            # ---------------------- check state na.rm ------------------------
            if (!missing(na.rm)) {
                self$na.rm <- na.rm
            }
            # ---------- subset if data.frame and  handling missings ---------
            if ((!missing(select)) && is.data.frame(x) && na.rm == FALSE) {
                self$x <- subset(x, select = self$select)
            } else if (
                (!missing(select)) && is.data.frame(x) && na.rm == TRUE
                ) {
                self$x <- na.omit(subset(x, select = self$select))
            } else if (
                (missing(select)) && is.data.frame(x) && na.rm == FALSE
            ) {
                self$x <- x
            } else if (
                missing(select) && is.data.frame(x) && na.rm == TRUE
            ) {
                self$x <- na.omit(x)
            } else if (
                (missing(select)) && !is.data.frame(x) && na.rm == FALSE
                ) {
                self$x <- x
            } else if (
                (missing(select)) && !is.data.frame(x) && na.rm == TRUE
            ) {
                self$x <- x[!is.na(x)]
            }
            # ---------------- stop if input x is not numeric -----------------
            if (is.atomic(self$x) & !is.numeric(x)) {
                stop("argument is not a numeric vector")
            }
            # else if (
            #     !missing(select) &&
            #     is.data.frame(self$x) && !isTRUE(
            #     all(
            #         sapply(
            #             self$x,
            #             is.numeric
            #         )
            #     )
            # )
            # ) {
            #     warning("argument has non-numeric vectors")
            # } else if (
            #     missing(select) &&
            #     is.data.frame(self$x) && !isTRUE(
            #         all(
            #             sapply(
            #                 self$x,
            #                 is.numeric
            #             )
            #         )
            #     )
            # ) {
            #     warning("argument has non-numeric vectors")
            # }
            # -------------- check for probs being in range [0,1] -------------
            self$epsp1 <- function(...) {1 + 100*.Machine$double.eps}
            self$epsm1 <- function(...) {-1 * (100*.Machine$double.eps)}
            if (any(
                !missing(probs) && (
                    probs < self$epsm1() | probs > self$epsp1())
            )
            ) {
                stop("probs outside [0,1]")
            } else if ((
                !missing(probs) && (probs <= self$epsp1() & probs >= 1)
            )) {
                self$probs <- 1
            } else if ((
                !missing(probs) && (probs >= self$epsm1() & probs <= 0)
            )) {
                self$probs <- 0
            } else if (
                (
                    !missing(probs) && (probs >= 0 & probs <= 1)
                )
            ) {
                self$probs <- probs
            }
            # ----------------------- check state max -------------------------
            if (!missing(max)) {
                self$max <- max
            }
            # ------------------- set digits with user input ------------------
            if (!missing(digits)) {
                self$digits <- digits
            }
            # ------------------- set type with user input --------------------
            if (!missing(type)) {
                self$type <- type
            }
            # --- initialize the internal functions for the public methods ----
            # ------------- initialize internal n() i.e., length(x) -----------
            self$n = function(...) {
                # returns the length of input x
                if (is.atomic(self$x)) {
                    return(length(self$x))
                } else if (is.data.frame(self$x)) {
                    return(nrow(self$x))
                }
            }
            # --------- initialize quant1() i.e., (1-p)% percentiles ----------
            self$quant1 = function(...) {
                if (is.atomic(self$x)) {
                    SampleQuantiles$new(
                        x = self$x,
                        probs = (1 - self$probs),
                        na.rm = TRUE,
                        type = self$type,
                        digits = self$digits
                    )
                } else if (is.data.frame(self$x)) {
                    sapply(
                        self$x,
                        function(x) {
                            SampleQuantiles$new(
                                x,
                                probs = (1 - self$probs),
                                na.rm = TRUE,
                                type = self$type,
                                digits = self$digits
                            )$qx()
                        }
                    )
                }
            }
            # ---------- initialize quant2() i.e., (p)% percentiles -----------
            self$quant2 = function(...) {
                if (is.atomic(self$x)) {
                    SampleQuantiles$new(
                        x = self$x,
                        probs = (self$probs),
                        na.rm = TRUE,
                        type = self$type,
                        digits = self$digits
                    )
                } else if (is.data.frame(self$x)) {
                    sapply(
                        self$x,
                        function(x) {
                            SampleQuantiles$new(
                                x,
                                probs = (self$probs),
                                na.rm = TRUE,
                                type = self$type,
                                digits = self$digits
                            )$qx()
                        }
                    )
                }
            }
            self$transform_x = function(...) {
                if (is.atomic(self$x)) {
                    for (i in 1:self$n()) {
                        if (is.na(self$x[i])) {
                            self$xtr[i] = NA
                        } else if (self$x[i] >= self$quant1()$qx()) {
                            self$xtr[i] = 1
                        } else if (self$x[i] <= self$quant2()$qx()) {
                            self$xtr[i] = 0
                        } else if (
                            (self$x[i] < self$quant1()$qx()) & (
                                self$x[i] > self$quant2()$qx()
                            )
                        ) {
                            self$xtr[i] = (
                                (self$x[i] -
                                     self$quant2()$qx())/(
                                         self$quant1()$qx() -
                                             self$quant2()$qx())
                            )
                        }
                    }
                    return(
                        round(
                            (self$xtr) * self$max, digits = self$digits
                        )
                    )
                } else if (!is.atomic(self$x)) {
                    stop("x is not an atomic vector")
                }
            }
            self$transform_df = function(...) {
                if (is.data.frame(self$x)) {
                    rbind.data.frame(lapply(
                        self$x,
                        function(x) {
                            xtr = 0
                            for (i in 1:length(x)) {
                                if (is.na(x[i])) {
                                    xtr[i] = NA
                                    } else if (x[i] >= SampleQuantiles$new(x,
                                probs = (1 - self$probs),
                                na.rm = TRUE,
                                type = self$type,
                                digits = self$digits
                            )$qx()) {
                                xtr[i] = 1
                            } else if (x[i] <= SampleQuantiles$new(
                                x,
                                probs = (self$probs),
                                na.rm = TRUE,
                                type = self$type,
                                digits = self$digits
                            )$qx()) {
                                xtr[i] = 0
                            } else if (
                                (x[i] < SampleQuantiles$new(
                                    x,
                                    probs = (1 - self$probs),
                                    na.rm = TRUE,
                                    type = self$type,
                                    digits = self$digits
                                )$qx()) & (
                                    x[i] > SampleQuantiles$new(
                                        x,
                                        probs = (self$probs),
                                        na.rm = TRUE,
                                        type = self$type,
                                        digits = self$digits
                                    )$qx()
                                )
                            ) {
                                xtr[i] = (
                                    (x[i] -
                                         SampleQuantiles$new(
                                             x,
                                             probs = (self$probs),
                                             na.rm = TRUE,
                                             type = self$type,
                                             digits = self$digits
                                         )$qx())/(
                                             SampleQuantiles$new(
                                                 x,
                                                 probs = (1 - self$probs),
                                                 na.rm = TRUE,
                                                 type = self$type,
                                                 digits = self$digits
                                             )$qx() -
                                                 SampleQuantiles$new(
                                                     x,
                                                     probs = (self$probs),
                                                     na.rm = TRUE,
                                                     type = self$type,
                                                     digits = self$digits
                                                 )$qx())
                                )
                            }
                            }
                            return(
                                round((xtr) * self$max, digits = self$digits)
                            )
                            }
                        ))
                } else if (!is.data.frame(self$x)) {
                    stop("x is not a data frame")
                }
                }
        }
    ),
    # ---- define super_ function to enable multiple levels of inheritance ----
    active = list(
        super_ = function() {super}
    )
)

