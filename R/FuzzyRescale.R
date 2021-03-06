#' @title R6 Fuzzy Rescale
#' @name FuzzyRescale
#' @description The R6 class \code{FuzzyRescale} transforms data into 0-1 or
#'              0-100 scale based on Fuzzy membership functions.
#' @param x An \code{R} object. Currently there are methods for
#'              vectors, matrices, and data frames. Factors are allowed, but
#'              characters are not.
#' @param select expression, indicating columns to select from a data frame or
#'              matrix
#' @param na.rm a logical value indicating whether \code{NA} values should be
#'              stripped before the computation proceeds. This class is
#'              missing tolerable (i.e., compatible), in which the
#'              transformation will not remove \code{NAs} if na.rm is FALSE, it
#'              abides the dimension of vectors and retrun NAs beside the truly
#'              transformed values.
#' @param digits integer indicating the number of decimal places to be used.
#' @param max a scalar indicating the maximum endpoint of rescaled variable. The
#'            might be 1, 100, etc.
#' @param center it determines the central value of a fuzzy set. It should be
#'            any of the values \link[base:mean]{"mean"},
#'            \link[stats:median]{"median"} or any numeric input. It will have
#'            a membership degree of 1, in \code{gaussmf()} method; 0.5 in
#'            \code{sigmf()};
#' @param sigma it determines the width of a fuzzy set obtained from either
#'            \code{gaussmf()} or \code{sigmf()}. It should be any of the
#'            values \link[stats:sd]{"sd"}, \link[stats:IQR]{"IQR"} or any
#'            numeric input. In \code{sigmf()}, to open the membership function
#'            to the left or right, specify a negative or positive value for
#'            \code{sigma}, respectively.
#' @param param_one This is the first parameter for \code{smf()}. If
#'            \code{param_one} is lower than \code{param_two}, it will be the
#'            \strong{foot} of the membership function; otherwise it will define
#'            the \strong{shoulder}. It should be either \link[base:min]{"min"}
#'            or \link[base:max]{"max"}, any
#'            \link[cvcqv:SampleQuantiles]{quantiles} defined by the prefix
#'            \code{"p"} followed by a value in \code{[0,100]} (\emph{e.g.,}
#'            "p2.5" indicates 2.5\% percentile of the input variable),
#'            or any numeric input.
#' @param param_two This is the second parameter for \code{smf()}. If
#'            \code{param_two} is higher than \code{param_one}, it will define
#'            the \strong{shoulder} of the membership function; otherwise it
#'            will be the \strong{foot} It should be either
#'            \link[base:max]{"max"} or \link[base:min]{"min"}, any
#'            \link[cvcqv:SampleQuantiles]{quantiles} defined by the prefix
#'            \code{"p"} followed by a value in \code{[0,100]} (\emph{e.g.,}
#'            "p97.5" indicates 97.5\% percentile of the input variable),
#'            or any numeric input.
#' @examples
#' x <- c(
#'    0.2, 0.5, 1.1, 1.4, 1.8, 2.3, 2.5, 2.7, 3.5, 4.4,
#'    4.6, 5.4, 5.4, 5.7, 5.8, 5.9, 6.0, 6.6, 7.1, 7.9
#' )
#' x_fuzzy <- FuzzyRescale$new(x)
#' R6::is.R6(x_fuzzy)
#' x_fuzzy$gaussmf()
#' @export
#' @import dplyr R6
NULL
#' @importFrom scales rescale
#' @importFrom stats median sd IQR
#' @importFrom cvcqv SampleQuantiles
NULL
FuzzyRescale <- R6::R6Class(
  classname = "QuantileRescale",
  inherit = SampleQuantiles,
  public = list(
    # ------------------ determining defaults for arguments -------------------
    x = NA,
    select = NA,
    na.rm = FALSE,
    digits = 1,
    max = 1,
    xtr = 0,
    n = NA,
    center = "mean",
    sigma = "sd",
    param_one = "p2.5",
    param_two = "p97.5",
    # ------------- determining constructor defaults for arguments ------------
    initialize = function(
      x = NA,
      select = NA,
      na.rm = FALSE,
      digits = 1,
      max = 1,
      center = "mean",
      sigma = "sd",
      param_one = "p2.5",
      param_two = "p97.5",
      ...
    ) {
      # ---------------------------- check state x ----------------------------
      if (missing(x) || is.null(x)) {
        stop("object 'x' not found")
      } else if (!missing(x)) {
        self$x <- x
      }
      # -------------------------- check state select -------------------------
      if (!missing(select)) {
        self$select <- select
      }
      # -------------------------- check state na.rm --------------------------
      if (!missing(na.rm)) {
        self$na.rm <- na.rm
      }
      # --------------- subset if data.frame and  handling missings -----------
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
      # -------------------- stop if input x is not numeric -------------------
      if (is.atomic(self$x) & is.character(x)) {
        stop("argument is a character vector")
      }
      # --------------------------- check state max ---------------------------
      if (!missing(max)) {
        self$max <- max
      }
      # ---------------------- set digits with user input ---------------------
      if (!missing(digits)) {
        self$digits <- digits
      }
      # ---------------------- set center with user input ---------------------
      if (
        !missing(center) & !is.numeric(center) & center != "mean" &
        center != "median"
      ) {
        stop(
          "Incorrect method for center"
        )
      } else if (
        (!missing(center) & !is.numeric(center)) & (center == "mean" |
        center == "median")
        ) {
        self$center <- tolower(center)
      }  else if (
        !missing(center) & is.numeric(center)
      ) {
        self$center <- center
      }
      # ---------------------- set sigma with user input ---------------------
      if (
        !missing(sigma) & !is.numeric(sigma) & sigma != "sd" &
        sigma != "IQR"
      ) {
        stop(
          "Incorrect method for sigma"
        )
      } else if (
        (!missing(sigma) & !is.numeric(sigma)) & (sigma == "sd" |
        sigma == "IQR")
        ) {
        self$sigma <- sigma
      } else if (
        !missing(sigma) & is.numeric(sigma)
      ) {
        self$sigma <- sigma
      }
      # ------------------ set param_one with user input ----------------------
      if (
        !missing(param_one) & !is.numeric(param_one) & param_one != "min" &
        param_one != "max" & gsub("[.[:digit:]]", "", param_one) != "p" &
        is.na(suppressWarnings(
          as.numeric(gsub("^[^[:digit:]]", "", param_one))))
      ) {
        stop(
          "Incorrect method for param_one"
        )
      } else if (param_one == "min" | param_one == "max") {
        self$param_one <- param_one
      } else if (
        (!missing(param_one) & !is.numeric(param_one) & (
          gsub("[.[:digit:]]", "", param_one) == "p" &
          !is.na(suppressWarnings(
            as.numeric(gsub("^[^[:digit:]]", "", param_one))))
        ))
      ) {
        if (
          as.numeric(
            gsub("^[^[:digit:]]", "", param_one))/100 > (
              1 + (100*.Machine$double.eps))
        ) {
          stop("probs outside [0,100]")
        } else if (
          (as.numeric(gsub("^[^[:digit:]]", "", param_one)
                      )/100 <= (1 + 100*.Machine$double.eps)) & (
           as.numeric(gsub("^[^[:digit:]]", "", param_one))/100 >= 1
            )
        ) {
          self$param_one <- "p100"
        } else if (
            (as.numeric(gsub("^[^[:digit:]]", "", param_one))
            )/100 >= 0 & (
              as.numeric(gsub("^[^[:digit:]]", "", param_one))/100 <= 1
            )
          ) {
          self$param_one <- param_one
        }
      } else if (!missing(param_one) & is.numeric(param_one)) {
        self$param_one <- param_one
      }
      # ------------------ set param_two with user input ----------------------
      if (
        !missing(param_two) & !is.numeric(param_two) & param_two != "min" &
        param_two != "max" & gsub("[.[:digit:]]", "", param_two) != "p" &
        is.na(suppressWarnings(
          as.numeric(gsub("^[^[:digit:]]", "", param_two))))
      ) {
        stop(
          "Incorrect method for param_two"
        )
      } else if (param_two == "min" | param_two == "max") {
        self$param_two <- param_two
      } else if (
        (!missing(param_two) & !is.numeric(param_two) & (
          gsub("[.[:digit:]]", "", param_two) == "p" &
          !is.na(suppressWarnings(
            as.numeric(gsub("^[^[:digit:]]", "", param_two))))
        ))
      ) {
        if (
          as.numeric(
            gsub("^[^[:digit:]]", "", param_two))/100 > (
              1 + (100*.Machine$double.eps))
        ) {
          stop("probs outside [0,100]")
        } else if (
          (as.numeric(gsub("^[^[:digit:]]", "", param_two)
          )/100 <= (1 + 100*.Machine$double.eps)) & (
            as.numeric(gsub("^[^[:digit:]]", "", param_two))/100 >= 1
          )
        ) {
          self$param_two <- "p100"
        } else if (
          (as.numeric(gsub("^[^[:digit:]]", "", param_two))
          )/100 >= 0 & (
            as.numeric(gsub("^[^[:digit:]]", "", param_two))/100 <= 1
          )
        ) {
          self$param_two <- param_two
        }
      } else if (!missing(param_two) & is.numeric(param_two)) {
        self$param_two <- param_two
      }
      # ------ initialize the internal functions for the public methods -------
      self$n = function(...) {
        # returns the length of input x
        if (is.atomic(self$x)) {
          return(length(self$x))
        } else if (is.data.frame(self$x)) {
          return(nrow(self$x))
        }
      }
    },
    # --------------- public methods for fuzzy membership values --------------
    # ---------- gaussmf() method using a Gaussian membership function --------
    gaussmf = function(...) {
      if (is.atomic(self$x)) {
        for (i in 1:self$n()) {
          if (is.na(self$x[i])) {
            self$xtr[i] = NA
          } else if (is.factor(self$x)) {
            self$xtr = scales::rescale(
              as.numeric(self$x), to = c(0, 1)
            )
          } else if (
            is.numeric(self$x) & self$center == "mean" & self$sigma == "sd"
            ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - mean(self$x, na.rm = TRUE))^2)/(
                2*(sd(self$x, na.rm = TRUE)^2)))
              )
          } else if (
            is.numeric(self$x) & self$center == "median" & self$sigma == "IQR"
            ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - median(self$x, na.rm = TRUE))^2)/(
                2*(IQR(self$x, na.rm = TRUE)^2)))
            )
          } else if (
            is.numeric(self$x) & self$center == "mean" & self$sigma == "IQR"
          ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - mean(self$x, na.rm = TRUE))^2)/(
                2*(IQR(self$x, na.rm = TRUE)^2)))
            )
          } else if (
            is.numeric(self$x) & self$center == "median" & self$sigma == "sd"
          ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - median(self$x, na.rm = TRUE))^2)/(
                2*(sd(self$x, na.rm = TRUE)^2)))
            )
          } else if (
            is.numeric(self$x) & is.numeric(self$center) &
            is.numeric(self$sigma)
          ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - self$center)^2)/(
                2*((self$sigma)^2)))
            )
          } else if (
            is.numeric(self$x) & is.numeric(self$center) &
            self$sigma == "sd"
          ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - self$center)^2)/(
                2*(sd(self$x, na.rm = TRUE)^2)))
            )
          } else if (
            is.numeric(self$x) & is.numeric(self$center) &
            self$sigma == "IQR"
          ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - self$center)^2)/(
                2*(IQR(self$x, na.rm = TRUE)^2)))
            )
          } else if (
            is.numeric(self$x) & self$center == "mean" &
            is.numeric(self$sigma)
          ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - mean(self$x, na.rm = TRUE))^2)/(
                2*((self$sigma)^2)))
            )
          } else if (
            is.numeric(self$x) & self$center == "median" &
            is.numeric(self$sigma)
          ) {
            self$xtr[i] = exp(
              -1*(((self$x[i] - median(self$x, na.rm = TRUE))^2)/(
                2*((self$sigma)^2)))
            )
          }
        }
        return(
          round(
            (self$xtr) * self$max, digits = self$digits
          )
        )
      } else if (is.data.frame(self$x)) {
        rbind.data.frame(lapply(
          self$x,
          function(x) {
            xtr = 0
            for (i in 1:length(x)) {
              if (is.numeric(x) & is.na(x[i])) {
                xtr[i] = NA
              } else if (is.factor(x)) {
                xtr = scales::rescale(as.numeric(x), to = c(0, 1))
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "mean" &
                self$sigma == "sd"
                ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - mean(x, na.rm = TRUE))^2)/(
                      2*(sd(x, na.rm = TRUE)^2))
                    )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "median" &
                self$sigma == "IQR"
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - median(x, na.rm = TRUE))^2)/(
                      2*(IQR(x, na.rm = TRUE)^2))
                  )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "mean" &
                self$sigma == "IQR"
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - mean(x, na.rm = TRUE))^2)/(
                      2*(IQR(x, na.rm = TRUE)^2))
                  )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "median" &
                self$sigma == "sd"
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - median(x, na.rm = TRUE))^2)/(
                      2*(sd(x, na.rm = TRUE)^2))
                  )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & is.numeric(self$center) &
                is.numeric(self$sigma)
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - self$center)^2)/(
                      2*((self$sigma)^2))
                  )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & is.numeric(self$center) &
                self$sigma == "sd"
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - self$center)^2)/(
                      2*(sd(x, na.rm = TRUE)^2))
                  )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & is.numeric(self$center) &
                self$sigma == "IQR"
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - self$center)^2)/(
                      2*(IQR(x, na.rm = TRUE)^2))
                  )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "mean" &
                is.numeric(self$sigma)
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - mean(x, na.rm = TRUE))^2)/(
                      2*((self$sigma)^2))
                  )
                )
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "median" &
                is.numeric(self$sigma)
              ) {
                xtr[i] = exp(
                  -1*(
                    ((x[i] - median(x, na.rm = TRUE))^2)/(
                      2*((self$sigma)^2))
                  )
                )
              }
            }
            return(
              round((xtr) * self$max, digits = self$digits)
            )
          }
        ))
      }
    },
    # ---------- sigmf() method using a Sigmoidal membership function ---------
    sigmf = function(...) {
      if (is.atomic(self$x)) {
        for (i in 1:self$n()) {
          if (is.na(self$x[i])) {
            self$xtr[i] = NA
          } else if (is.factor(self$x)) {
            self$xtr = scales::rescale(
              as.numeric(self$x), to = c(0, 1)
            )
          } else if (
            is.numeric(self$x) & self$center == "mean" & self$sigma == "sd"
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(sd(self$x, na.rm = TRUE))
                )*(self$x[i] - mean(self$x, na.rm = TRUE))))))
          } else if (
            is.numeric(self$x) & self$center == "median" & self$sigma == "IQR"
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(IQR(self$x, na.rm = TRUE))
              )*(self$x[i] - median(self$x, na.rm = TRUE))))))
          } else if (
            is.numeric(self$x) & self$center == "mean" & self$sigma == "IQR"
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(IQR(self$x, na.rm = TRUE))
              )*(self$x[i] - mean(self$x, na.rm = TRUE))))))
          } else if (
            is.numeric(self$x) & self$center == "median" & self$sigma == "sd"
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(sd(self$x, na.rm = TRUE))
              )*(self$x[i] - median(self$x, na.rm = TRUE))))))
          } else if (
            is.numeric(self$x) & is.numeric(self$center) &
            is.numeric(self$sigma)
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(self$sigma)
              )*(self$x[i] - self$center)))))
          } else if (
            is.numeric(self$x) & is.numeric(self$center) &
            self$sigma == "sd"
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(sd(self$x, na.rm = TRUE))
              )*(self$x[i] - self$center)))))
          } else if (
            is.numeric(self$x) & is.numeric(self$center) &
            self$sigma == "IQR"
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(IQR(self$x, na.rm = TRUE))
              )*(self$x[i] - self$center)))))
          } else if (
            is.numeric(self$x) & self$center == "mean" &
            is.numeric(self$sigma)
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(self$sigma)
              )*(self$x[i] - mean(self$x, na.rm = TRUE))))))
          } else if (
            is.numeric(self$x) & self$center == "median" &
            is.numeric(self$sigma)
          ) {
            self$xtr[i] = 1/(1 + (exp(
              ((-1*(self$sigma)
              )*(self$x[i] - median(self$x, na.rm = TRUE))))))
          }
        }
        return(
          round(
            (self$xtr) * self$max, digits = self$digits
          )
        )
      } else if (is.data.frame(self$x)) {
        rbind.data.frame(lapply(
          self$x,
          function(x) {
            xtr = 0
            for (i in 1:length(x)) {
              if (is.numeric(x) & is.na(x[i])) {
                xtr[i] = NA
              } else if (is.factor(x)) {
                xtr = scales::rescale(as.numeric(x), to = c(0, 1))
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "mean" &
                self$sigma == "sd"
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(sd(x, na.rm = TRUE))
                  )*(x[i] - mean(x, na.rm = TRUE))))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "median" &
                self$sigma == "IQR"
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(IQR(x, na.rm = TRUE))
                  )*(x[i] - median(x, na.rm = TRUE))))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "mean" &
                self$sigma == "IQR"
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(IQR(x, na.rm = TRUE))
                  )*(x[i] - mean(x, na.rm = TRUE))))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "median" &
                self$sigma == "sd"
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(sd(x, na.rm = TRUE))
                  )*(x[i] - median(x, na.rm = TRUE))))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & is.numeric(self$center) &
                is.numeric(self$sigma)
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(self$sigma)
                  )*(x[i] - self$center)))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & is.numeric(self$center) &
                self$sigma == "sd"
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(sd(x, na.rm = TRUE))
                  )*(x[i] - self$center)))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & is.numeric(self$center) &
                self$sigma == "IQR"
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(IQR(x, na.rm = TRUE))
                  )*(x[i] - self$center)))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "mean" &
                is.numeric(self$sigma)
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(self$sigma)
                  )*(x[i] - mean(x, na.rm = TRUE))))))
              } else if (
                is.numeric(x) & !is.na(x[i]) & self$center == "median" &
                is.numeric(self$sigma)
              ) {
                xtr[i] = 1/(1 + (exp(
                  ((-1*(self$sigma)
                  )*(x[i] - median(x, na.rm = TRUE))))))
              }
            }
            return(
              round((xtr) * self$max, digits = self$digits)
            )
          }
        ))
      }
    }
    # ,
    # ----------- smf() method using a S-shaped membership function -----------
    # smf = function(...) {
    #
    # }
    # ------------ zmf() method using a Z-shaped membership function ----------
    # zmf = function(...) {
    #   print("hello zmf")
    # }
    # --------- trimf() method using a Triangular membership function ---------
    # trimf = function(...) {
    #   print("hello trimf")
    # }
    # -------- trapmf() method using a Trapezoidal membership function --------
    # trapmf = function(...) {
    #   print("hello trapmf")
    # }
    # -- gbellmf() method using a Generalized bell-shaped membership function -
    # gbellmf = function(...) {
    #   print("hello gbellmf")
    # }
    # ---------- pimf() method using a Pi-shaped membership function ----------
    # pimf = function(...) {
    #   print("hello pimf")
    # }
    # --------- lingmf() method using a linguistic membership function --------
    # lingmf = function(...) {
    #   print("hello lingmf")
    # }
    # -- gauss2mf() method using a Gaussian combination membership function ---
    # gauss2mf = function(...) {
    #   print("hello gauss2mf")
    # }
  ),
    # ---- define super_ function to enable multiple levels of inheritance ----
    active = list(
      super_ = function() {super}
    )
)
