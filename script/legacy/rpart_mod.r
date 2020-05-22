# ----------------------------------------
# -- GREATER CIRCLE DISTANCE DEFINITION --
# ----------------------------------------

# library(Rcpp)
# library(RcppArmadillo)
# sourceCpp("script/dist_cpp.cpp")

# ------------------------------
# -- MODIFIED RPART FUNCTIONS --
# ------------------------------

# Helper distance formula
# If we feed it projected data, then we shouldn't have to worry about the
# geographic CPP script
distance <- function(x1, y1, x2, y2) {
  sqrt((x2-x1)^2 + (y2-y1)^2)
  # loc1 <- matrix(data = c(x1, y1), ncol = 2, byrow = TRUE)
  # loc2 <- matrix(data = c(x2, y2), ncol = 2, byrow = TRUE)
  # dist_cpp(loc1, loc2, geographic = TRUE)
}

# Mean square distance
msd <- function(parms) {
  long_bar <- mean(parms$long)
  lat_bar <- mean(parms$lat)
  distances <- distance(parms$long, parms$lat, long_bar, lat_bar)
  mean(distances^2)
}

# INITIALIZATION FUNCTION
# --------------------
# Pass in:
# y - response value in formula
# offset - offset term on RHS of formula
# parms - parameters supplied by user
# wt - weight vector from the call
# --------------------
# Returns a list with:
# y - resopnse value (might be updated with offset)
# numy - number of columns of y
# numresp - length of prediction vector for a node
# summary - optional functino that makes a 1-3 line summary for the node
# print - optional function which makes one line summary for print function
# text - optional, makes short label for node by plot function
init_mod <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (missing(parms))
    stop("Parameter with lat/long required")
  if (!missing(wt) && length(wt) > 0) {
    warning("weights ignored for this method")
    # print(wt)
  }
  if (!is.data.frame(parms))
    stop("parms must be a dataframe")
  if (dim(parms)[1] != length(y))
    warning("parms and y are of different lengths. Calcutions may be incorrect.")
  if (length(offset))
    y <- y - offset

  # Summary function
  sfun <- function(yval, dev, wt, ylevel, digits) {
    paste("  mean=", format(signif(yval, digits)),
          ", MSE=", format(signif(dev/wt, digits)),
          sep = '')
  }
  # Don't keep copy of summary function vars to bog down workspace
  environment(sfun) <- .GlobalEnv
  # Return value
  list(y = c(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
}

# TODO:
# Currently, the MSD is calculated by equally weighting the distances to the center with
# the RSS. You will probably want another parameter in here to control the effect of MSD on
# the deviance.

# EVALUATION FUNCTION
# -----------------------
# Produces deviance and label for the node
eval_mod <- function(y, wt, parms) {
  # Base RSS
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  # Extra loss parameter based on mean square distance 
  long_bar <- mean(parms$long)
  lat_bar <- mean(parms$lat)
  distances <- distance(parms$long, parms$lat, long_bar, lat_bar)
  sd_node <- sum(distances^2)
  
  # We'll let the deviance be the rss in addition to the penality term
  list(label = wmean, deviance = rss)
}


# SPLITTING FUNCTION
# ---------------------
# Input:
# y - vector/matrix of response values
# wt - vector of weights
# vector of x values
# parms - vector of user parameters being passed forward
# continous - if TRUE then treat the x variable as continuous
# goodness - utility of split (larger numbers better)
# direction - A vector that tells where values should be sent in the tree
split_mod <- function(y, wt, x, parms, continuous) {
  
  
  # How much should we weight the distance metric?
  lambda <- 1
  
  # Center y
  n <- length(y)
  y <- y - sum(y*wt)/sum(wt)
  
  if (continuous) {
    
    # We need parms to coincide with the ordering of x, so we if we
    # extract the ordering of x, then we can put parms in the same order
    long_vec <- parms$long[order(x, method="radix")]
    lat_vec <- parms$lat[order(x, method="radix")]
    locations <- as.data.frame(cbind(long_vec, lat_vec))
    
    # browser()
    
    # Response weighting
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp / left.wt
    rmean <- -temp / right.wt
    p1 <- (left.wt*lmean^2 + right.wt*rmean^2) / sum(wt*y^2)
    
    # Distance weighting
    # Unvectorized for the moment. This will probably take ages to run, so a TODO item will
    # be to vectorize all of this.
    g_length <- length(x) - 1
    p2 <- rep(0, g_length)
    for (split_loc in 1:g_length) {
      # Get left half and right half of long/lat
      left_half <- as.data.frame(locations[1:split_loc, ])
      right_half <- as.data.frame(locations[(split_loc+1):length(x), ])
      
      # browser()
      
      # p2 at split_loc will be BSS / TSS
      TSS <- sum(dist(locations)^2)
      BSS <- 0
      for (i in 1:(dim(left_half)[1])) {
        BSS <- BSS + sum((dist(rbind(left_half[i, ], right_half))^2)[1:dim(right_half)[1]])
      }
      p2[split_loc] <- (BSS / TSS)
    }
    
    # Add on to goodness and then return
    goodness <- (1-lambda)*p1 + lambda*p2
    
    list(goodness = goodness, direction = sign(lmean))
  }
  else {
    # categorical x var
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum <- tapply(y*wt, x, sum)
    means <- ysum/wtsum

    # With anova splits, order the categories by their means
    # then use the same code as for a non-categorical
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness = (left.wt*lmean^2 + right.wt*rmean^2) / sum(wt*y^2),
         direction = ux[ord])
  }
}

anova_mod <- list(eval = eval_mod, split = split_mod, init = init_mod)

# TEST OF METHOD
# ----------------
# library(rpart)
# mystate <- data.frame(state.x77, region=state.region)
# names(mystate) <- casefold(names(mystate)) #remove mixed case
# ulist <- list(eval = etemp, split = stemp, init = itemp)
#
# fit1 <- rpart(murder ~ population + illiteracy + income + life.exp +
#                 hs.grad + frost + region, data = mystate,
#               method = ulist, minsplit = 10)
# fit2 <- rpart(murder ~ population + illiteracy + income + life.exp +
#                 hs.grad + frost + region, data = mystate,
#               method = 'anova', minsplit = 10, xval = 0)
# all.equal(fit1$frame, fit2$frame)
# all.equal(fit1$splits, fit2$splits)
# all.equal(fit1$csplit, fit2$csplit)
# all.equal(fit1$where, fit2$where)
# all.equal(fit1$cptable, fit2$cptable)
