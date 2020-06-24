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
    stop("parms with long/lat columns required.")
  # Error check the parms
  if (!is.data.frame(parms))
    stop("parms must be a dataframe with long/lat column names.")
  if (dim(parms)[1] != length(y))
    stop("parms must be of same length as response vector.")
  if (length(offset))
    y <- y - offset
  
  print(length(y))

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

if (FALSE) {
# Test of new Moran I function
require(spdep)
e1

# Get E1 and E2 partitions
e1 <- as.data.frame(locations[1:split_loc, ])
e2 <- as.data.frame(locations[(split_loc+1):nrow(soil), ])
y1 <- y[1:split_loc]
y2 <- y[(split_loc+1):length(y)]

# E1
#IY
dists <- as.matrix(dist(cbind(e1$long, e1$lat)))
dists.inv <- 1 / dists
diag(dists.inv) <- 0
temp <- moran.test(y1, mat2listw(dists.inv), na.action = na.omit, zero.policy = TRUE)
iyhat <- (temp$estimate[1] + 1) / 2
t2[split_loc] <- iyhat * split_loc

# E2
dists <- as.matrix(dist(cbind(e2$long, e2$lat)))
dists.inv <- 1 / dists
diag(dist.inv) <- 0
temp <- Moran.I(y2, dists.inv)
iyhat <- (temp$observed + 1) / 2
t2[split_loc] <- t2[split_loc] + (iyhat * (length(x) - split_loc))

t2[split_loc] <- t2[split_loc] / length(x)
}
# END TEST

# EVALUATION FUNCTION
# -----------------------
# Produces deviance and label for the node
eval_mod <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
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
  # Center y
  n <- length(y)
  y <- y - sum(y*wt)/sum(wt)
  
  # Error check
  #if (length(y) != length(x) | length(y) != length(wt) | length(y) != nrow(parms)) {
  #  print("Error with sizes of values passed in.")
  #  browser()
  #}

  if (continuous) {
    # continuous x var
    
    # Weighting of term 1 and term 2 (t1 & t2)
    alpha <- 0.5
    
    # Term 1
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp / left.wt
    rmean <- -temp / right.wt
    t1 <- (left.wt*lmean^2 + right.wt*rmean^2) / sum(wt*y^2)
    
    # Term 2
    long_vec <- parms$long[order(x, method = "radix")]
    lat_vec <- parms$lat[order(x, method = "radix")]
    locations <- as.data.frame(cbind(long_vec, lat_vec))
    g_length <- length(x) - 1
    t2 <- rep(0, g_length)
    
    #browser()
    
    if (FALSE) {
    
    for (split_loc in 1:g_length) {
      # Get E1 and E2 partitions
      e1 <- as.data.frame(locations[1:split_loc, ])
      e2 <- as.data.frame(locations[(split_loc+1):length(x), ])
      y1 <- y[1:split_loc]
      y2 <- y[(split_loc+1):length(y)]
      
      # E1
      #IY
      if (split_loc == 1) {
        # Moran's I will fail if you only have one observation
        t2[1] <- 0
      } else {
        dists <- as.matrix(dist(cbind(e1$long, e1$lat)))
        dists.inv <- 1 / dists
        #diag(dists.inv) <- 0
        dists.inv[is.infinite(dists.inv)] <- 0
        dists.inv[is.na(dists.inv)] <- 0
        if (sum(dists.inv < 0)) {
          print("Houston, we have a problem.")
          browser()
        }
        if (length(y1) != dim(dists.inv)[1]) {
          print("Houston2, we have a problem.")
          browser()
        }
        temp <- Moran.I(y1, dists.inv, scaled = TRUE)
        if (temp$observed < -1 | temp$observed > 1) {
          print("Houston3, we have a problem.")
          browser()
        }
        iyhat <- (temp$observed + 1) / 2
        t2[split_loc] <- iyhat * split_loc
      }
      
      # E2
      if (split_loc == (length(x) - 1)) {
        # Leave it alone
        useless_line <- 5
      } else {
        dists <- as.matrix(dist(cbind(e2$long, e2$lat)))
        dists.inv <- 1 / dists
        #diag(dists.inv) <- 0
        dists.inv[is.infinite(dists.inv)] <- 0
        dists.inv[is.na(dists.inv)] <- 0
        if (sum(dists.inv < 0)) {
          print("Houston, we have a problem.")
          browser()
        }
        if (length(y2) != dim(dists.inv)[1]) {
          print("Houston2, we have a problem.")
          browser()
        }
        temp <- Moran.I(y2, dists.inv, scaled = TRUE)
        if (temp$observed < -1 | temp$observed > 1) {
          print("Houston3, we have a problem.")
          browser()
        }
        iyhat <- (temp$observed + 1) / 2
        t2[split_loc] <- t2[split_loc] + (iyhat * (length(x) - split_loc))
      }
      
      t2[split_loc] <- t2[split_loc] / length(x)
    }
    }
    
    #print(length(y))
    
    # Final goodness of split
    goodness <- (alpha * t1) + (1 - alpha) * t2
    list(goodness = goodness, direction = sign(lmean))
  } else {
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
