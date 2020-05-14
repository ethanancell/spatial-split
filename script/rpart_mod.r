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
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
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
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}


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

  if (continuous) {
    # continuous x var
    temp <- cumsum(y*wt)[-n]
    left.wt <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp / left.wt
    rmean <- -temp / right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2) / sum(wt*y^2)
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
