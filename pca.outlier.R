pca.outlier <- function (x, center = TRUE, scale = TRUE, conf.level = 0.975, pcs = c(1,2),
          ...) 
{
  panel.outlier <- function(x, y, groups = NULL, elli, labs, 
                            id, ..., type, col, lty, lwd) {
    if (is.null(groups)) {
      panel.xyplot(x, y, ...)
    }
    else {
      panel.superpose(x, y, groups, ...)
    }
    panel.abline(h = 0, v = 0, col = c("gray"), lty = 2)
    panel.points(elli[, 1], elli[, 2], type = "l", col = "red", 
                 lwd = 2, ...)
    if (any(id)) {
      ltext(x[id], y[id], labs[id], ...)
    }
  }
  dots <- list(...)
  if (length(dots) > 0) {
    args <- dots
  }
  else {
    args <- list()
  }
  pca <- prcomp(x, center = center, scale. = scale)
  vars <- pca$sdev^2
  vars <- vars/sum(vars)
  names(vars) <- colnames(pca$rotation)
  vars <- round(vars * 100, 2)
  dfn <- paste(names(vars), " (", vars[names(vars)], "%)", 
               sep = "")
  x <- data.frame(pca$x)
  names(x) <- dfn
  x <- x[, pcs]
  cent <- colMeans(x)
  cova <- cov(x)
  dist <- sqrt(mahalanobis(x, center = cent, cov = cova))
  cuto <- sqrt(qchisq(conf.level, ncol(x)))
  id <- dist > cuto
  elli <- ellipse(var(x), centre = cent, level = conf.level)
  labs <- rownames(x)
  args <- c(list(x = x[, 2] ~ x[, 1], data = x), args)
  if (is.null(args$xlab)) 
    args$xlab <- names(x)[1]
  if (is.null(args$ylab)) 
    args$ylab <- names(x)[2]
  if (F) {
    xlim <- c(min(x[, 1], elli[, 1]), max(x[, 1], elli[, 
                                                       1]))
    ylim <- c(min(x[, 2], elli[, 2]), max(x[, 2], elli[, 
                                                       2]))
    if (is.null(args$xlim)) 
      args$xlim <- xlim
    if (is.null(args$ylim)) 
      args$ylim <- ylim
  }
  args <- c(args, panel = panel.outlier)
  args$elli <- elli
  args$labs <- labs
  args$id <- id
  p <- do.call("xyplot", args)
  ret <- list(plot = p, outlier = which(id), conf.level = conf.level, 
              mah.dist = dist, cutoff = cuto)
  return(ret)
}