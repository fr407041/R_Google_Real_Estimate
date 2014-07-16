library(corpcor)
kriging2 = function (x, y, response, model = "spherical", lags = 10, pixels = 100, 
          polygons = NULL) 
{
  n <- length(response)
  p <- 2
  D <- kriging:::twodim(x, y, n)
  V <- kriging:::onedim(response, n)
  cutoff <- sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2)/3
  W <- kriging:::krig.vg(as.matrix(D, n, n), as.matrix(V, n, n), cutoff, 
               lags)
  fit.vg <- lm(W[, 3] ~ W[, 2])$coefficients
  nugget <- as.numeric(fit.vg[1])
  range <- max(W[, 2])
  sill <- nugget + as.numeric(fit.vg[2]) * range
  a <- 1/3
  A <- kriging:::krig.fit(D, nugget, range, sill, model, n)
  G <- kriging:::krig.grid(min(x), min(y), max(x), max(y), pixels)
  pixel <- unique(G$pixel)
  if (!is.null(polygons)) {
    G <- kriging:::krig.polygons(G$x, G$y, polygons)
  }
  G.pred <- kriging:::krig.pred(x, y, response, G$x, G$y, c(pseudoinverse(matrix(A, 
                                                               n + 1, n + 1))), nugget, range, sill, model, n)
  o <- list(model = model, nugget = nugget, range = range, 
            sill = sill, pixel = pixel, map = data.frame(x = G$x, 
                                                         y = G$y, pred = G.pred), semivariogram = data.frame(distance = W[, 
                                                                                                                          2], semivariance = W[, 3]))
  class(o) <- "kriging"
  o
}