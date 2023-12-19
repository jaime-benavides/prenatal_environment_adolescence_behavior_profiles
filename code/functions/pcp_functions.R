root_pcp_noncvx_na <- function(D, lambda, mu, r, verbose = FALSE,   MAX_ITER = 20000) 
{
  n = nrow(D)
  p = ncol(D)
  rho = 0.1
  L1 <- matrix(0, n, p)
  L2 <- matrix(0, n, p)
  S1 <- matrix(0, n, p)
  S2 <- matrix(0, n, p)
  Z <- matrix(0, n, p)
  Y1 <- matrix(0, n, p)
  Y2 <- matrix(0, n, p)
  Y3 <- matrix(0, n, p)
  mask = !is.na(D)
  D[!mask] = 0
  EPS_ABS = 1e-06
  EPS_REL = 1e-06
  flag_converge = 0
  for (i in 1:MAX_ITER) {
    L2_old = L2
    S2_old = S2
    L1 = proj_rank_r(L2 - Y1/rho, r)
    S1 = prox_l1(S2 - Y2/rho, lambda/rho)
    Z = prox_fro(mask * (D - L2 - S2) - Y3/rho, mu/rho)
    L2_obs = mask * (1/3 * (D - Z + 2 * L1 - S1 + (2 * Y1 - 
                                                     Y2 - Y3)/rho))
    L2_unobs = (1 - mask) * (L1 + Y1/rho)
    L2 = L2_obs + L2_unobs
    S2_obs = mask * (1/3 * (D - Z + 2 * S1 - L1 + (2 * Y2 - 
                                                     Y1 - Y3)/rho))
    S2_unobs = (1 - mask) * (S1 + Y2/rho)
    S2 = S2_obs + S2_unobs
    Y1 = Y1 + rho * (L1 - L2)
    Y2 = Y2 + rho * (S1 - S2)
    Y3 = Y3 + rho * (Z - mask * (D - L2 - S2))
    res_primal = sqrt(norm(L1 - L2, "F")^2 + norm(S1 - S2, 
                                                  "F")^2 + norm(Z - mask * (D - L2 - S2), "F")^2)
    res_dual = rho * sqrt(norm(L2 - L2_old, "F")^2 + norm(S2 - 
                                                            S2_old, "F")^2 + norm(mask * (L2 - L2_old + S2 - 
                                                                                            S2_old), "F")^2)
    if (res_primal > 10 * res_dual) {
      rho = rho * 2
    }
    else if (res_dual > 10 * res_primal) {
      rho = rho/2
    }
    thresh_primal = EPS_ABS * sqrt(3 * n * p) + EPS_REL * 
      max(sqrt(norm(L1, "F")^2 + norm(S1, "F")^2 + norm(Z, 
                                                        "F")^2), sqrt(norm(L2, "F")^2 + norm(S2, "F")^2 + 
                                                                        norm(mask * (L2 + S2), "F")^2), norm(D, "F"))
    thresh_dual = EPS_ABS * sqrt(3 * n * p) + EPS_REL * sqrt(norm(Y1, 
                                                                  "F")^2 + norm(Y2, "F")^2 + norm(Y3, "F")^2)
    final_iter = i
    if (res_primal < thresh_primal && res_dual < thresh_dual) {
      flag_converge = 1
      if (verbose) 
        print(paste0("Converged in ", i, " iterations."))
      break
    }
  }
  L = (L1 + L2)/2
  S = (S1 + S2)/2
  if (flag_converge == 0 & verbose) 
    print("Did not converge.")
  return(list(L = L, S = S, final_iter = final_iter))
}
prox_nuclear <- function(Y,c) {
  
  USV <- svd(Y)
  U <- USV$u
  S <- USV$d
  V <- USV$v
  
  S_new <- sign(S) * pmax(abs(S) - c, 0)
  X <- U %*% diag(S_new) %*% t(V)
  nuclearX  <- sum(abs(S_new))
  
  return(list(X = X, nuclearX = nuclearX))
}

#' Prox L1
#'
#' \code{prox_l1} implements the proximal gradient method for the L1 norm.
#' This soft thresholding encourages the \code{S} matrix to be sparse.
#' The proximal gradient method is used to solve non-differentiable convex optimization problems.
#' This is used in \code{stablePCP} and \code{rootPCP}.
#'
#' @param Y The \code{S} matrix.
#' @param c The amount by which the prox L1 method penalizes \code{Y}.
#'
#' @return The thresholded \code{S} matrix.
#'
prox_l1 <- function(Y, c) {
  X <- sign(Y) * pmax(abs(Y) - c, 0)
  return(X)
}

#' Prox Frobenius
#'
#' \code{prox_fro} implements the proximal gradient method for the Frobenius norm.
#' This thresholding minimizes the square root of the sum of squared error.
#' The proximal gradient method is used to solve non-differentiable convex optimization problems.
#' This is only used in \code{rootPCP} because the squared Frobenius error in \code{stablePCP} is differentiable.
#'
#' @param X The error matrix, \code{D-L-S}
#' @param c The amount by which the prox Frobenius method penalizes \code{X}.
#'
#' @return The thresholded error matrix.
#'
prox_fro <- function(X,c) {
  
  n = norm(X,'F')
  
  if (n <= c) {Y = matrix(0, nrow = nrow(X), ncol = ncol(X))}
  else {Y = (1 - c/n) * X}
  
  return(Y)
}

#' Loss for stablePCP-LOD
#'
#' \code{loss_lod} includes the LOD-specific penalty terms to compute the loss on the squared error term \code{L+S-D}.
#'
#' @param X The predicted value of \code{L + S} at the current iteration.
#' @param D The original dataset.
#' @param LOD The LOD. May be a scalar, vector (\code{length(LOD) = ncol(D)}), or matrix (\code{dim(LOD) == dim(D)}).
#'
#' @return Scalar value used to calculate loss in objective function.
#'
loss_lod <- function(X, D, LOD) {
  X_lod <- (X - D)   * (D >= 0) +
    (X - LOD)  * ((D < 0) & (X > LOD)) +
    X        * ((D < 0) & (X < 0))
  
  sum(X_lod^2) / 2
}

#' Project rank
#' Non-convex replacement for nuclear norm
#'
proj_rank_r = function(Y, r) {
  
  if (ncol(Y) < r) stop("r > matrix rank")
  if (ncol(Y) == r) {return(Y)}
  
  USV <- svd(Y)
  U <- USV$u
  S <- USV$d
  V <- USV$v
  
  s = S
  
  # used to read:
  #if (length(S)-1 == r) {
  #  s[r]  = 0
  #} else {
  #  s[(r+1):length(s)] = 0
  #}
  # now reads:
  s[(r+1):length(s)] = 0
  
  S_new  = diag(s)
  X = U %*% S_new %*% t(V)
  return(X)
}

#' HT
#' Hard-thresholding for the sparse matrix
#'
HT = function(Y, c) {
  X = Y
  X[abs(X) < c] = 0
  return(X)
}

