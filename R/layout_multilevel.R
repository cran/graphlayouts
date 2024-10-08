#' multilevel layout
#' @description Layout algorithm to visualize multilevel networks
#' @name layout_multilevel
#' @param g igraph object. Must have a vertex attribute "lvl" which is 1 or 2.
#' @param type one of "all", "separate","fix1" or "fix2". see details
#' @param FUN1 if type="separate", the layout function to be used for level 1
#' @param FUN2 if type="separate", the layout function to be used for level 2
#' @param params1 named list of parameters for FUN1
#' @param params2 named list of parameters for FUN2
#' @param ignore_iso treatment of isolates within levels. see details
#' @param project2D logical. Defaults to TRUE (project to 2D).
#' @param alpha angle for isometric projection between 0 and 90
#' @param beta  angle for isometric projection between 0 and 90
#' @details
#' The algorithm internally computes a 3D layout where each level is in a separate y-plane.
#' The layout is then projected into 2D via an isometric mapping, controlled by the parameters
#' `alpha` and `beta`. It may take some adjusting to `alpha` and `beta` to find a good perspective.
#'
#' If type="all", the layout is computed at once for the complete network.
#' For type="separate", two user specified layout algorithms (`FUN1` and `FUN2`) are used for the levels.
#' The named lists `param1` and `param2` can be used to set parameters for `FUN1` and `FUN2`.
#' This option helpful for situations where different structural features of the levels should be emphasized.
#'
#' For type="fix1" and type="fix2" only one of the level layouts is fixed. The other one is calculated by optimizing the
#' inter level ties, such that they are drawn (almost) vertical.
#'
#' The `ignore_iso` parameter controls the handling of isolates. If TRUE, nodes without inter level edges are ignored during the layout process
#' and added at the end. If FALSE they are left unchanged
#'
#' The layout_igraph_* function should not be used directly. It is only used as an argument for plotting with 'igraph'.
#' @return matrix of xy coordinates
#' @examples
#' library(igraph)
#' data("multilvl_ex")
#'
#' # compute a layout for the whole network
#' xy <- layout_as_multilevel(multilvl_ex, type = "all", alpha = 25, beta = 45)
#'
#' # compute a layout for each level separately and combine them
#' xy <- layout_as_multilevel(multilvl_ex,
#'     type = "separate",
#'     FUN1 = layout_as_backbone,
#'     FUN2 = layout_with_stress,
#'     alpha = 25, beta = 45
#' )
#'
#' @export
layout_as_multilevel <- function(g, type = "all", FUN1, FUN2,
                                 params1 = NULL, params2 = NULL,
                                 ignore_iso = TRUE,
                                 project2D = TRUE,
                                 alpha = 35, beta = 45) {
    type <- match.arg(type, c("all", "separate", "fix1", "fix2"))

    if (!"lvl" %in% igraph::vertex_attr_names(g)) {
        stop("level information should be stored in a vertex attribute called 'lvl'")
    }
    # 3D stress
    if (type == "all") {
        xyz <- layout_with_constrained_stress3D(g, coord = igraph::V(g)$lvl, fixdim = "y")
        xyz <- optim_rotation(g, xyz)
        xyz <- optim_isolates(g, xyz)
        xyz[, c(1, 3)] <- c(normalise(xyz[, 1], to = c(1, 2)), normalise(xyz[, 3], to = c(1, 2)))
        # separate
    } else if (type == "separate") {
        if (missing(FUN1) || missing(FUN2)) {
            stop("FUN1 and FUN2 must both be specified")
        }

        lvl1 <- which(igraph::V(g)$lvl == 1)
        lvl2 <- which(igraph::V(g)$lvl == 2)

        g1 <- igraph::induced_subgraph(g, lvl1)
        g2 <- igraph::induced_subgraph(g, lvl2)
        if (ignore_iso) {
            iso1 <- which(igraph::degree(g1) == 0)
            iso2 <- which(igraph::degree(g2) == 0)
            g1 <- igraph::delete_vertices(g1, iso1)
            g2 <- igraph::delete_vertices(g2, iso2)
        }

        if (is.null(params1)) {
            xy1 <- FUN1(g1)
        } else {
            if (!all(names(params1) %in% names(formals(FUN1)))) {
                stop("params1 contains invalid parameters.")
            }
            formals(FUN1)[names(params1)] <- params1
            xy1 <- FUN1(g1)
        }
        if (typeof(xy1) == "list") {
            xy1 <- xy1$xy
        }

        if (is.null(params2)) {
            xy2 <- FUN2(g2)
        } else {
            if (!all(names(params2) %in% names(formals(FUN2)))) {
                stop("params2 contains invalid parameters.")
            }
            formals(FUN2)[names(params2)] <- params2
            xy2 <- FUN2(g2)
        }
        if (typeof(xy1) == "list") {
            xy2 <- xy2$xy
        }
        xyz <- cbind(0, igraph::V(g)$lvl, 0)
        if (ignore_iso) {
            if (length(iso1) != 0) {
                xy1_tmp <- matrix(0, length(lvl1), 2)
                xy1_tmp[-iso1, ] <- xy1
                xy1 <- xy1_tmp
            }
            if (length(iso2) != 0) {
                xy2_tmp <- matrix(0, length(lvl2), 2)
                xy2_tmp[-iso2, ] <- xy2
                xy2 <- xy2_tmp
            }
        }
        xy1[, 1] <- normalise(xy1[, 1], to = c(1, 2))
        xy1[, 2] <- normalise(xy1[, 2], to = c(1, 2))
        xy2[, 1] <- normalise(xy2[, 1], to = c(1, 2))
        xy2[, 2] <- normalise(xy2[, 2], to = c(1, 2))
        xyz[lvl1, c(1, 3)] <- xy1
        xyz[lvl2, c(1, 3)] <- xy2
        xyz <- optim_rotation(g, xyz)
        xyz <- optim_isolates(g, xyz)
        # fix level 1
    } else if (type == "fix1") {
        if (missing(FUN1)) {
            stop("FUN1 must must be specified")
        }
        lvl1 <- which(igraph::V(g)$lvl == 1)
        lvl2 <- which(igraph::V(g)$lvl == 2)
        g1 <- igraph::induced_subgraph(g, lvl1)

        if (ignore_iso) {
            iso1 <- which(igraph::degree(g1) == 0)
            g1 <- igraph::delete_vertices(g1, iso1)
        }

        if (is.null(params1)) {
            xy1 <- FUN1(g1)
        } else {
            if (!all(names(params1 %in% names(formals(FUN1))))) {
                stop("params1 contains invalid parameters.")
            }
            formals(FUN1)[names(params1)] <- params1
            xy1 <- FUN1(g1)
        }
        if (typeof(xy1) == "list") {
            xy1 <- xy1$xy
        }
        xyz <- cbind(0, igraph::V(g)$lvl, 0)
        if (ignore_iso) {
            if (length(iso1) != 0) {
                xy1_tmp <- matrix(0, length(lvl1), 2)

                mx <- mean(xy1[, 1], na.rm = TRUE)
                my <- mean(xy1[, 2], na.rm = TRUE)
                r <- max(sqrt((xy1[, 1] - mx)^2 + (xy1[, 2] - my)^2))
                isox <- stats::runif(length(iso1), mx - r, mx + r)
                isoy <- sample(c(-1, 1), length(iso1), replace = T) * sqrt(r^2 - (isox - mx)^2) + my
                xy1_tmp[-iso1, ] <- xy1
                xy1_tmp[iso1, ] <- cbind(isox, isoy)
                xy1 <- xy1_tmp
            }
        }
        xy1[, 1] <- normalise(xy1[, 1], to = c(1, 2))
        xy1[, 2] <- normalise(xy1[, 2], to = c(1, 2))
        xy2 <- optim_level(g, 1, xy1)
        xyz[lvl1, c(1, 3)] <- xy1
        xyz[lvl2, c(1, 3)] <- xy2
        # fix level 2
    } else if (type == "fix2") {
        if (missing(FUN2)) {
            stop("FUN2 must must be specified")
        }
        lvl1 <- which(igraph::V(g)$lvl == 1)
        lvl2 <- which(igraph::V(g)$lvl == 2)
        g2 <- igraph::induced_subgraph(g, lvl2)

        if (ignore_iso) {
            iso2 <- which(igraph::degree(g2) == 0)
            g2 <- igraph::delete_vertices(g2, iso2)
        }

        if (is.null(params2)) {
            xy2 <- FUN2(g2)
        } else {
            if (!all(names(params2 %in% names(formals(FUN2))))) {
                stop("params1 contains invalid parameters.")
            }
            formals(FUN2)[names(params2)] <- params2
            xy2 <- FUN2(g2)
        }
        if (typeof(xy2) == "list") {
            xy2 <- xy2$xy
        }
        xyz <- cbind(0, igraph::V(g)$lvl, 0)

        if (ignore_iso) {
            if (length(iso2) != 0) {
                xy2_tmp <- matrix(0, length(lvl2), 2)

                mx <- mean(xy2[, 1], na.rm = TRUE)
                my <- mean(xy2[, 2], na.rm = TRUE)
                r <- max(sqrt((xy2[, 1] - mx)^2 + (xy2[, 2] - my)^2))

                isox <- stats::runif(length(iso2), mx - r, mx + r)
                isoy <- sample(c(-1, 1), length(iso2), replace = T) * sqrt(r^2 - (isox - mx)^2) + my
                xy2_tmp[-iso2, ] <- xy2
                xy2_tmp[iso2, ] <- cbind(isox, isoy)
                xy2 <- xy2_tmp
            }
        }

        xy2[, 1] <- normalise(xy2[, 1], to = c(1, 2))
        xy2[, 2] <- normalise(xy2[, 2], to = c(1, 2))
        xy1 <- optim_level(g, 2, xy2)
        xyz[lvl1, c(1, 3)] <- xy1
        xyz[lvl2, c(1, 3)] <- xy2
    }
    if (project2D) {
        xy <- iso_project(xyz, a = alpha, b = beta)
        return(xy)
    } else {
        return(xyz)
    }
}
