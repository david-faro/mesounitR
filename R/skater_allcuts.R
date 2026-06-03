#' Hierarchical SKATER clustering with complete partition history
#'
#' Extends the SKATER (Spatial 'K'luster Analysis by Tree Edge Removal)
#' algorithm from the spdep package by recording all admissible partitions generated during
#' recursive pruning of a minimum spanning tree. In contrast to
#' \code{spdep::skater()}, which returns a single partition satisfying
#' the specified pruning criteria, this implementation stores the complete
#' hierarchy of admissible partitions generated throughout the pruning process.
#'
#' The resulting object contains a \code{groups.history} matrix in which
#' each column represents a partition corresponding to a successive split
#' of the spanning tree. This enables reconstruction and analysis of the
#' full clustering hierarchy.
#'
#' @details
#' This function is a modified derivative of \code{spdep::skater()} from
#' the \pkg{spdep} package. Portions of the original implementation are
#' retained under the terms of the GNU General Public License.
#'
#' Original function:
#' \code{spdep::skater()}
#'
#' Original authors:
#' Roger Bivand and contributors to the \pkg{spdep} package.
#'
#' Modifications in this implementation include:
#' \itemize{
#'   \item iterative generation of all admissible tree partitions;
#'   \item storage of intermediate partitions in \code{groups.history};
#'   \item return of the complete clustering hierarchy rather than only a selected partition.
#' }
#'
#' Original code copyright:
#' Roger Bivand and contributors to \pkg{spdep}.
#'
#' Modifications copyright:
#' Copyright (C) 2026 Your Name.
#'
#' @param edges adjacency structure or skater object
#' @param data data matrix used for distance calculations
#' @param crit optional split criterion thresholds
#' @param vec.crit optional vector criterion per observation
#' @param method distance method used in spdep functions
#' @param p Minkowski power parameter
#' @param cov covariance matrix for mahalanobis distance
#' @param inverted logical, whether distances are inverted
#'
#' @return
#' An object of class \code{"skater"}.
#' In addition to the components returned by \code{spdep::skater()},
#' the object includes \code{groups.history}, an integer matrix recording
#' cluster membership across all generated partitions during recursive pruning.
#'
#' @references
#' Assunção, R. M., Neves, M. C., Câmara, G., & Da Costa Freitas, C.
#' (2006). Efficient regionalization techniques for socio-economic
#' geographical units using minimum spanning trees. International Journal
#' of Geographical Information Science, 20(7), 797-811.
#'
#' Bivand, R., Pebesma, E., & Gómez-Rubio, V. (2013). Applied Spatial
#' Data Analysis with R. Springer.
#'
#' @seealso
#' \code{\link[spdep]{skater}}
#'
#' @export
skater_allcuts <- function(edges, data, crit, vec.crit,
                           method = c("euclidean", "maximum",
                                      "manhattan", "canberra",
                                      "binary", "minkowski",
                                      "mahalanobis"),
                           p = 2, cov, inverted = FALSE) {

  if (inherits(edges, "skater")) {

    res <- edges
    n <- length(res$groups)

  } else {

    n <- nrow(edges) + 1

    res <- list(
      groups = rep(1, n),
      edges.groups = list(
        list(
          node = 1:n,
          edge = edges
        )
      ),
      not.prune = NULL,
      candidates = 1,
      ssto = spdep::ssw(data, 1:n, method, p, cov, inverted)
    )

    res$ssw <- res$edges.groups[[1]]$ssw <- res$ssto

    tmp <- sort(
      spdep::prunecost(
        res$edges.groups[[1]]$edge[, 1:2, drop = FALSE],
        data, method, p, cov, inverted
      ),
      decreasing = TRUE,
      method = "quick",
      index.return = TRUE
    )

    res$edges.groups[[1]]$edge <-
      cbind(
        res$edges.groups[[1]]$edge[tmp$ix, ],
        tmp$x
      )

    if (missing(crit))
      res$crit <- c(1, Inf)
    else
      res$crit <- crit

    if (missing(vec.crit))
      res$vec.crit <- rep(1, n)
    else
      res$vec.crit <- vec.crit
  }

  if (is.null(res$vec.crit))
    res$vec.crit <- rep(1, n)

  if (is.null(res$crit))
    res$crit <- c(1, Inf)

  if (length(res$crit) == 1)
    res$crit <- c(res$crit, Inf)

  res$candidates <- setdiff(
    seq_along(res$edges.groups),
    res$not.prune
  )

  ## --------------------------------------------------
  ## helper: obtain membership vector
  ## --------------------------------------------------

  get_groups <- function(edges.groups, n) {

    g <- integer(n)

    for (ii in seq_along(edges.groups))
      g[edges.groups[[ii]]$node] <- ii

    g
  }

  ## initial partition (1 group)

  groups.history <- matrix(
    get_groups(res$edges.groups, n),
    ncol = 1
  )

  colnames(groups.history) <- "k1"

  ## --------------------------------------------------
  ## build full hierarchy
  ## --------------------------------------------------

  repeat {

    if (length(res$candidates) == 0)
      break

    l.costs.ord <- lapply(
      res$edges.groups[res$candidates],
      function(x) x$edge[, 3]
    )

    if (sum(lengths(l.costs.ord)) == 0)
      break

    t.id <- rep(
      res$candidates,
      sapply(l.costs.ord, length)
    )

    t.cost <- unlist(l.costs.ord)

    t.idi <- unlist(
      lapply(l.costs.ord, function(x) {
        if (length(x) > 0)
          seq_along(x)
        else
          NULL
      })
    )

    dc <- cbind(t.id, t.cost, t.idi)

    dc <- dc[
      sort(
        dc[, 2],
        decreasing = TRUE,
        method = "quick",
        index.return = TRUE
      )$ix,
      ,
      drop = FALSE
    ]

    k <- 1
    split.found <- FALSE

    repeat {

      if (k > nrow(dc))
        break

      toprun <- rbind(
        res$edges.groups[[dc[k, 1]]]$edge[
          dc[k, 3], 1:2, drop = FALSE
        ],
        res$edges.groups[[dc[k, 1]]]$edge[
          -dc[k, 3], 1:2, drop = FALSE
        ]
      )

      g.pruned <- spdep::prunemst(
        toprun,
        only.nodes = FALSE
      )

      scrit <- sapply(
        g.pruned,
        function(x)
          sum(res$vec.crit[x$node])
      )

      cond <- any(
        findInterval(
          scrit,
          res$crit,
          rightmost.closed = TRUE
        ) != 1
      )

      if (cond) {

        id.not <- !is.element(
          res$candidates,
          unique(dc[-(1:k), 1])
        )

        res$not.prune <- unique(
          c(
            res$not.prune,
            res$candidates[id.not]
          )
        )

        res$candidates <- setdiff(
          seq_along(res$edges.groups),
          res$not.prune
        )

        k <- k + 1

      } else {

        gc.pruned <- lapply(
          g.pruned,
          function(e) {

            if (nrow(e$edge) == 0) {

              list(
                node = e$node,
                edge = matrix(0, 0, 3),
                ssw = spdep::ssw(
                  data,
                  e$node,
                  method,
                  p,
                  cov,
                  inverted
                )
              )

            } else {

              tmp <- sort(
                spdep::prunecost(
                  e$edge[, 1:2, drop = FALSE],
                  data,
                  method,
                  p,
                  cov,
                  inverted
                ),
                decreasing = TRUE,
                method = "quick",
                index.return = TRUE
              )

              list(
                node = e$node,
                edge = cbind(
                  e$edge[tmp$ix, , drop = FALSE],
                  tmp$x
                ),
                ssw = spdep::ssw(
                  data,
                  e$node,
                  method,
                  p,
                  cov,
                  inverted
                )
              )
            }
          }
        )

        old.group <- dc[k, 1]

        res$edges.groups[[old.group]] <- gc.pruned[[1]]

        new.group <- length(res$edges.groups) + 1

        res$edges.groups[[new.group]] <- gc.pruned[[2]]

        res$ssw <- c(
          res$ssw,
          sum(
            sapply(
              res$edges.groups,
              function(e) sum(e$ssw)
            )
          )
        )

        res$candidates <- setdiff(
          seq_along(res$edges.groups),
          res$not.prune
        )

        groups.history <- cbind(
          groups.history,
          get_groups(res$edges.groups, n)
        )

        colnames(groups.history)[ncol(groups.history)] <-
          paste0("k", length(res$edges.groups))

        split.found <- TRUE

        break
      }
    }

    if (!split.found)
      break
  }

  ## final partition

  final.groups <- get_groups(
    res$edges.groups,
    n
  )

  res$groups <- final.groups

  ## complete hierarchy

  res$groups.history <- groups.history

  attr(res, "class") <- "skater"

  res
}
