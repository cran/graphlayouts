test_that("it works on directed graph with 2 components, in the presence of weights", {
    adj <- matrix(
        c(
            0, 2, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 3, 1,
            0, 0, 1, 0, 1,
            0, 0, 0, 0, 0
        ),
        nrow = 5
    )
    g <- igraph::graph_from_adjacency_matrix(adj, weighted = TRUE)
    expect_silent(
        r <- layout_with_stress(g, weights = igraph::E(g)$weight)
    )
    expect_is(r, "matrix")
})


test_that("it works on directed connected graph", {
    g <- igraph::make_graph(~ a - +b + -+c - +d:e:f)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})



test_that("it works on undirected connected graph", {
    g <- igraph::make_graph(~ a - -b - -c - -d:e:f)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on undirected connected weighted graph", {
    g <- igraph::make_graph(~ a - -b - -c - -d:e:f)
    igraph::E(g)$weight <- c(1, 2, 3, 4, 5)
    expect_silent(
        r <- layout_with_stress(g, weights = NULL)
    )
    expect_is(r, "matrix")
})

test_that("it works on an isolates", {
    g <- igraph::make_graph(~a)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on a graph of 5 isolates", {
    g <- igraph::make_graph(~a, b, c, d, e)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on an undirected graph of two connected dyads", {
    g <- igraph::make_graph(~ a - -b, c - -d)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})


test_that("it works on an undirected graph of two connected dyads with 5 isolates", {
    g <- igraph::make_graph(~ a - -b, c - -d, e, f, g, h, i)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})


context("Test layout_with_stress3D() on connected graphs")

test_that("that works on directed graph with 2 components, in the presence of weights", {
    adj <- matrix(
        c(
            0, 2, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 3, 1,
            0, 0, 1, 0, 1,
            0, 0, 0, 0, 0
        ),
        nrow = 5
    )
    g <- igraph::graph_from_adjacency_matrix(adj, weighted = TRUE)
    expect_silent(
        r <- layout_with_stress3D(g, weights = igraph::E(g)$weight)
    )
    expect_is(r, "matrix")
})

test_that("it works on directed connected graph", {
    g <- igraph::make_graph(~ a - +b + -+c - +d:e:f)
    expect_silent(
        r <- layout_with_stress3D(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on undirected connected weighted graph", {
    g <- igraph::make_graph(~ a - -b - -c - -d:e:f)
    igraph::E(g)$weight <- c(1, 2, 3, 4, 5)
    expect_silent(
        r <- layout_with_stress3D(g, weights = NULL)
    )
    expect_is(r, "matrix")
})

test_that("it works on undirected connected graph", {
    g <- igraph::make_graph(~ a - -b - -c - -d:e:f)
    expect_silent(
        r <- layout_with_stress3D(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on undirected disconnected graph", {
    g <- igraph::graph.full(5) + igraph::graph.full(5)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})

context("layout_with_stress3D() works on disconnected graphs")

test_that("it works on an isolate", {
    g <- igraph::make_graph(~a)
    expect_silent(
        r <- layout_with_stress(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on a graph of 5 isolates", {
    g <- igraph::make_graph(~a, b, c, d, e)
    expect_silent(
        r <- layout_with_stress3D(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on an undirected graph of two connected dyads", {
    g <- igraph::make_graph(~ a - -b, c - -d)
    expect_silent(
        r <- layout_with_stress3D(g)
    )
    expect_is(r, "matrix")
})


test_that("it works on an undirected graph of two connected dyads with 5 isolates", {
    g <- igraph::make_graph(~ a - -b, c - -d, e, f, g, h, i)
    expect_silent(
        r <- layout_with_stress3D(g)
    )
    expect_is(r, "matrix")
})

test_that("it works on undirected disconnected graph", {
    g <- igraph::graph.full(5) + igraph::graph.full(5)
    expect_silent(
        r <- layout_with_stress3D(g)
    )
    expect_is(r, "matrix")
})

context("Test layout_with_focus() on connected graphs")

test_that("it works on undirected connected graphs", {
    g <- igraph::make_star(5, mode = "undirected", center = 1)
    expect_error(layout_with_focus(g))
    expect_silent(
        r <- layout_with_focus(g, v = 1)$xy
    )
    expect_is(r, "matrix")
})


test_that("it fails for disconnected graphs", {
    expect_error(layout_with_focus(igraph::graph.empty(n = 10, directed = FALSE)))
})

context("Test layout_with_centrality() on connected graphs")

test_that("it works on undirected connected graphs", {
    g <- igraph::make_star(5, mode = "undirected", center = 1)
    expect_error(layout_with_centrality(g))
    expect_silent(
        r <- layout_with_centrality(g, cent = igraph::degree(g))
    )
    expect_is(r, "matrix")
})


test_that("it fails for disconnected graphs", {
    expect_error(layout_with_centrality(igraph::graph.empty(n = 10, directed = FALSE)))
})

context("Test layout_with_constrained_stress()")


test_that("it works on undirected connected graph", {
    g <- igraph::make_graph(~ a - -b - -c - -d:e:f)
    expect_error(layout_with_constrained_stress(g))
    expect_silent(
        r <- layout_with_constrained_stress(g, coord = rep(1, 6))
    )
    expect_is(r, "matrix")
})

test_that("it works on undirected disconnected graph", {
    g <- igraph::make_full_graph(5) + igraph::make_full_graph(5)
    fix <- rep(c(1, 2), each = 5)
    expect_silent(
        r <- layout_with_constrained_stress(g, coord = fix)
    )
    expect_is(r, "matrix")
    expect_true(all(r[, 1] == fix))
})


context("Test layout_with_constrained_stress3D() on connected graphs")
test_that("it works on undirected connected graph", {
    g <- igraph::make_graph(~ a - -b - -c - -d:e:f)
    expect_error(layout_with_constrained_stress3D(g))
    expect_silent(
        r <- layout_with_constrained_stress3D(g, coord = rep(1, 6))
    )
    expect_is(r, "matrix")
})

test_that("it works on undirected disconnected graph", {
    g <- igraph::make_full_graph(5) + igraph::make_full_graph(5)
    fix <- rep(c(1, 2), each = 5)
    expect_silent(
        r <- layout_with_constrained_stress3D(g, coord = fix)
    )
    expect_is(r, "matrix")
    expect_true(all(r[, 1] == fix))
})

context("Test layout_with_*_grouped()")
test_that("grouped layouts work", {
    g <- igraph::graph.full(10)
    grp <- rep(c(1, 2), each = 5)
    expect_is(layout_with_focus_group(g, v = 1, grp), "matrix")

    g <- igraph::graph.star(10)
    grp <- rep(c(1, 2), each = 5)
    expect_is(layout_with_centrality_group(g, cent = igraph::degree(g), grp), "matrix")
})

test_that("test errors in stress layouts", {
    expect_error(layout_with_stress(5))
    expect_error(layout_with_constrained_stress(5))
    expect_error(layout_with_stress3D(5))
    expect_error(layout_with_focus(5))

    expect_error(layout_with_focus(igraph::graph.full(5)))
    expect_error(layout_with_centrality(igraph::graph.full(5)))

    expect_error(layout_with_constrained_stress(igraph::graph.full(5), fixdim = "z"))
    expect_error(layout_with_constrained_stress(igraph::graph.full(5), fixdim = "x"))

    expect_error(layout_with_focus_group(igraph::graph.full(5)))
    expect_error(layout_with_centrality_group(igraph::graph.full(5)))
    expect_error(layout_with_focus_group(igraph::graph.full(5), v = 2))
    expect_error(layout_with_centrality_group(igraph::graph.full(5), cent = igraph::degree(g)))
})

test_that("layout_with_fixed_nodes works", {
    set.seed(12)
    g <- sample_bipartite(10, 5, "gnp", 0.5)
    fxy <- cbind(c(rep(0, 10), rep(1, 5)), NA)
    expect_silent(
        r <- layout_with_fixed_coords(g, fxy)
    )
    expect_is(r, "matrix")
    expect_error(layout_with_fixed_coords(g))
    expect_warning(layout_with_fixed_coords(g, matrix(1, 15, 2)))
    expect_error(layout_with_fixed_coords(g, matrix(1, 12, 2)))
})
