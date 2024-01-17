## ---- include=FALSE-----------------------------------------------------------

# There are notes about data.table using too many cores when you try to submit a table with data.table-dependencies
# to cran. This is supposed to prevent that note. Also included in tests. 
data.table::setDTthreads(1)

library(pvda)
knitr::opts_chunk$set(echo = FALSE)

## -----------------------------------------------------------------------------
da_1 <- 
  drug_event_df |>
  da()

## -----------------------------------------------------------------------------
da_1 <-
  drug_event_df |> 
  dplyr::rename(reportID = report_id) |> 
  da(df_colnames = list(report_id = "reportID"))

## ---- echo = TRUE, results = FALSE, message = FALSE---------------------------
summary(da_1)

## ---- echo = TRUE, results = FALSE, message = FALSE---------------------------
print(da_1, n=3)

## -----------------------------------------------------------------------------
drug_event_df[1:6, ]

## -----------------------------------------------------------------------------
names(da_1)

# Different ways of extracting the results data frame
# da_1$da_df
# da_1[['da_df']]
# da_1 |> purrr::pluck("da_df")


## -----------------------------------------------------------------------------
drug_event_df |> 
  head()

da_grouped <- 
  drug_event_df |> 
  da(df_colnames = list(group_by="group")) 

print(da_grouped$da_df)


## -----------------------------------------------------------------------------
first_row <- drug_event_df[1,]

first_row |> 
  dplyr::bind_rows(first_row) |> 
  da() |> 
  purrr::pluck("da_df") |> 
  dplyr::pull(obs)


## -----------------------------------------------------------------------------
# Note the fourth column name:
drug_event_df |> 
  da() |> 
  purrr::pluck("da_df") |> 
  dplyr::select(drug:ic97.5) |> 
  print(n=1)


## ---- echo = TRUE, results = FALSE, message = FALSE---------------------------
drug_event_df |>
  da(
    rule_of_N = 3,
    number_of_digits = 8
  ) |> print(n = 3)

## -----------------------------------------------------------------------------
prr(obs = 10, n_drug = 1000, n_event_prr = 200, n_tot_prr = 10000)
ror(a = 10, b = 20, c = 200, d = 10000)
ic(obs = 10, exp = 5)

## ---- include=FALSE-----------------------------------------------------------
# plot_udfs_as_graph <- function(package_name = "pvda"){

# library(pvda)
# package_name <- "pvda"
# ns_names <- getNamespaceExports(package_name)
# ns_names_w_id <- dplyr::bind_cols("id" = seq_along(ns_names), "name" = ns_names)
# edge_output <- list()
#
# for (i in seq_along(ns_names)) {
#   # i = 9
#
#   ns_name_i <- ns_names[i]
#   code_function_i <- body(get(ns_name_i)) |> paste0((as.character()), collapse = "")
#
#   # Find first occurrence of prefix calls in code_function_i
#   ns_calls <- stringr::str_c(ns_names, "(")
#   # ns_calls_with_comma <- stringr::str_c("", ns_names, "(")
#   # Avoid "is.numeric" being counted as ic
#   code_function_i <- gsub(x = code_function_i, pattern = "as.numeric", replacement = "", fixed = T)
#   # str_replace didn't manage this.
#
#   first_call_pos <- stringr::str_locate(
#     stringr::fixed(code_function_i),
#     stringr::fixed(c(ns_calls))
#   )[, 1]
#
#   calls <- na.omit(dplyr::bind_rows(
#     "function_id" = ns_names_w_id$id,
#     "function_name" = ns_names_w_id$name,
#     "pos" = first_call_pos
#   )) |>
#     dplyr::arrange(pos) |>
#     dplyr::select(-pos) |>
#     dplyr::filter(!function_name %in% c("custom_colours", "custom_ggtheme", "sign_lvl_to_quantile_prob"))
#
#   edges <- dplyr::bind_cols(i, calls) |>
#     dplyr::select(-function_name)
#   colnames(edges) <- c("from", "to")
#   edges <- edges |>
#     dplyr::filter(from != to)
#   edge_output[[i]] <- edges
# }
#
# edges <- do.call(rbind.data.frame, edge_output)
# edge_df <- DiagrammeR::create_edge_df(edges$from, edges$to)
#
# nodes_with_edges <- c(unique(edges$from), unique(edges$to))
#
# nodes <- dplyr::bind_cols(
#   "id" = 1:length(ns_names),
#   "label" = ns_names
# )[1:length(ns_names) %in% nodes_with_edges, ]
# node_df <- DiagrammeR::create_node_df(length(nodes$id),
#   label = nodes$label,
#   fontsize = 6,
#   shape = "rectangle",
#   fixedsize = FALSE
# )
#
# node_df$id <- nodes$id
#
#
# graph <- DiagrammeR::create_graph(
#   nodes_df = node_df,
#   edges_df = edge_df
# )
#
# suppressWarnings(DiagrammeR::render_graph(graph,
#   layout = "tree",
#   title = "Overview of package functions"
# ))

## ---- echo=FALSE, fig.height = 8, fig.width = 8-------------------------------
# suppressWarnings(DiagrammeR::render_graph(graph,
#   layout = "tree",
#   title = "Overview of package functions"
# ))

