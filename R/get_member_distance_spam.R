#' @export
get_member_distance_spam <- function(E, member, from, max_distance = 3, chunksize = 1e2, file = "member_distance.csv"){
  n_f <- length(from)
  # reshuffle from
  n_chunks <- ceiling(n_f/chunksize)
  chunk_id <- sample(n_chunks, size = n_f, replace = TRUE)
  chunks <- split(from, chunk_id)
  res <- list()
  for (i in seq_along(chunks)){
    f <- chunks[[i]]
    perc <- (100*i/n_chunks) |> round(1)
    l <- rcpp_member_distance2(E, member = member, from = f, max_d = max_distance)

    dimnames(l$n_nodes) <- list(d = paste0("d", seq_len(max_d), "_nodes"), from = NULL)
    dimnames(l$n_members) <- list(d = paste0("d", seq_len(max_d), "_members"), from = NULL)

    d <-
      rbind(l$n_nodes, l$n_members) |>
      t() |>
      as.data.frame()

    d$from <- f
    cat("\r chunk ",i,"/", n_chunks, "...(",perc,"%)", sep="")
    write.table(d
               , file = file
               , append = (i > 1)
               , sep = ","
               , row.names = FALSE
               , col.names = (i == 1)
               )
  }
  message("\nFinished")
  read.csv(file)
}
