get_member_distance3_chunks <- function(E, member, from_nodes, max_d = 3L, chunksize = 1e2){
  # N <- length(from_nodes)
  # begin <- seq(1, N, by = chunksize)
  # end <- (begin - 1)[-1]
  # end <- c(end, N)
  #
  # list(
  #   begin = begin,
  #   end = end
  # )
  i <- seq_along(from_nodes)
  from_i <- split(from_nodes, ceiling(i/chunksize)) |> unname()
  list(
    from_i = from_i,
    i = i
  )
}

get_member_distance3_chunks(E = NULL, member=NULL, 11:22, chunksize = 4)
