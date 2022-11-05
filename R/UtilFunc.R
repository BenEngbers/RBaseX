addVoid <- function(input) {
  if (inherits(input, "character"))  {
    input %<>% charToRaw()
  } else {
    if (length(input) == 0) input <- raw(0)
  }
  input <- append(input, 0x00) %>% as.raw()  # Append \0x00 -byte
  return(input)
}

is.Binary <- function(rawVector) {
  checked <- rawVector[! rawVector %in% as.raw(seq(128,255))]
  isBin <- identical(checked, rawVector)
  return(! isBin)
}

SplitRaw <- function(rawVector, splitByte) {
  splitter <- as.raw(splitByte)
  splitPos <- which(splitter == rawVector)
  if (tail(rawVector,1 ) != splitter) splitPos <- c(splitPos, length(rawVector))
  begin <- head(c(1, splitPos+1),-1)
  end <- splitPos
  splitted <- mapply(function(sta, sto, vec) {as.raw(vec[sta:sto])},
                     begin, end, MoreArgs = list(rawVector),
                     SIMPLIFY = FALSE)
  return(splitted)
}

split_Response <- function(response) {
  error <- (length(which(response == as.raw(c("01")))) > 0)
  if (error) {            # An error occurred, indicated by a trailing 01-byte in response
    resp_list <- responseToChar(response)
  } else {
    FF <- which(response == 255)
    if (length(FF) > 0) {
      resp_list <- list()
      Zero  <- which(0 == response)
      Z_i <- 0
      for (i in 1:length(Zero)) {
        e <- Zero[i]
        F_cnt <- 0; F_i <- e - 1              # FF0 is treated different from F0
        while (response[F_i] == 0xFF) {F_cnt <- F_cnt + 1; F_i <- F_i - 1}
        if (F_cnt %% 2 == 0) break
      }
      resp_list[[1]] <- strip_FF(response[1:(e -1)])
      resp_list[[2]] <- response[(e+1):(length(response) -2)] %>% rawToChar()
      resp_list[[3]] <- TRUE
    } else {                                  # tail is \00
      resp_list <- responseToChar(response)
    }
  }
  return(resp_list)
}

responseToChar <- function(response) {
  if (is.PLATFORM("Windows")) response %<>% strip_CR()
  resp_list <- SplitRaw(response, c("00"))
  for (i in 1:(length(resp_list) -1)) resp_list[[i]] <- rawToChar(resp_list[[i]])
  resp_list[[length(resp_list)]] <- eval_status_byte(tail(response, 1))
  return(resp_list)
}
clean_Response <- function(response) {
  response_Clean <-  charToRaw(response) %>% strip_LF()
  if (is.PLATFORM("Windows")) response_Clean %<>% strip_CR()
  response_Clean  %<>% rawToChar()
  return(response_Clean)
}

eval_status_byte = function(Byte) {
  if (Byte == 0x00) success <- TRUE
  else if (Byte == 0x01) success <- FALSE
  return(success)
}

strip_CR <- function(cache_in) {
  CR <- which(13 == cache_in); if (length(CR) >0) cache_in <- cache_in[-CR]
  return(cache_in)
}
strip_LF <- function(cache_in) {
  LF <- which(10 == cache_in); if (length(LF) >0) cache_in <- cache_in[-LF]
  return(cache_in)
}
strip_FF <- function(cache_in) {
  FF <- which(255 == cache_in)
  if (length(FF) > 0) {
    remo <- which(cache_in == 0)
    if (length(remo) > 0) {                         # Remove FF that precedes 0
      remo <- remo -1
      removed <- cache_in[-remo]
    } else removed <- cache_in

    remo <- which(255 == removed)
    if (length(remo)) {                             # Remove FF that precedes FF
      n <- length(remo)/2
      odds <- remo[c(2*(1:n) -1)]
      removed <- removed[-odds]
    }
    return(removed)
  } else {return(cache_in)}
}
add_FF <- function(cache_in) {
  FF <- which(255 == cache_in)
  Z  <- which(0 == cache_in)

  addFF <- c(FF, Z)
  if (length(addFF) > 0) {
    val <- c(cache_in, rep(as.raw(255), length(addFF)))
    id  <- c(seq_along(cache_in), addFF-0.5)
    val <- val[order(id)]
    return(val)
  } else
    return(cache_in)
}

is.PLATFORM <- function(OS) {
  platform <- Sys.info()['sysname']
  return(platform == OS)
}

is.VALID <- function(input) {
  pi <- pingr::is_online()
  va <- grepl(re, input)
  ret <- isTRUE(pi && va)
  return(ret)
}

valid_chars <- rex::rex(except_some_of(".", "/", " ", "-"))

re <- rex::rex(
  start,
  # protocol identifier (optional) + //
  group(list("http", maybe("s")) %or% "ftp", "://"),
  # user:pass authentication (optional)
  maybe(non_spaces,
        maybe(":", zero_or_more(non_space)),
        "@"),
  #host name
  group(zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
  #domain name
  zero_or_more(".", zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),
  #TLD identifier
  group(".", valid_chars %>% at_least(2)),
  # server port number (optional)
  maybe(":", digit %>% between(2, 5)),
  # resource path (optional)
  maybe("/", non_space %>% zero_or_more()),
  end
)

