get_city_data <- function(x, region, date) {
    if (is(x, "nCov2019")) {
        stats <- x[region, ]
    } else {
        stats <- extract_history(x, region, date)
    }
    names(stats)[1] <- 'NAME'
    return(stats)
}



extract_history <- function(x, province, date) {
  if (missing(province)) {
    df <- summary(x)[, c('province','time','cum_confirm')]
  } else {
    df <- x[province, c('city','time','cum_confirm')]
  }  
  
  df <- df[df$time == as.Date(date, "%Y-%m-%d"), c(1,3)]  
  names(df) <- c("name", "confirm")
  return(df)
}  


extract_province <- function(object, i, by) {
  if (i == 'global') {
    res <- cbind(name = object$areaTree[[1]], object$areaTree[[by]])
    return(res)
  } 
  
  d <- object$areaTree[1,"children"][[1]]
  name = d[[1]]
  if (is.character(i)) {
    i <- which(name == i)
  }
  stats <- d[i, "children"][[1]]
  cbind(name=stats$name, stats[[by]])
}

.get_qq_data <- function() {
  # remove the Callback part in URL
  url <- 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5'
  x <- suppressWarnings(readLines(url, encoding="UTF-8"))
  y <- jsonlite::fromJSON(x)
  # get the data
  data = jsonlite::fromJSON(y$data)
  
  # get chinaDaylist info from url2
  url2 <- 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_other'
  x2 <- suppressWarnings(readLines(url2, encoding="UTF-8"))
  y2 = jsonlite::fromJSON(x2, flatten = TRUE)
  y2 = jsonlite::fromJSON(y2$data,flatten = T)
  
  # add chinaDaylist and dailyHistory into data
  data$dailyHistory <- y2$dailyHistory
  data$chinaDayList <- y2$chinaDayList
  data$chinaDayAddList <- y2$chinaDayAddList
  return(data)  
}

which_lang <- function(lang) {
  lang <- match.arg(lang, c("auto","zh", "en"))
  if (lang == "auto") {
    locale <- Sys.getlocale('LC_CTYPE')
    locale <- sub("^(\\w+)\\W.*", "\\1", locale)
    if (tolower(locale) %in% c("chinese", "zh")) {
      lang <- 'zh'
    } else {
      lang <- 'en'
    }   
  }
  return(lang)
}
