#' Read Website
#'
#' Connects to marketingphdjobs.org and loads the html into
#' an R session
#'
#' You can learn more about package authoring with RStudio at:
read_website <- function() {
    url <- "http://marketingphdjobs.org/"
    web <- xml2::read_html(url)

    return(web)
}

#' Get Text From HTML Table
#'
#'
#'
#'
#'
#'
extract_text <- function(html){
    table_html <- html %>%
        rvest::html_nodes("table") %>%
        rvest::html_nodes("tr") %>%
        rvest::html_text()

    return(table_html)
}

#' Format Jobs Data
#'
#'
#'
#'
#'
#'
format_jobs <- function(html_text){
    df <- html_text %>%
        stringr::str_replace_all("[\r\n]\\s+[\r\n]" , "&") %>%
        stringr::str_replace_all("[\r\n]" , "") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("\\s+", " ") %>%
        readr::read_delim(delim = "&") %>%
        dplyr::mutate_all(dplyr::funs(stringr::str_replace(., "--", "NA")))

    df %<>% janitor::clean_names() %>%
        dplyr::select(-job_ad) %>%
        dplyr::mutate(posted_on = lubridate::parse_date_time(posted_on, "b! d, y"),
                      apply_by  = lubridate::parse_date_time(apply_by, "b! d, y", quiet = TRUE)
                        )

    return(df)
}

#' Get Job Listings from marketingphdjobs.org
#'
#'
#'
#' @export
get_job_listings <- function(){
    jobs <- read_website() %>%
            extract_text() %>%
            format_jobs()

    return(jobs)
}
