#' Read Website
#'
#' Connects to marketingphdjobs.org and loads the html into
#' an R session
read_website <- function() {
    url <- "http://marketingphdjobs.org/"
    web <- xml2::read_html(url)

    return(web)
}

#' Get Text From HTML Table
#'
#' Extracts the text from the HTML elements of the a table
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
#' Cleans up the text output of the HTML table imported from
#' marketingphdjobs.org
#'
#' End result is a tidy data frame
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
        dplyr::mutate_if(is.character, stringr::str_trim, side = "both") %>%
        dplyr::mutate(posted_on = lubridate::parse_date_time(posted_on, "b! d, y"),
                      apply_by  = lubridate::parse_date_time(apply_by, "b! d, y", quiet = TRUE)
                        )

    return(df)
}

#' Get Job Listings from marketingphdjobs.org
#'
#' Extract job listings from the website marketingphdjobs.org
#'
#' @return Returns an object of class "data.frame"
#'
#' @export
#'
#' @examples
#' job_data <- get_job_listings()
get_job_listings <- function(){
    jobs <- read_website() %>%
            extract_text() %>%
            format_jobs()

    return(jobs)
}

#' Find new jobs based on a date criterion
#'
#' @param date_lower A date from which to start the search
#' @param date_upper A date from which the search ends
#'
#' @return A data.frame of jobs posted after date_lower and before date_upper
#'
#' @export
#'
#' @examples
#' ## between two set dates
#' date_start <- lubridate::ymd("2019/06/18")
#' date_end   <- lubridate::ymd("2019/06/20")
#' new_jobs <- get_new_jobs(date_start, date_end)
#'
#' ## all jobs from a given date until today
#' new_jobs2 <- get_new_jobs(date_lower = date_start)
get_new_jobs <- function(date_lower, date_upper = NULL){

    if(is.null(date_upper)){
        date_upper <- lubridate::today()
    }

    df <- get_job_listings() %>%
            dplyr::filter(dplyr::between(lubridate::ymd(posted_on),
                                         lubridate::ymd(date_lower),
                                         lubridate::ymd(date_upper)
            )
                          )
    return(df)
}

#' Extract jobs that will have AMA Interviews
#'
#' @param jobs_data A data.frame of jobs from which to filter
#'
#' @return A data.frame of jobs that will have AMA interviews
#'
#' @export
#'
#' @examples
#' jobs_data <- get_job_listings()
#' ama <- ama_interviews(jobs_data)
ama_interviews <- function(jobs_data){
    jobs_ama <- jobs_data %>%
                dplyr::filter(stringr::str_detect(stringr::str_to_lower(ama_interview), "yes"))
}

#' Extract jobs of a certain position
#'
#' @param jobs_data A data.frame of jobs from which to filter
#' @param job_level A character string containing the job level to locate
#'
#' @return A data.frame of jobs of a certain level
#'
#' @export
#'
#' @examples
#' jobs_data <- get_job_listings()
#' assist_prof <- job_type(jobs_data, "assistant")
#' ## equivalent to
#' assist_prof2 <- job_type(jobs_data, "Assistant")
job_type <- function(jobs_data, job_level){
    jobs <- jobs_data %>%
            dplyr::filter(stringr::str_detect(stringr::str_to_lower(job_title),
                                              stringr::str_to_lower(job_level)
                        )
                          )
}

#' Extract job listings for a certain country
#'
#' @param jobs_data A data.frame of jobs from which to filter
#' @param job_level A character string containing the country of interest
#'
#' @return A data.frame of job postings in a certain country
#'
#' @export
#'
#' @examples
#' df <- get_job_listings()
#' us_jobs <- jobs_in_country(df, "united states")
#' ## Multiple countries
#' selected_jobs <- jobs_in_country(df, "united states|canada")
jobs_in_country <- function(jobs_data, job_location){
    jobs <- jobs_data %>%
                dplyr::filter(stringr::str_detect(stringr::str_to_lower(country),
                                          stringr::str_to_lower(job_location)
                                )
                            )
}
