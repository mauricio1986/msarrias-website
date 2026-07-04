# Update OpenAlex citation counts and real OpenAlex work links in papers.yml
# Usage from the root of your Quarto site:
#   Rscript scripts/update_openalex_metrics.R

library(yaml)
library(httr2)
library(purrr)
library(stringr)

input_file  <- "data/papers.yml"
output_file <- "data/papers.yml"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalize_doi <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NA_character_)
  x |>
    str_replace("^https?://(dx\\.)?doi\\.org/", "") |>
    str_trim()
}

extract_doi <- function(paper) {
  if (!is.null(paper$doi)) return(normalize_doi(paper$doi))

  links <- paper$links %||% list()
  doi_link <- keep(links, ~ identical(.x$text, "DOI"))
  if (length(doi_link) == 0) return(NA_character_)

  normalize_doi(doi_link[[1]]$url)
}

fetch_openalex_work <- function(doi) {
  if (is.na(doi) || doi == "") return(NULL)

  # OpenAlex accepts external identifiers such as doi:10.xxxx/yyyy.
  # URLencode is needed because DOI strings contain slashes.
  url <- paste0("https://api.openalex.org/works/doi:", URLencode(doi, reserved = TRUE))

  req <- request(url) |>
    req_user_agent("mauricio-sarrias-quarto-site") |>
    req_retry(max_tries = 3)

  tryCatch(
    req_perform(req) |> resp_body_json(simplifyVector = FALSE),
    error = function(e) NULL
  )
}

upsert_link <- function(links, text, url) {
  links <- links %||% list()
  links <- discard(links, ~ identical(.x$text, text))
  if (!is.null(url) && !is.na(url) && url != "") {
    links <- append(links, list(list(text = text, url = url)))
  }
  links
}

papers <- read_yaml(input_file)

papers <- map(papers, function(paper) {
  doi <- extract_doi(paper)
  paper$doi <- doi

  # Remove old broken OpenAlex DOI links, if present.
  paper$links <- discard(paper$links %||% list(), ~ identical(.x$text, "OpenAlex"))

  work <- fetch_openalex_work(doi)

  if (!is.null(work)) {
    paper$metrics <- paper$metrics %||% list()
    paper$metrics$citations_openalex <- work$cited_by_count %||% NA_integer_
    paper$metrics$openalex_id <- work$id %||% NA_character_
    paper$metrics$openalex_url <- work$id %||% NA_character_
    paper$metrics$metrics_year <- as.integer(format(Sys.Date(), "%Y"))

    # This is the real OpenAlex work URL, e.g. https://openalex.org/W...
    paper$links <- upsert_link(paper$links, "OpenAlex", work$id %||% NA_character_)
  }

  paper
})

write_yaml(papers, output_file)
