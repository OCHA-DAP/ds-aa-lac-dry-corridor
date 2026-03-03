#' Listmonk API utilities
#'
#' R equivalent of the Python listmonk.py used in ds-aa-afg-drought.
#' Provides transactional and campaign email sending via the Listmonk API.

box::use(
  httr2[...],
  glue[...],
  logger
)

BASE_URL <- "https://listmonk-demo-afhcg8e2hde0fxca.eastus2-01.azurewebsites.net/api"

BASE_CAMPAIGN_ID <- 8
BASE_TRANSACTIONAL_ID <- 12


#' Send a transactional email via Listmonk
#'
#' @param to_emails list of lists with `name` and `email` fields,
#'   e.g. `list(list(name = "Alice", email = "alice@example.com"))`
#' @param subject character email subject line
#' @param template_id integer listmonk transactional template ID
#' @param cc_emails optional, same format as to_emails
#' @param data named list of template data (e.g. `list(content = "<html>...")`)
#' @param username API username (default: from env var)
#' @param password API key (default: from env var)
#' @return parsed JSON response
#' @export
send_transactional <- function(
    to_emails,
    subject,
    template_id = BASE_TRANSACTIONAL_ID,
    cc_emails = NULL,
    data = list(),
    username = Sys.getenv("DSCI_LISTMONK_API_USERNAME"),
    password = Sys.getenv("DSCI_LISTMONK_API_KEY")
) {
  to_formatted <- paste(
    vapply(to_emails, \(x) glue("{x$name} <{x$email}>"), character(1)),
    collapse = ", "
  )

  cc_formatted <- ""
  if (!is.null(cc_emails) && length(cc_emails) > 0) {
    cc_formatted <- paste(
      vapply(cc_emails, \(x) glue("{x$name} <{x$email}>"), character(1)),
      collapse = ", "
    )
  }

  payload <- list(
    subscriber_email = to_emails[[1]]$email,
    template_id = template_id,
    from_email = "OCHA Data Science <ocha-datascience@un.org>",
    content_type = "html",
    subject = subject,
    data = data,
    headers = list(
      list(To = to_formatted),
      list(Cc = cc_formatted)
    )
  )

  logger$log_info(glue("Sending transactional email to: {to_formatted}"))

  resp <- request(glue("{BASE_URL}/tx")) |>
    req_auth_basic(username, password) |>
    req_body_json(payload) |>
    req_error(body = \(resp) resp_body_string(resp)) |>
    req_perform()

  result <- resp_body_json(resp)
  logger$log_info("Transactional email sent successfully")
  result
}


#' Create a campaign in Listmonk
#'
#' @param name campaign name
#' @param subject email subject
#' @param list_ids integer vector of subscriber list IDs
#' @param template_id integer listmonk template ID
#' @param body HTML body content
#' @param username API username
#' @param password API key
#' @return integer campaign ID
#' @export
create_campaign <- function(
    name = "test_campaign",
    subject = "Test Subject",
    list_ids = list(),
    template_id = BASE_CAMPAIGN_ID,
    body = "TEST CONTENT",
    username = Sys.getenv("DSCI_LISTMONK_API_USERNAME"),
    password = Sys.getenv("DSCI_LISTMONK_API_KEY")
) {
  payload <- list(
    name = name,
    subject = subject,
    lists = list_ids,
    template_id = template_id,
    type = "regular",
    content_type = "html",
    body = body
  )

  resp <- request(glue("{BASE_URL}/campaigns")) |>
    req_auth_basic(username, password) |>
    req_body_json(payload) |>
    req_error(body = \(resp) resp_body_string(resp)) |>
    req_perform()

  campaign <- resp_body_json(resp)
  campaign_id <- campaign$data$id
  logger$log_info(glue("Campaign created: id={campaign_id}"))
  campaign_id
}


#' Start (send) a campaign
#'
#' @param campaign_id integer campaign ID
#' @param username API username
#' @param password API key
#' @export
send_campaign <- function(
    campaign_id,
    username = Sys.getenv("DSCI_LISTMONK_API_USERNAME"),
    password = Sys.getenv("DSCI_LISTMONK_API_KEY")
) {
  resp <- request(glue("{BASE_URL}/campaigns/{campaign_id}/status")) |>
    req_auth_basic(username, password) |>
    req_body_json(list(status = "running")) |>
    req_method("PUT") |>
    req_error(body = \(resp) resp_body_string(resp)) |>
    req_perform()

  logger$log_info(glue("Campaign {campaign_id} started"))
}
