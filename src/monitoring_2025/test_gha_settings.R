box::use(
  logger,
  eu=../utils/email_utils
)

logger$log_info(
  "EMAIL_WHO set to: {Sys.getenv('EMAIL_WHO')}"
)

EMAIL_LIST <- (
  Sys.getenv("EMAIL_WHO", unset = "test")
)

logger$log_info(
  "EMAIL_LIST: {EMAIL_LIST}"
)

df_email_receps <- eu$load_email_recipients(email_list = EMAIL_LIST)

cat(df_email_receps$name,"\n")
