####
#
# make the schedule

library(tidyverse)

#### Get data ----------

  # abstracts
  abstracts <- readr::read_csv("import/rpharma2018_abstracts.csv")

  # schedule
  schedule <- readr::read_csv("import/rpharma2018_schedule.csv")

#### Add order
  abstracts <- abstracts %>%
    left_join(
      schedule %>%
        filter(!is.na(speaker_id)) %>%
        mutate(order = row_number()) %>%
        select(speaker_id,order),
      by = "speaker_id"
    )

#### Split data

  abstracts_talks <- abstracts %>%
    filter(
      !is.na(email) & speaker_id %in% schedule$speaker_id
      )

  abstracts_keynotes <- abstracts %>% filter(is.na(email))

#### Arrange data

  abstracts_talks <- abstracts_talks %>% arrange(speakerName)

#### Check assumptions -------

  # emails unique in abstracts
  if (
    dplyr::n_distinct(abstracts_talks$email) != nrow(abstracts_talks)
    ) stop("EMAIL NOT UNIQUE")

#### Function to read in templates -------

  jb_readtemplate <- function(x){
    content <- readLines(paste0("templates/",x))
    content <- paste(content,collapse = "\n")
    content
  }

#### Make talks by name -------

  sink("talks.md")
  cat(jb_readtemplate("talks_header"))
  # loop through talks
  talks_atalk <- jb_readtemplate("talks_atalk")
  # by name
    for (i in 1:nrow(abstracts_talks)) {
      # get data on one talk
      i_talk <- abstracts_talks[i,]

      cat(sprintf(
        talks_atalk
        ,i_talk$title # title
        ,i_talk$speakerName # name
        ,i_talk$affiliation # affiliation
        ,i_talk$abstract # abstract
      ))
    }
  sink()

  #### Make talks by order -------

  sink("talks-order.md")
  cat(jb_readtemplate("talks_header"))
  # loop through talks
  # by order
  abstracts_talks <- abstracts_talks %>%
    arrange(order)
  for (i in 1:nrow(abstracts_talks)) {
    # get data on one talk
    i_talk <- abstracts_talks[i,]

    cat(sprintf(
      talks_atalk
      ,i_talk$title # title
      ,i_talk$speakerName # name
      ,i_talk$affiliation # affiliation
      ,i_talk$abstract # abstract
    ))
  }
  sink()

#### Make keynotes -------

  sink("keynotes.md")
  cat(jb_readtemplate("keynotes_header"))
  # loop through talks
  talks_atalk <- jb_readtemplate("talks_atalk")
  for (i in 1:nrow(abstracts_keynotes)) {
    # get data on one talk
      i_talk <- abstracts_keynotes[i,]

    cat(sprintf(
      talks_atalk
      ,i_talk$title # title
      ,i_talk$speakerName # name
      ,i_talk$affiliation # affiliation
      ,i_talk$abstract # abstract
    ))
  }
  sink()
