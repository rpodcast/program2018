####
#
# make the schedule

library(tidyverse)

#### Get data ----------

  # abstracts
  abstracts <- readr::read_csv("import/rpharma2018_abstracts.csv")

  # schedule
  schedule <- readr::read_csv("import/rpharma2018_schedule.csv")

  # slide links
  slides <- readr::read_csv("import/rpharma2018_slides.csv")

#### Add order
  abstracts <- abstracts %>%
    left_join(
      schedule %>%
        filter(!is.na(speaker_id)) %>%
        mutate(order = row_number()) %>%
        select(speaker_id,order, date,time),
      by = "speaker_id"
    )

#### Create time ----

  abstracts <- abstracts %>%
    mutate(
      day = case_when(
        date == "15.08.18" ~ "Wednesday, 15th August",
        date == "16.08.18" ~ "Thursday, 16th August",
        # date == "2018/8/15" ~ "Wednesday, 15th August",
        # date == "2018/8/16" ~ "Thursday, 16th August",
        TRUE ~ date
      ),
      when = paste0(
        day," from ",time
      )
    )

#### Split data ----

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
        ,i_talk$when
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
      ,i_talk$when
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
      ,i_talk$when
      ,i_talk$abstract # abstract
    ))
  }
  sink()

#### Make overview -------
  schedule_withtalks <- schedule %>%
    left_join(
      abstracts %>%
        select(speaker_id,title,speakerName),
      by = "speaker_id"
    ) %>%
    left_join(
      slides %>%
        select(speaker, slides),
      by = "speaker"
    ) %>%
    mutate(
      info  = case_when(
        type == "Tutorial" ~ paste(talk_desc,"(Workshop)"),
        type %in% c("Keynote","Talk","Lightning Talk") ~ title,
        TRUE ~ type
      ),
      slides = ifelse(is.na(slides), "", slides)
    )

  overview_header <- jb_readtemplate("overview_header")

  sink("overview.md")
cat(
"---
description: R/Pharma 2018 schedule.
---

# Schedule

## Wednesday

")
    # day 1
    temp <- schedule_withtalks %>%
      filter(date == "15.08.18")

      cat(paste0("| When | What | Slides | \n"))
      cat(paste0("|----|----|----| \n"))
      cat(
        paste0(
          "| **",temp$time,"** | _",temp$info,"_ | ", temp$slides, " | \n"
        ),
        sep = ""
      )

cat("

## Thursday

")

    # day 2
    temp <- schedule_withtalks %>%
      filter(date == "16.08.18")
      cat(
        paste0(
          "* **",temp$time,"** _",temp$info,"_ ", temp$slides, " \n"
        ),
        sep = ""
      )
  sink()
