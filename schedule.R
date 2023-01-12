library(htmltools)
library(htmlwidgets)
library(reactable)
library(readxl)
# library(stringr)
library(tidyverse)

schedule_df <- read_excel("data/RSE Workshop Applications V2 RS.xlsx", 
                          sheet = "Schedule", na = NA) %>% 
  select(-ID, -Note, -Confirms) %>% 
  relocate(Title, .before=Author) %>% 
  mutate(Session = str_to_title(Session)) %>% 
  # Remove whitespaces
  mutate(Session = str_squish(Session)) %>% 
  mutate(Session = as.factor(Session)) 


# Table preparations ------------------------------------------------------

# Nested information about sessions.
row_details <- function(index) {
  abstract <- schedule_df$Abstract[index]
  htmltools::div(class = "nested_table", abstract)

}


# Table -------------------------------------------------------------------

schedule <- reactable(
  schedule_df,
  height = 600,
  searchable = TRUE,
  # filterable = TRUE,
  # groupBy = "Day",
  # defaultExpanded = TRUE,
  pagination = FALSE, 
  highlight = TRUE,
  columns = list(
    Session = colDef(
      style = function(value) {
        if (value == "Panel") {
          color <- "#008000"
        } else if (str_starts(value, "Focus")) {
          color <- "#e00000"
        } else if (value == "Lightning Talks") {
          color <- "orange"
        } else {
          color <- "#777"
        }
        list(color = color, fontWeight = "bold")
      }
    ),
    Title = colDef(
      name = "Title & Author",
      # Show author and affiliation under Title's info.
      cell = function(value, index) {
        author <- schedule_df$Author[index]
        affiliation <- schedule_df$Organisation[index]
        # species <- if (!is.na(species)) species else "Unknown"
        div(
          div(style = list(fontWeight = 600), value),
          div(style = list(fontSize = "0.75rem"), paste0(author, ", ", affiliation))
        )
      }
    ),
    Author = colDef(show = FALSE),
    Organisation = colDef(show = FALSE),
    Abstract = colDef(show = FALSE)
    ),
  details = row_details
  )

saveWidget(schedule, "schedule.html")
