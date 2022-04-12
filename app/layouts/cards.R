box::use(
  shiny.fluent[Text,  Stack, IconButton.shinyInput, CommandBar],
  shiny[tagList, div, img, NS, strong, br],
)

box::use(
  app/view/howto_map
)

#' @export
card1 <- div(
  Stack(
    tokens = list(childrenGap = 0),
    strong(
      "Filter feedback database",
    ),
    br(),
    Text(
      "Module: filter"
    ),
    br()
  ),
  Stack(
    tokens = list(childrenGap = 5),
    Text(
      "Here you can choose"
    ),
    Text(
      "- region: Colombia/Africa, ..."
    ),
    Text(
      "- service types: only Healthcare and Cash can be tagged"
    ),
    Text(
      "- dates, sample size and user satisfaction"
    )
  )
)

#' @export
card2 <- function(id) {
  ns <-NS(id)
  div(
  Stack(
    tokens = list(childrenGap = 0),
    strong(
      "Map of selected feedback",
    ),
    br(),
    Text(
      "Module: Map"
    ),
    howto_map$ui(ns("minimap"))
  )
)
}

#' @export
card3 <- div(
  Stack(
    tokens = list(childrenGap = 5),
    strong(
      "Tag & Subset",
    ),
    br(),
    Text(
      "Module: tag"
    ),
    Text(
      "- Use the tag button after filtering database"
    ),
    Text(
      "Module: servicetype"
    ),
    Text(
      "- After that, subset with the cash or health buttons"
    )
  )
)

#' @export
card4 <- div(
  Stack(
    tokens = list(childrenGap = 5),
    Text(
      "Module: table"
    ),
    Text(
      "- After tagging, a new column will appear"
    ),
    Text(
      "- After subsetting, your selection will be here"
    )
  )
)

#' @export
card_wordcloud <- div(
  Stack(
    tokens = list(childrenGap = 5),
    Text(
      "Module: wordcloud"
    ),
    Text(
      "- Create a wordcloud for dataset"
    ),
    Text(
      "- Create a wordcloud for the subset (use subset first)"
    )
  )
)

#' @export
card5 <- div(
  Stack(
    tokens = list(childrenGap = 5),
    strong(
      "Input form",
    ),
    br(),
    Text(
      "Module: form"
    ),
    Text(
      "- Example data to feed the mongo database"
    ),
    Text(
      "- The mongo db is remote, it is hosted on mongo Atlas"
    )
  )
)

#' @export
card6 <- div(
  Stack(
    tokens = list(childrenGap = 5),
    strong(
      "Action buttons",
    ),
    br(),
    Text(
      "Module: form"
    ),
    Text(
      "- Upload form data to the database"
    ),
    Text(
      "- Remove last entry from the mongo db"
    )
  )
)

#' @export
card7list<-list(
  "The shiny app is connected to a source database of community feedback.
This feedback is provided continuously to NGOs (into the database of a third party - here as PoC only, not the real database)"
  ,
  "App PoC: The feedback should be constantly updated (module form in Database page), filtered (mod. filter),
whenever needed tagged (tag module), based on a NLP model developed using a
subset of tagged data (stored in .pkl files)."
  ,
  "Finally, for the health and cash service types (models), text can be subset (mod. servicetype).
This way NGOs can attend and prioritize solutions for communities."
)

#' @export
card8list<-list(
  "The python model is precomputed, saved in .pkl files,
  and loaded by python scripts, from a call in the R module
  `tag`. The pertinent python notebook developed in group is on the gitlab
  repository. In the notebook, the accepted model is SVC. See the 'About' link for the
  repositories"
  ,
  "Where is python in the repository?"
  ,
  "- `Dockerfile` file",
  "- `requirements.txt` file",
  "- `py` folder for scripts",
  "- models are saved in `.pkl` files (`pkl` folder)",
  "- See the `tag` module in folder `modules` for the calls `system('python3...`"
)
