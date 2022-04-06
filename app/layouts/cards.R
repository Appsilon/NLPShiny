box::use(
  shiny.fluent[Text,  Stack],
  shiny[tagList, div, img, NS],
  shiny.fluent[IconButton.shinyInput, Text, CommandBar],
)

box::use(
  app/view/howto_map
)

#' @export
card1 <- div(
  Stack(
    tokens = list(childrenGap = 5),
    Text(
      variant = "xLarge",
      "Filter feedback database",
      block = F
    ),
    Text(
      "Module: filter"
    ),
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
    tokens = list(childrenGap = 5),
    Text(
      variant = "xLarge",
      "Map of selected feedback",
      block = F
    ),
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
    Text(
      variant = "xLarge",
      "Tag & Subset",
      block = F
    ),
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
      variant = "xLarge",
      "Feedback table",
      block = F
    ),
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
card5 <- div(
  Stack(
    tokens = list(childrenGap = 5),
    Text(
      variant = "xLarge",
      "Input form",
      block = F
    ),
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
    Text(
      variant = "xLarge",
      "Action buttons",
      block = F
    ),
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
