setwd("../../")

box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app/main[...],
)

test_that("main server works", {
  testServer(server, {

  })
})
