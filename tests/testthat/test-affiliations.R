test_that("Affiliations", {
  expect_equal(
    {
      x <- tibble::tribble(
        ~id, ~name, ~affiliation,
        1, "Edmund Blackadder", list("University of Life", "School of Hard Knocks", "Kindergarten of Getting the Shit Kicked Out of Me"),
        2, "George Colthurst St. Barleigh", "Trinity College Cambridge",
        3, "Sodoff Baldrick", "School of Hard Knocks",
        4, "Anthony Cecil Hogmanay Melchett", "Trinity College, Cambridge"
      )
      test <- affilliations(x, nooutfile = T)
      test$names$id[1]
    },
    1
  )
})
