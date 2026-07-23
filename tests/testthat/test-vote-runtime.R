test_that("gen_vote validates scalar inputs before producing output", {
  expect_error(
    gen_vote("option 1", trigger = NA_character_),
    "`trigger` must be one non-empty string",
    fixed = TRUE
  )
  expect_error(
    gen_vote("option 1", trigger = c("option", "choice")),
    "`trigger` must be one non-empty string",
    fixed = TRUE
  )
  expect_error(
    gen_vote(1:3, trigger = "option"),
    "`voting_list` must be a list or character vector",
    fixed = TRUE
  )
  expect_error(
    gen_vote(
      "option 1",
      trigger = "option",
      return_winner = "content"
    ),
    "`underlying_list` must be provided",
    fixed = TRUE
  )
})

test_that("gen_vote treats regex punctuation in triggers literally", {
  votes <- c(
    "rating (final): 12",
    "rating (final): 12",
    "rating final: 2"
  )
  output <- capture.output(
    winner <- gen_vote(
      votes,
      trigger = "rating (final)",
      return_winner = "id"
    )
  )
  expect_identical(winner, "12")
  expect_true(any(grepl("Option 12", output, fixed = TRUE)))
})

test_that("gen_vote exposes stable scoreboard and content return modes", {
  votes <- list(
    list(response_value = "option b", model = "one"),
    list(response_value = "option a", model = "two"),
    list(response_value = "option b", model = "three")
  )
  choices <- c("alpha", "beta")

  invisible(capture.output(
    scoreboard <- gen_vote(
      votes,
      trigger = "option",
      type = "letter",
      return_winner = "scoreboard"
    )
  ))
  invisible(capture.output(
    content <- gen_vote(
      votes,
      underlying_list = choices,
      trigger = "option",
      type = "letter",
      return_winner = "content"
    )
  ))

  expect_identical(scoreboard$option, c("b", "a"))
  expect_identical(scoreboard$votes, c(2L, 1L))
  expect_identical(content, "beta")
})
