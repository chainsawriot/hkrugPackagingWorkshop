context("correct exceptions")

test_that("exceptions on length", {
    expect_that(evolution(origin="1234567", target="123"), throws_error())
})
