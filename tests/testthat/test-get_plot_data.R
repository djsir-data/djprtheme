library(dplyr)
library(ggplot2)

describe("get_plot_data()", {
  it("returns plot data passed to ggplot() call", {
    test_plot <-
      ggplot(iris, aes(Petal.Length, Petal.Width)) +
      geom_point()

    expect_equal(
      get_plot_data(test_plot),
      iris[, c("Petal.Length", "Petal.Width")]
    )
  })

  it("defaults to the last plot", {
    qplot(Sepal.Width, Sepal.Length, data=iris)
    expect_equal(
      get_plot_data(),
      iris[, c("Sepal.Width", "Sepal.Length")]
    )
  })

  it("supports formulae in aesthetics", {
    test_plot <-
      ggplot(
        iris,
        aes(
          Sepal.Width - mean(Sepal.Width),
          scale(Sepal.Length, center = T, scale = T),
          colour=Species
        )
      ) +
      geom_point()

    expect_equal(
      get_plot_data(test_plot),
      with(iris, data.frame(
        Sepal.Width - mean(Sepal.Width),
        scale(Sepal.Length, center = T, scale = T),
        Species,
        check.names=F
      ))
    )
  })

  it("detects aesthetics across multiple layers", {
    test_plot <-
      ggplot(iris, aes(Petal.Length, Petal.Width)) +
      # Adds Sepal.Length to expected return list
      geom_point(aes(size=Sepal.Length), colour="blue") +
      # Adds Sepal.Width to expected return list; Sepal.Length already on list
      # and should not be returned twice
      geom_point(aes(alpha=Sepal.Width, y=Sepal.Length), colour="red")

    expect_equal(
      get_plot_data(test_plot),
      iris[, c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")]
    )
  })

  it("ignores layers that have their own data", {
    test_dataset <-
      group_by(iris, Species) %>%
      summarise_all(mean)
    test_plot <-
      ggplot(iris, aes(Petal.Length, Petal.Width)) +
      # Adds Sepal.Length to expected return list
      geom_point(aes(size=Sepal.Length), colour="blue") +
      # Should be ignored, not using plot data
      geom_point(data=test_dataset, aes(colour=Species))

    expect_equal(
      get_plot_data(test_plot),
      iris[, c("Petal.Length", "Petal.Width", "Sepal.Length")]
    )
  })

  it("preserves row names", {
    qplot(mpg, hp, data=mtcars) +
      geom_point()

    expect_equal(
      get_plot_data(),
      mtcars[, c("mpg", "hp")]
    )

    expect_equal(
      rownames(get_plot_data()),
      rownames(mtcars)
    )
  })
})
