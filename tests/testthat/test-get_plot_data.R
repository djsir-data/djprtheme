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
    ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
      geom_point()
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
    ggplot(mtcars, aes(mpg, hp)) +
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

  it("exports facet variables", {
    test_plot <- ggplot(iris, aes(Petal.Width, Petal.Length)) +
      facet_wrap(~Species) +
      geom_point()
    expect_equal(
      get_plot_data(test_plot),
      iris[, c("Petal.Width", "Petal.Length", "Species")]
    )

    test_plot <- ggplot(mtcars) +
      facet_grid(cyl ~ gear) +
      geom_point(aes(mpg, hp))
    expect_equal(
      get_plot_data(test_plot),
      mtcars[, c("mpg", "hp", "cyl", "gear")]
    )

    # Expressions in facets
    test_plot <- ggplot(mtcars, aes(mpg, hp)) +
      facet_grid((cyl + carb) ~ paste("g", gear, sep=":")) +
      geom_point()
    expect_equal(
      get_plot_data(test_plot),
      with(mtcars, data.frame(
        mpg,
        hp,
        (cyl + carb),
        paste("g", gear, sep=":"),
        check.names=F,
        row.names=row.names(mtcars)
      ))
    )
  })

  it("exports patchwork plot data", {
    p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point() +
      labs(subtitle = "Plot 1 subtitle",
           title = "Plot 1 title")

    p2 <- ggplot(economics, aes(x = date, y = unemploy)) +
      geom_line() +
      labs(subtitle = "Plot 2 subtitle",
           title = "Plot 2 title",
           caption = "Plot 2 caption")

    comb <- patchwork::wrap_plots(p1, p2) +
      patchwork::plot_annotation(title = "Combined plot title",
                      subtitle = "Combined plot subtitle",
                      caption = "Combined plot caption")

    df1 <- mtcars %>%
      dplyr::select(.data$wt, .data$mpg)

    df2 <- ggplot2::economics %>%
      dplyr::select(.data$date, .data$unemploy)
    expect_equal(dplyr::bind_rows(df1, df2),
                 get_plot_data(comb))
  })
})
