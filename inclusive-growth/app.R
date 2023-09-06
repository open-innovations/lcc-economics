library(shiny)
library(patchwork)
library(bslib)
library(bsicons)
library(DT)

# UI ----------------------------------------------------------------------

ui <- bslib::page_fluid(
  #fluidPage(
  navbarPage(
    title = "Leeds Inclusive Growth Dashboard",
    id = "navbar",
    collapsible = TRUE,
    windowTitle = "Leeds Inclusive Growth Dashboard",

    tabPanel(
      title = "Dashboard",
      value = "dashboard",
      titlePanel("Latest economic indicators"),
      uiOutput("dashboardUI")
    ),

    tabPanel(
      title = "Time series",
      value = "time-series",
      titlePanel("Historic economic indicators"),
      uiOutput("headlineUI")
    ),

    tabPanel(
      title = "Data",
      value = "data",
      titlePanel("Explore and download the data"),
      uiOutput("data_table")
    ),

    tabPanel(
      title = "About",
      value = "about",
      titlePanel("About the Inclusive Growth Dashboard"),
      p("TODO Insert copy here ...")
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Bookmarking (persistent URLs) -------------------------------------------

  # Automatically bookmark every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })

  # Update the query string
  onBookmarked(updateQueryString)

  # Global variables
  plot.theme <- ggplot2::theme(
    panel.background   = ggplot2::element_blank(),
    panel.grid         = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(linetype = "dotted"),
    legend.position    = "top",
    axis.line.y.right  = NULL,
    axis.line          = ggplot2::element_line(),
    text               = ggplot2::element_text(size = 12)
  )

  data1 <- readr::read_csv("all_data.csv")

  vNames <- unique(data1$variable_name[data1$is_summary])

  # Generate dynamic UI components ----------------------------------------

  library(plotly)

  mini_plots <- lapply(seq_along(vNames), function(x) {
    data1 |>
      dplyr::filter(geography_name == 'Leeds',
                    variable_name == vNames[x],
                    is_summary,
                    !is.na(value))
  })

  build_mini_plots <- function(x) {
    sparkline <- plot_ly(mini_plots[[x]]) %>%
      add_lines(
        x = ~date, y = ~value,
        color = I("#ED7218"), span = I(1)#,
        # fill = 'tozeroy', alpha = 0.2
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
      )
    return(sparkline)
  }

  output$dashboardUI <- renderUI({
    latest_indictors <- lapply(seq_along(vNames), function(x) {
      tempdata <- data1 |>
        dplyr::filter(geography_name == 'Leeds',
                      variable_name == vNames[x],
                      is_summary,
                      !is.na(value))

      temp_value <- tempdata$value[tempdata$date == max(tempdata$date)]

      value_box(
        style = 'background-color: #1BACAF!important;',
        title = vNames[x],
        value = if (vNames[x] == "GVA") {
          paste0("£", temp_value/1000, "bn")
        } else if (vNames[x] == "Population") {
          paste0(format(temp_value, big.mark = ","))
        } else if (vNames[x] == "GVA per filled job") {
          paste0("£", format(round(temp_value), big.mark = ","))
        } else {
          paste0(temp_value, "%")
        },
        # showcase = bs_icon("bar-chart"),
        showcase = build_mini_plots(x),
        full_screen = TRUE,
        p(paste("Latest data is for", unique(tempdata$date_name[tempdata$date == max(tempdata$date)])
        ))
      )
    })

    layout_column_wrap(
      width = 1/4,
      !!!latest_indictors
    )
  })

  output$headlineUI <- renderUI({
    plots <- lapply(seq_along(vNames), function(x) {
      renderPlotly({ # change HERE
        p <- data1 |>
          dplyr::filter(geography_name == 'Leeds',
                        variable_name == vNames[x],
                        is_summary,
                        !is.na(value)) |>
          ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
          ggplot2::geom_line(colour = "#ED7218") +
          plot.theme +
          ggplot2::labs(title = vNames[x],
                        subtitle = NULL,
                        x = "",
                        y = "%")
        ggplotly(p) |>  # change HERE
          config(displayModeBar = F)
      })
    })

    list(
      checkboxInput("headline_2018",
                    label = "Show only 2018 onwards",
                    value = TRUE),
      layout_column_wrap(
        width = 1/4,
        !!!plots
      )
    )
  })

  details <- lapply(seq_along(vNames), function(i) {
    tabPanel(
      titlePanel(paste("Detail and comparisons:", vNames[i])),
      title = vNames[i],
      value = stringr::str_replace_all(vNames[i], " ", "-"),

      list(
        checkboxInput("details_2018",
                      label = "Show only 2018 onwards",
                      value = TRUE),
        renderPlotly({ # change HERE
          cities <- data1 |>
            dplyr::filter(variable_name == vNames[i]) |>
            dplyr::filter(geography_core_city == TRUE) |>
            dplyr::filter(!is.na(value)) |>
            dplyr::group_by(geography_name) |> #ajslkdf;ja
            ggplot2::ggplot(ggplot2::aes(x = date, y = value,
                                         colour = geography_name != "Leeds",
                                         group = geography_name)) +
            ggplot2::geom_line() +
            # gghighlight::gghighlight(geography_name == "Leeds",
            #                          use_direct_label = FALSE) +
            plot.theme +
            ggplot2::scale_color_manual(values = c("red", "lightgrey"))
            ggplot2::labs(#title = "Core Cities",
                          subtitle = vNames[i],
                          x = "",
                          y = "%",
                          colour = "") +
            ggplot2::theme(legend.position = "top")

          cities <- ggplotly(cities, tooltip = c("geography_name", "value"))

          others <- data1 |>
            dplyr::filter(variable_name == vNames[i]) |>
            dplyr::filter(geography_core_city == FALSE |
                            geography_name == "Leeds") |>
            dplyr::filter(!is.na(value)) |>
            ggplot2::ggplot(ggplot2::aes(x = date, y = value,
                                         colour = geography_name)) +
            ggplot2::geom_line() +
            plot.theme +
            ggplot2::labs(#title = "Other geographies",
                          subtitle = vNames[i],
                          x = "",
                          y = "%",
                          colour = "") +
            ggplot2::theme(legend.position = "top")

          others <- ggplotly(others)

          this_category <- unique(data1$category[data1$variable_name == vNames[i]])

          breakdown <- data1 |>
            dplyr::filter(category == this_category,
                          !is_summary,
                          geography_name == "Leeds",
                          !is.na(value),
                          !grepl("G-U", variable_name)) |> # emp by industry
            dplyr::mutate(variable_name = variable_name |>
                            stringr::str_remove("% all in employment who work in - ") |>
                            stringr::str_remove("\\(SIC 2007\\)") |>
                            stringr::str_remove("% of economically inactive ") |>
                            stringr::str_remove("% who are economically inactive - "))

          if (this_category == "Economic inactivity") {
            breakdown <- breakdown |>
              dplyr::mutate(sub_category = ifelse(grepl("aged", variable_name),
                                                  "Inactivity by age",
                                                  "Inactivity by reason"))
          }

          if (nrow(breakdown) > 0) {
            breakdown <- breakdown |>
              ggplot2::ggplot(ggplot2::aes(x = date, y = value,
                                           colour = variable_name)) +
              ggplot2::geom_line() +
              {if ("sub_category" %in% names(breakdown)) ggplot2::facet_wrap("sub_category") } +
              plot.theme +
              ggplot2::labs(x = "",
                            y = "%",
                            colour = "")

            breakdown <- ggplotly(breakdown)

            # print((cities + others) / breakdown)
            subplot(style(cities, showlegend = F), others, breakdown, nrows = 2)
          } else {
            # print(cities + others)
            subplot(style(cities, showlegend = F), others)
          }
        })
      )


    )
  })

  # Build details navbarMenu
  details_menu <- do.call(navbarMenu, c("Details", details))

  # Insert details navbarMenu into main navbar
  insertTab("navbar",
            details_menu,
            target = "data",
            position = "before"
  )

  # Data table

  output$data_table <- renderUI({
    list(
      DT::renderDT(data1 |>
                     dplyr::select(date = date_name,
                                   geography_code,
                                   geography_name,
                                   category,
                                   variable_name,
                                   value)),
      downloadButton("download_data",
                     label = "Download as CSV")
    )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("leeds-inclusive-growth-data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(data1, file)
    }
  )
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
