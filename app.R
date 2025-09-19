library(shiny)
library(dplyr)
library(lubridate)

perks_data <- data.frame(
  name = c("Grubhub", "Saks"),
  value = c("$10", "$50"),
  period = c("monthly", "semi-annual"),
  source = c("Gold Card", "Platinum Card"),
  category = c("Dining", "Shopping"),
  used = c(FALSE, FALSE),
  last_reset = c(Sys.Date(), Sys.Date()),
  stringsAsFactors = FALSE
)

get_next_reset_date <- function(period, last_reset) {
  current_date <- Sys.Date()
  current_year <- year(current_date)
  current_month <- month(current_date)

  switch(period,
    "monthly" = ceiling_date(current_date, "month"),

    "semi-annual" = {
      # Semi-annual: January 1st and July 1st
      jan_1 <- as.Date(paste0(current_year, "-01-01"))
      jul_1 <- as.Date(paste0(current_year, "-07-01"))
      next_jan_1 <- as.Date(paste0(current_year + 1, "-01-01"))

      if (current_date < jan_1) {
        jan_1
      } else if (current_date < jul_1) {
        jul_1
      } else {
        next_jan_1
      }
    },

    "seasonal" = {
      # Seasonal: January 1st, April 1st, July 1st, October 1st
      jan_1 <- as.Date(paste0(current_year, "-01-01"))
      apr_1 <- as.Date(paste0(current_year, "-04-01"))
      jul_1 <- as.Date(paste0(current_year, "-07-01"))
      oct_1 <- as.Date(paste0(current_year, "-10-01"))
      next_jan_1 <- as.Date(paste0(current_year + 1, "-01-01"))

      if (current_date < jan_1) {
        jan_1
      } else if (current_date < apr_1) {
        apr_1
      } else if (current_date < jul_1) {
        jul_1
      } else if (current_date < oct_1) {
        oct_1
      } else {
        next_jan_1
      }
    },

    "annual" = {
      # Annual: January 1st
      jan_1 <- as.Date(paste0(current_year, "-01-01"))
      next_jan_1 <- as.Date(paste0(current_year + 1, "-01-01"))

      if (current_date < jan_1) {
        jan_1
      } else {
        next_jan_1
      }
    }
  )
}

should_auto_reset <- function(period, last_reset) {
  next_reset <- get_next_reset_date(period, last_reset)
  return(Sys.Date() >= next_reset)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .main-title {
        text-align: center;
        color: #2c3e50;
        margin-bottom: 30px;
        font-weight: 300;
      }
      .perk-card {
        background: white;
        border-radius: 15px;
        padding: 20px;
        margin: 15px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        transition: all 0.3s ease;
        cursor: pointer;
        border-left: 5px solid #3498db;
      }
      .perk-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
      .perk-card.used {
        border-left-color: #e74c3c;
        background: #fdf2f2;
      }
      .perk-card.available {
        border-left-color: #27ae60;
        background: #f8fff8;
      }
      .perk-name {
        font-size: 1.3em;
        font-weight: bold;
        color: #2c3e50;
        margin-bottom: 10px;
      }
      .perk-value {
        font-size: 1.5em;
        font-weight: bold;
        color: #3498db;
        display: inline-block;
        margin-right: 15px;
      }
      .perk-period {
        color: #7f8c8d;
        font-style: italic;
        display: inline-block;
      }
      .perk-source {
        color: #95a5a6;
        font-size: 0.9em;
        margin-top: 8px;
      }
      .status-badge {
        float: right;
        padding: 8px 15px;
        border-radius: 20px;
        font-weight: bold;
        font-size: 0.9em;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .status-used {
        background-color: #e74c3c;
        color: white;
      }
      .status-available {
        background-color: #27ae60;
        color: white;
      }
      .next-reset {
        font-size: 0.8em;
        color: #e67e22;
        margin-top: 5px;
        font-style: italic;
      }
      .instruction-text {
        text-align: center;
        color: #7f8c8d;
        font-style: italic;
        margin-top: 20px;
      }
    "))
  ),

  div(class = "container-fluid",
    h1("Amex Credit Card Perks Manager", class = "main-title"),
    tabsetPanel(
      id = "perk_tabs",
      type = "pills",
      tabPanel("Dining",
        br(),
        uiOutput("dining_perks")
      ),
      tabPanel("Shopping",
        br(),
        uiOutput("shopping_perks")
      )
    ),
    br(),
    p("Click on any perk card to toggle its usage status", class = "instruction-text")
  )
)

server <- function(input, output, session) {

  perks_reactive <- reactiveVal(perks_data)

  observe({
    current_perks <- perks_reactive()

    for (i in 1:nrow(current_perks)) {
      if (current_perks$used[i] && should_auto_reset(current_perks$period[i], current_perks$last_reset[i])) {
        current_perks$used[i] <- FALSE
        current_perks$last_reset[i] <- Sys.Date()
      }
    }

    perks_reactive(current_perks)
  })

  create_perk_cards <- function(category_filter) {
    current_perks <- perks_reactive()
    filtered_perks <- current_perks[current_perks$category == category_filter, ]

    if (nrow(filtered_perks) == 0) {
      return(div(
        style = "text-align: center; color: #7f8c8d; margin-top: 50px;",
        h4("No perks in this category yet"),
        p("Add some perks to get started!")
      ))
    }

    cards <- lapply(1:nrow(filtered_perks), function(i) {
      perk <- filtered_perks[i, ]
      # Find the original row index in the full dataset
      original_index <- which(current_perks$name == perk$name & current_perks$category == perk$category)[1]

      status_class <- if(perk$used) "used" else "available"
      status_text <- if(perk$used) "USED" else "AVAILABLE"
      status_badge_class <- if(perk$used) "status-used" else "status-available"

      next_reset_text <- ""
      if(perk$used) {
        next_reset_date <- get_next_reset_date(perk$period, perk$last_reset)
        next_reset_text <- paste("Next reset:", format(next_reset_date, "%B %d, %Y"))
      }

      div(
        class = paste("perk-card", status_class),
        onclick = paste0("Shiny.onInputChange('perk_clicked', {id: ", original_index, ", nonce: Math.random()})"),
        div(
          span(class = paste("status-badge", status_badge_class), status_text),
          div(class = "perk-name", perk$name),
          div(
            span(class = "perk-value", perk$value),
            span(class = "perk-period", paste("Every", perk$period))
          ),
          div(class = "perk-source", paste("Source:", perk$source)),
          if(next_reset_text != "") div(class = "next-reset", next_reset_text) else NULL
        )
      )
    })

    do.call(tagList, cards)
  }

  output$dining_perks <- renderUI({
    create_perk_cards("Dining")
  })

  output$shopping_perks <- renderUI({
    create_perk_cards("Shopping")
  })


  observeEvent(input$perk_clicked, {
    clicked_info <- input$perk_clicked

    if (!is.null(clicked_info) && !is.null(clicked_info$id)) {
      current_perks <- perks_reactive()
      clicked_row <- clicked_info$id

      # Add bounds checking
      if (clicked_row > 0 && clicked_row <= nrow(current_perks)) {
        current_perks$used[clicked_row] <- !current_perks$used[clicked_row]

        if (current_perks$used[clicked_row]) {
          current_perks$last_reset[clicked_row] <- Sys.Date()
        }

        perks_reactive(current_perks)

        status_text <- if(current_perks$used[clicked_row]) "marked as used" else "marked as available"
        showNotification(
          paste(current_perks$name[clicked_row], status_text),
          type = "message",
          duration = 3
        )
      }
    }
  })


}

shinyApp(ui = ui, server = server)