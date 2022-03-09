# TCORS Project 4 Enrollment Dashboard
# Tony Barrows
# 2021-01-20

library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

# setup ------
load("./data/enrollment.RData")
theme_set(theme_classic(base_size = 15))

# common -------
session_levels <- c(
  "prescreen",
  "screening",
  "baseline_2"
)

# functions -------

current_enrollment <- function(df_enrl_filtered, checkSite) {
  # Produce snapshot of current trial enrollment.
  
  df_enrl_filtered %>%
    group_by(site) %>%
    filter(
      !is.na(sl_status),
      session == "screening"
      ) %>%
    count(sl_status, .drop = FALSE) %>%
    tidyr::pivot_wider(
      names_from = "sl_status",
      values_from = "n"
    ) %>%
    mutate(Withdrawn = `Withdrawn - Pre-Product` + `Withdrawn - Post-Product`) %>%
    select(-c(`Withdrawn - Pre-Product`, `Withdrawn - Post-Product`)) %>%
    relocate(Complete, .after = last_col()) %>%
    filter(site %in% checkSite)
}

plot_enrollment_ts <- function(df_enrl_filtered, session_levels, israte) {
  # Produce timeseries plot of trial enrollment history.
  
  df_sub <- df_enrl_filtered %>%
    filter(
      session %in% c("prescreen", "screening", "baseline_2"),
      date >= as.Date("2019-01-01"),
      date <= Sys.Date()
      ) %>%
    mutate(session = factor(session, levels = session_levels)) %>%
    group_by(session, site, date) %>%
    count() %>%
    group_by(session, site) %>%
    tidyr::complete(
      session, site, 
      date = seq.Date(min(date), Sys.Date(), by = "day")
      ) %>%
    arrange(session, date, site) %>%
    mutate(
      n_zero = ifelse(is.na(n), 0, n),
      cumsum = cumsum(n_zero),
      rate = zoo::rollmean(n_zero, 7, fill = NA)
      )
  
  if (israte == "Rate") {
    p <- ggplot(df_sub, aes(x = date, y = n, color = site)) +
      geom_point() +
      geom_line(aes(y = rate), color = "black") +
      labs(
        caption = "Black Line = Overall 7-day Rolling Average"
      )
  } else {
    p <- ggplot(df_sub, aes(x = date, y = cumsum, color = site)) +
      ylab("Cumulative") +
      geom_line()
  }
  
  p +
    facet_wrap(~session) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Define UI
ui <- fluidPage(

    # Application title
  titlePanel("TCORS Study 2 Project 4"),
  titlePanel(div(img(src = "vcbh_logo.png", width = 300))),
  # titlePanel(div(img(src = "this_is_fine.jpeg", width = 300))),

    # Parameters
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("checkSite",
                             h4("Site"),
                             choices = list(
                               "UVM" = "uvm",
                               "UKY" = "uky"
                             ),
                             selected = c("uvm", "uky")
          ),
          selectInput("selectPiProp",
                      h4("Study"),
                      choices = list(
                        "Pilot" = "pilot",
                        "Proper" = "proper"
                      ),
                      selected = "pilot"),
          radioButtons("switchRate",
                       "Plots",
                       choices = list("Cumulative", "Rate"))
          
        ),

        # Enrollment information and rates
        mainPanel(
          tableOutput("enrl_tab"),
          plotOutput("enrl_ts")
        )
        
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  site <- reactive(input$checkSite)
  pi_prop <- reactive(input$selectPiProp)
  
  df_enrl_filtered <- reactive({
    # make enrollment DF reactive
    df_enrl %>%
      filter(
        site %in% site(),
        pi_prop %in% pi_prop() | is.na(pi_prop)
      )
  })
  
  
  output$enrl_tab <- renderTable({
    current_enrollment(
      df_enrl_filtered(),
      site()
    )
  })
  
  output$enrl_ts <- renderPlot({
    if(length(site()) > 0) {
      plot_enrollment_ts(
        df_enrl_filtered(),
        session_levels,
        input$switchRate
      )
    }
 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# scratch -- 

# df_enrl_filtered <- df_enrl %>%
#   filter(
#     pi_prop == "pilot" | is.na(pi_prop)
#     )

#TODO Count prescreens by source, include BuildClinical
