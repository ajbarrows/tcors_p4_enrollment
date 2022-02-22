# TCORS Project 4 Enrollment Dashboard
# Tony Barrows
# 2021-01-20

library(shiny)
library(dplyr)

# setup ------
load("./data/enrollment.RData")

# functions -------

current_enrollment <- function(df_enrl, checkSite, selectPiProp) {
  
  # filter
  df_sub <- df_enrl %>%
    filter(
      site %in% checkSite,
      pi_prop %in% selectPiProp
      )
  
  df_sub %>%
    group_by(site) %>%
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


# Define UI
ui <- fluidPage(

    # Application title
  titlePanel("TCORS Study 2 Project 4"),
  titlePanel(div(img(src = "vcbh_logo.png", width = 300))),
  # titlePanel(div(img(src = "this_is_fine.jpeg", width = 300))),

    # Sidebar with a slider input for number of bins 
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
                      selected = "pilot")  
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("enrl_tab")
        )
        
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  site <- reactive(input$checkSite)
  pi_prop <- reactive(input$selectPiProp)
  
  output$enrl_tab <- renderTable({
    current_enrollment(
      df_enrl,
      site(),
      pi_prop()
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
