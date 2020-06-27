#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(here)
library(RcppRoll)
library(ggplot2)
library(ggrepel)
library(plotly)

# download data (if needed) and load ----
jhdata <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"

if (!isTruthy(difftime(Sys.time(), file.info(here('deaths.csv'))$c, units = 'hours') < 6 )) {
  cat('Updating data ...')
  download.file(paste0(jhdata, "time_series_covid19_deaths_US.csv"   ), here('deaths.csv'   ))
  download.file(paste0(jhdata, "time_series_covid19_confirmed_US.csv"), here('confirmed.csv'))
}
update_ago <- paste("Data pulled", as.character(signif(difftime(Sys.time(), file.info(here('deaths.csv'))$c, units = 'hours'), 1)), 'hours ago')
cat('Loading data ...')
deaths    <- readr::read_csv(here('deaths.csv'))
confirmed <- readr::read_csv(here('confirmed.csv'))

state_dict <- c("US", state.abb) %>% setNames(c("US", state.name))

# wrangle the data ----
covid <-
  # labels and population ----
deaths %>%
  select(Province_State, Combined_Key, Population) %>%
  group_by(Province_State) %>%
  mutate(population_state = sum(Population)) %>%
  rename(population_local = Population) %>%
  ungroup %>%
  separate(Combined_Key, into = c("A", "B"), sep = ", ", remove = FALSE) %>%
  mutate(B = state_dict[B]) %>%
  unite(col = "Local", A, B, sep = ", ") %>%
  select(Combined_Key, Local, Province_State, population_local, population_state) %>%
  # local stats ----
left_join(
  deaths %>%
    select(-c(UID:Long_), -Population) %>%
    pivot_longer(-Combined_Key, names_to = "date", values_to = "deaths_local_cum_total") %>%
    group_by(Combined_Key) %>%
    mutate(deaths_local_new_total    = diff(c(0, deaths_local_cum_total)) %>% replace_na(0),
           deaths_local_past10_total = roll_sumr(deaths_local_new_total, n = 10, fill = 0)) %>%
    ungroup
) %>%
  left_join(
    confirmed %>%
      select(-c(UID:Long_)) %>%
      pivot_longer(-Combined_Key, names_to = "date", values_to = "confirmed_local_cum_total") %>%
      group_by(Combined_Key) %>%
      mutate(confirmed_local_new_total    = diff(c(0, confirmed_local_cum_total)) %>% replace_na(0),
             confirmed_local_past10_total = roll_sumr(confirmed_local_new_total, n = 10, fill = 0)) %>%
      ungroup
  ) %>%
  # aggregate to state stats ----
group_by(date, Province_State) %>%
  mutate(
    deaths_state_cum_total    = sum(deaths_local_cum_total),
    deaths_state_new_total    = sum(deaths_local_new_total),
    deaths_state_past10_total = sum(deaths_local_past10_total),
    
    confirmed_state_cum_total    = sum(confirmed_local_cum_total),
    confirmed_state_new_total    = sum(confirmed_local_new_total),
    confirmed_state_past10_total = sum(confirmed_local_past10_total),
  ) %>%
  ungroup %>%
  # per 1000 residents stats ----
mutate(
  deaths_local_cum_per1000    = deaths_local_cum_total    / population_local * 1000,
  deaths_local_new_per1000    = deaths_local_new_total    / population_local * 1000,
  deaths_local_past10_per1000 = deaths_local_past10_total / population_local * 1000,
  
  confirmed_local_cum_per1000    = confirmed_local_cum_total    / population_local * 1000,
  confirmed_local_new_per1000    = confirmed_local_new_total    / population_local * 1000,
  confirmed_local_past10_per1000 = confirmed_local_past10_total / population_local * 1000,
  
  deaths_state_cum_per1000    = deaths_state_cum_total    / population_state * 1000,
  deaths_state_new_per1000    = deaths_state_new_total    / population_state * 1000,
  deaths_state_past10_per1000 = deaths_state_past10_total / population_state * 1000,
  
  confirmed_state_cum_per1000    = confirmed_state_cum_total    / population_state * 1000,
  confirmed_state_new_per1000    = confirmed_state_new_total    / population_state * 1000,
  confirmed_state_past10_per1000 = confirmed_state_past10_total / population_state * 1000,
  
  date = as.Date(date, format = "%m/%d/%y")
) %>%
  rename(State = Province_State)

# fixed options ----
basis_ch <- c('total', 'per1000') %>% 
  setNames(c('Total', 'Per 1000 Residents'))
counting_ch <- c('new', 'cum', 'past10') %>%
  setNames(c('Daily New', 'Cummulative', 'Past 10 days'))
stats_ch <- c('all', 'confirmed', 'deaths') %>%
  setNames(c('Confirmed & Deaths', 'Confirmed Cases', 'Deaths'))

simple_cap <- function(x) { # https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
  tibble(lab = x) %>%
    separate(col = lab, into = c("a", "b", "c", "d"), sep = "_") %>%
    mutate_all(toupper) %>%
    unite(col = result, sep = " ") %>%
    pull(result)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  column(
    width = 3,
    titlePanel('COVID-19 Explorer', windowTitle = "COVID19"),
    selectizeInput(
      'combined_key', 'Location by US County/Territory', 
      choices = unique(covid$Combined_Key),
      selected = c(
        "Washington, Oklahoma, US",
        "Tarrant, Texas, US",
        "Utah, Utah, US",
        "Salt Lake, Utah, US",
        "Harris, Texas, US",
        "Fulton, Georgia, US"
      ),
      multiple = TRUE
    ),
    radioButtons(
      'basis', 'Basis',
      choices = basis_ch,
      selected = basis_ch[2]
    ),
    radioButtons(
      'counting', 'Counting',
      choices = counting_ch,
      selected = counting_ch[2]
    ),
    radioButtons(
      'stats', 'Metrics',
      choices = stats_ch
    ),
    checkboxInput('trendline', 'Show Smoothed Line', value = TRUE),
    uiOutput('trendline_ui'),
    helpText(update_ago)
  ),
  column(
    width = 9,
    uiOutput('header_local'),
    plotOutput('plot_local'),
    uiOutput('header_state'),
    uiOutput('plot_state')
  ),
  helpText(tags$a(href = 'https://github.com/CSSEGISandData/COVID-19', 
                  'Data source: COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University'))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # duplicate data update in case the app is kept live in memory for a long time
  if (!isTruthy(difftime(Sys.time(), file.info(here('deaths.csv'))$c, units = 'hours') < 6 )) {
    cat('Updating data ...')
    download.file(paste0(jhdata, "time_series_covid19_deaths_US.csv"   ), here('deaths.csv'   ))
    download.file(paste0(jhdata, "time_series_covid19_confirmed_US.csv"), here('confirmed.csv'))
  }
  
  output$trendline_ui <- renderUI({
    tagList(
      checkboxInput('se', 'Show 95% Region', value = FALSE),
      sliderInput('span', 'Smooth line looseness', 
                  value = 0.1, min = 0.1, max = 1, step = 0.1)
    )
  })
  
  stat_list <- reactive({
    if (input$stats == 'all') c('confirmed', 'deaths') else input$stats
  })
  
  local_column_list <- reactive({
    paste(stat_list(), "local", input$counting, input$basis, sep = "_")
  })
  
  state_column_list <- reactive({
    paste(stat_list(), "state", input$counting, input$basis, sep = "_")
  })
  
  local_data <- reactive({ # local data ----
    covid %>%
      filter(Combined_Key %in% input$combined_key) %>%
      select(Local, date, !!local_column_list()) %>%
      pivot_longer(cols = -c(Local, date), names_to = "statistic") %>%
      group_by(Local) %>%
      mutate(my_label = if_else(date == max(date), Local, "")) %>%
      ungroup
  })
  
  state_data <- reactive({ # state data ----
    my_first_state_local <- covid %>%
      filter(Combined_Key %in% input$combined_key) %>%
      group_by(State) %>%
      summarize(Combined_Key = head(Combined_Key, 1)) %>%
      pull(Combined_Key)
    
    covid %>%
      filter(Combined_Key %in% my_first_state_local) %>%
      select(State, date, !!state_column_list()) %>%
      pivot_longer(cols = -c(State, date), names_to = "statistic") %>%
      group_by(State) %>%
      mutate(my_label = if_else(date == max(date), State, "")) %>%
      ungroup
  })
  
  output$header_local <- renderUI({ # local header ----
    h3("Local")
  })
  
  output$header_state <- renderUI({ # state header ----
    h3("State")
  })
  
  output$plot_local <- renderPlot({ # local plot ----
    req(isTruthy(input$combined_key))
    p <-
      local_data() %>%
      ggplot(aes(date, value, col = Local, label = my_label)) + 
      facet_wrap(vars(statistic), 
                 scales = "free_y",
                 labeller = labeller(statistic = simple_cap)) +
      geom_point() +
      geom_text_repel(nudge_x = 0.3*as.numeric(diff(range(local_data()$date)))) +
      theme_bw(base_size = 16) +
      theme(legend.position="none") +
      xlab(NULL) + ylab(NULL) +
      xlim(c(local_data()$date[1], 
             local_data()$date[1] + 
               1.3*diff(range(local_data()$date))))
    
    if (input$trendline) {
      p <- p + stat_smooth(se = input$se, span = input$span)
    }
    
    return(p)
  })
  
  output$plot_state <- renderUI({ # state plot ----
    req(isTruthy(input$combined_key))
    
    lapply(
      state_column_list(),
      function(my_stat) {
        p <-
          state_data() %>%
          filter(statistic == my_stat) %T>% print %>%
          ggplot(aes(date, value, col = State, label = my_label)) +
          # facet_wrap(vars(statistic),
          #            scales = "free_y",
          #            labeller = labeller(statistic = simple_cap)) +
          geom_point() +
          geom_text_repel(nudge_x = 0.3*as.numeric(diff(range(state_data()$date)))) +
          theme_bw(base_size = 16) +
          theme(legend.position="none") +
          xlab(NULL) + ylab(NULL) +
          xlim(c(state_data()$date[1],
                 state_data()$date[1] + 
                   1.3*diff(range(state_data()$date))))
        
        if (input$trendline) {
          p <- p + stat_smooth(se = input$se, span = input$span)
        }
        return( column(width = 12/length(stat_list()), tagList(h4(simple_cap(my_stat)), renderPlotly(ggplotly(p)))) )
      }
    )
    
    # p <-
    #   state_data() %>%
    #   ggplot(aes(date, value, col = State, label = my_label)) +
    #   facet_wrap(vars(statistic),
    #              scales = "free_y",
    #              labeller = labeller(statistic = simple_cap)) +
    #   geom_point() +
    #   geom_text_repel(nudge_x = 0.3*as.numeric(diff(range(state_data()$date)))) +
    #   theme_bw(base_size = 16) +
    #   theme(legend.position="none") +
    #   xlab(NULL) + ylab(NULL) +
    #   xlim(c(state_data()$date[1],
    #          state_data()$date[1] + 
    #            1.3*diff(range(state_data()$date))))
    # 
    # if (input$trendline) {
    #   p <- p + stat_smooth(se = input$se, span = input$span)
    # }
    
    # return(p)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
