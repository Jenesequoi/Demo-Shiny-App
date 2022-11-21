library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(janitor)
library(dplyr)
library(tidylog)
library(lubridate)
library(scales)
library(ggplot2)
library(lemon)

#Loading data
Merscov <- read_csv("Merscov.csv")

# Define UI 
ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  
#Theme
  navbarPage(
    "MERsCOV Hospital Surveillance Study 2022/2023", 
#Title
    tabPanel("Home", icon = icon("house"), 
# Sidebar  
    sidebarPanel(
      h4( 'HUMAN TRANSMISSION OF MIDDLE EAST RESPIRATORY SYNDROME CORONA VIRUS (MERS-CoV) AND ITS CLINICAL PROFILE AMONG SYMPTOMATIC PATIENTS IN NOMADIC COMMUNITIES OF NORTHERN KENYA',align = "left"),
      h4( 'Organisations Involved',align = "left"),
       fluidRow(column(width=2),column(width=5,
                                      tags$div(tags$ul(
                                        tags$li(tags$span("Washington State University Global Health Kenya")),
                                        tags$li(tags$span("Kenya Medical Research Institute, Kenya")),
                                        tags$li(tags$span("University of Nairobi, Kenya")), 
                                      )))),

    ),
#Main panel
        mainPanel(
            fluidRow(
              # A static valueBox
              #shinydashboard::valueBox(10 * 2, "New Orders", icon = icon("hourglass-3")),
              h3( 'Middle East Respiratory syndrome (MERS) is a viral respiratory disease caused by a novel corona virus known as Middle East Respiratory Syndrome corona virus (MERS - CoV).',align = "center",style = "color: white; font-size: 20px; font-weight: bold;background-color:teal;font-family: Arial"),
              h2("Participant Enrolment Summary"),
              shinydashboard::valueBoxOutput(width = 3,"target"),
              shinydashboard::valueBoxOutput(width = 3,"enrolled"),
              shinydashboard::valueBoxOutput(width = 3,"enrolmentrate"),
            ),
            br(),
            fluidRow(
              h2("Sample Collection Summary"),
              shinydashboard::valueBoxOutput(width = 3,"serum"),
              shinydashboard::valueBoxOutput(width = 3,"np"),
              shinydashboard::valueBoxOutput(width = 3,"rate"),
            ),
        ),
  ), #End of Navbar1
    tabPanel("Summary", icon = icon("list-ul"),
             mainPanel(
               fluidRow(
                 h2("Enrollment by Gender"),
                 splitLayout(cellWidths = c("80%", "70%"), tableOutput("enrolmentbygender"), plotOutput("age_pyramids")),
                 #tableOutput(outputId = "enrolmentbygender"),
                 br(),
                 fluidRow(
                 h2("Inclusion Creterias"),
                 splitLayout(cellWidths = c("50%", "70%"), plotOutput("fever"), plotOutput("inclusion")),
                 #plotOutput(outputId = "age_pyramids"),
                 #plotOutput(outputId = "fever"),
                 #plotOutput(outputId = "inclusion"),
                 #plotOutput(outputId = "camel"),
                 #plotOutput(outputId = "resp14"),
                 fluidRow(
                   h2("Plots"),
                   splitLayout(cellWidths = c("70%", "80%"), plotOutput("camel"), plotOutput("resp14")),
                   #plotOutput(outputId = "age_pyramids"),
                   #plotOutput(outputId = "fever"),
                   #plotOutput(outputId = "inclusion"),
                   #plotOutput(outputId = "camel"),
                   #plotOutput(outputId = "resp14"),
                 ),
               ),
             ),
             ),
    ),
    tabPanel("Trends", icon = icon("chart-area"), value = "trend", "Empty for now")
    ) #End of Nav page
  )#End of fluid page
        


# Define server logic required to draw a histogram
server <- function(input, output) {

  
  
  output$target <- renderValueBox({
    
    shinydashboard::valueBox( 
      1950, "Target Enrolment",icon = icon("bullseye"),color =  "aqua",
    )
  })
  
  output$enrolled <- renderValueBox({
    y <- Merscov %>% filter(consent == "Yes") %>% group_by(consent)  %>%
      summarise(n = n()) %>% .$n
    shinydashboard::valueBox(
      y, "Participants Enrolled",icon = icon("users"), color = "red"
    )
  })
 
    output$enrolmentrate <- renderValueBox({
      y <- Merscov %>% filter(consent == "Yes") %>% group_by(consent)  %>%
        summarise(n = n()) %>% .$n
      totaldays <- as.numeric(as.Date(Sys.Date()) - as.Date("2022-09-01"))
      expenrolled <- (totaldays/7)*25
      enrolrate <- (y/expenrolled)*100
    shinydashboard::valueBox(
      enrolrate, "Enrollment rate", icon = icon("bolt"), color = 'purple'
    )
  })

output$serum <- renderValueBox({
  s <- Merscov %>% filter(samples_collected___1 == "Serum") %>% group_by(samples_collected___1)  %>%
    summarise(n = n()) %>% .$n 
  shinydashboard::valueBox( 
    s, "Serum Collected",color =  "aqua",
  )
})

output$np <- renderValueBox({
  n <- Merscov %>% filter(samples_collected___2 == "NP/OP") %>% group_by(samples_collected___2)  %>%
    summarise(n = n()) %>% .$n
  shinydashboard::valueBox(
    n, "NP/OP Collected", color = "red"
  )
})

output$rate <- renderValueBox({
  s <- Merscov %>% filter(samples_collected___1 == "Serum") %>% group_by(samples_collected___1)  %>%
    summarise(n = n()) %>% .$n 
  n <- Merscov %>% filter(samples_collected___2 == "NP/OP") %>% group_by(samples_collected___2)  %>%
    summarise(n = n()) %>% .$n
   y <- Merscov %>% filter(consent == "Yes") %>% group_by(consent)  %>%
    summarise(n = n()) %>% .$n
  np_serum <- s+n
 np_serumrate <- (np_serum/(y*2))*100
  shinydashboard::valueBox(
    np_serumrate, "Samples success rate",color = "purple"
 )
})

output$age_pyramids <- renderPlot({
  Merscov$age_groups <- cut(all_data$calcage,
                             breaks = c(-Inf,10,20,30,40,50,60,70,80,Inf),
                             labels = c("0-9","10-19","20-29",
                                        "30-39", "40-49",
                                        "50-59","60-69",
                                        "70-79","80+"),
                             right = FALSE)
  all_data3 <- Merscov %>% select(age_groups,sex)
  all_grouping <- all_data3 %>% group_by(age_groups,sex)%>% summarise(n = n())
  ggplot(data = all_grouping, 
         mapping = aes(x = ifelse(test = sex == "Male", yes = -n, no = n), 
                       y = age_groups, fill = sex)) +
    geom_col() +
    scale_x_symmetric(labels = abs) +
    labs(x = "Enrolled Participants", y = "Age Groups")
  
})
output$fever <- renderPlot({
  fever_all <- Merscov%>% filter(fever_scr != "NA") %>% group_by(fever_scr) %>% summarise(n = n())
  ggplot(fever_all, aes(x = fever_scr, y = n, fill = fever_scr)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = 0)+
    labs(x="Fever Present",y="Enrolled Participants")
})

output$inclusion <- renderPlot({
  inclusionlong <- Merscov%>% filter(consent == "Yes") %>% select(starts_with("inclusion___")) %>%
    pivot_longer(cols = inclusion___1:inclusion___5, 
                 names_to = "inclusion", values_to = "symptoms")%>% filter(symptoms != "NA")%>%
    group_by(symptoms) %>% summarise(n=n())
  inclusionlong <- inclusionlong %>% 
    mutate(
      symptoms = case_when(
        # criteria                         # new value
        symptoms == "Cough OR difficulty in breathing OR reduced oxygen saturation (SpO2 < 90) OR chest in-drawing (for those < 5 years old)"          ~ "Cough/Breathing difficulty",
        symptoms == "Diarrhea (2 or more loose stools in a 24-hour window)"              ~ "Diarrhea",
        symptoms == "In the last 14 days has anyone else in your household/classroom/workplace/social gathering/herd been ill with a respiratory illness (cough, chest pain, sore throat, running nose, nasal stuffiness, fever)?"        ~ "Contact with other patients",
        symptoms == "Is the patient a healthcare worker and has been working in an environment where patients with severe acute respiratory infections are being cared for '"  ~ "Healthcare Worker",
        TRUE                             ~ symptoms
      )
    )
  
  #levels(inclusionlong$symptoms) <- c('Cough', 'Diarrhea', 'Contact with others','Healthcare Worker')
  ggplot(inclusionlong, aes(x = n, y = symptoms)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = 0)+
    labs(x="Number of Participants",y="Inclusion Indicators")
})

output$camel <- renderPlot({
  camel <- Merscov%>% filter(consent == "Yes") %>% 
    group_by(interactions) %>% # Variable to be transformed
    count() %>% 
    ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  ggplot(camel, aes(x = "", y = perc, fill = interactions)) +
    geom_col() +
    geom_label(aes(label = labels),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    guides(fill = guide_legend(title = "Interaction with Camels")) +
    coord_polar(theta = "y")
})

output$resp14 <- renderPlot({
  resp14 <- Merscov%>% filter(consent == "Yes") %>% 
    group_by(resp_illness14) %>% # Variable to be transformed
    count() %>% 
    ungroup() %>% 
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  ggplot(resp14, aes(x = "", y = perc, fill = resp_illness14)) +
    geom_col() +
    geom_label(aes(label = labels),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    guides(fill = guide_legend(title = "HH members ill with respiratory illness")) +
    coord_polar(theta = "y") 
})

output$enrolmentbygender <- renderTable({
  #Merscov%>% group_by(sex)  %>%
  #summarise(n = n())
  Merscov%>% filter(consent != "NA") %>%                                 
    tabyl(sex, consent) %>%                  #cross-tabulate counts
    adorn_totals(where = "row") %>%
    #adorn_totals(where = "col") %>%             # add a total row
    adorn_percentages(denominator = "col") %>%# convert to proportions
    adorn_pct_formatting() %>%                  # convert to percents
    adorn_ns(position = "front") %>%            # display as: "count (percent)"
    adorn_title(                                # adjust titles
      col_name = "Consent",
      row_name = "Gender",
      placement = "combined")
})

}
# Run the application 
shinyApp(ui = ui, server = server)
