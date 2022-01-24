fluidPage(
  titlePanel("NIH Planned Enrollment Calculator"),
  p("Generate planned enrollment tables that are racially and ethnically representative of a given region based on American Community Survey (ACS) data."),
  
  # Style for slider
  setSliderColor("#00356b", 1),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "state",
                  label = "Define Region",
                  choices = c("All", "Alabama", "Alaska", "Arizona", "Arkansas",
                              "California", "Colorado", "Connecticut", "Delaware",
                              "District of Columbia", "Florida", "Georgia", "Hawaii",
                              "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                              "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                              "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                              "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                              "New Mexico", "New York", "North Carolina", "North Dakota",
                              "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico",
                              "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                              "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                              "West Virginia", "Wisconsin", "Wyoming"), 
                  selected = "Connecticut",
                  multiple=TRUE),
      
      helpText("If no states are selected, data for the entire United States (including Puerto Rico and the District of Columbia) will be used."),
      
      numericInput(inputId = "target_n",
                   label = "Target Enrollment",
                   value = 100),
      
      sliderInput(inputId = "target_male_prop",
                  label = "Proportion of Males",
                  value = 0.5, min = 0, max = 1),
      
      selectInput(inputId = "estim_year",
                  label = "Select ACS Year",
                  c(2019, 2018, 2017, 2016, 2015)),
      
      helpText("The U.S. Census Bureau did not release its standard 2020 ACS 1-year estimates because of the impact of the COVID-19 pandemic on data collection."),
      
      selectInput(inputId = "estim_type",
                  label = "Select ACS Estimate Type",
                  choices = c("ACS 1-Year Estimates",
                              "ACS 5-Year Estimates")),
      
      helpText("The 5-year estimates from the ACS are \"period\" estimates that represent data collected over a period of time. Using multiyear estimates increases statistical reliability of the data for less populated areas and small population subgroups."),
      
      actionButton("generate", "Generate Enrollment Table"),
      
      downloadButton("download", "Download")
      
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Planned Enrollment", tableOutput("dist") %>% withSpinner(color="#00356b", size=2)),
                  tabPanel("Population Characteristics", tableOutput("pop") %>% withSpinner(color="#00356b", size=2))
      )
    )
  )
)