# Check the packages and install if it is not exist.
if (!require(stringr)) install.packages('stringr')
if (!require(shiny)) install.packages('shiny')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(lubridate)) install.packages('lubridate')
if (!require(leaflet)) install.packages('leaflet')
if (!require(plotly)) install.packages('plotly')
if (!require(dplyr)) install.packages('dplyr')
if (!require(scales)) install.packages('scales')
if (!require(maps)) install.packages('maptools')
if (!require(rgdal)) install.packages('rgdal')
if (!require(mapproj)) install.packages('mapproj')
if (!require(classInt)) install.packages('classInt')
if (!require(RColorBrewer)) install.packages('RColorBrewer')
if (!require(sf)) install.packages('sf')
if (!require(mapdata)) install.packages('mapdata')
if (!require(maps)) install.packages('maps')
if (!require(shinyWidgets)) install.packages('shinyWidgets')
if (!require(data.table)) install.packages('data.table')

# Import library
library(stringr)
library(shiny)
library(ggplot2)
library(lubridate)
library(leaflet)
library(plotly)
library(dplyr)
library(scales)
library(maptools)
library(rgdal)
library(mapproj)
library(classInt)
library(RColorBrewer)
library(sf)
library(mapdata)
library(maps)
library(shinyWidgets)
library(data.table)

# Read the csv file and set a name.
data <- read.csv('listings2019_2022_for_A2.csv')
data_id <- read.csv('state_id.csv')

data$state[which(data$state == 'Northern Territories')] <- 'Northern Territory'
data <- data %>%filter(!(state %in% c("Overseas", "UK & Ireland")))

# Data in 2019
data$listingDate <- dmy_hm(data$listingDate)
data_year <- data %>% mutate(listing_year=year(listingDate))
data_2019 <- data_year %>% 
  filter(listing_year == "2019")

# Data in 2020
data_2020 <- data_year %>% 
  filter(listing_year == "2020")

# Data in 2021
data_2021 <- data_year %>% 
  filter(listing_year == "2021")

# Data in 2022
data_2022 <- data_year %>% 
  filter(listing_year == "2022")

# Jobs requirement in different states
state_job <- data %>% group_by(state) %>% summarise(num=n()) %>%
  filter(!(state %in% c("Overseas", "UK & Ireland")))%>%
  arrange(desc(num))

state_id <- merge(state_job, data_id, by.x="state", by.y="Region")
state_jobs <- state_id %>% arrange(id)

mapAU_simp <- readOGR(dsn='SpatialData',layer='SpatialData')
mapAU_df <- fortify(mapAU_simp)
map_jobs <- merge(mapAU_df, state_jobs, by.x='id', by.y='id')


# Rank of job classification
jobs_rank <- data %>% 
  group_by(jobClassification) %>% 
  summarise(num=n()) %>% arrange(desc(num))

# Get 10 of the most popular job classifications 
jobs_rank_10 <- jobs_rank %>% top_n(10)

# Find the data of these 10 jobs in different years
jobs_rank_by_year <- data_year %>% 
  group_by(jobClassification, listing_year) %>% 
  filter(jobClassification %in% c(jobs_rank_10$jobClassification))%>%
  summarise(num=n()) %>% arrange(desc(listing_year))

# Change the data type of column
jobs_rank_by_year$listing_year <- as.character(jobs_rank_by_year$listing_year )
 
jobs_classification <- data %>% 
  filter(jobClassification == "Information & Communication Technology") %>%
  group_by(jobSubClassification) %>% 
  summarise(num=n()) %>% arrange(desc(num))

# Find the sub-classifications of jobs
jobs_class_top <- jobs_classification %>% 
  filter(jobSubClassification != "Other")%>%
  top_n(6)


# Get a data set of languages from the whole data set
language <- data_year %>% select(53,25:49) 

language <- aggregate(language,by=list(year=language$listing_year),sum) %>% 
  select(-2) %>% as.data.frame() %>% 
  t() %>% as.data.frame()
colnames(language)<-language[1,]

# Different job requirements in different years
year_vs_number <- language[-1,] %>% mutate(programming_requirement=rownames(language[-1,])) 

year_vs_number <- language[-1,] %>% 
  mutate(programming_requirement=rownames(language[-1,])) %>% 
  mutate(total=rowSums(year_vs_number[,1:4])) %>%
  arrange(desc(total))

# Find top 8 of the most popular language 
year_num_top8 <- year_vs_number%>% top_n(8)


# To reset the data set of language and years
year_vs_number_backup <- year_vs_number[, !colnames(year_vs_number) %in% c("programming_requirement","total")]

data_rownames <- rownames(year_vs_number_backup)
data_colnames <- colnames(year_vs_number_backup)
year_vs_number_backup$language <- data_rownames
year_vs_number_backup <- as.data.table(year_vs_number_backup)
year_vs_number_years <- melt(year_vs_number_backup, id.vars=c("language"))
colnames(year_vs_number_years)[1] <- "Language"
colnames(year_vs_number_years)[2] <- "Year"
colnames(year_vs_number_years)[3] <- "Number"



##################
# USER INTERFACE #
##################
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  # Set a title for this shiny app
  "Analysis of Data Science Job in Australia from 2019 to 2022",
  
  # The top page to show the topic
  tabPanel(
    "Background",
    sidebarLayout(
      sidebarPanel(
        span((h6("For graduates and job seekers")),
             style="color:black")
      ),
      mainPanel(
        span((h2("Analysis of Data Science Job in Australia from 2019 to 2022")),
             (h4("Includeing job distribution, classification and programming language requirements")),
             style="color:black")
      ),
    ),
  ),
  
  #####Page 1#####
  # The page shows jobs distribution in different states
  tabPanel(
    "Jobs distribution",
    
    # Side bar to show topic of each chart
    sidebarLayout(
      sidebarPanel(
        span((h4("Map for jobs:")),
             (h6("Lighter colors on the map indicating higher requirement")),
             style="color:black"),
        br(),
        br(),
        span((h4("Data for jobs:")),
             (h6("Number of jobs in each state")),
             style="color:black"),
        br(),
        br(),
        br(),
      ),
      
      # Display the map and chart
      mainPanel(
        tabsetPanel(
          # Show the map
          tabPanel("Map for jobs",
            br(),
            br(),
            plotOutput("Map",height = "900", width = "1200"),
            br(),
            br(),
          ),
          
          # Bar chart about jobs
          tabPanel("Data for jobs",
            br(),
            br(),
            plotlyOutput("Bar_jobs",height = "600", width = "900"),
            br(),
            br(),
          ),
        )
        
      )
    ),
  ),
  
  #####Page 2#####
  # The page shows top 10 of the most popular jobs
  tabPanel(
    "Popular 10 jobs",
    
    # Side bar shows the chart and interactions
    sidebarLayout(
      sidebarPanel(
        # Filter for the first part
        span((h4("Popular jobs top 10:")),
             (h6("Top 10 in 2019-2022.")),
             (h6("Only 2019-2022 can be selected in the filter to display the overall data.")),
             style = "color:black"),
        
        # Filter
        pickerInput(
          "jobs_these_years","Total:",
          choices = "2019-2022",
          selected = "2019-2022",
          multiple = FALSE
        ),
        br(),
        
        # Filter for the second part
        span((h4("Popular jobs each year:")),
             (h6("rRequirements jobs each year.")),
             (h6("Select different years and display the data in chart.")),
             style = "color:black"),
        pickerInput(
          "jobs_select","Year:",
          choices = unique(jobs_rank_by_year$listing_year),
          selected = unique(jobs_rank_by_year$listing_year)[1:4],
          multiple = TRUE
        ),
        br(),
        
        # The third part
        span((h4("Sub-classification of the most popular job:")),
             (h6("The sub-classification of the most popular job in these years")),
             style = "color:black"),
      ),
      
      # Main part
      mainPanel(
        tabsetPanel(
          # The first part
          tabPanel("Popular jobs top 10",
                   br(),
                   br(),
                   plotlyOutput("jobs_10", 
                                height = "600",
                                width = "1200"),
                   br(),
                   br(),
                   ),
          
          # The second part
          tabPanel("Popular jobs each year",
                   br(),
                   br(),
                   plotlyOutput("jobs_year",
                                height = "600",
                                width = "1200"),
                   br(),
                   br(),
                   ),
          
          # The third part
          tabPanel("Sub-classification of the most popular job",
                   br(),
                   br(),
                   plotlyOutput("sub_radar",
                                height = "600",
                                width = "600"),
                   br(),
                   br(),
                  ),
        )
      )
    )
  ),
  
  #####Page 3#####
  # It shows the language requirements
  tabPanel(
    "Required languages",
    sidebarLayout(
      sidebarPanel(
        # Side bar to show filter and simple introduction
        # The first part
        span((h4("Required languages 2019-2022:")),
             (h6("Top 8 of the most popular languages")),
             style = "color:black"),
        br(),
        
        # The second part
        span((h4("Languages:")),
             (h6("Specific requirements for all 25 languages.")),
             (h6("try use the slide bar to adjust the amount of languages")),
             style = "color:black"),
        sliderInput(
          "languages_requirement",
          "Languages requirements",
          min = 1,
          max = 25,
          value = 10
        ),
        br(),
        
        # The third part
        span((h4("Languages in different years:")),
             (h6("The specific requirements of 25 languages in different years.")),
             style = "color:black"),
      ),
      mainPanel(
        tabsetPanel(
          # Display the top 8 of required languages
          tabPanel("Required languages 2019-2022",
                   plotOutput("language_rose",
                              height = "1000",
                              width = "1000")),
          
          # Details of languages
          tabPanel("Languages",
                   br(),
                   br(),
                   plotlyOutput("all_languages",
                                height = "800",
                                width = "80%"),
                   br(),
                   br(),
                   ),
          
          # Details of languages in different years
          tabPanel("Languages in different years",
                   br(),
                   br(),
                   plotlyOutput("sort_languages",
                              height = "800",
                              width = "1200"),
                   br(),
                   br(),
                   ),
        )
      )
    ),
    
  ),
  
  # Special format of description of the app
  navbarMenu(
    "About",
    
    # Describe the topic of the app
    tabPanel(
      "About app",
      br(),
      h2(HTML("<b>Topic</b>"),
         style="text-align:center"),
      br(),
      
      # Details
      span((h4("Job seekers can get a lot of conclusions from recent 
               job information and requirements to help them find their favorite jobs more easily.")),
           br(),
           (h4("This Demo uses information visualization to display posted jobs 
               in Australia from 2019 to 2022.")),
           br(),
           (h4("Some conclusion"))
      ),
    ),
    
    # Data source and introduction
    tabPanel(
      "About data",
      span((h4("Jobs data source: ")),
           (h6("https://www.kaggle.com/datasets/nomilk/data-science-job-listings-australia-20192020")),
           ),
      a("Data link", 
        href="https://www.kaggle.com/datasets/nomilk/data-science-job-listings-australia-20192020"),
      span((h4("Spatial data source: ")),
           ),
      br(),
      br(),
      # Display several datasets
      span(h4("Several datasets filtered from the whole dataset:")),
      br(),
      dataTableOutput("data_state_job"),
      br(),
      br(),
      dataTableOutput("data_jobs_rank"),
      br(),
      br(),
    )))

 


################
# SHINY SERVER #
################

server <- function(input,output, session){
  
  # Use map to show jobs distribution
  output$Map <- renderPlot({
    ggplot(map_jobs) + aes(long, lat, group=group, fill=num) + 
      geom_polygon() + geom_path(color='dark grey')+
      labs(title = "Jobs requirement in differen states from 2019 to 2022", 
           x="Longitude",y="Latitude")+
      theme_minimal()+
      theme(panel.grid = element_blank(),
            plot.title = element_text(size = 35),
            axis.text = element_text(size=15),
            axis.title = element_text(size=20))
  })
  
  # Bar chart to display different states information
  output$Bar_jobs <- renderPlotly({
    Num <- state_jobs$num
    State <- reorder(state_jobs$state, state_jobs$num)
    label_position <- state_jobs$num + 80
    bar <- ggplot(data = state_jobs, aes(x= Num , y= State))+
      geom_col(fill = '#2E8EB8', width = 0.5)+
      labs(title = "Jobs in different states from 2019 to 2022", 
           x="Number of jobs",y="States")+
      geom_text(aes(label = num, x=label_position))+
      theme_minimal()+
      theme(plot.title = element_text(size = 20),
            panel.grid = element_blank(),
            axis.text = element_text(size=10),
            axis.title = element_text(size=15))
    ggplotly(bar)
  })
  
  # Show the top 10 of job classifications in line chart
  output$jobs_10  <- renderPlotly({
    jobs_rank_10 %>%
      ungroup()%>%
      arrange(jobClassification)%>%
      plot_ly(
        x=~jobClassification,
        y=~num,
        type = "scatter",
        mode = "lines+markers",
        fill = '#2E8EB8',
        linetype = 1,
        line = list(width=3),
        hovertemplate = paste("<br>Job: </br>%{x}",
                              "<br>Number: %{y}")
      ) %>%
      layout(title="Jobs requirement from 2019 to 2022")
  }) 
  
  # Show these 10 classifications in different years
  year_to_choose <- reactive(
    filter(jobs_rank_by_year, listing_year %in% input$jobs_select)
  )
  
  output$jobs_year <- renderPlotly({
    year_to_choose() %>%
      ungroup()%>%
      arrange(jobClassification)%>%
      plot_ly(
        x=~jobClassification,
        y=~num,
        color = ~listing_year,
        type = "scatter",
        mode = "lines+markers",
        linetype = ~listing_year,
        line = list(width=2),
        hovertemplate = paste("<br>Job: </br>%{x}",
                              "<br>Number: %{y}")
      ) %>%
      layout(title="Jobs requirement in different years")
  })
  
  # Get the top 1 job classification and show its sub-classifications
  output$sub_radar <- renderPlotly({
    plot_ly(
      type = "scatterpolar",
      mode = "markers",
      fill = "toself"
    ) %>%
      add_trace(
        r = c(jobs_class_top$num,jobs_class_top$num[1]),
        theta = c(jobs_class_top$jobSubClassification,jobs_class_top$jobSubClassification[1]),
        name = "Top 6 of jobs",
        hovertemplate = paste("<br>Job sub-classification: %{theta}",
                              "<br>Number: %{r}")
      ) %>%
      layout(title="Sub-classification of Information & Communication Technology",
             showlegend = F)
  })
  
  # The top 8 of required languages
  output$language_rose <- renderPlot({
    ggplot(year_num_top8, aes(x = reorder(programming_requirement,-total), y = total, 
                              fill=programming_requirement)) +
      geom_bar(stat="identity",width=1,size=0.1) +
      xlab(" ")+
      ylab("Number of requirement")+
      ggtitle("The required languages of jobs")+
      coord_polar()+
      geom_text(aes(label=total),y=1500)+
      scale_fill_manual(values = c("Python"="#6495ED",
                                   "SQL"="#527EB2",
                                   "R"="#1E90FF",
                                   "Tableau"="#71B6DA",
                                   "SAS"="#48D1CC",
                                   "Matlab"="#66CDAA",
                                   "Hadoop"="#7FFFD4",
                                   "Spark"="#2E8B57"))+
      theme_minimal()+
      theme(plot.title = element_text(size =40),
            axis.text = element_text(size=15),
            axis.title = element_text(size=20))
  })
  
  # Show the total 25 languages requirements from 2019 to 2022
  output$all_languages <- renderPlotly({
    Lang <- reorder(year_vs_number$programming_requirement,-year_vs_number$total)
    Tot <- year_vs_number$total
    year_vs_number_nth <- year_vs_number[1: input$languages_requirement[1], ]
    Languages <- reorder(year_vs_number_nth$programming_requirement, -year_vs_number_nth$total)
    Total <- year_vs_number_nth$total
    jobs_lang <- ggplot(year_vs_number_nth, aes(x=Languages , y=Total))+  
      geom_bar(fill = "#2E8EB8",stat="identity",width=0.5,size=0.1)+
      labs(title = "All languages requirement from 2019 to 2022")+
      geom_text(aes(label = Total, y=Total+150))+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size = 20),
            axis.text.x = element_text(angle = -30, size=10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
    
    ggplotly(jobs_lang)
  })
  
  # Show the languages requirements in different years
  output$sort_languages <- renderPlotly({
    LanguageType <- reorder(year_vs_number_years$Language, -year_vs_number_years$Number)
    sort_lang <- ggplot(
      data = year_vs_number_years, mapping = aes(
        x=LanguageType, fill = Year, y=Number)) + 
      geom_col(position = "dodge") +
      labs(title = "Languages from 2019 to 2022",
           x = "Language",
           y = "Number")+
      theme_minimal()+
      theme(plot.title = element_text(size = 20),
            axis.text.x = element_text(angle = -30, size=10),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size=15))
    
    ggplotly(sort_lang)
  })
  
  
  # Show two data sets after operation to the whole data set
  output$data_state_job <- renderDataTable({
    state_jobs
  })
  
  output$data_jobs_rank <- renderDataTable({
    jobs_rank
  })
  
  
}


#############
# RUN SHINY #
#############
shinyApp(ui=ui, server=server)
