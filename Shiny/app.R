library(shiny)
library(maps)
library(mapdata)
library(ggplot2)
library(RColorBrewer)
library(stringr)
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
library(shinyWidgets)
library(data.table)
dfstate <- read.csv("state.csv")
usmap <- map_data("state")
usmap$Density <- dfstate$num_of_rest[match(usmap$region,dfstate$state)]
dfword <- read.csv("food_in_all_reviews.csv")
myword_10 <- dfword[c(1:10),]
myword_100 <- dfword[c(1:100),]
dffood <- read.csv("review_food_topics.csv")
dffood$beta <- as.factor(dffood$beta)
dflong <- read.csv("total.csv")
##################
# USER INTERFACE #
##################
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  # Set a title for this shiny app
  "Project of analyzing the Yelp data",
  tabPanel(
    "Background",
    sidebarLayout(
      sidebarPanel(
        span((h6("As operating a business requires knowledge of the customer 
                 in order to adequately adapt to a changing market. Yelp gives 
                 business owners and operators lots of information about how their 
                 business appeals to the market, also provides an opportunity for
                 response to feedback from the platform that could increase customer satisfaction. ")),
             style="color:black")
      ),
      mainPanel(
        span((h2("Anaylsys method")),
             (h4("We used a method called Natural language processing(NLP) to handle the data
                 provided by yelp, it provides users four perspectives to understand how they
                 might take actions to improve their business ratings based on the data we 
                 provide.")),
             style="color:black")
      ),
    ),
  ),
  #####Page 1#####
  # The page shows jobs distribution in different states
  tabPanel(
    "Restaurants distribution",
    
    # Side bar to show topic of each chart
    sidebarLayout(
      sidebarPanel(
        span((h4("Map for Breakfast Restaurants:")),
             (h6("Lighter colors on the map indicating more restauants in the area.")),
             style="color:black"),
        br(),
        br(),
        span((h4("Data of Restaurants:")),
             (h6("Number of restaurants in each state.")),
             style="color:black"),
        br(),
        br(),
        br(),
      ),
      
      # Display the map and chart
      mainPanel(
        tabsetPanel(
          # Show the map
          tabPanel("Map for Breakfast Restaurants",
                   br(),
                   br(),
                   plotOutput("Map",height = "900", width = "1200"),
                   br(),
                   br(),
          ),
          
          # Bar chart about jobs
          tabPanel("Data of Restaurants",
                   br(),
                   br(),
                   plotlyOutput("Bar_Rests",height = "600", width = "900"),
                   br(),
                   br(),
          ),
        )
        
      )
    ),
  ), 
  #####Page 2#####
  # page about foods
  tabPanel(
    "Foods mentioned by customers",
    sidebarLayout(
      sidebarPanel(
        # The first part
        span((h4("10 most talked about foods:")),
             (h6("Top 10 foods that are mostly talked about.")),
             style = "color:black"),
        br(),
        
        # The second part
        span((h4("100 most talked about foods:")),
             (h6("Some more words that are widely talked about, can show up to 100 foods.")),
             (h6("try use the slide bar to adjust the amount of foods to see, you can use left click and
             hold to pull the scale of the plot, if the bar in the chart gets too skinny, there 
             is a autoscale function on the upper right of the plot which helps to get back to original position.")),
             style = "color:black"),
        sliderInput(
          "languages_requirement",
          "Words Amount",
          min = 1,
          max = 100,
          value = 10
        ),
        br(),
      ),
      mainPanel(
        tabsetPanel(
          # Display the top 10 words
          tabPanel("10 most talked about words",
                   plotOutput("Rest_rose",
                              height = "1000",
                              width = "1000")),
          
          # Details of more words
          tabPanel("100 most talked about words",
                   br(),
                   br(),
                   plotlyOutput("words_100",
                                height = "800",
                                width = "80%"),
                   br(),
                   br(),
          ),
        )
      )
    )
  ),
  #####Page 3#####
  # page about users
  tabPanel(
    "Customers topics",
    sidebarLayout(
      sidebarPanel(
        # The first part
        span((h4("Topics in reviews:")),
             (h6("In our analysis, we defined four topics to quantify what affects the user's ratings: customer loyalty,
                 food quality, serving speed and staff attitude. Here the charts demonstrate how each topic valued in 
                 customers rating for each star level ratings, although the mean values of topics for each star level tend 
                 to close to a quarter, we could still find that 3 stars have more outliers, meaning for 3 stars customers 
                 could easily be affected by some part of these four viewpoints.")),
             (h6("Select a number and the chart will display how value each 
                 topics affect users, from 1 star to 5 stars.")),
             style = "color:black"),
        numericInput(
          "obs","Stars:", 1, min = 1, max = 5
        ),
        br(),
        # The second part
        span((h4("Food Impact on Ratings:")),
             (h6("Here tells you how influential each food affects in four different topics")),
             (h6("Select different food and display the data in chart.")),
             style = "color:black"),
        pickerInput(
          "food_select","food:",
          choices = unique(dffood$term),
          selected = unique(dffood$term[1]),
          multiple = FALSE
        ),
        actionButton("foodselec","Generate"),
        br(),
        
        
      ),
      mainPanel(
        tabsetPanel(
          # Details of food impact user's rating
          tabPanel("Topic impact on review stars",
                   br(),
                   br(),
                   plotOutput("user_impact",
                              height = "800",
                              width = "1200"),
                   br(),
                   br(),
          ),
          # Details of food impact user's rating
          tabPanel("Food Impact on Ratings",
                   br(),
                   br(),
                   plotOutput("food_impact",
                              height = "800",
                              width = "1200"),
                   br(),
                   br(),
          ),
        )
      )
    )
  ),

  # Special format of description of the app
  navbarMenu(
    "About",
    
    # Describe the topic of the app
    tabPanel(
      "About app",
      br(),
      h2(HTML("<b>Project App </b>"),
         style="text-align:center"),
      br(),
      
      # Details
      span((h4("This app is built for stat628 module3, it uses data provided by Yelp, the purpose of this app
       is to help restaurants owners to get some conclusions from our review analysis, and this app
        introduces the data and visualize our group's finding, which could somehow help owners to figure
        out some possible ways to improve their ratings, or knowing where their restaurants have problems.")),
           br(),
           (h4("This Demo uses information visualization to display user's review analysis 
               in several states.")),
           br(),
           (h4("For bugs please contact dgao25@wisc.edu"))
      ),
    ),
    
    # Data source and introduction
    tabPanel(
      "About data",
      span((h4("data source: ")),
           (h6("https://uwmadison.app.box.com/s/2bpkip5e71nna1peb6plx54wbmlgtro8")),
      ),
      a("Data details", 
        href="https://www.yelp.com/dataset/documentation/main"),
      br(),
      br(),
      # Display several datasets
      span(h4("Several datasets filtered from the whole dataset:")),
      br(),
      dataTableOutput("data_long"),
      br(),
      br(),
      dataTableOutput("data_food"),
      br(),
      br(),
    )
  )
)

################
# SHINY SERVER #
################
server <- function(input,output, session){
  output$Map <- renderPlot({
    ggplot(usmap, aes(long, lat,group=group,fill = Density))+
      geom_polygon(color = "white")+
      geom_path(color='dark grey')+theme_minimal()+
      theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank())+
      ggtitle('States of Breakfast Restaurants used for analysis')+coord_fixed(1.3)
  })
  
  output$Bar_Rests <- renderPlotly({
    Num <- dfstate$num_of_rest
    State <- reorder(dfstate$state, dfstate$num_of_rest)
    label_position <- dfstate$num_of_rest + 80
    bar <- ggplot(dfstate, aes(x= Num , y= State))+
      geom_col(fill = '#2E8EB8', width = 0.5)+
      labs(title = "Restaurants in different states", 
           x="Number of Restaurants",y="States")+
      theme_minimal()+
      theme(plot.title = element_text(size = 20),
            panel.grid = element_blank(),
            axis.text = element_text(size=10),
            axis.title = element_text(size=15))
    ggplotly(bar)
  })
  output$Rest_rose <- renderPlot({
    ggplot(myword_10, aes(x = reorder(word,-n), y = n, 
                          fill=word)) +
      geom_bar(stat="identity",width=1,size=0.1) +
      xlab(" ")+
      ylab("Number of appearences")+
      ggtitle("10 most mentioned foods")+
      coord_polar()+
      scale_fill_manual(values = c("coffee"="#66B2FF",
                                   "cheese"="#527EB2",
                                   "chicken"="#1E90FF",
                                   "eggs"="#71B6DA",
                                   "sandwich"="#48D1CC",
                                   "salad"="#66CDAA",
                                   "sauce"="#7FFFD4",
                                   "bacon"="#99FFCC",
                                   "bread"="#2E8B57",
                                   "sweet"="#99CCFF"))+
      theme_minimal()+
      theme(plot.title = element_text(size =40),
            axis.text = element_text(size=15),
            axis.title = element_text(size=20))
  })
  # Show the 100 most mentioned words
  output$words_100 <- renderPlotly({
    Words100 <- reorder(myword_100$word,-myword_100$n)
    Tot <- myword_100$n
    Words100_nth <- myword_100[1: input$languages_requirement[1], ]
    Languages <- reorder(Words100_nth$word, -Words100_nth$n)
    Total <- Words100_nth$n
    words_num <- ggplot(Words100_nth, aes(x=Languages , y=Total))+  
      geom_bar(fill = "#2E8EB8",stat="identity",width=0.5,size=0.1)+
      labs(title = "Top 100 most mentioned words")+
      geom_text(aes(label = Total, y=Total+150))+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(size = 20),
            axis.text.x = element_text(angle = -30, size=10),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
    ggplotly(words_num)
  })
  
  observeEvent(input$foodselec,{
    output$food_impact <- renderPlot({
      filter_sample <- dffood[dffood$term == input$food_select,]
      filter_sample %>%
        ggplot(aes(factor(topic), beta)) +
        geom_bar(stat = "identity") +
        labs(x = "topics", y = expression(beta))+ 
        ggtitle("Topics distribution in selected food")+
        scale_x_discrete(labels = c("food quality","customer loyalty","staff attitude","serving speed"))+
        theme(axis.text.x=element_text(size=13))
    },height = 500, width = 600
    )
  })
  
  output$user_impact <- renderPlot({
    n <- input$obs
    user_filter_sample <- unique(dflong$stars)[n]
    dflongbuffer <- dflong
    dflongbuffer %>%
      ## filter by business/location/tag...
      filter(stars %in% user_filter_sample) %>%
      ggplot(aes(factor(topic), gamma)) +
      geom_boxplot() +
      labs(x = "topics", y = expression(gamma))+ 
      ggtitle("Topics distribution in user defined star reviews")+ 
      theme(axis.text.x=element_text(size=13))
  },height = 700, width = 1000
  )
  
  output$data_long <- renderDataTable({
    head(dflong)
  })
  
  output$data_food <- renderDataTable({
    head(dffood)
  })
}

#############
# RUN SHINY #
#############
shinyApp(ui=ui, server=server)