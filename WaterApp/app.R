#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
load(url("https://github.com/lspears14/ShinyApp/raw/master/wsd.rData"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Water source for small communities under 15,000 Population"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
                 
      selectInput(inputId = "y", 
                  label = "Primary Source",
                  choices = c("Ground water purchased", "Ground water",  "Surface water purchased", "Surface water", "Groundwater under influence of surface water", "Purchased ground water under influence of surface water source", "Unknown Primary Source")),
      
                 
      checkboxGroupInput(inputId = "x",
                  label = "Region",
                  choices = c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", 
                              "Region 6", "Region 7", "Region 8", "Region 9", "Region 10"),
                  selected = c("Region 1")),
    
 
      
      "States by region:",
      br(),"\nRegion 1: CT, ME, MA, NH, RI, VT",
      br(),"Region 2: NJ, NY, PR, US Virgin Islands",
      br(),"Region 3: DE, DC, MD, PA, VA, WV",
      br(),"Region 4: AL, FL, GA, KY, MS, NC, SC, TN",
      br(),"Region 5: IL, IN, MI, MN, OH, WI",
      br(),"Region 6: AR, LA, NM, OK, TX",
      br(),"Region 7: IA, KS, MO, NE",
      br(),"Region 8: CO, MT, ND, SD, UT, WY",
      br(),"Region 9: American Samoa, AZ, CA, Guam, HI, NV",
      br(),"Region 10: AK, ID, OR, WA",
      br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotOutput("FreqTab"),
      textOutput("selected_var")
     
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$FreqTab <- renderPlot({
    # user must select Category first
    cat<-wsd[wsd$`Primary Source` %in% input$y, ]
    reg<-cat[cat$`EPA Region` %in% input$x, ]
    ggplot(reg, aes(x = `Population Served Count`, fill=`EPA Region`, color=`EPA Region`)) + geom_density(alpha = .0, size=1) + theme_classic() + ylab("Number of Communitiee") + xlim(1, 15000)+ scale_y_continuous(name = "Percent", labels =scales::percent)
    
  
    
    
    })
    output$selected_var <- renderText({
      HTML(paste("You have selected", input$x, ". " ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
#test