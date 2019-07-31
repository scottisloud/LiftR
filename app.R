#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(tidyr)

# Global Variables
liftIDs <- c("Front.squat", "Back.squat", "Deadlift", "Power.clean", "Clean", "Snatch", "Power.snatch", "OH.Squat", "Clean.and.jerk", "Jerk", "Press", "Push.press")
liftLabels <- c("Front squat", "Back squat", "Deadlift", "Power clean", "Clean", "Snatch", "Power snatch", "OH Squat", "Clean and jerk", "Jerk", "Press", "Push press")
observedRatios.df <- c() # It's not clear that these globally-defined objects are accessible or bring properly referenced/written to elsewhere. 
targetRatios.df <- c()
df.wide <- c()


# Define UI for weightlifting ratios calculation app
ui <- pageWithSidebar(
    
    # App title ----
    headerPanel("Calculate Weightlifting Ratios"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(
        
        # Input: user enters current 1RM lifts for specified movements
    numericInput("Front.squat", "Front squat", 150, min = 10, max = 1000),
    numericInput("Back.squat", "Back squat", 150, min = 10, max = 1000),
    numericInput("Deadlift", "Deadlift", 150, min = 10, max = 1000),
    numericInput("Power.clean", "Power clean", 150, min = 10, max = 1000),
    numericInput("Clean", "Clean", 150, min = 10, max = 1000),
    numericInput("Snatch", "Snatch", 150, min = 10, max = 1000),
    numericInput("Power.snatch", "Power snatch", 150, min = 10, max = 1000),
    numericInput("OH.squat", "Overhead squat", 150, min = 10, max = 1000),
    numericInput("Clean.and.jerk", "Clean and jerk", 150, min = 10, max = 1000),
    numericInput("Jerk", "Jerk", 150, min = 10, max = 1000),
    numericInput("Press", "Press", 150, min = 10, max = 1000),
    numericInput("Push.press", "Push press", 150, min = 10, max = 1000),
    button <- actionButton("SubmitValues", " Submit")
        ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        plotOutput("ratioPlot")  # Need to create an output$ratioPlot object in Server that corrosponds to this
    )
)


# The below calculations work but it is not clear that observedRatios.df is accessible outside of this function. When attempting to access observedRatios.df NULL is returned. 
calculateObservedRatiosFrom <- function(observedValues) {

    snatch.to.backsquat <- observedValues$Snatch/observedValues$Back.squat*100
        # Clean and jerk to back squat
    cleanjerk.to.backsquat <- observedValues$Clean.and.jerk/observedValues$Back.squat*100
        # Clean and jerk to front squat
    cleanjerk.to.frontsquat <- observedValues$Clean.and.jerk/observedValues$Front.squat*100
        # Snatch to clean and jerk
    snatch.to.cleanjerk <- observedValues$Snatch/observedValues$Clean.and.jerk*100
        # Front squat to back squat
    frontsquat.to.backsquat <- observedValues$Front.squat/observedValues$Back.squat*100
        # Power snatch to full snatch
    powersnatch.to.snatch <- observedValues$Power.snatch/observedValues$Snatch*100
        # Power clean to full clean
    powerclean.to.clean <- observedValues$Power.clean/observedValues$Clean*100
        # Clean to deadlift
    Clean.to.deadlift <- observedValues$Clean/observedValues$Deadlift*100
        # Press to push press
    press.to.pushpress <- observedValues$Press/observedValues$Push.press*100
        # Push press to jerk
    pushpress.to.jerk <- observedValues$Push.press/observedValues$Jerk*100
        # Overhead squat to back squat
    ohsquat.to.backsquat <- observedValues$OH.squat/observedValues$Back.squat*100
    
    observedRatios.df <- c(snatch.to.backsquat, cleanjerk.to.backsquat, cleanjerk.to.frontsquat, snatch.to.cleanjerk, 
                                          frontsquat.to.backsquat, powersnatch.to.snatch, powerclean.to.clean, Clean.to.deadlift, 
                                          press.to.pushpress, pushpress.to.jerk, ohsquat.to.backsquat)

}


# Becuase observedRatios.df doesn't appear to be accessible these calculations seem to fail. 
calculateTargetRatiosFrom <- function(observedRatios) {

    targetRatios.df$snatch.to.backsquat.low <- observedRatios$snatch.to.backsquat - 60
    
    
    # Clean and jerk to back squat
    targetRatios.df$cleanjerk.to.backsquat.low <- observedRatios$cleanjerk.to.backsquat -80
    
    # Clean and jerk to front squat
    targetRatios.df$cleanjerk.to.frontsquat.low <- observedRatios$cleanjerk.to.frontsquat -85
    
    # Snatch to clean and jerk
    targetRatios.df$snatch.to.cleanjerk.low <- observedRatios$snatch.to.cleanjerk -80
    
    # Front squat to back squat
    targetRatios.df$frontsquat.to.backsquat.low <- observedRatios$frontsquat.to.backsquat -85
    
    # Power snatch to full snatch
    targetRatios.df$powersnatch.to.snatch.low <- observedRatios$powersnatch.to.snatch -80
    
    # Power clean to full clean
    targetRatios.df$powerclean.to.clean.low <- observedRatios$powerclean.to.clean -80
    
    # Clean to deadlift
    targetRatios.df$Clean.to.deadlift.low <- observedRatios$Clean.to.deadlift -70
    
    # Press to push press
    targetRatios.df$press.to.pushpress.low <- observedRatios$press.to.pushpress -70
    
    # Push press to jerk
    targetRatios.df$pushpress.to.jerk.low <- observedRatios$pushpress.to.jerk -75
    
    # Overhead squat to back squat
    targetRatios.df$ohsquat.to.backsquat.low <- observedRatios$ohsquat.to.backsquat -65
    
    print("Target Ratios Low:")
    print(targetRatios.df)

}


saveData <- function(input) {
    df <- data.frame(liftIDs, input)
    df.wide <- spread(df, liftIDs, input)
    calculateObservedRatiosFrom(df.wide)
    calculateTargetRatiosFrom(observedRatios.df)

    
}

server <- function(input, output){
    observeEvent(input$SubmitValues, {
        showNotification("Visualization in progress", duration = 1, type = "message")
        inputData <- c(input$Front.squat, input$Back.squat, input$Deadlift, input$Power.clean, input$Clean, 
                       input$Snatch, input$Power.snatch, input$OH.squat, input$Clean.and.jerk, input$Jerk, 
                       input$Press, input$Push.press)
        output$dataTest <- renderText( {
            input$Front.squat
        })
        saveData(inputData)   
         })
    
    # Fill in the spot we created for a plot
    # output$ratioPlot <- renderPlot({
    #     
    #     # Render a barplot
    #     # barplot(WorldPhones[,input$region]*1000, 
    #     #         main=input$region,
    #     #         ylab="Number of Telephones",
    #     #         xlab="Year")
    #     
    #     ggplot(data=df.long, aes(x=variable, y=value, fill = Comparison)) +
    #         geom_bar(stat="identity", position=position_dodge()) +
    #         #scale_fill_manual(values = c("Above target ratio" = "dodgerblue4", "Below target ratio" = "tomato3")) +
    #         labs(title="Jess's lifting ratios, June 22, 2019", 
    #              x="Movement Comparisons", y = "Discrepancy between actual and target ratio (percentage)") +
    #         coord_flip() +
    #         scale_y_continuous(breaks=seq(-50,50,10), limits=c(-50, 50)) +
    #         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #               panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    #         scale_x_discrete(breaks=c("snatch.to.backsquat.low", "cleanjerk.to.backsquat.low","cleanjerk.to.frontsquat.low","snatch.to.cleanjerk.low",     
    #                                   "frontsquat.to.backsquat.low","powersnatch.to.snatch.low","powerclean.to.clean.low","Clean.to.deadlift.low",       
    #                                   "press.to.pushpress.low", "pushpress.to.jerk.low","ohsquat.to.backsquat.low"),
    #                          labels=c("Snatch to back squat", "Clean & jerk to back squat","Clean & jerk to front squat","Snatch to clean & jerk",  
    #                                   "Front squat to back squat","Power snatch to snatch","Power clean to clean","Clean to deadlift",       
    #                                   "Press to push press", "Push press to jerk","Overhead squat to back squat"))
    # 
    #     
    # })
    
}



shinyApp(ui, server)

#runApp("~/shinyapp")

