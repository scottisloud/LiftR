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
liftIDs <- c("Front.squat", "Back.squat", "Deadlift", "Power.clean", "Clean", "Snatch", "Power.snatch", "OH.squat", "Clean.and.jerk", "Jerk", "Press", "Push.press")
liftLabels <- c("Front squat", "Back squat", "Deadlift", "Power clean", "Clean", "Snatch", "Power snatch", "OH Squat", "Clean and jerk", "Jerk", "Press", "Push press")
observedRatios.df <- c() 
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
        plotOutput("ratioPlot"),
        textOutput("observedRatioData"), # Need to create an output$ratioPlot object in Server that corrosponds to this
        textOutput("targetRatioData")
    )
)



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
    ohsquat.to.backsquat <- observedValues$OH.squat/observedValues$Back.squat*100 # TODO: this calculation isn't working for some reason?
    
    observedRatios.df <<- c(snatch.to.backsquat, cleanjerk.to.backsquat, cleanjerk.to.frontsquat, snatch.to.cleanjerk, 
                                          frontsquat.to.backsquat, powersnatch.to.snatch, powerclean.to.clean, Clean.to.deadlift, 
                                          press.to.pushpress, pushpress.to.jerk, ohsquat.to.backsquat)
print(observedValues$OH.squat)
}



calculateTargetRatiosFrom <- function(observedRatios) {
    
    
    
    snatch.to.backsquat.low <- observedRatios.df[1] - 60
    
    cleanjerk.to.backsquat.low <- observedRatios.df[2] - 80
    
    
    cleanjerk.to.frontsquat.low <- observedRatios[3] - 85
    
    
    snatch.to.cleanjerk.low <- observedRatios[4] - 80
    
    
    frontsquat.to.backsquat.low <- observedRatios[5] - 85
    
     
    powersnatch.to.snatch.low <- observedRatios[6] - 80
    
    
    powerclean.to.clean.low <- observedRatios[7] - 80
    
    
    clean.to.deadlift.low <- observedRatios[8] - 70
    
    
    press.to.pushpress.low <- observedRatios[9] - 70
    
    
    pushpress.to.jerk.low <- observedRatios[10] - 75
    

    ohsquat.to.backsquat.low <- observedRatios[11] - 65 # TODO: this calculation isn't working for some reason?
    
    targetRatios.df <<- c(snatch.to.backsquat.low, cleanjerk.to.backsquat.low, cleanjerk.to.frontsquat.low, snatch.to.cleanjerk.low, 
                          frontsquat.to.backsquat.low, powersnatch.to.snatch.low, powerclean.to.clean.low, clean.to.deadlift.low, 
                          press.to.pushpress.low, pushpress.to.jerk.low, ohsquat.to.backsquat.low)
}


saveData <- function(input) {
    df <- data.frame(liftIDs, input)
    df.wide <<- spread(df, liftIDs, input)
    calculateObservedRatiosFrom(df.wide)
    calculateTargetRatiosFrom(observedRatios.df)

    
}

server <- function(input, output){
    observeEvent(input$SubmitValues, {
        showNotification("Visualization in progress", duration = 1, type = "message")
        inputData <- c(input$Front.squat, input$Back.squat, input$Deadlift, input$Power.clean, input$Clean, 
                       input$Snatch, input$Power.snatch, input$OH.squat, input$Clean.and.jerk, input$Jerk, 
                       input$Press, input$Push.press)
        output$targetRatioData <- renderText( {
            targetRatios.df
            # observedRatios.df
        })
        output$observedRatioData <- renderText( {
            observedRatios.df
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

