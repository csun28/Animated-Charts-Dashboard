#import libraries 
library(shiny)
library(shinydashboard)
library(gganimate)
library(ggplot2)
library(stringr)
library(gifski)

#import RScripts for cleaning and recoding data 
source("newdash.R")

linebreaks <- function(n){HTML(strrep(br(), n))}

#define UI for application
ui <- fluidPage(
    #define title and inputs 
    titlePanel("Outcomes Charts"),
    selectInput("grant","Choose a Grant", choices=c("SOAR 2 Young Adult", "SOAR 3 Adult", "SOAR 3 Young Adult", "SOAR 4 Adult"), selected=""),
    #define layout of images 
    fluidRow(column(width = 6, offset = 0, style='padding:50px;', imageOutput("chart1", inline=TRUE), linebreaks(2), imageOutput("chart3", inline=TRUE)),
             column(width = 6, offset = 0, style='padding:50px;', imageOutput("chart2", inline=TRUE), linebreaks(2), imageOutput("chart4", inline=TRUE)))
)


# Run the application 
server <- function(input, output) {
  
  output$chart1 <- renderImage({
    
    #create temp .gif file to save output 
    outfile <- tempfile(fileext='.gif')
    
    #create chart based on input from UI
    Image1 <- if (input$grant == "SOAR 2 Young Adult")  {
      ggplot(data=S2Enroll, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF33CC", "#6699CC"),
                          labels=c("Enrollment Goal", "Enrollments")) +
        labs(x="Site", y="Number of Enrollments") +
        ggtitle("Enrollment Goals and Numbers by Site") +
        #coord_fixed(ratio=0.009) +
        scale_y_continuous(limits = c(0,300))
    }
    else if (input$grant == "SOAR 3 Adult") {
      ggplot(data=S3AEnroll, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#339966", "#9966CC"),
                          labels=c("Enrollment Goal", "Enrollments")) +
        labs(x="Site", y="Number of Enrollments") +
        ggtitle("Enrollment Goals and Numbers by Site") +
        #coord_fixed(ratio=0.009) +
        scale_y_continuous(limits = c(0,200))
    }
    else if (input$grant == 'SOAR 3 Young Adult') {
      ggplot(data=S3YEnroll, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FFCC66", "#FF6666"),
                          labels=c("Enrollment Goal", "Enrollments")) +
        labs(x="Site", y="Number of Enrollments") +
        ggtitle("Enrollment Goals and Numbers by Site") +
        #coord_fixed(ratio=0.009) +
        scale_y_continuous(limits = c(0,200))
    }
    else if (input$grant == 'SOAR 4 Adult') {
      ggplot(data=S4Enroll, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF9966", "#99FF66"),
                          labels=c("Enrollment Goal", "Enrollments")) +
        labs(x="Site", y="Number of Enrollments") +
        ggtitle("Enrollment Goals and Numbers by Site") +
        #coord_fixed(ratio=0.009) +
        scale_y_continuous(limits = c(0,250))
    }
    
    #animate the chart and save as .gif image in the temp file 
    anim_save("outfile.gif", animate(Image1, width = 550, height = 450, renderer = gifski_renderer())) 
    #return a list containing file name 
    list(src = "outfile.gif",
         width = input$shiny_width, height=input$shiny_height, 
         contentType = 'image/gif')}, deleteFile = TRUE)
  
  
  #repeat process for next 4 aminated images 
  output$chart2 <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
    
    Image2 <- if (input$grant == "SOAR 2 Young Adult")  {
      ggplot(data=S2IRC, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF33CC", "#6699CC"), 
                          labels=c("Industry\nRecogonized\nCredential Goal", "Attained Industry\nRecogonized\nCredential")) +
        labs(x="Site", y="Number of Participants") +
        ggtitle("Credential Goals and Numbers by Site") + 
        #coord_fixed(ratio=0.018) +
        theme(legend.key.size = unit(1.3, "cm")) +
        scale_y_continuous(limits = c(0, 150))
    }
    else if (input$grant == "SOAR 3 Adult") {
      ggplot(data=S3AIRC, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#339966", "#9966CC"), 
                          labels=c("Industry\nRecogonized\nCredential Goal", "Attained Industry\nRecogonized\nCredential")) +
        labs(x="Site", y="Number of Participants") +
        ggtitle("Credential Goals and Numbers by Site") + 
        #coord_fixed(ratio=0.018) +
        theme(legend.key.size = unit(1.3, "cm")) +
        scale_y_continuous(limits = c(0, 150))
    }
    else if (input$grant == "SOAR 3 Young Adult") {
      ggplot(data=S3YIRC, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FFCC66", "#FF6666"), 
                          labels=c("Industry\nRecogonized\nCredential Goal", "Attained Industry\nRecogonized\nCredential")) +
        labs(x="Site", y="Number of Participants") +
        ggtitle("Credential Goals and Numbers by Site") + 
        #coord_fixed(ratio=0.018) +
        theme(legend.key.size = unit(1.3, "cm")) +
        scale_y_continuous(limits = c(0, 150))
    }
    else if (input$grant == "SOAR 4 Adult") {
      ggplot(data=S4IRC, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF9966", "#99FF66"), 
                          labels=c("Industry\nRecogonized\nCredential Goal", "Attained Industry\nRecogonized\nCredential")) +
        labs(x="Site", y="Number of Participants") +
        ggtitle("Credential Goals and Numbers by Site") + 
        #coord_fixed(ratio=0.018) +
        theme(legend.key.size = unit(1.3, "cm")) +
        scale_y_continuous(limits = c(0, 150))
    }

    anim_save("outfile.gif", animate(Image2, width = 550, height = 450, renderer = gifski_renderer())) 
    list(src = "outfile.gif",
         width = input$shiny_width, height=input$shiny_height,
         contentType = 'image/gif')}, deleteFile = TRUE)
  
  
  output$chart3 <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
    
    Image3 <- if (input$grant == "SOAR 2 Young Adult") {
      ggplot(data=S2Training, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF33CC", "#6699CC"),
                          labels=c("Training Goal", "In Trainings")) +
        labs(x="Site", y="Number of Participants") +
        #coord_equal(ratio=0.0135) +
        ggtitle("Training Attendance Goals and Numbers by Site") + 
        scale_y_continuous(limits = c(0, 200)) 
    }
    else if (input$grant == "SOAR 3 Adult") {
      ggplot(data=S3ATraining, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#339966", "#9966CC"),
                          labels=c("Training Goal", "In Trainings")) +
        labs(x="Site", y="Number of Participants") +
        #coord_equal(ratio=0.0135) +
        ggtitle("Training Attendance Goals and Numbers by Site") + 
        scale_y_continuous(limits = c(0, 150)) 
    }
    else if (input$grant == "SOAR 3 Young Adult") {
      ggplot(data=S3YTraining, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FFCC66", "#FF6666"),
                          labels=c("Training Goal", "In Trainings")) +
        labs(x="Site", y="Number of Participants") +
        #coord_equal(ratio=0.0135) +
        ggtitle("Training Attendance Goals and Numbers by Site") + 
        scale_y_continuous(limits = c(0, 150)) 
    }
    else if (input$grant == "SOAR 4 Adult") {
      ggplot(data=S4Training, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF9966", "#99FF66"),
                          labels=c("Training Goal", "In Trainings")) +
        labs(x="Site", y="Number of Participants") +
        #coord_equal(ratio=0.0135) +
        ggtitle("Training Attendance Goals and Numbers by Site") + 
        scale_y_continuous(limits = c(0, 150)) 
    }
    
    anim_save("outfile.gif", animate(Image3, width = 550, height = 450, renderer = gifski_renderer())) 
    list(src = "outfile.gif",
         width = input$shiny_width, height=input$shiny_height,
         contentType = 'image/gif')}, deleteFile = TRUE)
  
  
  output$chart4 <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
    
    Image4 <- if (input$grant == "SOAR 2 Young Adult") {
      ggplot(data=S2JobP, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF33CC", "#6699CC"),
                          labels=c("Job Placement\nGoal", "Job Placements")) +
        labs(x="Site", y="Number of Participants") +
        #coord_fixed(ratio=0.0135) +
        ggtitle("Job Placement Goals and Numbers by Site") + 
        theme(legend.key.size = unit(0.8, "cm")) +
        scale_y_continuous(limits = c(0, 200)) 
    }
    else if (input$grant == "SOAR 3 Adult") {
      ggplot(data=S3AJobP, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#339966", "#9966CC"),
                          labels=c("Job Placement\nGoal", "Job Placements")) +
        labs(x="Site", y="Number of Participants") +
        #coord_fixed(ratio=0.0135) +
        ggtitle("Job Placement Goals and Numbers by Site") + 
        theme(legend.key.size = unit(0.8, "cm")) +
        scale_y_continuous(limits = c(0, 150)) 
    }
    else if (input$grant == "SOAR 3 Young Adult") {
      ggplot(data=S3YJobP, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FFCC66", "#FF6666"),
                          labels=c("Job Placement\nGoal", "Job Placements")) +
        labs(x="Site", y="Number of Participants") +
        #coord_fixed(ratio=0.0135) +
        ggtitle("Job Placement Goals and Numbers by Site") + 
        theme(legend.key.size = unit(0.8, "cm")) +
        scale_y_continuous(limits = c(0, 150)) 
    }
    else if (input$grant == "SOAR 4 Adult") {
      ggplot(data=S4JobP, aes(x=Site, y=count, fill= Outcome)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=count), vjust=(-0.5), size=3) +
        transition_states(Outcome, transition_length = 4, state_length = 20) +
        enter_fade() + exit_shrink() +
        scale_fill_manual(values=c("#FF9966", "#99FF66"),
                          labels=c("Job Placement\nGoal", "Job Placements")) +
        labs(x="Site", y="Number of Participants") +
        #coord_fixed(ratio=0.0135) +
        ggtitle("Job Placement Goals and Numbers by Site") + 
        theme(legend.key.size = unit(0.8, "cm")) +
        scale_y_continuous(limits = c(0, 150)) 
    }
    
    anim_save("outfile.gif", animate(Image4, width = 550, height = 450, renderer = gifski_renderer())) 
    list(src = "outfile.gif",
         width = input$shiny_width, height=input$shiny_height,
         contentType = 'image/gif')}, deleteFile = TRUE)
}      

shinyApp(ui, server)
