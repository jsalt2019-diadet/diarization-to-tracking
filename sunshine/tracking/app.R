library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(lattice)
library(dplyr)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "Speaker tracking"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("BabyTrain", tabName = "BabyTrain", icon = icon("database")),
      menuItem("AMI", tabName = "AMI", icon = icon("database")),
      menuItem("CHiME5", tabName = "CHiME5", icon = icon("database"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "BabyTrain",
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "BABYTRAIN_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_babytrain",height=400)),
                box(plotOutput("enrollment2_babytrain",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("test_segments"),
              fluidRow(column(width = 6, textOutput("nb_non_target_test_segments_babytrain")), column(width = 6, textOutput("nb_target_test_segments_babytrain"))),
              fluidRow(
                box(plotOutput("trial1_babytrain",height=400)),
                box(plotOutput("trial2_babytrain",height=400))
              ),
                div(plotOutput("trial3_babytrain",height=400),align="center")
              ,
              fluidRow(
                box(plotOutput("trial4_babytrain",height=400)),
                box(plotOutput("trial5_babytrain",height=400))
              ),
              fluidRow(
                box(plotOutput("trial6_babytrain",height=400)),
                box(plotOutput("trial7_babytrain",height=400))
              ),
              fluidRow(
                box(plotOutput("scatter_babytrain", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Tests\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Tests\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Tests\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Tests\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of Enrollments\n", align="center")
              )
      ),
      tabItem(tabName = "AMI",
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "AMI_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_ami",height=400)),
                box(plotOutput("enrollment2_ami",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("test_segments"),
              fluidRow(column(width = 6, textOutput("nb_non_target_test_segments_ami")), column(width = 6, textOutput("nb_target_test_segments_ami"))),
              fluidRow(
                box(plotOutput("trial1_ami",height=400)),
                box(plotOutput("trial2_ami",height=400))
              ),
              div(plotOutput("trial3_ami",height=400),align="center")
              ,
              fluidRow(
                box(plotOutput("trial4_ami",height=400)),
                box(plotOutput("trial5_ami",height=400))
              ),
              fluidRow(
                box(plotOutput("trial6_ami",height=400)),
                box(plotOutput("trial7_ami",height=400))
              ),
              fluidRow(
                column(width = 12, offset= 3, plotOutput("scatter_ami", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Tests\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Tests\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Tests\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Tests\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of Enrollments\n", align="center")
              )
      ),
      tabItem(tabName = "CHiME5",
              titlePanel("Enrollments"),
              fluidRow(
                column(width = 12, align = "center", sliderTextInput(inputId = "CHIME5_enr_dur", label = "Minimal speech duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              ),
              fluidRow(
                box(plotOutput("enrollment1_chime5",height=400)),
                box(plotOutput("enrollment2_chime5",height=400))
              ),
              # fluidRow(
              #   column(width = 12, align = "center", sliderTextInput(inputId = "trial_dur", label = "Trial duration (in seconds):", choices = c(5,10,15,20,25,30,45,50,55,60)))
              # ),
              titlePanel("test_segments"),
              fluidRow(column(width = 6, textOutput("nb_non_target_test_segments_chime5")), column(width = 6, textOutput("nb_target_test_segments_chime5"))),
              fluidRow(
                box(plotOutput("trial1_chime5",height=400)),
                box(plotOutput("trial2_chime5",height=400))
              ),
              div(plotOutput("trial3_chime5",height=400),align="center")
              ,
              fluidRow(
                box(plotOutput("trial4_chime5",height=400)),
                box(plotOutput("trial5_chime5",height=400))
              ),
              fluidRow(
                box(plotOutput("trial6_chime5",height=400)),
                box(plotOutput("trial7_chime5",height=400))
              ),
              fluidRow(
                column(width = 12, offset= 3, plotOutput("scatter_chime5", width=800))
              ),
              fluidRow(
                h5("\t#NTT : Number of Target Tests\n", align="center"),
                h5("\t#NNTT : Number of Non-Target Tests\n", align="center"),
                h5("\tTSTT : average duration of the Total Speech of the Target in Tests\n", align="center"),
                h5("\tOSTT : average duration of the Overlapping Speech of the Target in Tests\n", align="center"),
                h5("\tTSE : average duration of the Target Speech of the Target in Enrollments\n", align="center"),
                h5("\t#E : number of Enrollments\n", align="center")
              )
      )
    )
  )
)

get_enrollment <- function(folder_path, enrollment_duration){
  enrollment = read.table(file.path(folder_path, paste("enrollment_", enrollment_duration, ".txt", sep="")), header = TRUE, sep = "\t", dec = ".")
  enrollment["duration"] =  as.numeric(enrollment$offset) - as.numeric(enrollment$onset)
  return(enrollment)
}

get_test_segments <- function(folder_path, trial_duration=60){
  test_segments = read.table(file.path(folder_path, paste("test_segments_", trial_duration, ".txt", sep="")), header = TRUE, sep = "\t", dec = ".")
  return(test_segments)
}

plot_enrollment <- function(enrollment){
  # Plot distribution of number of enrollments (across speakers)
  enrollment_per_speaker = aggregate(model_number ~ speaker, data=enrollment, max)
  enrollment_per_speaker["model_number"] = enrollment_per_speaker["model_number"]+1
  p1 <- ggplot(data=enrollment_per_speaker, mapping=aes(model_number)) + 
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) + 
    xlab("number of enrollments") + ylab("number of speakers") + ggtitle("Distribution of the number of enrollments")
  
  # Plot distribution duration of enrollment
  dur_per_enrollment = aggregate(duration~speaker+model_number, enrollment, sum)
  p2 <- ggplot(data=dur_per_enrollment,  mapping=aes(duration)) +
    geom_histogram(color='brown', fill='cadetblue', bins=50, closed = "left", boundary=0) +
    ylab("number of enrollments") + xlab("duration (s)") +
    ggtitle("Distribution of the duration of speech pronounced by the target speaker")
  
  return(list("p1" = p1, "p2" = p2))
}

plot_test_segments <- function(test_segments){
  # Distribution of the number of non-target test_segments
  non_target_test_segments = test_segments[test_segments["duration_total_speech"] == 0,]
  non_target_test_segments = count(non_target_test_segments, target_speaker)
  
  p1 <- ggplot(data=non_target_test_segments,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("number of non-target test segments") + ylab("number of speakers") +
    ggtitle("Distribution of the number of non-target test segments")
  
  # Distribution of the number of target test_segments
  target_test_segments = test_segments[test_segments["duration_total_speech"] != 0,]
  nb_target_test_segments = count(target_test_segments, target_speaker)
  
  p2 <- ggplot(data=nb_target_test_segments,  mapping=aes(n)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("number of target test segments") + ylab("number of speakers") +
    ggtitle("Distribution of the number of target test segments")
  
  # Distribution of speech (overlapping+clean) (s)
  p3 <- ggplot(data=target_test_segments,  mapping=aes(duration_total_speech)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("Duration (s)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of speech (overlapping+clean) in target test segments")
  
  # Distribution of overlapping speech duration (s)
  p4 <- ggplot(data=target_test_segments,  mapping=aes(duration_overlapping_speech)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    #geom_histogram(color='brown', fill='red', mapping = aes(duration_clean_speech)) +
    xlab("Duration (s)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of overlapping speech (in s) in target test segments")
  
  # Distribution of overlapping speech duration (%)
  target_test_segments["duration_overlapping_percent"] = target_test_segments["duration_overlapping_speech"] * 100 / target_test_segments["duration_total_speech"]
  p5 <- ggplot(data=target_test_segments,  mapping=aes(duration_overlapping_percent)) +
    geom_histogram(color='brown', fill='cadetblue', bins=50, closed = "left", boundary=0) +
    #geom_histogram(color='brown', fill='red', mapping = aes(duration_clean_speech)) +
    xlab("Duration (%)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of overlapping speech (in %) in target test segments")
  
  # Distribution of clean speech duration (s)
  target_test_segments["duration_clean_speech"] = target_test_segments["duration_total_speech"] - target_test_segments["duration_overlapping_speech"]
  p6 <- ggplot(data=target_test_segments,  mapping=aes(duration_clean_speech)) +
    geom_histogram(color='brown', fill='cadetblue',bins=50, closed = "left", boundary=0) +
    xlab("Duration (s)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of clean speech (in s) in target test segments")
  
  # Distribution of clean speech duration (%)
  target_test_segments["duration_clean_percent"] = target_test_segments["duration_clean_speech"] * 100 / target_test_segments["duration_total_speech"]
  p7 <- ggplot(data=target_test_segments,  mapping=aes(duration_clean_percent)) +
    geom_histogram(color='brown', fill='cadetblue', bins=50, closed = "left", boundary=0) +
    xlab("Duration (%)") + ylab("number of target test segments") +
    ggtitle("Distribution of the amount of clean speech (in %) in target test segments")
  
  return(list("p1" = p1, "p2" = p2, "p3" = p3,
              "p4" = p4, "p5" = p5, "p6" = p6,
              "p7" = p7))
}

plot_scatter <- function(test_segments, enrollment, bbt=FALSE){
  # At the file scale
  # Let's compute number of target test_segments vs number of non-target test_segments
  test_segments[c("target","non_target")] = 0
  test_segments[test_segments["duration_total_speech"] != 0,"target"] = 1
  test_segments[test_segments["duration_total_speech"] == 0,"non_target"] = 1
  nb_test_segments = aggregate(.~filename, test_segments, sum)[c("filename", "target", "non_target")]
  colnames(nb_test_segments) = c("filename", "number_target_test_segments", "number_non_target_test_segments")
  
  # The average duration of total speech and overlapping speech
  average_duration_tri = aggregate(.~filename, test_segments, mean)[c("filename", "duration_total_speech", "duration_overlapping_speech")]
  colnames(average_duration_tri) = c("filename", "average_duration_total_speech_test_segments", "average_duration_clean_speech_test_segments")
  
  # The average duration of speech in enrollment for the target speaker
  aggregated_enrollment = aggregate(duration~filename+model_number+speaker, enrollment, sum)
  avg_duration_enr = aggregate(duration~filename, aggregated_enrollment, mean)
  colnames(avg_duration_enr) = c("filename", "average_duration_speech_target_speaker_enrollment")
  
  # The number of enrollment
  nb_enr = aggregate(model_number~filename,aggregated_enrollment, length)
  colnames(nb_enr) = c("filename", "number_enrollments")
  
  scatter = merge(nb_test_segments, average_duration_tri, by="filename")
  scatter = merge(scatter, avg_duration_enr, by="filename")
  scatter = merge(scatter, nb_enr, by="filename")
  colnames(scatter) = c("filename", "#NTT", "#NNTT", "TSTT", "OSTT", "TSE", "#E")
  
  if(bbt){
    alpha=.1
  } else {
    alpha=.3
  }
  p1 <- splom(scatter[2:7], pch=20, col=alpha("black",alpha), pscales=0)
  return(p1)
}

server <- function(input, output) {
  enrollment_duration_bbt = reactive({input$BABYTRAIN_enr_dur})
  enrollment_duration_ami = reactive({input$AMI_enr_dur})
  enrollment_duration_chime5 = reactive({input$CHIME5_enr_dur})
  
  # AMI
  ami_path = "data/ami"
  ami_test_segments = reactive({
    get_test_segments(ami_path)
  })
  ami_enrollment = reactive({
    get_enrollment(ami_path, enrollment_duration_ami())
  })
  
  enrollment_plots = reactive({
    plot_enrollment(ami_enrollment())
  })
  trial_plots = reactive({
    plot_test_segments(ami_test_segments())
  })
  
  output$enrollment1_ami = renderPlot({enrollment_plots()$p1})
  output$enrollment2_ami = renderPlot({enrollment_plots()$p2})
  
  output$nb_non_target_test_segments_ami <- renderText({paste("Number of non-target test segments : " , nrow(ami_test_segments()[ami_test_segments()["duration_total_speech"] == 0,]))})
  output$nb_target_test_segments_ami <- renderText({paste("Number of target test segments : " , nrow(ami_test_segments()[ami_test_segments()["duration_total_speech"] != 0,]))})
  
  output$trial1_ami <- renderPlot({trial_plots()$p1})
  output$trial2_ami <- renderPlot({trial_plots()$p2})
  output$trial3_ami <- renderPlot({trial_plots()$p3})
  output$trial4_ami <- renderPlot({trial_plots()$p4})
  output$trial5_ami <- renderPlot({trial_plots()$p5})
  output$trial6_ami <- renderPlot({trial_plots()$p6})
  output$trial7_ami <- renderPlot({trial_plots()$p7})
  
  output$scatter_ami <- renderPlot({plot_scatter(ami_test_segments(), ami_enrollment())})
  
  # CHiME5
  chime5_path = "data/chime5"
  chime5_test_segments = reactive({
    get_test_segments(chime5_path)
  })
  chime5_enrollment = reactive({
    get_enrollment(chime5_path, enrollment_duration_chime5())
  })

  chime5_enrollment_plots = reactive({
    plot_enrollment(chime5_enrollment())
  })
  chime5_trial_plots = reactive({
    plot_test_segments(chime5_test_segments())
  })
  
  output$enrollment1_chime5 = renderPlot({chime5_enrollment_plots()$p1})
  output$enrollment2_chime5 = renderPlot({chime5_enrollment_plots()$p2})
  
  output$nb_non_target_test_segments_chime5 <- renderText({paste("Number of non-target test segments: " , nrow(chime5_test_segments()[chime5_test_segments()["duration_total_speech"] == 0,]))})
  output$nb_target_test_segments_chime5 <- renderText({paste("Number of target test segments : " , nrow(chime5_test_segments()[chime5_test_segments()["duration_total_speech"] != 0,]))})
  
  output$trial1_chime5 <- renderPlot({chime5_trial_plots()$p1})
  output$trial2_chime5 <- renderPlot({chime5_trial_plots()$p2})
  output$trial3_chime5 <- renderPlot({chime5_trial_plots()$p3})
  output$trial4_chime5 <- renderPlot({chime5_trial_plots()$p4})
  output$trial5_chime5 <- renderPlot({chime5_trial_plots()$p5})
  output$trial6_chime5 <- renderPlot({chime5_trial_plots()$p6})
  output$trial7_chime5 <- renderPlot({chime5_trial_plots()$p7})
  
  
  output$scatter_chime5 <- renderPlot({plot_scatter(chime5_test_segments(), chime5_enrollment())})
  
  # babytrain
  babytrain_path = "data/babytrain"
  babytrain_test_segments = reactive({
    get_test_segments(babytrain_path)
  })
  babytrain_enrollment = reactive({
    get_enrollment(babytrain_path, enrollment_duration_bbt())
  })
  
  babytrain_enrollment_plots = reactive({
    plot_enrollment(babytrain_enrollment())
  })
  babytrain_trial_plots = reactive({
    plot_test_segments(babytrain_test_segments())
  })

  output$enrollment1_babytrain = renderPlot({babytrain_enrollment_plots()$p1})
  output$enrollment2_babytrain = renderPlot({babytrain_enrollment_plots()$p2})
  
  output$nb_non_target_test_segments_babytrain <- renderText({paste("Number of non-target test segments : " , nrow(babytrain_test_segments()[babytrain_test_segments()["duration_total_speech"] == 0,]))})
  output$nb_target_test_segments_babytrain <- renderText({paste("Number of target test segments : " , nrow(babytrain_test_segments()[babytrain_test_segments()["duration_total_speech"] != 0,]))})
  
  output$trial1_babytrain <- renderPlot({babytrain_trial_plots()$p1})
  output$trial2_babytrain <- renderPlot({babytrain_trial_plots()$p2})
  output$trial3_babytrain <- renderPlot({babytrain_trial_plots()$p3})
  output$trial4_babytrain <- renderPlot({babytrain_trial_plots()$p4})
  output$trial5_babytrain <- renderPlot({babytrain_trial_plots()$p5})
  output$trial6_babytrain <- renderPlot({babytrain_trial_plots()$p6})
  output$trial7_babytrain <- renderPlot({babytrain_trial_plots()$p7})
  
  output$scatter_babytrain <- renderPlot({plot_scatter(babytrain_test_segments(), babytrain_enrollment(), bbt=TRUE)})
}

shinyApp(ui, server)

