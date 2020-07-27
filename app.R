#
#    Shiny app to visualize non-linear modeling
#    2020-05-26, DD
#    based on the shiny apps from GH
#

# * Load packages ----
library(shiny)
library(shinythemes)
library(markdown)
library(splines)


# * Load data ----
data <- readRDS("./data/data.rds")


# * Helper functions ----
fp.scale <- function
(
    x
)
    ### taken from package <mfp>
    ### 2008-06
{
    scale <- 1
    shift <- 0
    if (min(x) <= 0) {
        z <- diff(sort(x))
        shift <- min(z[z > 0]) - min(x)
        shift <- ceiling(shift * 10)/10
    }
    range <- mean(x + shift)
    scale <- 10^(sign(log10(range)) * round(abs(log10(range))))
    
    list(
        shift = shift,
        scale = scale
    )
}


# * UI user interface ----
ui <- #shinyUI(
      fluidPage(theme = shinytheme("cerulean"),
                withMathJax(),

    navbarPage("Visualisation of non-linear modeling applying ...", 
                   
    tabPanel("a) fractional polynomials",
            
             # Application title
             # ** FP panel ----
             titlePanel("Visualisation of fractional polynomials (FP)",  
),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     strong("Obtain first- or second-degree fractional polynomials"),
                     h5("Please type in the required information:"),
                     br(),
                     
                     sliderInput("fp_x",
                                 "Select a variable:",
                                 min = 1,
                                 max = 11,
                                 value = 5, step=1),
                     radioButtons("fp_fp1", "First FP power", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected = 1,
                                  inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                     sliderInput("fp_value1",
                                 "Coefficient:",
                                 min = -3,
                                 max = 3,
                                 value = 0, step=0.1),
                     radioButtons("fp_fp2", "Second FP power", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3, "no second FP"), selected = 1,
                                  inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                     sliderInput("fp_value2",
                                 "Coefficient:",
                                 min = -3,
                                 max = 3,
                                 value = 0, step=0.1), br(),
                     sliderInput("fp_sd_data", "Assuming the residual standard error is", value=0.2, min=0.01, max=3, step=0.01),
                     checkboxInput("fp_sd_data_plot", "how could the outcome y look like", value=FALSE),
#                     numericInput("fp_sd_data_min", "Min. value of plotted y", value = NA),
#                     numericInput("fp_sd_data_max", "Max. value of plotted y", value = NA),

                     
                     
                     br(), br(),
                     strong("An explanatory shiny app"),
                     br(),
                     HTML("developed on behalf of"),
                     a(href = "https://stratos-initiative.org/", "STRATOS"),
                     br(), br(),
                     img(src='stratos_logo.png', align = "center", width="60%", height="60%", href = "https://stratos-initiative.org/"),
                     br(), br(),
                     strong("Impressum:"),
                     br(), 
                     HTML("written by Dunkler Daniela & Georg Heinze"), 
                     br(), 
                     HTML("July 2020, version 0.0.2"),
                     br(), br(),
                     HTML("contact daniela.dunkler @ meduniwien.ac.at")
                 ),
                 
                 
                 mainPanel(
                     br(),
                     htmlOutput("fp_des_header", style = "font-size: 22px; text-align: center"), 
                     br(),
                     
                     h4("Descriptive analysis"),
                     verbatimTextOutput("fp_des"),
                     
                     plotOutput("fp_his"),
                     br(),
                     
                     h4("Pretransformation"),
                     verbatimTextOutput("fp_preTrans"), br(),
                     
                     #textOutput("fun"),
                     h4("Selected FP transformations"),
                     verbatimTextOutput("fp_fun"), 
                     
                     plotOutput("fp_plot")
                     #           tableOutput("pow"),
                 )
             )
             
             
             
             
    ),                                                        
    
  # ** lb panel ----
  tabPanel("b) linear b-splines",
         # Application title
         titlePanel("Visualisation of linear B-splines"),
         
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
           sidebarPanel(
             strong("Obtain linear b-splines (degree = 1) with 4 degrees of freedom"),
             h5("Please type in the required information:"),
             br(),
             
             sliderInput("lb_x",
                         "Select a variable:",
                         min = 1,
                         max = 11,
                         value = 5, step=1),
             sliderInput("lb_value1",
                         "Coefficient 1:",
                         min = -3,
                         max = 3,
                         value = 0, step=0.01),
             sliderInput("lb_value2",
                         "Coefficient 2:",
                         min = -3,
                         max = 3,
                         value = 0, step=0.01),
             sliderInput("lb_value3",
                         "Coefficient 3:",
                         min = -3,
                         max = 3,
                         value = 0, step=0.01),
             sliderInput("lb_value4",
                         "Coefficient 4:",
                         min = -3,
                         max = 3,
                         value = 0, step=0.01), br(),
             sliderInput("lb_sd_data", "Assuming the residual standard error is", value=0.2, min=0.01, max=3, step=0.01),
             checkboxInput("lb_sd_data_plot", "how could the outcome y look like", value=FALSE),
             
             br(), br(),
             strong("An explanatory shiny app"),
             br(),
             HTML("developed on behalf of"),
             a(href = "https://stratos-initiative.org/", "STRATOS"),
             br(), br(),
             img(src='stratos_logo.png', align = "center", width="60%", height="60%", href = "https://stratos-initiative.org/"),
             br(), br(),
             strong("Impressum:"),
             br(), 
             HTML("written by Dunkler Daniela & Georg Heinze"), 
             br(), 
             HTML("July 2020, version 0.0.2"),
             br(), br(),
             HTML("contact daniela.dunkler @ meduniwien.ac.at")
             
             
             
           ),
           
           mainPanel(
             br(),
             htmlOutput("lb_des_header", style = "font-size: 22px; text-align: center"), 
             br(),
             
             h4("Descriptive analysis"),
             verbatimTextOutput("lb_des"),
             
             plotOutput("lb_his"),
             plotOutput("lb_plot")
           )
         )
         
  ),                 

  # ** ns panel ----
    tabPanel("c) natural (restricted cubic) splines",
             # Application title
             titlePanel("Visualisation of natural (restricted cubic) splines"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     strong("Obtain natural splines with 3 degrees of freedom"),
                     h5("Please type in the required information:"),
                     br(),
                     
                     sliderInput("ns_x",
                                 "Select a variable:",
                                 min = 1,
                                 max = 11,
                                 value = 5, step=1),
                     sliderInput("ns_value1",
                                 "Coefficient 1:",
                                 min = -3,
                                 max = 3,
                                 value = 0, step=0.01),
                     sliderInput("ns_value2",
                                 "Coefficient 2:",
                                 min = -3,
                                 max = 3,
                                 value = 0, step=0.01),
                     sliderInput("ns_value3",
                                 "Coefficient 3:",
                                 min = -3,
                                 max = 3,
                                 value = 0, step=0.01), br(),
                     sliderInput("ns_sd_data", "Assuming the residual standard error is", value=0.2, min=0.01, max=3, step=0.01),
                     checkboxInput("ns_sd_data_plot", "how could the outcome y look like", value=FALSE),
                     
                     
                     
                     br(), br(),
                     strong("An explanatory shiny app"),
                     br(),
                     HTML("developed on behalf of"),
                     a(href = "https://stratos-initiative.org/", "STRATOS"),
                     br(), br(),
                     img(src='stratos_logo.png', align = "center", width="60%", height="60%", href = "https://stratos-initiative.org/"),
                     br(), br(),
                     strong("Impressum:"),
                     br(), 
                     HTML("written by Dunkler Daniela & Georg Heinze"), 
                     br(), 
                     HTML("July 2020, version 0.0.2"),
                     br(), br(),
                     HTML("contact daniela.dunkler @ meduniwien.ac.at")
                     
                     
                 ),
                 
                 mainPanel(
                     br(),
                     htmlOutput("ns_des_header", style = "font-size: 22px; text-align: center"), 
                     br(),
                     
                     h4("Descriptive analysis"),
                     verbatimTextOutput("ns_des"),
                     
                     plotOutput("ns_his"),
                     plotOutput("ns_plot")
                 )
             )
             
    ),                                                        
    
    # ** comments panel ----
    tabPanel("explanatory comments",
             # Application title
             #titlePanel("Visualisation of non-linear modeling"),
             
             mainPanel(
              includeMarkdown("explanation.html")
             )
    )                                                        # end tabPanel explanation
             
))



# * Server logic ----

server <- function(input, output) {
    
    # ** FP ----
    out_x_fp <- reactive({
        switch(input$fp_x, 
               data[,1],
               data[,2],
               data[,3],
               data[,4],
               data[,5],
               data[,6],
               data[,7],
               data[,8],
               data[,9],
               data[,10],
               data[,11])
        })
    
    out_xname_fp <- reactive({
        switch(input$fp_x, 
               "weight (kg)", 
               "bmi (kg/m**2)", 
               "waist circumference (cm)",
               "triglyceride (mmol/L)",
               "triglyceride (mmol/L)",
               "HDL cholesterol (mmol/L)",
               "HDL cholesterol (mmol/L)",
               "systolic blood pressure (mm Hg)",
               "systolic blood pressure (mm Hg)",
               "diastolic blood pressure (mm Hg)",
               "age")
    })
    
    
    out_xgender_fp <- reactive({
        switch(input$fp_x, 
               "women", 
               "men", 
               "women",
               "men",
               "women",
               "men",
               "women",
               "men",
               "women",
               "men",
               "women")
    })
    
    
    
    output$fp_des_header <- renderText({
        paste(out_xname_fp(), "measured in 1000 ", out_xgender_fp())
    })
    
    
    output$fp_des <- renderPrint({
        summary(out_x_fp())
        })
    
    
    output$fp_his <- renderPlot({
        x <- out_x_fp()
        hist(x, xlab = out_xname_fp(), main = "", breaks = 15, 
             cex.axis = 1.2, cex.lab = 1.2, las = 1,
             xaxs = "i") 
        box()
        abline(v=quantile(x, c(0.25,0.5,0.75)), lty=3, col="darkgray")
    })
    
    
    output$fp_preTrans <- renderPrint({
        pT <- fp.scale(out_x_fp())
        
        preTrans <- paste("x = ", out_xname_fp())
        if (pT$shift==1) preTrans <- paste(preTrans, " + ", pT$shift, ")")
        if (pT$scale!=1) preTrans <- paste(preTrans, "/", pT$scale)
        
        cat(preTrans)
    })
    
    
    output$fp_fun <- renderPrint({
        pow1 <- as.numeric(input$fp_fp1)
        
        trans1 <- paste("x^", pow1, sep = "")
        if(pow1 == 0) trans1 <- "log(x)"
        if(pow1 == 1) trans1 <- "x"
        
        fp_fun <- c(paste("Power1 =", pow1), 
                    paste("\n\n", paste("fp = ", input$fp_value1, "*", trans1)))
        
        
        if (input$fp_fp2 != "no second FP")   {
            pow2 <- as.numeric(input$fp_fp2)
            
            trans2 <- paste("x^", pow2, sep = "")
            if(pow2 == 0) trans2 <- "log(x)"
            if(pow2 == 1) trans2 <- "x"
            if(pow1 == pow2) trans2 <- paste(trans2, "* log(x)")
            
            fp_fun <- paste(fp_fun[1], "\nPower2 =", pow2, fp_fun[2], "+", input$fp_value2, "*", trans2)
        }
        cat(fp_fun)
            
        })
    
    
    output$fp_plot <-  renderPlot({
        # draw the fp as function of x
        x_org <- sort(out_x_fp())
        pT <- fp.scale(x_org)
        
        x <- (x_org + pT$shift) / pT$scale
        
        pow1 <- as.numeric(input$fp_fp1)
        
        fp1 <- x**pow1
        if(pow1 == 0) fp1<-log(x)
        
        
        if (input$fp_fp2 != "no second FP")   {
            pow2 <- as.numeric(input$fp_fp2)
            
            fp2 <- x**pow2
            if(pow2 == 0) fp2<-log(x)
           
            if(pow1 == pow2) fp2 <- log(x) * fp2
            
            fp <- input$fp_value1 * fp1 +  input$fp_value2 * fp2
        } else {
            fp <- input$fp_value1 * fp1
        }
        
        y_i <- fp + rnorm(length(fp), 0, input$fp_sd_data)
    
        if (input$fp_sd_data_plot == TRUE) {
            plot(x*pT$scale -pT$shift, 
                 y_i, 
                 xlab = out_xname_fp(), ylab = "outcome y", 
                 cex.axis = 1.2, cex.lab = 1.2, las = 1
#                 ylim = c(input$fp_sd_data_min, input$fp_sd_data_max)
                 )
            lines(x*pT$scale -pT$shift, fp)
        } else plot(x*pT$scale -pT$shift, 
                    fp, 
                    type="l", 
                    xlab=out_xname_fp(), ylab = "outcome y", 
                    cex.axis = 1.2, cex.lab = 1.2, las = 1 
#                    ylim = c(input$fp_sd_data_min, input$fp_sd_data_max)
                    )
        abline(v = quantile(x_org, c(0.25, 0.5, 0.75)), lty = 3, col = "darkgray")
        })
   
    
 
    # ** linB ----
    out_x_lb <- reactive({
        switch(input$lb_x, 
               data[,1],
               data[,2],
               data[,3],
               data[,4],
               data[,5],
               data[,6],
               data[,7],
               data[,8],
               data[,9],
               data[,10],
               data[,11])
    })
    
    out_xname_lb <- reactive({
        switch(input$lb_x, 
               "weight (kg)", 
               "bmi (kg/m**2)", 
               "waist circumference (cm)",
               "triglyceride (mmol/L)",
               "triglyceride (mmol/L)",
               "HDL cholesterol (mmol/L)",
               "HDL cholesterol (mmol/L)",
               "systolic blood pressure (mm Hg)",
               "systolic blood pressure (mm Hg)",
               "diastolic blood pressure (mm Hg)",
               "age")
    })
    
    
    out_xgender_lb <- reactive({
        switch(input$lb_x, 
               "women", 
               "men", 
               "women",
               "men",
               "women",
               "men",
               "women",
               "men",
               "women",
               "men",
               "women")
    })
    
    
    output$lb_des_header <- renderText({
        paste(out_xname_lb(), "measured in 1000 ", out_xgender_lb())
    })
    
    
    output$lb_des <- renderPrint({
        summary(out_x_lb())
        })
    

    output$lb_his <- renderPlot({
        x <- out_x_lb()
        hist(x, xlab = out_xname_lb(), main = "", 
             cex.axis = 1.2, cex.lab = 1.2, las = 1, xaxs = "i") 
        abline(v = quantile(x, c(0.25, 0.5, 0.75)), lty = 3, col = "darkgray")
        box()
       })
    
    
    output$lb_plot <- renderPlot({
        x <- sort(out_x_lb())
        linb <- bs(x, degree = 1, df = 4)
        y <- linb %*% c(input$lb_value1, input$lb_value2, input$lb_value3, input$lb_value4)
        y_i <- y + rnorm(length(y), 0, input$lb_sd_data)
        
        if(input$lb_sd_data_plot == TRUE) {
            plot(x, y_i, cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab = out_xname_lb(), ylab = "outcome y")
            lines(x, y)
        } else plot(x, y, type = "l", cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab = out_xname_lb(), ylab = "outcome y")
        abline(v = quantile(x, c(0.25, 0.5, 0.75)), lty = 3, col = "darkgray")
        })
    
    
    
    
    # ** ns ----
    out_x_ns <- reactive({
      switch(input$ns_x, 
             data[,1],
             data[,2],
             data[,3],
             data[,4],
             data[,5],
             data[,6],
             data[,7],
             data[,8],
             data[,9],
             data[,10],
             data[,11])
    })
    
    out_xname_ns <- reactive({
      switch(input$ns_x, 
             "weight (kg)", 
             "bmi (kg/m**2)", 
             "waist circumference (cm)",
             "triglyceride (mmol/L)",
             "triglyceride (mmol/L)",
             "HDL cholesterol (mmol/L)",
             "HDL cholesterol (mmol/L)",
             "systolic blood pressure (mm Hg)",
             "systolic blood pressure (mm Hg)",
             "diastolic blood pressure (mm Hg)",
             "age")
    })
    
    
    out_xgender_ns <- reactive({
      switch(input$ns_x, 
             "women", 
             "men", 
             "women",
             "men",
             "women",
             "men",
             "women",
             "men",
             "women",
             "men",
             "women")
    })
    
    
    
    output$ns_des_header <- renderText({
      paste(out_xname_ns(), "measured in 1000 ", out_xgender_ns())
    })
    
    
    
    output$ns_des <- renderPrint({
      summary(out_x_ns())
    })
    
    
    output$ns_his <- renderPlot({
      x <- out_x_ns()
      hist(x, xlab = out_xname_ns(), main = "", 
           cex.axis = 1.2, cex.lab = 1.2, las = 1, xaxs = "i") 
      abline(v = quantile(x, c(0.25, 0.5, 0.75)), lty = 3, col = "darkgray")
      box()
    })
    
    
    output$ns_plot <- renderPlot({
      # draw the natural spline    
      x <- sort(out_x_ns())
      natspl <- ns(x,  df = 3)
      y <- natspl %*% c(input$ns_value1, input$ns_value2, input$ns_value3)
      
      y_i <- y + rnorm(length(y), 0, input$ns_sd_data)
      
      if(input$ns_sd_data_plot == TRUE) {
        plot(x, y_i, cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab = out_xname_ns(), ylab = "outcome y")
        lines(x, y)
      } else plot(x, y, type = "l", cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab = out_xname_ns(), ylab = "outcome y")
      abline(v=quantile(x, c(0.25, 0.5, 0.75)), lty=3, col="darkgray")
    })
}


# * Run the application ----
shinyApp(ui = ui, server = server)
