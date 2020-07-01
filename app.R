#
#    Shiny app to visualize non-linear modeling
#    2020-05-26, DD
#    based on the shiny apps from GH
#

#open issues:
#outcome is not defined, maybe use a realistic example
#which data to generate? Could also use different scenarios, e.g. with outlier (see scenario 6), etc.
#FP: preftransformation does not change with the current data


library(shiny)
library(splines)

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


gendat <- function
(
    seed
) 
{
    set.seed(seed)
    round(rnorm(1000, 130, 20))
}



# * UI ----
ui <- shinyUI(
   
    navbarPage("Non-linear modeling applying ...", 
                   
    tabPanel("a) fractional polynomials",
            
             # Application title
             titlePanel("Fractional Polynomial (FP) visualisation"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     h5(HTML("Please type in the required information:")),
                     br(),
                     
                     sliderInput("fp_seed",
                                 "Select a sample with the systolic blood pressure (mmHg) of 1000 random individuals:",
                                 min = 1,
                                 max = 100,
                                 value = 1, step=1),
                     radioButtons("fp_fp1", "First FP power", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected = 1,
                                  inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                     sliderInput("fp_value1",
                                 "Coefficient:",
                                 min = -3,
                                 max = 3,
                                 value = 0, step=0.1),
                     radioButtons("fp_fp2", "Second FP power", choices = c(-2, -1, -0.5, 0, 0.5, 1, 2, 3), selected = 1,
                                  inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                     sliderInput("fp_value2",
                                 "Coefficient:",
                                 min = -3,
                                 max = 3,
                                 value = 0, step=0.1), br(),
                     sliderInput("fp_sd_data", "Assuming the residual standard error is", value=0.2, min=0.01, max=3, step=0.01),
                     checkboxInput("fp_data", "how could the outcome y look like", value=FALSE),

                     
                     
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
                     HTML("written by Dunkler Daniela & Georg Heinze, May 2020, version 1"),
                     br(), br(),
                     HTML("contact daniela.dunkler @ meduniwien.ac.at")
                 ),
                 
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     h4("Descriptive analysis of blood pressure"),
                     verbatimTextOutput("fp_des"),
                     
                     plotOutput("fp_his"),
                     br(),
                     
                     h4("Pretransformation of blood pressure"),
                     verbatimTextOutput("fp_preTrans"), br(),
                     
                     #textOutput("fun"),
                     h4("Selected FP transformation"),
                     verbatimTextOutput("fp_fun"), 
                     
                     plotOutput("fp_plot")
                     #           tableOutput("pow"),
                 )
             )
             
             
             
             
    ),                                                        # end tabPanel FPs
    
    
    tabPanel("b) natural (restricted cubic) splines",
             # Application title
             titlePanel("Natural (restricted cubic) spline visualization"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     h5(HTML("Please type in the required information:")),
                     br(),
                     
                     sliderInput("ns_seed",
                                 "Select a sample with the systolic blood pressure (mmHg) of 1000 random individuals:",
                                 min = 1,
                                 max = 100,
                                 value = 1, step=1),
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
                     checkboxInput("ns_data", "how could the outcome y look like", value=FALSE),
                     
                     
                     
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
                     HTML("written by Dunkler Daniela & Georg Heinze, May 2020, version 1"),
                     br(), br(),
                     HTML("contact daniela.dunkler @ meduniwien.ac.at")
                     
                     
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     h4("Descriptive analysis of blood pressure"),
                     verbatimTextOutput("ns_des"),
                     plotOutput("ns_his"),
                     plotOutput("ns_plot")
                 )
             )
             
    ),                                                        # end tabPanel cubic splines
    
    tabPanel("c) linear b-splines",
             # Application title
             titlePanel("Linear B-spline visualization"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     h5(HTML("Please type in the required information:")),
                     br(),
                     
                     sliderInput("lb_seed",
                                 "Select a sample with the systolic blood pressure (mmHg) of 1000 random individuals:",
                                 min = 1,
                                 max = 100,
                                 value = 1, step=1),
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
                     checkboxInput("lb_data", "how could the outcome y look like", value=FALSE),
                     
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
                     HTML("written by Dunkler Daniela & Georg Heinze, May 2020, version 1"),
                     br(), br(),
                     HTML("contact daniela.dunkler @ meduniwien.ac.at")
                     
                     
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     h4("Descriptive analysis of blood pressure"),
                     verbatimTextOutput("lb_des"),
                     plotOutput("lb_his"),
                     plotOutput("lb_plot")
                 )
             )
             
    ),                                                        # end tabPanel b-splines
    tabPanel("explanatory comments",
             # Application title
             titlePanel("explanatory comments on non-linear modeling"),
             
             mainPanel(
                 h4("TO-DO: short text explaining the app, short explanation on the methods, add references for more information, etc."), br(),br(),br(),br(),
                 
                 h4("To be discussed:"), br(),
                 HTML("What is the goal of the app?"), br(),
                 HTML("Currently, the goal is visualization of of non-linear effects modeled with these three methods. Bot of course, this could be changed."), br(), br(),
                 HTML("The second part which shows the data based on the selected residual standard error, can also be deleted if it is too complicated to grasp."), br(), br(),
                 
                 HTML("Ideas for improvement:"), br(),
                 HTML("+ Data, prepare upt to 10 scenarios with different X. Maybe use real data from the NHANES data set. E.g. the FP pretransformation should change.
                       The scenarios can be selected by chossing between number 1 to 10. I would suggest only to use data where non-linear modeling can be used. (Otherwise
                      one would have to add some explanations.)"), br(), br(),
                 HTML("+ Maybe add a fourth tap showing an overlay plot of all three methods."), br(), br(),
                 HTML("+ Ideas, comments, suggestions are appreciated."), br(),
                 HTML("Small restriction: A shiny app should be intuitive and easy-to-understand for the user, as there is not much space for long explanations.")
             )
    )                                                        # end tabPanel explanation
             
))



# * Define server logic ----

server <- function(input, output) {
    
    # ** FP ----
    out_dat_fp <- reactive({
        gendat(seed = input$fp_seed) 
        })
    
    
    output$fp_des <- renderPrint({
        summary(out_dat_fp())
        })
    
    
    output$fp_his <- renderPlot({
        bp <- out_dat_fp()
        hist(bp, xlab = "blood pressure", main = "", cex.axis = 1.2, cex.lab = 1.2, las = 1) 
        abline(v=quantile(bp, c(0.25,0.5,0.75)), lty=3, col="darkgray")
        })
    
    output$pow<-renderTable({
        pow1<-as.numeric(input$fp1)
        pow2<-as.numeric(input$fp2)
        
        pow<-cbind(Power1=pow1, Power2=pow2)
        pow
        })
    

    output$fp_preTrans <- renderPrint({
        pT <- fp.scale(out_dat_fp())
        
        preTrans <- paste("x = blood pressure")
        if (pT$shift==1) preTrans <- paste(preTrans, " + ", pT$shift, ")")
        if (pT$scale!=1) preTrans <- paste(preTrans, "/", pT$scale)
        
        cat(preTrans)
    })
    
    
    output$fp_fun<-renderPrint({
        pow1<-as.numeric(input$fp_fp1)
        pow2<-as.numeric(input$fp_fp2)
        trans1<-paste("x^", pow1, sep="")
        if(pow1==0) trans1<-"log(x)"
        if(pow1==1) trans1<-"x"
        trans2<-paste("x^", pow2, sep="")
        if(pow2==0) trans2<-"log(x)"
        if(pow2==1) trans2<-"x"
        if(pow1==pow2) trans2<-paste(trans2,"* log(x)")
        
        cat(paste("fp = ", input$fp_value1, "*", trans1, "+", input$fp_value2, "*", trans2))
        })
    
    # draw the fp as function of x
    output$fp_plot <-  renderPlot({
        bp <- sort(out_dat_fp())
        pT <- fp.scale(bp)
        
        x <- (bp + pT$shift) / pT$scale
        
        pow1 <- as.numeric(input$fp_fp1)
        pow2 <- as.numeric(input$fp_fp2)
        
        
        fp1<-x**pow1
        if(pow1==0) fp1<-log(x)
        
        fp2<-x**pow2
        if(pow2==0) fp2<-log(x)
       
        if(pow1==pow2) fp2<-log(x)*fp2
        
        fp <- input$fp_value1 * fp1 +  input$fp_value2 * fp2
        y_i <- fp + rnorm(length(fp), 0, input$fp_sd_data)
    
        if (input$fp_data == TRUE) {
            plot(x*pT$scale -pT$shift, y_i, xlab="blood pressure", ylab = "outcome y", cex.axis = 1.2, cex.lab = 1.2, las = 1)
            lines(x*pT$scale -pT$shift, fp)
        } else plot(x*pT$scale -pT$shift, fp, type="l", xlab="blood pressure", ylab = "outcome y", cex.axis = 1.2, cex.lab = 1.2, las = 1)
        abline(v=quantile(bp, c(0.25,0.5,0.75)), lty=3, col="darkgray")
        })
   
    
    # ** ns ----
    out_dat_ns <- reactive({
        gendat(seed = input$ns_seed) 
        })
    
    output$ns_des <- renderPrint({
        summary(out_dat_ns())
        })
    
    
    output$ns_his <- renderPlot({
        bp <- out_dat_ns()
        hist(bp, xlab = "blood pressure", main = "", cex.axis = 1.2, cex.lab = 1.2, las = 1) 
        abline(v=quantile(bp, c(0.25,0.5,0.75)), lty=3, col="darkgray")
        })
    
    # draw the natural spline    
    output$ns_plot <- renderPlot({
        bp <- sort(out_dat_ns())
        natspl    <- ns(bp,  df=3)
        y <- natspl %*% c(input$ns_value1, input$ns_value2, input$ns_value3)
        
        y_i <- y + rnorm(length(y), 0, input$ns_sd_data)
        if(input$ns_data==TRUE) {
            plot(bp, y_i, cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab="blood pressure", ylab = "outcome y")
            lines(bp, y)
        } else plot(bp, y, type = "l", cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab="blood pressure", ylab = "outcome y")
        abline(v=quantile(bp, c(0.25,0.5,0.75)), lty=3, col="darkgray")
       })
    
    
    # ** linB ----
    out_dat_lb <- reactive({
        gendat(seed = input$lb_seed) 
       })
    
    output$lb_des <- renderPrint({
        summary(out_dat_lb())
        })
    

    output$lb_his <- renderPlot({
        bp <- out_dat_lb()
        hist(bp, xlab = "blood pressure", main = "", cex.axis = 1.2, cex.lab = 1.2, las = 1) 
        abline(v=quantile(bp, c(0.25,0.5,0.75)), lty=3, col="darkgray")
       })
    
    
    output$lb_plot <- renderPlot({
        bp <- sort(out_dat_lb())
        linb <- bs(bp, degree=1, df=4)
        y <- linb %*% c(input$lb_value1, input$lb_value2, input$lb_value3, input$lb_value4)
        y_i <- y + rnorm(length(y), 0, input$lb_sd_data)
        if(input$lb_data==TRUE) {
            plot(bp, y_i, cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab="blood pressure", ylab = "outcome y")
            lines(bp, y)
        } else plot(bp, y, type="l", cex.axis = 1.2, cex.lab = 1.2, las = 1, xlab="blood pressure", ylab = "outcome y")
        abline(v=quantile(bp, c(0.25,0.5,0.75)), lty=3, col="darkgray")
        })
}


# Run the application 

shinyApp(ui = ui, server = server)
