######################################################################################
## HSA Future Value Application                                                     ##                                     ##
#######################################################################################



####################################
# Load Libraries               #
####################################


library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)


####################################
# User Interface                   #
####################################




ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("HSA Value Calculator",
                           
                           #This is the UI code for the tab panel which allows users to enter the input parameters
                           
                           tabPanel("Home", 
                                    # Input parameters to be entered by the user
                                    sidebarPanel(
                                      HTML("<h4>Input Parameters</h4>"),
                                      numericInput("a", #This variable holds the years before retirement value entered by the user
                                                   label = "Years before retirement ($)", 
                                                   value = 1 
                                                   ,min = 0, 
                                                   max = 45
                                      ),
                                      numericInput("b", #This variable holds the yearly HSA contribution entered by the user
                                                   label = "Yearly HSA contribution ($)", 
                                                   value = 1000, 
                                                   min = 0, 
                                                   max = 7500,
                                                   step = 100),
                                      numericInput("c", #This variable holds the yearly healthcare expenses entered by the user
                                                   label = "Yearly health care expenses ($)", 
                                                   value = 500, 
                                                   min = 0, 
                                                   max = 12000,
                                                   step = 100),
                                      sliderInput("d", #This variable holds the rate of return percentage entered by the user
                                                  label = "Annual rate of return (%)", 
                                                  value = 1, 
                                                  min = 0, 
                                                  max = 15,post  = " %"),
                                      sliderInput("e", #This variable holds the federal tax percentage entered by the user
                                                  label = "Federal Tax Bracket (%)", 
                                                  value = 1, 
                                                  min = 0, 
                                                  max = 30, post  = " %"),
                                      sliderInput("f", #This variable holds the state tax percentage entered by the user
                                                  label = "State tax (%) ", 
                                                  value = 1, 
                                                  min = 0, 
                                                  max = 15, post  = " %")
                                    ),
                                    
                                    #This is the UI code for the main panel which displays the Future Value table, Tax Savings calculations and the Graph
                                    
                                    mainPanel(
                                      tags$label(h2('HSA Future Value Table')),  #This part displays the Future Value Table 
                                      DT::dataTableOutput("tabledata"),
                                      
                                      
                                      
                                      
                                      tags$label(h2('Tax Savings with HSA')),   #This part displays the Tax Savings Calculations
                                      h4(htmlOutput("tax_output"),
                                         h4(htmlOutput("tax_output2")),
                                         
                                        
                                         
                                         
                                         tags$label(h2('HSA Future Value Chart')),  #This part displays the Future Value Chart
                                         h4(htmlOutput("final_output")),
                                   
                                         plotOutput("bar",height=700)
                                         
                                         
                                      ) # mainPanel()
                                      
                                    ) #tabPanel(), Home
                                    
                                   
                                    
                           ) 
                           
                ) # navbarPage()
)# fluidPage()




####################################
# Server                           #
####################################


#This is the server function for the application 

server <- function(input, output, session) {
  
  
#Here we create a data frame to store the values of the future value table
  
  df <- data.frame(year=integer(),     
                   future_value=numeric(),
                   amount_spent=numeric(), 
                   amount_saved=numeric())
  
  
#This function is defined to calculate the HSA future value and populate the future value table with the results obtained
  
  calculate <- function(a,b,c,d,e,f){
    
    
    
    a<- as.numeric(input$a)
    
    savings<-(b-c)*a #Calculates Net HSA Contribution for the entered number of years before retirement by the user
    
    for(i in 1:input$a)  #Calculates future value for each year
    {
      
      if (i==1)
      {
        future_value<- savings
      }
      else
      {
        future_value<- savings+(d*0.01)*savings
      }
      savings<-future_value  
      
      
      #Amount saved is calculated here
      amount_saved<-(b-c)*i
      
      #Amount spent is calculated here
      amount_spent <- c*i
      
      
    #Data frame to display the output in a tabular form
      
      df[nrow(df) + 1,] = c(i,amount_spent,amount_saved, round(future_value,2))
    }#for loop
    
    
    
    #Table Headers are defined here
    
    names(df) = c("Year","Amount Spent ($)","Amount Saved ($)", "HSA Future Value ($)")
    return(df) #Output table
    
    
    
    
  }
  
  
  #This function is defined to calculate the Tax Savings due to HSA Contribution 
  
  output$tax_output <- renderUI({ 
    
    #Convert contribution to $ and federal and state tax brackets to %
    
    b = as.numeric(gsub('[$,]', '', input$b))
    e = as.numeric(gsub('[%,]', '', input$e))
    f = as.numeric(gsub('[%,]', '', input$f))
    a=as.numeric(input$a)
    
  #Calculates the HSA Tax savings on Contribution
    op <- input$b* (input$e+input$f)* (0.01)*input$a 

    
  #Output is printed and displayed as an HTML output
    HTML(paste(" Savings on HSA Contribution: $",round(op,2)))
    
    
  })
  
  #This function is defined to calculate the Tax Savings due to HSA Earnings
  
  output$tax_output2 <- renderUI ({ 
    
    #Calculates the Net HSA Contribution
    
    savings_init<-(input$b-input$c)*(input$a)
    savings_calc<-(input$b-input$c)*(input$a)
    
    #Convert contribution to $ and federal and state tax brackets to %
    
    b = as.numeric(gsub('[$,]', '', input$b))
    e = as.numeric(gsub('[%,]', '', input$e))
    f = as.numeric(gsub('[%,]', '', input$f))
    a=as.numeric(input$a)
    print(b)
    
    
    for(i in 1:input$a)
    {
      
      if (i==1)
      {
        future_value_tax<- savings_init
      }
      else
      {
        future_value_tax<- savings_init+(input$d*0.01)*savings_init
      }
      savings_init<-future_value_tax
    }

    #Calculates the HSA Tax savings on Earnings
    
    op1<- (future_value_tax -savings_calc)*(input$e+input$f)* (0.01)
    op1
    
    #Output is printed and displayed as an HTML output
    
    HTML(paste(" Savings on HSA Earnings: $",round(op1,2)))
    
  })
  
  #This function is defined to calculate the total Tax Savings due to HSA Contribution and HSA Earnings
  
  output$final_output <- renderUI({ 
    
  #Convert contribution to $ and federal and state tax brackets to %
    
    b = as.numeric(gsub('[$,]', '', input$b))
    e = as.numeric(gsub('[%,]', '', input$e))
    f = as.numeric(gsub('[%,]', '', input$f))
    a=as.numeric(input$a)
    
    # Calculates Tax Savings due to HSA Contribution
    
    op2 <- input$b* (input$e+input$f)* (0.01)*input$a
    
    
    #Calculates the Net HSA Contribution
    
    savings_init_final<-(input$b-input$c)*(input$a)
    savings_calc_final<-(input$b-input$c)*(input$a)
    b = as.numeric(gsub('[$,]', '', input$b))
    e = as.numeric(gsub('[%,]', '', input$e))
    f = as.numeric(gsub('[%,]', '', input$f))
    a=as.numeric(input$a)
    print(b)
    
    
    for(i in 1:input$a)
    {
      
      if (i==1)
      {
        future_value_tax_final<- savings_init_final
      }
      else
      {
        future_value_tax_final<- savings_init_final+(input$d*0.01)*savings_init_final
      }
      savings_init_final<-future_value_tax_final
    }
    
    # Calculates Tax Savings due to HSA Earnings
    
    op3<- (future_value_tax_final -savings_calc_final)*(input$e+input$f)* (0.01)
    op3
    
   # Total Tax Savings is computed here and displayed as an HTML output
    
    HTML(paste("<h4>With a HSA plan you could save: $", round(future_value_tax_final+op2+op3,2),"</h4>"))
   
    
    
  })
  
  
  
  # This is the function for rendering future value table output
  
  output$tabledata <- renderDataTable ({
    results <- calculate(input$a,input$b,input$c,input$d)
    dtt<-datatable(results,rownames = FALSE,options = list(dom = 'tp'))
    dtt
    
  })
  
  
 
  
  # This is the function for rendering future value graph output
  
  output$bar <- renderPlot({
    
    
    results <- calculate(input$a,input$b,input$c,input$d)
    bar2 <- tapply(results[,'HSA Future Value ($)'],results[,'Year'],sum)
    indices <- 1:length(bar2)
    dat <- data.frame(indices,bar2)
    names(dat) <- c("Year","HSA Future Value ($)")
    
    if(length(bar2)<5){
      x_c <- 5
      if(length(bar2)==1)
      {
        width1 <- 0.3
      }
      else
      {
        width1 <- 0.7
      }
      }
    else{
      x_c = length(bar2) + 1
      width1 <- 0.7
    }
    
    
     p <- ggplot(data=dat, aes(x=indices, y=bar2, label = paste0(bar2,"$"))) +  geom_bar(stat="identity",width=width1,fill="#69b3a2") + geom_text(aes(label = paste0(bar2,"$")),y = bar2,size=0, hjust = 0.5, vjust=30)+scale_x_discrete(limits = indices) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=20),axis.text.x=element_text(size=20),axis.text.y=element_text(size=20))+xlab("Year")+ylab("Future Value ($)") 
    
    p
    
  })
  
  
}


####################################
# Create Shiny App                 #
####################################

shinyApp(ui = ui, server = server)
