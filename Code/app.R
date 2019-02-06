if (!require('shiny')) install.packages("shiny")

d=data.frame("Classification"=c("Essential Fat","Athletes","Fitness","Acceptable","Obese"),
             "Bodyfat"=c("2-4%","6-13%","14-17%","18-25%","25% +"))

ui <- fluidPage(
  titlePanel("Bodyfat Calculator"),
  fluidRow(
    column(6,wellPanel(
      numericInput("num1", label = h4("Weight (lbs)"), value=180, min=0,max=400),
      numericInput("num2", label = h4("Abdomen 2 circumference (cm)"), value=90, min=0,max=180),
      numericInput("num3", label = h4("Wrist circumference (cm)"), value=20, min=0,max=30),
      helpText("Note: If you have any question, please email to yjiang258@wisc.edu")
    )),
    column(6,
           #h3("Bodyfat percentage"),
           verbatimTextOutput("value"),
           plotOutput("plot", width = 400, height = 180),
           h4("Bodyfat Percentage Categories"),
           tableOutput("table")
    )
  )
)

server <- function(input, output) {
  output$value <- renderText({ 
    bf=-24.7613-0.1056*input$num1+0.9019*input$num2-1.1457*input$num3
    if(input$num1>=0&input$num2>=0&input$num3>=0&bf>=0&bf<=40){
      paste("Bodyfat percentage: ",round(bf,1), "%", sep='')
    }
  })
  
  output$plot <- renderPlot({
    bf=-24.7613-0.1056*input$num1+0.9019*input$num2-1.1457*input$num3
    if(input$num1>=0&input$num2>=0&input$num3>=0&bf>=0&bf<=40){
      barplot(40,col="yellow",horiz=TRUE,xlab="Bodyfat Percentage",axes=F)
      axis(1,seq(0,40,10),c('0%','10%','20%','30%','40%'))
      abline(v=round(bf,1),col="red",lty=2,lwd=2)
      text(bf,1,labels=paste(round(bf,1), "%", sep=''),cex=1.5)
    }
  })
  
  output$table <- renderTable({ d })
}
shinyApp(ui = ui, server = server)