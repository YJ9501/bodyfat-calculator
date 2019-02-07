if (!require('shiny')) install.packages("shiny")

d=data.frame("Classification"=c("Essential Fat","Athletes","Fitness","Acceptable","Obese"),
             "Bodyfat"=c("2-4%","6-13%","14-17%","18-25%","25% +"))

ui <- fluidPage(
  titlePanel("Bodyfat Calculator"),
  fluidRow(
    column(6,wellPanel(
      numericInput("num1", label = h4("Weight (lbs)"), value=180, min=0,max=500),
      numericInput("num2", label = h4("Abdomen 2 circumference (cm)"), value=90, min=0,max=200),
      numericInput("num3", label = h4("Wrist circumference (cm)"), value=20, min=0,max=30),
      submitButton("Calculate"), 
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
    bf=-32.2227-2.4864*(input$num1/100)^2+0.8977*input$num2-1.3005*input$num3
    if(input$num1>0&input$num2>0&input$num3>0&bf > 0 & bf < 100){
      paste("Bodyfat percentage: ",round(bf,1), "%", sep='')
    }else if(bf < 0){
      print('Error: Negative body fat detected. Please input correct figures.')
    }else{
      'Error: Negative or zero body figure detected. Please input none negative body figures.'
    }
  })
  
  output$plot <- renderPlot({
    if(input$num1>0 & input$num2>0 & input$num3>0 & bf > 0 & bf < 100){
      bf=-32.2227-2.4864*(input$num1/100)^2+0.8977*input$num2-1.3005*input$num3
      barplot(40,col="yellow",horiz=TRUE,xlab="Bodyfat Percentage",axes=F)
      axis(1,seq(0,40,10),c('0%','10%','20%','30%','40%'))
      abline(v=round(bf,1),col="red",lty=2,lwd=2)
      text(bf,1,labels=paste(round(bf,1), "%", sep=''),cex=1.5)
    }
  })
  
  output$table <- renderTable({ d })
}
shinyApp(ui = ui, server = server)