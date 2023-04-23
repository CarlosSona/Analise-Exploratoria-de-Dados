library(shiny)
library(ggplot2)

# Carrega os dados
#Income_Democracy <- read.csv("D:/MBA/MBA/04 - Analise Exploratoria de Dados/Projeto/DB/dados.csv")
#Income_Democracy <- read_excel("D:/MBA/MBA/04 - Analise Exploratoria de Dados/Projeto/DB/income_democracy.xlsx", sheet = 1)


#Interface UI
shinyUI <- (
  fluidPage(
  navbarPage("Análise exploratória de dados",
  tabPanel("Income_Democracy",
           p("Gráfico de linhas para seleção de variáveis"),
           mainPanel(plotOutput("Income_linha")),
           flowLayout(
             varSelectInput("variaveis_income", "Variáveis Income Democracy:",Income_Democracy, multiple = FALSE),
             selectInput('cor', label = 'Escolha uma cor:',
                         choices = c("blue", "green", "red"), selected = "red"),
             selectInput('pais', label = 'Escolha um país:', choices = unique(Income_Democracy$country), multiple = FALSE),
           ),
           numericRangeInput(inputId = "x_lim", label = "Insira valor mínimo e máximo para eixo x:",
                             value=c(min(Income_Democracy$year), max(Income_Democracy$year))
                             ),
           numericRangeInput(inputId = "y_lim", label = "Insira valor mínimo e máximo para eixo y:",
                             value=c(min(Income_Democracy$variaveis_income), max(Income_Democracy$variaveis_income))
           ),
          )
        )
      ))




# Define a lógica do servidor
shinyServer <- function(input, output){
  
  #Criando um evento reativo que gera um plot quando uma das ações relacionadas 
  #ao gráfico de linhas muda, sendo elas, eixos, cores, e variáveis
  plot_ind_dem_reativo <- eventReactive(c(input$variaveis_income, input$cor, input$x_lim, input$y_lim, input$pais),{
    #Filtro de País
    Income_Democracy <- subset(Income_Democracy, country %in% input$pais)

    #Plotando o gráfico com as definições do eixo x, de cores, etc.
    ggplot(data = Income_Democracy, aes_string(x = "year", y = input$variaveis_income)) +
      geom_line(color = input$cor) + ggplot2::xlim(input$x_lim) + ggplot2::ylim(input$y_lim) + theme_classic()
  })
  
  #Atualizando o range do y quando uma variável é trocada
  update_ylim <- eventReactive(c(input$variaveis_income),{
    if(length(input$variaveis_income) == 0) return(numericRangeInput(inputId = "y_lim", label = "Insira valor mínimo e máximo para eixo y:", value = c(min(input$variaveis_income), max(input$variaveis_income))))
    updateNumericRangeInput(inputId = "y_lim", value = c(min(Income_Democracy[,input$variaveis_income], na.rm = T), max(Income_Democracy[,input$variaveis_income], na.rm = T))) 
  })
  
  update_ylim <- eventReactive(c(input$variaveis_income),{
    if(length(input$variaveis_income) == 0) return(numericRangeInput(inputId = "y_lim", label = "Insira valor mínimo e máximo para eixo y:", value = c(min(Income_Democracy$year), max(Income_Democracy$year))))
    updateNumericRangeInput(inputId = "y_lim", value = c(min(Income_Democracy[,input$variaveis_income], na.rm = T), max(Income_Democracy[,input$variaveis_income], na.rm = T))) 
  })
  
  
  #Renderizando o plot construído iterativamente 
  output$Income_linha <- renderPlot({
    #Controlando para o caso de não selecionar nenhuma variável, ou de a variável não ser numérica
    #De modo a não introduzir limites ao eixo y, para uma variável que não é numérica
    if ((length(input$variaveis_income) == 0) | (!is.numeric(unlist(Income_Democracy[,input$variaveis_income][1]))))
    {
      if((!is.numeric(unlist(Income_Democracy[,input$variaveis_income][1]))) & (length(input$variaveis_income) != 0)) return(ggplot(Income_Democracy, aes_string(x="year", y = input$variaveis_income)) + geom_line() + geom_line(color = input$cor)  + theme_classic())
      else return(ggplot(Income_Democracy, aes(x=year, y = year)) + geom_line() + geom_line(color = input$cor))
    }
    
    #Atualizando o eixo y
    update_ylim()
    #Plotando o gráfico de linhas reativamente
    plot_ind_dem_reativo()
  })
  
}

# Executa o aplicativo Shiny
shinyApp(ui = shinyUI, server = shinyServer)