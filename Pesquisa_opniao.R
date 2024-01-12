


library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(tm)
library(wordcloud)
library(plotly)

ui <- dashboardPage(title = "Pesquisa",
                    dashboardHeader(title = "Pesquisa Opnião"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("SÓCIOECONÔMICO", tabName = "socioeconomico",
                                 menuSubItem("NOME", tabName = "nome"),
                                 menuSubItem("IDADE", tabName = "idade"),
                                 menuSubItem("GÊNERO", tabName = "genero"),
                                 menuSubItem("RENDA FAMILIAR", tabName = "renda"),
                                 menuSubItem("RAÇA/COR", tabName = "raca"),
                                 menuSubItem("ESTADO CIVIL", tabName = "estadocivil")
                        ),
                        menuItem("TRABALHO", tabName = "trabalho",
                                 menuSubItem("FORMAÇÃO", tabName = "formacao"),
                                 menuSubItem("CARGO", tabName = "cargo"),
                                 menuSubItem("CIDADE", tabName = "cidade")
                        ),
                        menuItem("RESULTADOS", tabName = "resultados",
                                 menuSubItem("BASE DE DADOS", tabName = "basededados"),
                                 menuSubItem("NUVEM DE PALAVRAS", tabName = "nuvem"),
                                 menuSubItem("GRÁFICOS", tabName = "graficos")
                        )
                      ),
                      actionButton("atualizar_button", "ATUALIZAR", 
                                   class = "btn-primary")
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(
                          HTML("
          .skin-blue .main-header .logo {
            font-size: 24px; /* Adjust the font size as needed */
          }
        ")
                        )
                      ),
        tabItems(
          tabItem("socioeconomico",
                  plotOutput("nuvem_palavras"),
                  plotOutput("nuvem_palavras_genero"),
                  plotlyOutput("grafico_genero"),
                  plotlyOutput("grafico_raca"),
                  plotlyOutput("grafico_estadocivil"),
                  dataTableOutput("tabela_renda")
          ),
          tabItem("trabalho",
                  plotOutput("grafico_formacao"),
                  plotOutput("grafico_cargo"),
                  plotOutput("grafico_cidade")
          ),
          tabItem("nome",
                  textInput("nome_input", "Nome:")
          ),
          tabItem("idade",
                  numericInput("idade_input", "Idade:", value = 30)
          ),
          tabItem("genero",
                  selectInput("genero_input", "Gênero:", 
                              choices = c("Masculino", 
                                          "Feminino", 
                                          "Outros"), 
                              selected = "Masculino")
          ),
          tabItem("renda",
                  numericInput("renda_input", "Renda:", value = 5000)
          ),
          tabItem("raca",
                  selectInput("raca_input", "Raça:", 
                              choices = c("Branca", 
                                          "Parda", 
                                          "Negra"), 
                              selected = "Branca")
          ),
          tabItem("estadocivil",
                  selectInput("estadocivil_input", "Estado Civil:", 
                              choices = c("Solteiro(a)",
                                          "Noivo(a)", 
                                          "Casado(a)", 
                                          "Divorciado(a)"), 
                              selected = "Solteiro(a)")
          ),
          tabItem("formacao",
                  selectInput("formacao_input", "Formação:", 
                              choices = c("Fundamental",
                                          "Médio", 
                                          "Técnico", 
                                          "Superior"), 
                              selected = "Fundamental")
          ),
          tabItem("cargo",
                  selectInput("cargo_input", "Cargo:", 
                              choices = c("Estudante",
                                          "Auxiliar", 
                                          "Assistente",
                                          "Analista",
                                          "Gerente",
                                          "Coordenador",
                                          "Diretor"), 
                              selected = "Auxiliar")
          ),
          tabItem("cidade",
                  selectInput("cidade_input", "Cidade:", 
                              choices = c("São Paulo", "Belém", "Rio de Janeiro", "Belo Horizonte", "Salvador", "Recife", "Fortaleza", "Brasília", "Porto Alegre", "Curitiba", "Manaus"), 
                              selected = "Belém")
          ),
          tabItem("basededados",
                  DTOutput("table_dados")
          ),
          tabItem("nuvem",
                  plotOutput("nuvem_palavras_nome")
          ),
          tabItem("graficos",
                  fluidRow(
                    column(6, plotlyOutput("grafico_genero_resultado")),
                    column(6, plotlyOutput("grafico_raca_resultado")),
                    column(6, plotlyOutput("grafico_estadocivil_resultado")),
                    column(6, plotlyOutput("grafico_cargo_resultado"))
                  )
          )
        )
                    )
)

server <- function(input, output, session) {
  dados <- reactiveVal(data.frame())
  
  observeEvent(input$atualizar_button, {
    novo_dado <- data.frame(
      Nome = input$nome_input,
      Idade = input$idade_input,
      Gênero = input$genero_input,
      Renda = input$renda_input,
      Raça = input$raca_input,
      EstadoCivil = input$estadocivil_input,
      Formação = input$formacao_input,
      Cargo = input$cargo_input,
      Cidade = input$cidade_input
    )
    
    dados(rbind(dados(), novo_dado))
    
    # Clear input fields after updating data
    updateTextInput(session, "nome_input", value = "")
    updateNumericInput(session, "idade_input", value = 30)
    updateSelectInput(session, "genero_input", selected = "Masculino")
    updateNumericInput(session, "renda_input", value = 5000)
    updateSelectInput(session, "raca_input", selected = "Branca")
    updateSelectInput(session, "estadocivil_input", selected = "Solteiro(a)")
    updateSelectInput(session, "formacao_input", selected = "Superior")
    updateSelectInput(session, "cargo_input", selected = "Analista")
    updateSelectInput(session, "cidade_input", selected = "São Paulo")
  })
  
  output$table_dados <- renderDT({
    dados()
  })
  
  output$nuvem_palavras <- renderPlot({
    wordcloud(dados()$Nome, scale = c(3, 0.5), 
              colors = brewer.pal(8, "Dark2"))
  })
  
  output$nuvem_palavras_genero <- renderPlot({
    ggplot(dados(), aes(x = Nome, fill = Gênero)) +
      geom_text(aes(label = Nome), size = 5, position = position_jitter(width = 1, height = 0.5)) +
      facet_wrap(~Gênero) +
      theme_minimal() +
      labs(title = "Nuvem de Palavras por Gênero")
  })
  
  output$grafico_genero <- renderPlotly({
    pie(table(dados()$Gênero), main = "Distribuição por Gênero")
  })
  
  output$grafico_raca <- renderPlotly({
    ggplot(dados(), aes(x = Raça, fill = Raça)) +
      geom_bar() +
      labs(title = "Distribuição por Raça") +
      theme_minimal()
  })
  
  output$grafico_estadocivil <- renderPlotly({
    ggplot(dados(), aes(x = EstadoCivil, fill = EstadoCivil)) +
      geom_bar() +
      labs(title = "Distribuição por Estado Civil") +
      theme_minimal()
  })
  
  output$grafico_formacao <- renderPlotly({
    ggplot(dados(), aes(x = Formação, fill = Formação)) +
      geom_bar() +
      labs(title = "Distribuição por Formação") +
      theme_minimal()
  })
  
  output$grafico_cargo <- renderPlotly({
    ggplot(dados(), aes(x = Cargo, fill = Cargo)) +
      geom_bar() +
      labs(title = "Distribuição por Cargo") +
      theme_minimal()
  })
  
  output$grafico_cidade <- renderPlotly({
    ggplot(dados(), aes(x = Cidade, fill = Cidade)) +
      geom_bar() +
      labs(title = "Distribuição por Cidade") +
      theme_minimal()
  })
  
  output$tabela_renda <- renderDataTable({
    print("Inside renderTable for tabela_renda")
    resumo_renda <- data.frame(
      "Total" = sum(dados()$Renda),
      "Média" = mean(dados()$Renda),
      "Moda" = as.numeric(names(which.max(table(dados()$Renda))))
    )
    print(resumo_renda)
    resumo_renda
  })
  
  output$nuvem_palavras_nome <- renderPlot({
    wordcloud(dados()$Nome, scale = c(3, 0.5), 
              colors = brewer.pal(8, "Dark2"))
  })
  
  output$grafico_genero_resultado <- renderPlotly({
    ggplotly(
      ggplot(dados(), aes(x = Gênero, fill = Gênero)) +
        geom_bar() +
        labs(title = "Distribuição por Gênero") +
        theme_minimal()
    )
  })
  
  output$grafico_raca_resultado <- renderPlotly({
    ggplotly(
      ggplot(dados(), aes(x = Raça, fill = Raça)) +
        geom_bar() +
        labs(title = "Distribuição por Raça") +
        theme_minimal()
    )
  })
  
  output$grafico_estadocivil_resultado <- renderPlotly({
    ggplotly(
      ggplot(dados(), aes(x = EstadoCivil, fill = EstadoCivil)) +
        geom_bar() +
        labs(title = "Distribuição por Estado Civil") +
        theme_minimal()
    )
  })
  
  output$grafico_cargo_resultado <- renderPlotly({
    ggplotly(
      ggplot(dados(), aes(x = Cargo, fill = Cargo)) +
        geom_bar() +
        labs(title = "Distribuição por Cargo") +
        theme_minimal()
    )
  })
}

shinyApp(ui, server)
