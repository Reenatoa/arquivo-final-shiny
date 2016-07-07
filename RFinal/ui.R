shinyUI(navbarPage("T\u00F3picos de Infer\u00EAncia", theme = shinytheme("united"),
                   tabPanel("Estimação de parâmetros",
                            tags$head(tags$link(rel = "icon", type = "image/x-icon", href = 
                                                  "http://www.ufc.br/images/_images/a_universidade/identidade_visual/brasao/brasao2_vertical_cor_72dpi.png")),
                            
                            fluidRow(column(3,selectInput("select", label = h3("Escolha uma Distribuição"), 
                                                          choices = list("Geometrica" = "Geometrica", "Normal" = "Normal",
                                                                         "Beta" = "Beta"), selected = "Geometrica"))),
                            
                            
                            conditionalPanel(condition="input.select=='Geometrica'",
                                             
                                             fluidRow(
                                               column(3,
                                                      wellPanel(
                                                        
                                                        helpText("Defina um tamanho da amostra e o parâmetro p"),
                                                        numericInput("n", label="tamanho da amostra", value=10),
                                                        #shinyalert("shinyalert2", TRUE, auto.close.after=10),
                                                        numericInput("prob", label="valor de p entre (0,1)", value=0.5, min=0,max=1)
                                                        
                                                        
                                                      )),
                                               column(9,
                                                      wellPanel(h3("Máxima Verossimilhança para Distribuição Geométrica"),
                                                                withMathJax(p("Os gráficos abaixo representam a função de probabilidade e a função de log-verossimlhaça respectivamente
                                                                              da distribuição geométrica. Sua densidade é dada por: $$P(X=x) = p(1-p)^x ,   x=0,1,...$$"),
                                                                            p("O gráfico de log-verossimilhança é criado a partir de uma amostra aleatória de tamanho n, a linha vermelha indica
                                                                              o valor verdadeiro do parâmetro e a linha azul indica o valor do estimador de máxima verossimilhança obtido a partir da amostra.")),
                                                                plotOutput("pmf"),
                                                                br(),
                                                                p("Pode-se notar que quando o tamanho da amostra aumenta o estimador de máxima verossimilhança se aproxima do valor verdadeiro do parâmetro.")
                                                                
                                                                            ))
                                               
                                               
                                               )),
                            
                            
                            conditionalPanel(condition="input.select=='Normal'",
                                             
                                             fluidRow(
                                               column(3,
                                                      wellPanel(
                                                        
                                                        helpText("Defina um tamanho da amostra, média e variância"),
                                                        numericInput("n1", label="tamanho da amostra", value=100),
                                                        numericInput("mean", label="média", value=0),
                                                        numericInput("var", label="variância", value=1,min=0),
                                                        helpText("Defina x,y,z ângulos do gráfico"),
                                                        numericInput("x", label="x", value=45),
                                                        numericInput("y", label="y", value=45),
                                                        numericInput("z", label="z", value=90)
                                                        
                                                      )),
                                               column(9,
                                                      wellPanel(h3("Máxima Verossimilhança para Distribuição Normal"),
                                                                withMathJax(p("O gráfico abaixo representa a função de log-verossimlhaçada distribuição Normal dada por $$l(\\mu,\\sigma|x_1,x_2,...,x_n) = -\\frac{n}{2}log(2\\pi) - \\frac{n}{2}log(\\sigma^2) -\\frac{\\sum_{i=1}^{n}(x_i-\\mu)^2}{2\\sigma^2}$$"),
                                                                            p("O gráfico de log-verossimilhança é criado a partir de uma amostra aleatória de tamanho n, com média e variância especificada ao lado. Os estimadores de \\mu e \\sigma^2 são respectivamentes dados pela média e a variância amostral")),
                                                                plotOutput("norm"),
                                                                br(),
                                                                p("Pode-se notar que quando o tamanho da amostra aumenta o estimador de máxima verossimilhança se aproxima do valor verdadeiro do parâmetro.")
                                                                
                                                      ))
                                               
                                               
                                             )),
                            
                            
                            
                            conditionalPanel(condition="input.select=='Beta'",
                                             
                                             fluidRow(
                                               column(3,
                                                      wellPanel(
                                                        
                                                        helpText("Defina um tamanho da amostra, e os parâmetros da Beta"),
                                                        numericInput("n2", label="tamanho da amostra", value=10),
                                                        numericInput("Alpha", label="Alpha", value=2,min=0),
                                                        numericInput("Beta", label="Beta", value=2,min=0),
                                                        helpText("Defina x,y,z ângulos do gráfico"),
                                                        numericInput("x1", label="x", value=45),
                                                        numericInput("y1", label="y", value=45),
                                                        numericInput("z1", label="z", value=90)
                                                      )),
                                               column(9,
                                                      wellPanel(h3("Máxima Verossimilhança para Distribuição Beta"),
                                                                withMathJax(p("Os gráficos abaixo representam a função de probabilidade e a função de log-verossimlhaça respectivamente
                                                                              da distribuição Beta. Sua densidade é dada por: $$P(X=x) = p(1-p)^x ,   x=0,1,...$$"),
                                                                            p("O gráfico de log-verossimilhança é criado a partir de uma amostra aleatória de tamanho n, a linha vermelha indica
                                                                              o valor verdadeiro do parâmetro e a linha azul indica o valor do estimador de máxima verossimilhança obtido a partir da amostra.")),
                                                                column(6,   plotOutput("beta")),column(6,tableOutput("estimabeta")),
                                                                br(),
                                                                p("Pode-se notar que quando o tamanho da amostra aumenta o estimador de máxima verossimilhança se aproxima do valor verdadeiro do parâmetro.")
                                                                
                                                                            ))
                                               
                                               
                                               ))),
                   tabPanel(p("Intervalos de Confian\u00E7a"),
                            
                            fluidRow(
                              tabPanel("Intervalos de Confian\u00E7a",
                                       tabsetPanel(
                                         tabPanel("Introdu\u00E7\u00E3o",
                                                  
                                                  column(1),
                                                  column(5,br(),br(),br(),
                                                         withMathJax(p("Apresentaremos neste trabalho uma pequena introdu\u00E7\u00E3o a intervalo de confian\u00E7a, na qual abordaremos apenas os intervalos para m\u00E9dia (com vari\u00E2ncia conhecida e desconhecida) e vari\u00E2ncia. Todos baseados no m\u00E9todo da quantidade pivotal. 
                                                                       Apresentaremos tamb\u00E9m intervalos de confian\u00E7a para alguns conjuntos de dados contidos no pacote", strong("datasets"), "e por fim apresentaremos uma ilustra\u00E7\u00E3o da interpreta\u00E7\u00E3o de intervalo de confian\u00E7a.")
                                                         )),
                                                  
                                                  column(5,br(),br(),br(),
                                                         wellPanel(
                                                           code("Intervalo de confian\u00E7a para a m\u00E9dia:",style="color:navy"),
                                                           p(HTML("<ul> <li type=square>Vari\u00E2ncia conhecida:<li type=square>"),
                                                             p("IC = \\(\\bigg [\\bar x - z_{(1-\\frac{\\alpha}{2})}\\frac{\\sigma}{\\sqrt{n}};\\bar x + z_{(1-\\frac{\\alpha}{2})}\\frac{\\sigma}{\\sqrt{n}}\\bigg] \\)"), HTML("</ul>")),
                                                           p(HTML("<ul> <li type=square>Vari\u00E2ncia desconhecida:<li type=square>"),
                                                             p("IC = \\(\\bigg [\\bar x - t_{(n-1,\\frac{\\alpha}{2})}\\frac{s_{x}}{\\sqrt{n}} ; \\bar x + t_{(n-1,\\frac{\\alpha}{2})}\\frac{s_{x}}{\\sqrt{n}}\\bigg ]\\)"), HTML("</ul>")),br(),
                                                           code("Intervalo de confian\u00E7a para a vari\u00E2ncia:",style="color:navy"),
                                                           p(HTML("<ul>"),
                                                             p("IC = \\(\\bigg [\\frac{(n-1)s_{x}^2}{Q_{1-\\frac{\\alpha}{2}}} ;\\frac{(n-1)s_{x}^2}{Q_{\\frac{\\alpha}{2}}}\\bigg ]\\)"), HTML("</ul>")))),
                                                  
                                                  column(1)),
                                         
                                         tabPanel("Intervalos de Confian\u00E7a",
                                                  fluidRow(
                                                    column(3,
                                                           wellPanel(
                                                             radioButtons(inputId = "Intervpara",
                                                                          label = "Intervalo para:",
                                                                          choices = list("media", "vari\u00E2ncia"),
                                                                          selected = "media"),
                                                             
                                                             conditionalPanel(condition = "input.Intervpara=='media'",
                                                                              selectInput(inputId = "Varia", "Vari\u00E2ncia :",c("Conhecida", "Desconhecida"))
                                                                              
                                                             ),
                                                             sliderInput("graus", "Tamanho da amostra:", 10, min = 10, max = 100, step=10),
                                                             sliderInput("alpha","N\u00EDvel de signific\u00E2ncia ( \u03B1 ):", min = 0, max = 1, value = 0.05, step = 0.01)
                                                             
                                                             
                                                             
                                                             
                                                             ,
                                                             br(),br(),br(),
                                                             div("C\u00F3digos R:",
                                                                 a(href="https://github.com/RafaelOliv3veira/Shiny-inferencia/tree/master/RFinal",
                                                                   target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
                                                             div(a(href="http://www.dema.ufc.br/",target="_blank", 
                                                                   "Departamento de Estat\u00EDstica e Matem\u00E1tica Aplicada (DEMA)."),align="right", style = "font-size: 8pt")
                                                           )),
                                                    column(9,
                                                           column(5, plotlyOutput("plot2"),
                                                                  sliderInput("mu","\u03BC:", min = -10, max = 10, value = 0, step = 0.5),
                                                                  sliderInput("sigma","\u03C3 \u00B2", min = 0.001, max = 10, value =1, step = 0.5)), 
                                                           column(6,plotlyOutput("plot1"), 
                                                                  
                                                                  HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
                                                                  code("Intervalo de confian\u00E7a:",style="color:navy"),
                                                                  withMathJax(),
                                                                  uiOutput('ex4'),
                                                                  br(),
                                                                  code("Resumo dos dados:",style="color:navy"),
                                                                  tableOutput("resumo") 
                                                                  ,br(),
                                                                  HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
                                                           )
                                                    ))),
                                         
                                         
                                         
                                         tabPanel("Exemplos",
                                                  fluidRow(
                                                    column(3,
                                                           wellPanel(
                                                             HTML('</br>'),
                                                             selectInput("dataset", h5("Escolha o conjunto de dados:"), choices = c("crabs" , "anorexia", "genotype" ,"nhtemp","ToothGrowth")),        
                                                             HTML('</br>'),
                                                             uiOutput('dv'),    
                                                             HTML('</br>'),
                                                             sliderInput("alpha1","N\u00EDvel de signific\u00E2ncia ( \u03B1 ):", min = 0.001, max = 0.999, value = 0.05, step = 0.01),
                                                             br(),br(),br(),
                                                             div("C\u00F3digos R:",
                                                                 a(href="https://github.com/RafaelOliv3veira/Shiny-inferencia/tree/master/RFinal",
                                                                   target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
                                                             div(a(href="http://www.dema.ufc.br/",target="_blank", 
                                                                   "Departamento de Estat\u00EDstica e Matem\u00E1tica Aplicada (DEMA)."),align="right", style = "font-size: 8pt")
                                                           )),
                                                    column(9,column(6,wellPanel(checkboxInput("displaystats", "Resumo dos dados", FALSE),
                                                                                tableOutput("summarystats"),
                                                                                plotlyOutput("distPlot_dv", width = "90%", height = "370px"),
                                                                                strong("Ho: Os dados tem distribui\u00E7\u00E3o normal"),br(),
                                                                                strong("Ha: Os dados n\u00E3o tem distribui\u00E7\u00E3o normal"),br(),br(),
                                                                                em("Teste de Normalidade de Shapiro-Wilk:"),
                                                                                tableOutput("sw")
                                                    )        
                                                    ), column(6, 
                                                              HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
                                                              h4("Intervalo de confian\u00E7a para a m\u00E9dia:"),
                                                              br(),
                                                              strong("Vari\u00E2ncia desconhecida:"),
                                                              uiOutput("intervmed1"),
                                                              br(),
                                                              h4("Intervalo de confian\u00E7a para a vari\u00E2ncia:"),
                                                              uiOutput("intervarip"),
                                                              br(),
                                                              HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
                                                              
                                                              
                                                              
                                                              
                                                    )))),
                                         tabPanel("Interpreta\u00E7\u00E3o do intervalo de confian\u00E7a",
                                                  fluidRow(
                                                    column(3,
                                                           wellPanel(  actionButton("draw", "Nova amostra"),
                                                                       numericInput("confidence", "N\u00EDvel de Confian\u00E7a (%):", 95, min = 1, max = 99, step=1),
                                                                       numericInput("amostrasize", "Tamanho da amostra:", 100, min = 2, max = 1000, step=1),
                                                                       numericInput("nsamples", "N\u00FAmero de amostras:", 100, min = 1, max = 500, step=1),
                                                                       br(),br(),br(),
                                                                       div("C\u00F3digos R:",
                                                                           a(href="https://github.com/RafaelOliv3veira/Shiny-inferencia/tree/master/RFinal",
                                                                             target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
                                                                       div(a(href="http://www.dema.ufc.br/",target="_blank", 
                                                                             "Departamento de Estat\u00EDstica e Matem\u00E1tica Aplicada (DEMA)."),align="right", style = "font-size: 8pt")
                                                           )),
                                                    column(9,textOutput("npct"),plotOutput("plot"))))
                                         
                                         
                                         
                                         
                                         
                                                  )),
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ))
                   )                        
                   
                   
                   
                   
                   ))