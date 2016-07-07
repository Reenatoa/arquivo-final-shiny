require("shiny")
require("shinysky")
require("devtools")
require("MASS")
require("lattice")
require("datasets")
require("plotly")
require("ggplot2")
require("ggplot2movies")




norm_mean <- 0
norm_sd <- 1

##Intervalo de 

ci_media_amostral <- function(n, p=0.95, pop_mean=0, pop_sd=1) {
  amostra <- rnorm(n, norm_mean, norm_sd)
  amostra_sd <- sd(amostra)
  amostra_mean <- mean(amostra)
  tailprob <- (1 - p) / 2
  q <- -qt(tailprob, df=(n - 1), lower.tail=TRUE)
  se <- amostra_sd / sqrt(n)
  ci <- data.frame(ci_lb = amostra_mean - q * se,
                   ci_ub = amostra_mean + q * se)
  ci
}



server <- shinyServer(
  function(input, output, session) {
    
    
    
    output$pmf <- renderPlot({
      # draw.plotmath.cell(expression (bgroup("(", atop(x,y), ")")))
      if (!(input$n <= 0 | input$n %% 1 !=0)) n=input$n else n=10
      
      if (input$prob >= 0 & input$prob <=1 & is.numeric(input$prob)) prob = input$prob else prob=0.5
      
      p=seq(0,1,by=0.001)
      par(mfrow=c(1,2))
      plot(0:n, dgeom(0:n,prob=prob), col="blue",pch=16,
           xlab="X (n\u00FAmeros de tentativas at\u00E9 o primeiro sucesso)", ylab="", main=paste("Fun\u00E7\u00E3o de probabilidade, p=", prob, "n=",n))
      segments(x0=0:n, y0=rep(0, (n+1)), x1=0:n, y1=dgeom(0:n, prob=prob),lwd=2,col="blue")
      abline(h=0)
      X <- rgeom(n=n,prob=prob)
      loglikgeo<-function(p){
        n*log(p) + (sum(X))*log(1-p)}
      plot(p,loglikgeo(p=p), type="l",xlim=c(0,1), ylab="",xlab="p", main=
             paste("Log verossimilhan\u00E7a, \n log(L(p|X=x", ", ", "n=", n, "))", sep="") )
      abline(v=n/(n+sum(X)), col="blue", lty=2)
      abline(v=prob, col="red", lty=2)
      
    })
    
    
    
    
    
    output$norm <- renderPlot({
      
      if (!(input$n1 <= 0 | input$n %% 1 !=0)) n1=input$n else n1=10
      
      if (is.numeric(input$mean)) mean = input$mean else mean=0
      
      if (input$var >= 0 & is.numeric(input$var)) var= input$var else var=1
      
      if (is.numeric(input$x)) x = input$x else x=50
      
      if (is.numeric(input$y)) y = input$y else y=25
      
      if (is.numeric(input$z)) z = input$z else z=80
      
      X<-rnorm(n1,mean,sqrt(var))
      
      MLEnorm <- function(mean,var){
        SumX<-0
        for(i in 1:n1){
          SumX<-SumX + (X[i]-mean)^2
        }
        T <- -(n1/2)*log(2*pi) -(n1/2)*log(var) - (1/(2*var))*(SumX)
        return(T)
      } 
      #estimadores de máxima verossimilhança
      head(X)
      mean(X)
      var(X)
      #Gráfico da função de máxima verossimilhança
      g <- expand.grid(mu = seq(mean-3*var,mean+3*var,length=50) , sigma2 = seq(0.1,2*var,length=50), gr = 1:3)
      g$z <- MLEnorm(mean=g$mu,var=g$sigma2)
      
      wireframe(z ~ mu * sigma2, data = g, groups = gr,
                scales = list(arrows = FALSE),
                drape = TRUE, colorkey = TRUE,
                screen = list( x = x,y= y,z= z))
      
    })
    
    
    Amostrabeta <- reactive({
      
      if (!(input$n2 <= 0 | input$n2 %% 1 !=0)) n2=input$n2 else n2=10
      
      if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
      
      if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
      
      Amostra <- rbeta(n2,Alpha,Beta)
      return(Amostra)
    })
    
    
    
    Estima.rbeta <- reactive({
      
      if (!(input$n2 <= 0 | input$n2 %% 1 !=0)) n2=input$n2 else n2=10
      
      if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
      
      if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
      
      X <- Amostrabeta()
      
      MLE<-function(theta){
        T <- (theta[1]-1)*sum(log(X)) + (theta[2]-1)*sum(log(1-X)) - n2*log(beta(theta[1],theta[2]))
        return(T)
      }
      Xbar<-mean(X)
      Xvar <- var(X)
      chute[1] <- (Xbar*(1-Xbar) - Xvar)*Xbar/Xvar
      chute[2] <- chute[1]*(1-Xbar)/Xbar
      Hat<-optim(par=chute,fn=MLE,method="BFGS",control=list(fnscale=-1))$par
      Hessiana <- optim(par=chute,fn=MLE,method="BFGS",control=list(fnscale=-1),hessian = T)$hessian
      EP<-(-diag(Hessiana))^(-.5)
      
      tabelabeta <- matrix(c(Hat[1],EP[1],Hat[2],EP[2]),nrow=1)
      colnames(tabelabeta) <- c("\u03B1","ep(\u03B1)","\u03B2","ep(\u03B2)")
      row.names(tabelabeta) <- c("\u2023")
      return(tabelabeta)
    })
    
    
    output$estimabeta <- renderTable({
      Estima.rbeta()
      
    })
    
    
    
    
    output$beta <- renderPlot({
      
      
      if (!(input$n2 <= 0 | input$n2 %% 1 !=0)) n2=input$n2 else n2=10
      
      if (input$Alpha >= 0 & is.numeric(input$Alpha)) Alpha = input$Alpha else Alpha=2
      
      if (input$Beta >= 0  & is.numeric(input$Beta)) Beta = input$Beta else Beta=2
      
      if (is.numeric(input$x1)) x1 = input$x1 else x1=50
      
      if (is.numeric(input$y1)) y1 = input$y1 else y1=25
      
      if (is.numeric(input$z1)) z1 = input$z1 else z1=80
      
      
      library("lattice")
      
      X<-Amostrabeta()
      
      
      MLEwire<-function(Alpha,Beta){
        T <- (Alpha-1)*sum(log(X)) + (Beta-1)*sum(log(1-X)) - n2*log(beta(Alpha,Beta))
        return(T)
      }
      
      g <- expand.grid(alpha = seq(Alpha-3*sd(X),Alpha+3*sd(X),length=50)  , beta = seq(1,2*Beta,length=50), gr = 1:3)
      g$z <- MLEwire(Alpha=g$alpha,Beta=g$beta)
      wireframe(z ~ alpha * beta, data = g, groups = gr,
                scales = list(arrows = FALSE),
                drape = TRUE, colorkey = TRUE,
                screen = list(z = z1, y= y1, x = x1))
 
    })
    
    
    #Rafael 
    
    
    output$plot1 <- renderPlotly({
      
      
      graus <- input$graus
      alpha <-  input$alpha
      x <- seq(-4,4,len=1000)
      y <- dnorm(x)
      
      Linf <- qnorm(alpha/2)
      
      data = data.frame(x=x,y=y)
      dat = rbind(subset(data,x < Linf ),subset(data, x > -Linf  ))
      
      
      #Intervalo para a mÃ©dia  
      
      if(input$Intervpara=="media"){
        if(input$Varia == "Conhecida"){
      gg <- ggplot(data, aes(x=x,y=y)) + 
            theme_bw() +
            geom_line() +
            geom_segment(data = dat, aes(x=x, y = 0, xend =x, yend =y), colour="blue") +
            ggtitle(paste("Quantis te\u00F3ricos sim\u00E9tricos para 1-\u03B1 = ", paste0((1-alpha)*100,"%"), ".\n")) +
            ylab("f(z)") + 
            xlab("z") + 
            annotate('text', x = 0, y = 0.2, label = "1-\u03B1 ",parse = TRUE,size=5)
          
          p <- ggplotly(gg)
          p
          
        }
        else{
          # graus <- input$graus
          y1 <- dt(x,graus-1 ) 
          Linf1 <- qt(alpha/2,graus-1)
          
          data1 = data.frame(x=x,y=y1)
          dat1 = rbind(subset(data1,x <= Linf1 ),subset(data1, x >= -Linf1  ))
          
     gg <- ggplot(data1, aes(x=x,y=y)) +
            theme_bw() + 
            geom_line() +
            geom_segment(data = dat1, aes(x=x, y = 0, xend =x, yend =y), colour="green4") +
            ggtitle(paste("Quantis te\u00F3ricos sim\u00E9tricos para 1-\u03B1 =", paste0((1-alpha)*100,"%"), ".\n")) +
            ylab("t(x)") + 
            xlab("x") + 
            annotate('text', x = 0, y = 0.2, label = "1-\u03B1 ",parse = TRUE,size=5) 
          
          p <- ggplotly(gg)
          p
        }
        
      }
      
      # Intervalo para a variÃ¢ncia  
      
      else{
        
        x2 <- seq( 0 ,50 + graus,len=1000)
        y2 <- dchisq(x2,graus-1)
        Linf2 <- qchisq(alpha/2,graus-1)
        Lsup2 <- qchisq(1-alpha/2,graus-1)
        
        data2 = data.frame(x=x2,y=y2)
        dat2 = rbind(subset(data2,x <= Linf2 ),subset(data2, x >= Lsup2  ))
        
     gg<- ggplot(data2, aes(x=x,y=y)) + 
          theme_bw() +
          geom_line() +
          geom_segment(data = dat2, aes(x=x, y = 0, xend =x, yend =y), colour="red") +
          ggtitle(paste("Quantis te\u00F3ricos sim\u00E9tricos para 1-\u03B1 =", paste0((1-alpha)*100,"%"), ".\n")) +
          ylab("f(x)") 
        
        p <- ggplotly(gg)
        p
        
      }
      
      
      
      
    })
    
    ##################################################################   
    
    AmostraL <- reactive({
      n <- input$graus
      mu <- input$mu
      sigma <- input$sigma
      amostra <- rnorm(n,mu,sigma)
      return(amostra)
    })
    
    
    
    output$ex4 <- renderUI({
      n <- input$graus
      alpha <-  input$alpha
      mu <- input$mu
      sigma <- input$sigma
      amostra <- AmostraL()
      xbar <- round(mean(amostra),2)
      sqrtn <- round(sqrt(n),2)
      sx <- round(sd(amostra),2)
      NIC <- (1-alpha)*100
      #Normal
      
      quantil1 <- round(qnorm(alpha/2,lower.tail = F),2)
      sqsigma <- round(sqrt(sigma),2)
      inf1 <- round(xbar - quantil1*sqsigma/sqrtn,3)
      sup1 <- round(xbar + quantil1*sqsigma/sqrtn,3)
      
      #T-student
      
      quantil2 <- round(qt(alpha/2,n-1,lower.tail = F),2)
      inf2 <- round(xbar - quantil2*sx/sqrtn,3)
      sup2 <- round(xbar + quantil2*sx/sqrtn,3)
      
      #Chi
      
      quantil13 <- round(qchisq(alpha/2,n-1,lower.tail = T),2)
      quantil23 <- round(qchisq(1-alpha/2,n-1,lower.tail = T),2)
      inf3 <- round((n-1)*sx^2/quantil23,3)
      sup3 <- round((n-1)*sx^2/quantil13,3)
      
      
      
      if(input$Intervpara=="media"){
        if(input$Varia == "Conhecida"){
          
          withMathJax(sprintf("$$IC_{%d}(\\mu) = 
                              (%.03f \\pm %.03f \\times \\frac{%.03f}{%.03f} ) = [ %.03f ; %.03f]$$",NIC ,xbar,quantil1,sqsigma,sqrtn,inf1,sup1))
          
        }
        else{
          
          withMathJax(sprintf("$$IC_{%d}(\\mu) = 
                              (%.03f \\pm %.03f \\times \\frac{%.03f}{%.03f} ) = [ %.03f ; %.03f]$$",NIC ,xbar,quantil2,sx,sqrtn,inf2,sup2))
          
        }
        
      }
      
      # Intervalo para a variancia  
      
      else{
        
        
        withMathJax(sprintf("$$IC_{%d}(\\sigma^2) = 
                            (\\frac{%.03f \\times %.03f}{%.03f} ; \\frac{%.03f \\times %.03f}{%.03f}) = [ %.03f ; %.03f]$$",
                            NIC ,n-1,round(sx^2,2),quantil23,n-1,round(sx^2,2),quantil13,inf3,sup3))
        
        
        
      }
        })
    
    
    
    Nomestable <- reactive({
      tabela <- matrix(round(c(summary(AmostraL()),sd(AmostraL())),2),nrow = 1)
      colnames(tabela) <- c("Min","1\u00BA quartil","Mediana","M\u00E9dia","3\u00BA quartil","Max","sd")
      row.names(tabela) <- c("\u2023")
      return(tabela)
    })
    
    
    output$resumo <- renderTable({
      Nomestable()
      
    })
    
    
    
    output$plot2 <- renderPlotly({
      #set.seed(1234)
      n <- input$graus
      alpha <-  input$alpha
      mu <- input$mu
      sigma <- input$sigma
      amostra <- AmostraL()
      data = data.frame(x=amostra)
      bins <- seq(min(amostra), max(amostra), length.out = round(sqrt(n)) + round(sqrt(n)/2))
      
      
  gg <- ggplot(data = data, aes(x=x)) + 
        theme_bw(11)+
        ylab("Densidade") +
        xlab("amostra") +
        ggtitle(paste("Histograma de X \u007E N( \u03BC =",mu,", \u03C3 \u00B2 =",sigma,") \n")) + 
        geom_histogram(aes(y = ..density..),breaks = bins, colour = "white", fill = "purple",size=1)
      
      p <- ggplotly(gg)
      p
      
    })
    
    
    ##################################################################   
    
    
    
    
    sample_ci <- reactive({
      input$draw
      mutate(rdply(input$nsamples, ci_media_amostral(input$amostrasize,
                                                     input$confidence / 100)),
             n = seq_len(input$nsamples),
             contains_mean = ((0 > ci_lb) & (0 < ci_ub)))
    })
    
    output$plot <- renderPlot({
      print(ggplot(sample_ci(), aes(x = n, ymin = ci_lb, ymax = ci_ub,
                                    colour = contains_mean))
            + theme_bw()
            + geom_hline(yintercept = 0, colour="blue")
            + geom_linerange()
            + coord_flip()
            + scale_x_continuous("Amostras")
            + scale_y_continuous(sprintf("CI de %d%%", input$confidence))
            + scale_colour_manual(values = c("red", "black"))
            + theme(legend.position = "none",
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank())
            
            )
    })
    
    output$npct <- renderText({
      c("Porcentagem de intervalos que possuem a m\u00E9dia populacional:", round(mean(sample_ci()$contains_mean) * 100, 2), "%")
    })
    
    
    datasetInput <- reactive({
      switch(input$dataset,
             "crabs" = crabs[,-c(1:3)],
             "anorexia" = anorexia[,-1],
             "genotype" = data.frame(genotype = genotype[,3]),
             "nhtemp"= data.frame(nhtemp=nhtemp),
             "ToothGrowth"=data.frame(ToothGrowth=ToothGrowth[,1])
             )
    })
    
    output$dv = renderUI({
      selectInput('dv', h5('Vari\u00E1vel'), choices = names(datasetInput()))
    })
    
    
    output$summarystats = renderTable({
      if(input$displaystats){
        xb    <- datasetInput()[,input$dv]  
        xb  <- xb[is.na(xb)==FALSE]
        vec = as.numeric(as.character(xb))
        table = t(matrix(c((as.matrix(summary(vec)[1:6])),
                           round(sd(vec,na.rm=TRUE)))))
        
        rownames(table) = input$dv
        
        colnames(table) =  c("Min","1\u00BA quartil","Mediana","M\u00E9dia","3\u00BA quartil","Max","sd")
        return(table)
      }
    })
    
    
    output$sw = renderTable({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      dat=unlist(xb)
      dat1=data.frame(x=as.numeric(as.character(dat)))
      norm = shapiro.test(dat1$x)
      tab = matrix(c(norm$statistic,norm$p.value),nrow=1)
      colnames(tab) = c("Estat\u00EDstica W ","Valor p")
      rownames(tab) = input$dv
      tab
      
    })
    
    
    
    
    
    
    
    # histograms   
    output$distPlot_dv <- renderPlotly({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      n<-length(xb)
      bins <- seq(min(xb), max(xb),length.out = round(sqrt(n)) + round(sqrt(n)/2))
     
 gg <-  ggplot(data = data.frame(x=xb), aes(x) )+
        theme_bw() +
        ylab("")+
        xlab(input$dv)+
        ggtitle(paste("Histograma de",input$dv))+
        geom_histogram(aes(y = ..density..),breaks = bins,colour = "white", fill = "purple")
      
       p <- ggplotly(gg)
       p
      
    })
    
    
    output$intervmed1<- renderUI({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      n <- as.numeric(length(xb))
      alpha1 <- as.numeric(input$alpha1)
      xbar <- round(mean(xb),2)
      quantil <- round(qt(alpha1/2,n-1,lower.tail = F),2)
      sx <- round(sd(xb),2)
      sqrtn <- round(sqrt(n),2)
      inf2 <- round(xbar - quantil*sx/sqrtn,3)
      sup2 <- round(xbar + quantil*sx/sqrtn,3)
      NIC <- (1-alpha1)*100
      
      withMathJax(sprintf("$$IC_{%d}(\\mu) = 
                          (%.03f \\pm %.03f \\times \\frac{%.03f}{%.03f} ) = [ %.03f ; %.03f]$$",NIC ,xbar,quantil,sx,sqrtn,inf2,sup2))
      
      
    })
    
    output$intervari<- renderUI({
      xb    <- datasetInput()[,input$dv]  
      xb  <- xb[is.na(xb)==FALSE]
      xbar <- round(mean(xb),2)
      n <- as.numeric(length(xb))
      alpha1 <- as.numeric(input$alpha1)
      quantil <- round(qt(alpha1/2,n-1,lower.tail = F),2)
      sx2 <- round(var(xb),2)
      sqrtn <- round(sqrt(n),2)
      inf2 <- round(xbar - quantil*sx/sqrtn,3)
      sup2 <- round(xbar + quantil*sx/sqrtn,3)
      NIC <- (1-alpha1)*100
      
      quantil13 <- round(qchisq(alpha1/2,n-1,lower.tail = T),3)
      quantil23 <- round(qchisq(1-alpha1/2,n-1,lower.tail = T),3)
      inter.v1 <- (n-1)*sx2/quantil23
      inter.v2 <- (n-1)*sx2/quantil13
      
      withMathJax(sprintf("$$IC_{%d}(\\sigma^2) = 
                          (\\frac{%0.f \\times %.03f  }{%.03f} ; \\frac{%0.f \\times %.03f  }{%.03f} ) = [ %.03f ; %.03f]$$",NIC,n-1, sx2,quantil23,n-1,sx2,quantil13,inter.v1,inter.v2))
      
      
    })
    
    
  })










ui <- shinyUI(navbarPage("T\u00F3picos de Infer\u00EAncia",
                         tabPanel("Estima\u00E7\u00E3o de par\u00E2metros",
                                  tags$head(tags$link(rel = "icon", type = "image/x-icon", href = 
                                                        "http://www.ufc.br/images/_images/a_universidade/identidade_visual/brasao/brasao2_vertical_cor_72dpi.png")),
                                  
                                  fluidRow(column(3,selectInput("select", label = h3("Escolha uma Distribui\u00E7\u00E3o"), 
                                                                choices = list("Geometrica" = "Geometrica", "Normal" = "Normal",
                                                                               "Beta" = "Beta"), selected = "Geometrica"))),
                                  
                                  
                                  conditionalPanel(condition="input.select=='Geometrica'",
                                                   
                                                   fluidRow(
                                                     column(3,
                                                            wellPanel(
                                                              
                                                              helpText("Defina um tamanho da amostra e o par\u00E2metro p"),
                                                              numericInput("n", label="tamanho da amostra", value=10),
                                                              #shinyalert("shinyalert2", TRUE, auto.close.after=10),
                                                              numericInput("prob", label="valor de p entre (0,1)", value=0.5, min=0,max=1)
                                                              
                                                              
                                                            )),
                                                     column(9,
                                                            wellPanel(h3("M\u00E2xima Verossimilhan\u00E7a para Distribui\u00E7\u00E3o Geom\u00E9trica"),
                                                                      withMathJax(p("Os gr\u00E1ficos abaixo representam a fun\u00E7\u00E3o de probabilidade e a fun\u00E7\u00E3o de log-verossimilha\u00E7a respectivamente
                                                                                    da distribui\u00E7\u00E3o geom\u00E9trica. Sua densidade \u00E9 dada por: $$P(X=x) = p(1-p)^x ,   x=0,1,...$$"),
                                                                                  p("O gr\u00E1fico de log-verossimilhan\u00E7a \u00E9 criado a partir de uma amostra aleat\u00F3ria de tamanho n, a linha vermelha indica
                                                                                    o valor verdadeiro do par\u00E2metro e a linha azul indica o valor do estimador de m\u00E1xima verossimilhan\u00E7a obtido a partir da amostra.")),
                                                                      plotOutput("pmf"),
                                                                      br(),
                                                                      p("Pode-se notar que quando o tamanho da amostra aumenta o estimador de m\u00E1xima verossimilhan\u00E7a se aproxima do valor verdadeiro do par\u00E2metro.")
                                                                      
                                                                                  ))
                                                     
                                                     
                                                     )),
                                  
                                  
                                  conditionalPanel(condition="input.select=='Normal'",
                                                   
                                                   fluidRow(
                                                     column(3,
                                                            wellPanel(
                                                              
                                                              helpText("Defina um tamanho da amostra, m\u00E9dia e vari\u00E2ncia"),
                                                              numericInput("n1", label="tamanho da amostra", value=100),
                                                              numericInput("mean", label="m\u00E9dia", value=0),
                                                              numericInput("var", label="vari\u00E2ncia", value=1,min=0),
                                                              helpText("Defina x,y,z \u00E2ngulos do gr\u00E1fico"),
                                                              numericInput("x", label="x", value=45),
                                                              numericInput("y", label="y", value=45),
                                                              numericInput("z", label="z", value=90)
                                                              
                                                            )),
                                                     column(9,
                                                            wellPanel(h3("M\u00E2xima Verossimilhan\u00E7a para Distribui\u00E7\u00E3o Normal"),
                                                                      withMathJax(p("O gr\u00E1fico abaixo representa a fun\u00E7\u00E3o de log-verossimilhan\u00E7a  da distribui\u00E7\u00E3o Normal dada por $$l(\\mu,\\sigma|x_1,x_2,...,x_n) = -\\frac{n}{2}log(2\\pi) - \\frac{n}{2}log(\\sigma^2) -\\frac{\\sum_{i=1}^{n}(x_i-\\mu)^2}{2\\sigma^2}$$"),
                                                                                  p("O gr\u00E1fico de log-verossimilhan\u00E7a \u00E9 criado a partir de uma amostra aleat\u00F3ria de tamanho n, com m\u00E9dia e vari\u00E2ncia especificada ao lado. Os estimadores de \u03BC e \u03C3^2 s\u00E3o respectivamentes dados pela m\u00E9dia e a vari\u00E2ncia amostral")),
                                                                      plotOutput("norm"),
                                                                      br()
                                                                                                                                  ))
                                                     
                                                     
                                                   )),
                                  
                                  
                                   
                                  conditionalPanel(condition="input.select=='Beta'",
                                                   
                                                   fluidRow(
                                                     column(3,
                                                            wellPanel(
                                                              
                                                              helpText("Defina um tamanho da amostra, e os par\u00E2metros da Beta"),
                                                              numericInput("n2", label="tamanho da amostra", value=10),
                                                              numericInput("Alpha", label="Alpha", value=2,min=0),
                                                              numericInput("Beta", label="Beta", value=2,min=0),
                                                              helpText("Defina x,y,z \u00E2ngulos do gr\u00E1fico"),
                                                              numericInput("x1", label="x", value=45),
                                                              numericInput("y1", label="y", value=45),
                                                              numericInput("z1", label="z", value=90)
                                                            )),
                                                     column(9,
                                                            wellPanel(h3("M\u00E2xima Verossimilhan\u00E7a para Distribui\u00E7\u00E3o Beta"),
                                                                      withMathJax(p("O gr\u00E1fico abaixo representa a fun\u00E7\u00E3o de log-verossimlhan\u00E7a
                                                                                    da distribui\u00E7\u00E3o Beta dado por $$ l(\\alpha,\\beta|x_1,x_2,...,x_n)=(\\alpha-1)\\sum_{i=1}^{n}log(x_i) + ((\\beta-1)\\sum_{i=1}^{n}log(1-x_i)) - nlog(B(\\alpha,\\beta))   $$"),
                                                                                  p("O gr\u00E1fico \u00E9 criado a partir de uma amostra aleat\u00F3ria de tamanho n, retirada de uma distribui\u00E7\u00E3o com os par\u00E2metros  	\u03B1 e  	\u03B2 especificados ao lado. A tabela abaixo, mostra as estimativas e erros padr\u00F5es obtidos usando o m\u00E9todo de M\u00E2xima Verossimilhan\u00E7a")),
                                                                   column(6,   plotOutput("beta")),column(6,tableOutput("estimabeta")),
                                                                      br()
                                                                      )
                                                                      
                                                                                  )
                                                     
                                                     
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
                                                                       a(href="https://github.com/RafaelOliv3veira/Shiny_Inferencia/blob/master/R/versionbeta.R",
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
                                                                       a(href="https://github.com/RafaelOliv3veira/Shiny_Inferencia/blob/master/R/versionbeta.R",
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
                                                                    uiOutput("intervari"),
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
                                                                                 a(href="https://github.com/RafaelOliv3veira/Shiny_Inferencia/blob/master/R/versionbeta.R",
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

shinyApp(ui = ui, server = server)




