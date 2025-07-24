library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(shinyWidgets)
library(DT)
library(stringi)

municipios_ll <- read.csv("municipios_lloguer.csv",
                          colClasses = c("character","character","numeric",
                                         "character","character","numeric",
                                         "numeric","numeric","factor",
                                         "numeric","numeric"))



#=======================================================================
#=======================================================================


# Define UI for application that draws a histogram
ui <- fluidPage(
    #HTML('<footer><a href="https://gaiacooperacion.net" target="_blank" style="color: white; font-size:12pt; font-weight:normal; background-color: #DDA63A; font-weight:bold; width:100%;height:65px":>Disseny:<img src="./gaiaTransp45.png"/></a></footer>'),
    tags$head(
        tags$style(HTML("
      pre {
        color: white;
        font-weight: bold;
        font-family: arial;
        font-size: 15px;
        background-color: #07659e;
        border-color: #063970;
        height: 65px;
      }
      .footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        height: 30px; /* Set the fixed height of the footer here */
        background-color: #DDA63A;
      }
      body > div.container-fluid > h1{ 
        background-color: #DDA63A;
        color: white;
        padding: 10px;
      }
     .nowrap { 
     white-space: nowrap;
      }
      #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #f54d42;
               z-index: 105;
             }                
    "))
    ),

    # Application title
    titlePanel(windowTitle = 'Lloguer i renda als municipis',
            h1("Històric del Cost del lloguer (contractes nous) vs. Renda familiar neta als municipis de Catalunya", style= "font-size:22px")),

    # Sidebar with a slider input for number of bins 
    sidebarPanel(h4("Selecciona els paràmetres:"),width = 3, style = "font-weight: 500;font-size: 16px; color: white; background-color: #BF8B2E;",
                 pickerInput("Provincia", 
                             label = "Província:",
                             choices = unique(municipios_ll$Provincia),
                             multiple = T,
                             selected = "Barcelona"
                 ),
                 pickerInput("Comarca", 
                             label = "Comarca:",
                             choices = unique(municipios_ll$Comarca),
                             options = list(`actions-box` = TRUE),
                             multiple = T,
                             selected = unique(municipios_ll$Comarca[municipios_ll$Provincia== "Barcelona"])
                 ),
                 pickerInput("Municipio", 
                             label = "Municipi:",
                             choices = unique(municipios_ll$Municipio),
                             options = list(`actions-box` = TRUE),
                             multiple = T,
                             selected = unique(municipios_ll$Municipio[municipios_ll$Provincia== "Barcelona"])
                 ),
                 
                 #sliderInput("slider", label = "Població municipal l'any 2023", 
                 #            min = 0, 
                 #            max = max(municipios_ll$Població), 
                 #            value = c(0,max(municipios_ll$Població))
                 #),
                 pickerInput("catPoblacio","Magnitud població municipi l'any 2023" , 
                                 choices = c("< 1.000","1.000-5.000","5.000-10.000",
                                 "10.000-50.000","50.000-100.000","> 100.000"),
                                 options = list(`actions-box` = TRUE),
                                 selected = c("< 1.000","1.000-5.000","5.000-10.000",
                                              "10.000-50.000","50.000-100.000","> 100.000"),
                             multiple = T),
                 searchInput(
                     inputId = "search", 
                     label = "Cerca per nom del municipi: escriu, clica la lupa i el botó ENDAVANT!", 
                     placeholder = "Escriu-ne el nom o una part", 
                     btnSearch = icon("search"), 
                     btnReset = icon("remove"), 
                     width = "100%"
                 ),
                 actionButton(inputId   = "run",
                              label     = "Visualitza les dades",
                              style = "color: white;
                                         font-weight: bold;
                                         background-color: darkred; 
                                         #position: relative; 
                                         #left: 3%;
                                         height: 35px;
                                         width: 100%;
                                         text-align:center;
                                         #text-indent: -2px;
                                         border-radius: 6px;
                                         border-width: 2px"),
                 actionButton('reset', 'REINICIA ELS PARÀMETRES',
                              style= 'margin-top:6px'),
                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                  tags$div("Carregant dades... Loading data... Cargando datos",id="loadmessage"))
                 ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Cost del lloguer",plotlyOutput("llogPlot1")),
                tabPanel("Lloguer/Renda",plotlyOutput("llogPlot2")),
                tabPanel("Llistat interactiu descarregable", 
                         dataTableOutput("table1", width = '100%'))
                        )
           ),

    tags$footer(class = "footer",
                HTML('
                     
                     <a class="left-side" href="https://ods-municipios.es/catalunya" target="_blank"  style= "display:inline-block; margin-right:50px;padding:5px; color:white;font-size:12pt;">El multipanell dels ODS als municipis de Catalunya</a>
                     <a class="left-side" href="https://analysis.cat/contacte.html" target="_blank"  style= "display:inline-block; margin-right:50px;padding:5px; color:white;font-size:12pt;">Contacte</a>
                     <p style= "display:inline-block; color:white; float:right; padding:7px;margin-right:20px">Powered by: <a href="https://www.r-project.org/" target="_blank" style="color:white;">R</a> <a href="https://www.tidyverse.org/" target="_blank" style="color:white;">Tidyverse</a> <a href="https://shiny.posit.co/" target="_blank" style="color:white;">Shiny</a> <a href="https://plotly.com/" target="_blank" style="color:white;">Plotly</a></p>'
                    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

# FUNCIÓN REACTIVA PARA EL FILTRADO DE LOS DATOS
    

    dat <- eventReactive(input$run, {

        if (input$search == ""){
            req(input$Provincia, input$Comarca, input$catPoblacio)
            dat <- municipios_ll %>%
                filter(Provincia %in% input$Provincia &
                           Comarca %in% input$Comarca &
                           Municipio %in% input$Municipio & 
                           cat_pobl %in% input$catPoblacio)
        }
        
        else { 
            
            dat <- municipios_ll %>% 
                filter(grepl(stri_trans_general(tolower(input$search), "Latin-ASCII"), 
                             stri_trans_general(tolower(Municipio),"Latin-ASCII")))
        }
    })
    
    observeEvent(input$Provincia, {
        updatePickerInput(session = getDefaultReactiveDomain(),
                          inputId = 'Comarca',
                          choices = unique(municipios_ll$Comarca[municipios_ll$Provincia %in% input$Provincia]),
                          selected = unique(municipios_ll$Comarca[municipios_ll$Provincia %in% input$Provincia]))

    }, ignoreInit = TRUE)
 
    
    observeEvent(input$Comarca, {
        updatePickerInput(session = getDefaultReactiveDomain(),
                          inputId = 'Municipio',
                          choices = unique(municipios_ll$Municipio[municipios_ll$Comarca %in% input$Comarca]),
                          selected = unique(municipios_ll$Municipio[municipios_ll$Comarca %in% input$Comarca]))
    }, ignoreInit = TRUE)

     observeEvent(input$reset,{
         session$reload()
    })
        
        
    output$llogPlot1 <- renderPlotly({
    
            if (nrow(dat()) == 0) return(NULL)  # Per a evitar el missatge "Error: argument 1 is not a vector
     
        p <- dat() %>% 
            ggplot(aes(x= factor(any), 
                          y=rendaHabM,
                          group= Municipio,
                          colour = Municipio,
                          text= paste0(Municipio,
                                       "\nComarca: ", Comarca,
                                       "\nCost lloguer: ", round(rendaHabM,2),
                                       "\nPoblació (2023): ", Població)
                      ))+
            geom_line()+
            theme_ipsum(base_family = "Arial")+
            labs(title = "Preus mitjans dels lloguers nous (2007-2024)",
                 x= "Any del contracte",
                 y= "Preus mitjans del lloguer mensual")+
            theme(axis.title.y = element_text(size=18),
                  axis.title.x = element_text(size=18),
                  legend.position = 'none') +
            #scale_color_viridis(discrete = TRUE)+
            guides(col = guide_colourbar(title = "Variable"))
            
        
        ggplotly(p, tooltip = 'text') %>% 
            #layout(autosize = F, width = 500, height = 650)
            layout(annotations = 
                       list(x = 1, y = -0.47, text = "Font de les dades: Departament de Territori, via <a href= 'https://analisi.transparenciacatalunya.cat/Habitatge/Preu-mitj-del-lloguer-d-habitatges-per-municipi/qww9-bvhh/about_data' target='_blank'>Dades Obertes Catalunya</a>", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15))) %>% 
            layout(xaxis=list(tickangle=45, title = list( text ="Any del contracte", standoff = 5))) 
    })
    
    output$llogPlot2 <- renderPlotly({
        
        if (nrow(dat()) == 0) return(NULL)  # Per a evitar el missatge "Error: argument 1 is not a vector
        
        p2 <- ggplot(dat()[dat()$any %in% c(2015:2022), ],
                    aes(x= factor(any), 
                       y=pes_llog,
                       group= Municipio,
                       colour = Municipio,
                       text= paste0(Municipio,
                                    "\nComarca: ", Comarca,
                                    "\nRenda Familiar Neta (anual): ", renda_famNeta,
                                    "\nCost anual mitjà lloguer (contractes nous): ", round(rendaHabM*12,0),
                                    "\nRatio Lloguer nou/Renda Familiar Neta (%): ", pes_llog)
            ))+
            geom_line()+
            theme_ipsum(base_family = "Arial")+
            labs(title = "Històric (2015-2022): Cost lloguer vs. Renda Familiar Neta (%)",
                 #x= "Any",
                 y= "Ratio lloguer/renda fam. neta (%)")+
            theme(axis.title.y = element_text(size=18),
                  axis.title.x = element_text(size=18),
                  legend.position = 'none')
            #scale_color_viridis(discrete = TRUE)
        
        
        ggplotly(p2, tooltip = 'text') %>% 
            #layout(autosize = F, width = 500, height = 650)
            layout(annotations = 
                       list(x = 1, y = -0.47, text = "Font de les dades: Departament de Territori, via <a href= 'https://analisi.transparenciacatalunya.cat/Habitatge/Preu-mitj-del-lloguer-d-habitatges-per-municipi/qww9-bvhh/about_data' target='_blank'>Dades Obertes Catalunya</a> i el Ministerio de Economía via <a href='https://www.ine.es/dynt3/inebase/index.htm?padre=7132' target:'_blank'>INE</a>", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=15))) %>% 
            layout(xaxis=list(tickangle=45, title = list( text ="Any contracte", standoff = 5)))
    })
    # ================================================================   
    # TABLAS Y LISTADOS   
    
    output$table1 <- renderDT({
        dat() %>% 
            datatable(
                extensions = 'Buttons',
                options = list(
                    autoWidth= TRUE, 
                    columnDefs = list(
                        list(className = "nowrap", targets = "_all"),
                        list(className = 'dt-right', targets = c(3,7,8,10,11))),
                    scrollX = TRUE, 
                    dom = "Bfrtip",
                    buttons = list(
                        extend = c("excel"),
                        filename= "lista"),
                    columns= list(
                        list(title= NULL),
                        list(title= "Codi INE"),
                        list(title= "Municipi"),
                        list(title= "Població"),
                        list(title= "Província"),
                        list(title= "Comarca"),
                        list(title= "Any"),
                        list(title= "Contractes"),
                        list(title= "Preu lloguer/mes"),
                        list(title= "Dimensió població"),
                        list(title= "Renda Fam. Neta"),
                        list(title= "Ratio Lloguer nou /<br>Renda Mitj. (%)")
                    ))) %>%  
            formatStyle(c(0,1,2,3,4,5,6,7,8,9,10,11), fontSize = '85%') %>% 
            formatCurrency(c(3,7,8,10), currency = "", interval = 3, mark = ".", digits = 0) %>% 
            formatCurrency(c(11), currency = "", dec.mark = ",", digits = 2)
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
