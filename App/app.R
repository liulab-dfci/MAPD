library(shiny)
library(tidyverse)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(readxl)
source("./Data_preprocessing.R")
source("./Helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    #titlePanel("Protein Degradability Prediction"),
    
    navbarPage("MAPD", theme = shinytheme("lumen"),
               ###### Home Tab ##########
               tabPanel("Home", fluid = T, 
                        fluidRow(
                            column(
                                8, 
                                h3(p("Introduction of MAPD")),
                                h5(p("Targeted protein degradation (TPD) has rapidly emerged as a 
                                     therapeutic modality to eliminate previously undruggable proteins 
                                     through hijacking the cell’s endogenous protein degradation 
                                     machinery. However, development of TPD compounds is largely 
                                     driven by trial-and-error. Recent systematic TPD studies of 
                                     the kinome have shown dramatic differences in degradation 
                                     between kinases with otherwise similar drug-target engagement, 
                                     suggesting unexplained factors influence degradability. 
                                     We therefore developed a machine learning model, 
                                     MAPD (Model-based Analysis of Protein Degradability), 
                                     to predict degradability from 42 protein features that encompass 
                                     post-translational modifications, protein stability, protein 
                                     expression and protein-protein interactions. MAPD accurately 
                                     predicts kinases that are degradable by TPD compounds (auPRC=0.759) 
                                     and is generalizable to independent non-kinase proteins. Only 5 
                                     features are necessary to achieve optimal performance, 
                                     with ubiquitination rate being the most highly predictive. 
                                     By structural modelling, we found that E2-accessible ubiquitination 
                                     sites, but not lysine residues in general, are particularly 
                                     associated with protein degradability. Finally, we extend MAPD 
                                     predictions to the entire proteome for their tractability to TPD 
                                     approaches. This valuable resource will enable rapid prioritization 
                                     of targets amenable to TPD and provide insights into rational design of 
                                     TPD drugs.")),
                                br(),
                                h4(p("Source Code")),
                                h5(p("The source code for this MAPD is available ", 
                                     a("on github", href = "https://github.com/WubingZhang/MAPDx"), ".")),
                                br(),
                                hr(),
                                h4(p("Cite Us")),
                                h5(p("Citation")),
                                br(),
                                h4(p("Contact")),
                                h5(p("Contact Information"))
                                
                            )
                        )
               ),
               ###### Prioritization Tab ##########
               tabPanel("Prioritization", fluid = TRUE,
                        fluidRow(
                            column(
                                4,selectizeInput("Gene", "Gene:", 
                                                 choices = NULL,
                                                 multiple = T),
                                textAreaInput("Genetext", "", 
                                              placeholder = "Enter a list of genes HERE.",
                                              width = "400px",
                                              height = "200px")),
                            column(
                                4, selectizeInput("Protein", "Protein (Uniprot ID):", 
                                                  choices = NULL,
                                                  multiple = T),
                                textAreaInput("Proteintext", "", 
                                              placeholder = "Enter a list of proteins HERE.",
                                              width = "400px",
                                              height = "200px")
                            )),
                        hr(),
                        fluidRow(
                            column(
                                12,tabsetPanel(type = "tabs",
                                               tabPanel("Degradability & Ligandability",
                                                        br(),
                                                        DT::dataTableOutput("Ligandability_table")),
                                               tabPanel("Column Description",
                                                        br(),
                                                        DT::dataTableOutput("Database_Description"))
                                )
                            )
                            
                        )
                        
               ),
               ###### Feature Tab ##########
               tabPanel("Features", fluid = TRUE,
                        
                        fluidRow(
                            column(
                                4, selectizeInput("Gene2", "Gene:",
                                                  choices = NULL,
                                                  multiple = T),
                                textAreaInput("Genetext2", "", 
                                              placeholder = "Enter a list of genes HERE.",
                                              width = "400px",
                                              height = "200px")
                            ),
                            column(
                                4, selectizeInput("Protein2", "Protein (Uniprot ID):",
                                                  choices = NULL,
                                                  multiple = T),
                                textAreaInput("Proteintext2", "", 
                                              placeholder = "Enter a list of proteins HERE.",
                                              width = "400px",
                                              height = "200px")
                            ),
                            column(
                                4,selectInput("Features", "Feature:",
                                              choices = c("ALL", All_features),
                                              selected = c("Ubiquitination_2", "Acetylation_1", 
                                                           "Phosphorylation_2", 
                                                           "Zecha2018_Hela_Halflife", "Length"),
                                              multiple = T)
                            )
                        ),
                        hr(),
                        fluidRow(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Features",
                                                 br(),
                                                 DT::dataTableOutput("Prediction_table")),
                                        tabPanel("Feature Description",
                                                 br(),
                                                 DT::dataTableOutput("Feature_Description"))
                            )
                        )
                        
               ),
               
               ###### E2 Accessibility Tab ##########
               tabPanel("UB_Characteristics", fluid = TRUE,
                        
                        fluidRow(
                            column(
                                4, selectizeInput("Gene3", "Gene:",
                                                  choices = NULL,
                                                  multiple = T),
                                #br(),
                                textAreaInput("Genetext3", "", 
                                              placeholder = "Enter a list of genes HERE.",
                                              width = "400px",
                                              height = "200px")
                            ),
                            column(
                                4, selectizeInput("Protein3", "Protein (Uniprot ID):",
                                                  choices = NULL,
                                                  multiple = T),
                                #br(),
                                textAreaInput("Proteintext3", "", 
                                              placeholder = "Enter a list of proteins HERE.",
                                              width = "400px",
                                              height = "200px")
                            )
                        ),
                        hr(),
                        fluidRow(
                            tabsetPanel(type = "tabs",
                                        tabPanel("E2 Accessibility",
                                                 br(),
                                                 DT::dataTableOutput("E2_table")),
                                        tabPanel("Column Description",
                                                 br(),
                                                 DT::dataTableOutput("E2_Description"))
                            )
                        )
                        
               ),
               ###### Downloads Tab ##########
               tabPanel("Downloads", fluid = T,
                        # fluidRow(
                        #     column(8,
                        #            h3(p("Source Table")),
                        #            br(),
                        #            hr()
                        #     )),
                        fluidRow(
                            column(6,
                                   titlePanel("Features"),
                                   br(),
                                   h4(p(downloadButton("FeatureTable"), 
                                        " Table of Features used for Prediction")),br(),
                                   h4(p(downloadButton("FeatureDes"), 
                                        " Table of Feature Description")),br()),
                            column(6,
                                   titlePanel("Degradability & Ligandability"),
                                   br(),
                                   h4(p(downloadButton("LigandabilityTable"), 
                                        " Table of Protein Degradability & Ligandability")),br(),
                                   h4(p(downloadButton("LigandabilityDes"), 
                                        " Column Description of Protein Degradability & Ligandability")),br()),
                            column(6,
                                   titlePanel("E2 Accessibility"),
                                   br(),
                                   h4(p(downloadButton("E2Table"), 
                                        " Table of E2 Accessibility")),br(),
                                   h4(p(downloadButton("E2Des"), 
                                        " Column Description of E2 Accessibility")),br())
                        )
               ),
               ###### External Link ##########
               tabPanel("External Link", fluid = T,
                        fluidRow(
                            titlePanel("Related Database"),
                            hr(),
                            column(
                                3,br(),
                                h3(a(img(src="PROSETTAC_icon.png", height = "50px"),
                                     href = "https://prosettac.weizmann.ac.il/pacb/steps")),br(),
                                h3(a(img(src="PROTACDB_icon.png", height = "50px"),
                                     href = "http://cadd.zju.edu.cn/protacdb/about")),br(),
                                h3(a(img(src="PROTACpedia_icon.png", height = "50px"),
                                     href = "https://protacpedia.weizmann.ac.il/ptcb/main")),br()
                            ),
                            column(
                                3,br(),
                                h3(a(img(src="Drugbank_icon.png", height = "50px"),
                                     href = "https://go.drugbank.com/")),br(),
                                h3(a(img(src="CHEMBL_icon.png", height = "50px"),
                                     href = "https://www.ebi.ac.uk/chembl/")),br(),
                                h3(a(img(src="Chemical_Probe_icon.png", height = "50px"),
                                     href = "https://www.chemicalprobes.org/")),br()
                            ),
                            column(
                                3,br(),
                                h3(a(img(src="SLCABPP_icon.png", height = "50px"),
                                     href = "http://wren.hms.harvard.edu/cysteine_viewer/")),br(),
                                h3(a(img(src="Eubopen_icon.png", height = "50px"),
                                     href = "https://www.eubopen.org/")),br(),
                                h3(a(img(src="Ciulli_Lab_JC_icon.png", height = "50px"),
                                     href = "https://www.lifesci.dundee.ac.uk/groups/alessio-ciulli//publications/journal-club")),br()
                            )
                            
                        )
                        
               )
               
    )
)

##### Server #####
server <- function(input, output, session){
    
    # Update selectize input
    updateSelectizeInput(session, 'Gene', 
                         choices = c("ALL",unique(Prediction$Gene)), server = TRUE)
    updateSelectizeInput(session, 'Gene2', 
                         choices = c("ALL",unique(Prediction$Gene)), server = TRUE)
    updateSelectizeInput(session, 'Gene3', 
                         choices = c("ALL",unique(E2_table$GeneID)), server = TRUE)
    updateSelectizeInput(session, 'Protein', 
                         choices = c("ALL",unique(Prediction$Entry)), server = TRUE)
    updateSelectizeInput(session, 'Protein2', 
                         choices = c("ALL",unique(Prediction$Entry)), server = TRUE)
    updateSelectizeInput(session, 'Protein3', 
                         choices = c("ALL",unique(E2_table$Uniprot_AC)), server = TRUE)
    
    ###### Prioritization Tab #####################
    output$Ligandability_table <- DT::renderDataTable({
        
        temp_gene <- ""
        if ("ALL" %in% input$Gene){
            if (length(input$Gene) > 1){
                temp_gene <- input$Gene[2:length(input$Gene)]
            } else{
                temp_gene <- unique(Prediction$Gene)
            }
        } else{
            temp_gene <- input$Gene
        }
        
        temp_Protein <- ""
        if ("ALL" %in% input$Protein){
            if (length(input$Protein) > 1){
                temp_Protein <- input$Protein[2:length(input$Protein)]
            } else{
                temp_Protein <- unique(Prediction$Entry)
            }
        } else{
            temp_Protein <- input$Protein
        }
        
        if (input$Genetext != ""){
            temp_gene <- str_split(input$Genetext, "[,\n][ ]*", simplify = T)
        }
        if (input$Proteintext != ""){
            temp_Protein <- str_split(input$Proteintext, "[,\n][ ]*", simplify = T)
        }
        
        temp_table <- Prediction_Ligandability %>%
            filter(Gene %in% temp_gene | Entry %in% temp_Protein) 
        
        temp_table$Gene <- createLink(temp_table$Entry, temp_table$Gene)
        temp_table
        
    }, escape = FALSE, options = list(scrollX = 2000))
    
    output$Database_Description <- renderDataTable({
        
        temp_des_tab <- readxl::read_xlsx("./Data/S3_Target_Annotation.xlsx")
        temp_des_tab
        
    }, escape = FALSE, options = list(scrollX = 2000))
    
    ###### Feature Tab #######
    output$Prediction_table <- DT::renderDataTable({
        
        temp_feature <- ""
        if ("ALL" %in% input$Features){
            if (length(input$Features) > 1){
                temp_feature <- input$Features[2:length(input$Features)]
            } else{
                temp_feature <- All_features
            }
        } else{
            temp_feature <- input$Features %>% as.character()
        }
        
        print(temp_feature)
        
        temp_gene <- ""
        if ("ALL" %in% input$Gene2){
            if (length(input$Gene2) > 1){
                temp_gene <- input$Gene2[2:length(input$Gene2)]
            } else{
                temp_gene <- unique(Prediction$Gene)
            }
        } else{
            temp_gene <- input$Gene2
        }
        
        temp_Protein <- ""
        if ("ALL" %in% input$Protein2){
            if (length(input$Protein2) > 1){
                temp_Protein <- input$Protein2[2:length(input$Protein2)]
            } else{
                temp_Protein <- unique(Prediction$Entry)
            }
        } else{
            temp_Protein <- input$Protein2
        }
        
        if (input$Genetext2 != ""){
            temp_gene <- str_split(input$Genetext2, "[,\n][ ]*", simplify = T)
        }
        if (input$Proteintext2 != ""){
            temp_Protein <- str_split(input$Proteintext2, "[,\n][ ]*", simplify = T)
        }
        
        temp_table <- Prediction %>%
            dplyr::select(c(colnames(Prediction)[1:5], temp_feature)) %>%
            filter(Gene %in% temp_gene | Entry %in% temp_Protein) 
        
        temp_table$Gene <- createLink(temp_table$Entry, temp_table$Gene)
        temp_table
        
    }, escape = FALSE, options = list(scrollX = 2000))
    
    output$Feature_Description <- renderDataTable({
        
        temp_des_tab <- readxl::read_xlsx("./Data/S1_Protein_Features.xlsx")
        temp_des_tab
        
    }, escape = FALSE, options = list(scrollX = 2000))
    
    ###### E2 Accessibility Tab ######
    output$E2_table <- DT::renderDataTable({
        
        temp_gene <- ""
        if ("ALL" %in% input$Gene3){
            if (length(input$Gene3) > 1){
                temp_gene <- input$Gene3[2:length(input$Gene3)]
            } else{
                temp_gene <- unique(E2_table$GeneID)
            }
        } else{
            temp_gene <- input$Gene3
        }
        
        temp_Protein <- ""
        if ("ALL" %in% input$Protein3){
            if (length(input$Protein3) > 1){
                temp_Protein <- input$Protein3[2:length(input$Protein3)]
            } else{
                temp_Protein <- unique(E2_table$Uniprot_AC)
            }
        } else{
            temp_Protein <- input$Protein3
        }
        
        if (input$Genetext3 != ""){
            temp_gene <- str_split(input$Genetext3, "[,\n][ ]*", simplify = T)
        }
        if (input$Proteintext3 != ""){
            temp_Protein <- str_split(input$Proteintext3, "[,\n][ ]*", simplify = T)
        }
        
        temp_table <- E2_table %>%
            filter(GeneID %in% temp_gene | Uniprot_AC %in% temp_Protein) 
        
        #temp_table$GeneID <- createLink(temp_table$Uniprot_AC, temp_table$GeneID)
        temp_table
        
    }, escape = FALSE, options = list(scrollX = 2000))
    
    output$E2_Description <- renderDataTable({
        
        temp_des_tab <- readxl::read_xlsx("./Data/S4_E2_Accessibility.xlsx")
        temp_des_tab
        
    }, escape = FALSE, options = list(scrollX = 2000))
    ###### Downloads Tab ##################
    output$FeatureTable <- downloadHandler(
        filename = "Feature_Table.tsv",
        content = function(file) {
            write_tsv(readRDS("./Data/Integrated_Features_level1_2021-07-18.rds"),
                      file)
        }
    )
    
    output$FeatureDes <- downloadHandler(
        filename = "Feature_Description.tsv",
        content = function(file) {
            write_tsv(readxl::read_xlsx("./Data/S1_Protein_Features.xlsx"),
                      file)
        }
    )
    
    output$LigandabilityTable <- downloadHandler(
        filename = "Protein_Ligandability_Degradability.tsv",
        content = function(file) {
            write_tsv(readRDS("./Data/Predictions_Ligandability.rds") %>%
                          rownames_to_column(var = "Gene"),
                      file)
        }
    )
    
    output$LigandabilityDes <- downloadHandler(
        filename = "Description_Protein_Ligandability_Degradability.tsv",
        content = function(file) {
            write_tsv(readxl::read_xlsx("./Data/S3_Target_Annotation.xlsx"),
                      file)
        }
    )
    
    output$E2Table <- downloadHandler(
        filename = "Lysine_E2_Acessibility.tsv",
        content = function(file) {
            write_tsv(read_tsv("./Data/Lysine_E2_Accessibility.txt"),
                      file)
        }
    )
    
    output$E2Des <- downloadHandler(
        filename = "Description_Lysine_E2_Acessibility.tsv",
        content = function(file) {
            write_tsv(readxl::read_xlsx("./Data/S4_E2_Accessibility.xlsx"),
                      file)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
