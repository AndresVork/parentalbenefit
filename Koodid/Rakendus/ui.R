##Vanemahüvitise töölaua ui.R fail
##Projekt: CITIS ja sotsiaalministeerium

##Muutmise logi
# Alustatud 20.dets 2019 Andres Võrk
# 28.dets 2019 AV
# 08.02.2020 AV plotly katsetus

#Külgmenüü
side <- dashboardSidebar(
  sidebarMenu(
    menuItem("Taust", tabName = "tab_taust", icon = icon("book")),
    
    #menuItem("Poliitikareeglid ja muudatused", tabName = "poliitikamuutused", icon = icon("book")),
    
    menuItem("Vanemahüvitise reeglid",
             menuItem("Parameetrid", tabName = "tab_reeglid_tabelina", icon = icon("table")) , 
             # menuItem("Vanemahüvitise piirid", tabName = "reeglid_piirid", icon = icon("euro-sign")) ,
             menuItem("Vanemahüvitis ja samaaegne töötasu", tabName = "tab_reeglid_hyvitisjatootamine", icon = icon("bar-chart"))
             # menuItem("Vanemahüvitis ja eelnev töötasu", tabName = "reeglid_eelnevtootasu", icon = icon("globe"))
    ),

    menuItem("Perepoliitika kulud",
             menuItem("Euro, % SKPst", tabName = "tab_kulud", icon = icon("bar-chart")),
             menuItem("Lapse kohta", tabName = "tab_kuludlapsekohta", icon = icon("bar-chart"))
             # menuItem("Pindalana", tabName = "tab_kulud_aegread", icon = icon("area-chart"))
        ),
    
    menuItem("Vanemahüvitise saajad",
             menuItem("Suuruse järgi", tabName = "tab_saajatearv_suurussugu", icon = icon("area-chart")),
             menuItem("Soo järgi", tabName = "tab_saajatearv_sugusuurus", icon = icon("area-chart")),
             menuItem("Vanuse ja laste arvu järgi", tabName = "tab_saajatearv_vanusjalastearv", icon = icon("line-chart")),
             menuItem("Vanematevaheline jagamine", tabName = "tab_saajatearv_emajaisa", icon = icon("line-chart"))
             # menuItem("Varasema laste arvu järgi", tabName = "si_maakond", icon = icon("line-chart"))
    ),
    
    menuItem("Vanemahüvitise saajad ja töötamine",
             menuItem("Samaaegne töötamine soo ja töötamise järgi", tabName = "tab_vhjatootaminesugu_aegread", icon = icon("line-chart")),
             menuItem("Samaaegne töötamine töötasu ja soo järgi", tabName = "tab_vhjasugutootamine_aegread", icon = icon("line-chart")),
             menuItem("Eelnev ja järgnev töötamine", tabName = "tab_vhjaeelnevjargnevtootamine_aegread", icon = icon("line-chart")),
             menuItem("Teise vanema töötamine", tabName = "tab_emajaisatootamine", icon = icon("line-chart"))
             # menuItem("Järgnev töötamine", tabName = "si_maakond", icon = icon("line-chart"))
    ),
    
    menuItem("Vanemahüvitis ja sünnitamiskäitumine" ,
            menuItem("Sünnitamine sissetuleku lõikes", tabName = "tab_sissetulek_synnitamine", icon = icon("bar-chart"))
            # menuItem("Tulpadena", tabName = "pi_tulbad", icon = icon("bar-chart")),
            # menuItem("Kaardil", tabName = "pi_maakond", icon = icon("globe"))
    )

  #sidebar menu lõpp
  )
#side lõpp
)

#Keha
body <- dashboardBody(
  tabItems(
  
    tabItem(tabName = "tab_reeglid_tabelina",
            fluidRow(
              column(width = 9,
              h3("Vanemahüvitise kestuse ja suuruse parameetrid"),
              #dataTableOutput("parameetrid_tabelis")
              tableOutput("parameetrid_tabelis"))
            )
            ),

    tabItem(tabName = "tab_kulud_aegread",
          fluidRow(
            column(width = 9,
            inputPanel(
              # selectInput(inputId ="ti_aegread_indikaator", "Vali indikaator",
              #           unique(ti_indikaatorid$indnimi), selected=unique(ti_indikaatorid$indnimi)[2]),
              # checkboxInput(inputId="skpsuhtena", 
              #               label = "Suhtena SKPst",
              #               value = FALSE)
              materialSwitch(
                inputId = "lyliti_skpsuhtena_aegread",
                label = "Suhtena SKPst", 
                status = "primary",
                right = TRUE
              )
              
            )
            ) #end column
          ),
          fluidRow(
            plotOutput("kulud_aegread_nom", height = "600px", width = "1200px" )
          )
  ),
  
  tabItem(tabName = "tab_kulud",
          fluidRow(
            column(width = 9,
              materialSwitch(
                inputId = "lyliti_skpsuhtena_tulbad",
                label = "Suhtena SKPst", 
                status = "primary",
                right = TRUE),
              #plotOutput("kulud_tulpadena_nom", height = "600px", width = "1200px")
              plotlyOutput("lyliti_skpsuhtena_tulbad_plotly", height = "600px", width = "1200px")
            ) #end column
          )
  ),

  tabItem(tabName = "tab_kuludlapsekohta",
          fluidRow(
            column(width = 9,
            materialSwitch(
              inputId = "lyliti_skppc_tulbad",
              label = "Suhtena SKP inimese kohta", 
              status = "primary",
              right = TRUE),
            #plotOutput("kuludlapsekohta_tulpadena", height = "600px", width = "1200px"),
            plotlyOutput("kuludlapsekohta_tulpadena_plotly", height = "600px", width = "1200px")
            ) #end column
          )
          
  ),
  
  tabItem(tabName = "tab_saajatearv_suurussugu",
          fluidRow(
            column(width = 9,
            h3("Vanemahüvitis suuruse järgi soo lõikes"),
            inputPanel(
              materialSwitch(
                inputId = "lyliti_vhsuurus_isadeosakaal",
                label = "Isade osakaal", 
                status = "primary",
                right = TRUE
              )
              ),
            #box(title = "", width = 12, height = '700px', plotOutput("vhsuurusjoonis1", height = '650', width = '1200px'))
            plotOutput("vhsuurusjoonis1", height = '650', width = '1200px'),
            ) #end column
            )
  ),

  tabItem(tabName = "tab_saajatearv_sugusuurus",
          fluidRow(
            column(width = 9,
            h3("Vanemahüvitis soo järgi suuruse lõikes"),
            inputPanel(
              materialSwitch(
                inputId = "lyliti_vhsuurus_vordnetelg",
                label = "Sama telg", 
                status = "primary",
                right = TRUE
              )
            ),
            plotOutput("vhsuurusjoonis2", height = '500', width = '1200px'),
            ) #end column
          )
  ),
  
  tabItem(tabName = "tab_saajatearv_vanusjalastearv",
          fluidRow(
            column(width = 9,
              materialSwitch(
              inputId = "lyliti_vhjavanusjoonis1_osakaal",
              label = "Osakaal", 
              status = "primary",
              right = TRUE
            ),
            radioButtons(inputId = "suguvhvanusjoonis1", "Sugu",c("Mehed"="M","Naised"="N")),            
            
            h3("Vanemahüvitise saajate struktuur lapsevanema vanuse järgi"),
            br(),
            plotOutput("vhvanusjoonis1", height = '650', width = '1200px'),
            br(),
            h3("Vanemahüvitise saajate struktuur lapse sünnijärjekorra ja lapsevanema vanuse järgi"),
            br(),
            plotOutput("vhvanusjalapsjoonis2", height = '1000', width = '1200px'),
            br()
            ) #end column
          )
  ),

  tabItem(tabName = "tab_vhjatootaminesugu_aegread",
          fluidRow(
            column(width = 9,
                   
          #box(title = "", width = 12, height = '700px', plotOutput("vhjatootaminejoonis1", height = '650', width = '1200px'))
          materialSwitch(
              inputId = "lyliti_vhjatootaminejoonis1_vordnetelg",
              label = "Sama telg", 
              status = "primary",
              right = TRUE
            ),
          h3("Soo lõikes samaaegne töötamine"),
          br(),
          plotOutput("vhjatootaminejoonis1", height = '500', width = '1200px'),
          #plotlyOutput("vhjatootaminejoonis1", height = '500', width = '1200px'),
          ) #end column
          )
  ),
  
  tabItem(tabName = "tab_vhjasugutootamine_aegread",
          fluidRow(
            column(width = 9,
                   
            h3("Töötajate arv töötasu suuruse rühmade järgi"),
            plotOutput("vhjatootaminejoonis2", height = '650', width = '1200px')
            ) #end column
          )
  ),
  
  tabItem(tabName = "tab_sissetulek_synnitamine",
          fluidRow(
            column(width = 9,
                   
            #h3("Siia tulevad mikroandmete põhjal joonised, mis näitavad, kuidas on seotud vanemate eelnev töötasu ja sünnitamise tõenäosus")
            h3("Mikroandmete põhjal joonised, kuidas on seotud vanemate eelnev töötasu ja sünnitamise tõenäosus"),
            sliderInput("slidervanus", label = h3("Vanus"), min = 15, 
                        max = 45, value = c(20, 40)),
            plotOutput("sissetulekjasynnitaminejoonis1", height = '650', width = '1200px')
            ) #end column
          )            
  ),
  
  tabItem(tabName = "tab_vhjaeelnevjargnevtootamine_aegread",
          fluidRow(
            column(width = 9,
                   h3("Ema töötamine enne ja pärast sündi"),
            plotOutput("tootaminejasynnitaminejoonis1", height = '650', width = '1200px')
            ) #end column
          )
  ),

  tabItem(tabName = "tab_emajaisatootamine",
          fluidRow(
            column(width = 9,
                   
            h3("Isade ja emade töötamine, kui teine vanem saab vanemahüvitist"),
            img(src='isavhtootabema.jpg', align = "center", height = '600px', width = '800px')
            ) #end column
          )
  ),
  
  tabItem(tabName = "tab_saajatearv_emajaisa",
          fluidRow(
            column(width = 9,
                   
            h3("Siia tulevad mikroandmete põhjal joonised, mis näitavad ema ja isa vanemahüvitise järjestikust kasutamist")
            ) #end column
          )
  ),
  
  
    tabItem(tabName = "tab_taust",
          # "Käesolev töölaud on vanemahüvitise analüüsi prototüüp, mis on valminud Sotsiaalministeeriumile TÜ CITISe poolt.",
          # br(),
          # "Kontakt Andres Võrk, andres.vork@ut.ee"
          includeHTML("taust.html")
  ),
  
  tabItem(tabName = "tab_reeglid_hyvitisjatootamine",
          fluidRow(
            column(width = 9,
            h3("Valemid ja joonised, kuidas vanemahüvitise suurus sõltub samaaegsest töötasust"),
            br() ,
            #uiOutput('markdown'),
            #withMathJax(includeMarkdown("vanemahyvitisjatootaminevalemid_ver3.md"))
            #includeHTML("vanemahyvitisjatootaminevalemid_ver2.html")
            tags$a(href = "http://kodu.ut.ee/~avork/files/citis/vanemahyvitisjatootaminevalemid_3jaan2020.html", 
            "Vanemahüvitise ja töötamise valemid üle aja (link dokumendile)", target = '_blank'),
            br(),
            br(),
            
            #Vali vanemahüvitise määra, alampalga ja maksimumi aasta,
            selectInput("vhsuurusteaasta", label = "Hüvitiste miinimumi ja maksimumi aasta",
                        choices = seq(from = 2004, to = 2020, by = 1), 
                        selected = 2019, multiple = FALSE),
            
            #Vali vanemahüvitise esialgne suurus
            sliderInput("vanhyvsuurus", label = "Vali vanemahüvitise algne suurus",
                        min = 200, max = 3000, 
                        value = 700, step = 100, round=TRUE),
            
            actionButton("button_arvuta", "Arvuta",  
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            br(),
            br(),
            plotOutput("vhajatootaminereeglid", height = '650', width = '1200px')
          )
          ) #end column
  )
  
  
#tabItems lõpp
)
#body lõpp
)

dashboardPage(skin = "blue",
              dashboardHeader(title = "Vanemahüvitise indikaatorid", 
                              titleWidth = 300),
              dashboardSidebar(side, width = 300),
              body,
              title = "Vanemahüvitise indikaatorid"
)

