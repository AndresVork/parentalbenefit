##server.R
#Logi: 
# Alustatud 19.12.2019 Andres Võrk
# 10.07.2020 AV lisasin joonisele labels, plotly joonistele dtick =1, et oleks iga aasta

function(input, output, session) {
  
  
  #Vanemahüvitise parameetrid
  
  # output$parameetrid_tabelis <- renderDataTable({
  #   vhparameetrid
  # })
  #renderTable({
  #vhparameetrid
  # })
  
  output$parameetrid_tabelis <- function() ({
    vhparameetridtabelisse %>% knitr::kable("html")  %>%  
      kable_styling(bootstrap_options = c("striped", "condensed"), full_width = F, 
                    position = "left")
    #https://cran.r-project.org/web/packages/kableExtra/vignettes/use_kable_in_shiny.html
  })
  
  
  # kulude joonised -----------
  # alana
  # output$kulud_aegread_nom <- renderPlot({
  #   
  #   if (input$lyliti_skpsuhtena_aegread==TRUE) {
  #     ggplot(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "skpsuhtes") , 
  #            aes(x=aasta, y = vaartus*100, fill = toetuseliik)) +
  #       geom_area() + labs(x="", y = "% SKPst", fill = "", title ="Rahalised toetused lastega peredele") +
  #       scale_x_continuous(breaks=c(2000:2018))+
  #       theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
  #     
  #   } else {
  #     ggplot(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "vaartus") , 
  #            aes(x=aasta, y = vaartus/1000, fill = toetuseliik)) +
  #       geom_area() + labs(x="", y = "Mln eurot", fill = "", title ="Rahalised toetused lastega peredele") +
  #       scale_x_continuous(breaks=c(2000:2018))+
  #       theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
  #   }
  #   
  # })
  
  #tulbana  
  output$kulud_tulpadena_nom <- renderPlot({
    
    if (input$lyliti_skpsuhtena_tulbad==TRUE) {
      ggplot(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "skpsuhtes") , 
             aes(x=aasta, y = vaartus*100, fill = toetuseliik)) +
        geom_col() + labs(x="", y = "% SKPst", fill = "", title ="Rahalised toetused lastega peredele") +
        scale_x_continuous(breaks=c(2000:2019), labels = c(2000:2019))+
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))  
    } else {
      ggplot(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "vaartus") , 
             aes(x=aasta, y = vaartus/1000, fill = toetuseliik)) +
        geom_col() + labs(x="", y = "Mln eurot", fill = "", title ="Rahalised toetused lastega peredele") +
        scale_x_continuous(breaks=c(2000:2019), labels = c(2000:2019))+
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
    }  
  })
  
  output$lyliti_skpsuhtena_tulbad_plotly <- renderPlotly({
    
    if (input$lyliti_skpsuhtena_tulbad==TRUE) {
      
      plot_ly(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "skpsuhtes")) %>% 
        add_trace(data = toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "skpsuhtes"), 
                  type = "bar", 
                  x = ~aasta, 
                  y = ~vaartus*100, 
                  color = ~toetuseliik) %>%  
        layout(barmode = "stack", 
               title="Rahalised toetused lastega peredele", 
               yaxis=list(title="% SKPst"), 
               xaxis=list(title="", dtick = 1)) #lisasin dtick täiendi
    } else {
      
      plot_ly(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "vaartus")) %>% 
        add_trace(data = toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "vaartus"), 
                  type = "bar", 
                  x = ~aasta, 
                  y = ~vaartus/1000, 
                  color = ~toetuseliik) %>%  
        layout(barmode = "stack", title="Rahalised toetused lastega peredele", 
               yaxis=list(title="Mln eurot"), 
               xaxis=list(title="", dtick = 1))
    }
  })
  
  #Lapse kohta
  output$kuludlapsekohta_tulpadena <- renderPlot({
    
    if (input$lyliti_skppc_tulbad==TRUE) {
      ggplot(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "lapsekohtavsskppc") , 
             aes(x=aasta, y = vaartus, fill = toetuseliik)) +
        geom_col() + labs(x="", y = "Euro lapse kohta versus SKP inimese kohta", fill = "", 
                          title ="Rahalised toetused lastega peredele") +
        scale_x_continuous(breaks=c(2000:2018))+
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
    } else {
      ggplot(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "lapsekohta") , 
             aes(x=aasta, y = vaartus, fill = toetuseliik)) +
        geom_col() + labs(x="", y = "Eurot lapse kohta aastas", fill = "", title ="Rahalised toetused lastega peredele") +
        scale_x_continuous(breaks=c(2000:2018))+
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))  
    }  
  })
  
  ### Plotly katsetus
  output$kuludlapsekohta_tulpadena_plotly <- renderPlotly({
    
    if (input$lyliti_skppc_tulbad==TRUE) {
      
      plot_ly(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "lapsekohtavsskppc")) %>% 
        add_trace(data = toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "lapsekohtavsskppc"), 
                  type = "bar", 
                  x = ~aasta, y = ~vaartus, color = ~toetuseliik) %>%  
        layout(barmode = "stack", title="Rahalised toetused lastega peredele", 
               yaxis=list(title="Euro lapse kohta versus SKP inimese kohta"), 
               xaxis=list(title="", dtick = 1))
    } else {

      plot_ly(toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "lapsekohta")) %>% 
        add_trace(data = toetusedkokku %>%  dplyr::filter(toetuseliik != "VHjaperetoetuskokku", tunnus == "lapsekohta"), 
                  type = "bar", 
                  x = ~aasta, y = ~vaartus, color = ~toetuseliik) %>%  
        layout(barmode = "stack", title="Rahalised toetused lastega peredele", 
               yaxis=list(title="Eurot lapse kohta aastas"), 
               xaxis=list(title="", dtick = 1))
    }
  })
  
#Näide OOPi töölaualt
  # output$oop2Plotly <- renderPlotly({
  #   
  #   if (input$oop2eurovspercent=="Spending") myytitle <- "EUR"
  #   if (input$oop2eurovspercent=="Share") myytitle <- "%"
  #   
  #   
  #   #minuaasta<-2000
  #   oop2<-oop2koos[oop2koos$Year==input$leuaasta, ] 
  #   
  #   mytitle= paste0("Structure of OOPs in ", input$leuaasta)
  #   
    # plot_ly(oop2) %>% 
    #   add_trace(data = oop2, type = "bar", x = ~Quintile, y = ~get(input$oop2eurovspercent), color = ~Category) %>%  
    #   layout(barmode = "stack", title=mytitle, yaxis=list(title=myytitle), xaxis=list(title="Quintiles based on consumption expenditure"))
    
  #})
  
  
  
  
  
  
  # Vanemahüvitise saajad ---------------------
  #Hüvitise suuruse järgi
  # vhsuurusenimed <- c('kokku' =  'Kokku',
  #                       'vanemahüvitise määr' =  'Vanemahüvitise määras',
  #                        'kuupalga alammäär' = 'Kuupalga alammääras',
  #                       '100' =  'Eelmine töötasu',
  #                       'maksimum' =  'Maksimaalses määras')
  
  isadeosakaal <- vhsuurus_df %>%  dplyr::filter(!(ryhm %in% c("kokku"))) %>% 
    group_by(aasta, vhsuurus) %>%
    mutate(arvosakaal = arv/sum(arv)) %>%  
    dplyr::filter(ryhm =="isa") %>% 
    ungroup()
  
  #Suuruse ja soo järgi
  output$vhsuurusjoonis1 <- renderPlot({
    
    if (input$lyliti_vhsuurus_isadeosakaal==TRUE) {
      #Isade osakaal
      ggplot(isadeosakaal, aes(x=aasta, y = arvosakaal*100)) +
        geom_line()+
        #geom_vline(xintercept = c(2008, 2014, 2018))+
        facet_wrap(~vhsuurus, scales = "free_y") + #, labeller = as_labeller(vhsuurusenimed)) +
        labs(x = "", y = "%", title = "Isade osakaal vanemahüvitise saajate seas",
             subtitle = "Aasta lõpu seisuga") +
        scale_x_continuous(breaks=c(2005:2020))+
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
      
      
    } else {
      vhsuurus_df %>% dplyr::filter(ryhm %in% c("ema", "isa")) %>% 
        ggplot(aes(x=aasta, y = arv, fill = ryhm)) +
        geom_area()+
        scale_x_continuous(breaks=c(2005:2020))+
        facet_wrap(~vhsuurus, scales = "free_y" ) + #, labeller = as_labeller(vhsuurusenimed)) +
        labs(fill = "", x = "", y = "Inimeste arv aasta lõpus", 
             title = "Vanemahüvitise saajad suuruse ja saaja järgi",
             subtitle = "Aasta lõpu seisuga") +
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
      
    }
    
  })
  
  
  output$vhsuurusjoonis2 <- renderPlot({
    
    if (input$lyliti_vhsuurus_vordnetelg==TRUE) {
      
      vhsuurus_df %>% dplyr::filter(ryhm %in% c("ema", "isa", "kokku"), vhsuurus != "Kokku") %>% 
        ggplot(aes(x=aasta, y = arv, fill = vhsuurus)) +
        geom_area()+
        scale_x_continuous(breaks=c(2005:2020))+
        facet_wrap(~ryhm) +  #, scales = "free_y"
        labs(fill = "", x = "", y = "Inimeste arv aasta lõpus") +
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
      
      
    } else {
      vhsuurus_df %>% dplyr::filter(ryhm %in% c("ema", "isa", "kokku"), vhsuurus != "Kokku") %>% 
        ggplot(aes(x=aasta, y = arv, fill = vhsuurus)) +
        geom_area()+
        scale_x_continuous(breaks=c(2005:2020))+
        facet_wrap(~ryhm, scales = "free_y") + 
        labs(fill = "", x = "", y = "Inimeste arv aasta lõpus") +
        theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
      
    }
    
  })
  
  ############## Vanuse järgi ---
  
  output$vhvanusjoonis1 <- renderPlot({
    
    if (input$lyliti_vhjavanusjoonis1_osakaal==TRUE) {
      
      ggplot(vhtabel1grprtypevanusgr %>%  dplyr::filter(gender == input$suguvhvanusjoonis1), aes(x=vanusgr, y = osakaal, fill = pbtype0f)) +
        geom_col() +
        facet_wrap(~pbbegyear0) +
        theme(axis.text.x = element_text(angle = 90),text = element_text(size=12),
              legend.position = "bottom") +
        labs(fill = "", x = "Vanema vanus", y = "")
      
    } else {
      ggplot(vhtabel1grprtypevanusgr %>%  dplyr::filter(gender ==input$suguvhvanusjoonis1), aes(x=vanusgr, y = vaartus, fill = pbtype0f)) +
        geom_col() +
        facet_wrap(~pbbegyear0) +
        theme(axis.text.x = element_text(angle = 90),text = element_text(size=12),
              legend.position = "bottom") +
        labs(fill = "", x = "Vanema vanus", y = "")
      
    }
    
  })
  
  
  output$vhvanusjalapsjoonis2 <- renderPlot({
    
    if (input$lyliti_vhjavanusjoonis1_osakaal==TRUE) {
      
      ggplot(vhtabel1grprtype %>%  dplyr::filter(gender == input$suguvhvanusjoonis1), aes(x=vanusgr, y = osakaal, fill = pbtype0f)) +
        geom_col() +
        facet_wrap(mitmeslaps~pbbegyear0, scales = "free") +
        theme(axis.text.x = element_text(angle = 90),text = element_text(size=12),
              legend.position = "bottom") +
        labs(title= "Mitmes laps ja VH aasta", fill = "", x = "Vanema vanus", y = "")
      
    } else {
      ggplot(vhtabel1grprtype %>%  dplyr::filter(gender ==input$suguvhvanusjoonis1), aes(x=vanusgr, y = vaartus, fill = pbtype0f)) +
        geom_col() +
        facet_wrap(mitmeslaps~pbbegyear0, scales = "free") +
        theme(axis.text.x = element_text(angle = 90),text = element_text(size=12),
              legend.position = "bottom") +
        labs(title= "Mitmes laps ja VH aasta", fill = "", x = "Vanema vanus", y = "")
      
    }
    
  })
  
  #Töötasu järgi
  vhtootamisenimed <- c(koik =  'Kõik',
                        tootavad =  'Töötavad',
                        mittetootavad = "Mittetöötavad",
                        tootavadkunialammaar = 'Töötasu kuni alammäär',
                        tootavadulealammaar =  'Töötasu üle alammäära',
                        tootavadkuniviisalammaara =  'Töötasu kuni viis alammäära')
  
  
  #VH ja samaaegne töötamine -------------------
  
  output$vhjatootaminejoonis1 <- renderPlot({
    #output$vhjatootaminejoonis1 <- renderPlotly({  
    
    temp <- vhtootaminepikk %>% dplyr::filter(as.character(tootab) %in%
                                                c("Mittetöötavad", "Töötasu kuni alammäär", "Töötasu üle alammäära")) 
    # %>% 
    #mutate(tootab = factor(tootab, levels = c('Töötasu üle alammäära', 'Töötasu kuni alammäär',"Mittetöötavad")))
    
    p <-  ggplot(temp,
                      aes(x=as.Date(kpv), y = vaartus, fill=tootab)) +
      geom_area()+
      geom_vline(xintercept = as.Date(c("2018-03-01", "2014-01-01", "2007-09-01")),
                 linetype="dotted", 
                 color = "blue", size=0.5)+
      facet_wrap(~sugu) +
      labs(x="", y = "", fill = "",
           title = "Vanemahüvitise saajad kalendrikuu alguse seisuga töötasu suuruse järgi") +
      theme(legend.position="bottom", axis.text.x = element_text(angle = 45),text = element_text(size=12))
    
        
    if (input$lyliti_vhjatootaminejoonis1_vordnetelg==TRUE) {
      
      p + facet_wrap(~sugu)
      
    } else {
      
      p + facet_wrap(~sugu, scales = "free_y")

    }
    
  })
  
  
  output$vhjatootaminejoonis2 <- renderPlot({
    ggplot(vhtootaminepikk, aes(x=as.Date(kpv), y = vaartus, color=sugu)) +
      geom_line()+
      facet_wrap(~tootab, scales = "free_y") + #, labeller = as_labeller(vhtootamisenimed)
      labs(x="", y = "", color = "", title = "Vanemahüvitise saajad kalendrikuu alguse seisuga töötasu suuruse järgi") +
      theme(axis.text.x = element_text(angle = 45),text = element_text(size=14))
  })
  
  #Sissetulek ja sünnitamise tõenäosus---------------
  
  output$sissetulekjasynnitaminejoonis1 <- renderPlot({
    
    aluminevanuspiir = input$slidervanus[1]
    yleminevanuspiir = input$slidervanus[2]
    #Synnitamise tõenäosus
    syndtn <- syndjasm %>%  dplyr::filter(vanus>=aluminevanuspiir, vanus<=yleminevanuspiir,
                                          befchild<3) %>%
      group_by(aasta, syndivlaps, eelpalkq) %>%
      summarise(synnitamisetn = mean(synd)) %>%  #,vaatlusi = n())
      ungroup()
    
    ggplot(syndtn, aes(x=as.factor(eelpalkq), y=synnitamisetn, 
                       fill = factor(aasta))) +  #fill = (aasta>2003),group = factor(aasta)
      geom_col(position = "dodge") +
      #scale_fill_brewer(palette="Blues") +
      facet_wrap(~syndivlaps, nrow = 1) +
      labs(x= "Ema eelneva aasta palgarühm", y = "Lapse sünnitamise tõenäosus", fill ="",
           title = "Ema varasem sissetulek ja sünnitamise tõenäosus",
           subtitle = paste0("Ema vanus vahemikus ", as.character(input$slidervanus[1]), " - ", as.character(input$slidervanus[2])),
           caption = "Kvintiilid leitud iga aasta ja laste arvu kombinatsiooni kohta eraldi") +
      theme(axis.text.x = element_text(angle = 90),
            text = element_text(size=16))
    
  })
  
  
  #save(vhempl, file = paste0(workpath, "vhempl.RData"))
  
  
  output$tootaminejasynnitaminejoonis1 <- renderPlot({
    
    ggplot(vhemplgr %>%  dplyr::filter(byearch>=2002, befchild<4, timebirth<=40, timebirth>=-24), 
           aes(x=timebirth, y=employed, group=byearch, color=as.factor(byearch))) +
      geom_line()+
      facet_wrap(~befchild) +
      scale_x_continuous(breaks = seq(-24, 36, by = 6)) +
      labs(x = "Kuud peale sündi", y = "Hõivatute osakaal", 
           color = "",
           title = "Sünnitamise ja töötamise seosed varasemate laste arvu järgi")
    
  })
  
  #See ajab formaadi sassi
  #output$markdown <- renderUI({
  #  HTML(markdown::markdownToHTML(knit('vanemahyvitisjatootaminevalemid_ver2.Rmd', quiet = TRUE),fragment.only = TRUE))
  #})
  
  # observeEvent(input$button_uuenda, {
  # }
  # )
  #input$button_arvuta, 
  
  #uuendab vanemahüvitise alumise ja ülemise piiri
  
  observe({
  
  updateSliderInput(session, "vanhyvsuurus", label = "Vali vanemahüvitise algne suurus", value = 700,
                     min = round(vhparameetridm[as.character(input$vhsuurusteaasta), "vanhyvmaar"],0), 
                     max = round(vhparameetridm[as.character(input$vhsuurusteaasta), "maksimum"],0), step = 100)
  })
  
  
  output$vhajatootaminereeglid <- renderPlot({
    
    input$button_arvuta
    
    palkylemine = 2000 #eurodes
    lapsehooldustasu=700/15.6466
    
    #Min ja max väärtuste aasta
    Aasta = isolate(as.character(input$vhsuurusteaasta))
    
    #vanemahüvitis on etteantud, kuid ei saa olla väiksem vanemahüvitise määrast, ega suurem maksimumist
    #Panin selle sliderisse sisse
    #vanhyv = isolate(as.numeric(min(max(input$vanhyvsuurus, vhparameetridm[Aasta, "vanhyvmaar"]), vhparameetridm[Aasta, "maksimum"])))

    vanhyv = isolate(as.numeric(input$vanhyvsuurus))
    
    df <- data.frame(palk=seq(1, palkylemine, by = 1))
    
    df2004 <- df %>% 
      mutate(aasta = 2004) %>% 
      #uus vanemahüvitis
      mutate(uusvanhyv = case_when(
        #kui palk on väiksem kui vanemahüvitise määr, siis ei muutu midagi
        palk <= vhparameetridm[Aasta, "vanhyvmaar"] ~ vanhyv,
        #kui palk on suurem kui vanemahüvitise määr aga väiksem kui 5kordne vanemahüvitise määr, siis rakendub vähendamise valem
        palk >= vhparameetridm[Aasta, "vanhyvmaar"] & palk <=5*vhparameetridm[Aasta, "vanhyvmaar"] ~ (vanhyv+palk)/vhparameetridm["2004", "vahendamisemaar"]- palk,
        #Kui palk on suurem kui 5kordne vanemahüvitise määr, siis on uus hüvitis 0
        palk >= 5*vhparameetridm[Aasta, "vanhyvmaar"] ~ 0)) %>% 
      #kokku brutotulu
      mutate(brutotulukokku= uusvanhyv+palk)
    
    df2008 <- df %>%  
      mutate(aasta = 2008) %>% 
      #uus vanemahüvitis 2008
      mutate(uusvanhyv = case_when(
        #kui palk on väiksem kui vanemahüvitise määr, siis ei muutu midagi
        palk <= vhparameetridm[Aasta, "vanhyvmaar"] ~ vanhyv,
        #kui palk on suurem kui vanemahüvitise määr aga väiksem kui 5kordne vanemahüvitise määr, siis rakendub vähendamise valem
        #Vanemahüvitis ei tohi minna väiksemaks kui lapsehooldustasu
        palk >= vhparameetridm[Aasta, "vanhyvmaar"] & palk <=5*vhparameetridm[Aasta, "vanhyvmaar"] ~
          pmax(lapsehooldustasu, (vanhyv+(palk - vhparameetridm[Aasta, "vanhyvmaar"]))/vhparameetridm["2008", "vahendamisemaar"] - (palk - vhparameetridm[Aasta, "vanhyvmaar"])),
        #Kui palk on suurem kui 5kordne vanemahüvitise määr, siis on uus hüvitis lapsehooldustasu
        palk >= 5*vhparameetridm[Aasta, "vanhyvmaar"] ~ lapsehooldustasu)) %>% 
      #kokku brutotulu
      mutate(brutotulukokku= uusvanhyv+palk)
    
    df2014 <- df %>%  
      mutate(aasta = 2014) %>% 
      #uus vanemahüvitis 2014
      mutate(uusvanhyv = case_when(
        #kui palk on väiksem kui vanemahüvitise määr, siis ei muutu midagi
        palk <= vhparameetridm[Aasta, "vanhyvmaar"] ~ vanhyv,
        #kui palk on suurem kui vanemahüvitise määr, siis rakendub vähendamise valem
        #Vanemahüvitis ei tohi minna väiksemaks kui pool algset vanemahüvitist
        #Endale kontroll: kas pool vanemahüvitise määras vanemahüvitist oleks alati suurem kui lapsehooldustasu
        palk >= vhparameetridm[Aasta, "vanhyvmaar"] ~ 
          pmax(0.5*vanhyv, vanhyv - (palk - vhparameetridm[Aasta, "vanhyvmaar"])/vhparameetridm["2014", "vahendamisemaar"]))) %>% 
      #kokku brutotulu
      mutate(brutotulukokku= uusvanhyv + palk)
    
    
    df2018 <- df %>%  
      mutate(aasta = 2018) %>% 
      #uus vanemahüvitis 2018
      mutate(uusvanhyv = case_when(
        #kui palk on väiksem kui pool maksimaalsest vanemahüvitisest, siis ei muutu midagi
        palk <= 0.5*vhparameetridm[Aasta, "maksimum"] ~ vanhyv,
        #kui palk on suurem kui pool maksimaalsest vanemahüvitisest, siis rakendub vähendamise valem
        #Vanemahüvitis ei tohi minna väiksemaks kui pool algset vanemahüvitist
        #Endale kontroll: kas pool vanemahüvitise määras vanemahüvitist oleks alati suurem kui lapsehooldustasu
        palk >= 0.5*vhparameetridm[Aasta, "maksimum"] ~ 
          pmax(0.5*vanhyv, vanhyv - (palk - 0.5*vhparameetridm[Aasta, "maksimum"])/vhparameetridm["2018", "vahendamisemaar"]))) %>% 
      #kokku brutotulu
      mutate(brutotulukokku= uusvanhyv + palk)
    
    dfkoos <- bind_rows(df2004, df2008, df2014, df2018)
    
    dfpikk <- dfkoos %>% 
      pivot_longer(cols = c(uusvanhyv, brutotulukokku),
                   names_to = c("tunnus"),
                   values_to = c("vaartus"))
    
    nimedjoonisele <- c('brutotulukokku' =  'Töötasu ja VH kokku',
                        'uusvanhyv' =  'Uus VH')
    
    dfpikk$tunnus <- factor(dfpikk$tunnus,  levels = c("uusvanhyv", "brutotulukokku"))
    
    
    ggplot(dfpikk, 
           aes(x = palk, y = vaartus, color = factor(aasta), group=factor(aasta))) +
      geom_line() +
      scale_y_continuous(limits = c(0, NA)) +
      facet_wrap(~tunnus, scales = "free_y", labeller = as_labeller(nimedjoonisele)) + 
      labs(color = "", x = "Vanemahüvitise saamise ajal teenitav brutopalk",
           y = "Eur kuus", 
           title = "Samaaegse töötamise mõju vanemahüvitise suurusele ja kogu brutotulule läbi aastate",
           subtitle = paste0("Sinu valitud esialgne vanemahüvitis ", vanhyv, " eurot. \nVanemahüvitise määr ", round(vhparameetridm[Aasta, "vanhyvmaar"],1), " eurot. \nVanemahüvitise maksimum ", round(vhparameetridm[Aasta, "maksimum"],1), " eurot."))  
    #+ 
    #  theme_minimal()  
    
  })
  
  
  
}
