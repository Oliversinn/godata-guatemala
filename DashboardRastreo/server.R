#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#personas_cuarentena = read_csv("data/cuarentena_nacional.csv",guess_max = 50000)
rastreo_cases = read_csv("data/rastreo_cases.csv",guess_max = 50000, col_types = cols()) %>%
    mutate(`Creado En` = case_when(`Creado Por` == "3de6d4e6-b00d-4a71-9da1-9e7b8bbf1720" & `Creado En` == as.Date("2021-05-12") ~ `Fecha de notificacion`,
                                   T ~ `Creado En`))
rastreo_contacts = read_csv("data/rastreo_contacts.csv",guess_max = 50000, col_types = cols())
rastreo_followups = read_csv("data/rastreo_followups.csv",guess_max = 50000, col_types = cols())
reportCases = read_csv("data/report_cases.csv", guess_max = 50000, col_types = cols())




#dasGeo = read_csv('data/dasGeo.csv')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #####################################################################
    #  ___                         _       ___       _                  #
    # / __|__ _ _ _ __ _ __ _   __| |___  |   \ __ _| |_ ___ ___        #
    #| (__/ _` | '_/ _` / _` | / _` / -_) | |) / _` |  _/ _ (_-<        #
    # \___\__,_|_| \__, \__,_| \__,_\___| |___/\__,_|\__\___/__/        #
    #               |___/                                               #
    #####################################################################
    # Por ahora se cargan varios documentos por separado y el backend
    # hace un poco de la limpieza, se espera reducir a cargar solo un csv 
    # y shape files
    #####################################################################
    
    # Informacion poblacional de las DAS
    # poblacion<- read_csv("data/ine_db.csv")
    # poblacion = poblacion %>%
    #     mutate(area_salud = gsub("-OCCIDENTE", " OCCIDENTE", area_salud)) %>%
    #     mutate(area_salud = gsub("-ORIENTE", " ORIENTE", area_salud)) %>%
    #     mutate(area_salud = gsub("OCCIDENTAL", "OCCIDENTE", area_salud)) %>%
    #     mutate(area_salud = gsub("ORIENTAL", "ORIENTE", area_salud))
    
    # # POLIGONOS DE LAS DAS PARA LOS MAPAS
    # DAS_shapes = st_read("data/DAS_shapes", layer = "AreasGuate2018")
    # DAS_shapes = st_transform(DAS_shapes, "+proj=longlat +datum=WGS84")
    # colnames(DAS_shapes)[3] = 'area_salud'
    
    # DAS_shapes = DAS_shapes %>%
    #     mutate(area_salud = toupper(area_salud)) %>%
    #     mutate(area_salud = gsub("EL QUICHÉ", "QUICHÉ", area_salud)) %>%
    #     mutate(area_salud = gsub("OCCIDENTAL", "OCCIDENTE", area_salud)) %>%
    #     mutate(area_salud = gsub("ORIENTAL", "ORIENTE", area_salud))
    
    
    ############################################################################
    #COLORS
    ############################################################################
    colorVector = c("MASCULINO" = "#1f77b4", "FEMENINO" = "#ff7f0e", "SIN DATOS" = "#7f7f7f")
    colorPlotly = c("#1f77b4", "#ff7f0e", "#7f7f7f")
    ggplotColors = c('Activo'="#1f77b4",'Recuperado'="#ff7f0e", 'Imposible de contactar'='#d62728', 'Perdido'='#8c564b','Hospitalizado'='#e377c2', 'Fallecido'='#9467bd', 'Hospitalizados/Fallecidos'='#2ca02c', 'Concluído por otra razón'='#7f7f7f', 'Sin estado de seguimiento'='#bcbd22')
    donaColors = c("#1f77b4","#ff7f0e", '#2ca02c', '#d62728', '#8c564b', '#9467bd', '#e377c2', '#7f7f7f', '#bcbd22')
    
    # TABLA REACTIVE DE CASOS RASTREADOS ACUMULADOS
    rastreoCases_reactive = reactive({ 
        data = rastreo_cases %>%
            req(input$DASFilter,input$fechaReporte[1],input$fechaReporte[2],input$ClasificacionFilter, input$DMSFilter) %>%
            filter(if(input$DASFilter != 'TODOS')  (area_salud %like% input$DASFilter) else TRUE,
                   if(input$DMSFilter != 'TODOS')  (dms %like% input$DMSFilter) else TRUE,
                   if(input$ClasificacionFilter != 'TODOS')  (Clasificacion == input$ClasificacionFilter | `¿Se tomo una muestra respiratoria?` == 3) else TRUE,
                   #if(input$unidadNotificadoraFilter != 'TODOS')  (`Clinicas Temporales` == input$unidadNotificadoraFilter) else TRUE,
                   `Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2]),
                   Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                   Clasificacion != 'SOSPECHOSO E') 
    })
    
    # TABLA REACTIVE DE CASOS RASTREADOS ACTIVOS
    rastreoCasesActive_reactive = reactive({ 
        data = rastreo_cases %>%
            req(input$DASFilter,input$fechaReporte[1],input$fechaReporte[2],input$ClasificacionFilter) %>%
            filter(if(input$DASFilter != 'TODOS')  (area_salud %like% input$DASFilter) else TRUE,
                   if(input$DMSFilter != 'TODOS')  (dms %like% input$DMSFilter) else TRUE,
                   if(input$ClasificacionFilter != 'TODOS')  (Clasificacion == input$ClasificacionFilter | `¿Se tomo una muestra respiratoria?` == 3) else TRUE,
                   #if(input$unidadNotificadoraFilter != 'TODOS')  (`Clinicas Temporales` == input$unidadNotificadoraFilter) else TRUE,
                   `Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2]),
                   `Estado de seguimiento` == 'Activo',
                   Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                   Clasificacion != 'SOSPECHOSO E')
    })
    
    reportCases_reactive = reactive({ 
        data = reportCases %>%
            req(input$DASFilter,input$fechaReporte[1],input$fechaReporte[2],input$ClasificacionFilter, input$DMSFilter) %>%
            filter(if(input$DASFilter != 'TODOS')  (area_salud %like% input$DASFilter) else TRUE,
                   if(input$DMSFilter != 'TODOS')  (dms %like% input$DMSFilter) else TRUE,
                   if(input$ClasificacionFilter != 'TODOS')  (Clasificacion == input$ClasificacionFilter | `¿Se tomo una muestra respiratoria?` == 3) else TRUE,
                   #if(input$unidadNotificadoraFilter != 'TODOS')  (`Clinicas Temporales` == input$unidadNotificadoraFilter) else TRUE,
                   `Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2]),
                   Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                   Clasificacion != 'SOSPECHOSO E') 
    })
    
    
    
    dmses = reactive({
        rastreoCases_reactive() %>%
            select(dms) %>%
            unique() %>%
            arrange(dms)
    })
    
    dmsList = reactive({
        append(c("TODOS"),dmses()$dms)
        })
    
    observe({
        updateSelectInput(session, "DMSFilter",
                          choices = dmsList(),
                          selected = input$DMSFilter
        )})
    
    dases = reactive({
        rastreoCases_reactive() %>%
            select(area_salud) %>%
            unique() %>%
            arrange(area_salud)
    })
    
    dasList = reactive({
        append(c("TODOS"),dases()$area_salud)
    })
    
    observe({
        updateSelectInput(session, "DASFilter",
                          choices = dasList(),
                          selected = input$DASFilter
        )})
    
    
    

    # TABLA REACTIVE DE CONTACTOS RASTREADOS ACUMULADOS
    rastreoContacts_reactive = reactive({ 
        data = rastreo_contacts %>%
            req(input$DASFilter,input$fechaReporte[1],input$fechaReporte[2]) %>%
            filter(if(input$DASFilter != 'TODOS')  (area_salud %like% input$DASFilter) else TRUE,
                   if(input$DMSFilter != 'TODOS')  (dms %like% input$DMSFilter) else TRUE,
                   `Caso relacionado` %in% unique(rastreoCases_reactive()$`Carne De Identidad`),
                   `Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2]))
    })

    # TABLA REACTIVE DE CONTACTOS RASTREADOS ACTIVOS
    rastreoContactsActive_reactive = reactive({ 
        data = rastreo_contacts %>%
            req(input$DASFilter,input$fechaReporte[1],input$fechaReporte[2]) %>%
            filter(if(input$DASFilter != 'TODOS')  (area_salud %like% input$DASFilter) else TRUE,
                   if(input$DMSFilter != 'TODOS')  (dms %like% input$DMSFilter) else TRUE,
                   `Caso relacionado` %in% unique(rastreoCases_reactive()$`Carne De Identidad`),
                   `Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2]),
                   `Status final de seguimiento` == 'Bajo Seguimiento') 
    })
    

    
    
    ##########################################################################
    ##########################################################################
    #####################   CASOS DEL RASTREO    #############################
    ##########################################################################
    ##########################################################################
    
    #### INDICADORES ####

    output$rastreoCases <- shinydashboard::renderValueBox({
        casos_acumulado <- nrow(rastreoCases_reactive())
        casos_acumulado <- format(casos_acumulado, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Acumulados",
            value = casos_acumulado,
            icon = icon("head-side-virus"),
            color = "green"
        )
    })
    
    output$rastreoCasesContactable <- shinydashboard::renderValueBox({
        casos_acumulado <- nrow(rastreoCases_reactive() %>% filter(`Telefono` == 'CONTACTABLE'))
        casos_acumulado <- format(casos_acumulado, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Acumulados Contactables",
            value = casos_acumulado,
            icon = icon("phone"),
            color = "green"
        )
    })
    
    output$rastreoCasesActive <- shinydashboard::renderValueBox({
        casos_activos = nrow(rastreoCasesActive_reactive())
        casos_activos <- format(casos_activos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Activos",
            value = casos_activos,
            icon = icon("virus"),
            color = "yellow"
        )
    })
    
    output$rastreoCasesActiveContactable <- shinydashboard::renderValueBox({
        casos_activos = nrow(rastreoCasesActive_reactive() %>% filter(`Telefono` == 'CONTACTABLE'))
        casos_activos <- format(casos_activos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Activos Contactables",
            value = casos_activos,
            icon = icon("phone"),
            color = "yellow"
        )
    })

    output$rastreoCasesFromContactsContactable <- shinydashboard::renderValueBox({
        casosContactables = rastreoCases_reactive() %>%
            select(`Telefono`) %>%
            filter(`Telefono` == 'CONTACTABLE')
        
        casosContactos = rastreoCases_reactive() %>%
            select(`Fue Un Contacto`) %>%
            filter(`Fue Un Contacto` == TRUE)

        casos = round((nrow(casosContactos)/nrow(casosContactables))*100, digits = 0)
        casos <- format(casos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        casos = paste(as.character(casos),'%',sep = '')
        
        shinydashboard::valueBox(
            "Casos por nexo epidemiologico contactables.",
            value = casos,
            icon = icon(""),
            color = "blue"
        )
    })

    output$rastreoContactsToCases <- shinydashboard::renderValueBox({
        contacts = rastreoContacts_reactive() 
        
        casosContactos = rastreoCases_reactive() %>%
            select(`Fue Un Contacto`) %>%
            filter(`Fue Un Contacto` == TRUE)

        contactos = round((nrow(casosContactos)/(nrow(contacts)+nrow(casosContactos)))*100, digits = 0)
        contactos <- format(contactos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        contactos = paste(as.character(contactos),'%', sep = '')
        
        shinydashboard::valueBox(
            "Contactos que se volvieron casos.",
            value = contactos,
            icon = icon(""),
            color = "blue"
        )
    })

    output$rastreoCasesRecuperados <- shinydashboard::renderValueBox({
        casosContactables = rastreoCases_reactive() %>%
            select(`Telefono`) %>%
            filter(`Telefono` == 'CONTACTABLE')
        
        casosRecuperados = rastreoCases_reactive() %>%
            select(`Estado de seguimiento`) %>%
            filter(`Estado de seguimiento` == 'Recuperado')

        casos = round((nrow(casosRecuperados)/nrow(casosContactables))*100, digits = 0)
        casos <- format(casos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        casos = paste(as.character(casos),"%",sep="")
        
        shinydashboard::valueBox(
            "Casos recuperados",
            value = casos,
            icon = icon(""),
            color = "blue"
        )
    })
    
    output$rastreoContactsCompleto <- shinydashboard::renderValueBox({
        contacts = rastreoContacts_reactive() 
        
        contactsCompleto = rastreoContacts_reactive() %>%
            select(`Status final de seguimiento`) %>%
            filter(`Status final de seguimiento` == "Seguimiento Completo")

        contactos = round((nrow(contactsCompleto)/nrow(contacts))*100, digits = 0)
        contactos <- format(contactos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        contactos = paste(as.character(contactos),'%',sep = '')
        
        shinydashboard::valueBox(
            "Contactos con seguimiento completo.",
            value = contactos,
            icon = icon(""),
            color = "blue"
        )
    })
    

    
    ################ MAPAS ###################
    
    #### Por DAS acumulado
    # output$rastreoCasesMap = renderLeaflet({
    #     por_DAS = rastreoCases_reactive() %>%
    #         filter(`Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2])) %>%
    #         group_by(area_salud) %>%
    #         tally()
        
    #     DAS_shapes = left_join(DAS_shapes, por_DAS, by = c("area_salud"="area_salud"))
    #     rm(por_DAS)
        
    #     DAS_pob = poblacion %>%
    #         select(area_salud,poblacion) %>%
    #         group_by(area_salud) %>%
    #         summarise(poblacion = round(sum(poblacion),0))
        
    #     DAS_shapes = left_join(DAS_shapes, DAS_pob, by = c("area_salud"="area_salud"))
    #     rm(DAS_pob)
        
    #     DAS_shapes = DAS_shapes %>%
    #         mutate(poblacion = ifelse(is.na(poblacion),0,poblacion)) %>%
    #         mutate(n = ifelse(is.na(n),0,n)) %>%
    #         mutate(incidencia_100k = ((n/ifelse(is.na(poblacion),1,poblacion)) * 100000 ) %>% round(1))
        
    #     pal <- colorNumeric(c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd","#08519c"), domain = DAS_shapes$incidencia_100k)
    #     label_incidencia_DAS <- sprintf(
    #         "<strong>%s</strong><br/>Personas en cuarentena: %s<br/>Por cada 100,000 hab: %s<br/>Poblacion: %s",
    #         DAS_shapes$area_salud,
    #         DAS_shapes$n,
    #         DAS_shapes$incidencia_100k,
    #         format(DAS_shapes$poblacion, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)) %>% 
    #         lapply(HTML)
        
    #     leaflet(DAS_shapes) %>%
    #         req(input$DASFilter) %>%
    #         addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
    #         addPolygons(
    #             # fill
    #             group = "Departamentos",
    #             fillColor   = ~pal(incidencia_100k),
    #             fillOpacity = 1,
    #             # line
    #             dashArray   = "3",
    #             weight      = 2,
    #             color       = "grey",
    #             opacity     = 1,
    #             # interaction
    #             highlight = highlightOptions(
    #                 weight = 5,
    #                 color = "#666",
    #                 dashArray = "",
    #                 fillOpacity = 1,
    #                 bringToFront = TRUE),
    #             label = label_incidencia_DAS,
    #             labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "15px",
    #                 direction = "bottom")
    #         ) %>%
    #         addLegend(
    #             pal = pal, values = ~incidencia_100k, opacity = 0.7, title = HTML("Por cada<br>100,000<br>habitantes"),
    #             position = "topright", group = "Departamentos"
    #         ) %>%
    #         setView(lat = dasGeo[dasGeo$DAS == input$DASFilter ,'lat'], lng = dasGeo[dasGeo$DAS == input$DASFilter ,'lng'], zoom = ifelse(input$DASFilter=='TODOS',7,8))
    # })
    
    # #### Por DAS activo
    # output$rastreoCasesActiveMap = renderLeaflet({
    #     por_DAS = rastreoCasesActive_reactive() %>%
    #         group_by(area_salud) %>%
    #         tally()
        
    #     DAS_shapes = left_join(DAS_shapes, por_DAS, by = c("area_salud"="area_salud"))
    #     rm(por_DAS)
        
    #     DAS_pob = poblacion %>%
    #         select(area_salud,poblacion) %>%
    #         group_by(area_salud) %>%
    #         summarise(poblacion = round(sum(poblacion),0))
        
    #     DAS_shapes = left_join(DAS_shapes, DAS_pob, by = c("area_salud"="area_salud"))
    #     rm(DAS_pob)
        
    #     DAS_shapes = DAS_shapes %>%
    #         mutate(poblacion = ifelse(is.na(poblacion),0,poblacion)) %>%
    #         mutate(n = ifelse(is.na(n),0,n)) %>%
    #         mutate(incidencia_100k = ((n/ifelse(is.na(poblacion),1,poblacion)) * 100000 ) %>% round(1))
        
    #     pal <- colorNumeric(c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd","#08519c"), domain = DAS_shapes$incidencia_100k)
    #     label_incidencia_DAS <- sprintf(
    #         "<strong>%s</strong><br/>Personas en cuarentena: %s<br/>Por cada 100,000 hab: %s<br/>Poblacion: %s",
    #         DAS_shapes$area_salud,
    #         DAS_shapes$n,
    #         DAS_shapes$incidencia_100k,
    #         format(DAS_shapes$poblacion, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)) %>% 
    #         lapply(HTML)
        
    #     leaflet(DAS_shapes) %>%
    #         req(input$DASFilter) %>%
    #         addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
    #         addPolygons(
    #             # fill
    #             group = "Departamentos",
    #             fillColor   = ~pal(incidencia_100k),
    #             fillOpacity = 1,
    #             # line
    #             dashArray   = "3",
    #             weight      = 2,
    #             color       = "grey",
    #             opacity     = 1,
    #             # interaction
    #             highlight = highlightOptions(
    #                 weight = 5,
    #                 color = "#666",
    #                 dashArray = "",
    #                 fillOpacity = 1,
    #                 bringToFront = TRUE),
    #             label = label_incidencia_DAS,
    #             labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "15px",
    #                 direction = "bottom")
    #         ) %>%
    #         addLegend(
    #             pal = pal, values = ~incidencia_100k, opacity = 0.7, title = HTML("Por cada<br>100,000<br>habitantes"),
    #             position = "topright", group = "Departamentos"
    #         ) %>%
    #         setView(lat = dasGeo[dasGeo$DAS == input$DASFilter ,'lat'], lng = dasGeo[dasGeo$DAS == input$DASFilter ,'lng'], zoom = ifelse(input$DASFilter=='TODOS',7,8))
    # })
    
    #### PIE DE CUARENTENA POR SEXO
    rastreoCases_sexo = reactive({
        df = rastreoCases_reactive() %>%
            mutate(Sexo = factor(Sexo,levels = c("MASCULINO", "FEMENINO","SIN DATOS") )) %>%
            group_by(Sexo) %>%
            tally() %>%
            rename(N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%")) 
    })
    
    output$rastreoCasesSexo = renderPlotly({
        fig = plot_ly(rastreoCases_sexo(),
                      labels = ~Sexo,
                      values = ~N,
                      text = ~paste0(round((N / sum(N))*100, 0),"%"),
                      textinfo='text',
                      hoverinfo = ~N,
                      type = 'pie',
                      hole = 0.6,
                      sort = FALSE,
                      marker = list(colors=colorPlotly))
        fig = fig %>% 
            layout(title = 'Casos COVID-19<br>por Sexo',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
            plotly::config(displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })
    
    output$rastreoCasesSexoDB = renderDataTable(
        datatable(
            rastreoCases_sexo() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosPorSexo'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosPorSexo'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    rastreoCasesActive_sexo = reactive({
        df = rastreoCasesActive_reactive() %>%
            mutate(Sexo = factor(Sexo,levels = c("MASCULINO", "FEMENINO","SIN DATOS") )) %>%
            filter(Condicion != 'Fallecido' |
                   Condicion != 'Recuperado' | 
                   is.na(Condicion)) %>%
            group_by(Sexo) %>%
            tally() %>%
            rename(N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%")) 
    })
    
   
    output$rastreoCasesActiveSexo = renderPlotly({
        fig = plot_ly(rastreoCasesActive_sexo(),
                      labels = ~Sexo,
                      values = ~N,
                      text = ~paste0(round((N / sum(N))*100, 0),"%"),
                      textinfo='text',
                      hoverinfo = ~N,
                      type = 'pie',
                      hole = 0.6,
                      sort = FALSE,
                      marker = list(colors=colorPlotly))
        fig = fig %>% 
            layout(title = 'Casos COVID-19 Activos<br>por Sexo',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
            plotly::config(displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })
    
    output$rastreoCasesActiveSexoDB = renderDataTable(
        datatable(
            rastreoCasesActive_sexo() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosActivosPorSexo'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosActivosPorSexo'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    
    #### DATA FRAME REACTIVE DE CASOS RASTREADOS POR CRUPO ETARIO C/10 ANOS
    rastreoCasesAgeGroup_reactive = reactive({
        df = rastreoCases_reactive() %>%
            group_by(grupo_etario,Sexo) %>%
            tally() %>%
            rename(`Grupo Etario` = grupo_etario, N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%"))
    })
    
    #### Plot casos rastreados por edad y sexo
    output$rastreoCasesEdad = renderPlotly({
        p = rastreoCasesAgeGroup_reactive() %>%
            ggplot(aes(x = `Grupo Etario`, y = N, fill = Sexo)) +
            geom_bar(stat = "identity", position = "stack") + 
            ggtitle("Casos COVID-19<br>por Grupo Etario") +
            scale_fill_manual(values = colorVector)+
            ylab("No. de Casos")
        
        ggplotly(p + theme(plot.title = element_text(hjust=0.5), 
                           legend.position="bottom",
                           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                           panel.grid.major = element_blank(), 
                           legend.title = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank()), 
                 tooltip = c("N")) %>% 
            #layout(legend = list(orientation = "h",  y = -0.4)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })
    
    output$rastreoCasesEdadDB = renderDataTable(
        datatable(
            rastreoCasesAgeGroup_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosPorGrupoEtario'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosPorGrupoEtario'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    #### DATA FRAME REACTIVE DE CASOS RASTREADOS ACTIVOS POR CRUPO ETARIO C/10 ANOS
    rastreoCasesActiveAgeGroup_reactive = reactive({
        df = rastreoCasesActive_reactive() %>%
            group_by(grupo_etario,Sexo) %>%
            tally() %>%
            rename(`Grupo Etario` = grupo_etario, N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%"))
    })
    
    #### Plot en cuarentena por edad y sexo
    output$rastreoCasesActiveEdad = renderPlotly({
        p = rastreoCasesActiveAgeGroup_reactive() %>%
            ggplot(aes(x = `Grupo Etario`, y = N, fill = Sexo)) +
            geom_bar(stat = "identity", position = "stack") + 
            ggtitle("Casos COVID-19 Activos<br>por Grupo Etario") +
            scale_fill_manual(values = colorVector)+
            ylab("No. de Casos")
        
        ggplotly(p + theme(plot.title = element_text(hjust=0.5), 
                           legend.position="bottom",
                           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                           panel.grid.major = element_blank(),
                           legend.title = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank()), 
                 tooltip = c("N")) %>% 
            #layout(legend = list(orientation = "h",  y = -0.3)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })
    
    output$rastreoCasesActiveEdadDB = renderDataTable(
        datatable(
            rastreoCasesActiveAgeGroup_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosActivosPorGrupoEtario'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosActivosPorGrupoEtario'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    #### Tabla de datos Personas en cuarentena por edad y sexo
    output$tablaRastreoCasesEdad = renderDataTable(datatable(rastreoCasesAgeGroup_reactive() %>% 
                                                               adorn_totals("row"),
                                                           options = list(pageLength = 5)))
    
    
    #### cuadro de datos casos rastreados por creado en
    rastreoCasesCreadoEn_reactive = reactive({
        enCuarentena = rastreoCases_reactive() %>%
            select(`Creado En`, `Estado de seguimiento`) %>%
            complete(`Creado En` = seq.Date(min(input$fechaReporte[1]),max(input$fechaReporte[2]), by='day')) %>%
            group_by(`Creado En`, `Estado de seguimiento`) %>%
            mutate(`Creado En` = as.Date(`Creado En`, format="%d/%m/%Y")) %>%
            filter(`Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2])) %>%
            arrange(`Creado En`) %>%
            tally() %>%
            mutate(n = ifelse(`Creado En` %in% rastreoCases_reactive()$`Creado En`, n, 0 ),
                   `Estado de seguimiento` = ifelse(is.na(`Estado de seguimiento`),'Sin estado de seguimiento', `Estado de seguimiento`),
                   `Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Activo","Recuperado","Imposible de contactar",
                                                                                        'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                                                        "Sin estado de seguimiento")))
    })
    
    #### plot casos rastreados por Creado En
    output$rastreoCasesFechaDeNotificacion = renderPlotly({
        enCuarentena = rastreoCasesCreadoEn_reactive()
        la_escala = case_when(as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 31 ~ "1 days",
                              as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 61 ~ "2 days",
                              as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 120 ~ "7 days",
                              T ~ "15 days")
        p = ggplot(enCuarentena, aes(x=`Creado En`, y=n, fill=`Estado de seguimiento`)) +
            geom_bar(stat="identity") +
            scale_fill_manual(values = ggplotColors) +
            labs(x="Fecha", y="No. de Casos")+ 
            scale_x_date(breaks = la_escala, date_labels = "%d/%m",expand = c(0,0),
                         limits = c(input$fechaReporte[1]-1,max(enCuarentena$`Creado En`)+1))
        
        rm(enCuarentena)
        
        ggplotly(p + theme(plot.title = element_text(hjust=0.5,vjust=1),
                           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank()
                           ), tooltip = c("n",'Creado En','Estado de seguimiento')) %>%
            layout(legend = list(orientation = "h",  y = -0.3),
                    title = list(text = paste0('Seguimiento de casos COVID-19 <br> leve/moderado',
                                    '<br>',
                                    '<sup>',
                                    '(N= ',
                                    nrow(rastreoCases_reactive()),
                                    ')',
                                    '</sup>'), y = 0.95)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)        
    })
    
    output$rastreoCasesFechaDeNotificacionDB = renderDataTable(
        datatable(
            rastreoCasesCreadoEn_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosPorFechaDeCreadoEn'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosPorFechaDeCreadoEn'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    
    #### cuadro de datos casos contactables rastreados por creado en
    rastreoCasesContactableCreadoEn_reactive = reactive({
        enCuarentena = rastreoCases_reactive() %>%
            filter(Telefono == 'CONTACTABLE') %>%
            select(`Creado En`, `Estado de seguimiento`) %>%
            complete(`Creado En` = seq.Date(min(input$fechaReporte[1]),max(input$fechaReporte[2]), by='day')) %>%
            group_by(`Creado En`, `Estado de seguimiento`) %>%
            mutate(`Creado En` = as.Date(`Creado En`, format="%d/%m/%Y")) %>%
            filter(`Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2])) %>%
            arrange(`Creado En`) %>%
            tally() %>%
            mutate(n = ifelse(`Creado En` %in% rastreoCases_reactive()$`Creado En`, n, 0 ),
                   `Estado de seguimiento` = ifelse(is.na(`Estado de seguimiento`),'Sin estado de seguimiento', `Estado de seguimiento`),
                   `Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Activo","Recuperado","Imposible de contactar",
                                                                                        'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                                                        "Sin estado de seguimiento")))
    })

    #### Casos rastreados contactables por creado En
    output$rastreoCasesContactableFechaDeNotificacion = renderPlotly({
        enCuarentena = rastreoCasesContactableCreadoEn_reactive()
        
        la_escala = case_when(as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 31 ~ "1 days",
                              as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 61 ~ "2 days",
                              as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 120 ~ "7 days",
                              T ~ "15 days")
        p = ggplot(enCuarentena, aes(x=`Creado En`, y=n, fill=`Estado de seguimiento`)) +
            geom_bar(stat="identity") +
            scale_fill_manual(values = ggplotColors) +
            labs(x="Fecha", y="No. de Casos")+ 
            scale_x_date(breaks = la_escala, date_labels = "%d/%m",expand = c(0,0),
                         limits = c(input$fechaReporte[1]-1,max(enCuarentena$`Creado En`)+1))
        
        rows = nrow(rastreoCases_reactive() %>% filter(Telefono == 'CONTACTABLE'))
        rm(enCuarentena)
        
        ggplotly(p + theme(plot.title = element_text(hjust=0.5,vjust=1),
                           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank()
                           ), tooltip = c("n",'Creado En','Estado de seguimiento')) %>%
            layout(legend = list(orientation = "h",  y = -0.3),
                    title = list(text = paste0('Seguimiento de casos COVID-19 <br>contactables leve/moderado',
                                    '<br>',
                                    '<sup>',
                                    '(N= ',
                                    rows,
                                    ')',
                                    '</sup>'), y = 0.95)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)        
    })
    
    output$rastreoCasesContactableFechaDeNotificacionDB = renderDataTable(
        datatable(
            rastreoCasesContactableCreadoEn_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosContactablesPorFechaDeCreadoEn'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosContactablesPorFechaDeCreadoEn'
                    )
                ),
                scrollx = T
            )
        )
    )

    # DB Distribucion de los Estados de Seguimiento en Casos 
    casosEstadosDeSeguimiento_reactive = reactive({
        estadosDeSeguimiento = rastreoCases_reactive() %>%
            complete(`Estado de seguimiento` = factor(`Estado de seguimiento`, 
                                                      levels = c("Activo","Recuperado","Imposible de contactar",
                                                                 'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                                 "Sin estado de seguimiento"))) %>%
            select(`Estado de seguimiento`) %>%
            group_by(`Estado de seguimiento`) %>%
            tally() %>%
            mutate(`Estado de seguimiento` = fct_reorder(`Estado de seguimiento`,n),
                   n = ifelse(`Estado de seguimiento` %in% rastreoCases_reactive()$`Estado de seguimiento`, n, 0 )
            )
    })
    
    output$rastreoCasesEstadoDeSeguimientoDB = renderDataTable(
        datatable(
            casosEstadosDeSeguimiento_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosEstadosDeSeguimiento'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosEstadosDeSeguimiento'
                    )
                ),
                scrollx = T
            )
        )
    )

    # Distribucion de los Estados de Seguimiento en Casos
    output$rastreoCasesEstadoDeSeguimiento = renderPlotly({
        estadosDeSeguimiento = casosEstadosDeSeguimiento_reactive()

        p = ggplot(estadosDeSeguimiento, aes(x=`Estado de seguimiento`, y=n)) +
            geom_bar(stat='identity', fill='steelblue') +
            labs(x="Estado de Seguimiento", y = "No. de Casos") 

        rm(estadosDeSeguimiento)

        ggplotly(p + theme(plot.title = element_text(hjust=0.5),
                           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1,),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank()
                           ), tooltip = c("n")) %>%
            layout(legend = list(orientation = "h",  y = 0),
                    showlegend = FALSE,
                    title = list(text = paste0('Estados de seguimiento',
                                    '<br>',
                                    '<sup>',
                                    '(N= ',
                                    nrow(rastreoCases_reactive() ),
                                    ')',
                                    '</sup>'), y = 0.95)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)   
            
    })
    
    # DB Distribucion de los Estados de Seguimiento en Casos Contactables
    casosContactableEstadosDeSeguimiento_reactive = reactive({
        estadosDeSeguimiento = rastreoCases_reactive() %>%
            filter(Telefono == 'CONTACTABLE') %>%
            complete(`Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Activo","Recuperado","Imposible de contactar",
                                                                                          'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                                                          "Sin estado de seguimiento"))) %>%
            select(`Estado de seguimiento`) %>%
            group_by(`Estado de seguimiento`) %>%
            tally() %>%
            mutate(`Estado de seguimiento` = fct_reorder(`Estado de seguimiento`,n),
                   n = ifelse(`Estado de seguimiento` %in% rastreoCases_reactive()$`Estado de seguimiento`, n, 0 )
            )
    })
    
    output$rastreoCasesContactableEstadoDeSeguimientoDB = renderDataTable(
        datatable(
            casosContactableEstadosDeSeguimiento_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosContactablesEstadosDeSeguimiento'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosContactablesEstadosDeSeguimiento'
                    )
                ),
                scrollx = T
            )
        )
    )

    # Distribucion de los Estados de Seguimiento en Casos Contactables
    output$rastreoCasesContactableEstadoDeSeguimiento = renderPlotly({
        estadosDeSeguimiento = casosContactableEstadosDeSeguimiento_reactive()

        p = ggplot(estadosDeSeguimiento, aes(x=`Estado de seguimiento`, y=n)) +
            geom_bar(stat='identity', fill='steelblue') +
            labs(x="Estado de Seguimiento", y = "No. de Casos") 

        rm(estadosDeSeguimiento)

        ggplotly(p + theme(plot.title = element_text(hjust=0.5),
                           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1,),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank()
                           ), tooltip = c("n")) %>%
            layout(legend = list(orientation = "h",  y = 0),
                    showlegend = FALSE,
                    title = list(text = paste0('Estados de seguimiento',
                                    '<br>',
                                    '<sup>Casos contactables</sub><br>',
                                    '<sup>',
                                    '(N= ',
                                    nrow(rastreoCases_reactive() %>% filter(Telefono == 'CONTACTABLE')),
                                    ')',
                                    '</sup>'), y = 0.95)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)   
            
    })

    #### PIE DE ESTADOS DE SEGUIMIENTOS DE CASOS
    output$rastreoCasesEstadoDeSeguimientoDona = renderPlotly({
        enCuarentena = rastreoCases_reactive() %>%
            complete(`Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Activo","Recuperado","Imposible de contactar",
                                                                                          'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                                                          "Sin estado de seguimiento"))) %>%
            select(`Estado de seguimiento`) %>%
            group_by(`Estado de seguimiento`) %>%
            tally() %>%
            mutate(`Estado de seguimiento` = fct_reorder(`Estado de seguimiento`,n),
                   n = ifelse(`Estado de seguimiento` %in% rastreoCases_reactive()$`Estado de seguimiento`, n, 0 ))

        fig = plot_ly(enCuarentena,
                      labels = ~`Estado de seguimiento`,
                      values = ~n,
                      textposition = 'inside',
                      text = ~paste( as.character(round((n/sum(n))*100,0)),'%',sep = ''),
                      textinfo = 'text',
                      hovertemplate = ~paste('<b>',`Estado de seguimiento`,'</b><br>',
                                    'N: ', n,' <extra></extra> '),
                      type = 'pie',
                      hole = 0.6,
                      sort = F,
                      marker = list(colors=factor(c("Activo","Recuperado","Imposible de contactar",
                                                    'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                    "Sin estado de seguimiento"), labels = donaColors))) %>%
            layout(legend = list(orientation = 'h'),
                   title = ~paste('Estados de Seguimiento<br><sup>(N = ',nrow(rastreoCases_reactive()),')</sup>'))

    })

    #### PIE DE ESTADOS DE SEGUIMIENTOS DE CASOS CONTACTABLES
    output$rastreoCasesContactableEstadoDeSeguimientoDona = renderPlotly({
        enCuarentena = rastreoCases_reactive() %>%
            filter(Telefono == 'CONTACTABLE') %>%
            complete(`Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Activo","Recuperado","Imposible de contactar",
                                                                                          'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                                                          "Sin estado de seguimiento"))) %>%
            select(`Estado de seguimiento`) %>%
            group_by(`Estado de seguimiento`) %>%
            tally() %>%
            mutate(`Estado de seguimiento` = fct_reorder(`Estado de seguimiento`,n),
                   n = ifelse(`Estado de seguimiento` %in% rastreoCases_reactive()$`Estado de seguimiento`, n, 0 ))

        fig = plot_ly(enCuarentena,
                      labels = ~`Estado de seguimiento`,
                      values = ~n,
                      textposition = 'inside',
                      text = ~paste( as.character(round((n/sum(n))*100,0)),'%',sep = ''),
                      textinfo = 'text',
                      hovertemplate = ~paste('<b>',`Estado de seguimiento`,'</b><br>',
                                             'N: ', n,' <extra></extra> '),
                      type = 'pie',
                      hole = 0.6,
                      sort = FALSE,
                      marker = list(colors=factor(c("Activo","Recuperado","Imposible de contactar",
                                                    'Perdido',"Fallecido", "Hospitalizado", "Hospitalizados/Fallecidos","Concluído por otra razón",
                                                    "Sin estado de seguimiento"), labels = donaColors))) %>%
            layout(title = ~paste('Estados de Seguimiento<br><sup>(N = ',
                                    nrow(rastreoCases_reactive() %>% filter(Telefono=='CONTACTABLE')),
                                    ')</sup>'))

    })







    ##########################################################################
    ##########################################################################
    #####################   CONTACTOS DEL RASTREO    #############################
    ##########################################################################
    ##########################################################################

    output$rastreoContacts <- shinydashboard::renderValueBox({
        contactos_acumulado <- nrow(rastreoContacts_reactive())
        contactos_acumulado <- format(contactos_acumulado, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Acumulados",
            value = contactos_acumulado,
            icon = icon("shield-virus"),
            color = "green"
        )
    })
    
    output$rastreoContactsContactable <- shinydashboard::renderValueBox({
        contactos_acumulado <- nrow(rastreoContacts_reactive() %>% filter(`Telefono` == 'CONTACTABLE'))
        contactos_acumulado <- format(contactos_acumulado, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Acumulados Contactables",
            value = contactos_acumulado,
            icon = icon("phone"),
            color = "green"
        )
    })
    
    output$rastreoContactsActive <- shinydashboard::renderValueBox({
        contactos_activos = nrow(rastreoContactsActive_reactive())
        contactos_activos <- format(contactos_activos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Activos",
            value = contactos_activos,
            icon = icon("head-side-mask"),
            color = "yellow"
        )
    })
    
    output$rastreoContactsActiveContactable <- shinydashboard::renderValueBox({
        contactos_activos = nrow(rastreoContactsActive_reactive() %>% filter(`Telefono` == 'CONTACTABLE'))
        contactos_activos <- format(contactos_activos, decimal.mark=".",big.mark=",",small.mark=".",small.interval=3)
        
        shinydashboard::valueBox(
            "Activos Contactables",
            value = contactos_activos,
            icon = icon("phone"),
            color = "yellow"
        )
    })
    
    #### DB contactos rastreados por Creado En
    rastreoContactsFechaDeNotificacion_reactive = reactive({
        enCuarentena = rastreoContacts_reactive() %>%
            select(`Creado En`, `Status final de seguimiento`) %>%
            complete(`Creado En` = seq.Date(min(input$fechaReporte[1]),max(input$fechaReporte[2]), by='day')) %>%
            group_by(`Creado En`, `Status final de seguimiento`) %>%
            mutate(`Creado En` = as.Date(`Creado En`, format="%d/%m/%Y")) %>%
            filter(`Creado En` >= format(input$fechaReporte[1]) & `Creado En` <= format(input$fechaReporte[2])) %>%
            arrange(`Creado En`) %>%
            tally() %>%
            mutate(n = ifelse(`Creado En` %in% rastreoContacts_reactive()$`Creado En`, n, 0 ),
                   `Status final de seguimiento` = ifelse(is.na(`Status final de seguimiento`),'Sin Datos', `Status final de seguimiento`))
    })
    
    output$rastreoContactsFechaDeNotificacionDB = renderDataTable(
        datatable(
            rastreoContactsFechaDeNotificacion_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'contactosPorFechaCreadoEn'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'contactosPorFechaCreadoEn'
                    )
                ),
                scrollx = T
            )
        )
    )

    #### contactos rastreados por Creado En
    output$rastreoContactsFechaDeNotificacion = renderPlotly({
        enCuarentena = rastreoContactsFechaDeNotificacion_reactive()
        la_escala = case_when(as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 31 ~ "1 days",
                              as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 61 ~ "2 days",
                              as.numeric(as.Date(format(input$fechaReporte[2])) - as.Date(format(input$fechaReporte[1]))) <= 120 ~ "7 days",
                              T ~ "15 days")
        p = ggplot(enCuarentena, aes(x=`Creado En`, y=n, fill=`Status final de seguimiento`)) +
            geom_bar(stat="identity") +
            labs(x="Fecha", y="No. de Contactos", title = "Contactos en Seguimiento", subtitle = "Por Creado En")+ 
            scale_x_date(breaks = la_escala, date_labels = "%d/%m",expand = c(0,0),
                         limits = c(input$fechaReporte[1]-1,max(enCuarentena$`Creado En`)+1))
        
        rm(enCuarentena)
        
        ggplotly(p + theme(plot.title = element_text(hjust=0.5),
                           plot.subtitle=element_text(hjust=0.5),
                           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank()
                           ), tooltip = c("n",'Creado En','Status final de seguimiento')) %>%
            layout(legend = list(orientation = "h",  y = -0.3),
                    title = list(text = paste0('Contactos COVID-19 en seguimiento',
                                    '<br>',
                                    '<sup>',
                                    '(N= ',
                                    nrow(rastreoContacts_reactive()),
                                    ')',
                                    '</sup>'))) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })

    #### PIE DE CUARENTENA POR SEXO
    rastreoContacts_sexo = reactive({
        df = rastreoContacts_reactive() %>%
            mutate(Sexo = factor(Sexo,levels = c("MASCULINO", "FEMENINO","SIN DATOS") )) %>%
            group_by(Sexo) %>%
            tally() %>%
            rename(N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%")) 
    })
    
    output$rastreoContactsSexoDB = renderDataTable(
        datatable(
            rastreoContacts_sexo() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'contactosPorSexo'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'contactosPorSexo'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    output$rastreoContactsSexo = renderPlotly({
        fig = plot_ly(rastreoContacts_sexo(),
                      labels = ~Sexo,
                      values = ~N,
                      text = ~paste0(round((N / sum(N))*100, 0),"%"),
                      textinfo='text',
                      hoverinfo = ~N,
                      type = 'pie',
                      hole = 0.6,
                      sort = FALSE,
                      marker = list(colors=colorPlotly))
        fig = fig %>% 
            layout(title = 'Contactos de casos COVID-19<br>por sexo',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
            plotly::config(displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })
    
    
    rastreoContactsActive_sexo = reactive({
        df = rastreoContactsActive_reactive() %>%
            mutate(Sexo = factor(Sexo,levels = c("MASCULINO", "FEMENINO","SIN DATOS") )) %>%
            group_by(Sexo) %>%
            tally() %>%
            rename(N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%")) 
    })
    
    output$rastreoContactsActiveSexoDB = renderDataTable(
        datatable(
            rastreoContactsActive_sexo() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'contactosActivosPorSexo'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'contactosActivosPorSexo'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    output$rastreoContactsActiveSexo = renderPlotly({
        fig = plot_ly(rastreoContactsActive_sexo(),
                      labels = ~Sexo,
                      values = ~N,
                      text = ~paste0(round((N / sum(N))*100, 0),"%"),
                      textinfo='text',
                      #hovertemplate = "<b> %{sexo} :</b> %{n} <extra></extra>",
                      type = 'pie',
                      hole = 0.6,
                      sort = FALSE,
                      marker = list(colors=colorPlotly))
        fig = fig %>% 
            layout(title = 'Contactos activos de casos COVID-19<br>por sexo',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
            plotly::config(displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })

    #### DATA FRAME REACTIVE DE CASOS RASTREADOS POR CRUPO ETARIO C/10 ANOS
    rastreoContactsAgeGroup_reactive = reactive({
        df = rastreoContacts_reactive() %>%
            group_by(grupo_etario,Sexo) %>%
            tally() %>%
            rename(`Grupo Etario` = grupo_etario, N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%"))
    })
    
    output$rastreoContactsEdadDB = renderDataTable(
        datatable(
            rastreoContactsAgeGroup_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'contactosPorGrupoEtario'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'contactosPorGrupoEtario'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    #### Plot casos rastreados por edad y sexo
    output$rastreoContactsEdad = renderPlotly({
        p = rastreoContactsAgeGroup_reactive() %>%
            ggplot(aes(x = `Grupo Etario`, y = N, fill = Sexo)) +
            geom_bar(stat = "identity", position = "stack") + 
            ggtitle("Contactos de caso COVID-19<br>por grupo etario") +
            scale_fill_manual(values = colorVector)+
            ylab("No. de Contactos")
        
        ggplotly(p + theme(plot.title = element_text(hjust=0.5), 
                           legend.position="bottom",
                           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           legend.title = element_blank(),
                           panel.background = element_blank()), 
                 tooltip = c("N")) %>% 
            #layout(legend = list(orientation = "h",  y = -0.3)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })
    
    #### DATA FRAME REACTIVE DE CASOS RASTREADOS ACTIVOS POR CRUPO ETARIO C/10 ANOS
    rastreoContactsActiveAgeGroup_reactive = reactive({
        df = rastreoContactsActive_reactive() %>%
            group_by(grupo_etario,Sexo) %>%
            tally() %>%
            rename(`Grupo Etario` = grupo_etario, N = n) %>%
            mutate(Porcentaje = paste0(round(N/sum(N)*100,0), "%"))
    })
    
    output$rastreoContactsActiveEdadDB = renderDataTable(
        datatable(
            rastreoContactsActiveAgeGroup_reactive() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'contactosActivosPorGrupoEtario'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'contactosActivosPorGrupoEtario'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    #### Plot en cuarentena por edad y sexo
    output$rastreoContactsActiveEdad = renderPlotly({
        p = rastreoContactsActiveAgeGroup_reactive() %>%
            ggplot(aes(x = `Grupo Etario`, y = N, fill = Sexo)) +
            geom_bar(stat = "identity", position = "stack") + 
            ggtitle("Contactos activos de caso COVID-19<br>por grupo etario") +
            scale_fill_manual(values = colorVector)+
            ylab("No. de Contactos")
        
        ggplotly(p + theme(plot.title = element_text(hjust=0.5), 
                           legend.position="bottom",
                           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                           panel.grid.major = element_blank(), 
                           legend.title = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank()), 
                 tooltip = c("N")) %>% 
            #layout(legend = list(orientation = "h",  y = -0.3)) %>%
            plotly::config(locale = "es", displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })
    
    #### DB de nivel de riesgo
    enRiesgo = reactive({
        enRiesgo = rastreoContacts_reactive() %>%
            filter(Telefono == 'CONTACTABLE') %>%
            select(riesgo) %>%
            mutate(riesgo = ifelse('Contacto cercano (menos de 1.5 mts)' == riesgo, 'Contacto cercano', riesgo), 
                   riesgo = ifelse('Contacto directo (permanece en el mismo entorno cercano)' == riesgo, 'Contacto directo', riesgo)) %>%
            group_by(riesgo) %>%
            tally() %>%
            mutate(riesgo = fct_reorder(riesgo,n)) %>%
            rename(`Nivel de Riesgo` = riesgo,
                   N = n)
    })
    
    output$rastreoContactsRiesgoDB = renderDataTable(
        datatable(
            enRiesgo() %>%
                adorn_totals('row'),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'contactosNivelDeRiesgo'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'contactosNivelDeRiesgo'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    #### Plot de nivel de riesgo
    output$rastreoContactsRiesgo = renderPlotly({
        enRiesgo = enRiesgo()

        fig = plot_ly(enRiesgo,
                      x = ~`Nivel de Riesgo`,
                      y = ~N,
                      hovertemplate = "<b>%{x}:</b> %{y} <extra></extra>",
                      type = 'bar',
                      color = ~`Nivel de Riesgo`)
        fig = fig %>% 
            layout(title = list(text = paste0('Nivel de Riesgo en Contactos Contactables',
                                    '<br>',
                                    '<sup>',
                                    '(N= ',
                                    nrow(rastreoContacts_reactive() %>% filter(Telefono == 'CONTACTABLE')),
                                    ')',
                                    '</sup>')),
                   xaxis = list(title="Nivel de Riesgo",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                   yaxis = list(title="No. de Contactos",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                   showlegend=FALSE) %>%
            plotly::config(displayModeBar=T, modeBarButtons = list(list("resetScale2d", "toImage", 'hoverCompareCartesian', 'hoverClosestCartesian')), displaylogo = F)
    })

    #########################################################################
    #########################       REPORTES            #####################
    #########################################################################
    ### ACUMULADO
    
    
    
    output$reporteAcumulado = renderDataTable({
        fecha = as.character(format(input$fechaReporte[2],'%d/%m/%Y'))
        
        casosAcumulados = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E') %>%
            nrow()

        casosAcumuladosContactables = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                `Telefono` == "CONTACTABLE" 
                ) %>%
            nrow()
        
        confirmadosEnCentroRespiratorio = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                `Resultado de la muestra.` == 1) %>%
            nrow()

        confirmadosAutoreporte = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                `¿Se tomo una muestra respiratoria?` == 3
                ) %>%
            nrow()

        confirmadosParaRastreo = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                `¿Se tomo una muestra respiratoria?` == 3 | `Resultado de la muestra.` == 1
                ) %>%
            nrow()

         

        confirmadosParaRastreoTexto = paste(as.character(confirmadosParaRastreo),' (',as.character(round((confirmadosParaRastreo / casosAcumulados) * 100, 1)),'%)', sep='')
        
        tamizadosEnCentroRespiratorio = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                `¿Se tomo una muestra respiratoria?` == 1) %>%
            nrow()
        
        positividad = paste(as.character(round((confirmadosEnCentroRespiratorio/tamizadosEnCentroRespiratorio)*100,1)),'%',sep='')
        
        casosPorNexoEpidemiologico = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                (`Fue Un Contacto` == TRUE) | `Clasificacion` == "CONFIRMADO POR NEXO EPIDEMIOLÓGICO") %>%
            nrow()
        
        casosElegiblesRastreoContactos = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E', 
                `¿Se tomo una muestra respiratoria?` == 3 |
                `Resultado de la muestra.` == 1,
                `Telefono` == "CONTACTABLE",
                `Estado de seguimiento` != "Imposible de contactar") %>%
            nrow()

        casosNoElegiblesRastreoContactos = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                `¿Se tomo una muestra respiratoria?` == 3 |
                `Resultado de la muestra.` == 1,
                `Telefono` != "CONTACTABLE" |
                `Estado de seguimiento` == "Imposible de contactar") %>%
            nrow()
        
        porcentajeCasosElegibleRastreoContactos = paste(as.character(round((casosElegiblesRastreoContactos/casosAcumulados)*100,2)),'%',sep = '')
        
        casosLlamadosParaContactos = rastreoCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                `Resultado de la muestra.` == 1,
                `Telefono` == "CONTACTABLE",
                `Estado de seguimiento` != "Imposible de contactar") %>%
            select(starts_with('Seguimiento')) %>%
            mutate(min = pmin(.)) %>%
            filter(min == 1)%>%
            nrow()
        
        casosQueReportaronContactos = length(unique(rastreoContacts_reactive()$`Caso relacionado`))
        
        contactosReportados = nrow(rastreoContacts_reactive())
        
        contactosPorCasoElegible = round(contactosReportados/casosQueReportaronContactos,1)
        
        contactosPorCasos = round(contactosReportados/confirmadosParaRastreo,1)
        
        reporteAcumulado = matrix(c(fecha, casosAcumulados, casosAcumuladosContactables, confirmadosEnCentroRespiratorio,tamizadosEnCentroRespiratorio,
                                    positividad, confirmadosAutoreporte, casosPorNexoEpidemiologico, confirmadosParaRastreoTexto, casosElegiblesRastreoContactos,
                                    casosNoElegiblesRastreoContactos, casosQueReportaronContactos, contactosReportados, contactosPorCasoElegible,
                                    contactosPorCasos),ncol = 1,byrow = TRUE)
        rownames(reporteAcumulado) = c('Acumulado al', 'Total notificaciones (casos sospechosos y confirmados)', 'Notificaciones contactables por llamada', 'Casos confirmados en centros de salud o CBR',
                                       'Tamizados en centros de salud o CBR', 'Positividad en centros de salud o CBR', 'Casos que reportan prueba positiva de otros laboratorios', 'Casos confirmados por nexo epidemiológico detectado por seguimiento',
                                       'Total casos confirmados para rastreo de contactos (porcentaje de las notificaciones)' ,'Casos confirmados elegibles para rastreo de contactos vía telefónica',
                                       'Casos confirmados imposibles de contactar después de diagnóstico por vía telefónica' , 'Casos que reportan contactos',
                                       'Contactos reportados','Número de contactos por caso que ha reportado contactos','Número de contactos por casos confirmados para rastreo de contactos')
        datatable(reporteAcumulado, options = list( pageLength =15), colnames = rep('',ncol(reporteAcumulado)))
    })

    ######### CASOS

    output$reporteCases = renderDataTable({
        fecha = as.character(format(input$fechaReporte[2],'%d/%m/%Y'))

        casosNuevos = reportCases_reactive() %>%
            req(input$fechaReporte[2]) %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                case_when(format(as.Date(input$fechaReporte[2]),'%a') == 'Mon' ~ `Creado En` == as.Date(input$fechaReporte[2]) | `Creado En` == as.Date(input$fechaReporte[2])-1,
                        T ~ `Creado En` == as.Date(input$fechaReporte[2]))
            )

        casosNuevosContactables = casosNuevos %>%
        filter(
            Telefono == 'CONTACTABLE',
            `Estado de seguimiento` != "Imposible de contactar"
        )
        
        casosNuevosConfirmados = casosNuevos %>%
            filter(
                Clasificacion == "CONFIRMADO" | `¿Se tomo una muestra respiratoria?` == 3
            ) %>%
            nrow()

        porcentajeCasosNuevosContactables = paste(as.character(round((nrow(casosNuevosContactables)/nrow(casosNuevos))*100,1)),"%", sep='')

        casosConSeguimientoRealizado = reportCases_reactive() %>%
            filter(
                maxDate == Sys.Date()
            ) %>%
            nrow()
        
        casosSinEstado = reportCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                Telefono == 'CONTACTABLE',
                `Estado de seguimiento` == 'Sin estado de seguimiento',
            )
        
        casosActivos = reportCases_reactive() %>%
          filter(
            Clasificacion != 'NO ES UN CASO (DESCARTADO)',
            Clasificacion != 'SOSPECHOSO E',
            `Estado de seguimiento` == 'Activo'
          )

        casosSeguimientoRealizado = reportCases_reactive() %>%
            filter(
                Clasificacion != 'NO ES UN CASO (DESCARTADO)',
                Clasificacion != 'SOSPECHOSO E',
                maxDate == Sys.Date()
            )

        casosDelDia = casosNuevosContactables %>%
            bind_rows(casosSinEstado) %>%
            bind_rows(casosActivos) %>%
            bind_rows(casosSeguimientoRealizado) %>%
            distinct()

        casosNoRespuesta = casosDelDia %>%
            filter(
                maxDate == Sys.Date(),
                ultimoSeguimiento == 2,
                ultimoPorque == 3 |
                ultimoPorque == 1
            ) %>%
            nrow()

        casosNoContactable = casosDelDia %>%
            filter(
                maxDate == Sys.Date(),
                ultimoSeguimiento == 2,
                ultimoPorque == 6
            ) %>%
            nrow()
        
        casosRechazoSeguimiento = casosDelDia %>%
            filter(
                maxDate == Sys.Date(),
                ultimoSeguimiento == 2,
                ultimoPorque == 2
            ) %>%
            nrow()

        casosNoOtraRazon = casosDelDia %>%
            filter(
                maxDate == Sys.Date(),
                ultimoSeguimiento == 2,
                ultimoPorque == 4 |
                ultimoPorque == 7
            ) %>%
            nrow()
        

        casosConSeguimientoLogrado = casosDelDia %>%
            filter(
                maxDate == Sys.Date(),
                ultimoSeguimiento == 1
            ) %>%
            nrow()

        porcentajeCasosConSeguimiento = paste(as.character(round((casosConSeguimientoLogrado/nrow(casosDelDia))*100,0)),'%',sep='')

        casosNoDioTiempo = nrow(casosDelDia) - casosConSeguimientoLogrado - casosNoOtraRazon - casosNoContactable - casosNoRespuesta - casosRechazoSeguimiento

        reporteCases = matrix(c(fecha, nrow(casosNuevos), nrow(casosNuevosContactables), porcentajeCasosNuevosContactables, casosNuevosConfirmados,
                                    nrow(casosDelDia), casosConSeguimientoRealizado, casosNoRespuesta, casosNoContactable, casosRechazoSeguimiento, casosNoOtraRazon, 
                                    casosConSeguimientoLogrado, porcentajeCasosConSeguimiento, casosNoDioTiempo),ncol = 1,byrow = TRUE)

        rownames(reporteCases) = c('Fecha', 'Total de notificaciones nuevas' ,'Notificaciones nuevas contactables por llamada', 'Porcentaje de notificaciones nuevass contactables (%)', 'Casos confirmados nuevos', 'Total casos a contactar en el día',
                                    'Casos con seguimiento intentado', 'Casos con seguimiento no realizado, no contestó', 'Casos con seguimiento no realizado, número incorrecto',
                                    'Casos con seguimiento no realizado, rechazó llamada', 'Casos con seguimiento no realizado, no se pudo contactar por otra razón', 'Casos con seguimiento logrado',
                                    'Porcentaje casos con seguimiento (%)', 'Casos que no dio tiempo llamar o seguimiento no registrado')

         datatable(reporteCases, options = list( pageLength =14), colnames = rep('',ncol(reporteCases)))

        
    })
    
    ######### CONTACTOS

    output$reporteContacts = renderDataTable({
        fecha = as.character(format(input$fechaReporte[2],'%d/%m/%Y'))

        casosElegiblesContactos = rastreoCases_reactive() %>%
            filter(
                case_when(format(as.Date(input$fechaReporte[2] , "%d/%m/%Y"), '%a') == "Mon" ~ `Creado En` == input$fechaReporte[2] | `Creado En` == input$fechaReporte[2]-1,
                          T ~ `Creado En` == input$fechaReporte[2]),
                `Clasificacion` == "CONFIRMADO" |
                `¿Se tomo una muestra respiratoria?` == 3
            )

        casosElegiblesContactosContactables = casosElegiblesContactos %>%
            filter(
                Telefono == "CONTACTABLE",
                `Estado de seguimiento` != "Imposible de contactar"
                ) 

        contactosNuevos = rastreoContacts_reactive() %>%
            filter(`Creado En` == input$fechaReporte[2])

        casosReportaronContactos = length(unique(contactosNuevos$`Caso relacionado`))

        todayFollowUp = rastreo_followups %>%
            mutate(`Creado En` = as.Date(`Creado En`)) %>%
            filter(`Creado En` == input$fechaReporte[2])

        contactosPorLlamar = todayFollowUp %>%
            filter(Estado != "Seguimiento/llamada  no programada") %>%
            nrow()

        contactosCuarentenaPorLlamar = contactosPorLlamar - nrow(contactosNuevos)

        contactosNoRespuesta = todayFollowUp %>%
            filter(Estado == "No Realizado") %>%
            nrow()

        contactosSeguimientoLogrado = todayFollowUp %>%
            filter(Estado == "Visto, esta bien (sin síntomas)" |
                    Estado == "Visto, no bien (con síntomas)") %>%
            nrow()

        porcentajeContactosSeguimiento = paste(as.character(round((contactosSeguimientoLogrado/contactosPorLlamar)*100,0)),'%',sep='')

        contactosNoDioTiempo = contactosPorLlamar - contactosSeguimientoLogrado - contactosNoRespuesta

        #contactosNoDioTiempo = todayFollowUp %>%
        #    filter(Estado == "No Realizado") %>%
        #    nrow()
        
        reporteContacts = matrix(c(fecha, nrow(casosElegiblesContactos), nrow(casosElegiblesContactosContactables), casosReportaronContactos,
                                    nrow(contactosNuevos), contactosCuarentenaPorLlamar, contactosPorLlamar, contactosNoRespuesta,
                                    contactosSeguimientoLogrado, porcentajeContactosSeguimiento, contactosNoDioTiempo),ncol = 1,byrow = TRUE)

        for (i in c(1:4)) {
            fecha = as.character(format(input$fechaReporte[2]-i,'%d/%m/%Y'))

            casosElegiblesContactos = rastreoCases_reactive() %>%
                filter(
                    case_when(format(as.Date(input$fechaReporte[2]-i, "%d/%m/%Y"), '%a') == "Mon" ~ `Creado En` == input$fechaReporte[2]-i | `Creado En` == input$fechaReporte[2]-i-1,
                              T ~ `Creado En` == input$fechaReporte[2]-i),
                    `Clasificacion` == "CONFIRMADO" |
                    `¿Se tomo una muestra respiratoria?` == 3
                )

            casosElegiblesContactosContactables = casosElegiblesContactos %>%
                filter(
                    Telefono == "CONTACTABLE",
                    `Estado de seguimiento` != "Imposible de contactar"
                ) 

            contactosNuevos = rastreoContacts_reactive() %>%
                filter(`Creado En` == input$fechaReporte[2]-i)

            casosReportaronContactos = length(unique(contactosNuevos$`Caso relacionado`))

            todayFollowUp = rastreo_followups %>%
                mutate(`Creado En` = as.Date(`Creado En`)) %>%
                filter(`Creado En` == input$fechaReporte[2]-i)

            contactosPorLlamar = todayFollowUp %>%
                filter(Estado != "Seguimiento/llamada  no programada") %>%
                nrow()

            contactosCuarentenaPorLlamar = contactosPorLlamar - nrow(contactosNuevos)

            contactosNoRespuesta = todayFollowUp %>%
                filter(Estado == "No Realizado") %>%
                nrow()

            contactosSeguimientoLogrado = todayFollowUp %>%
                filter(Estado == "Visto, esta bien (sin síntomas)" |
                        Estado == "Visto, no bien (con síntomas)") %>%
                nrow()

            porcentajeContactosSeguimiento = paste(as.character(round((contactosSeguimientoLogrado/contactosPorLlamar)*100,0)),'%',sep='')

            contactosNoDioTiempo = contactosPorLlamar - contactosSeguimientoLogrado - contactosNoRespuesta

            #contactosNoDioTiempo = todayFollowUp %>%
            #    filter(Estado == "No Realizado") %>%
            #    nrow()
            
            reporteContactsTemporal = matrix(c(fecha, nrow(casosElegiblesContactos), nrow(casosElegiblesContactosContactables), casosReportaronContactos,
                                        nrow(contactosNuevos), contactosCuarentenaPorLlamar, contactosPorLlamar, contactosNoRespuesta,
                                        contactosSeguimientoLogrado, porcentajeContactosSeguimiento, contactosNoDioTiempo),ncol = 1,byrow = TRUE)
            
            reporteContacts = cbind(reporteContactsTemporal ,reporteContacts)


        }

        rownames(reporteContacts) = c('Fecha', 'Casos a quienes investigar contactos (confirmados)', 'Casos contactables para listado de contactos vía llamada', 
                                'Casos que reportaron contactos', 'Contactos nuevos', 'Contactos en cuarentena a llamar',
                                'Total de contactos a llamar en el dia', 'Contactos llamados con seguimiento no logrado', 'Contactos con seguimiento logrado',
                                'Porcentaje de contactos con seguimiento (%)', 'Contactos que no dio tiempo llamar o seguimiento no registrado')

        
        datatable(reporteContacts, options = list( pageLength =11), colnames = rep('',ncol(reporteContacts)))


         

    })
    
    ######## HERRAMIENTAS RASTREADORES #######
    herramientaData = reactive({
        req(input$file1)
        req(input$workersNumber)
        tryCatch(
            {
                
                raw_casos <- as.data.frame(read_csv(input$file1$datapath, guess_max = 5000))
                raw_casos <- raw_casos %>% mutate(
                    Clasificación = toupper(Clasificación),
                    FE12102fecha_y_hora_de_toma_de_la_muestra = format(as.Date(raw_casos$FE12102fecha_y_hora_de_toma_de_la_muestra), '%d-%m-%Y'),
                    FE13001fecha_de_hospitalizacion = format(as.Date(raw_casos$FE13001fecha_de_hospitalizacion), '%d-%m-%Y'),
                )
                
                
                #asignacion numero de trabajadores
                divided_work <- input$workersNumber
                
                final <-  raw_casos %>%
                    mutate(
                        Clasificación = toupper(Clasificación),
                    ) %>%
                    filter(
                        Clasificación != 'NO ES UN CASO (DESCARTADO)',
                        Clasificación != 'SOSPECHOSO E'
                    ) %>%
                    select(
                        Rastreador = FE107correo_electronico_del_trabajador_que_notifica ,
                        `Clasificación Epi` = Clasificación ,
                        Nombre = `Primer Nombre`,
                        Apellido = Apellido,
                        Direccion = `Direcciones Dirección Línea 1 [1]`  ,
                        Direccion2 = `Direcciones Comunidad, aldea o zona [1]` ,
                        Tel = `Direcciones Número De Teléfono [1]` ,
                        Sexo = Sexo,
                        Ocupación = Ocupación,
                        Edad = `Edad Años De Edad`,
                        E_A1 = `FE113enfermedades_asociadas 1`,
                        E_A2 = `FE113enfermedades_asociadas 2`,
                        E_A3 = `FE113enfermedades_asociadas 3`,
                        E_A4 = `FE113enfermedades_asociadas 4`,
                        E_A5 = `FE113enfermedades_asociadas 5`,
                        E_A6 = `FE113enfermedades_asociadas 6`,
                        E_A7 = `FE113enfermedades_asociadas 7`,
                        E_A8 = `FE113enfermedades_asociadas 8`,
                        E_A9 = `FE113enfermedades_asociadas 9`,
                        E_A10 = `FE113enfermedades_asociadas 10`,
                        E_A11 = `FE113enfermedades_asociadas 11`,
                        E_A12 = `FE113enfermedades_asociadas 12`,
                        E_Aotras =FE11301especifique,
                        `Embarazo` = `Estado De Embarazo`,
                        Unidad_Notificadora = `Direcciones Ubicación [1]`,
                        `Fecha Inicio Sintomas` = `Fecha de inicio de síntomas`,
                        `Fecha primera visita CBR` = `Fecha de notificación`,
                        `Se tomó muestra` = FE121se_tomo_una_muestra_respiratoria,
                        `Fecha muestra` = FE12102fecha_y_hora_de_toma_de_la_muestra,
                        `Resultado muestra` = FE12103resultado_de_la_muestra,
                        `Dias entre inicio sintomas y visita CBR` = 1,
                        `Fecha ingreso Go.Data` = `Creado En`,
                        `Fecha hospitalizacion` = `FE13001fecha_de_hospitalizacion`,
                        fecha_s1_s,
                        fecha_s1_n,
                        fecha_s2_s,
                        fecha_s2_n,
                        fecha_s3_s,
                        fecha_s3_n,
                        fecha_s4_s,
                        fecha_s4_n,
                        fecha_s5_s,
                        fecha_s5_n,
                        fecha_s6_s,
                        fecha_s6_n,
                        fecha_s7_s,
                        fecha_s7_n,
                        fecha_s8_s,
                        fecha_s8_n,
                        fecha_s9_s,
                        fecha_s9_n,
                        fecha_s10_s,
                        fecha_s10_n,
                        fecha_s11_s,
                        fecha_s11_n,
                        fecha_s12_s,
                        fecha_s12_n,
                        fecha_s13_s,
                        fecha_s13_n,
                        fecha_s14_s,
                        fecha_s14_n,
                        `Estado de seguimiento` = estado_de_seguimiento_1,
                        hay_sintomas_1 = presenta_sintomas,
                        hay_sintomas_2 = ha_presentado_sintomas_s2,
                        hay_sintomas_3 = presenta_sintomas_s3,
                        hay_sintomas_4 = presenta_sintomas_s4,
                        hay_sintomas_5 = presenta_sintomas_s5,
                        hay_sintomas_6 = presenta_sintomas_s6,
                        hay_sintomas_7 = presenta_sintomas_s7,
                        hay_sintomas_8 = presenta_sintomas_s8,
                        hay_sintomas_9 = presenta_sintomas_s9,
                        hay_sintomas_10 = presenta_sintomas_s10,
                        hay_sintomas_11 = presenta_sintomas_s11,
                        hay_sintomas_12 = presenta_sintomas_s12,
                        hay_sintomas_13 = presenta_sintomas_s13,
                        hay_sintomas_14 = presenta_sintomas_s14,
                        por_que_s1 = por_que_1,
                        por_que_s2 = por_que_s2,
                        por_que_s3 = por_que_s3,
                        por_que_s4 = por_que_s4,
                        por_que_s5 = por_que_s5,
                        por_que_s6 = por_que_s6,
                        por_que_s7 = por_que_7,
                        por_que_s8 = por_que_s8,
                        por_que_s9 = por_que_9,
                        por_que_s10 = por_que_s10,
                        por_que_s11 = por_que_11,
                        por_que_s12 = por_que_s12,
                        por_que_s13 = por_que_s13,
                        por_que_s14 = por_que_s14,
                        fecha_es,
                    ) %>%
                    unite(
                        'comorbilidades', c(contains('E_A')),sep = ', ',na.rm = T
                    ) %>%
                    mutate(
                        `Fecha Inicio Sintomas` = as.Date(`Fecha Inicio Sintomas`, "%d-%m-%Y"),
                        Direccion  = paste( Direccion, 'zona' , Direccion2 ),
                        Direccion2 = NULL,
                        comorbilidades = gsub('14', 'Otro', comorbilidades),
                        comorbilidades = gsub('11', 'Obesidad', comorbilidades),
                        comorbilidades = gsub('10', 'Disfuón Neuromuscular', comorbilidades),
                        comorbilidades = gsub('9', 'Cardiopatía Crónica (hipertensión arterial)', comorbilidades),
                        comorbilidades = gsub('8', 'Enfermedad Hepática Crónica', comorbilidades),
                        comorbilidades = gsub('7', 'Tratamiento Con Corticosteroides', comorbilidades),
                        comorbilidades = gsub('6', 'Inmunosupresión', comorbilidades),
                        comorbilidades = gsub('5', 'Asma', comorbilidades),
                        comorbilidades = gsub('4', 'Cáncer', comorbilidades),
                        comorbilidades = gsub('3', 'Insuficiencia Renal Crónica', comorbilidades),
                        comorbilidades = gsub('2', 'Enfermedad Pulmonar Obstructiva Crónica', comorbilidades),
                        comorbilidades = gsub('1', 'Diabetes Mellitus', comorbilidades),
                        `Embarazo` = substr(`Embarazo`, start = 1, stop = 2),
                        `Fecha primera visita CBR` = as.Date(`Fecha primera visita CBR`),
                        `Se tomó muestra` = case_when(
                            as.character(`Se tomó muestra`) == "1" ~ 'Si',
                            as.character(`Se tomó muestra`) == "2" ~ 'No',
                            as.character(`Se tomó muestra`) == "3" ~ 'Autoreporte'),
                        `Resultado muestra` = case_when(
                            `Resultado muestra` == 1 ~ 'Positiva',
                            `Resultado muestra` == 2 ~ 'Negativa',
                            `Resultado muestra` == 3 ~ 'Autoreporte'),
                        `Dias entre inicio sintomas y visita CBR` = as.Date(`Fecha primera visita CBR`, '%d-%m-%Y') - as.Date(`Fecha Inicio Sintomas`, '%d-%m-%Y'),
                        `Fecha ingreso Go.Data` = as.Date(`Fecha ingreso Go.Data`),
                        fecha_s1_s = as.Date(fecha_s1_s),
                        fecha_s1_n = as.Date(fecha_s1_n),
                        fecha_s2_s = as.Date(fecha_s2_s),
                        fecha_s2_n = as.Date(fecha_s2_n),
                        fecha_s3_s = as.Date(fecha_s3_s),
                        fecha_s3_n = as.Date(fecha_s3_n),
                        fecha_s4_s = as.Date(fecha_s4_s),
                        fecha_s4_n = as.Date(fecha_s4_n),
                        fecha_s5_s = as.Date(fecha_s5_s),
                        fecha_s5_n = as.Date(fecha_s5_n),
                        fecha_s6_s = as.Date(fecha_s6_s),
                        fecha_s6_n = as.Date(fecha_s6_n),
                        fecha_s7_s = as.Date(fecha_s7_s),
                        fecha_s7_n = as.Date(fecha_s7_n),
                        fecha_s8_s = as.Date(fecha_s8_s),
                        fecha_s8_n = as.Date(fecha_s8_n),
                        fecha_s9_s = as.Date(fecha_s9_s),
                        fecha_s9_n = as.Date(fecha_s9_n),
                        fecha_s10_s = as.Date(fecha_s10_s),
                        fecha_s10_n = as.Date(fecha_s10_n),
                        fecha_s11_s = as.Date(fecha_s11_s),
                        fecha_s11_n = as.Date(fecha_s11_n),
                        fecha_s12_s = as.Date(fecha_s12_s),
                        fecha_s12_n = as.Date(fecha_s12_n),
                        fecha_s13_s = as.Date(fecha_s13_s),
                        fecha_s13_n = as.Date(fecha_s13_n),
                        fecha_s14_s = as.Date(fecha_s14_s),
                        fecha_s14_n = as.Date(fecha_s14_n),
                        fecha_es = as.Date(fecha_es),
                        
                        fecha_seguimiento_1 = case_when(is.na(fecha_s1_s) ~ as.Date(fecha_s1_n), T ~ as.Date(fecha_s1_s)),
                        fecha_seguimiento_2 = case_when(is.na(fecha_s2_s) ~ as.Date(fecha_s2_n), T ~ as.Date(fecha_s2_s)),
                        fecha_seguimiento_3 = case_when(is.na(fecha_s3_s) ~ as.Date(fecha_s3_n), T ~ as.Date(fecha_s3_s)),
                        fecha_seguimiento_4 = case_when(is.na(fecha_s4_s) ~ as.Date(fecha_s4_n), T ~ as.Date(fecha_s4_s)),
                        fecha_seguimiento_5 = case_when(is.na(fecha_s5_s) ~ as.Date(fecha_s5_n), T ~ as.Date(fecha_s5_s)),
                        fecha_seguimiento_6 = case_when(is.na(fecha_s6_s) ~ as.Date(fecha_s6_n), T ~ as.Date(fecha_s6_s)),
                        fecha_seguimiento_7 = case_when(is.na(fecha_s7_s) ~ as.Date(fecha_s7_n), T ~ as.Date(fecha_s7_s)),
                        fecha_seguimiento_8 = case_when(is.na(fecha_s8_s) ~ as.Date(fecha_s8_n), T ~ as.Date(fecha_s8_s)),
                        fecha_seguimiento_9 = case_when(is.na(fecha_s9_s) ~ as.Date(fecha_s9_n), T ~ as.Date(fecha_s9_s)),
                        fecha_seguimiento_10 = case_when(is.na(fecha_s10_s) ~ as.Date(fecha_s10_n), T ~ as.Date(fecha_s10_s)),
                        fecha_seguimiento_11 = case_when(is.na(fecha_s11_s) ~ as.Date(fecha_s11_n), T ~ as.Date(fecha_s11_s)),
                        fecha_seguimiento_12 = case_when(is.na(fecha_s12_s) ~ as.Date(fecha_s12_n), T ~ as.Date(fecha_s12_s)),
                        fecha_seguimiento_13 = case_when(is.na(fecha_s13_s) ~ as.Date(fecha_s13_n), T ~ as.Date(fecha_s13_s)),
                        fecha_seguimiento_14 = case_when(is.na(fecha_s14_s) ~ as.Date(fecha_s14_n), T ~ as.Date(fecha_s14_s)),
                        `Estado de seguimiento` = case_when(`Estado de seguimiento` == 1 ~ "Activo",
                                                            `Estado de seguimiento` == 2 ~ "Recuperado",
                                                            `Estado de seguimiento` == 3 ~ "Imposible de contactar",
                                                            `Estado de seguimiento` == 4 ~ "Perdido",
                                                            `Estado de seguimiento` == 5 ~ "Hospitalizados/Fallecidos",
                                                            `Estado de seguimiento` == 6 ~ "Concluído por otra razón",
                                                            is.na(`Estado de seguimiento`) ~ "Sin estado de seguimiento"),
                        `seguimiento_1_resultado` = case_when(
                            hay_sintomas_1 == 1 ~ 'Sintomático',
                            hay_sintomas_1 == 2 ~ 'Asintomático',
                            por_que_s1 == 1  ~ "No respondió",
                            por_que_s1 == 2 ~ "Rechazó",
                            por_que_s1 == 3 ~ "No entró llamada",
                            por_que_s1 == 4 ~ "Conexión perdida",
                            por_que_s1 == 5 ~ "No intentada",
                            por_que_s1 == 6 ~ "Num incorrecto",
                            por_que_s1 == 7 ~ "Otro"
                        ),
                        `seguimiento_2_resultado` = case_when(
                            hay_sintomas_2 == '1' ~ 'Sintomatico',
                            hay_sintomas_2 == '2' ~ 'Asintomatico',
                            por_que_s2 == '1' ~ "No respondió",
                            por_que_s2 == '2' ~ "Rechazó",
                            por_que_s2 == '3' ~ "No entró llamada",
                            por_que_s2 == '4' ~ "Conexión perdida",
                            por_que_s2 == '5' ~ "No intentada",
                            por_que_s2 == '6' ~ "Num incorrecto",
                            por_que_s2 == '7' ~ "Otro"
                        ),
                        `seguimiento_3_resultado` = case_when(
                            hay_sintomas_3 == '1' ~ 'Sintomatico',
                            hay_sintomas_3 == '2' ~ 'Asintomatico',
                            por_que_s3 == '1' ~ "No respondió",
                            por_que_s3 == '2' ~ "Rechazó",
                            por_que_s3 == '3' ~ "No entró llamada",
                            por_que_s3 == '4' ~ "Conexión perdida",
                            por_que_s3 == '5' ~ "No intentada",
                            por_que_s3 == '6' ~ "Num incorrecto",
                            por_que_s3 == '7' ~ "Otro"
                        ),
                        `seguimiento_4_resultado` = case_when(
                            hay_sintomas_4 == '1' ~ 'Sintomatico',
                            hay_sintomas_4 == '2' ~ 'Asintomatico',
                            por_que_s4 == '1' ~ "No respondió",
                            por_que_s4 == '2' ~ "Rechazó",
                            por_que_s4 == '3' ~ "No entró llamada",
                            por_que_s4 == '4' ~ "Conexión perdida",
                            por_que_s4 == '5' ~ "No intentada",
                            por_que_s4 == '6' ~ "Num incorrecto",
                            por_que_s4 == '7' ~ "Otro"
                        ),
                        `seguimiento_5_resultado` = case_when(
                            hay_sintomas_5 == '1' ~ 'Sintomatico',
                            hay_sintomas_5 == '2' ~ 'Asintomatico',
                            por_que_s5 == '1' ~ "No respondió",
                            por_que_s5 == '2' ~ "Rechazó",
                            por_que_s5 == '3' ~ "No entró llamada",
                            por_que_s5 == '4' ~ "Conexión perdida",
                            por_que_s5 == '5' ~ "No intentada",
                            por_que_s5 == '6' ~ "Num incorrecto",
                            por_que_s5 == '7' ~ "Otro"
                        ),
                        `seguimiento_6_resultado` = case_when(
                            hay_sintomas_6 == '1' ~ 'Sintomatico',
                            hay_sintomas_6 == '2' ~ 'Asintomatico',
                            por_que_s6 == '1' ~ "No respondió",
                            por_que_s6 == '2' ~ "Rechazó",
                            por_que_s6 == '3' ~ "No entró llamada",
                            por_que_s6 == '4' ~ "Conexión perdida",
                            por_que_s6 == '5' ~ "No intentada",
                            por_que_s6 == '6' ~ "Num incorrecto",
                            por_que_s6 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_7_resultado` = case_when(
                            hay_sintomas_7 == '1' ~ 'Sintomatico',
                            hay_sintomas_7 == '2' ~ 'Asintomatico',
                            por_que_s7 == '1' ~ "No respondió",
                            por_que_s7 == '2' ~ "Rechazó",
                            por_que_s7 == '3' ~ "No entró llamada",
                            por_que_s7 == '4' ~ "Conexión perdida",
                            por_que_s7 == '5' ~ "No intentada",
                            por_que_s7 == '6' ~ "Num incorrecto",
                            por_que_s7 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_8_resultado` = case_when(
                            hay_sintomas_8 == '1' ~ 'Sintomatico',
                            hay_sintomas_8 == '2' ~ 'Asintomatico',
                            por_que_s8 == '1' ~ "No respondió",
                            por_que_s8 == '2' ~ "Rechazó",
                            por_que_s8 == '3' ~ "No entró llamada",
                            por_que_s8 == '4' ~ "Conexión perdida",
                            por_que_s8 == '5' ~ "No intentada",
                            por_que_s8 == '6' ~ "Num incorrecto",
                            por_que_s8 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_9_resultado` = case_when(
                            hay_sintomas_9 == '1' ~ 'Sintomatico',
                            hay_sintomas_9 == '2' ~ 'Asintomatico',
                            por_que_s9 == '1' ~ "No respondió",
                            por_que_s9 == '2' ~ "Rechazó",
                            por_que_s9 == '3' ~ "No entró llamada",
                            por_que_s9 == '4' ~ "Conexión perdida",
                            por_que_s9 == '5' ~ "No intentada",
                            por_que_s9 == '6' ~ "Num incorrecto",
                            por_que_s9 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_10_resultado` = case_when(
                            hay_sintomas_10 == '1' ~ 'Sintomatico',
                            hay_sintomas_10 == '2' ~ 'Asintomatico',
                            por_que_s10 == '1' ~ "No respondió",
                            por_que_s10 == '2' ~ "Rechazó",
                            por_que_s10 == '3' ~ "No entró llamada",
                            por_que_s10 == '4' ~ "Conexión perdida",
                            por_que_s10 == '5' ~ "No intentada",
                            por_que_s10 == '6' ~ "Num incorrecto",
                            por_que_s10 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_11_resultado` = case_when(
                            hay_sintomas_11 == '1' ~ 'Sintomatico',
                            hay_sintomas_11 == '2' ~ 'Asintomatico',
                            por_que_s11 == '1' ~ "No respondió",
                            por_que_s11 == '2' ~ "Rechazó",
                            por_que_s11 == '3' ~ "No entró llamada",
                            por_que_s11 == '4' ~ "Conexión perdida",
                            por_que_s11 == '5' ~ "No intentada",
                            por_que_s11 == '6' ~ "Num incorrecto",
                            por_que_s11 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_12_resultado` = case_when(
                            hay_sintomas_12 == '1' ~ 'Sintomatico',
                            hay_sintomas_12 == '2' ~ 'Asintomatico',
                            por_que_s12 == '1' ~ "No respondió",
                            por_que_s12 == '2' ~ "Rechazó",
                            por_que_s12 == '3' ~ "No entró llamada",
                            por_que_s12 == '4' ~ "Conexión perdida",
                            por_que_s12 == '5' ~ "No intentada",
                            por_que_s12 == '6' ~ "Num incorrecto",
                            por_que_s12 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_13_resultado` = case_when(
                            hay_sintomas_13 == '1' ~ 'Sintomatico',
                            hay_sintomas_13 == '2' ~ 'Asintomatico',
                            por_que_s13 == '1' ~ "No respondió",
                            por_que_s13 == '2' ~ "Rechazó",
                            por_que_s13 == '3' ~ "No entró llamada",
                            por_que_s13 == '4' ~ "Conexión perdida",
                            por_que_s13 == '5' ~ "No intentada",
                            por_que_s13 == '6' ~ "Num incorrecto",
                            por_que_s13 == '7' ~ "Otro"
                        ),
                        
                        `seguimiento_14_resultado` = case_when(
                            hay_sintomas_14 == '1' ~ 'Sintomatico',
                            hay_sintomas_14 == '2' ~ 'Asintomatico',
                            por_que_s14 == '1' ~ "No respondió",
                            por_que_s14 == '2' ~ "Rechazó",
                            por_que_s14 == '3' ~ "No entró llamada",
                            por_que_s14 == '4' ~ "Conexión perdida",
                            por_que_s14 == '5' ~ "No intentada",
                            por_que_s14 == '6' ~ "Num incorrecto",
                            por_que_s14 == '7' ~ "Otro"
                        ),
                        
                    ) %>%
                    mutate(
                        Ultimo_resultado = case_when(
                            !is.na(fecha_seguimiento_14) ~ seguimiento_14_resultado,
                            !is.na(fecha_seguimiento_13) ~ seguimiento_13_resultado,
                            !is.na(fecha_seguimiento_12) ~ seguimiento_12_resultado,
                            !is.na(fecha_seguimiento_11) ~ seguimiento_11_resultado,
                            !is.na(fecha_seguimiento_10) ~ seguimiento_10_resultado,
                            !is.na(fecha_seguimiento_9) ~ seguimiento_9_resultado,
                            !is.na(fecha_seguimiento_8) ~ seguimiento_8_resultado,
                            !is.na(fecha_seguimiento_7) ~ seguimiento_7_resultado,
                            !is.na(fecha_seguimiento_6) ~ seguimiento_6_resultado,
                            !is.na(fecha_seguimiento_5) ~ seguimiento_5_resultado,
                            !is.na(fecha_seguimiento_4) ~ seguimiento_4_resultado,
                            !is.na(fecha_seguimiento_3) ~ seguimiento_3_resultado,
                            !is.na(fecha_seguimiento_2) ~ seguimiento_2_resultado,
                            !is.na(fecha_seguimiento_1) ~ seguimiento_1_resultado,
                        ),
                        `Ultima llamada` = case_when(
                            !is.na(fecha_seguimiento_14) ~ as.Date(fecha_seguimiento_14),
                            !is.na(fecha_seguimiento_13) ~ as.Date(fecha_seguimiento_13),
                            !is.na(fecha_seguimiento_12) ~ as.Date(fecha_seguimiento_12),
                            !is.na(fecha_seguimiento_11) ~ as.Date(fecha_seguimiento_11),
                            !is.na(fecha_seguimiento_10) ~ as.Date(fecha_seguimiento_10),
                            !is.na(fecha_seguimiento_9) ~ as.Date(fecha_seguimiento_9),
                            !is.na(fecha_seguimiento_8) ~ as.Date(fecha_seguimiento_8),
                            !is.na(fecha_seguimiento_7) ~ as.Date(fecha_seguimiento_7),
                            !is.na(fecha_seguimiento_6) ~ as.Date(fecha_seguimiento_6),
                            !is.na(fecha_seguimiento_5) ~ as.Date(fecha_seguimiento_5),
                            !is.na(fecha_seguimiento_4) ~ as.Date(fecha_seguimiento_4),
                            !is.na(fecha_seguimiento_3) ~ as.Date(fecha_seguimiento_3),
                            !is.na(fecha_seguimiento_2) ~ as.Date(fecha_seguimiento_2),
                            !is.na(fecha_seguimiento_1) ~ as.Date(fecha_seguimiento_1),
                            
                        ),
                        `Delta dias`= `Ultima llamada`- `Fecha Inicio Sintomas`,
                        `Fecha Estado de seguimiento` = fecha_es,
                        `Fecha muestra` = format(as.Date(`Fecha muestra`), '%d-%m-%Y'),
                        `Estado de seguimiento` = toupper(`Estado de seguimiento`),
                        distrib_grupo = case_when(
                            `Estado de seguimiento` == "ACTIVO" ~ 1 ,
                            `Estado de seguimiento` == "SIN ESTADO DE SEGUIMIENTO" ~ 2 ,
                            T ~ 3
                        ),
                    ) %>% select(!ends_with('_s')) %>%
                    select(!ends_with('_n')) %>%
                    select(!starts_with('por_que_'))%>%
                    select(!starts_with('E_A'))%>%
                    select(!starts_with('presenta_sintomas'))%>%
                    select(!starts_with('ha_presentado_sintomas_s2'))%>%
                    select(!fecha_es)%>%
                    select(!starts_with('fecha_s3')) %>%
                    arrange(distrib_grupo,Apellido) %>%
                    .[, c(
                        'distrib_grupo',
                        'Clasificación Epi',
                        'Nombre',
                        'Apellido',
                        'Fecha Inicio Sintomas',
                        'Direccion',
                        'Tel',
                        'Sexo',
                        'Ocupación',
                        'Edad',
                        'comorbilidades',
                        'Embarazo',
                        'Unidad_Notificadora',
                        'Estado de seguimiento',
                        'Ultima llamada',
                        "Ultimo_resultado",
                        'Delta dias',
                        'Fecha Estado de seguimiento',
                        'Se tomó muestra',
                        'Fecha muestra',
                        'Resultado muestra',
                        'Fecha primera visita CBR',
                        'Dias entre inicio sintomas y visita CBR',
                        'Fecha ingreso Go.Data',
                        'Fecha hospitalizacion',
                        'fecha_seguimiento_1',
                        'fecha_seguimiento_2',
                        'fecha_seguimiento_3',
                        'fecha_seguimiento_4',
                        'fecha_seguimiento_5',
                        'fecha_seguimiento_6',
                        'fecha_seguimiento_7',
                        'fecha_seguimiento_8',
                        'fecha_seguimiento_9',
                        'fecha_seguimiento_10',
                        'fecha_seguimiento_11',
                        'fecha_seguimiento_12',
                        'fecha_seguimiento_13',
                        'fecha_seguimiento_14',
                        'seguimiento_1_resultado',
                        'seguimiento_2_resultado',
                        'seguimiento_3_resultado',
                        'seguimiento_4_resultado',
                        'seguimiento_5_resultado',
                        'seguimiento_6_resultado',
                        'seguimiento_7_resultado',
                        'seguimiento_8_resultado',
                        'seguimiento_9_resultado',
                        'seguimiento_10_resultado',
                        'seguimiento_11_resultado',
                        'seguimiento_12_resultado',
                        'seguimiento_13_resultado',
                        'seguimiento_14_resultado',
                        'Rastreador')]
                
                
                
                job_1 <- final %>%
                    filter(distrib_grupo == 1)
                
                job_2 <- final %>%
                    filter(distrib_grupo == 2)
                
                job_3 <- final %>%
                    filter(distrib_grupo == 3) %>%
                    mutate(distrib_grupo = "no aplica")
                
                job_1_size <-ceiling(nrow(job_1)/divided_work)
                job_2_size <-ceiling(nrow(job_2)/divided_work)
                job_3_size <-ceiling(nrow(job_3)/divided_work)
                
                for (i in 1:divided_work) {
                    if (i == divided_work) {
                        job_1[((i-1)*job_1_size+1):nrow(job_1),]$distrib_grupo <- i
                    } else {
                        job_1[((i-1)*job_1_size+1):((i-1)*job_1_size+job_1_size),]$distrib_grupo <- i
                    }  
                    
                    if (i == divided_work) {
                        job_2[((i-1)*job_2_size+1):nrow(job_2),]$distrib_grupo <- i
                    } else {
                        job_2[((i-1)*job_2_size+1):((i-1)*job_2_size+job_2_size),]$distrib_grupo <- i
                    }
                }
                
                final  <- rbind(
                    job_1,
                    job_2,
                    job_3
                )
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        
    })
    
    output$herramientaRastreadores <- renderDataTable({
      head(herramientaData())
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
          paste0('Base_datos_brote_DAS_GT_central_',Sys.time(), ".xlsx", sep="")
      },
      content = function(file) {
        write_xlsx(herramientaData(), path = file)
      }
    )

})


