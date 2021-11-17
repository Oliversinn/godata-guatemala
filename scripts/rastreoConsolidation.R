###################################################################################################
url <- "https://godataguatemala.mspas.gob.gt/"                   # <--------------------- insert instance url here, don't forget the slash at end !
username <- "olivera@mazariegos.gt"                           # <--------------------- insert your username for signing into Go.Data webapp here
password <- ""                           # <--------------------- insert your password for signing into Go.Data webapp here
outbreak_id <- "a44faf32-bf27-4b39-a4fb-b9fcf29ac2d7"   # <--------------------- insert your outbreak ID here
language_id = 'd86b2070-fad9-4699-8be9-a8ae5cc0edd2'   # <--------------------- insert your language ID here
###################################################################################################

setwd("~/Documentos/godata-guatemala")

# packages for this script
packages = c("httr","xml2","readr","data.table","dplyr","jsonlite","magrittr","sqldf",'tidyr','rsconnect','godataR')

# check if packages are installed
package.check = lapply(
  packages,
  FUN = function(x) {
    if (! suppressMessages(require(x,character.only = TRUE))) {
      install.packages(x, dependencies = TRUE)
      suppressMessages(library(x, character.only = TRUE))
    }
  }
)

httr::config(ssl_verifypeer = FALSE)
options(dplyr.summarise.inform = FALSE)
options(warn=-1)

# Login para generar el token de autentificación
loginURL = paste(url,'api/users/login', sep = '')
## Credentials
loginBody = list(
  email = username,
  password = password
)
write('Haciendo login...',stdout())
## Post
login = POST(loginURL, body = loginBody, encode = 'json')
## token
token = content(login)$id
userID = content(login)$userId
write('Login Exitoso!',stdout())
# Activating outbreaks
## Seting user URL
activateURL = paste(url,
                    'api/users/',
                    userID,
                    sep=""
)
## Set outbreak ID
activeBody = list(
  activeOutbreakId = outbreak_id,
  languageId = language_id
)  
write('Activando brote de rastreo...', stdout())
##  activate outbreak of cases to download
activeOutbreak = PATCH(activateURL,body = activeBody, add_headers(Authorization=token) ,encode = 'json')
write('Brote activado!',stdout())

# Downloading outbreak cases
## Setting months to download
#meses = seq(from=as.Date("2020-08-12"), to=Sys.Date(), by='month')
#meses_inicio = seq(from=as.Date("2020-08-01"), to=Sys.Date(), by='month')
#meses_final = seq(as.Date('2020-09-01'), by='month', length = length(meses))-1
#meses_inicio[1] = as.Date("2020-08-12")
#meses_final[length(meses)] = Sys.Date()


meses_inicio = seq(from=as.Date("2021-05-12"), to=Sys.Date(), by=15)
meses_final = c(seq(from=as.Date("2021-05-26"), to=Sys.Date(), by=15),Sys.Date())

#meses_inicio = seq(from=as.Date("2020-08-12"), to=as.Date("2020-12-31"), by='month')
#meses_inicio = c(meses_inicio, seq(from=as.Date("2021-01-01"), to=as.Date("2021-05-11"), by=15))
#meses_final = c(seq(from=as.Date("2020-09-11"), to=as.Date("2020-12-31"), by='month'),as.Date("2020-12-31"))
#meses_final = c(meses_final, c(seq(from=as.Date("2021-01-15"), to=as.Date("2021-05-11"), by=15), as.Date("2021-05-11")))

meses_inicio = format(meses_inicio, '%Y-%m-%dT00:00:00.000Z')
meses_inicio = gsub(":","%3A",meses_inicio)
meses_final = format(meses_final, '%Y-%m-%dT23:59:59.999Z')
meses_final = gsub(":","%3A",meses_final)

## Set URL of outbreak cases
caseURL = paste(url,
                'api/outbreaks/',
                outbreak_id,
                '/cases/export?filter=%7B%22where%22%3A%7B%22and%22%3A%5B%7B%22createdAt%22%3A%7B%22between%22%3A%5B%22',
                meses_inicio[1],'%22,%22',meses_final[1],
                '%22%5D%7D%7D%5D,%22useQuestionVariable%22%3A%20true%7D%7D',
                '&type=csv&access_token=',
                token,
                sep="")


## Download outbreak cases
write(paste0('Descargando casos creados desde el ',format(as.Date(meses_inicio[1]),'%d-%m-%Y'),' al ',format(as.Date(meses_final[1]),'%d-%m-%Y'),'...'),stdout())
outbreakCases = GET(caseURL)
request_id = content(outbreakCases)$exportLogId
export.request.status = get_export_status(url = url, username = username, password = password, request_id = request_id)
while(export.request.status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
  Sys.sleep(2)
  export.request.status <- GET(paste0(url,"api/export-logs/",request_id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content()
  message(paste0("...processed ",export.request.status$processedNo, " of ", export.request.status$totalNo, " records"))
}
Sys.sleep(2)
cases <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
  content("text", encoding="UTF-8") %>%
  textConnection() %>%
  read.csv() %>%
  data.table()

cases_clean = cases %>%
  select(`Date of reporting` = `Fecha.de.notificación`, 
         Sexo, 
         `Años Años De Edad` = `Edad.Años.De.Edad`, 
         Clasificación,  
         `Fue Un Contacto` = `Era.un.contacto`, 
         `Fecha De Convertirse En Caso` = `Fecha.de.convertirse.en.caso`,
         Tipo,
         `Condición del Paciente`= Condición.del.Paciente,
         `Fecha de condición` = Fecha.de.condición, 
         `Direcciones Ubicación [1]` = Direcciones.Ubicación..1.,
         `Direcciones Ubicación [1] Localización Geográfica De Nivel [3]` = Direcciones.Ubicación..1..Ubicación.principal..3., 
         dms = Direcciones.Ubicación..1..Ubicación.principal..5., 
         area_salud = Direcciones.Ubicación..1..Ubicación.principal..3.,
         `Direcciones Número De Teléfono [1]` = Direcciones.Número.De.Teléfono..1.,
         `Direcciones Comunidad, aldea o zona [1]` = Direcciones.Comunidad..aldea.o.zona..1., 
         `Creado En` = Creado.En, 
         `Creado Por` = Creado.Por,
         `Estado de seguimiento` = estado_de_seguimiento_1..MV.1.,
         `Resultado de la muestra.` = FE12103resultado_de_la_muestra..MV.1., 
         `¿Se tomó una muestra respiratoria?` = FE121se_tomo_una_muestra_respiratoria..MV.1.,
         `Carné De Identidad` = ID, 
         hospitalizado = FE130la_persona_fue_hospitalizada..MV.1.) %>%
  mutate(Sexo = toupper(Sexo),
         Sexo = ifelse(is.na(Sexo),"SIN DATOS",Sexo),
         Sexo = ifelse(Sexo == "MASCULILNO", "MASCULINO",Sexo),
         Sexo = ifelse(Sexo == "MSCULINO", "MASCULINO",Sexo),
         Sexo = ifelse(Sexo == "MASCULINIO", "MASCULINO",Sexo),
         Sexo = ifelse(Sexo == "MASCULLINO", "MASCULINO",Sexo),
         Sexo = ifelse(Sexo == "M", "MASCULINO",Sexo),
         Sexo = ifelse(Sexo == "F", "FEMENINO",Sexo),
         Sexo= factor(Sexo,levels=c("MASCULINO", "FEMENINO", "SIN DATOS")),
         grupo_etario = case_when(`Años Años De Edad` < 10 ~ "0-9 años",
                                  `Años Años De Edad` >= 10 & `Años Años De Edad` < 20 ~ "10-19 años",
                                  `Años Años De Edad` >= 20 & `Años Años De Edad` < 30 ~ "20-29 años",
                                  `Años Años De Edad` >= 30 & `Años Años De Edad` < 40 ~ "30-39 años",
                                  `Años Años De Edad` >= 40 & `Años Años De Edad` < 50 ~ "40-49 años",
                                  `Años Años De Edad` >= 50 & `Años Años De Edad` < 60 ~ "50-59 años",
                                  `Años Años De Edad` >= 60 & `Años Años De Edad` < 70 ~ "60-69 años",
                                  `Años Años De Edad` >= 70 & `Años Años De Edad` < 80 ~ "70-79 años",
                                  `Años Años De Edad` >= 80 ~ "80+ años",
                                  is.na(`Años Años De Edad`) ~"SIN DATOS"),
         grupo_etario = factor(grupo_etario,
                               levels = c("0-9 años","10-19 años","20-29 años","30-39 años",
                                          "40-49 años","50-59 años","60-69 años","70-79 años",
                                          "80+ años","SIN DATOS")),
         `Date of reporting` = as.Date(as.POSIXlt(as.character(`Date of reporting`),format = "%F")),
         `Fecha De Convertirse En Caso` = as.Date(as.POSIXlt(as.character(`Fecha De Convertirse En Caso`),format = "%F")),
         `Fecha de condición` = as.Date(as.POSIXlt(as.character(`Fecha de condición`),format = "%F")),
         `Direcciones Número De Teléfono [1]` = ifelse(is.na(`Direcciones Número De Teléfono [1]`),'NO CONTACTABLE','CONTACTABLE'),
         area_salud = as.character(area_salud),
         area_salud = case_when(area_salud == "DAS Alta Verapáz" ~ "DAS Alta Verapaz",
                                area_salud %like% 'DAS' ~ area_salud,
                                area_salud %like% 'SIN DATO' ~ area_salud,
                                T ~ 'SIN DATO'),
         area_salud = toupper(area_salud),
         dms = as.character(dms),
         dms = case_when(dms %like% 'DMS' ~ dms,
                         T ~ "SIN DATO"),
         dms = toupper(dms),
         Clasificación = toupper(Clasificación),
         `Creado En` = as.Date(as.POSIXlt(as.character(`Creado En`),format = "%F")),
         `Estado de seguimiento` = as.character(`Estado de seguimiento`),
         `Estado de seguimiento` = case_when(`Estado de seguimiento` == "Bajo seguimiento" ~ "Bajo seguimiento",
                                             `Estado de seguimiento` == "Recuperado" ~ "Recuperado",
                                             `Estado de seguimiento` == "Imposible de contactar" ~ "Imposible de contactar",
                                             `Estado de seguimiento` == "Perdido durante el seguimiento" ~ "Perdido",
                                             `Estado de seguimiento` == "No es posible dar seguimiento domiciliar" ~ case_when(Clasificación == "PROBABLE" ~ 'Fallecido',
                                                                                      hospitalizado == "Si" ~ 'Hospitalizado',
                                                                                      T ~ "Hospitalizado"),
                                             `Estado de seguimiento` == "Otra razón" ~ "Concluído por otra razón",
                                             is.na(`Estado de seguimiento`) ~ "Sin estado de seguimiento",
                                             `Estado de seguimiento` == "" ~ "Sin estado de seguimiento"),
         `Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Bajo seguimiento","Recuperado","Imposible de contactar",
                                                                              'Perdido',"Fallecido", "Hospitalizado","Concluído por otra razón",
                                                                              "Sin estado de seguimiento")),
         `Direcciones Comunidad, aldea o zona [1]` = as.character(`Direcciones Comunidad, aldea o zona [1]`)) %>%
  rename(`Fecha de notificación` = `Date of reporting`,
         Edad = `Años Años De Edad`,
         Teléfono = `Direcciones Número De Teléfono [1]`,
         Condicion = `Condición del Paciente`,
         zona = `Direcciones Comunidad, aldea o zona [1]`) 

### Manipulació inicial para analizar los seguimientos de casos
reportCases = cases %>%
  select(
    `Carné De Identidad` = ID,
    `Creado En` = Creado.En,
    Clasificación,
    `Teléfono` = `Direcciones.Número.De.Teléfono..1.`,
    fecha_es = fecha_es..MV.1.,
    `Estado de seguimiento` = estado_de_seguimiento_1..MV.1.,
    starts_with('seguimiento_'),
    starts_with('fecha_s'),
    starts_with('por_que_'),
    presenta_sintomas_s1 = presenta_sintomas..MV.1.,
    presenta_sintomas_s2 = ha_presentado_sintomas_s2..MV.1.,
    starts_with('presenta_sintomas_s'),
    contains('enfermedades_asociadas'),
    otra_enfermedad = FE11301especifique..MV.1.,
    hospitalizado = FE130la_persona_fue_hospitalizada..MV.1.
  ) %>%
  unite('comorbilidades', c(contains('enfermedades_asociadas'),otra_enfermedad),sep = ', ',na.rm = T) %>%
  mutate(
    fecha_s1 = case_when(is.na(as.POSIXlt(as.character(fecha_s1_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s1_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s1_s..MV.1.),format = "%F"))),
    fecha_s2 = case_when(is.na(as.POSIXlt(as.character(fecha_s2_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s2_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s2_s..MV.1.),format = "%F"))),
    fecha_s3 = case_when(is.na(as.POSIXlt(as.character(fecha_s3_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s3_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s3_s..MV.1.),format = "%F"))),
    fecha_s4 = case_when(is.na(as.POSIXlt(as.character(fecha_s4_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s4_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s4_s..MV.1.),format = "%F"))),
    fecha_s5 = case_when(is.na(as.POSIXlt(as.character(fecha_s5_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s5_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s5_s..MV.1.),format = "%F"))),
    fecha_s6 = case_when(is.na(as.POSIXlt(as.character(fecha_s6_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s6_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s6_s..MV.1.),format = "%F"))),
    fecha_s7 = case_when(is.na(as.POSIXlt(as.character(fecha_s7_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s7_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s7_s..MV.1.),format = "%F"))),
    fecha_s8 = case_when(is.na(as.POSIXlt(as.character(fecha_s8_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s8_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s8_s..MV.1.),format = "%F"))),
    fecha_s9 = case_when(is.na(as.POSIXlt(as.character(fecha_s9_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s9_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s9_s..MV.1.),format = "%F"))),
    fecha_s10 = case_when(is.na(as.POSIXlt(as.character(fecha_s10_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s10_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s10_s..MV.1.),format = "%F"))),
    fecha_s11 = case_when(is.na(as.POSIXlt(as.character(fecha_s11_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s11_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s11_s..MV.1.),format = "%F"))),
    fecha_s12 = case_when(is.na(as.POSIXlt(as.character(fecha_s12_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s12_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s12_s..MV.1.),format = "%F"))),
    fecha_s13 = case_when(is.na(as.POSIXlt(as.character(fecha_s13_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s13_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s13_s..MV.1.),format = "%F"))),
    fecha_s14 = case_when(is.na(as.POSIXlt(as.character(fecha_s14_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s14_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s14_s..MV.1.),format = "%F"))),
    fecha_s15 = case_when(is.na(as.POSIXlt(as.character(fecha_s15_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s15_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s15_s..MV.1.),format = "%F"))),
    fecha_s16 = case_when(is.na(as.POSIXlt(as.character(fecha_s16_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s16..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s16_s..MV.1.),format = "%F"))),
    fecha_s17 = case_when(is.na(as.POSIXlt(as.character(fecha_s17_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s17_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s17_s..MV.1.),format = "%F"))),
    fecha_s18 = case_when(is.na(as.POSIXlt(as.character(fecha_s18_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s18_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s18_s..MV.1.),format = "%F"))),
    fecha_s19 = case_when(is.na(as.POSIXlt(as.character(fecha_s19_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s19_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s19_s..MV.1.),format = "%F"))),
    fecha_s20 = case_when(is.na(as.POSIXlt(as.character(fecha_s20_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s20_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s20_s..MV.1.),format = "%F"))),
    #maxDate = apply(across(starts_with('fecha_s')),1,max, na.rm = T),
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
    `Teléfono` = ifelse(is.na(`Teléfono`),'NO CONTACTABLE','CONTACTABLE'),
    `Creado En` = as.Date(`Creado En`),
    Clasificación = toupper(Clasificación),
    `Estado de seguimiento` = as.character(`Estado de seguimiento`),
    `Estado de seguimiento` = case_when(`Estado de seguimiento` == "Bajo seguimiento" ~ "Bajo seguimiento",
                                        `Estado de seguimiento` == "Recuperado" ~ "Recuperado",
                                        `Estado de seguimiento` == "Imposible de contactar" ~ "Imposible de contactar",
                                        `Estado de seguimiento` == "Perdido durante el seguimiento" ~ "Perdido",
                                        `Estado de seguimiento` == "No es posible dar seguimiento domiciliar" ~ case_when(Clasificación == "PROBABLE" ~ 'Fallecido',
                                                                                                                          hospitalizado == "Si" ~ 'Hospitalizado',
                                                                                                                          T ~ "Hospitalizado"),
                                        `Estado de seguimiento` == "Otra razón" ~ "Concluído por otra razón",
                                        is.na(`Estado de seguimiento`) ~ "Sin estado de seguimiento",
                                        `Estado de seguimiento` == "" ~ "Sin estado de seguimiento"),
    `Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Bajo seguimiento","Recuperado","Imposible de contactar",
                                                                         'Perdido',"Fallecido", "Hospitalizado","Concluído por otra razón",
                                                                         "Sin estado de seguimiento")),
  ) %>%
  select(!ends_with('_s..MV.1.')) %>%
  select(!ends_with('_n..MV.1.')) %>%
  select(!ends_with('fecha_s16..MV.1.')) %>%
  select(-fecha_segunda_dosis..MV.1.) %>%
  filter(
    Clasificación != 'NO ES UN CASO (DESCARTADO)',
    Clasificación != 'SOSPECHOSO E'
  ) %>%
  rename(
    seguimiento1 = seguimiento_1..MV.1.,
    seguimiento2 = seguimiento_2..MV.1.,
    seguimiento3 = seguimiento_3..MV.1.,
    seguimiento4 = seguimiento_4..MV.1.,
    seguimiento5 = seguimiento_5..MV.1.,
    seguimiento6 = seguimiento_6..MV.1.,
    seguimiento7 = seguimiento_7..MV.1.,
    seguimiento8 = seguimiento_8..MV.1.,
    seguimiento9 = seguimiento_9..MV.1.,
    seguimiento10 = seguimiento_10..MV.1.,
    seguimiento11 = seguimiento_11..MV.1.,
    seguimiento12 = seguimiento_12..MV.1.,
    seguimiento13 = seguimiento_13..MV.1.,
    seguimiento14 = seguimiento_14..MV.1.,
    seguimiento15 = seguimiento_15..MV.1.,
    seguimiento16 = seguimiento_16..MV.1.,
    seguimiento17 = seguimiento_17..MV.1.,
    seguimiento18 = seguimiento_18..MV.1.,
    seguimiento19 = seguimiento_19..MV.1.,
    seguimiento20 = seguimiento_20..MV.1.,
    porque1 = por_que_1..MV.1.,
    porque2 = por_que_s2..MV.1.,
    porque3 = por_que_s3..MV.1.,
    porque4 = por_que_s4..MV.1.,
    porque5 = por_que_s5..MV.1.,
    porque6 = por_que_s6..MV.1.,
    porque7 = por_que_7..MV.1.,
    porque8 = por_que_s8..MV.1.,
    porque9 = por_que_9..MV.1.,
    porque10 = por_que_s10..MV.1.,
    porque11 = por_que_11..MV.1.,
    porque12 = por_que_s12..MV.1.,
    porque13 = por_que_s13..MV.1.,
    porque14 = por_que_s14..MV.1.,
    porque15 = por_que_s15..MV.1.,
    porque16 = por_que_s16..MV.1.,
    porque17 = por_que_s17..MV.1.,
    porque18 = por_que_s18..MV.1.,
    porque19 = por_que_s19..MV.1.,
    porque20 = por_que_s20..MV.1.,
    sintomas1 = presenta_sintomas_s1,
    sintomas2 = presenta_sintomas_s2,
    sintomas3 = presenta_sintomas_s3..MV.1.,
    sintomas4 = presenta_sintomas_s4..MV.1.,
    sintomas5 = presenta_sintomas_s5..MV.1.,
    sintomas6 = presenta_sintomas_s6..MV.1.,
    sintomas7 = presenta_sintomas_s7..MV.1.,
    sintomas8 = presenta_sintomas_s8..MV.1.,
    sintomas9 = presenta_sintomas_s9..MV.1.,
    sintomas10 = presenta_sintomas_s10..MV.1.,
    sintomas11 = presenta_sintomas_s11..MV.1.,
    sintomas12 = presenta_sintomas_s12..MV.1.,
    sintomas13 = presenta_sintomas_s13..MV.1.,
    sintomas14 = presenta_sintomas_s14..MV.1.,
    sintomas15 = presenta_sintomas_s15..MV.1.,
    sintomas16 = presenta_sintomas_s16..MV.1.,
    sintomas17 = presenta_sintomas_s17..MV.1.,
    sintomas18 = presenta_sintomas_s18..MV.1.,
    sintomas19 = presenta_sintomas_s19..MV.1.,
    sintomas20 = presenta_sintomas_s20..MV.1.,
  )
### Seleccion de solo la información de los seguimientos
seguimientos_casos = reportCases %>%
  select(
    `Carné De Identidad`,
    starts_with('fecha_s'),
    starts_with('seguimiento'),
    starts_with('sintomas'),
    starts_with('porque')
  )
### obtener la información del primer seguimiento
seguimientos = seguimientos_casos %>%
  select(
    `Carné De Identidad`,
    paste0('fecha_s','1'),
    paste0('seguimiento','1'),
    paste0('porque','1'),
    paste0('sintomas','1'),
  ) 
### Renombrar las variables de manera general
colnames(seguimientos) = c('Carné De Identidad', 'fecha_seguimiento', 'seguimiento', 'porque', 'sintomas')
### Eliminar aquellos seguimientos vacios
seguimientos = seguimientos %>%
  mutate(
    seguimiento = as.character(seguimiento),
    porque = as.character(porque),
    sintomas = as.character(sintomas)
  )
### Hacer lo anterior para los 20 posibles seguimientos
for(i in 2:20) {
  seguimientosi = seguimientos_casos %>%
    select(
      `Carné De Identidad`,
      paste0('fecha_s',as.character(i)),
      paste0('seguimiento',as.character(i)),
      paste0('porque',as.character(i)),
      paste0('sintomas',as.character(i)),
    ) 
  colnames(seguimientosi) = c('Carné De Identidad', 'fecha_seguimiento', 'seguimiento', 'porque', 'sintomas')
  seguimientosi = seguimientosi %>%
    filter(
      
    ) %>%
    mutate(
      seguimiento = as.character(seguimiento),
      porque = as.character(porque),
      sintomas = as.character(sintomas)
    )
  seguimientos = bind_rows(seguimientos,seguimientosi)
}
### información de los casos utilizada en los reportes
caseInfo = reportCases %>%
  select(
    `Carné De Identidad`,
    `Estado de seguimiento`,
    fecha_es,
    `Teléfono`,
    Clasificación
  ) %>%
  mutate(
    fecha_es = as.Date(as.POSIXlt(as.character(fecha_es),format='%F'))
  )
### Unir la informacion de los casos con la informacion de sus seguimientos
seguimientos = left_join(seguimientos, caseInfo)

write('Casos descargados!',stdout())


for (i in c(2:length(meses_inicio))) {
  mesi = meses_inicio[i]
  mesf = meses_final[i]
  caseURL = paste(url,
                  'api/outbreaks/',
                  outbreak_id,
                  '/cases/export?filter=%7B%22where%22%3A%7B%22and%22%3A%5B%7B%22createdAt%22%3A%7B%22between%22%3A%5B%22',
                  mesi,'%22,%22',mesf,
                  '%22%5D%7D%7D%5D,%22useQuestionVariable%22%3A%20true%7D%7D',
                  '&type=csv&access_token=',
                  get_access_token(url = url, username = username, password = password),
                  sep="")
  
  write(caseURL)
  write(paste0('Descargando casos creados desde el ',format(as.Date(mesi),'%d-%m-%Y'),' al ',format(as.Date(mesf),'%d-%m-%Y'),'...'),stdout())
  outbreakCases = GET(caseURL)
  request_id = content(outbreakCases)$exportLogId
  export.request.status = get_export_status(url = url, username = username, password = password, request_id = request_id)
  while(export.request.status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
    Sys.sleep(2)
    export.request.status <- GET(paste0(url,"api/export-logs/",request_id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
      content()
    message(paste0("...processed ",export.request.status$processedNo, " of ", export.request.status$totalNo, " records"))
  }
  Sys.sleep(2)
  cases <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content("text", encoding="UTF-8") %>%
    textConnection() %>%
    read.csv() %>%
    data.table()
  
  cases_clean2 = cases %>%
    select(`Date of reporting` = `Fecha.de.notificación`, 
           Sexo, 
           `Años Años De Edad` = `Edad.Años.De.Edad`, 
           Clasificación,  
           `Fue Un Contacto` = `Era.un.contacto`, 
           `Fecha De Convertirse En Caso` = `Fecha.de.convertirse.en.caso`,
           Tipo,
           `Condición del Paciente`= Condición.del.Paciente,
           `Fecha de condición` = Fecha.de.condición, 
           `Direcciones Ubicación [1]` = Direcciones.Ubicación..1.,
           `Direcciones Ubicación [1] Localización Geográfica De Nivel [3]` = Direcciones.Ubicación..1..Ubicación.principal..3., 
           dms = Direcciones.Ubicación..1..Ubicación.principal..5., 
           area_salud = Direcciones.Ubicación..1..Ubicación.principal..3.,
           `Direcciones Número De Teléfono [1]` = Direcciones.Número.De.Teléfono..1.,
           `Direcciones Comunidad, aldea o zona [1]` = Direcciones.Comunidad..aldea.o.zona..1., 
           `Creado En` = Creado.En, 
           `Creado Por` = Creado.Por,
           `Estado de seguimiento` = estado_de_seguimiento_1..MV.1.,
           `Resultado de la muestra.` = FE12103resultado_de_la_muestra..MV.1., 
           `¿Se tomó una muestra respiratoria?` = FE121se_tomo_una_muestra_respiratoria..MV.1.,
           `Carné De Identidad` = ID, 
           hospitalizado = FE130la_persona_fue_hospitalizada..MV.1.) %>%
    mutate(Sexo = toupper(Sexo),
           Sexo = ifelse(is.na(Sexo),"SIN DATOS",Sexo),
           Sexo = ifelse(Sexo == "MASCULILNO", "MASCULINO",Sexo),
           Sexo = ifelse(Sexo == "MSCULINO", "MASCULINO",Sexo),
           Sexo = ifelse(Sexo == "MASCULINIO", "MASCULINO",Sexo),
           Sexo = ifelse(Sexo == "MASCULLINO", "MASCULINO",Sexo),
           Sexo = ifelse(Sexo == "M", "MASCULINO",Sexo),
           Sexo = ifelse(Sexo == "F", "FEMENINO",Sexo),
           Sexo= factor(Sexo,levels=c("MASCULINO", "FEMENINO", "SIN DATOS")),
           grupo_etario = case_when(`Años Años De Edad` < 10 ~ "0-9 años",
                                    `Años Años De Edad` >= 10 & `Años Años De Edad` < 20 ~ "10-19 años",
                                    `Años Años De Edad` >= 20 & `Años Años De Edad` < 30 ~ "20-29 años",
                                    `Años Años De Edad` >= 30 & `Años Años De Edad` < 40 ~ "30-39 años",
                                    `Años Años De Edad` >= 40 & `Años Años De Edad` < 50 ~ "40-49 años",
                                    `Años Años De Edad` >= 50 & `Años Años De Edad` < 60 ~ "50-59 años",
                                    `Años Años De Edad` >= 60 & `Años Años De Edad` < 70 ~ "60-69 años",
                                    `Años Años De Edad` >= 70 & `Años Años De Edad` < 80 ~ "70-79 años",
                                    `Años Años De Edad` >= 80 ~ "80+ años",
                                    is.na(`Años Años De Edad`) ~"SIN DATOS"),
           grupo_etario = factor(grupo_etario,
                                 levels = c("0-9 años","10-19 años","20-29 años","30-39 años",
                                            "40-49 años","50-59 años","60-69 años","70-79 años",
                                            "80+ años","SIN DATOS")),
           `Date of reporting` = as.Date(as.POSIXlt(as.character(`Date of reporting`),format = "%F")),
           `Fecha De Convertirse En Caso` = as.Date(as.POSIXlt(as.character(`Fecha De Convertirse En Caso`),format = "%F")),
           `Fecha de condición` = as.Date(as.POSIXlt(as.character(`Fecha de condición`),format = "%F")),
           `Direcciones Número De Teléfono [1]` = ifelse(is.na(`Direcciones Número De Teléfono [1]`),'NO CONTACTABLE','CONTACTABLE'),
           area_salud = as.character(area_salud),
           area_salud = case_when(area_salud == "DAS Alta Verapáz" ~ "DAS Alta Verapaz",
                                  area_salud %like% 'DAS' ~ area_salud,
                                  area_salud %like% 'SIN DATO' ~ area_salud,
                                  T ~ 'SIN DATO'),
           area_salud = toupper(area_salud),
           dms = as.character(dms),
           dms = case_when(dms %like% 'DMS' ~ dms,
                           T ~ "SIN DATO"),
           dms = toupper(dms),
           Clasificación = toupper(Clasificación),
           `Creado En` = as.Date(as.POSIXlt(as.character(`Creado En`),format = "%F")),
           `Estado de seguimiento` = as.character(`Estado de seguimiento`),
           `Estado de seguimiento` = case_when(`Estado de seguimiento` == "Bajo seguimiento" ~ "Bajo seguimiento",
                                               `Estado de seguimiento` == "Recuperado" ~ "Recuperado",
                                               `Estado de seguimiento` == "Imposible de contactar" ~ "Imposible de contactar",
                                               `Estado de seguimiento` == "Perdido durante el seguimiento" ~ "Perdido",
                                               `Estado de seguimiento` == "No es posible dar seguimiento domiciliar" ~ case_when(Clasificación == "PROBABLE" ~ 'Fallecido',
                                                                                                                                 hospitalizado == "Si" ~ 'Hospitalizado',
                                                                                                                                 T ~ "Hospitalizado"),
                                               `Estado de seguimiento` == "Otra razón" ~ "Concluído por otra razón",
                                               is.na(`Estado de seguimiento`) ~ "Sin estado de seguimiento",
                                               `Estado de seguimiento` == "" ~ "Sin estado de seguimiento"),
           `Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Bajo seguimiento","Recuperado","Imposible de contactar",
                                                                                'Perdido',"Fallecido", "Hospitalizado","Concluído por otra razón",
                                                                                "Sin estado de seguimiento")),
           `Direcciones Comunidad, aldea o zona [1]` = as.character(`Direcciones Comunidad, aldea o zona [1]`)) %>%
    rename(`Fecha de notificación` = `Date of reporting`,
           Edad = `Años Años De Edad`,
           Teléfono = `Direcciones Número De Teléfono [1]`,
           Condicion = `Condición del Paciente`,
           zona = `Direcciones Comunidad, aldea o zona [1]`) 
  
  reportCases2 =  cases %>%
    select(
      `Carné De Identidad` = ID,
      `Creado En` = Creado.En,
      Clasificación,
      `Teléfono` = `Direcciones.Número.De.Teléfono..1.`,
      fecha_es = fecha_es..MV.1.,
      `Estado de seguimiento` = estado_de_seguimiento_1..MV.1.,
      starts_with('seguimiento_'),
      starts_with('fecha_s'),
      starts_with('por_que_'),
      presenta_sintomas_s1 = presenta_sintomas..MV.1.,
      presenta_sintomas_s2 = ha_presentado_sintomas_s2..MV.1.,
      starts_with('presenta_sintomas_s'),
      contains('enfermedades_asociadas'),
      otra_enfermedad = FE11301especifique..MV.1.,
      hospitalizado = FE130la_persona_fue_hospitalizada..MV.1.
    ) %>%
    unite('comorbilidades', c(contains('enfermedades_asociadas'),otra_enfermedad),sep = ', ',na.rm = T) %>%
    mutate(
      fecha_s1 = case_when(is.na(as.POSIXlt(as.character(fecha_s1_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s1_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s1_s..MV.1.),format = "%F"))),
      fecha_s2 = case_when(is.na(as.POSIXlt(as.character(fecha_s2_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s2_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s2_s..MV.1.),format = "%F"))),
      fecha_s3 = case_when(is.na(as.POSIXlt(as.character(fecha_s3_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s3_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s3_s..MV.1.),format = "%F"))),
      fecha_s4 = case_when(is.na(as.POSIXlt(as.character(fecha_s4_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s4_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s4_s..MV.1.),format = "%F"))),
      fecha_s5 = case_when(is.na(as.POSIXlt(as.character(fecha_s5_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s5_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s5_s..MV.1.),format = "%F"))),
      fecha_s6 = case_when(is.na(as.POSIXlt(as.character(fecha_s6_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s6_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s6_s..MV.1.),format = "%F"))),
      fecha_s7 = case_when(is.na(as.POSIXlt(as.character(fecha_s7_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s7_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s7_s..MV.1.),format = "%F"))),
      fecha_s8 = case_when(is.na(as.POSIXlt(as.character(fecha_s8_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s8_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s8_s..MV.1.),format = "%F"))),
      fecha_s9 = case_when(is.na(as.POSIXlt(as.character(fecha_s9_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s9_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s9_s..MV.1.),format = "%F"))),
      fecha_s10 = case_when(is.na(as.POSIXlt(as.character(fecha_s10_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s10_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s10_s..MV.1.),format = "%F"))),
      fecha_s11 = case_when(is.na(as.POSIXlt(as.character(fecha_s11_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s11_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s11_s..MV.1.),format = "%F"))),
      fecha_s12 = case_when(is.na(as.POSIXlt(as.character(fecha_s12_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s12_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s12_s..MV.1.),format = "%F"))),
      fecha_s13 = case_when(is.na(as.POSIXlt(as.character(fecha_s13_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s13_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s13_s..MV.1.),format = "%F"))),
      fecha_s14 = case_when(is.na(as.POSIXlt(as.character(fecha_s14_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s14_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s14_s..MV.1.),format = "%F"))),
      fecha_s15 = case_when(is.na(as.POSIXlt(as.character(fecha_s15_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s15_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s15_s..MV.1.),format = "%F"))),
      fecha_s16 = case_when(is.na(as.POSIXlt(as.character(fecha_s16_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s16..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s16_s..MV.1.),format = "%F"))),
      fecha_s17 = case_when(is.na(as.POSIXlt(as.character(fecha_s17_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s17_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s17_s..MV.1.),format = "%F"))),
      fecha_s18 = case_when(is.na(as.POSIXlt(as.character(fecha_s18_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s18_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s18_s..MV.1.),format = "%F"))),
      fecha_s19 = case_when(is.na(as.POSIXlt(as.character(fecha_s19_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s19_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s19_s..MV.1.),format = "%F"))),
      fecha_s20 = case_when(is.na(as.POSIXlt(as.character(fecha_s20_s..MV.1.),format = "%F")) ~ as.Date(as.POSIXlt(as.character(fecha_s20_n..MV.1.),format = "%F")), T ~ as.Date(as.POSIXlt(as.character(fecha_s20_s..MV.1.),format = "%F"))),
      #maxDate = apply(across(starts_with('fecha_s')),1,max, na.rm = T),
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
      `Teléfono` = ifelse(is.na(`Teléfono`),'NO CONTACTABLE','CONTACTABLE'),
      `Creado En` = as.Date(`Creado En`),
      Clasificación = toupper(Clasificación),
      `Estado de seguimiento` = as.character(`Estado de seguimiento`),
      `Estado de seguimiento` = case_when(`Estado de seguimiento` == "Bajo seguimiento" ~ "Bajo seguimiento",
                                          `Estado de seguimiento` == "Recuperado" ~ "Recuperado",
                                          `Estado de seguimiento` == "Imposible de contactar" ~ "Imposible de contactar",
                                          `Estado de seguimiento` == "Perdido durante el seguimiento" ~ "Perdido",
                                          `Estado de seguimiento` == "No es posible dar seguimiento domiciliar" ~ case_when(Clasificación == "PROBABLE" ~ 'Fallecido',
                                                                                                                            hospitalizado == "Si" ~ 'Hospitalizado',
                                                                                                                            T ~ "Hospitalizado"),
                                          `Estado de seguimiento` == "Otra razón" ~ "Concluído por otra razón",
                                          is.na(`Estado de seguimiento`) ~ "Sin estado de seguimiento",
                                          `Estado de seguimiento` == "" ~ "Sin estado de seguimiento"),
      `Estado de seguimiento` = factor(`Estado de seguimiento`, levels = c("Bajo seguimiento","Recuperado","Imposible de contactar",
                                                                           'Perdido',"Fallecido", "Hospitalizado","Concluído por otra razón",
                                                                           "Sin estado de seguimiento")),
    ) %>%
    select(!ends_with('_s..MV.1.')) %>%
    select(!ends_with('_n..MV.1.')) %>%
    select(!ends_with('fecha_s16..MV.1.')) %>%
    select(-fecha_segunda_dosis..MV.1.) %>%
    filter(
      Clasificación != 'NO ES UN CASO (DESCARTADO)',
      Clasificación != 'SOSPECHOSO E'
    ) %>%
    rename(
      seguimiento1 = seguimiento_1..MV.1.,
      seguimiento2 = seguimiento_2..MV.1.,
      seguimiento3 = seguimiento_3..MV.1.,
      seguimiento4 = seguimiento_4..MV.1.,
      seguimiento5 = seguimiento_5..MV.1.,
      seguimiento6 = seguimiento_6..MV.1.,
      seguimiento7 = seguimiento_7..MV.1.,
      seguimiento8 = seguimiento_8..MV.1.,
      seguimiento9 = seguimiento_9..MV.1.,
      seguimiento10 = seguimiento_10..MV.1.,
      seguimiento11 = seguimiento_11..MV.1.,
      seguimiento12 = seguimiento_12..MV.1.,
      seguimiento13 = seguimiento_13..MV.1.,
      seguimiento14 = seguimiento_14..MV.1.,
      seguimiento15 = seguimiento_15..MV.1.,
      seguimiento16 = seguimiento_16..MV.1.,
      seguimiento17 = seguimiento_17..MV.1.,
      seguimiento18 = seguimiento_18..MV.1.,
      seguimiento19 = seguimiento_19..MV.1.,
      seguimiento20 = seguimiento_20..MV.1.,
      porque1 = por_que_1..MV.1.,
      porque2 = por_que_s2..MV.1.,
      porque3 = por_que_s3..MV.1.,
      porque4 = por_que_s4..MV.1.,
      porque5 = por_que_s5..MV.1.,
      porque6 = por_que_s6..MV.1.,
      porque7 = por_que_7..MV.1.,
      porque8 = por_que_s8..MV.1.,
      porque9 = por_que_9..MV.1.,
      porque10 = por_que_s10..MV.1.,
      porque11 = por_que_11..MV.1.,
      porque12 = por_que_s12..MV.1.,
      porque13 = por_que_s13..MV.1.,
      porque14 = por_que_s14..MV.1.,
      porque15 = por_que_s15..MV.1.,
      porque16 = por_que_s16..MV.1.,
      porque17 = por_que_s17..MV.1.,
      porque18 = por_que_s18..MV.1.,
      porque19 = por_que_s19..MV.1.,
      porque20 = por_que_s20..MV.1.,
      sintomas1 = presenta_sintomas_s1,
      sintomas2 = presenta_sintomas_s2,
      sintomas3 = presenta_sintomas_s3..MV.1.,
      sintomas4 = presenta_sintomas_s4..MV.1.,
      sintomas5 = presenta_sintomas_s5..MV.1.,
      sintomas6 = presenta_sintomas_s6..MV.1.,
      sintomas7 = presenta_sintomas_s7..MV.1.,
      sintomas8 = presenta_sintomas_s8..MV.1.,
      sintomas9 = presenta_sintomas_s9..MV.1.,
      sintomas10 = presenta_sintomas_s10..MV.1.,
      sintomas11 = presenta_sintomas_s11..MV.1.,
      sintomas12 = presenta_sintomas_s12..MV.1.,
      sintomas13 = presenta_sintomas_s13..MV.1.,
      sintomas14 = presenta_sintomas_s14..MV.1.,
      sintomas15 = presenta_sintomas_s15..MV.1.,
      sintomas16 = presenta_sintomas_s16..MV.1.,
      sintomas17 = presenta_sintomas_s17..MV.1.,
      sintomas18 = presenta_sintomas_s18..MV.1.,
      sintomas19 = presenta_sintomas_s19..MV.1.,
      sintomas20 = presenta_sintomas_s20..MV.1.,
    )
  ### Seleccion de solo la información de los seguimientos
  seguimientos_casos2 = reportCases2 %>%
    select(
      `Carné De Identidad`,
      starts_with('fecha_s'),
      starts_with('seguimiento'),
      starts_with('sintomas'),
      starts_with('porque')
    )
  ### obtener la información del primer seguimiento
  seguimientos2 = seguimientos_casos2 %>%
    select(
      `Carné De Identidad`,
      paste0('fecha_s','1'),
      paste0('seguimiento','1'),
      paste0('porque','1'),
      paste0('sintomas','1'),
    ) 
  ### Renombrar las variables de manera general
  colnames(seguimientos2) = c('Carné De Identidad', 'fecha_seguimiento', 'seguimiento', 'porque', 'sintomas')
  ### Eliminar aquellos seguimientos vacios
  seguimientos2 = seguimientos2 %>%
    mutate(
      seguimiento = as.character(seguimiento),
      porque = as.character(porque),
      sintomas = as.character(sintomas)
    )
  ### Hacer lo anterior para los 20 posibles seguimientos
  for(i in 2:20) {
    seguimientosi = seguimientos_casos2 %>%
      select(
        `Carné De Identidad`,
        paste0('fecha_s',as.character(i)),
        paste0('seguimiento',as.character(i)),
        paste0('porque',as.character(i)),
        paste0('sintomas',as.character(i)),
      ) 
    colnames(seguimientosi) = c('Carné De Identidad', 'fecha_seguimiento', 'seguimiento', 'porque', 'sintomas')
    seguimientosi = seguimientosi %>%
      filter(
        
      ) %>%
      mutate(
        seguimiento = as.character(seguimiento),
        porque = as.character(porque),
        sintomas = as.character(sintomas)
      )
    seguimientos2 = bind_rows(seguimientos2,seguimientosi)
  }
  ### información de los casos utilizada en los reportes
  caseInfo2 = reportCases2 %>%
    select(
      `Carné De Identidad`,
      `Estado de seguimiento`,
      fecha_es,
      `Teléfono`,
      Clasificación
    ) %>%
    mutate(
      fecha_es = as.Date(as.POSIXlt(as.character(fecha_es),format='%F'))
    )
  ### Unir la informacion de los casos con la informacion de sus seguimientos
  seguimientos2 = left_join(seguimientos2, caseInfo2)
  
  write('Casos descargados!',stdout())
  
  cases_clean = dplyr::bind_rows(cases_clean,cases_clean2)
  seguimientos = dplyr::bind_rows(seguimientos,seguimientos2)
  
  
}



###########################################################################################
###########################################################################################
################## Termina descarga de base de datos de casos #############################
##################  Empieza union con base de datos anterior  #############################
###########################################################################################
###########################################################################################

rastreo_cases20210511 = read_csv("databases/rastreo_cases20210511.csv",guess_max = 50000)
seguimientos20210511 = read_csv("databases/report_cases20210511.csv", guess_max = 50000) %>% mutate(fecha_es = as.Date(fecha_es))

write('Uniendo bases de datos nueva con base de datos anterior',stdout())
cases_clean = bind_rows(rastreo_cases20210511, cases_clean)
seguimientos = bind_rows(seguimientos20210511, seguimientos)
seguimientos = left_join(seguimientos, cases_clean %>% select(`Carné De Identidad`, `Creado En`, `¿Se tomó una muestra respiratoria?`, area_salud, dms))
write('Exito!',stdout())
cases_clean = cases_clean %>%
  rename(`Fecha de notificacion` = `Fecha de notificación`,
         Clasificacion = Clasificación,
         `Fecha de condicion` = `Fecha de condición`,
         `Direcciones Ubicacion [1]` = `Direcciones Ubicación [1]`,
         `Direcciones Ubicacion [1] Localizacion Geografica De Nivel [3]` = `Direcciones Ubicación [1] Localización Geográfica De Nivel [3]`,
         `Telefono` = `Teléfono`,
         `¿Se tomo una muestra respiratoria?` = `¿Se tomó una muestra respiratoria?`,
         `Carne De Identidad` = `Carné De Identidad`)

seguimientos = seguimientos %>%
  rename(`¿Se tomo una muestra respiratoria?` =  `¿Se tomó una muestra respiratoria?`,
         Telefono = Teléfono,
         `Carne De Identidad` = `Carné De Identidad`,
         Clasificacion = Clasificación)

#seguimientos = seguimientos %>%
#  rename(`¿Se tomó una muestra respiratoria?` = `¿Se tomo una muestra respiratoria?`,
#         Teléfono = Telefono,
#         `Carné De Identidad` = `Carne De Identidad`,
#         Clasificación = Clasificacion )

###########################################################################################
###########################################################################################
################## Empieza descarga de base de datos de contactos #########################
###########################################################################################
###########################################################################################


# Downloading outbreak contacts
## Set URL of of outbreak contacts
contactsURL = paste(url,
                    'api/outbreaks/',
                    outbreak_id,
                    '/contacts/export?type=csv&access_token=',
                    get_access_token(url = url, username = username, password = password),
                    sep="")
## Download outbreak contacts
write('Descargando contactos...',stdout())
outbreakContacts = GET(contactsURL)
#contacts = data.table(suppressMessages(content(outbreakContacts, guess_max = 50000)))
request_id = content(outbreakContacts)$exportLogId
export.request.status = get_export_status(url = url, username = username, password = password, request_id = request_id)
while(export.request.status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
  Sys.sleep(2)
  export.request.status <- GET(paste0(url,"api/export-logs/",request_id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content()
  message(paste0("...processed ",export.request.status$processedNo, " of ", export.request.status$totalNo, " records"))
}
Sys.sleep(2)
contacts <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
  content("text", encoding="UTF-8") %>%
  textConnection() %>%
  read.csv() %>%
  data.table()


#contacts = get_contacts(url = url, username = username, password = password, outbreak_id = outbreak_id)

contacts_clean = contacts %>%
  select(`Fecha de notificación` = Fecha.de.notificación, 
         Sexo = Sexo,
         Ocupación = Ocupación, 
         `Años Años De Edad)` = Edad.Edad..años., 
         `Fecha De Convertirse En Caso` = Fecha.de.convertirse.en.contacto,
         Tipo = Tipo, 
         `Seguimiento Seguimiento De Estatus Final` = Seguimiento.Estado.final.del.seguimiento, 
         `Seguimiento Comienzo Del Seguimiento` = Seguimiento.Inicio.de.seguimiento, 
         `Seguimiento Final Del Seguimiento` = Seguimiento.Final.Del.Seguimiento, 
         `Relación Nivel de riesgo` = Relación.Nivel.de.riesgo, 
         `Relación Personas Persona De Referencia` = Relación.Personas.Persona.De.Referencia,
         `Direcciones Ubicación [1]` = Direcciones.Ubicación..1., 
         `Direcciones Ubicación [1] Localización Geográfica De Nivel [1]` = Direcciones.Ubicación..1..Ubicación.principal..1.,
         `Direcciones Ubicación [1] Localización Geográfica De Nivel [2]` = Direcciones.Ubicación..1..Ubicación.principal..2., 
         dms = Direcciones.Ubicación..1..Ubicación.principal..5., 
         area_salud = Direcciones.Ubicación..1..Ubicación.principal..3.,
         `Direcciones Número De Teléfono [1]` = Direcciones.Número.De.Teléfono..1.,
         `Direcciones Comunidad, aldea o zona [1]` = Direcciones.Comunidad..aldea.o.zona..1., 
         `Creado En` = Creado.En) %>%
  mutate(`Fecha de notificación` = as.Date(as.POSIXlt(as.character(`Fecha de notificación`)),format = "%F"),
         `Creado En` = as.Date(as.POSIXlt(as.character(`Creado En`)),format = "%F"),
         `Fecha De Convertirse En Caso` = as.Date(as.POSIXlt(as.character(`Fecha De Convertirse En Caso`),format="%F")),
         Sexo = toupper(Sexo),
         Sexo = ifelse(is.na(Sexo),"SIN DATOS",Sexo),
         Sexo= factor(Sexo,levels=c("MASCULINO", "FEMENINO", "SIN DATOS")),
         grupo_etario = case_when(`Años Años De Edad)` < 10 ~ "0-9 años",
                                  `Años Años De Edad)` >= 10 & `Años Años De Edad)` < 20 ~ "10-19 años",
                                  `Años Años De Edad)` >= 20 & `Años Años De Edad)` < 30 ~ "20-29 años",
                                  `Años Años De Edad)` >= 30 & `Años Años De Edad)` < 40 ~ "30-39 años",
                                  `Años Años De Edad)` >= 40 & `Años Años De Edad)` < 50 ~ "40-49 años",
                                  `Años Años De Edad)` >= 50 & `Años Años De Edad)` < 60 ~ "50-59 años",
                                  `Años Años De Edad)` >= 60 & `Años Años De Edad)` < 70 ~ "60-69 años",
                                  `Años Años De Edad)` >= 70 & `Años Años De Edad)` < 80 ~ "70-79 años",
                                  `Años Años De Edad)` >= 80 ~ "80+ años",
                                  is.na(`Años Años De Edad)`) ~"SIN DATOS"),
         grupo_etario = factor(grupo_etario,
                               levels = c("0-9 años","10-19 años","20-29 años","30-39 años",
                                          "40-49 años","50-59 años","60-69 años","70-79 años",
                                          "80+ años","SIN DATOS")),
         `Direcciones Número De Teléfono [1]` = ifelse(is.na(`Direcciones Número De Teléfono [1]`),'NO CONTACTABLE','CONTACTABLE'),
         `Direcciones Número De Teléfono [1]` = factor(`Direcciones Número De Teléfono [1]`, levels = c('NO CONTACTABLE','CONTACTABLE')),
         area_salud = as.character(area_salud),
         area_salud = case_when(area_salud == "DAS Alta Verapáz" ~ "DAS Alta Verapaz",
                                area_salud %like% 'DAS' ~ area_salud,
                                area_salud %like% 'SIN DATO' ~ area_salud,
                                T ~ 'SIN DATO'),
         area_salud = toupper(area_salud),
         dms = as.character(dms),
         dms = case_when(dms %like% 'DMS' ~ dms,
                         T ~ "SIN DATO"),
         dms = toupper(dms),
         `Seguimiento Seguimiento De Estatus Final` = as.character(`Seguimiento Seguimiento De Estatus Final`),
         `Seguimiento Seguimiento De Estatus Final` = case_when(`Seguimiento Seguimiento De Estatus Final` == "Información rastreo incompleta" ~ "Información insuficiente",
                                                                T ~ `Seguimiento Seguimiento De Estatus Final`)) %>%
  rename(Edad = `Años Años De Edad)`,
         `Caso relacionado` = `Relación Personas Persona De Referencia`,
         `Status final de seguimiento` = `Seguimiento Seguimiento De Estatus Final`,
         `Inicio del seguimiento` = `Seguimiento Comienzo Del Seguimiento`,
         `Final del seguimiento` = `Seguimiento Final Del Seguimiento`,
         Teléfono = `Direcciones Número De Teléfono [1]`,
         zona = `Direcciones Comunidad, aldea o zona [1]`,
         riesgo = `Relación Nivel de riesgo`)

write('Contactos descargados!', stdout())

contacts_clean = contacts_clean %>%
  rename(`Fecha de notificacion` = `Fecha de notificación`,
         Ocupacion = Ocupación,
         `Direcciones Ubicacion [1]` = `Direcciones Ubicación [1]`,
         `Direcciones Ubicacion [1] Localizacion Geografica De Nivel [1]` = `Direcciones Ubicación [1] Localización Geográfica De Nivel [1]`,
         `Direcciones Ubicacion [1] Localizacion Geografica De Nivel [2]` = `Direcciones Ubicación [1] Localización Geográfica De Nivel [2]`,
         `Telefono` = `Teléfono`)

## Dwnload Followups
#specify date ranges, for follow up filters
date_now <- format(Sys.time(), "%Y-%m-%dT23:59:59.999Z")                  
date_5d_ago <- format((Sys.Date()-30), "%Y-%m-%dT23:59:59.999Z")
date_start = "2020-08-12T00:00:00.000Z"



# import contact follow-ups, last 21 days only to avoid system time-out 
write('Descargando seguimientos de contactos...',stdout())
response_followups <- GET(paste0(
  url,
  "api/outbreaks/",
  outbreak_id,
  "/follow-ups/export?type=csv&filter={%22where%22:{%22and%22:[{%22date%22:{%22between%22:[%22",
  date_start,
  "%22,%22",
  date_now,
  "%22]}}]}}&access_token=",
  get_access_token(url = url, username = username, password = password)))
request_id = content(response_followups)$exportLogId
export.request.status = get_export_status(url = url, username = username, password = password, request_id = request_id)
while(export.request.status$statusStep != "LNG_STATUS_STEP_EXPORT_FINISHED") {
  Sys.sleep(2)
  export.request.status <- GET(paste0(url,"api/export-logs/",request_id,"?access_token=",get_access_token(url=url, username=username, password=password))) %>%
    content()
  message(paste0("...processed ",export.request.status$processedNo, " of ", export.request.status$totalNo, " records"))
}
Sys.sleep(2)
followups <- GET(paste0(url,"api/export-logs/",request_id,"/download?access_token=",get_access_token(url=url, username=username, password=password))) %>%
  content("text", encoding="UTF-8") %>%
  textConnection() %>%
  read.csv() %>%
  data.table()
#followups = data.table(suppressMessages(content(response_followups, guess_max = 50000)))
write('Seguimientos descargados!', stdout())
followups = followups %>%
  select(ID, `Creado En` = Creado.En, `Estado`)

write('Guardando bases de datos...',stdout())
write_excel_csv(cases_clean,'./databases/rastreo_cases.csv')
write_excel_csv(cases_clean,'./DashboardRastreo/data/rastreo_cases.csv')

write_excel_csv(seguimientos,'./databases/report_cases.csv')
write_excel_csv(seguimientos,'./DashboardRastreo/data/report_cases.csv')


write_excel_csv(contacts_clean,'./databases/rastreo_contacts.csv')
write_excel_csv(contacts_clean,'./DashboardRastreo/data/rastreo_contacts.csv')

write_excel_csv(followups,'./databases/rastreo_followups.csv')
write_excel_csv(followups,'./DashboardRastreo/data/rastreo_followups.csv')
write('Bases de datos guardadas!',stdout())

write('Actualizando tablero...',stdout())
deployApp(paste(getwd(),'/DashboardRastreo', sep = ''), forceUpdate = TRUE, launch.browser = FALSE)
