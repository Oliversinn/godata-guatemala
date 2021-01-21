<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->

<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/othneildrew/Best-README-Template">
    <img src="https://extranet.who.int/goarn/sites/default/files/go.data_.png" alt="Logo"  height="80">
  </a>

  <h3 align="center">Go.Data Guatemala - Interoperabilidad</h3>

  <p align="center">
    Transformación, manipulación y visualización de los datos de la implementación de GoData en Guatemala para el rastreo de casos y contactos COVID-19.
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Tabla de contenidos</summary>
  <ol>
    <li>
      <a href="#sobre-el-proyecto">Sobre el proyecto</a>
      <ul>
        <li><a href="#desarrollado-con">Desarrollado con</a></li>
      </ul>
    </li>
    <li>
      <a href="#antes-de-empezar">Antes de empezar</a>
      <ul>
        <li><a href="#prerequisitos">Prerequisitos</a></li>
        <li><a href="#instalacion">Instalación</a></li>
      </ul>
    </li>
    <li><a href="#uso">Uso</a>
      <ul>
        <li><a href="#script-de-automatización">Script de automatización</a></li>
        <li><a href="#dashboard">Dashboard</a></li>
      </ul>
    </li>
    <li><a href="#contacto">Contacto</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## Sobre el proyecto

Los datos de instituciones o sistemas externos a Go.Data son muy valiosos para el país y el Ministerio de Salud de Guatemala. Por lo que la interoperabilidad con estos otros sistemas es de suma importancia para tener una rica base de datos. Esta interoperabilidad es posible gracias al API de Go.Data.

Esta es una guía para que futuras organizaciones puedan interoperar en conjunto de Go.Data para reportar los resultados del rastreo de casos y contantos COVID-19. En esta guia se encuentran las funciones del API a utilizar, el modelo del JSON a enviar y un diccionario de variables que se utilizan para mandar los datos a Go.Data.

<strong>Nota: la ruta al API está en `https://godataguatemala.mspas.gob.gt/api/`. Todas las funciones mencionadas en esta guia deben de agregarse a este <em>string</em>. En las respeustas del API solo se mencionarán los campos de utilidad e importante.</strong>

<!-- GETTING STARTED -->
## Antes de empezar

Se debe tener un usuario en Go.Data con permiso de publicar casos y contactos en Go.Data con acceso a un brote de pruebas. Esto se debe solicitar al administrador de Go.Data Guatemala al igual que el ID del brote de pruebas. Al finalizar las pruebas se dara acceso al brote en producción y su ID.

Recomiendo hacer las pruebas individuales en https://godataguatemala.mspas.gob.gt/explorer el cual es un ambiente amigable para el debuging de los JSONs.

### Iniciar sesión

* Funcion del API: POST `users/login`
* Modelo
  ```python
  {
    "email": "correo@correo.com", #Correo del usuario que inicia sesión
    "password": "contraseña" #Contraseña del usuario que inicia sesión
  }
  ```
* Respuesta: es importante guardar el token de identificación con el cual se llaman el resto de funciones del API.
  ```python
  {
    ...
    "id": "fZasdQYweHtgbIkyszlW03asdfHJJTeCh9fgaIA33xbT0eKKXkOv3Basdfds0UC" #Token
    ...
  }
  ```


## Nuevo caso

No todos los sistemas son iguales por lo que es entendible que no recopilen la misma información que Go.Data para los casos. Aun así, hay variables mínimas que deben ser capturadas. Las variables opcionales tendran una tituto de "Opcionales" para que las puedan identificar y estarán hasta el final del modelo.

* Funcion del API: POST `/outbreaks/{id}/cases`. Sustituir `{id}` con el ID del brote.
* Modelo
  ```python
  {
    'firstName' :'nombres', #Colocar ambos nombres
    'lastName' : 'apellidos', #Colocar ambos apellidos
    'visualId' : "RCC-2020-99999", #DEFAULT Esta variable ASÍ se manda! es la generadora de IDs
    'gender' : 'LNG_REFERENCE_DATA_CATEGORY_GENDER_FEMALE', #genero (revisar diccionario de variables)
    'age' : {'years': 22}, # edad del caso
    'dateOfReporting' : '2020-07-24T00:00:00.000Z', #Fecha de notificacion (usar ese formato para las fechas) 
    'outbreakId': "a44faf32-bf27-4b39-a4fb-b9fcf29acasd", #ID del brote
    'outcomeId' : "LNG_REFERENCE_DATA_CATEGORY_OUTCOME_ALIVE", #Condicion del paciente = Vivo (revisar diccionario de variables)
    'dateOfOutcome' : '2020-07-24T00:00:00.000Z', #Fecha de la condicion del paciente (puede ser la misma que la fecha de notificación)
    'classification' : "LNG_REFERENCE_DATA_CATEGORY_CASE_CLASSIFICATION_SUSPECT", #Clasificación epidemiológica = Sospechos (revisar diccionario de variables)
    'addresses': [{ # información relacionada a la direccion de vivienda de la persona              
      'typeId': 'LNG_REFERENCE_DATA_CATEGORY_ADDRESS_TYPE_USUAL_PLACE_OF_RESIDENCE', #DEFAULT Esta variable ASÍ se manda!
      'date' : '2020-07-24T00:00:00.000Z',#Fecha de notificacion (usar ese formato para las fechas)
      'locationId' : '27810132', #DMS ID relacionado a su vivienda (revisar base de datos de locationsID)
      'phoneNumber' : '5555 5555', #Numero de teléfono (si no tiene, enviar un string vacio '')
      # Las siguiente información de vivienda es OPCIONAL
      'city': 'zona', # zona en la que vive
      'addressLine1': 'Dirección', #Barrio + colonia + direccion
    }],


    #LOS SIGUIENTES CAMPOS SON OPCIONALES. AGREGAR SI ES POSIBLE


    "pregnancyStatus": "LNG_REFERENCE_DATA_CATEGORY_PREGNANCY_STATUS_YES_TRIMESTER_UNKNOWN", #Estado de embarazo (revisar diccionario de variables)
    'dateOfOnset': '2020-07-24T00:00:00.000Z', #Fecha de inicio de sintomas,
    'questionnaireAnswers': { # Dentro de esta variabla va la información de la ficha epidemiológica del MSPAS e información relacionada a los seguimientos de CASOS
      'Case_WhichForm': [{'value': 'Ficha Epidemiológica 1'}], #DEFAULT Esta variable ASÍ se manda! esta variable es necesaria para ingresar información en esta sección!
      'servicio_de_salud': [{'value': row['Servicio de salud']}], #El servicio de salud que captura el caso (e.g. Nombre del Hospital o Nombre de la institucion que interopera)
      'FE103no_de_ficha_de_notificacion': [{'value': row['No. Ficha Epidemiológica']}], # ID ficha de notificacíon
      "FE111grupo_cultural": [{"value": '1'}], #Grupo cultural (revisar diccionario de variables para ver correlativo)
      "FE112escolaridad": [{"value": "1"}], #Nivel escolar alcanzado (revisar diccionario de variabels para ver correlativo)
      "FE108documento_de_identificacion": [{"value": "1"}], #DEFAULT esta variable así se manda! Define que el documento de identificación es el DPI y el OBLIGATORIA para agregar DPI
      "FE10801numero_de_documento_cui": [{"value": "0000 00000 0001"}], #CUI (es obligatoria la variable anterior)
      "FE124tipo_de_vigilancia": [{"value": ["1"]}], #Define el tipo de vigilancia que se dara al caso ETI/IRAG (revisar diccionario de variables para ver correlativo)
      'FE113enfermedades_asociadas': [{'value': ['1', '4', '5', '9', '11', '14']}], #Enfermedades asociadas al caso. Pueden ser multiples por lo que se separan con comas "," (revisar diccionario de variables para ver correlativo)
      'FE11301especifique': [{'value': 'ENFERMEDAD1, ENFERMEDAD2'}], #Una de las opciones de enfermedades asociadas (variable anterior) es "otro" la cual se especifica en esta variable
      'FE114sintomas': [{'value': ['1', '5', '6', '10', '11', '14']}], #Sintomas del caso. Pueden ser multiples por lo que se separan con comas "," (revisar diccionario de variables para ver correlativo)
      'FE11401especifique': [{'value': 'especifique otro sintomas'}], #Una de las opciones de los sintomas (variable anterior) es "otro" la cual se especifica en esta variable
      'FE121se_tomo_una_muestra_respiratoria': [{"value": '1'}], #Si se tomo muestra respiratoria: SI/NO == 1/2. OBLIGATORIA para ingresar cualquier información relacionada a la prueba respiratoria (revisar diccionario de variables para ver correlativo)
      'FE12102fecha_y_hora_de_toma_de_la_muestra': [{"value": '2020-07-24T00:00:00.000Z'}],  #Fecha en que se tomo la muestra respiratorio (seguir el formato de fecha)
      'FE12101tipo_de_muestra': [{ "value": ['1']}], #Tipo de muestra respiratoria (revisar diccionario de variables)
      "FE12102especifique": [{"value": 'Otro tipo de muestra' }], #Una de las opciones de muestra respiratoria (variable anterior) es "otro" la cual se especifica en esta variable
      "FE12103resultado_de_la_muestra": [{"value": '1'}], #Virus que se detecto de la muestra respiratoria
      "FE125la_persona_es_un_trabajador_de_salud": [{"value": '1'}] #Si el caso es un trabajador de salud (en caso no lo sea no agregar este campo)
    }
  }
  ```
* Respuesta: La funcion responde con el modelo del caso ya guardado en Go.Data. De esta respuesta es importante guardar los IDs de los casos para poder hacerles cambios en el futuro. Por ejemplo agregar un nuevo seguimiento.
  ```python
  {
    ...
    "id": "062f3b7a-3fd0-42d9-928f-d6d50e239641" #ID del caso
    ...
  }
  ```

## Seguimientos de casos

Para el seguimiento de casos se  agregaran variables a la variable `questionnaireAnswers`. Los seguimiento pueden agregarse al mismo tiempo que se crea un nuevo caso (sección anterior) o modificando un caso ya existente.

* Funcion del API: PUT `/outbreaks/{id}/cases/{fk}`. Cambiar `{id}` por el ID del brote y `{fk}` por el ID del Caso.
* Modelo: este modelo puede variar mucho por la forma en que Go.Data generó el modelo. En el diccionario de variables están todas las posibles variables pero aca está la explicación de como funcionan (la x en el nombre de las variables representa el número de seguimiento).
  1. **estado_de_seguimiento_1**: Aca se especifica cual es el estado de seguimiento del caso ("1" = "Activo", "2" = "Recuperado", "3" = "Imposible de contactar", "4" = "Perdido", "5" = "No es posible dar seguimiento domiciliar", "6" = "Perdido por otra razón"). El caso permanecera en "1" ("Activo") hasta que se concluya el seguimiento. Cuando se concluya el seguimiento se pondra cualquier otro estado_de_seguimiento_1 segun el resultado del seguimiento.

  2. **seguimiento_x**: Estas variables definen si el seguimiento sí se logró o si no ("1" = "Si", "2" = "No").

  3. **fecha_sx_s**: Si el seguimiento_x resulta en <strong>Sí ("1")</strong> se utiliza esta variable para definir la fecha en la que se realizó el seguimiento.

  4. **fecha_sx_n**: Si el seguimiento_x resulta en <strong>No ("2")</strong> se utiliza esta variable para definir la fecha en la que se intento el seguimiento pero no se logró. <strong>Ojo: en el listado de variables la fecha_s16 hace referencia a la de ese seguimiento aunque no tenga la "n" al final.</strong>

  5. **por_que_sx**: En estas variables se especifica por qué no se logró hacer el seguimiento ("1" = "No respondió la llamada", "2" = "Respondió pero rechazo seguimiento", "3" = "No entró la llamada al número registrado", "4" = "Se inició seguimiento pero se perdió la comunicación", "5" = "No intentada/ no dió tiempo", "6" = "Número de telefono incorrecto", "7" = "Otro" ). En caso no se tienen implementadas las razones por las que no se realizo un seguimiento mandar el valor "7" = "Otro". <strong>Ojo. En el listado veras que para el seguimiento 1, 7, 9, 11 no se encuentra la "s" en la variable.</strong>

  6. **estado_de_seguimiento**: Esta es una variable que se debe agregar por defecto con valor "6". Con que se agregue con el primer seguimiento es suficiente, no debe volver a agregarse.

  Ejemplo: Primer seguimiento.
  ```python
  {
    'questionnaireAnswers': { # Dentro de esta variabla va la información de la ficha epidemiológica del MSPAS e información relacionada a los seguimientos de CASOS
      "estado_de_seguimiento": "6", #Default esta variable ASÍ se manda!
      "estado_de_seguimiento_1": "1", #Estado de seguimiento del caso (Revisar diccionario de variables para ver el correlativo)
      "seguimiento_1": "1", #Si se pudo o no realizar el seguimiento 1(Revisar diccionario de variables para ver el correlativo)
      "fecha_s1_s": '2020-07-24T00:00:00.000Z' #Fecha en que se realizo el primer seguimiento (usar este formato para las fechas)
    }
  }
  ```

  Ejemplo: Segundo seguimiento el cual no pudo realizarse.
  ```python
  {
    'questionnaireAnswers': { # Dentro de esta variabla va la información de la ficha epidemiológica del MSPAS e información relacionada a los seguimientos de CASOS
      "seguimiento_2": "2",  #Si se pudo o no realizar el seguimiento 2 (Revisar diccionario de variables para ver el correlativo)
      "por_que_s2": "3", #Por que no se pudo realizar el seguimiento 2 (Revisar diccionario de variables para ver el correlativo)
      "fecha_s2_n": '2020-07-24T00:00:00.000Z' #Fecha en que se intento realizar el segundo seguimiento (usar este formato para las fechas)
    }
  }
  ```

  Ejemplo: Concluir el seguimiento
  ```python
  {
    'questionnaireAnswers': { # Dentro de esta variabla va la información de la ficha epidemiológica del MSPAS e información relacionada a los seguimientos de CASOS
      "estado_de_seguimiento_1": "2" #Estado de seguimiento del caso (Revisar diccionario de variables para ver el correlativo)
    }
  }
  ```
* Respuesta: La funcion responde con el modelo entero del caso con los campos ya agregados.

## Modificar un Caso

Puede que algún campo haya sido modificado o agregado, por lo que tambien hay que actualizarlo en Go.Data

* Función del API: PUT `/outbreaks/{id}/cases/{fk}`. Cambiar `{id}` por el ID del brote y `{fk}` por el ID del Caso.
* Modelo: se utilizan los mismos campos mostrados en la seccion de Nuevo Caso y de Seguimientos.  
  Ejemplo: Agregar número de teléfono.
  ```python
  {
    'addresses': [{ # información relacionada a la direccion de vivienda de la persona              
      'phoneNumber' : '5555 5555', #Numero de teléfono 
    }]
  }
  ```
* Respuesta: La funcion responde con el modelo entero del caso con los campos ya modificados.

## Nuevo Contacto

Agregar un nuevo contacto es muy similar a agregar un caso. La unica diferencia es que todo contacto proviene de un caso. Por lo que a los contactos, luego de agregarlos hay que relacionarlos con el caso con el que tuvo contacto.

### Agregar el contacto

* Función del API: POST `/outbreaks/{id}/contacts`. Cambiar `{id}` por el ID del brote.
* Modelo: 
  ```python
  {
    "firstName": "nombres", #Colocar ambos nombres
    "lastName": "apellidos", #Colocar ambos apellidos
    "gender": "LNG_REFERENCE_DATA_CATEGORY_GENDER_MALE", #Genero (revisar diccionario de variables)
    "age": {"years": 32}, #Edad del caso
    "dateOfReporting": "2020-11-26T00:00:00.000Z", #Fecha de notificacion (usar ese formato para las fechas) 
    "riskLevel": "LNG_REFERENCE_DATA_CATEGORY_RISK_LEVEL_3_HIGH", #Nivel de riesgo (revisar diccionario de variables)
    "addresses": [{ #Información relacionada a la direccion de vivienda de la persona   
      "typeId": "LNG_REFERENCE_DATA_CATEGORY_ADDRESS_TYPE_USUAL_PLACE_OF_RESIDENCE", #DEFAULT Esta variable ASÍ se manda!
      "date": "2020-12-01T00:00:00.000Z", #Fecha de notificacion (usar ese formato para las fechas)
      'locationId' : '27810132', #DMS ID relacionado a su vivienda (revisar base de datos de locationsID)
      'phoneNumber' : '5555 5555', #Numero de teléfono (si no tiene, enviar un string vacio '')
      # La siguiente información de vivienda es OPCIONAL
      'city': 'zona', # zona en la que vive
      'addressLine1': 'Dirección', #Barrio + colonia + direccion
    }],
    "dateOfLastContact": "2020-11-22T00:00:00.000Z", #Fecha en la que el contacto tuvo el ultimo contacto con el caso. Si no se tiene la info poner la fecha de notificación. (usar ese formato para las fechas)
    # INFORMACION DE LOS SEGUIMIENTOS ES OBLIGATORIA
    "followUp": { #Informacion de los seguimientos del contacto
      "originalStartDate": "2020-11-23T00:00:00.000Z", #Fecha de inicio de seguimiento. Usualmente es el dia siguiente al de notificación (usar ese formato para las fechas). Si los seguimientos no empiezan en esa fecha no habrá problema, pero hay que darle un valor inicial.
      "startDate": "2020-11-23T00:00:00.000Z", #Fecha de inicio de seguimiento. Usualmente es el dia siguiente al de notificación (usar ese formato para las fechas). Si los seguimientos no empiezan en esa fecha no habrá problema, pero hay que darle un valor inicial.
      "status": "LNG_REFERENCE_DATA_CONTACT_FINAL_FOLLOW_UP_STATUS_TYPE_UNDER_FOLLOW_UP" #DEFAULT esta variable ASÍ se manda! Estado de seguimiento del contacto = "En seguimiento" (revisar diccionario de variables)
    },

    #LOS SIGUIENTES CAMPOS SON OPCIONALES. AGREGAR SI ES POSIBLE.

    "pregnancyStatus": "LNG_REFERENCE_DATA_CATEGORY_PREGNANCY_STATUS_NOT_PREGNANT" #Estado de embarazo (revisar diccionario de variables)
    
  }
  ```
* Respuesta: La funcion responde con el modelo del contacto ya guardado en Go.Data. De esta respuesta es importante guardar los IDs de los contactos para poder hacerles cambios en el futuro. Por ejemplo agregar un nuevo seguimiento o relacionarlos a un caso.
  ```python
  {
    ...
    "id": "062f3b7a-3fd0-42d9-928f-d6d50e239641" #ID del contacto
    ...
  }
  ```

### Relacionar un contacto a un caso

* Función del API: POST `/outbreaks/{id}/cases/{nk}/relationships`. Cambiar `{id}` por el ID del brote y `{nk}` por el **ID del caso** al que se relacionara el contacto.
* Modelo:
  ```python
  {
    "persons":[{
      "id":"ac52813f-27bf-451d-b171-b1asdf86c6e17" #ID del contacto
    }],
    "contactDate":"2020-12-15T00:00:00.000Z", #Fecha en la que el contacto tuvo el ultimo contacto con el caso. Si no se tiene la info poner la fecha de notificación. (usar ese formato para las fechas)
    "contactDateEstimated":false, #DEFAULT esta variable ASÍ se manda!
    "certaintyLevelId":"LNG_REFERENCE_DATA_CATEGORY_CERTAINTY_LEVEL_VIVE_EN_EL_MISMO_LUGAR", #Nivel de riesgo (revisar diccionario de variables)
    "people":[], #DEFAULT esta variable ASÍ se manda!
    "dateOfFirstContact":"2020-12-15T00:00:00.000Z" #Fecha en la que el contacto tuvo el primer contacto con el caso. Si no se tiene la info poner la fecha de ultimo contacto. (usar ese formato para las fechas)
  }
  ```
* Respuesta: La función responde con el modelo ya guardado en la base de datos. Con esto se finaliza el proceso de agregar un nuevo contacto.

## Seguimiento de contactos

Para los seguimientos de contactos, a diferencia que los casos, sí hay una sección especializada en seguimientos. Para agregar cada seguimiento se utiliza esta sección especializada aunque el estado de seguimiento se modifica en la información del contacto.

### Agregar un seguimiento

* Función del API: POST `/outbreaks/{id}/contacts/{nk}/follow-ups`. Cambiar `{id}` por el ID del brote y `{nk}` por el ID del contacto.
* Modelo:
  ```python
  {
    "date":"2020-12-15T00:00:00.000Z", #Fecha del seguimiento
    "targeted":true, #DEFAULT ests variable ASÍ se manda!
    "statusId":"LNG_REFERENCE_DATA_CONTACT_DAILY_FOLLOW_UP_STATUS_TYPE_SEEN_OK" #Resultado del seguimiento (revisar diccionario de variables)
  }
  ```
* Respuesta: La función responde con el modelo ya guardado en la base de datos.

### Finalizar seguimientos (modificar un contacto)

Cuando al contacto terminó sus seguimientos o se le dara de baja debemos modificarle su estado de seguimiento. Al crear un contacto instanciamos a todos los contactos como "Bajo seguimiento" y ahora es momento de modificarlo a "Seguimiento completado". Esta función puede utilizarse para modificar cualquier variable de un contacto.

* Función del API: PUT `/outbreaks/{id}/contacts/{fk}`. Cambiar `{id}` por el ID del brote y `{nk}` por el ID del contacto.
* Modelo:
  ```python
  {
    "followUp":{ #Informacion de los seguimientos del contacto
        "status":"LNG_REFERENCE_DATA_CONTACT_FINAL_FOLLOW_UP_STATUS_TYPE_FOLLOW_UP_COMPLETED" #DEFAULT esta variable ASÍ se manda! Estado de seguimiento del contacto = "Seguimiento completado" (revisar diccionario de variables)
    }
  }
  ```
* Respuesta: La funcion responde con el modelo entero del contacto con los campos ya modificados.

<!-- CONTACT -->
## Contacto

[Oliver Mazariegos](https://mazariegos.gt/) - olivera@mazariegos.gt

Repo Link: [https://github.com/Oliversinn/godata-guatemala](https://github.com/Oliversinn/godata-guatemala)

