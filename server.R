#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # these default values are passed along from global.R
    RV_defaults <- reactiveValues(
        n_inputs            = n_inputs,
        input_types         = input_types,
        isolated_input_sets = isolated_input_sets
    )
    
    # the default values sit there in case anything becomes NULL and needs
    # a value to stop the app falling over
    
    RV_ISO <- reactiveValues(
        n_inputs_max        = n_inputs,
        n_inputs            = n_inputs,
        input_types         = input_types,
        isolated_input_sets = isolated_input_sets,
        names               = list(),
        names_backup        = list()
    )
    
    
    # slider to determine how many UIs
    output$UI_Nuis_slider <- renderUI({
        
        sliderInput(
            inputId = "UI_n_inputs",
            label = "Number of sets of inputs",
            min = 0,
            max = 20,
            value = 5,
            step = 1,
            post = " set(s)",
            width = "100%"
        )
        
    })
    
    
    # thing to record the highest n_inputs has ever been
    observeEvent(input$UI_updateInputSet,{
        n_now <- isolate(input$UI_n_inputs)
        
        if (RV_ISO$n_inputs_max <= n_now) {
            RV_ISO$n_inputs_max <- n_now
        }
    },ignoreNULL = TRUE)
    
    # so, whenever the max changes, populate it with the names, so that there is always
    # a backup containing all of the names no matter what happens. this one is quite reactive
    # but is happening in the back end, storing all the names and types as they're updated, but
    # only responding to the maximum amount of them going up, so not to reducions in the number
    # of input sets!
    observe({
        
        req(!is.null(input$UI_n_inputs))
            
        RV_ISO$names_backup <- lapply(1:input$UI_n_inputs, function(this_input) {
            isolate(input[[paste0("UI_inputset_name", this_input)]])
        })
        
        # also store the types whenever the max goes up, or the types change
        RV_ISO$input_types <- unlist(lapply(1:input$UI_n_inputs, function(this_input){
            isolate(input[[paste0("UI_inputset_type",this_input)]])
        }))
        
    })
    
    
    observeEvent(input$UI_updateInputSet,{
        RV_ISO$input_types <- unlist(lapply(1:RV_ISO$n_inputs_max, function(this_input){
            isolate(input[[paste0("UI_inputset_type",this_input)]])
        }))
    })
    
    output$debugout <- renderPrint({
        req(!is.null(RV_ISO))
        
        print(reactiveValuesToList(RV_ISO))
        
    })
    
    
        # now a bunch of observeevents to isolate input values from the UI side
    
    observeEvent(input$UI_updateInputSet,{RV_ISO$n_inputs <- isolate(input$UI_n_inputs)},ignoreNULL = TRUE)
    
    # names: this should rely on n_inputs_max and not n_inputs from RV_ISO. this keeps it
    names_iso <- reactive({
        req(!is.null(input$UI_n_inputs))
        lapply(1:input$UI_n_inputs, function(this_input) {
            isolate(input[[paste0("UI_inputset_name", this_input)]])
        })
    })
    observeEvent(names_iso(),{
        RV_ISO$names <- isolate(names_iso())
    })
    
    
    # inputs/buttons to control the type and name of each input set
    
    output$UI_name_and_type <- renderUI({

        req(!is.null(input$UI_n_inputs))
        req(length(RV_ISO$names_backup) > 0)
        
        print(input$UI_n_inputs)
        print(RV_ISO$names_backup)
        print(RV_ISO$input_types)
        
        Func_Make_L1_UI(
            n_uis = input$UI_n_inputs,
            names = RV_ISO$names_backup,
            types = RV_ISO$input_types
        )
    })

    
    output$UI_input_set <- renderUI({
        Func_Make_L2_UI(
            n_inputs = input$UI_n_inputs, 
            input_types = RV_ISO$input_types, 
            isolated_input_sets = RV_ISO$isolated_input_sets
        )
    })
    
    
    
    output$global_test <- renderPrint({
        print(x)
    })
    
})
