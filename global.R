# global.R
# 
# 


global_test_text <- "global.R is working"



# globally available objects ----------------------------------------------

# EXAMPLES

# dummy inputs for 5 uis: also works as the default values on boot!
# A reactivevalues object is populated with this data at boot. it is called
# RV_defaults

n_inputs <- 5
input_types <- c("A", "B", "C", "B", "A")
isolated_input_sets <- list(
  inputs_1 = list(
    logicals = list(),
    numerics = list(
      A_numb1 = 1,
      A_numb2 = 5,
      A_numb3 = 10,
      A_numb4 = 20,
      A_numb5 = 50
    ),
    pickers = list(
      A_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      A_pick2 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      ),
      A_pick3 = list(
        selected = "option #4",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_2 = list(
    logicals = list(),
    numerics = list(B_numb1 = 0, B_numb2 = 1),
    pickers = list(
      B_pick1 = list(
        selected = "option #3",
        choices = paste0("option #",1:5)
      ),
      B_pick2 = list(
        selected = "option #4",
        choices = paste0("option #",1:5)
      ),
      B_pick3 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_3 = list(
    logicals = list(C_switch1 = TRUE, C_switch2 = FALSE),
    numerics = list(C_numb1 = 0, C_numb2 = 1),
    pickers = list(
      C_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      C_pick2 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      ),
      C_pick3 = list(
        selected = "option #5",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_4 = list(
    logicals = list(),
    numerics = list(B_numb1 = 0, B_numb2 = 1),
    pickers = list(
      B_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      B_pick2 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      )
    )
  ),
  inputs_5 = list(
    logicals = list(),
    numerics = list(
      A_numb1 = 1,
      A_numb2 = 5,
      A_numb3 = 10,
      A_numb4 = 20,
      A_numb5 = 50
    ),
    pickers = list(
      A_pick1 = list(
        selected = "option #1",
        choices = paste0("option #",1:5)
      ),
      A_pick2 = list(
        selected = "option #2",
        choices = paste0("option #",1:5)
      ),
      A_pick3 = list(
        selected = "option #4",
        choices = paste0("option #",1:5)
      )
    )
  )
)

# To test:
# Func_Make_L2_UI_tab(
#   n = 3,
#   type = "C",
#   input_set = isolated_input_sets$inputs_3
# )




# Functions making UI elements --------------------------------------------

# level 1 UI creates the name and type of each level 2 UI. it is reasonably simple logically

Func_Make_L1_UI <- function(n_uis, names, types) {
  lapply(1:n_uis, function(this_ui) {
    fluidRow(
      width = 12,
      column(
        3,
        textInputIcon(
          inputId = paste0("UI_inputset_name",this_ui),
          label = NULL,
          placeholder = "Insert a name for this input set",
          value = names[this_ui],
          icon = icon("signature"),
          size = "sm",
          width = "100%"
        )
      ),
      column(
        9,
        radioGroupButtons(
          inputId = paste0("UI_inputset_type",this_ui),
          label = NULL,
          choices = c("A", "B", "C"),
          selected = types[this_ui],
          width = "100%",
          individual = FALSE,
          size = "sm",
          justified = TRUE,
          
        )
      )
    )
  })
}



# Makes a set of UIs using a number and a list of typings. This is the level 2 UI.
# The level 1 UI determines the name and type of each level 2 UI

Func_Make_L2_UI <- function(n_inputs, input_types, isolated_input_sets = NULL) {
  
  require(shinydashboard)
  require(shinyWidgets)
  
  # generate some tab names for simplicity and reduction of repitition
  
  tab_names <- paste0("tab #", 1:n_inputs)
  
  # depending on whether there are any isolated inputs or not, generate
  # a list of length 1:n_inputs containing default values
  
  isolated_input_sets <- lapply(1:n_inputs, function(this_input_set) {
    if (length(isolated_input_sets) < n_inputs) {
      Func_blank_input_set(type = "A")
    } else {
      isolated_input_sets[[this_input_set]]
    }
  })
  
  
  # generate the UI elements. note that they get put into containers
  # when they get put into tabs. All that needs to go below is the
  # UI elements themselves
  
  
  tab_content <- lapply(1:n_inputs, function(This_ui) {
    # print(This_ui)
    # print(input_types[This_ui])
    # print(isolated_input_sets[[This_ui]])
    Func_Make_L2_UI_tab(
      n         = This_ui,
      type      = input_types[This_ui],
      input_set = isolated_input_sets[[This_ui]]
    )
  })
  
  # put all of the UIs in a tab to go in the tab box
  
  final_tabs <- lapply(1:n_inputs, function(This_ui) {
    fluidRow(
      width = 12,
      column(
        12,
        tab_content[[This_ui]]
      )
    )
  })
  
  
  final_tabs
  
}




# ~ Sub-functions creating UI components ----------------------------------

Func_blank_input_set <- function(type) {
  if (type == "A") {
    list(
      numerics = list(
        A_numb1 = 1,
        A_numb2 = 5,
        A_numb3 = 10,
        A_numb4 = 20,
        A_numb5 = 50
      ),
      pickers = list(
        A_pick1 = "option #1",
        A_pick2 = "option #2",
        A_pick3 = "option #4"
      )
    )
  } else if (type == "B") {
    list(
      numerics = list(
        B_numb1 = 0,
        B_numb2 = 1
      ),
      pickers = list(
        B_pick1 = "option #3",
        B_pick2 = "option #4",
        B_pick3 = "option #2"
      )
    )
  } else if (type == "C") {
    list(
      logicals = list(
        C_switch1 = TRUE,
        C_switch2 = FALSE
      ),
      numerics = list(
        C_numb1 = 0,
        C_numb2 = 1
      ),
      pickers = list(
        C_pick1 = "option #3",
        C_pick2 = "option #4",
        C_pick3 = "option #2"
      )
    )
  } else {
    list(
      logicals = list(
        D_switch1 = FALSE,
        D_switch2 = TRUE
      ),
      numerics = list(
        D_numb1 = 1,
        D_numb2 = 15
      ),
      pickers = list(
        D_pick1 = "option #1",
        D_pick2 = "option #1"
      )
    ) 
  }
}


# function to make the "level 2" UI: takes the input set for the corresponding type
# of UI. The level 1 UI controls the types of each input set

Func_Make_L2_UI_tab <- function(n, type, input_set) {
  
  # First, pull out all of the required inputs. Create the UI elements
  
  logicals <- input_set$logicals
  numerics <- input_set$numerics
  pickers  <- input_set$pickers
  
  # for each type of input - generate the appropriate inputs, with the appropriate names
  
  if (length(logicals) > 0) {
    ui_logicals <- lapply(1:length(logicals), function(this_logical) {
      materialSwitch(
        inputId = paste0("UI_set_",n,"_type_",type,"_logical_",this_logical),
        label = paste0("UI_set_",n,"_type_",type,"_logical_",this_logical),
        value = logicals[[paste0(type,"_switch",this_logical)]]
      )
    })
  }
  if (length(numerics) > 0) {
    ui_numerics <- lapply(1:length(numerics), function(this_numeric) {
      numericInputIcon(
        inputId = paste0("UI_set_",n,"_type_",type,"_numeric_",this_numeric),
        label = paste0("UI_set_",n,"_type_",type,"_numeric_",this_numeric),
        value = numerics[[paste0(type,"_numb",this_numeric)]],
        size = "sm",
        icon = icon("calculator"),
        width = "100%"
      )
    })
  }
  if (length(pickers) > 0) {
    ui_pickers <- lapply(1:length(pickers), function(this_picker) {
      pickerInput(
        inputId = paste0("UI_set_",n,"_type_",type,"_picker_",this_picker),
        label = paste0("UI_set_",n,"_type_",type,"_picker_",this_picker),
        choices = pickers[[paste0(type,"_pick",this_picker)]][["choices"]],
        selected = pickers[[paste0(type,"_pick",this_picker)]][["selected"]],
        width = "100%"
      )
    })
  }
  
  
  # Now organise those UI elements according to the UI type
  
  if (type == "A") {
    
    UI <- box(
      title = paste0("Input set ", n, ", type ", type),
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "primary",
      
      # Type A has 5 numerics, 3 pickers. These should be in a box with primary status.
      fluidRow(
        width = 12,
        h3("Type A"),
        column(
          6,
          tagList(
            ui_numerics[1:3]
          )
        ),
        column(
          6,
          tagList(
            ui_numerics[4:5]
          )
        )
      ),
      fluidRow(
        width = 12,
        column(
          12,
          ui_pickers
        )
      )
    )
    return(UI)
  }
  
  if (type == "B") {
    
    # 2 numerics 3 pickers, not in a box
    UI <- div(
      h3("Type B"),
      fluidRow(
        width = 12,
        column(6,ui_numerics[[1]]),
        column(6,ui_numerics[[2]])
      ),
      fluidRow(
        width = 12,
        column(
          12,
          ui_pickers
        )
      )
    )
    
    return(UI)
  }
  
  if (type == "C") {
    
    # logicals numerics  pickers 
    # 2        2        3 
    
    # in a box
    
    UI <- box(
      title = paste0("Input set ", n, ", type ", type),
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "info",
      h3("Type C"),
      fluidRow(
        width = 12,
        column(6,ui_logicals[[1]]),
        column(6,ui_logicals[[2]]),
      ),
      fluidRow(
        width = 12,
        splitLayout(ui_numerics)
      ),
      verticalLayout(ui_pickers,fluid = TRUE)
      
    )
    
    return(UI)
  }
  
}

# this_type <- A
# n_logical <- 0
# n_numeric <- 5
# n_picker  <- 3


Func_get_inputs <- function(set, type) {

  # number of each type of input
  if (type == "A") {
    n <- list(
      logical = 0,
      numeric = 5,
      picker  = 3
    )
  } else if (type == "B") {
    n <- list(
      logical = 0,
      numeric = 2,
      picker  = 3
    )
  } else {
    n <- list(
      logical = 2,
      numeric = 2,
      picker  = 3
    )
  }
  
  # logicals
  if (n_logical == 0) {logicals <- list()} else {
    nams_logicals <- paste0("UI_set_",set,"_type_",type,"_logical_",1:n_logical)
    logicals <- lapply(1:n_logical, function(this_logical){
      isolate(input[[nams_logicals[this_logical]]])
    })
    names(logicals) <- paste0(type,"_switch",1:n_logical)
  }
  
  # numerics
  if (n_numeric == 0) {numerics <- list()} else {
    nams_numerics <- paste0("UI_set_",set,"_type_",type,"_numeric_",1:n_numeric)
    numerics <- lapply(1:n_numeric, function(this_numeric){
      isolate(input[[nams_numerics[this_numeric]]])
    })
    names(numerics) <- paste0(type,"_numb",1:n_numeric)
  }
  
  # pickers
  if (n_picker == 0) {pickers <- list()} else {
    nams_pickers <- paste0("UI_set_",set,"_type_",type,"_picker_",1:n_picker)
    pickers <- lapply(1:n_picker, function(this_picker){
      isolate(input[[nams_pickers[this_picker]]])
    })
    names(pickers) <- paste0(type,"_pick",1:n_picker)
  }
  
  
  return(list(
    logicals = logicals,
    numerics = numerics,
    pickers = pickers
  ))
  
}
