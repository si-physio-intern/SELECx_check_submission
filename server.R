### Join SELECx by State v2 

library(shiny)
library(shinyFeedback)
library(tidyverse)
library(lubridate)
library(vroom)
library(tools)
library(readxl)
library(openxlsx)

# Functions ---------------------------------------------------------------

read_multi <- function(file_name,file_path){
    
    df <- tibble(ext = tools::file_ext(file_name), path = file_path)
    
    read <- function(ext,path){  
        
        switch(ext,
               csv = vroom::vroom(path, delim = ","),
               xls = readxl::read_excel(path),
               xlsx = readxl::read_excel(path),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
               
        )
    }
    
    data_ls <- df %>% pmap(read)
    
    names(data_ls) <- str_remove(file_name,"\\.[^\\.]+$") #remove .xxx (content after last dot)
    
    data_ls
    
}

clean_list <- function(list.raw){
    
    list.raw %>% 
        map(~select(.x,-Institution,-Department,
                    -starts_with("Q"),-starts_with("G"),-starts_with("Response"))) %>% 
        map(~rename(.x,Email="Email address" ,
                    Started="Started on",Time_taken = `Time taken`)) %>% 
        map(~unite(.x ,"First name","Surname",sep = " ",col = "Name")) %>% 
        map(~mutate(.x, State = as_factor(State), 
                    ID = str_extract(Email,"[:digit:]+") %>% as.character())) %>% 
        map(~filter(.x,!is.na(Email))) %>% 
        map(~select(.x,Name,ID,State))
    
}

get_unique_at <- function(list, var, na.rm = T){
    
    Uni <- list %>% 
        map(~distinct(.x, .data[[var]] )) %>% 
        flatten() %>% flatten_chr() %>% 
        unique() 
    
    if(na.rm == T){
        Uni %>%  
            na.omit() %>% as.character() # remove NAs
    }else{
        Uni
    }
}

encode_gen_list <- function(list.cleaned, 
                            text = c("Finished","In progress"), 
                            num = c(1,0)){
    
    encode <- function(list.cleaned, text, num){
        list.cleaned %>% 
            map(~mutate(.x,across(starts_with("S"), ~case_when(State == text ~ num))))
    }
    
    join_dept <- function(list){
        
        list %>% 
            reduce(full_join, by = c("ID","Name")) %>%
            rowwise() %>% 
            mutate(State_final = max(c_across(where(is.numeric)), na.rm = T)) %>% 
            group_by(Name, ID) %>% 
            summarise(State_final = max(State_final)) %>% ungroup()  
    }
    
    encode_2 <- map2(.x = text, .y = num , ~encode(list.cleaned, .x , .y))
    
    encode_2 %>% 
        transpose() %>% 
        map_depth(.depth = 1, ~join_dept(.x)) %>% 
        map(~rename(.x, State = State_final))
    
}

rename_list <- function(list,var){
    
    rename_2 <- function(list,no,var){
        
        list %>%
            pluck(no) %>% 
            rename_with( .cols = {{var}}, ~ names(list)[no]  )
        
    }
    
    
    seq_along(list) %>% 
        map(~rename_2(list, no = .x, var = {{var}})) %>% 
        set_names(names(list))
}


join_list <- function(list.renamed){
    
    list.renamed %>% 
        reduce(full_join, by = c("ID","Name")) %>% 
        rowwise() %>% 
        mutate(Finished_total = sum(c_across(where(is.numeric)),na.rm = T )) 
}

join_id <- function(ids, df, ... ){
    
    ids %>% 
        map_df(as.character) %>% 
        mutate(ID = str_extract(ID,"[:digit:]+")) %>% 
        select(ID,Name, ... ) %>% 
        full_join(df, by = "ID") %>% 
        rename(Name_from_ID = "Name.x", 
               Name_from_SELECx = "Name.y") %>% 
        relocate(... , .after = Name_from_SELECx) %>% 
        arrange(ID)
    
}

get_state_stat <- function(list.encoded, joined.df,
                           text =c("Finished","In progress"), num =c(1,0)){
    
    un_encode <- function(list.encoded, text, num){    
        
        list.encoded %>% 
            map(~count(.x, State)) %>% 
            rename_list(var = "n") %>% 
            reduce(full_join, by = "State") %>% 
            mutate(State = case_when(State == num ~ text)) %>% 
            filter(!is.na(State))
    }  
    
    l  <- map2(.x = text , .y = num , ~un_encode(list.encoded, .x , .y))
    
    state <- l %>% 
        bind_rows() %>% 
        mutate(across(-1, ~replace_na(.x,0))) %>% 
        pivot_longer(!State, names_to = "File", values_to = "n") %>% 
        pivot_wider(names_from = "State",  values_from = "n")
    
    get_no_rec <- function(joined.df){
        
        joined.df %>% 
            map_df( ~sum(is.na(.x)) ) %>% 
            pivot_longer(!c(ID,starts_with("Name")),
                         names_to = "File", values_to = "No_Record_in_SELECx") %>% 
            select(File,No_Record_in_SELECx) %>% 
            filter(File != "Finished_total")
        
    }
    
    no_rec <- get_no_rec(joined.df)
    
    full_join(state, no_rec, by = "File") %>% 
        rowwise() %>% 
        mutate(Total = sum(c_across(where(is.numeric)),na.rm = T )) %>% 
        mutate_if(is.numeric, as.integer)
    
}

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
    
    # Upload ------------------------------------------------------------------
    
    
    list.raw <- reactive({
        
        req(input$file)
        read_multi(file_name = input$file$name, file_path = input$file$datapath)
        
    })
    
    ids <- reactive({
        
        req(input$file_id) # Require - code wait until file uploaded
        
        ext <- tools::file_ext(input$file_id$name)
        switch(ext,
               csv = vroom::vroom(input$file_id$datapath, delim = ","),
               xls = readxl::read_excel(input$file_id$datapath),
               xlsx = readxl::read_excel(input$file_id$datapath),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
        )
    })
    
    proper_id <- reactive({  
        
        all(c("ID","Name") %in% colnames( ids() ) && all(str_detect(ids() %>% pull(ID),"[:digit:]+"))) 
        
    })
    
    
    observeEvent(input$file_id,
                 shinyFeedback::feedbackWarning(
                     "file_id", 
                     !proper_id(),
                     "Incorrect ID file specification"
                 )  
    )
    
    # Clean ---------------------------------------------------------------
    
    
    list.cleaned <- reactive({
        
        list.raw() %>% clean_list()
        
    })
    
    # Encode  ---------------------------------------------------------------
    
    check_string <- reactive({  
        
        if(input$check_enc == T){"show"}else{"not_show"}
        
    })
    
    observeEvent(input$check_enc,{
        updateTabsetPanel(session, "tab_enc", selected = check_string())
        
    })
    ## Get State  ---------------------------------------------------------------
    
    state <- reactive({
        
        list.raw() %>% get_unique_at(var = "State")
        
    })
    
    output$enc_disp <- renderUI({
        
        map(state(), ~ numericInput(.x, label = paste(.x , ":") ,value = NULL))
    })
    
    ## Get Encoding  ---------------------------------------------------------------
    
    encode_num <- reactive({
        map_dbl(state(), ~input[[.x]]) 
    })
    
    is_all.encode_num <- reactive({ 
        !any(is.na(encode_num()))
    })
    
    ## Encode list  ---------------------------------------------------------------
    
    list.encoded <- reactive({
        
        if(isTruthy(input$check_enc)){
            
            if(isTruthy(is_all.encode_num()) ){
                
                list.cleaned() %>% encode_gen_list(text = state(), num = encode_num())
            }else{
                
                list.cleaned() %>% encode_gen_list()  
            }
            
        }else{ list.cleaned() %>% encode_gen_list() }
        
        
    })
    
    # Rename list  ---------------------------------------------------------------
    
    list.renamed <- reactive({
        
        list.encoded() %>% rename_list(State)
        
    })
    
    # Join to single DF ---------------------------------------------------------------
    
    
    df.joined <- reactive({
        
        list.renamed() %>% join_list()
        
    })
    
    # Join with ids (+/- select more cols) ---------------------------------------------------------------
    
    id_cols <- reactive({ 
        
        req(proper_id())
        ids() %>% select(-ID,-Name) %>% colnames() })
    
    output$select <- renderUI({
        
        if(input$add_cols == T){
            selectInput("cols","Choose column",choices = id_cols(), multiple = TRUE)
        }
        
    }) 
    
    df.joined_2 <- reactive({
        
        if(( !isTruthy(input$file_id)) || ( !proper_id()) ){  
            
            df.joined()
            
        }else if(input$add_cols == T){
            
            join_id(ids = ids() ,df = df.joined(), input$cols)
            
        }else{
            join_id(ids = ids() ,df = df.joined())}
        
        
    })
    
    # Display table ---------------------------------------------------------------
    
    output$table <- renderDataTable(
        
        df.joined_2(), options = list(lengthMenu = c(5,10,20,50), pageLength = 5) 
        
    )
    
    # Missing names or no record ---------------------------------------------------------------
    
    filter_na <- reactive({
        
        df.joined_2() %>%   filter_at(vars(-ID), any_vars(is.na(.))) 
        
    })
    
    output$missing <- renderDataTable( 
        
        filter_na() ,options = list(lengthMenu = c(5,10,20,50), pageLength = 5)
        
    )
    
    # Filter student ---------------------------------------------------------------
    
    output$table_filter <- renderDataTable({
        
        req(input$filter)
        
        df.joined_2() %>% filter(Finished_total <= input$filter)
        
    }, options = list(lengthMenu = c(5,10,20,50), pageLength = 5))
    
    
    # Stat --------------------------------------------------------------------
    
    stat_df <- reactive({
        
        if(isTruthy(input$check_enc)){
            
            if(isTruthy(is_all.encode_num()) ){
                
                get_state_stat(list.encoded = list.encoded(), joined.df = df.joined_2(),
                               text = state(), num = encode_num())
                
            }else{
                
                get_state_stat(list.encoded = list.encoded(), joined.df = df.joined_2())
            }
            
        }else{ get_state_stat(list.encoded = list.encoded(), joined.df = df.joined_2())  }
        
        
    })
    
    output$stat <- renderTable({ stat_df() })
    
    
    # Download ---------------------------------------------------------------
    
    output$download <- downloadHandler(
        
        filename = function() {
            paste0("Combined_SELECx",".xlsx") #remove .xxx
        },
        content = function(file) {
            
            openxlsx::write.xlsx(df.joined_2(), file)
        }
    )
    
    output$download_miss <- downloadHandler(
        
        filename = function() {
            paste0("Missing",".xlsx") #remove .xxx
        },
        content = function(file) {
            
            openxlsx::write.xlsx(filter_na(), file)
        }
    )
    
    
    # output$raw <- renderPrint({ isTruthy(is_all.encode_num()) })
    # output$raw_2 <- renderPrint({ encode_num() })
    # output$raw_3 <- renderPrint({ df.joined() })
    
    
    
}



