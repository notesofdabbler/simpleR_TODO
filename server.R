library(shiny)
library(dplyr)

tododf = read.csv("db/todolist.csv", stringsAsFactors = FALSE)


function(input, output){
  
  todo_filt = reactive({
    todo_filt = tododf
    todo_filt$desckeywd = paste0(todo_filt$desc,";;",todo_filt$keywd)
    if(input$filtCrit != ""){
      rowidx = grepl(input$filtCrit, todo_filt$desckeywd)
      todo_filt = todo_filt[rowidx,]
    }
    
    tododf_l1 = tododf %>% filter(l2_key == 0, l3_key == 0)
    todofilt_l1 = todo_filt %>% group_by(l1_key) %>% summarize(l2_key_min = min(l2_key)) %>% filter(l2_key_min > 0)
    tododf_l1 = inner_join(tododf_l1, todofilt_l1, by = "l1_key")
    
    if(nrow(tododf_l1) > 0){
      todo_filt = bind_rows(tododf_l1, todo_filt)
    }

    return(list(todo_filt = todo_filt))
  })
  
#  observeEvent(input$gettodo,{
    output$todolist = renderUI({
      
      todo_filt = todo_filt()$todo_filt
      numtodo = nrow(todo_filt)
      
      todo_filt = todo_filt %>% arrange(l1_key, l2_key, l3_key)
     
      todolist = tagList()
      for(i in 1:numtodo){
        l1_key = todo_filt$l1_key[i]
        l2_key = todo_filt$l2_key[i]
        desc = todo_filt$desc[i]
        if(l2_key == 0){
          todolist[[i]] = p(desc, br(),actionLink(paste0("todolist_",l1_key,"_",l2_key),"Edit"))
        } else {
          todolist[[i]] = p(style = "margin-left: 40px",desc, br(),actionLink(paste0("todolist_",l1_key,"_",l2_key),"Edit"))
        }
      }
      
      return(todolist)
  })

#  })
  
  observeEvent(input$addnewtask,{
    output$addedit_task = renderUI({
      tagList(
        textInput("taskdesc","Enter Task"),
        textInput("taskkeywd","Enter Task Keywords")
      )
    })
    
  })
  
  
  observeEvent(todo_filt(),{
    
    todo_filt = todo_filt()$todo_filt
    #print(todo_filt)
    numtodo = nrow(todo_filt)
    todo_filt = todo_filt %>% arrange(l1_key, l2_key, l3_key)
    
    pkey = paste0(todo_filt$l1_key,"_",todo_filt$l2_key)
      
    for(i in 1:numtodo){
        local({
          my_i = i

          observeEvent(input[[paste0("todolist_",pkey[my_i])]],{
             output$addedit_task = renderUI({
               tagList(
                 textInput("taskdesc","Enter Task",value = todo_filt$desc[my_i]),
                 textInput("taskkeywd","Enter Task Keywords",value = todo_filt$keywd[my_i])
               )
             })
             
           })
        })
     }

  })
  
    
   observeEvent(input$updatetask,{
      l1_key = max(tododf$l1_key) + 1
      desc = input$taskdesc
      keywd = input$taskkeywd
      todonew = data.frame(l1_key, desc = desc, keywd = keywd)
      tododf = rbind(tododf, todonew)
      write.csv(tododf,"db/todolist.csv",row.names = FALSE)
    })
    
    

  

}