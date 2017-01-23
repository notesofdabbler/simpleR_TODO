library(shiny)

tododf = read.csv("db/todolist.csv", stringsAsFactors = FALSE)


function(input, output){
  
  todo_filt = reactive({
    todo_filt = tododf
    todo_filt$desckeywd = paste0(todo_filt$desc,";;",todo_filt$keywd)
    if(input$filtCrit != ""){
      rowidx = grepl(input$filtCrit, todo_filt$desckeywd)
      todo_filt = todo_filt[rowidx,]
    }


    return(list(todo_filt = todo_filt))
  })
  
#  observeEvent(input$gettodo,{
    output$todolist = renderUI({
      
      todo_filt = todo_filt()$todo_filt
      
      numtodo = nrow(todo_filt)
      todolist = tagList()
      for(i in 1:numtodo){
        todolist[[i]] = p(todo_filt$desc[i], br(),actionLink(paste0("todolist",todo_filt$l1_key[i]),"Edit"))
      }
      
      #print(todolist)
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
    print(todo_filt)

    numtodo = nrow(todo_filt)
    pkey = todo_filt$l1_key
      
    for(i in 1:numtodo){
        local({
          my_i = i

          observeEvent(input[[paste0("todolist",pkey[my_i])]],{
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