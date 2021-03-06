library(shiny)
library(dplyr)
library(stringr)

tododf = read.csv("db/todolist.csv", stringsAsFactors = FALSE)


function(input, output){
  
  rvs = reactiveValues(tododf = tododf, todo_filt = NULL, currKey = NULL)
  

  todo_filt = eventReactive(input$gettodo,{
    todo_filt = rvs$tododf
    todo_filt$desckeywd = paste0(todo_filt$desc,";;",todo_filt$keywd)
    filtCrit = isolate(input$filtCrit)
    if(filtCrit != ""){
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
  
    output$todolist = renderUI({
      
      todo_filt = todo_filt()$todo_filt
      numtodo = nrow(todo_filt)
      
      todo_filt = todo_filt %>% arrange(l1_key, l2_key, l3_key)
     
      todolist = tagList()
      for(i in 1:numtodo){
        l1_key = todo_filt$l1_key[i]
        l2_key = todo_filt$l2_key[i]
        l3_key = todo_filt$l3_key[i]
        desc = todo_filt$desc[i]
        keywd = todo_filt$keywd[i]
        
        if(grepl("pr:1",keywd)){
          stylestr = "color:red"
        } else if (grepl("pr:2",keywd)){
          stylestr = "color:green"
        } else if (grepl("pr:3",keywd)){
          stylestr = "color:blue"
        } else {
          stylestr = ""
        }
        
        showLvl = isolate(input$showLvl)
        
        if(l2_key == 0){
          todolist[[i]] = p(style = stylestr, desc," ; [", keywd,"]", br(),actionLink(paste0("edit_",l1_key,"_",l2_key),"EditTask"),"  ",actionLink(paste0("addsub_",l1_key,"_",l2_key),"AddSubtask"))
        } else if (l3_key == 0 & showLvl == "1 and 2") {
          todolist[[i]] = p(style = paste0("margin-left: 40px; ",stylestr),desc, " ; [", keywd,"]", br(),actionLink(paste0("edit_",l1_key,"_",l2_key),"EditSubtask")," ",actionLink(paste0("details_",l1_key,"_",l2_key),"ViewDetails"))
        }
      }
      
      return(todolist)
  })


  
  observeEvent(input$addnewtask,{
    rvs$currKey = NULL
    output$addedit_task = renderUI({
      tagList(
        textAreaInput("taskdesc","Enter Task",height = '100px',width = '500px'),
        textInput("taskkeywd","Enter Task Keywords")
      )
    })
    
  })
  
  
  observeEvent(todo_filt(),{
    
    todo_filt = todo_filt()$todo_filt
    
    numtodo = nrow(todo_filt)
    todo_filt = todo_filt %>% arrange(l1_key, l2_key, l3_key)
    
    pkey = paste0(todo_filt$l1_key,"_",todo_filt$l2_key)
      
    for(i in 1:numtodo){
        local({
          my_i = i

          observeEvent(input[[paste0("edit_",pkey[my_i])]],{
             rvs$currKey = pkey[my_i]
             output$addedit_task = renderUI({
               tagList(
                 textAreaInput("taskdesc","Enter Task",value = todo_filt$desc[my_i],height = '100px',width = '500px'),
                 textInput("taskkeywd","Enter Task Keywords",value = todo_filt$keywd[my_i])
               )
             })
             
           })
          
          if(todo_filt$l2_key[my_i] == 0){
            observeEvent(input[[paste0("addsub_",pkey[my_i])]],{
              currl1_key = todo_filt$l1_key[my_i]
              currl2_key = max(todo_filt$l2_key[todo_filt$l1_key == currl1_key])+1
              rvs$currKey = paste0(currl1_key,"_",currl2_key)
              output$addedit_task = renderUI({
                tagList(
                  textAreaInput("taskdesc","Enter Task",height = '100px', width = '500px'),
                  textInput("taskkeywd","Enter Task Keywords")
                )
              })
            })
          }
          
          if(todo_filt$l2_key[my_i] > 0){
            observeEvent(input[[paste0("details_",pkey[my_i])]],{
              currl1_key = todo_filt$l1_key[my_i]
              currl2_key = todo_filt$l2_key[my_i]
              rvs$currKey = paste0(currl1_key,"_",currl2_key)
              
              output$viewDetails = renderUI({
                detailsdf = todo_filt %>% filter(l1_key == currl1_key, l2_key == currl2_key)
                #print(detailsdf)
                detailsList = tagList()
                for(k in 1:nrow(detailsdf)){
                  detailsList[[k]] = p(detailsdf$desc[k])
                  #print(detailsList[[k]])
                }
                detailsList[[k+1]] = actionLink("addDetails","Add Details")
                return(detailsList)
              })
            })
            
            observeEvent("addDetails",{
              output$addDetailsUI = renderUI({
                tagList(
                  textAreaInput("addDetailsTxt","Description",height = '100px',width = '500px'),
                  actionButton("addDetailsdb","Add Details")
                )
              })
            })
          }
          
        })
     }

  })
  
  
   observeEvent(input$updatetask,{
     
      desc = input$taskdesc
      keywd = input$taskkeywd
      
      tododf = rvs$tododf
      
      if(is.null(rvs$currKey)){
        l1_key = max(tododf$l1_key) + 1
        l2_key = 0
        l3_key = 0
        todonew = data.frame(l1_key, l2_key, l3_key, desc = desc, keywd = keywd)
        tododf = rbind(tododf, todonew)
        
      } else {
        
        currKey = rvs$currKey
        l1_key = as.numeric(str_split(currKey,"_")[[1]][1])
        l2_key = as.numeric(str_split(currKey,"_")[[1]][2])
        l3_key = 0
        
        pKey = paste0(tododf$l1_key,"_",tododf$l2_key)
        idx = which(pKey == currKey)
        
        if(length(idx) == 0){
          todonew = data.frame(l1_key, l2_key, l3_key, desc = desc, keywd = keywd)
          tododf = rbind(tododf, todonew)
        } else {
          tododf$desc[idx] = desc
          tododf$keywd[idx] = keywd        
        }
      }

      rvs$tododf = tododf

      write.csv(rvs$tododf,"db/todolist.csv",row.names = FALSE)
    })
   
   observeEvent(input$addDetailsdb,{
     currKey = rvs$currKey
     tododf = rvs$tododf
     
     currl1_key = as.numeric(str_split(currKey,"_")[[1]][1])
     currl2_key = as.numeric(str_split(currKey,"_")[[1]][2])
     currl3_key = max(tododf$l3_key[tododf$l1_key == currl1_key & tododf$l2_key == currl2_key])+1

     desc = input$addDetailsTxt
     keywd = ""

     todonew = data.frame(l1_key = currl1_key, l2_key = currl2_key, l3_key = currl3_key, desc = desc, keywd = keywd)
     tododf = rbind(tododf, todonew)

     rvs$tododf = tododf

     write.csv(rvs$tododf,"db/todolist.csv",row.names = FALSE)

   })
    
    

  

}