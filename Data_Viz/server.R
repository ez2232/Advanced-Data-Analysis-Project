
library(shiny)
library(igraph)


setwd('C:/Users/Jack/Desktop/Columbia Masters/Fall 2016 Courses/Advanced Data Analysis/Group Project/Advanced Data Analysis Project')

#preparing edges and aliases data
edges_1 = read.csv('Hillary_edges.csv'); edges_1 = edges_1[!is.na(edges_1$from),]
persons = read.csv('Aliases.csv'); persons = persons[!duplicated(persons[,3]),]

#preparing data from Zoe by adding names to ids
cluster_output1 = read.csv('Community_assignment.csv'); cluster_output1 = cluster_output1[,-1]
cluster_output1 = merge(cluster_output1, persons, by.x = "Person.ID", by.y = 'PersonId', all.x = TRUE, all.y = FALSE)


#preparing data from Daitong by adding names to ids
cluster_output2 = read.csv('top10_clustering.csv', sep = ",", header = TRUE)
names(cluster_output2)[2] = "Person.ID"
names(cluster_output2)[1] = 'name'
cluster_output2 = merge(cluster_output2, persons, by.x = "Person.ID", by.y = 'PersonId', all.x = TRUE, all.y = FALSE)




#preparing summary data from palmer
summary_dat = read.table("summary.csv", sep=",", fill=T, quote='\"', row.names=NULL)


shinyServer(function(input, output) {

	
  #plotting zoe and palmer's work
  output$network1 <- renderVisNetwork({
  	
    cluster_output1_restrict = cluster_output1[cluster_output1$Community.ID == input$Group,]
    edges_1_restrict = edges_1[(is.element(edges_1$from, cluster_output1_restrict$Person.ID) & 
    					  	is.element(edges_1$to, cluster_output1_restrict$Person.ID)), ]
  	
    #defining the graph nodes and edges
    nodes <- data.frame(
    				id = cluster_output1_restrict$Person.ID, 
    				label = cluster_output1_restrict$Alias,
    				value = rep(1, dim(cluster_output1_restrict)[1]),
    				group = cluster_output1_restrict$Community.ID
    				# level = c(rep(1, floor(input$Nodes/2)), rep(2, input$Nodes - floor(input$Nodes/2)))
    )

    
    edges <- data.frame(
    				from = unlist(edges_1_restrict$from),
    				to = unlist(edges_1_restrict$to), 
    				title = paste("Edge", 1:dim(edges_1_restrict)[1]),
    				length = rep(200, dim(edges_1_restrict)[1]),
    				width = unlist(edges_1_restrict$weight)
    )
    
    #graph visualization and option
    visNetwork(nodes, edges, main = "Social Network 1", height = "800px", width = "100%") %>%
    visIgraphLayout(layout = input$Layout) %>%
    visPhysics(stabilization = FALSE) %>%
    # visHierarchicalLayout() %>%
    visLayout(improvedLayout = TRUE) %>%
    visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE,
    			hoverConnectedEdges = TRUE, selectable = TRUE) %>%
    visEdges(arrows = 'from', smooth = FALSE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE)
    
  })
  
  
  
  #plotting Daitong's and Yihe's work
  output$network2 <- renderVisNetwork({
  	
    #defining the graph nodes and edges
    edges_1_restrict = edges_1[(is.element(edges_1$from, cluster_output2$Person.ID) & 
    					  	is.element(edges_1$to, cluster_output2$Person.ID)), ]
    
    nodes <- data.frame(
    				id = cluster_output2$Person.ID, 
    				label = cluster_output2$name,
    				value = rep(1, dim(cluster_output2)[1])
    				# level = c(rep(1, floor(input$Nodes/2)), rep(2, input$Nodes - floor(input$Nodes/2)))
    )
    
    if(input$N_clusters == 2){
    		nodes$group = group = cluster_output2$X2.groups
    } else if (input$N_clusters == 3){
    		nodes$group = group = cluster_output2$X3.groups
    } else {
    		
    }

    
    edges <- data.frame(
    				from = unlist(edges_1_restrict$from),
    				to = unlist(edges_1_restrict$to), 
    				title = paste("Edge", 1:dim(edges_1_restrict)[1]),
    				length = rep(200, dim(edges_1_restrict)[1]),
    				width = unlist(edges_1_restrict$weight)
    )
    
    #graph visualization and options
    visNetwork(nodes, edges, main = "Social Network 1", height = "800px", width = "100%") %>%
    visIgraphLayout(layout = input$Layout2) %>%
    visPhysics(stabilization = FALSE) %>%
    # visHierarchicalLayout() %>%
    visLayout(improvedLayout = TRUE) %>%
    visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE,
    			hoverConnectedEdges = TRUE, selectable = TRUE) %>%
    visEdges(arrows = 'from', smooth = FALSE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE)
    
  })
  
  
})
