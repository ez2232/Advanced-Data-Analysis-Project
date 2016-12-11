
library(shiny)
library(igraph)


#preparing edges, aliases, and summary data
edges_1 = read.csv('Hillary_edges.csv'); edges_1 = edges_1[!is.na(edges_1$from),c(1,2)]
edges_1 = edges_1[!duplicated(edges_1),]
aliases = read.csv('Aliases.csv'); aliases = aliases[!duplicated(aliases[,3]),]

#summarizing the summary data for each sender/receiver pair
# unclean.text.and.summs=unclean.text.and.summs[(!is.na(unclean.text.and.summs$SenderPersonId) & 
#                                                 !is.na(unclean.text.and.summs$ReceiverPersonId)),c(3,5,6)]
# unclean.text.and.summs$sender_receiver = paste(unclean.text.and.summs$SenderPersonId, unclean.text.and.summs$ReceiverPersonId)
# unique_pairs = unclean.text.and.summs[,c(1,2,4)]; unique_pairs = unique_pairs[!duplicated(unique_pairs),]
# unique_pairs$summary = unclean.text.and.summs$Summary[[1]]

# n = dim(unique_pairs)[1]
# for (row_i in c(1:n)){
#   which_rows = (unclean.text.and.summs$sender_receiver == unique_pairs[row_i,3])
#   combined_text = paste(unclean.text.and.summs$Summary[which_rows], collapse = ' ')
#   unique_pairs$summary[row_i] = if(length(summarize.document(combined_text, top.k=1)) == 0){
#     ''
#   } else {
#     summarize.document(combined_text, top.k=1)
#   }
# 
#   print(row_i)
# }

#preparing data from Zoe by adding names to ids
cluster_output1 = read.csv('Community_assignment.csv'); cluster_output1 = cluster_output1[,-1]
cluster_output1 = merge(cluster_output1, aliases, by.x = "Person.ID", by.y = 'PersonId', all.x = TRUE, all.y = FALSE)


#preparing data from Daitong by adding names to ids
cluster_output2 = read.csv('top10_clustering.csv', sep = ",", header = TRUE)
names(cluster_output2)[2] = "Person.ID"
names(cluster_output2)[1] = 'name'
cluster_output2 = merge(cluster_output2, aliases, by.x = "Person.ID", by.y = 'PersonId', all.x = TRUE, all.y = FALSE)


#preparing summary data from palmer
# summary_dat = read.table("summary.csv", sep=",", fill=T, quote='\"', row.names=NULL)


shinyServer(function(input, output) {

	
  #plotting zoe and palmer's work
  output$network1 <- renderVisNetwork({
  	
    cluster_output1_restrict = cluster_output1[cluster_output1$Community.ID == input$Group,]
    unique_pairs_restrict = unique_pairs[(is.element(unique_pairs$SenderPersonId, cluster_output1_restrict$Person.ID) & 
    					  	is.element(unique_pairs$ReceiverPersonId, cluster_output1_restrict$Person.ID)), -3]
  	
    #defining the graph nodes and edges
    nodes <- data.frame(
    				id = cluster_output1_restrict$Person.ID, 
    				label = cluster_output1_restrict$Alias,
    				value = rep(1, dim(cluster_output1_restrict)[1]),
    				group = cluster_output1_restrict$Community.ID
    				# level = c(rep(1, floor(input$Nodes/2)), rep(2, input$Nodes - floor(input$Nodes/2)))
    )

    
    edges <- data.frame(
    				from = unlist(unique_pairs_restrict$SenderPersonId),
    				to = unlist(unique_pairs_restrict$ReceiverPersonId), 
    				title = unlist(unique_pairs_restrict$summary),
    				length = rep(200, dim(unique_pairs_restrict)[1])
    )
    
    #graph visualization and option
    if (dim(edges)[1] > 0){
        visNetwork(nodes, edges, main = "Social Network 1") %>%
        visIgraphLayout(layout = input$Layout) %>%
        visPhysics(stabilization = FALSE) %>%
        # visHierarchicalLayout() %>%
        visLayout(improvedLayout = TRUE) %>%
        visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE,
                       hoverConnectedEdges = TRUE, selectable = TRUE) %>%
        visEdges(arrows = 'from', smooth = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE)
    } else {
        visNetwork(nodes, main = "Social Network 1")
    }
    
    
  })
  
  
  
  #plotting Daitong's and Yihe's work
  output$network2 <- renderVisNetwork({
  	
    #defining the graph nodes and edges
    unique_pairs_restrict = unique_pairs[(is.element(unique_pairs$SenderPersonId, cluster_output2$Person.ID) & 
    					  	is.element(unique_pairs$ReceiverPersonId, cluster_output2$Person.ID)), ]
    
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
    				from = unlist(unique_pairs_restrict$SenderPersonId),
    				to = unlist(unique_pairs_restrict$ReceiverPersonId), 
    				title = unlist(unique_pairs_restrict$summary),
    				length = rep(200, dim(unique_pairs_restrict)[1])
    )
    
    #graph visualization and options
    if (dim(edges)[1] > 0){
        visNetwork(nodes, edges, main = "Social Network 1") %>%
        visIgraphLayout(layout = input$Layout2) %>%
        visPhysics(stabilization = FALSE) %>%
        # visHierarchicalLayout() %>%
        visLayout(improvedLayout = TRUE) %>%
        visInteraction(navigationButtons = TRUE, hideEdgesOnDrag = FALSE, hideNodesOnDrag = FALSE,
                       hoverConnectedEdges = TRUE, selectable = TRUE) %>%
        visEdges(arrows = 'from', smooth = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), nodesIdSelection = TRUE)
    } else {
        visNetwork(nodes, main = "Social Network 1")
    }
    
  })
  
  
})
