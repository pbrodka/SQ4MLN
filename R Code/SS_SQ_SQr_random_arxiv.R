library(multinet)
library(stringr)

#sciezka do katalogu z sieciami
path <- "D:/SS4MLN/arxiv/"



#full/compleete network
fullnet <- read_ml("d:/SS4MLN/FullNet/arxiv_netscience_multiplex_4NoNature.edges", name="arxiv", sep=',', aligned=FALSE)
#fullnet <- read_ml("d:/SS4MLN/FullNet/papapaTransportation_multiplex_4NoNature.edges", name="papapa", sep=',', aligned=FALSE)


out.file<-""
file.names <- dir(path)
results_ss <- data.frame(network_name=factor(), network=factor(), pp=numeric(), network_id=numeric(), seed_count=numeric(), no_of_seeds=numeric(), no_of_activated=numeric(), no_of_iterations=numeric())
results_sq <- data.frame(network_name=factor(), network=factor(), pp=numeric(), network_id=numeric(), seed_count=numeric(), no_of_seeds=numeric(), no_of_activated=numeric(), no_of_iterations=numeric())
results_sqr <- data.frame(network_name=factor(), network=factor(), pp=numeric(), network_id=numeric(), seed_count=numeric(), no_of_seeds=numeric(), no_of_activated=numeric(), no_of_iterations=numeric())



#for each network file in the directory
for(i in 1:length(file.names)){
  fn<-file.names[i]
  #fn<-file.names[1]
  ICM_net <- read_ml(paste(path,fn,sep=""))
  #print(ICM_net)
  print(paste(fn,Sys.time()))
  network <- unlist(strsplit(fn, "_"))[2]
  pp <- unlist(strsplit(fn, "_"))[4]
  network_id <- unlist(strsplit(fn, "_"))[5]
  
  #calculate measure for ranking
  #node degrees
  #deg <- degree_ml(fullnet)
  #print(degree)
  
  #each node multilayer neigbourhood size - it is not the same as degree since in multilayer if a node is connetced to the same node on 5 layers its degree will be 5 but neigbourhood size will be 1
  #neig<-neighborhood_ml(fullnet)
  #degree
  #neig<-degree_ml(fullnet)
  #Random
  
  ##top_actors - ranking list according to some measure
  top_actors <- actors_ml(fullnet)[order(sample(1:length(actors_ml(fullnet)), length(actors_ml(fullnet)), replace=F),actors_ml(fullnet))]
  
  
  #seeds percentage
  seed_count_v <- c(0.02, 0.05, 0.10, 0.20)
  #seed_count<-0.05
  
#single stage seeding
  for (seed_count in seed_count_v) {
    #print(paste(seed_count,Sys.time()))
    
    #number of seed based on seeds % based on oryginal network
    no_of_seeds <- as.integer(length(actors_ml(fullnet))*seed_count)
    
    #all active nodes at any moment
    active_nodes <- head(top_actors,no_of_seeds)
    
    #nodes which can activate in this iteration
    can_activate <- head(top_actors,no_of_seeds)
    interation_no <- 0
    #while there is any node that can activate
    while (length(can_activate)>0) {
      
      #nodes activated in this step/iteration
      interation_no <- interation_no + 1
      activated_nodes <- vector()
      for (node in can_activate) {
        
        #dodaj jego s¹siadów do activated i active je¿eli  'b' %in% v
        #!!!!!!!! s¹siadów bierzemy z sieci z coordinated 
        node_neigbours <- neighbors_ml(ICM_net, node)
        for (node_neigbour in node_neigbours)
        {
          if(!(node_neigbour %in% active_nodes))
          {
            activated_nodes <- append(activated_nodes, node_neigbour)
            active_nodes <- append(active_nodes, node_neigbour)
          }
        }
      }
      
      #put nodes activated in this step/iteration as a nodes which can activate in the next step/iteration
      can_activate <- activated_nodes
    }
    result <- data.frame(network_name=fn, network=network, pp=pp, network_id=network_id, seed_count=seed_count, no_of_seeds=no_of_seeds, no_of_activated=length(active_nodes), no_of_iterations=interation_no)
  #results <- data.frame(network_name=factor(), network=factor(), pp=numeric(), network_id=numeric(), seed_count=numeric(), no_of_seeds=numeric(), no_of_activated=numeric(), no_of_iterations=numeric())
    
    results_ss <- rbind(results_ss, result)
    save_path <- "D:/SS4MLN/ResultsR1/results_arxiv_random_ss.txt"
    write.csv(results_ss, save_path)
  }
  


#sequential seeding
for (seed_count in seed_count_v) {
  #print(paste(seed_count,Sys.time()))
  
  #number of seed based on seeds % based on oryginal network
  no_of_seeds <- as.integer(length(actors_ml(fullnet))*seed_count)
  
  #all active nodes at any moment
  active_nodes <- head(top_actors,1)
  
  #nodes which can activate in this iteration
  can_activate <- head(top_actors,1)
  
  #we have activated first node
  no_of_seeds <- no_of_seeds-1
  
  interation_no <- 0
  #while there is any node that can activate
  while (length(can_activate)>0) {
    
    #nodes activated in this step/iteration
    interation_no <- interation_no + 1
    activated_nodes <- vector()
    for (node in can_activate) {
      #dodaj jego s¹siadów do activated i active je¿eli  'b' %in% v
      #!!!!!!!! s¹siadów bierzemy z sieci z coordinated 
      node_neigbours <- neighbors_ml(ICM_net, node)
      for (node_neigbour in node_neigbours)
      {
        if(!(node_neigbour %in% active_nodes))
        {
          activated_nodes <- append(activated_nodes, node_neigbour)
          active_nodes <- append(active_nodes, node_neigbour)
        }
        
      }
      
    }
    #put nodes activated in this step/iteration as a nodes which can activate in the next step/iteration
    can_activate <- activated_nodes
    
    #activate next seed if we have any left
    if(no_of_seeds>0)
    {
      for (next_seed in top_actors) {
        if(!(next_seed %in% active_nodes))
        {
          no_of_seeds <- no_of_seeds-1
          can_activate <- append(can_activate, next_seed)
          active_nodes <- append(active_nodes, next_seed)
          break
        }
        
      }
    }
  }
  result <- data.frame(network_name=fn, network=network, pp=pp, network_id=network_id, seed_count=seed_count, no_of_seeds=as.integer(length(actors_ml(fullnet))*seed_count), no_of_activated=length(active_nodes), no_of_iterations=interation_no, saved_seeds=no_of_seeds)
  results_sq <- rbind(results_sq, result)
  save_path <- "D:/SS4MLN/ResultsR1/results_arxiv_random_sq.txt"
  write.csv(results_sq, save_path)
  
}


#sequential seeding with reviwal
for (seed_count in seed_count_v) {
  #print(paste(seed_count,Sys.time()))
  
  #number of seed based on seeds % based on oryginal network
  no_of_seeds <- as.integer(length(actors_ml(fullnet))*seed_count)
  
  #all active nodes at any moment
  active_nodes <- head(top_actors,1)
  
  #nodes which can activate in this iteration
  can_activate <- head(top_actors,1)
  
  #we have activated first node
  no_of_seeds <- no_of_seeds-1
  
  interation_no <- 0
  #while there is any node that can activate
  while (length(can_activate)>0) {
    
    #nodes activated in this step/iteration
    interation_no <- interation_no + 1
    activated_nodes <- vector()
    for (node in can_activate) {
      #dodaj jego s¹siadów do activated i active je¿eli  'b' %in% v
      #!!!!!!!! s¹siadów bierzemy z sieci z coordinated 
      node_neigbours <- neighbors_ml(ICM_net, node)
      for (node_neigbour in node_neigbours)
      {
        if(!(node_neigbour %in% active_nodes))
        {
          activated_nodes <- append(activated_nodes, node_neigbour)
          active_nodes <- append(active_nodes, node_neigbour)
        }
        
      }
      
    }
    #put nodes activated in this step/iteration as a nodes which can activate in the next step/iteration
    can_activate <- activated_nodes
    
    #activate next seed if we have any left
    #here w can add some other conditions for example "and length(can_activate)=0" and we have reviwal mode
    if(no_of_seeds>0 & length(can_activate)==0)
    {
      for (next_seed in top_actors) {
        if(!(next_seed %in% active_nodes))
        {
          no_of_seeds <- no_of_seeds-1
          can_activate <- append(can_activate, next_seed)
          active_nodes <- append(active_nodes, next_seed)
          break
        }
        
      }
    }
  }
  #if(no_of_seeds>0 & length(active_nodes)<61) { 
  #  print(no_of_seeds) 
  #  print(length(active_nodes))
  #  }
  result <- data.frame(network_name=fn, network=network, pp=pp, network_id=network_id, seed_count=seed_count, no_of_seeds=as.integer(length(actors_ml(fullnet))*seed_count), no_of_activated=length(active_nodes), no_of_iterations=interation_no, saved_seeds=no_of_seeds)
  results_sqr <- rbind(results_sqr, result)
  save_path <- "D:/SS4MLN/ResultsR1/results_arxiv_random_sqr.txt"
  write.csv(results_sqr, save_path)
  
}

}






