simulate.round <- function(n.doors, policy){
  ChosenDoor = sample(1:n.doors,1)
  rightDoor = sample(1:n.doors,1)
  if (policy == "KEEP"){
    if (ChosenDoor == rightDoor) {
      return (1); 
    }
    else{ 
      return (0);
    }  
  }
  if (policy == "CHANGE"){
    if (ChosenDoor != rightDoor) {
      return (1); 
    }
    else{ 
      return (0);
    }  
  }
}

main <- function(policy, n.min, n.max, n.trials){
  win.probability <- rep(0, n.max)
  for(n.doors in n.min:n.max){
    win <- 0
    for(i in 1:n.trials){
      win <- win + if(simulate.round(n.doors, policy)){
        1
      } else {
        0
      }
    }
    win.probability[n.doors] <- win/n.trials
  }
  plot(3:n.max, win.probability[3:n.max], type="l", xlab="# of Doors", ylab="Win Probability", main=policy)
}

main("NAME", 3, 100, 10*1000)