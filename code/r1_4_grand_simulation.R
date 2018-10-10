###########################################################################
#### Modification, adaptation, or use of any portion of this code for  ####
#### any purposse must include a reference to the full manuscript      ####
#### citation provided below. This code is provided 'as is.' The       ####
#### authors are under no obligation to provide assistance or          ####
#### instruction pertaining to the use or operation of this code.      ####
####                                                                   ####
####                                         ####
####                                                       ####
###########################################################################

############ System Time ##################################################
Start_Time       <- Sys.time()                                                                                  # records system start time

###########################################################################
############ Program Level Inputs #########################################
###########################################################################
m                <- 1                                                                                           # number of iterations - keep this at 1!!!
k                <- 1                                                                                           # number of teams
cycl             <- 50                                                                                          # number of experimental cycles (learning and sharing)
lcl              <- 50                                                                                          # number of experimental time points in each learning phase
scl              <- 50                                                                                          # number of experimental time points in each sharing phase

###########################################################################
############ Creates Output Matrices ######################################
###########################################################################
ww               <- array(0,dim=c(1,29))                                                                        # creates a matrix to hold the team-level dynamic ouput
ww               <- data.frame(ww)                                                                              # makes the output matrix into a dataframe

zz               <- array(0,dim=c(k,16))                                                                        # creates the matrix to hold the team-level  static outputs
zz               <- data.frame(zz)                                                                              # makes the team level output matrix into a dataframe

xx               <- array(0,dim=c(1,8))                                                                         # creates a matrix to hold the individual-level dynamic outputouput
xx               <- data.frame(xx)

yy               <- array(0,dim=c(1,13))                                                                        # creates a matrix to hold the individual-level static outputouput
yy               <- data.frame(yy)

wwww               <- array(0,dim=c(1,29))                                                                      # creates a matrix to hold the team-level dynamic ouput
wwww               <- data.frame(wwww)                                                                          # makes the output matrix into a dataframe

zzzz               <- array(0,dim=c(k,16))                                                                      # creates the matrix to hold the team-level  static outputs
zzzz               <- data.frame(zzzz)                                                                          # makes the team level output matrix into a dataframe

xxxx               <- array(0,dim=c(1,8))                                                                       # creates a matrix to hold the individual-level dynamic outputouput
xxxx               <- data.frame(xxxx)

###########################################################################
############ Team Knowledge Building Simulation ###########################
###########################################################################
for (i in 1:m) {                                                                                                # begins the iteration loop
  np <- 0                                                                                                      # initializes the person counter to zero
  for (j in 1:k) {                                                                                             # begins the team loop
    n             <- 3                                                                                        # assigns the sample size to the team
    
    ############ Team-Level Dynamic Output Matrix (data) #############################
    z             <- array(0,dim=c((2*cycl),29))                                                              # creates the team-level dynamic output matrix for an individual team
    z[,1]         <- j                                                                                        # column that identifies the team
    z[,2]         <- 1:(2*cycl)                                                                               # column that identifies each phase
    z             <- data.frame(z)                                                                            # makes z a dataframe
    
    ############ Team-Level Dynamic Output Matrix (info) #############################
    zzz           <- array(0,dim=c((2*cycl),29))                                                              # creates the team-level dynamic output matrix for an individual team
    zzz[,1]       <- j                                                                                        # column that identifies the team
    zzz[,2]       <- 1:(2*cycl)                                                                               # column that identifies each phase
    zzz           <- data.frame(zzz)                                                                          # makes z a dataframe
    
    ############ Individual-Level Dynamic Output Matrix (data) #######################
    x             <- array(0, dim=c((n*2*cycl),8))                                                            # creates the individual-level dynamic output matrix for an individual team
    x[,1]         <- j                                                                                        # column that identifies the team
    x[,2]         <- array((rep((np+1):(np+n), each=(2*cycl))), dim=c((2*cycl*n),1))                          # column that identifies the individual
    x[,3]         <- array((rep(1:(2*cycl), n)), dim=c((2*cycl*n),1))                                         # column that identifies each phase
    x             <- data.frame(x)                                                                            # makes x a dataframe
    
    ############ Individual-Level Dynamic Output Matrix (info) #######################
    xxx           <- array(0, dim=c((n*2*cycl),8))                                                            # creates the individual-level dynamic output matrix for an individual team
    xxx[,1]       <- j                                                                                        # column that identifies the team
    xxx[,2]       <- array((rep((np+1):(np+n), each=(2*cycl))), dim=c((2*cycl*n),1))                          # column that identifies the individual
    xxx[,3]       <- array((rep(1:(2*cycl), n)), dim=c((2*cycl*n),1))                                         # column that identifies each phase
    xxx           <- data.frame(xxx)                                                                          # makes x a dataframe
    
    ############ Common/Unique Data ###########################################
    totd          <- n*12                                                                                     # creates the total information pool for the team to learn
    CtoU          <- 1                                                                                        # selects which of the 3 c/u conditions the team will be
    if (CtoU == 1){                                                                                           # c/u condition 1 - 1/3 common, 2/3 unique
      com         <- totd * (1/3)
      uniq        <- (totd-com)/n
    }
    if (CtoU == 2){                                                                                           # c/u condition 2 - 1/2 common, 1/2 unique
      com         <- totd * (1/2)
      uniq        <- (totd-com)/n
    }
    if (CtoU == 3){                                                                                           # c/u condition 3 - 2/3 common, 1/3 unique
      com         <- totd * (2/3)
      uniq        <- (totd-com)/n
    }
    
    ############ Common/Unique Info ###########################################
    comi          <- (com*(com-1))/2                                                                          # creates the amount of common information fpr each individual
    uniqi         <- (uniq*(uniq-1))/2                                                                        # creates the amount of unique information for each individual
    
    ########### Learning and Sharing Profiles #################################
    lplr           <- array(0,dim=c(n,1))                                                                     # creates the array for learning rates
    lrs            <- 1                                                                                       # selects which of the 3 learning conditions the team will be
    if (lrs == 1) {                                                                                           # lplr condition 1 - fast learning (3-5 reps each)
      for (hb in 1:n) {
        lplr[hb,1] <- sample(3:5,1)
      }
    }
    if (lrs == 2) {                                                                                           # lplr condition 2 - average learning (6-8 reps each)
      for (hb in 1:n) {
        lplr[hb,1] <- sample(6:8,1)
      }
    }
    if (lrs == 3) {                                                                                           # lplr condition 3 - slow learning (9-11 reps each)
      for (hb in 1:n) {
        lplr[hb,1] <- sample(9:11,1)
      }
    }
    if (lrs == 4) {                                                                                           # lplr condition 4 - variable learning 1 fast (3-5 reps), 1 average (6-8 reps), 1 slow (9-11 reps)
      temp <- sample(1:3, n)                                                                                  # note: must set Replace = TRUE in the "sample" function on the line above if n > 3
      for (hb in 1:n) {
        if(temp[hb]==1) {lplr[hb,1] <- sample(3:5,1)}
        if(temp[hb]==2) {lplr[hb,1] <- sample(6:8,1)}
        if(temp[hb]==3) {lplr[hb,1] <- sample(9:11,1)}
      }
    }
    lplr <- sort(lplr)
    spkprob        <- array(0,dim=c(n,1))                                                                     # creates the array for sharing rates
    sps            <- 2                                                                                       # selects which of the 3 speaking probability conditions the team will be
    if (sps == 1) {                                                                                           # spkprob condition 1 - fastest learner speaks 3x more
      spkprob[1,1] <- 3*(1/((n-1)+3))
      for (hb in 2:n) {
        spkprob[hb,1] <- (1/((n-1)+3))
      }
    }
    if (sps == 2) {                                                                                           # spkprob condition 2 - all speak same amount
      for (hb in 1:n) {
        spkprob[hb,1] <- (1/n)
      }
    }
    if (sps == 3) {                                                                                           # spkprob condition 3 - slowest learner speaks 3x more
      for (hb in 1:(n-1)) {
        spkprob[hb,1] <- (1/((n-1)+3))
      }
      spkprob[n,1] <- 3*(1/((n-1)+3))
    }
    
    ############ Data Breakdown Array #########################################
    know              <- array(0,dim=c((n+1),1))                                                              # creates the array for the overall data distribution
    know[1,1]         <- com                                                                                  # assigns the amount of common data to each individual
    for (u in 2:(n+1)) {
      know[u,1]      <- uniq                                                                                 # assigns amount of unique data to each individual
    }
    
    ############ Info Breakdown Array #########################################
    knowi             <- array(0,dim=c((n+1),1))
    knowi             <- array(0,dim=c((n+1),1))                                                              # creates the array for the overall information distribution
    knowi[1,1]        <- comi                                                                                 # assigns amount of common information to each individual
    for (u in 2:(n+1)) {
      knowi[u,1]     <- uniqi                                                                                # assigns amount of unique information to each individual
    }
    
    ############ Data Matrix ##################################################
    w                 <- array(0,dim=c((sum(know)*n),(1+ (2*cycl))))                                          # creates a matrix to hold the ouput for data
    w[,1]             <- 1:sum(know)                                                                          # column that identifies the team
    w                 <- data.frame(w)                                                                        # makes the matrix w a data frame
    
    ############ Info Matrix ##################################################
    www               <- array(0,dim=c((sum(knowi)*n),(1+ (2*cycl))))                                         # creates a matrix to hold the ouput for information
    www[,1]           <- 1:sum(knowi)                                                                         # column that identifies the team
    www               <- data.frame(www)                                                                      # makes the matrix www a data frame
    
    ############ Data Distribution Matrix #####################################
    datadist   <- array(0,dim=c((sum(know)*n),1))                                                             # array of data distribution
    for (a in 1:n) {
      datadist[((a-1)*sum(know)+1):((a-1)*sum(know)+know[1,1]),1] <- 1                                       # asigns common data to each person
      datadist[((a-1)*sum(know)+sum(know[(1:a),1])+1):((a-1)*sum(know)+sum(know[(1:(a+1)),1])),1] <- 1       # asigns unique data to each person
    }
    
    ############ Info Distribution Matrix #####################################
    infodist   <- array(0,dim=c((sum(knowi)*n),1))                                                            # array of information distribution
    for (a in 1:n) {
      infodist[((a-1)*sum(knowi)+1):((a-1)*sum(knowi)+knowi[1,1]),1] <- 1                                    # asigns common information to each person
      infodist[((a-1)*sum(knowi)+sum(knowi[(1:a),1])+1):((a-1)*sum(knowi)+sum(knowi[(1:(a+1)),1])),1] <- 1   # asigns unique information to each person
    }
    
    ############ Info Learning Matrices ######################################
    cominfo      <- matrix(0,n*com,com)                                                                       # creates a matrix to track what common information has been learned
    uniqinfo     <- matrix(0,n*uniq,uniq)                                                                     # creates a matrix to track what unique information has been learned
    remd         <- array(0,dim=c(n,2))                                                                       # creates a matrix to remember the last piece of data learned during the learning phase
    
    ############ Team Level Variables ########################################
    v            <- 0                                                                                         # counting variable to store output
    lp           <- 0                                                                                         # counting variable to store number of learning phases
    sp           <- 0                                                                                         # counting variable to store number of sharing phases
    data         <- array(0,dim=c((sum(know)*n),2))                                                           # array of learned
    info         <- array(0,dim=c((sum(knowi)*n),2))                                                          # array of learned
    startdat     <- array(0,dim=c((sum(know)*n),2))                                                           # creates array to determine the amount of starting
    startdat[,2] <- datadist                                                                                  # puts datadist into the second column of the array to represent starting with all common and unique data and information
    learn        <- array(0,dim=c(n,2))                                                                       # matrix of learning and sharing phase learning rates
    share        <- array(0,dim=c(sum(know),n))                                                               # matrix of shared information
    shareinfo    <- array(0,dim=c(sum(knowi),n))                                                              # matrix of shared information
    learned      <- array(0,dim=c(sum(know),n))                                                               # matrix of learned information
    learnedinfo  <- array(0, dim=c(sum(knowi),n))
    zz[j,1]      <- j                                                                                         # records team number
    zz[j,2]      <- lrs                                                                                       # randomly chooses the learning rate profile
    zz[j,3]      <- zz[j,2]                                                                                   # sets the sharing phase learning rate profile
    zz[j,4]      <- sps                                                                                       # randomly selects sharing profile
    zz[j,5]      <- n                                                                                         # records team size set above
    zz[j,6]      <- CtoU                                                                                      # records the common to unique ratio
    zzzz[j,1]    <- j                                                                                         # records team number
    zzzz[j,2]    <- lrs                                                                                       # randomly chooses the learning rate profile
    zzzz[j,3]    <- zzzz[j,2]                                                                                 # sets the sharing phase learning rate profile
    zzzz[j,4]    <- sps                                                                                       # randomly selects sharing profile
    zzzz[j,5]    <- n                                                                                         # records team size set above
    zzzz[j,6]    <- CtoU                                                                                      # records the common to unique ratio
    data[,1]     <- startdat[,1]                                                                              # assigns the starting condition
    learn[,1]    <- lplr                                                                                      # assigns the learning phase learning rates based on profile selected above
    learn[,2]    <- floor((learn[,1]+1))                                                                      # creates the sharing phase learning rates from the learning phase learning rates selected above
    
    ############ Individual-Level Dynamic Output Matrix (data) #######################
    y            <- array(0,dim=c(n,13))                                                                      # creates individual-level static output matrix
    y            <- data.frame(y)                                                                             # makes y a dataframe
    y[1:n,1]     <- j                                                                                         # identifies the team
    y[1:n,2]     <- (np+1):(np+n)                                                                             # identifies each team member
    y[1:n,3]     <- learn[,1]                                                                                 # learning phase learning rates
    y[1:n,4]     <- learn[,2]                                                                                 # sharing phase learning rates
    y[1:n,5]     <- spkprob                                                                                   # sharing probability
    y[1:n,6]     <- com/sum(know)                                                                             # proportion of common data
    y[1:n,7]     <- uniq/sum(know)                                                                            # proportion of unique data
    y[1:n,8]     <- comi/sum(knowi)                                                                           # proportion of common data
    y[1:n,9]     <- uniqi/sum(knowi)                                                                          # proportion of unique data
    
    ############ Learning and Sharing Cycle ##################################
    for (c in 1:cycl) {                                                                                       # begins each cycle (learning + sharing)
      if ((sum(info) < n*sum(knowi)) | (sum(shareinfo) < n*sum(knowi))) {                                    # checks to see if all data and information is learned
        #psel <- sample(c(1,2),1)                                                                             # randomly selects learning or sharing - currently commented out so that it iterates between learning and sharing.
        
        ############ Learning Phase ##############################################
        #if (psel ==1) {                                                                                      # if using the random selection of learning and sharing phase then uncomment this
        dat   <- array(0,dim=c((sum(know)*n),1))                                                           # array to calculate the amount of learning phase data or information learned
        for (a in 1:n) {
          dat[((a-1)*sum(know)+1):((a-1)*sum(know)+know[1,1]),1] <-  data[((a-1)*sum(know)+1):((a-1)*sum(know)+know[1,1]),1]
          dat[((a-1)*sum(know)+sum(know[(1:a),1])+1):((a-1)*sum(know)+sum(know[(1:(a+1)),1])),1] <- data[((a-1)*sum(know)+sum(know[(1:a),1])+1):((a-1)*sum(know)+sum(know[(1:(a+1)),1])),1]
        }                                                                                                  # ends for loop
        if ((sum(cominfo) < (comi*2*n)) | (sum(uniqinfo) < (uniqi*2*n))) {                                 # checks if all learning phase information has been learned
          for (f in 1:n) {                                                                                 # begins for loop for each person's learning phase
            l = 1                                                                                         # initializes the counter for time used in the learning phase
            if (sum(datadist) == sum(dat)) {
              for (iii in 1:com) {
                for (ii in ((f-1)*sum(know)+1):((f-1)*sum(know)+know[1,1])) {                            # begins for loop that finishes combining common data into information that was left over from prior learning phase
                  if (l < lcl) {
                    if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know))) != iii) & 
                        ((cominfo[(((f-1)*com)+iii),(ii-((f-1)*sum(know)))] != 1) |
                         (cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),iii] != 1))) {
                      cominfo[(((f-1)*com)+iii),(ii-((f-1)*sum(know)))] = cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),iii] <- 1
                      l <- l + 1
                    }
                  }
                  else
                    break
                }
              }
              for (iii in 1:uniq) { 
                for (ii in ((f-1)*sum(know)+sum(know[(1:f),1])+1):((f-1)*sum(know)+sum(know[(1:(f+1)),1]))) {  # begins for loop that finishes combining unique data into information that was left over from prior learning phase
                  if (l < lcl) {
                    if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know)+com+(f-1)*uniq)) != iii) & 
                        ((uniqinfo[(((f-1)*uniq)+iii),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] != 1) | 
                         (uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),iii] != 1))) {
                      uniqinfo[(((f-1)*uniq)+iii),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] = uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),iii] <- 1
                      l <- l + 1
                    }
                  }
                  else
                    break
                }
              }
            }
            if (remd[f,1] > 0) {                                                                          # checks whether any common data has been learned
              for (ii in ((f-1)*sum(know)+1):((f-1)*sum(know)+know[1,1])) {                               # begins for loop that finishes combining common data into information that was left over from prior learning phase
                if (l < lcl) {
                  if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know))) != remd[f,1]) & 
                      ((cominfo[(((f-1)*com)+remd[f,1]),(ii-((f-1)*sum(know)))] != 1) |
                       (cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),remd[f,1]] != 1))) {
                    cominfo[(((f-1)*com)+remd[f,1]),(ii-((f-1)*sum(know)))] = cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),remd[f,1]] <- 1
                    l <- l + 1
                  }
                }
                else
                  break
              }
            }
            if (remd[f,2] > 0) {                                                                          # checks whether any unique data has been learned
              for (ii in ((f-1)*sum(know)+sum(know[(1:f),1])+1):((f-1)*sum(know)+sum(know[(1:(f+1)),1]))) {  # begins for loop that finishes combining unique data into information that was left over from prior learning phase
                if (l < lcl) {
                  if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know)+com+(f-1)*uniq)) != remd[f,2]) & 
                      ((uniqinfo[(((f-1)*uniq)+remd[f,2]),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] != 1) | 
                       (uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),remd[f,2]] != 1))) {
                    uniqinfo[(((f-1)*uniq)+remd[f,2]),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] = uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),remd[f,2]] <- 1
                    l <- l + 1
                  }
                }
                else
                  break
              }
            }
            for (q in (((f-1)*sum(know)+1):((f-1)*sum(know)+know[1,1]))) {                                # begins for loop to check partially learned common data
              while (data[q,1] < 1 & data[q,1] > 0) {
                if (l <= lcl) {                                                                         # checks if any time is left to learn
                  data[q,1] <- data[q,1] + (1/learn[f,1])                                               # increases piece of data by the persons learning rate
                  l = l + 1
                  if (data[q,1] > 0.99){                                                                # rounds any small fraction up to 1 to correct for rounding issue
                    data[q,1] <- 1
                    remd[f,1] <- (q - (f-1)*sum(know))                                                  # records the last piece of common data learned
                    for (ii in ((f-1)*sum(know)+1):((f-1)*sum(know)+know[1,1])) {                       # begins for loop that combines common data into information
                      if (l < lcl) {
                        if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know))) != remd[f,1]) & 
                            ((cominfo[(((f-1)*com)+remd[f,1]),(ii-((f-1)*sum(know)))] != 1) |
                             (cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),remd[f,1]] != 1))) {
                          cominfo[(((f-1)*com)+remd[f,1]),(ii-((f-1)*sum(know)))] = cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),remd[f,1]] <- 1
                          l <- l + 1
                        }
                      }
                      else
                        break
                    }
                  }
                } else
                  break
              }                                                                                          # ends while loop
            }                                                                                             # ends common data for loop
            for (q in ((f-1)*sum(know)+sum(know[(1:f),1])+1):((f-1)*sum(know)+sum(know[(1:(f+1)),1]))) {  # begins for loop to check partially learned unique data or information
              while (data[q,1] < 1 & data[q,1] > 0) {
                if (l <= lcl) {                                                                         # checks if any time is left to learn
                  data[q,1] <- data[q,1] + (1/learn[f,1])                                               # increases piece of data or information by the persons learning rate
                  l = l + 1
                  if (data[q,1] > 0.99){                                                                # rounds any small fraction up to 1 to correct for rounding issue
                    data[q,1] <- 1
                    remd[f,2] <- (q - ((f-1)*sum(know)+com + (f-1)*uniq))                               # records the last piece of unique data learned
                    for (ii in ((f-1)*sum(know)+sum(know[(1:f),1])+1):((f-1)*sum(know)+sum(know[(1:(f+1)),1]))) {  # begins for loop that combines unique data into information
                      if (l < lcl) {
                        if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know)+com+(f-1)*uniq)) != remd[f,2]) & 
                            ((uniqinfo[(((f-1)*uniq)+remd[f,2]),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] != 1) | 
                             (uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),remd[f,2]] != 1))) {
                          uniqinfo[(((f-1)*uniq)+remd[f,2]),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] = uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),remd[f,2]] <- 1
                          l <- l + 1
                        }
                      }
                      else
                        break
                    }
                  }
                } else
                  break
              }                                                                                          # ends while loop
            }                                                                                             # ends unique data for loop
            lorder <- array(0,dim=c((know[1,1]+know[(f+1),1]),1))                                         # creates an array for the order of learning new data or information
            lorder[1:know[1,1]] <- ((f-1)*sum(know)+1):((f-1)*sum(know)+know[1,1])
            lorder[(know[1,1]+1):(know[1,1]+know[(f+1),1])] <- ((f-1)*sum(know)+sum(know[(1:f),1])+1):((f-1)*sum(know)+sum(know[(1:(f+1)),1]))
            lorder <- sample(lorder, (know[1,1]+know[(f+1),1]), replace = FALSE, prob = NULL)             # randomizes the order of new data or information learning
            for (q in 1:(know[1,1]+know[(f+1),1])) {                                                      # begins phase when new dta or information is selected and learned
              while (data[(lorder[q]),1] < 1) {
                if (l <= lcl) {                                                                          # checks if any time is left to learn
                  data[lorder[q],1] <- data[lorder[q],1] + (1/learn[f,1])                                # increases piece of data or information by the persons learning rate
                  l = l + 1
                  if (data[lorder[q],1] > 0.99) {                                                        # rounds any small fraction up to 1 to correct for rounding issue
                    data[lorder[q],1] <- 1
                    if ((lorder[q] > (f-1)*sum(know)) & (lorder[q] < (f-1)*sum(know)+com)) {
                      remd[f,1] <- (lorder[q] - (f-1)*sum(know))                                         # records the last piece of common data learned
                      for (ii in ((f-1)*sum(know)+1):((f-1)*sum(know)+know[1,1])) {                      # begins for loop that combines common data into information
                        if (l < lcl) {
                          if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know))) != remd[f,1]) & 
                              ((cominfo[(((f-1)*com)+remd[f,1]),(ii-((f-1)*sum(know)))] != 1) |
                               (cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),remd[f,1]] != 1))) {
                            cominfo[(((f-1)*com)+remd[f,1]),(ii-((f-1)*sum(know)))] = cominfo[((ii-((f-1)*sum(know)))+ ((f-1)*com)),remd[f,1]] <- 1 
                            l <- l + 1
                          }
                        }
                        else
                          break
                      }
                    }
                    if ((lorder[q] > (f-1)*sum(know)+ com) & (lorder[q] < f*sum(know))) {
                      remd[f,2] <- (lorder[q] - ((f-1)*sum(know)+com + (f-1)*uniq))                      # records the last piece of unique data learned
                      for (ii in ((f-1)*sum(know)+sum(know[(1:f),1])+1):((f-1)*sum(know)+sum(know[(1:(f+1)),1]))) { # begins for loop that combines unique data into information
                        if (l < lcl) {
                          if ((data[ii,1] == 1) & ((ii-((f-1)*sum(know)+com+(f-1)*uniq)) != remd[f,2]) & 
                              ((uniqinfo[(((f-1)*uniq)+remd[f,2]),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] != 1) | 
                               (uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),remd[f,2]] != 1))) {
                            uniqinfo[(((f-1)*uniq)+remd[f,2]),(ii-((f-1)*sum(know)+com+(f-1)*uniq))] = uniqinfo[(((f-1)*uniq) + (ii-((f-1)*sum(know)+com+(f-1)*uniq))),remd[f,2]] <- 1
                            l <- l + 1
                          }
                        }
                        else
                          break
                      }
                    }
                  }
                }
                else
                  break
              }                                                                                          # ends while loop
            }                                                                                             # ends new learning for loop
          }                                                                                                # ends learning phase for loop
          ############ Learning Phase Team-Level Dynamic Output (data) ####################
          v  <- v+1                                                                                        # iterates the counter on number of cycles (learning or sharing) being recorded
          lp <- lp+1                                                                                       # iterates during each learning phase
          z[v,3] <- 1                                                                                      # records that it is a learning phase for the team-level dynamic output
          for (vv in 1:n) {
            x[((vv-1)*2*cycl+v),4] <- 1                                                                   # records that it is a learning phase for the individual-level dynamic output
          }                                                                                               
          for (h in 1:n) {                                                                                 # inputs the data into the learned matrix
            learned[,h] <- data[(((h-1)*sum(know)+1):(h*sum(know))),1]
          }
          learned <- floor(learned)                                                                        # makes all partially learned pieces of data zero so they can't be shared
          for (r in 1:(sum(know))) {                                                                       # checks each piece of data for its distribution of learning and sharing
            if (sum(learned[r,]) == 1)                                                                    # records pieces of data that only one member of the team knows
              z[v,4] <- z[v,4] + 1
            if ((sum(learned[r,]) > 1) & (sum(learned[r,]) < n))                                          # records pieces of data that more than one but less than the whole team knows
              z[v,5] <- z[v,5] + 1 
            if (sum(learned[r,]) == n)                                                                    # records pieces of data that the whole team knows
              z[v,6] <- z[v,6] + 1
            if (sum(share[r,]) == 1)                                                                      # records pieces of data that only one person has shared
              z[v,7] <- z[v,7] + 1
            if ((sum(share[r,]) > 1) & (sum(share[r,]) < n))                                              # records pieces of data that more than one but less than the whole team has shared
              z[v,8] <- z[v,8] + 1
            if (sum(share[r,]) == n)                                                                      # records pieces of data that the whole team has shared
              z[v,9] <- z[v,9] + 1
            if (sum(learned[r,]) >= 1)                                                                    # records pieces of data that the whole team knows
              z[v,10] <- z[v,10] + 1
          }
          for (ou in 4:10) {
            z[v,ou] <- (z[v,ou]/sum(know))                                                                # turns the output into a proportion
          }
          for (r in 1:(sum(know[1,1]))) {
            if (sum(learned[r,]) == 1)                                                                    # records pieces of common data that only one person knows
              z[v,11] <- z[v,11] + 1
            if ((sum(learned[r,]) > 1) & (sum(learned[r,]) < n))                                          # records pieces of common data that more than one person but less than the whole team knows
              z[v,12] <- z[v,12] + 1 
            if (sum(learned[r,]) == n)                                                                    # records pieces of common data that the whole team knows
              z[v,13] <- z[v,13] + 1
            if (sum(share[r,]) == 1)                                                                      # records pieces of common data only one person has shared
              z[v,14] <- z[v,14] + 1
            if ((sum(share[r,]) > 1) & (sum(share[r,]) < n))                                              # records pieces of common data that more than one person but less than the whole team has shared
              z[v,15] <- z[v,15] + 1
            if (sum(share[r,]) == n)                                                                      # records pieces of common data that the whole team has shared
              z[v,16] <- z[v,16] + 1
            if (sum(learned[r,]) >= 1)                                                                    # records pieces of common data that the whole team knows
              z[v,17] <- z[v,17] + 1
          }
          for (ou in 11:17) {
            z[v,ou] <- (z[v,ou]/sum(know[1,1]))                                                           # turns the output into a proportion
          }
          z[v,18] <- sum(learned[(1:know[1,1]),])/(n*sum(know[1,1]))
          for (r in (know[1,1]+1):sum(know)) {
            if (sum(learned[r,]) == 1)                                                                    # records pieces of unique data that only one person knows
              z[v,19] <- z[v,19] + 1
            if ((sum(learned[r,]) > 1) & (sum(learned[r,]) < n))                                          # records pieces of unique data that more than one person but less than the whole team knows
              z[v,20] <- z[v,20] + 1 
            if (sum(learned[r,]) == n)                                                                    # records pieces of unique data that the whole team knows
              z[v,21] <- z[v,21] + 1
            if (sum(share[r,]) == 1)                                                                      # records pieces of unique data only one person has shared
              z[v,22] <- z[v,22] + 1
            if ((sum(share[r,]) > 1) & (sum(share[r,]) < n))                                              # records pieces of unique data that more than one person but less than the whole team has shared
              z[v,23] <- z[v,23] + 1
            if (sum(share[r,]) == n)                                                                      # records pieces of unique data that the whole team has shared
              z[v,24] <- z[v,24] + 1
            if (sum(learned[r,]) >= 1)                                                                    # records pieces of unique data that the whole team knows
              z[v,25] <- z[v,25] + 1
          }
          for (ou in 19:25) {
            z[v,ou] <- (z[v,ou]/sum(know[(2:(n+1)),1]))                                                   # turns the output into a proportion
          }
          z[v,26] <- sum(learned[((know[1,1]+1):sum(know)),])/(n*sum(know[(2:(n+1)),1]))                   # records the proportion of total unique team data
          z[v,27] <- sum(learned)/(n*sum(know))                                                            # records the proportion of total data learned by everyone in team
          z[1,28] <-  z[1,27]                                                                              # records the team learning rate
          z[(2:(2*cycl)),28] <- diff(z[(1:(2*cycl)),27])
          ############ Learning Phase Team-Level Dynamic Output (info) ####################
          for (f in 1:n) {
            cominfo1 <- cominfo[(((f-1)*com)+1):(((f-1)*com)+com),1:com]
            learnedinfo[1:knowi[1],f] <- cominfo1[lower.tri(cominfo1)]
            info[((f-1)*sum(knowi)+1):((f-1)*sum(knowi)+knowi[1,1]),1] <- learnedinfo[1:knowi[1],f] 
            
            uniqinfo1 <- uniqinfo[(((f-1)*uniq)+1):(((f-1)*uniq)+uniq),1:uniq]
            learnedinfo[(sum(knowi[1:f]) + 1) : sum(knowi[1:(f+1)]), f] <- uniqinfo1[lower.tri(uniqinfo1)]
            info[((f-1)*sum(knowi)+sum(knowi[(1:f),1])+1):((f-1)*sum(knowi)+sum(knowi[(1:(f+1)),1])),1] <- learnedinfo[(sum(knowi[1:f]) + 1) : sum(knowi[1:(f+1)]), f] 
            
          }
          zzz[v,3] <- 1  
          for (vv in 1:n) {
            xxx[((vv-1)*2*cycl+v),4] <- 1                                                                 # records that it is a learning phase for the individual-level dynamic output
          }
          learnedinfo <- floor(learnedinfo)                                                                # makes all partially learned pieces of information zero so they can't be shared
          for (r in 1:(sum(knowi))) {                                                                      # checks each piece of information for its distribution of learning and sharing
            if (sum(learnedinfo[r,]) == 1)                                                                # records pieces of information that only one member of the team knows
              zzz[v,4] <- zzz[v,4] + 1
            if ((sum(learnedinfo[r,]) > 1) & (sum(learnedinfo[r,]) < n))                                  # records pieces of information that more than one but less than the whole team knows
              zzz[v,5] <- zzz[v,5] + 1 
            if (sum(learnedinfo[r,]) == n)                                                                # records pieces of information that the whole team knows
              zzz[v,6] <- zzz[v,6] + 1
            if (sum(shareinfo[r,]) == 1)                                                                  # records pieces of information that only one person has shared
              zzz[v,7] <- zzz[v,7] + 1
            if ((sum(shareinfo[r,]) > 1) & (sum(shareinfo[r,]) < n))                                      # records pieces of information that more than one but less than the whole team has shared
              zzz[v,8] <- zzz[v,8] + 1
            if (sum(shareinfo[r,]) == n)                                                                  # records pieces of information that the whole team has shared
              zzz[v,9] <- zzz[v,9] + 1
            if (sum(learnedinfo[r,]) >= 1)                                                                # records pieces of information that the whole team knows
              zzz[v,10] <- zzz[v,10] + 1
          }
          for (ou in 4:10) {
            zzz[v,ou] <- (zzz[v,ou]/sum(knowi))                                                           # turns the output into a proportion
          }
          for (r in 1:(sum(knowi[1,1]))) {
            if (sum(learnedinfo[r,]) == 1)                                                                # records pieces of common information that only one person knows
              zzz[v,11] <- zzz[v,11] + 1
            if ((sum(learnedinfo[r,]) > 1) & (sum(learnedinfo[r,]) < n))                                  # records pieces of common information that more than one person but less than the whole team knows
              zzz[v,12] <- zzz[v,12] + 1 
            if (sum(learnedinfo[r,]) == n)                                                                # records pieces of common information that the whole team knows
              zzz[v,13] <- zzz[v,13] + 1
            if (sum(shareinfo[r,]) == 1)                                                                  # records pieces of common information only one person has shared
              zzz[v,14] <- zzz[v,14] + 1
            if ((sum(shareinfo[r,]) > 1) & (sum(shareinfo[r,]) < n))                                      # records pieces of common information that more than one person but less than the whole team has shared
              zzz[v,15] <- zzz[v,15] + 1
            if (sum(shareinfo[r,]) == n)                                                                  # records pieces of common information that the whole team has shared
              zzz[v,16] <- zzz[v,16] + 1
            if (sum(learnedinfo[r,]) >= 1)                                                                # records pieces of common information that the whole team knows
              zzz[v,17] <- zzz[v,17] + 1
          }
          for (ou in 11:17) {
            zzz[v,ou] <- (zzz[v,ou]/sum(knowi[1,1]))                                                      # turns the output into a proportion
          }
          zzz[v,18] <- sum(learnedinfo[(1:knowi[1,1]),])/(n*sum(knowi[1,1]))
          for (r in (knowi[1,1]+1):sum(knowi)) {
            if (sum(learnedinfo[r,]) == 1)                                                                # records pieces of unique information that only one person knows
              zzz[v,19] <- zzz[v,19] + 1
            if ((sum(learnedinfo[r,]) > 1) & (sum(learnedinfo[r,]) < n))                                  # records pieces of unique information that more than one person but less than the whole team knows
              zzz[v,20] <- zzz[v,20] + 1 
            if (sum(learnedinfo[r,]) == n)                                                                # records pieces of unique information that the whole team knows
              zzz[v,21] <- zzz[v,21] + 1
            if (sum(shareinfo[r,]) == 1)                                                                  # records pieces of unique information only one person has shared
              zzz[v,22] <- zzz[v,22] + 1
            if ((sum(shareinfo[r,]) > 1) & (sum(shareinfo[r,]) < n))                                      # records pieces of unique information that more than one person but less than the whole team has shared
              zzz[v,23] <- zzz[v,23] + 1
            if (sum(shareinfo[r,]) == n)                                                                  # records pieces of unique information that the whole team has shared
              zzz[v,24] <- zzz[v,24] + 1
            if (sum(learnedinfo[r,]) >= 1)                                                                # records pieces of unique information that the whole team knows
              zzz[v,25] <- zzz[v,25] + 1
          }
          for (ou in 19:25) {
            zzz[v,ou] <- (zzz[v,ou]/sum(knowi[(2:(n+1)),1]))                                              # turns the output into a proportion
          }
          zzz[v,26] <- sum(learnedinfo[((knowi[1,1]+1):sum(knowi)),])/(n*sum(knowi[(2:(n+1)),1]))          # records the proportion of total unique team information
          zzz[v,27] <- sum(learnedinfo)/(n*sum(knowi))                                                     # records the proportion of total data learned by everyone in team
          zzz[1,28] <-  zzz[1,27]                                                                          # records the team learning rate
          zzz[(2:(2*cycl)),28] <- diff(zzz[(1:(2*cycl)),27])
          
          ############ Learning Phase Individual-Level Dynamic Output (data) ##############
          for (a in 1:n) {
            x[((a-1)*2*cycl+v),5] <- (sum(learned[,a])/sum(know))                                         # records proportion of data that is learned by each team member
            x[((a-1)*2*cycl+v),7] <- (sum(share[,a])/sum(know))                                           # records proportion of data that is shared by each team member
          }
          for (rr in 1:n) {
            x[((rr-1)*2*cycl+1),6] <-  x[((rr-1)*2*cycl+1),5]                                             # records proportion of data learned by each member in phase 1
            x[((rr-1)*2*cycl+1),8] <-  x[((rr-1)*2*cycl+1),7]                                             # records proportion of data shared by each member in phase 1
            x[((rr-1)*2*cycl+2):(rr*2*cycl),6] <- diff(x[((rr-1)*2*cycl+1):(rr*2*cycl),5])                # records proportion of data learned by each member in each phase
            x[((rr-1)*2*cycl+2):(rr*2*cycl),8] <- diff(x[((rr-1)*2*cycl+1):(rr*2*cycl),7])                # records proportion of data shared by each member in each phase
          }
          
          w[,(1+v)] <- data[,1]                                                                            # records the learned data to the output matrix
          
          ############ Learning Phase Individual-Level Dynamic Output (info) ##############
          for (a in 1:n) {
            xxx[((a-1)*2*cycl+v),5] <- (sum(learnedinfo[,a])/sum(knowi))                                  # records proportion of data that is learned by each team member
            xxx[((a-1)*2*cycl+v),7] <- (sum(shareinfo[,a])/sum(knowi))                                    # records proportion of data that is shared by each team member
          }
          for (rr in 1:n) {
            xxx[((rr-1)*2*cycl+1),6] <-  xxx[((rr-1)*2*cycl+1),5]                                         # records proportion of data learned by each member in phase 1
            xxx[((rr-1)*2*cycl+1),8] <-  xxx[((rr-1)*2*cycl+1),7]                                         # records proportion of data shared by each member in phase 1
            xxx[((rr-1)*2*cycl+2):(rr*2*cycl),6] <- diff(xxx[((rr-1)*2*cycl+1):(rr*2*cycl),5])            # records proportion of data learned by each member in each phase
            xxx[((rr-1)*2*cycl+2):(rr*2*cycl),8] <- diff(xxx[((rr-1)*2*cycl+1):(rr*2*cycl),7])            # records proportion of data shared by each member in each phase
          }
          
          www[,(1+v)] <- info[,1]                                                                          # records the learned data to the output matrix
        }                                                                                                  # ends learning phase if statement
        #}                                                                                                   # ends learning psel if statement
        
        ############ Sharing Phase ###############################################
        #if(psel == 2) {                                                                                     # if using the random phase selection then uncomment this
        s      <- 0
        while (s < scl) {
          if ((sum(info) < n*sum(knowi)) | (sum(shareinfo) < n*sum(knowi)))  {                             # checks whether all information has been learned
            sl     <- array(0,dim=c(n,1))                                                                  # array that records in this loop if a person says a piece of data or information
            p      <- sample(1:n,1, replace = FALSE, prob = spkprob)                                       # randomly chooses the person who speaks
            datasp <- array(0,dim=c(sum(knowi),2))                                                         # creates an array of possible data or information to share
            datasp[1:sum(knowi),1] <- info[((p-1)*sum(knowi)+1):((p-1)*sum(knowi)+sum(knowi)),1]
            datasp[1:sum(knowi),2] <- 1:sum(knowi)
            tempsp <- floor(datasp)
            dasp    <- 0
            speak   <- array(0,dim=c(sum(tempsp[,1]),2))
            sharesp <- array(0,dim=c(sum(tempsp[,1]),3))
            for (q in 1:sum(knowi)) {                                                                      # creates an array of data or information able to be shared and data or information that has been externalized
              if (tempsp[q,1] == 1) {
                dasp <- dasp + 1
                speak[dasp,1:2] <- datasp[q,1:2]
                sharesp[dasp,1:3] <- shareinfo[datasp[q,2],1:3]
              }
            }
            if (s < scl) {
              while (sl[p,1] < 1) {
                if (sum(speak[,1])*n > sum(sharesp))  {                                                    # checks whether a person has anything left to share (able to be share against fully externalized)
                  rint <- sample(1:sum(speak[,1]),1, replace = FALSE, prob = NULL)                         # randomly selects a piece of data or information to share
                  if ((speak[rint,1] >= 1) & (sum(shareinfo[speak[rint,2],1:n]) < n))  {                   # checks if piece of data or information selected is learned
                    for (d in 1:n) {
                      if (info[(d-1)*sum(knowi)+speak[rint,2],1] < 1)  {                                  # for each person checks if they have learned it - if not increases by their learning rate
                        info[(d-1)*sum(knowi)+speak[rint,2],1] <- info[(d-1)*sum(knowi)+speak[rint,2],1] + (1/learn[d,2])
                        if (info[(d-1)*sum(knowi)+speak[rint,2],1] > 0.99)                                # rounds any small fraction up to 1 to correct for rounding issue
                          info[(d-1)*sum(knowi)+speak[rint,2],1] <- 1  
                      }                                                                                   # ends if
                    }                                                                                      # ends for
                    if (shareinfo[speak[rint,2],p] < 1){                                                   # if the piece of data or information hasn't been externalized it globally externalizes it
                      shareinfo[speak[rint,2],p] <- 1
                      sl[p,1] <- 1
                    } else                                                                                 # records that a person has spoken for the local piece of data or information
                      sl[p,1] <- 1
                    s <- s + 1                                                                             # ends first speaker externalization
                  }                                                                                        # ends if checking if piece is learned
                } else
                  break                                                                                  # closes the if statement that checks whether all data and information has been shared by an individual
              }                                                                                            # ends while
            }                                                                                              # ends if - and ends overall externalization process for that piece of data or information at that time
          } else
            break                                                                                        # ends sharing phase if all data and information has been learned
        }                                                                                                  # ends while
        ############ Sharing Phase Team-Level Dynamic Output (data) #####################
        v  <- v+1                                                                                          # iterates the counter on number of cycles (learning or sharing) being recorded             
        sp <- sp+1                                                                                         # iterates each sharing phase
        z[v,3] <- 2                                                                                        # records that it is a sharing phase for the team-level dynamic output
        for (vv in 1:n) {
          x[((vv-1)*2*cycl+v),4] <- 2                                                                     # records that it is a sharing phase for the individual-level dynamic output
        }
        for (h in 1:n) {                                                                                   # inputs the data into the learned matrix
          learned[,h] <- data[(((h-1)*sum(know)+1):(h*sum(know))),1]
        }
        learned <- floor(learned)
        for (r in 1:(sum(know))) {                                                                         # checks each piece of data for its distribution of learning and sharing
          if (sum(learned[r,]) == 1)                                                                      # records pieces of data that only one member of the team knows
            z[v,4] <- z[v,4] + 1
          if ((sum(learned[r,]) > 1) & (sum(learned[r,]) < n))                                            # records pieces of data that more than one but less than the whole team knows
            z[v,5] <- z[v,5] + 1 
          if (sum(learned[r,]) == n)                                                                      # records pieces of data that the whole team knows
            z[v,6] <- z[v,6] + 1
          if (sum(share[r,]) == 1)                                                                        # records pieces of data that only one person has shared
            z[v,7] <- z[v,7] + 1
          if ((sum(share[r,]) > 1) & (sum(share[r,]) < n))                                                # records pieces of data that more than one but less than the whole team has shared
            z[v,8] <- z[v,8] + 1
          if (sum(share[r,]) == n)                                                                        # records pieces of data that the whole team has shared
            z[v,9] <- z[v,9] + 1
          if (sum(learned[r,]) >= 1)                                                                      # records pieces of data that the whole team knows
            z[v,10] <- z[v,10] + 1
        }
        for (ou in 4:10) {
          z[v,ou] <- (z[v,ou]/sum(know))                                                                  # turns the output into a proportion
        }
        for (r in 1:(sum(know[1,1]))) {
          if (sum(learned[r,]) == 1)                                                                      # records pieces of common data that only one person knows
            z[v,11] <- z[v,11] + 1
          if ((sum(learned[r,]) > 1) & (sum(learned[r,]) < n))                                            # records pieces of common data that more than one person but less than the whole team knows
            z[v,12] <- z[v,12] + 1 
          if (sum(learned[r,]) == n)                                                                      # records pieces of common data that the whole team knows
            z[v,13] <- z[v,13] + 1
          if (sum(share[r,]) == 1)                                                                        # records pieces of common data only one person has shared
            z[v,14] <- z[v,14] + 1
          if ((sum(share[r,]) > 1) & (sum(share[r,]) < n))                                                # records pieces of common data that more than one person but less than the whole team has shared
            z[v,15] <- z[v,15] + 1
          if (sum(share[r,]) == n)                                                                        # records pieces of common data that the whole team has shared
            z[v,16] <- z[v,16] + 1
          if (sum(learned[r,]) >= 1)                                                                      # records pieces of common data that the whole team knows
            z[v,17] <- z[v,17] + 1
        }
        for (ou in 11:17) {
          z[v,ou] <- (z[v,ou]/sum(know[1,1]))                                                             # turns the output into a proportion
        }
        z[v,18] <- sum(learned[(1:know[1,1]),])/(n*sum(know[1,1]))
        for (r in (know[1,1]+1):sum(know)) {
          if (sum(learned[r,]) == 1)                                                                      # records pieces of unique data that only one person knows
            z[v,19] <- z[v,19] + 1
          if ((sum(learned[r,]) > 1) & (sum(learned[r,]) < n))                                            # records pieces of unique data that more than one person but less than the whole team knows
            z[v,20] <- z[v,20] + 1 
          if (sum(learned[r,]) == n)                                                                      # records pieces of unique data that the whole team knows
            z[v,21] <- z[v,21] + 1
          if (sum(share[r,]) == 1)                                                                        # records pieces of unique data only one person has shared
            z[v,22] <- z[v,22] + 1
          if ((sum(share[r,]) > 1) & (sum(share[r,]) < n))                                                # records pieces of unique data that more than one person but less than the whole team has shared
            z[v,23] <- z[v,23] + 1
          if (sum(share[r,]) == n)                                                                        # records pieces of unique data that the whole team has shared
            z[v,24] <- z[v,24] + 1
          if (sum(learned[r,]) >= 1)                                                                      # records pieces of unique data that the whole team knows
            z[v,25] <- z[v,25] + 1
        }
        for (ou in 19:25) {
          z[v,ou] <- (z[v,ou]/sum(know[(2:(n+1)),1]))                                                     # turns the output into a proportion
        }
        z[v,26] <- sum(learned[((know[1,1]+1):sum(know)),])/(n*sum(know[(2:(n+1)),1]))                     # records the proportion of total unique team data
        z[v,27] <- sum(learned)/(n*sum(know))                                                              # records the proportion of total data learned by everyone in team
        z[1,28] <-  z[1,27]                                                                                # records the team learning rate
        z[(2:(2*cycl)),28] <- diff(z[(1:(2*cycl)),27])
        
        ############ Sharing Phase Team-Level Dynamic Output (info) #####################
        for (f in 1:n) {
          learnedinfo[1:knowi[1],f] <-  info[((f-1)*sum(knowi)+1):((f-1)*sum(knowi)+knowi[1,1]),1]
          learnedinfo[(knowi[1,1] + 1):sum(knowi),f] <-  info[((f-1)*sum(knowi)+ knowi[1,1] + 1):(f*sum(knowi)),1]
          
          cominfo1[lower.tri(cominfo1)] <- cominfo1[upper.tri(cominfo1)] <- learnedinfo[1:knowi[1],f]
          cominfo[(((f-1)*com)+1):(((f-1)*com)+com),1:com] <- cominfo1
        }
        zzz[v,3] <- 2  
        for (vv in 1:n) {
          xxx[((vv-1)*2*cycl+v),4] <- 2                                                                 # records that it is a learning phase for the individual-level dynamic output
        }
        learnedinfo <- floor(learnedinfo)                                                                # makes all partially learned pieces of information zero so they can't be shared
        for (r in 1:(sum(knowi))) {                                                                      # checks each piece of information for its distribution of learning and sharing
          if (sum(learnedinfo[r,]) == 1)                                                                # records pieces of information that only one member of the team knows
            zzz[v,4] <- zzz[v,4] + 1
          if ((sum(learnedinfo[r,]) > 1) & (sum(learnedinfo[r,]) < n))                                  # records pieces of information that more than one but less than the whole team knows
            zzz[v,5] <- zzz[v,5] + 1 
          if (sum(learnedinfo[r,]) == n)                                                                # records pieces of information that the whole team knows
            zzz[v,6] <- zzz[v,6] + 1
          if (sum(shareinfo[r,]) == 1)                                                                  # records pieces of information that only one person has shared
            zzz[v,7] <- zzz[v,7] + 1
          if ((sum(shareinfo[r,]) > 1) & (sum(shareinfo[r,]) < n))                                      # records pieces of information that more than one but less than the whole team has shared
            zzz[v,8] <- zzz[v,8] + 1
          if (sum(shareinfo[r,]) == n)                                                                  # records pieces of information that the whole team has shared
            zzz[v,9] <- zzz[v,9] + 1
          if (sum(learnedinfo[r,]) >= 1)                                                                # records pieces of information that the whole team knows
            zzz[v,10] <- zzz[v,10] + 1
        }
        for (ou in 4:10) {
          zzz[v,ou] <- (zzz[v,ou]/sum(knowi))                                                           # turns the output into a proportion
        }
        for (r in 1:(sum(knowi[1,1]))) {
          if (sum(learnedinfo[r,]) == 1)                                                                # records pieces of common information that only one person knows
            zzz[v,11] <- zzz[v,11] + 1
          if ((sum(learnedinfo[r,]) > 1) & (sum(learnedinfo[r,]) < n))                                  # records pieces of common information that more than one person but less than the whole team knows
            zzz[v,12] <- zzz[v,12] + 1 
          if (sum(learnedinfo[r,]) == n)                                                                # records pieces of common information that the whole team knows
            zzz[v,13] <- zzz[v,13] + 1
          if (sum(shareinfo[r,]) == 1)                                                                  # records pieces of common information only one person has shared
            zzz[v,14] <- zzz[v,14] + 1
          if ((sum(shareinfo[r,]) > 1) & (sum(shareinfo[r,]) < n))                                      # records pieces of common information that more than one person but less than the whole team has shared
            zzz[v,15] <- zzz[v,15] + 1
          if (sum(shareinfo[r,]) == n)                                                                  # records pieces of common information that the whole team has shared
            zzz[v,16] <- zzz[v,16] + 1
          if (sum(learnedinfo[r,]) >= 1)                                                                # records pieces of common information that the whole team knows
            zzz[v,17] <- zzz[v,17] + 1
        }
        for (ou in 11:17) {
          zzz[v,ou] <- (zzz[v,ou]/sum(knowi[1,1]))                                                      # turns the output into a proportion
        }
        zzz[v,18] <- sum(learnedinfo[(1:knowi[1,1]),])/(n*sum(knowi[1,1]))
        for (r in (knowi[1,1]+1):sum(knowi)) {
          if (sum(learnedinfo[r,]) == 1)                                                                # records pieces of unique information that only one person knows
            zzz[v,19] <- zzz[v,19] + 1
          if ((sum(learnedinfo[r,]) > 1) & (sum(learnedinfo[r,]) < n))                                  # records pieces of unique information that more than one person but less than the whole team knows
            zzz[v,20] <- zzz[v,20] + 1 
          if (sum(learnedinfo[r,]) == n)                                                                # records pieces of unique information that the whole team knows
            zzz[v,21] <- zzz[v,21] + 1
          if (sum(shareinfo[r,]) == 1)                                                                  # records pieces of unique information only one person has shared
            zzz[v,22] <- zzz[v,22] + 1
          if ((sum(shareinfo[r,]) > 1) & (sum(shareinfo[r,]) < n))                                      # records pieces of unique information that more than one person but less than the whole team has shared
            zzz[v,23] <- zzz[v,23] + 1
          if (sum(shareinfo[r,]) == n)                                                                  # records pieces of unique information that the whole team has shared
            zzz[v,24] <- zzz[v,24] + 1
          if (sum(learnedinfo[r,]) >= 1)                                                                # records pieces of unique information that the whole team knows
            zzz[v,25] <- zzz[v,25] + 1
        }
        for (ou in 19:25) {
          zzz[v,ou] <- (zzz[v,ou]/sum(knowi[(2:(n+1)),1]))                                              # turns the output into a proportion
        }
        zzz[v,26] <- sum(learnedinfo[((knowi[1,1]+1):sum(knowi)),])/(n*sum(knowi[(2:(n+1)),1]))          # records the proportion of total unique team information
        zzz[v,27] <- sum(learnedinfo)/(n*sum(knowi))                                                     # records the proportion of total data learned by everyone in team
        zzz[1,28] <-  zzz[1,27]                                                                          # records the team learning rate
        zzz[(2:(2*cycl)),28] <- diff(zzz[(1:(2*cycl)),27])
        
        ############ Sharing Phase Individual-Level Dynamic Output (data) ##############
        for (a in 1:n) {
          x[((a-1)*2*cycl+v),5] <- (sum(learned[,a])/sum(know))                                           # records proportion of data that is learned by each team member
          x[((a-1)*2*cycl+v),7] <- (sum(share[,a])/sum(know))                                             # records proportion of data that is shared by each team member
        }
        for (rr in 1:n) {
          x[((rr-1)*2*cycl+1),6] <-  x[((rr-1)*2*cycl+1),5]                                               # records proportion of data learned by each member in phase 1
          x[((rr-1)*2*cycl+1),8] <-  x[((rr-1)*2*cycl+1),7]                                               # records proportion of data shared by each member in phase 1
          x[((rr-1)*2*cycl+2):(rr*2*cycl),6] <- diff(x[((rr-1)*2*cycl+1):(rr*2*cycl),5])                  # records proportion of data learned by each member in each phase
          x[((rr-1)*2*cycl+2):(rr*2*cycl),8] <- diff(x[((rr-1)*2*cycl+1):(rr*2*cycl),7])                  # records proportion of data shared by each member in each phase
        }
        
        w[,(1+v)] <- data[,1]                                                                              # records the learned data to the output matrix
        
        ############ Learning Phase Individual-Level Dynamic Output (info) ##############
        for (a in 1:n) {
          xxx[((a-1)*2*cycl+v),5] <- (sum(learnedinfo[,a])/sum(knowi))                                  # records proportion of information that is learned by each team member
          xxx[((a-1)*2*cycl+v),7] <- (sum(shareinfo[,a])/sum(knowi))                                    # records proportion of information that is shared by each team member
        }
        for (rr in 1:n) {
          xxx[((rr-1)*2*cycl+1),6] <-  xxx[((rr-1)*2*cycl+1),5]                                         # records proportion of information learned by each member in phase 1
          xxx[((rr-1)*2*cycl+1),8] <-  xxx[((rr-1)*2*cycl+1),7]                                         # records proportion of information shared by each member in phase 1
          xxx[((rr-1)*2*cycl+2):(rr*2*cycl),6] <- diff(xxx[((rr-1)*2*cycl+1):(rr*2*cycl),5])            # records proportion of information learned by each member in each phase
          xxx[((rr-1)*2*cycl+2):(rr*2*cycl),8] <- diff(xxx[((rr-1)*2*cycl+1):(rr*2*cycl),7])            # records proportion of information shared by each member in each phase
        }
        
        www[,(1+v)] <- info[,1]                                                                          # records the learned information to the output matrix
      } else
        break                                                                                              # ends cycle if statement
    }                                                                                                         # ends cycle for loop
    avevar <- array(0,dim=c(v,n))
    for(av in 1:n) {
      avevar[,av] <- xxx[((av-1)*v+1):(av*v), 5]
    }
    zzz[1:v,29]  <- as.numeric(apply(avevar, 1, var))
    ############ Team-Level Dynamic Output (data) ##################################
    z <- z[-(v+1):-(max(dim(z))),]                                                                            # deletes all unused rows in z
    ww <- rbind(ww,z)                                                                                         # assigns team dynamic output to overall team-level dynamic output matrix
    if (j == 1)                                                                                               # deletes first row of ww after first team has been added
      ww <- ww[-1,]
    
    ############ Team-Level Dynamic Output (info) ##################################
    zzz <- zzz[-(v+1):-(max(dim(zzz))),]                                                                      # deletes all unused rows in z
    wwww <- rbind(wwww,zzz)                                                                                   # assigns team dynamic output to overall team-level dynamic output matrix
    if (j == 1)                                                                                               # deletes first row of ww after first team has been added
      wwww <- wwww[-1,]
    
    ############ Individual-Level Dynamic Output (data) ##############################
    for (cc in n:1) {                                                                                         # deleteds unused rows in x
      x <- x[-(((cc-1)*2*cycl)+v+1):-(cc*2*cycl),]
    }
    xx <- rbind(xx,x)                                                                                         # adds most recent teams dynamic output to overall dynamic output matrix
    if (j == 1)                                                                                               # deletes first row of xx after first team has been added
      xx <- xx[-1,]
    
    ############ Individual-Level Dynamic Output (info) ##############################
    for (cc in n:1) {                                                                                         # deleteds unused rows in x
      xxx <- xxx[-(((cc-1)*2*cycl)+v+1):-(cc*2*cycl),]
    }
    xxxx <- rbind(xxxx,xxx)                                                                                   # adds most recent teams dynamic output to overall dynamic output matrix
    if (j == 1)                                                                                               # deletes first row of xx after first team has been added
      xxxx <- xxxx[-1,]
    
    ############ Team-Level Static Output (data) ###################################
    zz[j,7]    <- v                                                                                           # number of phases it takes for team to learn all data
    zz[j,8]    <- lp                                                                                          # total number of learning phases
    zz[j,9]    <- sp                                                                                          # total number of sharing phases
    for (bb in 1:n) {                                                                                         # number of phases for each member to learn all
      for (bbb in 1:v) {
        if (x[((bb-1)*v + bbb),5] == 1) {
          y[bb,10] <- bbb
          break
        }
      }
    }
    zz[j,10] <- min(y[bb,12])                                                                                 # number of cycles it takes for one person to learn all data
    for (bb in 1:v) {
      if (z[bb,18] == 1) {
        zz[j,11] <- bb
        break
      }
    }                                                                                                         # total cycles for team to learn all common data
    for (bb in 1:v) {
      if (z[bb,26] == 1) {
        zz[j,12] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all unique data
    avevar <- array(0,dim=c(v,n))
    for(av in 1:n) {
      avevar[,av] <- x[((av-1)*v+1):(av*v), 5]
    }
    zz[j,13]  <- mean(apply(avevar, 1, var))                                                                  # average variance in proportion of total data learned across team members
    for (bb in 1:v) {
      if (z[bb,10] == 1) {
        zz[j,14] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all type 2 knowledge
    for (bb in 1:v) {
      if (z[bb,6] == 1) {
        zz[j,15] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all fully overlapping internalized
    for (bb in 1:v) {
      if (z[bb,9] == 1) {
        zz[j,16] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all fully overlapping externalized
    
    ############ Team-Level Static Output (info) ###################################
    zzzz[j,7]    <- v                                                                                         # number of phases it takes for team to learn all information
    zzzz[j,8]    <- lp                                                                                        # total number of learning phases
    zzzz[j,9]    <- sp                                                                                        # total number of sharing phases
    for (bb in 1:n) {                                                                                         # number of phases for each member to learn all
      for (bbb in 1:v) {
        if (xxx[((bb-1)*v + bbb),5] == 1) {
          y[bb,12] <- bbb
          break
        }
      }
    }
    zzzz[j,10] <- min(y[bb,12])                                                                               # number of cycles it takes for one person to learn all information
    for (bb in 1:v) {
      if (zzz[bb,18] == 1) {
        zzzz[j,11] <- bb
        break
      }
    }                                                                                                         # total cycles for team to learn all common information
    for (bb in 1:v) {
      if (zzz[bb,26] == 1) {
        zzzz[j,12] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all unique information
    zzzz[j,13]  <- mean(apply(avevar, 1, var))                                                                # average variance in proportion of total information learned across team members
    for (bb in 1:v) {
      if (zzz[bb,10] == 1) {
        zzzz[j,14] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all type 2 knowledge
    for (bb in 1:v) {
      if (zzz[bb,6] == 1) {
        zzzz[j,15] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all fully overlapping internalized
    for (bb in 1:v) {
      if (zzz[bb,9] == 1) {
        zzzz[j,16] <- bb
        break
      }
    }                                                                                                         # total cycles required to learn all fully overlapping externalized
    
    ############ Individual-Level Static Output (data) ##############################
    for (bb in 1:n) {                                                                                         # number of phases for each member to share all
      for (bbb in 1:v) {
        if (x[((bb-1)*v + bbb),7] == 1) {
          y[bb,11] <- bbb
          break
        }
      }
    }
    
    ############ Individual-Level Static Output (info) ##############################
    for (bb in 1:n) {                                                                                         # number of phases for each member to share all
      for (bbb in 1:v) {
        if (xxx[((bb-1)*v + bbb),7] == 1) {
          y[bb,13] <- bbb
          break
        }
      }
    }
    
    yy <- rbind(yy,y)                                                                                         # adds most recent individual static output to overall individual static output matrix
    if (j == 1)                                                                                               # deletes first row of yy after first team has been added
      yy <- yy[-1,]
    #### Next Team ####
    np <- np + n                                                                                              # increments the counter for total number of people
  } # ends the team for loop
  #### Next Set of Teams ####
}  # ends the iteration for loop

############ Names Output Matrices ##############lplr#########################
names(ww)[1:29] <- names(wwww)[1:29]  <- c("team","phase","L_or_S","non_int", "part_int", "full_int",           # names the team-level dynamic output columns
                                           "non_ext", "part_ext", "full_ext", "team_know","com_non_int", "com_part_int",
                                           "com_full_int", "com_non_ext", "com_part_ext", "com_full_ext", "com_team_know",
                                           "tot_com_team_know","unq_non_int", "unq_part_int", "unq_full_int", "unq_non_ext",
                                           "unq_part_ext", "unq_full_ext", "unq_team_know","tot_unq_team_know",
                                           "tot_team_know", "team_lr","variance")

names(zz)[1:16] <- names(zzzz)[1:16] <- c("team","lplr_profile","splr_profile","spk_prob","team_size",          # names the team-level static output columns
                                          "C_to_U","total_phases","learning_phases","sharing_phases","one_person","common",
                                          "unique","ave_variance","type_2_k","fully_int","fully_ext")

names(xx)[1:8] <- names(xxxx)[1:8] <- c("team","ind","phase","L_or_S","learn","learn_rate","share",             # names individual-level dynamic output
                                        "share_rate") 

names(yy)[1:13] <- c("team","ind","lplr","splr","spk_prob","com_data","uniq_data","com_info","uniq_info",       # names the individual-level static output
                     "full_int_data","full_ext_data","full_int_info","full_ext_info")

##########################################################################
Run_Time <- Sys.time() - Start_Time                                                                             # calculates and records how long the simulation ran

##########################################################################
#### For interpreting output, use the following dataframes:           ####
#### wwww for team-level within-trial output                          ####
#### zzzz for team-level between-trial output                         ####
#### xxxx for individual-level within-trial output                    ####
#### yy for individual-level between-trial output                     ####
##########################################################################