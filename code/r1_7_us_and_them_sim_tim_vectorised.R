#R code for:
########################################################################
###########The Emergence of “Us and Them” in 80 Lines of Code:##########
########### Modeling Group Genesis in Homogenous Populations.###########
########################################################################
##K. Gray, D. G. Rand, E. Ert, K. Lewis, S. Hershman, and M. I. Norton##
########################################################################
#Included here are the 80 line of code for the agent-based model, accompanied by
#extensive comments and an avi to visualize group genesis. See also
#http://www.mpmlab.org/groups for an interactive version of this model


##########
##Setup:##
##########

#Model Parameters:
#numplay = number of players in the game
#numrounds = the number of generations
#setpoint = the probability of interaction where we start everyone at.
#Anything higher than the setpoint is seen as being "closer" to someone,
#whereas anything further away is seen as being "further" from someone.
#(Value: .50, which is indifference or neither close nor far)
#A = cooperation adjustment: influences the probability of coop/defection (range:
# -1 to 1, though most meaningful between -.2 to .2.
#r = the reciprocity coefficient: players move closer after cooperating/further
#after defecting (range: 0 - infinity)
#t = the transitivity coefficient: players move closer/further to the friends/enemies of
#another player after an instance of mutual cooperation (0 - infinity).
#graphrounds = the number of rounds that a graph appears for

numplay = 12
numrounds = 10000
setpoint = .5
A = 0
r = 3
t = 2
graphrounds = 10



#Payoffs: prisoner's dilemma payoffs for each strategy. First word is
#what the first person decides to do. Second is what the second person decides to do.
coopcoop = 1
coopdefect = -3
defectdefect = -1
defectcoop = 3

#setpoint values, with zeroes on the diagonal.
Probx = (1-diag(1,numplay))*setpoint

#Payoff Matrix: Payx = total payoff matrix from interactions of coop and
#defecting, initialize matrix as all zeroes
Payx = rep(0,numplay)

##############
##Simulation##
##############

#loop for number of generations
for(rounds in 1:numrounds){

  ##################################
  ##1. Probability of interaction.##
  ##################################

  #in the matrix of players, select two players to play with each other
  #from row n and column m of the matrix
  n = ceiling(runif(1)*numplay) #CAN BE DONE OUTSIDE LOOP
  m = ceiling(runif(1)*(numplay-1)) #CAN BE DONE OUTSIDE LOOP

  #Because you cannot play with oneself, if the row and column equal
  #each other, we add 1 to m. The m function above is numplay-1 because
  #we don't want adding 1 to go beyond the number of players.
  if (m>=n) {
    m=m+1 #CAN BE DONE OUTSIDE LOOP
  }

  #n and m have been matched up, but they are more likely to interact if
  #they are closer to each other. Closeness--Probx(n,m)--is isomorphic
  #with the probability of interaction, ProbInt.
  ProbInt = Probx[n,m]

  #the higher Probx(n,m) the more likley n and m are to interact. A
  #random number is drawn and if ProbInt is higher than this number, they
  #interact. If it is lower, then a new pair of players are chosen.
  if (ProbInt > runif(1)) {  #RANDOM NUMBER CAN BE GENERATED OUTSIDE LOOP

    ########################################
    ##2. Interaction behavior and payoffs.##
    ########################################

    #Now player play a prisoner's dilemma. To pick a strategy (coop or
    #defect), new random numbers are generated for both n and m. If
    #the probability of interaction (i.e., closeness), as given by
    #ProbInt/Probx(n,m), is higher than this number, a player will
    #cooperate. If the random number is lower, than a player will
    #defect. Thus, the closer you are, the more likely you are to
    #cooperate. However, this can be modified if the players are
    #relatively more suspicious or trusting (i.e., the coopadj
    #parameter A. If this is greater than zero, then people are more
    #trusting, if it is less than zero, people are less trusting.

    #player n; 0 = defect, 1 = cooperate.
    if (ProbInt > (runif(1) - A)){
      pnstr = 1 #pnstr = player n's strategy.
    } else {
      pnstr = 0
    }


    #player m; 0 = defect, 1 = cooperate.
    if (ProbInt > (runif(1) - A)){
      pmstr = 1 #pmstr = player m's strategy.
    } else {
      pmstr = 0
    }

    #Calculate prisoner's dilemma payoffs according to the values
    #defined above.
    if ((pnstr == 0) && (pmstr == 0)){
      Payx[m] = Payx[m] + defectdefect
      Payx[n] = Payx[n] + defectdefect
    } else if ((pnstr == 0) && (pmstr == 1)){
      Payx[n] = Payx[n] + defectcoop
      Payx[m] = Payx[m] + coopdefect
    } else if ((pnstr == 1) && (pmstr == 0)){
      Payx[n] = Payx[n] + coopdefect
      Payx[m] = Payx[m] + defectcoop
    } else if ((pnstr == 1) && (pmstr == 1)){
      Payx[m] = Payx[m] + coopcoop
      Payx[n] = Payx[n] + coopcoop
    }

    #############################################################
    ##3. Reciprocity-moving closer or further from partners (r)##
    #############################################################

    #When someone cooperates with you, you move closer. When someone
    #defects with you, you move further away. people prefer to play
    #with others who are cooperate and prefer not to play with people
    #who defect. So, if player n and m cooperate, they move closer by
    #halving the distance between the 1 and their current closeness
    #(ProbInt). If players n and m defect, they both move further
    #away, by halving the distance between their current closeness
    #(ProbInt) and 0. If one defects and one cooperate, then one moves
    #closer and one further and these cancel each other out for
    #ProbInt.

    if ((pnstr == 1) && (pmstr == 1)){
      Probx[n,m] = 1 - (1 - ProbInt)/r
      Probx[m,n] = Probx[n,m]
    } else if ((pmstr == 0) && (pnstr == 0)){
      Probx[n,m] = ProbInt/r
      Probx[m,n] = Probx[n,m]
    }

    #######################
    ##4. Transitivity (t)##
    #######################

    #When both players cooperate, they not only move closer, but bring
    #their choices inline with the person they're moving closer to.
    #Specifically, whoever has the stronger feelings on given person
    #influences the other person. For example, if I really hate
    #someone, and you're indifferent or just like them a little, then
    #my extreme opinion will sway you more than vice versa. This
    #transitivity is operationalized by t, which functions like r--it
    #functions as a ration by which closeness is changed. t is set
    #lower than r, because presumably this vicarious knowledge is less
    #powerful than first hand experience. Players compare their
    #opinion on all players, represented by 'g'.
    if ((pnstr == 1) && (pmstr == 1)){
      for (g in 1:numplay){
        #vicarious updating does not include the dyad.
        if ((g!=n) && (g!=m)){
          #if player n has stronger feelings than player m, about
          #player g, then they provide the greater influence.
          if ( abs(Probx[n,g] - setpoint) > abs(Probx[m,g] - setpoint) ) {

            #so if Player n is friends with that player g
            #(i.e., closeness above the setpoint), then Player
            #m moves closer to player g. By symmetry, Player g
            #is also now closer to Player m.
            if (Probx[n,g] > setpoint) {
              Probx[m,g] = 1 - (1 - Probx[m,g])/t
              Probx[g,m] = Probx[m,g]
              #now if n feels stronger and dislikes g, then m
              #moves further away by t
            } else if ( Probx[n,g] < setpoint ){
              Probx[m,g] = Probx[m,g]/t;
              Probx[g,m] = Probx[m,g]
            }

          #if Player m has stronger feelings than player n, then they
          #provide the greater influence
          } else if ( abs(Probx[m,g] - setpoint) > abs(Probx[n,g] - setpoint)){
            if (Probx[m,g] > setpoint) { #likes g
              Probx[n,g] = 1 - (1 - Probx[n,g])/t # closer
              Probx[g,n] = Probx[n,g]
            } else if (Probx[m,g] < setpoint) { #dislikes g
              Probx[n,g] = Probx[n,g]/t #further
              Probx[g,n] = Probx[n,g]
            }
          }
        }
      }
    }
  }
}


#items can be displayed by writing them here. For example, if you want to
#view the final probability/closeness matrix, you would write
Probx
#To calculate clustering, make a clustering matrix and add a small amount
#of jitter to each cell so that all starting positions (.5) don't all round up to 1.
ClusterMatrix = Probx

for (g in 1:numplay){
  for (h in 1:numplay){
    ClusterMatrix[g,h] = ClusterMatrix[g,h] + ((runif(1)-.5)/1000)
  }
}

#clustering coefficient works on binary values so you need to round.
ClusterMatrix=round(ClusterMatrix)

#the clustering coefficient (ClusteringCoeff) is the ratio of closed triplets
#(numerator)over the number of total triplets (denominator)
Numerator=sum(diag(ClusterMatrix^3))
Denominator=sum(sum(ClusterMatrix^2))-sum(diag(ClusterMatrix^2))

if (Denominator == 0){ #can't divide by zero.
  #If there are no triplets, there can be no clustering and so it will be zero.
  ClusterCoeff=0
} else {
  #this is the clustering coefficient
  ClusterCoeff=Numerator/Denominator
}

#to calculate average payoff, take the mean of the payoff matrix (Payx),
#and multiply by the number of players, and divide by the number of rounds
#and the number of players in a round (i.e., 2)
AveragePayoff=mean(Payx)*numplay/(numrounds*2)

