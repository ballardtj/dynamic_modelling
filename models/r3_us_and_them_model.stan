functions {
  real clustering_coefficient(matrix Probx,row_vector rnorm1,int Nplayers){

    int ctr = 0;
    matrix[Nplayers,Nplayers] tmp1;
    matrix[Nplayers,Nplayers] tmp2;
    matrix[Nplayers,Nplayers] tmp3;
    real numerator;
    real denominator;
    real coef;

    for(g in 1:Nplayers){
      for(h in 1:Nplayers){
        ctr=ctr+1;
        tmp1[g,h] = round( Probx[g,h] + rnorm1[ctr] );
      }
    }

    tmp2 = tmp1 * tmp1;
    tmp3 = tmp2 * tmp1;

    numerator = sum(diagonal(tmp3));
    denominator =  sum(tmp2) - sum(diagonal(tmp2));

    if(denominator == 0){
      coef = 0;
    }
    if(denominator > 0){
      coef = numerator / denominator ;
    }

    return coef;
  }

  real[,] simulate_abm( int Ntotal,                   //Total number of trials in the dataset (600)
                       int Nsims,
                       int Nplayers,
                       matrix runif1,
                       matrix runif2,
                       matrix runif3,
                       matrix rnorm1,
                       matrix Probx_0,
                       real[] Payx_0,
                       int[,] player_n,
                       int[,] player_m,
                       real A){

    real setpoint;
    //real A = 0;
    real r = 3;
    real t = 2;
    real coopcoop = 1;
    real coopdefect = -3;
    real defectdefect = -1;
    real defectcoop = 3;
    real predictions[Ntotal,2];
    //real predicted_sd[Ntotal];

    matrix[Nplayers,Nplayers]  Probx;
    real Payx[Nplayers];
    matrix[Nsims,Ntotal] coefficients;
    int n;
    int m;
    real ProbInt;
    real pnstr;
    real pmstr;

    setpoint = 0.5;

    for(i in 1:Nsims){
      Probx = Probx_0;
      Payx = Payx_0;

      for(j in 1:Ntotal){

        //##################################
        //##1. Probability of interaction.##
        //##################################
        //print(Probx);
        n = player_n[i,j];
        m = player_m[i,j];

        ProbInt = Probx[n,m];

        if(ProbInt > runif1[i,j]){

          //print(Ntotal);

          //########################################
          //##2. Interaction behavior and payoffs.##
          //########################################

          pnstr = (ProbInt > (runif2[i,j] - A)) ? 1 : 0;
          pmstr = (ProbInt > (runif3[i,j] - A)) ? 1 : 0;

          //Calculate prisoner's dilemma payoffs according to the values
          //defined above.
          if ((pnstr == 0) && (pmstr == 0)){
            Payx[m] = Payx[m] + defectdefect;
            Payx[n] = Payx[n] + defectdefect;
          }

          if ((pnstr == 0) && (pmstr == 1)){
            Payx[n] = Payx[n] + defectcoop;
            Payx[m] = Payx[m] + coopdefect;
          }

          if ((pnstr == 1) && (pmstr == 0)){
            Payx[n] = Payx[n] + coopdefect;
            Payx[m] = Payx[m] + defectcoop;
          }

          if ((pnstr == 1) && (pmstr == 1)){
            Payx[m] = Payx[m] + coopcoop;
            Payx[n] = Payx[n] + coopcoop;
          }

          //#############################################################
          //##3. Reciprocity-moving closer or further from partners (r)##
          //#############################################################

          if ((pnstr == 1) && (pmstr == 1)){
            Probx[n,m] = 1 - (1 - ProbInt)/r;
            Probx[m,n] = Probx[n,m];
          }

          if ((pmstr == 0) && (pnstr == 0)){
            Probx[n,m] = ProbInt/r;
            Probx[m,n] = Probx[n,m];
          }

          //#######################
          //##4. Transitivity (t)##
          //#######################

          if ((pnstr == 1) && (pmstr == 1)){

            for (g in 1:Nplayers){
              //vicarious updating does not include the dyad.
              if ((g!=n) && (g!=m)){
                //if player n has stronger feelings than player m, about
                //player g, then they provide the greater influence.
                if ( fabs(Probx[n,g] - setpoint) > fabs(Probx[m,g] - setpoint) ) {

                  //so if Player n is friends with that player g
                  //(i.e., closeness above the setpoint), then Player
                  //m moves closer to player g. By symmetry, Player g
                  //is also now closer to Player m.
                  if (Probx[n,g] > setpoint) {
                    Probx[m,g] = 1 - (1 - Probx[m,g])/t;
                    Probx[g,m] = Probx[m,g];
                  }

                  //now if n feels stronger and dislikes g, then m
                  //moves further away by t
                  if ( Probx[n,g] < setpoint ){
                    Probx[m,g] = Probx[m,g]/t;
                    Probx[g,m] = Probx[m,g];
                  }
                }
                //if Player m has stronger feelings than player n, then they
                //provide the greater influence
                if ( fabs(Probx[m,g] - setpoint) > fabs(Probx[n,g] - setpoint)){

                  if (Probx[m,g] > setpoint) { //likes g
                    Probx[n,g] = 1 - (1 - Probx[n,g])/t; //closer
                    Probx[g,n] = Probx[n,g];
                  }

                  if (Probx[m,g] < setpoint) { //dislikes g
                    Probx[n,g] = Probx[n,g]/t; //further
                    Probx[g,n] = Probx[n,g];
                  }
                }
              }
            } //close player looop
          } //close transitivity if statement
        } //close interaction if statement

        coefficients[i,j] = clustering_coefficient(Probx,rnorm1[i,],Nplayers);
        //print(coefficients[i,j]);
      } //close round loop
    } //close sim loop
    //print(Probx);
    //print(coefficients[Nsims,Ntotal]);
    //get predicted mean and sd for each round
    for(j in 1:Ntotal){
      predictions[j,1] = mean(coefficients[,j]);
      predictions[j,2] = sd(coefficients[,j]) + 10e-10;
    }

    return predictions;
  }
} //end transformed parameters





data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  int Nsims;
  int Nplayers;
  int Nplayers_sq;
  real value[Ntotal];
  matrix[Nsims,Ntotal] runif1;
  matrix[Nsims,Ntotal] runif2;
  matrix[Nsims,Ntotal] runif3;
  matrix[Nsims,Nplayers_sq] rnorm1;
  matrix[Nplayers,Nplayers] Probx_0;
  real Payx_0[Nplayers];
  int player_n[Nsims,Ntotal];
  int player_m[Nsims,Ntotal];
}

parameters {
  real<lower=0,upper=1> A;
}

transformed parameters {
  real predictions[Ntotal,2];
  predictions = simulate_abm(Ntotal,
                                Nsims,
                                Nplayers,
                                runif1,
                                runif2,
                                runif3,
                                rnorm1,
                                Probx_0,
                                Payx_0,
                                player_n,
                                player_m,
                                A);
}

model {
  value ~ normal(predictions[,1],predictions[,2]);
}
