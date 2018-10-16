functions {
  real clustering_coefficient(matrix Probx,row_vector rnorm1,int Nplayers){

    int ctr = 0;
    real tmp_term;
    real term1 = 0;
    real term2 = 0;
    real term3 = 0;
    real coef;

    for(g in 1:Nplayers){
      for(h in 1:Nplayers){
        ctr=ctr+1;
        tmp_term = round( Probx[g,h] + rnorm1[ctr] );
        if(g==h){
          term1 += pow(tmp_term,3);
          term2 += square(tmp_term);
        }
        term3 += square(tmp_term);
      }
    }

    if((term2+term3) == 0){
      coef = 0;
    }
    if((term2+term3) > 0){
      coef = term1 / (term2+term3);
    }

    return coef;
  }
}


data {
  int Ntotal;                   //Total number of trials in the dataset (600)
  int Nsims;
  int Nplayers;
  int Nplayers_sq;
  real value[Ntotal];
  matrix[Ntotal,Nsims] runif1;
  matrix[Ntotal,Nsims] runif2;
  matrix[Ntotal,Nsims] runif3;
  matrix[Nsims,Nplayers_sq] rnorm1;
  matrix[Nplayers,Nplayers] Probx_0;
  real Payx_0[Nplayers];
  int player_n[Ntotal,Nsims];
  int player_m[Ntotal,Nsims];
}

parameters {
  real<lower=0,upper=1> dummy;
}

transformed parameters {
  real setpoint;
  real A = 0;
  real r = 3;
  real t = 2;
  real coopcoop = 1;
  real coopdefect = -3;
  real defectdefect = -1;
  real defectcoop = 3;
  real predicted_mean[Ntotal];
  real predicted_sd[Ntotal];

  { //local variables

    matrix[Nplayers,Nplayers]  Probx;
    real Payx[Nplayers];
    matrix[Ntotal,Nsims] coefficients;
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

        n = player_n[i,j];
        m = player_m[i,j];

        ProbInt = Probx[n,m];

        if(ProbInt > runif1[i,j]){

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
      } //close round loop
    } //close sim loop

    //get predicted mean and sd for each round
    for(j in 1:Ntotal){
      predicted_mean[j] = mean(coefficients[,j]);
      predicted_sd[j] = sd(coefficients[,j]) + 10e-10;
    }
  } //close local variables
} //end transformed parameters



model {
  value ~ normal(predicted_mean,predicted_sd);
}
