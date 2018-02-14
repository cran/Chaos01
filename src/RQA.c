#include <math.h>
#include <stdio.h>

/*
TS: time series
RQA: array with RQA measures (results)
R_RPy: index of the row
R_max: maximum size of the recurrence plot
R_dim: embedding dimension
R_lag: embedding lag/delay
R_eps: threshold/neighbourhood size
*/

void diag_rqa_max(double *TS, double *RQA, int *R_theta, int *R_rmax, int *R_dim, int *R_lag, double *R_eps, int *R_lmin)
{
#define TILE (8 /* doubles per vector */ * 5)
  
    int theta  = *R_theta;
    int rmax   = *R_rmax;
    int dim    = *R_dim;
    int lag    = *R_lag;
    //double eps   = *R_eps * (*R_eps); //Euclidean distance
    double eps = *R_eps; //Maximum and Manhattan distance
    int lmin   = *R_lmin;
  
    int line_length = 0;       
    int max_diag    = 0;       // maximal length of diagonal lines       
    int max_vert    = 0;       // maximal length of vertical lines
    long long recurrences = 0; //number of reccurence points in diagonals       
    long long points_diag = 0; // number of reccurence points in diagonals of length lmin and more
    long long count_diag  = 0; // number of diagonals of length lmin and more
    long long points_vert = 0; // number of reccurence points in verticals of length lmin and more
    long long count_vert  = 0; // number of verticals of length lmin and more
    int RPy = 0;
    int RPx = 0;

    int ub = (rmax - RPy) / TILE;

    //================================== Compute diagonal lines ==============================   
    
    for(RPy = theta; RPy < rmax; RPy++)
    {
        int ub = (rmax - RPy) / TILE;
        for(int t = 0; t < ub; ++t)
        {
            double temp[TILE] = {0};
            for(int m = 0; m < dim; ++m)
            {
                int ind = TILE * t + m * lag;
                for(RPx = 0; RPx < TILE; RPx++)
                {
                    double a = TS[ind + RPx];
                    double b = TS[ind + RPx + RPy];
                    if(temp[RPx] < fabs(a - b)){temp[RPx] = fabs(a - b);} // Maximum distance L_infinity
                    //temp[RPx] += (a - b) * (a - b); //Euclidean computation (faster, but hard to estimate threshold) L_2
                    //temp[RPx] += fabs(a - b); // Manhattan metric L_1
                }
            }
    
            for(int ti = 0; ti < TILE; ++ti) {
                if(temp[ti] > eps)
                {
                    recurrences += line_length;
                    if(line_length >= lmin) {
                         points_diag += line_length;
                         count_diag++;}
                    if(max_diag < line_length){max_diag = line_length;}
                    line_length = 0;
                } else
                {
                    line_length++;
                }
            }
        }
    
        { /* Remainder handling of elements < TILE */
            double temp[TILE] = {0};
            int rest = (rmax - RPy) % TILE;
            for(int m = 0; m < dim; ++m)
            {
                int ind = ub * TILE + m * lag;
                for(int RPx = 0; RPx < rest; RPx++)
                {
                    double a = TS[ind + RPx];
                    double b = TS[ind + RPx + RPy];
                    if(temp[RPx] < fabs(a - b)){temp[RPx] = fabs(a - b);} // Maximum distance L_infinity
                    //temp[RPx] += (a - b) * (a - b); //Euclidean computation (faster, but hard to estimate threshold) L_2
                    //temp[RPx] += fabs(a - b); // Manhattan metric L_1
                }
            }
            for(int ti = 0; ti < rest; ++ti) {
                if(temp[ti] > eps)
                {
                    recurrences += line_length;
                    if(line_length >= lmin) {
                        points_diag += line_length;
                        count_diag++;}
                    if(max_diag < line_length){max_diag = line_length;}
                    line_length = 0;
                } else
                {
                    line_length++;
                }
            }
        }

        recurrences += line_length;
        if(line_length >= lmin) {
          points_diag += line_length;
          count_diag++;}
        if(max_diag < line_length){max_diag = line_length;}
        line_length = 0;
    }
    
//================================== Compute vertical lines ==============================    
line_length = 0;
    
    // If theta == 0, it is possible for the vertical to transit "through" the main diagonal and it is necessary to compute the whole recurrence plot
    if(theta == 0){
      
      for(RPx = 0; RPx < (rmax); RPx++)
      {    
        int ub = rmax / TILE;
        for(int t = 0; t < ub; ++t)
        {
          int const_idx = t * TILE + theta;
          double temp[TILE] = {0};
          for(int m = 0; m < dim; m++)
          {
            int ind = m * lag + RPx;
            double a = TS[ind];
            for(int RPy = 0; RPy < TILE; RPy++)
            {
              double b = TS[RPy + const_idx];
              if(temp[RPy] < fabs(a - b)){temp[RPy] = fabs(a - b);} // Maximum distance L_infinity
              //temp[RPy] += (a - b) * (a - b); //Euclidean computation (faster, but hard to estimate threshold) L_2
              //temp[RPy] += fabs(a - b); // Manhattan distance L_1
            }
          }
          
          for(int ti = 0; ti < TILE; ++ti) {
            if(temp[ti] > eps)
            {
              if(line_length >= lmin) {
                points_vert += line_length;
                count_vert++;}
              if(max_vert < line_length){max_vert = line_length;}
              line_length = 0;
            } else
            {
              line_length++;
            }
          }
        }
        
        { /* Remainder handling of elements < TILE */
double temp[TILE] = {0};
          int rest = rmax % TILE;
          int const_idx = ub * TILE + theta;
          for(int m = 0; m < dim; ++m)
          {
            int ind = m * lag + RPx;
            double a = TS[ind];
            for(int RPy = 0; RPy < rest; RPy++)
            {
              double b = TS[RPy + const_idx];
              if(temp[RPy] < fabs(a - b)){temp[RPy] = fabs(a - b);} // Maximum distance L_infinity
              //temp[RPy] += (a - b) * (a - b); //Euclidean computation (faster, but hard to estimate threshold) L_2
              //temp[RPy] += fabs(a - b); // Manhattan distance L_1
            }
          }
          
          for(int ti = 0; ti < rest; ++ti) {
            if(temp[ti] > eps)
            {
              if(line_length >= lmin) {
                points_vert += line_length;
                count_vert++;}
              if(max_vert < line_length){max_vert = line_length;}
              line_length = 0;
            } else
            {
              line_length++;
            }
          }
        }
        
        if(line_length >= lmin) {
          points_vert += line_length;
          count_vert++;}
        if(max_vert < line_length){max_vert = line_length;}
        line_length = 0;
      }
      
      
    } else {
    
        for(RPx = 0; RPx < (rmax - 1); RPx++)
        {    
          int ub = (rmax - RPx - theta) / TILE;
          for(int t = 0; t < ub; ++t)
          {
            int const_idx = t * TILE + theta;
            double temp[TILE] = {0};
            for(int m = 0; m < dim; m++)
            {
                int ind = m * lag + RPx;
                double a = TS[ind];
                for(int RPy = 0; RPy < TILE; RPy++)
                {
                  double b = TS[RPy + ind + const_idx];
                  if(temp[RPy] < fabs(a - b)){temp[RPy] = fabs(a - b);} // Maximum distance L_infinity
                  //temp[RPy] += (a - b) * (a - b); //Euclidean computation (faster, but hard to estimate threshold) L_2
                  //temp[RPy] += fabs(a - b); // Manhattan distance L_1
                }
            }
            
            for(int ti = 0; ti < TILE; ++ti) {
              if(temp[ti] > eps)
              {
                if(line_length >= lmin) {
                  points_vert += line_length;
                  count_vert++;}
                if(max_vert < line_length){max_vert = line_length;}
                line_length = 0;
              } else
              {
                line_length++;
              }
            }
          }
          
          { /* Remainder handling of elements < TILE */
      double temp[TILE] = {0};
            int rest = (rmax - RPx - theta) % TILE;
            int const_idx = ub * TILE + theta;
            for(int m = 0; m < dim; ++m)
            {
              int ind = m * lag + RPx;
              double a = TS[ind];
              for(int RPy = 0; RPy < rest; RPy++)
              {
                double b = TS[RPy + ind + const_idx];
                if(temp[RPy] < fabs(a - b)){temp[RPy] = fabs(a - b);} // Maximum distance L_infinity
                //temp[RPy] += (a - b) * (a - b); //Euclidean computation (faster, but hard to estimate threshold) L_2
                //temp[RPy] += fabs(a - b); // Manhattan distance L_1
              }
            }
            
            for(int ti = 0; ti < rest; ++ti) {
              if(temp[ti] > eps)
              {
                if(line_length >= lmin) {
                  points_vert += line_length;
                  count_vert++;}
                if(max_vert < line_length){max_vert = line_length;}
                line_length = 0;
              } else
              {
                line_length++;
              }
            }
          }
          
          if(line_length >= lmin) {
            points_vert += line_length;
            count_vert++;}
          if(max_vert < line_length){max_vert = line_length;}
          line_length = 0;
        }
    }
//======================== Compute RQA =================================
    
    if(theta == 0){
      recurrences = recurrences * 2 - rmax;
      points_diag = points_diag * 2 - rmax;
      count_diag  = count_diag  * 2 - 1;
      RQA[0] = (double) recurrences / (rmax * rmax); //percentage of recurrence points, corresponds to the correlation sum. RR
    } else {
      recurrences = recurrences * 2;
      points_diag = points_diag * 2;
      count_diag  = count_diag  * 2;
      points_vert = points_vert * 2;
      count_vert  = count_vert  * 2;
      RQA[0] = (double) recurrences / (((rmax-theta+1)*(rmax-theta+1))-(rmax-theta+1)); //percentage of recurrence points, corresponds to the correlation sum. RR
    }
      
      
    RQA[1] = (double) points_diag / recurrences; //ratio of all reccurence points and those with diagonals of length lmin and more  DET
    RQA[2] = (double) RQA[0] / RQA[1];  //ratio between DET and RR  RATIO
    RQA[3] = (double) points_diag / count_diag;  //average diagonal line length
    RQA[4] = (double) max_diag;  // longest diagonal line L_max
    RQA[5] = (double) 1/max_diag;  // divergence, inverse of L_max DIV
    RQA[6] = (double) points_vert / recurrences; // ratio of all recurrence points and those with vertical lines of length vmin and more (laminarity) LAM
    RQA[7] = (double) points_vert / count_vert;  // average length of vertical lines (trapping time) TT
    RQA[8] = (double) max_vert;
    RQA[9] = (double) recurrences;
    RQA[10] = (double) points_diag;
    RQA[11] = (double) count_diag;
    RQA[12] = (double) points_vert;
    RQA[13] = (double) count_vert;

}

