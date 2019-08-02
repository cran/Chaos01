#include <math.h>

/*
TS: time series
width: rolling mean width
N: time series length
out: output vector
*/

void myrollmean(double *TS, int *width, int *N, double *out
	){
    double temp = 0;
    int i, counter = 0;
    
    for(i = 0; i < *width; i++) {
      temp += TS[i];
    }
    out[0] = temp / *width;
    counter += 1;
    for( i = *width; i < *N; i++ ) {
      temp = temp - TS[i - *width] + TS[i];
      out[counter] = temp / *width;
      counter += 1;
    }

}

