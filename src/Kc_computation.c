#include <math.h>
#include <R.h>
#include <Rinternals.h>

/*
vec_x: time series
c: coefficient c (determine series translation)
N: time series length
mean_x2: time series mean on second power
pc, qc: total number of reference points
Mc: number of neighbours for each reference point
Dc: time steps
alpha: noise dampening parameter
*/

void compute_kc(double *vec_x, double *c, int *N, double *mean_x2,
	double *pc, double *qc, double *Mc, double *Dc, double *alpha
	){

double Ma;
int i,j,n_cut;

	pc[0] = vec_x[0];
	qc[0] = vec_x[0];

	for(i = 1; i<*N; i++){
		pc[i] = pc[i-1] + vec_x[i]*cos(*c*(i+1));
		qc[i] = qc[i-1] + vec_x[i]*sin(*c*(i+1));
        }
	n_cut = *N/10;

	for(i = 0; i<n_cut; i++){
		Ma = 0;
		for(j = 0; j<(*N-i); j++){
			Ma += pow((pc[j+(i+1)] - pc[j]), 2) + pow((qc[j+(i+1)] - qc[j]), 2);
		}

		Mc[i] = Ma/(*N-(i+1));
		Dc[i] = Mc[i] - (*mean_x2)*(1 - cos(*c*(i+1)))/(1 - cos(*c));

		if(*alpha>0){
                    Dc[i] = Dc[i] + *mean_x2*sin(sqrt(2)*(i+1))*(*alpha);
                }
	}

}

