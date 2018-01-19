#include <math.h>

/*
TS: time series
c: coefficient c (determine series translation)
N: time series length
mean_x2: time series mean on second power
pc, qc: total number of reference points
Mc: number of neighbours for each reference point
Dc: time steps
alpha: noise dampening parameter
*/

void compute_kc(double *TS, double *c, int *N, double *mean_x2,
	double *pc, double *qc, double *Mc, double *Dc, double *alpha
	){

double Ma;
int i,j,n_cut;

	pc[0] = TS[0]*cos(*c);
	qc[0] = TS[0]*sin(*c);

	for(i = 1; i<(*N); i++){
		pc[i] = pc[i-1] + TS[i]*cos(*c*(i+1));
		qc[i] = qc[i-1] + TS[i]*sin(*c*(i+1));
        }
	n_cut = *N/10;

	for(i = 1; i<(n_cut+1); i++){
		Ma = 0;
		for(j = 0; j<(*N-i); j++){
			Ma += pow((pc[j+i] - pc[j]), 2) + pow((qc[j+i] - qc[j]), 2);
		}

		Mc[i-1] = Ma/(*N-(i+1));
		Dc[i-1] = Mc[i-1] - (*mean_x2)*(1 - cos(*c*(i+1)))/(1 - cos(*c));

		if(*alpha>0){
                    Dc[i-1] = Dc[i-1] + *mean_x2*sin(sqrt(2)*(i+1))*(*alpha);
                }
	}

}

