/*
	cuba.h
		Prototypes for the Cuba library
		this file is part of Cuba
		last modified 16 Jun 10 th
*/

#ifdef __cplusplus
extern "C" {
#endif

typedef int (*integrand_t)(const int *ndim, const double x[],
  const int *ncomp, double f[], void *userdata);

typedef void (*peakfinder_t)(const int *ndim, const double b[],
  int *n, double x[]);

void Vegas(const int ndim, const int ncomp,
  integrand_t integrand, void *userdata,
  const double epsrel, const double epsabs,
  const int flags, const int seed,
  const int mineval, const int maxeval,
  const int nstart, const int nincrease, const int nbatch,
  const int gridno, const char *statefile,
  int *neval, int *fail,
  double integral[], double error[], double prob[]);

void llVegas(const int ndim, const int ncomp,
  integrand_t integrand, void *userdata,
  const double epsrel, const double epsabs,
  const int flags, const int seed,
  const long long int mineval, const long long int maxeval,
  const long long int nstart, const long long int nincrease,
  const long long int nbatch,
  const int gridno, const char *statefile,
  long long int *neval, int *fail,
  double integral[], double error[], double prob[]);

#ifdef __cplusplus
}
#endif

