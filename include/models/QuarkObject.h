#ifndef PARTICLEOBJECT_H
#define PARTICLEOBJECT_H

#include<complex>
using namespace std;

class ParticleObject
{
public:
	ParticleObject(){};
	~ParticleObject(){};
	ParticleObject(double,double,double,double);
	double mq_nominal;
	complex<double> mq_sq;
	complex<double> Wq;
	complex<double> Xq;
	double Gq;
	double Yq;
private:
	
	
};


#endif