#ifndef VIRTUAL_AMPLITUDE_H
#define VIRTUAL_AMPLITUDE_H


#include "kinematic_variables.h"
#include<vector>
#include <complex>
#include "chaplin.h"
#include "epsilon_series.h"
using namespace std;



class VirtualAmplitude{
public:
    double Evaluate(const KinematicInvariants& kv) const;
    double EpsilonM1(const KinematicInvariants& kv) const;
    double EpsilonM2(const KinematicInvariants& kv) const;
    double EpsilonM3(const KinematicInvariants& kv) const;
    double EpsilonP1(const KinematicInvariants& kv) const;
    double Epsilon0(const KinematicInvariants& kv) const ;

    double Master(int i,const KinematicInvariants& kv,int eps) const;
    double Coefficient(int i,const KinematicInvariants& kv,int eps) const;
    
    PolynomialEpsilon Master(int i,int eps);
    void SetMu(const double mu){mu_=mu;}
protected:
    PolynomialEpsilon em3_;
    PolynomialEpsilon em2_;
    PolynomialEpsilon em1_;
    PolynomialEpsilon e0_;
    PolynomialEpsilon ep1_;
    PolynomialEpsilon log_pieces_;
    vector<PolynomialEpsilon> II;
    vector<PolynomialEpsilon> cc;
    double mu_;
};

class GstarVirtual: public VirtualAmplitude
{
public:
    GstarVirtual();
    
};

class GstarVirtualVirtual : public VirtualAmplitude
{
public:
    GstarVirtualVirtual();
};
#endif