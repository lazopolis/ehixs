/**
 *
 * \file   parametrizations.h
 * \author Simone Lionetti
 * \date   September 2014
 *
 */

#ifndef PARAMETRIZATIONS_H
#define PARAMETRIZATIONS_H

#include "fourvector.h"
#include <vector>
#include "bjorken.h"
using namespace std;


namespace LightCone
{
    const FourVector n    = FourVector(1., 0., 0.,  1.);
    const FourVector nbar = FourVector(1., 0., 0., -1.);

    inline FourVector eperp(const double& phi)
    {
        return FourVector(0.,cos(phi),sin(phi),0.);
    }
};

class PGenerator
{

public:

    PGenerator(Momenta& ps, double& jacobian, const Bjorken& xs) :
    _p(ps), _jacobian(jacobian), _xs(xs), _E(), _m()
    {}

    virtual void setParameters(const double& S, const vector<double>& masses)
    {
        _E = sqrt(S)/2.;
        _m = masses;
        computeConstants();
        cout << "PGenerator configured with E = " << _E << endl;
        return;
    }

    virtual void operator()(const double* const randoms) const
    {
        _p[1] = x1 * _E * LightCone::n;
        _p[2] = x2 * _E * LightCone::nbar;
        generateFSMomenta(randoms);
        return;
    };

    virtual void computeConstants() = 0;
    virtual void generateFSMomenta(const double* const randoms) const = 0;

    virtual size_t N() const = 0;

protected:

    Momenta& _p;               /// < Reference to target momenta to be generated
    double& _jacobian;         /// < Reference to target jacobian

    double _E;                 /// < COM energy of the incoming hadrons
    const Bjorken& _xs;        /// < Where to read Bjorken xs
    vector<double> _m;         /// < Masses (beware: counting from 0)

    const double& x1 = _xs.x1; /// < Shorthand for the 1st Bjorken x
    const double& x2 = _xs.x2; /// < Shorthand for the 2nd Bjorken x

};

class deltaPG : public PGenerator
{
public:

    deltaPG(Momenta& ps, double& jacobian, const Bjorken& xs) :
    PGenerator(ps, jacobian, xs)
    {}

    void computeConstants() {}

    void generateFSMomenta(const double* const randoms) const
    {
        _p[3] = _p[1] + _p[2];
        return;
    }

    size_t N() const {return 3;}
};

class zlambda : public PGenerator
{

public:

    zlambda(Momenta& ps, double& jacobian, const Bjorken& xs) :
    PGenerator(ps, jacobian, xs)
    {}

    void computeConstants()
    {
        _tau = _m[0]*_m[0]/_E;
        return;
    }

    void generateFSMomenta(const double* const randoms) const
    {
        // with one extra particle we set
        // p4 = (1-z) [lambdabar p1 + lambda p2 + sqrt(lambda*lambdabar) s12 eperp]
        // (this is covariant and holds in any reference frame if p1, p2 and eperp are transformed)
        // we still define z = mH^2 / s12 with z in (0,1)
        const double phi = 2.*consts::Pi*randoms[0];
        const double lambda = randoms[1];
        const double z = _tau / (x1*x2);
        const double sllbar = sqrt(lambda*(1.-lambda))*x1*x2*_E;
        _p[4] = (1.-z)*(
                        (1.-lambda) * _p[1] +
                        lambda * _p[2] +
                        sllbar * LightCone::eperp(phi)
                       );
        _p[3] = _p[1] + _p[2] - _p[4];
        // nothing to be done for jacobian = 1
        return;
    }

private:

    double _tau;

};

#endif
