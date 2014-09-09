#ifndef BOTTOM_FUSION_KINEMATICS_H
#define BOTTOM_FUSION_KINEMATICS_H

#include "kinematics.h"
using namespace std;

/**
 *
 * \class BottomFusionKinematics
 * \brief Manages four momenta and invariants for the current phase space point
 *
 */

template<size_t extraParticles>
class BottomFusionKinematics : public KinematicInvariants
{

public:

    static const size_t minParticles = 3;
    static const size_t randomsN = 2 + 3 * (extraParticles+minParticles-2) - 4;

    /// \name Data members
    /// @{

    /// Jacobian
    double jacobian;
    
    /// Bundle of Four-momenta
    vector<FourVector>& ps = _p;

    /// Return the first Bjorken x
    const double& x1 = _x1;
    /// Return the second Bjorken x
    const double& x2 = _x2;

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    BottomFusionKinematics() :
        KinematicInvariants(), _x1(1.), _x2(1.), _mH2(0.), _tau(0.), jacobian(1.), _p(minParticles+extraParticles)
    {}

    /// Copy constructor
    BottomFusionKinematics(const BottomFusionKinematics& that) :
        KinematicInvariants(that), _x1(that._x1), _x2(that._x2),
        _mH2(that._mH2), _tau(that._tau), jacobian(that._jacobian), _p(that._p)
    {}
    
    /// Destructor
    ~BottomFusionKinematics()
    {}
    
    /// @}

    /// \name Input functions
    /// @{

    /// Four-momenta, conventions are:
    /// - p[1] = parton from hadron 1
    /// - p[2] = parton from hadron 2
    /// - p[3] = Higgs
    /// - others = extra partons in the final state
    FourVector& p(const size_t& i)
    {
        return _p[i-1];
    }

    /// Sets mH2 and the collider S from outside
    void setBoundaries(const double& mH2,const double& S);

    /// Generate kinematics according to random variables
    void generate(const double* const randoms);

    /// @}

    /// \name Output functions
    /// @{

    /// Read-only version of Four-momenta
    const FourVector& p(const size_t& i) const
    {
        return _p[i-1];
    }

    ///Stream operator
    template<size_t extra>
    friend ostream& operator<<(ostream&, const BottomFusionKinematics<extra>&);

    ///@}

protected:
    
    /// \name Data members
    /// @{
    
    double _x1, _x2;    // < Bjorken x variables
    double _mH2;        // < Squared Higgs mass
    double _S;          // < Center of mass collider energy squared
    double _tau;        // < Shorthand for mH^2/S
    vector<FourVector> _p;   // < Momenta of particles

    /// @}

    /// \name Auxiliary functions
    /// @{
    
    /// Generate Bjorken x's
    void generateX(const double* const randoms);
    /// Generate momenta
    void generateP(const double* const randoms);
    
    ///@}
    
};

template<size_t extraParticles>
void BottomFusionKinematics<extraParticles>::setBoundaries(const double& mH2,const double& S)
{
    cout<<"\n[in BottomFusionKinematics] configuration: mH = "<<sqrt(mH2);
    _mH2 = mH2;
    _S = S;
    // definition of tau
    _tau = _mH2 / S;
    cout<<"\t tau="<<_tau<<endl;
}

template<size_t extraParticles>
ostream& operator<<(ostream& stream, const BottomFusionKinematics<extraParticles>& kk)
{
    
    stream << setprecision(16)
    <<"\n-------"
    <<static_cast<KinematicInvariants>(kk)
    <<"\n tau = "<<kk._tau<<","
    <<"\n x1 = "<<kk.x1<<","
    <<"\n x2 = "<<kk.x2<<",";
    for (size_t i = 1; i <= kk.minParticles+extraParticles; ++i)
        stream << "\n p" << i << " = " << kk.p(i) << ",";
    return stream;
}

template<>
inline void BottomFusionKinematics<0>::generateX(const double* const randoms)
{
    // we are at LO so s_12 = mh^2
    // we could generate x_1 in [0,1] and then check that x1>tau
    // instead we generate x1 flat in [tau,1] which is slightly more efficient
    // actually we should generate x1 in a more efficient way
    _x1 = _tau + (1.-_tau) * randoms[0];
    // x2 = tau / x1 always at LO
    _x2 = _tau / _x1 ;
    
    jacobian = (1.-_tau) / _x1;
    
    return;
}

template<size_t extraParticles>
inline void BottomFusionKinematics<extraParticles>::generateX(const double* const randoms)
{
    // dumbest way possible: flat distribution
    const double x1x2 = _tau + (1.-_tau) * randoms[0];
    _x2 = randoms[1] * x1x2;
    
    jacobian = (1.-_tau) * x1x2;
    
    return;
}

template<>
inline void BottomFusionKinematics<0>::generateP(const double* const randoms)
{
    const double E = sqrt(_S)/2.;
    p(1) = x1 * E * FourVector(1., 0., 0.,  1.);
    p(2) = x2 * E * FourVector(1., 0., 0., -1.);
    p(3) = p(1) + p(2);
    return;
}

template<>
inline void BottomFusionKinematics<1>::generateP(const double* const randoms)
{
    // with one extra particle we set
    // p4 = (1-z) [lambdabar p1 + lambda p2 + sqrt(lambda*lambdabar) s12 eperp]
    // (this is covariant and holds in any reference frame if p1, p2 and eperp are transformed)
    // we still define z = mH^2 / s12 with z in (0,1)
    const double E = sqrt(_S)/2.;
    const double phi = 2.*consts::Pi*randoms[0];
    const double lambda = randoms[1];
    const double z = _tau / (x1*x2);
    const double sllbar = sqrt(lambda*(1.-lambda))*x1*x2*E;
    p(1) = x1 * E * FourVector(1., 0., 0.,  1.);
    p(2) = x2 * E * FourVector(1., 0., 0., -1.);
    p(4) = (1.-z)*(
                   (1.-lambda) * p(1) +
                   lambda * p(2) +
                   sllbar * FourVector(0.,cos(phi),sin(phi),0.)
                   );
    p(3) = p(1) + p(2) - p(4);
    return;
}

template<>
inline void BottomFusionKinematics<0>::generate(const double* const randoms)
{
    BottomFusionKinematics<0>::generateX(randoms);
    BottomFusionKinematics<0>::generateP(NULL);
    set(_p);
    return;
}

template<size_t extraParticles>
inline void BottomFusionKinematics<extraParticles>::generate(const double* const randoms)
{
    BottomFusionKinematics<extraParticles>::generateX(randoms);
    BottomFusionKinematics<extraParticles>::generateP(&randoms[2]);
    set(_p);
    return;
}

#endif
