#ifndef BOTTOM_FUSION_KINEMATICS_H
#define BOTTOM_FUSION_KINEMATICS_H

#include "kinematics.h"
#include "fourmomentum.h"
//#include <sstream>      // std::stringstream
using namespace std;

/**
 *
 * \class BottomFusionKinematics
 * \brief Manages four momenta and invariants for the current phase space point
 *
 */

template<size_t extraParticles> class BottomFusionKinematics : public KinematicInvariants
{

public:

    static const size_t minParticles = 3;
    static const size_t randomsN = 2 + 3 * (extraParticles+minParticles-2) - 4;

    /// \name Data members
    /// @{

    /// Jacobian
    double jacobian;
    
    /// Four-momenta, conventions are:
    /// - p[1] = parton from hadron 1
    /// - p[2] = parton from hadron 2
    /// - p[3] = Higgs
    /// - others = extra partons in the final state
    vector<FMomentum> p;

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    BottomFusionKinematics() :
        KinematicInvariants(), _x1(1.), _x2(1.), _mH2(0.), _tau(0.), jacobian(1.), p()
    {}

    /// @}

    /// \name Input functions
    /// @{

    /// Sets the mh_sq and the collider S from outside
    void setBoundaries(const double& mh_sq,const double& S);

    /// Generate kinematics according to random variables
    void generate(const double* const randoms);

    /// @}

    /// \name Output functions
    /// @{

    /// Return the first Bjorken x
    double x1() const {return _x1;}
    /// Return the second Bjorken x
    double x2() const {return _x2;}

    ///Stream operator
    friend ostream& operator<<(ostream&, const BottomFusionKinematics&);

    ///@}

protected:
    
    /// \name Data members
    /// @{
    
    double _x1, _x2;    // < Bjorken x variables
    double _mH2;        // < Squared Higgs mass
    double _S;          // < Center of mass collider energy squared
    double _tau;        // < Shorthand for mH^2/S
    
    /// @}

    /// \name Auxiliary functions
    /// @{
    
    /// Generate Bjorken x's
    void generate_x(const double* const randoms);
    /// Generate momenta
    void generate_p(const double* const randoms);
    
    ///Stream operator
    friend ostream& operator<<(ostream&, const BottomFusionKinematics&);
    
    ///@}
    
};

template<size_t extraParticles>
void BottomFusionKinematics<extraParticles>::setBoundaries(const double& mh_sq,const double& S)
{
    cout<<"\n[in BottomFusionKinematics] configuration: mh_sq = "<<mh_sq;
    _mH2 = mh_sq;
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
    <<"\n tau = "<<kk.tau_<<","
    <<"\n x1 = "<<kk.x1()<<","
    <<"\n x2 = "<<kk.x2()<<",";
    for (size_t i = 0; i < kk.minParticles()+extraParticles; ++i)
        stream << "\n p" << i << " = " << kk.p[i] << ",";
    return stream;
}

template<>
void BottomFusionKinematics<0>::generate_x(const double* const randoms)
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
void BottomFusionKinematics<extraParticles>::generate_x(const double* const randoms)
{
    // dumbest way possible: flat distribution
    const double x1x2 = _tau + (1.-_tau) * randoms[0];
    _x2 = randoms[1]*x1x2;
    
    jacobian = 2.*consts::Pi*(1.-_tau) / _x1;
    
    return;
}

template<>
void BottomFusionKinematics<0>::generate_p(const double* const randoms)
{
    const double E = sqrt(_S)/2.;
    p[1] = FMomentum( _x1 * E, 0., 0.,  _x1 * E);
    p[2] = FMomentum( _x2 * E, 0., 0., -_x2 * E);
    p[3] = p[1]+p[2];
    return;
}

template<>
void BottomFusionKinematics<1>::generate_p(const double* const randoms)
{
    // with one extra particle and p4 = (1-z) [lambdabar p1 + lambda p2 + sqrt(lambda*lambdabar) s12 eperp]
    // (in the CM frame) we still define z = mH^2 / s12 with z in (0,1)
    const double E = sqrt(_S)/2.;
    const double phi = randoms[0];
    p[1] = FMomentum( _x1 * E, 0., 0.,  _x1 * E);
    p[2] = FMomentum( _x2 * E, 0., 0., -_x2 * E);
    p[3] = p[1]+p[2];
    p[4] = p[1];
    return;
    
}

template<>
void BottomFusionKinematics<0>::generate(const double* const randoms)
{
    BottomFusionKinematics<0>::generate_x(randoms);
    BottomFusionKinematics<0>::generate_p(NULL);
    static_cast<KinematicInvariants> (*this) = p;
    return;
}

template<size_t extraParticles>
void BottomFusionKinematics<extraParticles>::generate(const double* const randoms)
{
    BottomFusionKinematics<extraParticles>::generate_x(randoms);
    BottomFusionKinematics<extraParticles>::generate_p(&randoms[2]);
    static_cast<KinematicInvariants> (*this) = p;
    return;
}

#endif
