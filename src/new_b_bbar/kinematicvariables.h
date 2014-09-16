/**
 * \file    kinematicvariables.h
 * \author  Simone Lionetti
 * \date    September 2014
 */

#ifndef KINEMATIC_VARIABLES_H
#define KINEMATIC_VARIABLES_H

#include "kinematics.h"
#include "bjorken.h"
#include "parametrizations.h"
using namespace std;

/**
 *
 * \class KinematicVariables
 * \brief Manages generic collider kinematic variables
 * \note  Should the name be changed to ColliderKinematics?
 * This class contains four momenta, invariants and Bjorken xs for the current phase space point
 * along with the information needed to generate them from Vegas
 *
 */

template<class xGenerator, class pGenerator>
class KinematicVariables : public KinematicInvariants
{

public:

    /// \name Data members
    /// @{

    /// Return the first Bjorken x
    const double& x1 = _x.x1;
    /// Return the second Bjorken x
    const double& x2 = _x.x2;

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    KinematicVariables() :
    KinematicInvariants(), _xGen(_x, _jacobian), _pGen(_p, _jacobian, _x)
    {}

    /// Copy constructor
    //KinematicVariables(const KinematicVariables& that) :
    //KinematicInvariants(that), _S(that._S), _m(that._m), _x(that._x), _p(that._p)
    //{}
    
    /// Destructor
    ~KinematicVariables()
    {}
    
    /// @}

    /// \name Input/output functions
    /// @{

    /// Minimum number of final state particles
    //virtual const size_t minFSparticles(void) const = 0;

    /// Number of extra final state partons from real emission
    //virtual const size_t realEmissions(void) const = 0;

    /// Total number of final state particles
    //const size_t nFS(void) const
    //{
    //    return minFSparticles()+realEmissions();
    //}

    /// Number of degrees of freedom
    //const size_t nDOF(void) const
    //{
    //    return 2 + 3 * nFS() - 4;
    //}

    /// Four-momenta, conventions are:
    /// - p[1] = parton from hadron 1
    /// - p[2] = parton from hadron 2
    /// - p[i>2] = particles in the final state
    const Momenta& p = _p;

    /// Generate kinematics according to random variables
    virtual void generate(const double* const randoms);

    /// Global jacobian
    const double& jacobian = _jacobian;

    /// Setting parameters
    virtual void setParameters(const double& S, const vector<double>& m)
    {
        cout << "KinematicVariables configured with S = " << S << endl;
        _S = S;
        _m = m;
        double sumOfMasses = 0.;
        for (vector<double>::const_iterator it = _m.begin(); it < _m.end(); ++it)
            sumOfMasses = (*it)*(*it);
        _xGen.setParameters(sumOfMasses/_S);
        _pGen.setParameters(_S,_m);
        _p.resize(_pGen.N());
        return;
    }

    /// @}

protected:
    
    /// \name Data members
    /// @{
    
    double _S;          /// < Center of mass collider energy squared
    vector<double> _m;  /// < Minimum value of the ratio smin/S = x1*x2
    Bjorken _x;         /// < Bjorken x variables
    Momenta _p;         /// < Momenta of particles
    double _jacobian;   /// < Jacobian
    xGenerator _xGen;
    pGenerator _pGen;

    /// @}

    /// \name Auxiliary functions
    /// @{
    
    ///@}
    
};

template <class xGenerator, class pGenerator>
inline void KinematicVariables<xGenerator,pGenerator>::generate(const double* const randoms)
{
    _jacobian = 1.;
    _xGen(randoms);
    _pGen(&randoms[_xGen.Nran()]);
    set(_p);
    return;
}


/**
 *
 * \class DeltaKinematics
 * \brief Collider kinematic variables for delta-like processes
 *
 * Manages kinematics for processes with one and only one final-state particle
 * This information is sufficient to completely determine the momenta
 *
 */

//template<class xGenerator>
//class DeltaKinematics : public KinematicVariables<xGenerator,deltaPG>
//{
//
//public:
//
//    /// \name Constructors and destructor
//    /// @{
//
//    /// Default constructor
//    DeltaKinematics() :
//    KinematicVariables()
//    {}
//
//    /// Copy constructor
//    DeltaKinematics(const DeltaKinematics& that) :
//    KinematicVariables(that)
//    {}
//
//    /// Destructor
//    ~DeltaKinematics()
//    {}
//
//    /// @}
//
//    /// \name Input/output functions
//    /// @{
//
//    /// Minimum number of final state particles
//    const size_t minFSparticles(void) const
//    {
//        return 3;
//    }
//
//    /// Number of extra final state partons from real emission
//    const size_t realEmissions(void) const
//    {
//        return 0;
//    }
//
//    /// Generate kinematics according to random variables
//    void generate(const double* const randoms);
//
//    /// @}
//
//};
//
//template<class xGenerator>
//inline void DeltaKinematics<xGenerator>::generate(const double* const randoms)
//{
//    const Generated<Bjorken> fooX = xGenerator(_smin_S,randoms);
//    _x = fooX.value;
//    const Generated<Momenta> fooP = pGenerator(NULL);
//    _p = fooP.value;
//    set(_p);
//    _jacobian = fooX.jacobian * fooP.jacobian;
//    return;
//}

#endif
