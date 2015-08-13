/**
 *
 * \file    isolationcone.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef KINEMATICS_ISOLATIONCONE_H
#define KINEMATICS_ISOLATIONCONE_H

#include "fourvector.h"

/**
 *
 * \fn    Rdist
 * \brief Distance in eta-phi plane of two particles
 *
 */

double Rdist(const FourVector& p1, const FourVector& p2);

/**
 *
 * \class SmoothPhotonIsolation
 * \brief This class implements an smooth isolation cone as in arXiv:9801442
 *
 */

class SmoothPhotonIsolation
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    SmoothPhotonIsolation(const double& delta0 = 0.5, const double& n = 1., const double& epsilongamma = 1.) :
    _delta0(delta0), _n(n), _epsilongamma(epsilongamma)
    {}

    /// Destructor
    ~SmoothPhotonIsolation()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Criterion
    /// \note 1 parton only, needs to be rewritten for more (overload const Momenta& as 2nd arg?)
    bool inside(const FourVector& pgamma, const FourVector& pparton) const
    {
        const double Rig = Rdist(pgamma,pparton);
        return Rig<_delta0 && pparton[0]<Chi(pgamma[0],Rig);
    }

    /// @}

private:

    /// \name Data members
    /// @{

    const double _delta0;       ///< Cone angle
    const double _n;            ///< Exponent
    const double _epsilongamma; ///< Energy fraction

    /// @}

    /// \name Auxiliary functions
    /// @{

    /// This is Frixione's Chi function eq. (3.4)
    /// Modified to contain the minimum in eq. (3.10)
    double Chi(const double& Egamma, const double& delta) const
    {
        if (delta<_delta0) return Egamma*_epsilongamma;
        return Egamma*_epsilongamma*pow((1.-cos(delta))/(1.-cos(_delta0)),_n);
    }
    
    /// @}
    
};

#endif
