/**
 *
 * \file    lightcone.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    October 2014
 *
 */

#ifndef KINEMATICS_LIGHTCONE_H
#define KINEMATICS_LIGHTCONE_H

#include "fourvector.h"

/**
 *
 * \namespace LightCone
 * \brief     Contains functions and definitions for light-cone coordinates
 * \todo      Complete with functions that return the plus, minus and perp components
 *
 */

namespace LightCone
{
    const FourVector n    = FourVector(1., 0., 0.,  1.);
    const FourVector nbar = FourVector(1., 0., 0., -1.);

    inline FourVector eperp(const double& phi)
    {
        return FourVector(0.,cos(phi),sin(phi),0.);
    }
};

#endif
