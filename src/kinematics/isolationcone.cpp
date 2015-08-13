/**
 *
 * \file    isolationcone.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "isolationcone.h"

/// \fn    Rdist

double Rdist(const FourVector& p1, const FourVector& p2)
{
    return sqrt(pow(p1.eta()-p2.eta(),2)+pow(p1.phi()-p2.phi(),2));
}
