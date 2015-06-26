/**
 *
 * \file    coeffsstucol6Dnobar.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef COEFFSSTUCOL6DNOBAR_H
#define COEFFSSTUCOL6DNOBAR_H

#include "dirstucol6Dnobar/coeffstu6LCnobar.h"
#include "dirstucol6Dnobar/coeffstu6SCnobar.h"

/**
 * \fn    qq2yygstu6LCnobarbub
 */

double qq2yygstu6LCnobarbub(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygstu6LCnobarbox
 */

double qq2yygstu6LCnobarbox(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygstu6SCnobarbub
 */

double qq2yygstu6SCnobarbub(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygstu6SCnobarbox
 */

double qq2yygstu6SCnobarbox(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygstu6colnobar
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yygstu6colnobar(const double& s13, const double& s14, const double& s23, const double& s24);

#endif
