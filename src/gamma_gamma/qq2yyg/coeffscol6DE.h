/**
 *
 * \file    coeffscol6DE.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#ifndef COEFFSCOL6DE_H
#define COEFFSCOL6DE_H

#include "dircol6DE/coeff6ELC.h"
#include "dircol6DE/coeff6ESC.h"

/**
 * \fn    qq2yyg6ELCbub
 */

double qq2yyg6ELCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6ELCbox
 */

double qq2yyg6ELCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6ESCbub
 */

double qq2yyg6ESCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6ESCbox
 */

double qq2yyg6ESCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6Ecol
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yyg6Ecol(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif
