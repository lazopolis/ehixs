/**
 *
 * \file    bb2HRVcoeffs.h
 * \ingroup new_b_bbar
 * \author  Simone Lionetti
 * \date    February 2015
 *
 */

#ifndef BB2HRVCOEFFS_H
#define BB2HRVCOEFFS_H

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

/**
 * \fn    bb2Hgbis
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, CA piece
 */

template<size_t master, int eps>
double bb2Hgbis(
                const double& s12,
                const double& s14,
                const double& s24
                );

/**
 * \fn    bb2Hgbis
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, CA piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> bb2Hgbis(
                                               const double& s12,
                                               const double& s14,
                                               const double& s24
                                               );

/**
 * \fn    bb2Hgbis
 * \brief Order epsilon^0 part of the qq->gammagamma+g matrix element
 */

double bb2Hgbis(const double& s12, const double& s14, const double& s24);

#endif
