/**
 *
 * \file    coeffscolor.h
 * \ingroup new_b_bbar
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#ifndef COEFFSCOLOR_H
#define COEFFSCOLOR_H

/**
 * \fn    bb2HgLC
 * \brief Master coefficients for qq->Hg at one loop, power by power in epsilon, leading color
 */

template<size_t master, int eps>
double bb2HgLC(
              const double& s12,
              const double& s14,
              const double& s24
              );

/**
 * \fn    bb2HgLC
 * \brief Master coefficients for qq->Hg at one loop, series in epsilon, leading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> bb2HgLC(
                const double& s12,
                const double& s14,
                const double& s24
                );

/**
 * \fn    bb2HgSC
 * \brief Master coefficients for qq->Hg at one loop, power by power in epsilon, subleading color
 */

template<size_t master, int eps>
double bb2HgSC(
                const double& s12,
                const double& s14,
                const double& s24
                );

/**
 * \fn    bb2HgSC
 * \brief Master coefficients for qq->Hg at one loop, series in epsilon, subleading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> bb2HgSC(
                const double& s12,
                const double& s14,
                const double& s24
                );

/**
 * \fn    bb2HgLcol
 * \brief Order epsilon^0 part of the leading color qq->Hg matrix element
 */

double bb2HgLcol(
                const double& s12,
                const double& s14,
                const double& s24
                );

/**
 * \fn    bb2HgScol
 * \brief Order epsilon^0 part of the qq->Hg subleading color matrix element
 */

double bb2HgScol(
                const double& s12,
                const double& s14,
                const double& s24
                );

/**
 * \fn    bb2Hgcol
 * \brief Order epsilon^0 part of the qq->Hg matrix element
 */

double bb2Hgcol(
                const double& s12,
                const double& s14,
                const double& s24
                );

#endif
