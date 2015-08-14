/**
 *
 * \file    bb2HX.h
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#ifndef BB2HX_H
#define BB2HX_H

#include "expansion.h" // EpsExp

/// \todo Move this to either Constants, UserInterface, Model or whatever
constexpr double yukawa_bottom = 1.0;
constexpr double alphas = 1.0;

/// \brief Matrix elements for bb->H
template<size_t loop, int eps>
double bb2H();

/// \brief Matrix elements for bb->Hg
/// \note  These matrix elements are already multiplied by the (1-z) in the phase space
template<size_t loop, int eps>
double bb2Hg(const double& z, const double& lambda);

/// \brief Shorthand for full epsilon expansion of bb->H
template<size_t loop>
EpsExp bb2H();

/// \brief Full epsilon expansion of bb->H, implementation with Bubble
template<size_t loop>
EpsExp bb2Hbis();

/// \brief Shorthand for full epsilon expansion of bb->Hg
template<size_t loop>
EpsExp bb2Hg(const double& z, const double& lambda);

#endif
