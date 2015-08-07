/**
 *
 * \file    qq2yygz1.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

////////////////////////////////////////////////////////////////////////////////
// This file has no header guard because it should NOT be #included directly! //
////////////////////////////////////////////////////////////////////////////////

#ifndef my_float
#error  qq2yygz1.h included without defining my_float!
#else

// Consider moving into .cpp, beware of nontrivial recursive inclusions though
#include "boxmaster.h" // box, bubble
#include "coeffsz1/coeffz1LC.h"
#include "coeffsz1/coeffz1SC.h"
#include "coeffsz1/coeffz1Nf.h"

// Functions with the whole epsilon expansion do not include regularization
// of large numerical cancellations inside the phase space

/**
 * \fn    qq2yygz1LCbub
 */

double qq2yygz1LCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz1LCbox
 */

double qq2yygz1LCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz1SCbub
 */

double qq2yygz1SCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz1SCbox
 */

double qq2yygz1SCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz1Nfbub
 */

double qq2yygz1Nfbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz1Nfbox
 */

double qq2yygz1Nfbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz1col
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yygz1col(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz1LCbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygz1LCbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz1LCboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygz1LCboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz1SCbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygz1SCbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz1SCboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygz1SCboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz1Nfbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygz1Nfbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz1Nfboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygz1Nfboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );


/**
 * \fn    qq2yygz1colexp
 * \brief Epsilon expansion of the qq->yyg matrix element
 */

Expansion<Parameter::epsilon, double> qq2yygz1colexp(
                                                     const my_float& s13,
                                                     const my_float& s14,
                                                     const my_float& s23,
                                                     const my_float& s24
                                                     );

#endif
