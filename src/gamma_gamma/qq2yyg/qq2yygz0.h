/**
 *
 * \file    qq2yygz0.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

////////////////////////////////////////////////////////////////////////////////
// This file has no header guard because it should NOT be #included directly! //
////////////////////////////////////////////////////////////////////////////////

#ifndef my_float
#error  qq2yygz0.h included without defining my_float!
#else

// Consider moving into .cpp, beware of nontrivial recursive inclusions though
#include "boxmaster.h" // box, bubble
#include "coeffsz0/coeffz0LC.h"
#include "coeffsz0/coeffz0SC.h"
#include "coeffsz0/coeffz0Nf.h"

// Functions with the whole epsilon expansion do not include regularization
// of large numerical cancellations inside the phase space

/**
 * \fn    qq2yygz0LCbub
 */

double qq2yygz0LCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz0LCbox
 */

double qq2yygz0LCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz0SCbub
 */

double qq2yygz0SCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz0SCbox
 */

double qq2yygz0SCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz0Nfbub
 */

double qq2yygz0Nfbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz0Nfbox
 */

double qq2yygz0Nfbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz0col
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yygz0col(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygz0LCbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygz0LCbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz0LCboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygz0LCboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz0SCbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygz0SCbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz0SCboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygz0SCboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz0Nfbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygz0Nfbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

/**
 * \fn    qq2yygz0Nfboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygz0Nfboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );


/**
 * \fn    qq2yygz0colexp
 * \brief Epsilon expansion of the qq->yyg matrix element
 */

Expansion<Parameter::epsilon, double> qq2yygz0colexp(
                                                     const my_float& s13,
                                                     const my_float& s14,
                                                     const my_float& s23,
                                                     const my_float& s24
                                                     );

#endif
