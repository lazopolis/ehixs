/**
 *
 * \file    coeffsstucol6D.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

////////////////////////////////////////////////////////////////////////////////
// This file has no header guard because it should NOT be #included directly! //
////////////////////////////////////////////////////////////////////////////////

#ifndef my_float
#error  coeffsstucol6D.h included without defining my_float!
#else

// Consider moving into .cpp, beware of nontrivial recursive inclusions though
#include "boxmaster.h" // box, bubble
#include "dirstucol6D/coeffstu6LC.h"
#include "dirstucol6D/coeffstu6SC.h"
#include "dirstucol6D/coeffstu6Nf.h"

// Functions with the whole epsilon expansion do not include regularization
// of large numerical cancellations inside the phase space

/**
 * \fn    qq2yygstu6LCbub
 */

double qq2yygstu6LCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygstu6LCbox
 */

double qq2yygstu6LCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygstu6SCbub
 */

double qq2yygstu6SCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygstu6SCbox
 */

double qq2yygstu6SCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygstu6Nfbub
 */

double qq2yygstu6Nfbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygstu6Nfbox
 */

double qq2yygstu6Nfbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygstu6col
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yygstu6col(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

/**
 * \fn    qq2yygstu6LCbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygstu6LCbubexp(
                                                         const my_float& s13,
                                                         const my_float& s14,
                                                         const my_float& s23,
                                                         const my_float& s24
                                                         );

/**
 * \fn    qq2yygstu6LCboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygstu6LCboxexp(
                                                         const my_float& s13,
                                                         const my_float& s14,
                                                         const my_float& s23,
                                                         const my_float& s24
                                                         );

/**
 * \fn    qq2yygstu6SCbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygstu6SCbubexp(
                                                         const my_float& s13,
                                                         const my_float& s14,
                                                         const my_float& s23,
                                                         const my_float& s24
                                                         );

/**
 * \fn    qq2yygstu6SCboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygstu6SCboxexp(
                                                         const my_float& s13,
                                                         const my_float& s14,
                                                         const my_float& s23,
                                                         const my_float& s24
                                                         );

/**
 * \fn    qq2yygstu6Nfbubexp
 */

Expansion<Parameter::epsilon, double> qq2yygstu6Nfbubexp(
                                                         const my_float& s13,
                                                         const my_float& s14,
                                                         const my_float& s23,
                                                         const my_float& s24
                                                         );

/**
 * \fn    qq2yygstu6Nfboxexp
 */

Expansion<Parameter::epsilon, double> qq2yygstu6Nfboxexp(
                                                         const my_float& s13,
                                                         const my_float& s14,
                                                         const my_float& s23,
                                                         const my_float& s24
                                                         );


/**
 * \fn    qq2yygstu6colexp
 * \brief Epsilon expansion of the qq->yyg matrix element
 */

Expansion<Parameter::epsilon, double> qq2yygstu6colexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       );

#endif
