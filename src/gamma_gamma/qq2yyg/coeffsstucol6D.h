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
 * \fn    qq2yygstu6col
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yygstu6col(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24);

#endif
