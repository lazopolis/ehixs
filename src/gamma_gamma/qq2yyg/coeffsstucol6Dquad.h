#ifndef COEFFSSTUCOL6D_QUAD_H
#define COEFFSSTUCOL6D_QUAD_H

#ifdef my_float

#if my_float = __float128
#include <quadmath.h>
#include "coeffsstucol6D.h"
#else
#error coeffstucol6Dquad.h included, but my_float is not __float128
#endif

#else

#define my_float __float128
#include <quadmath.h>
#include "coeffsstucol6D.h"
#undef my_float

#endif

#endif
