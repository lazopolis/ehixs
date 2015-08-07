#ifndef QQ2YYG_Z1_QUAD_H
#define QQ2YYG_Z1_QUAD_H

#ifdef my_float

#if my_float = __float128
#include <quadmath.h>
#include "qq2yygz1.h"
#else
#error qq2yygz1quad.h included, but my_float is not __float128
#endif

#else

#define my_float __float128
#include <quadmath.h>
#include "qq2yygz1.h"
#undef my_float

#endif

#endif
