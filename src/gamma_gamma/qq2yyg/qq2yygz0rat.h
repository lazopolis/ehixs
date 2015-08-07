#ifndef QQ2YYG_Z0_RAT_H
#define QQ2YYG_Z0_RAT_H

#ifdef my_float

#if my_float = cl_RA
#include "cln.h"
#include "qq2yygz0.h"
#else
#error qq2yygz0rat.h included, but my_float is not cl_RA
#endif

#else

#define my_float cl_RA
#include "cln.h"
#include "qq2yygz0.h"
#undef my_float

#endif

#endif
