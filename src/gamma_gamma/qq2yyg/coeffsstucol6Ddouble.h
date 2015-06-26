#ifndef COEFFSSTUCOL6D_DOUBLE_H
#define COEFFSSTUCOL6D_DOUBLE_H

#ifdef my_float

#if my_float = double
#include "coeffsstucol6D.h"
#else
#error coeffstucol6Ddouble.h included, but my_float is not double
#endif

#else

#define my_float double
#include "coeffsstucol6D.h"
#undef my_float

#endif

#endif
