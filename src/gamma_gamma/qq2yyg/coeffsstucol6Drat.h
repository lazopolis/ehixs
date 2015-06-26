#ifndef COEFFSSTUCOL6D_RAT_H
#define COEFFSSTUCOL6D_RAT_H

#ifdef my_float

#if my_float = cl_RA
#include "cln.h"
#include "coeffsstucol6D.h"
#else
#error coeffstucol6Drat.h included, but my_float is not cl_RA
#endif

#else

#define my_float cl_RA
#include "cln.h"
#include "coeffsstucol6D.h"
#undef my_float

#endif

#endif
