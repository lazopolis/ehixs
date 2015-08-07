#ifndef QQ2YYG_Z1_DOUBLE_H
#define QQ2YYG_Z1_DOUBLE_H

#ifdef my_float

#if my_float = double
#include "qq2yygz1.h"
#else
#error qq2yygz1double.h included, but my_float is not double
#endif

#else

#define my_float double
#include "qq2yygz1.h"
#undef my_float

#endif

#endif
