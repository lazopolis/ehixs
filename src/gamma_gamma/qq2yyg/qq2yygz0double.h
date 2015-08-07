#ifndef QQ2YYG_Z0_DOUBLE_H
#define QQ2YYG_Z0_DOUBLE_H

#ifdef my_float

#if my_float = double
#include "qq2yygz0.h"
#else
#error qq2yygz0double.h included, but my_float is not double
#endif

#else

#define my_float double
#include "qq2yygz0.h"
#undef my_float

#endif

#endif
