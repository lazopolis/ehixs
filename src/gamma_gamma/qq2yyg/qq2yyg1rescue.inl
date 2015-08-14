/**
 *
 * \file    qq2yyg1rescue.inl
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

// Inline functions calling qq2yyg1_rescue's methods

// qq2yyg1

inline double qq2yyg1_rescue::eval(const PSpoint& p, const int i)
{
    return LC::eval(p,i)+SC::eval(p,i)+Nf::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::eval(const PSpoint& p)
{
    return LC::eval(p)+SC::eval(p)+Nf::eval(p);
}

// LC

inline double qq2yyg1_rescue::LC::eval(const PSpoint& p, const int i)
{
    return bub::eval(p,i)+box::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::LC::eval(const PSpoint& p)
{
    return bub::eval(p)+box::eval(p);
}

inline double qq2yyg1_rescue::LC::bub::eval(const PSpoint& p, const int i, const bool taylor)
{
    // Inefficient, but _checkpoles requires all terms to catch exceptional kinematics
    return eval(p,taylor).getCoefficient(i);
}

// SC

inline double qq2yyg1_rescue::SC::eval(const PSpoint& p, const int i)
{
    return bub::eval(p,i)+box::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::SC::eval(const PSpoint& p)
{
    return bub::eval(p)+box::eval(p);
}

inline double qq2yyg1_rescue::SC::bub::eval(const PSpoint& p, const int i, const bool taylor)
{
    // Inefficient, but _checkpoles requires all terms to catch exceptional kinematics
    return eval(p,taylor).getCoefficient(i);
}

// Inline functions calling qq2yyg1<dbl>'s methods

// LC box

inline double qq2yyg1_rescue::LC::box::eval(const PSpoint& p, const int i)
{
    return qq2yyg1<dbl>::LC::box::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::LC::box::eval(const PSpoint& p)
{
    return qq2yyg1<dbl>::LC::box::eval(p);
}

// SC box

inline double qq2yyg1_rescue::SC::box::eval(const PSpoint& p, const int i)
{
    return qq2yyg1<dbl>::SC::box::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::SC::box::eval(const PSpoint& p)
{
    return qq2yyg1<dbl>::SC::box::eval(p);
}

// Nf

inline double qq2yyg1_rescue::Nf::eval(const PSpoint& p, const int i)
{
    return qq2yyg1<dbl>::Nf::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::Nf::eval(const PSpoint& p)
{
    return qq2yyg1<dbl>::Nf::eval(p);
}

// Nf bub

inline double qq2yyg1_rescue::Nf::bub::eval(const PSpoint& p, const int i)
{
    return qq2yyg1<dbl>::Nf::bub::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::Nf::bub::eval(const PSpoint& p)
{
    return qq2yyg1<dbl>::Nf::bub::eval(p);
}

// Nf box

inline double qq2yyg1_rescue::Nf::box::eval(const PSpoint& p, const int i)
{
    return qq2yyg1<dbl>::Nf::box::eval(p,i);
}

inline EpsExp qq2yyg1_rescue::Nf::box::eval(const PSpoint& p)
{
    return qq2yyg1<dbl>::Nf::box::eval(p);
}
