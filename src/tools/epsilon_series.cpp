#include "epsilon_series.h"

MonomialEpsilon operator*(
                          MonomialEpsilon lhs,
                          const MonomialEpsilon& rhs
                          )
{
    for (size_t i=0;i<rhs.funcs_.size();i++)
    {
        lhs.funcs_.push_back(rhs.funcs_[i]);
    }
    lhs.epsilon_ += rhs.epsilon_;
    return lhs;
}

double MonomialEpsilon::operator()(const vector<double>& x) const
{
    if (funcs_.empty()) return 0.0;
    else {
        double res=1.0;
        for (size_t i=0;i<funcs_.size();i++)
            res = res * (*(funcs_[i]))(x);
        return res;
    }
}

ostream& operator<<(ostream& ss, const MonomialEpsilon& M)
{
    ss<<"e^"<<M.epsilon_;
    return ss;
}

ostream& operator<<(ostream& ss, const PolynomialEpsilon& M)
{
    for (size_t i=0;i<M.size()-1;i++)
        ss<<M.terms_[i]<<" + ";
    ss<<M.terms_[M.size()-1];
    return ss;
}

double PolynomialEpsilon::operator()(const vector<double>& x) const
{
    double res=0.0;
    for (size_t i=0;i<terms_.size();i++)
        res += terms_[i](x);
    return res;
}


inline PolynomialEpsilon operator+(
                                   PolynomialEpsilon lhs,
                                   const PolynomialEpsilon& rhs
                                   )
{
    lhs += rhs;
    return lhs;
}
inline PolynomialEpsilon operator+(
                                   PolynomialEpsilon lhs,
                                   const MonomialEpsilon& rhs
                                   )
{
    lhs += rhs;
    return lhs;
}

PolynomialEpsilon operator*(const PolynomialEpsilon& lhs, 
                            const PolynomialEpsilon& rhs)
{
    PolynomialEpsilon res;
    for (size_t i=0;i<lhs.size();i++)
        {
            for (size_t j=0;j<rhs.size();j++)
                {
                    res += lhs[i]*rhs[j];
                }
        }
    return res;
}

PolynomialEpsilon& PolynomialEpsilon::operator+=(const PolynomialEpsilon& rhs)
{
    for (size_t i=0;i<rhs.size();i++)
    {
        terms_.push_back(rhs[i]);
    }
    return *this;
}
PolynomialEpsilon& PolynomialEpsilon::operator+=(const MonomialEpsilon& rhs)
{
    
    terms_.push_back(rhs);
    return *this;
}


double null_function(const vector<double>&){return 0.0;}

PolynomialEpsilon PolynomialEpsilon::EpsilonCoefficient(const int eps) const
{
    PolynomialEpsilon res;
    bool found = false;
    for (size_t i=0;i<terms_.size();i++)
        {
            if (terms_[i].EpsPower() == eps)
                {
                res += terms_[i];
                found =true;
                }
        }
    if (found==false) res +=MonomialEpsilon(null_function,eps);
    return res;
}



