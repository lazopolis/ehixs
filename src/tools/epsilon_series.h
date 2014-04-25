#ifndef EPSILON_SERIES_H
#define EPSILON_SERIES_H

#include<vector>
#include <iostream>
#include <sstream>
using namespace std;
typedef double (*ptr_to_function)(const vector<double>& );



// A monomial in epsilon of the form f1(x)*f2(x)*...*fN(x)*e^a
// epsilon_ = a 
// the product of functions is saved as a vector 
// to function pointers
class MonomialEpsilon
{
public:
    MonomialEpsilon(ptr_to_function func,int eps )
        :epsilon_(eps){funcs_.push_back(func);}
    friend MonomialEpsilon operator*( MonomialEpsilon lhs,
                                const MonomialEpsilon& rhs);
    virtual double operator()(const vector<double>& x) const;
    int EpsPower()const {return epsilon_;}
    friend ostream& operator<<(ostream&, const MonomialEpsilon&);
private:
    vector<ptr_to_function> funcs_;
    int epsilon_;
};



class PolynomialEpsilon
{
public:
    PolynomialEpsilon& operator+=(const PolynomialEpsilon& rhs);
    PolynomialEpsilon& operator+=(const MonomialEpsilon& rhs);
    friend PolynomialEpsilon operator*(const PolynomialEpsilon& lhs,
                        const PolynomialEpsilon& rhs);
    MonomialEpsilon operator[](unsigned i) const{return terms_[i];}
    PolynomialEpsilon EpsilonCoefficient(int eps) const;
    double operator()(const vector<double>& x) const;
    int size() const {return terms_.size();}
    friend ostream& operator<<(ostream&, const PolynomialEpsilon&);
private:
    vector<MonomialEpsilon> terms_;
};






#endif



