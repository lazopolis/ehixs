/**
 *
 * \file    expansion.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#ifndef EXPANSION_H
#define EXPANSION_H

#include <algorithm>   // std::min
#include <cmath>       // std::pow
#include <float.h>     //DBL_EPSILON
#include "FormalSum.h"

/**
 *
 * \fn    m1n
 * \brief Shorthand for (-1)^n
 *
 */

template <typename intype, typename outtype>
outtype m1n(const intype& n)
{
    if (n%2==0) return 1;
    else return -1;
}

/**
 *
 * \enum  Parameter
 * \brief List of the valid parameters for expansions
 *
 */

enum class Parameter : char {
    alphas='a',
    epsilon='e'
    };

/**
 *
 * \class Expansion
 * \brief Class designed to handle formal expansions by order
 *
 */

template <Parameter Par, typename Type>
class Expansion : public FormalSum<Type>
{
    
public:

    /// \name Constructors etc.
    /// @{

    /// Default constructor
    Expansion() :
    FormalSum<Type>()
    {}

    /// Constructor with size and data
    Expansion(const int lowestTerm, const int highestTerm, const Type* const coefficients, const bool isExact = false) :
    FormalSum<Type>(lowestTerm, highestTerm, coefficients)
    {
        if (!isExact) _check(highestTerm-lowestTerm+1);
        return;
    }

    /// Constructor with size and data
    Expansion(const int lowestTerm, const vector<Type>& coefficients, const bool isExact = false) :
    FormalSum<Type>(lowestTerm, coefficients)
    {
        if (!isExact) _check(coefficients.size());
        return;
    }

    /// Constructor with size and data
    Expansion(const int lowestTerm, const initializer_list<Type>& coefficients, const bool isExact = false) :
    FormalSum<Type>(lowestTerm, coefficients)
    {
        if (!isExact) _check(coefficients.size());
        return;
    }

    /// Constructor with size and data
    Expansion(const FormalSum<Type>& sum, const bool isExact = false) :
    FormalSum<Type>(sum)
    {
        if (!isExact) _check(sum.size());
        return;
    }

    /// Constructor of size one object
    Expansion(const int term, const Type& coefficient, const bool isExact = false):
    FormalSum<Type>(term, coefficient)
    {
        if (!isExact) _check(1);
        return;
    }

    // Copy constructor
    Expansion(const Expansion<Par, Type>& that):
    FormalSum<Type>(that)
    {}

    // Destructor
    ~Expansion(void)
    {}

    /// @}

    /// \name Input Functions
    /// @{

    /// Override setCoefficient to keep consistent internal status
    Expansion<Par,Type>& setCoefficient(const int term, const Type& value);

    /// @}

    /// \name Output Functions
    /// @{

    /// Evaluation
    Type operator()(const double& parvalue = 1.) const;

    /// @}

    /// \name Operations
    /// @{

    /// Negative of this expansions
    Expansion<Par,Type> operator-() const
    {
        return Expansion<Par,Type>(this->FormalSum<Type>::operator-(),true);
    }

    /// Add two expansions
    Expansion<Par,Type> operator+(const Expansion<Par,Type>& that) const;

    /// Subtract two expansions
    Expansion<Par,Type> operator-(const Expansion<Par,Type>& that) const;

    /// Multiply two expansions
    Expansion<Par,Type> operator*(const Expansion<Par,Type>& that) const;

    /// Cut this expression after the first n terms
    const Expansion<Par,Type>& cut(const size_t n);

    /// @}

    /// \brief Geometric series expansion
    /// Expansion in Par of the function 1/(1 - a Par)
    /// Type has to be double, int, complex<>, etc...
    static Expansion<Par,Type> geometric(const Type& a, const size_t trunc = accuracy)
    {
        vector<Type> foo({1});
        foo.reserve(trunc);
        while (foo.size()<trunc)
            foo.push_back(foo.back()*a);
        return Expansion<Par,Type>(0,foo);
    }

    /// \brief Exponential series expansion
    /// Expansion in Par of the function e^(a*Par)
    /// Type has to be double, int, complex<>, etc...
    static Expansion<Par,Type> exp(const Type& a, const size_t trunc = accuracy)
    {
        vector<Type> foo({1});
        foo.reserve(trunc);
        while (foo.size()<trunc)
            foo.push_back(foo.back()*a/foo.size());
        return Expansion<Par,Type>(0,foo);
    }

    /// \name Data members
    /// @{

    static size_t accuracy; ///< Target number of terms that need to be valid

    /// @}

private:

    Expansion<Par,Type>& _check(const size_t mySize)
    {
        if (accuracy != 0) {
            if (mySize < accuracy) {
                cerr << "Not enough terms in " << *this << endl;
                throw;
            } else {
                this->cut(accuracy);
            }
        }
        return *this;
    }

};

/// Non-member operations

/// Multiply an expansion by a number
template <Parameter Par, typename Type>
Expansion<Par,Type> operator*(const Expansion<Par,Type>& first, const Type second);
template <Parameter Par, typename Type>
Expansion<Par,Type> operator*(const Type first, const Expansion<Par,Type>& second);
template <Parameter Par, typename Type>
Expansion<Par,Type> operator/(const Expansion<Par,Type>& numer, const Type denom);

/// Multiply an expansion by another one, with finite size
template <Parameter Par, typename Type>
Expansion<Par,Type> times(const Expansion<Par,Type>& first, const Expansion<Par,Type>& second);

/// Get the inverse expansion 1/..., truncated at finite size
template <Parameter Par, typename Type>
Expansion<Par,Type> inverse(const Expansion<Par,Type>& that, const size_t trunc, const bool warn = true);
    
/// Get the coefficient of a product
template<Parameter Par, typename Type>
Type productCoeff(const Expansion<Par,Type>& in1, const Expansion<Par,Type>& in2, const int term);

/// Output function
template <Parameter Par,typename Type>
std::ostream& operator<<(std::ostream& myOut, const Expansion<Par,Type>& myExpansion);

template <Parameter Par, typename Type>
size_t Expansion<Par,Type>::accuracy = 0;

/// \name Input Functions

/// Override setCoefficient to keep consistent internal status
template <Parameter Par, typename Type>
Expansion<Par,Type>& Expansion<Par,Type>::setCoefficient(const int term, const Type& value)
{
    FormalSum<Type>::setCoefficient(term, value);
    this->cut(accuracy);
    return *this;
}

/// \name Output Functions

/// Evaluate expansion
template <Parameter Par, typename Type>
Type Expansion<Par,Type>::operator()(const double& parvalue) const
{
    Type foo = Type();
    for (int counter = this->minTerm(); counter <= this->maxTerm(); ++counter)
    {
        foo += this->getCoefficient(counter)*pow(parvalue,counter);
    }
    return foo;
}

/// \name Operations

/// Add expansions
template <Parameter Par, typename Type>
Expansion<Par,Type> Expansion<Par,Type>::operator+(const Expansion<Par,Type>& that) const
{
    int fooMin(min(this->minTerm(),that.minTerm())), fooMax(max(this->maxTerm(),that.maxTerm()));
    if ( accuracy!=0 && fooMax > fooMin+static_cast<int>(accuracy)-1)
        fooMax = fooMin+static_cast<int>(accuracy)-1;
    // By starting with extrema one can avoid a lot of resizing...
    Expansion<Par,Type> foo(fooMin,this->getCoefficient(fooMin)+that.getCoefficient(fooMin),true);
    for (int counter = fooMax; counter > fooMin; --counter)
        foo.setCoefficient(counter, this->getCoefficient(counter) + that.getCoefficient(counter));
    return foo;
}

/// Subtract expansions
template <Parameter Par, typename Type>
Expansion<Par,Type> Expansion<Par,Type>::operator-(const Expansion<Par,Type>& that) const
{
    return *this+(-that);
}
    
/// Multiply expansions
template <Parameter Par, typename Type>
Expansion<Par,Type> Expansion<Par,Type>::operator*(const Expansion<Par,Type>& that) const
{
    // This is maybe slightly heavy from the point of view of resizing, but nicer than everything else I can think about
    Expansion<Par,Type> foo;
    const int aMin = this->minTerm();
    const int aMax = this->maxTerm();
    const int bMin = that.minTerm();
    const int bMax = that.maxTerm();
    const int cMin = aMin+bMin;
    int cMax = aMax+bMax;
    if (accuracy!=0) cMax = min(cMax,cMin+static_cast<int>(accuracy)-1);
    for (int c = cMin; c <= cMax; ++c)
        for (int a = max(c-bMax,aMin); a <= min(c-bMin,aMax); ++a)
            foo.addCoefficient(c, this->getCoefficient(a) * that.getCoefficient(c-a));
    return foo;
}

/// Cut this expression after the first n terms
template <Parameter Par, typename Type>
const Expansion<Par,Type>& Expansion<Par,Type>::cut(const size_t n)
{
    // Assuming the user knows what he's doing
    this->FormalSum<Type>::cut(n);
    return *this;
}


/// \name Non-member operations

/// Multiply by a factor
template <Parameter Par, typename Type>
Expansion<Par,Type> operator*(const Expansion<Par,Type>& first, const Type second)
{
    Expansion<Par,Type> foo;
    FormalSum<Type> patch = first;
    foo.FormalSum<Type>::operator=(operator*(patch, second));
    return foo;
}

template <Parameter Par, typename Type>
Expansion<Par,Type> operator*(const Type first, const Expansion<Par,Type>& second)
{
    return second * first;
}

template <Parameter Par, typename Type>
Expansion<Par,Type> operator/(const Expansion<Par,Type>& numer, const Type denom)
{
    return numer * (1./denom);
}

/// Multiply an expansion by another one, with finite size
template <Parameter Par, typename Type>
Expansion<Par,Type> times(const Expansion<Par,Type>& first, const Expansion<Par,Type>& second,
                          const size_t accuracy = Expansion<Par,Type>::accuracy)
{
    // This is maybe slightly heavy from the point of view of resizing, but nicer than everything else I can think about
    Expansion<Par,Type> foo; /// \todo Make a function to reserve space
    const int aMin = first.minTerm();
    const int aMax = first.maxTerm();
    const int bMin = second.minTerm();
    const int bMax = second.maxTerm();
    const int cMin = aMin+bMin;
    int cMax = aMax+bMax;
    if (accuracy!=0) cMax = min(cMax,cMin+static_cast<int>(accuracy)-1);
    for (int c = cMin; c <= cMax; ++c)
    {
        foo.setCoefficient(c,Type());
        for (int a = max(c-bMax,aMin); a <= min(c-bMin,aMax); ++a)
            foo.addCoefficient(c, first.getCoefficient(a) * second.getCoefficient(c-a));
    }
    return foo;
}

/// Auxiliary function for inverse
/// Note that 'that' had better start at order 0 with coefficient 1
template <Parameter Par, typename Type>
Type _expansionInverseP(const Expansion<Par,Type>& that, const size_t j, const size_t k)
{
    if (that.minTerm()!=0 || abs(that.getCoefficient(that.minTerm())-1.)>DBL_EPSILON)
    {
        cerr << "called _expansionInverseP with unnormalized expansion " << that << endl;
        throw;
    }
    if (k==0) return 1;
    Type foo = Type();
    for (size_t m = 1; m <= k; ++m)
    {
        foo += (static_cast<int>((j+1)*m)-static_cast<int>(k))*that.getCoefficient(m)*_expansionInverseP(that,j,k-m);
    }
    return foo/k;
}

/// Return n!/k! for every k<=n, 1 for k>=n
inline size_t factorial(const size_t n, const size_t k = 0)
{
    return (n <= k) ? 1 : factorial(n-1,k) * n;
}

/// Return the binomial coefficient n!/(k!(n-k)!)
inline size_t binomial(const size_t n, const size_t k)
{
    return factorial(n,k)/factorial(n-k);
}

/// Get the inverse expansion 1/..., truncated at finite size at trunc terms
template <Parameter Par, typename Type>
Expansion<Par,Type> inverse(const Expansion<Par,Type>& that, const size_t trunc, const bool warn)
{
    if (trunc>that.size() && warn) {
        cerr << "Expansion order too low for computing the desired terms of its inverse." << endl;
        cerr << "If you know what you are doing, disable this warning with the flag 'warn' in inverse." << endl;
        throw;
    }
    const int start = that.minTerm();
    Expansion<Par, Type> normd(that);
    normd = times(Expansion<Par,Type>(-start,1),normd,trunc)/normd.getCoefficient(start);
    Expansion<Par, Type> foo(-start,1);
    for (size_t k = 1; k<trunc; ++k)
    {
        foo.setCoefficient(-start+k,Type());
        for (size_t r = 0; r<=k; ++r)
        {
            const Type appo = m1n<size_t,double>(r)*binomial(k,r)*(k+1.)/(r+1.)*_expansionInverseP(normd,r,k);
            foo.addCoefficient(-start+k,appo);
        }
    }
    return foo/that.getCoefficient(start);
}

    
/// Get the coefficient of a product
template <Parameter Par, typename Type>
Type productCoeff(const Expansion<Par,Type>& in1, const Expansion<Par,Type>& in2, const int term)
{
    Type foo(0);
    for (int i = in1.minTerm(); i<=in1.maxTerm() && (term-i)>=in2.minTerm(); ++i)
        foo += in1.getCoefficient(i) * in2.getCoefficient(term-i);
    return foo;
}
    
/// Standard output function
template <Parameter Par, typename Type>
std::ostream& operator<<(std::ostream& myOut, const Expansion<Par,Type>& myExpansion)
{
    if (myExpansion == Expansion<Par,Type>()) {
        return myOut << "0";
    } else {
        const char par = static_cast<char>(Par);
        for (int counter = myExpansion.minTerm(); counter <= myExpansion.maxTerm(); counter++)
        {
            if ( myExpansion.getCoefficient(counter) != 0 ) {
                if ( myExpansion.getCoefficient(counter) < 0 ) {
                    myOut << " -";
                } else {
                    if ( counter != myExpansion.minTerm()) {
                        myOut << " +";
                    }
                }
                if (!( (std::abs(myExpansion.getCoefficient(counter)) == 1 ) && (counter != 0) )) {
                    myOut << " " << std::abs(myExpansion.getCoefficient(counter));
                }
                if (counter < 0) {
                    myOut << " " << par << "^(" << counter <<")";
                } else if (counter > 0) {
                    if (counter != 1) {
                        myOut << " " << par << "^" << counter;
                    } else {
                        myOut << " " << par;
                    }
                }
            }
        }
        return myOut;
    }
}

#endif
