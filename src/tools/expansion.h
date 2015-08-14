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

#include "FormalSum.h"
#include <algorithm>   // std::min
#include <cmath>       // std::pow
#include <float.h>     // DBL_EPSILON

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

// Short name for epsilon expansion with double coefficients
typedef Expansion<Parameter::epsilon, double> EpsExp;

#include "expansion.inl"

#endif
