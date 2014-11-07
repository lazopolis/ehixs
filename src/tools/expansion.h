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

#include <algorithm>
#include "FormalSum.h"

enum Parameter{
    alphas,
    epsilon
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

    // Default constructor
    Expansion(const bool isExact = false) :
    FormalSum<Type>()
    {
        if (!isExact) _cut();
        return;
    }

    /// Constructor with size and data
    Expansion(const int lowestTerm, const int highestTerm, const Type* const coefficients, const bool isExact = false) :
    FormalSum<Type>(lowestTerm, highestTerm, coefficients)
    {
        if (!isExact) _cut();
        return;
    }

    /// Constructor with size and data
    Expansion(const int lowestTerm, const vector<Type>& coefficients, const bool isExact = false) :
    FormalSum<Type>(lowestTerm, coefficients)
    {
        if (!isExact) _cut();
        return;
    }

    /// Constructor with size and data
    Expansion(const int lowestTerm, const initializer_list<Type>& coefficients, const bool isExact = false) :
    FormalSum<Type>(lowestTerm, coefficients)
    {
        if (!isExact) _cut();
        return;
    }

    /// Constructor of size one object
    Expansion(const int term, const Type& coefficient, const bool isExact = false):
    FormalSum<Type>(term, coefficient)
    {
        if (!isExact) _cut();
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

    /// Add two expansions
    virtual Expansion<Par,Type> operator+(const Expansion<Par,Type>& that) const;

    /// Multiply two expansions
    virtual Expansion<Par,Type> operator*(const Expansion<Par,Type>& that) const;

    /// @}

    /// \name Data members
    /// @{

    static size_t accuracy; ///< Target number of terms that need to be valid

    /// @}

    Expansion<Par,Type>& _cut()
    {
        if (accuracy != 0) {
            if (this->size()<accuracy) {
                throw "Not enough terms";
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

/// Output function
template <Parameter Par,typename Type>
std::ostream& operator<<(std::ostream& myOut, const Expansion<Par,Type>& myExpansion);


/// \name Input Functions

/// Override setCoefficient to keep consistent internal status
template <Parameter Par, typename Type>
Expansion<Par,Type>& Expansion<Par,Type>::setCoefficient(const int term, const Type& value)
{
    FormalSum<Type>::setCoefficient(term, value);
    return _cut();
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
    int fooMin(min(this->minTerm(),that.minTerm())), fooMax(max(this->maxTerm(),that.minTerm()));
    if ( accuracy!=0 && fooMax > fooMin+static_cast<int>(accuracy)-1)
        fooMax = fooMin+static_cast<int>(accuracy)-1;
    // By starting with extrema one can avoid a lot of resizing...
    Expansion<Par,Type> foo(fooMin,this->getCoefficient(fooMin)+that.getCoefficient(fooMin));
    for (int counter = fooMax; counter > fooMin; --counter)
        foo.setCoefficient(counter, this->getCoefficient(counter) + that.getCoefficient(counter));
    return foo;
}

/// Multiply expansions
template <Parameter Par, typename Type>
Expansion<Par,Type> Expansion<Par,Type>::operator*(const Expansion<Par,Type>& that) const
{
    // This is maybe slightly heavy from the point of view of resizing, but nicer than everything else I can think about
    Expansion<Par,Type> foo;
    if (accuracy==0) foo = this->FormalSum<Type>::operator*(that);
    else {
        const int aMin = this->minTerm();
        const int aMax = this->maxTerm();
        const int bMin = that.minTerm();
        const int bMax = that.maxTerm();
        const int cMin = aMin+bMin;
        const int cMax = min(aMax+bMax,cMin+static_cast<int>(accuracy)-1);
        for (int c = cMin; c <= cMax; ++c)
            for (int a = min(c-bMax,aMin); a <= min(c-bMin+1,aMax); ++a)
                foo.addCoefficient(c, this->getCoefficient(a) * that.getCoefficient(a-c));
    }
    return foo;
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

/// Standard output function
template <Parameter Par, typename Type>
std::ostream& operator<<(std::ostream& myOut, const Expansion<Par,Type>& myExpansion)
{
    if (myExpansion == Expansion<Par,Type>()) {
        return myOut << "0";
    } else {
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
                    myOut << " x^(" << counter <<")";
                } else if (counter > 0) {
                    if (counter != 1) {
                        myOut << " x^" << counter;
                    } else {
                        myOut << " x";
                    }
                }
            }
        }
        return myOut;
    }
}

#endif
