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
#include "FormalSum.h"

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

    // Default constructor
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

    /// @}

    /// \name Data members
    /// @{

    static size_t accuracy; ///< Target number of terms that need to be valid

    /// @}

private:

    Expansion<Par,Type>& _check(const size_t mySize)
    {
        if (accuracy != 0) {
            if (mySize < accuracy) {
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
    if (accuracy!=0) cMax = min(aMax+bMax,cMin+static_cast<int>(accuracy)-1);
    for (int c = cMin; c <= cMax; ++c)
        for (int a = max(c-bMax,aMin); a <= min(c-bMin,aMax); ++a)
            foo.addCoefficient(c, this->getCoefficient(a) * that.getCoefficient(c-a));
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
