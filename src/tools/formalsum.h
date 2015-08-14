/**
 *
 * \file    formalsum.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#ifndef FORMAL_SUM_H
#define FORMAL_SUM_H

#include <iostream>
#include <vector>

using namespace std;

/**
 *
 * \class   FormalSum
 * \ingroup tools
 * \brief   Class designed to handle formal sums
 * \note    Internal status coherence requires the object to have no zeros at the beginning and at the end
 *          where zero = default-constructed Type object.
 * \note    The "zero object" has an empty coefficient vector, with any _lowestTerm.
 * \todo    Write down detailed requirements for Type.
 *
 */

template <class Type>
class FormalSum
{

public:

    /// \name Type definitions
    /// @{

    /// Iterator
    typedef typename vector<Type>::iterator iterator;

    /// @}

private:

    /// \name Data members
    /// @{

    int _lowestTerm;            ///< Number identifying the lowest term
    vector<Type> _coefficients; ///< Vector of coefficients

    /// @}

public:

    /// \name Constructors etc.
    /// @{

    /// Default constructor
    FormalSum(void);

    /// Constructor with size and data
    FormalSum(const int lowestTerm, const int highestTerm, const Type* const coefficients);

    /// Constructor with size and data
    FormalSum(const int lowestTerm, const vector<Type>& coefficients);

    /// Constructor with size and data
    FormalSum(const int lowestTerm, const initializer_list<Type>& coefficients);

    /// Constructor of size one object
    FormalSum(const int term, const Type& coefficient);

    /// Copy constructor
    FormalSum(const FormalSum<Type>& that);

    /// Destructor
    virtual ~FormalSum(void);

    /// @}

    /// \name Input Functions
    /// @{

    /// Set a coefficient
    virtual FormalSum& setCoefficient(const int term, const Type& value);

    /// Add something to a coefficient
    virtual FormalSum& addCoefficient(const int term, const Type& value);

    /// @}

    /// \name Output Functions
    /// @{

    /// Return the maximum term number
    int maxTerm(void) const;

    /// Return the minimum term number
    int minTerm(void) const;

    /// Return the number of terms
    size_t size(void) const;

    /// Return a specific coefficient
    Type getCoefficient(const int term) const;

    /// Checks if the expansion is formally equivalent to zero
    bool isZero(void) const;

    /// @}

    /// \name Operations
    /// @{

    /// Assign formal expression
    FormalSum<Type>& operator=(const FormalSum<Type>& that);

    /// Compare two formal expressions (of the same kind)
    virtual bool operator==(const FormalSum<Type>& that) const;

    /// Compare two formal expressions (of the same kind)
    virtual bool operator!=(const FormalSum<Type>& that) const;

    /// Negative of this formal expression
    FormalSum<Type> operator-(void) const;

    /// Add the formal expression to another one
    FormalSum<Type> operator+(const FormalSum<Type>& that) const;

    /// Subtract a formal expression from this one
    FormalSum<Type> operator-(const FormalSum<Type>& that) const;

    /// Add the formal expression to another one
    const FormalSum<Type>& operator+=(const FormalSum<Type>& that);

    /// Subtract a formal expression from this one
    const FormalSum<Type>& operator-=(const FormalSum<Type>& that);

    /// Multiply this formal expression for a double
    const FormalSum<Type>& operator*=(const double that);

    /// Divide this formal expression by a double
    const FormalSum<Type>& operator/=(const double that);

    /// Truncate the sum after the n-th term
    virtual const FormalSum<Type>& truncate(const int n);

    /// Cut the sum after the first n elements (n=0 does not truncate)
    virtual const FormalSum<Type>& cut(const size_t n);

    /// @}

};

/// Multiplications
FormalSum<double> operator*(const FormalSum<double>& myExpansion, const double& factor);

FormalSum<double> operator*(const double& factor, FormalSum<double>& myExpansion);

template <class Type>
FormalSum<Type> operator*(const FormalSum<Type>& myExpansion, const Type& factor);

template <class Type>
FormalSum<Type> operator*(const Type& factor, FormalSum<Type>& myExpansion);

template <class Type>
FormalSum<Type> operator*(const FormalSum<Type>& myExpansion, const double& factor);

template <class Type>
FormalSum<Type> operator*(const double& factor, FormalSum<Type>& myExpansion);

/// Standard output function
template <class Type>
std::ostream& operator<<(std::ostream& myOut, const FormalSum<Type>& myExp);

#include "formalsum.inl"

#endif
