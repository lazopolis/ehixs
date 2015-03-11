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

/// \name Constructors

/// Default constructor
template <class Type>
FormalSum<Type>::FormalSum(void) :
_lowestTerm(0), _coefficients(0)
{}

/// Constructor with size and data
template <class Type>
FormalSum<Type>::FormalSum(const int lowestTerm, const int highestTerm, const Type* const coefficients) :
_lowestTerm(lowestTerm), _coefficients(0)
{
    if (highestTerm < lowestTerm) {
        throw("bad_parameters");
    }
    while (_lowestTerm <= highestTerm && coefficients[_lowestTerm - lowestTerm] == Type())
    {
        ++_lowestTerm;
    }
    for (size_t i = static_cast<size_t>(_lowestTerm - lowestTerm); i <= static_cast<size_t>(highestTerm - lowestTerm); ++i)
    {
        _coefficients.push_back(coefficients[i]);
    }
    while ( (!_coefficients.empty()) && (_coefficients.back() == Type()) )
    {
        _coefficients.pop_back();
    }
    return;
}

/// Constructor with size and data
template <class Type>
FormalSum<Type>::FormalSum(const int lowestTerm, const vector<Type>& coefficients) :
_lowestTerm(lowestTerm), _coefficients(0)
{
    for (typename vector<Type>::const_iterator it = coefficients.begin(); it != coefficients.end(); ++it) {
        if (_coefficients.empty() && *it == Type()) {
            ++_lowestTerm;
        } else {
            _coefficients.push_back(*it);
        }
    }
    while (!_coefficients.empty() && _coefficients.back() == Type())
    {
        _coefficients.pop_back();
    }
    return;
}

/// Constructor with size and data
template <class Type>
FormalSum<Type>::FormalSum(const int lowestTerm, const initializer_list<Type>& coefficients) :
_lowestTerm(lowestTerm), _coefficients(0)
{
    for (typename initializer_list<Type>::const_iterator it = coefficients.begin(); it != coefficients.end(); ++it) {
        if (_coefficients.empty() && *it == Type()) {
            ++_lowestTerm;
        } else {
            _coefficients.push_back(*it);
        }
    }
    while (!_coefficients.empty() && _coefficients.back() == Type())
    {
        _coefficients.pop_back();
    }
    return;
}

/// Constructor of size one object
template <class Type>
FormalSum<Type>::FormalSum(const int term, const Type& coefficient) :
_lowestTerm(term), _coefficients(0)
{
    if ( coefficient != Type() ) {
        _coefficients.push_back(coefficient);
    }
    return;
}

/// Copy constructor
template <class Type>
FormalSum<Type>::FormalSum(const FormalSum<Type>& that) :
_lowestTerm(that.minTerm()), _coefficients(that._coefficients)
{}

/// Destructor
template <class Type>
FormalSum<Type>::~FormalSum(void)
{}

/// \name Input Functions

/// Set a coefficient
template <class Type>
FormalSum<Type>& FormalSum<Type>::setCoefficient(const int term, const Type& value)
{
    if ( isZero() && value != Type() ) {
        _lowestTerm = term;
        _coefficients.push_back(value);
        return *this;
    }
    else if ( value == Type() ) {
        if ( term < minTerm() || term > maxTerm() ) {
            return *this;
        } else {
            _coefficients[static_cast<size_t>(term - minTerm())] = value;
            if ( term == maxTerm() ) {
                while ( _coefficients.back() == Type() )
                {
                    _coefficients.pop_back();
                }
            }
            if ( term == minTerm() ) {
                while ( _coefficients.front() == Type() && !_coefficients.empty() )
                {
                    _coefficients.erase(_coefficients.begin());
                    ++_lowestTerm;
                }
            }
            return *this;
        }
    }
    else if ( term > maxTerm() ) {
        _coefficients.resize(static_cast<size_t>(term - minTerm() + 1));
        _coefficients[static_cast<size_t>(term - _lowestTerm)] = value;
    }
    else if ( term < minTerm() ) {
        while ( term < minTerm() ) {
            _coefficients.insert(_coefficients.begin(), Type());
            --_lowestTerm;
        }
        _coefficients[0] = value;
    }
    else {
        _coefficients[static_cast<size_t>(term - _lowestTerm)] = value;
    }
    return *this;
}

/// Add something to a coefficient
template <class Type>
FormalSum<Type>& FormalSum<Type>::addCoefficient(const int term, const Type& value)
{
    return setCoefficient(term, getCoefficient(term)+value);
}

/// \name Output Functions

/// Gives the maximum nonzero term number
/// \note Returns minTerm-1 if the expression is empty
template <class Type>
inline int FormalSum<Type>::maxTerm(void) const
{
    return minTerm()+static_cast<int>(_coefficients.size())-1;
}

/// Gives the minimum nonzero term number
/// \note Returns a random value if the expression is empty
template <class Type>
inline int FormalSum<Type>::minTerm(void) const
{
    return _lowestTerm;
}

/// Gives the number of terms between the last two nonzero ones
template <class Type>
inline size_t FormalSum<Type>::size(void) const
{
    return _coefficients.size();
}

/// Returns a specific coefficient, for terms outside the range returns "zero" i.e. default-constructed object
template <class Type>
inline Type FormalSum<Type>::getCoefficient(const int term) const
{
    if ( term < minTerm() || term > maxTerm() ) {
        return Type();
    }
    return _coefficients[static_cast<size_t>(term - minTerm())];
}

/// Checks if the expansion is formally equivalent to or different from zero
template <class Type>
inline bool FormalSum<Type>::isZero(void) const
{
    return _coefficients.empty();
}

/// \name Operations

/// Assign formal expansion
template <class Type>
FormalSum<Type>& FormalSum<Type>::operator=(const FormalSum<Type>& that)
{
    _lowestTerm = that.minTerm();
    _coefficients = that._coefficients;
    return *this;
}

/// Compare two formal expressions of the same kind
template <class Type>
bool FormalSum<Type>::operator==(const FormalSum<Type>& that) const
{
    if ( isZero() && that.isZero() ) return true;
    if ( that.minTerm() != minTerm() ) {
        return false;
    } else {
        return _coefficients == that._coefficients;
    }
}

/// Compare two formal expressions of the same kind
template <class Type>
bool FormalSum<Type>::operator!=(const FormalSum<Type>& that) const
{
    return !(operator==(that));
}

/// Negative of this formal expansion
template <class Type>
FormalSum<Type> FormalSum<Type>::operator-(void) const
{
    FormalSum foo(*this);
    for (iterator it = foo._coefficients.begin(); it < foo._coefficients.end(); ++it)
    {
        *it = -(*it);
    }
    return foo;
}

/// Add the formal expansion to another
template <class Type>
FormalSum<Type> FormalSum<Type>::operator+(const FormalSum<Type>& that) const
{
    FormalSum<Type> foo(*this);
    return foo += that;
}

/// Subtract a formal expansion from this one
template <class Type>
FormalSum<Type> FormalSum<Type>::operator-(const FormalSum<Type>& that) const
{
    return (*this + (-that));
}

/// Add the formal expansion to another
template <class Type>
const FormalSum<Type>& FormalSum<Type>::operator+=(const FormalSum<Type>& that)
{
    for (int i = that.minTerm(); i <= that.maxTerm(); ++i)
        addCoefficient(i, that.getCoefficient(i));
    return *this;
}

/// Subtract a formal expansion from this one
template <class Type>
const FormalSum<Type>& FormalSum<Type>::operator-=(const FormalSum<Type>& that)
{
    return operator+=(-that);
}

/// Truncate the sum after the n-th term
template <class Type>
const FormalSum<Type>& FormalSum<Type>::truncate(const int n)
{
    while (maxTerm()>n) _coefficients.pop_back();
    return *this;
}

/// Cut the sum after the first n elements
template <class Type>
const FormalSum<Type>& FormalSum<Type>::cut(const size_t n)
{
    if (n!=0) while (size()>n) _coefficients.pop_back();
    return *this;
}


/// Multiplications
template <class Type>
FormalSum<Type> operator*(const FormalSum<Type>& myExpansion, const Type& factor)
{
    FormalSum<Type> foo;
    for (int i = myExpansion.minTerm(); i <= myExpansion.maxTerm(); ++i)
        foo.setCoefficient(i, myExpansion.getCoefficient(i) * factor);
    return foo;
}

template <class Type>
FormalSum<Type> operator*(const Type& factor, FormalSum<Type>& myExpansion)
{
    return myExpansion * factor;
}

template <class Type>
FormalSum<Type> operator*(const FormalSum<Type>& myExpansion, const double& factor)
{
    if ( factor == 0. ) {
        return FormalSum<Type>();
    } else {
        FormalSum<Type> foo;
        for (int i = myExpansion.minTerm(); i <= myExpansion.maxTerm(); ++i)
            foo.setCoefficient(i, myExpansion.getCoefficient(i) * factor);
        return foo;
    }
}

template <class Type>
FormalSum<Type> operator*(const double& factor, FormalSum<Type>& myExpansion)
{
    return myExpansion * factor;
}

/// Standard output function
template <class Type>
std::ostream& operator<<(std::ostream& myOut, const FormalSum<Type>& myExp)
{
    if ( myExp.isZero() ) {
        return myOut << Type();
    } else {
        myOut << "{ ";
        for (int i = myExp.minTerm(); i <= myExp.maxTerm(); ++i)
        {
            if ( myExp.getCoefficient(i) != Type() ) {
                myOut << myExp.getCoefficient(i) << " a(" << i <<") ";
                if ( i != myExp.maxTerm() ) {
                    myOut << "+ ";
                }
            }
        }
        return myOut << "}";
    }
}


#endif
