/**
 *
 * \file   bjorken.h
 * \author Simone Lionetti
 * \date   September 2014
 *
 */

#ifndef BJORKEN_H
#define BJORKEN_H

#include "fourvector.h"
#include <vector>
using namespace std;

/**
 *
 * \struct Bjorken
 * \brief  Container for Bjorken x data
 *
 */
struct Bjorken
{
    double x1;
    double x2;
    /// \todo Introduce "bool isGood" to account for rejected events;

    Bjorken() :
    x1(), x2()
    {}

    Bjorken(const double& x1in, const double& x2in) :
    x1(x1in), x2(x2in)
    {}

};

class XGenerator
{

public:

    XGenerator(Bjorken& x, double& jacobian) :
    _x(x), _jacobian(jacobian), _x1x2min(0.)
    {}

    virtual void setParameters(const double& x1x2min)
    {
        _x1x2min = x1x2min;
        return;
    }

    virtual void operator()(const double* const randoms) = 0;
    virtual size_t Nran() const = 0;

protected:

    Bjorken& _x;
    double& _jacobian;

    double _x1x2min;

};

class TwoXGenerator : public XGenerator
{

public:

    TwoXGenerator(Bjorken& x, double& jacobian) :
    XGenerator(x,jacobian)
    {}

    void operator()(const double* const randoms)
    {
        const double x1x2 = _x1x2min + (1.-_x1x2min) * randoms[0];
        _x.x1 = x1x2 + (1.-x1x2) * randoms[1];
        _x.x2 = x1x2/_x.x1;
        _jacobian /= _x.x1;
        return;
    }
    size_t Nran() const {return 2;}
};

class OneXGenerator : public XGenerator
{

public:

    OneXGenerator(Bjorken& x, double& jacobian) :
    XGenerator(x,jacobian)
    {}

    void operator()(const double* const randoms)
    {
        _x.x1 = _x1x2min + (1.-_x1x2min) * randoms[0];
        _x.x2 = _x1x2min/_x.x1;
        _jacobian *= (1.-_x1x2min)/_x.x1;
        return;
    }
    size_t Nran() const {return 1;}
};

#endif
