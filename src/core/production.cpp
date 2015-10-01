/**
 *
 * \file    production.cpp
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "production.h"

template<>
void Option<Production*>::set(const string& input)
{
    NameAndArgs foo = NameAndArgs::split(input);
    BaseFactory<Production>::map::iterator factory = BaseFactory<Production>::lookup().find(foo.name);
    if (factory != BaseFactory<Production>::lookup().end())
        value = factory->second->create(foo.args);
    return;
}

template<>
string Option<Production*>::print() const
{
    return "This is the poineter to production";
}
