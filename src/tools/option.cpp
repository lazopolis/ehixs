/**
 *
 * \file    option.cpp
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "option.h"
#include <iostream>

NameAndArgs NameAndArgs::split(const string& input, const string delimiters, const string start)
{
    NameAndArgs foo;
    size_t pos1 = input.find_first_of(start);
    size_t pos2 = input.find_first_of(delimiters);
    foo.name = input.substr(pos1+1,pos2-pos1-1);

    pos1 = pos2;
    pos2 = input.find_first_of(delimiters,pos1+1);
    while (pos2 != input.npos)
    {
        foo.args.push_back(input.substr(pos1+1,pos2-pos1-1));
        pos1 = pos2;
        pos2 = input.find_first_of(delimiters,pos1+1);
    }
    return foo;
}

void NameAndArgs::checksize(const string& fname, const vector<string>& args, const size_t n)
{
    if (args.size() != n) {
        cerr << to_string(args.size());
        cerr << " arguments passed to " + fname + " instead of ";
        cerr << to_string(n) << ": fatal error." << endl;
        throw;
    }
    return;
}


template<>
void Option<string>::set(const string& input)
{
    value = input;
    return;
}

template<>
void Option<double>::set(const string& input)
{
    value = stod(input);
    return;
}

template<>
void Option<int>::set(const string& input)
{
    value = stoi(input);
    return;
}

template<>
void Option<bool>::set(const string& input)
{
    value = (input=="true" or input=="True");
    return;
}

template<>
void Option<NameAndArgs>::set(const string& input)
{
    value = NameAndArgs::split(input);
    return;
}

template<>
string Option<string>::print() const
{
    return value;
}

template<>
string Option<double>::print() const
{
    return to_string(value);
}

template<>
string Option<int>::print() const
{
    return to_string(value);
}

template<>
string Option<bool>::print() const
{
    if (value) return "True";
    else return "False";
}

template<>
string Option<NameAndArgs>::print() const
{
    string foo = value.name;
    for (vector<string>::const_iterator it = value.args.cbegin(); it != value.args.cend(); ++it)
        foo = foo+" "+(*it);
    return foo;
}

BaseOption* OptionSet::find(const string& name)
{
   for (vector<BaseOption*>::iterator it = _opts().begin(); it != _opts().end(); ++it)
      if ((*it)->name == name) return *it;
   cerr << "[OptionSet] Warning: option " << name << " not found." << endl;
   return NULL;
}

BaseOption* OptionSet::find(const char& name)
{
   for (vector<BaseOption*>::iterator it = _opts().begin(); it != _opts().end(); ++it)
      if ((*it)->abbr == name) return *it;
   cerr << "[OptionSet] Warning: short option " << name << " not recognized." << endl;
   return NULL;
}


vector<BaseOption*>& OptionSet::_opts()
{
   static vector<BaseOption*>* __opts = new vector<BaseOption*>();
   return *__opts;
}
