/** \file parser.hpp
  *
  * Defines the option parser and related methods
  *
  * Remarks:
  *  - There is also a way to parse std::vectors with the following notation --opt '{a,b,c,...,d}'. You need the '' because otherwise the parser gets crazy.
  *  - There is another way (uniformize?) to pass std::vectors<unsigned>. Namely --opt x:y+a+b+c:d. Try a bit you will get it !
  *
  * \author Romain Mueller, muellrom@itp.phys.ethz.ch
  */

#ifndef PARSER_HPP
#define PARSER_HPP

// need stdlib
#include <stdlib.h>
#include<vector>
#include <sstream>
#include <iostream>
namespace Parser {

// ==================================================
// Option Descriptor

/** Types of arguments */
enum {
  NONE,
  REQUIRED,
  OPTIONAL
};

/** Option descriptor.
  *
  * Encloses all the information relevant for the option parsing. */
struct OptionDesc {
  /** Option type */
  int type;
  /** Full name */
  std::string name;
  /** Abreviated name */
  char short_name;
  /** Description (for output) */
  std::string desc;

  OptionDesc(const std::string& name_, char short_name_, const std::string& desc_, int type_);

  /** Set and match... */
  virtual void set(const std::string&) const
  {}
  /** Print... */
  virtual void print(std::ostream&) const
  {}
};

/** User defined conversions
  * 
  * We use types because of partial template specialization. Sometimes i just hate C++. See OptionDescT::set(). */
template<class T>
struct Conv
{
  inline T operator()(const std::string& str);
};

/** Template definition for OptionDesc 
  *
  * Probable issues:
  *  - flags
  *  - flags in input file
  */
template<class T>
struct OptionDescT : public OptionDesc {
  /** Generic pointer */
  T* p;

public:
  /** Constructor */
  OptionDescT(T* p_, const std::string& name_, char short_name_, const std::string& desc_, int type_)
    : OptionDesc(name_, short_name_, desc_, type_), p(p_)
  {}

  /** Call Cov<T>(str) */
  void set(const std::string& str) const
  { *p = Conv<T>()(str); }

  /** Print in stream */
  void print(std::ostream& stream) const
  { stream << *p; }
};

#if 0
/** Explicit template specialization for arrays */
template<class T>
inline void OptionDescT<std::vector<T> >::print(std::ostream& stream) const
{
  std::stringstream ss;

  ss << '{';
  if(!p->empty())
  {
    ss << *(p->begin());
    for(std::vector<T>::iterator i=p->begin()+1; i<p->end(); ++i)
      ss << ", " << *i;
  }
  ss << '}';

  stream << ss.str();
}
#endif

/** Explicit template specialization for unsigned arrays */
template<>
inline void OptionDescT<std::vector<unsigned> >::print(std::ostream& stream) const
{
  // to keep the iomanips
  std::stringstream ss;
  // some flag
  bool series = false;

  ss << '{';
  if(!p->empty())
  {
    std::vector<unsigned>::iterator i=p->begin()+1;

    ss << p->front();
    while(i<p->end())
    {
      series = false;
      // if we are in a 'series'
      // iterate until the end of it
      while( i<p->end() and *i == *(i-1)+1 ) {
        ++i;
        series = true;
      }
      // print series last value
      if(series) ss << ':' << *(i-1);
      // if we are at the end
      if( i == p->end() ) break;
      // print next value
      ss << ", " << *(i++);
    }
  }
  ss << '}';

  stream << ss.str();
}

// ==================================================
// Conversion functions 

template<>
inline double Conv<double>::operator()(const std::string& str)
{ return atof(str.c_str()); }

template<>
inline float Conv<float>::operator()(const std::string& str)
{ return atof(str.c_str()); }

template<>
inline int Conv<int>::operator()(const std::string& str)
{ return atoi(str.c_str()); }

template<>
inline unsigned Conv<unsigned>::operator()(const std::string& str)
{ return atoi(str.c_str()); }

template<>
inline std::string Conv<std::string>::operator()(const std::string& str)
{ return str; }

template<>
inline bool Conv<bool>::operator()(const std::string& str)
{ return str == "1"; }

/** Template specialization for arrays */
template<class T>
struct Conv<std::vector<T> >
{
  inline std::vector<T> operator()(const std::string& s)
  {
    std::vector<T> v;
    if(s.find('{')==0 && s.find('}')==s.length()-1)
    {
      std::string str;
      // erase spaces and copy
      for(unsigned i=1; i<s.length()-1; ++i)
        if(s[i] != ' ') str.push_back(s[i]);
      // do the job
      size_t p;
      do
      {
        // find comma
        p = str.find(',');
        // convert it
        v.push_back(Conv<T>()(str.substr(0, p)));
        // erase (+comma)
        str.erase(0, p+1);
      } while( p != str.npos );
    }

    return v;
  }
};

/** \brief Template specialization for unsigned arrays
  *
  * This is just to introduce the fancy a:b = {a, ..., b} notation. Enjoy ! */
template<>
inline std::vector<unsigned> Conv<std::vector<unsigned> >::operator()(const std::string& s)
{
  std::vector<unsigned> v;
  std::string    str = s;

  size_t p, q;
  do
  {
    // find '+' and ':'
    p=str.find('+');
    q=str.find(':');
    // dodododo
    if(q > p)
      v.push_back(Conv<unsigned>()(str.substr(0, p)));
    else
      for(unsigned i=Conv<unsigned>()(str.substr(0, q)); i<=Conv<unsigned>()(str.substr(q+1, p)); ++i)
        v.push_back(i);
    // erase the plus
    str.erase(0, p+1);
  } while(p != str.npos );

  return v;
}

// ==================================================
// User interface

/** Add OptionDesc pointer
  *
  * You should not use this function. Use Declare() instead. This has to be here because Declare() is template. */
void _add(OptionDesc*);

/** Declare a variable as a program option 
  *
  * For the memory freak: memory is never desallocated. */
template<class T>
void Declare(T* ptr, const std::string& name, char short_name, const std::string& desc, int type=REQUIRED)
{ _add(new OptionDescT<T>(ptr, name, short_name, desc, type)); }

/** Parse input file
  *
  * Last argument is to be set to true for initial parsing. */
int ParseFile(const std::string&, bool=true);

/** Parse command line arguments
  *
  * Last argument is to be set to true for initial parsing. */
int ParseCmd(int, char **, bool=true);

/** Set description
  *
  * The string will be printed when the user types --help. */
void Description(const std::string&);

/** Set version
  *
  * Will be printed when the user types --version. */
void Version(const std::string&);

/** Print to stream
  *
  * Prints all the variables with their values. */
void Print(std::ostream& s=std::cout);

} // namespace Parser

#endif 
