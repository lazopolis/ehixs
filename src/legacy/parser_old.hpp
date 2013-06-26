/** \file parser.hpp
  *
  * Defines the option parser and related methods
  *
  * \author Romain Mueller, muellrom@itp.phys.ethz.ch
  */

#ifndef PARSER_HPP
#define PARSER_HPP

// GCC bug ???
// See http://gcc.gnu.org/gcc-4.3/porting_to.html, "Explicit template specialization cannot have a storage class".
#if __GNUC__ < 4  || ( __GNUC__ == 4  && __GNUC_MINOR__ < 3 )
  #define STATIC_TMPLSP static
#else
  // This is ISO-C++
  #define STATIC_TMPLSP
#endif

// need stdlib
#include <stdlib.h>

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

  /** Types of arguments */
  enum {
    NONE,
    REQUIRED,
    OPTIONAL
  };

  OptionDesc(const std::string& name_, char short_name_, const std::string& desc_, int type_);

  /** Set and match... */
  virtual void set(const std::string&) const = 0;
  /** print... */
  virtual void print(std::ostream&) const = 0;
};

/** User defined conversion functions
  * 
  * See OptionDescT::set(). */
template<class T>
static T Conv(const std::string& str)
{}

template<>
STATIC_TMPLSP double Conv<double>(const std::string& str)
{ return atof(str.c_str()); }

template<>
STATIC_TMPLSP float Conv<float>(const std::string& str)
{ return atof(str.c_str()); }

template<>
STATIC_TMPLSP int Conv<int>(const std::string& str)
{ return atoi(str.c_str()); }

template<>
STATIC_TMPLSP unsigned Conv<unsigned>(const std::string& str)
{ return atoi(str.c_str()); }

template<>
STATIC_TMPLSP std::string Conv<std::string>(const std::string& str)
{ return str; }

template<>
STATIC_TMPLSP bool Conv<bool>(const std::string& str)
{ return str == "1"; }

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
  { *p = Conv<T>(str); }

  /** Print in stream */
  void print(std::ostream& stream) const
  { stream << *p; }
};

/** Option parser
  *
  * More info... */
class OptionParser {
  /** Vector containing all options */
  std::vector<OptionDesc*> options;
  /** Some description of the program printed with --help */
  std::string desc;

public:
  /** Constructor 
    *
    * Add standard stuff like --help, --version. */
  OptionParser(const std::string& = "");
  /** Desalocate memory */
  ~OptionParser()
  {
    for(std::vector<OptionDesc*>::iterator i=options.begin(); i<options.end(); ++i)
      delete *i;
  }

  /** Declare a variable as a program option 
    *
    * ... */
  template<class T>
  void declare(T* ptr, const std::string& name, char short_name, const std::string& desc, int type=OptionDesc::REQUIRED)
  { options.push_back(new OptionDescT<T>(ptr, name, short_name, desc, type)); }

  /** Parse input file
    *
    * Last argument is to be set to true for initial parsing. */
  int parse_file(const std::string&, bool=true) const;
  /** Parse command line arguments
    *
    * Last argument is to be set to true for initial parsing. */
  int parse_cmd(int, char **, bool=true) const;

  /** Set description */
  void set_desc(const std::string& desc_)
  { desc=desc_; }

  friend std::ostream& operator<<(std::ostream&, const OptionParser&);
};

#endif 
