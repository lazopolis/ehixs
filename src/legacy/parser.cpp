/** \file parser.cpp
  *
  * Implement parser.hpp.
  *
  * \author Romain Mueller, muellrom@itp.phys.ethz.ch
  */

//#include "headers.hpp"
#include <iomanip>
#include <fstream>
#include "parser.hpp"
// C
#include <string.h>
#include <getopt.h>
namespace Parser {

// ==================================================
// Some global stuff

/** Vector containing all options */
std::vector<OptionDesc*> options;
/** Some description of the program printed with --help */
std::string desc;
/** Version */
std::string version;

OptionDesc::OptionDesc(const std::string& name_, char short_name_, const std::string& desc_, int type_)
  : name(name_), short_name(short_name_), desc(desc_)
{
  // convert from internal to getops standards
  switch(type_)
  {
    case NONE:
      type = no_argument;
      break;
    case REQUIRED:
      type = required_argument;
      break;
    case OPTIONAL:
      type = optional_argument;
      break;
  }
}

void Version(const std::string& v)
{
  // copy
  version = v;
  // register special
  Declare((bool*)NULL, "version", 2, "prints version", NONE);
}

void Description(const std::string& d)
{
  // copy
  desc = d;
}

void _add(OptionDesc* p)
{ options.push_back(p); }

int ParseFile(const std::string& in, bool verbose)
{
  // oooopen
  std::fstream file(in.c_str(), std::fstream::in);

  // and do the job
  if(!file.good()) {
    std::cout << "Error while opening runcard file " << in << "." << std::endl;
    return 1;
  }
  else
  {
    char buff[256];
    // get first line
    file.getline(buff, 256);
    // loop over lines
    for(unsigned c=1; !file.eof(); ++c)
    {
      // convert to string
      std::string s;
      // ...discard spaces and comments
      for(char *p=buff; *p!='\0'; ++p)
        if(*p == '#')
          break;
        else
          if(*p != ' ')
            s += *p;

      // non-empty lines
      if(!s.empty())
      {
        size_t pos = s.find('=');

        // Invalid line ?
        if(pos == s.npos)
        {
          if(verbose) {
            std::cout << "Invalid line in input file " << in << ":\n" << c << ": " << s << std::endl;
            return 1;
          }
        }
        else
        {
          // valid line !
          bool did=false;
          for(unsigned i=0; i<options.size(); ++i)
            if(s.substr(0, pos) == options[i]->name)
            {
              options[i]->set(s.substr(pos+1, s.npos));
              did=true;
              break;
            }

          if(!did && verbose) {
            std::cout << "Unexpected option in input file " << in << ": " << s << std::endl;
            return 1;
          }
        }
      }

      // iterate
      file.getline(buff, 256);
    }
  }

  return 0;
}

int ParseCmd(int argc, char **argv, bool verbose)
{
  // create the long_option array
  option *long_options = new option[options.size()+2];
  // create short options descriptor
  std::string optdesc;

  // feed them
  for(unsigned i=0; i<options.size(); ++i)
  {
    long_options[i].name = new char[strlen(options[i]->name.c_str())];
    strcpy(const_cast<char*>(long_options[i].name), options[i]->name.c_str());
    long_options[i].has_arg = options[i]->type;
    long_options[i].flag = NULL;
    long_options[i].val = options[i]->short_name;

    // help and version need special treatment later
    if(options[i]->short_name>2) {
      // feed the optiondesc string
      optdesc += options[i]->short_name;
      if(options[i]->type == required_argument) optdesc += ":";
      if(options[i]->type == optional_argument) optdesc += "::";
    }
  }

  // help is always there
  char help_str[] = "help";
  long_options[options.size()].name    = help_str;
  long_options[options.size()].has_arg = no_argument;
  long_options[options.size()].flag    = NULL;
  long_options[options.size()].val     = 1;

  // last element (viva old school C)
  long_options[options.size()+1].name = NULL;
  long_options[options.size()+1].has_arg = 0;
  long_options[options.size()+1].flag = NULL;
  long_options[options.size()+1].val = 0;

  // Get opts with getopt_long
  int option_index=0;
  int c;
  // reinit global (?) vars... (i do not get it but seems to work)
  optarg=0;
  optind=0;
  while((c = getopt_long(argc, argv, optdesc.c_str(), long_options, &option_index)) != -1) 
  {
    // do flag
    bool did=false;
    switch (c)
    {
      // help
      case 1:
        if(verbose) {
          // print user defined message
          if(!desc.empty())
            std::cout << desc << std::endl;
          // Prints help message
          std::cout << "Command line options have to be given the gnu way, i.e. '--full' or '-f'.\n\nOptions are" << std::endl;
          // Print for --help
            std::cout << ' ' << std::setw(20) << std::left << "help" << " " << "prints this friendly message" << std::endl;
          // Print all other options (version is included here)
          for(unsigned i=0; i<options.size(); ++i)
            std::cout << ' ' << std::setw(20) << std::left 
              // name (+short name)
              << options[i]->name+( (i>1 && options[i]->short_name!=0) ? std::string(", ")+options[i]->short_name : std::string()) << " " 
              // description
              << options[i]->desc << std::endl;
          exit(1);
        }
        break;
      // version
      case 2:
        if(verbose) {
          std::cout << version << std::endl;
          exit(1);
        }
        break;
      // Long option without short name
      case 0:
        // loop over other possibilities
        for(unsigned i=0; i<options.size(); ++i)
          if( options[i]->name == long_options[option_index].name )
          {
            did = true;
            // for normal options
            if(optarg) options[i]->set(optarg);
            // for flags (those types have to define the proper conversion)
            else options[i]->set("1");
            
            break;
          }

        if(!did && verbose) {
          std::cout << argv[0] << ": unexpected long option: " << c;
          return 1;
        }
        break;
      // beuh ?!
      case '?':
        return 1;
        break;
      // Other (possibly) known options
      default:
        // loop over other possibilities
        for(unsigned i=0; i<options.size(); ++i)
          if( c == options[i]->short_name )
          {
            did = true;
            // for normal options
            if(optarg) options[i]->set(optarg);
            // for flags
            else options[i]->set("1");
            
            break;
          }

        if(!did && verbose) {
          std::cout << argv[0] << ": unexpected option: getopt returned character code 0" << c;
          return 1;
        }
    }
  }

  if(optind < argc)
  {
    std::cout << argv[0] << ": non-option ARGV-elements: ";
    while (optind < argc)
      std::cout << argv[optind++];
    std::cout << std::endl;

    return 1;
  }

  return 0;
}

void Print(std::ostream& stream)
{
  for(unsigned i=0; i<options.size(); ++i)
  {
    // do not print version
    if(options[i]->short_name == 0 or options[i]->short_name > 2)
    {
      stream << std::left << std::setw(40) << options[i]->name << " = ";
      stream << std::right << std::setw(17);
      options[i]->print(stream);
      stream << std::endl;
    }
  }
}

} // namespace Parser
