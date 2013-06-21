/** \file parser.cpp
  *
  * Implement parser.hpp.
  *
  * \author Romain Mueller, muellrom@itp.phys.ethz.ch
  */

#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <string>
#include <fstream>
// C
#include <string.h>
#include <getopt.h>

#include "parser.hpp"

OptionDesc::OptionDesc(const std::string& name_, char short_name_, const std::string& desc_, int type_)
  : name(name_), short_name(short_name_), desc(desc_)
{
  // convert from internal to getops standards
  switch(type_)
  {
    case OptionDesc::NONE:
      type = no_argument;
      break;
    case OptionDesc::REQUIRED:
      type = required_argument;
      break;
    case OptionDesc::OPTIONAL:
      type = optional_argument;
      break;
  }
}

OptionParser::OptionParser(const std::string& desc_)
  : desc(desc_)
{
  // if you add something here you should go to parse_cmd and set 2-> 3...
  declare((bool*)NULL, "help", 1, "prints this message", OptionDesc::NONE);
  declare((bool*)NULL, "version", 2, "prints version", OptionDesc::NONE);
}

int OptionParser::parse_file(const std::string& in, bool verbose) const
{
  std::fstream file(in.c_str(), std::fstream::in);

  if(!file.good()) {
    std::cout << "Error while opening runcard file " << in << "." << std::endl;
    return 1;
  } else
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

int OptionParser::parse_cmd(int argc, char **argv, bool verbose) const
{
  // create the long_option array
  option *long_options = new option[options.size()+1];
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

    if(options[i]->short_name>2) {
      optdesc += options[i]->short_name;
      if(options[i]->type == required_argument) optdesc += ":";
      if(options[i]->type == optional_argument) optdesc += "::";
    }
  }

  // last element
  long_options[options.size()].name = NULL;
  long_options[options.size()].has_arg = 0;
  long_options[options.size()].flag = NULL;
  long_options[options.size()].val = 0;

  // Get opts with getopt_long
  int option_index=0;
  int c;
  // reinit global (?) vars...
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
          for(unsigned i=0; i<options.size(); ++i)
            std::cout << ' ' << std::setw(20) << std::left << options[i]->name+( (i>1 && options[i]->short_name!=0) ? std::string(", ")+options[i]->short_name : std::string()) << " " << options[i]->desc << std::endl;
          exit(1);
        }
        break;
      // version
      case 2:
        if(!verbose) {
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
            // for flags
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

/** Ostream output */
std::ostream& operator<<(std::ostream& stream, const OptionParser& parser)
{
  // starting from 2 becaus eof help and version
  for(unsigned i=2; i<parser.options.size(); ++i)
  {
    stream << std::left << std::setw(40) << parser.options[i]->name << " = ";
    stream << std::right << std::setw(17);
      parser.options[i]->print(stream);
    stream << std::endl;
  }

  return stream;
}
