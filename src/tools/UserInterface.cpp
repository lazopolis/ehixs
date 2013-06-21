

#include <iomanip>
#include <fstream>
// C
#include <string.h>
#include <getopt.h>


#include "UserInterface.h"


UserInterface::UserInterface()
{
     options.push_back(new DoublePrecisionOption("Etot",0,"COM energy of the collider","Required",&Etot,8000.0));
     options.push_back(new DoublePrecisionOption("m_higgs",0,"higgs mass in GeV","Required",&m_higgs,125.0));
     options.push_back(new DoublePrecisionOption("epsrel",0,"vegas argument: target relative error","Required",&epsrel,0.01));
     options.push_back(new DoublePrecisionOption("epsabs",0,"vegas argument: target absolute error","Required",&epsabs,0.0));
     options.push_back(new DoublePrecisionOption("muf_over_mhiggs",0,"mu_f / m_h","Required",&muf_over_mhiggs,0.5));
     options.push_back(new DoublePrecisionOption("mur_over_mhiggs",0,"mu_r / m_h","Required",&mur_over_mhiggs,0.5));
     options.push_back(new DoublePrecisionOption("number_of_flavours",0,"number of active flavors (do not change)","Required",&number_of_flavours,5.0));

     options.push_back(new StringOption("production",0,"production process","Required",&production,"ggF"));
     options.push_back(new StringOption("decay",0,"decay process","Required",&decay,""));
     options.push_back(new StringOption("pdf_provider",0,"pdf provider","Required",&pdf_provider,"MSTW"));
     options.push_back(new StringOption("sector_name",0,"name of the production sector to run","Required",&sector_name,"none"));
     options.push_back(new StringOption("sector_for_production",'s',"number of the production sector to run: attention: the id number depends on other user defined parameters, like the channel, the perturbative order, the pole etc.  ","Required",&sector_for_production,"none"));
     options.push_back(new StringOption("input_filename",'i',"filename to use as runcard","Required",&input_filename,"runcard"));
     options.push_back(new StringOption("output_filename",'o',"filename to write output","Required",&output_filename,"ehixs_output"));
     options.push_back(new StringOption("matrix_element_approximation",0,"effective vs exact field theory for Gluon Fusion","Required",&matrix_element_approximation,"effective"));
     options.push_back(new StringOption("Fleft",0,"specifies the flavor of the parton on left beam","Required",&Fleft, "none"));
     options.push_back(new StringOption("Fright",0,"specifies the flavor of the parton on right beam","Required",&Fright, "none"));


     options.push_back(new IntOption("verbose",0,"level of verbosity","Required",&verbose,2));
     options.push_back(new IntOption("mineval",0,"vegas argument: minimum points to be evaluated","Required",&mineval,20000));
     options.push_back(new IntOption("maxeval",0,"vegas argument: maximum points to be evaluated","Required",&maxeval,50000000));
     options.push_back(new IntOption("nstart",0,"vegas argument: #of points for first iteration","Required",&nstart,20000));
     options.push_back(new IntOption("nincrease",0,"vegas argument: # of points for step increase","Required",&nincrease,1000));
     options.push_back(new IntOption("perturbative_order",0,"a_s perturbative order (0,1,2)","Required",&perturbative_order,2));
     options.push_back(new IntOption("pole",'p',"pole coefficient degree (0,-1,-2,-3,...)","Required",&pole,0));
     options.push_back(new IntOption("decay_sector",0,"decay sector: DEPRECATED","Required",&decay_sector,0));
     options.push_back(new IntOption("pdf_error",0,"whether or not to compute error due to pdfs: DEPRECATED","Required",&pdf_error,0));
     options.push_back(new IntOption("sector_control",0,"sector id number used in bbH: DEPRECATED","Required",&sector_control,0));

     options.push_back(new BoolOption("info",0,"information mode","Optional",&info,false));
     options.push_back(new BoolOption("histogram_info",0,"histograms available","Optional",&histogram_info, false));
     options.push_back(new BoolOption("cut_info",0,"cuts available","Optional",&cut_info,false));
     options.push_back(new BoolOption("list_processes",0,"list all implemented processes","Optional",&list_processes, false));
     options.push_back(new BoolOption("help",0,"help with command line options","Optional",&help, false));
}

void UserInterface::ParseInput(int argc, char **argv)
{
     // parse command line arguments
     vector<vector<string> > parsed_options = ParseCmd(argc, argv, true);
     // look for runcard overwritting
     for (unsigned i=0;i<parsed_options.size();i++)
          {
          cout<<"command line option: "<<parsed_options[i][0]<<" set to "<<parsed_options[i][1]<<endl;
          if (parsed_options[i][0]=="input_filename")
               {
               input_filename=parsed_options[i][1];
               break;
               }
          }
     // Read the (potentially user defined) filename
     ParseFile(input_filename, true);
     // Modify parameters that were declared in command line, overwritting those of runcard
     for (unsigned i=0;i<parsed_options.size();i++)
          {
          for (unsigned j=0;j<options.size();j++)
               {
               if (parsed_options[i][0]==options[j]->name)
                    {
                    options[j]->set(parsed_options[i][1]);
                    break;
                    }
               }
          
          }
     if (help) print_help_message();
}


int UserInterface::ParseFile(const string& in, bool verbose)
{
     // oooopen
     std::fstream file(in.c_str(), std::fstream::in);
     

     // and do the job
     if(!file.good()) {
          std::cout << "Tried to open runcard file named: " << in << " but failed!\nThe program will use default parameters overwritten by command line options." << std::endl;
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

option * UserInterface::create_getopt_option_array()
{
     // create the long_option array
     // it has #options elements
     int N=int(options.size());
     option *long_options = new option[N+1];
     // feed them
     for(unsigned i=0; i<N; ++i)
          {
          long_options[i].name = new char[strlen(options[i]->name.c_str())];
          strcpy(const_cast<char*>(long_options[i].name), options[i]->name.c_str());
          long_options[i].has_arg = options[i]->get_type();
          long_options[i].flag = NULL;
          long_options[i].val = options[i]->short_name;
          }
     // last element (viva old school C)
     long_options[N+1].name = NULL;
     long_options[N+1].has_arg = 0;
     long_options[N+1].flag = NULL;
     long_options[N+1].val = 0;


     return long_options;
}

string UserInterface::create_getopt_optdesc()
{
     // create short options descriptor
     std::string optdesc;
     for(unsigned i=0; i<options.size(); ++i)
          {
          // help and version need special treatment later
          if(options[i]->short_name!=0) //: 0 is the default value for short_name when there is no short name
               {
               // feed the optiondesc string
               optdesc += options[i]->short_name;
               if(options[i]->get_type() == required_argument) optdesc += ":";
               if(options[i]->get_type() == optional_argument) optdesc += "::";
               }
          }
     return optdesc;
}

void UserInterface::print_help_message()
{
     std::cout << "Command line options have to be given the gnu way, i.e. '--full' or '-f'.\n\nOptions are" << std::endl;
     // Print all other options (version is included here)
     for(unsigned i=0; i<options.size(); i++)
          {
          std::cout << ' ' << std::setw(20) << std::left<< options[i]->name<< " "<< options[i]->desc << std::endl;
          }
     exit(1);
}

vector<vector<string> > UserInterface::ParseCmd(int argc, char **argv, bool locverbose)
{
     // create the long_option array
     option *long_options = create_getopt_option_array();
     // create short options descriptor
     string optdesc=create_getopt_optdesc();
     // we will put all found options in parsed_options
     vector<vector<string> > parsed_options;
     
     // Get all options with getopt_long     
     int option_index=0;
     int c;
     // reinit global (?) vars... (i do not get it but seems to work)
     
     optarg=0;
     optind=0;
     //: getopt_long returns a character equal to
     //: 0 for any other LONG option
     //: -1 for the end of parsing
     //: 'x' for any short option corresponding to short-name='x'
     //: in the case 0, the variable option_index holds
     //: the position of the recognized option in the long_option array
     while((c = getopt_long(argc, argv, optdesc.c_str(), long_options, &option_index)) != -1)
          {
          // do flag
          bool did=false;
          switch (c)
               {
                    // --long_option
                    case 0:
                    // loop over other possibilities
                    for(unsigned i=0; i<options.size(); ++i)
                         {
                         // check if indeed the name in long_options is the same as the name in options
                         if( options[i]->name == long_options[option_index].name )
                              {
                              vector<string> loc_parsed_opt;
                              loc_parsed_opt.push_back(options[i]->name);
                              did = true;
                              
                              
                              // for normal options
                              if(optarg!=0)
                                   {
                                   string locarg = optarg;
                                   loc_parsed_opt.push_back(optarg);
                                   }
                              // for flags
                              else loc_parsed_opt.push_back("true");//options[i]->set("1");
                              parsed_options.push_back(loc_parsed_opt);
                              break;
                              }
                         }

                    break;
                    
                    case '?':// getopt_long already printed an error message.
                    //exit(1);
                    break;
                    // short options
                    default:
                    // loop over expected short options
                    for(unsigned i=0; i<options.size(); ++i)
                         {
                         if( c == options[i]->short_name )
                              {
                              vector<string> loc_parsed_opt;
                              loc_parsed_opt.push_back(options[i]->name);
                              did = true;
                              // for normal options
                              if(optarg)
                                   {
                                   loc_parsed_opt.push_back(optarg);
                                   }
                              // for flags
                              else loc_parsed_opt.push_back("true");
                              parsed_options.push_back(loc_parsed_opt);
                              break;
                              }
                         }
                    }
          }
     

     return parsed_options;
}