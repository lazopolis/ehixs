

#include <iomanip>
#include <fstream>

// C
#include <string.h>
#include <getopt.h>

#include "ihixs_ui.h"

int Option::get_type()
{
    if (type=="Required") return 1;
    else if (type=="Optional")  return 2;
    else if (type=="None") return 0;
    else
    {
        cout<<"\n wrong option option"<<endl;
        exit(1);
    }
}



IhixsUI::IhixsUI()
{
    options.push_back(new DoublePrecisionOption("Etot",0,"COM energy of the collider in GeV","Required",&Etot,7000.0));
    options.push_back(new DoublePrecisionOption("m_higgs",0,"higgs mass in GeV","Required",&m_higgs,125.0));
    options.push_back(new DoublePrecisionOption("epsrel",0,"vegas argument: target relative error","Required",&epsrel,0.01));
    options.push_back(new DoublePrecisionOption("epsabs",0,"vegas argument: target absolute error","Required",&epsabs,1e-10));
    
    options.push_back(new DoublePrecisionOption("mur",0,"mur (do not use this for Higgs production)","Required",&mur,80.0));
    options.push_back(new DoublePrecisionOption("muf",0,"muf (do not use this for Higgs production)","Required",&muf,80.0));
   
    options.push_back(new StringOption("pdf_provider",0,"pdf provider","Required",&pdf_provider,"none"));
    

    
    options.push_back(new StringOption("input_filename",'i',"filename to use as runcard","Required",&input_filename,"runcard"));
    options.push_back(new StringOption("output_filename",'o',"filename to write output","Required",&output_filename,"ehixs_output"));
    
    
    options.push_back(new StringOption(
                                       "qcd_perturbative_order",0,
                                       "LO, NLO, NNLO : ehixs will compute up to this order in a_s",
                                       "Required",
                                       &qcd_perturbative_order,
                                       "none"));
    
    options.push_back(new StringOption(
                                       "pdf_set",0,
                                       "choose a specific pdf set name (one from the LHAPDF6 list found at\
                                       lhapdf.hepforge.org/pdfsets.html). This set will be used irresspectively of order for the entire computation. This option is incompatible with pdf_provider. ",
                                       "Required",
                                       &pdf_set,
                                       "none"));
    
    options.push_back(new IntOption("verbose",0,"level of verbosity","Required",&verbose,2));
    options.push_back(new IntOption("mineval",0,"vegas argument: minimum points to be evaluated","Required",&mineval,200000));
    options.push_back(new IntOption("maxeval",0,"vegas argument: maximum points to be evaluated","Required",&maxeval,50000000));
    options.push_back(new IntOption("nstart",0,"vegas argument: #of points for first iteration","Required",&nstart,20000));
    options.push_back(new IntOption("nincrease",0,"vegas argument: # of points for step increase","Required",&nincrease,1000));
    
    options.push_back(new IntOption("perturbative_order_for_pdfs",0,
                                    "a_s perturbative order (0,1,2) for PDF choice and mass evolution",
                                    "Required",
                                    &perturbative_order_for_pdfs,2));
    
    
    options.push_back(new BoolOption("info",0,"information mode","Optional",&info,false));
    
    options.push_back(new BoolOption("help",0,"help with command line options","Optional",&help, false));
    
    options.push_back(new BoolOption("pdf_error",0,
                                     "whether or not to compute error due to pdfs: only used in ihixs++",
                                     "Required",
                                     &pdf_error,0));
    
    
}

void IhixsUI::ParseInput(int argc, char * const *argv)
{
    // parse command line arguments
    vector<vector<string> > parsed_options = ParseCmd(argc, argv, true);
    // look for runcard overwritting
    for (unsigned i=0;i<parsed_options.size();i++)
    {
        cout  <<"[ehixs] command line option: "<<parsed_options[i][0]
        <<" set to "<<parsed_options[i][1]<<endl;
        if (parsed_options[i][0]=="input_filename")
        {
            input_filename=parsed_options[i][1];
            break;
        }
    }
    // Read the (potentially user defined) filename
    ParseFile(input_filename, true);
    // Modify parameters that were declared in command line,
    // overwritting those of runcard
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


int IhixsUI::ParseFile(const string& in, bool verbose)
{
    // oooopen
    std::fstream file(in.c_str(), std::fstream::in);
    
    
    // and do the job
    if(!file.good()) {
        std::cout << "[ehixs] Tried to open runcard file named: " << in << " but failed!\nThe program will use default parameters overwritten by command line options." << std::endl;
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
                    
                    if(!did && verbose)
                    {
                        stringstream msg;
                        msg << "Unrecognised option in input file " << in << ": " << s.substr(0,pos);
                        throw(msg.str());
                    }
                }
            }
            
            // iterate
            file.getline(buff, 256);
        }
    }
    
    return 0;
}

option * IhixsUI::create_getopt_option_array()
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

string IhixsUI::create_getopt_optdesc()
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

void IhixsUI::print_help_message()
{
    std::cout << "Command line options have to be given the gnu way, i.e. '--full' or '-f'.\n\nOptions are" << std::endl;
    // Print all other options (version is included here)
    for(unsigned i=0; i<options.size(); i++)
    {
        std::cout << ' ' << std::setw(20) << std::left<< options[i]->name<< " "<< options[i]->desc << std::endl;
    }
    exit(1);
}

void IhixsUI::PrintAllOptions() const
{
    cout<<"\n-----------------------------------------------------------------";
    cout<<"UI options:"<<endl;
    for (int i=0;i<options.size();i++)
    {
        cout<<"\n"<<options[i]->name<<" : "<<options[i]->print();
    }
    cout<<"\n-----------------------------------------------------------------";
    cout<<endl;
}

vector<vector<string> > IhixsUI::ParseCmd(int argc,  char * const *argv, bool locverbose)
{
    cout<<"[ehixs] command line args: ";
    for (int i=0;i<argc;i++) cout<<argv[i]<<" ";
    cout<<endl;
    // getopt quirckiness:
    char * const * argv_for_getopt = argv;
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
    while((c = getopt_long(argc, argv_for_getopt, optdesc.c_str(), long_options, &option_index)) != -1)
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
            {stringstream msg;
                msg << "Unrecognised command line option";
                throw(msg.str());
                
                //exit(1);
                break;}
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







