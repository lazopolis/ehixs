#include "userinterface.h"

#include <iomanip>
#include <fstream>

UserInterface::UserInterface()
{
    options.push_back(new Option<double>("Etot",0,"COM energy of the collider in GeV",Need::Required,&Etot,7000.0));
    options.push_back(new Option<double>("m_higgs",0,"higgs mass in GeV","Required",&m_higgs,125.0));

    options.push_back(new Option<double>("muf_over_mhiggs",0,"mu_f / m_h","Required",&muf_over_mhiggs,1.0));
    options.push_back(new Option<double>("mur_over_mhiggs",0,"mu_r / m_h","Required",&mur_over_mhiggs,1.0));

    options.push_back(new Option<double>("mur",0,"mur (do not use this for Higgs production)","Required",&mur,80.0));
    options.push_back(new Option<double>("muf",0,"muf (do not use this for Higgs production)","Required",&muf,80.0));

    options.push_back(new Option<double>("astar_m3",0,")virtuality of photon no 3","Required",&astar_m3,5.0));
    options.push_back(new Option<double>("astar_m4",0,")virtuality of photon no 4","Required",&astar_m4,5.0));


    options.push_back(new Option<string>("sector_name",0,"name of the production sector to run","Required",&sector_name,"none"));
    options.push_back(new Option<string>("input_filename",'i',"filename to use as runcard","Required",&input_filename,"runcard"));
    options.push_back(new Option<string>("output_filename",'o',"filename to write output","Required",&output_filename,"ehixs_output"));
    options.push_back(new Option<string>("matrix_element_approximation",0,"effective vs exact field theory for Gluon Fusion","Required",&matrix_element_approximation,"effective"));
    options.push_back(new Option<string>("leptonic_decay_mode_in_wwzz",0,"specifies the decay mode in WWZZ decay: eemumu,llll, lvlv, lvlv_interference","Required",&leptonic_decay_mode_in_wwzz, "none"));
    options.push_back(new Option<string>("xml_info",0,"file for xml-formatted info (only active with --info) ","Required",&xml_info, "none"));
    options.push_back(new Option<string>(
                                         "qcd_perturbative_order",0,
                                         "LO, NLO, NNLO : ehixs will compute up to this order in a_s",
                                         "Required",
                                         &qcd_perturbative_order,
                                         "none"));
    options.push_back(new Option<string>(
                                         "rr_treatment",0,
                                         "split or group : whether to split subsectors for each topology or to group them together",
                                         "Required",
                                         &rr_treatment,
                                         "none"));
    options.push_back(new Option<int>("perturbative_order",0,
                                      "a_s perturbative order (0,1,2) for PDF choice and mass evolution",
                                      "Required",
                                      &perturbative_order,2));
    options.push_back(new Option<int>("alpha_s_power",0,"a_s power  (0,1,2)","Required",&alpha_s_power,-1));
    options.push_back(new Option<int>("pole",'p',"pole coefficient degree (0,-1,-2,-3,...)","Required",&pole,0));
    options.push_back(new Option<int>("decay_sector",0,"decay sector: required when a decay with many sectors is hooked up, like WWZZ->llll or lvlv","Required",&decay_sector,-1));
    options.push_back(new Option<int>("alpha_ew_power",0,"a_ew power(0,1)","Required",&alpha_ew_power,-1));


    options.push_back(new Option<int>("sector_control",0,"sector id number used in bbH: DEPRECATED","Required",&sector_control,0));
    options.push_back(new Option<int>("requested_histogram",0,"requested histogram id number","Required",&requested_histogram,-1));
    options.push_back(new Option<int>("requested_cut",0,"requested cut id number","Required",&requested_cut,-1));

    options.push_back(new Option<bool>("info",0,"information mode","Optional",&info,false));
    options.push_back(new Option<bool>("histogram_info",0,"histograms available","Optional",&histogram_info, false));
    options.push_back(new Option<bool>("cut_info",0,"cuts available","Optional",&cut_info,false));
    options.push_back(new Option<bool>("list_processes",0,"list all implemented processes","Optional",&list_processes, false));
    options.push_back(new Option<bool>("help",0,"help with command line options","Optional",&help, false));
    options.push_back(new Option<bool>("show_me_list",0,"show all matrix elements declared","Optional",&show_me_list, false));
    options.push_back(new Option<bool>("pdf_error",0,
                                       "whether or not to compute error due to pdfs: only used in ihixs++",
                                       "Required",
                                       &pdf_error,0));
    options.push_back(new Option<bool>("ew_soft",0,
                                       "Soft NLO ew corrections: true/false (default false)",
                                       "Optional",
                                       &ew_soft,false));
    options.push_back(new Option<bool>("ew_h_plus_j",0,
                                       "Include real mixed QCD-ew corrections to h+j: true/false (default false)",
                                       "Optional",
                                       &ew_h_plus_j,false));
    options.push_back(new Option<bool>("only_ew_h_j",0,
                                       "Only compute real mixed QCD-ew corrections to h+j: true/false (default false)",
                                       "Optional",
                                       &only_ew_h_j,false));
    options.push_back(new Option<bool>("dummy_process",0,"indicate that this is a dummy_process, i.e. without a sector_name defined (used to get  a vector of sector names, for tests etc.)","Optional",&dummy_process, false));
    options.push_back(new Option<bool>("write_events",0,"write events to file (default is off)","Optional",&write_events, false));
    options.push_back(new Option<bool>("convolutions_by_interpolation",0,"convolutions by interpolation, as opposed to \'on the fly\' which requires on more monte carlo variable","Optional",&write_events, true));

    vector<string>empty_vector;
    options.push_back(new CutOption("cut",0,"generic cut option","Required",my_generic_cut, empty_vector));
    vector<string> default_hist;
    default_hist.push_back("20");
    default_hist.push_back("0.0");
    default_hist.push_back("100.0");

    options.push_back(new HistogramOption("histogram",0,"generic histogram option","Required",my_generic_cut,default_hist));
}

void UserInterface::ParseInput(int argc, char* const *argv)
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

    // reading cuts
    for (int i=0;i<options.size();i++)
    {
        if (options[i]->name=="cut")
        {
            CutOption* total_cut = (CutOption*)(options[i]);

            //cout<<"\n*************parsing the cut "<<endl;
            for (int j=0;j<total_cut->all_cut_values.size();j++)
            {
                string s = total_cut->all_cut_values[j];
                //cout<<"\n value = "<<s;
                size_t posbracket = s.find('[');
                size_t posfirstcomma = s.find(',');
                string cutname = s.substr(posbracket+1, posfirstcomma-1);
                //cout<<"\n cut name = "<<cutname<<endl;
                string rest_of_string = s.substr(posfirstcomma+1,s.npos);
                vector<string> cutvalues;
                size_t next_comma = rest_of_string.find(',');
                //cout<<"rest of string = "<<rest_of_string<<endl;

                while(next_comma != rest_of_string.npos)
                {
                    cutvalues.push_back(rest_of_string.substr(0,next_comma));
                    rest_of_string = rest_of_string.substr(next_comma+1,rest_of_string.npos);
                    next_comma = rest_of_string.find(',');
                }
                if (next_comma == rest_of_string.npos)
                {
                    size_t pos_sq_bracket = rest_of_string.find(']');
                    cutvalues.push_back(rest_of_string.substr(0,pos_sq_bracket));
                }
                //cout<<"values : ";
                //for (int i=0;i<cutvalues.size();i++) cout<<cutvalues[i]<<" | ";
                // construct the vector of string values
                all_cuts.push_back(new CutOption("cut",
                                                 0,
                                                 "cut",
                                                 "Required",
                                                 cutname,
                                                 cutvalues));
            }
        }

        if (options[i]->name=="histogram")
        {
            HistogramOption* total_hist = (HistogramOption*)(options[i]);

            //cout<<"\n*************parsing the histogram "<<endl;
            for (int j=0;j<total_hist->all_hist_values.size();j++)
            {
                string s = total_hist->all_hist_values[j];
                //cout<<"\n value = "<<s;
                size_t posbracket = s.find('[');
                size_t posfirstcomma = s.find(',');
                string histname = s.substr(posbracket+1, posfirstcomma-1);
                //cout<<"\n histogram name = "<<histname<<endl;
                string rest_of_string = s.substr(posfirstcomma+1,s.npos);
                vector<string> histvalues;
                size_t next_comma = rest_of_string.find(',');
                //cout<<"rest of string = "<<rest_of_string<<endl;

                while(next_comma != rest_of_string.npos)
                {
                    histvalues.push_back(rest_of_string.substr(0,next_comma));
                    rest_of_string = rest_of_string.substr(next_comma+1,rest_of_string.npos);
                    next_comma = rest_of_string.find(',');
                }
                if (next_comma == rest_of_string.npos)
                {
                    size_t pos_sq_bracket = rest_of_string.find(']');
                    histvalues.push_back(rest_of_string.substr(0,pos_sq_bracket));
                }
                //cout<<"values : ";
                //for (int i=0;i<histvalues.size();i++) cout<<histvalues[i]<<" | ";
                // construct the vector of string values
                all_hists.push_back(new HistogramOption("histogram",
                                                        0,
                                                        "histogram",
                                                        "Required",
                                                        histname,
                                                        histvalues));
            }
        }
    }
    for(int i=0;i<all_hists.size();i++)
    {
        //cout<<"\n[ehixs] requested histogram : "<<all_hists[i]->print();
    }
    if (help) print_help_message();
}


int UserInterface::ParseFile(const string& in, bool verbose)
{
    // oooopen
    fstream file(in.c_str(), fstream::in);

    // and do the job
    if(!file.good()) {
        cout << "[ehixs] Failed to open runcard file named: " << in << " !\nThe program will use default parameters overwritten by command line options." << endl;
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
            string s;
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
                    for(size_t i=0; i<options.size(); ++i)
                        if(s.substr(0, pos) == options[i]->name)
                        {
                            options[i]->set(s.substr(pos+1, s.npos));
                            //                                   cout<<"\n having recognised option "
                            //                                        <<options[i]->name
                            //                                        <<" in runcard, with value : "
                            //                                        <<s.substr(pos+1, s.npos);
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

option* UserInterface::create_getopt_option_array()
{
    // create the long_option array
    // it has #options elements
    size_t N = options.size();
    option* long_options = new option[N+1];
    // feed them
    for (size_t i = 0; i < N; ++i)
    {
        char* foo = new char[strlen(options[i]->name.c_str())+1];
        strcpy(foo, options[i]->name.c_str());
        long_options[i].name = foo;
        long_options[i].has_arg = options[i]->get_type();
        long_options[i].flag = NULL;
        long_options[i].val = options[i]->short_name;
    }
    // last element (viva old school C)
    long_options[N].name = NULL;
    long_options[N].has_arg = 0;
    long_options[N].flag = NULL;
    long_options[N].val = 0;

    return long_options;
}

string UserInterface::create_getopt_optdesc()
{
    // create short options descriptor
    string optdesc;
    for (size_t i=0; i < options.size(); ++i)
    {
        // help and version need special treatment later
        if (options[i]->short_name!=0) //: 0 is the default value for short_name when there is no short name
        {
            // feed the optiondesc string
            optdesc += options[i]->short_name;
           if (options[i]->get_type() == Need::Required) optdesc += ":";
           if ((*it)->need == Need::Optional) optdesc += "::";
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

void UserInterface::PrintAllOptions() const
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

void UserInterface::parseCmd(int argc, char* const *argv)
{
   cout << "[ehixs] command line args: ";
   for (int i = 0; i < argc; ++i) cout << argv[i] << " ";
   cout << endl;
   // getopt quirckiness:
   char* const *argv_for_getopt = argv;
   // create the long_option array
   option* long_options = create_getopt_option_array();
   // create short options descriptor
   string optdesc = create_getopt_optdesc();
   
   // Get all options with getopt_long
   int opti=0;
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
   while ( (c = getopt_long(argc, argv_for_getopt, optdesc.c_str(), long_options, &opti)) != -1 )
   {
      // do flag
      bool did = false;
      switch (c)
      {
         // long option
         case 0:
            BaseOption* foo = find(long_options[opti].name);
            if(optarg!=0) foo->set(optarg);
            else foo->set("true"); // for flags
            break;
            
         // getopt_long already printed an error message
         case '?':
            throw("Unrecognised command line option");
            break;
         
         // short option
         default:
            BaseOption* foo = find(c);
            if(optarg!=0) foo->set(optarg);
            else foo->set("true"); // for flags
            break;
      }
   }
   return;
}
