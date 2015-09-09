/**
 *
 * \file    parser.cpp
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2015
 *
 */

#include "parser.h"

#include <iostream> // cout, cerr
#include <stdlib.h> // exit()
#include <iomanip>  // left, setw
#include <fstream>  // fstream

Parser::Parser()
{
    _opts().push_back(new Option<string>("inputfile",'i',
                                         "filename to use as runcard",
                                         Arg::Required,inputfile,""));
    _opts().push_back(new Option<string>("outputfile",'o',
                                         "filename to write output",
                                         Arg::Required,outputfile,""));
    _opts().push_back(new Option<bool>  ("help",0,
                                         "help with command line options",
                                         Arg::Optional,_help,false));

//    options.push_back(new Option<double>("Etot",0,"COM energy of the collider in GeV",Arg::Required,&Etot,7000.0));
//    options.push_back(new Option<double>("m_higgs",0,"higgs mass in GeV","Required",&m_higgs,125.0));
//
//    options.push_back(new Option<double>("mur",0,"mur (do not use this for Higgs production)","Required",&mur,80.0));
//    options.push_back(new Option<double>("muf",0,"muf (do not use this for Higgs production)","Required",&muf,80.0));
//
//    options.push_back(new Option<double>("astar_m3",0,")virtuality of photon no 3","Required",&astar_m3,5.0));
//    options.push_back(new Option<double>("astar_m4",0,")virtuality of photon no 4","Required",&astar_m4,5.0));

//    options.push_back(new Option<string>("xml_info",0,"file for xml-formatted info (only active with --info) ","Required",&xml_info, "none"));
//    options.push_back(new Option<string>(
//                                         "qcd_perturbative_order",0,
//                                         "LO, NLO, NNLO : ehixs will compute up to this order in a_s",
//                                         "Required",
//                                         &qcd_perturbative_order,
//                                         "none"));
//    options.push_back(new Option<string>(
//                                         "rr_treatment",0,
//                                         "split or group : whether to split subsectors for each topology or to group them together",
//                                         "Required",
//                                         &rr_treatment,
//                                         "none"));
//    options.push_back(new Option<int>("perturbative_order",0,
//                                      "a_s perturbative order (0,1,2) for PDF choice and mass evolution",
//                                      "Required",
//                                      &perturbative_order,2));
//    options.push_back(new Option<int>("alpha_s_power",0,"a_s power  (0,1,2)","Required",&alpha_s_power,-1));
//    options.push_back(new Option<int>("pole",'p',"pole coefficient degree (0,-1,-2,-3,...)","Required",&pole,0));
//    options.push_back(new Option<int>("decay_sector",0,"decay sector: required when a decay with many sectors is hooked up, like WWZZ->llll or lvlv","Required",&decay_sector,-1));
//    options.push_back(new Option<int>("alpha_ew_power",0,"a_ew power(0,1)","Required",&alpha_ew_power,-1));
//
//
//    options.push_back(new Option<int>("sector_control",0,"sector id number used in bbH: DEPRECATED","Required",&sector_control,0));
//    options.push_back(new Option<int>("requested_histogram",0,"requested histogram id number","Required",&requested_histogram,-1));
//    options.push_back(new Option<int>("requested_cut",0,"requested cut id number","Required",&requested_cut,-1));
//
//    options.push_back(new Option<bool>("info",0,"information mode","Optional",&info,false));
//    options.push_back(new Option<bool>("histogram_info",0,"histograms available","Optional",&histogram_info, false));
//    options.push_back(new Option<bool>("cut_info",0,"cuts available","Optional",&cut_info,false));
//    options.push_back(new Option<bool>("list_processes",0,"list all implemented processes","Optional",&list_processes, false));
//    options.push_back(new Option<bool>("show_me_list",0,"show all matrix elements declared","Optional",&show_me_list, false));
//    options.push_back(new Option<bool>("pdf_error",0,
//                                       "whether or not to compute error due to pdfs: only used in ihixs++",
//                                       "Required",
//                                       &pdf_error,0));
//    options.push_back(new Option<bool>("ew_soft",0,
//                                       "Soft NLO ew corrections: true/false (default false)",
//                                       "Optional",
//                                       &ew_soft,false));
//    options.push_back(new Option<bool>("ew_h_plus_j",0,
//                                       "Include real mixed QCD-ew corrections to h+j: true/false (default false)",
//                                       "Optional",
//                                       &ew_h_plus_j,false));
//    options.push_back(new Option<bool>("only_ew_h_j",0,
//                                       "Only compute real mixed QCD-ew corrections to h+j: true/false (default false)",
//                                       "Optional",
//                                       &only_ew_h_j,false));
//    options.push_back(new Option<bool>("dummy_process",0,"indicate that this is a dummy_process, i.e. without a sector_name defined (used to get  a vector of sector names, for tests etc.)","Optional",&dummy_process, false));
//    options.push_back(new Option<bool>("write_events",0,"write events to file (default is off)","Optional",&write_events, false));
//    options.push_back(new Option<bool>("convolutions_by_interpolation",0,"convolutions by interpolation, as opposed to \'on the fly\' which requires on more monte carlo variable","Optional",&write_events, true));
}

void Parser::print_help_message()
{
    cout << "[Parser] Command line options have to be given the GNU way, i.e. '--full' or '-f'." << endl;
    cout << "[Parser] Options are:" << left << endl;
    // Print all other options (version is included here)
    for (OptionSet::iterator it = _opts().begin(); it != _opts().end(); ++it)
        cout << setw(21) << "[Parser]" << setw(21) << (*it)->name << " " << (*it)->desc << endl;
    exit(1);
}

void Parser::print() const
{
    cout<<"\n-----------------------------------------------------------------";
    cout<<"UI options:"<<endl;
    for (OptionSet::iterator it = _opts().begin(); it != _opts().end(); ++it)
    {
        cout << "\n" << (*it)->name << " : " << (*it)->print() << endl;
    }
    cout<<"\n-----------------------------------------------------------------";
    cout<<endl;
}


void Parser::parse(int argc, char* const *argv)
{
    // Parse command line arguments
    parseCmd(argc, argv);
    // Read the (potentially user defined) filename
    parseFile();

    if (_help) print_help_message();
}


int Parser::parseFile()
{
    // Open file if appropriate
    if (inputfile=="") return 0;
    fstream file(inputfile,fstream::in);
    if(!file.good()) {
        cerr << "[Parser] Failed to open runcard file named: " << inputfile;
        cerr << " !\nThe program will use default parameters overwritten by command line options." << endl;
        return 1;
    }

    // Read and set options
    string buff;
    while (!file.eof())
    {
        getline(file,buff);
        // Skip comments and empty lines
        if (buff.front() != '#' && !buff.empty())
        {
            // Get rid of whitespaces
            for (string::iterator it = buff.begin(); it != buff.end(); ++it)
                if (*it == ' ') buff.erase(it);

            // Check for '='
            size_t pos = buff.find('=');
            if (pos == buff.npos)
                cerr << "[Parser] Warning: invalid line in input file " << inputfile << ":\n" << buff << endl;
            else {
                BaseOption* foo = find(buff.substr(0,pos));
                if (foo) foo->set(buff.substr(pos+1));
            }
        }
    }
    return 0;
}

void Parser::parseCmd(int argc, char* const *argv)
{
    cout << "[Parser] Command line args: ";
    for (int i = 0; i < argc; ++i) cout << argv[i] << " ";
    cout << endl;
    // getopt quirckiness:
    char* const *argv_for_getopt = argv;
    // create the long_option array
    option* long_options = _longopts();
    // create short options descriptor
    string optdesc = _shortopts();

    // Make getopt verbose
    optarg=0;

    // Get all options with getopt_long
    int opti=0;
    int c;

    //: getopt_long returns a character equal to
    //: 0 for any other LONG option
    //: -1 for the end of parsing
    //: 'x' for any short option corresponding to short-name='x'
    //: in the case 0, the variable option_index holds
    //: the position of the recognized option in the long_option array
    while ( (c = getopt_long(argc, argv_for_getopt, optdesc.c_str(), long_options, &opti)) != -1 )
    {
        BaseOption* foo = 0;
        switch (c)
        {
            // long option
            case 0:
                foo = find(long_options[opti].name);
                if (foo) {
                    if(optarg!=0) foo->set(optarg);
                    else foo->set("true"); // for flags
                }
                break;

            // getopt_long already printed an error message
            case '?':
                throw("Unrecognised command line option");
                break;

            // short option
            default:
                foo = find(c);
                if (foo) {
                    if(optarg!=0) foo->set(optarg);
                    else foo->set("true"); // for flags
                }
                break;
        }
    }
    return;
}

option* Parser::_longopts()
{
    // create the long_option array
    // it has #options elements
    const size_t N = _opts().size();
    option* long_options = new option[N+1];
    // feed them
    for (size_t i = 0; i < N; ++i)
//    {
//        char* foo = new char[strlen(_opts()[i]->name.c_str())+1];
//        strcpy(foo,_opts()[i]->name.c_str());
//        long_options[i].name = foo;
//        long_options[i].name = _opts()[i]->name.data();
//        long_options[i].has_arg = _opts()[i]->need;
//        long_options[i].flag = NULL;
//        long_options[i].val = _opts()[i]->abbr;
        /// \warning only pointing to internal char* array of string is easy but unchecked
        long_options[i] = {_opts()[i]->name.data(), _opts()[i]->need, NULL, _opts()[i]->abbr};
//    }
    // last element is blank
    long_options[N] = option();
    return long_options;
}

string Parser::_shortopts()
{
    // create short options descriptor
    string optdesc;
    for (OptionSet::iterator it = _opts().begin(); it != _opts().end(); ++it)
    {
        // help and version need special treatment later
        if ((*it)->abbr!=0) //: 0 is the default value for short_name when there is no short name
        {
            optdesc += (*it)->abbr;
            for (int i = 0; i<(*it)->need; ++i) optdesc += ":";
        }
    }
    return optdesc;
}
