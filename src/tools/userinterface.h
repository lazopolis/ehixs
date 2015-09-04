#ifndef USER_INTERFACE_H
#define USER_INTERFACE_H

#include <string>
#include <vector>
#include <iostream>
#include <stdlib.h> //: for exit()
#include <sstream> //: for stringstream
#include <getopt.h>
#include "option.h"

using namespace std;

class UserInterface
{

public://methods
    UserInterface();
    void ParseInput(int argc, char * const *argv);
    void print_help_message();
    void RunSanityChecks();
    void PrintAllOptions() const;
public://data
    double Etot,m_higgs,epsrel,epsabs,muf_over_mhiggs,mur_over_mhiggs,number_of_flavours,epsrel_therm,epsabs_therm,mur,muf,
    astar_m3,astar_m4;
    string production,decay,pdf_provider,sector_name,sector_for_production,input_filename,output_filename,matrix_element_approximation,Fleft,Fright, leptonic_decay_mode_in_wwzz,xml_info,
    qcd_perturbative_order,rr_treatment,pdf_set;
    int verbose,maxeval,mineval,nstart,nincrease,perturbative_order,pole,decay_sector,sector_control,requested_histogram,requested_cut;
    int alpha_s_power,alpha_ew_power,maxeval_therm,mineval_therm,nstart_therm,nincrease_therm;
    bool info,histogram_info,cut_info,list_processes,help,show_me_list,
    pdf_error, dummy_process,ew_soft,ew_h_plus_j,only_ew_h_j,bin_by_bin_integration,no_grid_adaptation,write_events, convolutions_by_interpolation;

    string my_generic_cut;
    vector<CutOption*> all_cuts;
    vector<HistogramOption*> all_hists;
private://methods
    int ParseFile(const string &, bool);
    vector<vector<string> > ParseCmd(int argc,  char* const *argv,
                                     bool verbose);
    //: getopt interface
    option * create_getopt_option_array();
    string create_getopt_optdesc();
    //: checks
    void CheckIf(bool condition, const string& error_message);

private://data
    vector<Option*> options;
};

#endif