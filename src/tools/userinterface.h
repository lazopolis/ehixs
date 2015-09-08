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

class UserInterface : protected OptionSet
{

public://methods
    UserInterface();
    void ParseInput(int argc, char * const *argv);
    void print_help_message();
    void PrintAllOptions() const;
public://data
    double Etot,m_higgs,muf_over_mhiggs,mur_over_mhiggs,
        mur,muf,astar_m3,astar_m4;
    string sector_name,input_filename,output_filename,
        matrix_element_approximation,leptonic_decay_mode_in_wwzz,
        xml_info,
        qcd_perturbative_order,rr_treatment;
    int perturbative_order,pole,decay_sector,sector_control,
        requested_histogram,requested_cut;
    int alpha_s_power,alpha_ew_power;
    bool info,histogram_info,cut_info,list_processes,help,show_me_list,
    pdf_error, dummy_process,ew_soft,ew_h_plus_j,only_ew_h_j,write_events, convolutions_by_interpolation;

    string my_generic_cut;
    vector<CutOption*> all_cuts;
    vector<HistogramOption*> all_hists;
private://methods
    int ParseFile(const string&, bool);
    void parseCmd(int argc, char* const *argv);
    //: getopt interface
    option* create_getopt_option_array();
    string create_getopt_optdesc();

private://data
    vector<Option*> options;
};

#endif
