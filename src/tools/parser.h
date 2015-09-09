/**
 *
 * \file    parser.h
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2015
 *
 */

#ifndef PARSER_H
#define PARSER_H

#include "option.h"
#include <string>
#include <getopt.h>

using namespace std;

class Parser : protected OptionSet
{

public:

    /// \name Data members
    /// @{

    string inputfile, outputfile;
//    double Etot,m_higgs,muf_over_mhiggs,mur_over_mhiggs,
//        mur,muf,astar_m3,astar_m4;
//    string sector_name,
//        matrix_element_approximation,leptonic_decay_mode_in_wwzz,
//        xml_info,
//        qcd_perturbative_order,rr_treatment;
//    int perturbative_order,pole,decay_sector,sector_control,
//        requested_histogram,requested_cut;
//    int alpha_s_power,alpha_ew_power;
//    bool info,histogram_info,cut_info,list_processes,show_me_list,
//        pdf_error, dummy_process,ew_soft,ew_h_plus_j,only_ew_h_j,write_events, convolutions_by_interpolation;

    /// @}

private:

    /// \name Data members
    /// @{

    bool _help;

    /// @}

public:

    Parser();
    void print_help_message();
    void print() const;
    void parse(int argc, char* const *argv);

private:

    int  parseFile();
    void parseCmd (int argc, char* const *argv);

    option* _longopts();
    string _shortopts();

};

#endif
