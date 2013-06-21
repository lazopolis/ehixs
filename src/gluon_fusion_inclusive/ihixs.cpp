/** \file ehixs.cpp
 *
 * Main/Entry of the program. 
 */



#include <stdlib.h> 
#include <string.h>
#include <fstream>
#include<vector>
#include <iostream>
#include <iomanip>
#include <map>
#include <math.h>
//#include "GluonFusion.h"
//#include "BottomFusion.h"
//#include "g_g_to_b_bbar_H.h"
//#include "q_qbar_to_b_bbar_H.h"
//#include "b_bbar_to_H_X.h"
//#include "b_b_to_H_b_b.h"
//#include "bg_to_H_X.h"
//#include "b_q_to_H_X.h"
//#include "convolutions_total.h"

#include "GluonFusionInclusive.h"
#include "parser.hpp"
#include "UserInterface.h"

using namespace std;


void print_logo();
void read_input(UserInterface& UI,int argc, char** argv);


int main(int argc, char** argv)
{
    
    UserInterface UI;
    //:reading input from command line and input runcards defined there
    read_input(UI,argc,argv);
    
    //: Forking processes
    InclusiveProcess the_process(UI);
    //: choosing production
    InclusiveClass* production;
    if (UI.ProductionMode=="gg") 
    {
        production = new GluonFusionInclusive();
    }
    else
    {
        cout<<"\nUnknown process "<<UI.ProductionMode<<endl;
        exit(1);
    }
    //: setting up the process
    the_process.set_production(production);
    //the_process.set_decay(my_decay);
    //: do everything
    the_process.perform();
    return 0;
}


//: assisting functions

void print_logo()
{
    cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n*                ihixs++                        *";
    cout<<"\n*                                               *";
    cout<<"\n*                version 1.0                    *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
    cout<<"\n\n";
}


void read_input(UserInterface& UI,int argc, char** argv)
{
    
    UI.input_filename = "input.txt";
    UI.output_filename = "";
    UI.sector_control = -1000;
    
    OptionParser Reader;
    
    Reader.declare(&UI.sector_control,"sector",'s',"sector number");
    Reader.declare(&UI.input_filename,"input_filename",'i',"runcard filename");
    Reader.declare(&UI.output_filename,"output_filename",'o',"runcard filename");
    
    // Parse first time to get input output and sector (without errors)
    Reader.parse_cmd(argc,argv,false);
    
    Reader.declare(&UI.ProductionMode,"ProductionMode",0,"determines the physical process");
    Reader.declare(&UI.pole,"pole",'p',"defines pole");
    Reader.declare(&UI.perturbative_order,"perturbative_order",0,"LO, NLO or NNLO");
    Reader.declare(&UI.muf_over_mhiggs,"muf/mhiggs",0,"fraction of mu_f / m_H");
    Reader.declare(&UI.mur_over_mhiggs,"mur/mhiggs",0,"fraction of mu_r / m_H");
    Reader.declare(&UI.nincrease,"nincrease",0,"Vegas argument: # of points to increase the N of each iteration");
    Reader.declare(&UI.nstart,"nstart",0,"Vegas argument: # of points to start first iteration");
    Reader.declare(&UI.maxeval,"maxeval",0,"Vegas argument: maximal # of points even if error target is not achieved");
    Reader.declare(&UI.mineval,"mineval",0,"Vegas argument: minimal # of points even if error target is  achieved");
    Reader.declare(&UI.verbose,"verbose",0,"Vegas argument: level of verbosity");
    Reader.declare(&UI.epsrel,"epsrel",0,"Vegas argument: relative target of accuracy");
    Reader.declare(&UI.epsabs,"epsabs",0,"Vegas argument: absolute target of accuracy");
    Reader.declare(&UI.m_higgs,"m_higgs",0,"The Higgs mass");
    Reader.declare(&UI.pdf_error,"pdf_error",0,"pdf error off (0) or on (1)");
    Reader.declare(&UI.pdf_provider,"pdf_provider",0,"pdf error off (0) or on (1)");
    Reader.declare(&UI.decay,"decay",0,"decay: none");
    Reader.declare(&UI.Etot,"Etot",0,"Total energy of the collider");
    Reader.declare(&UI.info,"info",0,"Information flag");
    // Parse with errors
    if(Reader.parse_file(UI.input_filename)) exit(1);
    if(Reader.parse_cmd(argc,argv)) exit(1);
    
    // Check that sector is set
    check_that_sector_was_set(UI);
    
    // Print parameters
    cout << "\n##############\n" << Reader << "\n##############\n";
    
    
}




