#include "Process.h"

#include "histograms.hpp" //: particular available histograms are defined here. The base class for histograms is at CHistogram.h
#include "cuts.hpp" //: the particular available cuts are defined here. The base class is at CCut.h
//#define debug


#ifndef ONCE_PTR_2_PROCESS
#define ONCE_PTR_2_PROCESS
//ExclusiveClass* EC; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
Process* ptr_to_process;
#endif


#define my_hdecay my_hdecay_

int Integrand(const int *ndim, const double xx[],
              const int *ncomp, double ff[],void * therun, double* weight, int* iteration_number)
{
    
	ptr_to_process->Vegas.vegas_weight = *weight;
	ptr_to_process->Vegas.vegas_iteration_number = *iteration_number;
	ptr_to_process->Evaluate_integral(xx);
	for(int p = 0; p <= ptr_to_process->Vegas.number_of_components; ++p)
    {
		ff[p]=ptr_to_process->Vegas.ff_vegas[p];
		//cout<<"\nff["<<p<<"]="<<ff[p];
    }
    return(0);
}



//---------------------------------------------------------------------------------------------

Process::Process(const UserInterface & UI)
: 
Vegas(UI)
{
    //: DEFAULT SETTINGS
    Vegas.number_of_components=1;
    Vegas.set_ptr_to_the_hatch(&the_hatch);
    
    perturbative_order = UI.perturbative_order;
    
    input_filename = UI.input_filename;
    output_filename = UI.output_filename;
        
    //consts.nf = UI.number_of_flavours;
    if (UI.number_of_flavours!=5)
    {
        cout<<"\n\nerror in constructor of ExclusiveClass: during refactoring the nf became a global variable consts::nf and we don;t want to be changing it from 5. If you really feel like doing so, uncomment the next line in the code";exit(1);
    }
    //consts::nf=UI.number_of_flavours;
    //jetalgorithm=false;
    //: setting the start time
    //time(&start_of_program);
    //time(&time_since_prev_iteration);
    //total_intended_time_for_run = 10.0;
    my_UI=UI;
    ptr_to_process=this;
    
    production_is_defined=false;
    decay_is_defined=false;
    
    
}

void Process::set_production(Production * theproduction)
{
    my_production = theproduction;
    my_production->init(my_UI,&the_hatch);
    production_is_defined=true;
}




void Process::perform()
{
    calculate_number_of_components();
    Vegas.call_vegas();
    print_output();
}



void Process::Evaluate_integral(const double xx[])
{
    //cout<<"\n------------------";
    //: when the bookkeeping of histograms is done by us instead of vegas
    //: we need to update averages and errors per bin at all histograms
    //: at the end of every iteration
    //: we also need to reset the vegas_NOP_in_current_iteration
//    if (Vegas.new_iteration_has_started())
//        update_histograms_end_of_iteration(Vegas.NOP_in_previous_iteration);
    
    
    //: copying vegas random variables from xx to TheHatch 
    the_hatch.SetVars(xx);
    
    proceed_to_production_phase();
    
    update_histograms_end_of_vegas_point();
    
    //: calculating the running f^2 for all bins in histograms 
    //: this does what the end-of-point flag does in the NLO les houches accord.
    
}


void Process::proceed_to_production_phase()
{
    if (production_is_defined) 
    { 
        my_production->evaluate_all_components();
        for (int i=0;i<my_production->production_events.size();i++)
        {            
            book_event(my_production->production_events[i]);                    
        }
    }
    else //: there is no production
    {
        cout<<"\n no production is defined but the integration should have been stopped before this point!\n\n";
        exit(1);
    }     
}


void Process::book_event(Event* the_event)
{    
    Vegas.set_up_vegas_ff(the_event->w);
    
}

void Process::calculate_number_of_components()
{
    Vegas.number_of_components=1;//lumi.pdf_size();

    cout<<"\nnumber_of_components = "<<Vegas.number_of_components;
    
    //initializing ff_vegas to have number_of_components components
    for (int i=0;i<Vegas.number_of_components;i++)
    {
        Vegas.ff_vegas.push_back(0.0);
    }
}

void Process::print_output()
{
    cout << Vegas;
}




