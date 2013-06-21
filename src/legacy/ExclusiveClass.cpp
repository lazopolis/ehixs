#include "ExclusiveClass.h"

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
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
	
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
     my_Ui=UI;
     ptr_to_process=this;
}

void Process::perform()
{
     calculate_dimension_of_integration();
     setup_histograms();
     setup_cuts();
     calculate_number_of_components();
     //define_overall_normalization();
     
     //calculate_wilson_coefficients();
     //compute_ewk_corrections();
     Vegas.call_vegas();
     //put_results_from_vegas_array_to_histograms();
     update_histograms_end_of_iteration(Vegas.vegas_NOP_in_current_iteration);//: this is after the final iteration
     print_output();
}

void Process::calculate_dimension_of_integration()
{
     int production_dim=0;int decay_dim=0;
     if (my_production!=NULL) production_dim=my_production->dimension_of_integration();
     if (my_decay!=NULL) decay_dim = my_decay->dimension_of_integration();
     dimension_of_integration = production_dim+decay_dim;
     
     
}

 


void Process::Evaluate_integral(const double xx[])
{
     //cout<<"\n------------------";
     //: when the bookkeeping of histograms is done by us instead of vegas
     //: we need to update averages and errors per bin at all histograms
     //: at the end of every iteration
     //: we also need to reset the vegas_NOP_in_current_iteration
     if (Vegas.new_iteration_has_started())
          update_histograms_end_of_iteration(Vegas.NOP_in_previous_iteration);
     
     
     //: setting variables at Hub
     Hub::SetVars(xx);  
     
     proceed_to_production_phase();
     
     update_histograms_end_of_vegas_point();
     
     //: calculating the running f^2 for all bins in histograms 
     //: this does what the end-of-point flag does in the NLO les houches accord.
     
}


void Process::proceed_to_production_phase()
{
     if (my_production!=NULL) 
          { 
               my_production->evaluate_sector();
               for (int i=0;i<my_production->production_events.size();i++)
                    {
                    Event* production_event=production_events[i];
                    my_decay->do_decay(production_event.P["h"]);
                    for (int j=0;i<my_decay->decay_events.size();i++)
                         {
                         Event* decay_event=my_decay->decay_events[i];
                         decay_event.merge(production_event);
                         
                         if (decay_event->w!=0.0 and passes_cuts(decay_event))
                              {
                              book_event(decay_event);
                              }
                         }
                    
                    }
          }
     else //: there is no production
     {
          perform_decay_alone();
     }     
}

void Process::proceed_to_decay_phase(Event* production_event)
{
     if (my_decay!=NULL) //: decay is defined
     { 
          my_decay->do_decay(production_event.P["h"]);
          for (int j=0;i<my_decay->decay_events.size();i++)
          {
               Event* decay_event=my_decay->decay_events[i];
               decay_event.merge(production_event);
               if (decay_event->w!=0.0 and passes_cuts(decay_event))
               {
                    book_event(decay_event);
               }
          }
     }
     else //: there is no decay
     {
     book_event(production_event);
     }
}


void Process::perform_decay_alone()//: no production was defined
{
     if (my_decay!=NULL) //: decay is defined
          { 
               my_decay->do_decay();
               for (int j=0;i<my_decay->decay_events.size();i++)
                    {
                         Event* decay_event=my_decay->decay_events[i];
                         if (decay_event->w!=0.0 and passes_cuts(decay_event))
                         {
                              book_event(decay_event);
                         }
                    }
          }
     else //: there is no decay and no production
          {
          cout<<"\n nNo production or decay is defined. There is nothing to do."<<endl;
          }
}




bool Process::passes_cuts(Event* the_event)
{
     bool event_passes=true;
     for (int i=0;i<cuts.size();i++)
          {
          
          if(not((*cuts[i])(the_event->give_ptr_to_momenta()))) 
               // decay_cuts[i].check gives true if the event passes
               {
               event_passes = false; // the event is cut
               break;// we don't proceed with the rest of the cuts
               }
          }
     return event_passes;
}

void Process::book_event(Event* the_event)
{    
     
     //: Higgs propagator treatment
     const double propagator = consts::Pi/Model.higgs.m/Model.higgs.G; //: narrow width approximation
               //: the need to interface the width is evident here
               //:at the moment the width is hard coded for 120 !
     
     the_event->w=the_event->w*propagator;
     book_histograms(the_event);
     Vegas.set_up_vegas_ff(the_event->w);
          
}


void Process::book_null_event()
{
     Vegas.set_up_vegas_ff(0.0);
}



void Process::book_histograms( Event* the_event)
{

     for (int i=0; i<histogram_vector.size(); i++)
          {
          histogram_vector[i]->bin_event(the_event->give_ptr_to_momenta(), the_event->w(),Vegas.vegas_weight);
          }
     for (int i=0; i<histogram2d_vector.size(); i++)
          {
          histogram2d_vector[i]->bin_event(the_event->give_ptr_to_momenta(), the_event->w(),Vegas.vegas_weight);
          }
     for (int i=0; i<average_observable_vector.size(); i++)
          {
          average_observable_vector[i]->bin_event(the_event->give_ptr_to_momenta(), the_event->w(),Vegas.vegas_weight);
          }
}



void Process::setup_cuts()
{
     /*
      //production_cuts.push_back(new pt_3_cut(45.0));
     //production_cuts.push_back(new pt_4_cut(50.0));
     //production_cuts.push_back(new pt_higgs(5.0)); //: production_cuts is a vector of pointers
     //: to CCut objects
     
     
     decay_cuts.push_back(new LeadingPhotonPT(40.0));
     decay_cuts.push_back(new TrailingPhotonPT(25.0));  
     decay_cuts.push_back(new Y_gamma1(2.5));
     decay_cuts.push_back(new Y_gamma2(2.5));
     
     jetalgorithm=true;
     decay_cuts.push_back(new PhotonIsolationCut(15.0,0.4));
     //decay_cuts.push_back(new LeadingPhotonPTBIN(60.0,65.0));
     //decay_cuts.push_back(new LeadingPhotonPTMAX(65.0));
     //decay_cuts.push_back(new AveragePhotonPTBIN(60.0,65.0));
     decay_cuts.push_back(new YSTARBIN(0.5,0.75));
      */
}

void Process::setup_histograms()
{
/* 
     int counter = lumi.pdf_size(); // this is to correctly set the "firstbin" value of a Histogram
     
     if (decay_mode == "none") // higgs only
          {
          //int nbins = 80;
          //histogram_vector.push_back(new H_pt_higgs(nbins,counter,0.0,800.0,"pt_higgs"));
          //counter+=nbins; // plus two from the "restbins"
          // histogram_vector.push_back(new H_zrap_higgs(20,counter,0.0,5.0,"zrap_higgs"));
          // histogram_vector.push_back(new H_pt_higgs(mynbins,counter,0.0,200.0,"higgs_pT"));
          // histogram_vector.push_back(new JetRatesHistogram(5.0,0.4, "JetRates_5"));
          // histogram_vector.push_back(new JetRatesHistogram(10.0,0.4, "JetRates_10"));
          // histogram_vector.push_back(new JetRatesHistogram(15.0,0.4, "JetRates_15"));
          // histogram_vector.push_back(new JetRatesHistogram(20.0,0.4, "JetRates_20_0.4"));
          // histogram_vector.push_back(new JetRatesHistogram(20.0,0.01, "JetRates_20_0.01"));
          // histogram_vector.push_back(new JetRatesHistogram(25.0,0.4, "JetRates_25"));
          // histogram_vector.push_back(new JetRatesHistogram(30.0,0.4, "JetRates_30"));
          // histogram_vector.push_back(new JetRatesHistogram(35.0,0.4, "JetRates_35"));
          // histogram_vector.push_back(new JetRatesHistogram(40.0,0.4, "JetRates_40"));
          // histogram_vector.push_back(new JetRatesHistogram(45.0,0.4, "JetRates_45"));
          // histogram_vector.push_back(new JetRatesHistogram(50.0,0.4, "JetRates_50"));
          // histogram_vector.push_back(new JetRatesHistogram(55.0,0.4, "JetRates_55"));
          // histogram_vector.push_back(new JetRatesHistogram(60.0,0.4, "JetRates_60"));
          // histogram_vector.push_back(new JetRatesHistogram(80.0,0.4, "JetRates_80"));
          
          histogram_vector.push_back(new H_pt_higgs(20,0,0.0,100.0,"higgs_pT"));
          average_observable_vector.push_back(new Total_XS("total_cross_section"));
          //average_observable_vector.push_back(new PT_H_AVG("average_higgs_pT"));
          //average_observable_vector.push_back(new ABS_Y_H_AVG("average_higgs_rapidity"));
          // histogram2d_vector.push_back(new ZYHIST(20,0.0,100.0,20,0.0,5.0,"2D-PT-Y"));
          
          }
     else if(decay_mode=="gamma_gamma")
          {
          histogram_vector.push_back(new AveragePTofPhotons(20,0,0.0,100.0,"AV_pT"));
          histogram_vector.push_back(new PTofLeadingPhoton(20,0,0.0,100.0,"LEAD_pT"));
          histogram_vector.push_back(new PTofTralingPhoton(20,0,0.0,100.0,"TRAIL_pT"));
          histogram_vector.push_back(new Ystar(20,0,0.0,5.0,"Ystar"));
          average_observable_vector.push_back(new Total_XS("total_cross_section"));
          //int nbins = 100;
          //histogram_vector.push_back(new H_Gamma1_pt(nbins,counter,0.0,65.0,"pt_gamma_1","no adapt"));
          //counter+=nbins; // plus two from the "restbins"
          //histogram_vector.push_back(new H_Gamma1_zrap(nbins,counter,-5.0,5.0,"zrap_gamma_1","no adapt"));
          //: introducing neural net 
          
          //histogram_vector.push_back(new ANN(nbins,counter,0.0,1.001,"ANN number 1","no adapt"));
          //counter+=nbins;
          }
     else
          {
          cout << "\nDecay mode " << decay_mode << " in " << __func__ << " not supported yet!\n\n";
          }
 */    
}



void Process::update_histograms_end_of_iteration(int NOP)
{
     for (int i=0;i<histogram_vector.size();i++)
          {
          histogram_vector[i]->end(NOP);
          }
     for (int i=0;i<histogram2d_vector.size();i++)
          {
          histogram2d_vector[i]->end(NOP);
          }
     for (int i=0;i<average_observable_vector.size();i++)
          {
          average_observable_vector[i]->end(NOP);
          }
     //: code for time termination of a job: needs refactoring of vegas_adapter and histograms
     /*
      time_t now;
      time(&now);
      time_needed_for_last_iteration = difftime(now,time_since_prev_iteration);
      double time_since_start = difftime(now,start_of_program);
      cout<<"\n last iteration took "<<time_needed_for_last_iteration<<" secs";
      if (time_since_start+time_needed_for_last_iteration >= total_intended_time_for_run)
      {
      cout<<"\n********************************** time limit reached :"
      <<"\t next iteration would need "<<time_since_start+time_needed_for_last_iteration
      <<"s in total, which would exceed total intended time for run = "<<total_intended_time_for_run
      <<endl;
      
      //: print and terminate!! 
      //update_histograms_end_of_iteration(vegas_NOP_in_current_iteration);//: this is after the final iteration
      print_output();
      exit(1);
      //EXIT_FAILURE;
      }
      */
     print_output_intermediate();
}


void Process::print_output_intermediate()
{
     /*
      for(unsigned i=0; i<histogram_vector.size(); ++i)
      cout << *histogram_vector[i];
      for(unsigned i=0; i<histogram2d_vector.size(); ++i)
      cout << *histogram2d_vector[i];
      for(unsigned i=0; i<average_observable_vector.size(); ++i)
      cout << *average_observable_vector[i];
      */
     
     
     if (output_filename.empty()) return;
     
     
     
     const char * output_fname = output_filename.c_str();
     fstream my_local_outfile(output_fname, fstream::out);
     
     if(my_local_outfile.is_open())
          {
          my_local_outfile << "\n-------------------------------------------";
          my_local_outfile.precision(5);
          my_local_outfile << "\n ehixs last iteration (" <<Vegas.vegas_iteration_number-1<<")"<< endl;
          
          for(unsigned i=0; i<histogram_vector.size(); ++i)
               my_local_outfile << *histogram_vector[i];
          for(unsigned i=0; i<histogram2d_vector.size(); ++i)
               my_local_outfile << *histogram2d_vector[i];
          for(unsigned i=0; i<average_observable_vector.size(); ++i)
               my_local_outfile << *average_observable_vector[i];
          my_local_outfile << "\n\nRuncard used: ";
          my_local_outfile << input_filename<<"\n\n";
          
          }
     else
          {
          cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
          cout << "Error opening file "<<output_filename.c_str()<<endl;
          }
     my_local_outfile.close();
     
     
     
     
}

void Process::update_histograms_end_of_vegas_point()
{
     for (int i=0;i<histogram_vector.size();i++)
          {
          histogram_vector[i]->update();
          }
     for (int i=0;i<histogram2d_vector.size();i++)
          {
          histogram2d_vector[i]->update();
          }
     for (int i=0;i<average_observable_vector.size();i++)
          {
          average_observable_vector[i]->update();
          }
}




void Process::calculate_number_of_components()
{
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
     // the first "Nmember" components are for the pdf error
     Vegas.number_of_components=lumi.pdf_size();
     
     /*adapt functionality removed   
      // adding all the histograms
      for (int i=0;i<histogram_vector.size();i++)
      {
      if(histogram_vector[i]->adapt)
      {
      number_of_components+=histogram_vector[i]->size(); // plus two from the "restbins"
      }
      }
      */
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
     
     for(unsigned i=0; i<histogram_vector.size(); ++i)
          cout << *histogram_vector[i];
     for(unsigned i=0; i<histogram2d_vector.size(); ++i)
          cout << *histogram2d_vector[i];
     for(unsigned i=0; i<average_observable_vector.size(); ++i)
          cout << *average_observable_vector[i];
     //cout<<"\n # of wrong events = "<<counter_of_wrong_events<<endl;
     
     if (output_filename.empty()) return;
     
     cout << "\n writing output at " << output_filename << endl;
     
     const char * output_fname = output_filename.c_str();
     fstream my_local_outfile(output_fname, fstream::out);
     
     if(my_local_outfile.is_open())
          {
          my_local_outfile << "\n-------------------------------------------";
          my_local_outfile.precision(5);
          my_local_outfile << "\n ehixs output" << endl;
          my_local_outfile <<Vegas;
          
          for(unsigned i=0; i<histogram_vector.size(); ++i)
               my_local_outfile << *histogram_vector[i];
          for(unsigned i=0; i<histogram2d_vector.size(); ++i)
               my_local_outfile << *histogram2d_vector[i];
          for(unsigned i=0; i<average_observable_vector.size(); ++i)
               my_local_outfile << *average_observable_vector[i];
          my_local_outfile << "\n\n* * * * * * * * * * * * * * * * * * * * * * * * *\n\nRuncard used\n\n";
          my_local_outfile << input_filename;
          cout << "\n output written" << endl;
          }
     else
          {
          cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
          cout << "Error opening file "<<output_filename.c_str()<<endl;
          }
     my_local_outfile.close();
}




