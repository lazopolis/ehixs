#include "Process.h"
#include "GluonFusion.h"
#include "BottomFusion.h"

#include "histograms.hpp" //: particular available histograms are defined here. The base class for histograms is at CHistogram.h
#include "cuts.hpp" //: the particular available cuts are defined here. The base class is at CCut.h
//#define debug









#ifndef ONCE_PTR_2_PROCESS
#define ONCE_PTR_2_PROCESS
//ExclusiveClass* EC; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
Process* ptr_to_process;
InclusiveProcess* ptr_to_inclusive_process;

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

int InclusiveIntegrand(const int *ndim, const double xx[],
              const int *ncomp, double ff[],void * therun, double* weight, int* iteration_number)
{
     
	ptr_to_inclusive_process->Vegas.vegas_weight = *weight;
	ptr_to_inclusive_process->Vegas.vegas_iteration_number = *iteration_number;
	ptr_to_inclusive_process->Evaluate_integral(xx);
	for(int p = 0; p <= ptr_to_inclusive_process->Vegas.number_of_components; ++p)
		{
		ff[p]=ptr_to_inclusive_process->Vegas.ff_vegas[p];
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
     Vegas.set_ptr_to_integrand(&Integrand);
     
     perturbative_order = UI.perturbative_order;
     
     input_filename = UI.input_filename;
     output_filename = UI.output_filename;
     
     if (UI.histogram_info)
     {
          setup_histograms();
          cout<<"\n======================================\nAvailable histograms:";
          for (unsigned i=0;i<available_histograms.size();i++) 
               cout<<"\n"<<available_histograms[i]->info();
          cout<<"\n======================================\nRequested histograms:";
          for (unsigned i=0;i<histogram_vector.size();i++) 
               cout<<"\n"<<histogram_vector[i]->info();
          cout<<"\n======================================\n";
     exit(0);
     }
     if (UI.cut_info)
          {
          setup_cuts();
          cout<<"\n**************************************\nAvailable cuts:";
          for (unsigned i=0;i<available_cuts.size();i++) 
               cout<<"\n"<<available_cuts[i]->info();
          cout<<"\n**************************************\nRequested cuts:";
          for (unsigned i=0;i<cuts.size();i++) 
               cout<<"\n"<<cuts[i]->info();
          cout<<"\n**************************************\n";
          exit(0);
          }
     
     //consts.nf = UI.number_of_flavours;
     if (UI.number_of_flavours!=5)
          {
          cout<<"\n\nerror in constructor of ExclusiveClass: during refactoring the nf became a global variable consts::nf and we don't want to be changing it from 5. If you really feel like doing so, uncomment the next line in the code";exit(1);
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
     
     
     //: setting production
     // process forking
     if( UI.production == "" || UI.production == "1" )
          {
          std::cout << "No process specified" << endl;
          }
     else
          {
          if (UI.production=="ggF")
               {
               set_production(new GluonFusion());
               }
          else
               {
               cout<<endl<<"Process "<<UI.production<<" not implemented";
               throw "non-implemented process";
               }
          }
     // setting decay here!!
     
}

void Process::set_production(Production * theproduction)
{
     my_production = theproduction;
     my_production->init(my_UI,&the_hatch);
     production_is_defined=true;
}
void Process::set_decay(Decay * thedecay)
{
     my_decay = thedecay;
     my_decay->init(my_UI,&the_hatch);
     
     if (production_is_defined)
     {
          my_decay->set_alpha_s(my_production->Model.alpha_strong);
          my_decay->set_y_b(my_production->y_b_vec());
     }
     else 
     {
          //:dirty hack here
          //: we need some value for a_s and y_b
          //: in case there is no production at all
          //: To get one, we init a lumi
     
     Luminosity loclumi(consts::nf,my_UI.muf_over_mhiggs * my_UI.m_higgs,my_UI.mur_over_mhiggs * my_UI.m_higgs,my_UI.perturbative_order,my_UI.pdf_provider,my_UI.pdf_error);
          //: we add a pair of pdfs (which is getting evolved for nothing)
     
          loclumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_00);
          vector<double> alpha_s;
          vector<double> yukawa_b_vector;


          //: we evolve a_s and yukawa
          loclumi.evolve_alpha_s_from_mz_to_mur(alpha_s); // necessarily before evolve_mb_from_mb_ref_to_mur
          for (int i=0;i<alpha_s.size();i++){cout<<"\nalpha_s["<<i<<"]="<<alpha_s[i];}
          cout<<"\n*** mb is not evolved here - needs fixing at Process.cpp, set_decay(Decay* the_decay)";
     //loclumi.evolve_mb_from_mb_ref_to_mur(my_decay->Model.bottom.m(),yukawa_b_vector);
          //: and we set the couplings to decay
          my_decay->set_alpha_s(alpha_s);
          my_decay->set_y_b(yukawa_b_vector);
          cout<<"\ny_b="<<yukawa_b_vector[0]<<"\t"<<my_decay->Model.bottom.m();
     }
     
     decay_is_defined=true;
}


void Process::perform()
{
    // calculate_dimension_of_integration();
     if (!sectors_are_defined_in_production_and_decay())
          {
          cout<<"\n Sectors are not properly defined. I exit!"<<endl;
          exit(1);
          }
     setup_histograms();
     setup_cuts();
     calculate_number_of_components();
     //define_overall_normalization();
     
     
     open_event_filename();
     if (!my_event_stream.is_open()) {cout<<"\n couldn't open event file"<<endl;exit(0);}
     
     //calculate_wilson_coefficients();
     //compute_ewk_corrections();
     Vegas.call_vegas();
     //put_results_from_vegas_array_to_histograms();
     update_histograms_end_of_iteration(Vegas.vegas_NOP_in_current_iteration);//: this is after the final iteration
     print_output();
     close_event_filename();
     
}



bool Process::sectors_are_defined_in_production_and_decay()
{
     bool res_prod=false;
     bool res_dec=false;
     if (production_is_defined)
          {
          res_prod=true;
          res_prod=my_production->is_sector_defined();
          }
     if (decay_is_defined)
          {
          res_dec=true;
          res_dec=my_decay->is_sector_defined();
          }
     if ((res_prod) or (res_dec)) return true;
     else return false;
}
/*
void Process::calculate_dimension_of_integration()
{
     int production_dim=0;int decay_dim=0;
     if (production_is_defined) production_dim=my_production->dimension_of_integration();
     if (decay_is_defined) decay_dim = my_decay->dimension_of_integration();
     dimension_of_integration = production_dim+decay_dim;
     

     
}
*/
vector<string> Process::give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & eord,const string & _me_approx)
{
     if (production_is_defined)
          {
          return my_production->give_sector_names(pleft,pright,myorder,eord,_me_approx);
          }
     else
          {
          cout<<"\n production is not defined, and you asked for sector names!! I exit! "<<endl;
          exit(1);
          }
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
               my_production->evaluate_sector();
               for (int i=0;i<my_production->production_events.size();i++)
                    {
                    Event* production_event=my_production->production_events[i];
                    if (production_event->w!=0.0 and passes_cuts(production_event))
                         {
                         proceed_to_decay_phase(production_event);
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
     if (decay_is_defined) //: decay is defined
          { 
               my_decay->do_decay(production_event->p["h"]);
               for (int j=0;j<my_decay->decay_events.size();j++)
                    {
                    Event* decay_event=my_decay->decay_events[j];
                    decay_event->merge(*production_event);
                    
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
     if (decay_is_defined) //: decay is defined
          { 
               my_decay->do_decay();
               for (int j=0;j<my_decay->decay_events.size();j++)
                    {
                    Event* decay_event=my_decay->decay_events[j];
                    if (decay_event->w!=0.0 and passes_cuts(decay_event))
                         {
                         book_event(decay_event);
                         }
                    }
          }
     else //: there is no decay and no production
          {
          cout<<"\n No production or decay is defined. There is nothing to do."<<endl;
          }
}




bool Process::passes_cuts(Event* the_event)
{
     bool event_passes=true;
     for (int i=0;i<cuts.size();i++)
          {
          
          if(not((*cuts[i])(the_event))) 
               // decay_cuts[i].check gives true if the event passes
               {
               event_passes = false; // the event is cut
               break;// we don't proceed with the rest of the cuts
               }
          }
     //if (event_passes and the_event->p["pf3"].pT()<235.3) {cout<<"\nerror : event passes with pT for pf3 = "<<the_event->p["pf3"].pT();}
     //if (event_passes) {cout<<"\n passes with pf3.pt="<<the_event->p["pf3"].pT();}
     //else {cout<<"\n fails with pf3.pt="<<the_event->p["pf3"].pT();}
     return event_passes;
}

void Process::book_event(Event* the_event)
{    
     book_histograms(the_event);
     //print_event(the_event);
     //the_cluster.process(the_event);
     Vegas.set_up_vegas_ff(the_event->w);
}


void Process::book_null_event()
{
     Vegas.set_up_vegas_ff(0.0);
}



void Process::book_histograms( Event* the_event)
{
     
     //the_event->xx_vegas = vegas_variables_for_histograming_use;
     for (int i=0; i<histogram_vector.size(); i++)
          {
          histogram_vector[i]->bin_event(the_event,Vegas.vegas_weight);
          }
//     for (int i=0; i<histogram2d_vector.size(); i++)
//          {
//          histogram2d_vector[i]->bin_event(the_event,Vegas.vegas_weight);
//          }
//     for (int i=0; i<average_observable_vector.size(); i++)
//          {
//          average_observable_vector[i]->bin_event(the_event,Vegas.vegas_weight);
//          }
}

void Process::open_event_filename()
{
     string event_filename="events.dat";
     const char * output_fname = event_filename.c_str();
     my_event_stream.open(output_fname, fstream::out);
}


void Process::print_event(Event* the_event)
{
     if(my_event_stream.is_open())
          {
          my_event_stream<<the_event->w<<" ";
          for (int i=0;i<the_event->p.P.size();i++)
               {
               for (int j=0;j<4;j++)
                    {
                    my_event_stream<<the_event->p.P[i][j]<<" ";
                    }
               }
          my_event_stream<<endl;
          }
     else
          {
          cout<<"\nfailbit = "<<my_event_stream.fail()<<endl;
          cout << "Error opening file "<<output_filename.c_str()<<endl;
          }
     
     
     
     
}

void Process::close_event_filename()
{
     my_event_stream.close();

}

void Process::setup_cuts()
{
     available_cuts.push_back(new Pt_cut("b1",62.3,"pt_cut_b1"));
     available_cuts.push_back(new Pt_cut("b2",45.0,"pt_cut_b2"));
     available_cuts.push_back(new Abs_y_cut("b1",2.0,"y_cut_b1"));
     available_cuts.push_back(new Pt_cut("pf3",35.3,"pt_cut_p3"));
     available_cuts.push_back(new Pt_cut("h",2.0,"pt_cut_pH"));
     
     available_cuts.push_back(new Pt_bin("h",20.0,25.0,"pt_bin_pH"));
     available_cuts.push_back(new Pt_bin("h",-10.0,5.0,"pt_zero_bin_pH"));

     for (unsigned requested_cut_k=0;requested_cut_k<my_UI.requested_cuts.size();requested_cut_k++)
          {
          bool requested_cut_available=false;
          for (unsigned available_cut_k=0;available_cut_k<available_cuts.size();available_cut_k++)
               {
               if (available_cuts[available_cut_k]->give_name()==my_UI.requested_cuts[requested_cut_k])
                    {
                    requested_cut_available=true;
                    cuts.push_back(available_cuts[available_cut_k]);
                    }
               }
          if (not(requested_cut_available))
               {
               cout<<"\n The requested cut "<<my_UI.requested_cuts[requested_cut_k]<<" is not available. We'll proceed without it. Use UI.cut_info = true; to see which cuts are available"<<endl;
               }
          }


}



void Process::setup_histograms()
{
     available_histograms.push_back(new Hist_rap("b1",20,0,-5.0,5.0,"b1_rapidity"));
     available_histograms.push_back(new Hist_rap("b2",20,0,-5.0,5.0,"b2_rapidity"));
     available_histograms.push_back(new Hist_PT("b1",70,0,0.0,70.0,"b1_pT"));
     available_histograms.push_back(new Hist_PT("b2",70,0,0.0,70.0,"b2_pT"));
     
     available_histograms.push_back(new Hist_PT("h",20,0,0.0,100.0,"higgs_pT"));
     available_histograms.push_back(new Hist_rap("h",20,0,-5.0,5.0,"higgs_rapidity"));
     
     available_histograms.push_back(new Hist_PT("pf3",20,0,0.0,100.0,"p3_pT"));
     
     available_histograms.push_back(new Xhistogram(0,20,"vegas x[0]"));
     available_histograms.push_back(new Xhistogram(1,20,"vegas x[1]"));
     available_histograms.push_back(new Xhistogram(2,20,"vegas x[2]"));
     available_histograms.push_back(new Xhistogram(3,20,"vegas x[3]"));
     available_histograms.push_back(new Xhistogram(4,20,"vegas x[4]"));
     available_histograms.push_back(new Xhistogram(5,20,"vegas x[5]"));
     
     
     for (unsigned requested_histo_k=0;requested_histo_k<my_UI.requested_histograms.size();requested_histo_k++)
     {
          bool requested_histo_available=false;
          for (unsigned available_histo_k=0;available_histo_k<available_histograms.size();available_histo_k++)
          {
               if (available_histograms[available_histo_k]->give_name()==my_UI.requested_histograms[requested_histo_k])
               {
                    requested_histo_available=true;
                    histogram_vector.push_back(available_histograms[available_histo_k]);
               }
          }
          if (not(requested_histo_available))
               {
               cout<<"\n The requested histogram "<<my_UI.requested_histograms[requested_histo_k]<<" is not available. We'll proceed without it. Use UI.histogram_info = true; to see which histograms are available"<<endl;
               }
     }

   
}



void Process::update_histograms_end_of_iteration(int NOP)
{
     for (int i=0;i<histogram_vector.size();i++)
          {
          histogram_vector[i]->end(NOP);
          }
     print_output_intermediate();
}


void Process::print_output_intermediate()
{
     
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

}




void Process::calculate_number_of_components()
{
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
     // the first "Nmember" components are for the pdf error
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
     for(unsigned i=0; i<histogram_vector.size(); ++i)
          cout << *histogram_vector[i];
     

     if (not(output_filename.empty()))
     {
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
     
     
     for (int i=0;i<histogram_vector.size();i++)
          {
          stringstream hfname;
          hfname << "hist"<<i<<".dat";
          const char * output_fname = hfname.str().c_str();
          fstream local_file(output_fname, fstream::out);
          if (local_file.is_open())local_file<<histogram_vector[i]->plotinfo();
          else cout<<"\n cannot write to histogram file"<<endl;
          local_file.close();
          }
     
}


string Process::sector_info()
{
     string res="production: ";
     if (production_is_defined)
          {
          res += my_production->sector_name();
          }
     else
          {
          res += "none";
          }
     res += " | decay: ";
     if (decay_is_defined)
          {
          res += my_decay->sector_name();
          }
     else
          {
          res += "none";
          }
     return res;
}





InclusiveProcess::InclusiveProcess(const UserInterface& UI)
:
Vegas(UI)
{
     //: DEFAULT SETTINGS
     Vegas.number_of_components=1;
     Vegas.set_ptr_to_the_hatch(&the_hatch);
     Vegas.set_ptr_to_integrand(&InclusiveIntegrand);
     
//     perturbative_order = UI.perturbative_order;
     
     input_filename = UI.input_filename;
     output_filename = UI.output_filename;
     
     my_UI=UI;
     ptr_to_inclusive_process=this;
     
//     production_is_defined=false;
//     decay_is_defined=false;
}


void InclusiveProcess::set_production(InclusiveProduction * theproduction)
{
     my_production = theproduction;
     my_production->init(my_UI,&the_hatch);
     production_is_defined=true;
}


void InclusiveProcess::perform()
{
     if (!sectors_are_defined_in_production())
          {
          cout<<"\n Sectors are not properly defined. I exit!"<<endl;
          exit(1);
          }
     calculate_number_of_components();
     Vegas.call_vegas();
     print_output();
}

bool InclusiveProcess::sectors_are_defined_in_production()
{
     return true;
}


void InclusiveProcess::calculate_number_of_components()
{

     // the first "Nmember" components are for the pdf error
     Vegas.number_of_components=1;//lumi.pdf_size();
     
     cout<<"\nnumber_of_components = "<<Vegas.number_of_components;
     
     //initializing ff_vegas to have number_of_components components
     for (int i=0;i<Vegas.number_of_components;i++)
          {
          Vegas.ff_vegas.push_back(0.0);
          }
}


void InclusiveProcess::print_output()
{
     cout << Vegas;
     if (not(output_filename.empty()))
          {
          cout << "\n writing output at " << output_filename << endl;
          
          const char * output_fname = output_filename.c_str();
          fstream my_local_outfile(output_fname, fstream::out);
          
          if(my_local_outfile.is_open())
               {
               my_local_outfile << "\n-------------------------------------------";
               my_local_outfile.precision(5);
               my_local_outfile << "\n ehixs output" << endl;
               my_local_outfile <<Vegas;
               
              
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
     
          
}

void InclusiveProcess::Evaluate_integral(const double xx[])
{
          
     //: copying vegas random variables from xx to TheHatch
     the_hatch.SetVars(xx);
     
     proceed_to_production_phase();

     
}



void InclusiveProcess::proceed_to_production_phase()
{
     if (production_is_defined)
          {
               my_production->evaluate_sector();
               book_event(my_production->weight);
          }
}



void InclusiveProcess::book_event(const double & w)
{
     Vegas.set_up_vegas_ff(w);
     
}




