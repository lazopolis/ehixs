#include "process.h"
//#include "gluon_fusion.h"
//#include "gamma_star_gamma_star.h"
#include "bottom_fusion.h"

//#include "histograms.hpp" //: particular available histograms are defined here. The base class for histograms is at CHistogram.h
//#include "cuts.hpp" //: the particular available cuts are defined here. The base class is at CCut.h

#include <stdlib.h> //: for exit()


#ifndef ONCE_PTR_2_PROCESS
#define ONCE_PTR_2_PROCESS

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
		}
     return(0);
}
//-----------------------------------------------------------------------------



Process::Process(const UserInterface & UI)
: 
Vegas(UI)
{
    decay_particle_id_=-1;
    //cout<<"\n[ehixs] new Process"<<endl;
    //UI.PrintAllOptions();
    //: DEFAULT SETTINGS
    Vegas.number_of_components=1;
    Vegas.set_ptr_to_the_hatch(&the_hatch);
    Vegas.set_ptr_to_integrand(&Integrand);
    _histograms = new HistogramBox(UI);
    //_cuts = new CutBox(UI);
    
    if (UI.histogram_info) _histograms->show_histogram_info_and_exit();
        
    if (UI.number_of_flavours!=5)
        {
        cout<<"\n\nerror in constructor of ExclusiveClass: during refactoring the nf became a global variable consts::nf and we don't want to be changing it from 5. If you really feel like doing so, uncomment the next line in the code";exit(1);
        }
    my_UI=UI;
    ptr_to_process=this;
    production_is_defined=false;
    decay_is_defined=false;
    //: setting production
    // process forking
    choose_production(UI);
    choose_decay(UI);
    
    if (UI.cut_info)
        {
        my_production->show_cut_info_and_exit();
        my_decay->cuts_->show_cut_info_and_exit();
        }
    
    final_iteration_ = false;
    bin_by_bin_integration_ = UI.bin_by_bin_integration;
    no_grid_adaptation_ = UI.no_grid_adaptation;
    current_bin_ =0;
    
    if (UI.write_events) events_writing_=true;
    else events_writing_ = false;
    
}

void Process::choose_production(const UserInterface & UI)
{
//    std::cout<<"\n[ehixs] Process initiated with production "
//            <<UI.production<<endl;
    if (UI.production=="ggF")
    {cout<<"\nError: ggF temporarily unavailable"<<endl;exit(0);}
        //my_production = new GluonFusion;
    else if (UI.production=="GammaStarGammaStar")
        //my_production = new GammaStarGammaStar;
    {cout<<"\nError: GammaStarGammaStar temporarily unavailable"<<endl;exit(0);}
    else if (UI.production=="bbH")
        my_production = new BottomFusion;
    else
    {
        cout<<endl<<"[ehixs] Process "<<UI.production<<" not implemented";
        throw "non-implemented process";
    }
    my_production->Configure(UI);
    my_production->set_up_the_hatch(&the_hatch);
    Vegas.set_number_of_dimensions(the_hatch.GetVEGASDim());
    decay_particle_id_ = my_production->event_box.DecayParticleId();
    production_is_defined=true;
}

void Process::choose_decay(const UserInterface & UI)
{
    if( UI.decay == "" || UI.decay == "1" )
    {
        std::cout << "[ehixs] No decay specified" << endl;
    }
    else
    {
        std::cout<<"[ehixs] Process initiated with decay "<<UI.decay;
        if (UI.decay=="4leptons") set_decay(new Decay_WWZZ(UI));
        else if(UI.decay=="gamma_gamma") set_decay(new Decay_gammagamma(UI));
        else if(UI.decay=="Z_gamma") set_decay(new Decay_H_to_Z_Gamma(UI));
        else
        {
            cout<<endl<<"[ehixs] Process "<<UI.decay<<" not implemented";
            throw "non-implemented process";
        }
    }

}


void Process::set_decay(Decay * thedecay)
{
    my_decay = thedecay;
    my_decay->set_up_the_hatch(&the_hatch);
    Vegas.set_number_of_dimensions(the_hatch.GetVEGASDim());
    decay_is_defined=true;
    if (production_is_defined) my_decay->SetModel(my_production->Model);
    
}


void Process::perform()
{
     if (!sectors_are_defined_in_production_and_decay())
          {
          cout<<"\n[ehixs] : Sectors are not properly defined. I exit!"<<endl;
          exit(1);
          }
    Vegas.ConfigureNumberOfComponents(1);
    
    if (events_writing_) open_event_filename();
     
     if (bin_by_bin_integration_) 
         perform_bin_by_bin_mode();
     else if (no_grid_adaptation_) 
         perform_no_adaptation_mode();
     else   
         perform_default_mode();
    
    if (events_writing_) close_event_filename();

}

void Process::perform_bin_by_bin_mode()
{
    cout<<"[ehixs] mode of operation: bin by bin integration"<<endl;
    cout<<"[ehixs] there are "<<_histograms->size()
    <<" histograms"<<endl;
    for (int chist=0;chist<_histograms->size();chist++)
    {
        
        Vegas.flush();
        current_histogram_ = _histograms->ptr_to_histogram_with_id(chist);
        cout<<"[ehixs] evaluating histogram #"<<chist+1
            <<":" << current_histogram_->info()<<endl;
        cout<<"[ehixs] with "<<current_histogram_->size()
        <<" bins " <<endl;
        for (current_bin_=0;current_bin_<current_histogram_->size();current_bin_++)
        {
            cout<<"[ehixs] evaluating histogram #"<<chist+1
            <<" bin # " << current_bin_+1<<" / "<<current_histogram_->size()<<endl;
            Vegas.flush();
            Vegas.call_vegas();
            current_histogram_->set_bin(current_bin_,
                                        Vegas.vegas_integral_output[0],
                                        Vegas.vegas_error_output[0],
                                        Vegas.vegas_prob_output[0],
                                        Vegas.vegas_iteration_number,
                                        Vegas.total_number_of_points());
            _histograms->update_histograms_end_of_iteration(Vegas.vegas_NOP_in_current_iteration);
            print_output();
        }
    }
}

void Process::perform_no_adaptation_mode()
{
    cout<<"[ehixs] mode of operation: no adaptation"<<endl;
    cout<<"[ehixs] main integration"<<endl;
    Vegas.call_vegas();
    
    _histograms->update_histograms_end_of_iteration(Vegas.vegas_NOP_in_current_iteration);
    print_output();
}

void Process::perform_default_mode()
{
    cout<<"\n[ehixs] mode of operation: default"<<endl;
    cout<<"[ehixs] adaptation phase"<<endl;
    Vegas.call_vegas();    
    Vegas.prepare_for_final_iteration();
    final_iteration_ = true;
    cout<<"[ehixs]"<<endl;
    cout<<"[ehixs] main integration"<<endl;
    Vegas.call_vegas();
    _histograms->update_histograms_end_of_iteration(Vegas.vegas_NOP_in_current_iteration);
    print_output();

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



void Process::Evaluate_integral(const double xx[])
{
     //cout<<"\n------------------";
     //: when the bookkeeping of histograms is done by us instead of vegas
     //: we need to update averages and errors per bin at all histograms
     //: at the end of every iteration
     //: we also need to reset the vegas_NOP_in_current_iteration
     if (Vegas.new_iteration_has_started() and Vegas.vegas_iteration_number>1)
         {
         //_histograms->print_running_f();
         _histograms->update_histograms_end_of_iteration(Vegas.NOP_in_previous_iteration);
         print_output_intermediate();
         if (events_writing_)
             my_event_stream<<"\n#"<<Vegas.NOP_in_previous_iteration<<endl;
         }
     
     //: copying vegas random variables from xx to TheHatch 
     the_hatch.SetVars(xx);
    //cout<<"\n[Process::EvaluateIntegral] : proceeding to production phase"
    //<<endl;
     proceed_to_production_phase();
     
     _histograms->update_histograms_end_of_vegas_point();
    //cout<<"\n End of EvaluateIntegral "<<endl;
     //: calculating the running f^2 for all bins in histograms
     //: this does what the end-of-point flag does in the NLO les houches accord.
     
}


void Process::proceed_to_production_phase()
{
     if (production_is_defined) 
        {
        my_production->evaluate_sector();
        int number_of_prod_events = my_production->event_box.size();
        if (number_of_prod_events==0) book_null_event();
        else
            {
            for (int i=0;i<number_of_prod_events;i++)
                {
                if (my_production->this_event_passes_cuts(i))
                    {
                    Event* production_event=my_production->event_box.ptr_to_event(i);
                //cout<<"\nIn Process, event weight = "<<production_event->weight();
                    proceed_to_decay_phase(production_event);
                    }
                else book_null_event();
                    
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
    //cout<<"\n[Process] production event "<<production_event->w<<endl;
    if (decay_is_defined) //: decay is defined
        {
        //cout<<"\n$$$ decay_particle id = "<<decay_particle_id_<<endl;
        my_decay->do_decay(production_event->ParticleMomentum(decay_particle_id_));
        int number_of_decay_events = my_decay->event_box.size();
        if (number_of_decay_events==0) book_null_event();
        else
            {
            for (int j=0;j<number_of_decay_events;j++)
                {
                if (my_decay->this_event_passes_cuts(j))
                    {
                    CombinedEvent the_event(production_event,my_decay->event_box.ptr_to_event(j));
                    book_event(the_event);
                    }
                else book_null_event();
                }
            }
        }
    else //: there is no decay
        {
        //cout<<"\n no decay phase "<<endl;
        CombinedEvent the_event(production_event, NULL);
        book_event(the_event);
        }
}

void Process::perform_decay_alone()//: no production was defined
{
     if (decay_is_defined) //: decay is defined
          { 
               my_decay->do_decay();
               for (int j=0;j<my_decay->event_box.size();j++)
                    {
                    if (my_decay->this_event_passes_cuts(j))
                         {
                         Event* decay_event=my_decay->event_box.ptr_to_event(j);

                         CombinedEvent the_event( NULL, decay_event);
                         book_event(the_event);
                         }
                    }
          }
     else //: there is no decay and no production
          {
          cout<<"\n No production or decay is defined. There is nothing to do."<<endl;
          }
}

void Process::book_event(const CombinedEvent& the_event)
{
    if (events_writing_) write_event(the_event);

    if (bin_by_bin_integration_)
    {
        if (current_histogram_->the_event_is_in_ith_bin(current_bin_,the_event))
            {
            Vegas.set_up_vegas_ff(the_event.weight());    
            }
        else
            {
                book_null_event();
            }
    }
    else if (no_grid_adaptation_)
    {
      
            _histograms->book_histograms(the_event,Vegas.vegas_weight);
            my_event_stream<<the_event;
        
        Vegas.set_up_vegas_ff(the_event.weight());
    }
    else
    {
        if (final_iteration_)
        {
            _histograms->book_histograms(the_event,Vegas.vegas_weight);
            my_event_stream<<the_event;
        }
        Vegas.set_up_vegas_ff(the_event.weight());
    }
}

void Process::book_null_event()
{
    Vegas.set_up_vegas_ff(0.0);
//    cout<<"\t null event booked";
}

void Process::open_event_filename()
{
     string event_filename="events.dat";
     const char * output_fname = event_filename.c_str();
     my_event_stream.open(output_fname, fstream::out);
}

void Process::write_event(const CombinedEvent& the_event)
{
    my_event_stream<<the_event;
}


void Process::close_event_filename()
{
     my_event_stream.close();

}

void Process::print_output_intermediate()
{
//    if (final_iteration_)
//    {
//    _histograms->print_histograms();
//    }
     if (my_UI.output_filename.empty()) return;
     
     
     
     const char * output_fname = my_UI.output_filename.c_str();
     fstream my_local_outfile(output_fname, fstream::out);
     
     if(my_local_outfile.is_open())
          {
          my_local_outfile << "\n-------------------------------------------";
          my_local_outfile.precision(5);
          my_local_outfile << "\n ehixs last iteration (" <<Vegas.vegas_iteration_number-1<<")"<< endl;
          
          my_local_outfile << _histograms->print_histograms_to_string();
          my_local_outfile << "\n\nRuncard used: ";
          my_local_outfile << my_UI.input_filename<<"\n\n";
          
          }
     else
          {
          cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
          cout << "Error opening file "<<my_UI.output_filename.c_str()<<endl;
          }
     my_local_outfile.close();
}

void Process::print_output()
{
    cout << Vegas;
    _histograms->print_histograms();
    

    if (not(my_UI.output_filename.empty()))
        {
        cout << "[ehixs] writing xml output at " << my_UI.output_filename << endl;
        const char * output_fname = my_UI.output_filename.c_str();
        fstream my_local_outfile(output_fname, fstream::out);
        if(my_local_outfile.is_open())
            {
            my_local_outfile.precision(5);
            my_local_outfile << "<ehixs_data " << endl;
            my_local_outfile <<Vegas.xml();
            my_local_outfile << "\ntime=\""
                            <<myclock_.GiveMeasurement()<<"\"";
            my_local_outfile << "\n secs_per_point=\""
            <<myclock_.GiveMeasurement()/Vegas.total_number_of_points()
            <<"\"";
            my_local_outfile << "\n runcard_name=\""<<my_UI.input_filename
            <<"\" >"<<endl;
            my_local_outfile << _histograms->print_histograms_to_string();
            
            my_local_outfile << "</ehixs_data>" << endl;

            //cout << "\noutput written in "<< my_UI.output_filename<< endl;
            }
        else
            {
            cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
            cout << "Error opening file "<<my_UI.output_filename.c_str()<<endl;
            }
        my_local_outfile.close();
        }
     
    //_histograms->write_to_histogram_file();
     
     
}




