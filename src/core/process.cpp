#include "process.h"
#include "gluon_fusion.h"
#include "gamma_star_gamma_star.h"
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
HistogramBox::HistogramBox(const UserInterface& my_UI)
{
    #include "user_defined_histograms_initialization.h"

    //cout<<"\nParseHistograms : available histograms size = "
    //    <<available_histograms.size()<<endl;
    
    for (int i=0;i<my_UI.all_hists.size();i++)
        {
        string name = my_UI.all_hists[i]->give_name();
        //cout<<"\n checking "<<name<<endl;
        for (int j=0;j<available_histograms.size();j++)
            {
            if (name==available_histograms[j]->give_name())
                {
                available_histograms[j]->set_parameters(my_UI.all_hists[i]->numbins(),my_UI.all_hists[i]->lowend(),my_UI.all_hists[i]->highend());
                histogram_vector.push_back(available_histograms[j]);
                //cout<<"\t\tfound"<<endl;
                break;
                }
            }
        }
    
    //cout<<"nParseHistograms : requested histogram size = "<<histogram_vector.size()<<endl;
    
    for (int j=0;j<histogram_vector.size();j++)
        {
        cout<<"\n new histogram added : "<<histogram_vector[j]->give_name()
        <<" ["<<histogram_vector[j]->_lowend<<","
            <<histogram_vector[j]->_highend<<"] with "
            <<histogram_vector[j]->_numbins<<" bins!"<<endl;
        }

//    
//    if (my_UI.requested_histogram>-1)
//        {
//        cout<<"\n[HistogramBox]: will push histogram since UI.requested_histogram = "<<my_UI.requested_histogram<<endl;
//        histogram_vector.push_back(available_histograms[my_UI.requested_histogram]);
//        }
    
    cout<<"\n[HistogramBox] : number of histograms requested = "<<histogram_vector.size();
}

void HistogramBox::show_histogram_info_and_exit()
{
    cout<<"\n======================================\nAvailable histograms:";
    for (unsigned i=0;i<available_histograms.size();i++)
        cout<<"\n"<<available_histograms[i]->info();
    cout<<"\n======================================\nRequested histograms:";
    for (unsigned i=0;i<histogram_vector.size();i++)
        cout<<"\n"<<histogram_vector[i]->info();
    cout<<"\n======================================\n";
    exit(0);
}

void HistogramBox::book_histograms( const CombinedEvent& the_event, const double& vegas_weight)
{
    
    for (int i=0; i<histogram_vector.size(); i++)
        {
        histogram_vector[i]->bin_event(the_event,vegas_weight);
        }
    
}

void HistogramBox::update_histograms_end_of_iteration(int NOP)
{
    for (int i=0;i<histogram_vector.size();i++)
        {
        histogram_vector[i]->end(NOP);
        }
    
    
}
void HistogramBox::update_histograms_end_of_vegas_point()
{
    for (int i=0;i<histogram_vector.size();i++)
        {
        histogram_vector[i]->update();
        }
    
}

void HistogramBox::print_histograms()
{
    for(unsigned i=0; i<histogram_vector.size(); ++i)
        cout << *histogram_vector[i];
}

string HistogramBox::print_histograms_to_string()
{
    stringstream s;
    for(unsigned i=0; i<histogram_vector.size(); ++i)
        s << histogram_vector[i]->xml();
    return s.str();
}

void HistogramBox::write_to_histogram_file()
{
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
//-----------------------------------------------------------------------------



Process::Process(const UserInterface & UI)
: 
Vegas(UI)
{
    decay_particle_id_=-1;
    cout<<"\n------------------------ new Process"<<endl;
    UI.PrintAllOptions();
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
    if( UI.production == "" || UI.production == "1" )
        {
        std::cout << "No process specified" << endl;
        }
    else
        {
        if (UI.production=="ggF")
            {
            std::cout<<"\nProcess initiated with production "
            <<UI.production<<endl;
            set_production(new GluonFusion(UI));
            }
        else if (UI.production=="GammaStarGammaStar")
            {
            std::cout<<"\nProcess initiated with production "<<UI.production;
            set_production(new GammaStarGammaStar(UI));
            }
        else
            {
            cout<<endl<<"Process "<<UI.production<<" not implemented";
            throw "non-implemented process";
            }
        }
    // setting decay here!!
    if( UI.decay == "" || UI.decay == "1" )
        {
        std::cout << "No decay specified" << endl;
        }
    else
        {
        std::cout<<"\nProcess initiated with decay "<<UI.decay;
        if (UI.decay=="4leptons") set_decay(new Decay_WWZZ(UI));
        else if(UI.decay=="gamma_gamma") set_decay(new Decay_gammagamma(UI));
        else if(UI.decay=="Z_gamma") set_decay(new Decay_H_to_Z_Gamma(UI));
        else
            {
            cout<<endl<<"Process "<<UI.decay<<" not implemented";
            throw "non-implemented process";
            }
        }
    if (UI.cut_info)
        {
        my_production->cuts_->show_cut_info_and_exit();
        my_decay->cuts_->show_cut_info_and_exit();
        }
    
}




void Process::set_production(Production * theproduction)
{
    cout<<"\nSetting production"<<endl;
    my_production = theproduction;
    my_production->set_up_the_hatch(&the_hatch);
    Vegas.set_number_of_dimensions(the_hatch.GetVEGASDim());
    decay_particle_id_ = my_production->event_box.DecayParticleId();
    production_is_defined=true;
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
          cout<<"\n[Process] : Sectors are not properly defined. I exit!"<<endl;
          exit(1);
          }
     calculate_number_of_components();     
     open_event_filename();
     if (!my_event_stream.is_open()) {cout<<"\n couldn't open event file"<<endl;exit(0);}
     
     Vegas.call_vegas();
     _histograms->update_histograms_end_of_iteration(Vegas.vegas_NOP_in_current_iteration);
        //: this is after the final iteration
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
         {
         _histograms->update_histograms_end_of_iteration(Vegas.NOP_in_previous_iteration);
         print_output_intermediate();
         my_event_stream<<Vegas.NOP_in_previous_iteration<<"$"<<endl;
         }
     
    // cout<<"\n[Process::EvaluateIntegral] : setting the vegas vars to the_hatch"
    //    <<endl;
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
    //cout<<"\n[Process]: book event "<<endl;
     _histograms->book_histograms(the_event,Vegas.vegas_weight);
    //cout<<"\n[Process]: after histograms are booked"<<endl;
     Vegas.set_up_vegas_ff(the_event.weight());
//    cout<<"\n[Process]: bined event with weight "<<the_event.weight()<<endl;

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

void Process::close_event_filename()
{
     my_event_stream.close();

}

void Process::print_output_intermediate()
{
    _histograms->print_histograms();
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

void Process::calculate_number_of_components()
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

void Process::print_output()
{
    cout << Vegas;
    _histograms->print_histograms();
    

    if (not(my_UI.output_filename.empty()))
        {
        cout << "\n writing output at " << my_UI.output_filename << endl;
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

            cout << "\noutput written in "<< my_UI.output_filename<< endl;
            }
        else
            {
            cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
            cout << "Error opening file "<<my_UI.output_filename.c_str()<<endl;
            }
        my_local_outfile.close();
        }
     
    _histograms->write_to_histogram_file();
     
     
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




