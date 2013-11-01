#include "process.h"
#include "gluon_fusion.h"
#include "bottom_fusion.h"

#include "histograms.hpp" //: particular available histograms are defined here. The base class for histograms is at CHistogram.h
#include "cuts.hpp" //: the particular available cuts are defined here. The base class is at CCut.h

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
//    available_histograms.push_back
//            (new Hist_rap("b1",20,0,-5.0,5.0,"b1_rapidity"));
//    available_histograms.push_back
//            (new Hist_rap("b2",20,0,-5.0,5.0,"b2_rapidity"));
//    available_histograms.push_back(new Hist_PT("b1",70,0,0.0,70.0,"b1_pT"));
//    available_histograms.push_back(new Hist_PT("b2",70,0,0.0,70.0,"b2_pT"));
//    available_histograms.push_back(new Hist_PT("h",20,0,0.0,100.0,"higgs_pT"));
//    available_histograms.push_back
//            (new Hist_rap("h",20,0,-5.0,5.0,"higgs_rapidity"));
//    available_histograms.push_back(new Hist_PT("pf3",20,0,0.0,100.0,"p3_pT"));
//    available_histograms.push_back(new Xhistogram(0,20,"vegas x[0]"));
//    available_histograms.push_back(new Xhistogram(1,20,"vegas x[1]"));
//    available_histograms.push_back(new Xhistogram(2,20,"vegas x[2]"));
//    available_histograms.push_back(new Xhistogram(3,20,"vegas x[3]"));
//    available_histograms.push_back(new Xhistogram(4,20,"vegas x[4]"));
//    available_histograms.push_back(new Xhistogram(5,20,"vegas x[5]"));
//    available_histograms.push_back(new Hist_PT("lepton1",20,0,0.0,80.0,"lepton1_pT"));
    if (my_UI.requested_histogram>-1)
        {
        cout<<"\n[HistogramBox]: will push histogram since UI.requested_histogram = "<<my_UI.requested_histogram<<endl;
        histogram_vector.push_back(available_histograms[my_UI.requested_histogram]);
        }
    
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

void HistogramBox::book_histograms( CombinedEvent* the_event, const double& vegas_weight)
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
        s << *histogram_vector[i];
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
    
        cout<<"\n------------------------ new Process"<<endl;
    UI.PrintAllOptions();
        //: DEFAULT SETTINGS
        Vegas.number_of_components=1;
        Vegas.set_ptr_to_the_hatch(&the_hatch);
        Vegas.set_ptr_to_integrand(&Integrand);
        _histograms = new HistogramBox(UI);
        //_cuts = new CutBox(UI);
        if (UI.histogram_info) _histograms->show_histogram_info_and_exit();
        if (UI.cut_info) my_production->cuts_->show_cut_info_and_exit();
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
                std::cout<<"\nProcess initiated with production "<<UI.production;
                set_production(new GluonFusion(UI));
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
        if (UI.decay=="4leptons")
            {
            std::cout<<"\nProcess initiated with decay "<<UI.decay;
            set_decay(new Decay_WWZZ(UI));
            }
        else
            {
            cout<<endl<<"Process "<<UI.decay<<" not implemented";
            throw "non-implemented process";
            }
        }

    
}




void Process::set_production(Production * theproduction)
{
    cout<<"\nSetting production"<<endl;
    my_production = theproduction;
    my_production->set_up_the_hatch(&the_hatch);
    Vegas.set_number_of_dimensions(the_hatch.GetVEGASDim());
    production_is_defined=true;
}

void Process::set_decay(Decay * thedecay)
{
    my_decay = thedecay;
    my_decay->set_up_the_hatch(&the_hatch);
    Vegas.set_number_of_dimensions(the_hatch.GetVEGASDim());
    decay_is_defined=true;
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
            if (my_production->this_event_passes_cuts(i))
                {
                Event* production_event=my_production->production_events[i];
//            if (production_event->weight()!=0.0
//                    and _cuts->passes_cuts(production_event)
//                )
//                    {
//                    if (Vegas.vegas_iteration_number>-1)
//                        {
//                        for (int i=0;i<my_production->light_events.size();i++)
//                            {
//                            //LightEvent* production_light_event=my_production->light_events[i];
//                            //production_light_event->w = production_light_event->w*Vegas.vegas_weight;
//                            //print_event(production_light_event);
//                            }
//                        if(my_event_stream.is_open())
//                            {
//                            my_event_stream<<"#****"<<endl;
//                            }
//                        else
//                            {
//                            cout<<"\nfailbit = "<<my_event_stream.fail()<<endl;
//                            cout << "Error opening file "<<my_UI.output_filename.c_str()<<endl;
//                            }
//                        }
                proceed_to_decay_phase(production_event);
                }
            }
        }
    else //: there is no production
        {
        perform_decay_alone();
        }     
}

//void Process::print_event(LightEvent* the_event)
//{
//    if(my_event_stream.is_open())
//        {
//        my_event_stream<<the_event->x1<<" ";
//        my_event_stream<<the_event->z<<" ";
//        my_event_stream<<the_event->lambda<<" ";
//        my_event_stream<<the_event->w<<" ";
//        my_event_stream<<endl;
//        }
//    else
//        {
//        cout<<"\nfailbit = "<<my_event_stream.fail()<<endl;
//        cout << "Error opening file "<<my_UI.output_filename.c_str()<<endl;
//        }
//}

void Process::proceed_to_decay_phase(Event* production_event)
{
    //cout<<"\n[Process] production event "<<production_event->w<<endl;
    if (decay_is_defined) //: decay is defined
        {
        my_decay->do_decay(production_event->DecayParticleFourMomentum());
        for (int j=0;j<my_decay->decay_events.size();j++)
            {
            if (my_decay->this_event_passes_cuts(j))
                {
                Event* decay_event=my_decay->decay_events[j];
            //decay_event->merge(*production_event);
            //cout<<"\n[Process]: merged weight = "<<decay_event->w
            //<<"----------------------"<<endl;
            
            //if (decay_event->weight()!=0.0 and _cuts->passes_cuts(decay_event))
                //{
                // cout<<"\n[Process]: production event "
                //     <<production_event->w
                //     <<" x decay event "
                //<<decay_event->w<<endl;
                book_event(new CombinedEvent(production_event,decay_event));
                }
            }
        }
    else //: there is no decay
        {
        book_event(new CombinedEvent(production_event, new Event(1.0)));
        }
}

void Process::perform_decay_alone()//: no production was defined
{
     if (decay_is_defined) //: decay is defined
          { 
               my_decay->do_decay();
               for (int j=0;j<my_decay->decay_events.size();j++)
                    {
                    
                    if (my_decay->this_event_passes_cuts(j))
                         {
                         Event* decay_event=my_decay->decay_events[j];
                         book_event(new CombinedEvent(new Event(1.0),decay_event));
                         }
                    }
          }
     else //: there is no decay and no production
          {
          cout<<"\n No production or decay is defined. There is nothing to do."<<endl;
          }
}

void Process::book_event(CombinedEvent* the_event)
{    
     _histograms->book_histograms(the_event,Vegas.vegas_weight);
     Vegas.set_up_vegas_ff(the_event->weight());
}

void Process::book_null_event()
{
     Vegas.set_up_vegas_ff(0.0);
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
            my_local_outfile << "#ehixs output" << endl;
            my_local_outfile <<Vegas;
            my_local_outfile << "\ntime needed (in sec) = "
                            <<myclock_.GiveMeasurement();
            my_local_outfile << "\nsecs per point = "
            <<myclock_.GiveMeasurement()/Vegas.total_number_of_points();
          
            my_local_outfile << _histograms->print_histograms_to_string();
            my_local_outfile << "\n#Runcard used: ./"<<my_UI.input_filename;
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




