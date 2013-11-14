#include "cut.h"










void CutBox::ParseCuts(const UserInterface & my_UI)
{
    //    _available_cuts.push_back(new Pt_cut("b1",62.3,"pt_cut_b1"));
    //    _available_cuts.push_back(new Pt_cut("b2",45.0,"pt_cut_b2"));
    //    _available_cuts.push_back(new Abs_y_cut("b1",2.0,"y_cut_b1"));
    //    _available_cuts.push_back(new Pt_cut("pf3",35.3,"pt_cut_p3"));
    //    _available_cuts.push_back(new Pt_cut("h",2.0,"pt_cut_pH"));
    //
    //    _available_cuts.push_back(new Pt_bin("h",20.0,25.0,"pt_bin_pH"));
    //    _available_cuts.push_back(new Pt_bin("h",-10.0,40.0,"pt_zero_bin_pH"));
    
    //cout<<"\nParseCuts : available cuts size = "<<available_cuts_.size()<<endl;

    for (int i=0;i<my_UI.all_cuts.size();i++)
        {
        string cutname = my_UI.all_cuts[i]->give_name();
        //cout<<"\n checking "<<cutname<<endl;
        for (int j=0;j<available_cuts_.size();j++)
            {
            if (cutname==available_cuts_[j]->give_name())
                {
                available_cuts_[j]->set_value(my_UI.all_cuts[i]->give_values()[0]);
                cuts_.push_back(available_cuts_[j]);
                //cout<<"\t\tfound"<<endl;
                break;
                }
            }
        }
    
    //cout<<"ParseCuts : requested cuts size = "<<cuts_.size()<<endl;

    for (int j=0;j<cuts_.size();j++)
        {
        cout<<"\n new cut added : "<<cuts_[j]->give_name()
        <<" @ "<<cuts_[j]->min<<endl;
        }
}


void CutBox::show_cut_info_and_exit()
{
    cout<<"\n**************************************\nAvailable cuts:";
    for (unsigned i=0;i<available_cuts_.size();i++)
        cout<<"\n"<<available_cuts_[i]->info();
    cout<<"\n**************************************\nRequested cuts:";
    for (unsigned i=0;i<cuts_.size();i++)
        cout<<"\n"<<cuts_[i]->info();
    cout<<"\n**************************************\n";
    exit(0);
}


bool CutBox::passes_cuts(Event* the_event)
{
    if (the_event->weight()!=0.0)
        {
        bool event_passes=true;
        for (int i=0;i<cuts_.size();i++)
            {
        
            if(not((*cuts_[i])(the_event)))
                {
                event_passes = false; // the event is cut
                break;// we don't proceed with the rest of the cuts
                }
            }
        return event_passes;
        }
    else
        return false;
}



