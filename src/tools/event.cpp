#include "event.h"
#include <iostream>
using namespace std;



void Event::SetP(int i,const double& E,const double& px, const double& py, const double& pz)
{
    p_[i-1][0]=E;
    p_[i-1][1]=px;
    p_[i-1][2]=py;
    p_[i-1][3]=pz;

}


double CombinedEvent::weight() const
{
    double w1=1.0;
    double w2=1.0;
    if (production!=NULL) w1 = production->weight();
    if (decay!=NULL) w2 = decay->weight();
    
    return w1*w2;
}

EventBox::EventBox()
{
    number_of_particles_=0;
    decay_particle_id_=0;
    effective_size_=0;
    current_event_pointer_=0;
}

Event* EventBox::ptr_to_event(int i)
{
    //cout<<"\n returning pointer, w="<<events_[i].weight();
    return &(events_[i]);
}

//void EventBox::AddNewEvent()
//{
//    
//    if (effective_size_>events_.size())
//        events_.push_back(Event(0.0));
//    else
//        effective_size_++;
//    
//    current_event_pointer_ = int(events_.size())-1;
//}

void EventBox::AddNewEvent(const double& weight)
{
    
    if (effective_size_>=events_.size())
        {
        events_.push_back(Event(weight));
        effective_size_++;
        current_event_pointer_ = effective_size_-1;
        }
    else
        {
        effective_size_++;
        current_event_pointer_ = effective_size_-1;
        events_[current_event_pointer_].SetWeight(weight);
        }
    
    
    //cout<<"\n[EventBox] new event added with weight "<<weight;
    //cout<<"\n and to verify, w="<<events_[current_event_pointer_].weight();
//    cout<<"[EventBox] event added. Current Size = "<<effective_size_
//    <<", current pointer points to event number "<<current_event_pointer_
//    <<" \t real events_.size() = "<<events_.size()
//    <<endl;
}

void EventBox::SetNumberOfParticles(int n)
{
    number_of_particles_ = n;
    if (number_of_particles_ >MAX_NUMBER_OF_PARTICLES)
        {
        cout<<"\n[EventBox]: ERROR: more than 10 particle momenta "
            <<"are not supported. Please change the default Event in event.h"
            <<endl;
            exit(1);
        }
}

void EventBox::SetP(int i,const double& E,const double& px, const double& py, const double& pz)
{
    events_[current_event_pointer_].SetP(i,E,px,py,pz);
//    if (i==5)
//        {
//        cout<<"\n event with pth= "<<sqrt(px*px+py*py);
//        }
}










