#ifndef EVENT_H
#define EVENT_H

#include <vector>
#include <math.h> // for fabs
using namespace std;

#define MAX_NUMBER_OF_PARTICLES 10

class Event
{
public:
    Event(const double& weight){weight_ = weight;}
    virtual ~Event(){}
    double weight(){return weight_;}
    void SetWeight(const double& w){weight_ = w;}
    void SetP(int i,const double& E,const double& px, const double& py, const double& pz);
    double* ParticleMomentum(int i){return p_[i-1];}
private:
    double weight_;
    double p_[MAX_NUMBER_OF_PARTICLES][4];
    };


class CombinedEvent
{
public:
    CombinedEvent(Event* prod,Event* dec)
    :production(prod),decay(dec){};
    Event* production;
    Event* decay;
    double weight() const ;
};

class EventBox{
public:
    EventBox();
    void SetDecayParticleId(int k){decay_particle_id_=k;}
    int DecayParticleId(){return decay_particle_id_;}
    void SetNumberOfParticles(int n);
    int size(){return effective_size_;}
    void SetP(int i,const double& E,const double& px, const double& py, const double& pz);
    //void AddNewEvent();
    void AddNewEvent(const double& weight);
    Event* ptr_to_event(int i);
    void CleanUp(){effective_size_=0;current_event_pointer_=0;}
private:
    vector<Event> events_;
    int number_of_particles_;
    int decay_particle_id_;
    int effective_size_;
    int current_event_pointer_;
};

#endif