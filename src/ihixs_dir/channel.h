#ifndef CHANNEL_IHIXS_H
#define CHANNEL_IHIXS_H

#include "sigma_term.h"
#include "as_series.h"
#include "higgs_eft.h"


class Channel
{
public:
    int size() const {return _terms.size();}
    SigmaTerm* Term(int i) const {return _terms[i];}
    void AddTerm(SigmaTerm* st){_terms.push_back(st);}
    void Truncate(int i);
    AsSeries Sum();
    ResultPair Result();
    ResultPair CoeffAs(int m){return Sum().term_of_order(m);}
    string Name(){return _name;}
    friend ostream& operator<<(ostream& stream, const Channel& ch);
protected:
    vector<SigmaTerm*> _terms;
    string _name;
};

class HiggsGGFChannelGG: public Channel
{
public:
    HiggsGGFChannelGG(const double& L);
};

class HiggsGGFChannelQG: public Channel
{
public:
    HiggsGGFChannelQG(const double& L);

};

class HiggsGGFChannelQQBAR: public Channel
{
public:
    HiggsGGFChannelQQBAR(const double& L);

};

class HiggsGGFChannelQQ: public Channel
{
public:
    HiggsGGFChannelQQ(const double& L);

};

class HiggsGGFChannelQ1Q2: public Channel
{
public:
    HiggsGGFChannelQ1Q2(const double& L);
};




#endif

