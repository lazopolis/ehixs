
#include "convolutions.h"
#include <iostream>
#include <sstream>
using namespace std;

bool FFF::is_valid()
{
     bool res=true;
     if (order==0 and parton_i!=parton_from) {res=false; }
     if (order==1 and parton_i=="quark" and parton_from=="antiquark"){res=false;}
     if (order==1 and parton_i=="antiquark" and parton_from=="quark"){res=false;}
     if (order>2) {res=false;}
     //if (not(res)) cout<< " failed";
     return res;
}

ostream& operator<<(ostream& stream, const FFF& F)
{
     if (F.order==0) stream <<"F_"<<F.parton_i<<"_from_"<<F.parton_from<<"_"<<F.order<<F.epsilon_order;
     else stream <<"F_"<<F.parton_i<<"_from_"<<F.parton_from<<"_"<<F.order<<F.epsilon_order;
     return stream;
}


string FFF::name()
{
     ostringstream  stream;
     stream<<*this;
     return (stream.str());
}

ExpansionTerm operator*(const ExpansionTerm& E1,const ExpansionTerm& E2)
{
    string newname;
    string name1 = E1.give_name();
    string name2 = E2.give_name();
    if (name1=="1") newname = name2;
    else if (name2=="1") newname = name1;
    else newname = name1+"*"+name2;
    return  ExpansionTerm(newname,
                      E1.give_value()*E2.give_value(),
                      E1.give_a_power()+E2.give_a_power(),
                      E1.give_e_power()+E2.give_e_power());
}

void Polynomial::truncate_in_alpha_up_to_power(int M)
{
    vector<ExpansionTerm> newterms;
    for (int i=0;i<terms.size();i++)
        {
        if (terms[i].give_a_power()<M+1) newterms.push_back(terms[i]);
        }
    terms = newterms;
}
vector<ExpansionTerm> Polynomial::coeff(int a,int e)
{
    vector<ExpansionTerm> res;
    for (int i=0;i<terms.size();i++)
        {
        if (terms[i].give_a_power()==a and terms[i].give_e_power()==e)
            res.push_back(terms[i]);
        }
    return res;
}
void Polynomial::collect()
{
    for (int i=0;i<terms.size();i++)
        {
        for (int j=i+1;j<terms.size();j++)
            {
            ExpansionTerm* t1 = &terms[i];
            ExpansionTerm* t2 = &terms[j];
            if (
                t1->give_a_power()==t2->give_a_power()
                and
                t1->give_e_power()==t2->give_e_power()
                and t1->give_name()!="absent"
                and t2->give_name()!="absent"
                )
                {
                string name1 = t1->give_name();
                string name2 = t2->give_name();
                name1 = name1.substr(name1.find("(")+1,name1.find_last_of(")")-1);
                name2 = name2.substr(name2.find("(")+1,name2.find_last_of(")")-1);

                t1->set_name("("+name1+"+"+name2+")");
                t1->set_value(t1->give_value()+t2->give_value());
                t2->set_name("absent");
                t2->set_value(0.0);
                }
            }
        }
    vector<ExpansionTerm> newterms;
    for (int i=0;i<terms.size();i++)
        {
        if (terms[i].give_name()!="absent") newterms.push_back(terms[i]);
        }
    terms = newterms;
}

Polynomial operator*(const Polynomial& p1,const Polynomial& p2)
{
    Polynomial res;
    for (int i=0;i<p1.size();i++)
        {
        for (int j=0;j<p2.size();j++)
            {
            res.add_term(p1[i]*p2[j]);
            }
        }
    return res;
}

Polynomial operator+(const Polynomial& p1,const Polynomial& p2)
{
    Polynomial res;
    for (int i=0;i<p1.size();i++) res.add_term(p1[i]);
    for (int j=0;j<p2.size();j++) res.add_term(p2[j]);
    return res;
}

Polynomial operator+(const Polynomial& p1,const ExpansionTerm& p2)
{
    Polynomial res = p1;
    res.add_term(p2);
    return res;
}

Polynomial operator+(const ExpansionTerm& p2,const Polynomial& p1)
{return p1+p2;}



ostream& operator<<(ostream& stream, const ExpansionTerm& P)
{
    
    stream << P.give_name()<<" * a^("<<P.give_a_power()<<")"
            <<" * e^("<<P.give_e_power()<<")";
    
    return stream;
}


ostream& operator<<(ostream& stream, const Polynomial& P)
{
    if (P.size()>0)
        {
        for (int i=0;i<P.size()-1;i++)
            {
            stream << P[i]<<" + ";
            }
        int k=P.size()-1;
        stream << P[k];
        }
    
    return stream;
}


/*
Coefficient operator*(const Coefficient& c1,const Coefficient& c2)
{
    return Coefficient(c1.name()+"*"+c2.name(),c1.val()*c2.cal());
}


AEMonomial operator*(const AEMonomial& AE,const Coefficient& c)
{
    AEMonomial res(AE.a_power,AE.e_power);
    res.set_coefficient(c*AE.coefficient());
    return res;
}

AEMonomial operator*(const Coefficient& c,const AEMonomial& AE)
{
    AEMonomial res(AE.a_power,AE.e_power);
    res.set_coefficient(c*AE.coefficient());
    return res;
}
*/