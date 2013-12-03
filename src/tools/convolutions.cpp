
#include "convolutions.h"
#include <iostream>
#include <sstream>
using namespace std;



//------------------------------------------------------------------------------




ostream& operator<<(ostream& stream, const NewMatrixElement& ME)
{
    stream<<"S("<<ME.parton_i()<<","<<ME.parton_j()<<","<< ME.name()
    <<",a^"<<ME.alpha_power()<<",e^"<<ME.epsilon_power()
    <<" ,dim="<<ME.dimension()<<")";
    
    return stream;
}


//------------------------------------------------------------------------------


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
//------------------------------------------------------------------------------

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
//------------------------------------------------------------------------------

void Polynomial::truncate_in_alpha_up_to_power(int M)
{
    vector<ExpansionTerm*> newterms;
    for (int i=0;i<terms.size();i++)
        {
        if (terms[i]->give_a_power()<M+1) newterms.push_back(terms[i]);
        }
    terms = newterms;
}
vector<ExpansionTerm*> Polynomial::coeff(int a,int e)
{
    vector<ExpansionTerm*> res;
    for (int i=0;i<terms.size();i++)
        {
        if (terms[i]->give_a_power()==a and terms[i]->give_e_power()==e)
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
            ExpansionTerm* t1 = terms[i];
            ExpansionTerm* t2 = terms[j];
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
    vector<ExpansionTerm*> newterms;
    for (int i=0;i<terms.size();i++)
        {
        if (terms[i]->give_name()!="absent") newterms.push_back(terms[i]);
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
            ExpansionTerm* prod = new ExpansionTerm((*(p1[i])) * (*(p2[j])));
            res.add_term(prod);
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
    ExpansionTerm* pp2 = new ExpansionTerm(p2.give_name(),p2.give_value(),p2.give_a_power(),p2.give_e_power());
    Polynomial res = p1;
    res.add_term(pp2);
    return res;
}

Polynomial operator+(const ExpansionTerm& p2,const Polynomial& p1)
{return p1+p2;}


Polynomial ppow(const Polynomial& p,int k)
{
    Polynomial res = p;
    for (int i=0;i<k-1;i++) res = res * p;
    return res;
    
}

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


//------------------------------------------------------------------------------

// defines a map from integer parton id numbers to keywords like gluon or upbar
// which are ued as placeholders in defining matrix elements
// This has to change, presumably, for each new process
void FSingle::construct_name()
{
if (parton_from_==0) name_from_ = "gluon";
else
    {
    if (parton_from_>0)
        {
        if (parton_from_ % 2 ==1) name_from_ = "down";
        else name_from_ = "up";
        }
    else
        {
        if (-parton_from_ % 2 ==1) name_from_ = "downbar";
        else name_from_ = "upbar";
        }
    }
}

ListOfSingleF::ListOfSingleF(int parton_i)
{
    f_.push_back(new FSingle(parton_i,parton_i,0,0));
    for (int j=-5;j<6;j++)
        {
        if (parton_i==0 or parton_i==j)
            {
            f_.push_back(new FSingle(parton_i,j,1,1));
            }
        f_.push_back(new FSingle(parton_i,j,2,1));
        f_.push_back(new FSingle(parton_i,j,2,2));
        }
        
}

ListOfFF::ListOfFF(const string& fleft,const string& fright)
{
    for (int i=-5;i<6;i++)
        {
        for (int j=-5;j<6;j++)
            {
            if (flavors_match(i,j,fleft,fright))
                {
                ListOfSingleF* Fleft = new ListOfSingleF(i);
                ListOfSingleF* Fright = new ListOfSingleF(j);
                for (int k=0;k<Fleft->size();k++)
                    {
                    for (int m=0;m<Fright->size();m++)
                        {
                        ff_.push_back(new FxF(Fleft->give(i),Fright->give(j)));
                        }
                    }
                }
            }
        }
}

// checks wether keyword(i)==left and keyword(j)==right
//  where keyword(int) is a map from the integer id numbers of partons to
// user defined keywords used in the definition of matrix elements
// note: by construction here gluon,quark matches to (0,1), but not to (0,-1),
// or (1,0) or (-1,0) : initial state crossing and charge conjugation are not
// implied!
bool ListOfFF::flavors_match(int i,int j,
                             const string& left, const string& right)
{
    FSingle Fi(i,i,0,0);
    FSingle Fj(j,j,0,0);
    if (Fi.init_flavor_matches(left) and Fj.init_flavor_matches(right))
        {
        return true;
        }
    else return false;
}


Sector::Sector(NewMatrixElement* me)
{
    // we need to get the log(mur/muf) from somewhere or set it through the
    // coefficients later on!
    double L = 0.0;
    me_ = me;
    ListOfFF FF(me->parton_i(),me->parton_j());
    // a_renorm = a_bare
    Polynomial a_renorm("1",1.0,1,0);
    Polynomial one("1",1.0,0,0);
    // a_renorm = a_bare * exp(-a*e*L) (1 - b0/e*a + (b0^2/e^2-b_1/e) * a)
    a_renorm = a_renorm * ( one + Polynomial("-b0",-consts::beta_zero,1,-1)
                           + Polynomial("b1",consts::beta_one,2,-1)
                           + Polynomial("b0^2",pow(consts::beta_zero,2.0),2,-2))
    * ( one + Polynomial("-L",-L,1,1)
       + Polynomial("L^2/2",L*L/2.0,2,2));
    
    a_renorm = ppow(a_renorm,me->alpha_power());
    a_renorm.truncate_in_alpha_up_to_power(2);
    
    for (int i=0;i<FF.size();i++)
        {
        for (int j=0;j<a_renorm.size();i++)
            {
            for (int k = me->epsilon_power_min();k<me->epsilon_power_max()+1;k++ )
            FFA_.push_back(new FxFxA(FF[i],a_renorm[i],k));
            }
        }
    
}


void Sector::restrict_as(int min_a_power_requested,int max_a_power_requested)
{
    int min_a_rest = min_a_power_requested - me_->alpha_power();
    int max_a_rest = max_a_power_requested - me_->alpha_power();
    for (std::list<FxFxA*>::iterator it1 = FFA_.begin();it1!=FFA_.end();++it1)
        {
        if ((*it1)->alpha_power()<min_a_rest or (*it1)->alpha_power()>max_a_rest)
            {
            it1 = FFA_.erase(it1);
            it1--;
            }
        }
}

void Sector::restrict_epsilon(int pole)
{
    for (std::list<FxFxA*>::iterator it1 = FFA_.begin();it1!=FFA_.end();++it1)
        {
        if ((*it1)->epsilon_power()!=pole)
            {
            it1 = FFA_.erase(it1);
            it1--;
            }
        }
}

void Sector::restrict_flavor(const string& left,const string& right)
{
    for (std::list<FxFxA*>::iterator it1 = FFA_.begin();it1!=FFA_.end();++it1)
        {
        if (not((*it1)->initial_flavor_is(left,right)))
            {
            it1 = FFA_.erase(it1);
            it1--;
            }
        }
}


// this can be moved to production
void Sector::AllocateLuminosity(Luminosity* lumi)
{
    // luminosity allocation to take place here !!!
    
//    pdf_pair_list list_of_pdf_pairs=the_sector->give_list_of_pdf_pairs();
//    
//    for (unsigned i=0;i<list_of_pdf_pairs.size();i++)
//        {
//        //cout<<"\n pair #"<<i+1;
//        pair<pdf_desc,pdf_desc> cur_pair=list_of_pdf_pairs.give_one_pair(i);
//        lumi->add_pair(cur_pair.first,cur_pair.second);
//        }
    me_->SetLuminosity(lumi);
}


//------------------------------------------------------------------------------




SectorBox::SectorBox(const vector<NewMatrixElement*>& mes,
                          const UserInterface& UI)
{
    
    
    // this is where the runcard semantics are resolved
    // qcd_perturbative_order
    int min_a_power_requested,max_a_power_requested;
    // default  option: qcd_perturbative_order
    if (UI.qcd_perturbative_order == "LO")
        {
        min_a_power_requested = 2;
        max_a_power_requested = 2;
        }
    if (UI.qcd_perturbative_order == "NLO")
        {
        min_a_power_requested = 2;
        max_a_power_requested = 3;
        }
    if (UI.qcd_perturbative_order == "NNLO")
        {
        min_a_power_requested = 2;
        max_a_power_requested = 4;
        }
    // advanced  : alpha_s_power. If defined by user it fixes the a_s order
    if (UI.alpha_s_power != -1)
        {
        min_a_power_requested = UI.alpha_s_power;
        max_a_power_requested = UI.alpha_s_power;
        }
    
    
    for (int ime = 0; ime<mes.size();ime++)
        {
        NewMatrixElement* me = mes[ime];
        Sector* newsector = new Sector(me);
        newsector->restrict_as(min_a_power_requested,max_a_power_requested);
        newsector->restrict_epsilon(UI.pole);
        newsector->restrict_flavor(UI.Fleft,UI.Fright);
        available_sectors.push_back(newsector);
        }
}





/*
NewSimpleSector::filter_initial_state_flavor(const string& left,
                                             const string& right)
{
    if ((left != "none") and (right != "none"))
        {
        
        }
}

void NewSimpleSector::Evaluate(double* xx_vegas)
{
    //SetInitialStateVars();
    
    
    ME->Evaluate(xx_vegas);
    
}


void NewSimpleSector::add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    curlumi.add_pair(
                     pdf_desc(i,j,F1.order,F1.epsilon_order),
                     pdf_desc(k,m,F2.order,F2.epsilon_order)
                     );
}

void NewSimpleSector::single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    for (int s=-5;s<6;s++) {if (s!=0) add_pair(i*s,j*s,k*s,m*s,curlumi);}
}

void NewSimpleSector::double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    for (int s=-5;s<6;s++)
        {
        for (int r=-5;r<6;r++)
            {
            if (s!=0 and r!=0 and s!=r and s!=-r)
                {
                int ii,jj,kk,mm;
                if (abs(i)==1){ii=s*i;} else if (i==2){ii=r;} else {ii=0;}
                if (abs(j)==1){jj=s*j;} else if (j==2){jj=r;} else {jj=0;}
                if (abs(k)==1){kk=s*k;} else if (k==2){kk=r;} else {kk=0;}
                if (abs(m)==1){mm=s*m;} else if (m==2){mm=r;} else {mm=0;}
                
                add_pair(ii,jj,kk,mm,curlumi);
                }
            }
        }
}

int NewSimpleSector::give_pid(const string & name)
{
    if (name=="gluon") return 0;
    if (name=="quark") return 1;
    if (name=="antiquark") return -1;
    if (name=="quark2") return 2;
    cout<<"\nSimpleSector::give_pid doesn't recognize parton name: "<<name;
    exit(1);
    return 0;
}

pdf_pair_list NewSimpleSector::give_list_of_pdf_pairs()
{
    pdf_pair_list curlumi;
    //: mapping glion,quark,antiquark,quark2 to 0,1,-1,2
    int pid1=give_pid(F1.parton_i);
    int pid2=give_pid(F1.parton_from);
    int pid3=give_pid(F2.parton_i);
    int pid4=give_pid(F2.parton_from);
    //: case g_from_g g_from_g
    if (abs(pid1)==0 and abs(pid2)==0 and abs(pid3)==0 and abs(pid4)==0) add_pair(0,0,0,0,curlumi);
    //: case with no second quark flavor, so single sum over flavors
    else if (abs(pid1)<2 and abs(pid2)<2 and abs(pid3)<2 and abs(pid4)<2) single_quark(pid1,pid2,pid3,pid4,curlumi);
    //: case with two different quark flavors
    else double_quark(pid1, pid2, pid3, pid4, curlumi);
    
    return curlumi;
}

void NewSimpleSector::SetUpPrefactor(const double& a_s_over_pi)
{
    double prefactor =1.0;
    for (unsigned i=0;i<factors.size();i++)
        {
        prefactor = prefactor * factors[i]->give_value();
        }
    prefactor = prefactor * pow(a_s_over_pi,alpha_power);
    ME->SetUpPrefactor(prefactor);
}
*/
