
#include "convolutions.h"
#include <iostream>
#include <sstream>
using namespace std;





/*





//------------------------------------------------------------------------------


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

// pow including P^0 case
Polynomial ppow(const Polynomial& p,int k)
{
    Polynomial res("1",1.0,0,0);
    for (int i=0;i<k;i++) res = res * p;
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
*/

//------------------------------------------------------------------------------

// defines a map from integer parton id numbers to keywords like gluon or upbar
// which are used as placeholders in defining matrix elements
// This has to change, presumably, for each new process
/*
void FSingle::construct_names()
{
    set_name(name_from_,name_descriptors_from,parton_from_);
    set_name(name_i_,name_descriptors_to,parton_i_);
}

void FSingle::set_name(string& pname,vector<string>& name_descriptor,int pid)
{
    string specific_name[6]={"g","d","u","s","c","b"};
    if (pid==0) 
        {
        pname = "gluon";
        name_descriptor.push_back("gluon");
        name_descriptor.push_back("g");
        }
    else
        {
        if (pid>0)
            {
            name_descriptor.push_back("quark");
            name_descriptor.push_back("q");

            if (pid % 2 ==1) 
                {
                pname = "down";
                name_descriptor.push_back("down type");

                }
            else 
                {
                pname = "up";
                name_descriptor.push_back("up type");

                }
            name_descriptor.push_back(specific_name[pid]);
            }
        else
            {
            name_descriptor.push_back("antiquark");
            name_descriptor.push_back("qbar");
            if (-pid % 2 ==1) 
                {
                pname = "downbar";
                name_descriptor.push_back("downbar type");

                }
            else {
                pname = "upbar";
                name_descriptor.push_back("upbar type");
                }
            name_descriptor.push_back(specific_name[-pid]+"b");

            }
        }
}
*/
void LuminosityBox::AllocateLuminosity(Luminosity* lumi)
{
    for (int i=0;i<luminosities_.size();i++)
    {
    my_lumis_.push_back(lumi->give_single_pair_pt(pdf_desc(luminosities_[i].left),pdf_desc(luminosities_[i].right)));
    }
}

void LuminosityBox::MatchPDFs(const string& left, const string& right, const string& pdf_selector_)
{
    if (left=="u" and right=="ub")
        {
        luminosities_.push_back(pdf_desc_pair(2,-2));
        flavor_dependent_prefactor_.push_back(1.0);
        }
    else if (left=="q" and right=="qbar" and pdf_selector_=="crossed")
        {
            for (int i=-5;i<6;i++)
                {
                    if (i!=0)
                        {
                        luminosities_.push_back
                            (pdf_desc_pair(i,-i));
                        flavor_dependent_prefactor_.push_back(1.0);
                        }
                }
        }
    else if (left=="q" and right=="qbar" and pdf_selector_=="crossed_charged")
    {
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                luminosities_.push_back
                (pdf_desc_pair(i,-i));
                if (abs(i)==1 or abs(i)==3 or abs(i)==5)
                    {
                    flavor_dependent_prefactor_.push_back(pow(-1./3.,4.));
                    }
                if (abs(i)==2 or abs(i)==4)
                {
                    flavor_dependent_prefactor_.push_back(pow(2./3.,4.));
                }
            }
        }
    }
    else if (left=="q" and right=="qbar" and pdf_selector_=="up_type_only")
    {
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                if (abs(i)==2 or abs(i)==4)
                {
                    luminosities_.push_back
                    (pdf_desc_pair(i,-i));
                    flavor_dependent_prefactor_.push_back(pow(2./3.,4.));
                }
            }
        }
    }
    else if (left=="u" and right=="g")
        {
        luminosities_.push_back(pdf_desc_pair(2,0));
        flavor_dependent_prefactor_.push_back(1.0);
        }
    else if (left=="q" and right=="g" and pdf_selector_=="crossed")
        {
            for (int i=-5;i<6;i++)
            {
                if (i!=0)
                    {
                    luminosities_.push_back
                    (pdf_desc_pair(i,0));
                    flavor_dependent_prefactor_.push_back(1.0);
                    }
            }
        }
    else if (left=="q" and right=="g" and pdf_selector_=="crossed_charged")
    {
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                luminosities_.push_back
                (pdf_desc_pair(i,0));
                if (abs(i)==1 or abs(i)==3 or abs(i)==5)
                {
                    flavor_dependent_prefactor_.push_back(pow(-1./3.,4.));
                }
                if (abs(i)==2 or abs(i)==4)
                {
                    flavor_dependent_prefactor_.push_back(pow(2./3.,4.));
                }
            }
        }
    }
    else if (left=="q" and right=="g" and pdf_selector_=="up_type_only")
    {
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                if (abs(i)==2 or abs(i)==4)
                {
                    luminosities_.push_back
                    (pdf_desc_pair(i,0));
                    flavor_dependent_prefactor_.push_back(pow(2./3.,4.));
                }
            }
        }
    }
    else if (left=="g" and right=="ubar")
        {
        luminosities_.push_back(pdf_desc_pair(0,-2));
        flavor_dependent_prefactor_.push_back(1.0);
        }
    else
        {
        cout<<"\n undefined pdfs : "<<left<<","<<right<<endl;
        exit(1);
        }
    
}

double LuminosityBox::give(const double& x1,const double& x2)
{
    double res=0.0;
    for (int i=0;i<my_lumis_.size();i++)
    {
        res = res + my_lumis_[i]->give(x1,x2)*flavor_dependent_prefactor_[i];
    }
    return res;
}



// legacy for gluon fusion below


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


/*

ostream& operator<<(ostream& stream, const NewMatrixElement& ME)
{
    stream<<ME.name()<<"("<<ME.parton_i()<<","<<ME.parton_j()
    <<"): a^"<<ME.alpha_power()
    <<" ,dim="<<ME.dimension();
    
    return stream;
}



 ostream& operator<<(ostream& stream, const FxF& P)
{
    stream<<P.fleft_->complete_name()<<"."<<P.fright_->complete_name();
    return stream;
}


 ostream& operator<<(ostream& stream, const FxFxA& P)
{
    stream<<*(P.ff_)<<"."<<*(P.term_);
    return stream;
}

ostream& operator<<(ostream& stream, const Sector& P)
{
    stream<<"{";
    int N=P.FFA_.size();
    for (int i=0;i<N-1;i++) stream<<" "<<*(P.FFA_[i])<<" + ";
    stream<<" "<<*(P.FFA_[N-1])<<" } * "<<*(P.me_)<<",e^"<<P.e_pow_of_matrix_element_;

    return stream;
}


//------------------------------------------------------------------------------

// this can be moved to production
void Sector::AllocateLuminosity(Luminosity* lumi)
{
    // luminosity allocation to take place here !!!
    for (int i=0;i<FFA_.size();i++)
        {
      
        FFA_[i]->AllocateLuminosity(lumi);
        }

    me_->SetFFA(FFA_);
}

void Sector::SetUpPrefactor(const double & a_s_over_pi)
{
    for (int i=0;i<FFA_.size();i++)
        {
        FFA_[i]->SetUpPrefactor(a_s_over_pi,me_->alpha_power());
        }
}

void Sector::PassAlphaStrong(const double& alpha_s_over_pi)
{
    me_->SetAlphaStrong(alpha_s_over_pi);
}

void Sector::Evaluate(double* xx_vegas)
{
    me_->Evaluate(xx_vegas);
}





//------------------------------------------------------------------------------

void SectorBox::determine_as_power_requested(const UserInterface& UI)
{
    // default  option: qcd_perturbative_order
    if (UI.qcd_perturbative_order == "LO") as_control_.Set(0,0);
    if (UI.qcd_perturbative_order == "NLO") as_control_.Set(0,1);
    if (UI.qcd_perturbative_order == "NNLO") as_control_.Set(0,2);
    // advanced option : alpha_s_power. 
    //                  If defined by user it fixes 
    //                  the a_s order overwriting qcd_perturbative_order
    if (UI.alpha_s_power != -1) 
        as_control_.Set(UI.alpha_s_power,UI.alpha_s_power);
}


SectorBox::SectorBox()
{
//    for (int avail_matrix_el=0;avail_matrix_el<mes.size();avail_matrix_el++)
//    {
//        vector<FxF*> FF_pairs_that_fit_=DeterminePdfs(me);
//        for (int i=0;i<FF_pairs_that_fit_.size();i++)
//            {
//            available_sectors.push_back(new Sector(FFA,me,me_e_pow));    
//            }
//    }

// -- useful only above

//    
//    set_up_pdfs();
//    //set_up_polynomial();
//    // this is where the runcard semantics are resolved
//    //determine_as_power_requested(UI);
//    
//    for (int ime = 0; ime<mes.size();ime++)
//        {
//        NewMatrixElement* me = mes[ime];
////        if (as_control_.IsAboveMaximum(me->alpha_power() ) )
////            {
////            continue;
////            }
//        vector<FxF*> FF_pairs_that_fit_=DeterminePdfs(me,
//                                                      UI.Fleft,UI.Fright);
////        for (int me_e_pow=me->epsilon_power_min();
////                me_e_pow<me->epsilon_power_max()+1;me_e_pow++)
////            {
//            //cout<<"\n[SectorBox] e^"<<me_e_pow;
//            vector<FxFxA*> FFA;
//            for (int i=0;i<FF_pairs_that_fit_.size();i++)
//                {
//                //: remove this if you want to automatically generate convolutions
////                if (not(FF_pairs_that_fit_[i]->alpha_power()==0)) 
////                    continue;
////                int alpha_tot = FF_pairs_that_fit_[i]->alpha_power()
////                    + me->alpha_power();
////                int etot = FF_pairs_that_fit_[i]->epsilon_power()
////                        +me_e_pow;
////                bool pole_ok = etot == UI.pole;
////                if (as_control_.IsWithinRequestedRange(alpha_tot) and pole_ok)
////                    {
//                    FxFxA* newffa = new FxFxA(FF_pairs_that_fit_[i],
//                    new ExpansionTerm("1",1.0,0,0));
//                    FFA.push_back(newffa);
////                    }
////                else
////                    {
//////                    cout<<"\n[SectorBox] a_s control = "<<alpha_tot<<" which is outside the range ";
////                    }
//                }
//            if (FFA.size()>0) available_sectors.push_back(new Sector(FFA,me,me_e_pow));
//            else
//                {
////                cout<<"\n[SectorBox] no matching luminosity combination";
//                }
//            //}
//        }
}

void SectorBox::set_up_pdfs()
{
    for (int i=-5;i<6;i++)
        {
        all_pdfs.push_back(new FSingle(i,i,0,0));
        // 11 pdfs
        //gluon case F11[x->gluon] for all x
//        if (i==0)
//            {
//            for (int j=-5;j<6;j++)
//                {
//                all_pdfs.push_back(new FSingle(0,j,1,1));
//                }
//            }
//        //quark case F11[g->q] and F11[q->q] only
//        else
//            {
//            all_pdfs.push_back(new FSingle(i,0,1,1));
//            all_pdfs.push_back(new FSingle(i,i,1,1));
//            }
//        for (int j=-5;j<6;j++)
//            {
//            all_pdfs.push_back(new FSingle(i,j,2,1));
//            all_pdfs.push_back(new FSingle(i,j,2,2));
//            }
        }
    //cout<<"\n[SectorBox] total number of pdfs : "<<all_pdfs.size()<<endl;
}

void SectorBox::set_up_polynomial()
{
    double L=0.0;
    // a_renorm = a_bare
    a_renorm = Polynomial("1",1.0,1,0);
    Polynomial one("1",1.0,0,0);
    // a_renorm = a_bare * exp(-a*e*L) (1 - b0/e*a + (b0^2/e^2-b_1/e) * a)
    a_renorm = a_renorm * ( one + Polynomial("-b0",-consts::beta_zero,1,-1)
                           + Polynomial("b1",consts::beta_one,2,-1)
                           + Polynomial("b0^2",pow(consts::beta_zero,2.0),2,-2))
    * ( one + Polynomial("-L",-L,1,1)
       + Polynomial("L^2/2",L*L/2.0,2,2));
    
    //a_renorm = ppow(a_renorm,me->alpha_power());
    //a_renorm.truncate_in_alpha_up_to_power(2);
    cout<<"\n[SectorBox] a_renorm constructed"<<endl;
    
}

vector<FxF*>  SectorBox::DeterminePdfs(NewMatrixElement* me,
                                       const string& Fleft,const string& Fright)
{
    vector<FxF*> FF_pairs_that_fit_;
    for (int i=0;i<all_pdfs.size();i++)
        {
        if (all_pdfs[i]->flavor_matches(me->parton_i())
            and all_pdfs[i]->init_flavor_matches(Fleft))
            {
            for (int j=0;j<all_pdfs.size();j++)
                {
                if (all_pdfs[j]->flavor_matches(me->parton_j())
                    and all_pdfs[j]->init_flavor_matches(Fright))
                    {
                        int as_total = all_pdfs[i]->alpha_power()
                        +all_pdfs[j]->alpha_power();
                    if (
                        pdfs_match(all_pdfs[i],all_pdfs[j],me->pdf_selection())
                        and not(as_control_.IsAboveMaximum(as_total)))
                        {
                        FF_pairs_that_fit_.push_back(new FxF(all_pdfs[i],all_pdfs[j]));
                        }
                    }
                
                }
            }
        }
   // cout<<"\n[SectorBox] pdf pairs that match: "<<FF_pairs_that_fit_.size()<<endl;

    return FF_pairs_that_fit_;
}


    
bool SectorBox::pdfs_match(FSingle* f1, FSingle* f2,const string& selection)
{
    if (selection=="none") return true;
    if (selection=="same flavor")
        {
        if (abs(f1->parton_i()) == abs(f2->parton_i()))
            {
            return true;
            }
        else
            return false;
        }
    if (selection=="different flavor")
        {
        if (abs(f1->parton_i()) == abs(f2->parton_i()))
            {
            return false;
            }
        else
            return true;
        }
    return false;
}
*/


