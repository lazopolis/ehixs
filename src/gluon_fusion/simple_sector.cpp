
#include "simple_sector.h"

SimpleSector::SimpleSector(const FFF& _f1,const FFF& _f2,const vector<ExpansionTerm*>& _factors,MatrixElement* _ME):F1(_f1),F2(_f2),factors(_factors),ME(_ME)
{
    alpha_power= F1.order+F2.order+ME->alpha_power();
    //: the minus below: FFF has an epsilon order defined positive (otherwise the pdf complain)
    epsilon_power = -F1.epsilon_order-F2.epsilon_order+ME->epsilon_power();
    for (int i=0;i<factors.size();i++)
    {
        alpha_power += factors[i]->give_a_power();
        epsilon_power += factors[i]->give_e_power();
    }
    stringstream stream;
    stream<<F1<<"(*)"<<F2<<"(*)";
    for (int i=0;i<factors.size();i++)
    {
        stream<<factors[i]->give_name()<<"(*)";
    }
    stream<<*ME;
    stream<<" : a_s^"<<alpha_power
    <<",a_w^"<<ME->alpha_ew_power()
    <<",e^"<<epsilon_power;
    name=stream.str();
}



void SimpleSector::add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    curlumi.add_pair(
                     pdf_desc(i,j,F1.order,F1.epsilon_order),
                     pdf_desc(k,m,F2.order,F2.epsilon_order)
                     );
}

void SimpleSector::single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    for (int s=-5;s<6;s++) {if (s!=0) add_pair(i*s,j*s,k*s,m*s,curlumi);}
}

void SimpleSector::double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
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

void SimpleSector::uubar(pdf_pair_list& curlumi)
{
    // lhapdf id numbers for cbar, ubar, u, c
    int upflavors[4] = {-4,-2,2,4};
    
    for (int s=0;s<4;s++)
    {
        int qq = upflavors[s];
        // the pdf combination for u ubar is always LO
        add_pair(qq,qq,-qq,-qq,curlumi);
    }
    
}

void SimpleSector::ddbar(pdf_pair_list& curlumi)
{
    // lhapdf id numbers for bbar,sbar,dbar,d,s,b
    int downflavors[6] = {-5,-3,-1,1,3,5};
    
    for (int s=0;s<6;s++)
    {
        int qq = downflavors[s];
        // the pdf combination for u ubar is always LO
        add_pair(qq,qq,-qq,-qq,curlumi);
    }
    
}

int SimpleSector::give_pid(const string & name)
{
    if (name=="gluon") return 0;
    if (name=="quark") return 1;
    if (name=="antiquark") return -1;
    if (name=="quark2") return 2;
    if (name=="up") return 3;
    if (name=="upbar") return -3;
    if (name=="down") return 4;
    if (name=="downbar") return -4;
    cout<<"\nSimpleSector::give_pid doesn't recognize parton name: "<<name;
    exit(1);
    return 0;
}



pdf_pair_list SimpleSector::give_list_of_pdf_pairs()
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
    else if (abs(pid1)<3 and abs(pid2)<3 and abs(pid3)<3 and abs(pid4)<3)double_quark(pid1, pid2, pid3, pid4, curlumi);
    // case with restricted up and down quark flavors
    else if (F1.parton_i == "up" and F2.parton_i == "upbar")
        uubar(curlumi);
    else if (F1.parton_i == "down" and F2.parton_i == "downbar")
        ddbar(curlumi);
    
    return curlumi;
}

void SimpleSector::setUpPrefactor(const double & a_s_over_pi)
{
    _prefactor =1.0;
    for (unsigned i=0;i<factors.size();i++)
    {
        _prefactor = _prefactor * factors[i]->give_value();
    }
    _prefactor = _prefactor * pow(a_s_over_pi,alpha_power);
}





