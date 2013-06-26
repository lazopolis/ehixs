
#include "Convolutions.h"
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


//void Channel::build_convolutions()
//{
//     //: LO : partitions of 0 in 3
//     build_convolutions_special(0,0,0,parton_left,parton_right);
//     //: NLO : partitions of 1 in 3
//     build_convolutions_special(0,0,1,parton_left,parton_right);
//     build_convolutions_special(0,1,0,parton_left,parton_right);
//     build_convolutions_special(1,0,0,parton_left,parton_right);
//     //: partitions of 2 in 3
//     build_convolutions_special(0,0,2,parton_left,parton_right);
//     build_convolutions_special(1,0,1,parton_left,parton_right);
//     build_convolutions_special(0,1,1,parton_left,parton_right);
//     build_convolutions_special(1,1,0,parton_left,parton_right);
//     build_convolutions_special(2,0,0,parton_left,parton_right);
//     build_convolutions_special(0,2,0,parton_left,parton_right);
//     
//}
//void Channel::build_convolutions_special(int f1order,int f2order,int Sorder,const string&pleft,const string &pright)
//{
//     //cout<<"\n convolution "<<f1order<<" "<<f2order<<" "<<Sorder<<" : "<<pleft<<" "<<pright;
//     vector<FFF> possible_f1,possible_f2;
////     vector<string> available_partons;
////
////     available_partons.push_back("gluon");
////     available_partons.push_back("quark");
////     available_partons.push_back("quark2");
////     available_partons.push_back("antiquark");
//     for (unsigned i=0;i<available_partons.size();i++)
//          {
//          possible_f1.push_back(FFF(available_partons[i],pleft,f1order));
//          if (f1order==2)
//               {
//               possible_f1.push_back(FFF(available_partons[i],pleft,f1order,1));//: adding the 2_1 pdfs
//               }
//          possible_f2.push_back(FFF(available_partons[i],pright,f2order));
//          if (f2order==2)
//               {
//               possible_f2.push_back(FFF(available_partons[i],pright,f2order,1));//: adding the 2_1 pdfs
//               }
//          }
//     
//     for (int i=0;i<possible_f1.size();i++)
//          {
//          //cout<<"\nchecking left";
//          if (possible_f1[i].is_valid())
//               {
//               for (int j=0;j<possible_f2.size();j++)
//                    {
//                   // cout<<"\nchecking right";
//                    if (possible_f2[j].is_valid())
//                         {
//                         int pole_for_partonic_part = pole -( possible_f1[i].epsilon_order + possible_f2[i].epsilon_order);
//                         //cout<<"\n valid combi: "<<possible_f1[i].give_parton()<<" "<<possible_f2[j].give_parton();
//                         partonicXs_nameholder possible_sigma(possible_f1[i].give_parton(),possible_f2[j].give_parton(),Sorder,pole_for_partonic_part);
//                         
//                         check_and_create(possible_f1[i],possible_f2[j],possible_sigma);
//                         }
//                    }
//               }
//          }
//     
//}

//int Channel::number_of_sectors()
//{
//     int counter=0;
//     for (int m=0;m<all_convs.size();m++)
//          {
//          Convolution * CON=all_convs[m];
//          for (int i=0;i<CON->sigma->partonic_modes.size();i++)
//               {
//               counter += CON->sigma->partonic_modes[i]->number_of_sectors();
//               
//               }
//          }
//     return counter;
//}

//void Channel::check_and_create(const FFF& f_i,const FFF& f_j,const partonicXs_nameholder & pos_sig)
//{
//     for (unsigned k1=0;k1<ET1.size();k1++)
//          {
//          for (unsigned k2=0;k2<ET2.size();k2++)
//               {
//               for (unsigned i=0;i<available_xs.size();i++)
//                    {
//                    int tot_alpha_order = ET1[k1]->give_a_power()+ET2[k2]->give_a_power()+available_xs[i]->order;
//                    int tot_e_power = ET1[k1]->give_e_power()+ET2[k2]->give_e_power()+available_xs[i]->lowest_e_power;
//                    if (pos_sig.order==tot_alpha_order and pos_sig.e_power==tot_e_power)
//                         {
//                         string pleft=pos_sig.p_i;
//                         string pright=pos_sig.p_j;
//                         if (pleft=="gluon" and pright=="antiquark") pright="quark";
//                         if (pleft=="antiquark" and pright=="gluon") pleft="quark";
//                         if (pleft=="gluon" and pright=="quark2") pright="quark";
//                         if (pleft=="quark2" and pright=="gluon") pleft="quark";
//                         if (pleft=="antiquark" and pright=="quark") {pleft="quark"; pright="antiquark";}
//                         if (pleft==available_xs[i]->parton_i and pright==available_xs[i]->parton_j)
//                              {
//                              all_convs.push_back(new Convolution(f_i,f_j,available_xs[i]));
//                              }
//                         }
//                    }
//               }
//          }
//
//}




//vector<string> Convolution::all_names()
//{
//     vector<string> myv;
//     for (int i=0;i<sigma->partonic_modes.size();i++)
//          {
//          for (int j=0;j<sigma->partonic_modes[i]->topologies.size();j++)
//               {
//               myv.push_back(topology_name(i,j));
//               }
//          }
//     return myv;
//}



//
//
//pdf_pair_list Convolution::give_list_of_pdf_pairs()
//{
//     pdf_pair_list curlumi;
//     //: mapping glion,quark,antiquark,quark2 to 0,1,-1,2
//     int pid1=give_pid(f1.parton_i);
//     int pid2=give_pid(f1.parton_from);
//     int pid3=give_pid(f2.parton_i);
//     int pid4=give_pid(f2.parton_from);
//     //: case g_from_g g_from_g
//     if (abs(pid1)==0 and abs(pid2)==0 and abs(pid3)==0 and abs(pid4)==0) add_pair(0,0,0,0,curlumi);
//     //: case with no second quark flavor, so single sum over flavors
//     else if (abs(pid1)<2 and abs(pid2)<2 and abs(pid3)<2 and abs(pid4)<2) single_quark(pid1,pid2,pid3,pid4,curlumi);
//     //: case with two different quark flavors
//     else double_quark(pid1, pid2, pid3, pid4, curlumi);
//     
//     return curlumi;
//}



//ostream& operator<<(ostream& stream, const PartonicMode& S)
//{
//     for (int i=0;i<S.topologies.size();i++)
//          {
//          stream <<S.name<<" : "<<i+1;
//          }
//     return stream;
//}
//
//
//ostream& operator<<(ostream& stream, const PartonicXS& S)
//{
//     for (int i=0;i<S.partonic_modes.size();i++)
//          {
//          PartonicMode * curpm=S.partonic_modes[i];
//          
//          for (int j=0;j<curpm->topologies.size();j++)
//               stream <<"S_("<<S.parton_i<<","<<S.parton_j<<")_"<<S.order<<"_"<<curpm->name<<"_t"<<j+1<<endl;
//          }
//     return stream;
//}
//

//
//string Convolution::topology_name(int i_partonic_mode,int j_topology) const
//{
//     stringstream stream;
//     stream <<"\n"<<  f1 <<" x "<<f2<<" x "<<partonic_modes[i];
//     stream <<"S_("<<sigma->parton_i<<","<<sigma->parton_j<<")_"<<sigma->order<<"_"<<sigma->partonic_modes[i_partonic_mode]->name<<"_t"<<j_topology+1<<" : "<<order()<<endl;
//     return stream.str();
//}
//ostream& operator<<(ostream& stream, const Convolution& CON)
//{
//     for (int i=0;i<CON.sigma->partonic_modes.size();i++)
//          {
//          for(int j=0;j<CON.sigma->partonic_modes[i]->topologies.size();j++)
//               {
//               stream<<CON.topology_name(i,j);
//               }
//          }
//     return stream;
//}

//ostream& operator<<(ostream& stream, const Channel& CH)
//{
//     for (int i=0;i<CH.all_convs.size();i++)
//          {
//          stream << *CH.all_convs[i];
//          }
//     return stream;
//}
