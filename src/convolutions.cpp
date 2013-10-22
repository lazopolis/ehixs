
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

