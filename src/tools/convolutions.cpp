
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