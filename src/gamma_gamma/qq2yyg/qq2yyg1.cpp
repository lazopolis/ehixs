/**
 *
 * \file    qq2yyg1.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "constants.h" // QCD::CF, etc...
#include "boxmaster.h" // box, bubble

#include "qq2yyg1.h"
#include "mastersums.inl"

#define CXM(a,b) [](const PSpoint& p){return std::make_pair(a,b);}
#define PTC(a)   [](const PSpoint& p){return a;}

/**
 * \par   patchDelta
 * \brief Relative distance of two Mandelstams for the Taylor-expansion patch to kick in
 * \todo  Find a way to move this to a header file
 */

const double patchDelta = 0.001;

// qq2yyg1

template<typename T>
double qq2yyg1<T>::eval(const PSpoint& p, const int i, const bool t)
{
    return LC::eval(p,i,t)+SC::eval(p,i,t)+Nf::eval(p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::eval(const PSpoint& p, const bool t)
{
    return LC::eval(p,t)+SC::eval(p,t)+Nf::eval(p);
}

// LC

template<typename T>
const double& qq2yyg1<T>::LC::factor()
{
    return QCD::CA;
}

template<typename T>
double qq2yyg1<T>::LC::eval(const PSpoint& p, const int i, const bool t)
{
    return bub::eval(p,i,t)+box::eval(p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::LC::eval(const PSpoint& p, const bool t)
{
    return bub::eval(p,t)+box::eval(p);
}

// LC bub

template<typename T>
array<bool,7> qq2yyg1<T>::LC::bub::_on = array<bool,7>();

template<typename T>
array<bool,6> qq2yyg1<T>::LC::bub::_patch = array<bool,6>();

template<typename T>
double qq2yyg1<T>::LC::bub::master(const size_t i, const PSpoint& p, const int j)
{
    if (i>masters().size()) throw;
    return xTerm(masters()[i-1](p),j);
}

template<typename T>
EpsExp qq2yyg1<T>::LC::bub::master(const size_t i, const PSpoint& p)
{
    if (i>masters().size()) throw;
    return x2Finite(masters()[i-1](p));
}

template<typename T>
double qq2yyg1<T>::LC::bub::eval(const PSpoint& p, const int i, const bool taylor)
{
    patch(p, taylor);
    return factor()*(
                     selAccTerm(masters().begin(),masters().end(),_on.begin(),p,i)+
                     selAccPatchTerm(patches().begin(),patches().end(),_patch.begin(),p,i)
                     );
}

template<typename T>
EpsExp qq2yyg1<T>::LC::bub::eval(const PSpoint& p, const bool taylor)
{
    patch(p,taylor);
    return factor()*(
                     selAcc2Finite(masters().begin(),masters().end(),_on.begin(),p)+
                     selAccPatch2Finite(patches().begin(),patches().end(),_patch.begin(),p)
                     );
}

template<typename T>
array<typename qq2yyg1<T>::Master,7>& qq2yyg1<T>::LC::bub::masters()
{
    static array<Master,7>* _masters = new array<Master,7>({
        CXM(c<1>(p.zb,p.t12,p.t34,p.u),  bubble(p.s13,2)),
        CXM(c<1>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s14,2)),
        CXM(c<1>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s23,2)),
        CXM(c<1>(p.zb,-p.t12,-p.t34,p.u),bubble(p.s24,2)),
        CXM(c<2>(p.zb,p.t12,p.t34,p.u),  bubble(p.s15,3)),
        CXM(c<2>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s25,3)),
        CXM(c<3>(p.zb,p.t12,p.t34,p.u),  bubble(p.s34,2))
    });
    return *_masters;
}

template<typename T>
array<typename qq2yyg1<T>::Patch,6>& qq2yyg1<T>::LC::bub::patches()
{
    static array<Patch,6>* _patches = new array<Patch,6>({
        PTC(c1325(p.zb,p.t12,p.t34)),
        PTC(c1325(p.zb,p.t12,-p.t34)),
        PTC(c1325(p.zb,-p.t12,p.t34)),
        PTC(c1325(p.zb,-p.t12,-p.t34)),
        PTC(c1324(p.zb,p.t12,p.u)),
        PTC(c1324(p.zb,-p.t12,-p.u))
    });
    return *_patches;
}

template<typename T>
void qq2yyg1<T>::LC::bub::patch(const PSpoint& p, const bool taylor)
{
    _on.fill(true);
    _patch.fill(false);
    if (!taylor) return;
    size_t nPatch = 0;
    if (todouble(fabs(p.s13-p.s25)/(fabs(p.s13)+fabs(p.s25)))<patchDelta) {
        _on[1-1]=false; _on[6-1]=false; _patch[1-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s25)/(fabs(p.s14)+fabs(p.s25)))<patchDelta) {
        _on[2-1]=false; _on[6-1]=false; _patch[2-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s23-p.s15)/(fabs(p.s23)+fabs(p.s15)))<patchDelta) {
        _on[3-1]=false; _on[5-1]=false; _patch[3-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s24-p.s15)/(fabs(p.s24)+fabs(p.s15)))<patchDelta) {
        _on[4-1]=false; _on[5-1]=false; _patch[4-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s13-p.s24)/(fabs(p.s13)+fabs(p.s24)))<patchDelta*todouble(p.zb)) {
        _on[1-1]=false; _on[4-1]=false; _patch[5-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s23)/(fabs(p.s14)+fabs(p.s23)))<patchDelta*todouble(p.zb)) {
        _on[2-1]=false; _on[3-1]=false; _patch[6-1]=true;
        ++nPatch;
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg1 patch: too many large cancellations!" << std::endl;
        _on.fill(true);
        _patch.fill(false);
    }
    return;
}


// LC box

template<typename T>
double qq2yyg1<T>::LC::box::master(const size_t i, const PSpoint& p, const int j)
{
    if (i>masters().size()) throw;
    return xTerm(masters()[i-1](p),j);
}

template<typename T>
EpsExp qq2yyg1<T>::LC::box::master(const size_t i, const PSpoint& p)
{
    if (i>masters().size()) throw;
    return x2Finite(masters()[i-1](p));
}

template<typename T>
double qq2yyg1<T>::LC::box::eval(const PSpoint& p, const int i)
{
    return factor()*accTerm(masters().begin(),masters().end(),p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::LC::box::eval(const PSpoint& p)
{
    return factor()*acc2Finite(masters().begin(),masters().end(),p);
}

template<typename T>
array<typename qq2yyg1<T>::Master,9>& qq2yyg1<T>::LC::box::masters()
{
    static array<Master,9>* _masters = new array<Master,9>({
        CXM(c<1>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s15,p.s24,1)),
        CXM(c<1>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s15,p.s23,1)),
        CXM(c<1>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s25,p.s14,1)),
        CXM(c<1>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s25,p.s13,1)),
        CXM(c<2>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s34,p.s25,1)),
        CXM(c<2>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s34,p.s25,1)),
        CXM(c<2>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s34,p.s15,1)),
        CXM(c<2>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s34,p.s15,1)),
        CXM(c<3>(p.zb,p.t12,p.t34,p.u),  box6(p.s15,p.s25,p.s34,1))
    });
    return *_masters;
}

// SC

template<typename T>
const double& qq2yyg1<T>::SC::factor()
{
    static double* _factor = new double(static_cast<double>(QCD::CA-2.*QCD::CF));
    return *_factor;
}

template<typename T>
double qq2yyg1<T>::SC::eval(const PSpoint& p, const int i, const bool t)
{
    return bub::eval(p,i,t)+box::eval(p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::SC::eval(const PSpoint& p, const bool t)
{
    return bub::eval(p,t)+box::eval(p);
}

// SC bub

template<typename T>
array<bool,10> qq2yyg1<T>::SC::bub::_on = array<bool,10>();

template<typename T>
array<bool,6 > qq2yyg1<T>::SC::bub::_patch = array<bool,6>();

template<typename T>
double qq2yyg1<T>::SC::bub::master(const size_t i, const PSpoint& p, const int j)
{
    if (i>masters().size()) throw;
    return xTerm(masters()[i-1](p),j);
}

template<typename T>
EpsExp qq2yyg1<T>::SC::bub::master(const size_t i, const PSpoint& p)
{
    if (i>masters().size()) throw;
    return x2Finite(masters()[i-1](p));
}

template<typename T>
double qq2yyg1<T>::SC::bub::eval(const PSpoint& p, const int i, const bool taylor)
{
    patch(p, taylor);
    return factor()*(
                     selAccTerm(masters().begin(),masters().end(),_on.begin(),p,i)+
                     selAccPatchTerm(patches().begin(),patches().end(),_patch.begin(),p,i)
                     );
}

template<typename T>
EpsExp qq2yyg1<T>::SC::bub::eval(const PSpoint& p, const bool taylor)
{
    patch(p,taylor);
    return factor()*(
                     selAcc2Finite(masters().begin(),masters().end(),_on.begin(),p)+
                     selAccPatch2Finite(patches().begin(),patches().end(),_patch.begin(),p)
                     );
}

template<typename T>
array<typename qq2yyg1<T>::Master,10>& qq2yyg1<T>::SC::bub::masters()
{
    static array<Master,10>* _masters = new array<Master,10>({
        CXM(c<1>(p.zb,p.t12,p.t34,p.u),  bubble(p.s12,3)),
        CXM(c<2>(p.zb,p.t12,p.t34,p.u),  bubble(p.s13,3)),
        CXM(c<2>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s14,3)),
        CXM(c<2>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s23,3)),
        CXM(c<2>(p.zb,-p.t12,-p.t34,p.u),bubble(p.s24,3)),
        CXM(c<3>(p.zb,p.t12,p.t34,p.u),  bubble(p.s15,3)),
        CXM(c<3>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s25,3)),
        CXM(c<4>(p.zb,p.t12,p.t34,p.u),  bubble(p.s34,3)),
        CXM(c<5>(p.zb,p.t12,p.t34,p.u),  bubble(p.s35,3)),
        CXM(c<5>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s45,3))
    });
    return *_masters;
}

template<typename T>
array<typename qq2yyg1<T>::Patch,6>& qq2yyg1<T>::SC::bub::patches()
{
    static array<Patch,6>* _patches = new array<Patch,6>({
        PTC(c1325(p.zb,p.t12,p.t34)),
        PTC(c1325(p.zb,p.t12,-p.t34)),
        PTC(c1325(p.zb,-p.t12,p.t34)),
        PTC(c1325(p.zb,-p.t12,-p.t34)),
        PTC(c1324(p.zb,p.t12,p.u)),
        PTC(c1324(p.zb,-p.t12,-p.u))
    });
    return *_patches;
}

template<typename T>
void qq2yyg1<T>::SC::bub::patch(const PSpoint& p, const bool taylor)
{
    _on.fill(true);
    _patch.fill(false);
    if (!taylor) return;
    size_t nPatch = 0;
    if (todouble(fabs(p.s13-p.s25)/(fabs(p.s13)+fabs(p.s25)))<patchDelta) {
        _on[2-1]=false; _on[7-1]=false; _patch[1-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s25)/(fabs(p.s14)+fabs(p.s25)))<patchDelta) {
        _on[3-1]=false; _on[7-1]=false; _patch[2-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s23-p.s15)/(fabs(p.s23)+fabs(p.s15)))<patchDelta) {
        _on[4-1]=false; _on[6-1]=false; _patch[3-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s24-p.s15)/(fabs(p.s24)+fabs(p.s15)))<patchDelta) {
        _on[5-1]=false; _on[6-1]=false; _patch[4-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s13-p.s24)/(fabs(p.s13)+fabs(p.s24)))<patchDelta*todouble(p.zb)) {
        _on[2-1]=false; _on[5-1]=false; _patch[5-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s23)/(fabs(p.s14)+fabs(p.s23)))<patchDelta*todouble(p.zb)) {
        _on[3-1]=false; _on[4-1]=false; _patch[6-1]=true;
        ++nPatch;
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg1 patch: too many large cancellations!" << std::endl;
        _on.fill(true);
        _patch.fill(false);
    }
    return;
}


// SC box

template<typename T>
double qq2yyg1<T>::SC::box::master(const size_t i, const PSpoint& p, const int j)
{
    if (i>masters().size()) throw;
    return xTerm(masters()[i-1](p),j);
}

template<typename T>
EpsExp qq2yyg1<T>::SC::box::master(const size_t i, const PSpoint& p)
{
    if (i>masters().size()) throw;
    return x2Finite(masters()[i-1](p));
}

template<typename T>
double qq2yyg1<T>::SC::box::eval(const PSpoint& p, const int i)
{
    return factor()*accTerm(masters().begin(),masters().end(),p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::SC::box::eval(const PSpoint& p)
{
    return factor()*acc2Finite(masters().begin(),masters().end(),p);
}

template<typename T>
array<typename qq2yyg1<T>::Master,21>& qq2yyg1<T>::SC::box::masters()
{
    static array<Master,21>* _masters = new array<Master,21>({
        CXM(c<1>(p.zb,p.t12,p.t34,p.u),  box6(p.s12,p.s13,p.s45,1)),
        CXM(c<1>(p.zb,p.t12,-p.t34,-p.u),box6(p.s12,p.s14,p.s35,1)),
        CXM(c<1>(p.zb,-p.t12,p.t34,-p.u),box6(p.s12,p.s23,p.s45,1)),
        CXM(c<1>(p.zb,-p.t12,-p.t34,p.u),box6(p.s12,p.s24,p.s35,1)),
        CXM(c<2>(p.zb,p.t12,p.t34,p.u),  box6(p.s12,p.s15,p.s34,1)),
        CXM(c<2>(p.zb,-p.t12,p.t34,-p.u),box6(p.s12,p.s25,p.s34,1)),
        CXM(c<3>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s34,p.s25,1)),
        CXM(c<3>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s34,p.s25,1)),
        CXM(c<3>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s34,p.s15,1)),
        CXM(c<3>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s34,p.s15,1)),
        CXM(c<4>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s35,p.s24,1)),
        CXM(c<4>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s45,p.s23,1)),
        CXM(c<4>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s35,p.s14,1)),
        CXM(c<4>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s45,p.s13,1)),
        CXM(c<5>(p.zb,p.t12,p.t34,p.u),  box6(p.s15,p.s35,p.s24,1)),
        CXM(c<5>(p.zb,p.t12,-p.t34,-p.u),box6(p.s15,p.s45,p.s23,1)),
        CXM(c<5>(p.zb,-p.t12,p.t34,-p.u),box6(p.s25,p.s35,p.s14,1)),
        CXM(c<5>(p.zb,-p.t12,-p.t34,p.u),box6(p.s25,p.s45,p.s13,1)),
        CXM(c<6>(p.zb,p.t12,p.t34,p.u),  box6(p.s34,p.s35,p.s12,1)),
        CXM(c<6>(p.zb,p.t12,-p.t34,-p.u),box6(p.s34,p.s45,p.s12,1)),
        CXM(c<7>(p.zb,p.t12,p.t34,p.u),  box6(p.s35,p.s45,p.s12,1))
    });
    return *_masters;
}

// Nf

template<typename T>
const double& qq2yyg1<T>::Nf::factor()
{
    // CHECK THIS FACTOR
    static double* _factor = new double(-QCD::sumQ2/512.);
    return *_factor;
}

template<typename T>
double qq2yyg1<T>::Nf::eval(const PSpoint& p, const int i)
{
    return bub::eval(p,i)+box::eval(p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::eval(const PSpoint& p)
{
    return bub::eval(p)+box::eval(p);
}

// Nf bub

template<typename T>
double qq2yyg1<T>::Nf::bub::master(const size_t i, const PSpoint& p, const int j)
{
    if (i>masters().size()) throw;
    return xTerm(masters()[i-1](p),j);
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::bub::master(const size_t i, const PSpoint& p)
{
    if (i>masters().size()) throw;
    return x2Finite(masters()[i-1](p));
}

template<typename T>
double qq2yyg1<T>::Nf::bub::eval(const PSpoint& p, const int i)
{
    return factor()*accTerm(masters().begin(),masters().end(),p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::bub::eval(const PSpoint& p)
{
    return factor()*acc2Finite(masters().begin(),masters().end(),p);
}

template<typename T>
array<typename qq2yyg1<T>::Master,4>& qq2yyg1<T>::Nf::bub::masters()
{
    static array<Master,4>* _masters = new array<Master,4>({
    CXM(c<1>(p.zb,p.t12,p.t34,p.u),  bubble(p.s12,3)),
    CXM(c<2>(p.zb,p.t12,p.t34,p.u),  bubble(p.s34,3)),
    CXM(c<3>(p.zb,p.t12,p.t34,p.u),  bubble(p.s35,3)),
    CXM(c<3>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s45,3))
    });
    return *_masters;
}

// Nf box

template<typename T>
double qq2yyg1<T>::Nf::box::master(const size_t i, const PSpoint& p, const int j)
{
    if (i>masters().size()) throw;
    return xTerm(masters()[i-1](p),j);
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::box::master(const size_t i, const PSpoint& p)
{
    if (i>masters().size()) throw;
    return x2Finite(masters()[i-1](p));
}

template<typename T>
double qq2yyg1<T>::Nf::box::eval(const PSpoint& p, const int i)
{
    return factor()*accTerm(masters().begin(),masters().end(),p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::box::eval(const PSpoint& p)
{
    return factor()*acc2Finite(masters().begin(),masters().end(),p);
}

template<typename T>
array<typename qq2yyg1<T>::Master,3>& qq2yyg1<T>::Nf::box::masters()
{
    static array<Master,3>* _masters = new array<Master,3>({
        CXM(c<1>(p.zb,p.t12,p.t34,p.u),  box6(p.s34,p.s35,p.s12,1)),
        CXM(c<1>(p.zb,p.t12,-p.t34,-p.u),box6(p.s34,p.s45,p.s12,1)),
        CXM(c<2>(p.zb,p.t12,p.t34,p.u),  box6(p.s35,p.s45,p.s12,1))
    });
    return *_masters;
}

// Explicitly compute stuff once to enforce compilation

EpsExp foomdblLCbub = qq2yyg1<dbl>::LC::bub::master(1,qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomqplLCbub = qq2yyg1<qpl>::LC::bub::master(1,qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomrtnLCbub = qq2yyg1<rtn>::LC::bub::master(1,qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomdblLCbox = qq2yyg1<dbl>::LC::box::master(1,qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomqplLCbox = qq2yyg1<qpl>::LC::box::master(1,qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomrtnLCbox = qq2yyg1<rtn>::LC::box::master(1,qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomdblSCbub = qq2yyg1<dbl>::SC::bub::master(1,qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomqplSCbub = qq2yyg1<qpl>::SC::bub::master(1,qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomrtnSCbub = qq2yyg1<rtn>::SC::bub::master(1,qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomdblSCbox = qq2yyg1<dbl>::SC::box::master(1,qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomqplSCbox = qq2yyg1<qpl>::SC::box::master(1,qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomrtnSCbox = qq2yyg1<rtn>::SC::box::master(1,qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomdblNfbub = qq2yyg1<dbl>::Nf::bub::master(1,qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomqplNfbub = qq2yyg1<qpl>::Nf::bub::master(1,qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomrtnNfbub = qq2yyg1<rtn>::Nf::bub::master(1,qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomdblNfbox = qq2yyg1<dbl>::Nf::box::master(1,qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomqplNfbox = qq2yyg1<qpl>::Nf::box::master(1,qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foomrtnNfbox = qq2yyg1<rtn>::Nf::box::master(1,qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));

EpsExp foodbl = qq2yyg1<dbl>::eval(qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp fooqpl = qq2yyg1<qpl>::eval(qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foortn = qq2yyg1<rtn>::eval(qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));
double foodbl0 = qq2yyg1<dbl>::eval(qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4),0);
double fooqpl0 = qq2yyg1<qpl>::eval(qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4),0);
double foortn0 = qq2yyg1<rtn>::eval(qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4),0);
