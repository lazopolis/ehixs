#ifndef CROSS_SECTION_H
#define CROSS_SECTION_H

#include <map>
#include <string>
#include "convolutions.h"
//#include "luminosity.h"
//#include "event.h"
//#include "production.h"
#include "model.h"
#include "fourvector.h"
using namespace std;

class XSection
{
public:

    virtual ~XSection()
    {
        delete _lumi;
    }

    virtual void Evaluate(double*)=0;
    virtual void Configure()=0;
    virtual NewLuminosity* AllocateLuminosity(const UserInterface&)=0;
    void initialize(const UserInterface&);

    void SetEventBox(EventBox& event_box);

    friend ostream& operator<<(ostream& stream, const XSection&);

    const CModel& model = _model;

protected:

    /// \name Data Members
    /// @{

    CModel _model;
    EventBox* event_box_;
    NewLuminosity* _lumi;    ///< Pointer to the luminosity object

    double _smax;
    double _as_pi;
    double _muR;
    double _muF;
    
    /// @}
    
};

//class XSectionFactory
//{
//public:
//    static XSection* create(const size_t n);
//    static void registerit(BaseSector* maker);
//private:
//    static vector<BaseSector*>& bookkeeper();
//};


/**
 *
 * \struct BaseSector
 * \brief  Container for information about sectors
 *
 */

struct BaseSector
{

    /// \name Data members
    /// @{

    string name;
    InitialStateFlavors isf;
    int alpha_power;
    size_t dim;

    /// @}

    /// \name Constructors and destructor
    /// @{

    BaseSector();

    BaseSector(const string& iname, const InitialStateFlavors& iisf, const int ialphapow, const size_t idim);

    BaseSector(const BaseSector& that):
    name(that.name), isf(that.isf), alpha_power(that.alpha_power), dim(that.dim)
    {}

    ~BaseSector()
    {}

    /// @}

    /// \name Member functions
    /// @{

    virtual XSection* create() = 0;

    friend ostream& operator<<(ostream& stream, const BaseSector& info)
    {
        return stream
        << info.name << " "
        << info.isf << ": "
        << "a^" << info.alpha_power << ", "
        << "dim=" << info.dim;
    }
    
    /// @}
    
};

template<typename XSectionType>
class Sector : public BaseSector
{
    static const string name;
    static const InitialStateFlavors isf;
    static const int alpha_power;
    static const size_t dim;
public:
    Sector() : BaseSector(name, isf, alpha_power, dim) {}
    virtual XSection* create() {return new XSectionType;};
};

#endif




