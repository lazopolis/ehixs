/**
 *
 * \file    factory.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef FACTORY_H
#define FACTORY_H

#include <string>
#include <vector>
#include <map>

#include <iostream>

/**
 *
 * \class BaseFactory
 * \brief Base class for factories
 *
 */

template<class BaseObj>
class BaseFactory
{

public:

    typedef std::map<std::string,BaseFactory<BaseObj>*> map;

    /// \name Static functions
    /// @{

//    static map lookup;

    static map& lookup()
    {
        static map* _lookup = new map();
        for (typename map::iterator it = _lookup->begin(); it != _lookup->end(); ++it)
            std::cout << it->first << std::endl;
        return *_lookup;
    };

    /// @}
    
    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    BaseFactory()
    {}

    /// Destructor
    virtual ~BaseFactory()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Calls the constructor for the object we want to create
    virtual BaseObj* create(const std::vector<std::string>& specs) const = 0;

    /// Returns the information about the object
    virtual const std::string& info() const
    {
        static const std::string* _noinfo = new std::string("no information provided.");
        return *_noinfo;
    };
    
    /// @}

private:

};

/**
 *
 * \class Factory
 * \brief Templatized factory object
 *
 */

template<class BaseObj, class Obj>
class Factory : public BaseFactory<BaseObj>
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    Factory(const std::string& name = "", const std::string& info = "") :
    BaseFactory<BaseObj>(), _info(info)
    {
        std::cout << "emplacing" << std::endl;
        BaseFactory<BaseObj>::lookup().emplace(name,this);
    }

    /// Destructor
    ~Factory()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Calls the constructor for a specific object Obj
    virtual BaseObj* create(const std::vector<std::string>& specs) const
    {
        Obj* foo = new Obj(specs);
        return dynamic_cast<BaseObj*>(foo);
    };

    /// Returns the information about the object
    virtual const std::string& info() const
    {
        return _info;
    };

    /// @}

private:

    /// \name Data members
    /// @{

    const std::string _info;  ///< Extra information about the object

    /// @}

};

#endif
