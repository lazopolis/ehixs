/**
 * \file   event_test.cpp
 * \author Simone Lionetti
 * \date   September 2014
 * \brief  This file contains the tests for the Event, CombinedEvent and FourVector classes
 */

#include "xcodetest.h"
//#include "gtest/gtest.h"
#include "event.h"
using namespace std;

//TEST(EventTest,eventtest)
//{
//
//    return;
//
//}

int main(int argc, char**argv)
{

    // Testing Event

    vector<FourVector> myPs;
    myPs.push_back(FourVector(1.,0.,0.,1.));
    myPs.push_back(FourVector(1.,0.,0.,-1.));
    myPs.push_back(FourVector(2.,0.,0.,0.));
    Event myEvent(0.1,myPs);
    cout << myEvent << endl;

    Event anotherEvent(myEvent);
    cout << anotherEvent.p[0] << endl;
    cout << anotherEvent.weight << endl;

    myPs.pop_back();
    myPs.push_back(FourVector(1.,0.,0.5,0.5));
    myPs.push_back(FourVector(1.,0.,-0.5,-0.5));

    Event yetAnotherEvent(1.,myPs);
    cout << yetAnotherEvent << endl;
    myEvent = yetAnotherEvent;
    Event empty;
    empty=anotherEvent;

    cout << empty << endl;


    // Testing EventBox
    EventBox myBox;
    myBox.add(1., myPs);


//    ::testing::InitGoogleTest(&argc, argv);
//    return  RUN_ALL_TESTS();

    return 0;

}
