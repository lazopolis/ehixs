/**
 * \file   xcodetest.h
 * \author Simone Lionetti
 * \date   September 2014
 * \brief  This file contains dummy functions mimicking Google tests for debugging tests within Xcode
 */

#include <iostream>
using namespace std;

void ASSERT_TRUE(const bool foo)
{
    if (foo) cout << "Passed" << endl;
    else cout << "Failed" << endl;
    return;
}

void ASSERT_EQUAL(const double a, const double b)
{
    if ( a == b ) cout << "Passed" << endl;
    else cout << "Failed" << endl;
    return;
}

void ASSERT_LT(const double a, const double b)
{
    if ( a < b ) cout << "Passed" << endl;
    else cout << "Failed" << endl;
    return;
}

void ASSERT_GT(const double a, const double b)
{
    if ( a > b ) cout << "Passed" << endl;
    else cout << "Failed" << endl;
    return;
}

