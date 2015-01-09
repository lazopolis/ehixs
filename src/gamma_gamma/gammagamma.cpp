#include <iostream>
using namespace std;
#include "bottom_fusion.h"
#include "bbh_cuts.h"

void BottomFusion::ConfigureCuts()
{
    // The file includes the explicit declarations of cut objects.
    // The cuts declared there are available, not actually active.
    // They can be activated in runcard. The parsing of runcard for cuts
    // is done in Production, after the call to this function.
    #include "bbh_cut_initialization.h"
}
