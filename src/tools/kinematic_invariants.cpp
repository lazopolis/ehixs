#include "kinematic_invariants.h"


void KinematicInvariants::compute_dimensionless_invariants()
{
    const double s12 = s_ij_[index(1)][index(2)];
    for (int i=1;i<max_+1;i++)
    {
        q_i_[index(i)] = s_i_[index(i)]/ s12;
        for (int j=1;j<i;j++)
        {
            q_ij_[index(j)][index(i)] = s_ij_[index(j)][index(i)]/s12;
        }
    }
}

int KinematicInvariants::index(int i) const
{
    check_size(i);
    return i-1;
}

int KinematicInvariants::max(int i,int j) const
{
    if (i<j) return j;
    if (j<i) return i;
    cout<<"\n Error in Kinematic Invariants: you asked for s"
    <<i<<j
    <<" which is illegal (use s("<<i<<") for masses)"
    <<endl;
    exit(1);
}

int KinematicInvariants::min(int i,int j) const
{
    if (i<j) return i;
    if (j<i) return j;
    cout<<"\n Error in Kinematic Invariants: you asked for s"
    <<i<<j
    <<" which is illegal (use s("<<i<<") for masses)"
    <<endl;
    exit(1);
}

void KinematicInvariants::check_size(int i) const
{
    if (i>max_)
    {
        cout<<"\nError in Kinematic Invariants: you tried to set an invariant with index "<<i<<" while the maximum allowed is "<<max_<<endl;
        exit(1);
    }
}


