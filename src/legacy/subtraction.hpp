/** \file subtraction.hpp
  *
  * Define topologies and subtractions in terms of ... Explain how it is going.
  * Uses a mathematica-generated file subtraction_expanded.hpp.
  *
  * \author Romain Mueller, muellrom@ethz.ch.
  */

/* ==================================================
   Typedefs
   ================================================== */

/** \brief Ref. to numerator functions type */
typedef void   (&rNumF)(Parametrization&, double*);
/** \brief Ptr. to numerator functions type */
typedef void   (*pNumF)(Parametrization&, double*);
/** \brief Ref. to subtracted functions type */
typedef double (&rSubF)(double, double, double, double, double, double, double, double, double, double, double, unsigned);
/** \brief Ptr. to subtracted functions type */
typedef void   (*pSubF)(double, double, double, double, double, double, double, double, double, double, double, unsigned);
/** \brief Ref. to wrapper for jet functions */
typedef double (&rWrpF)(const double [3][3][3], const double [3][3][3], double, double, double, double, double, double);
/** \brief Ref. to jet functions type */
typedef void   (&rJetF)(double, double, Parametrization&);

/* ==================================================
   Wrappers
   ================================================== */

/** \brief Rotation wrapper */
inline void Rot12(rNumF F, Parametrization& P, double* v)
{
  double v1[4];
  double v2[4];

  ParametrizationFlip12 Q(P);
  F(P, v1);
  F(Q, v2);

  v[0] = v1[0]+v2[1];
  v[1] = v1[1]+v2[0];
  v[2] = v1[2]+v2[3];
  v[3] = v1[3]+v2[2];
}

/** \brief Rotation wrapper */
inline void Rot34(rNumF F, Parametrization& P, double* v)
{
  double v1[4];
  double v2[4];

  ParametrizationFlip34 Q(P);
  F(P, v1);
  F(Q, v2);

  v[0] = v1[0]+v2[2];
  v[1] = v1[1]+v2[3];
  v[2] = v1[2]+v2[0];
  v[3] = v1[3]+v2[1];
}

/** \brief Rotation wrapper */
inline void Rot1234(rNumF F, Parametrization& P, double* v)
{
  double v1[4];
  double v2[4];

  ParametrizationFlip1234 Q(P);
  F(P, v1);
  F(Q, v2);

  v[0] = v1[0]+v2[3];
  v[1] = v1[1]+v2[2];
  v[2] = v1[2]+v2[1];
  v[3] = v1[3]+v2[0];
}

/** \brief Power function (from Mathematica) */
inline double Power(double a, double e)
{ return pow(a, e); }

/** \brief Power function (from Mathematica) */
inline double Power(double a, int e)
{
  switch(e)
  {
  case 1:
    return a;
  case 2:
    return a*a;
  case 3:
    return a*a*a;
  case 4:
    return a*a*a*a;
  default:
    return pow(a, e);
  }
}

/* ==================================================
   The actual stuff
   ================================================== */

/** \brief Template definition for F
  *
  * Should not be called but overwritten, see parametrizations.hpp for examples. Calls assert(true). */
template<class TopologyDescriptor>
inline double F(double x1, double x2, double x3, double x4, double z, double s)
{ assert(true); return 0; }

/** \brief Template definition for G
  *
  * Should not be called but overwritten, see parametrizations.hpp for examples. Calls assert(true). */
template<class TopologyDescriptor>
inline double G(double x1, double x2, double x3, double x4, double z, double s)
{ assert(true); return 0; }

/** \brief Template definition for M
  *
  * Can be called, and is simply the identity mapping. */
template<class TopologyDescriptor, unsigned I>
inline double M(double x1, double x2, double x3, double x4)
{ return ( I==1 ? x1 : ( I==2 ? x2 : ( I==3 ? x3 : x4 ) ) ); }

/** \breif Template definition for W
  *
  * Can be called, and simply calls F(P, v). */
template<class TopologyDescriptor>
inline void  W(rNumF F, Parametrization& P, double *v)
{ F(P, v); }

/** The non-linear mappings */
inline double NLM(double x, double A, double B)
{ /*return ( x==1 ? 1 : ( x==0 ? 0 : x*A/(A*x+B*(1-x)) ) ); */
  if(x==1)
    return 1;
  else if(x==0)
    return 0;
  else {
    if(B==0)
      return 1;
    else
      return x*A/(A*x+B*(1-x));
  }
}

/* ==================================================
   Topology subtraction
   ================================================== */
inline void Zero_p(Parametrization & P, double *v)
{}

/** \brief Topology subtraction up three singular variables
  *
  * The z subtraction is done there for the moment...
  */
template<class TopologyDescriptor, 
         rNumF f0, rNumF f1, rNumF f2, rNumF f3, rJetF J>
void TopologySubtraction(double n, double d, 
                         double x1, double x2, double x3, double x4, 
                         double z, double s, double bx1, double bx2, double lh,
                         unsigned Pole=0)
{
  // Explain how the pole stuff is implemented...

  // Get params from TopologyDescriptor (for notational convenience)
  const int A1  = TopologyDescriptor::A1;
  const int A1b = TopologyDescriptor::B1;
  const int A2  = TopologyDescriptor::A2;
  const int A2b = TopologyDescriptor::B2;
  const int A3  = TopologyDescriptor::A3;
  const int A3b = TopologyDescriptor::B3;

  // Some preparative stuff
  const double D1  = ( A1  ?   x1 : 1 );
  const double D1b = ( A1b ? 1-x1 : 1 );
  const double D2  = ( A2  ?   x2 : 1 );
  const double D2b = ( A2b ? 1-x2 : 1 );
  const double D3  = ( A3  ?   x3 : 1 );
  const double D3b = ( A3b ? 1-x3 : 1 );

  const double R1 = ( A1 ? A1*log(x1) : 0 ) + ( A1b ? A1b*log(1-x1) : 0 );
  const double R2 = ( A2 ? A2*log(x2) : 0 ) + ( A2b ? A2b*log(1-x2) : 0 );
  const double R3 = ( A3 ? A3*log(x3) : 0 ) + ( A3b ? A3b*log(1-x3) : 0 );

  // Normalizations ...
  // ... Franz second great normalization
  const double C0 =  0.03125000000000000; 
  const double C1 = -0.04332169878499658;
  const double C2 = -0.07278006580812656;
  const double C3 =  0.02847524056751834;
  // ... NNLO paper normalization
/*  const double C0 =  0.0002519651127593710;
  const double C1 =  0.0006352853843646054;
  const double C2 = -0.00002805223225977534;
  const double C3 = -0.002224583413900933;*/

  // Bad implementation of the z-subtraction ???
  // -> compute f(z)(1-z)^(-1-4 e) and f(1)(1-z)^(-1-4 e).
  for(unsigned zi=0; zi<2; ++zi)
  {
  // compute all relevant limits
  // One should expand this to gain time !
  for(unsigned i=0; i<3; ++i)
  for(unsigned j=0; j<3; ++j)
  for(unsigned k=0; k<3; ++k)
  {
    if( ( i==0 ? A1 : ( i==1 ? A1b : true ) ) && 
        ( j==0 ? A2 : ( j==1 ? A2b : true ) ) && 
        ( k==0 ? A3 : ( k==1 ? A3b : true ) ) )
    {
      // All limits
      const double y1 = ( i==2 ? x1 : i );
      const double y2 = ( j==2 ? x2 : j );
      const double y3 = ( k==2 ? x3 : k );
      const double y4 = x4;
      const double w  = ( zi ? 1 : z);

      double N0[4] = { 0, 0, 0, 0 };
      double N1[4] = { 0, 0, 0, 0 };
      double N2[4] = { 0, 0, 0, 0 };
      double N3[4] = { 0, 0, 0, 0 };
      double L = 0;

      // ... parametrization
      ParametrizationDef<TopologyDescriptor::Parametrization> 
        p(M<TopologyDescriptor, 1>(y1, y2, y3, y4),
          M<TopologyDescriptor, 2>(y1, y2, y3, y4),
          M<TopologyDescriptor, 3>(y1, y2, y3, y4),
          M<TopologyDescriptor, 4>(y1, y2, y3, y4),
          w, s, bx1, bx2);

      // ... f at order 0
      if(Pole==0) W<TopologyDescriptor>(f0, p, N0);

      if( i!=2 || j!=2 || k!=2 ) 
      {
        // ... f at order 1
        switch(Pole)
        {
          case 0:
            W<TopologyDescriptor>(f1, p, N1);
            break;
          case 1:
            W<TopologyDescriptor>(f0, p, N1);
            break;
        }

        // ... logs
        L = log(F<TopologyDescriptor>(y1, y2, y3, y4, w, s)) - 2*lh - 2*log(w) + 4*log(1-z);

        if( (i!=2&&j!=2) || (j!=2&&k!=2) || (k!=2&&i!=2) )
        {
          // ... f at order 2
          switch(Pole)
          {
            case 0:
              W<TopologyDescriptor>(f2, p, N2);
              break;
            case 1:
              W<TopologyDescriptor>(f1, p, N2);
              break;
            case 2:
              W<TopologyDescriptor>(f0, p, N2);
              break;
          }

          // ... f at order 3
          if( i!=2 && j!=2 && k!=2 )
          {
            switch(Pole)
            {
              case 0:
                W<TopologyDescriptor>(f3, p, N3);
                break;
              case 1:
                W<TopologyDescriptor>(f2, p, N3);
                break;
              case 2:
                W<TopologyDescriptor>(f1, p, N3);
                break;
              case 3:
                W<TopologyDescriptor>(f0, p, N3);
                break;
            }
          }
        }
      }

      // Initial values for weights
      const double a0 = n*G<TopologyDescriptor>(y1, y2, y3, y4, w, s)*( zi ? -1 : 1);
      const double b0 = d*(1-z);

      // Jet Rotations, 0->3
      for(unsigned r=0; r<4; ++r)
      {
        double a = a0;
        double b = b0;

        // include file generated with mathematica
        // OMG this is very raw !
        #include "subtraction_expanded.hpp"

        // it there is nothing to do, then do nothing
        if(a==0) continue;

        // Call the jet function, taking care of the jet rotations
        switch(r)
        {
          case 0:
            {
              J(a, b, p);
            }
            break;
          case 1:
            {
              ParametrizationFlip12 q(p);
              J(a, b, q);
            }
            break;
          case 2:
            {
              ParametrizationFlip34 q(p);
              J(a, b, q);
            }
            break;
          case 3:
            {
              ParametrizationFlip1234 q(p);
              J(a, b, q);
            }
            break;
        }
      }
    } // if singularities
  } // i,j,k-loops
  //break;
  } // z-loop
}
