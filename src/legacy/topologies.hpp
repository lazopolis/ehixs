/** \file topologies.hpp
  *
  * This defines all the topologies. A topology declaration needs an object (typically CXXX) that enumerates the Parametrization and the singularity structure associated to the denominator, and the template redefinitions for the functions F, G and W. This file has been generated by maple, using do.py.
  *
  * \author Romain Mueller, muellrom@ethz.ch
  */



/* ==================================================
  C00
*/
struct C00 {
  enum {
    Parametrization = EnergyAngles, 
    A1 = 0, 
    B1 = 0, 
    A2 = 0, 
    B2 = 0, 
    A3 = 0, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<C00> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t10;
  double t2;
  double t3;
  double t9;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t3 = t2 * t2;
  t9 = pow(z - 0.1e1, 0.2e1);
  t10 = t9 * t9;
  return(s * t3 / (-0.2e1 + t1) * t10 * x1 * (-0.1e1 + x1));
}


/** \brief (gen. with maple) */
template<>
inline double F<C00> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t10;
  double t2;
  double t3;
  double t5;
  double t6;
  double t8;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t3 = t2 * t2;
  t5 = sin(0.3141592654e1 * x4);
  t6 = t5 * t5;
  t8 = x1 * x1;
  t10 = pow(-0.1e1 + x1, 0.2e1);
  return(t3 * t6 * t8 * t10 * x3 * (-0.1e1 + x3) * x2 * (-0.1e1 + x2));
}


/* ==================================================
  C11a
*/
struct C11a {
  enum {
    Parametrization = EnergyAngles, 
    A1 = -2, 
    B1 = -2, 
    A2 = -1, 
    B2 = -1, 
    A3 = -1, 
    B3 = -1
  };
};

/** \brief (gen. with maple) */
template<>
inline double G<C11a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t4;
  t1 = s * s;
  t4 = kappa(x1, x2, x3, x4, z);
  return(-0.1e1 / t1 / s / (-0.2e1 + t4));
}


/** \brief (gen. with maple) */
template<>
inline double F<C11a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t2;
  double t3;
  double t5;
  double t6;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t3 = t2 * t2;
  t5 = sin(0.3141592654e1 * x4);
  t6 = t5 * t5;
  return(0.1000000000e1 * t3 * t6);
}


/* ==================================================
  C11b
*/
struct C11b {
  enum {
    Parametrization = EnergyAngles, 
    A1 = 0, 
    B1 = 0, 
    A2 = 0, 
    B2 = -1, 
    A3 = -1, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<C11b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t2;
  double t5;
  double t6;
  t2 = pow(z - 0.1e1, 0.2e1);
  t5 = kappa(x1, x2, x3, x4, z);
  t6 = t5 * t5;
  return(-0.1e1 * t2 / s * t6 / (-0.2e1 + t5));
}


/** \brief (gen. with maple) */
template<>
inline double F<C11b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t11;
  double t2;
  double t3;
  double t5;
  double t6;
  double t8;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t3 = t2 * t2;
  t5 = sin(0.3141592654e1 * x4);
  t6 = t5 * t5;
  t8 = x1 * x1;
  t11 = pow(-0.1e1 + x1, 0.2e1);
  return(-0.1000000000e1 * t3 * t6 * t8 * t11 * (-0.1e1 + x3) * x2);
}


/* ==================================================
  C11c
*/
struct C11c {
  enum {
    Parametrization = EnergyAngles, 
    A1 = 0, 
    B1 = 0, 
    A2 = 0, 
    B2 = 0, 
    A3 = 0, 
    B3 = -1
  };
};

/** \brief (gen. with maple) */
template<>
inline double G<C11c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t2;
  double t7;
  double t8;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t7 = z - 0.1e1;
  t8 = t7 * t7;
  return(t2 * t1 / (-0.2e1 + t1) * t8 * t7 * (-0.1e1 + x1));
}


/** \brief (gen. with maple) */
template<>
inline double F<C11c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t11;
  double t2;
  double t3;
  double t5;
  double t6;
  double t8;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t3 = t2 * t2;
  t5 = sin(0.3141592654e1 * x4);
  t6 = t5 * t5;
  t8 = x1 * x1;
  t11 = pow(-0.1e1 + x1, 0.2e1);
  return(-0.1e1 * t3 * t6 * t8 * t11 * x3 * x2 * (-0.1e1 + x2));
}


/* ==================================================
  C11d
*/
struct C11d {
  enum {
    Parametrization = EnergyAngles, 
    A1 = 0, 
    B1 = 0, 
    A2 = 0, 
    B2 = -1, 
    A3 = 0, 
    B3 = 0
  };
};

/** \brief (gen. with maple) */
template<>
inline double G<C11d> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t2;
  double t7;
  double t8;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t7 = z - 0.1e1;
  t8 = t7 * t7;
  return(-0.1e1 * t2 * t1 / (-0.2e1 + t1) * t8 * t7 * x1);
}


/** \brief (gen. with maple) */
template<>
inline double F<C11d> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t11;
  double t2;
  double t3;
  double t5;
  double t6;
  double t8;
  t1 = kappa(x1, x2, x3, x4, z);
  t2 = t1 * t1;
  t3 = t2 * t2;
  t5 = sin(0.3141592654e1 * x4);
  t6 = t5 * t5;
  t8 = x1 * x1;
  t11 = pow(-0.1e1 + x1, 0.2e1);
  return(-0.1000000000e1 * t3 * t6 * t8 * t11 * x3 * (-0.1e1 + x3) * x2);
}


/* ==================================================
  C12a
*/
struct C12a {
  enum {
    Parametrization = Hierarchical, 
    A1 = -2, 
    B1 = -2, 
    A2 = -1, 
    B2 = -2, 
    A3 = 0, 
    B3 = -1
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<C12a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t12;
  double t16;
  double t17;
  double t18;
  double t22;
  double t25;
  double t26;
  double t32;
  double t37;
  double t38;
  double t41;
  double t42;
  double t43;
  double t5;
  double t6;
  double t63;
  double t8;
  double t9;
  t1 = s * s;
  t5 = x1 * z;
  t6 = -0.1e1 * z - 0.1e1 * x1 + t5;
  t8 = x2 * x2;
  t9 = t8 * x1;
  t12 = x3 * t8;
  t16 = x1 * x1;
  t17 = x3 * t16;
  t18 = x2 * z;
  t22 = z * z;
  t25 = x2 * x1;
  t26 = t25 * z;
  t32 = cos(0.3141592654e1 * x4);
  t37 = sqrt(x3 * t6 * x2 * (-0.1e1 + x3));
  t38 = t32 * t37;
  t41 = 0.1e1 * t25;
  t42 = -0.1e1 * t9 * z - 0.1e1 * t12 * x1 + t9 + t12 * t5 - 0.2e1 * t17 * t18 + t17 * x2 + t17 * t22 * x2 + t26 - 0.1e1 * t18 - 0.1e1 * x3 * t22 * t25 - 0.2e1 * t38 * t25 - t41;
  t43 = x3 * x2;
  t63 = t43 * x1 + 0.2e1 * t38 * t26 + t43 * z - 0.2e1 * t38 * t5 - 0.2e1 * t5 + 0.2e1 * t38 * z + 0.2e1 * t38 * x1 - 0.1e1 * t22 + 0.2e1 * x1 * t22 - 0.1e1 * t16 + 0.2e1 * t16 * z - 0.1e1 * t16 * t22;
  return(0.1e1 / t1 * t6 / (t42 + t63) * (z + x1 - 0.1e1 * t5 - t41 + t26));
}


/** \brief (gen. with maple) */
template<>
inline double F<C12a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t4;
  double t8;
  double t9;
  t4 = pow(0.1e1 - 0.1e1 * x2 + x3 * x2, 0.2e1);
  t8 = sin(0.3141592654e1 * x4);
  t9 = t8 * t8;
  return(-0.1e1 / t4 * x3 * t9 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z));
}

/** \brief (gen. with maple) */
template<>
inline double M<C12a, 3> (
  double x1,
  double x2,
  double x3,
  double x4)
{
  return(NLM(x3, 1, 0.1e1 - x2));
}

/** (gen. with maple) */
template<>
inline void W<C12a>(rNumF F, Parametrization& P, double *v)
{ Rot1234(F, P, v); }

/* ==================================================
  C12b
*/
struct C12b {
  enum {
    Parametrization = Hierarchical, 
    A1 = -2, 
    B1 = -2, 
    A2 = -1, 
    B2 = 0, 
    A3 = -1, 
    B3 = 0
  };
};

/** \brief (gen. with maple) */
template<>
inline double G<C12b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  t1 = s * s;
  return(0.1e1 / t1);
}


/** \brief (gen. with maple) */
template<>
inline double F<C12b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t7;
  double t8;
  t7 = sin(0.3141592654e1 * x4);
  t8 = t7 * t7;
  return(-0.1e1 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z) * t8 * (-0.1e1 + x2) * (-0.1e1 + x3));
}


/* ==================================================
  C22b
*/
struct C22b {
  enum {
    Parametrization = Hierarchical, 
    A1 = -2, 
    B1 = -2, 
    A2 = -1, 
    B2 = 0, 
    A3 = 0, 
    B3 = 0
  };
};

/** \brief (gen. with maple) */
template<>
inline double G<C22b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t5;
  double t9;
  t1 = s * s;
  t5 = x1 * z;
  t9 = x1 * x2;
  return(-0.1e1 / t1 * (-0.1e1 * z - 0.1e1 * x1 + t5) / (z + x1 - 0.1e1 * t5 - 0.1e1 * t9 + t9 * z));
}


/** \brief (gen. with maple) */
template<>
inline double F<C22b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t2;
  double t3;
  t2 = sin(0.3141592654e1 * x4);
  t3 = t2 * t2;
  return(-0.1000000000e1 * t3 * (-0.1e1 + x2) * x3 * (-0.1e1 + x3) / (-0.1e1 * z - 0.1e1 * x1 + x1 * z));
}


/* ==================================================
  C33b
*/
struct C33b {
  enum {
    Parametrization = Hierarchical, 
    A1 = -2, 
    B1 = -2, 
    A2 = -2, 
    B2 = 0, 
    A3 = -1, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<C33b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t10;
  double t13;
  double t17;
  double t18;
  double t22;
  double t25;
  double t26;
  double t27;
  double t30;
  double t33;
  double t38;
  double t4;
  double t45;
  double t46;
  double t49;
  double t5;
  double t53;
  double t6;
  double t64;
  double t7;
  double t80;
  double t9;
  t1 = s * s;
  t4 = 0.1e1 * z;
  t5 = 0.1e1 * x1;
  t6 = x1 * z;
  t7 = -t4 - t5 + t6;
  t9 = x3 * x1;
  t10 = x2 * z;
  t13 = z * z;
  t17 = x1 * x1;
  t18 = x3 * t17;
  t22 = x3 * x2;
  t25 = x2 * x2;
  t26 = x1 * t25;
  t27 = x3 * z;
  t30 = t17 * t25;
  t33 = x3 * t13;
  t38 = cos(0.3141592654e1 * x4);
  t45 = sqrt(-0.1e1 * x3 * (-0.1e1 + x2) * t7 * (-0.1e1 + x3));
  t46 = t38 * t45;
  t49 = x1 * x2;
  t53 = z * t38 * t45;
  t64 = 0.4e1 * t9 * t10 - 0.3e1 * t9 * x2 * t13 - 0.4e1 * t18 * t10 + 0.2e1 * t17 * t13 * t22 - 0.1e1 * t26 * t27 + 0.2e1 * t27 * t30 + t26 * t33 - 0.1e1 * t30 * t33 - 0.2e1 * t6 * t46 - 0.2e1 * t49 * t46 + 0.2e1 * t49 * t53 - 0.1e1 * t49 * z - 0.5e1 * t9 * z - 0.1e1 * t9 * x2 + 0.4e1 * t9 * t13;
  t80 = 0.4e1 * t18 * z - 0.2e1 * t18 * t13 + t22 * t13 + 0.2e1 * t18 * x2 - 0.1e1 * t30 * x3 + 0.2e1 * t53 + 0.2e1 * x1 * t38 * t45 + t6 + t49 + t27 + t9 - 0.2e1 * t33 - 0.2e1 * t18 - t4 - t5;
  return(0.1e1 / t1 / s * t7 / (t64 + t80));
}


/** \brief (gen. with maple) */
template<>
inline double F<C33b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t11;
  double t4;
  t4 = pow(0.1e1 - 0.1e1 * x3 + x3 * x2, 0.2e1);
  t10 = sin(0.3141592654e1 * x4);
  t11 = t10 * t10;
  return(-0.1e1 / t4 * (-0.1e1 + x2) * (-0.1e1 + x3) * t11 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z));
}

/** \brief (gen. with maple) */
template<>
inline double M<C33b, 3> (
  double x1,
  double x2,
  double x3,
  double x4)
{
  return(NLM(x3, x2, 1));
}

/** (gen. with maple) */
template<>
inline void W<C33b>(rNumF F, Parametrization& P, double *v)
{ Rot12(F, P, v); }

/* ==================================================
  C33c
*/
struct C33c {
  enum {
    Parametrization = Hierarchical, 
    A1 = -2, 
    B1 = -2, 
    A2 = 0, 
    B2 = -2, 
    A3 = 0, 
    B3 = -1
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<C33c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t1;
  double t10;
  double t18;
  double t19;
  double t20;
  double t23;
  double t25;
  double t26;
  double t31;
  double t32;
  double t37;
  double t45;
  double t47;
  double t49;
  double t6;
  double t66;
  double t7;
  double t9;
  t1 = s * s;
  t6 = x1 * z;
  t7 = -0.1e1 * z - 0.1e1 * x1 + t6;
  t9 = x3 * x1;
  t10 = x2 * x2;
  t18 = x3 * x2;
  t19 = z * z;
  t20 = x1 * t19;
  t23 = x1 * x2;
  t25 = x1 * x1;
  t26 = t25 * z;
  t31 = cos(0.3141592654e1 * x4);
  t32 = x1 * t31;
  t37 = sqrt(x3 * t7 * x2 * (-0.1e1 + x3));
  t45 = -0.1e1 * t9 * t10 + x1 * t10 - 0.1e1 * t6 * t10 + t6 * x3 * t10 - 0.1e1 * t18 * t20 + t23 * z - 0.2e1 * t18 * t26 + t18 * t25 - 0.2e1 * t32 * t37 * x2 - 0.1e1 * x2 * z + t18 * z - 0.1e1 * t23;
  t47 = t19 * t25;
  t49 = t31 * t37;
  t66 = t9 * x2 + t18 * t47 + 0.2e1 * t6 * t49 * x2 - 0.2e1 * t6 * t49 - 0.2e1 * t6 + 0.2e1 * z * t31 * t37 + 0.2e1 * t32 * t37 - 0.1e1 * t19 + 0.2e1 * t20 - 0.1e1 * t25 + 0.2e1 * t26 - 0.1e1 * t47;
  return(0.1e1 / t1 / s * t7 / (t45 + t66));
}


/** \brief (gen. with maple) */
template<>
inline double F<C33c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t4;
  double t8;
  double t9;
  t4 = pow(0.1e1 - 0.1e1 * x2 + x3 * x2, 0.2e1);
  t8 = sin(0.3141592654e1 * x4);
  t9 = t8 * t8;
  return(-0.1e1 / t4 * x2 * x3 * t9 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z));
}

/** \brief (gen. with maple) */
template<>
inline double M<C33c, 3> (
  double x1,
  double x2,
  double x3,
  double x4)
{
  return(NLM(x3, 1, 0.1e1 - x2));
}

/** (gen. with maple) */
template<>
inline void W<C33c>(rNumF F, Parametrization& P, double *v)
{ Rot34(F, P, v); }

/* ==================================================
  CQ1a
*/
struct CQ1a {
  enum {
    Parametrization = Hierarchical, 
    A1 = 0, 
    B1 = -2, 
    A2 = 0, 
    B2 = 0, 
    A3 = 0, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ1a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t11;
  double t15;
  double t4;
  double t8;
  t4 = x1 * z;
  t8 = pow(-0.1e1 + z, 0.2e1);
  t11 = x1 * x2;
  t15 = pow(z + x1 - 0.1e1 * t4 - 0.1e1 * t11 + t11 * z, 0.2e1);
  return(-0.1e1 / s * (-0.1e1 * z - 0.1e1 * x1 + t4) * t8 * x1 / t15);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ1a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t2;
  double t3;
  double t4;
  t2 = sin(0.3141592654e1 * x4);
  t3 = t2 * t2;
  t4 = x1 * x1;
  return(-0.1000000000e1 * t3 * t4 * x2 * (-0.1e1 + x2) * x3 * (-0.1e1 + x3) / (-0.1e1 * z - 0.1e1 * x1 + x1 * z));
}


/* ==================================================
  CQ1b
*/
struct CQ1b {
  enum {
    Parametrization = Hierarchical, 
    A1 = 0, 
    B1 = -2, 
    A2 = -1, 
    B2 = 0, 
    A3 = 0, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ1b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t12;
  double t16;
  double t4;
  double t6;
  t4 = x1 * z;
  t6 = pow(-0.1e1 * z - 0.1e1 * x1 + t4, 0.2e1);
  t12 = x1 * x2;
  t16 = pow(z + x1 - 0.1e1 * t4 - 0.1e1 * t12 + t12 * z, 0.2e1);
  return(-0.1e1 / s * t6 * (-0.1e1 + z) * (-0.1e1 + x3) / t16);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ1b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t7;
  double t8;
  t7 = sin(0.3141592654e1 * x4);
  t8 = t7 * t7;
  t10 = x1 * x1;
  return(-0.1000000000e1 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z) * t8 * t10 * (-0.1e1 + x2) * x3 * (-0.1e1 + x3));
}


/* ==================================================
  CQ2a
*/
struct CQ2a {
  enum {
    Parametrization = Hierarchical, 
    A1 = 0, 
    B1 = -2, 
    A2 = 0, 
    B2 = 0, 
    A3 = -1, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ2a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t14;
  double t2;
  double t3;
  double t6;
  t2 = -0.1e1 + z;
  t3 = t2 * t2;
  t6 = x1 * x1;
  t10 = x1 * x2;
  t14 = pow(z + x1 - 0.1e1 * x1 * z - 0.1e1 * t10 + t10 * z, 0.2e1);
  return(0.1e1 / s * t3 * t2 * t6 * x2 / t14);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ2a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t7;
  double t8;
  t7 = sin(0.3141592654e1 * x4);
  t8 = t7 * t7;
  t10 = x1 * x1;
  return(-0.1e1 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z) * t8 * t10 * x2 * (-0.1e1 + x2) * (-0.1e1 + x3));
}


/* ==================================================
  CQ2b
*/
struct CQ2b {
  enum {
    Parametrization = Hierarchical, 
    A1 = 0, 
    B1 = -2, 
    A2 = 0, 
    B2 = 0, 
    A3 = -1, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ2b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t13;
  double t17;
  double t4;
  double t8;
  t4 = x1 * z;
  t8 = pow(-0.1e1 + z, 0.2e1);
  t13 = x1 * x2;
  t17 = pow(z + x1 - 0.1e1 * t4 - 0.1e1 * t13 + t13 * z, 0.2e1);
  return(0.1e1 / s * (-0.1e1 * z - 0.1e1 * x1 + t4) * t8 * x1 * (-0.1e1 + x3) / t17);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ2b> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t2;
  double t3;
  double t4;
  t2 = sin(0.3141592654e1 * x4);
  t3 = t2 * t2;
  t4 = x1 * x1;
  return(-0.1e1 * t3 * t4 * x2 * (-0.1e1 + x2) * (-0.1e1 + x3) / (-0.1e1 * z - 0.1e1 * x1 + x1 * z));
}


/* ==================================================
  CQ3a
*/
struct CQ3a {
  enum {
    Parametrization = Hierarchical, 
    A1 = -2, 
    B1 = -2, 
    A2 = -1, 
    B2 = 0, 
    A3 = 0, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ3a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t13;
  double t15;
  double t16;
  double t19;
  double t23;
  double t27;
  double t30;
  double t31;
  double t35;
  double t36;
  double t4;
  double t41;
  double t5;
  double t50;
  double t51;
  double t57;
  double t61;
  double t68;
  double t75;
  double t9;
  double t93;
  double t95;
  t4 = x1 * z;
  t5 = -0.1e1 * z - 0.1e1 * x1 + t4;
  t9 = x2 * x1;
  t13 = pow(z + x1 - 0.1e1 * t4 - 0.1e1 * t9 + t9 * z, 0.2e1);
  t15 = sqrt(x2);
  t16 = t15 * x3;
  t19 = z * z;
  t23 = x1 * x1;
  t27 = t23 * t19;
  t30 = x2 * t15;
  t31 = t23 * t30;
  t35 = x1 * t30;
  t36 = t19 * x3;
  t41 = cos(0.3141592654e1 * x4);
  t50 = sqrt(-0.1e1 * x3 * (-0.1e1 + t15) * (t15 + 0.1e1) * t5 * (-0.1e1 + x3));
  t51 = t41 * t50;
  t57 = z * t41 * t50;
  t61 = t23 * t15;
  t68 = 0.2e1 * t16 * t4 - 0.3e1 * t16 * x1 * t19 - 0.4e1 * t16 * t23 * z + 0.2e1 * t27 * t16 + 0.2e1 * t31 * x3 * z + t35 * t36 - 0.1e1 * t31 * t36 - 0.2e1 * t9 * t51 - 0.2e1 * t4 * t51 + 0.2e1 * t9 * t57 + t19 * t16 + 0.4e1 * t61 * z - 0.1e1 * t35 * t19 + 0.2e1 * t16 * t23;
  t75 = x1 * t15;
  t93 = -0.2e1 * t31 * z + t16 * x1 - 0.1e1 * t31 * x3 + t16 * z - 0.3e1 * t75 * z + t31 * t19 - 0.1e1 * t35 * x3 - 0.2e1 * t27 * t15 + 0.3e1 * t75 * t19 + t35 * z + 0.2e1 * x1 * t41 * t50 + 0.2e1 * t57 - 0.1e1 * t15 * t19 - 0.2e1 * t61 + t31;
  t95 = pow(t68 + t93, 0.2e1);
  return(-0.1e1 / s / t5 / t13 * t95);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ3a> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t14;
  double t15;
  double t6;
  t6 = sqrt(x2);
  t14 = sin(0.3141592654e1 * x4);
  t15 = t14 * t14;
  return(-0.1000000000e1 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z) * (-0.1e1 + t6) * (t6 + 0.1e1) * x3 * (-0.1e1 + x3) * t15);
}


/* ==================================================
  CQ1c
*/
struct CQ1c {
  enum {
    Parametrization = Hierarchical, 
    A1 = 0, 
    B1 = -2, 
    A2 = -1, 
    B2 = 0, 
    A3 = 0, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ1c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t11;
  double t15;
  double t4;
  double t6;
  t4 = x1 * z;
  t6 = pow(-0.1e1 * z - 0.1e1 * x1 + t4, 0.2e1);
  t11 = x1 * x2;
  t15 = pow(z + x1 - 0.1e1 * t4 - 0.1e1 * t11 + t11 * z, 0.2e1);
  return(0.1e1 / s * t6 * (-0.1e1 + z) * x3 / t15);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ1c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t7;
  double t8;
  t7 = sin(0.3141592654e1 * x4);
  t8 = t7 * t7;
  t10 = x1 * x1;
  return(-0.1e1 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z) * t8 * t10 * (-0.1e1 + x2) * x3 * (-0.1e1 + x3));
}


/* ==================================================
  CQ2c
*/
struct CQ2c {
  enum {
    Parametrization = Hierarchical, 
    A1 = 0, 
    B1 = -2, 
    A2 = 0, 
    B2 = 0, 
    A3 = -1, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ2c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t11;
  double t13;
  double t16;
  double t18;
  double t20;
  double t23;
  double t26;
  double t3;
  double t33;
  double t36;
  double t37;
  double t4;
  double t41;
  double t42;
  double t47;
  double t5;
  double t55;
  double t56;
  double t67;
  double t68;
  double t89;
  double t96;
  double t98;
  t3 = x1 * z;
  t4 = -0.1e1 * z - 0.1e1 * x1 + t3;
  t5 = t4 * t4;
  t10 = pow(-0.1e1 + z, 0.2e1);
  t11 = t10 * t10;
  t13 = x1 * x1;
  t16 = x1 * x2;
  t18 = t16 * z;
  t20 = pow(z + x1 - 0.1e1 * t3 - 0.1e1 * t16 + t18, 0.2e1);
  t23 = x3 * x2;
  t26 = z * z;
  t33 = t13 * t26;
  t36 = x2 * x2;
  t37 = t13 * t36;
  t41 = x1 * t36;
  t42 = t26 * x3;
  t47 = cos(0.3141592654e1 * x4);
  t55 = sqrt(-0.1e1 * x3 * (-0.1e1 + x2) * t4 * x2 * (-0.1e1 + x3));
  t56 = t47 * t55;
  t67 = 0.2e1 * t23 * t3 - 0.3e1 * t23 * x1 * t26 - 0.4e1 * t23 * t13 * z + 0.2e1 * t33 * t23 + 0.2e1 * t37 * x3 * z + t41 * t42 - 0.1e1 * t37 * t42 - 0.2e1 * t3 * t56 - 0.2e1 * t16 * t56 - 0.3e1 * t18 + t23 * z + t23 * x1 + 0.3e1 * t16 * t26 + t23 * t26;
  t68 = t13 * x2;
  t89 = z * t47 * t55;
  t96 = 0.4e1 * t68 * z + 0.2e1 * t23 * t13 - 0.2e1 * t33 * x2 + t41 * z - 0.2e1 * t37 * z - 0.1e1 * t37 * x3 - 0.1e1 * t41 * x3 - 0.1e1 * t41 * t26 + t37 * t26 + 0.2e1 * x1 * t47 * t55 + 0.2e1 * t89 + 0.2e1 * t16 * t89 - 0.1e1 * x2 * t26 - 0.2e1 * t68 + t37;
  t98 = pow(t67 + t96, 0.2e1);
  return(-0.1e1 * s / t5 / t4 * t11 * t13 * x1 / t20 * t98);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ2c> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t7;
  double t8;
  t7 = sin(0.3141592654e1 * x4);
  t8 = t7 * t7;
  t10 = x1 * x1;
  return(-0.1000000000e1 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z) * t8 * t10 * x2 * (-0.1e1 + x2) * (-0.1e1 + x3));
}


/* ==================================================
  CQ2d
*/
struct CQ2d {
  enum {
    Parametrization = Hierarchical, 
    A1 = 0, 
    B1 = -2, 
    A2 = 0, 
    B2 = 0, 
    A3 = -1, 
    B3 = 0
  };
};


/** \brief (gen. with maple) */
template<>
inline double G<CQ2d> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t11;
  double t14;
  double t18;
  double t21;
  double t3;
  double t35;
  double t4;
  double t43;
  double t47;
  double t8;
  double t9;
  t3 = x1 * z;
  t4 = -0.1e1 * z - 0.1e1 * x1 + t3;
  t8 = pow(-0.1e1 + z, 0.2e1);
  t9 = t8 * t8;
  t11 = x1 * x1;
  t14 = x1 * x2;
  t18 = pow(z + x1 - 0.1e1 * t3 - 0.1e1 * t14 + t14 * z, 0.2e1);
  t21 = x3 * x1;
  t35 = cos(0.3141592654e1 * x4);
  t43 = sqrt(-0.1e1 * x3 * (-0.1e1 + x2) * t4 * x2 * (-0.1e1 + x3));
  t47 = pow(t21 * z - 0.2e1 * t21 * x2 * z - 0.1e1 * t21 + 0.2e1 * t21 * x2 - 0.1e1 * x3 * z + 0.2e1 * x3 * x2 * z + 0.2e1 * t35 * t43, 0.2e1);
  return(-0.1e1 * s / t4 * t9 * t11 * x1 / t18 * t47);
}


/** \brief (gen. with maple) */
template<>
inline double F<CQ2d> (
  double x1,
  double x2,
  double x3,
  double x4,
  double z,
  double s)
{
  double t10;
  double t2;
  double t3;
  t2 = sin(0.3141592654e1 * x4);
  t3 = t2 * t2;
  t10 = x1 * x1;
  return(-0.1000000000e1 * t3 / (-0.1e1 * z - 0.1e1 * x1 + x1 * z) * t10 * x2 * (-0.1e1 + x2) * (-0.1e1 + x3));
}

