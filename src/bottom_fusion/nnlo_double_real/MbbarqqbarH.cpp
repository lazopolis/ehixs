/* Topology 1: {S134, S234, S34} (gen. with maple) */
inline void MbbarqqbarH_1_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t16;
  double t18;
  double t43;
  double t45;
  double t7;
  t7 = mh * mh;
  t16 = t7 * t7;
  t18 = S13 * S13;
  t43 = S23 * S23;
  t45 = 0.32e2 * S234 * S134 + 0.16e2 * S234 * S34 - 0.16e2 * S34 * S134 - 0.16e2 * S134 * S13 - 0.32e2 * S134 * S23 - 0.32e2 * S134 * t7 + 0.16e2 * S134 * S14 - 0.16e2 * S234 * S23 + 0.16e2 * S234 * S24 - 0.32e2 * S234 * t7 + 0.32e2 * t43;
  cgret[0] = 0.64e2 * S13 * S34 - 0.32e2 * S24 * S34 + 0.32e2 * S23 * S34 + 0.32e2 * t7 * S34 + 0.32e2 * t7 * S13 + 0.32e2 * t7 * S23 + 0.64e2 * S13 * S23 + 0.32e2 * t16 + 0.32e2 * t18 - 0.32e2 * S13 * S234 + t45;
}
inline void MbbarqqbarH_1_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  t1 = mh * mh;
  cgret[0] = 0.32e2 * t1 * S34 - 0.16e2 * S134 * S14 - 0.32e2 * S234 * S134 - 0.16e2 * S34 * S134 - 0.16e2 * S234 * S34 - 0.16e2 * S134 * S13 - 0.16e2 * S234 * S24 - 0.16e2 * S234 * S23;
}
inline void MbbarqqbarH_1_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarqqbarH_1_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 2: {S34, 1/S23, S234^2} (gen. with maple) */
inline void MbbarqqbarH_2_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.32e2 * t1;
  cgret[0] = -t2;
  cgret[1] = -t2;
}
inline void MbbarqqbarH_2_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarqqbarH_2_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarqqbarH_2_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 3: {S34, S234^2, 1/S34} (gen. with maple) */
inline void MbbarqqbarH_3_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.16e2 * t1;
  cgret[0] = -t2;
  cgret[1] = -t2;
}
inline void MbbarqqbarH_3_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.16e2 * t1;
  cgret[0] = t2;
  cgret[1] = t2;
}
inline void MbbarqqbarH_3_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarqqbarH_3_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
