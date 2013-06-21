/* Topology 1: {S134, S234, S34} (gen. with maple) */
inline void MbbarggH_1_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t14;
  double t16;
  double t18;
  double t20;
  double t42;
  double t5;
  t5 = mh * mh;
  t14 = S34 * S34;
  t16 = t5 * t5;
  t18 = S24 * S24;
  t20 = S14 * S14;
  t42 = -0.384e3 * S24 * S34 - 0.384e3 * S14 * S34 - 0.1792e4 / 0.3e1 * t5 * S34 - 0.192e3 * t5 * S24 - 0.192e3 * t5 * S14 - 0.384e3 * S24 * S14 - 0.928e3 / 0.3e1 * t14 - 0.768e3 * t16 - 0.192e3 * t18 - 0.192e3 * t20 + 0.192e3 * S134 * S24 + 0.192e3 * S234 * S14 + 0.96e2 * S234 * S24 + 0.384e3 * S234 * t5 - 0.96e2 * S234 * S23 + 0.384e3 * S134 * t5 + 0.96e2 * S134 * S14 - 0.96e2 * S134 * S13 + 0.240e3 * S134 * S34 + 0.240e3 * S234 * S34;
  cgret[0] = t42;
}
inline void MbbarggH_1_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t3;
  double t36;
  double t5;
  double t7;
  t1 = S34 * S34;
  t3 = S24 * S24;
  t5 = S14 * S14;
  t7 = mh * mh;
  t36 = 0.928e3 / 0.3e1 * t1 + 0.192e3 * t3 + 0.192e3 * t5 + 0.640e3 / 0.3e1 * t7 * S34 + 0.192e3 * t7 * S24 + 0.384e3 * S24 * S34 + 0.384e3 * S14 * S34 + 0.192e3 * t7 * S14 + 0.384e3 * S24 * S14 - 0.96e2 * S134 * S14 - 0.832e3 / 0.3e1 * S234 * S34 - 0.832e3 / 0.3e1 * S134 * S34 - 0.192e3 * S234 * S14 - 0.96e2 * S234 * S24 + 0.96e2 * S234 * S23 - 0.192e3 * S134 * S24 + 0.96e2 * S134 * S13;
  cgret[0] = t36;
}
inline void MbbarggH_1_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t3;
  t1 = S34 * S34;
  t3 = mh * mh;
  cgret[0] = 0.192e3 * t1 + 0.192e3 * t3 * S34 - 0.96e2 * S234 * S34 - 0.96e2 * S134 * S34;
}
inline void MbbarggH_1_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 2: {S13, S14, S23, S24} (gen. with maple) */
inline void MbbarggH_2_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t100;
  double t101;
  double t134;
  double t29;
  double t30;
  double t33;
  double t36;
  double t4;
  double t65;
  double t7;
  double t71;
  double t74;
  double t77;
  double t82;
  double t91;
  t1 = S23 * S23;
  t4 = S13 * S13;
  t7 = S24 * S24;
  t10 = S14 * S14;
  t29 = mh * mh;
  t30 = t29 * t29;
  t33 = S34 * S34;
  t36 = 0.32e2 * S13 * t1 + 0.32e2 * t4 * S23 + 0.32e2 * t7 * S14 + 0.32e2 * S24 * t10 + 0.112e3 * t10 * S23 + 0.112e3 * S14 * t1 + 0.16e2 * S14 * t4 + 0.16e2 * t10 * S13 + 0.16e2 * S24 * t1 + 0.16e2 * t7 * S23 + 0.112e3 * t7 * S13 + 0.112e3 * S24 * t4 + 0.64e2 / 0.3e1 * S24 * t30 + 0.16e2 * S24 * t33;
  t65 = 0.16e2 * t7 * S34 - 0.16e2 * t29 * t7 - 0.16e2 * t29 * t10 + 0.64e2 / 0.3e1 * S14 * t30 + 0.16e2 * t10 * S34 + 0.16e2 * S14 * t33 + 0.16e2 * t4 * S34 - 0.16e2 * t29 * t4 + 0.16e2 * S13 * t33 + 0.64e2 / 0.3e1 * S13 * t30 + 0.16e2 * S23 * t33 - 0.16e2 * t29 * t1 + 0.64e2 / 0.3e1 * S23 * t30 + 0.16e2 * t1 * S34;
  t71 = S14 * S13;
  t74 = S24 * S13;
  t77 = S24 * S14;
  t82 = S13 * S23;
  t91 = S14 * S23;
  t100 = -0.16e2 * t29 * t33 + 0.64e2 / 0.3e1 * S34 * t30 + 0.224e3 / 0.3e1 * t71 * S23 + 0.224e3 / 0.3e1 * t74 * S23 + 0.128e3 * t77 * S23 + 0.128e3 * t74 * S14 + 0.32e2 * t82 * S34 - 0.64e2 * t82 * t29 - 0.64e2 * t77 * t29 + 0.32e2 * t77 * S34 + 0.80e2 * t91 * S34 - 0.256e3 / 0.3e1 * t91 * t29 + 0.32e2 * t71 * S34 - 0.32e2 * t71 * t29;
  t101 = S24 * S23;
  t134 = 0.32e2 * t101 * S34 - 0.32e2 * t101 * t29 - 0.256e3 / 0.3e1 * t74 * t29 + 0.80e2 * t74 * S34 - 0.32e2 * S24 * t29 * S34 - 0.32e2 * S14 * t29 * S34 - 0.32e2 * S13 * t29 * S34 - 0.32e2 * S23 * t29 * S34 - 0.32e2 / 0.3e1 * t30 * t29 + 0.16e2 / 0.3e1 * t7 * S24 + 0.16e2 / 0.3e1 * t10 * S14 + 0.16e2 / 0.3e1 * t4 * S13 + 0.16e2 / 0.3e1 * t1 * S23 + 0.16e2 / 0.3e1 * t33 * S34;
  cgret[0] = t36 + t65 + t100 + t134;
}
inline void MbbarggH_2_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t12;
  double t121;
  double t15;
  double t22;
  double t31;
  double t32;
  double t4;
  double t53;
  double t56;
  double t59;
  double t62;
  double t65;
  double t68;
  double t7;
  double t94;
  t1 = S14 * S13;
  t4 = S24 * S13;
  t7 = S24 * S14;
  t12 = S13 * S23;
  t15 = mh * mh;
  t22 = S14 * S23;
  t31 = -0.224e3 / 0.3e1 * t1 * S23 - 0.224e3 / 0.3e1 * S23 * t4 - 0.128e3 * t7 * S23 - 0.128e3 * S14 * t4 - 0.32e2 * t12 * S34 + 0.64e2 / 0.3e1 * t12 * t15 + 0.64e2 / 0.3e1 * t7 * t15 - 0.32e2 * t7 * S34 - 0.256e3 / 0.3e1 * t22 * S34 + 0.176e3 / 0.3e1 * t22 * t15 - 0.80e2 / 0.3e1 * t1 * S34 + 0.16e2 / 0.3e1 * t1 * t15;
  t32 = S24 * S23;
  t53 = S24 * S24;
  t56 = S14 * S14;
  t59 = S13 * S13;
  t62 = S23 * S23;
  t65 = S34 * S34;
  t68 = -0.80e2 / 0.3e1 * S34 * t32 + 0.16e2 / 0.3e1 * t15 * t32 + 0.176e3 / 0.3e1 * t4 * t15 - 0.256e3 / 0.3e1 * t4 * S34 + 0.32e2 / 0.3e1 * S24 * t15 * S34 + 0.32e2 / 0.3e1 * S14 * t15 * S34 + 0.32e2 / 0.3e1 * S13 * t15 * S34 + 0.32e2 / 0.3e1 * S23 * t15 * S34 - 0.16e2 / 0.3e1 * t53 * S24 - 0.16e2 / 0.3e1 * S14 * t56 - 0.16e2 / 0.3e1 * t59 * S13 - 0.16e2 / 0.3e1 * t62 * S23 - 0.16e2 / 0.3e1 * t65 * S34;
  t94 = -0.32e2 * t53 * S14 - 0.32e2 * S24 * t56 - 0.352e3 / 0.3e1 * t56 * S23 - 0.352e3 / 0.3e1 * t62 * S14 - 0.32e2 / 0.3e1 * S14 * t59 - 0.32e2 / 0.3e1 * t56 * S13 - 0.32e2 / 0.3e1 * t62 * S24 - 0.32e2 / 0.3e1 * t53 * S23 - 0.352e3 / 0.3e1 * t53 * S13 - 0.352e3 / 0.3e1 * S24 * t59 - 0.16e2 * S24 * t65 - 0.16e2 * t53 * S34;
  t121 = 0.16e2 / 0.3e1 * t15 * t53 + 0.16e2 / 0.3e1 * t15 * t56 - 0.16e2 * t56 * S34 - 0.16e2 * S14 * t65 - 0.16e2 * t59 * S34 + 0.16e2 / 0.3e1 * t15 * t59 - 0.16e2 * S13 * t65 - 0.16e2 * t65 * S23 + 0.16e2 / 0.3e1 * t15 * t62 - 0.16e2 * t62 * S34 + 0.16e2 / 0.3e1 * t15 * t65 - 0.32e2 * t62 * S13 - 0.32e2 * t59 * S23;
  cgret[0] = t31 + t68 + t94 + t121;
}
inline void MbbarggH_2_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t10;
  double t13;
  double t18;
  double t39;
  double t4;
  double t42;
  double t45;
  double t46;
  double t49;
  double t56;
  double t7;
  double t81;
  t4 = S24 * S13;
  t7 = S14 * S23;
  t10 = mh * mh;
  t13 = S14 * S13;
  t18 = S24 * S23;
  t39 = S34 * S34;
  t42 = S14 * S14;
  t45 = -0.48e2 * S24 * S14 * S23 - 0.48e2 * t4 * S14 - 0.176e3 / 0.3e1 * t7 * S34 + 0.128e3 / 0.3e1 * t7 * t10 - 0.16e2 * t13 * S34 + 0.16e2 / 0.3e1 * t13 * t10 - 0.16e2 * t18 * S34 + 0.16e2 / 0.3e1 * t18 * t10 + 0.128e3 / 0.3e1 * t4 * t10 - 0.176e3 / 0.3e1 * t4 * S34 + 0.16e2 / 0.3e1 * S24 * t10 * S34 + 0.16e2 / 0.3e1 * S14 * t10 * S34 + 0.16e2 / 0.3e1 * S13 * t10 * S34 + 0.16e2 / 0.3e1 * S23 * t10 * S34 - 0.16e2 / 0.3e1 * t39 * S34 - 0.48e2 * t42 * S23;
  t46 = S23 * S23;
  t49 = S13 * S13;
  t56 = S24 * S24;
  t81 = -0.48e2 * S14 * t46 - 0.16e2 / 0.3e1 * t49 * S14 - 0.16e2 / 0.3e1 * t42 * S13 - 0.16e2 / 0.3e1 * S24 * t46 - 0.16e2 / 0.3e1 * t56 * S23 - 0.48e2 * t56 * S13 - 0.48e2 * t49 * S24 - 0.32e2 / 0.3e1 * S24 * t39 - 0.16e2 / 0.3e1 * t56 * S34 - 0.16e2 / 0.3e1 * t42 * S34 - 0.32e2 / 0.3e1 * S14 * t39 - 0.16e2 / 0.3e1 * t49 * S34 - 0.32e2 / 0.3e1 * S13 * t39 - 0.32e2 / 0.3e1 * S23 * t39 - 0.16e2 / 0.3e1 * t46 * S34 + 0.16e2 / 0.3e1 * t10 * t39;
  cgret[0] = t45 + t81;
}
inline void MbbarggH_2_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 3: {S13, S134, S234, S24} (gen. with maple) */
inline void MbbarggH_3_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t11;
  double t22;
  double t29;
  double t4;
  double t43;
  double t44;
  double t5;
  double t51;
  double t58;
  double t67;
  double t72;
  double t8;
  double t81;
  double t82;
  t1 = S14 * S14;
  t4 = mh * mh;
  t5 = t4 * t4;
  t8 = S34 * S34;
  t11 = S24 * S24;
  t22 = S13 * S13;
  t29 = S14 * S13;
  t43 = -0.48e2 * S13 * t1 - 0.192e3 * S24 * t5 - 0.16e2 / 0.3e1 * S24 * t8 + 0.16e2 / 0.3e1 * t11 * S34 + 0.144e3 * t4 * t11 - 0.176e3 / 0.3e1 * S13 * t8 - 0.192e3 * S13 * t5 - 0.16e2 / 0.3e1 * S134 * t11 - 0.48e2 * S134 * t22 - 0.256e3 / 0.3e1 * S234 * t5 - 0.256e3 / 0.3e1 * S134 * t5 - 0.304e3 / 0.3e1 * t29 * S34 - 0.144e3 * t4 * t29 - 0.16e2 * S24 * t4 * S34 - 0.160e3 * S13 * t4 * S34 + 0.128e3 / 0.3e1 * S234 * S24 * t4;
  t44 = S134 * S24;
  t51 = S134 * S13;
  t58 = S13 * S234;
  t67 = S234 * t4;
  t72 = S134 * t4;
  t81 = 0.112e3 / 0.3e1 * S14 * t44 + 0.320e3 / 0.3e1 * t44 * t4 + 0.16e2 / 0.3e1 * S34 * t44 + 0.144e3 * t51 * t4 + 0.160e3 / 0.3e1 * t51 * S34 + 0.48e2 * t51 * S14 + 0.160e3 * t58 * t4 + 0.176e3 / 0.3e1 * t58 * S34 - 0.176e3 / 0.3e1 * t58 * S23 + 0.304e3 / 0.3e1 * S14 * t58 + 0.128e3 / 0.3e1 * t67 * S23 - 0.128e3 / 0.3e1 * S14 * t67 + 0.128e3 / 0.3e1 * t72 * S34 + 0.128e3 / 0.3e1 * t72 * S14 + 0.256e3 / 0.3e1 * t5 * t4 - 0.48e2 * S24 * t11;
  t82 = t43 + t81;
  cgret[0] = t82;
  cgret[1] = t82;
}
inline void MbbarggH_3_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t17;
  double t22;
  double t33;
  double t36;
  double t39;
  double t4;
  double t44;
  double t51;
  double t60;
  double t65;
  double t7;
  double t74;
  double t75;
  t1 = S14 * S14;
  t4 = S34 * S34;
  t7 = S24 * S24;
  t10 = mh * mh;
  t17 = S13 * S13;
  t22 = S14 * S13;
  t33 = S234 * S24;
  t36 = S134 * S24;
  t39 = 0.48e2 * S13 * t1 + 0.16e2 / 0.3e1 * S24 * t4 - 0.16e2 / 0.3e1 * t7 * S34 - 0.48e2 * t7 * t10 + 0.176e3 / 0.3e1 * S13 * t4 + 0.16e2 / 0.3e1 * S134 * t7 + 0.128e3 / 0.3e1 * S134 * t17 - 0.16e2 / 0.3e1 * S234 * t7 + 0.304e3 / 0.3e1 * t22 * S34 + 0.48e2 * t10 * t22 + 0.16e2 / 0.3e1 * S24 * t10 * S34 + 0.160e3 / 0.3e1 * S13 * t10 * S34 + 0.16e2 / 0.3e1 * t33 * t10 - 0.80e2 * t36 * S14;
  t44 = S134 * S13;
  t51 = S13 * S234;
  t60 = S234 * t10;
  t65 = S134 * t10;
  t74 = -0.144e3 * t36 * t10 - 0.16e2 / 0.3e1 * t36 * S34 + 0.128e3 / 0.3e1 * t44 * t10 - 0.160e3 / 0.3e1 * t44 * S34 - 0.16e2 / 0.3e1 * t44 * S14 - 0.176e3 / 0.3e1 * t51 * t10 - 0.176e3 / 0.3e1 * t51 * S34 + 0.64e2 * t51 * S23 - 0.304e3 / 0.3e1 * S14 * t51 - 0.128e3 / 0.3e1 * t60 * S23 + 0.256e3 / 0.3e1 * S14 * t60 - 0.256e3 / 0.3e1 * t65 * S34 - 0.128e3 / 0.3e1 * t65 * S14 + 0.128e3 / 0.3e1 * S23 * t33 + 0.48e2 * S24 * t7;
  t75 = t39 + t74;
  cgret[0] = t75;
  cgret[1] = t75;
}
inline void MbbarggH_3_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t11;
  double t14;
  double t19;
  double t24;
  double t29;
  double t32;
  double t37;
  double t4;
  double t56;
  double t59;
  double t60;
  t4 = mh * mh;
  t11 = S24 * S24;
  t14 = S34 * S34;
  t19 = S234 * S24;
  t24 = S134 * S24;
  t29 = S134 * S13;
  t32 = 0.48e2 * S14 * S13 * S34 + 0.48e2 * S24 * t4 * S34 + 0.48e2 * S13 * t4 * S34 - 0.48e2 * t11 * S34 + 0.16e2 / 0.3e1 * S24 * t14 + 0.160e3 / 0.3e1 * t14 * S13 - 0.128e3 / 0.3e1 * t19 * S23 + 0.112e3 / 0.3e1 * t19 * t4 + 0.48e2 * S14 * t24 - 0.16e2 / 0.3e1 * t24 * S34 - 0.48e2 * t29 * S34;
  t37 = S13 * S234;
  t56 = S13 * S13;
  t59 = -0.128e3 / 0.3e1 * t29 * S14 - 0.16e2 / 0.3e1 * t4 * t29 - 0.48e2 * S14 * t37 - 0.160e3 / 0.3e1 * S34 * t37 - 0.128e3 / 0.3e1 * t37 * t4 + 0.48e2 * S23 * t37 - 0.128e3 / 0.3e1 * S234 * t4 * S14 + 0.128e3 / 0.3e1 * S134 * t4 * S34 + 0.16e2 / 0.3e1 * S234 * t11 + 0.48e2 * S134 * t11 + 0.16e2 / 0.3e1 * S134 * t56;
  t60 = t32 + t59;
  cgret[0] = t60;
  cgret[1] = t60;
}
inline void MbbarggH_3_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 4: {S13, S134, S23, S234} (gen. with maple) */
inline void MbbarggH_4_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t18;
  double t2;
  double t21;
  double t26;
  double t29;
  double t42;
  double t5;
  t1 = mh * mh;
  t2 = t1 * t1;
  t5 = S34 * S34;
  t18 = S24 * S24;
  t21 = S14 * S14;
  t26 = S234 * S24;
  t29 = S134 * t1;
  t42 = -0.32e2 / 0.3e1 * t2 * t1 - 0.16e2 / 0.3e1 * t5 * S34 - 0.16e2 * t1 * t5 - 0.64e2 / 0.3e1 * t2 * S34 + 0.64e2 / 0.3e1 * S234 * t2 + 0.64e2 / 0.3e1 * S134 * t2 + 0.16e2 / 0.3e1 * S234 * t5 + 0.16e2 / 0.3e1 * S234 * t18 + 0.16e2 / 0.3e1 * S134 * t21 + 0.16e2 / 0.3e1 * S134 * t5 - 0.16e2 * t26 * t1 + 0.16e2 * t29 * S34 - 0.16e2 * t29 * S14 - 0.16e2 / 0.3e1 * t26 * S34 + 0.16e2 * S234 * t1 * S34 - 0.16e2 / 0.3e1 * S134 * S14 * S34;
  cgret[0] = t42;
  cgret[2] = t42;
}
inline void MbbarggH_4_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t14;
  double t2;
  double t21;
  double t22;
  double t5;
  double t7;
  t1 = mh * mh;
  t2 = S34 * S34;
  t5 = S234 * S24;
  t7 = S134 * t1;
  t10 = S24 * S24;
  t14 = S14 * S14;
  t21 = t1 * t2 + t2 * S34 + t1 * t5 - t7 * S34 + S14 * t7 - S234 * t10 - S234 * t2 - S134 * t2 - S134 * t14 + S34 * t5 - S234 * t1 * S34 + S134 * S14 * S34;
  t22 = 0.16e2 / 0.3e1 * t21;
  cgret[0] = t22;
  cgret[2] = t22;
}
inline void MbbarggH_4_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t16;
  double t2;
  t1 = mh * mh;
  t2 = S34 * S34;
  t16 = 0.16e2 / 0.3e1 * t2 * t1 + 0.16e2 / 0.3e1 * t2 * S34 - 0.16e2 / 0.3e1 * S134 * t1 * S34 - 0.16e2 / 0.3e1 * S234 * t2 - 0.16e2 / 0.3e1 * S134 * t2 + 0.16e2 / 0.3e1 * S234 * S24 * S34 - 0.16e2 / 0.3e1 * S234 * t1 * S34 + 0.16e2 / 0.3e1 * S134 * S14 * S34;
  cgret[0] = t16;
  cgret[2] = t16;
}
inline void MbbarggH_4_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 5: {S13, S24, S34} (gen. with maple) */
inline void MbbarggH_5_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t16;
  double t20;
  double t22;
  double t26;
  double t3;
  double t30;
  t1 = S13 * S13;
  t3 = mh * mh;
  t16 = S24 * S24;
  t20 = t3 * t3;
  t22 = S23 * S23;
  t26 = S14 * S14;
  t30 = 0.96e2 * t1 - 0.192e3 * t3 * S13 + 0.48e2 * S14 * S13 + 0.192e3 * S13 * S23 + 0.96e2 * S24 * S14 + 0.96e2 * S24 * S23 - 0.96e2 * t3 * S24 + 0.48e2 * t16 - 0.96e2 * t3 * S23 + 0.96e2 * t20 + 0.48e2 * t22 + 0.96e2 * S14 * S23 + 0.48e2 * t26 - 0.96e2 * t3 * S14;
  cgret[0] = t30;
  cgret[1] = t30;
}
inline void MbbarggH_5_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t13;
  double t15;
  double t19;
  double t5;
  double t7;
  t5 = S13 * S13;
  t7 = S24 * S24;
  t13 = S23 * S23;
  t15 = S14 * S14;
  t19 = -0.48e2 * S14 * S13 - 0.192e3 * S13 * S23 - 0.96e2 * t5 - 0.48e2 * t7 - 0.96e2 * S24 * S23 - 0.48e2 * S24 * S14 - 0.48e2 * t13 - 0.48e2 * t15 - 0.144e3 * S14 * S23;
  cgret[0] = t19;
  cgret[1] = t19;
}
inline void MbbarggH_5_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarggH_5_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 6: {S134, S23, S34} (gen. with maple) */
inline void MbbarggH_6_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t14;
  double t15;
  double t2;
  double t21;
  double t3;
  double t4;
  double t5;
  double t6;
  double t9;
  t1 = mh * mh;
  t2 = t1 * t1;
  t3 = 0.96e2 * t2;
  t4 = S24 * S24;
  t5 = 0.48e2 * t4;
  t6 = S14 * S14;
  t9 = 0.96e2 * t1 * S24;
  t14 = t3 + t5 + 0.48e2 * t6 - t9 - 0.96e2 * t1 * S14 + 0.96e2 * S24 * S14;
  t15 = S13 * S13;
  t21 = t3 + t5 + 0.48e2 * t15 - t9 + 0.96e2 * t1 * S13 - 0.96e2 * S24 * S13;
  cgret[0] = t14;
  cgret[1] = t14;
  cgret[2] = t21;
  cgret[3] = t21;
}
inline void MbbarggH_6_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t10;
  double t12;
  double t3;
  double t5;
  double t6;
  double t7;
  t3 = S14 * S14;
  t5 = S24 * S24;
  t6 = 0.48e2 * t5;
  t7 = -0.144e3 * S24 * S14 - 0.48e2 * t3 - t6;
  t10 = S13 * S13;
  t12 = 0.144e3 * S24 * S13 - t6 - 0.48e2 * t10;
  cgret[0] = t7;
  cgret[1] = t7;
  cgret[2] = t12;
  cgret[3] = t12;
}
inline void MbbarggH_6_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarggH_6_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 7: {S34, S234^2, 1/S24} (gen. with maple) */
inline void MbbarggH_7_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.192e3 * t1;
  cgret[0] = t2;
  cgret[1] = t2;
}
inline void MbbarggH_7_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.192e3 * t1;
  cgret[0] = -t2;
  cgret[1] = -t2;
}
inline void MbbarggH_7_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarggH_7_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 8: {S34, S234^2, 1/S34} (gen. with maple) */
inline void MbbarggH_8_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.416e3 / 0.3e1 * t1;
  cgret[0] = t2;
  cgret[1] = t2;
}
inline void MbbarggH_8_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.192e3 * t1;
  cgret[0] = -t2;
  cgret[1] = -t2;
}
inline void MbbarggH_8_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.160e3 / 0.3e1 * t1;
  cgret[0] = t2;
  cgret[1] = t2;
}
inline void MbbarggH_8_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 9: {S23, S234^2, 1/S34} (gen. with maple) */
inline void MbbarggH_9_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.128e3 / 0.3e1 * t1;
  cgret[2] = t2;
  cgret[3] = t2;
}
inline void MbbarggH_9_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.256e3 / 0.3e1 * t1;
  cgret[2] = -t2;
  cgret[3] = -t2;
}
inline void MbbarggH_9_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.128e3 / 0.3e1 * t1;
  cgret[2] = t2;
  cgret[3] = t2;
}
inline void MbbarggH_9_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 10: {S23, S234^2, 1/S24} (gen. with maple) */
inline void MbbarggH_10_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.128e3 / 0.3e1 * t1;
  cgret[0] = -t2;
  cgret[1] = -t2;
}
inline void MbbarggH_10_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.256e3 / 0.3e1 * t1;
  cgret[0] = t2;
  cgret[1] = t2;
}
inline void MbbarggH_10_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.128e3 / 0.3e1 * t1;
  cgret[0] = -t2;
  cgret[1] = -t2;
}
inline void MbbarggH_10_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 11: {S134^2, S234^2, S34^2, 1/(S134*S24-S234*S14)^2} (gen. with maple) */
inline void MbbarggH_11_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  t1 = mh * mh;
  cgret[0] = 0.192e3 * t1;
}
inline void MbbarggH_11_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  t1 = mh * mh;
  cgret[0] = -0.192e3 * t1;
}
inline void MbbarggH_11_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarggH_11_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
