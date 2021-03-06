
namespace GstarGstar{

#include "kinematic_invariants.h"

    
double RR (const KinematicInvariants & kk
  )
{
  const   double t12 = kk.s(1,2);
const     double t31 = kk.q(1,3);
const     double t41 = kk.q(1,4);
const     double t51 = kk.q(1,5);
const     double t23 = kk.q(2,3);
const     double t24 = kk.q(2,4);
const     double t25 = kk.q(2,5);
const     double t34 = kk.q(3,4);
const     double t35 = kk.q(3,5);
const     double t45 = kk.q(4,5);
const     double t3 = kk.q(3);
const     double t4 = kk.q(4);
const     double t5 = kk.q(5);
    const double V=4.0;
  double t1;
  double t10;
  double t1002;
  double t103;
  double t1031;
  double t104;
  double t1064;
  double t1071;
  double t109;
  double t1096;
  double t11;
  double t111;
  double t1127;
  double t1143;
  double t1146;
  double t1149;
  double t1152;
  double t1157;
  double t1166;
  double t1171;
  double t1174;
  double t118;
  double t1199;
  double t1202;
  double t1205;
  double t1210;
  double t1231;
  double t1261;
  double t128;
  double t129;
  double t1290;
  double t13;
  double t132;
  double t139;
  double t14;
  double t143;
  double t144;
  double t147;
  double t148;
  double t151;
  double t156;
  double t157;
  double t158;
  double t159;
  double t162;
  double t167;
  double t168;
  double t17;
  double t171;
  double t18;
  double t183;
  double t187;
  double t19;
  double t194;
  double t198;
  double t199;
  double t2;
  double t20;
  double t200;
  double t203;
  double t204;
  double t207;
  double t208;
  double t21;
  double t210;
  double t211;
  double t215;
  double t218;
  double t219;
  double t22;
  double t222;
  double t225;
  double t230;
  double t231;
  double t232;
  double t236;
  double t240;
  double t243;
  double t244;
  double t247;
  double t251;
  double t254;
  double t257;
  double t26;
  double t260;
  double t264;
  double t265;
  double t266;
  double t269;
  double t272;
  double t275;
  double t278;
  double t279;
  double t280;
  double t284;
  double t285;
  double t288;
  double t289;
  double t290;
  double t291;
  double t294;
  double t297;
  double t30;
  double t300;
  double t303;
  double t307;
  double t308;
  double t311;
  double t312;
  double t315;
  double t316;
  double t317;
  double t319;
  double t32;
  double t321;
  double t323;
  double t325;
  double t326;
  double t328;
  double t33;
  double t332;
  double t336;
  double t338;
  double t341;
  double t343;
  double t348;
  double t355;
  double t36;
  double t365;
  double t37;
  double t38;
  double t383;
  double t384;
  double t402;
  double t438;
  double t44;
  double t442;
  double t46;
  double t466;
  double t469;
  double t47;
  double t472;
  double t475;
  double t478;
  double t48;
  double t480;
  double t489;
  double t49;
  double t50;
  double t505;
  double t509;
  double t52;
  double t525;
  double t529;
  double t533;
  double t537;
  double t541;
  double t55;
  double t56;
  double t560;
  double t564;
  double t568;
  double t57;
  double t572;
  double t576;
  double t579;
  double t583;
  double t587;
  double t588;
  double t591;
  double t6;
  double t603;
  double t625;
  double t629;
  double t63;
  double t64;
  double t651;
  double t659;
  double t673;
  double t675;
  double t68;
  double t684;
  double t69;
  double t695;
  double t698;
  double t7;
  double t70;
  double t705;
  double t708;
  double t71;
  double t711;
  double t714;
  double t715;
  double t72;
  double t744;
  double t757;
  double t76;
  double t765;
  double t77;
  double t78;
  double t786;
  double t787;
  double t79;
  double t792;
  double t795;
  double t8;
  double t804;
  double t807;
  double t810;
  double t825;
  double t828;
  double t832;
  double t86;
  double t860;
  double t868;
  double t87;
  double t872;
  double t877;
  double t88;
  double t9;
  double t901;
  double t91;
  double t917;
  double t92;
  double t920;
  double t93;
  double t938;
  double t941;
  double t947;
  double t96;
  double t962;
  double t97;
  double t971;
  double t973;
  double t98;
  t1 = 0.1e1 / t12;
  t2 = t4 * t1;
  t6 = t2 * t5;
  t7 = t25 * t25;
  t8 = 0.1e1 / t7;
  t9 = t51 * t8;
  t10 = 0.1e1 / t31;
  t11 = 0.1e1 / t41;
  t13 = t10 * t11;
  t14 = t9 * t13;
  t17 = t3 * t1;
  t18 = 0.1e1 / t51;
  t19 = t17 * t18;
  t20 = t23 * t23;
  t21 = 0.1e1 / t20;
  t22 = t21 * t31;
  t26 = t11 * t45;
  t30 = 0.1e1 / t25;
  t32 = t17 * t30;
  t33 = 0.1e1 / t24;
  t36 = t23 * t33;
  t37 = t31 * t31;
  t38 = 0.1e1 / t37;
  t44 = t17 * t5;
  t46 = t51 * t51;
  t47 = 0.1e1 / t46;
  t48 = t47 * t25;
  t49 = 0.1e1 / t23;
  t50 = t49 * t33;
  t52 = t48 * t50;
  t55 = t5 * t1;
  t56 = t55 * t47;
  t57 = t25 * t49;
  t63 = t8 * t10;
  t64 = t11 * t34;
  t68 = t2 * t18;
  t69 = t24 * t24;
  t70 = 0.1e1 / t69;
  t71 = t70 * t10;
  t72 = t41 * t35;
  t76 = t2 * t30;
  t77 = t49 * t24;
  t78 = t41 * t41;
  t79 = 0.1e1 / t78;
  t86 = t18 * t70;
  t87 = t10 * t41;
  t88 = t86 * t87;
  t91 = t18 * t21;
  t92 = t31 * t11;
  t93 = t91 * t92;
  t96 = t30 * t23;
  t97 = t33 * t38;
  t98 = t96 * t97;
  t103 = -0.32e2 * t6 * t14 + 0.16e2 * t19 * t22 * t26 + 0.16e2 * t32 * t36 * t38 * t45 - 0.32e2 * t44 * t52 + 0.16e2 * t56 * t57 * t33 * t34 + 0.16e2 * t55 * t51 * t63 * t64 + 0.16e2 * t68 * t71 * t72 + 0.16e2 * t76 * t77 * t79 * t35 - 0.32e2 * t6 * t52 - 0.32e2 * t6 * t88 - 0.32e2 * t44 * t93 - 0.32e2 * t44 * t98 - 0.32e2 * t44 * t14;
  t104 = t17 * t4;
  t109 = t30 * t49;
  t111 = t109 * t24 * t79;
  t118 = t47 * t21;
  t128 = t18 * t49;
  t129 = t128 * t33;
  t132 = t128 * t11;
  t139 = t8 * t23;
  t143 = t30 * t33;
  t144 = t143 * t10;
  t147 = t30 * t10;
  t148 = t147 * t11;
  t151 = 0.8e1 * t35 * t38 * t44 * t8 + 0.8e1 * t118 * t31 * t44 + 0.8e1 * t118 * t35 * t44 + 0.8e1 * t139 * t38 * t44 + 0.8e1 * t21 * t44 * t48 - 0.32e2 * t104 * t111 - 0.32e2 * t104 * t88 - 0.32e2 * t104 * t93 - 0.32e2 * t104 * t98 - 0.32e2 * t111 * t6 - 0.32e2 * t129 * t44 - 0.32e2 * t132 * t44 - 0.32e2 * t144 * t44 - 0.32e2 * t148 * t44;
  t156 = t5 * t5;
  t157 = t17 * t156;
  t158 = t47 * t49;
  t159 = t158 * t33;
  t162 = t63 * t11;
  t167 = t18 * t33;
  t168 = t167 * t10;
  t171 = t109 * t11;
  t183 = t21 * t24;
  t187 = t70 * t38;
  t194 = t23 * t70;
  t198 = t4 * t4;
  t199 = t17 * t198;
  t200 = t86 * t10;
  t203 = 0.8e1 * t44 * t9 * t38 + 0.32e2 * t157 * t159 + 0.32e2 * t157 * t162 - 0.32e2 * t104 * t132 - 0.32e2 * t104 * t168 - 0.32e2 * t104 * t171 - 0.32e2 * t104 * t144 + 0.8e1 * t104 * t21 * t79 * t34 + 0.8e1 * t104 * t22 * t79 + 0.8e1 * t104 * t183 * t79 + 0.8e1 * t104 * t187 * t34 + 0.8e1 * t104 * t187 * t41 + 0.8e1 * t104 * t194 * t38 + 0.32e2 * t199 * t200;
  t204 = t109 * t79;
  t207 = t3 * t3;
  t208 = t207 * t1;
  t210 = t21 * t11;
  t211 = t210 * t45;
  t215 = t97 * t45;
  t218 = t208 * t5;
  t219 = t91 * t11;
  t222 = t143 * t38;
  t225 = t208 * t4;
  t230 = t1 * t18;
  t231 = t230 * t30;
  t232 = t49 * t31;
  t236 = t11 * t35;
  t240 = t77 * t92;
  t243 = t33 * t10;
  t244 = t243 * t72;
  t247 = t10 * t45;
  t251 = t36 * t87;
  t254 = t230 * t49;
  t257 = -0.16e2 * t18 * t208 * t211 - 0.16e2 * t208 * t215 * t30 - 0.8e1 * t231 * t232 * t26 - 0.8e1 * t231 * t236 * t77 - 0.8e1 * t231 * t247 * t36 + 0.32e2 * t199 * t204 + 0.32e2 * t218 * t219 + 0.32e2 * t218 * t222 + 0.32e2 * t219 * t225 + 0.32e2 * t222 * t225 - 0.8e1 * t231 * t240 - 0.8e1 * t231 * t244 - 0.8e1 * t231 * t251 - 0.8e1 * t244 * t254;
  t260 = t33 * t31;
  t264 = t230 * t25;
  t265 = t10 * t34;
  t266 = t50 * t265;
  t269 = t50 * t87;
  t272 = t50 * t64;
  t275 = t50 * t92;
  t278 = t1 * t30;
  t279 = t278 * t49;
  t280 = t24 * t10;
  t284 = t278 * t23;
  t285 = t243 * t26;
  t288 = t51 * t1;
  t289 = t288 * t30;
  t290 = t49 * t10;
  t291 = t290 * t64;
  t294 = t77 * t13;
  t297 = t243 * t64;
  t300 = t36 * t13;
  t303 = t55 * t18;
  t307 = t33 * t11;
  t308 = t57 * t307;
  t311 = t17 * t51;
  t312 = t143 * t13;
  t315 = -0.8e1 * t109 * t303 * t64 - 0.8e1 * t236 * t279 * t280 - 0.8e1 * t254 * t26 * t260 + 0.16e2 * t19 * t308 - 0.8e1 * t264 * t266 - 0.8e1 * t264 * t269 - 0.8e1 * t264 * t272 - 0.8e1 * t264 * t275 - 0.8e1 * t284 * t285 - 0.8e1 * t289 * t291 - 0.8e1 * t289 * t294 - 0.8e1 * t289 * t297 - 0.8e1 * t289 * t300 + 0.16e2 * t311 * t312;
  t316 = t2 * t51;
  t317 = t109 * t13;
  t319 = t109 * t92;
  t321 = t96 * t243;
  t323 = t57 * t243;
  t325 = t24 * t11;
  t326 = t109 * t325;
  t328 = t143 * t87;
  t332 = t55 * t30;
  t336 = t49 * t1;
  t338 = -t1 * t11 + t19 * t326 + t19 * t328 + t269 * t303 + t275 * t303 + t294 * t32 + t294 * t332 + t300 * t332 + t316 * t317 + t319 * t68 + t321 * t68 + t323 * t68 - t230 - t336;
  t341 = t33 * t1;
  t343 = t1 * t10;
  t348 = t230 * t33;
  t355 = t278 * t33;
  t365 = 0.16e2 * t10 * t230 + 0.32e2 * t10 * t278 - 0.16e2 * t10 * t336 + 0.16e2 * t11 * t230 + 0.32e2 * t11 * t278 + 0.16e2 * t33 * t336 - 0.16e2 * t231 + 0.32e2 * t254 - 0.16e2 * t278 + 0.16e2 * t279 - 0.16e2 * t341 - 0.16e2 * t343 + 0.32e2 * t348 + 0.16e2 * t355;
  t383 = t18 * t30;
  t384 = t49 * t11;
  t402 = -0.32e2 * t104 * t243 * t383 - 0.32e2 * t104 * t383 * t384 + 0.8e1 * t109 * t19 * t26 - 0.32e2 * t128 * t243 * t6 - 0.32e2 * t128 * t307 * t44 + 0.8e1 * t143 * t19 * t247 + 0.32e2 * t10 * t341 + 0.32e2 * t11 * t336 - 0.16e2 * t11 * t341 + 0.16e2 * t11 * t343 + 0.8e1 * t285 * t32 + 0.16e2 * t311 * t317 - 0.32e2 * t312 * t44 - 0.32e2 * t317 * t6;
  t438 = -0.8e1 * t143 * t265 * t303 - 0.8e1 * t19 * t247 * t50 + 0.8e1 * t19 * t26 * t50 - 0.8e1 * t26 * t290 * t32 + 0.16e2 * t19 * t269 + 0.16e2 * t19 * t323 + 0.8e1 * t266 * t303 + 0.8e1 * t272 * t303 + 0.8e1 * t291 * t332 + 0.8e1 * t297 * t332 + 0.16e2 * t303 * t319 + 0.16e2 * t303 * t321 + 0.16e2 * t303 * t326 + 0.16e2 * t303 * t328;
  t442 = t10 * t35;
  t466 = t384 * t34;
  t469 = t232 * t11;
  t472 = t243 * t45;
  t475 = t243 * t34;
  t478 = 0.8e1 * t109 * t236 * t68 + 0.8e1 * t143 * t442 * t68 - 0.8e1 * t236 * t243 * t76 + 0.8e1 * t236 * t290 * t76 - 0.8e1 * t236 * t50 * t68 + 0.8e1 * t442 * t50 * t68 + 0.8e1 * t230 * t466 + 0.16e2 * t230 * t469 + 0.8e1 * t230 * t472 + 0.8e1 * t230 * t475 + 0.16e2 * t275 * t68 + 0.16e2 * t300 * t76 + 0.16e2 * t308 * t68 + 0.16e2 * t312 * t316;
  t480 = t243 * t41;
  t489 = t47 * t33;
  t505 = t8 * t11;
  t509 = t8 * t24;
  t525 = -0.8e1 * t10 * t139 * t55 + 0.16e2 * t10 * t55 * t9 - 0.8e1 * t11 * t509 * t55 + 0.16e2 * t11 * t55 * t9 - 0.8e1 * t158 * t31 * t55 - 0.8e1 * t158 * t35 * t55 - 0.8e1 * t2 * t45 * t86 + 0.16e2 * t33 * t48 * t55 - 0.8e1 * t35 * t55 * t63 - 0.8e1 * t41 * t489 * t55 - 0.8e1 * t45 * t489 * t55 - 0.8e1 * t45 * t505 * t55 + 0.16e2 * t48 * t49 * t55 + 0.16e2 * t230 * t480;
  t529 = t18 * t25;
  t533 = t30 * t79;
  t537 = t30 * t24;
  t541 = t49 * t79;
  t560 = t5 * t47;
  t564 = t5 * t18;
  t568 = t5 * t8;
  t572 = t5 * t30;
  t576 = -0.8e1 * t10 * t194 * t2 + 0.16e2 * t11 * t2 * t568 - 0.8e1 * t2 * t232 * t79 + 0.16e2 * t2 * t33 * t560 - 0.8e1 * t2 * t34 * t541 - 0.8e1 * t2 * t34 * t71 + 0.16e2 * t2 * t41 * t71 + 0.16e2 * t2 * t41 * t86 - 0.8e1 * t2 * t45 * t533 - 0.8e1 * t2 * t529 * t70 + 0.16e2 * t2 * t537 * t79 + 0.16e2 * t2 * t564 * t70 + 0.16e2 * t2 * t572 * t79 + 0.16e2 * t2 * t77 * t79;
  t579 = t156 * t47;
  t583 = t156 * t8;
  t587 = t198 * t1;
  t588 = t560 * t70;
  t591 = t568 * t79;
  t603 = t30 * t38;
  t625 = -0.8e1 * t11 * t17 * t183 + 0.16e2 * t11 * t17 * t22 - 0.8e1 * t17 * t21 * t529 - 0.8e1 * t17 * t210 * t34 + 0.16e2 * t17 * t31 * t91 - 0.8e1 * t17 * t34 * t97 - 0.8e1 * t17 * t35 * t603 - 0.8e1 * t17 * t35 * t91 + 0.16e2 * t17 * t38 * t96 - 0.8e1 * t17 * t41 * t97 - 0.16e2 * t2 * t579 * t70 - 0.16e2 * t2 * t583 * t79 - 0.16e2 * t587 * t588 - 0.16e2 * t587 * t591;
  t629 = t51 * t30;
  t651 = t4 * t21;
  t659 = t4 * t70;
  t673 = 0.16e2 * t17 * t36 * t38 - 0.8e1 * t17 * t629 * t38 + 0.16e2 * t17 * t560 * t49 + 0.16e2 * t17 * t564 * t21 + 0.16e2 * t17 * t568 * t10 + 0.16e2 * t17 * t572 * t38 - 0.16e2 * t17 * t579 * t21 - 0.16e2 * t17 * t583 * t38 + 0.16e2 * t17 * t651 * t11 + 0.16e2 * t17 * t4 * t49 * t79 + 0.16e2 * t17 * t659 * t10 + 0.16e2 * t17 * t4 * t33 * t38 + 0.8e1 * t230 * t50 * t45 + 0.8e1 * t230 * t50 * t35;
  t675 = t384 * t35;
  t684 = t55 * t383;
  t695 = t71 * t35;
  t698 = t541 * t35;
  t705 = t243 * t11;
  t708 = t50 * t10;
  t711 = t290 * t11;
  t714 = 0.8e1 * t17 * t269 * t529 + 0.8e1 * t17 * t294 * t629 + 0.8e1 * t2 * t275 * t529 + 0.8e1 * t2 * t300 * t629 + 0.32e2 * t144 * t303 + 0.32e2 * t171 * t303 + 0.32e2 * t19 * t708 + 0.8e1 * t230 * t675 + 0.8e1 * t240 * t684 + 0.8e1 * t251 * t684 + 0.32e2 * t32 * t711 + 0.16e2 * t68 * t695 + 0.16e2 * t698 * t76 + 0.32e2 * t705 * t76;
  t715 = t50 * t11;
  t744 = -0.32e2 * t104 * t200 - 0.32e2 * t104 * t204 - 0.32e2 * t104 * t219 - 0.32e2 * t104 * t222 - 0.32e2 * t159 * t44 - 0.32e2 * t159 * t6 - 0.32e2 * t162 * t44 - 0.32e2 * t162 * t6 + 0.16e2 * t19 * t211 - 0.32e2 * t200 * t6 - 0.32e2 * t204 * t6 - 0.32e2 * t219 * t44 - 0.32e2 * t222 * t44 + 0.32e2 * t68 * t715;
  t757 = t50 * t34;
  t765 = t13 * t34;
  t786 = -0.8e1 * t10 * t303 * t96 - 0.8e1 * t109 * t303 * t31 - 0.8e1 * t11 * t303 * t537 - 0.8e1 * t143 * t303 * t41 - 0.16e2 * t254 * t45 * t92 - 0.16e2 * t279 * t325 * t35 - 0.16e2 * t348 * t35 * t87 + 0.16e2 * t215 * t32 - 0.16e2 * t264 * t757 - 0.16e2 * t284 * t472 - 0.16e2 * t289 * t765 + 0.24e2 * t303 * t469 + 0.24e2 * t303 * t480 + 0.16e2 * t303 * t757;
  t787 = t77 * t11;
  t792 = t36 * t10;
  t795 = t156 * t1;
  t804 = t243 * t35;
  t807 = t57 * t33;
  t810 = t25 * t33;
  t825 = t143 * t11;
  t828 = 0.24e2 * t332 * t787 + 0.16e2 * t332 * t765 + 0.24e2 * t332 * t792 - 0.16e2 * t795 * t47 * t757 - 0.16e2 * t795 * t8 * t765 + 0.24e2 * t68 * t469 + 0.16e2 * t68 * t804 + 0.24e2 * t68 * t807 - 0.8e1 * t68 * t810 * t11 + 0.16e2 * t76 * t675 + 0.24e2 * t76 * t792 - 0.8e1 * t2 * t49 * t260 * t11 - 0.8e1 * t2 * t23 * t705 - 0.8e1 * t316 * t825;
  t832 = t47 * t70;
  t860 = t156 * t2;
  t868 = 0.24e2 * t316 * t148 + 0.8e1 * t6 * t832 * t45 + 0.8e1 * t6 * t832 * t41 + 0.8e1 * t6 * t48 * t70 - 0.32e2 * t6 * t129 - 0.32e2 * t6 * t168 + 0.8e1 * t6 * t8 * t79 * t45 + 0.8e1 * t6 * t509 * t79 - 0.32e2 * t6 * t171 - 0.32e2 * t6 * t148 + 0.8e1 * t6 * t9 * t79 + 0.32e2 * t860 * t159 + 0.32e2 * t860 * t162 - 0.16e2 * t587 * t18 * t695;
  t872 = t587 * t5;
  t877 = t384 * t45;
  t901 = -0.8e1 * t10 * t19 * t57 - 0.16e2 * t30 * t587 * t698 + 0.24e2 * t19 * t480 + 0.24e2 * t19 * t807 + 0.16e2 * t19 * t877 + 0.32e2 * t200 * t872 + 0.32e2 * t204 * t872 + 0.8e1 * t231 * t466 - 0.8e1 * t231 * t472 + 0.8e1 * t231 * t475 - 0.8e1 * t231 * t675 - 0.8e1 * t231 * t804 - 0.8e1 * t231 * t877 + 0.8e1 * t254 * t472;
  t917 = t13 * t45;
  t920 = t13 * t35;
  t938 = -0.8e1 * t254 * t307 * t34 + 0.8e1 * t254 * t307 * t35 - 0.8e1 * t254 * t307 * t45 + 0.16e2 * t55 * t765 * t8 - 0.8e1 * t254 * t475 - 0.8e1 * t254 * t804 - 0.8e1 * t279 * t765 + 0.8e1 * t279 * t917 - 0.8e1 * t279 * t920 + 0.24e2 * t32 * t787 - 0.8e1 * t355 * t765 - 0.8e1 * t355 * t917 + 0.8e1 * t355 * t920 + 0.16e2 * t56 * t757;
  t941 = t17 * t49;
  t947 = t109 * t10;
  t962 = t18 * t11;
  t971 = -0.8e1 * t11 * t280 * t941 + 0.8e1 * t109 * t55 + 0.8e1 * t13 * t17 + 0.8e1 * t143 * t55 + 0.8e1 * t147 * t55 + 0.24e2 * t148 * t311 - 0.8e1 * t208 * t210 - 0.8e1 * t208 * t603 - 0.8e1 * t208 * t91 - 0.8e1 * t208 * t97 - 0.8e1 * t311 * t947 + 0.16e2 * t32 * t472 - 0.8e1 * t480 * t941 + 0.8e1 * t55 * t962;
  t973 = t30 * t11;
  t1002 = 0.8e1 * t13 * t2 + 0.8e1 * t143 * t2 - 0.8e1 * t158 * t795 + 0.8e1 * t167 * t2 + 0.8e1 * t243 * t2 + 0.16e2 * t2 * t307 + 0.8e1 * t2 * t384 + 0.8e1 * t2 * t50 + 0.8e1 * t2 * t962 + 0.8e1 * t2 * t973 - 0.8e1 * t489 * t795 - 0.8e1 * t505 * t795 + 0.8e1 * t55 * t973 - 0.8e1 * t63 * t795;
  t1031 = 0.16e2 * t109 * t230 + 0.16e2 * t13 * t336 + 0.16e2 * t13 * t341 + 0.16e2 * t143 * t230 + 0.16e2 * t147 * t230 + 0.16e2 * t158 * t55 + 0.16e2 * t230 * t290 + 0.16e2 * t230 * t307 + 0.16e2 * t230 * t973 + 0.16e2 * t243 * t336 + 0.16e2 * t278 * t290 + 0.16e2 * t278 * t307 + 0.16e2 * t307 * t336 - 0.8e1 * t587 * t86;
  t1064 = 0.8e1 * t230 * t35 * t49 + 0.16e2 * t17 * t210 + 0.16e2 * t17 * t603 + 0.16e2 * t17 * t91 + 0.16e2 * t17 * t97 + 0.16e2 * t2 * t533 + 0.16e2 * t2 * t541 + 0.16e2 * t2 * t71 + 0.16e2 * t2 * t86 - 0.8e1 * t230 * t232 + 0.16e2 * t489 * t55 + 0.16e2 * t505 * t55 - 0.8e1 * t533 * t587 + 0.16e2 * t55 * t63;
  t1071 = t18 * t10;
  t1096 = -0.8e1 * t230 * t33 * t41 + 0.8e1 * t230 * t33 * t45 + 0.8e1 * t1071 * t17 + 0.8e1 * t109 * t17 + 0.8e1 * t128 * t17 + 0.8e1 * t147 * t17 + 0.8e1 * t17 * t243 + 0.16e2 * t17 * t290 + 0.8e1 * t17 * t384 + 0.8e1 * t17 * t50 - 0.8e1 * t230 * t57 - 0.8e1 * t230 * t810 - 0.8e1 * t541 * t587 - 0.8e1 * t587 * t71;
  t1127 = -0.8e1 * t1 * t23 * t243 - 0.8e1 * t10 * t23 * t278 + 0.8e1 * t128 * t55 - 0.8e1 * t147 * t288 + 0.8e1 * t26 * t278 + 0.8e1 * t265 * t341 - 0.8e1 * t278 * t325 + 0.8e1 * t278 * t442 - 0.8e1 * t288 * t973 - 0.8e1 * t325 * t336 + 0.8e1 * t336 * t64 - 0.8e1 * t336 * t92 - 0.8e1 * t341 * t87 + 0.16e2 * t684;
  t1143 = t560 * t21;
  t1146 = t568 * t38;
  t1149 = t651 * t79;
  t1152 = t659 * t38;
  t1157 = t167 * t11;
  t1166 = 0.8e1 * t55 * t167 + 0.8e1 * t55 * t1071 - 0.8e1 * t2 * t629 * t79 - 0.16e2 * t17 * t198 * t21 * t79 - 0.16e2 * t17 * t198 * t70 * t38 - 0.16e2 * t208 * t1143 - 0.16e2 * t208 * t1146 - 0.16e2 * t208 * t1149 - 0.16e2 * t208 * t1152 - 0.16e2 * t55 * t168 + 0.8e1 * t55 * t1157 + 0.8e1 * t55 * t947 - 0.16e2 * t55 * t171 - 0.16e2 * t55 * t144;
  t1171 = t383 * t33;
  t1174 = t383 * t11;
  t1199 = -0.8e1 * t1157 * t2 + 0.8e1 * t1171 * t2 + 0.8e1 * t1174 * t2 - 0.16e2 * t129 * t2 - 0.16e2 * t132 * t2 - 0.16e2 * t144 * t2 - 0.16e2 * t148 * t2 + 0.8e1 * t2 * t588 - 0.8e1 * t2 * t705 + 0.8e1 * t2 * t708 + 0.8e1 * t2 * t711 - 0.8e1 * t2 * t715 - 0.8e1 * t2 * t825 + 0.8e1 * t55 * t825;
  t1202 = t383 * t49;
  t1205 = t383 * t10;
  t1210 = t128 * t10;
  t1231 = 0.8e1 * t1143 * t17 + 0.8e1 * t1202 * t17 + 0.8e1 * t1205 * t17 - 0.8e1 * t1210 * t17 - 0.16e2 * t129 * t17 - 0.16e2 * t148 * t17 - 0.16e2 * t168 * t17 - 0.16e2 * t17 * t171 + 0.8e1 * t17 * t705 - 0.8e1 * t17 * t708 - 0.8e1 * t17 * t711 + 0.8e1 * t17 * t715 - 0.8e1 * t17 * t947 + 0.8e1 * t2 * t591;
  t1261 = 0.8e1 * t1146 * t17 + 0.8e1 * t1149 * t17 + 0.8e1 * t1152 * t17 - 0.32e2 * t129 * t55 - 0.32e2 * t132 * t17 - 0.32e2 * t144 * t17 - 0.32e2 * t148 * t55 - 0.32e2 * t168 * t2 - 0.32e2 * t171 * t2 + 0.16e2 * t230 * t807 + 0.8e1 * t278 * t466 + 0.16e2 * t278 * t787 + 0.8e1 * t278 * t804 + 0.8e1 * t278 * t877;
  t1290 = -0.8e1 * t1171 * t55 - 0.8e1 * t1174 * t55 - 0.8e1 * t1202 * t55 - 0.8e1 * t1205 * t55 + 0.8e1 * t1210 * t55 - 0.16e2 * t132 * t55 + 0.16e2 * t148 * t288 + 0.16e2 * t230 * t825 + 0.16e2 * t230 * t947 + 0.8e1 * t278 * t475 + 0.16e2 * t278 * t792 + 0.8e1 * t278 * t917 + 0.8e1 * t278 * t920 + 0.16e2 * t336 * t705;
  return(V * (t257 + t1002 + t103 + t1031 + t1064 + t1096 + t1166 + t1199 + t1231 + t1290 + t1261 + t151 + t1127 + t525 + t673 + t315 + t478 + t714 + t744 + t901 + t203 + t938 + t971 + t786 + t828 + t868 + 0.16e2 * t338 + t365 + t402 + t438 + t576 + t625));
}
double RRi (
  double V,
  double t12,
  double t31,
  double t41,
  double t51,
  double t23,
  double t24,
  double t25,
  double t34,
  double t35,
  double t45,
  double t3,
  double t4,
  double t5)
{
  double t1;
  double t10;
  double t100;
  double t1004;
  double t1023;
  double t103;
  double t104;
  double t105;
  double t1070;
  double t1071;
  double t108;
  double t1089;
  double t11;
  double t1106;
  double t1114;
  double t1140;
  double t115;
  double t116;
  double t1169;
  double t117;
  double t1192;
  double t120;
  double t1200;
  double t1203;
  double t121;
  double t1232;
  double t124;
  double t125;
  double t126;
  double t1262;
  double t127;
  double t1290;
  double t13;
  double t130;
  double t131;
  double t132;
  double t133;
  double t136;
  double t14;
  double t141;
  double t142;
  double t144;
  double t147;
  double t155;
  double t156;
  double t157;
  double t161;
  double t168;
  double t17;
  double t174;
  double t178;
  double t179;
  double t18;
  double t183;
  double t188;
  double t189;
  double t19;
  double t190;
  double t193;
  double t194;
  double t197;
  double t2;
  double t20;
  double t202;
  double t21;
  double t212;
  double t22;
  double t225;
  double t228;
  double t233;
  double t236;
  double t245;
  double t251;
  double t252;
  double t26;
  double t267;
  double t279;
  double t285;
  double t286;
  double t287;
  double t29;
  double t294;
  double t297;
  double t30;
  double t303;
  double t306;
  double t309;
  double t313;
  double t32;
  double t322;
  double t324;
  double t325;
  double t329;
  double t33;
  double t332;
  double t333;
  double t336;
  double t339;
  double t342;
  double t345;
  double t348;
  double t351;
  double t354;
  double t357;
  double t36;
  double t360;
  double t366;
  double t37;
  double t370;
  double t371;
  double t373;
  double t375;
  double t377;
  double t378;
  double t38;
  double t380;
  double t382;
  double t384;
  double t387;
  double t389;
  double t39;
  double t395;
  double t402;
  double t403;
  double t406;
  double t409;
  double t412;
  double t415;
  double t43;
  double t433;
  double t436;
  double t439;
  double t44;
  double t442;
  double t449;
  double t455;
  double t46;
  double t462;
  double t468;
  double t469;
  double t47;
  double t472;
  double t475;
  double t476;
  double t48;
  double t483;
  double t485;
  double t490;
  double t493;
  double t508;
  double t513;
  double t52;
  double t523;
  double t526;
  double t527;
  double t528;
  double t53;
  double t535;
  double t546;
  double t56;
  double t564;
  double t568;
  double t57;
  double t578;
  double t598;
  double t6;
  double t602;
  double t609;
  double t61;
  double t610;
  double t614;
  double t615;
  double t620;
  double t621;
  double t628;
  double t633;
  double t64;
  double t640;
  double t647;
  double t65;
  double t652;
  double t66;
  double t667;
  double t673;
  double t678;
  double t69;
  double t690;
  double t7;
  double t707;
  double t714;
  double t717;
  double t72;
  double t724;
  double t727;
  double t73;
  double t730;
  double t733;
  double t740;
  double t751;
  double t756;
  double t76;
  double t77;
  double t78;
  double t787;
  double t8;
  double t81;
  double t815;
  double t819;
  double t838;
  double t84;
  double t842;
  double t85;
  double t864;
  double t872;
  double t873;
  double t876;
  double t877;
  double t880;
  double t89;
  double t9;
  double t90;
  double t901;
  double t906;
  double t909;
  double t912;
  double t913;
  double t916;
  double t917;
  double t936;
  double t964;
  double t971;
  double t978;
  double t979;
  double t983;
  double t987;
  t1 = 0.1e1 / t12;
  t2 = t4 * t1;
  t6 = t2 * t5;
  t7 = t51 * t51;
  t8 = 0.1e1 / t7;
  t9 = t8 * t25;
  t10 = 0.1e1 / t23;
  t11 = 0.1e1 / t24;
  t13 = t10 * t11;
  t14 = t9 * t13;
  t17 = 0.1e1 / t51;
  t18 = t24 * t24;
  t19 = 0.1e1 / t18;
  t20 = t17 * t19;
  t21 = 0.1e1 / t31;
  t22 = t21 * t41;
  t26 = t20 * t22;
  t29 = t3 * t1;
  t30 = t29 * t5;
  t32 = t23 * t23;
  t33 = 0.1e1 / t32;
  t36 = t17 * t33;
  t37 = 0.1e1 / t41;
  t38 = t31 * t37;
  t39 = t36 * t38;
  t43 = t5 * t1;
  t44 = t43 * t17;
  t46 = 0.1e1 / t25;
  t47 = t46 * t10;
  t48 = t47 * t38;
  t52 = t24 * t37;
  t53 = t47 * t52;
  t56 = t46 * t11;
  t57 = t21 * t34;
  t61 = t56 * t22;
  t64 = t46 * t23;
  t65 = t11 * t21;
  t66 = t64 * t65;
  t69 = t13 * t57;
  t72 = t37 * t34;
  t73 = t13 * t72;
  t76 = t43 * t46;
  t77 = t10 * t21;
  t78 = t77 * t72;
  t81 = t65 * t72;
  t84 = t2 * t17;
  t85 = t37 * t35;
  t89 = -0.8e1 * t44 * t56 * t57 + 0.8e1 * t47 * t84 * t85 - 0.32e2 * t14 * t6 - 0.32e2 * t26 * t6 - 0.32e2 * t30 * t39 + 0.16e2 * t44 * t48 + 0.16e2 * t44 * t53 + 0.16e2 * t44 * t61 + 0.16e2 * t44 * t66 + 0.8e1 * t44 * t69 + 0.8e1 * t44 * t73 + 0.8e1 * t76 * t78 + 0.8e1 * t76 * t81;
  t90 = t21 * t35;
  t100 = t13 * t38;
  t103 = t25 * t10;
  t104 = t11 * t37;
  t105 = t104 * t103;
  t108 = t2 * t46;
  t115 = t23 * t11;
  t116 = t21 * t37;
  t117 = t115 * t116;
  t120 = t2 * t51;
  t121 = t56 * t116;
  t124 = t31 * t31;
  t125 = 0.1e1 / t124;
  t126 = t11 * t125;
  t127 = t64 * t126;
  t130 = t25 * t25;
  t131 = 0.1e1 / t130;
  t132 = t51 * t131;
  t133 = t132 * t116;
  t136 = t29 * t4;
  t141 = t41 * t41;
  t142 = 0.1e1 / t141;
  t144 = t47 * t24 * t142;
  t147 = -0.8e1 * t108 * t65 * t85 + 0.8e1 * t108 * t77 * t85 - 0.8e1 * t13 * t84 * t85 + 0.8e1 * t13 * t84 * t90 + 0.8e1 * t56 * t84 * t90 + 0.16e2 * t100 * t84 + 0.16e2 * t105 * t84 + 0.16e2 * t108 * t117 + 0.16e2 * t120 * t121 - 0.32e2 * t127 * t30 - 0.32e2 * t133 * t30 - 0.32e2 * t136 * t144 - 0.32e2 * t136 * t26 - 0.32e2 * t136 * t39;
  t155 = t29 * t17;
  t156 = t33 * t31;
  t157 = t37 * t45;
  t161 = t29 * t46;
  t168 = t43 * t8;
  t174 = t131 * t21;
  t178 = t19 * t21;
  t179 = t41 * t35;
  t183 = t10 * t24;
  t188 = t1 * t46;
  t189 = t188 * t23;
  t190 = t65 * t157;
  t193 = t1 * t51;
  t194 = t193 * t46;
  t197 = t183 * t116;
  t202 = -0.32e2 * t136 * t127 - 0.32e2 * t6 * t144 - 0.32e2 * t6 * t133 + 0.16e2 * t155 * t156 * t157 + 0.16e2 * t161 * t115 * t125 * t45 - 0.32e2 * t30 * t14 + 0.16e2 * t168 * t103 * t11 * t34 + 0.16e2 * t43 * t51 * t174 * t72 + 0.16e2 * t84 * t178 * t179 + 0.16e2 * t108 * t183 * t142 * t35 - 0.8e1 * t189 * t190 - 0.8e1 * t194 * t78 - 0.8e1 * t194 * t197 - 0.8e1 * t194 * t81;
  t212 = t13 * t22;
  t225 = t103 * t65;
  t228 = t47 * t116;
  t233 = t29 * t51;
  t236 = -0.8e1 * t44 * t47 * t72 + 0.16e2 * t100 * t44 + 0.16e2 * t105 * t155 - 0.8e1 * t117 * t194 + 0.16e2 * t117 * t76 + 0.16e2 * t120 * t228 + 0.16e2 * t121 * t233 + 0.16e2 * t155 * t53 + 0.16e2 * t155 * t61 + 0.16e2 * t197 * t76 + 0.16e2 * t212 * t44 + 0.16e2 * t225 * t84 + 0.16e2 * t48 * t84 + 0.16e2 * t66 * t84;
  t245 = t17 * t10;
  t251 = t17 * t46;
  t252 = t10 * t37;
  t267 = t21 * t45;
  t279 = -0.32e2 * t104 * t245 * t30 + 0.8e1 * t13 * t155 * t157 - 0.8e1 * t13 * t155 * t267 - 0.32e2 * t136 * t251 * t252 - 0.32e2 * t136 * t251 * t65 + 0.8e1 * t155 * t157 * t47 + 0.8e1 * t155 * t267 * t56 - 0.32e2 * t245 * t6 * t65 - 0.32e2 * t121 * t30 + 0.16e2 * t155 * t212 + 0.8e1 * t161 * t190 + 0.16e2 * t161 * t197 + 0.16e2 * t228 * t233 - 0.32e2 * t228 * t6;
  t285 = t1 * t17;
  t286 = t285 * t46;
  t287 = t10 * t31;
  t294 = t183 * t38;
  t297 = t65 * t179;
  t303 = t115 * t22;
  t306 = t285 * t10;
  t309 = t11 * t31;
  t313 = t285 * t25;
  t322 = -0.8e1 * t115 * t267 * t286 - 0.8e1 * t157 * t161 * t77 - 0.8e1 * t157 * t286 * t287 - 0.8e1 * t157 * t306 * t309 - 0.8e1 * t183 * t286 * t85 - 0.8e1 * t100 * t313 + 0.16e2 * t155 * t225 - 0.8e1 * t212 * t313 - 0.8e1 * t286 * t294 - 0.8e1 * t286 * t297 - 0.8e1 * t286 * t303 - 0.8e1 * t297 * t306 - 0.8e1 * t313 * t69 - 0.8e1 * t313 * t73;
  t324 = t188 * t10;
  t325 = t24 * t21;
  t329 = t178 * t35;
  t332 = t10 * t142;
  t333 = t332 * t35;
  t336 = t47 * t37;
  t339 = t56 * t21;
  t342 = t13 * t37;
  t345 = t252 * t45;
  t348 = t252 * t35;
  t351 = t252 * t34;
  t354 = t65 * t41;
  t357 = t183 * t37;
  t360 = t116 * t34;
  t366 = t285 * t11;
  t370 = -0.16e2 * t22 * t35 * t366 - 0.16e2 * t306 * t38 * t45 - 0.8e1 * t324 * t325 * t85 + 0.16e2 * t108 * t333 - 0.8e1 * t286 * t345 - 0.8e1 * t286 * t348 + 0.8e1 * t286 * t351 + 0.16e2 * t329 * t84 + 0.32e2 * t336 * t44 + 0.32e2 * t339 * t44 + 0.32e2 * t342 * t84 + 0.24e2 * t354 * t44 + 0.24e2 * t357 * t76 + 0.16e2 * t360 * t76;
  t371 = t65 * t37;
  t373 = t13 * t21;
  t375 = t77 * t37;
  t377 = t8 * t10;
  t378 = t377 * t11;
  t380 = t36 * t37;
  t382 = t174 * t37;
  t384 = t56 * t125;
  t387 = t20 * t21;
  t389 = t47 * t142;
  t395 = t108 * t371 - t136 * t380 - t136 * t384 - t136 * t387 - t136 * t389 + t155 * t373 + t161 * t375 - t30 * t378 - t30 * t380 - t30 * t382 - t30 * t384 - t378 * t6 - t382 * t6 - t387 * t6;
  t402 = t33 * t37;
  t403 = t402 * t45;
  t406 = t126 * t45;
  t409 = t65 * t45;
  t412 = t65 * t35;
  t415 = t65 * t34;
  t433 = t116 * t45;
  t436 = t116 * t35;
  t439 = -0.8e1 * t104 * t306 * t34 + 0.8e1 * t104 * t306 * t35 - 0.8e1 * t104 * t306 * t45 + 0.16e2 * t155 * t403 + 0.16e2 * t161 * t406 - 0.8e1 * t286 * t409 - 0.8e1 * t286 * t412 + 0.8e1 * t286 * t415 + 0.8e1 * t306 * t409 - 0.8e1 * t306 * t412 - 0.8e1 * t306 * t415 + 0.8e1 * t324 * t433 - 0.8e1 * t324 * t436 - 0.32e2 * t389 * t6;
  t442 = t188 * t11;
  t449 = t13 * t34;
  t455 = t245 * t37;
  t462 = t131 * t23;
  t468 = t46 * t21;
  t469 = t468 * t37;
  t472 = t115 * t21;
  t475 = t5 * t5;
  t476 = t475 * t1;
  t483 = 0.8e1 * t125 * t131 * t30 * t35 + 0.8e1 * t125 * t30 * t462 + 0.16e2 * t131 * t360 * t43 - 0.16e2 * t131 * t360 * t476 - 0.16e2 * t449 * t476 * t8 + 0.16e2 * t168 * t449 - 0.32e2 * t30 * t339 - 0.32e2 * t30 * t455 - 0.32e2 * t30 * t469 - 0.8e1 * t324 * t360 - 0.8e1 * t360 * t442 - 0.8e1 * t433 * t442 + 0.8e1 * t436 * t442 + 0.24e2 * t472 * t76;
  t485 = t287 * t37;
  t490 = t103 * t11;
  t493 = t25 * t11;
  t508 = t56 * t37;
  t513 = t8 * t19;
  t523 = t245 * t11;
  t526 = 0.24e2 * t84 * t485 + 0.16e2 * t84 * t412 + 0.24e2 * t84 * t490 - 0.8e1 * t84 * t493 * t37 + 0.16e2 * t108 * t348 + 0.24e2 * t108 * t472 - 0.8e1 * t2 * t10 * t309 * t37 - 0.8e1 * t2 * t23 * t371 - 0.8e1 * t120 * t508 + 0.24e2 * t120 * t469 + 0.8e1 * t6 * t513 * t45 + 0.8e1 * t6 * t513 * t41 + 0.8e1 * t6 * t9 * t19 - 0.32e2 * t6 * t523;
  t527 = t17 * t11;
  t528 = t527 * t21;
  t535 = t131 * t24;
  t546 = t2 * t475;
  t564 = t46 * t24;
  t568 = -0.32e2 * t6 * t528 + 0.8e1 * t6 * t131 * t142 * t45 + 0.8e1 * t6 * t535 * t142 - 0.32e2 * t6 * t336 - 0.32e2 * t6 * t469 + 0.8e1 * t6 * t132 * t142 + 0.32e2 * t546 * t378 - 0.16e2 * t313 * t449 - 0.16e2 * t324 * t52 * t35 - 0.16e2 * t189 * t409 - 0.16e2 * t194 * t360 - 0.8e1 * t44 * t47 * t31 - 0.8e1 * t44 * t56 * t41 - 0.8e1 * t44 * t564 * t37;
  t578 = t29 * t475;
  t598 = t33 * t24;
  t602 = t19 * t125;
  t609 = 0.8e1 * t136 * t142 * t33 * t34 + 0.8e1 * t136 * t142 * t156 + 0.8e1 * t136 * t142 * t598 + 0.8e1 * t136 * t34 * t602 + 0.8e1 * t136 * t41 * t602 - 0.8e1 * t21 * t44 * t64 - 0.32e2 * t136 * t336 - 0.32e2 * t136 * t339 - 0.32e2 * t136 * t455 - 0.32e2 * t136 * t528 + 0.32e2 * t378 * t578 + 0.32e2 * t382 * t578 + 0.16e2 * t44 * t449 + 0.24e2 * t44 * t485;
  t610 = t23 * t19;
  t614 = t4 * t4;
  t615 = t29 * t614;
  t620 = t3 * t3;
  t621 = t620 * t1;
  t628 = t621 * t5;
  t633 = t621 * t4;
  t640 = t614 * t1;
  t647 = t640 * t5;
  t652 = 0.8e1 * t125 * t136 * t610 - 0.16e2 * t17 * t329 * t640 - 0.16e2 * t17 * t403 * t621 - 0.16e2 * t333 * t46 * t640 - 0.16e2 * t406 * t46 * t621 + 0.32e2 * t380 * t628 + 0.32e2 * t380 * t633 + 0.32e2 * t382 * t546 + 0.32e2 * t384 * t628 + 0.32e2 * t384 * t633 + 0.32e2 * t387 * t615 + 0.32e2 * t387 * t647 + 0.32e2 * t389 * t615 + 0.32e2 * t389 * t647;
  t667 = t29 * t10;
  t673 = t47 * t21;
  t678 = t8 * t33;
  t690 = -0.8e1 * t103 * t155 * t21 + 0.8e1 * t30 * t31 * t678 + 0.8e1 * t30 * t33 * t9 + 0.8e1 * t30 * t35 * t678 - 0.8e1 * t325 * t37 * t667 + 0.16e2 * t155 * t345 + 0.24e2 * t155 * t354 + 0.24e2 * t155 * t490 + 0.24e2 * t161 * t357 + 0.16e2 * t161 * t409 + 0.24e2 * t233 * t469 - 0.8e1 * t233 * t673 - 0.32e2 * t30 * t523 - 0.8e1 * t354 * t667;
  t707 = t1 * t10;
  t714 = t1 * t11;
  t717 = 0.8e1 * t125 * t132 * t30 + 0.16e2 * t11 * t707 + 0.32e2 * t188 * t21 + 0.32e2 * t188 * t37 + 0.16e2 * t21 * t285 - 0.16e2 * t21 * t707 + 0.32e2 * t21 * t714 + 0.16e2 * t285 * t37 + 0.32e2 * t37 * t707 - 0.16e2 * t286 + 0.32e2 * t306 + 0.16e2 * t324 + 0.32e2 * t366 + 0.16e2 * t442;
  t724 = t1 * t21;
  t727 = t251 * t21;
  t730 = t251 * t37;
  t733 = t245 * t21;
  t740 = t527 * t37;
  t751 = t251 * t11;
  t756 = 0.8e1 * t2 * t730 + 0.8e1 * t2 * t751 - 0.16e2 * t336 * t43 - 0.16e2 * t339 * t43 - 0.16e2 * t37 * t714 + 0.16e2 * t37 * t724 - 0.16e2 * t43 * t455 + 0.8e1 * t43 * t508 - 0.16e2 * t43 * t528 + 0.8e1 * t43 * t673 - 0.8e1 * t43 * t727 - 0.8e1 * t43 * t730 + 0.8e1 * t43 * t733 + 0.8e1 * t43 * t740;
  t787 = 0.8e1 * t13 * t285 * t35 + 0.8e1 * t13 * t285 * t45 - 0.16e2 * t2 * t339 - 0.8e1 * t2 * t342 + 0.8e1 * t2 * t373 + 0.8e1 * t2 * t375 - 0.16e2 * t2 * t455 - 0.16e2 * t2 * t469 - 0.8e1 * t2 * t508 - 0.16e2 * t2 * t523 - 0.8e1 * t2 * t740 + 0.8e1 * t285 * t348 + 0.8e1 * t285 * t351 + 0.16e2 * t285 * t485;
  t815 = t131 * t37;
  t819 = -0.8e1 * t43 * t45 * t815 + 0.8e1 * t188 * t345 + 0.8e1 * t188 * t351 + 0.16e2 * t188 * t357 + 0.8e1 * t188 * t412 + 0.8e1 * t188 * t415 + 0.8e1 * t188 * t433 + 0.8e1 * t188 * t436 + 0.16e2 * t188 * t472 + 0.16e2 * t193 * t469 + 0.16e2 * t285 * t354 + 0.8e1 * t285 * t409 + 0.8e1 * t285 * t415 + 0.16e2 * t285 * t490;
  t838 = t17 * t25;
  t842 = t46 * t142;
  t864 = 0.16e2 * t132 * t21 * t43 + 0.16e2 * t132 * t37 * t43 + 0.16e2 * t142 * t183 * t2 - 0.8e1 * t142 * t2 * t287 + 0.16e2 * t142 * t2 * t564 - 0.8e1 * t178 * t2 * t34 + 0.16e2 * t178 * t2 * t41 - 0.8e1 * t19 * t2 * t838 + 0.16e2 * t2 * t20 * t41 - 0.8e1 * t2 * t20 * t45 - 0.8e1 * t2 * t332 * t34 - 0.8e1 * t2 * t45 * t842 - 0.8e1 * t21 * t43 * t462 - 0.8e1 * t37 * t43 * t535;
  t872 = t5 * t8;
  t873 = t872 * t19;
  t876 = t5 * t131;
  t877 = t876 * t142;
  t880 = t251 * t10;
  t901 = -0.8e1 * t2 * t21 * t610 - 0.8e1 * t2 * t371 + 0.8e1 * t2 * t873 + 0.8e1 * t2 * t877 - 0.16e2 * t29 * t336 + 0.8e1 * t29 * t342 - 0.8e1 * t29 * t373 - 0.16e2 * t29 * t469 - 0.16e2 * t29 * t523 - 0.16e2 * t29 * t528 - 0.8e1 * t29 * t673 + 0.8e1 * t29 * t727 - 0.8e1 * t29 * t733 + 0.8e1 * t29 * t880;
  t906 = t872 * t33;
  t909 = t876 * t125;
  t912 = t4 * t33;
  t913 = t912 * t142;
  t916 = t4 * t19;
  t917 = t916 * t125;
  t936 = -0.32e2 * t2 * t336 - 0.32e2 * t2 * t528 + 0.16e2 * t285 * t508 + 0.16e2 * t285 * t673 - 0.32e2 * t29 * t339 + 0.8e1 * t29 * t371 - 0.8e1 * t29 * t375 - 0.32e2 * t29 * t455 + 0.8e1 * t29 * t906 + 0.8e1 * t29 * t909 + 0.8e1 * t29 * t913 + 0.8e1 * t29 * t917 - 0.32e2 * t43 * t469 - 0.32e2 * t43 * t523;
  t964 = t51 * t46;
  t971 = t5 * t17;
  t978 = 0.16e2 * t707 * t371 - 0.8e1 * t43 * t880 - 0.8e1 * t43 * t751 + 0.16e2 * t29 * t4 * t11 * t125 - 0.16e2 * t29 * t614 * t33 * t142 - 0.16e2 * t29 * t614 * t19 * t125 - 0.16e2 * t621 * t906 - 0.16e2 * t621 * t909 - 0.16e2 * t621 * t913 - 0.16e2 * t621 * t917 - 0.8e1 * t2 * t964 * t142 + 0.16e2 * t2 * t872 * t11 + 0.16e2 * t2 * t971 * t19 + 0.16e2 * t2 * t876 * t37;
  t979 = t5 * t46;
  t983 = t475 * t8;
  t987 = t475 * t131;
  t1004 = t46 * t125;
  t1023 = -0.8e1 * t1004 * t29 * t35 + 0.16e2 * t125 * t29 * t64 - 0.8e1 * t126 * t29 * t34 + 0.16e2 * t142 * t2 * t979 - 0.16e2 * t142 * t2 * t987 + 0.16e2 * t156 * t29 * t37 - 0.16e2 * t19 * t2 * t983 + 0.16e2 * t29 * t31 * t36 - 0.8e1 * t29 * t33 * t838 - 0.8e1 * t29 * t34 * t402 - 0.8e1 * t29 * t35 * t36 - 0.8e1 * t29 * t37 * t598 - 0.16e2 * t640 * t873 - 0.16e2 * t640 * t877;
  t1070 = -0.8e1 * t29 * t126 * t41 + 0.16e2 * t29 * t115 * t125 - 0.8e1 * t29 * t964 * t125 + 0.16e2 * t29 * t872 * t10 + 0.16e2 * t29 * t971 * t33 + 0.16e2 * t29 * t876 * t21 + 0.16e2 * t29 * t979 * t125 - 0.16e2 * t29 * t983 * t33 - 0.16e2 * t29 * t987 * t125 + 0.16e2 * t29 * t912 * t37 + 0.16e2 * t29 * t4 * t10 * t142 + 0.16e2 * t29 * t916 * t21 - 0.8e1 * t43 * t377 * t35 - 0.8e1 * t43 * t377 * t31;
  t1071 = t8 * t11;
  t1089 = t46 * t37;
  t1106 = 0.16e2 * t10 * t43 * t9 - 0.8e1 * t1071 * t41 * t43 - 0.8e1 * t1071 * t43 * t45 + 0.16e2 * t11 * t43 * t9 - 0.8e1 * t174 * t35 * t43 + 0.16e2 * t104 * t2 + 0.8e1 * t1089 * t2 + 0.8e1 * t116 * t2 + 0.8e1 * t13 * t2 + 0.8e1 * t2 * t252 + 0.8e1 * t2 * t56 + 0.8e1 * t2 * t65 - 0.8e1 * t20 * t640 - 0.8e1 * t640 * t842;
  t1114 = t17 * t21;
  t1140 = 0.8e1 * t10 * t285 * t35 - 0.8e1 * t11 * t285 * t41 + 0.8e1 * t11 * t285 * t45 - 0.8e1 * t103 * t285 + 0.16e2 * t104 * t707 + 0.8e1 * t1114 * t29 + 0.16e2 * t116 * t707 + 0.16e2 * t116 * t714 - 0.8e1 * t178 * t640 + 0.8e1 * t188 * t90 + 0.8e1 * t245 * t29 - 0.8e1 * t285 * t287 - 0.8e1 * t285 * t493 - 0.8e1 * t332 * t640;
  t1169 = 0.16e2 * t1004 * t29 + 0.16e2 * t1071 * t43 + 0.16e2 * t126 * t29 + 0.8e1 * t157 * t188 + 0.16e2 * t174 * t43 + 0.16e2 * t178 * t2 - 0.8e1 * t188 * t52 + 0.16e2 * t2 * t20 + 0.16e2 * t2 * t332 + 0.16e2 * t2 * t842 + 0.16e2 * t29 * t36 + 0.16e2 * t29 * t402 + 0.16e2 * t377 * t43 + 0.16e2 * t43 * t815;
  t1192 = t43 * t251;
  t1200 = t17 * t37;
  t1203 = -0.8e1 * t1 * t23 * t65 - 0.8e1 * t188 * t21 * t23 - 0.8e1 * t1089 * t193 + 0.8e1 * t1114 * t43 + 0.8e1 * t1200 * t43 - 0.8e1 * t193 * t468 - 0.8e1 * t22 * t714 + 0.8e1 * t245 * t43 - 0.8e1 * t38 * t707 + 0.8e1 * t43 * t527 - 0.8e1 * t52 * t707 + 0.8e1 * t57 * t714 + 0.8e1 * t707 * t72 + 0.16e2 * t1192;
  t1232 = -0.8e1 * t1071 * t476 + 0.8e1 * t1089 * t43 + 0.8e1 * t1200 * t2 + 0.8e1 * t13 * t29 - 0.8e1 * t174 * t476 + 0.8e1 * t2 * t527 + 0.8e1 * t29 * t468 + 0.8e1 * t29 * t47 + 0.16e2 * t29 * t77 - 0.8e1 * t377 * t476 + 0.8e1 * t43 * t468 + 0.8e1 * t43 * t47 + 0.8e1 * t43 * t56 - 0.8e1 * t476 * t815;
  t1262 = -0.8e1 * t1004 * t621 + 0.16e2 * t104 * t285 + 0.16e2 * t1089 * t285 + 0.8e1 * t116 * t29 - 0.8e1 * t126 * t621 + 0.16e2 * t188 * t77 + 0.8e1 * t252 * t29 + 0.16e2 * t285 * t468 + 0.16e2 * t285 * t47 + 0.16e2 * t285 * t56 + 0.16e2 * t285 * t77 + 0.8e1 * t29 * t65 - 0.8e1 * t36 * t621 - 0.8e1 * t402 * t621;
  t1290 = 0.8e1 * t100 * t2 * t838 + 0.8e1 * t117 * t2 * t964 + 0.8e1 * t197 * t29 * t964 + 0.8e1 * t212 * t29 * t838 - 0.16e2 * t1 * t37 + 0.16e2 * t104 * t188 + 0.8e1 * t1192 * t294 + 0.8e1 * t1192 * t303 + 0.16e2 * t65 * t707 - 0.16e2 * t188 - 0.16e2 * t285 - 0.16e2 * t707 - 0.16e2 * t714 - 0.16e2 * t724;
  return(V * (t901 + t936 + t978 + t609 + t652 + t690 + t717 + t756 + t787 + t819 + t864 + t370 + 0.32e2 * t395 + t439 + t483 + t526 + t568 + t202 + t236 + t279 + t322 + t1203 + t1232 + t1290 + t1262 + t147 + t1023 + t1070 + t1169 + t1106 + t1140 + t89));
}
    
};

