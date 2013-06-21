/* Topology 1: {S134, S234, S34} (gen. with maple) */
inline void MbbarbbarH_1_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t103;
  double t104;
  double t107;
  double t108;
  double t11;
  double t112;
  double t116;
  double t120;
  double t123;
  double t126;
  double t129;
  double t130;
  double t132;
  double t14;
  double t141;
  double t145;
  double t15;
  double t171;
  double t172;
  double t175;
  double t178;
  double t18;
  double t187;
  double t197;
  double t2;
  double t202;
  double t206;
  double t21;
  double t211;
  double t223;
  double t231;
  double t236;
  double t239;
  double t24;
  double t247;
  double t248;
  double t249;
  double t25;
  double t255;
  double t256;
  double t268;
  double t28;
  double t281;
  double t288;
  double t291;
  double t296;
  double t3;
  double t305;
  double t31;
  double t313;
  double t318;
  double t32;
  double t333;
  double t343;
  double t35;
  double t358;
  double t361;
  double t362;
  double t365;
  double t366;
  double t369;
  double t370;
  double t373;
  double t376;
  double t38;
  double t392;
  double t4;
  double t402;
  double t41;
  double t426;
  double t437;
  double t44;
  double t445;
  double t455;
  double t5;
  double t6;
  double t62;
  double t63;
  double t64;
  double t66;
  double t67;
  double t70;
  double t71;
  double t78;
  double t79;
  double t83;
  double t88;
  double t9;
  double t91;
  double t98;
  t1 = S124 * S124;
  t2 = S123 * S123;
  t3 = t1 * t2;
  t4 = S12 * S12;
  t5 = S234 * t4;
  t6 = t5 * S134;
  t9 = S34 * t1;
  t10 = t2 * t4;
  t11 = t10 * S134;
  t14 = t2 * S234;
  t15 = t14 * t4;
  t18 = t5 * S14;
  t21 = t5 * S23;
  t24 = mh * mh;
  t25 = t5 * t24;
  t28 = t5 * S24;
  t31 = t4 * S134;
  t32 = t31 * S14;
  t35 = t31 * S23;
  t38 = t31 * t24;
  t41 = t31 * S13;
  t44 = t31 * S24;
  t62 = S124 * t2;
  t63 = S24 * S24;
  t64 = t5 * t63;
  t66 = 0.8e1 * t3 * t6 + 0.4e1 * t9 * t11 - 0.8e1 * t9 * t15 - 0.9e1 * t3 * t18 + 0.3e1 * t3 * t21 - 0.6e1 * t3 * t25 - 0.2e1 * t3 * t28 - 0.3e1 * t3 * t32 - 0.3e1 * t3 * t35 - 0.6e1 * t3 * t38 + 0.4e1 * t3 * t41 - 0.6e1 * t3 * t44 - 0.5e1 * t9 * t10 * S13 + 0.8e1 * t9 * t10 * S14 + 0.13e2 * t9 * t10 * S24 + 0.6e1 * t9 * t10 * t24 + 0.2e1 * t9 * t10 * S23 - t62 * t64;
  t67 = S23 * S23;
  t70 = t4 * S12;
  t71 = S234 * t70;
  t78 = S13 * S13;
  t79 = t31 * t78;
  t83 = t1 * S123;
  t88 = S14 * S14;
  t91 = t70 * S134;
  t98 = t71 * S134;
  t103 = S34 * S34;
  t104 = t103 * S124;
  t107 = S34 * S124;
  t108 = t2 * t70;
  t112 = S123 * S234;
  t116 = t103 * t1;
  t120 = t14 * S12;
  t123 = -t62 * t5 * t67 - 0.3e1 * t62 * t71 * S24 - 0.4e1 * t62 * t71 * S23 - t62 * t79 - t62 * t31 * t63 - t83 * t64 - t83 * t5 * t78 - t83 * t79 - t83 * t31 * t88 - 0.3e1 * t83 * t91 * S13 - 0.4e1 * t83 * t91 * S14 - 0.2e1 * t62 * t98 - 0.2e1 * t83 * t98 + 0.12e2 * t104 * t11 - 0.6e1 * t107 * t108 * S134 + 0.12e2 * t9 * t112 * t70 - 0.6e1 * t116 * t112 * t4 - 0.2e1 * t116 * t120;
  t126 = t2 * S12 * S134;
  t129 = t103 * S34;
  t130 = t129 * S124;
  t132 = t129 * t1;
  t141 = t4 * t24;
  t145 = t4 * S24;
  t171 = t4 * t4;
  t172 = S234 * t171;
  t175 = t171 * S134;
  t178 = -0.2e1 * t116 * t126 - t130 * t126 - t132 * t112 * S12 - 0.4e1 * t130 * t120 - 0.4e1 * t132 * S123 * S12 * S134 + 0.6e1 * t3 * t141 * S14 + 0.12e2 * t3 * t145 * S14 + 0.6e1 * t3 * t141 * S24 + 0.4e1 * t107 * t14 * t70 + 0.4e1 * t104 * t15 + 0.4e1 * t116 * S123 * t4 * S134 + 0.4e1 * t9 * S123 * t70 * S134 + 0.2e1 * t3 * t71 + 0.2e1 * t3 * t91 + 0.4e1 * t9 * t108 - 0.4e1 * t62 * t172 - t62 * t175 - t83 * t172;
  t187 = t24 * t24;
  t197 = t62 * S234;
  t202 = S134 * S24;
  t206 = t83 * S234;
  t211 = S134 * S14;
  t223 = t107 * t2;
  t231 = -0.4e1 * t83 * t175 + 0.2e1 * t3 * t70 * S24 + 0.6e1 * t3 * t4 * t63 + 0.6e1 * t3 * t4 * t187 + 0.6e1 * t3 * t4 * t88 + 0.2e1 * t3 * t70 * S13 - 0.2e1 * t197 * t145 * S23 + 0.2e1 * t62 * t4 * t202 * S13 + 0.2e1 * t206 * t145 * S13 - 0.2e1 * t83 * t4 * t211 * S13 - 0.3e1 * t197 * t35 - 0.3e1 * t197 * t41 - 0.3e1 * t206 * t32 - 0.3e1 * t206 * t44 + 0.7e1 * t223 * t32 - 0.2e1 * t223 * t44 + 0.4e1 * t223 * t41 + t223 * t35;
  t236 = t9 * S123;
  t239 = t5 * S13;
  t247 = t9 * t2;
  t248 = S12 * S234;
  t249 = t248 * S23;
  t255 = S12 * S134;
  t256 = t255 * S14;
  t268 = t255 * t63;
  t281 = t248 * S134;
  t288 = 0.6e1 * t223 * t38 + 0.7e1 * t236 * t18 - 0.2e1 * t236 * t239 + 0.4e1 * t236 * t28 + t236 * t21 + 0.6e1 * t236 * t25 - 0.3e1 * t247 * t249 - 0.3e1 * t247 * t248 * S24 - 0.3e1 * t247 * t256 - 0.3e1 * t247 * t255 * S13 - 0.2e1 * t116 * S234 * t255 * S23 - t223 * t255 * t78 - t223 * t268 - 0.6e1 * t24 * t78 * S34 * t1 * S234 * S134 - 0.6e1 * t24 * t88 * S34 * t14 * S134 + 0.8e1 * t247 * t281 + 0.4e1 * t236 * t6 - 0.8e1 * t223 * t6;
  t291 = t248 * t78;
  t296 = t104 * t2;
  t305 = t116 * S123;
  t313 = t104 * S123;
  t318 = t9 * S234;
  t333 = -t236 * t248 * t63 - t236 * t291 - t223 * t291 - t223 * t248 * t67 - 0.3e1 * t296 * t248 * S13 - 0.4e1 * t296 * t249 - t236 * t268 - t236 * t255 * t88 - 0.3e1 * t305 * t255 * S24 - 0.4e1 * t305 * t256 + 0.2e1 * t305 * t281 + 0.4e1 * t313 * t6 + 0.2e1 * t296 * t281 - 0.6e1 * t318 * t38 - 0.6e1 * S34 * t2 * S234 * t38 - 0.2e1 * S14 * t103 * t2 * t281 + 0.4e1 * t223 * t21 + 0.2e1 * t223 * t28;
  t343 = S24 * S13;
  t358 = t248 * t211;
  t361 = S134 * S23;
  t362 = t248 * t361;
  t365 = S134 * t24;
  t366 = t248 * t365;
  t369 = S13 * S134;
  t370 = t248 * t369;
  t373 = t248 * t202;
  t376 = t107 * S123;
  t392 = 0.2e1 * t223 * t239 + 0.4e1 * t236 * t32 + 0.2e1 * t236 * t44 + 0.2e1 * t236 * t41 + 0.2e1 * t223 * t255 * t343 + 0.2e1 * t236 * t248 * t343 - 0.2e1 * t223 * t248 * S13 * S23 - 0.2e1 * t236 * t255 * S24 * S14 - 0.3e1 * t236 * t358 - 0.3e1 * t236 * t362 - 0.6e1 * t236 * t366 - 0.6e1 * t236 * t370 + 0.4e1 * t236 * t373 + 0.13e2 * t376 * t5 * t369 + 0.2e1 * t376 * t5 * t361 - 0.5e1 * t376 * t5 * t202 + 0.8e1 * t376 * t5 * t211 + 0.6e1 * t376 * t5 * t365;
  t402 = S14 * S34 * t2;
  t426 = t255 * t24 * S13;
  t437 = t24 * S14;
  t445 = t107 * t112;
  t455 = -0.9e1 * t223 * t358 + 0.3e1 * t223 * t362 - 0.6e1 * t223 * t366 - 0.2e1 * t223 * t370 - 0.2e1 * t402 * t370 - 0.2e1 * t402 * t362 - 0.6e1 * t402 * t366 + 0.2e1 * t313 * t373 + 0.2e1 * t313 * t370 + 0.6e1 * t376 * t248 * S134 * t88 + 0.6e1 * t376 * t248 * S134 * t187 + 0.6e1 * t376 * t248 * S134 * t78 - 0.6e1 * t318 * t426 - 0.2e1 * t318 * t255 * S24 * S23 - 0.2e1 * t318 * t255 * S14 * S23 + 0.12e2 * t437 * S13 * S34 * S124 * S123 * S234 * S134 + 0.6e1 * t445 * t426 + 0.12e2 * t445 * t255 * S14 * S13 + 0.6e1 * t445 * t255 * t437;
  cgret[0] = 0.16e2 / 0.3e1 * (t66 + t123 + t178 + t231 + t288 + t333 + t392 + t455) / t1 / t2 / t4;
}
inline void MbbarbbarH_1_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t102;
  double t103;
  double t104;
  double t108;
  double t111;
  double t112;
  double t114;
  double t12;
  double t124;
  double t128;
  double t139;
  double t142;
  double t151;
  double t159;
  double t16;
  double t160;
  double t161;
  double t162;
  double t165;
  double t166;
  double t169;
  double t17;
  double t170;
  double t179;
  double t18;
  double t180;
  double t184;
  double t188;
  double t189;
  double t194;
  double t2;
  double t202;
  double t206;
  double t21;
  double t217;
  double t228;
  double t23;
  double t233;
  double t238;
  double t248;
  double t250;
  double t258;
  double t263;
  double t267;
  double t273;
  double t282;
  double t289;
  double t293;
  double t299;
  double t3;
  double t300;
  double t306;
  double t307;
  double t310;
  double t320;
  double t33;
  double t331;
  double t364;
  double t37;
  double t38;
  double t4;
  double t41;
  double t42;
  double t44;
  double t46;
  double t47;
  double t49;
  double t5;
  double t51;
  double t52;
  double t6;
  double t62;
  double t69;
  double t70;
  double t72;
  double t75;
  double t78;
  double t79;
  double t89;
  double t9;
  double t92;
  double t95;
  double t97;
  t1 = S124 * S124;
  t2 = S123 * S123;
  t3 = t1 * t2;
  t4 = S12 * S12;
  t5 = t4 * S12;
  t6 = S234 * t5;
  t9 = t5 * S134;
  t12 = S34 * t1;
  t16 = S124 * t2;
  t17 = t4 * t4;
  t18 = S234 * t17;
  t21 = t17 * S134;
  t23 = t1 * S123;
  t33 = t2 * t4;
  t37 = t2 * S234;
  t38 = t37 * t4;
  t41 = S234 * t4;
  t42 = t41 * S14;
  t44 = t41 * S24;
  t46 = t4 * S134;
  t47 = t46 * S23;
  t49 = t46 * S13;
  t51 = 0.2e1 * t3 * t6 + 0.2e1 * t3 * t9 - 0.4e1 * t12 * t2 * t5 - 0.2e1 * t16 * t18 + t16 * t21 + t23 * t18 - 0.2e1 * t23 * t21 - 0.2e1 * t3 * t5 * S24 - 0.2e1 * t3 * t5 * S13 - 0.2e1 * t12 * t33 * S134 - 0.2e1 * t12 * t38 + t3 * t42 - t3 * t44 + t3 * t47 - t3 * t49;
  t52 = t46 * S24;
  t62 = mh * mh;
  t69 = S24 * S24;
  t70 = t41 * t69;
  t72 = S23 * S23;
  t75 = t6 * S24;
  t78 = S13 * S13;
  t79 = t46 * t78;
  t89 = S14 * S14;
  t92 = t9 * S13;
  t95 = -0.2e1 * t3 * t52 - t12 * t33 * S13 - 0.2e1 * t12 * t33 * S14 - t12 * t33 * S24 + 0.6e1 * t12 * t33 * t62 - 0.2e1 * t12 * t33 * S23 - t16 * t70 + t16 * t41 * t72 - 0.3e1 * t16 * t75 + t16 * t79 + 0.3e1 * t16 * t46 * t69 + t23 * t70 + 0.3e1 * t23 * t41 * t78 - t23 * t79 + t23 * t46 * t89 - 0.3e1 * t23 * t92;
  t97 = t6 * S134;
  t102 = S34 * S34;
  t103 = t102 * t1;
  t104 = t37 * S12;
  t108 = t2 * S12 * S134;
  t111 = t102 * S34;
  t112 = t111 * S124;
  t114 = t111 * t1;
  t124 = S34 * S124;
  t128 = t102 * S124;
  t139 = t41 * S13;
  t142 = t41 * S134;
  t151 = -0.4e1 * t16 * t97 - 0.4e1 * t23 * t97 - 0.4e1 * t103 * t104 - 0.4e1 * t103 * t108 + t112 * t108 + t114 * S123 * S234 * S12 - 0.2e1 * t104 * t112 - 0.2e1 * t114 * S123 * S12 * S134 - 0.4e1 * t124 * t37 * t5 - 0.4e1 * t128 * t38 - 0.4e1 * t103 * S123 * t4 * S134 - 0.4e1 * t12 * S123 * t5 * S134 - 0.2e1 * t3 * t139 + 0.2e1 * t103 * t142 + 0.2e1 * t2 * t102 * t142 + 0.2e1 * t16 * t9 * S24;
  t159 = t128 * S123;
  t160 = S12 * S234;
  t161 = S134 * S24;
  t162 = t160 * t161;
  t165 = S13 * S134;
  t166 = t160 * t165;
  t169 = t12 * S234;
  t170 = S12 * S134;
  t179 = t124 * t2;
  t180 = S24 * S13;
  t184 = t12 * S123;
  t188 = S134 * S23;
  t189 = t160 * t188;
  t194 = t124 * S123;
  t202 = S134 * S14;
  t206 = -0.2e1 * t16 * t92 + 0.2e1 * t23 * t6 * S13 - 0.2e1 * t23 * t75 - 0.2e1 * t159 * t162 - 0.2e1 * t159 * t166 + 0.2e1 * t169 * t170 * S24 * S23 + 0.2e1 * t169 * t170 * S14 * S23 - 0.4e1 * t179 * t170 * t180 - 0.4e1 * t184 * t160 * t180 + t184 * t189 - 0.2e1 * t184 * t166 - t184 * t162 - t194 * t41 * t165 - 0.2e1 * t194 * t41 * t188 - t194 * t41 * t161 - 0.2e1 * t194 * t41 * t202;
  t217 = S14 * S34 * t2;
  t228 = t23 * S234;
  t233 = t16 * S234;
  t238 = t46 * S14;
  t248 = 0.6e1 * t194 * t41 * S134 * t62 + t179 * t160 * t202 - t179 * t166 + 0.2e1 * t217 * t166 + 0.2e1 * t217 * t189 - 0.2e1 * t179 * t162 - 0.4e1 * t16 * t4 * t161 * S13 - 0.4e1 * t228 * t4 * S24 * S13 + 0.3e1 * t233 * t47 + 0.3e1 * t233 * t49 + 0.3e1 * t228 * t238 + 0.3e1 * t228 * t52 - t179 * t238 + 0.2e1 * t179 * t52 - 0.4e1 * t179 * t49;
  t250 = t46 * t62;
  t258 = t41 * S23;
  t263 = t12 * t2;
  t267 = t160 * S24;
  t273 = t170 * S13;
  t282 = t170 * t69;
  t289 = -t179 * t47 + 0.6e1 * t179 * t250 - t184 * t42 + 0.2e1 * t184 * t139 - 0.4e1 * t184 * t44 - t184 * t258 + 0.6e1 * t184 * t41 * t62 + 0.3e1 * t263 * t160 * S23 + 0.3e1 * t263 * t267 + 0.3e1 * t263 * t170 * S14 + 0.3e1 * t263 * t273 + 0.2e1 * t103 * S234 * t170 * S23 + t179 * t170 * t78 + 0.3e1 * t179 * t282 - 0.2e1 * t184 * t142 - 0.2e1 * t179 * t142;
  t293 = t160 * t78;
  t299 = t128 * t2;
  t300 = t160 * S13;
  t306 = t103 * S123;
  t307 = t170 * S24;
  t310 = t160 * S134;
  t320 = S34 * t2 * S234;
  t331 = t184 * t160 * t69 + 0.3e1 * t184 * t293 - t179 * t293 + t179 * t160 * t72 - 0.3e1 * t299 * t300 - t184 * t282 + t184 * t170 * t89 - 0.3e1 * t306 * t307 + 0.2e1 * t306 * t310 - 0.4e1 * t159 * t142 + 0.2e1 * t299 * t310 + 0.6e1 * t169 * t250 + 0.6e1 * t320 * t250 + 0.2e1 * S14 * t102 * t2 * t310 - 0.4e1 * t179 * t258 - 0.2e1 * t179 * t44;
  t364 = -0.2e1 * t179 * t139 - 0.4e1 * t184 * t238 - 0.2e1 * t184 * t52 - 0.2e1 * t184 * t49 - 0.4e1 * t233 * t52 - 0.4e1 * t228 * t49 - 0.4e1 * t263 * t300 - 0.4e1 * t263 * t307 + 0.2e1 * t299 * t307 - 0.2e1 * t299 * t273 + 0.2e1 * t306 * t300 - 0.2e1 * t306 * t267 + 0.2e1 * t169 * t52 + 0.2e1 * t169 * t238 + 0.2e1 * t320 * t49 + 0.2e1 * t320 * t47;
  cgret[0] = 0.16e2 / 0.3e1 * (t51 + t95 + t151 + t206 + t248 + t289 + t331 + t364) / t1 / t2 / t4;
}
inline void MbbarbbarH_1_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t104;
  double t109;
  double t11;
  double t112;
  double t114;
  double t117;
  double t118;
  double t119;
  double t12;
  double t120;
  double t124;
  double t127;
  double t130;
  double t131;
  double t132;
  double t143;
  double t151;
  double t155;
  double t17;
  double t173;
  double t183;
  double t195;
  double t2;
  double t20;
  double t23;
  double t24;
  double t25;
  double t29;
  double t3;
  double t33;
  double t4;
  double t44;
  double t48;
  double t49;
  double t50;
  double t53;
  double t57;
  double t58;
  double t61;
  double t64;
  double t65;
  double t70;
  double t71;
  double t79;
  double t87;
  double t9;
  double t93;
  double t94;
  double t99;
  t1 = S34 * S124;
  t2 = S123 * S123;
  t3 = t1 * t2;
  t4 = S12 * S234;
  t9 = S124 * S124;
  t10 = S34 * t9;
  t11 = t10 * S123;
  t12 = S12 * S134;
  t17 = S134 * S14;
  t20 = S13 * S134;
  t23 = t1 * S123;
  t24 = S12 * S12;
  t25 = S234 * t24;
  t29 = S134 * S23;
  t33 = S134 * S24;
  t44 = t24 * S12;
  t48 = t10 * S234;
  t49 = t24 * S134;
  t50 = t49 * S24;
  t53 = t49 * S14;
  t57 = S34 * t2 * S234;
  t58 = t49 * S13;
  t61 = t49 * S23;
  t64 = S124 * t2;
  t65 = t64 * S234;
  t70 = 0.2e1 * t3 * t4 * S13 * S23 + 0.2e1 * t11 * t12 * S24 * S14 - t11 * t4 * t17 + t11 * t4 * t20 - 0.2e1 * t23 * t25 * t20 - 0.2e1 * t23 * t25 * t29 - 0.2e1 * t23 * t25 * t33 - 0.2e1 * t23 * t25 * t17 - t3 * t4 * t29 + t3 * t4 * t33 - 0.4e1 * t10 * t2 * t44 - 0.2e1 * t48 * t50 - 0.2e1 * t53 * t48 - 0.2e1 * t57 * t58 - 0.2e1 * t57 * t61 + 0.2e1 * t65 * t24 * S24 * S23;
  t71 = t9 * S123;
  t79 = t71 * S234;
  t87 = t25 * S13;
  t93 = t10 * t2;
  t94 = t4 * S23;
  t99 = t12 * S14;
  t104 = t25 * S134;
  t109 = S13 * S13;
  t112 = 0.2e1 * t71 * t24 * t17 * S13 + 0.2e1 * t65 * t61 + t65 * t58 + 0.2e1 * t79 * t53 + t79 * t50 + 0.4e1 * t3 * t50 - 0.4e1 * t3 * t58 + 0.4e1 * t11 * t87 - 0.4e1 * t11 * t25 * S24 + 0.2e1 * t93 * t94 + t93 * t4 * S24 + 0.2e1 * t93 * t99 + t93 * t12 * S13 - 0.4e1 * t104 * t11 - 0.4e1 * t3 * t104 + t3 * t4 * t109;
  t114 = S23 * S23;
  t117 = S34 * S34;
  t118 = t117 * S124;
  t119 = t118 * t2;
  t120 = t4 * S13;
  t124 = S24 * S24;
  t127 = S14 * S14;
  t130 = t117 * t9;
  t131 = t130 * S123;
  t132 = t12 * S24;
  t143 = t2 * t24;
  t151 = t9 * t2;
  t155 = t3 * t4 * t114 + t119 * t120 + 0.2e1 * t119 * t94 + t11 * t12 * t124 + t11 * t12 * t127 + t131 * t132 + 0.2e1 * t131 * t99 - 0.4e1 * t118 * S123 * t104 + t65 * t50 + t79 * t58 + t93 * t120 + t93 * t132 - 0.4e1 * t10 * t143 * S134 - 0.4e1 * t10 * t2 * S234 * t24 - t151 * t25 * S23 - t151 * t53;
  t173 = S234 * t44;
  t183 = t44 * S134;
  t195 = t151 * t50 - 0.2e1 * t10 * t143 * S13 - 0.2e1 * t10 * t143 * S14 - 0.2e1 * t10 * t143 * S24 - 0.2e1 * t10 * t143 * S23 + t64 * t25 * t124 + t64 * t25 * t114 + t64 * t173 * S24 + 0.2e1 * t64 * t173 * S23 + t71 * t49 * t109 + t71 * t49 * t127 + t71 * t183 * S13 + 0.2e1 * t71 * t183 * S14 + t151 * t87 - 0.2e1 * t130 * t104 - 0.2e1 * t2 * t117 * t104;
  cgret[0] = 0.16e2 / 0.3e1 * (t70 + t112 + t155 + t195) / t9 / t2 / t24;
}
inline void MbbarbbarH_1_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 2: {S13, S24} (gen. with maple) */
inline void MbbarbbarH_2_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t100;
  double t102;
  double t105;
  double t108;
  double t112;
  double t126;
  double t131;
  double t14;
  double t144;
  double t15;
  double t16;
  double t17;
  double t185;
  double t2;
  double t20;
  double t24;
  double t25;
  double t26;
  double t29;
  double t3;
  double t32;
  double t33;
  double t36;
  double t37;
  double t45;
  double t46;
  double t48;
  double t51;
  double t57;
  double t64;
  double t7;
  double t71;
  double t74;
  double t75;
  double t77;
  double t78;
  double t83;
  double t87;
  double t88;
  double t93;
  t1 = S24 * S124;
  t2 = S123 * S123;
  t3 = t2 * S14;
  t7 = t2 * S23;
  t14 = S124 * S124;
  t15 = S24 * t14;
  t16 = S12 * S23;
  t17 = t16 * S34;
  t20 = S14 * S12;
  t24 = S14 * S13;
  t25 = t2 * S12;
  t26 = t25 * S34;
  t29 = t25 * S23;
  t32 = mh * mh;
  t33 = t25 * t32;
  t36 = S13 * S124;
  t37 = t25 * S14;
  t45 = S123 * S12;
  t46 = t45 * S23;
  t48 = t45 * S14;
  t51 = t45 * S34;
  t57 = -0.2e1 * t1 * t3 * S23 + 0.2e1 * t1 * t7 * S34 - 0.2e1 * t1 * t3 * S34 - 0.2e1 * t15 * t17 - 0.2e1 * t15 * t20 * S23 - 0.2e1 * t24 * t26 - 0.2e1 * t24 * t29 - 0.6e1 * t24 * t33 - 0.7e1 * t36 * t37 - t36 * t26 - 0.6e1 * t36 * t33 + 0.2e1 * t36 * t29 - t15 * t46 - 0.4e1 * t15 * t48 + 0.5e1 * t15 * t51 - 0.6e1 * t15 * t45 * t32;
  t64 = S13 * t14;
  t71 = S14 * S14;
  t74 = S34 * S34;
  t75 = t45 * t74;
  t77 = S24 * S24;
  t78 = t77 * S124;
  t83 = S23 * S23;
  t87 = S13 * S13;
  t88 = t87 * S124;
  t93 = S123 * S14;
  t100 = 0.2e1 * t1 * t37 - 0.3e1 * t1 * t29 - 0.5e1 * t1 * t26 + 0.2e1 * t64 * t46 - 0.3e1 * t64 * t48 - 0.5e1 * t64 * t51 - t1 * t45 * t71 - t1 * t75 - 0.2e1 * t78 * t48 - 0.2e1 * t78 * t51 - t36 * t45 * t83 - t36 * t75 - 0.2e1 * t88 * t46 - 0.2e1 * t88 * t51 - 0.2e1 * t64 * t93 * S23 + 0.2e1 * t64 * t93 * S34;
  t102 = t87 * t14;
  t105 = t77 * t14;
  t108 = t87 * S13;
  t112 = t77 * S24 * S124;
  t126 = S12 * S12;
  t131 = t2 * S34;
  t144 = 0.2e1 * t2 * t102 + 0.2e1 * t105 * t2 - t108 * t14 * S123 - t112 * t2 - 0.2e1 * t1 * S123 * t20 * S34 - 0.2e1 * t36 * S123 * t17 - 0.2e1 * t64 * S123 * S23 * S34 + t88 * t25 - 0.6e1 * t15 * S123 * t126 + t105 * t45 + 0.2e1 * t64 * t131 + 0.4e1 * t64 * t3 + 0.2e1 * t15 * t131 + 0.4e1 * t15 * t7 - 0.2e1 * t78 * t25 - 0.2e1 * t102 * t45;
  t185 = -0.2e1 * t112 * t45 - 0.2e1 * t108 * S124 * t45 - 0.2e1 * t102 * t93 + 0.2e1 * t64 * S123 * t74 + t64 * S123 * t71 + 0.2e1 * t64 * S123 * t83 - t102 * S123 * S34 - 0.2e1 * t78 * t7 + 0.2e1 * t1 * t2 * t74 + t1 * t2 * t83 + 0.2e1 * t1 * t2 * t71 - t78 * t131 - 0.6e1 * t15 * t126 * t32 - 0.2e1 * t105 * t16 - 0.2e1 * S14 * t87 * t25 + 0.6e1 * t15 * t25 + 0.6e1 * t64 * t25;
  cgret[0] = 0.16e2 / 0.3e1 * (t57 + t100 + t144 + t185) / t14 / t2 / S12;
}
inline void MbbarbbarH_2_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t102;
  double t11;
  double t110;
  double t114;
  double t12;
  double t13;
  double t14;
  double t141;
  double t17;
  double t2;
  double t20;
  double t21;
  double t28;
  double t29;
  double t3;
  double t32;
  double t35;
  double t38;
  double t4;
  double t43;
  double t46;
  double t53;
  double t57;
  double t58;
  double t60;
  double t61;
  double t66;
  double t7;
  double t71;
  double t72;
  double t77;
  double t79;
  double t83;
  double t93;
  double t97;
  double t99;
  t1 = S124 * S124;
  t2 = S24 * t1;
  t3 = S12 * S23;
  t4 = t3 * S34;
  t7 = S14 * S12;
  t11 = S14 * S13;
  t12 = S123 * S123;
  t13 = t12 * S12;
  t14 = t13 * S34;
  t17 = t13 * S23;
  t20 = S13 * S124;
  t21 = t13 * S14;
  t28 = S123 * S12;
  t29 = t28 * S23;
  t32 = t28 * S14;
  t35 = t28 * S34;
  t38 = S24 * S124;
  t43 = 0.2e1 * t2 * t4 + 0.2e1 * t2 * t7 * S23 + 0.2e1 * t11 * t14 + 0.2e1 * t11 * t17 + 0.3e1 * t20 * t21 - 0.6e1 * t20 * t14 - 0.3e1 * t17 * t20 + 0.3e1 * t2 * t29 - 0.3e1 * t2 * t32 - 0.6e1 * t2 * t35 + 0.2e1 * t38 * t21 + 0.3e1 * t38 * t17;
  t46 = S13 * t1;
  t53 = S14 * S14;
  t57 = S34 * S34;
  t58 = t28 * t57;
  t60 = S24 * S24;
  t61 = t60 * S124;
  t66 = S23 * S23;
  t71 = S13 * S13;
  t72 = t71 * S124;
  t77 = 0.5e1 * t38 * t14 + 0.2e1 * t46 * t29 + 0.3e1 * t46 * t32 + 0.5e1 * t35 * t46 + 0.3e1 * t38 * t28 * t53 + t58 * t38 + 0.4e1 * t61 * t32 + 0.2e1 * t61 * t35 + 0.3e1 * t20 * t28 * t66 + t58 * t20 + 0.4e1 * t72 * t29 + 0.2e1 * t72 * t35;
  t79 = t71 * S13;
  t83 = t60 * S24 * S124;
  t93 = S12 * S12;
  t97 = t60 * t1;
  t99 = t12 * S14;
  t102 = t12 * S23;
  t110 = t71 * t1;
  t114 = -t79 * t1 * S123 - t83 * t12 + 0.4e1 * t38 * S123 * t7 * S34 + 0.4e1 * t20 * S123 * t4 - t72 * t13 + 0.2e1 * t2 * S123 * t93 - t97 * t28 - 0.4e1 * t46 * t99 - 0.4e1 * t102 * t2 + 0.2e1 * t83 * t28 + 0.2e1 * t79 * S124 * t28 + 0.2e1 * t110 * S123 * S14;
  t141 = -t46 * S123 * t53 + t110 * S123 * S34 + 0.2e1 * t61 * t102 - t38 * t12 * t66 + t61 * t12 * S34 + 0.2e1 * t97 * t3 + 0.2e1 * S14 * t71 * t13 - 0.10e2 * t2 * t13 - 0.10e2 * t46 * t13 + 0.2e1 * t20 * t12 * t93 - 0.3e1 * t46 * t102 - 0.3e1 * t2 * t99;
  cgret[0] = 0.16e2 / 0.3e1 * (t43 + t77 + t114 + t141) / t1 / t12 / S12;
}
inline void MbbarbbarH_2_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t11;
  double t14;
  double t15;
  double t2;
  double t20;
  double t23;
  double t28;
  double t29;
  double t3;
  double t31;
  double t32;
  double t33;
  double t37;
  double t38;
  double t42;
  double t44;
  double t48;
  double t50;
  double t58;
  double t6;
  double t7;
  double t8;
  t1 = S24 * S124;
  t2 = S123 * S123;
  t3 = S23 * S23;
  t6 = S124 * S124;
  t7 = S24 * t6;
  t8 = t2 * S12;
  t11 = S13 * t6;
  t14 = S13 * S124;
  t15 = S12 * S12;
  t20 = t2 * S14;
  t23 = t2 * S23;
  t28 = S24 * S24;
  t29 = t28 * S124;
  t31 = S13 * S13;
  t32 = t31 * t6;
  t33 = S123 * S12;
  t37 = -t1 * t2 * t3 + 0.2e1 * t7 * t8 + 0.2e1 * t11 * t8 + t14 * t2 * t15 + t7 * S123 * t15 - 0.2e1 * t11 * t20 - t11 * t23 - 0.2e1 * t7 * t23 - t7 * t20 - t29 * t8 - t32 * t33 + t32 * S123 * S14;
  t38 = S14 * S14;
  t42 = t8 * S14;
  t44 = t8 * S34;
  t48 = t33 * S23;
  t50 = t33 * S34;
  t58 = -t11 * S123 * t38 + t29 * t23 - t14 * t42 + t14 * t44 + t14 * t8 * S23 - t48 * t7 + t7 * t50 + t7 * t33 * S14 - t1 * t42 + t1 * t44 - t11 * t48 + t11 * t50;
  cgret[0] = 0.16e2 / 0.3e1 * (t37 + t58) / t6 / t2 / S12;
}
inline void MbbarbbarH_2_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 3: {S13, S134, S234, S24} (gen. with maple) */
inline void MbbarbbarH_3_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t102;
  double t111;
  double t113;
  double t116;
  double t121;
  double t124;
  double t131;
  double t138;
  double t14;
  double t15;
  double t150;
  double t156;
  double t16;
  double t160;
  double t163;
  double t17;
  double t175;
  double t19;
  double t197;
  double t199;
  double t2;
  double t201;
  double t207;
  double t22;
  double t228;
  double t28;
  double t29;
  double t3;
  double t34;
  double t35;
  double t40;
  double t42;
  double t43;
  double t55;
  double t58;
  double t61;
  double t66;
  double t67;
  double t68;
  double t69;
  double t7;
  double t72;
  double t73;
  double t76;
  double t77;
  double t80;
  double t85;
  double t87;
  double t90;
  double t92;
  double t95;
  double t96;
  double t98;
  t1 = S124 * S123;
  t2 = t1 * S234;
  t3 = S24 * S12;
  t7 = mh * mh;
  t14 = S13 * S134;
  t15 = t14 * S123;
  t16 = S124 * S12;
  t17 = t16 * S23;
  t19 = t16 * S34;
  t22 = t16 * S14;
  t28 = S13 * S234;
  t29 = t28 * S123;
  t34 = S134 * S24;
  t35 = t34 * S123;
  t40 = S24 * S234;
  t42 = S12 * t7;
  t43 = t42 * S14;
  t55 = S24 * S24;
  t58 = S123 * S12;
  t61 = S13 * S13;
  t66 = -0.7e1 * t2 * t3 * S14 - 0.6e1 * t2 * t3 * t7 + 0.2e1 * t2 * t3 * S23 - t15 * t17 - 0.6e1 * t15 * t19 - 0.4e1 * t15 * t22 - 0.6e1 * t15 * t16 * t7 + 0.2e1 * t22 * t29 - 0.3e1 * t29 * t17 + 0.2e1 * t35 * t17 - 0.3e1 * t35 * t22 + 0.6e1 * t40 * S124 * t43 + 0.6e1 * t15 * t43 + 0.12e2 * t15 * S12 * S14 * S34 + 0.6e1 * t15 * t42 * S34 - 0.2e1 * t55 * S24 * S124 * t58 - 0.2e1 * t61 * S13 * S124 * t58;
  t67 = S12 * S12;
  t68 = t67 * S12;
  t69 = S124 * t68;
  t72 = t55 * S234;
  t73 = t67 * S124;
  t76 = t61 * S134;
  t77 = S123 * t67;
  t80 = S123 * t68;
  t85 = S13 * S124;
  t87 = t61 * S124;
  t90 = S24 * S124;
  t92 = t55 * S124;
  t95 = S34 * S34;
  t96 = t95 * S34;
  t98 = S134 * S124;
  t102 = S234 * S123;
  t111 = t1 * S12;
  t113 = t1 * t67;
  t116 = t61 * S234;
  t121 = S134 * t55;
  t124 = 0.6e1 * t40 * t69 + 0.2e1 * t72 * t73 + 0.2e1 * t76 * t77 + 0.2e1 * t28 * t80 + 0.2e1 * t34 * t69 - t85 * t80 - 0.2e1 * t87 * t77 - t90 * t80 - 0.2e1 * t92 * t77 - 0.2e1 * t96 * S13 * t98 - 0.2e1 * t96 * S24 * t102 - t1 * S234 * t67 * S24 + t1 * S234 * S12 * t55 + t76 * t111 + 0.5e1 * t14 * t113 - 0.2e1 * t111 * t116 - 0.5e1 * t28 * t113 - 0.2e1 * t121 * t111;
  t131 = t7 * t7;
  t138 = S14 * S14;
  t150 = t58 * t138;
  t156 = t58 * S14;
  t160 = S23 * S23;
  t163 = S12 * S13;
  t175 = -0.5e1 * t34 * t113 + 0.12e2 * t40 * t73 * S14 + 0.6e1 * t40 * t16 * t131 + 0.6e1 * t40 * t73 * t7 + 0.6e1 * t40 * t16 * t138 + 0.2e1 * t72 * t19 + 0.6e1 * t14 * t58 * t95 + 0.6e1 * t14 * t58 * t131 + 0.6e1 * t14 * t150 + 0.2e1 * t76 * t58 * S34 - t116 * t156 - t28 * t150 - t121 * t17 - t34 * t16 * t160 + 0.2e1 * t163 * t98 * t95 - t61 * S12 * t98 * S23 - t163 * t98 * t160 + 0.2e1 * t3 * t102 * t95;
  t197 = S134 * S123 * S124;
  t199 = t95 * S13;
  t201 = S34 * S124;
  t207 = S13 * S34;
  t228 = -t55 * S12 * t102 * S14 - t3 * t102 * t138 - t85 * t150 - 0.2e1 * t87 * t156 - 0.2e1 * t85 * t77 * S14 - t90 * t58 * t160 - 0.2e1 * t92 * t58 * S23 - 0.2e1 * t90 * t77 * S23 - S34 * t61 * t197 - t199 * t197 - t201 * t102 * t55 - t95 * S124 * t102 * S24 - t207 * t98 * t138 - 0.2e1 * t199 * t98 * S14 - S34 * S24 * t102 * t160 - 0.2e1 * t95 * S24 * t102 * S23 - 0.2e1 * t207 * S134 * t1 * S14 - 0.2e1 * t201 * S123 * t40 * S23;
  cgret[0] = 0.16e2 / 0.3e1 * (t66 + t124 + t175 + t228) / S123 / S124 / S12;
}
inline void MbbarbbarH_3_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t119;
  double t12;
  double t123;
  double t129;
  double t13;
  double t132;
  double t136;
  double t143;
  double t148;
  double t151;
  double t156;
  double t17;
  double t173;
  double t18;
  double t21;
  double t22;
  double t23;
  double t26;
  double t30;
  double t32;
  double t35;
  double t37;
  double t4;
  double t40;
  double t50;
  double t51;
  double t52;
  double t59;
  double t60;
  double t67;
  double t69;
  double t7;
  double t70;
  double t73;
  double t75;
  double t78;
  double t79;
  double t80;
  double t89;
  double t90;
  double t94;
  t1 = S24 * S24;
  t4 = S123 * S12;
  t7 = S13 * S13;
  t12 = t1 * S234;
  t13 = S12 * S12;
  t17 = t7 * S134;
  t18 = S123 * t13;
  t21 = S13 * S234;
  t22 = t13 * S12;
  t23 = S123 * t22;
  t26 = S134 * S24;
  t30 = S13 * S124;
  t32 = t7 * S124;
  t35 = S24 * S124;
  t37 = t1 * S124;
  t40 = S124 * S123;
  t50 = 0.2e1 * t1 * S24 * S124 * t4 + 0.2e1 * t7 * S13 * S124 * t4 - 0.2e1 * t12 * S124 * t13 - 0.2e1 * t18 * t17 - 0.2e1 * t21 * t23 - 0.2e1 * t26 * S124 * t22 + t30 * t23 + 0.2e1 * t18 * t32 + t23 * t35 + 0.2e1 * t37 * t18 - 0.6e1 * t40 * S234 * t13 * S24 - t40 * S234 * S12 * t1 - t17 * t40 * S12;
  t51 = S13 * S134;
  t52 = t40 * t13;
  t59 = S124 * S12;
  t60 = t59 * S34;
  t67 = t4 * S14;
  t69 = S14 * S14;
  t70 = t4 * t69;
  t73 = t59 * S23;
  t75 = S23 * S23;
  t78 = S12 * S13;
  t79 = S134 * S124;
  t80 = S34 * S34;
  t89 = S24 * S12;
  t90 = S234 * S123;
  t94 = -0.6e1 * t51 * t52 + 0.5e1 * t21 * t52 + 0.5e1 * t52 * t26 - 0.2e1 * t12 * t60 - 0.2e1 * t17 * t4 * S34 + t7 * S234 * t67 + t21 * t70 + S134 * t1 * t73 + t26 * t59 * t75 - 0.2e1 * t78 * t79 * t80 + t7 * S12 * t79 * S23 + t78 * t79 * t75 - 0.2e1 * t89 * t90 * t80;
  t119 = S134 * S123 * S124;
  t123 = S34 * S124;
  t129 = S13 * S34;
  t132 = t1 * S12 * t90 * S14 + t89 * t90 * t69 + 0.3e1 * t30 * t70 + 0.4e1 * t67 * t32 + 0.4e1 * t30 * t18 * S14 + 0.3e1 * t35 * t4 * t75 + 0.4e1 * t37 * t4 * S23 + 0.4e1 * t35 * t18 * S23 + S34 * t7 * t119 - t80 * S13 * t119 + t123 * t90 * t1 - t80 * S124 * t90 * S24 + t129 * t79 * t69;
  t136 = t40 * S234;
  t143 = t51 * S123;
  t148 = t59 * S14;
  t151 = t21 * S123;
  t156 = t26 * S123;
  t173 = S34 * S24 * t90 * t75 + 0.3e1 * t136 * t89 * S14 - 0.3e1 * t136 * t89 * S23 + 0.3e1 * t143 * t73 + 0.2e1 * t143 * t60 - 0.3e1 * t143 * t148 + 0.2e1 * t151 * t148 + 0.3e1 * t151 * t73 + 0.2e1 * t156 * t73 + 0.3e1 * t148 * t156 + 0.2e1 * t129 * S134 * t40 * S14 + 0.2e1 * t123 * S123 * S234 * S24 * S23 + 0.2e1 * t136 * t89 * S34;
  cgret[0] = 0.16e2 / 0.3e1 * (t50 + t94 + t132 + t173) / S123 / S124 / S12;
}
inline void MbbarbbarH_3_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t11;
  double t15;
  double t17;
  double t2;
  double t20;
  double t23;
  double t3;
  double t34;
  double t45;
  double t57;
  double t58;
  double t60;
  double t61;
  double t62;
  double t65;
  double t66;
  double t69;
  double t72;
  double t76;
  double t8;
  double t80;
  double t86;
  double t9;
  double t96;
  t1 = S124 * S123;
  t2 = t1 * S234;
  t3 = S24 * S12;
  t8 = S13 * S134;
  t9 = t8 * S123;
  t10 = S124 * S12;
  t11 = t10 * S23;
  t15 = t10 * S14;
  t17 = S13 * S234;
  t20 = S134 * S24;
  t23 = S13 * S34;
  t34 = S12 * S13;
  t45 = S12 * S12;
  t57 = -t2 * t3 * S14 + t2 * t3 * S23 - t9 * t11 + t9 * t10 * S34 + t9 * t15 - t17 * S123 * t15 - t20 * S123 * t11 + t23 * S134 * t1 * S14 + S34 * S124 * S123 * S234 * S24 * S23 + t2 * t3 * S34 + 0.2e1 * t34 * S134 * S124 * S14 * S34 + 0.2e1 * t3 * S234 * S123 * S34 * S23 + 0.2e1 * t17 * S123 * t45 * S23 + 0.2e1 * t20 * S124 * t45 * S14 + t1 * S234 * t45 * S24;
  t58 = t1 * t45;
  t60 = S13 * S13;
  t61 = t60 * S234;
  t62 = t1 * S12;
  t65 = S24 * S24;
  t66 = t65 * S134;
  t69 = S123 * S12;
  t72 = S14 * S14;
  t76 = S23 * S23;
  t80 = S134 * S124;
  t86 = S234 * S123;
  t96 = t8 * t58 - t61 * t62 + t17 * t58 - t62 * t66 + t20 * t58 + t61 * t69 * S14 + t17 * t69 * t72 + t66 * t11 + t20 * t10 * t76 + t60 * S12 * t80 * S23 + t34 * t80 * t76 + t65 * S12 * t86 * S14 + t3 * t86 * t72 + t23 * t80 * t72 + S34 * S24 * t86 * t76;
  cgret[0] = 0.16e2 / 0.3e1 * (t57 + t96) / S123 / S124 / S12;
}
inline void MbbarbbarH_3_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 4: {S13, S24, S34} (gen. with maple) */
inline void MbbarbbarH_4_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t13;
  double t2;
  double t24;
  double t36;
  double t4;
  double t5;
  double t7;
  t1 = S24 * S124;
  t2 = S12 * S12;
  t4 = S24 * S24;
  t5 = t4 * S124;
  t7 = S13 * S13;
  t10 = S123 * S12;
  t13 = S13 * S124;
  t24 = S13 * S123;
  t36 = t1 * t2 + t5 * S12 + t7 * S123 * S12 - 0.2e1 * t10 * t1 - 0.2e1 * t10 * t13 - 0.2e1 * t5 * S123 - 0.2e1 * t7 * S124 * S123 + 0.2e1 * t1 * S12 * S14 + 0.2e1 * t24 * S12 * S23 - 0.4e1 * S24 * S123 * S124 * S14 - 0.4e1 * t13 * S123 * S23 + t2 * t24;
  cgret[0] = -0.16e2 / 0.3e1 * t36 / S124 / S123;
}
inline void MbbarbbarH_4_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t10;
  double t15;
  double t18;
  double t2;
  double t25;
  double t4;
  t1 = S24 * S124;
  t2 = S12 * S12;
  t4 = S24 * S24;
  t10 = S13 * S123;
  t15 = S13 * S13;
  t18 = S24 * S123;
  t25 = S13 * S124;
  cgret[0] = -0.16e2 / 0.3e1 * (t1 * t2 - t4 * S124 * S12 - 0.2e1 * t1 * S12 * S14 - 0.2e1 * t10 * S12 * S23 + t10 * t2 - t15 * S123 * S12 + 0.3e1 * t18 * S124 * S23 + 0.4e1 * t18 * S124 * S14 + 0.3e1 * t25 * S123 * S14 + 0.4e1 * t25 * S123 * S23) / S124 / S123;
}
inline void MbbarbbarH_4_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t13;
  double t7;
  t7 = S24 * S123;
  t13 = S13 * S124;
  cgret[0] = -0.16e2 / 0.3e1 * (-S24 * S124 * S12 * S14 - S13 * S123 * S12 * S23 + 0.2e1 * t7 * S124 * S14 + t7 * S124 * S23 + 0.2e1 * t13 * S123 * S23 + t13 * S123 * S14) / S124 / S123;
}
inline void MbbarbbarH_4_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 5: {S134, S23, S34} (gen. with maple) */
inline void MbbarbbarH_5_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t11;
  double t20;
  double t29;
  double t32;
  double t4;
  double t7;
  t1 = S24 * S123;
  t4 = S12 * S12;
  t7 = S24 * S24;
  t11 = S13 * S13;
  t20 = S14 * S14;
  t29 = 0.2e1 * t1 * S14 - 0.2e1 * S123 * t4 - S123 * t7 + 0.2e1 * S13 * t1 - 0.2e1 * t11 * S123 - 0.2e1 * S24 * S12 * S123 + 0.2e1 * S13 * S123 * S12 + t20 * S123 + S123 * S12 * S14 + S12 * t7 + 0.2e1 * t4 * S12 + 0.2e1 * S24 * t4;
  t32 = 0.16e2 / 0.3e1 * t29 / S123;
  cgret[1] = -t32;
  cgret[2] = -t32;
}
inline void MbbarbbarH_5_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t14;
  double t8;
  t1 = S14 * S14;
  t8 = S24 * S24;
  t14 = 0.16e2 / 0.3e1 * (t1 * S123 - 0.2e1 * S123 * S24 * S14 - S123 * S12 * S14 + t8 * S123 - t8 * S12) / S123;
  cgret[1] = -t14;
  cgret[2] = -t14;
}
inline void MbbarbbarH_5_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t3;
  double t9;
  t3 = S24 * S24;
  t9 = 0.16e2 / 0.3e1 * (-S123 * S24 * S14 + t3 * S123 - t3 * S12) / S123;
  cgret[1] = -t9;
  cgret[2] = -t9;
}
inline void MbbarbbarH_5_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 6: {S34, 1/S23, S234^2} (gen. with maple) */
inline void MbbarbbarH_6_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  cgret[0] = -0.32e2 / 0.3e1 * S13 - 0.32e2 / 0.3e1 * S12 - 0.32e2 / 0.3e1 * S14;
}
inline void MbbarbbarH_6_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  cgret[0] = 0.32e2 / 0.3e1 * S12 + 0.32e2 / 0.3e1 * S13 + 0.32e2 / 0.3e1 * S14;
}
inline void MbbarbbarH_6_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_6_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 7: {S34, S234^2, 1/S24} (gen. with maple) */
inline void MbbarbbarH_7_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.32e2 * t1;
  cgret[0] = -t2;
  cgret[1] = -0.32e2 / 0.3e1 * S13 - 0.32e2 / 0.3e1 * S14 - t2 - 0.32e2 / 0.3e1 * S12;
}
inline void MbbarbbarH_7_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  cgret[1] = 0.32e2 / 0.3e1 * S12 + 0.32e2 / 0.3e1 * S13 + 0.32e2 / 0.3e1 * S14;
}
inline void MbbarbbarH_7_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_7_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 8: {S34, S234^2, 1/S34} (gen. with maple) */
inline void MbbarbbarH_8_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = 0.32e2 * t1;
  cgret[0] = -t2;
  cgret[1] = -t2;
}
inline void MbbarbbarH_8_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t4;
  double t6;
  t4 = mh * mh;
  t6 = 0.32e2 / 0.3e1 * S12 + 0.32e2 / 0.3e1 * S13 + 0.32e2 / 0.3e1 * S14 + 0.32e2 * t4;
  cgret[0] = t6;
  cgret[1] = t6;
}
inline void MbbarbbarH_8_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t2;
  t2 = -0.32e2 / 0.3e1 * S13 - 0.32e2 / 0.3e1 * S12 - 0.32e2 / 0.3e1 * S14;
  cgret[0] = t2;
  cgret[1] = t2;
}
inline void MbbarbbarH_8_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 9: {S23, S234^2, 1/S34} (gen. with maple) */
inline void MbbarbbarH_9_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  t1 = mh * mh;
  cgret[2] = -0.32e2 * t1;
}
inline void MbbarbbarH_9_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_9_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_9_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 10: {S23, S234^2, 1/S24} (gen. with maple) */
inline void MbbarbbarH_10_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t3;
  t3 = mh * mh;
  cgret[1] = -0.32e2 / 0.3e1 * S13 - 0.32e2 / 0.3e1 * S14 - 0.32e2 * t3 - 0.32e2 / 0.3e1 * S12;
  cgret[2] = -0.32e2 / 0.3e1 * S13 - 0.32e2 / 0.3e1 * S12 - 0.32e2 / 0.3e1 * S14;
}
inline void MbbarbbarH_10_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t2;
  t2 = 0.32e2 / 0.3e1 * S12 + 0.32e2 / 0.3e1 * S13 + 0.32e2 / 0.3e1 * S14;
  cgret[1] = t2;
  cgret[2] = t2;
}
inline void MbbarbbarH_10_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_10_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 11: {S134^2, S234^2, S34^2, 1/(S134*S24-S234*S14)^2} (gen. with maple) */
inline void MbbarbbarH_11_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  t1 = mh * mh;
  cgret[0] = -0.32e2 * t1;
}
inline void MbbarbbarH_11_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_11_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_11_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 12: {S23^2, S234^2, 1/(S12*S234-S24*S123)^2} (gen. with maple) */
inline void MbbarbbarH_12_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = S123 * S123;
  cgret[1] = -0.32e2 * t1 / t2;
}
inline void MbbarbbarH_12_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_12_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_12_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
/* Topology 13: {S23^2, S234^2, 1/(S34*S123-S13*S234)^2} (gen. with maple) */
inline void MbbarbbarH_13_e0 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
  double t1;
  double t2;
  t1 = mh * mh;
  t2 = S123 * S123;
  cgret[2] = -0.32e2 * t1 / t2;
}
inline void MbbarbbarH_13_e1 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_13_e2 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
inline void MbbarbbarH_13_e3 (Parametrization & P, double cgret[4])
{
  DECLARE_INVARIANTS;
}
