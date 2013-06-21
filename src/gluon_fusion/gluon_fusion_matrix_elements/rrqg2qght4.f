  
      subroutine rrqg2qght4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh41J1  
      doubleprecision rrqg2qgh41J2  
      doubleprecision rrqg2qgh41J3  
      doubleprecision rrqg2qgh41J4  
      doubleprecision rrqg2qgh41J5  
      doubleprecision rrqg2qgh41J6  
      doubleprecision rrqg2qgh42J1  
      doubleprecision rrqg2qgh42J2  
      doubleprecision rrqg2qgh42J3  
      doubleprecision rrqg2qgh42J4  
      doubleprecision rrqg2qgh42J5  
      doubleprecision rrqg2qgh42J6  
      doubleprecision rrqg2qght4s1e1  
      doubleprecision rrqg2qght4s1e0  
      doubleprecision rrqg2qght4s1em1  
      doubleprecision rrqg2qght4s1em2  
      doubleprecision rrqg2qght4s1em3  
      doubleprecision rrqg2qght4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght4s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh41J1
      doubleprecision rrqg2qgh41J2
      doubleprecision rrqg2qgh41J3
      doubleprecision rrqg2qgh41J4
      doubleprecision rrqg2qgh41J5
      doubleprecision rrqg2qgh41J6
      doubleprecision rrqg2qgh42J1
      doubleprecision rrqg2qgh42J2
      doubleprecision rrqg2qgh42J3
      doubleprecision rrqg2qgh42J4
      doubleprecision rrqg2qgh42J5
      doubleprecision rrqg2qgh42J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = x2 * 0.3141592653589793D1
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t17 = x1 ** 2
      t18 = t4 ** 2
      t19 = t17 * t18
      t23 = log(0.4D1 * t16 * t19 * x4)
      t24 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t26 = t23 ** 2
      t27 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t30 = rrqg2qgh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t35 = 0.3141592653589793D1 * lh
      t36 = t1 * t7
      t42 = lh ** 2
      t44 = 0.3141592653589793D1 ** 2
      t46 = 0.180D3 * t42 - 0.30D2 * t44
      t47 = 0.3141592653589793D1 * t46
      t48 = t36 * t27
      t49 = t47 * t48
      t51 = 0.1D1 / x4
      t54 = t15 * t17
      t55 = t54 * t18
      t58 = log(0.4D1 * t13 * t55)
      t59 = t58 * 0.3141592653589793D1
      t62 = t58 ** 2
      t63 = t62 * 0.3141592653589793D1
      t66 = (t47 + 0.180D3 * t59 * lh + 0.45D2 * t63) * t1
      t70 = rrqg2qgh41J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t87 = (0.3141592653589793D1 * (0.60D2 * lh * t44 - 0.2884936567583
     #026D3 - 0.120D3 * t42 * lh) - t59 * t46 - 0.90D2 * t63 * lh - 0.15
     #D2 * t62 * t58 * 0.3141592653589793D1) * t1
      t94 = (-0.180D3 * t35 - 0.90D2 * t59) * t1
      t98 = x3 * t10
      t99 = t98 * t12
      t104 = log(0.4D1 * t99 * t54 * t18 * x4)
      t113 = 0.1D1 / x3
      t119 = log(0.4D1 * t99 * t55)
      t121 = t119 ** 2
      t136 = -(0.90D2 * t6 * t7 * (-t23 * t24 + t26 * t27 / 0.2D1 + t30)
     # - 0.180D3 * t35 * t36 * (t24 - t23 * t27) + t49) * t51 / 0.720D3 
     #- t66 * t7 * t24 / 0.720D3 - t6 * t7 * t70 / 0.8D1 - t87 * t7 * t2
     #7 / 0.720D3 - t94 * t7 * t30 / 0.720D3 + (0.90D2 * t6 * t7 * (-t24
     # + t104 * t27) + 0.180D3 * t35 * t48) * t113 * t51 / 0.720D3 - (0.
     #90D2 * t6 * t7 * (-t119 * t24 + t30 + t121 * t27 / 0.2D1) - 0.180D
     #3 * t35 * t36 * (-t119 * t27 + t24) + t49) * t113 / 0.720D3
      t137 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t136)
      t139 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t140 = s * t139
      t141 = t1 * x1
      t142 = t140 * t141
      t143 = t1 * t4
      t144 = t143 * x4
      t145 = t140 * t144
      t146 = -0.1D1 + x4
      t147 = t143 * t146
      t148 = t140 * t147
      t149 = t139 ** 2
      t152 = x1 * t4
      t154 = s * t149 * t14 * t152 * x4
      t156 = t12 * t15
      t158 = x4 * t146
      t159 = t149 ** 2
      t164 = log(-0.4D1 * t17 * t10 * t156 * t158 * t159 * t18)
      t165 = t164 * t149
      t167 = 0.1D1 / (-0.2D1 + t139)
      t168 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t171 = t149 * t167
      t172 = rrqg2qgh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t174 = t164 ** 2
      t175 = t174 * t149
      t176 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t177 = t167 * t176
      t185 = t171 * t168
      t190 = t47 * t1
      t191 = t7 * t149
      t192 = t191 * t177
      t196 = x3 * t17
      t204 = log(-0.4D1 * t196 * t13 * t15 * x4 * t146 * t18 * t159)
      t205 = t204 * t149
      t211 = t35 * t1
      t218 = -(0.90D2 * t6 * t7 * (-t165 * t167 * t168 + t171 * t172 + t
     #175 * t177 / 0.2D1) - 0.180D3 * t35 * t36 * (-t165 * t177 + t185) 
     #+ t190 * t192) * t51 / 0.720D3 + (0.90D2 * t6 * t7 * (t205 * t177 
     #- t185) + 0.180D3 * t211 * t192) * t113 * t51 / 0.720D3
      t219 = FJET(XB1, XB2, s, 0.0D0, t142, -t145, t148, -t154, t218)
      t221 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t224 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t226 = rrqg2qgh42J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t236 = t36 * t221
      t237 = t47 * t236
      t244 = rrqg2qgh42J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t280 = -(0.90D2 * t6 * t7 * (t26 * t221 / 0.2D1 - t23 * t224 + t22
     #6) - 0.180D3 * t35 * t36 * (-t23 * t221 + t224) + t237) * t51 / 0.
     #720D3 - t66 * t7 * t224 / 0.720D3 - t6 * t7 * t244 / 0.8D1 - t87 *
     # t7 * t221 / 0.720D3 - t94 * t7 * t226 / 0.720D3 + (0.90D2 * t6 * 
     #t7 * (-t224 + t104 * t221) + 0.180D3 * t35 * t236) * t113 * t51 / 
     #0.720D3 - (0.90D2 * t6 * t7 * (t226 - t119 * t224 + t121 * t221 / 
     #0.2D1) - 0.180D3 * t35 * t36 * (t224 - t119 * t221) + t237) * t113
     # / 0.720D3
      t281 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t280)
      t283 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t286 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t287 = t167 * t286
      t290 = rrqg2qgh42J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t296 = t171 * t283
      t302 = t191 * t287
      t317 = -(0.90D2 * t6 * t7 * (-t165 * t167 * t283 + t175 * t287 / 0
     #.2D1 + t171 * t290) - 0.180D3 * t35 * t36 * (t296 - t165 * t287) +
     # t190 * t302) * t51 / 0.720D3 + (0.90D2 * t6 * t7 * (t205 * t287 -
     # t296) + 0.180D3 * t211 * t302) * t113 * t51 / 0.720D3
      t318 = FJET(XB1, XB2, s, t142, 0.0D0, t148, -t145, -t154, t317)
      t320 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t321 = s * t320
      t322 = t141 * x3
      t323 = t321 * t322
      t324 = -0.1D1 + x3
      t325 = t141 * t324
      t326 = t321 * t325
      t327 = t321 * t143
      t328 = t320 ** 2
      t332 = s * t328 * t14 * t152 * x3
      t334 = 0.1D1 / (-0.2D1 + t320)
      t335 = t328 * t334
      t336 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t337 = t335 * t336
      t338 = t98 * t156
      t340 = t328 ** 2
      t345 = log(-0.4D1 * t338 * t19 * t324 * x4 * t340)
      t346 = t345 * t328
      t347 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t348 = t334 * t347
      t354 = t7 * t328
      t355 = t354 * t348
      t365 = log(-0.4D1 * t338 * t19 * t324 * t340)
      t366 = t365 * t328
      t369 = rrqg2qgh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t371 = t365 ** 2
      t372 = t371 * t328
      t388 = (0.90D2 * t6 * t7 * (-t337 + t346 * t348) + 0.180D3 * t211 
     #* t355) * t113 * t51 / 0.720D3 - (0.90D2 * t6 * t7 * (-t366 * t334
     # * t336 + t335 * t369 + t372 * t348 / 0.2D1) - 0.180D3 * t35 * t36
     # * (t337 - t366 * t348) + t190 * t355) * t113 / 0.720D3
      t389 = FJET(XB1, XB2, s, t323, -t326, 0.0D0, -t327, -t332, t388)
      t391 = KAPPA2(x1, x2, x3, x4, z)
      t392 = s * t391
      t393 = t392 * t322
      t394 = t392 * t325
      t395 = t392 * t144
      t396 = t392 * t147
      t397 = t391 ** 2
      t402 = cos(t8)
      t405 = Sqrt(x3 * t324 * t158)
      t410 = s * t397 * t14 * t152 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t402 * t405)
      t412 = 0.1D1 / (-0.2D1 + t391)
      t413 = t397 * t412
      t414 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t418 = t397 ** 2
      t423 = log(0.4D1 * t196 * t16 * t158 * t18 * t324 * t418)
      t424 = t423 * t397
      t425 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t426 = t412 * t425
      t432 = t7 * t397
      t436 = 0.90D2 * t6 * t7 * (t413 * t414 - t424 * t426) - 0.180D3 * 
     #t211 * t432 * t426
      t440 = FJET(XB1, XB2, s, t393, -t394, -t395, t396, t410, t436 * t1
     #13 * t51 / 0.720D3)
      t442 = t113 * t51
      t445 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t446 = t335 * t445
      t447 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t448 = t334 * t447
      t454 = t354 * t448
      t462 = rrqg2qgh42J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t479 = (0.90D2 * t6 * t7 * (-t446 + t346 * t448) + 0.180D3 * t211 
     #* t454) * t113 * t51 / 0.720D3 - (0.90D2 * t6 * t7 * (-t366 * t334
     # * t445 + t335 * t462 + t372 * t448 / 0.2D1) - 0.180D3 * t35 * t36
     # * (t446 - t366 * t448) + t190 * t454) * t113 / 0.720D3
      t480 = FJET(XB1, XB2, s, -t326, t323, -t327, 0.0D0, -t332, t479)
      t482 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t484 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t485 = t412 * t484
      t494 = 0.90D2 * t6 * t7 * (t413 * t482 - t424 * t485) - 0.180D3 * 
     #t211 * t432 * t485
      t498 = FJET(XB1, XB2, s, -t394, t393, t396, -t395, t410, t494 * t1
     #13 * t51 / 0.720D3)
      rrqg2qght4s1e1 = t137 * t136 + t219 * t218 + t281 * t280 + t318 * 
     #t317 + t389 * t388 + t440 * t436 * t442 / 0.720D3 + t480 * t479 + 
     #t498 * t494 * t442 / 0.720D3

      end function



      doubleprecision function rrqg2qght4s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh41J1
      doubleprecision rrqg2qgh41J2
      doubleprecision rrqg2qgh41J3
      doubleprecision rrqg2qgh41J4
      doubleprecision rrqg2qgh41J5
      doubleprecision rrqg2qgh41J6
      doubleprecision rrqg2qgh42J1
      doubleprecision rrqg2qgh42J2
      doubleprecision rrqg2qgh42J3
      doubleprecision rrqg2qgh42J4
      doubleprecision rrqg2qgh42J5
      doubleprecision rrqg2qgh42J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D0
     #)
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x1 ** 2
      t19 = t4 ** 2
      t20 = t18 * t19
      t24 = log(0.4D1 * t14 * t16 * t20 * x4)
      t25 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t31 = 0.3141592653589793D1 * lh
      t32 = t1 * t7
      t35 = 0.180D3 * t31 * t32 * t25
      t37 = 0.1D1 / x4
      t40 = t6 * t7
      t41 = 0.1D1 / x3
      t46 = x3 * t11
      t49 = t16 * t18 * t19
      t52 = log(0.4D1 * t46 * t13 * t49)
      t64 = log(0.4D1 * t14 * t49)
      t65 = t64 * 0.3141592653589793D1
      t68 = (-0.180D3 * t31 - 0.90D2 * t65) * t1
      t72 = lh ** 2
      t74 = 0.3141592653589793D1 ** 2
      t80 = t64 ** 2
      t84 = (0.3141592653589793D1 * (0.180D3 * t72 - 0.30D2 * t74) + 0.1
     #80D3 * t65 * lh + 0.45D2 * t80 * 0.3141592653589793D1) * t1
      t88 = rrqg2qgh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t92 = -(0.90D2 * t6 * t7 * (t8 - t24 * t25) - t35) * t37 / 0.720D3
     # - t40 * t25 * t41 * t37 / 0.8D1 - (0.90D2 * t6 * t7 * (-t52 * t25
     # + t8) - t35) * t41 / 0.720D3 - t68 * t7 * t8 / 0.720D3 - t84 * t7
     # * t25 / 0.720D3 - t6 * t7 * t88 / 0.8D1
      t93 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t92)
      t95 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t96 = s * t95
      t97 = t1 * x1
      t98 = t96 * t97
      t99 = t1 * t4
      t100 = t99 * x4
      t101 = t96 * t100
      t102 = -0.1D1 + x4
      t103 = t99 * t102
      t104 = t96 * t103
      t105 = t95 ** 2
      t108 = x1 * t4
      t110 = s * t105 * t15 * t108 * x4
      t111 = t7 * t105
      t112 = t6 * t111
      t114 = 0.1D1 / (-0.2D1 + t95)
      t115 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t116 = t114 * t115
      t117 = t41 * t37
      t122 = t13 * t16
      t124 = x4 * t102
      t125 = t105 ** 2
      t130 = log(-0.4D1 * t18 * t11 * t122 * t124 * t125 * t19)
      t131 = t130 * t105
      t133 = t105 * t114
      t134 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t140 = t31 * t1
      t147 = -t112 * t116 * t117 / 0.8D1 - (0.90D2 * t6 * t7 * (-t131 * 
     #t116 + t133 * t134) - 0.180D3 * t140 * t111 * t116) * t37 / 0.720D
     #3
      t148 = FJET(XB1, XB2, s, 0.0D0, t98, -t101, t104, -t110, t147)
      t150 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t152 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t159 = 0.180D3 * t31 * t32 * t150
      t181 = rrqg2qgh42J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0
     #D0)
      t185 = -(0.90D2 * t6 * t7 * (-t24 * t150 + t152) - t159) * t37 / 0
     #.720D3 - t84 * t7 * t150 / 0.720D3 - t40 * t150 * t41 * t37 / 0.8D
     #1 - (0.90D2 * t6 * t7 * (t152 - t52 * t150) - t159) * t41 / 0.720D
     #3 - t68 * t7 * t152 / 0.720D3 - t6 * t7 * t181 / 0.8D1
      t186 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t185)
      t188 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t190 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t191 = t114 * t190
      t206 = -(0.90D2 * t6 * t7 * (t133 * t188 - t131 * t191) - 0.180D3 
     #* t140 * t111 * t191) * t37 / 0.720D3 - t112 * t191 * t117 / 0.8D1
      t207 = FJET(XB1, XB2, s, t98, 0.0D0, t104, -t101, -t110, t206)
      t209 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t210 = s * t209
      t211 = t97 * x3
      t212 = t210 * t211
      t213 = -0.1D1 + x3
      t214 = t97 * t213
      t215 = t210 * t214
      t216 = t210 * t99
      t217 = t209 ** 2
      t221 = s * t217 * t15 * t108 * x3
      t222 = t7 * t217
      t223 = t6 * t222
      t225 = 0.1D1 / (-0.2D1 + t209)
      t226 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t227 = t225 * t226
      t231 = t217 * t225
      t232 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t235 = t217 ** 2
      t240 = log(-0.4D1 * t46 * t122 * t20 * t213 * t235)
      t241 = t240 * t217
      t253 = -t223 * t227 * t117 / 0.8D1 - (0.90D2 * t6 * t7 * (t231 * t
     #232 - t241 * t227) - 0.180D3 * t140 * t222 * t227) * t41 / 0.720D3
      t254 = FJET(XB1, XB2, s, t212, -t215, 0.0D0, -t216, -t221, t253)
      t256 = KAPPA2(x1, x2, x3, x4, z)
      t257 = s * t256
      t258 = t257 * t211
      t259 = t257 * t214
      t260 = t257 * t100
      t261 = t257 * t103
      t262 = t256 ** 2
      t267 = cos(t9)
      t270 = Sqrt(x3 * t213 * t124)
      t275 = s * t262 * t15 * t108 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t267 * t270)
      t277 = t6 * t7 * t262
      t279 = 0.1D1 / (-0.2D1 + t256)
      t280 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t285 = FJET(XB1, XB2, s, t258, -t259, -t260, t261, t275, t277 * t2
     #79 * t280 * t117 / 0.8D1)
      t288 = t262 * t279
      t294 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t295 = t225 * t294
      t299 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t312 = -t223 * t295 * t117 / 0.8D1 - (0.90D2 * t6 * t7 * (t231 * t
     #299 - t241 * t295) - 0.180D3 * t140 * t222 * t295) * t41 / 0.720D3
      t313 = FJET(XB1, XB2, s, -t215, t212, -t216, 0.0D0, -t221, t312)
      t315 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t320 = FJET(XB1, XB2, s, -t259, t258, t261, -t260, t275, t277 * t2
     #79 * t315 * t117 / 0.8D1)
      rrqg2qght4s1e0 = t93 * t92 + t148 * t147 + t186 * t185 + t207 * t2
     #06 + t254 * t253 + t285 * 0.3141592653589793D1 * t32 * t288 * t280
     # * t41 * t37 / 0.8D1 + t313 * t312 + t320 * 0.3141592653589793D1 *
     # t32 * t288 * t315 * t41 * t37 / 0.8D1

      end function



      doubleprecision function rrqg2qght4s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh41J1
      doubleprecision rrqg2qgh41J2
      doubleprecision rrqg2qgh41J3
      doubleprecision rrqg2qgh41J4
      doubleprecision rrqg2qgh41J5
      doubleprecision rrqg2qgh41J6
      doubleprecision rrqg2qgh42J1
      doubleprecision rrqg2qgh42J2
      doubleprecision rrqg2qgh42J3
      doubleprecision rrqg2qgh42J4
      doubleprecision rrqg2qgh42J5
      doubleprecision rrqg2qgh42J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D0
     #)
      t9 = t7 * t8
      t10 = 0.1D1 / x3
      t14 = rrqg2qgh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t28 = x1 ** 2
      t30 = t4 ** 2
      t34 = log(0.4D1 * t22 / t23 * t27 * t28 * t30)
      t38 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t34 * 0.314
     #1592653589793D1) * t1
      t41 = 0.1D1 / x4
      t45 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.8D1 - t38 * t9 / 
     #0.720D3 - t6 * t9 * t41 / 0.8D1
      t46 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t45)
      t48 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t49 = s * t48
      t50 = t1 * x1
      t51 = t49 * t50
      t52 = t1 * t4
      t54 = t49 * t52 * x4
      t57 = t49 * t52 * (-0.1D1 + x4)
      t58 = t48 ** 2
      t61 = x1 * t4
      t63 = s * t58 * t26 * t61 * x4
      t64 = t6 * t7
      t67 = t58 / (-0.2D1 + t48)
      t68 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t70 = t67 * t68 * t41
      t73 = FJET(XB1, XB2, s, 0.0D0, t51, -t54, t57, -t63, -t64 * t70 / 
     #0.8D1)
      t75 = t1 * t7
      t79 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t80 = t7 * t79
      t84 = rrqg2qgh42J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t93 = -t6 * t80 * t10 / 0.8D1 - t6 * t7 * t84 / 0.8D1 - t38 * t80 
     #/ 0.720D3 - t6 * t80 * t41 / 0.8D1
      t94 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t93)
      t96 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t98 = t67 * t96 * t41
      t101 = FJET(XB1, XB2, s, t51, 0.0D0, t57, -t54, -t63, -t64 * t98 /
     # 0.8D1)
      t106 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t107 = s * t106
      t109 = t107 * t50 * x3
      t112 = t107 * t50 * (-0.1D1 + x3)
      t113 = t107 * t52
      t114 = t106 ** 2
      t118 = s * t114 * t26 * t61 * x3
      t121 = t114 / (-0.2D1 + t106)
      t122 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t124 = t121 * t122 * t10
      t127 = FJET(XB1, XB2, s, t109, -t112, 0.0D0, -t113, -t118, -t64 * 
     #t124 / 0.8D1)
      t132 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0D0)
      t134 = t121 * t132 * t10
      t137 = FJET(XB1, XB2, s, -t112, t109, -t113, 0.0D0, -t118, -t64 * 
     #t134 / 0.8D1)
      rrqg2qght4s1em1 = t46 * t45 - t73 * 0.3141592653589793D1 * t75 * t
     #70 / 0.8D1 + t94 * t93 - t101 * 0.3141592653589793D1 * t75 * t98 /
     # 0.8D1 - t127 * 0.3141592653589793D1 * t75 * t124 / 0.8D1 - t137 *
     # 0.3141592653589793D1 * t75 * t134 / 0.8D1

      end function



      doubleprecision function rrqg2qght4s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh41J1
      doubleprecision rrqg2qgh41J2
      doubleprecision rrqg2qgh41J3
      doubleprecision rrqg2qgh41J4
      doubleprecision rrqg2qgh41J5
      doubleprecision rrqg2qgh41J6
      doubleprecision rrqg2qgh42J1
      doubleprecision rrqg2qgh42J2
      doubleprecision rrqg2qgh42J3
      doubleprecision rrqg2qgh42J4
      doubleprecision rrqg2qgh42J5
      doubleprecision rrqg2qgh42J6
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqg2qgh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D0
     #)
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.8D1)
      t14 = t1 * t7
      t17 = rrqg2qgh42J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.0D
     #0)
      t21 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.8D1)
      rrqg2qght4s1em2 = -t12 * 0.3141592653589793D1 * t14 * t8 / 0.8D1 -
     # t21 * 0.3141592653589793D1 * t14 * t17 / 0.8D1

      end function



      doubleprecision function rrqg2qght4s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh41J1
      doubleprecision rrqg2qgh41J2
      doubleprecision rrqg2qgh41J3
      doubleprecision rrqg2qgh41J4
      doubleprecision rrqg2qgh41J5
      doubleprecision rrqg2qgh41J6
      doubleprecision rrqg2qgh42J1
      doubleprecision rrqg2qgh42J2
      doubleprecision rrqg2qgh42J3
      doubleprecision rrqg2qgh42J4
      doubleprecision rrqg2qgh42J5
      doubleprecision rrqg2qgh42J6
      rrqg2qght4s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght4s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh41J1
      doubleprecision rrqg2qgh41J2
      doubleprecision rrqg2qgh41J3
      doubleprecision rrqg2qgh41J4
      doubleprecision rrqg2qgh41J5
      doubleprecision rrqg2qgh41J6
      doubleprecision rrqg2qgh42J1
      doubleprecision rrqg2qgh42J2
      doubleprecision rrqg2qgh42J3
      doubleprecision rrqg2qgh42J4
      doubleprecision rrqg2qgh42J5
      doubleprecision rrqg2qgh42J6
      rrqg2qght4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh41J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t6 = t2 * t4 * t3
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t18 = t6 * t9
      t19 = 0.1D1 - x1
      t20 = t19 ** 2
      t21 = 0.1D1 - x4
      t22 = t21 ** 2
      t24 = x1 * t13
      t28 = s * t3
      t29 = t7 * x1
      t32 = t29 * t13
      t34 = s - t28 * t29 * x3 - t28 * t32
      t35 = t34 * t1
      t36 = t35 * t3
      t38 = t7 * t19 * t21
      t41 = t2 * t4
      t43 = t8 * t10 * t14
      t46 = t2 * t3
      t50 = t8 * t20 * t22
      t55 = t35 * t4
      t58 = t19 * t21
      t59 = t24 * t58
      t75 = t6 * t9 * t10 * x1 * t14 * t13 + t2 + 0.2D1 * t18 * t20 * t2
     #2 * t24 - 0.6D1 * t36 * t38 + 0.3D1 * t41 * t43 + 0.3D1 * t46 * t3
     #2 + 0.2D1 * t41 * t50 + 0.4D1 * t36 * t32 + t55 * t43 - 0.4D1 * t4
     #1 * t8 * t59 - 0.2D1 * t35 * t4 * t8 * t59 - 0.2D1 * t18 * t58 * t
     #10 * t14 + 0.2D1 * t55 * t50 + 0.7D1 * t35 - 0.2D1 * t46 * t38
      rrqg2qgh41J1 = wd * t75 / t34 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh41J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t10 * x1
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t18 = t6 * t9
      t19 = 0.1D1 - x1
      t20 = t19 ** 2
      t21 = 0.1D1 - x4
      t22 = t21 ** 2
      t24 = x1 * t13
      t26 = t18 * t20 * t22 * t24
      t28 = s * t3
      t29 = t7 * x1
      t32 = t29 * t13
      t34 = s - t28 * t29 * x3 - t28 * t32
      t35 = t34 * t1
      t36 = t35 * t3
      t38 = t7 * t19 * t21
      t41 = t2 * t4
      t43 = t8 * t10 * t14
      t46 = t2 * t3
      t50 = t8 * t20 * t22
      t51 = t41 * t50
      t55 = t35 * t4
      t58 = t19 * t21
      t59 = t24 * t58
      t70 = t55 * t50
      t75 = t6 * t9 * t11 * t14 * t13 + t2 + 0.2D1 * t26 - 0.6D1 * t36 *
     # t38 + 0.3D1 * t41 * t43 + 0.3D1 * t46 * t32 + 0.2D1 * t51 + 0.4D1
     # * t36 * t32 + t55 * t43 - 0.4D1 * t41 * t8 * t59 - 0.2D1 * t35 * 
     #t4 * t8 * t59 - 0.2D1 * t18 * t58 * t10 * t14 + 0.2D1 * t70 + 0.7D
     #1 * t35 - 0.2D1 * t46 * t38
      t83 = cos(x2 * 0.3141592653589793D1)
      t87 = Sqrt(x3 * t13 * x4 * t21)
      t90 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t83 * t87
      t92 = t20 * t21 * x1 * t90
      t94 = t4 ** 2
      t96 = t8 ** 2
      t97 = t96 * t10
      t98 = t90 ** 2
      t99 = t20 * t98
      t102 = t2 * t94
      rrqg2qgh41J2 = (wd * t75 + wd * (-0.2D1 * t35 * t5 * t9 * t92 - 0.
     #2D1 * t26 + 0.2D1 * t35 * t94 * t97 * t99 - 0.2D1 * t70 + 0.2D1 * 
     #t102 * t96 * t10 * t20 * t98 + 0.2D1 * t2 * t94 * t3 * t96 * t7 * 
     #t11 * t13 * t99 - 0.2D1 * t102 * t97 * t13 * t20 * t21 * t90 - 0.2
     #D1 * t18 * t92 - 0.2D1 * t51)) / t34 / s / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrqg2qgh41J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t10 * x1
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t18 = t6 * t9
      t19 = 0.1D1 - x1
      t20 = t19 ** 2
      t21 = 0.1D1 - x4
      t22 = t21 ** 2
      t24 = x1 * t13
      t26 = t18 * t20 * t22 * t24
      t28 = s * t3
      t29 = t7 * x1
      t32 = t29 * t13
      t34 = s - t28 * t29 * x3 - t28 * t32
      t35 = t34 * t1
      t36 = t35 * t3
      t38 = t7 * t19 * t21
      t41 = t2 * t4
      t43 = t8 * t10 * t14
      t46 = t2 * t3
      t50 = t8 * t20 * t22
      t51 = t41 * t50
      t55 = t35 * t4
      t58 = t19 * t21
      t59 = t24 * t58
      t70 = t55 * t50
      t75 = t6 * t9 * t11 * t14 * t13 + t2 + 0.2D1 * t26 - 0.6D1 * t36 *
     # t38 + 0.3D1 * t41 * t43 + 0.3D1 * t46 * t32 + 0.2D1 * t51 + 0.4D1
     # * t36 * t32 + t55 * t43 - 0.4D1 * t41 * t8 * t59 - 0.2D1 * t35 * 
     #t4 * t8 * t59 - 0.2D1 * t18 * t58 * t10 * t14 + 0.2D1 * t70 + 0.7D
     #1 * t35 - 0.2D1 * t46 * t38
      t83 = cos(x2 * 0.3141592653589793D1)
      t87 = Sqrt(x3 * t13 * x4 * t21)
      t90 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t83 * t87
      t92 = t20 * t21 * x1 * t90
      t94 = t4 ** 2
      t96 = t8 ** 2
      t97 = t96 * t10
      t98 = t90 ** 2
      t99 = t20 * t98
      t102 = t2 * t94
      rrqg2qgh41J3 = (wd * t75 + wd * (-0.2D1 * t35 * t5 * t9 * t92 - 0.
     #2D1 * t26 + 0.2D1 * t35 * t94 * t97 * t99 - 0.2D1 * t70 + 0.2D1 * 
     #t102 * t96 * t10 * t20 * t98 + 0.2D1 * t2 * t94 * t3 * t96 * t7 * 
     #t11 * t13 * t99 - 0.2D1 * t102 * t97 * t13 * t20 * t21 * t90 - 0.2
     #D1 * t18 * t92 - 0.2D1 * t51)) / t34 / s / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrqg2qgh41J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t10 * x1
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t18 = t6 * t9
      t19 = 0.1D1 - x1
      t20 = t19 ** 2
      t21 = 0.1D1 - x4
      t22 = t21 ** 2
      t24 = x1 * t13
      t26 = t18 * t20 * t22 * t24
      t28 = s * t3
      t29 = t7 * x1
      t32 = t29 * t13
      t34 = s - t28 * t29 * x3 - t28 * t32
      t35 = t34 * t1
      t36 = t35 * t3
      t38 = t7 * t19 * t21
      t41 = t2 * t4
      t43 = t8 * t10 * t14
      t46 = t2 * t3
      t50 = t8 * t20 * t22
      t51 = t41 * t50
      t55 = t35 * t4
      t58 = t19 * t21
      t59 = t24 * t58
      t70 = t55 * t50
      t75 = t6 * t9 * t11 * t14 * t13 + t2 + 0.2D1 * t26 - 0.6D1 * t36 *
     # t38 + 0.3D1 * t41 * t43 + 0.3D1 * t46 * t32 + 0.2D1 * t51 + 0.4D1
     # * t36 * t32 + t55 * t43 - 0.4D1 * t41 * t8 * t59 - 0.2D1 * t35 * 
     #t4 * t8 * t59 - 0.2D1 * t18 * t58 * t10 * t14 + 0.2D1 * t70 + 0.7D
     #1 * t35 - 0.2D1 * t46 * t38
      t83 = cos(x2 * 0.3141592653589793D1)
      t87 = Sqrt(x3 * t13 * x4 * t21)
      t90 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t83 * t87
      t92 = t20 * t21 * x1 * t90
      t94 = t4 ** 2
      t96 = t8 ** 2
      t97 = t96 * t10
      t98 = t90 ** 2
      t99 = t20 * t98
      t102 = t2 * t94
      rrqg2qgh41J4 = (wd * t75 + wd * (-0.2D1 * t35 * t5 * t9 * t92 - 0.
     #2D1 * t26 + 0.2D1 * t35 * t94 * t97 * t99 - 0.2D1 * t70 + 0.2D1 * 
     #t102 * t96 * t10 * t20 * t98 + 0.2D1 * t2 * t94 * t3 * t96 * t7 * 
     #t11 * t13 * t99 - 0.2D1 * t102 * t97 * t13 * t20 * t21 * t90 - 0.2
     #D1 * t18 * t92 - 0.2D1 * t51)) / t34 / s / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrqg2qgh41J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - z
      t8 = t7 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = t10 * x1
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t18 = t6 * t9
      t19 = 0.1D1 - x1
      t20 = t19 ** 2
      t21 = 0.1D1 - x4
      t22 = t21 ** 2
      t24 = x1 * t13
      t26 = t18 * t20 * t22 * t24
      t28 = s * t3
      t29 = t7 * x1
      t32 = t29 * t13
      t34 = s - t28 * t29 * x3 - t28 * t32
      t35 = t34 * t1
      t36 = t35 * t3
      t38 = t7 * t19 * t21
      t41 = t2 * t4
      t43 = t8 * t10 * t14
      t46 = t2 * t3
      t50 = t8 * t20 * t22
      t51 = t41 * t50
      t55 = t35 * t4
      t58 = t19 * t21
      t59 = t24 * t58
      t70 = t55 * t50
      t75 = t6 * t9 * t11 * t14 * t13 + t2 + 0.2D1 * t26 - 0.6D1 * t36 *
     # t38 + 0.3D1 * t41 * t43 + 0.3D1 * t46 * t32 + 0.2D1 * t51 + 0.4D1
     # * t36 * t32 + t55 * t43 - 0.4D1 * t41 * t8 * t59 - 0.2D1 * t35 * 
     #t4 * t8 * t59 - 0.2D1 * t18 * t58 * t10 * t14 + 0.2D1 * t70 + 0.7D
     #1 * t35 - 0.2D1 * t46 * t38
      t83 = cos(x2 * 0.3141592653589793D1)
      t87 = Sqrt(x3 * t13 * x4 * t21)
      t90 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t83 * t87
      t92 = t20 * t21 * x1 * t90
      t94 = t4 ** 2
      t96 = t8 ** 2
      t97 = t96 * t10
      t98 = t90 ** 2
      t99 = t20 * t98
      t102 = t2 * t94
      rrqg2qgh41J5 = (wd * t75 + wd * (-0.2D1 * t35 * t5 * t9 * t92 - 0.
     #2D1 * t26 + 0.2D1 * t35 * t94 * t97 * t99 - 0.2D1 * t70 + 0.2D1 * 
     #t102 * t96 * t10 * t20 * t98 + 0.2D1 * t2 * t94 * t3 * t96 * t7 * 
     #t11 * t13 * t99 - 0.2D1 * t102 * t97 * t13 * t20 * t21 * t90 - 0.2
     #D1 * t18 * t92 - 0.2D1 * t51)) / t34 / s / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrqg2qgh41J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t12 = t10 * t11
      t13 = t1 ** 2
      t14 = t13 * t1
      t15 = t3 ** 2
      t16 = t15 * t3
      t20 = (0.1D1 - x1) ** 2
      t21 = 0.1D1 - x4
      t26 = cos(x2 * 0.3141592653589793D1)
      t30 = Sqrt(x3 * t7 * x4 * t21)
      t33 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t26 * t30
      t35 = t20 * t21 * x1 * t33
      t37 = t11 * s
      t39 = t37 * t14 * t16
      t40 = t21 ** 2
      t45 = t13 ** 2
      t47 = t15 ** 2
      t48 = x1 ** 2
      t49 = t47 * t48
      t50 = t33 ** 2
      t51 = t20 * t50
      t56 = t15 * t20 * t40
      t58 = t37 * t45
      rrqg2qgh41J6 = wd * (-0.2D1 * t12 * t14 * t16 * t35 - 0.2D1 * t39 
     #* t20 * t40 * x1 * t7 + 0.2D1 * t12 * t45 * t49 * t51 - 0.2D1 * t1
     #2 * t13 * t56 + 0.2D1 * t58 * t47 * t48 * t20 * t50 + 0.2D1 * t37 
     #* t45 * t1 * t47 * t3 * t48 * x1 * t7 * t51 - 0.2D1 * t58 * t49 * 
     #t7 * t20 * t21 * t33 - 0.2D1 * t39 * t35 - 0.2D1 * t37 * t13 * t56
     #) / t10 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh42J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t9 = 0.1D1 - x1
      t14 = cos(x2 * 0.3141592653589793D1)
      t15 = 0.1D1 - x3
      t17 = 0.1D1 - x4
      t20 = Sqrt(x3 * t15 * x4 * t17)
      t23 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t14 * t20
      t27 = t4 ** 2
      t30 = t7 ** 2
      t33 = x1 ** 2
      t35 = t9 ** 2
      t36 = t35 * t9
      t38 = t23 ** 2
      t48 = x1 * t23
      t54 = t17 ** 2
      t61 = s * t3
      t62 = t6 * x1
      rrqg2qgh42J1 = wd * (-0.4D1 * t2 * t4 * t7 * x1 * t9 * t23 - 0.4D1
     # * t2 * t27 * t4 * t30 * t7 * t33 * x1 * t36 * t38 * t23 + 0.8D1 *
     # t2 * t4 * t3 * t7 * t6 * t35 * t17 * t48 - 0.4D1 * t2 * t27 * t30
     # * t36 * t54 * t48) / (s - t61 * t62 * x3 - t61 * t62 * t15) / s /
     # z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh42J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t9 = 0.1D1 - x1
      t14 = cos(x2 * 0.3141592653589793D1)
      t15 = 0.1D1 - x3
      t17 = 0.1D1 - x4
      t20 = Sqrt(x3 * t15 * x4 * t17)
      t23 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t14 * t20
      t26 = 0.4D1 * t2 * t4 * t7 * x1 * t9 * t23
      t27 = t4 ** 2
      t30 = t7 ** 2
      t33 = x1 ** 2
      t35 = t9 ** 2
      t36 = t35 * t9
      t38 = t23 ** 2
      t42 = 0.4D1 * t2 * t27 * t4 * t30 * t7 * t33 * x1 * t36 * t38 * t2
     #3
      t48 = x1 * t23
      t50 = t2 * t4 * t3 * t7 * t6 * t35 * t17 * t48
      t53 = t2 * t27 * t30
      t54 = t17 ** 2
      t58 = 0.4D1 * t53 * t36 * t54 * t48
      t78 = s * t3
      t79 = t6 * x1
      rrqg2qgh42J2 = (wd * (-t26 - t42 + 0.8D1 * t50 - t58) + wd * (t42 
     #+ 0.12D2 * t53 * t33 * t35 * t38 + t58 - 0.12D2 * t50 - 0.8D1 * t2
     # * t27 * t3 * t30 * t6 * t36 * t17 * t33 * t38 + t26)) / (s - t78 
     #* t79 * x3 - t78 * t79 * t15) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh42J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t9 = 0.1D1 - x1
      t14 = cos(x2 * 0.3141592653589793D1)
      t15 = 0.1D1 - x3
      t17 = 0.1D1 - x4
      t20 = Sqrt(x3 * t15 * x4 * t17)
      t23 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t14 * t20
      t26 = 0.4D1 * t2 * t4 * t7 * x1 * t9 * t23
      t27 = t4 ** 2
      t30 = t7 ** 2
      t33 = x1 ** 2
      t35 = t9 ** 2
      t36 = t35 * t9
      t38 = t23 ** 2
      t42 = 0.4D1 * t2 * t27 * t4 * t30 * t7 * t33 * x1 * t36 * t38 * t2
     #3
      t48 = x1 * t23
      t50 = t2 * t4 * t3 * t7 * t6 * t35 * t17 * t48
      t53 = t2 * t27 * t30
      t54 = t17 ** 2
      t58 = 0.4D1 * t53 * t36 * t54 * t48
      t78 = s * t3
      t79 = t6 * x1
      rrqg2qgh42J3 = (wd * (-t26 - t42 + 0.8D1 * t50 - t58) + wd * (t42 
     #+ 0.12D2 * t53 * t33 * t35 * t38 + t58 - 0.12D2 * t50 - 0.8D1 * t2
     # * t27 * t3 * t30 * t6 * t36 * t17 * t33 * t38 + t26)) / (s - t78 
     #* t79 * x3 - t78 * t79 * t15) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh42J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t9 = 0.1D1 - x1
      t14 = cos(x2 * 0.3141592653589793D1)
      t15 = 0.1D1 - x3
      t17 = 0.1D1 - x4
      t20 = Sqrt(x3 * t15 * x4 * t17)
      t23 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t14 * t20
      t26 = 0.4D1 * t2 * t4 * t7 * x1 * t9 * t23
      t27 = t4 ** 2
      t30 = t7 ** 2
      t33 = x1 ** 2
      t35 = t9 ** 2
      t36 = t35 * t9
      t38 = t23 ** 2
      t42 = 0.4D1 * t2 * t27 * t4 * t30 * t7 * t33 * x1 * t36 * t38 * t2
     #3
      t48 = x1 * t23
      t50 = t2 * t4 * t3 * t7 * t6 * t35 * t17 * t48
      t53 = t2 * t27 * t30
      t54 = t17 ** 2
      t58 = 0.4D1 * t53 * t36 * t54 * t48
      t78 = s * t3
      t79 = t6 * x1
      rrqg2qgh42J4 = (wd * (-t26 - t42 + 0.8D1 * t50 - t58) + wd * (t42 
     #+ 0.12D2 * t53 * t33 * t35 * t38 + t58 - 0.12D2 * t50 - 0.8D1 * t2
     # * t27 * t3 * t30 * t6 * t36 * t17 * t33 * t38 + t26)) / (s - t78 
     #* t79 * x3 - t78 * t79 * t15) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh42J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t9 = 0.1D1 - x1
      t14 = cos(x2 * 0.3141592653589793D1)
      t15 = 0.1D1 - x3
      t17 = 0.1D1 - x4
      t20 = Sqrt(x3 * t15 * x4 * t17)
      t23 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t14 * t20
      t26 = 0.4D1 * t2 * t4 * t7 * x1 * t9 * t23
      t27 = t4 ** 2
      t30 = t7 ** 2
      t33 = x1 ** 2
      t35 = t9 ** 2
      t36 = t35 * t9
      t38 = t23 ** 2
      t42 = 0.4D1 * t2 * t27 * t4 * t30 * t7 * t33 * x1 * t36 * t38 * t2
     #3
      t48 = x1 * t23
      t50 = t2 * t4 * t3 * t7 * t6 * t35 * t17 * t48
      t53 = t2 * t27 * t30
      t54 = t17 ** 2
      t58 = 0.4D1 * t53 * t36 * t54 * t48
      t78 = s * t3
      t79 = t6 * x1
      rrqg2qgh42J5 = (wd * (-t26 - t42 + 0.8D1 * t50 - t58) + wd * (t42 
     #+ 0.12D2 * t53 * t33 * t35 * t38 + t58 - 0.12D2 * t50 - 0.8D1 * t2
     # * t27 * t3 * t30 * t6 * t36 * t17 * t33 * t38 + t26)) / (s - t78 
     #* t79 * x3 - t78 * t79 * t15) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh42J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 * s
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t4 ** 2
      t8 = 0.1D1 - z
      t9 = t8 ** 2
      t10 = t9 ** 2
      t13 = x1 ** 2
      t15 = 0.1D1 - x1
      t16 = t15 ** 2
      t17 = t16 * t15
      t22 = cos(x2 * 0.3141592653589793D1)
      t23 = 0.1D1 - x3
      t25 = 0.1D1 - x4
      t28 = Sqrt(x3 * t23 * x4 * t25)
      t31 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t28
      t32 = t31 ** 2
      t38 = t2 * t5 * t10
      t43 = t25 ** 2
      t45 = x1 * t31
      t74 = s * t3
      t75 = t8 * x1
      rrqg2qgh42J6 = wd * (0.4D1 * t2 * t5 * t4 * t10 * t9 * t13 * x1 * 
     #t17 * t32 * t31 + 0.12D2 * t38 * t13 * t16 * t32 + 0.4D1 * t38 * t
     #17 * t43 * t45 - 0.12D2 * t2 * t4 * t3 * t9 * t8 * t16 * t25 * t45
     # - 0.8D1 * t2 * t5 * t3 * t10 * t8 * t17 * t25 * t13 * t32 + 0.4D1
     # * t2 * t4 * t9 * x1 * t15 * t31) / (s - t74 * t75 * x3 - t74 * t7
     #5 * t23) / s / z / 0.3141592653589793D1

      end function
  
 