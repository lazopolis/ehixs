  
      subroutine rrgq2qght4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh41J1  
      doubleprecision rrgq2qgh41J2  
      doubleprecision rrgq2qgh41J3  
      doubleprecision rrgq2qgh41J4  
      doubleprecision rrgq2qgh41J5  
      doubleprecision rrgq2qgh41J6  
      doubleprecision rrgq2qgh42J1  
      doubleprecision rrgq2qgh42J2  
      doubleprecision rrgq2qgh42J3  
      doubleprecision rrgq2qgh42J4  
      doubleprecision rrgq2qgh42J5  
      doubleprecision rrgq2qgh42J6  
      doubleprecision rrgq2qght4s1e1  
      doubleprecision rrgq2qght4s1e0  
      doubleprecision rrgq2qght4s1em1  
      doubleprecision rrgq2qght4s1em2  
      doubleprecision rrgq2qght4s1em3  
      doubleprecision rrgq2qght4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght4s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh41J1
      doubleprecision rrgq2qgh41J2
      doubleprecision rrgq2qgh41J3
      doubleprecision rrgq2qgh41J4
      doubleprecision rrgq2qgh41J5
      doubleprecision rrgq2qgh41J6
      doubleprecision rrgq2qgh42J1
      doubleprecision rrgq2qgh42J2
      doubleprecision rrgq2qgh42J3
      doubleprecision rrgq2qgh42J4
      doubleprecision rrgq2qgh42J5
      doubleprecision rrgq2qgh42J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = x2 * pi
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
      t24 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t26 = t23 ** 2
      t27 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t30 = rrgq2qgh41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t35 = pi * lh
      t36 = t1 * t7
      t42 = lh ** 2
      t44 = pi ** 2
      t46 = 0.180D3 * t42 - 0.30D2 * t44
      t47 = pi * t46
      t48 = t36 * t27
      t49 = t47 * t48
      t51 = 0.1D1 / x4
      t54 = t15 * t17
      t55 = t54 * t18
      t58 = log(0.4D1 * t13 * t55)
      t59 = t58 * pi
      t62 = t58 ** 2
      t63 = t62 * pi
      t66 = (t47 + 0.180D3 * t59 * lh + 0.45D2 * t63) * t1
      t70 = rrgq2qgh41J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t88 = (pi * (0.60D2 * lh * t44 - 0.240D3 * zeta3 - 0.120D3 * t42 *
     # lh) - t59 * t46 - 0.90D2 * t63 * lh - 0.15D2 * t62 * t58 * pi) * 
     #t1
      t95 = (-0.180D3 * t35 - 0.90D2 * t59) * t1
      t99 = x3 * t10
      t100 = t99 * t12
      t105 = log(0.4D1 * t100 * t54 * t18 * x4)
      t114 = 0.1D1 / x3
      t120 = log(0.4D1 * t100 * t55)
      t122 = t120 ** 2
      t137 = -(0.90D2 * t6 * t7 * (-t23 * t24 + t26 * t27 / 0.2D1 + t30)
     # - 0.180D3 * t35 * t36 * (t24 - t23 * t27) + t49) * t51 / 0.720D3 
     #- t66 * t7 * t24 / 0.720D3 - t6 * t7 * t70 / 0.8D1 - t88 * t7 * t2
     #7 / 0.720D3 - t95 * t7 * t30 / 0.720D3 + (0.90D2 * t6 * t7 * (-t24
     # + t105 * t27) + 0.180D3 * t35 * t48) * t114 * t51 / 0.720D3 - (0.
     #90D2 * t6 * t7 * (-t120 * t24 + t30 + t122 * t27 / 0.2D1) - 0.180D
     #3 * t35 * t36 * (-t120 * t27 + t24) + t49) * t114 / 0.720D3
      t138 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t137)
      t140 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t141 = s * t140
      t142 = t1 * x1
      t143 = t141 * t142
      t144 = t1 * t4
      t145 = t144 * x4
      t146 = t141 * t145
      t147 = -0.1D1 + x4
      t148 = t144 * t147
      t149 = t141 * t148
      t150 = t140 ** 2
      t153 = x1 * t4
      t155 = s * t150 * t14 * t153 * x4
      t157 = t12 * t15
      t159 = x4 * t147
      t160 = t150 ** 2
      t165 = log(-0.4D1 * t17 * t10 * t157 * t159 * t160 * t18)
      t166 = t165 * t150
      t168 = 0.1D1 / (-0.2D1 + t140)
      t169 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t146, t
     #143, t149, -t155)
      t172 = t150 * t168
      t173 = rrgq2qgh41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t146, t
     #143, t149, -t155)
      t175 = t165 ** 2
      t176 = t175 * t150
      t177 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t146, t
     #143, t149, -t155)
      t178 = t168 * t177
      t186 = t172 * t169
      t191 = t47 * t1
      t192 = t7 * t150
      t193 = t192 * t178
      t197 = x3 * t17
      t205 = log(-0.4D1 * t197 * t13 * t15 * x4 * t147 * t18 * t160)
      t206 = t205 * t150
      t212 = t35 * t1
      t219 = -(0.90D2 * t6 * t7 * (-t166 * t168 * t169 + t172 * t173 + t
     #176 * t178 / 0.2D1) - 0.180D3 * t35 * t36 * (-t166 * t178 + t186) 
     #+ t191 * t193) * t51 / 0.720D3 + (0.90D2 * t6 * t7 * (t206 * t178 
     #- t186) + 0.180D3 * t212 * t193) * t114 * t51 / 0.720D3
      t220 = FJET(XB1, XB2, s, 0.0D0, t143, -t146, t149, -t155, t219)
      t222 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #3, -t5, 0.0D0)
      t225 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #3, -t5, 0.0D0)
      t227 = rrgq2qgh42J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #3, -t5, 0.0D0)
      t237 = t36 * t222
      t238 = t47 * t237
      t245 = rrgq2qgh42J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #3, -t5, 0.0D0)
      t281 = -(0.90D2 * t6 * t7 * (t26 * t222 / 0.2D1 - t23 * t225 + t22
     #7) - 0.180D3 * t35 * t36 * (-t23 * t222 + t225) + t238) * t51 / 0.
     #720D3 - t66 * t7 * t225 / 0.720D3 - t6 * t7 * t245 / 0.8D1 - t88 *
     # t7 * t222 / 0.720D3 - t95 * t7 * t227 / 0.720D3 + (0.90D2 * t6 * 
     #t7 * (-t225 + t105 * t222) + 0.180D3 * t35 * t237) * t114 * t51 / 
     #0.720D3 - (0.90D2 * t6 * t7 * (t227 - t120 * t225 + t122 * t222 / 
     #0.2D1) - 0.180D3 * t35 * t36 * (t225 - t120 * t222) + t238) * t114
     # / 0.720D3
      t282 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t281)
      t284 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t146, t
     #143, t149, -t155)
      t287 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t146, t
     #143, t149, -t155)
      t288 = t168 * t287
      t291 = rrgq2qgh42J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t146, t
     #143, t149, -t155)
      t297 = t172 * t284
      t303 = t192 * t288
      t318 = -(0.90D2 * t6 * t7 * (-t166 * t168 * t284 + t176 * t288 / 0
     #.2D1 + t172 * t291) - 0.180D3 * t35 * t36 * (t297 - t166 * t288) +
     # t191 * t303) * t51 / 0.720D3 + (0.90D2 * t6 * t7 * (t206 * t288 -
     # t297) + 0.180D3 * t212 * t303) * t114 * t51 / 0.720D3
      t319 = FJET(XB1, XB2, s, t143, 0.0D0, t149, -t146, -t155, t318)
      t321 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t322 = s * t321
      t323 = t142 * x3
      t324 = t322 * t323
      t325 = -0.1D1 + x3
      t326 = t142 * t325
      t327 = t322 * t326
      t328 = t322 * t144
      t329 = t321 ** 2
      t333 = s * t329 * t14 * t153 * x3
      t335 = 0.1D1 / (-0.2D1 + t321)
      t336 = t329 * t335
      t337 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, t324, 0.0D0, -t
     #327, -t328, -t333)
      t338 = t336 * t337
      t339 = t99 * t157
      t341 = t329 ** 2
      t346 = log(-0.4D1 * t339 * t19 * t325 * x4 * t341)
      t347 = t346 * t329
      t348 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, t324, 0.0D0, -t
     #327, -t328, -t333)
      t349 = t335 * t348
      t355 = t7 * t329
      t356 = t355 * t349
      t366 = log(-0.4D1 * t339 * t19 * t325 * t341)
      t367 = t366 * t329
      t370 = rrgq2qgh41J3(s, XB1, XB2, z, lh, wd, nf, s, t324, 0.0D0, -t
     #327, -t328, -t333)
      t372 = t366 ** 2
      t373 = t372 * t329
      t389 = (0.90D2 * t6 * t7 * (-t338 + t347 * t349) + 0.180D3 * t212 
     #* t356) * t114 * t51 / 0.720D3 - (0.90D2 * t6 * t7 * (-t367 * t335
     # * t337 + t336 * t370 + t373 * t349 / 0.2D1) - 0.180D3 * t35 * t36
     # * (t338 - t367 * t349) + t191 * t356) * t114 / 0.720D3
      t390 = FJET(XB1, XB2, s, t324, -t327, 0.0D0, -t328, -t333, t389)
      t392 = KAPPA2(x1, x2, x3, x4, z)
      t393 = s * t392
      t394 = t393 * t323
      t395 = t393 * t326
      t396 = t393 * t145
      t397 = t393 * t148
      t398 = t392 ** 2
      t403 = cos(t8)
      t406 = Sqrt(x3 * t325 * t159)
      t411 = s * t398 * t14 * t153 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t403 * t406)
      t413 = 0.1D1 / (-0.2D1 + t392)
      t414 = t398 * t413
      t415 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, t394, -t396, -t
     #395, t397, t411)
      t419 = t398 ** 2
      t424 = log(0.4D1 * t197 * t16 * t159 * t18 * t325 * t419)
      t425 = t424 * t398
      t426 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, t394, -t396, -t
     #395, t397, t411)
      t427 = t413 * t426
      t433 = t7 * t398
      t437 = 0.90D2 * t6 * t7 * (t414 * t415 - t425 * t427) - 0.180D3 * 
     #t212 * t433 * t427
      t441 = FJET(XB1, XB2, s, t394, -t395, -t396, t397, t411, t437 * t1
     #14 * t51 / 0.720D3)
      t443 = t114 * t51
      t446 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, t324, 0.0D0, -t
     #327, -t328, -t333)
      t447 = t336 * t446
      t448 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, t324, 0.0D0, -t
     #327, -t328, -t333)
      t449 = t335 * t448
      t455 = t355 * t449
      t463 = rrgq2qgh42J3(s, XB1, XB2, z, lh, wd, nf, s, t324, 0.0D0, -t
     #327, -t328, -t333)
      t480 = (0.90D2 * t6 * t7 * (-t447 + t347 * t449) + 0.180D3 * t212 
     #* t455) * t114 * t51 / 0.720D3 - (0.90D2 * t6 * t7 * (-t367 * t335
     # * t446 + t336 * t463 + t373 * t449 / 0.2D1) - 0.180D3 * t35 * t36
     # * (t447 - t367 * t449) + t191 * t455) * t114 / 0.720D3
      t481 = FJET(XB1, XB2, s, -t327, t324, -t328, 0.0D0, -t333, t480)
      t483 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, t394, -t396, -t
     #395, t397, t411)
      t485 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, t394, -t396, -t
     #395, t397, t411)
      t486 = t413 * t485
      t495 = 0.90D2 * t6 * t7 * (t414 * t483 - t425 * t486) - 0.180D3 * 
     #t212 * t433 * t486
      t499 = FJET(XB1, XB2, s, -t395, t394, t397, -t396, t411, t495 * t1
     #14 * t51 / 0.720D3)
      rrgq2qght4s1e1 = t138 * t137 + t220 * t219 + t282 * t281 + t319 * 
     #t318 + t390 * t389 + t441 * t437 * t443 / 0.720D3 + t481 * t480 + 
     #t499 * t495 * t443 / 0.720D3

      end function



      doubleprecision function rrgq2qght4s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh41J1
      doubleprecision rrgq2qgh41J2
      doubleprecision rrgq2qgh41J3
      doubleprecision rrgq2qgh41J4
      doubleprecision rrgq2qgh41J5
      doubleprecision rrgq2qgh41J6
      doubleprecision rrgq2qgh42J1
      doubleprecision rrgq2qgh42J2
      doubleprecision rrgq2qgh42J3
      doubleprecision rrgq2qgh42J4
      doubleprecision rrgq2qgh42J5
      doubleprecision rrgq2qgh42J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t9 = x2 * pi
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
      t25 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t31 = pi * lh
      t32 = t1 * t7
      t35 = 0.180D3 * t31 * t32 * t25
      t37 = 0.1D1 / x4
      t40 = t6 * t7
      t41 = 0.1D1 / x3
      t46 = x3 * t11
      t49 = t16 * t18 * t19
      t52 = log(0.4D1 * t46 * t13 * t49)
      t64 = log(0.4D1 * t14 * t49)
      t65 = t64 * pi
      t68 = (-0.180D3 * t31 - 0.90D2 * t65) * t1
      t72 = lh ** 2
      t74 = pi ** 2
      t80 = t64 ** 2
      t84 = (pi * (0.180D3 * t72 - 0.30D2 * t74) + 0.180D3 * t65 * lh + 
     #0.45D2 * t80 * pi) * t1
      t88 = rrgq2qgh41J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
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
      t115 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t101, t
     #98, t104, -t110)
      t116 = t114 * t115
      t117 = t41 * t37
      t122 = t13 * t16
      t124 = x4 * t102
      t125 = t105 ** 2
      t130 = log(-0.4D1 * t18 * t11 * t122 * t124 * t125 * t19)
      t131 = t130 * t105
      t133 = t105 * t114
      t134 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t101, t
     #98, t104, -t110)
      t140 = t31 * t1
      t147 = -t112 * t116 * t117 / 0.8D1 - (0.90D2 * t6 * t7 * (-t131 * 
     #t116 + t133 * t134) - 0.180D3 * t140 * t111 * t116) * t37 / 0.720D
     #3
      t148 = FJET(XB1, XB2, s, 0.0D0, t98, -t101, t104, -t110, t147)
      t150 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #3, -t5, 0.0D0)
      t152 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #3, -t5, 0.0D0)
      t159 = 0.180D3 * t31 * t32 * t150
      t181 = rrgq2qgh42J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #3, -t5, 0.0D0)
      t185 = -(0.90D2 * t6 * t7 * (-t24 * t150 + t152) - t159) * t37 / 0
     #.720D3 - t84 * t7 * t150 / 0.720D3 - t40 * t150 * t41 * t37 / 0.8D
     #1 - (0.90D2 * t6 * t7 * (t152 - t52 * t150) - t159) * t41 / 0.720D
     #3 - t68 * t7 * t152 / 0.720D3 - t6 * t7 * t181 / 0.8D1
      t186 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t185)
      t188 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t101, t
     #98, t104, -t110)
      t190 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t101, t
     #98, t104, -t110)
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
      t226 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, t212, 0.0D0, -t
     #215, -t216, -t221)
      t227 = t225 * t226
      t231 = t217 * t225
      t232 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, t212, 0.0D0, -t
     #215, -t216, -t221)
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
      t280 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, -t
     #259, t261, t275)
      t285 = FJET(XB1, XB2, s, t258, -t259, -t260, t261, t275, t277 * t2
     #79 * t280 * t117 / 0.8D1)
      t288 = t262 * t279
      t294 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, t212, 0.0D0, -t
     #215, -t216, -t221)
      t295 = t225 * t294
      t299 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, t212, 0.0D0, -t
     #215, -t216, -t221)
      t312 = -t223 * t295 * t117 / 0.8D1 - (0.90D2 * t6 * t7 * (t231 * t
     #299 - t241 * t295) - 0.180D3 * t140 * t222 * t295) * t41 / 0.720D3
      t313 = FJET(XB1, XB2, s, -t215, t212, -t216, 0.0D0, -t221, t312)
      t315 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, -t
     #259, t261, t275)
      t320 = FJET(XB1, XB2, s, -t259, t258, t261, -t260, t275, t277 * t2
     #79 * t315 * t117 / 0.8D1)
      rrgq2qght4s1e0 = t93 * t92 + t148 * t147 + t186 * t185 + t207 * t2
     #06 + t254 * t253 + t285 * pi * t32 * t288 * t280 * t41 * t37 / 0.8
     #D1 + t313 * t312 + t320 * pi * t32 * t288 * t315 * t41 * t37 / 0.8
     #D1

      end function



      doubleprecision function rrgq2qght4s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh41J1
      doubleprecision rrgq2qgh41J2
      doubleprecision rrgq2qgh41J3
      doubleprecision rrgq2qgh41J4
      doubleprecision rrgq2qgh41J5
      doubleprecision rrgq2qgh41J6
      doubleprecision rrgq2qgh42J1
      doubleprecision rrgq2qgh42J2
      doubleprecision rrgq2qgh42J3
      doubleprecision rrgq2qgh42J4
      doubleprecision rrgq2qgh42J5
      doubleprecision rrgq2qgh42J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x3
      t14 = rrgq2qgh41J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t28 = x1 ** 2
      t30 = t4 ** 2
      t34 = log(0.4D1 * t22 / t23 * t27 * t28 * t30)
      t38 = (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t1
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
      t68 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t54, t51
     #, t57, -t63)
      t70 = t67 * t68 * t41
      t73 = FJET(XB1, XB2, s, 0.0D0, t51, -t54, t57, -t63, -t64 * t70 / 
     #0.8D1)
      t75 = t1 * t7
      t79 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t80 = t7 * t79
      t84 = rrgq2qgh42J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t93 = -t6 * t80 * t10 / 0.8D1 - t6 * t7 * t84 / 0.8D1 - t38 * t80 
     #/ 0.720D3 - t6 * t80 * t41 / 0.8D1
      t94 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t93)
      t96 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t54, t51
     #, t57, -t63)
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
      t122 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, t109, 0.0D0, -t
     #112, -t113, -t118)
      t124 = t121 * t122 * t10
      t127 = FJET(XB1, XB2, s, t109, -t112, 0.0D0, -t113, -t118, -t64 * 
     #t124 / 0.8D1)
      t132 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, t109, 0.0D0, -t
     #112, -t113, -t118)
      t134 = t121 * t132 * t10
      t137 = FJET(XB1, XB2, s, -t112, t109, -t113, 0.0D0, -t118, -t64 * 
     #t134 / 0.8D1)
      rrgq2qght4s1em1 = t46 * t45 - t73 * pi * t75 * t70 / 0.8D1 + t94 *
     # t93 - t101 * pi * t75 * t98 / 0.8D1 - t127 * pi * t75 * t124 / 0.
     #8D1 - t137 * pi * t75 * t134 / 0.8D1

      end function



      doubleprecision function rrgq2qght4s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh41J1
      doubleprecision rrgq2qgh41J2
      doubleprecision rrgq2qgh41J3
      doubleprecision rrgq2qgh41J4
      doubleprecision rrgq2qgh41J5
      doubleprecision rrgq2qgh41J6
      doubleprecision rrgq2qgh42J1
      doubleprecision rrgq2qgh42J2
      doubleprecision rrgq2qgh42J3
      doubleprecision rrgq2qgh42J4
      doubleprecision rrgq2qgh42J5
      doubleprecision rrgq2qgh42J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrgq2qgh41J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t6 * t7 * t
     #8 / 0.8D1)
      t14 = t1 * t7
      t17 = rrgq2qgh42J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t21 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t6 * t7 * t
     #17 / 0.8D1)
      rrgq2qght4s1em2 = -t12 * pi * t14 * t8 / 0.8D1 - t21 * pi * t14 * 
     #t17 / 0.8D1

      end function



      doubleprecision function rrgq2qght4s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh41J1
      doubleprecision rrgq2qgh41J2
      doubleprecision rrgq2qgh41J3
      doubleprecision rrgq2qgh41J4
      doubleprecision rrgq2qgh41J5
      doubleprecision rrgq2qgh41J6
      doubleprecision rrgq2qgh42J1
      doubleprecision rrgq2qgh42J2
      doubleprecision rrgq2qgh42J3
      doubleprecision rrgq2qgh42J4
      doubleprecision rrgq2qgh42J5
      doubleprecision rrgq2qgh42J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght4s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght4s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh41J1
      doubleprecision rrgq2qgh41J2
      doubleprecision rrgq2qgh41J3
      doubleprecision rrgq2qgh41J4
      doubleprecision rrgq2qgh41J5
      doubleprecision rrgq2qgh41J6
      doubleprecision rrgq2qgh42J1
      doubleprecision rrgq2qgh42J2
      doubleprecision rrgq2qgh42J3
      doubleprecision rrgq2qgh42J4
      doubleprecision rrgq2qgh42J5
      doubleprecision rrgq2qgh42J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh41J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t9 = S34 ** 2
      t12 = S24 ** 2
      rrgq2qgh41J1 = (-0.4D1 * S34 * t2 * S12 - 0.8D1 * S24 * S34 * t2 +
     # (-0.4D1 * t2 * t9 * S34 - 0.4D1 * t12 * t2 * S34) / S12) / pi * w
     #d / z

      end function
  
   
 

      doubleprecision function rrgq2qgh41J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S34 ** 2
      rrgq2qgh41J2 = (0.12D2 * t2 * t3 + 0.4D1 * S24 * S34 * t2 + 0.8D1 
     #* S24 * t3 * t2 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh41J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S34 ** 2
      rrgq2qgh41J3 = (0.12D2 * t2 * t3 + 0.4D1 * S24 * S34 * t2 + 0.8D1 
     #* S24 * t3 * t2 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh41J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S34 ** 2
      rrgq2qgh41J4 = (0.12D2 * t2 * t3 + 0.4D1 * S24 * S34 * t2 + 0.8D1 
     #* S24 * t3 * t2 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh41J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S34 ** 2
      rrgq2qgh41J5 = (0.12D2 * t2 * t3 + 0.4D1 * S24 * S34 * t2 + 0.8D1 
     #* S24 * t3 * t2 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh41J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t6 = S34 ** 2
      t18 = S24 ** 2
      rrgq2qgh41J6 = (0.4D1 * S34 * t2 * S12 + 0.12D2 * t2 * t6 + 0.12D2
     # * S24 * S34 * t2 + (0.4D1 * t2 * t6 * S34 + 0.8D1 * S24 * t6 * t2
     # + 0.4D1 * t18 * t2 * S34) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh42J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S12 ** 2
      t13 = S23 * S24
      t15 = S24 ** 2
      t16 = 0.2D1 * t15
      t17 = S23 ** 2
      rrgq2qgh42J1 = (t2 * t3 + (0.7D1 + (0.2D1 * S24 - 0.3D1 * S23) * t
     #2) * S12 + 0.6D1 * S24 - 0.4D1 * S23 + (-0.4D1 * t13 + t16 + 0.3D1
     # * t17) * t2 + (t17 - 0.2D1 * t13 + t16 + (-t17 * S23 - 0.2D1 * t1
     #5 * S23 + 0.2D1 * S24 * t17) * t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh42J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S12 ** 2
      t11 = S34 ** 2
      t19 = S23 * S24
      t21 = S23 ** 2
      rrgq2qgh42J2 = (t2 * t3 + (0.7D1 + (0.2D1 * S24 - 0.3D1 * S23) * t
     #2) * S12 + 0.2D1 * t2 * t11 + 0.2D1 * S24 * S34 * t2 + 0.6D1 * S24
     # - 0.4D1 * S23 + (-0.4D1 * t19 + 0.3D1 * t21) * t2 + ((0.2D1 - 0.2
     #D1 * S23 * t2) * t11 + (0.2D1 * S24 - 0.2D1 * t19 * t2) * S34 + t2
     #1 - 0.2D1 * t19 + (-t21 * S23 + 0.2D1 * S24 * t21) * t2) / S12) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh42J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S12 ** 2
      t11 = S34 ** 2
      t19 = S23 * S24
      t21 = S23 ** 2
      rrgq2qgh42J3 = (t2 * t3 + (0.7D1 + (0.2D1 * S24 - 0.3D1 * S23) * t
     #2) * S12 + 0.2D1 * t2 * t11 + 0.2D1 * S24 * S34 * t2 + 0.6D1 * S24
     # - 0.4D1 * S23 + (-0.4D1 * t19 + 0.3D1 * t21) * t2 + ((0.2D1 - 0.2
     #D1 * S23 * t2) * t11 + (0.2D1 * S24 - 0.2D1 * t19 * t2) * S34 + t2
     #1 - 0.2D1 * t19 + (-t21 * S23 + 0.2D1 * S24 * t21) * t2) / S12) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh42J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S12 ** 2
      t11 = S34 ** 2
      t19 = S23 * S24
      t21 = S23 ** 2
      rrgq2qgh42J4 = (t2 * t3 + (0.7D1 + (0.2D1 * S24 - 0.3D1 * S23) * t
     #2) * S12 + 0.2D1 * t2 * t11 + 0.2D1 * S24 * S34 * t2 + 0.6D1 * S24
     # - 0.4D1 * S23 + (-0.4D1 * t19 + 0.3D1 * t21) * t2 + ((0.2D1 - 0.2
     #D1 * S23 * t2) * t11 + (0.2D1 * S24 - 0.2D1 * t19 * t2) * S34 + t2
     #1 - 0.2D1 * t19 + (-t21 * S23 + 0.2D1 * S24 * t21) * t2) / S12) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh42J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S12 ** 2
      t11 = S34 ** 2
      t19 = S23 * S24
      t21 = S23 ** 2
      rrgq2qgh42J5 = (t2 * t3 + (0.7D1 + (0.2D1 * S24 - 0.3D1 * S23) * t
     #2) * S12 + 0.2D1 * t2 * t11 + 0.2D1 * S24 * S34 * t2 + 0.6D1 * S24
     # - 0.4D1 * S23 + (-0.4D1 * t19 + 0.3D1 * t21) * t2 + ((0.2D1 - 0.2
     #D1 * S23 * t2) * t11 + (0.2D1 * S24 - 0.2D1 * t19 * t2) * S34 + t2
     #1 - 0.2D1 * t19 + (-t21 * S23 + 0.2D1 * S24 * t21) * t2) / S12) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh42J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S34 ** 2
      t9 = S24 ** 2
      rrgq2qgh42J6 = (0.2D1 * t2 * t3 + 0.2D1 * S24 * S34 * t2 - 0.2D1 *
     # t9 * t2 + ((0.2D1 - 0.2D1 * S23 * t2) * t3 + (0.2D1 * S24 - 0.2D1
     # * S23 * S24 * t2) * S34 - 0.2D1 * t9 + 0.2D1 * t9 * S23 * t2) / S
     #12) / pi * wd / z

      end function
  
 