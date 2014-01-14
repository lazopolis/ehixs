      subroutine rrgg2gght9
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt9
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt9
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhard91J1  
      doubleprecision rrgg2gghhard91J2  
      doubleprecision rrgg2gghhard91J3  
      doubleprecision rrgg2gghhard91J4  
      doubleprecision rrgg2gghhard91J5  
      doubleprecision rrgg2gghhard91J6  
      doubleprecision rrgg2gghhardt9s1e1  
      doubleprecision rrgg2gghhardt9s1e0  
      doubleprecision rrgg2gghhardt9s1em1  
      doubleprecision rrgg2gghhardt9s1em2  
      doubleprecision rrgg2gghhardt9s1em3  
      doubleprecision rrgg2gghhardt9s1em4  
      doubleprecision rrgg2gghhardt9s2e1  
      doubleprecision rrgg2gghhardt9s2e0  
      doubleprecision rrgg2gghhardt9s2em1  
      doubleprecision rrgg2gghhardt9s2em2  
      doubleprecision rrgg2gghhardt9s2em3  
      doubleprecision rrgg2gghhardt9s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt9s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt9s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt9s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt9s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt9s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt9s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt9s1e1
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + x2
      t2 = t1 * s
      t3 = -0.1D1 + z
      t4 = t2 * t3
      t6 = x2 * s * t3
      t7 = s ** 2
      t8 = 0.1D1 / t7
      t9 = pi * t8
      t10 = x2 * x3
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t17 = t16 * t1
      t20 = log(-0.4D1 * t10 * t17)
      t21 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t6, -t4, 0.0
     #D0, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t6, -t4, 0.0
     #D0, 0.0D0, 0.0D0)
      t27 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, t6, -t4, 0.0
     #D0, 0.0D0, 0.0D0)
      t28 = t20 * t21 - t23 * t24 / 0.2D1 - t27
      t31 = pi * lh
      t33 = -t21 + t20 * t24
      t37 = pi ** 2
      t39 = lh ** 2
      t41 = -0.30D2 * t37 + 0.180D3 * t39
      t42 = pi * t41
      t43 = t8 * t24
      t44 = t42 * t43
      t46 = 0.1D1 / x3
      t48 = 0.1D1 / x2
      t50 = (0.90D2 * t9 * t28 - 0.180D3 * t31 * t8 * t33 - t44) * t46 *
     # t48 / 0.1440D4
      t51 = x2 * t13
      t52 = t15 * t1
      t55 = log(-0.4D1 * t51 * t52)
      t57 = -t21 + t55 * t24
      t60 = t55 ** 2
      t63 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, t6, -t4, 0.0
     #D0, 0.0D0, 0.0D0)
      t68 = -t60 * t21 / 0.2D1 - t63 + t60 * t55 * t24 / 0.6D1 + t55 * t
     #27
      t76 = -0.240D3 * zeta3 - 0.120D3 * t39 * lh + 0.60D2 * lh * t37
      t77 = pi * t76
      t78 = t77 * t43
      t82 = t55 * t21 - t60 * t24 / 0.2D1 - t27
      t89 = x1 ** 2
      t90 = t10 * t89
      t93 = log(-0.4D1 * t90 * t17)
      t102 = 0.1D1 / x1
      t103 = t102 * t48
      t105 = (0.90D2 * t9 * (t21 - t93 * t24) - 0.180D3 * t31 * t43) * t
     #46 * t103 / 0.720D3
      t106 = x2 * t89
      t109 = log(-0.4D1 * t106 * t17)
      t111 = t109 ** 2
      t125 = (0.90D2 * t9 * (-t109 * t21 + t111 * t24 / 0.2D1 + t27) - 0
     #.180D3 * t31 * t8 * (t21 - t109 * t24) + t44) * t102 * t48 / 0.720
     #D3
      t126 = -t50 - (t42 * t8 * t57 + 0.90D2 * t9 * t68 - t78 - 0.180D3 
     #* t31 * t8 * t82) * t48 / 0.1440D4 + t105 + t125
      t127 = FJET(XB1, XB2, s, 0.0D0, -t4, 0.0D0, t6, 0.0D0, t126)
      t129 = s * t3
      t130 = -0.1D1 + x3
      t131 = t129 * t130
      t132 = t129 * x3
      t133 = x3 * t89
      t134 = t16 * t130
      t137 = log(-0.4D1 * t133 * t134)
      t138 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t132, -t131
     #, 0.0D0, 0.0D0, 0.0D0)
      t140 = t137 ** 2
      t141 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t132, -t131
     #, 0.0D0, 0.0D0, 0.0D0)
      t144 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, t132, -t131
     #, 0.0D0, 0.0D0, 0.0D0)
      t153 = t8 * t141
      t154 = t42 * t153
      t158 = (0.90D2 * t9 * (t137 * t138 - t140 * t141 / 0.2D1 - t144) -
     # 0.180D3 * t31 * t8 * (-t138 + t137 * t141) - t154) * t46 * t102 /
     # 0.720D3
      t161 = log(-0.4D1 * t90 * t134)
      t171 = (0.90D2 * t9 * (t138 - t161 * t141) - 0.180D3 * t31 * t153)
     # * t46 * t103 / 0.720D3
      t172 = x3 * t13
      t176 = log(-0.4D1 * t172 * t15 * t130)
      t178 = -t138 + t176 * t141
      t181 = t176 ** 2
      t184 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, t132, -t131
     #, 0.0D0, 0.0D0, 0.0D0)
      t189 = -t181 * t138 / 0.2D1 - t184 + t181 * t176 * t141 / 0.6D1 + 
     #t176 * t144
      t192 = t77 * t153
      t196 = t176 * t138 - t181 * t141 / 0.2D1 - t144
      t207 = log(-0.4D1 * t51 * t15 * x3 * t130)
      t209 = t207 ** 2
      t223 = (0.90D2 * t9 * (t207 * t138 - t209 * t141 / 0.2D1 - t144) -
     # 0.180D3 * t31 * t8 * (-t138 + t207 * t141) - t154) * t46 * t48 / 
     #0.1440D4
      t224 = -t158 + t171 - (t42 * t8 * t178 + 0.90D2 * t9 * t189 - t192
     # - 0.180D3 * t31 * t8 * t196) * t46 / 0.1440D4 - t223
      t225 = FJET(XB1, XB2, s, 0.0D0, -t131, 0.0D0, t132, 0.0D0, t224)
      t227 = t130 * s
      t229 = t227 * t3 * x1
      t230 = -0.1D1 + x1
      t231 = t3 * t230
      t232 = t227 * t231
      t233 = x3 * x1
      t234 = t129 * t233
      t236 = x3 * s * t231
      t237 = x1 * z
      t238 = 0.1D1 - x1 + t237
      t239 = 0.1D1 / t238
      t240 = t89 * t239
      t242 = t230 ** 2
      t243 = x3 * t242
      t247 = log(-0.4D1 * t240 * t130 * t16 * t243)
      t248 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t236, t232
     #, t234, -t229, 0.0D0)
      t250 = t247 ** 2
      t251 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t236, t232
     #, t234, -t229, 0.0D0)
      t254 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, -t236, t232
     #, t234, -t229, 0.0D0)
      t263 = t8 * t251
      t274 = log(-0.4D1 * t240 * t130 * t13 * t15 * x2 * t243)
      t285 = -(0.90D2 * t9 * (-t247 * t248 + t250 * t251 / 0.2D1 + t254)
     # - 0.180D3 * t31 * t8 * (t248 - t247 * t251) + t42 * t263) * t46 *
     # t102 / 0.720D3 + (0.90D2 * t9 * (-t248 + t274 * t251) + 0.180D3 *
     # t31 * t263) * t46 * t103 / 0.720D3
      t286 = FJET(XB1, XB2, s, -t229, t232, t234, -t236, 0.0D0, t285)
      t288 = t129 * x1
      t289 = t129 * t230
      t291 = t15 * t239
      t292 = t291 * t242
      t295 = log(0.4D1 * t133 * t13 * t292)
      t296 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t28
     #9, 0.0D0, t288, 0.0D0)
      t298 = t295 ** 2
      t299 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t28
     #9, 0.0D0, t288, 0.0D0)
      t302 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t28
     #9, 0.0D0, t288, 0.0D0)
      t311 = t8 * t299
      t312 = t42 * t311
      t315 = (0.90D2 * t9 * (t295 * t296 - t298 * t299 / 0.2D1 - t302) -
     # 0.180D3 * t31 * t8 * (-t296 + t295 * t299) - t312) * t46 * t102
      t316 = t89 * t13
      t319 = log(0.4D1 * t316 * t292)
      t321 = t296 - t319 * t299
      t324 = t319 ** 2
      t327 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t28
     #9, 0.0D0, t288, 0.0D0)
      t332 = t324 * t296 / 0.2D1 + t327 - t324 * t319 * t299 / 0.6D1 - t
     #319 * t302
      t335 = t77 * t311
      t339 = -t319 * t296 + t324 * t299 / 0.2D1 + t302
      t349 = log(0.4D1 * t90 * t16 * t242 * t239)
      t358 = (0.90D2 * t9 * (t296 - t349 * t299) - 0.180D3 * t31 * t311)
     # * t46 * t103
      t359 = t106 * t13
      t362 = log(0.4D1 * t359 * t292)
      t364 = t362 ** 2
      t377 = (0.90D2 * t9 * (-t362 * t296 + t364 * t299 / 0.2D1 + t302) 
     #- 0.180D3 * t31 * t8 * (t296 - t362 * t299) + t312) * t102 * t48
      t379 = -t315 / 0.720D3 + (t42 * t8 * t321 + 0.90D2 * t9 * t332 + t
     #335 - 0.180D3 * t31 * t8 * t339) * t102 / 0.720D3 + t358 / 0.720D3
     # + t377 / 0.720D3
      t380 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t288, -t289, 0.0D0, t379)
      t405 = (t42 * t8 * t57 + 0.90D2 * t9 * t68 - t78 - 0.180D3 * t31 *
     # t8 * t82) * t48 / 0.1440D4
      t406 = t105 + t125 - (0.90D2 * t9 * t28 - 0.180D3 * t31 * t8 * t33
     # - t44) * t46 * t48 / 0.1440D4 - t405
      t407 = FJET(XB1, XB2, s, -t4, 0.0D0, t6, 0.0D0, 0.0D0, t406)
      t409 = 0.2D1 * t10
      t410 = cos(t11)
      t414 = Sqrt(x2 * t1 * x3 * t130)
      t416 = 0.2D1 * t410 * t414
      t418 = t129 * (-x3 + t409 - x2 + t416)
      t420 = t129 * (0.1D1 - x2 - x3 + t409 + t416)
      t425 = log(0.4D1 * t10 * t13 * t52 * t130)
      t426 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t418, t420
     #, 0.0D0, 0.0D0, 0.0D0)
      t428 = t425 ** 2
      t429 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t418, t420
     #, 0.0D0, 0.0D0, 0.0D0)
      t432 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, -t418, t420
     #, 0.0D0, 0.0D0, 0.0D0)
      t441 = t8 * t429
      t451 = log(0.4D1 * t90 * t16 * t1 * t130)
      t462 = -(0.90D2 * t9 * (-t425 * t426 + t428 * t429 / 0.2D1 + t432)
     # - 0.180D3 * t31 * t8 * (t426 - t425 * t429) + t42 * t441) * t46 *
     # t48 / 0.1440D4 + (0.90D2 * t9 * (-t426 + t451 * t429) + 0.180D3 *
     # t31 * t441) * t46 * t103 / 0.720D3
      t463 = FJET(XB1, XB2, s, -t418, 0.0D0, t420, 0.0D0, 0.0D0, t462)
      t465 = t2 * t231
      t468 = t129 * t230 * x2 * t239
      t469 = t3 ** 2
      t474 = s * t469 * x2 * x1 * t230 * t239
      t475 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t468, t465
     #, 0.0D0, t288, -t474)
      t476 = t10 * t316
      t477 = t242 * t1
      t478 = t291 * t477
      t481 = log(-0.4D1 * t476 * t478)
      t482 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t468, t465
     #, 0.0D0, t288, -t474)
      t487 = t8 * t482
      t492 = (0.90D2 * t9 * (-t475 + t481 * t482) + 0.180D3 * t31 * t487
     #) * t46 * t103
      t495 = log(-0.4D1 * t359 * t478)
      t497 = t495 ** 2
      t500 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, -t468, t465
     #, 0.0D0, t288, -t474)
      t501 = t495 * t475 - t497 * t482 / 0.2D1 - t500
      t505 = -t475 + t495 * t482
      t509 = t42 * t487
      t514 = t492 / 0.720D3 + (0.90D2 * t9 * t501 - 0.180D3 * t31 * t8 *
     # t505 - t509) * t102 * t48 / 0.720D3
      t515 = FJET(XB1, XB2, s, t465, t288, -t468, 0.0D0, -t474, t514)
      t517 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t129
     #, 0.0D0, 0.0D0, 0.0D0)
      t520 = log(0.4D1 * t172 * t15)
      t521 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t129
     #, 0.0D0, 0.0D0, 0.0D0)
      t526 = t520 ** 2
      t529 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t129
     #, 0.0D0, 0.0D0, 0.0D0)
      t533 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t129
     #, 0.0D0, 0.0D0, 0.0D0)
      t538 = t8 * t521
      t539 = t77 * t538
      t551 = log(0.4D1 * t16)
      t552 = t551 ** 2
      t553 = t552 * pi
      t557 = t552 * t551 * pi
      t559 = t551 * pi
      t578 = rrgg2gghhard91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t129
     #, 0.0D0, 0.0D0, 0.0D0)
      t586 = t37 ** 2
      t587 = t39 ** 2
      t595 = t552 ** 2
      t604 = log(0.4D1 * t133 * t16)
      t606 = t604 ** 2
      t617 = t42 * t538
      t622 = t316 * t15
      t624 = log(0.4D1 * t622)
      t629 = t624 ** 2
      t651 = log(0.4D1 * t10 * t622)
      t664 = log(0.4D1 * t106 * t16)
      t666 = t664 ** 2
      t683 = log(0.4D1 * t10 * t16)
      t685 = t683 ** 2
      t702 = log(0.4D1 * t51 * t15)
      t707 = t702 ** 2
      t727 = -(t42 * t8 * (t517 - t520 * t521) + 0.90D2 * t9 * (t526 * t
     #517 / 0.2D1 + t529 - t526 * t520 * t521 / 0.6D1 - t520 * t533) + t
     #539 - 0.180D3 * t31 * t8 * (-t520 * t517 + t526 * t521 / 0.2D1 + t
     #533)) * t46 / 0.1440D4 - (-0.90D2 * t553 * lh + t77 - 0.15D2 * t55
     #7 - t559 * t41) * t8 * t517 / 0.1440D4 - (0.180D3 * t559 * lh + 0.
     #45D2 * t553 + t42) * t8 * t533 / 0.1440D4 - (-0.180D3 * t31 - 0.90
     #D2 * t559) * t8 * t529 / 0.1440D4 - t9 * t578 / 0.16D2 - (0.30D2 *
     # t557 * lh + t553 * t41 / 0.2D1 - t559 * t76 + pi * (t586 + 0.60D2
     # * t587 + 0.480D3 * lh * zeta3 - 0.60D2 * t39 * t37) + 0.15D2 / 0.
     #4D1 * t595 * pi) * t8 * t521 / 0.1440D4 - (0.90D2 * t9 * (-t604 * 
     #t517 + t606 * t521 / 0.2D1 + t533) - 0.180D3 * t31 * t8 * (t517 - 
     #t604 * t521) + t617) * t46 * t102 / 0.720D3 + (t42 * t8 * (-t517 +
     # t624 * t521) + 0.90D2 * t9 * (-t629 * t517 / 0.2D1 - t529 + t629 
     #* t624 * t521 / 0.6D1 + t624 * t533) - t539 - 0.180D3 * t31 * t8 *
     # (t624 * t517 - t629 * t521 / 0.2D1 - t533)) * t102 / 0.720D3 + (0
     #.90D2 * t9 * (-t517 + t651 * t521) + 0.180D3 * t31 * t538) * t46 *
     # t103 / 0.720D3 + (0.90D2 * t9 * (t664 * t517 - t666 * t521 / 0.2D
     #1 - t533) - 0.180D3 * t31 * t8 * (-t517 + t664 * t521) - t617) * t
     #102 * t48 / 0.720D3 - (0.90D2 * t9 * (-t683 * t517 + t685 * t521 /
     # 0.2D1 + t533) - 0.180D3 * t31 * t8 * (t517 - t683 * t521) + t617)
     # * t46 * t48 / 0.1440D4 - (t42 * t8 * (t517 - t702 * t521) + 0.90D
     #2 * t9 * (t707 * t517 / 0.2D1 + t529 - t707 * t702 * t521 / 0.6D1 
     #- t702 * t533) + t539 - 0.180D3 * t31 * t8 * (-t702 * t517 + t707 
     #* t521 / 0.2D1 + t533)) * t48 / 0.1440D4
      t728 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t129, 0.0D0, 0.0D0, t727)
      t730 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t289, t288, 0.0D0, t379)
      t732 = FJET(XB1, XB2, s, 0.0D0, t6, 0.0D0, -t4, 0.0D0, t126)
      t734 = FJET(XB1, XB2, s, t420, 0.0D0, -t418, 0.0D0, 0.0D0, t462)
      t736 = FJET(XB1, XB2, s, -t236, t234, t232, -t229, 0.0D0, t285)
      t738 = FJET(XB1, XB2, s, t234, -t236, -t229, t232, 0.0D0, t285)
      t740 = FJET(XB1, XB2, s, 0.0D0, t132, 0.0D0, -t131, 0.0D0, t224)
      t742 = FJET(XB1, XB2, s, t129, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t727)
      t757 = -t158 + t171 - (t42 * t8 * t178 + 0.90D2 * t9 * t189 - t192
     # - 0.180D3 * t31 * t8 * t196) * t46 / 0.1440D4 - t223
      t758 = FJET(XB1, XB2, s, t132, 0.0D0, -t131, 0.0D0, 0.0D0, t757)
      t760 = t127 * t126 + t225 * t224 + t286 * t285 + t380 * t379 + t40
     #7 * t406 + t463 * t462 + t515 * t514 + t728 * t727 + t730 * t379 +
     # t732 * t126 + t734 * t462 + t736 * t285 + t738 * t285 + t740 * t2
     #24 + t742 * t727 + t758 * t757
      t774 = -t315 / 0.720D3 + (t42 * t8 * t321 + 0.90D2 * t9 * t332 + t
     #335 - 0.180D3 * t31 * t8 * t339) * t102 / 0.720D3 + t358 / 0.720D3
     # + t377 / 0.720D3
      t775 = FJET(XB1, XB2, s, -t289, t288, 0.0D0, 0.0D0, 0.0D0, t774)
      t777 = t105 + t125 - t50 - t405
      t778 = FJET(XB1, XB2, s, t6, 0.0D0, -t4, 0.0D0, 0.0D0, t777)
      t780 = FJET(XB1, XB2, s, -t131, 0.0D0, t132, 0.0D0, 0.0D0, t757)
      t782 = FJET(XB1, XB2, s, 0.0D0, -t468, t288, t465, -t474, t514)
      t784 = FJET(XB1, XB2, s, t288, t465, 0.0D0, -t468, -t474, t514)
      t786 = FJET(XB1, XB2, s, 0.0D0, -t418, 0.0D0, t420, 0.0D0, t462)
      t788 = FJET(XB1, XB2, s, t288, -t289, 0.0D0, 0.0D0, 0.0D0, t774)
      t790 = FJET(XB1, XB2, s, 0.0D0, t420, 0.0D0, -t418, 0.0D0, t462)
      t792 = t233 * z
      t793 = t233 * x2
      t795 = t233 * x2 * z
      t800 = Sqrt(x3 * t1 * t238 * x2 * t130)
      t802 = 0.2D1 * t410 * t800
      t806 = t129 * t230 * (-x3 + t233 - t792 + t409 - t793 + t795 - x2 
     #+ t802) * t239
      t807 = x1 * x2
      t809 = 0.1D1 - x1 + t237 - x2 + t807 - t807 * z - x3 + t233 - t792
     # + t409 - t793 + t795 + t802
      t812 = t129 * t230 * t809 * t239
      t813 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t806, -t812
     #, t234, -t229, -t474)
      t818 = log(0.4D1 * t476 * t291 * t477 * t130)
      t819 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t806, -t812
     #, t234, -t229, -t474)
      t827 = 0.90D2 * t9 * (t813 - t818 * t819) - 0.180D3 * t31 * t8 * t
     #819
      t830 = t827 * t46 * t103 / 0.720D3
      t831 = FJET(XB1, XB2, s, t234, t806, -t229, -t812, -t474, t830)
      t834 = t46 * t102 * t48
      t837 = FJET(XB1, XB2, s, -t229, -t812, t234, t806, -t474, t830)
      t841 = FJET(XB1, XB2, s, -t812, -t229, t806, t234, -t474, t830)
      t856 = t492 / 0.720D3 + (0.90D2 * t9 * t501 - 0.180D3 * t31 * t8 *
     # t505 - t509) * t102 * t48 / 0.720D3
      t857 = FJET(XB1, XB2, s, -t468, 0.0D0, t465, t288, -t474, t856)
      t859 = FJET(XB1, XB2, s, 0.0D0, t129, 0.0D0, 0.0D0, 0.0D0, t727)
      t861 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t129, 0.0D0, t727)
      t863 = FJET(XB1, XB2, s, t232, -t229, -t236, t234, 0.0D0, t285)
      t865 = FJET(XB1, XB2, s, t806, t234, -t812, -t229, -t474, t830)
      t869 = t775 * t774 + t778 * t777 + t780 * t757 + t782 * t514 + t78
     #4 * t514 + t786 * t462 + t788 * t774 + t790 * t462 + t831 * t827 *
     # t834 / 0.720D3 + t837 * t827 * t834 / 0.720D3 + t841 * t827 * t83
     #4 / 0.720D3 + t857 * t856 + t859 * t727 + t861 * t727 + t863 * t28
     #5 + t865 * t827 * t834 / 0.720D3
      rrgg2gghhardt9s1e1 = t760 + t869

      end function



      doubleprecision function rrgg2gghhardt9s1e0
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + x3
      t2 = t1 * s
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = s * t3
      t7 = -0.1D1 + x1
      t8 = x1 * z
      t9 = x1 * x2
      t11 = x3 * x1
      t12 = t11 * z
      t13 = x2 * x3
      t14 = 0.2D1 * t13
      t15 = t11 * x2
      t17 = t11 * x2 * z
      t18 = x4 * pi
      t19 = cos(t18)
      t20 = -0.1D1 + x2
      t22 = 0.1D1 - x1 + t8
      t26 = Sqrt(x3 * t20 * t22 * x2 * t1)
      t28 = 0.2D1 * t19 * t26
      t29 = 0.1D1 - x1 + t8 - x2 + t9 - t9 * z - x3 + t11 - t12 + t14 - 
     #t15 + t17 + t28
      t31 = 0.1D1 / t22
      t33 = t6 * t7 * t29 * t31
      t34 = t6 * t11
      t38 = t6 * t7 * (-x3 + t11 - t12 + t14 - t15 + t17 - x2 + t28) * t
     #31
      t39 = t3 ** 2
      t44 = s * t39 * x2 * x1 * t7 * t31
      t45 = s ** 2
      t46 = 0.1D1 / t45
      t47 = pi * t46
      t48 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t38, -t33, t
     #34, -t5, -t44)
      t50 = 0.1D1 / x3
      t51 = 0.1D1 / x1
      t53 = 0.1D1 / x2
      t54 = t50 * t51 * t53
      t56 = t47 * t48 * t54 / 0.8D1
      t57 = FJET(XB1, XB2, s, -t5, -t33, t34, t38, -t44, t56)
      t62 = t48 * t50 * t51 * t53
      t65 = FJET(XB1, XB2, s, t38, t34, -t33, -t5, -t44, t56)
      t70 = FJET(XB1, XB2, s, t34, t38, -t5, -t33, -t44, t56)
      t75 = FJET(XB1, XB2, s, -t33, -t5, t38, t34, -t44, t56)
      t80 = Sin(t18)
      t81 = t80 ** 2
      t82 = x3 * t81
      t83 = z ** 2
      t84 = 0.1D1 / t83
      t87 = log(0.4D1 * t82 * t84)
      t88 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0
     #.0D0, 0.0D0, 0.0D0)
      t90 = t87 ** 2
      t91 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0
     #.0D0, 0.0D0, 0.0D0)
      t94 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 0
     #.0D0, 0.0D0, 0.0D0)
      t98 = pi * lh
      t104 = pi ** 2
      t106 = lh ** 2
      t108 = -0.30D2 * t104 + 0.180D3 * t106
      t109 = pi * t108
      t110 = t46 * t91
      t111 = t109 * t110
      t115 = t84 * t81
      t117 = log(0.4D1 * t115)
      t118 = t117 * pi
      t121 = t117 ** 2
      t122 = t121 * pi
      t128 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t6, 
     #0.0D0, 0.0D0, 0.0D0)
      t156 = log(0.4D1 * t13 * t115)
      t162 = 0.180D3 * t98 * t110
      t167 = x2 * t81
      t170 = log(0.4D1 * t167 * t84)
      t172 = t170 ** 2
      t186 = x1 ** 2
      t187 = x3 * t186
      t190 = log(0.4D1 * t187 * t115)
      t202 = x2 * t186
      t205 = log(0.4D1 * t202 * t115)
      t214 = t186 * t81
      t217 = log(0.4D1 * t214 * t84)
      t219 = t217 ** 2
      t233 = -(0.90D2 * t47 * (-t87 * t88 + t90 * t91 / 0.2D1 + t94) - 0
     #.180D3 * t98 * t46 * (t88 - t87 * t91) + t111) * t50 / 0.1440D4 - 
     #(0.180D3 * t118 * lh + 0.45D2 * t122 + t109) * t46 * t88 / 0.1440D
     #4 - t47 * t128 / 0.16D2 - (-0.90D2 * t122 * lh + pi * (-0.240D3 * 
     #zeta3 - 0.120D3 * t106 * lh + 0.60D2 * lh * t104) - 0.15D2 * t121 
     #* t117 * pi - t118 * t108) * t46 * t91 / 0.1440D4 - (-0.180D3 * t9
     #8 - 0.90D2 * t118) * t46 * t94 / 0.1440D4 - (0.90D2 * t47 * (t88 -
     # t156 * t91) - t162) * t50 * t53 / 0.1440D4 - (0.90D2 * t47 * (-t1
     #70 * t88 + t172 * t91 / 0.2D1 + t94) - 0.180D3 * t98 * t46 * (t88 
     #- t170 * t91) + t111) * t53 / 0.1440D4 - (0.90D2 * t47 * (t88 - t1
     #90 * t91) - t162) * t50 * t51 / 0.720D3 - t47 * t91 * t54 / 0.8D1 
     #+ (0.90D2 * t47 * (-t88 + t205 * t91) + t162) * t51 * t53 / 0.720D
     #3 + (0.90D2 * t47 * (t217 * t88 - t219 * t91 / 0.2D1 - t94) - 0.18
     #0D3 * t98 * t46 * (-t88 + t217 * t91) - t111) * t51 / 0.720D3
      t234 = FJET(XB1, XB2, s, t6, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t233)
      t236 = t3 * t7
      t237 = t2 * t236
      t239 = x3 * s * t236
      t240 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t239, t237
     #, t34, -t5, 0.0D0)
      t243 = t7 ** 2
      t248 = log(-0.4D1 * t186 * t31 * t1 * t115 * x3 * t243)
      t249 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t239, t237
     #, t34, -t5, 0.0D0)
      t264 = -(0.90D2 * t47 * (t240 - t248 * t249) - 0.180D3 * t98 * t46
     # * t249) * t50 * t51 / 0.720D3 - t47 * t249 * t54 / 0.8D1
      t265 = FJET(XB1, XB2, s, t237, -t5, -t239, t34, 0.0D0, t264)
      t267 = t6 * t1
      t268 = t6 * x3
      t269 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t268, -t267
     #, 0.0D0, 0.0D0, 0.0D0)
      t274 = log(-0.4D1 * t167 * t84 * x3 * t1)
      t275 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t268, -t267
     #, 0.0D0, 0.0D0, 0.0D0)
      t280 = t46 * t275
      t282 = 0.180D3 * t98 * t280
      t286 = (0.90D2 * t47 * (-t269 + t274 * t275) + t282) * t50 * t53 /
     # 0.1440D4
      t290 = log(-0.4D1 * t82 * t84 * t1)
      t292 = t290 ** 2
      t295 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, t268, -t267
     #, 0.0D0, 0.0D0, 0.0D0)
      t296 = t290 * t269 - t292 * t275 / 0.2D1 - t295
      t300 = -t269 + t290 * t275
      t304 = t109 * t280
      t311 = log(-0.4D1 * t187 * t115 * t1)
      t319 = (0.90D2 * t47 * (-t269 + t311 * t275) + t282) * t50 * t51 /
     # 0.720D3
      t322 = t47 * t275 * t54 / 0.8D1
      t323 = -t286 - (0.90D2 * t47 * t296 - 0.180D3 * t98 * t46 * t300 -
     # t304) * t50 / 0.1440D4 - t319 + t322
      t324 = FJET(XB1, XB2, s, 0.0D0, -t267, 0.0D0, t268, 0.0D0, t323)
      t326 = FJET(XB1, XB2, s, -t5, t237, t34, -t239, 0.0D0, t264)
      t328 = FJET(XB1, XB2, s, t34, -t239, -t5, t237, 0.0D0, t264)
      t332 = t6 * t7 * x2 * t31
      t333 = t6 * x1
      t334 = t20 * s
      t335 = t334 * t236
      t336 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t332, t335
     #, 0.0D0, t333, -t44)
      t339 = t47 * t336 * t54 / 0.8D1
      t340 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t332, t335
     #, 0.0D0, t333, -t44)
      t341 = t202 * t81
      t342 = t84 * t31
      t347 = log(-0.4D1 * t341 * t342 * t243 * t20)
      t349 = -t340 + t347 * t336
      t354 = 0.180D3 * t98 * t46 * t336
      t359 = -t339 + (0.90D2 * t47 * t349 + t354) * t51 * t53 / 0.720D3
      t360 = FJET(XB1, XB2, s, 0.0D0, -t332, t333, t335, -t44, t359)
      t362 = FJET(XB1, XB2, s, 0.0D0, t268, 0.0D0, -t267, 0.0D0, t323)
      t364 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t6, 0.0D0, t233)
      t369 = Sqrt(x2 * t20 * x3 * t1)
      t371 = 0.2D1 * t19 * t369
      t373 = t6 * (0.1D1 - x2 - x3 + t14 + t371)
      t375 = t6 * (-x3 + t14 - x2 + t371)
      t376 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t375, t373
     #, 0.0D0, 0.0D0, 0.0D0)
      t380 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t375, t373
     #, 0.0D0, 0.0D0, 0.0D0)
      t382 = t84 * t20
      t386 = log(0.4D1 * t13 * t81 * t382 * t1)
      t398 = -t47 * t376 * t54 / 0.8D1 - (0.90D2 * t47 * (t380 - t386 * 
     #t376) - 0.180D3 * t98 * t46 * t376) * t50 * t53 / 0.1440D4
      t399 = FJET(XB1, XB2, s, t373, 0.0D0, -t375, 0.0D0, 0.0D0, t398)
      t401 = FJET(XB1, XB2, s, 0.0D0, -t375, 0.0D0, t373, 0.0D0, t398)
      t410 = -t339 + (0.90D2 * t47 * t349 + t354) * t51 * t53 / 0.720D3
      t411 = FJET(XB1, XB2, s, -t332, 0.0D0, t335, t333, -t44, t410)
      t413 = t334 * t3
      t415 = x2 * s * t3
      t416 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t415, -t413
     #, 0.0D0, 0.0D0, 0.0D0)
      t417 = t115 * t20
      t420 = log(-0.4D1 * t13 * t417)
      t421 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t415, -t413
     #, 0.0D0, 0.0D0, 0.0D0)
      t423 = t416 - t420 * t421
      t426 = t46 * t421
      t428 = 0.180D3 * t98 * t426
      t435 = log(-0.4D1 * t167 * t382)
      t437 = t435 ** 2
      t440 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, t415, -t413
     #, 0.0D0, 0.0D0, 0.0D0)
      t441 = -t435 * t416 + t437 * t421 / 0.2D1 + t440
      t445 = t416 - t435 * t421
      t449 = t109 * t426
      t452 = (-0.90D2 * t47 * t441 + 0.180D3 * t98 * t46 * t445 - t449) 
     #* t53 / 0.1440D4
      t455 = t47 * t421 * t54 / 0.8D1
      t458 = log(-0.4D1 * t202 * t417)
      t466 = (0.90D2 * t47 * (t416 - t458 * t421) - t428) * t51 * t53 / 
     #0.720D3
      t467 = -(-0.90D2 * t47 * t423 + t428) * t50 * t53 / 0.1440D4 - t45
     #2 + t455 + t466
      t468 = FJET(XB1, XB2, s, -t413, 0.0D0, t415, 0.0D0, 0.0D0, t467)
      t470 = t57 * pi * t46 * t62 / 0.8D1 + t65 * pi * t46 * t62 / 0.8D1
     # + t70 * pi * t46 * t62 / 0.8D1 + t75 * pi * t46 * t62 / 0.8D1 + t
     #234 * t233 + t265 * t264 + t324 * t323 + t326 * t264 + t328 * t264
     # + t360 * t359 + t362 * t323 + t364 * t233 + t399 * t398 + t401 * 
     #t398 + t411 * t410 + t468 * t467
      t471 = FJET(XB1, XB2, s, -t239, t34, t237, -t5, 0.0D0, t264)
      t473 = t6 * t7
      t474 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t47
     #3, 0.0D0, t333, 0.0D0)
      t476 = t342 * t243
      t479 = log(0.4D1 * t187 * t81 * t476)
      t480 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t47
     #3, 0.0D0, t333, 0.0D0)
      t485 = t46 * t480
      t487 = 0.180D3 * t98 * t485
      t491 = (0.90D2 * t47 * (-t474 + t479 * t480) + t487) * t50 * t51 /
     # 0.720D3
      t494 = t47 * t480 * t54 / 0.8D1
      t497 = log(0.4D1 * t341 * t476)
      t505 = (0.90D2 * t47 * (t474 - t497 * t480) - t487) * t51 * t53 / 
     #0.720D3
      t508 = log(0.4D1 * t214 * t476)
      t510 = t508 ** 2
      t513 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t47
     #3, 0.0D0, t333, 0.0D0)
      t514 = t508 * t474 - t510 * t480 / 0.2D1 - t513
      t518 = -t474 + t508 * t480
      t522 = t109 * t485
      t526 = -t491 + t494 + t505 + (-0.90D2 * t47 * t514 + 0.180D3 * t98
     # * t46 * t518 + t522) * t51 / 0.720D3
      t527 = FJET(XB1, XB2, s, t333, -t473, 0.0D0, 0.0D0, 0.0D0, t526)
      t529 = FJET(XB1, XB2, s, -t473, t333, 0.0D0, 0.0D0, 0.0D0, t526)
      t541 = -t491 + t494 + t505 + (-0.90D2 * t47 * t514 + 0.180D3 * t98
     # * t46 * t518 + t522) * t51 / 0.720D3
      t542 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t333, -t473, 0.0D0, t541)
      t550 = (-0.90D2 * t47 * t423 + t428) * t50 * t53 / 0.1440D4
      t561 = t455 + t466 - t550 - (-0.90D2 * t47 * t441 + 0.180D3 * t98 
     #* t46 * t445 - t449) * t53 / 0.1440D4
      t562 = FJET(XB1, XB2, s, 0.0D0, -t413, 0.0D0, t415, 0.0D0, t561)
      t564 = FJET(XB1, XB2, s, 0.0D0, t373, 0.0D0, -t375, 0.0D0, t398)
      t566 = FJET(XB1, XB2, s, 0.0D0, t6, 0.0D0, 0.0D0, 0.0D0, t233)
      t568 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t473, t333, 0.0D0, t541)
      t570 = -t550 - t452 + t455 + t466
      t571 = FJET(XB1, XB2, s, t415, 0.0D0, -t413, 0.0D0, 0.0D0, t570)
      t573 = FJET(XB1, XB2, s, -t375, 0.0D0, t373, 0.0D0, 0.0D0, t398)
      t575 = FJET(XB1, XB2, s, 0.0D0, t415, 0.0D0, -t413, 0.0D0, t561)
      t577 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t6, 0.0D0, 0.0D0, t233)
      t589 = -t286 - (0.90D2 * t47 * t296 - 0.180D3 * t98 * t46 * t300 -
     # t304) * t50 / 0.1440D4 - t319 + t322
      t590 = FJET(XB1, XB2, s, t268, 0.0D0, -t267, 0.0D0, 0.0D0, t589)
      t592 = FJET(XB1, XB2, s, -t267, 0.0D0, t268, 0.0D0, 0.0D0, t589)
      t594 = FJET(XB1, XB2, s, t333, t335, 0.0D0, -t332, -t44, t359)
      t596 = FJET(XB1, XB2, s, t335, t333, -t332, 0.0D0, -t44, t359)
      t598 = t471 * t264 + t527 * t526 + t529 * t526 + t542 * t541 + t56
     #2 * t561 + t564 * t398 + t566 * t233 + t568 * t541 + t571 * t570 +
     # t573 * t398 + t575 * t561 + t577 * t233 + t590 * t589 + t592 * t5
     #89 + t594 * t359 + t596 * t359
      rrgg2gghhardt9s1e0 = t470 + t598

      end function



      doubleprecision function rrgg2gghhardt9s1em1
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t15 = log(0.4D1 * t10 * t12)
      t16 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t21 = pi * lh
      t24 = 0.180D3 * t21 * t4 * t16
      t26 = 0.1D1 / x3
      t32 = log(0.4D1 * t12 * t9)
      t33 = t32 * pi
      t41 = t32 ** 2
      t44 = pi ** 2
      t46 = lh ** 2
      t54 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t57 = t16 * t26
      t58 = 0.1D1 / x2
      t62 = x2 * t9
      t65 = log(0.4D1 * t62 * t12)
      t73 = 0.1D1 / x1
      t78 = x1 ** 2
      t79 = t78 * t9
      t82 = log(0.4D1 * t79 * t12)
      t93 = -(0.90D2 * t5 * (t6 - t15 * t16) - t24) * t26 / 0.1440D4 - (
     #-0.180D3 * t21 - 0.90D2 * t33) * t4 * t6 / 0.1440D4 - (0.180D3 * t
     #33 * lh + 0.45D2 * t41 * pi + pi * (-0.30D2 * t44 + 0.180D3 * t46)
     #) * t4 * t16 / 0.1440D4 - t5 * t54 / 0.16D2 - t5 * t57 * t58 / 0.1
     #6D2 - (0.90D2 * t5 * (t6 - t65 * t16) - t24) * t58 / 0.1440D4 - t5
     # * t16 * t73 * t58 / 0.8D1 + (0.90D2 * t5 * (-t6 + t82 * t16) + t2
     #4) * t73 / 0.720D3 - t5 * t57 * t73 / 0.8D1
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t93)
      t96 = t2 * x3
      t97 = -0.1D1 + x3
      t98 = t2 * t97
      t99 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t96, -t98, 0
     #.0D0, 0.0D0, 0.0D0)
      t103 = log(-0.4D1 * t10 * t12 * t97)
      t104 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t96, -t98, 
     #0.0D0, 0.0D0, 0.0D0)
      t106 = t99 - t103 * t104
      t111 = 0.180D3 * t21 * t4 * t104
      t115 = t104 * t26
      t118 = t5 * t115 * t58 / 0.16D2
      t121 = t5 * t115 * t73 / 0.8D1
      t122 = -(-0.90D2 * t5 * t106 + t111) * t26 / 0.1440D4 + t118 + t12
     #1
      t123 = FJET(XB1, XB2, s, t96, 0.0D0, -t98, 0.0D0, 0.0D0, t122)
      t131 = -(-0.90D2 * t5 * t106 + t111) * t26 / 0.1440D4 + t118 + t12
     #1
      t132 = FJET(XB1, XB2, s, 0.0D0, t96, 0.0D0, -t98, 0.0D0, t131)
      t134 = t2 * x1
      t135 = -0.1D1 + x1
      t136 = t2 * t135
      t137 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t13
     #6, 0.0D0, t134, 0.0D0)
      t141 = t5 * t137 * t73 * t58 / 0.8D1
      t142 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t13
     #6, 0.0D0, t134, 0.0D0)
      t145 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t147 = t135 ** 2
      t151 = log(0.4D1 * t79 * t12 * t145 * t147)
      t153 = t142 - t151 * t137
      t158 = 0.180D3 * t21 * t4 * t137
      t165 = t5 * t137 * t26 * t73 / 0.8D1
      t166 = t141 + (0.90D2 * t5 * t153 - t158) * t73 / 0.720D3 + t165
      t167 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t134, -t136, 0.0D0, t166)
      t169 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t93)
      t177 = t141 + (0.90D2 * t5 * t153 - t158) * t73 / 0.720D3 + t165
      t178 = FJET(XB1, XB2, s, t134, -t136, 0.0D0, 0.0D0, 0.0D0, t177)
      t180 = FJET(XB1, XB2, s, -t98, 0.0D0, t96, 0.0D0, 0.0D0, t122)
      t183 = x2 * s * t1
      t184 = -0.1D1 + x2
      t185 = t184 * s
      t186 = t185 * t1
      t187 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t183, -t186
     #, 0.0D0, 0.0D0, 0.0D0)
      t191 = t5 * t187 * t73 * t58 / 0.8D1
      t195 = t5 * t187 * t26 * t58 / 0.16D2
      t196 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t183, -t186
     #, 0.0D0, 0.0D0, 0.0D0)
      t200 = log(-0.4D1 * t62 * t12 * t184)
      t202 = -t196 + t200 * t187
      t207 = 0.180D3 * t21 * t4 * t187
      t211 = t191 + t195 - (0.90D2 * t5 * t202 + t207) * t58 / 0.1440D4
      t212 = FJET(XB1, XB2, s, 0.0D0, t183, 0.0D0, -t186, 0.0D0, t211)
      t214 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t93)
      t216 = FJET(XB1, XB2, s, 0.0D0, -t98, 0.0D0, t96, 0.0D0, t131)
      t224 = t191 + t195 - (0.90D2 * t5 * t202 + t207) * t58 / 0.1440D4
      t225 = FJET(XB1, XB2, s, -t186, 0.0D0, t183, 0.0D0, 0.0D0, t224)
      t227 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t93)
      t229 = t1 * t135
      t230 = t185 * t229
      t233 = t2 * t135 * x2 * t145
      t234 = t1 ** 2
      t239 = s * t234 * x2 * x1 * t135 * t145
      t240 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t233, t230
     #, 0.0D0, t134, -t239)
      t242 = t240 * t73 * t58
      t244 = t5 * t242 / 0.8D1
      t245 = FJET(XB1, XB2, s, t230, t134, -t233, 0.0D0, -t239, -t244)
      t251 = 0.2D1 * x2 * x3
      t252 = cos(t7)
      t256 = Sqrt(x2 * t184 * x3 * t97)
      t258 = 0.2D1 * t252 * t256
      t260 = t2 * (-x3 + t251 - x2 + t258)
      t262 = t2 * (0.1D1 - x2 - x3 + t251 + t258)
      t263 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t260, t262
     #, 0.0D0, 0.0D0, 0.0D0)
      t265 = t263 * t26 * t58
      t267 = t5 * t265 / 0.16D2
      t268 = FJET(XB1, XB2, s, 0.0D0, -t260, 0.0D0, t262, 0.0D0, -t267)
      t273 = t94 * t93 + t123 * t122 + t132 * t131 + t167 * t166 + t169 
     #* t93 + t178 * t177 + t180 * t122 + t212 * t211 + t214 * t93 + t21
     #6 * t131 + t225 * t224 + t227 * t93 - t245 * pi * t4 * t242 / 0.8D
     #1 - t268 * pi * t4 * t265 / 0.16D2
      t275 = x3 * s * t229
      t277 = t2 * x1 * x3
      t278 = t97 * s
      t279 = t278 * t229
      t281 = t278 * t1 * x1
      t282 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t275, t279
     #, t277, -t281, 0.0D0)
      t284 = t282 * t26 * t73
      t286 = t5 * t284 / 0.8D1
      t287 = FJET(XB1, XB2, s, -t275, t277, t279, -t281, 0.0D0, -t286)
      t292 = FJET(XB1, XB2, s, -t281, t279, t277, -t275, 0.0D0, -t286)
      t297 = FJET(XB1, XB2, s, t279, -t281, -t275, t277, 0.0D0, -t286)
      t302 = FJET(XB1, XB2, s, t262, 0.0D0, -t260, 0.0D0, 0.0D0, -t267)
      t307 = FJET(XB1, XB2, s, 0.0D0, -t233, t134, t230, -t239, -t244)
      t312 = FJET(XB1, XB2, s, 0.0D0, t262, 0.0D0, -t260, 0.0D0, -t267)
      t317 = FJET(XB1, XB2, s, t134, t230, 0.0D0, -t233, -t239, -t244)
      t322 = FJET(XB1, XB2, s, t277, -t275, -t281, t279, 0.0D0, -t286)
      t327 = FJET(XB1, XB2, s, -t260, 0.0D0, t262, 0.0D0, 0.0D0, -t267)
      t332 = FJET(XB1, XB2, s, 0.0D0, -t186, 0.0D0, t183, 0.0D0, t211)
      t334 = FJET(XB1, XB2, s, -t136, t134, 0.0D0, 0.0D0, 0.0D0, t177)
      t336 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t136, t134, 0.0D0, t166)
      t338 = FJET(XB1, XB2, s, t183, 0.0D0, -t186, 0.0D0, 0.0D0, t224)
      t340 = FJET(XB1, XB2, s, -t233, 0.0D0, t230, t134, -t239, -t244)
      t345 = -t287 * pi * t4 * t284 / 0.8D1 - t292 * pi * t4 * t284 / 0.
     #8D1 - t297 * pi * t4 * t284 / 0.8D1 - t302 * pi * t4 * t265 / 0.16
     #D2 - t307 * pi * t4 * t242 / 0.8D1 - t312 * pi * t4 * t265 / 0.16D
     #2 - t317 * pi * t4 * t242 / 0.8D1 - t322 * pi * t4 * t284 / 0.8D1 
     #- t327 * pi * t4 * t265 / 0.16D2 + t332 * t211 + t334 * t177 + t33
     #6 * t166 + t338 * t224 - t340 * pi * t4 * t242 / 0.8D1
      rrgg2gghhardt9s1em1 = t273 + t345

      end function



      doubleprecision function rrgg2gghhardt9s1em2
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t24 = z ** 2
      t27 = Sin(x4 * pi)
      t28 = t27 ** 2
      t31 = log(0.4D1 / t24 * t28)
      t38 = -t5 * t6 * t7 / 0.16D2 - t5 * t6 * t11 / 0.16D2 - t5 * t6 * 
     #t15 / 0.8D1 - t5 * t19 / 0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t
     #31 * pi) * t4 * t6 / 0.1440D4
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t38)
      t43 = t2 * x1
      t45 = t2 * (-0.1D1 + x1)
      t46 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t45,
     # 0.0D0, t43, 0.0D0)
      t49 = t5 * t46 * t15 / 0.8D1
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t49)
      t53 = t4 * t46 * t15
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t49)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t62 = t2 * x3
      t64 = t2 * (-0.1D1 + x3)
      t65 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t62, -t64, 0
     #.0D0, 0.0D0, 0.0D0)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t64, 0.0D0, t68)
      t72 = t4 * t65 * t7
      t76 = x2 * s * t1
      t79 = (-0.1D1 + x2) * s * t1
      t80 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t76, -t79, 0
     #.0D0, 0.0D0, 0.0D0)
      t83 = t5 * t80 * t11 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t76, 0.0D0, -t79, 0.0D0, t83)
      t87 = t4 * t80 * t11
      t90 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t62, 0.0D0, t68)
      t94 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t76, 0.0D0, t83)
      t98 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t100 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t49)
      t104 = FJET(XB1, XB2, s, t62, 0.0D0, -t64, 0.0D0, 0.0D0, t68)
      t108 = FJET(XB1, XB2, s, t76, 0.0D0, -t79, 0.0D0, 0.0D0, t83)
      t112 = FJET(XB1, XB2, s, -t64, 0.0D0, t62, 0.0D0, 0.0D0, t68)
      t116 = FJET(XB1, XB2, s, -t79, 0.0D0, t76, 0.0D0, 0.0D0, t83)
      t120 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t49)
      rrgg2gghhardt9s1em2 = t39 * t38 + t41 * t38 + t50 * pi * t53 / 0.8
     #D1 + t56 * pi * t53 / 0.8D1 + t60 * t38 + t69 * pi * t72 / 0.16D2 
     #+ t84 * pi * t87 / 0.16D2 + t90 * pi * t72 / 0.16D2 + t94 * pi * t
     #87 / 0.16D2 + t98 * t38 + t100 * pi * t53 / 0.8D1 + t104 * pi * t7
     #2 / 0.16D2 + t108 * pi * t87 / 0.16D2 + t112 * pi * t72 / 0.16D2 +
     # t116 * pi * t87 / 0.16D2 + t120 * pi * t53 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt9s1em3
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gghhardt9s1em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0
     #.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt9s1em4
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt9s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt9s2e1
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t24 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t28 = pi * lh
      t34 = pi ** 2
      t36 = lh ** 2
      t38 = -0.30D2 * t34 + 0.180D3 * t36
      t39 = pi * t38
      t40 = t4 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t6 * t13
      t49 = t48 * t10
      t51 = log(0.4D1 * t49)
      t56 = t51 ** 2
      t59 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t72 = -0.240D3 * zeta3 - 0.120D3 * t36 * lh + 0.60D2 * lh * t34
      t73 = pi * t72
      t74 = t73 * t40
      t85 = x2 * x3
      t86 = t85 * t6
      t87 = -0.1D1 + x2
      t88 = t14 * t87
      t91 = log(-0.4D1 * t86 * t88)
      t95 = log(0.4D1 * t85 * t49)
      t100 = 0.1D1 / x2
      t101 = t43 * t45 * t100
      t104 = x2 * t6
      t107 = log(0.4D1 * t104 * t14)
      t109 = t107 ** 2
      t114 = log(-0.4D1 * t104 * t88)
      t116 = t114 ** 2
      t132 = x3 * t10
      t135 = log(0.4D1 * t132 * t13)
      t140 = t135 ** 2
      t162 = log(-0.4D1 * t85 * t88)
      t164 = t162 ** 2
      t169 = log(0.4D1 * t85 * t14)
      t171 = t169 ** 2
      t192 = x2 * t10
      t196 = log(-0.4D1 * t192 * t13 * t87)
      t197 = t196 ** 2
      t200 = log(0.4D1 * t192 * t13)
      t201 = t200 ** 2
      t224 = log(0.4D1 * t14)
      t225 = t224 ** 2
      t228 = t225 * t224
      t247 = t34 ** 2
      t248 = t36 ** 2
      t261 = rrgg2gghhard91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, 0.0D0, t2, 0.0D0)
      t262 = t225 ** 2
      t271 = (0.90D2 * t5 * (t17 * t18 - t20 * t21 / 0.2D1 - t24) - 0.18
     #0D3 * t28 * t4 * (-t18 + t17 * t21) - t41) * t43 * t45 / 0.720D3 -
     # (t39 * t4 * (t18 - t51 * t21) + 0.90D2 * t5 * (t56 * t18 / 0.2D1 
     #+ t59 - t56 * t51 * t21 / 0.6D1 - t51 * t24) + t74 - 0.180D3 * t28
     # * t4 * (-t51 * t18 + t56 * t21 / 0.2D1 + t24)) * t45 / 0.720D3 + 
     #t5 * (-t91 * t21 + t95 * t21) * t101 / 0.8D1 - (0.90D2 * t5 * (-t1
     #07 * t18 + t109 * t21 / 0.2D1 + t114 * t18 - t116 * t21 / 0.2D1) -
     # 0.180D3 * t28 * t4 * (-t107 * t21 + t114 * t21)) * t45 * t100 / 0
     #.720D3 + (t39 * t4 * (-t18 + t135 * t21) + 0.90D2 * t5 * (-t140 * 
     #t18 / 0.2D1 - t59 + t140 * t135 * t21 / 0.6D1 + t135 * t24) - t74 
     #- 0.180D3 * t28 * t4 * (t135 * t18 - t140 * t21 / 0.2D1 - t24)) * 
     #t43 / 0.1440D4 - (0.90D2 * t5 * (t162 * t18 - t164 * t21 / 0.2D1 -
     # t169 * t18 + t171 * t21 / 0.2D1) - 0.180D3 * t28 * t4 * (t162 * t
     #21 - t169 * t21)) * t43 * t100 / 0.1440D4 - ((0.90D2 * t5 * t18 - 
     #0.180D3 * t28 * t40) * (-t197 / 0.2D1 + t201 / 0.2D1) + 0.90D2 * t
     #5 * t21 * (-t201 * t200 / 0.6D1 + t197 * t196 / 0.6D1) + (-0.180D3
     # * t28 * t4 * t18 + t41 + 0.90D2 * t5 * t24) * (t196 - t200)) * t1
     #00 / 0.1440D4 - (-0.180D3 * (t225 * t18 / 0.2D1 + t59 - t228 * t21
     # / 0.6D1 - t224 * t24) * pi * lh + (-t224 * t18 + t225 * t21 / 0.2
     #D1 + t24) * pi * t38 + (t18 - t224 * t21) * pi * t72 + t21 * pi * 
     #(t247 + 0.60D2 * t248 + 0.480D3 * lh * zeta3 - 0.60D2 * t36 * t34)
     # + 0.90D2 * (-t228 * t18 / 0.6D1 + t225 * t24 / 0.2D1 - t224 * t59
     # + t261 + t262 * t21 / 0.24D2) * pi) * t4 / 0.1440D4
      t272 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t271)
      t274 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t271)
      t276 = t2 * x1
      t277 = -0.1D1 + x1
      t278 = t2 * t277
      t279 = t7 * t13
      t280 = 0.1D1 / t8
      t281 = x1 * z
      t282 = -z - x1 + t281
      t283 = 0.1D1 / t282
      t284 = t280 * t283
      t285 = t277 ** 2
      t286 = t284 * t285
      t289 = log(-0.4D1 * t279 * t286)
      t290 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t276
     #, 0.0D0, -t278, 0.0D0)
      t292 = t289 ** 2
      t293 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t276
     #, 0.0D0, -t278, 0.0D0)
      t296 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t276
     #, 0.0D0, -t278, 0.0D0)
      t305 = t4 * t293
      t306 = t39 * t305
      t309 = (0.90D2 * t5 * (-t289 * t290 + t292 * t293 / 0.2D1 + t296) 
     #- 0.180D3 * t28 * t4 * (t290 - t289 * t293) + t306) * t43 * t45
      t312 = log(-0.4D1 * t48 * t286)
      t314 = -t290 + t312 * t293
      t317 = t312 ** 2
      t320 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t276
     #, 0.0D0, -t278, 0.0D0)
      t325 = -t317 * t290 / 0.2D1 - t320 + t317 * t312 * t293 / 0.6D1 + 
     #t312 * t296
      t328 = t73 * t305
      t332 = t312 * t290 - t317 * t293 / 0.2D1 - t296
      t343 = log(-0.4D1 * t86 * t13 * t280 * t283 * t285)
      t352 = t45 * t100
      t353 = (0.90D2 * t5 * (t290 - t343 * t293) - 0.180D3 * t28 * t305)
     # * t43 * t352
      t354 = t104 * t13
      t357 = log(-0.4D1 * t354 * t286)
      t359 = t357 ** 2
      t372 = (0.90D2 * t5 * (t357 * t290 - t359 * t293 / 0.2D1 - t296) -
     # 0.180D3 * t28 * t4 * (-t290 + t357 * t293) - t306) * t45 * t100
      t374 = t309 / 0.720D3 - (t39 * t4 * t314 + 0.90D2 * t5 * t325 - t3
     #28 - 0.180D3 * t28 * t4 * t332) * t45 / 0.720D3 + t353 / 0.720D3 -
     # t372 / 0.720D3
      t375 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t276, -t278, 0.0D0, t374)
      t377 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t278, t276, 0.0D0, t374)
      t379 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t271)
      t381 = t2 * x3
      t382 = -0.1D1 + x3
      t383 = t2 * t382
      t384 = t14 * t382
      t387 = log(-0.4D1 * t7 * t384)
      t388 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t381, -t383, 0.0D0)
      t390 = t387 ** 2
      t391 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t381, -t383, 0.0D0)
      t394 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t381, -t383, 0.0D0)
      t403 = t4 * t391
      t411 = log(-0.4D1 * t86 * t384)
      t417 = log(0.4D1 * t86 * t14 * t87 * t382)
      t423 = t13 * t382
      t426 = log(-0.4D1 * t132 * t423)
      t431 = t426 ** 2
      t434 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t381, -t383, 0.0D0)
      t455 = log(-0.4D1 * t85 * t384)
      t457 = t455 ** 2
      t464 = log(0.4D1 * t85 * t10 * t423 * t87)
      t466 = t464 ** 2
      t482 = (0.90D2 * t5 * (-t387 * t388 + t390 * t391 / 0.2D1 + t394) 
     #- 0.180D3 * t28 * t4 * (t388 - t387 * t391) + t39 * t403) * t43 * 
     #t45 / 0.720D3 + t5 * (-t411 * t391 + t417 * t391) * t101 / 0.8D1 +
     # (t39 * t4 * (t388 - t426 * t391) + 0.90D2 * t5 * (t431 * t388 / 0
     #.2D1 + t434 - t431 * t426 * t391 / 0.6D1 - t426 * t394) + t73 * t4
     #03 - 0.180D3 * t28 * t4 * (-t426 * t388 + t431 * t391 / 0.2D1 + t3
     #94)) * t43 / 0.1440D4 - (0.90D2 * t5 * (t455 * t388 - t457 * t391 
     #/ 0.2D1 - t464 * t388 + t466 * t391 / 0.2D1) - 0.180D3 * t28 * t4 
     #* (t455 * t391 - t464 * t391)) * t43 * t100 / 0.1440D4
      t483 = FJET(XB1, XB2, s, 0.0D0, t381, 0.0D0, -t383, 0.0D0, t482)
      t485 = FJET(XB1, XB2, s, 0.0D0, -t383, 0.0D0, t381, 0.0D0, t482)
      t487 = x1 * x2
      t489 = t2 * t487 * t283
      t491 = t1 * x1
      t492 = t87 * s * t491
      t493 = t1 ** 2
      t498 = s * t493 * x2 * x1 * t277 * t283
      t499 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t489, -t49
     #2, 0.0D0, -t278, t498)
      t500 = t85 * t48
      t501 = t285 * t87
      t502 = t284 * t501
      t505 = log(0.4D1 * t500 * t502)
      t506 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t489, -t49
     #2, 0.0D0, -t278, t498)
      t511 = t4 * t506
      t519 = log(0.4D1 * t354 * t502)
      t521 = t519 ** 2
      t524 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, -t489, -t49
     #2, 0.0D0, -t278, t498)
      t538 = (0.90D2 * t5 * (-t499 + t505 * t506) + 0.180D3 * t28 * t511
     #) * t43 * t352 / 0.720D3 - (0.90D2 * t5 * (-t519 * t499 + t521 * t
     #506 / 0.2D1 + t524) - 0.180D3 * t28 * t4 * (t499 - t519 * t506) + 
     #t39 * t511) * t45 * t100 / 0.720D3
      t539 = FJET(XB1, XB2, s, 0.0D0, -t489, -t278, -t492, t498, t538)
      t541 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t271)
      t556 = t309 / 0.720D3 - (t39 * t4 * t314 + 0.90D2 * t5 * t325 - t3
     #28 - 0.180D3 * t28 * t4 * t332) * t45 / 0.720D3 + t353 / 0.720D3 -
     # t372 / 0.720D3
      t557 = FJET(XB1, XB2, s, t276, -t278, 0.0D0, 0.0D0, 0.0D0, t556)
      t559 = FJET(XB1, XB2, s, t381, 0.0D0, -t383, 0.0D0, 0.0D0, t482)
      t561 = x3 * x1
      t562 = t2 * t561
      t564 = t1 * t277
      t565 = x3 * s * t564
      t566 = t382 * s
      t567 = t566 * t491
      t568 = t566 * t564
      t569 = t285 * t382
      t573 = log(0.4D1 * t279 * t284 * t569)
      t574 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t562, -t567
     #, -t565, t568, 0.0D0)
      t576 = t573 ** 2
      t577 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t562, -t567
     #, -t565, t568, 0.0D0)
      t580 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, t562, -t567
     #, -t565, t568, 0.0D0)
      t581 = t573 * t574 - t576 * t577 / 0.2D1 - t580
      t585 = -t574 + t573 * t577
      t589 = t4 * t577
      t590 = t39 * t589
      t600 = log(0.4D1 * t48 * t280 * x2 * x3 * t283 * t569)
      t609 = (0.90D2 * t5 * (-t574 + t600 * t577) + 0.180D3 * t28 * t589
     #) * t43 * t352
      t611 = (0.90D2 * t5 * t581 - 0.180D3 * t28 * t4 * t585 - t590) * t
     #43 * t45 / 0.720D3 + t609 / 0.720D3
      t612 = FJET(XB1, XB2, s, t562, -t565, -t567, t568, 0.0D0, t611)
      t614 = t272 * t271 + t274 * t271 + t375 * t374 + t377 * t374 + t37
     #9 * t271 + t483 * t482 + t485 * t482 + t539 * t538 + t541 * t271 +
     # t557 * t556 + t559 * t482 + t612 * t611
      t615 = FJET(XB1, XB2, s, t568, -t567, -t565, t562, 0.0D0, t611)
      t617 = x2 * z
      t619 = x3 * z
      t620 = t561 * z
      t621 = t85 * z
      t622 = t561 * x2
      t623 = t561 * t617
      t624 = cos(t11)
      t629 = Sqrt(-x3 * t87 * t282 * x2 * t382)
      t631 = 0.2D1 * t624 * t629
      t632 = z + x1 - t281 - t617 - t487 + t487 * z - t619 - t561 + t620
     # + t621 + t622 - t623 + t85 + t631
      t635 = t2 * x1 * t632 * t283
      t639 = t2 * x1 * (-t619 - t561 + t620 + t621 + t622 - t623 - x2 + 
     #t85 + t631) * t283
      t640 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t639, -t635
     #, -t565, t568, t498)
      t645 = log(-0.4D1 * t500 * t284 * t501 * t382)
      t646 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t639, -t635
     #, -t565, t568, t498)
      t654 = 0.90D2 * t5 * (t640 - t645 * t646) - 0.180D3 * t28 * t4 * t
     #646
      t657 = t654 * t43 * t352 / 0.720D3
      t658 = FJET(XB1, XB2, s, t568, -t635, -t565, t639, t498, t657)
      t662 = FJET(XB1, XB2, s, t639, -t565, -t635, t568, t498, t657)
      t666 = FJET(XB1, XB2, s, -t278, t276, 0.0D0, 0.0D0, 0.0D0, t556)
      t668 = FJET(XB1, XB2, s, -t278, -t492, 0.0D0, -t489, t498, t538)
      t670 = FJET(XB1, XB2, s, -t383, 0.0D0, t381, 0.0D0, 0.0D0, t482)
      t683 = (0.90D2 * t5 * t581 - 0.180D3 * t28 * t4 * t585 - t590) * t
     #43 * t45 / 0.720D3 + t609 / 0.720D3
      t684 = FJET(XB1, XB2, s, -t565, t562, t568, -t567, 0.0D0, t683)
      t686 = FJET(XB1, XB2, s, -t565, t639, t568, -t635, t498, t657)
      t690 = FJET(XB1, XB2, s, -t492, -t278, -t489, 0.0D0, t498, t538)
      t692 = FJET(XB1, XB2, s, -t567, t568, t562, -t565, 0.0D0, t683)
      t694 = FJET(XB1, XB2, s, -t489, 0.0D0, -t492, -t278, t498, t538)
      t696 = FJET(XB1, XB2, s, -t635, t568, t639, -t565, t498, t657)
      t700 = t615 * t611 + t658 * t654 * t101 / 0.720D3 + t662 * t654 * 
     #t101 / 0.720D3 + t666 * t556 + t668 * t538 + t670 * t482 + t684 * 
     #t683 + t686 * t654 * t101 / 0.720D3 + t690 * t538 + t692 * t683 + 
     #t694 * t538 + t696 * t654 * t101 / 0.720D3
      rrgg2gghhardt9s2e1 = t614 + t700

      end function



      doubleprecision function rrgg2gghhardt9s2e0
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t18 = log(0.4D1 * t8 * t15)
      t19 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t24 = pi * lh
      t25 = t4 * t19
      t27 = 0.180D3 * t24 * t25
      t29 = 0.1D1 / x3
      t31 = 0.1D1 / x1
      t34 = x2 * t7
      t37 = log(0.4D1 * t34 * t15)
      t39 = -0.1D1 + x2
      t40 = t15 * t39
      t43 = log(-0.4D1 * t34 * t40)
      t47 = 0.1D1 / x2
      t51 = t7 * t14
      t54 = log(0.4D1 * t51 * t11)
      t56 = t54 ** 2
      t59 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t68 = pi ** 2
      t70 = lh ** 2
      t72 = -0.30D2 * t68 + 0.180D3 * t70
      t73 = pi * t72
      t74 = t73 * t25
      t78 = x3 * t11
      t81 = log(0.4D1 * t78 * t14)
      t83 = t81 ** 2
      t98 = log(0.4D1 * t15)
      t100 = t98 ** 2
      t117 = rrgg2gghhard91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, 0.0D0, t2, 0.0D0)
      t132 = x2 * x3
      t135 = log(-0.4D1 * t132 * t40)
      t139 = log(0.4D1 * t132 * t15)
      t146 = x2 * t11
      t150 = log(-0.4D1 * t146 * t14 * t39)
      t151 = t150 ** 2
      t154 = log(0.4D1 * t146 * t14)
      t155 = t154 ** 2
      t169 = (0.90D2 * t5 * (-t6 + t18 * t19) + t27) * t29 * t31 / 0.720
     #D3 - t5 * (-t37 * t19 + t43 * t19) * t31 * t47 / 0.8D1 - (0.90D2 *
     # t5 * (-t54 * t6 + t56 * t19 / 0.2D1 + t59) - 0.180D3 * t24 * t4 *
     # (t6 - t54 * t19) + t74) * t31 / 0.720D3 + (0.90D2 * t5 * (t81 * t
     #6 - t83 * t19 / 0.2D1 - t59) - 0.180D3 * t24 * t4 * (-t6 + t81 * t
     #19) - t74) * t29 / 0.1440D4 - (-0.180D3 * (-t98 * t6 + t100 * t19 
     #/ 0.2D1 + t59) * pi * lh + t19 * pi * (-0.240D3 * zeta3 - 0.120D3 
     #* t70 * lh + 0.60D2 * lh * t68) + 0.90D2 * (t100 * t6 / 0.2D1 + t1
     #17 - t100 * t98 * t19 / 0.6D1 - t98 * t59) * pi + (t6 - t98 * t19)
     # * pi * t72) * t4 / 0.1440D4 - t5 * (t135 * t19 - t139 * t19) * t2
     #9 * t47 / 0.16D2 - (0.90D2 * t5 * t19 * (-t151 / 0.2D1 + t155 / 0.
     #2D1) + (0.90D2 * t5 * t6 - t27) * (t150 - t154)) * t47 / 0.1440D4
      t170 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t169)
      t172 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t169)
      t174 = t2 * x1
      t175 = -0.1D1 + x1
      t176 = t2 * t175
      t177 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174
     #, 0.0D0, -t176, 0.0D0)
      t178 = t8 * t14
      t180 = x1 * z
      t181 = -z - x1 + t180
      t182 = 0.1D1 / t181
      t183 = 0.1D1 / t9 * t182
      t184 = t175 ** 2
      t185 = t183 * t184
      t188 = log(-0.4D1 * t178 * t185)
      t189 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174
     #, 0.0D0, -t176, 0.0D0)
      t194 = t4 * t189
      t196 = 0.180D3 * t24 * t194
      t200 = (0.90D2 * t5 * (t177 - t188 * t189) - t196) * t29 * t31 / 0
     #.720D3
      t203 = t29 * t31 * t47
      t205 = t5 * t189 * t203 / 0.8D1
      t206 = t34 * t14
      t209 = log(-0.4D1 * t206 * t185)
      t217 = (0.90D2 * t5 * (-t177 + t209 * t189) + t196) * t31 * t47 / 
     #0.720D3
      t220 = log(-0.4D1 * t51 * t185)
      t222 = t220 ** 2
      t225 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174
     #, 0.0D0, -t176, 0.0D0)
      t226 = t220 * t177 - t222 * t189 / 0.2D1 - t225
      t230 = -t177 + t220 * t189
      t234 = t73 * t194
      t238 = t200 + t205 - t217 - (0.90D2 * t5 * t226 - 0.180D3 * t24 * 
     #t4 * t230 - t234) * t31 / 0.720D3
      t239 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t174, -t176, 0.0D0, t238)
      t241 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t176, t174, 0.0D0, t238)
      t243 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t169)
      t245 = t2 * x3
      t246 = -0.1D1 + x3
      t247 = t2 * t246
      t248 = t14 * t246
      t251 = log(-0.4D1 * t78 * t248)
      t252 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t245, -t247, 0.0D0)
      t254 = t251 ** 2
      t255 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t245, -t247, 0.0D0)
      t258 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t245, -t247, 0.0D0)
      t267 = t4 * t255
      t272 = t15 * t246
      t275 = log(-0.4D1 * t8 * t272)
      t288 = log(-0.4D1 * t132 * t272)
      t294 = log(0.4D1 * t132 * t11 * t248 * t39)
      t301 = (0.90D2 * t5 * (-t251 * t252 + t254 * t255 / 0.2D1 + t258) 
     #- 0.180D3 * t24 * t4 * (t252 - t251 * t255) + t73 * t267) * t29 / 
     #0.1440D4 + (0.90D2 * t5 * (t252 - t275 * t255) - 0.180D3 * t24 * t
     #267) * t29 * t31 / 0.720D3 - t5 * (t288 * t255 - t294 * t255) * t2
     #9 * t47 / 0.16D2
      t302 = FJET(XB1, XB2, s, 0.0D0, t245, 0.0D0, -t247, 0.0D0, t301)
      t304 = FJET(XB1, XB2, s, 0.0D0, -t247, 0.0D0, t245, 0.0D0, t301)
      t306 = x1 * x2
      t308 = t2 * t306 * t182
      t310 = t1 * x1
      t311 = t39 * s * t310
      t312 = t1 ** 2
      t317 = s * t312 * x2 * x1 * t175 * t182
      t318 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t308, -t31
     #1, 0.0D0, -t176, t317)
      t322 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, -t308, -t31
     #1, 0.0D0, -t176, t317)
      t327 = log(0.4D1 * t206 * t183 * t184 * t39)
      t339 = -t5 * t318 * t203 / 0.8D1 - (0.90D2 * t5 * (t322 - t327 * t
     #318) - 0.180D3 * t24 * t4 * t318) * t31 * t47 / 0.720D3
      t340 = FJET(XB1, XB2, s, 0.0D0, -t308, -t176, -t311, t317, t339)
      t342 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t169)
      t354 = t200 + t205 - t217 - (0.90D2 * t5 * t226 - 0.180D3 * t24 * 
     #t4 * t230 - t234) * t31 / 0.720D3
      t355 = FJET(XB1, XB2, s, t174, -t176, 0.0D0, 0.0D0, 0.0D0, t354)
      t357 = FJET(XB1, XB2, s, t245, 0.0D0, -t247, 0.0D0, 0.0D0, t301)
      t359 = x3 * x1
      t360 = t2 * t359
      t362 = t1 * t175
      t363 = x3 * s * t362
      t364 = t246 * s
      t365 = t364 * t310
      t366 = t364 * t362
      t367 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, t360, -t365
     #, -t363, t366, 0.0D0)
      t372 = log(0.4D1 * t178 * t183 * t184 * t246)
      t373 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t360, -t365
     #, -t363, t366, 0.0D0)
      t375 = -t367 + t372 * t373
      t380 = 0.180D3 * t24 * t4 * t373
      t387 = t5 * t373 * t203 / 0.8D1
      t388 = (0.90D2 * t5 * t375 + t380) * t29 * t31 / 0.720D3 - t387
      t389 = FJET(XB1, XB2, s, t360, -t363, -t365, t366, 0.0D0, t388)
      t391 = t170 * t169 + t172 * t169 + t239 * t238 + t241 * t238 + t24
     #3 * t169 + t302 * t301 + t304 * t301 + t340 * t339 + t342 * t169 +
     # t355 * t354 + t357 * t301 + t389 * t388
      t392 = FJET(XB1, XB2, s, t366, -t365, -t363, t360, 0.0D0, t388)
      t394 = x2 * z
      t396 = x3 * z
      t397 = t359 * z
      t398 = t132 * z
      t399 = t359 * x2
      t400 = t359 * t394
      t401 = cos(t12)
      t406 = Sqrt(-x3 * t39 * t181 * x2 * t246)
      t408 = 0.2D1 * t401 * t406
      t409 = z + x1 - t180 - t394 - t306 + t306 * z - t396 - t359 + t397
     # + t398 + t399 - t400 + t132 + t408
      t412 = t2 * x1 * t409 * t182
      t416 = t2 * x1 * (-t396 - t359 + t397 + t398 + t399 - t400 - x2 + 
     #t132 + t408) * t182
      t417 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t416, -t412
     #, -t363, t366, t317)
      t420 = t5 * t417 * t203 / 0.8D1
      t421 = FJET(XB1, XB2, s, t366, -t412, -t363, t416, t317, t420)
      t426 = t417 * t29 * t31 * t47
      t429 = FJET(XB1, XB2, s, t416, -t363, -t412, t366, t317, t420)
      t434 = FJET(XB1, XB2, s, -t176, t174, 0.0D0, 0.0D0, 0.0D0, t354)
      t436 = FJET(XB1, XB2, s, -t176, -t311, 0.0D0, -t308, t317, t339)
      t438 = FJET(XB1, XB2, s, -t247, 0.0D0, t245, 0.0D0, 0.0D0, t301)
      t447 = (0.90D2 * t5 * t375 + t380) * t29 * t31 / 0.720D3 - t387
      t448 = FJET(XB1, XB2, s, -t363, t360, t366, -t365, 0.0D0, t447)
      t450 = FJET(XB1, XB2, s, -t363, t416, t366, -t412, t317, t420)
      t455 = FJET(XB1, XB2, s, -t311, -t176, -t308, 0.0D0, t317, t339)
      t457 = FJET(XB1, XB2, s, -t365, t366, t360, -t363, 0.0D0, t447)
      t459 = FJET(XB1, XB2, s, -t308, 0.0D0, -t311, -t176, t317, t339)
      t461 = FJET(XB1, XB2, s, -t412, t366, t416, -t363, t317, t420)
      t466 = t392 * t388 + t421 * pi * t4 * t426 / 0.8D1 + t429 * pi * t
     #4 * t426 / 0.8D1 + t434 * t354 + t436 * t339 + t438 * t301 + t448 
     #* t447 + t450 * pi * t4 * t426 / 0.8D1 + t455 * t339 + t457 * t447
     # + t459 * t339 + t461 * pi * t4 * t426 / 0.8D1
      rrgg2gghhardt9s2e0 = t391 + t466

      end function



      doubleprecision function rrgg2gghhardt9s2em1
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * pi)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t30 = x1 ** 2
      t31 = t30 * t13
      t34 = log(0.4D1 * t31 * t9)
      t40 = 0.1D1 / x1
      t47 = x2 * t9
      t48 = -0.1D1 + x2
      t52 = log(-0.4D1 * t47 * t13 * t48)
      t55 = log(0.4D1 * t47 * t13)
      t58 = 0.1D1 / x2
      t64 = log(0.4D1 * t9 * t13)
      t71 = t64 ** 2
      t74 = rrgg2gghhard91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t79 = pi ** 2
      t81 = lh ** 2
      t88 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 - (
     #0.90D2 * t5 * (t6 - t34 * t17) - t25) * t40 / 0.720D3 - t5 * t17 *
     # t27 * t40 / 0.8D1 - t5 * t17 * (t52 - t55) * t58 / 0.16D2 - (-0.1
     #80D3 * (t6 - t64 * t17) * pi * lh + 0.90D2 * (-t64 * t6 + t71 * t1
     #7 / 0.2D1 + t74) * pi + t17 * pi * (-0.30D2 * t79 + 0.180D3 * t81)
     #) * t4 / 0.1440D4
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t88)
      t91 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t88)
      t93 = t2 * x1
      t94 = -0.1D1 + x1
      t95 = t2 * t94
      t96 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t93, 
     #0.0D0, -t95, 0.0D0)
      t100 = t5 * t96 * t40 * t58 / 0.8D1
      t101 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t93,
     # 0.0D0, -t95, 0.0D0)
      t105 = 0.1D1 / (-z - x1 + x1 * z)
      t107 = t94 ** 2
      t111 = log(-0.4D1 * t31 / t7 * t105 * t107)
      t113 = -t101 + t111 * t96
      t118 = 0.180D3 * t22 * t4 * t96
      t125 = t5 * t96 * t27 * t40 / 0.8D1
      t126 = t100 - (0.90D2 * t5 * t113 + t118) * t40 / 0.720D3 + t125
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t93, -t95, 0.0D0, t126)
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t95, t93, 0.0D0, t126)
      t131 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t88)
      t133 = t2 * x3
      t134 = -0.1D1 + x3
      t135 = t2 * t134
      t136 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t133, -t135, 0.0D0)
      t141 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D
     #0, t133, -t135, 0.0D0)
      t145 = log(-0.4D1 * t10 * t13 * t134)
      t156 = t5 * t136 * t27 * t40 / 0.8D1 + (0.90D2 * t5 * (t141 - t145
     # * t136) - 0.180D3 * t22 * t4 * t136) * t27 / 0.1440D4
      t157 = FJET(XB1, XB2, s, 0.0D0, t133, 0.0D0, -t135, 0.0D0, t156)
      t159 = FJET(XB1, XB2, s, 0.0D0, -t135, 0.0D0, t133, 0.0D0, t156)
      t163 = t2 * x1 * x2 * t105
      t165 = t1 * x1
      t166 = t48 * s * t165
      t167 = t1 ** 2
      t172 = s * t167 * x2 * x1 * t94 * t105
      t173 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, -t163, -t16
     #6, 0.0D0, -t95, t172)
      t175 = t173 * t40 * t58
      t177 = t5 * t175 / 0.8D1
      t178 = FJET(XB1, XB2, s, 0.0D0, -t163, -t95, -t166, t172, -t177)
      t183 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t88)
      t191 = t100 - (0.90D2 * t5 * t113 + t118) * t40 / 0.720D3 + t125
      t192 = FJET(XB1, XB2, s, t93, -t95, 0.0D0, 0.0D0, 0.0D0, t191)
      t194 = FJET(XB1, XB2, s, t133, 0.0D0, -t135, 0.0D0, 0.0D0, t156)
      t197 = t2 * x1 * x3
      t199 = t1 * t94
      t200 = x3 * s * t199
      t201 = t134 * s
      t202 = t201 * t165
      t203 = t201 * t199
      t204 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, t197, -t202
     #, -t200, t203, 0.0D0)
      t206 = t204 * t27 * t40
      t208 = t5 * t206 / 0.8D1
      t209 = FJET(XB1, XB2, s, t197, -t200, -t202, t203, 0.0D0, -t208)
      t214 = FJET(XB1, XB2, s, t203, -t202, -t200, t197, 0.0D0, -t208)
      t219 = FJET(XB1, XB2, s, -t95, t93, 0.0D0, 0.0D0, 0.0D0, t191)
      t221 = FJET(XB1, XB2, s, -t95, -t166, 0.0D0, -t163, t172, -t177)
      t226 = FJET(XB1, XB2, s, -t135, 0.0D0, t133, 0.0D0, 0.0D0, t156)
      t228 = FJET(XB1, XB2, s, -t200, t197, t203, -t202, 0.0D0, -t208)
      t233 = FJET(XB1, XB2, s, -t166, -t95, -t163, 0.0D0, t172, -t177)
      t238 = FJET(XB1, XB2, s, -t202, t203, t197, -t200, 0.0D0, -t208)
      t243 = FJET(XB1, XB2, s, -t163, 0.0D0, -t166, -t95, t172, -t177)
      rrgg2gghhardt9s2em1 = t89 * t88 + t91 * t88 + t127 * t126 + t129 *
     # t126 + t131 * t88 + t157 * t156 + t159 * t156 - t178 * pi * t4 * 
     #t175 / 0.8D1 + t183 * t88 + t192 * t191 + t194 * t156 - t209 * pi 
     #* t4 * t206 / 0.8D1 - t214 * pi * t4 * t206 / 0.8D1 + t219 * t191 
     #- t221 * pi * t4 * t175 / 0.8D1 + t226 * t156 - t228 * pi * t4 * t
     #206 / 0.8D1 - t233 * pi * t4 * t175 / 0.8D1 - t238 * pi * t4 * t20
     #6 / 0.8D1 - t243 * pi * t4 * t175 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt9s2em2
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t18 = rrgg2gghhard91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t19 = z ** 2
      t23 = Sin(x4 * pi)
      t24 = t23 ** 2
      t27 = log(0.4D1 / t19 / z * t24)
      t35 = -t5 * t6 * t7 / 0.8D1 - t5 * t6 * t11 / 0.16D2 - (-0.180D3 *
     # t6 * pi * lh + 0.90D2 * (t18 - t27 * t6) * pi) * t4 / 0.1440D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t40 = t2 * x1
      t42 = t2 * (-0.1D1 + x1)
      t43 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t40, 
     #0.0D0, -t42, 0.0D0)
      t46 = t5 * t43 * t7 / 0.8D1
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t40, -t42, 0.0D0, t46)
      t50 = t4 * t43 * t7
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t42, t40, 0.0D0, t46)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t59 = t2 * x3
      t61 = t2 * (-0.1D1 + x3)
      t62 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t59, -t61, 0.0D0)
      t65 = t5 * t62 * t11 / 0.16D2
      t66 = FJET(XB1, XB2, s, 0.0D0, t59, 0.0D0, -t61, 0.0D0, t65)
      t69 = t4 * t62 * t11
      t72 = FJET(XB1, XB2, s, 0.0D0, -t61, 0.0D0, t59, 0.0D0, t65)
      t76 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t78 = FJET(XB1, XB2, s, t40, -t42, 0.0D0, 0.0D0, 0.0D0, t46)
      t82 = FJET(XB1, XB2, s, t59, 0.0D0, -t61, 0.0D0, 0.0D0, t65)
      t86 = FJET(XB1, XB2, s, -t42, t40, 0.0D0, 0.0D0, 0.0D0, t46)
      t90 = FJET(XB1, XB2, s, -t61, 0.0D0, t59, 0.0D0, 0.0D0, t65)
      rrgg2gghhardt9s2em2 = t36 * t35 + t38 * t35 + t47 * pi * t50 / 0.8
     #D1 + t53 * pi * t50 / 0.8D1 + t57 * t35 + t66 * pi * t69 / 0.16D2 
     #+ t72 * pi * t69 / 0.16D2 + t76 * t35 + t78 * pi * t50 / 0.8D1 + t
     #82 * pi * t69 / 0.16D2 + t86 * pi * t50 / 0.8D1 + t90 * pi * t69 /
     # 0.16D2

      end function



      doubleprecision function rrgg2gghhardt9s2em3
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2gghhard91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2gghhardt9s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0
     #.16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt9s2em4
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
      doubleprecision rrgg2gghhard91J1
      doubleprecision rrgg2gghhard91J2
      doubleprecision rrgg2gghhard91J3
      doubleprecision rrgg2gghhard91J4
      doubleprecision rrgg2gghhard91J5
      doubleprecision rrgg2gghhard91J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt9s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2gghhard91J1
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
      t5 = S24 ** 2
      t11 = S13 ** 2
      t13 = S14 ** 2
      t18 = 0.1D1 / S12
      t23 = t11 ** 2
      t31 = t5 ** 2
      rrgg2gghhard91J1 = ((0.9D1 * S12 + 0.9D1 / 0.2D1 * S13 - 0.9D1 / 0
     #.2D1 * S14 + 0.9D1 * S24 + (0.18D2 * t5 - 0.27D2 / 0.2D1 * S13 * S
     #24 + 0.27D2 / 0.2D1 * S14 * S24 + 0.9D1 / 0.2D1 * t11 + 0.9D1 / 0.
     #2D1 * t13 - 0.9D1 * S13 * S14) * t18) * s * z + (0.9D1 * t23 - 0.1
     #8D2 * t11 * S13 * S24 - 0.18D2 * S13 * t5 * S24 + 0.9D1 * t31 + 0.
     #27D2 * t5 * t11) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard91J2
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
      t7 = S13 ** 2
      t9 = S24 ** 2
      t11 = S14 ** 2
      t14 = 0.1D1 / S12
      t19 = t7 ** 2
      t27 = t9 ** 2
      rrgg2gghhard91J2 = ((0.9D1 * S12 + 0.9D1 / 0.2D1 * S13 - 0.9D1 / 0
     #.2D1 * S14 + 0.9D1 * S24 + (-0.9D1 * S13 * S14 + 0.9D1 / 0.2D1 * t
     #7 + 0.18D2 * t9 + 0.9D1 / 0.2D1 * t11) * t14) * s * z + (0.9D1 * t
     #19 - 0.18D2 * t7 * S13 * S24 - 0.18D2 * S13 * t9 * S24 + 0.9D1 * t
     #27 + 0.27D2 * t9 * t7) / (S12 + S13 + S23) * t14) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard91J3
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
      t9 = S14 ** 2
      t13 = S24 ** 2
      t15 = S13 ** 2
      t18 = 0.1D1 / S12
      t23 = t15 ** 2
      t31 = t13 ** 2
      rrgg2gghhard91J3 = ((0.9D1 * S12 + 0.9D1 / 0.2D1 * S13 - 0.9D1 / 0
     #.2D1 * S14 + 0.9D1 * S24 + (0.27D2 / 0.2D1 * S13 * S24 - 0.27D2 / 
     #0.2D1 * S14 * S24 + 0.9D1 / 0.2D1 * t9 - 0.9D1 * S13 * S14 + 0.18D
     #2 * t13 + 0.9D1 / 0.2D1 * t15) * t18) * s * z + (0.9D1 * t23 - 0.1
     #8D2 * t15 * S13 * S24 - 0.18D2 * S13 * t13 * S24 + 0.9D1 * t31 + 0
     #.27D2 * t13 * t15) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard91J4
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
      t9 = S14 ** 2
      t13 = S24 ** 2
      t15 = S13 ** 2
      t18 = 0.1D1 / S12
      t23 = t15 ** 2
      t31 = t13 ** 2
      rrgg2gghhard91J4 = ((0.9D1 * S12 + 0.9D1 / 0.2D1 * S13 - 0.9D1 / 0
     #.2D1 * S14 + 0.9D1 * S24 + (0.27D2 * S13 * S24 - 0.27D2 * S14 * S2
     #4 + 0.9D1 / 0.2D1 * t9 - 0.9D1 * S13 * S14 + 0.18D2 * t13 + 0.9D1 
     #/ 0.2D1 * t15) * t18) * s * z + (0.9D1 * t23 - 0.18D2 * t15 * S13 
     #* S24 - 0.18D2 * S13 * t13 * S24 + 0.9D1 * t31 + 0.27D2 * t13 * t1
     #5) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard91J5
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
      t9 = S14 ** 2
      t13 = S24 ** 2
      t15 = S13 ** 2
      t18 = 0.1D1 / S12
      t23 = t15 ** 2
      t31 = t13 ** 2
      rrgg2gghhard91J5 = ((0.9D1 * S12 + 0.9D1 / 0.2D1 * S13 - 0.9D1 / 0
     #.2D1 * S14 + 0.9D1 * S24 + (0.81D2 / 0.2D1 * S13 * S24 - 0.81D2 / 
     #0.2D1 * S14 * S24 + 0.9D1 / 0.2D1 * t9 - 0.9D1 * S13 * S14 + 0.18D
     #2 * t13 + 0.9D1 / 0.2D1 * t15) * t18) * s * z + (0.9D1 * t23 - 0.1
     #8D2 * t15 * S13 * S24 - 0.18D2 * S13 * t13 * S24 + 0.9D1 * t31 + 0
     #.27D2 * t13 * t15) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard91J6
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
      t7 = S13 ** 2
      t11 = S14 ** 2
      t13 = S24 ** 2
      t18 = 0.1D1 / S12
      t23 = t13 ** 2
      t25 = t7 ** 2
      rrgg2gghhard91J6 = ((-0.45D2 * S12 + 0.45D2 / 0.2D1 * S14 - 0.45D2
     # * S24 - 0.45D2 / 0.2D1 * S13 + (-0.135D3 * S14 * S24 - 0.45D2 / 0
     #.2D1 * t7 + 0.135D3 * S13 * S24 - 0.45D2 / 0.2D1 * t11 - 0.90D2 * 
     #t13 + 0.45D2 * S13 * S14) * t18) * s * z + (-0.45D2 * t23 - 0.45D2
     # * t25 + 0.90D2 * S13 * t13 * S24 - 0.135D3 * t13 * t7 + 0.90D2 * 
     #t7 * S13 * S24) / (S12 + S13 + S23) * t18) / pi * wd / z

      end function
  
   
      subroutine rrgg2gghsoftt9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt9s1e1  
      doubleprecision rrgg2gghsoftt9s1e0  
      doubleprecision rrgg2gghsoftt9s1em1  
      doubleprecision rrgg2gghsoftt9s1em2  
      doubleprecision rrgg2gghsoftt9s1em3  
      doubleprecision rrgg2gghsoftt9s1em4  
      doubleprecision rrgg2gghsoftt9s2e1  
      doubleprecision rrgg2gghsoftt9s2e0  
      doubleprecision rrgg2gghsoftt9s2em1  
      doubleprecision rrgg2gghsoftt9s2em2  
      doubleprecision rrgg2gghsoftt9s2em3  
      doubleprecision rrgg2gghsoftt9s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt9s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt9s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt9s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt9s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt9s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt9s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt9s1e1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = Sin(x4 * pi)
      t3 = t2 ** 2
      t4 = x3 * t3
      t5 = x1 ** 2
      t7 = (-0.1D1 + x1) ** 2
      t9 = -0.1D1 + x3
      t13 = log(-0.4D1 * t4 * t5 * t7 * t9)
      t14 = t13 ** 2
      t15 = x3 * t5
      t16 = t15 * t3
      t18 = log(0.4D1 * t16)
      t19 = t18 ** 2
      t20 = t3 * t9
      t23 = log(-0.4D1 * t15 * t20)
      t24 = t23 ** 2
      t25 = t3 * t7
      t28 = log(0.4D1 * t15 * t25)
      t29 = t28 ** 2
      t35 = 0.180D3 * wd * lh
      t36 = 0.90D2 * wd
      t37 = -t35 + t36
      t41 = 0.1D1 / x3
      t43 = 0.1D1 / x1
      t46 = x2 * x3
      t47 = t5 * t3
      t48 = -0.1D1 + x2
      t52 = log(-0.4D1 * t46 * t47 * t48)
      t55 = x2 * t5
      t56 = t55 * t25
      t59 = log(0.4D1 * t48 * t9 * x3 * t56)
      t60 = t46 * t5
      t61 = t25 * t48
      t64 = log(-0.4D1 * t60 * t61)
      t65 = t20 * t48
      t68 = log(0.4D1 * t60 * t65)
      t72 = log(-0.4D1 * t46 * t47 * t9)
      t75 = log(0.4D1 * t46 * t47)
      t80 = log(-0.4D1 * t16 * t7 * x2 * t9)
      t81 = t47 * t7
      t84 = log(0.4D1 * t46 * t81)
      t87 = 0.1D1 / x2
      t94 = log(-0.4D1 * t55 * t61)
      t95 = t94 ** 2
      t96 = t3 * t48
      t99 = log(-0.4D1 * t55 * t96)
      t100 = t99 ** 2
      t102 = log(0.4D1 * t56)
      t103 = t102 ** 2
      t106 = log(0.4D1 * t55 * t3)
      t107 = t106 ** 2
      t119 = log(0.4D1 * t47)
      t120 = t119 ** 2
      t122 = log(0.4D1 * t81)
      t123 = t122 ** 2
      t133 = pi ** 2
      t135 = lh ** 2
      t137 = -0.30D2 * t133 + 0.180D3 * t135
      t139 = wd * t137 - t35 + t36
      t146 = log(0.4D1 * t4)
      t147 = t146 ** 2
      t150 = log(-0.4D1 * t4 * t9)
      t151 = t150 ** 2
      t168 = log(0.4D1 * t3)
      t169 = t168 * wd
      t171 = t168 ** 2
      t172 = t171 * wd
      t174 = 0.3D1 * wd - 0.2D1 * t169 + t172 / 0.2D1
      t182 = -0.240D3 * zeta3 - 0.120D3 * t135 * lh + 0.60D2 * lh * t133
      t189 = t171 * t168 * wd
      t192 = 0.2D1 * wd - t169
      t205 = t133 ** 2
      t206 = t135 ** 2
      t215 = t171 ** 2
      t220 = log(-0.4D1 * t46 * t96)
      t221 = t220 ** 2
      t224 = log(0.4D1 * t46 * t3)
      t225 = t224 ** 2
      t228 = log(-0.4D1 * t46 * t20)
      t229 = t228 ** 2
      t232 = log(0.4D1 * t46 * t65)
      t233 = t232 ** 2
      t244 = x2 * t3
      t247 = log(-0.4D1 * t244 * t48)
      t248 = t247 ** 2
      t250 = log(0.4D1 * t244)
      t251 = t250 ** 2
      t266 = -(0.90D2 * wd * (t14 / 0.2D1 + t19 / 0.2D1 - t24 / 0.2D1 - 
     #t29 / 0.2D1) + t37 * (-t13 - t18 + t23 + t28)) * t41 * t43 / 0.20D
     #2 + 0.9D1 / 0.2D1 * wd * (-t52 - t59 + t64 + t68 - t72 + t75 + t80
     # - t84) * t41 * t87 * t43 + (0.90D2 * wd * (-t95 / 0.2D1 + t100 / 
     #0.2D1 + t103 / 0.2D1 - t107 / 0.2D1) + t37 * (t94 - t99 - t102 + t
     #106)) * t87 * t43 / 0.20D2 + (t37 * (-t120 / 0.2D1 + t123 / 0.2D1)
     # + 0.90D2 * wd * (-t123 * t122 / 0.6D1 + t120 * t119 / 0.6D1) + t1
     #39 * (t119 - t122)) * t43 / 0.20D2 + (t37 * (-t147 / 0.2D1 + t151 
     #/ 0.2D1) + 0.90D2 * wd * (-t151 * t150 / 0.6D1 + t147 * t146 / 0.6
     #D1) + t139 * (t146 - t150)) * t41 / 0.40D2 - 0.9D1 / 0.2D1 * t174 
     #* lh + wd * t182 / 0.40D2 - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.4D1 * t
     #169 - 0.9D1 / 0.8D1 * t172 + 0.3D1 / 0.8D1 * t189 + t192 * t137 / 
     #0.40D2 + 0.9D1 / 0.2D1 * (0.4D1 * wd - 0.3D1 * t169 + t172 - t189 
     #/ 0.6D1) * lh - t174 * t137 / 0.40D2 - t192 * t182 / 0.40D2 - wd *
     # (t205 + 0.60D2 * t206 + 0.480D3 * lh * zeta3 - 0.60D2 * t135 * t1
     #33) / 0.40D2 - 0.3D1 / 0.32D2 * t215 * wd - (0.90D2 * wd * (-t221 
     #/ 0.2D1 + t225 / 0.2D1 - t229 / 0.2D1 + t233 / 0.2D1) + t37 * (t22
     #0 - t224 + t228 - t232)) * t41 * t87 / 0.40D2 + (t37 * (t248 / 0.2
     #D1 - t251 / 0.2D1) + 0.90D2 * wd * (t251 * t250 / 0.6D1 - t248 * t
     #247 / 0.6D1) + t139 * (-t247 + t250)) * t87 / 0.40D2
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t266)
      rrgg2gghsoftt9s1e1 = t267 * t266

      end function



      doubleprecision function rrgg2gghsoftt9s1e0
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t3 = Sin(x4 * pi)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = log(0.4D1 * t5)
      t8 = t7 ** 2
      t10 = (-0.1D1 + x1) ** 2
      t13 = log(0.4D1 * t5 * t10)
      t14 = t13 ** 2
      t22 = -0.180D3 * wd * lh + 0.90D2 * wd
      t26 = 0.1D1 / x1
      t29 = x3 * t4
      t31 = -0.1D1 + x3
      t35 = log(-0.4D1 * t29 * t1 * t10 * t31)
      t36 = x3 * t1
      t39 = log(0.4D1 * t36 * t4)
      t40 = t4 * t31
      t43 = log(-0.4D1 * t36 * t40)
      t44 = t4 * t10
      t47 = log(0.4D1 * t36 * t44)
      t50 = 0.1D1 / x3
      t54 = x2 * t1
      t55 = -0.1D1 + x2
      t56 = t4 * t55
      t60 = log(-0.4D1 * t54 * t56 * t10)
      t63 = log(-0.4D1 * t54 * t56)
      t66 = log(0.4D1 * t54 * t44)
      t69 = log(0.4D1 * t54 * t4)
      t72 = 0.1D1 / x2
      t78 = log(0.4D1 * t4)
      t79 = t78 * wd
      t80 = 0.2D1 * wd - t79
      t85 = t78 ** 2
      t86 = t85 * wd
      t88 = pi ** 2
      t90 = lh ** 2
      t92 = -0.30D2 * t88 + 0.180D3 * t90
      t115 = log(0.4D1 * t29)
      t116 = t115 ** 2
      t119 = log(-0.4D1 * t29 * t31)
      t120 = t119 ** 2
      t130 = x2 * x3
      t133 = log(-0.4D1 * t130 * t56)
      t136 = log(0.4D1 * t130 * t4)
      t139 = log(-0.4D1 * t130 * t40)
      t143 = log(0.4D1 * t130 * t40 * t55)
      t149 = x2 * t4
      t152 = log(-0.4D1 * t149 * t55)
      t153 = t152 ** 2
      t155 = log(0.4D1 * t149)
      t156 = t155 ** 2
      t166 = (0.90D2 * wd * (-t8 / 0.2D1 + t14 / 0.2D1) + t22 * (t7 - t1
     #3)) * t26 / 0.20D2 - 0.9D1 / 0.2D1 * wd * (-t35 - t39 + t43 + t47)
     # * t50 * t26 + 0.9D1 / 0.2D1 * wd * (t60 - t63 - t66 + t69) * t72 
     #* t26 - 0.9D1 / 0.2D1 * t80 * lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.
     #4D1 * t79 - 0.9D1 / 0.8D1 * t86 + wd * t92 / 0.40D2 + 0.9D1 / 0.2D
     #1 * (0.3D1 * wd - 0.2D1 * t79 + t86 / 0.2D1) * lh - wd * (-0.240D3
     # * zeta3 - 0.120D3 * t90 * lh + 0.60D2 * lh * t88) / 0.40D2 + 0.3D
     #1 / 0.8D1 * t85 * t78 * wd - t80 * t92 / 0.40D2 + (0.90D2 * wd * (
     #-t116 / 0.2D1 + t120 / 0.2D1) + t22 * (t115 - t119)) * t50 / 0.40D
     #2 - 0.9D1 / 0.4D1 * wd * (t133 - t136 + t139 - t143) * t50 * t72 +
     # (0.90D2 * wd * (t153 / 0.2D1 - t156 / 0.2D1) + t22 * (-t152 + t15
     #5)) * t72 / 0.40D2
      t167 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t166)
      rrgg2gghsoftt9s1e0 = t167 * t166

      end function



      doubleprecision function rrgg2gghsoftt9s1em1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t3 = Sin(x4 * pi)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = log(0.4D1 * t5)
      t9 = (-0.1D1 + x1) ** 2
      t12 = log(0.4D1 * t5 * t9)
      t22 = log(0.4D1 * t4)
      t23 = t22 * wd
      t29 = t22 ** 2
      t32 = pi ** 2
      t34 = lh ** 2
      t39 = x3 * t4
      t41 = log(0.4D1 * t39)
      t45 = log(-0.4D1 * t39 * (-0.1D1 + x3))
      t51 = x2 * t4
      t55 = log(-0.4D1 * t51 * (-0.1D1 + x2))
      t57 = log(0.4D1 * t51)
      t63 = 0.9D1 / 0.2D1 * wd * (t7 - t12) / x1 - 0.9D1 / 0.2D1 * wd * 
     #lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.4D1 * t23 + 0.9D1 / 0.2D1 * (0
     #.2D1 * wd - t23) * lh - 0.9D1 / 0.8D1 * t29 * wd - wd * (-0.30D2 *
     # t32 + 0.180D3 * t34) / 0.40D2 + 0.9D1 / 0.4D1 * wd * (t41 - t45) 
     #/ x3 + 0.9D1 / 0.4D1 * wd * (-t55 + t57) / x2
      t64 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t63)
      rrgg2gghsoftt9s1em1 = t64 * t63

      end function



      doubleprecision function rrgg2gghsoftt9s1em2
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t5 = Sin(x4 * pi)
      t6 = t5 ** 2
      t8 = log(0.4D1 * t6)
      t11 = -0.9D1 / 0.4D1 * wd + 0.9D1 / 0.2D1 * wd * lh + 0.9D1 / 0.4D
     #1 * t8 * wd
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t11)
      rrgg2gghsoftt9s1em2 = t12 * t11

      end function



      doubleprecision function rrgg2gghsoftt9s1em3
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.4D1 * wd)
      rrgg2gghsoftt9s1em3 = -0.9D1 / 0.4D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt9s1em4
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt9s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt9s2e1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = Sin(x4 * pi)
      t3 = t2 ** 2
      t4 = x3 * t3
      t5 = x1 ** 2
      t7 = (-0.1D1 + x1) ** 2
      t9 = -0.1D1 + x3
      t13 = log(-0.4D1 * t4 * t5 * t7 * t9)
      t14 = t13 ** 2
      t15 = x3 * t5
      t16 = t15 * t3
      t18 = log(0.4D1 * t16)
      t19 = t18 ** 2
      t20 = t3 * t9
      t23 = log(-0.4D1 * t15 * t20)
      t24 = t23 ** 2
      t25 = t3 * t7
      t28 = log(0.4D1 * t15 * t25)
      t29 = t28 ** 2
      t35 = 0.180D3 * wd * lh
      t36 = 0.90D2 * wd
      t37 = -t35 + t36
      t41 = 0.1D1 / x3
      t43 = 0.1D1 / x1
      t46 = x2 * x3
      t47 = t5 * t3
      t48 = -0.1D1 + x2
      t52 = log(-0.4D1 * t46 * t47 * t48)
      t55 = x2 * t5
      t56 = t55 * t25
      t59 = log(0.4D1 * t48 * t9 * x3 * t56)
      t60 = t46 * t5
      t61 = t25 * t48
      t64 = log(-0.4D1 * t60 * t61)
      t65 = t20 * t48
      t68 = log(0.4D1 * t60 * t65)
      t72 = log(-0.4D1 * t46 * t47 * t9)
      t75 = log(0.4D1 * t46 * t47)
      t80 = log(-0.4D1 * t16 * t7 * x2 * t9)
      t81 = t47 * t7
      t84 = log(0.4D1 * t46 * t81)
      t87 = 0.1D1 / x2
      t94 = log(-0.4D1 * t55 * t61)
      t95 = t94 ** 2
      t96 = t3 * t48
      t99 = log(-0.4D1 * t55 * t96)
      t100 = t99 ** 2
      t102 = log(0.4D1 * t56)
      t103 = t102 ** 2
      t106 = log(0.4D1 * t55 * t3)
      t107 = t106 ** 2
      t119 = log(0.4D1 * t47)
      t120 = t119 ** 2
      t122 = log(0.4D1 * t81)
      t123 = t122 ** 2
      t133 = pi ** 2
      t135 = lh ** 2
      t137 = -0.30D2 * t133 + 0.180D3 * t135
      t139 = wd * t137 - t35 + t36
      t146 = log(0.4D1 * t4)
      t147 = t146 ** 2
      t150 = log(-0.4D1 * t4 * t9)
      t151 = t150 ** 2
      t168 = log(0.4D1 * t3)
      t169 = t168 * wd
      t171 = t168 ** 2
      t172 = t171 * wd
      t174 = 0.3D1 * wd - 0.2D1 * t169 + t172 / 0.2D1
      t182 = -0.240D3 * zeta3 - 0.120D3 * t135 * lh + 0.60D2 * lh * t133
      t189 = t171 * t168 * wd
      t192 = 0.2D1 * wd - t169
      t205 = t133 ** 2
      t206 = t135 ** 2
      t215 = t171 ** 2
      t220 = log(-0.4D1 * t46 * t96)
      t221 = t220 ** 2
      t224 = log(0.4D1 * t46 * t3)
      t225 = t224 ** 2
      t228 = log(-0.4D1 * t46 * t20)
      t229 = t228 ** 2
      t232 = log(0.4D1 * t46 * t65)
      t233 = t232 ** 2
      t244 = x2 * t3
      t247 = log(-0.4D1 * t244 * t48)
      t248 = t247 ** 2
      t250 = log(0.4D1 * t244)
      t251 = t250 ** 2
      t266 = -(0.90D2 * wd * (t14 / 0.2D1 + t19 / 0.2D1 - t24 / 0.2D1 - 
     #t29 / 0.2D1) + t37 * (-t13 - t18 + t23 + t28)) * t41 * t43 / 0.20D
     #2 + 0.9D1 / 0.2D1 * wd * (-t52 - t59 + t64 + t68 - t72 + t75 + t80
     # - t84) * t41 * t87 * t43 + (0.90D2 * wd * (-t95 / 0.2D1 + t100 / 
     #0.2D1 + t103 / 0.2D1 - t107 / 0.2D1) + t37 * (t94 - t99 - t102 + t
     #106)) * t87 * t43 / 0.20D2 + (t37 * (-t120 / 0.2D1 + t123 / 0.2D1)
     # + 0.90D2 * wd * (-t123 * t122 / 0.6D1 + t120 * t119 / 0.6D1) + t1
     #39 * (t119 - t122)) * t43 / 0.20D2 + (t37 * (-t147 / 0.2D1 + t151 
     #/ 0.2D1) + 0.90D2 * wd * (-t151 * t150 / 0.6D1 + t147 * t146 / 0.6
     #D1) + t139 * (t146 - t150)) * t41 / 0.40D2 - 0.9D1 / 0.2D1 * t174 
     #* lh + wd * t182 / 0.40D2 - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.4D1 * t
     #169 - 0.9D1 / 0.8D1 * t172 + 0.3D1 / 0.8D1 * t189 + t192 * t137 / 
     #0.40D2 + 0.9D1 / 0.2D1 * (0.4D1 * wd - 0.3D1 * t169 + t172 - t189 
     #/ 0.6D1) * lh - t174 * t137 / 0.40D2 - t192 * t182 / 0.40D2 - wd *
     # (t205 + 0.60D2 * t206 + 0.480D3 * lh * zeta3 - 0.60D2 * t135 * t1
     #33) / 0.40D2 - 0.3D1 / 0.32D2 * t215 * wd - (0.90D2 * wd * (-t221 
     #/ 0.2D1 + t225 / 0.2D1 - t229 / 0.2D1 + t233 / 0.2D1) + t37 * (t22
     #0 - t224 + t228 - t232)) * t41 * t87 / 0.40D2 + (t37 * (t248 / 0.2
     #D1 - t251 / 0.2D1) + 0.90D2 * wd * (t251 * t250 / 0.6D1 - t248 * t
     #247 / 0.6D1) + t139 * (-t247 + t250)) * t87 / 0.40D2
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t266)
      rrgg2gghsoftt9s2e1 = t267 * t266

      end function



      doubleprecision function rrgg2gghsoftt9s2e0
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t3 = Sin(x4 * pi)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = log(0.4D1 * t5)
      t8 = t7 ** 2
      t10 = (-0.1D1 + x1) ** 2
      t13 = log(0.4D1 * t5 * t10)
      t14 = t13 ** 2
      t22 = -0.180D3 * wd * lh + 0.90D2 * wd
      t26 = 0.1D1 / x1
      t29 = x3 * t4
      t31 = -0.1D1 + x3
      t35 = log(-0.4D1 * t29 * t1 * t10 * t31)
      t36 = x3 * t1
      t39 = log(0.4D1 * t36 * t4)
      t40 = t4 * t31
      t43 = log(-0.4D1 * t36 * t40)
      t44 = t4 * t10
      t47 = log(0.4D1 * t36 * t44)
      t50 = 0.1D1 / x3
      t54 = x2 * t1
      t55 = -0.1D1 + x2
      t56 = t4 * t55
      t60 = log(-0.4D1 * t54 * t56 * t10)
      t63 = log(-0.4D1 * t54 * t56)
      t66 = log(0.4D1 * t54 * t44)
      t69 = log(0.4D1 * t54 * t4)
      t72 = 0.1D1 / x2
      t78 = log(0.4D1 * t4)
      t79 = t78 * wd
      t80 = 0.2D1 * wd - t79
      t85 = t78 ** 2
      t86 = t85 * wd
      t88 = pi ** 2
      t90 = lh ** 2
      t92 = -0.30D2 * t88 + 0.180D3 * t90
      t115 = log(0.4D1 * t29)
      t116 = t115 ** 2
      t119 = log(-0.4D1 * t29 * t31)
      t120 = t119 ** 2
      t130 = x2 * x3
      t133 = log(-0.4D1 * t130 * t56)
      t136 = log(0.4D1 * t130 * t4)
      t139 = log(-0.4D1 * t130 * t40)
      t143 = log(0.4D1 * t130 * t40 * t55)
      t149 = x2 * t4
      t152 = log(-0.4D1 * t149 * t55)
      t153 = t152 ** 2
      t155 = log(0.4D1 * t149)
      t156 = t155 ** 2
      t166 = (0.90D2 * wd * (-t8 / 0.2D1 + t14 / 0.2D1) + t22 * (t7 - t1
     #3)) * t26 / 0.20D2 - 0.9D1 / 0.2D1 * wd * (-t35 - t39 + t43 + t47)
     # * t50 * t26 + 0.9D1 / 0.2D1 * wd * (t60 - t63 - t66 + t69) * t72 
     #* t26 - 0.9D1 / 0.2D1 * t80 * lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.
     #4D1 * t79 - 0.9D1 / 0.8D1 * t86 + wd * t92 / 0.40D2 + 0.9D1 / 0.2D
     #1 * (0.3D1 * wd - 0.2D1 * t79 + t86 / 0.2D1) * lh - wd * (-0.240D3
     # * zeta3 - 0.120D3 * t90 * lh + 0.60D2 * lh * t88) / 0.40D2 + 0.3D
     #1 / 0.8D1 * t85 * t78 * wd - t80 * t92 / 0.40D2 + (0.90D2 * wd * (
     #-t116 / 0.2D1 + t120 / 0.2D1) + t22 * (t115 - t119)) * t50 / 0.40D
     #2 - 0.9D1 / 0.4D1 * wd * (t133 - t136 + t139 - t143) * t50 * t72 +
     # (0.90D2 * wd * (t153 / 0.2D1 - t156 / 0.2D1) + t22 * (-t152 + t15
     #5)) * t72 / 0.40D2
      t167 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t166)
      rrgg2gghsoftt9s2e0 = t167 * t166

      end function



      doubleprecision function rrgg2gghsoftt9s2em1
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = x1 ** 2
      t3 = Sin(x4 * pi)
      t4 = t3 ** 2
      t5 = t1 * t4
      t7 = log(0.4D1 * t5)
      t9 = (-0.1D1 + x1) ** 2
      t12 = log(0.4D1 * t5 * t9)
      t22 = log(0.4D1 * t4)
      t23 = t22 * wd
      t29 = t22 ** 2
      t32 = pi ** 2
      t34 = lh ** 2
      t39 = x3 * t4
      t41 = log(0.4D1 * t39)
      t45 = log(-0.4D1 * t39 * (-0.1D1 + x3))
      t51 = x2 * t4
      t55 = log(-0.4D1 * t51 * (-0.1D1 + x2))
      t57 = log(0.4D1 * t51)
      t63 = 0.9D1 / 0.2D1 * wd * (t7 - t12) / x1 - 0.9D1 / 0.2D1 * wd * 
     #lh - 0.9D1 / 0.4D1 * wd + 0.9D1 / 0.4D1 * t23 + 0.9D1 / 0.2D1 * (0
     #.2D1 * wd - t23) * lh - 0.9D1 / 0.8D1 * t29 * wd - wd * (-0.30D2 *
     # t32 + 0.180D3 * t34) / 0.40D2 + 0.9D1 / 0.4D1 * wd * (t41 - t45) 
     #/ x3 + 0.9D1 / 0.4D1 * wd * (-t55 + t57) / x2
      t64 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t63)
      rrgg2gghsoftt9s2em1 = t64 * t63

      end function



      doubleprecision function rrgg2gghsoftt9s2em2
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t5 = Sin(x4 * pi)
      t6 = t5 ** 2
      t8 = log(0.4D1 * t6)
      t11 = -0.9D1 / 0.4D1 * wd + 0.9D1 / 0.2D1 * wd * lh + 0.9D1 / 0.4D
     #1 * t8 * wd
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t11)
      rrgg2gghsoftt9s2em2 = t12 * t11

      end function



      doubleprecision function rrgg2gghsoftt9s2em3
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.4D1 * wd)
      rrgg2gghsoftt9s2em3 = -0.9D1 / 0.4D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt9s2em4
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghsoftt9s2em4 = 0.0D0

      end function
