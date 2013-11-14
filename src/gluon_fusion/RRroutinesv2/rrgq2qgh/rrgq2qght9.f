  
      subroutine rrgq2qght9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh91J1  
      doubleprecision rrgq2qgh91J2  
      doubleprecision rrgq2qgh91J3  
      doubleprecision rrgq2qgh91J4  
      doubleprecision rrgq2qgh91J5  
      doubleprecision rrgq2qgh91J6  
      doubleprecision rrgq2qgh91J7  
      doubleprecision rrgq2qgh92J1  
      doubleprecision rrgq2qgh92J2  
      doubleprecision rrgq2qgh92J3  
      doubleprecision rrgq2qgh92J4  
      doubleprecision rrgq2qgh92J5  
      doubleprecision rrgq2qgh92J6  
      doubleprecision rrgq2qght9s1e1  
      doubleprecision rrgq2qght9s1e0  
      doubleprecision rrgq2qght9s1em1  
      doubleprecision rrgq2qght9s1em2  
      doubleprecision rrgq2qght9s1em3  
      doubleprecision rrgq2qght9s1em4  
      doubleprecision rrgq2qght9s2e1  
      doubleprecision rrgq2qght9s2e0  
      doubleprecision rrgq2qght9s2em1  
      doubleprecision rrgq2qght9s2em2  
      doubleprecision rrgq2qght9s2em3  
      doubleprecision rrgq2qght9s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght9s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght9s1e1
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrgq2qgh91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 ** 2
      t23 = t22 * pi
      t26 = pi ** 2
      t30 = lh ** 2
      t33 = 0.60D2 * lh * t26 - 0.240D3 * zeta3 - 0.120D3 * t30 * lh
      t34 = pi * t33
      t36 = t22 * t21 * pi
      t38 = t21 * pi
      t41 = 0.180D3 * t30 - 0.30D2 * t26
      t44 = (-0.90D2 * t23 * lh + t34 - 0.15D2 * t36 - t38 * t41) * t3
      t45 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t52 = pi * t41
      t54 = (0.180D3 * t38 * lh + 0.45D2 * t23 + t52) * t3
      t55 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t59 = pi * lh
      t63 = (-0.180D3 * t59 - 0.90D2 * t38) * t3
      t64 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t73 = t26 ** 2
      t74 = t30 ** 2
      t82 = t22 ** 2
      t86 = (0.30D2 * t36 * lh + t23 * t41 / 0.2D1 - t38 * t33 + pi * (t
     #73 + 0.60D2 * t74 + 0.480D3 * lh * zeta3 - 0.60D2 * t30 * t26) + 0
     #.15D2 / 0.4D1 * t82 * pi) * t3
      t87 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t91 = x1 ** 2
      t92 = x3 * t91
      t95 = log(0.4D1 * t92 * t19)
      t97 = t95 ** 2
      t104 = t3 * t6
      t110 = t104 * t87
      t111 = t52 * t110
      t113 = 0.1D1 / x3
      t115 = 0.1D1 / x1
      t118 = t91 * t15
      t119 = t12 * t18
      t120 = t118 * t119
      t122 = log(0.4D1 * t120)
      t127 = t122 ** 2
      t130 = t127 * t122
      t138 = t34 * t110
      t149 = x2 * x3
      t150 = t149 * t91
      t153 = log(0.4D1 * t150 * t19)
      t163 = 0.1D1 / x2
      t164 = t163 * t115
      t167 = x2 * t91
      t170 = log(0.4D1 * t167 * t19)
      t172 = t170 ** 2
      t191 = log(0.4D1 * x3 * t15 * t119)
      t196 = t191 ** 2
      t199 = t196 * t191
      t219 = log(0.4D1 * t149 * t19)
      t221 = t219 ** 2
      t240 = log(0.4D1 * x2 * t18 * t16)
      t245 = t240 ** 2
      t248 = t245 * t240
      t266 = t4 * t6 * t7 / 0.16D2 + t44 * t6 * t45 / 0.1440D4 + t54 * t
     #6 * t55 / 0.1440D4 + t63 * t6 * t64 / 0.1440D4 + t86 * t6 * t87 / 
     #0.1440D4 - (0.90D2 * t4 * t6 * (t95 * t45 - t55 - t97 * t87 / 0.2D
     #1) - 0.180D3 * t59 * t104 * (-t45 + t95 * t87) - t111) * t113 * t1
     #15 / 0.720D3 - (t52 * t104 * (-t45 + t122 * t87) + 0.90D2 * t4 * t
     #6 * (-t127 * t45 / 0.2D1 - t64 + t130 * t87 / 0.6D1 + t122 * t55) 
     #- t138 - 0.180D3 * t59 * t104 * (t122 * t45 - t55 - t127 * t87 / 0
     #.2D1)) * t115 / 0.720D3 - (0.90D2 * t4 * t6 * (t153 * t87 - t45) +
     # 0.180D3 * t59 * t110) * t113 * t164 / 0.720D3 - (0.90D2 * t4 * t6
     # * (t170 * t45 - t55 - t172 * t87 / 0.2D1) - 0.180D3 * t59 * t104 
     #* (t170 * t87 - t45) - t111) * t163 * t115 / 0.720D3 - (t52 * t104
     # * (t191 * t87 - t45) + 0.90D2 * t4 * t6 * (-t196 * t45 / 0.2D1 - 
     #t64 + t199 * t87 / 0.6D1 + t191 * t55) - t138 - 0.180D3 * t59 * t1
     #04 * (t191 * t45 - t55 - t196 * t87 / 0.2D1)) * t113 / 0.1440D4 - 
     #(0.90D2 * t4 * t6 * (t219 * t45 - t55 - t221 * t87 / 0.2D1) - 0.18
     #0D3 * t59 * t104 * (t219 * t87 - t45) - t111) * t113 * t163 / 0.14
     #40D4 - (t52 * t104 * (-t45 + t240 * t87) + 0.90D2 * t4 * t6 * (-t2
     #45 * t45 / 0.2D1 - t64 + t248 * t87 / 0.6D1 + t240 * t55) - t138 -
     # 0.180D3 * t59 * t104 * (t240 * t45 - t55 - t245 * t87 / 0.2D1)) *
     # t163 / 0.1440D4
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t266)
      t269 = t2 * x1
      t270 = -0.1D1 + x1
      t271 = t2 * t270
      t273 = x1 * z
      t274 = 0.1D1 - x1 + t273
      t275 = 0.1D1 / t274
      t276 = t270 ** 2
      t277 = t275 * t276
      t281 = log(0.4D1 * t92 * t15 * t119 * t277)
      t282 = t281 ** 2
      t283 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 0
     #.0D0, t269, 0.0D0)
      t286 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 0
     #.0D0, t269, 0.0D0)
      t288 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 0
     #.0D0, t269, 0.0D0)
      t298 = t104 * t283
      t299 = t52 * t298
      t303 = t118 * t12
      t304 = t18 * t275
      t308 = log(0.4D1 * t303 * t304 * t276)
      t313 = t308 ** 2
      t314 = t313 * t308
      t319 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 0
     #.0D0, t269, 0.0D0)
      t336 = t149 * t91 * t18
      t337 = t16 * t277
      t340 = log(0.4D1 * t336 * t337)
      t354 = log(0.4D1 * t167 * t18 * t337)
      t355 = t354 ** 2
      t372 = -(0.90D2 * t4 * t6 * (t282 * t283 / 0.2D1 - t281 * t286 + t
     #288) - 0.180D3 * t59 * t104 * (-t281 * t283 + t286) + t299) * t113
     # * t115 / 0.720D3 - (t52 * t104 * (-t308 * t283 + t286) + 0.90D2 *
     # t4 * t6 * (-t314 * t283 / 0.6D1 + t313 * t286 / 0.2D1 + t319 - t3
     #08 * t288) + t34 * t298 - 0.180D3 * t59 * t104 * (t313 * t283 / 0.
     #2D1 - t308 * t286 + t288)) * t115 / 0.720D3 - (0.90D2 * t4 * t6 * 
     #(-t340 * t283 + t286) - 0.180D3 * t59 * t298) * t113 * t164 / 0.72
     #0D3 - (0.90D2 * t4 * t6 * (t355 * t283 / 0.2D1 - t354 * t286 + t28
     #8) - 0.180D3 * t59 * t104 * (-t354 * t283 + t286) + t299) * t163 *
     # t115 / 0.720D3
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t269, -t271, 0.0D0, t372)
      t375 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t378 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t379 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t390 = t104 * t375
      t391 = t52 * t390
      t403 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t410 = t34 * t390
      t421 = rrgq2qgh92J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t526 = -(0.90D2 * t4 * t6 * (-t221 * t375 / 0.2D1 - t378 + t219 * 
     #t379) - 0.180D3 * t59 * t104 * (-t379 + t219 * t375) - t391) * t11
     #3 * t163 / 0.1440D4 - (t52 * t104 * (-t379 + t240 * t375) + 0.90D2
     # * t4 * t6 * (-t245 * t379 / 0.2D1 + t240 * t378 - t403 + t248 * t
     #375 / 0.6D1) - t410 - 0.180D3 * t59 * t104 * (-t245 * t375 / 0.2D1
     # - t378 + t240 * t379)) * t163 / 0.1440D4 + t4 * t6 * t421 / 0.16D
     #2 - (t52 * t104 * (-t379 + t191 * t375) + 0.90D2 * t4 * t6 * (-t19
     #6 * t379 / 0.2D1 - t403 + t199 * t375 / 0.6D1 + t191 * t378) - t41
     #0 - 0.180D3 * t59 * t104 * (-t196 * t375 / 0.2D1 - t378 + t191 * t
     #379)) * t113 / 0.1440D4 + t54 * t6 * t378 / 0.1440D4 + t63 * t6 * 
     #t403 / 0.1440D4 + t86 * t6 * t375 / 0.1440D4 + t44 * t6 * t379 / 0
     #.1440D4 - (0.90D2 * t4 * t6 * (-t378 + t95 * t379 - t97 * t375 / 0
     #.2D1) - 0.180D3 * t59 * t104 * (-t379 + t95 * t375) - t391) * t113
     # * t115 / 0.720D3 - (t52 * t104 * (-t379 + t122 * t375) + 0.90D2 *
     # t4 * t6 * (-t127 * t379 / 0.2D1 + t130 * t375 / 0.6D1 + t122 * t3
     #78 - t403) - t410 - 0.180D3 * t59 * t104 * (-t378 + t122 * t379 - 
     #t127 * t375 / 0.2D1)) * t115 / 0.720D3 - (0.90D2 * t4 * t6 * (t153
     # * t375 - t379) + 0.180D3 * t59 * t390) * t113 * t164 / 0.720D3 - 
     #(0.90D2 * t4 * t6 * (-t172 * t375 / 0.2D1 - t378 + t170 * t379) - 
     #0.180D3 * t59 * t104 * (-t379 + t170 * t375) - t391) * t163 * t115
     # / 0.720D3
      t527 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t526)
      t529 = t2 * x3
      t530 = -0.1D1 + x3
      t531 = t2 * t530
      t532 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t533 = x3 * t18
      t534 = t16 * t530
      t537 = log(-0.4D1 * t533 * t534)
      t538 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t543 = t537 ** 2
      t546 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t547 = t543 * t537
      t550 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t556 = t104 * t538
      t571 = log(-0.4D1 * t92 * t18 * t534)
      t572 = t571 ** 2
      t585 = t52 * t556
      t595 = log(-0.4D1 * t150 * t18 * t15 * t12 * t530)
      t607 = x2 * t15
      t612 = log(-0.4D1 * t607 * t12 * t533 * t530)
      t613 = t612 ** 2
      t630 = -(t52 * t104 * (t532 - t537 * t538) + 0.90D2 * t4 * t6 * (t
     #543 * t532 / 0.2D1 + t546 - t547 * t538 / 0.6D1 - t537 * t550) + t
     #34 * t556 - 0.180D3 * t59 * t104 * (t543 * t538 / 0.2D1 + t550 - t
     #537 * t532)) * t113 / 0.1440D4 - (0.90D2 * t4 * t6 * (t572 * t538 
     #/ 0.2D1 - t571 * t532 + t550) - 0.180D3 * t59 * t104 * (-t571 * t5
     #38 + t532) + t585) * t113 * t115 / 0.720D3 - (0.90D2 * t4 * t6 * (
     #t532 - t595 * t538) - 0.180D3 * t59 * t556) * t113 * t164 / 0.720D
     #3 - (0.90D2 * t4 * t6 * (t613 * t538 / 0.2D1 + t550 - t612 * t532)
     # - 0.180D3 * t59 * t104 * (t532 - t612 * t538) + t585) * t113 * t1
     #63 / 0.1440D4
      t631 = FJET(XB1, XB2, s, 0.0D0, t529, 0.0D0, -t531, 0.0D0, t630)
      t633 = 0.2D1 * t149
      t634 = cos(t13)
      t635 = -0.1D1 + x2
      t637 = x3 * t530
      t639 = Sqrt(x2 * t635 * t637)
      t641 = 0.2D1 * t634 * t639
      t643 = t2 * (0.1D1 - x2 - x3 + t633 + t641)
      t645 = t2 * (-x3 + t633 - x2 + t641)
      t646 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t645, t643, 0.
     #0D0, 0.0D0, 0.0D0)
      t647 = t149 * t118
      t649 = t119 * t635 * t530
      t652 = log(0.4D1 * t647 * t649)
      t653 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t645, t643, 0.
     #0D0, 0.0D0, 0.0D0)
      t659 = t104 * t653
      t666 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t645, t643, 0.
     #0D0, 0.0D0, 0.0D0)
      t667 = t149 * t15
      t670 = log(0.4D1 * t667 * t649)
      t671 = t670 ** 2
      t689 = -(0.90D2 * t4 * t6 * (-t646 + t652 * t653) + 0.180D3 * t59 
     #* t659) * t113 * t164 / 0.720D3 - (0.90D2 * t4 * t6 * (-t666 - t67
     #1 * t653 / 0.2D1 + t670 * t646) - 0.180D3 * t59 * t104 * (-t646 + 
     #t670 * t653) - t52 * t659) * t113 * t163 / 0.1440D4
      t690 = FJET(XB1, XB2, s, 0.0D0, t643, 0.0D0, -t645, 0.0D0, t689)
      t693 = x2 * s * t1
      t694 = t635 * s
      t695 = t694 * t1
      t700 = log(-0.4D1 * t150 * t16 * t18 * t635)
      t701 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t703 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t708 = t104 * t701
      t716 = t119 * t635
      t719 = log(-0.4D1 * t167 * t15 * t716)
      t720 = t719 ** 2
      t723 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t734 = t52 * t708
      t741 = log(-0.4D1 * t667 * t716)
      t742 = t741 ** 2
      t761 = log(-0.4D1 * t607 * t716)
      t766 = t761 ** 2
      t770 = t766 * t761
      t773 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t789 = -(0.90D2 * t4 * t6 * (-t700 * t701 + t703) - 0.180D3 * t59 
     #* t708) * t113 * t164 / 0.720D3 - (0.90D2 * t4 * t6 * (t720 * t701
     # / 0.2D1 + t723 - t719 * t703) - 0.180D3 * t59 * t104 * (t703 - t7
     #19 * t701) + t734) * t163 * t115 / 0.720D3 - (0.90D2 * t4 * t6 * (
     #t742 * t701 / 0.2D1 - t741 * t703 + t723) - 0.180D3 * t59 * t104 *
     # (t703 - t741 * t701) + t734) * t113 * t163 / 0.1440D4 - (t52 * t1
     #04 * (t703 - t761 * t701) + 0.90D2 * t4 * t6 * (t766 * t703 / 0.2D
     #1 - t761 * t723 - t770 * t701 / 0.6D1 + t773) + t34 * t708 - 0.180
     #D3 * t59 * t104 * (-t761 * t703 + t723 + t766 * t701 / 0.2D1)) * t
     #163 / 0.1440D4
      t790 = FJET(XB1, XB2, s, 0.0D0, t693, 0.0D0, -t695, 0.0D0, t789)
      t792 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t793 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t800 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t803 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t809 = t104 * t793
      t833 = t52 * t809
      t865 = -(t52 * t104 * (t792 - t537 * t793) + 0.90D2 * t4 * t6 * (t
     #543 * t792 / 0.2D1 + t800 - t547 * t793 / 0.6D1 - t537 * t803) + t
     #34 * t809 - 0.180D3 * t59 * t104 * (-t537 * t792 + t803 + t543 * t
     #793 / 0.2D1)) * t113 / 0.1440D4 - (0.90D2 * t4 * t6 * (-t571 * t79
     #2 + t572 * t793 / 0.2D1 + t803) - 0.180D3 * t59 * t104 * (t792 - t
     #571 * t793) + t833) * t113 * t115 / 0.720D3 - (0.90D2 * t4 * t6 * 
     #(t792 - t595 * t793) - 0.180D3 * t59 * t809) * t113 * t164 / 0.720
     #D3 - (0.90D2 * t4 * t6 * (-t612 * t792 + t803 + t613 * t793 / 0.2D
     #1) - 0.180D3 * t59 * t104 * (t792 - t612 * t793) + t833) * t113 * 
     #t163 / 0.1440D4
      t866 = FJET(XB1, XB2, s, 0.0D0, -t531, 0.0D0, t529, 0.0D0, t865)
      t868 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t645, t643, 0.
     #0D0, 0.0D0, 0.0D0)
      t870 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t645, t643, 0.
     #0D0, 0.0D0, 0.0D0)
      t875 = t104 * t868
      t882 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t645, t643, 0.
     #0D0, 0.0D0, 0.0D0)
      t900 = -(0.90D2 * t4 * t6 * (t652 * t868 - t870) + 0.180D3 * t59 *
     # t875) * t113 * t164 / 0.720D3 - (0.90D2 * t4 * t6 * (-t882 + t670
     # * t870 - t671 * t868 / 0.2D1) - 0.180D3 * t59 * t104 * (-t870 + t
     #670 * t868) - t52 * t875) * t113 * t163 / 0.1440D4
      t901 = FJET(XB1, XB2, s, 0.0D0, -t645, 0.0D0, t643, 0.0D0, t900)
      t903 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t904 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t910 = t104 * t904
      t919 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t930 = t52 * t910
      t958 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, t693, -t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t976 = -(0.90D2 * t4 * t6 * (t903 - t700 * t904) - 0.180D3 * t59 *
     # t910) * t113 * t164 / 0.720D3 - (0.90D2 * t4 * t6 * (t720 * t904 
     #/ 0.2D1 + t919 - t719 * t903) - 0.180D3 * t59 * t104 * (-t719 * t9
     #04 + t903) + t930) * t163 * t115 / 0.720D3 - (0.90D2 * t4 * t6 * (
     #t742 * t904 / 0.2D1 + t919 - t741 * t903) - 0.180D3 * t59 * t104 *
     # (t903 - t741 * t904) + t930) * t113 * t163 / 0.1440D4 - (t52 * t1
     #04 * (t903 - t761 * t904) + 0.90D2 * t4 * t6 * (-t761 * t919 - t77
     #0 * t904 / 0.6D1 + t958 + t766 * t903 / 0.2D1) + t34 * t910 - 0.18
     #0D3 * t59 * t104 * (t766 * t904 / 0.2D1 + t919 - t761 * t903)) * t
     #163 / 0.1440D4
      t977 = FJET(XB1, XB2, s, 0.0D0, -t695, 0.0D0, t693, 0.0D0, t976)
      t981 = t2 * t270 * x2 * t275
      t982 = t1 * t270
      t983 = t694 * t982
      t988 = s * t17 * x2 * x1 * t270 * t275
      t993 = log(-0.4D1 * t647 * t119 * t277 * t635)
      t994 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t981, t983, 0.
     #0D0, t269, -t988)
      t996 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t981, t983, 0.
     #0D0, t269, -t988)
      t1001 = t104 * t994
      t1008 = t276 * t635
      t1012 = log(-0.4D1 * t167 * t16 * t304 * t1008)
      t1013 = t1012 ** 2
      t1017 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t981, t983, 0
     #.0D0, t269, -t988)
      t1032 = -(0.90D2 * t4 * t6 * (t993 * t994 - t996) + 0.180D3 * t59 
     #* t1001) * t113 * t164 / 0.720D3 - (0.90D2 * t4 * t6 * (-t1013 * t
     #994 / 0.2D1 + t1012 * t996 - t1017) - 0.180D3 * t59 * t104 * (t101
     #2 * t994 - t996) - t52 * t1001) * t163 * t115 / 0.720D3
      t1033 = FJET(XB1, XB2, s, 0.0D0, -t981, t269, t983, -t988, t1032)
      t1035 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t981, t983, 0
     #.0D0, t269, -t988)
      t1036 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t981, t983, 0
     #.0D0, t269, -t988)
      t1042 = t104 * t1036
      t1051 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t981, t983, 0
     #.0D0, t269, -t988)
      t1066 = -(0.90D2 * t4 * t6 * (-t1035 + t993 * t1036) + 0.180D3 * t
     #59 * t1042) * t113 * t164 / 0.720D3 - (0.90D2 * t4 * t6 * (t1012 *
     # t1035 - t1013 * t1036 / 0.2D1 - t1051) - 0.180D3 * t59 * t104 * (
     #-t1035 + t1012 * t1036) - t52 * t1042) * t163 * t115 / 0.720D3
      t1067 = FJET(XB1, XB2, s, t269, t983, 0.0D0, -t981, -t988, t1066)
      t1069 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 
     #0.0D0, t269, 0.0D0)
      t1070 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 
     #0.0D0, t269, 0.0D0)
      t1072 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 
     #0.0D0, t269, 0.0D0)
      t1084 = t104 * t1072
      t1085 = t52 * t1084
      t1093 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, 
     #0.0D0, t269, 0.0D0)
      t1139 = -(0.90D2 * t4 * t6 * (t1069 - t281 * t1070 + t282 * t1072 
     #/ 0.2D1) - 0.180D3 * t59 * t104 * (t1070 - t281 * t1072) + t1085) 
     #* t113 * t115 / 0.720D3 - (t52 * t104 * (t1070 - t308 * t1072) + 0
     #.90D2 * t4 * t6 * (t1093 + t313 * t1070 / 0.2D1 - t314 * t1072 / 0
     #.6D1 - t308 * t1069) + t34 * t1084 - 0.180D3 * t59 * t104 * (t313 
     #* t1072 / 0.2D1 + t1069 - t308 * t1070)) * t115 / 0.720D3 - (0.90D
     #2 * t4 * t6 * (-t340 * t1072 + t1070) - 0.180D3 * t59 * t1084) * t
     #113 * t164 / 0.720D3 - (0.90D2 * t4 * t6 * (-t354 * t1070 + t355 *
     # t1072 / 0.2D1 + t1069) - 0.180D3 * t59 * t104 * (t1070 - t354 * t
     #1072) + t1085) * t163 * t115 / 0.720D3
      t1140 = FJET(XB1, XB2, s, t269, -t271, 0.0D0, 0.0D0, 0.0D0, t1139)
      t1142 = x3 * x1
      t1143 = t2 * t1142
      t1144 = t1142 * z
      t1145 = t149 * x1
      t1146 = t149 * t273
      t1151 = Sqrt(x3 * t635 * t274 * x2 * t530)
      t1153 = 0.2D1 * t634 * t1151
      t1157 = t2 * t270 * (-x3 + t1142 - t1144 + t633 - t1145 + t1146 - 
     #x2 + t1153) * t275
      t1158 = t530 * s
      t1160 = t1158 * t1 * x1
      t1161 = x2 * x1
      t1163 = 0.1D1 - x1 + t273 - x2 + t1161 - t1161 * z - x3 + t1142 - 
     #t1144 + t633 - t1145 + t1146 + t1153
      t1166 = t2 * t270 * t1163 * t275
      t1172 = log(0.4D1 * t149 * t303 * t304 * t1008 * t530)
      t1173 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1166,
     # t1143, -t1160, -t988)
      t1175 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1166,
     # t1143, -t1160, -t988)
      t1183 = 0.90D2 * t4 * t6 * (-t1172 * t1173 + t1175) - 0.180D3 * t5
     #9 * t104 * t1173
      t1187 = FJET(XB1, XB2, s, t1143, t1157, -t1160, -t1166, -t988, -t1
     #183 * t113 * t164 / 0.720D3)
      t1190 = t113 * t163 * t115
      t1194 = x3 * s * t982
      t1195 = t1158 * t982
      t1199 = log(-0.4D1 * t120 * t637 * t277)
      t1200 = t1199 ** 2
      t1201 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t1194, t1195,
     # t1143, -t1160, 0.0D0)
      t1204 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t1194, t1195,
     # t1143, -t1160, 0.0D0)
      t1205 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t1194, t1195,
     # t1143, -t1160, 0.0D0)
      t1216 = t104 * t1201
      t1225 = log(-0.4D1 * t336 * t16 * t277 * t530)
      t1237 = -(0.90D2 * t4 * t6 * (-t1200 * t1201 / 0.2D1 - t1204 + t11
     #99 * t1205) - 0.180D3 * t59 * t104 * (-t1205 + t1199 * t1201) - t5
     #2 * t1216) * t113 * t115 / 0.720D3 - (0.90D2 * t4 * t6 * (-t1205 +
     # t1225 * t1201) + 0.180D3 * t59 * t1216) * t113 * t164 / 0.720D3
      t1238 = FJET(XB1, XB2, s, t1143, -t1194, -t1160, t1195, 0.0D0, t12
     #37)
      t1240 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t1194, t1195,
     # t1143, -t1160, 0.0D0)
      t1242 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t1194, t1195,
     # t1143, -t1160, 0.0D0)
      t1243 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t1194, t1195,
     # t1143, -t1160, 0.0D0)
      t1255 = t104 * t1243
      t1271 = -(0.90D2 * t4 * t6 * (t1199 * t1240 - t1242 - t1200 * t124
     #3 / 0.2D1) - 0.180D3 * t59 * t104 * (-t1240 + t1199 * t1243) - t52
     # * t1255) * t113 * t115 / 0.720D3 - (0.90D2 * t4 * t6 * (t1225 * t
     #1243 - t1240) + 0.180D3 * t59 * t1255) * t113 * t164 / 0.720D3
      t1272 = FJET(XB1, XB2, s, -t1160, t1195, t1143, -t1194, 0.0D0, t12
     #71)
      t1274 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1166,
     # t1143, -t1160, -t988)
      t1275 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1166,
     # t1143, -t1160, -t988)
      t1284 = 0.90D2 * t4 * t6 * (t1274 - t1172 * t1275) - 0.180D3 * t59
     # * t104 * t1275
      t1288 = FJET(XB1, XB2, s, -t1160, -t1166, t1143, t1157, -t988, -t1
     #284 * t113 * t164 / 0.720D3)
      rrgq2qght9s1e1 = t267 * t266 + t373 * t372 + t527 * t526 + t631 * 
     #t630 + t690 * t689 + t790 * t789 + t866 * t865 + t901 * t900 + t97
     #7 * t976 + t1033 * t1032 + t1067 * t1066 + t1140 * t1139 - t1187 *
     # t1183 * t1190 / 0.720D3 + t1238 * t1237 + t1272 * t1271 - t1288 *
     # t1284 * t1190 / 0.720D3

      end function



      doubleprecision function rrgq2qght9s1e0
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t15 = t12 * t14
      t18 = log(0.4D1 * x3 * t9 * t15)
      t19 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = t18 ** 2
      t23 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t30 = pi * lh
      t31 = t3 * t6
      t37 = lh ** 2
      t39 = pi ** 2
      t41 = 0.180D3 * t37 - 0.30D2 * t39
      t42 = pi * t41
      t43 = t31 * t23
      t44 = t42 * t43
      t46 = 0.1D1 / x3
      t49 = t12 * t9
      t50 = t49 * t14
      t52 = log(0.4D1 * t50)
      t53 = t52 * pi
      t56 = t52 ** 2
      t57 = t56 * pi
      t60 = (0.180D3 * t53 * lh + 0.45D2 * t57 + t42) * t3
      t64 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t82 = (-0.90D2 * t57 * lh + pi * (0.60D2 * lh * t39 - 0.240D3 * ze
     #ta3 - 0.120D3 * t37 * lh) - 0.15D2 * t56 * t52 * pi - t53 * t41) *
     # t3
      t89 = (-0.180D3 * t30 - 0.90D2 * t53) * t3
      t93 = x2 * x3
      t96 = log(0.4D1 * t93 * t50)
      t103 = 0.180D3 * t30 * t43
      t106 = 0.1D1 / x2
      t112 = log(0.4D1 * x2 * t14 * t49)
      t114 = t112 ** 2
      t129 = x1 ** 2
      t130 = x3 * t129
      t133 = log(0.4D1 * t130 * t50)
      t141 = 0.1D1 / x1
      t144 = t4 * t6
      t146 = t106 * t141
      t150 = x2 * t129
      t153 = log(0.4D1 * t150 * t50)
      t163 = t129 * t9
      t164 = t163 * t15
      t166 = log(0.4D1 * t164)
      t168 = t166 ** 2
      t183 = -(0.90D2 * t4 * t6 * (t18 * t19 - t21 - t22 * t23 / 0.2D1) 
     #- 0.180D3 * t30 * t31 * (t18 * t23 - t19) - t44) * t46 / 0.1440D4 
     #+ t60 * t6 * t19 / 0.1440D4 + t4 * t6 * t64 / 0.16D2 + t82 * t6 * 
     #t23 / 0.1440D4 + t89 * t6 * t21 / 0.1440D4 - (0.90D2 * t4 * t6 * (
     #t96 * t23 - t19) + t103) * t46 * t106 / 0.1440D4 - (0.90D2 * t4 * 
     #t6 * (t112 * t19 - t21 - t114 * t23 / 0.2D1) - 0.180D3 * t30 * t31
     # * (-t19 + t112 * t23) - t44) * t106 / 0.1440D4 - (0.90D2 * t4 * t
     #6 * (-t19 + t133 * t23) + t103) * t46 * t141 / 0.720D3 + t144 * t2
     #3 * t46 * t146 / 0.8D1 - (0.90D2 * t4 * t6 * (t153 * t23 - t19) + 
     #t103) * t106 * t141 / 0.720D3 - (0.90D2 * t4 * t6 * (t166 * t19 - 
     #t21 - t168 * t23 / 0.2D1) - 0.180D3 * t30 * t31 * (-t19 + t166 * t
     #23) - t44) * t141 / 0.720D3
      t184 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t183)
      t186 = t2 * x1
      t187 = -0.1D1 + x1
      t188 = t2 * t187
      t190 = x1 * z
      t191 = 0.1D1 - x1 + t190
      t192 = 0.1D1 / t191
      t193 = t187 ** 2
      t194 = t192 * t193
      t198 = log(0.4D1 * t130 * t9 * t15 * t194)
      t199 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t186, 0.0D0)
      t201 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t186, 0.0D0)
      t206 = t31 * t199
      t208 = 0.180D3 * t30 * t206
      t221 = log(0.4D1 * t150 * t14 * t49 * t194)
      t232 = t14 * t192
      t236 = log(0.4D1 * t163 * t12 * t232 * t193)
      t237 = t236 ** 2
      t241 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t186, 0.0D0)
      t255 = -(0.90D2 * t4 * t6 * (-t198 * t199 + t201) - t208) * t46 * 
     #t141 / 0.720D3 - t144 * t199 * t46 * t146 / 0.8D1 - (0.90D2 * t4 *
     # t6 * (-t221 * t199 + t201) - t208) * t106 * t141 / 0.720D3 - (0.9
     #0D2 * t4 * t6 * (t237 * t199 / 0.2D1 - t236 * t201 + t241) - 0.180
     #D3 * t30 * t31 * (-t236 * t199 + t201) + t42 * t206) * t141 / 0.72
     #0D3
      t256 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t186, -t188, 0.0D0, t255)
      t258 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t261 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t262 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t273 = t31 * t258
      t274 = t42 * t273
      t278 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t294 = 0.180D3 * t30 * t273
      t354 = -(0.90D2 * t4 * t6 * (-t22 * t258 / 0.2D1 - t261 + t18 * t2
     #62) - 0.180D3 * t30 * t31 * (-t262 + t18 * t258) - t274) * t46 / 0
     #.1440D4 + t4 * t6 * t278 / 0.16D2 + t89 * t6 * t261 / 0.1440D4 + t
     #60 * t6 * t262 / 0.1440D4 - (0.90D2 * t4 * t6 * (-t262 + t96 * t25
     #8) + t294) * t46 * t106 / 0.1440D4 - (0.90D2 * t4 * t6 * (-t114 * 
     #t258 / 0.2D1 - t261 + t112 * t262) - 0.180D3 * t30 * t31 * (-t262 
     #+ t112 * t258) - t274) * t106 / 0.1440D4 + t82 * t6 * t258 / 0.144
     #0D4 - (0.90D2 * t4 * t6 * (-t262 + t133 * t258) + t294) * t46 * t1
     #41 / 0.720D3 + t144 * t258 * t46 * t146 / 0.8D1 - (0.90D2 * t4 * t
     #6 * (-t262 + t153 * t258) + t294) * t106 * t141 / 0.720D3 - (0.90D
     #2 * t4 * t6 * (-t261 + t166 * t262 - t168 * t258 / 0.2D1) - 0.180D
     #3 * t30 * t31 * (-t262 + t166 * t258) - t274) * t141 / 0.720D3
      t355 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t354)
      t357 = t2 * x3
      t358 = -0.1D1 + x3
      t359 = t2 * t358
      t360 = x3 * t14
      t361 = t49 * t358
      t364 = log(-0.4D1 * t360 * t361)
      t365 = t364 ** 2
      t366 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t369 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t370 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t381 = t31 * t366
      t386 = x2 * t9
      t391 = log(-0.4D1 * t386 * t12 * t360 * t358)
      t398 = 0.180D3 * t30 * t381
      t406 = log(-0.4D1 * t130 * t14 * t361)
      t420 = -(0.90D2 * t4 * t6 * (t365 * t366 / 0.2D1 + t369 - t364 * t
     #370) - 0.180D3 * t30 * t31 * (t370 - t364 * t366) + t42 * t381) * 
     #t46 / 0.1440D4 - (0.90D2 * t4 * t6 * (t370 - t391 * t366) - t398) 
     #* t46 * t106 / 0.1440D4 - (0.90D2 * t4 * t6 * (-t406 * t366 + t370
     #) - t398) * t46 * t141 / 0.720D3 - t144 * t366 * t46 * t146 / 0.8D
     #1
      t421 = FJET(XB1, XB2, s, 0.0D0, t357, 0.0D0, -t359, 0.0D0, t420)
      t423 = 0.2D1 * t93
      t424 = cos(t7)
      t425 = -0.1D1 + x2
      t427 = x3 * t358
      t429 = Sqrt(x2 * t425 * t427)
      t431 = 0.2D1 * t424 * t429
      t433 = t2 * (0.1D1 - x2 - x3 + t423 + t431)
      t435 = t2 * (-x3 + t423 - x2 + t431)
      t436 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t435, t433, 0.
     #0D0, 0.0D0, 0.0D0)
      t437 = t93 * t9
      t442 = log(0.4D1 * t437 * t15 * t425 * t358)
      t443 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t435, t433, 0.
     #0D0, 0.0D0, 0.0D0)
      t460 = -(0.90D2 * t4 * t6 * (-t436 + t442 * t443) + 0.180D3 * t30 
     #* t31 * t443) * t46 * t106 / 0.1440D4 + t144 * t443 * t46 * t146 /
     # 0.8D1
      t461 = FJET(XB1, XB2, s, 0.0D0, t433, 0.0D0, -t435, 0.0D0, t460)
      t464 = x2 * s * t1
      t465 = t425 * s
      t466 = t465 * t1
      t467 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t464, -t466, 0.
     #0D0, 0.0D0, 0.0D0)
      t468 = t15 * t425
      t471 = log(-0.4D1 * t437 * t468)
      t472 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t464, -t466, 0.
     #0D0, 0.0D0, 0.0D0)
      t478 = t31 * t472
      t480 = 0.180D3 * t30 * t478
      t487 = log(-0.4D1 * t386 * t468)
      t489 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t464, -t466, 0.
     #0D0, 0.0D0, 0.0D0)
      t490 = t487 ** 2
      t513 = log(-0.4D1 * t150 * t9 * t468)
      t523 = -(0.90D2 * t4 * t6 * (t467 - t471 * t472) - t480) * t46 * t
     #106 / 0.1440D4 - (0.90D2 * t4 * t6 * (-t487 * t467 + t489 + t490 *
     # t472 / 0.2D1) - 0.180D3 * t30 * t31 * (t467 - t487 * t472) + t42 
     #* t478) * t106 / 0.1440D4 - t144 * t472 * t46 * t146 / 0.8D1 - (0.
     #90D2 * t4 * t6 * (t467 - t513 * t472) - t480) * t106 * t141 / 0.72
     #0D3
      t524 = FJET(XB1, XB2, s, 0.0D0, t464, 0.0D0, -t466, 0.0D0, t523)
      t526 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t528 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t529 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t541 = t31 * t529
      t552 = 0.180D3 * t30 * t541
      t570 = -(0.90D2 * t4 * t6 * (-t364 * t526 + t528 + t365 * t529 / 0
     #.2D1) - 0.180D3 * t30 * t31 * (t526 - t364 * t529) + t42 * t541) *
     # t46 / 0.1440D4 - (0.90D2 * t4 * t6 * (t526 - t391 * t529) - t552)
     # * t46 * t106 / 0.1440D4 - (0.90D2 * t4 * t6 * (t526 - t406 * t529
     #) - t552) * t46 * t141 / 0.720D3 - t144 * t529 * t46 * t146 / 0.8D
     #1
      t571 = FJET(XB1, XB2, s, 0.0D0, -t359, 0.0D0, t357, 0.0D0, t570)
      t573 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t435, t433, 0.
     #0D0, 0.0D0, 0.0D0)
      t574 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t435, t433, 0.
     #0D0, 0.0D0, 0.0D0)
      t591 = -(0.90D2 * t4 * t6 * (-t573 + t442 * t574) + 0.180D3 * t30 
     #* t31 * t574) * t46 * t106 / 0.1440D4 + t144 * t574 * t46 * t146 /
     # 0.8D1
      t592 = FJET(XB1, XB2, s, 0.0D0, -t435, 0.0D0, t433, 0.0D0, t591)
      t594 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t464, -t466, 0.
     #0D0, 0.0D0, 0.0D0)
      t595 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t464, -t466, 0.
     #0D0, 0.0D0, 0.0D0)
      t601 = t31 * t595
      t603 = 0.180D3 * t30 * t601
      t610 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t464, -t466, 0.
     #0D0, 0.0D0, 0.0D0)
      t638 = -(0.90D2 * t4 * t6 * (t594 - t471 * t595) - t603) * t46 * t
     #106 / 0.1440D4 - (0.90D2 * t4 * t6 * (t490 * t595 / 0.2D1 + t610 -
     # t487 * t594) - 0.180D3 * t30 * t31 * (t594 - t487 * t595) + t42 *
     # t601) * t106 / 0.1440D4 - t144 * t595 * t46 * t146 / 0.8D1 - (0.9
     #0D2 * t4 * t6 * (-t513 * t595 + t594) - t603) * t106 * t141 / 0.72
     #0D3
      t639 = FJET(XB1, XB2, s, 0.0D0, -t466, 0.0D0, t464, 0.0D0, t638)
      t643 = t2 * t187 * x2 * t192
      t644 = t1 * t187
      t645 = t465 * t644
      t650 = s * t13 * x2 * x1 * t187 * t192
      t651 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t643, t645, 0.
     #0D0, t186, -t650)
      t661 = log(-0.4D1 * t150 * t49 * t232 * t193 * t425)
      t663 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t643, t645, 0.
     #0D0, t186, -t650)
      t675 = t144 * t651 * t46 * t146 / 0.8D1 - (0.90D2 * t4 * t6 * (t66
     #1 * t651 - t663) + 0.180D3 * t30 * t31 * t651) * t106 * t141 / 0.7
     #20D3
      t676 = FJET(XB1, XB2, s, 0.0D0, -t643, t186, t645, -t650, t675)
      t678 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t643, t645, 0.
     #0D0, t186, -t650)
      t683 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t643, t645, 0.
     #0D0, t186, -t650)
      t696 = t144 * t678 * t46 * t146 / 0.8D1 - (0.90D2 * t4 * t6 * (-t6
     #83 + t661 * t678) + 0.180D3 * t30 * t31 * t678) * t106 * t141 / 0.
     #720D3
      t697 = FJET(XB1, XB2, s, t186, t645, 0.0D0, -t643, -t650, t696)
      t699 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t186, 0.0D0)
      t700 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t186, 0.0D0)
      t706 = t31 * t700
      t708 = 0.180D3 * t30 * t706
      t728 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t186, 0.0D0)
      t743 = -(0.90D2 * t4 * t6 * (t699 - t198 * t700) - t708) * t46 * t
     #141 / 0.720D3 - t144 * t700 * t46 * t146 / 0.8D1 - (0.90D2 * t4 * 
     #t6 * (t699 - t221 * t700) - t708) * t106 * t141 / 0.720D3 - (0.90D
     #2 * t4 * t6 * (t237 * t700 / 0.2D1 + t728 - t236 * t699) - 0.180D3
     # * t30 * t31 * (t699 - t236 * t700) + t42 * t706) * t141 / 0.720D3
      t744 = FJET(XB1, XB2, s, t186, -t188, 0.0D0, 0.0D0, 0.0D0, t743)
      t746 = x3 * x1
      t747 = t2 * t746
      t748 = t746 * z
      t749 = t93 * x1
      t750 = t93 * t190
      t755 = Sqrt(x3 * t425 * t191 * x2 * t358)
      t757 = 0.2D1 * t424 * t755
      t761 = t2 * t187 * (-x3 + t746 - t748 + t423 - t749 + t750 - x2 + 
     #t757) * t192
      t762 = t358 * s
      t764 = t762 * t1 * x1
      t765 = x2 * x1
      t767 = 0.1D1 - x1 + t190 - x2 + t765 - t765 * z - x3 + t746 - t748
     # + t423 - t749 + t750 + t757
      t770 = t2 * t187 * t767 * t192
      t771 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t761, -t770, t7
     #47, -t764, -t650)
      t773 = t771 * t46 * t146
      t776 = FJET(XB1, XB2, s, t747, t761, -t764, -t770, -t650, -t144 * 
     #t773 / 0.8D1)
      t782 = x3 * s * t644
      t783 = t762 * t644
      t784 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t782, t783, t7
     #47, -t764, 0.0D0)
      t788 = log(-0.4D1 * t164 * t427 * t194)
      t789 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t782, t783, t7
     #47, -t764, 0.0D0)
      t806 = -(0.90D2 * t4 * t6 * (-t784 + t788 * t789) + 0.180D3 * t30 
     #* t31 * t789) * t46 * t141 / 0.720D3 + t144 * t789 * t46 * t146 / 
     #0.8D1
      t807 = FJET(XB1, XB2, s, t747, -t782, -t764, t783, 0.0D0, t806)
      t809 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t782, t783, t7
     #47, -t764, 0.0D0)
      t810 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t782, t783, t7
     #47, -t764, 0.0D0)
      t827 = -(0.90D2 * t4 * t6 * (-t809 + t788 * t810) + 0.180D3 * t30 
     #* t31 * t810) * t46 * t141 / 0.720D3 + t144 * t810 * t46 * t146 / 
     #0.8D1
      t828 = FJET(XB1, XB2, s, -t764, t783, t747, -t782, 0.0D0, t827)
      t830 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t761, -t770, t7
     #47, -t764, -t650)
      t832 = t830 * t46 * t146
      t835 = FJET(XB1, XB2, s, -t764, -t770, t747, t761, -t650, -t144 * 
     #t832 / 0.8D1)
      rrgq2qght9s1e0 = t184 * t183 + t256 * t255 + t355 * t354 + t421 * 
     #t420 + t461 * t460 + t524 * t523 + t571 * t570 + t592 * t591 + t63
     #9 * t638 + t676 * t675 + t697 * t696 + t744 * t743 - t776 * pi * t
     #31 * t773 / 0.8D1 + t807 * t806 + t828 * t827 - t835 * pi * t31 * 
     #t832 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s1em1
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t15 = t12 * t14
      t18 = log(0.4D1 * x3 * t9 * t15)
      t19 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = pi * lh
      t27 = t3 * t6
      t30 = 0.180D3 * t26 * t27 * t19
      t32 = 0.1D1 / x3
      t35 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t40 = t12 * t9
      t43 = log(0.4D1 * t40 * t14)
      t44 = t43 * pi
      t47 = (-0.180D3 * t26 - 0.90D2 * t44) * t3
      t53 = t43 ** 2
      t56 = lh ** 2
      t58 = pi ** 2
      t63 = (0.180D3 * t44 * lh + 0.45D2 * t53 * pi + pi * (0.180D3 * t5
     #6 - 0.30D2 * t58)) * t3
      t67 = t4 * t6
      t68 = t19 * t32
      t69 = 0.1D1 / x2
      t76 = log(0.4D1 * x2 * t14 * t40)
      t86 = 0.1D1 / x1
      t90 = x1 ** 2
      t91 = t90 * t9
      t94 = log(0.4D1 * t91 * t15)
      t106 = -(0.90D2 * t4 * t6 * (t18 * t19 - t21) + t30) * t32 / 0.144
     #0D4 + t4 * t6 * t35 / 0.16D2 + t47 * t6 * t21 / 0.1440D4 + t63 * t
     #6 * t19 / 0.1440D4 + t67 * t68 * t69 / 0.16D2 - (0.90D2 * t4 * t6 
     #* (-t21 + t76 * t19) + t30) * t69 / 0.1440D4 + t67 * t19 * t69 * t
     #86 / 0.8D1 - (0.90D2 * t4 * t6 * (-t21 + t94 * t19) + t30) * t86 /
     # 0.720D3 + t67 * t68 * t86 / 0.8D1
      t107 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t106)
      t109 = t2 * x1
      t110 = -0.1D1 + x1
      t111 = t2 * t110
      t112 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t111, 0
     #.0D0, t109, 0.0D0)
      t120 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t122 = t110 ** 2
      t126 = log(0.4D1 * t91 * t12 * t14 * t120 * t122)
      t128 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t111, 0
     #.0D0, t109, 0.0D0)
      t143 = -t67 * t112 * t69 * t86 / 0.8D1 - (0.90D2 * t4 * t6 * (-t12
     #6 * t112 + t128) - 0.180D3 * t26 * t27 * t112) * t86 / 0.720D3 - t
     #67 * t112 * t32 * t86 / 0.8D1
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t109, -t111, 0.0D0, t143)
      t146 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t150 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t154 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t162 = 0.180D3 * t26 * t27 * t150
      t166 = t150 * t32
      t196 = t4 * t6 * t146 / 0.16D2 + t63 * t6 * t150 / 0.1440D4 - (0.9
     #0D2 * t4 * t6 * (-t154 + t18 * t150) + t162) * t32 / 0.1440D4 + t6
     #7 * t166 * t69 / 0.16D2 - (0.90D2 * t4 * t6 * (-t154 + t76 * t150)
     # + t162) * t69 / 0.1440D4 + t47 * t6 * t154 / 0.1440D4 + t67 * t15
     #0 * t69 * t86 / 0.8D1 - (0.90D2 * t4 * t6 * (-t154 + t94 * t150) +
     # t162) * t86 / 0.720D3 + t67 * t166 * t86 / 0.8D1
      t197 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t196)
      t199 = t2 * x3
      t200 = -0.1D1 + x3
      t201 = t2 * t200
      t202 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t207 = log(-0.4D1 * x3 * t14 * t40 * t200)
      t208 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t220 = t208 * t32
      t227 = -(0.90D2 * t4 * t6 * (t202 - t207 * t208) - 0.180D3 * t26 *
     # t27 * t208) * t32 / 0.1440D4 - t67 * t220 * t69 / 0.16D2 - t67 * 
     #t220 * t86 / 0.8D1
      t228 = FJET(XB1, XB2, s, 0.0D0, t199, 0.0D0, -t201, 0.0D0, t227)
      t231 = 0.2D1 * x2 * x3
      t232 = cos(t7)
      t233 = -0.1D1 + x2
      t237 = Sqrt(x2 * t233 * x3 * t200)
      t239 = 0.2D1 * t232 * t237
      t241 = t2 * (0.1D1 - x2 - x3 + t231 + t239)
      t243 = t2 * (-x3 + t231 - x2 + t239)
      t244 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t243, t241, 0.
     #0D0, 0.0D0, 0.0D0)
      t249 = FJET(XB1, XB2, s, 0.0D0, t241, 0.0D0, -t243, 0.0D0, t67 * t
     #244 * t32 * t69 / 0.16D2)
      t253 = t32 * t69
      t258 = x2 * s * t1
      t259 = t233 * s
      t260 = t259 * t1
      t261 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t266 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t271 = log(-0.4D1 * x2 * t9 * t15 * t233)
      t287 = -t67 * t261 * t32 * t69 / 0.16D2 - (0.90D2 * t4 * t6 * (t26
     #6 - t271 * t261) - 0.180D3 * t26 * t27 * t261) * t69 / 0.1440D4 - 
     #t67 * t261 * t69 * t86 / 0.8D1
      t288 = FJET(XB1, XB2, s, 0.0D0, t258, 0.0D0, -t260, 0.0D0, t287)
      t290 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t291 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t303 = t291 * t32
      t310 = -(0.90D2 * t4 * t6 * (t290 - t207 * t291) - 0.180D3 * t26 *
     # t27 * t291) * t32 / 0.1440D4 - t67 * t303 * t69 / 0.16D2 - t67 * 
     #t303 * t86 / 0.8D1
      t311 = FJET(XB1, XB2, s, 0.0D0, -t201, 0.0D0, t199, 0.0D0, t310)
      t313 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t243, t241, 0.
     #0D0, 0.0D0, 0.0D0)
      t318 = FJET(XB1, XB2, s, 0.0D0, -t243, 0.0D0, t241, 0.0D0, t67 * t
     #313 * t32 * t69 / 0.16D2)
      t325 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t330 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t346 = -t67 * t325 * t32 * t69 / 0.16D2 - (0.90D2 * t4 * t6 * (t33
     #0 - t271 * t325) - 0.180D3 * t26 * t27 * t325) * t69 / 0.1440D4 - 
     #t67 * t325 * t69 * t86 / 0.8D1
      t347 = FJET(XB1, XB2, s, 0.0D0, -t260, 0.0D0, t258, 0.0D0, t346)
      t351 = t2 * t110 * x2 * t120
      t352 = t1 * t110
      t353 = t259 * t352
      t358 = s * t13 * x2 * x1 * t110 * t120
      t359 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, t353, 0.
     #0D0, t109, -t358)
      t364 = FJET(XB1, XB2, s, 0.0D0, -t351, t109, t353, -t358, t67 * t3
     #59 * t69 * t86 / 0.8D1)
      t368 = t69 * t86
      t372 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, t353, 0.
     #0D0, t109, -t358)
      t377 = FJET(XB1, XB2, s, t109, t353, 0.0D0, -t351, -t358, t67 * t3
     #72 * t69 * t86 / 0.8D1)
      t384 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t111, 0
     #.0D0, t109, 0.0D0)
      t389 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t111, 0
     #.0D0, t109, 0.0D0)
      t405 = -t67 * t384 * t69 * t86 / 0.8D1 - (0.90D2 * t4 * t6 * (t389
     # - t126 * t384) - 0.180D3 * t26 * t27 * t384) * t86 / 0.720D3 - t6
     #7 * t384 * t32 * t86 / 0.8D1
      t406 = FJET(XB1, XB2, s, t109, -t111, 0.0D0, 0.0D0, 0.0D0, t405)
      t409 = t2 * x1 * x3
      t411 = x3 * s * t352
      t412 = t200 * s
      t414 = t412 * t1 * x1
      t415 = t412 * t352
      t416 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, t415, t4
     #09, -t414, 0.0D0)
      t421 = FJET(XB1, XB2, s, t409, -t411, -t414, t415, 0.0D0, t67 * t4
     #16 * t32 * t86 / 0.8D1)
      t425 = t32 * t86
      t429 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, t415, t4
     #09, -t414, 0.0D0)
      t434 = FJET(XB1, XB2, s, -t414, t415, t409, -t411, 0.0D0, t67 * t4
     #29 * t32 * t86 / 0.8D1)
      rrgq2qght9s1em1 = t107 * t106 + t144 * t143 + t197 * t196 + t228 *
     # t227 + t249 * pi * t3 * t6 * t244 * t253 / 0.16D2 + t288 * t287 +
     # t311 * t310 + t318 * pi * t3 * t6 * t313 * t253 / 0.16D2 + t347 *
     # t346 + t364 * pi * t3 * t6 * t359 * t368 / 0.8D1 + t377 * pi * t3
     # * t6 * t372 * t368 / 0.8D1 + t406 * t405 + t421 * pi * t3 * t6 * 
     #t416 * t425 / 0.8D1 + t434 * pi * t3 * t6 * t429 * t425 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s1em2
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = z ** 2
      t30 = Sin(x4 * pi)
      t31 = t30 ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t31 * t34)
      t41 = (-0.180D3 * pi * lh - 0.90D2 * t37 * pi) * t3
      t44 = t4 * t8 * t9 / 0.16D2 + t4 * t8 * t13 / 0.16D2 + t4 * t8 * t
     #17 / 0.8D1 + t4 * t6 * t21 / 0.16D2 + t41 * t8 / 0.1440D4
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t44)
      t47 = t2 * x1
      t49 = t2 * (-0.1D1 + x1)
      t50 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49, 0.0
     #D0, t47, 0.0D0)
      t52 = t6 * t50 * t17
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t47, -t49, 0.0D0, -t4 * t52 
     #/ 0.8D1)
      t60 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = t6 * t60
      t71 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t77 = t4 * t61 * t9 / 0.16D2 + t4 * t61 * t13 / 0.16D2 + t4 * t61 
     #* t17 / 0.8D1 + t4 * t6 * t71 / 0.16D2 + t41 * t61 / 0.1440D4
      t78 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t77)
      t80 = t2 * x3
      t82 = t2 * (-0.1D1 + x3)
      t83 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t82, 0.0D0
     #, 0.0D0, 0.0D0)
      t85 = t6 * t83 * t9
      t88 = FJET(XB1, XB2, s, 0.0D0, t80, 0.0D0, -t82, 0.0D0, -t4 * t85 
     #/ 0.16D2)
      t94 = x2 * s * t1
      t97 = (-0.1D1 + x2) * s * t1
      t98 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t94, -t97, 0.0D0
     #, 0.0D0, 0.0D0)
      t100 = t6 * t98 * t13
      t103 = FJET(XB1, XB2, s, 0.0D0, t94, 0.0D0, -t97, 0.0D0, -t4 * t10
     #0 / 0.16D2)
      t108 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t82, 0.0D
     #0, 0.0D0, 0.0D0)
      t110 = t6 * t108 * t9
      t113 = FJET(XB1, XB2, s, 0.0D0, -t82, 0.0D0, t80, 0.0D0, -t4 * t11
     #0 / 0.16D2)
      t118 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t94, -t97, 0.0D
     #0, 0.0D0, 0.0D0)
      t120 = t6 * t118 * t13
      t123 = FJET(XB1, XB2, s, 0.0D0, -t97, 0.0D0, t94, 0.0D0, -t4 * t12
     #0 / 0.16D2)
      t128 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49, 0.
     #0D0, t47, 0.0D0)
      t130 = t6 * t128 * t17
      t133 = FJET(XB1, XB2, s, t47, -t49, 0.0D0, 0.0D0, 0.0D0, -t4 * t13
     #0 / 0.8D1)
      rrgq2qght9s1em2 = t45 * t44 - t55 * pi * t3 * t52 / 0.8D1 + t78 * 
     #t77 - t88 * pi * t3 * t85 / 0.16D2 - t103 * pi * t3 * t100 / 0.16D
     #2 - t113 * pi * t3 * t110 / 0.16D2 - t123 * pi * t3 * t120 / 0.16D
     #2 - t133 * pi * t3 * t130 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s1em3
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t6 * 
     #t7 / 0.16D2)
      t13 = t3 * t6
      t16 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t6 * 
     #t16 / 0.16D2)
      rrgq2qght9s1em3 = t11 * pi * t13 * t7 / 0.16D2 + t20 * pi * t13 * 
     #t16 / 0.16D2

      end function



      doubleprecision function rrgq2qght9s1em4
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght9s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght9s2e1
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = lh ** 2
      t5 = pi ** 2
      t7 = 0.180D3 * t3 - 0.30D2 * t5
      t8 = pi * t7
      t9 = 0.1D1 / t1
      t10 = s ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = x3 * t16
      t18 = x4 * pi
      t19 = Sin(t18)
      t20 = t19 ** 2
      t21 = t1 ** 2
      t22 = t21 ** 2
      t23 = t20 * t22
      t26 = log(0.4D1 * t17 * t23)
      t27 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t32 = pi * t9
      t33 = t26 ** 2
      t36 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t37 = t33 * t26
      t40 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t51 = 0.60D2 * lh * t5 - 0.240D3 * zeta3 - 0.120D3 * t3 * lh
      t52 = pi * t51
      t53 = t12 * t27
      t54 = t52 * t53
      t55 = pi * lh
      t64 = 0.1D1 / x3
      t67 = x2 * x3
      t68 = t67 * t16
      t69 = -0.1D1 + x2
      t70 = t23 * t69
      t73 = log(-0.4D1 * t68 * t70)
      t75 = t73 ** 2
      t78 = t16 * t20
      t79 = t78 * t22
      t82 = log(0.4D1 * t67 * t79)
      t84 = t82 ** 2
      t99 = 0.1D1 / x2
      t102 = t11 * t13
      t108 = x2 * t16
      t111 = log(-0.4D1 * t108 * t70)
      t112 = t111 ** 2
      t115 = log(0.4D1 * t108 * t23)
      t116 = t115 ** 2
      t118 = t112 / 0.2D1 - t116 / 0.2D1
      t120 = t11 * t27
      t124 = t116 * t115 / 0.6D1 - t112 * t111 / 0.6D1
      t128 = t11 * t40
      t134 = t8 * t53
      t136 = -t111 + t115
      t141 = rrgq2qgh91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t146 = log(0.4D1 * t79)
      t147 = t146 ** 2
      t148 = t147 * pi
      t152 = t147 * t146 * pi
      t154 = t146 * pi
      t157 = (-0.90D2 * t148 * lh + t52 - 0.15D2 * t152 - t154 * t7) * t
     #9
      t164 = (0.180D3 * t154 * lh + 0.45D2 * t148 + t8) * t9
      t170 = (-0.180D3 * t55 - 0.90D2 * t154) * t9
      t174 = x1 ** 2
      t175 = x3 * t174
      t178 = log(0.4D1 * t175 * t79)
      t180 = t178 ** 2
      t194 = 0.1D1 / x1
      t197 = t174 * t20
      t198 = t16 * t22
      t201 = log(0.4D1 * t197 * t198)
      t206 = t201 ** 2
      t209 = t206 * t201
      t227 = t32 * t11
      t228 = t67 * t174
      t231 = log(0.4D1 * t228 * t79)
      t237 = log(-0.4D1 * t228 * t78 * t22 * t69)
      t241 = t99 * t194
      t245 = x2 * t174
      t248 = log(0.4D1 * t245 * t79)
      t250 = t248 ** 2
      t257 = log(-0.4D1 * t245 * t20 * t198 * t69)
      t259 = t257 ** 2
      t281 = t5 ** 2
      t282 = t3 ** 2
      t290 = t147 ** 2
      t294 = (0.30D2 * t152 * lh + t148 * t7 / 0.2D1 - t154 * t51 + pi *
     # (t281 + 0.60D2 * t282 + 0.480D3 * lh * zeta3 - 0.60D2 * t3 * t5) 
     #+ 0.15D2 / 0.4D1 * t290 * pi) * t9
      t297 = (t8 * t12 * (t13 - t26 * t27) + 0.90D2 * t32 * t11 * (t33 *
     # t13 / 0.2D1 + t36 - t37 * t27 / 0.6D1 - t26 * t40) + t54 - 0.180D
     #3 * t55 * t12 * (-t26 * t13 + t40 + t33 * t27 / 0.2D1)) * t64 / 0.
     #1440D4 - (0.90D2 * t32 * t11 * (-t73 * t13 + t75 * t27 / 0.2D1 + t
     #82 * t13 - t84 * t27 / 0.2D1) - 0.180D3 * t55 * t12 * (t82 * t27 -
     # t73 * t27)) * t64 * t99 / 0.1440D4 - ((0.90D2 * t32 * t102 - 0.18
     #0D3 * t55 * t53) * t118 + 0.90D2 * t32 * t120 * t124 + (0.90D2 * t
     #32 * t128 - 0.180D3 * t55 * t12 * t13 + t134) * t136) * t99 / 0.14
     #40D4 + t32 * t11 * t141 / 0.16D2 + t157 * t102 / 0.1440D4 + t164 *
     # t128 / 0.1440D4 + t170 * t11 * t36 / 0.1440D4 - (0.90D2 * t32 * t
     #11 * (t178 * t13 - t40 - t180 * t27 / 0.2D1) - 0.180D3 * t55 * t12
     # * (t178 * t27 - t13) - t134) * t64 * t194 / 0.720D3 - (t8 * t12 *
     # (-t13 + t201 * t27) + 0.90D2 * t32 * t11 * (-t206 * t13 / 0.2D1 -
     # t36 + t209 * t27 / 0.6D1 + t201 * t40) - t54 - 0.180D3 * t55 * t1
     #2 * (t201 * t13 - t40 - t206 * t27 / 0.2D1)) * t194 / 0.720D3 + t2
     #27 * (-t231 * t27 + t237 * t27) * t64 * t241 / 0.8D1 + (0.90D2 * t
     #32 * t11 * (-t248 * t13 + t250 * t27 / 0.2D1 + t257 * t13 - t259 *
     # t27 / 0.2D1) - 0.180D3 * t55 * t12 * (-t248 * t27 + t257 * t27)) 
     #* t99 * t194 / 0.720D3 + t294 * t120 / 0.1440D4
      t298 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t297)
      t300 = -0.1D1 + x1
      t301 = t2 * t300
      t302 = t2 * x1
      t304 = 0.1D1 / t14
      t305 = t304 * t22
      t306 = x1 * z
      t307 = -z - x1 + t306
      t308 = 0.1D1 / t307
      t309 = t300 ** 2
      t310 = t308 * t309
      t314 = log(-0.4D1 * t175 * t20 * t305 * t310)
      t315 = t314 ** 2
      t316 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t319 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t320 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t331 = t12 * t316
      t332 = t8 * t331
      t336 = t197 * t304
      t337 = t22 * t308
      t341 = log(-0.4D1 * t336 * t337 * t309)
      t346 = t341 ** 2
      t347 = t346 * t341
      t352 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t369 = t67 * t174 * t22
      t370 = t20 * t304
      t371 = t370 * t310
      t374 = log(-0.4D1 * t369 * t371)
      t388 = log(-0.4D1 * t245 * t22 * t371)
      t389 = t388 ** 2
      t406 = -(0.90D2 * t32 * t11 * (t315 * t316 / 0.2D1 + t319 - t314 *
     # t320) - 0.180D3 * t55 * t12 * (t320 - t314 * t316) + t332) * t64 
     #* t194 / 0.720D3 - (t8 * t12 * (-t341 * t316 + t320) + 0.90D2 * t3
     #2 * t11 * (-t347 * t316 / 0.6D1 + t346 * t320 / 0.2D1 + t352 - t34
     #1 * t319) + t52 * t331 - 0.180D3 * t55 * t12 * (t346 * t316 / 0.2D
     #1 - t341 * t320 + t319)) * t194 / 0.720D3 + (0.90D2 * t32 * t11 * 
     #(t374 * t316 - t320) + 0.180D3 * t55 * t331) * t64 * t241 / 0.720D
     #3 + (0.90D2 * t32 * t11 * (-t389 * t316 / 0.2D1 + t388 * t320 - t3
     #19) - 0.180D3 * t55 * t12 * (t388 * t316 - t320) - t332) * t99 * t
     #194 / 0.720D3
      t407 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t301, t302, 0.0D0, t406)
      t409 = x2 * x1
      t411 = t2 * t409 * t308
      t413 = t1 * x1
      t414 = t69 * s * t413
      t419 = s * t21 * x2 * x1 * t300 * t308
      t420 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t411, -t414, 0
     #.0D0, -t301, t419)
      t421 = t67 * t197
      t426 = log(0.4D1 * t421 * t305 * t310 * t69)
      t427 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, -t414, 0
     #.0D0, -t301, t419)
      t433 = t12 * t427
      t440 = t309 * t69
      t444 = log(0.4D1 * t245 * t370 * t337 * t440)
      t446 = t444 ** 2
      t449 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t411, -t414, 0
     #.0D0, -t301, t419)
      t464 = (0.90D2 * t32 * t11 * (t420 - t426 * t427) - 0.180D3 * t55 
     #* t433) * t64 * t241 / 0.720D3 + (0.90D2 * t32 * t11 * (-t444 * t4
     #20 + t446 * t427 / 0.2D1 + t449) - 0.180D3 * t55 * t12 * (-t444 * 
     #t427 + t420) + t8 * t433) * t99 * t194 / 0.720D3
      t465 = FJET(XB1, XB2, s, 0.0D0, -t411, -t301, -t414, t419, t464)
      t467 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t468 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t470 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t482 = t12 * t470
      t483 = t8 * t482
      t497 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t502 = t52 * t482
      t560 = t11 * t468
      t567 = t11 * t470
      t571 = t11 * t467
      t582 = rrgq2qgh92J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t618 = -(0.90D2 * t32 * t11 * (-t467 + t178 * t468 - t180 * t470 /
     # 0.2D1) - 0.180D3 * t55 * t12 * (-t468 + t178 * t470) - t483) * t6
     #4 * t194 / 0.720D3 - (t8 * t12 * (-t468 + t201 * t470) + 0.90D2 * 
     #t32 * t11 * (-t206 * t468 / 0.2D1 + t209 * t470 / 0.6D1 + t201 * t
     #467 - t497) - t502 - 0.180D3 * t55 * t12 * (-t467 + t201 * t468 - 
     #t206 * t470 / 0.2D1)) * t194 / 0.720D3 + t227 * (-t231 * t470 + t2
     #37 * t470) * t64 * t241 / 0.8D1 + (0.90D2 * t32 * t11 * (t250 * t4
     #70 / 0.2D1 - t248 * t468 - t259 * t470 / 0.2D1 + t257 * t468) - 0.
     #180D3 * t55 * t12 * (-t248 * t470 + t257 * t470)) * t99 * t194 / 0
     #.720D3 - (0.90D2 * t32 * t11 * (-t73 * t468 + t82 * t468 - t84 * t
     #470 / 0.2D1 + t75 * t470 / 0.2D1) - 0.180D3 * t55 * t12 * (-t73 * 
     #t470 + t82 * t470)) * t64 * t99 / 0.1440D4 - ((0.90D2 * t32 * t560
     # - 0.180D3 * t55 * t482) * t118 + 0.90D2 * t32 * t567 * t124 + (0.
     #90D2 * t32 * t571 - 0.180D3 * t55 * t12 * t468 + t483) * t136) * t
     #99 / 0.1440D4 + t32 * t11 * t582 / 0.16D2 + t294 * t567 / 0.1440D4
     # + (t8 * t12 * (t468 - t26 * t470) + 0.90D2 * t32 * t11 * (t33 * t
     #468 / 0.2D1 - t26 * t467 + t497 - t37 * t470 / 0.6D1) + t502 - 0.1
     #80D3 * t55 * t12 * (t33 * t470 / 0.2D1 + t467 - t26 * t468)) * t64
     # / 0.1440D4 + t157 * t560 / 0.1440D4 + t164 * t571 / 0.1440D4 + t1
     #70 * t11 * t497 / 0.1440D4
      t619 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t618)
      t621 = t2 * x3
      t622 = -0.1D1 + x3
      t623 = t2 * t622
      t628 = log(-0.4D1 * t175 * t22 * t78 * t622)
      t629 = t628 ** 2
      t630 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t633 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t634 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t645 = t12 * t630
      t651 = t69 * t622
      t655 = log(0.4D1 * t421 * t198 * t651)
      t661 = log(-0.4D1 * t228 * t23 * t16 * t622)
      t671 = log(0.4D1 * t68 * t23 * t651)
      t672 = t671 ** 2
      t676 = t23 * t622
      t679 = log(-0.4D1 * t68 * t676)
      t680 = t679 ** 2
      t700 = log(-0.4D1 * t17 * t676)
      t705 = t700 ** 2
      t709 = t705 * t700
      t712 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t728 = -(0.90D2 * t32 * t11 * (t629 * t630 / 0.2D1 + t633 - t628 *
     # t634) - 0.180D3 * t55 * t12 * (t634 - t628 * t630) + t8 * t645) *
     # t64 * t194 / 0.720D3 + t227 * (-t655 * t630 + t661 * t630) * t64 
     #* t241 / 0.8D1 - (0.90D2 * t32 * t11 * (-t672 * t630 / 0.2D1 + t67
     #1 * t634 + t680 * t630 / 0.2D1 - t679 * t634) - 0.180D3 * t55 * t1
     #2 * (-t679 * t630 + t671 * t630)) * t64 * t99 / 0.1440D4 + (t8 * t
     #12 * (-t634 + t700 * t630) + 0.90D2 * t32 * t11 * (-t705 * t634 / 
     #0.2D1 + t700 * t633 + t709 * t630 / 0.6D1 - t712) - t52 * t645 - 0
     #.180D3 * t55 * t12 * (t700 * t634 - t633 - t705 * t630 / 0.2D1)) *
     # t64 / 0.1440D4
      t729 = FJET(XB1, XB2, s, t621, 0.0D0, -t623, 0.0D0, 0.0D0, t728)
      t731 = t622 * s
      t732 = t1 * t300
      t733 = t731 * t732
      t734 = t731 * t413
      t736 = x3 * s * t732
      t737 = x3 * x1
      t738 = t2 * t737
      t745 = log(0.4D1 * t175 * t23 * t304 * t308 * t309 * t622)
      t746 = t745 ** 2
      t747 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t738, -t734, -t
     #736, t733, 0.0D0)
      t750 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t738, -t734, -t
     #736, t733, 0.0D0)
      t752 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t738, -t734, -t
     #736, t733, 0.0D0)
      t762 = t12 * t747
      t771 = log(0.4D1 * t369 * t370 * t310 * t622)
      t783 = -(0.90D2 * t32 * t11 * (-t746 * t747 / 0.2D1 + t745 * t750 
     #- t752) - 0.180D3 * t55 * t12 * (t745 * t747 - t750) - t8 * t762) 
     #* t64 * t194 / 0.720D3 + (0.90D2 * t32 * t11 * (t750 - t771 * t747
     #) - 0.180D3 * t55 * t762) * t64 * t241 / 0.720D3
      t784 = FJET(XB1, XB2, s, t733, -t734, -t736, t738, 0.0D0, t783)
      t788 = x3 * z
      t789 = t737 * z
      t790 = t67 * z
      t791 = t67 * x1
      t792 = t67 * t306
      t793 = cos(t18)
      t798 = Sqrt(-x3 * t69 * t307 * x2 * t622)
      t800 = 0.2D1 * t793 * t798
      t801 = z + x1 - t306 - x2 * z - t409 + t409 * z - t788 - t737 + t7
     #89 + t790 + t791 - t792 + t67 + t800
      t804 = t2 * x1 * t801 * t308
      t808 = t2 * x1 * (-t788 - t737 + t789 + t790 + t791 - t792 - x2 + 
     #t67 + t800) * t308
      t809 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t808, -t804, -t
     #736, t733, t419)
      t815 = log(-0.4D1 * t67 * t336 * t337 * t440 * t622)
      t816 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t808, -t804, -t
     #736, t733, t419)
      t825 = 0.90D2 * t32 * t11 * (-t809 + t815 * t816) + 0.180D3 * t55 
     #* t12 * t816
      t829 = FJET(XB1, XB2, s, t733, -t804, -t736, t808, t419, t825 * t6
     #4 * t241 / 0.720D3)
      t832 = t64 * t99 * t194
      t835 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t836 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t838 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t850 = t12 * t838
      t851 = t8 * t850
      t859 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t302, 0.
     #0D0, -t301, 0.0D0)
      t905 = -(0.90D2 * t32 * t11 * (t835 - t314 * t836 + t315 * t838 / 
     #0.2D1) - 0.180D3 * t55 * t12 * (-t314 * t838 + t836) + t851) * t64
     # * t194 / 0.720D3 - (t8 * t12 * (t836 - t341 * t838) + 0.90D2 * t3
     #2 * t11 * (t859 + t346 * t836 / 0.2D1 - t347 * t838 / 0.6D1 - t341
     # * t835) + t52 * t850 - 0.180D3 * t55 * t12 * (t346 * t838 / 0.2D1
     # + t835 - t341 * t836)) * t194 / 0.720D3 + (0.90D2 * t32 * t11 * (
     #t374 * t838 - t836) + 0.180D3 * t55 * t850) * t64 * t241 / 0.720D3
     # + (0.90D2 * t32 * t11 * (t388 * t836 - t389 * t838 / 0.2D1 - t835
     #) - 0.180D3 * t55 * t12 * (-t836 + t388 * t838) - t851) * t99 * t1
     #94 / 0.720D3
      t906 = FJET(XB1, XB2, s, -t301, t302, 0.0D0, 0.0D0, 0.0D0, t905)
      t908 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t411, -t414, 0
     #.0D0, -t301, t419)
      t909 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, -t414, 0
     #.0D0, -t301, t419)
      t915 = t12 * t909
      t924 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t411, -t414, 0
     #.0D0, -t301, t419)
      t939 = (0.90D2 * t32 * t11 * (t908 - t426 * t909) - 0.180D3 * t55 
     #* t915) * t64 * t241 / 0.720D3 + (0.90D2 * t32 * t11 * (-t444 * t9
     #08 + t446 * t909 / 0.2D1 + t924) - 0.180D3 * t55 * t12 * (-t444 * 
     #t909 + t908) + t8 * t915) * t99 * t194 / 0.720D3
      t940 = FJET(XB1, XB2, s, -t301, -t414, 0.0D0, -t411, t419, t939)
      t942 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t945 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t946 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t957 = t12 * t942
      t997 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #621, -t623, 0.0D0)
      t1015 = -(0.90D2 * t32 * t11 * (t629 * t942 / 0.2D1 + t945 - t628 
     #* t946) - 0.180D3 * t55 * t12 * (-t628 * t942 + t946) + t8 * t957)
     # * t64 * t194 / 0.720D3 + t227 * (t661 * t942 - t655 * t942) * t64
     # * t241 / 0.8D1 - (0.90D2 * t32 * t11 * (-t672 * t942 / 0.2D1 + t6
     #71 * t946 + t680 * t942 / 0.2D1 - t679 * t946) - 0.180D3 * t55 * t
     #12 * (-t679 * t942 + t671 * t942)) * t64 * t99 / 0.1440D4 + (t8 * 
     #t12 * (-t946 + t700 * t942) + 0.90D2 * t32 * t11 * (t700 * t945 + 
     #t709 * t942 / 0.6D1 - t997 - t705 * t946 / 0.2D1) - t52 * t957 - 0
     #.180D3 * t55 * t12 * (-t705 * t942 / 0.2D1 - t945 + t700 * t946)) 
     #* t64 / 0.1440D4
      t1016 = FJET(XB1, XB2, s, -t623, 0.0D0, t621, 0.0D0, 0.0D0, t1015)
      t1018 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t738, -t734, -
     #t736, t733, 0.0D0)
      t1020 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t738, -t734, -
     #t736, t733, 0.0D0)
      t1023 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t738, -t734, -
     #t736, t733, 0.0D0)
      t1033 = t12 * t1020
      t1049 = -(0.90D2 * t32 * t11 * (t745 * t1018 - t746 * t1020 / 0.2D
     #1 - t1023) - 0.180D3 * t55 * t12 * (-t1018 + t745 * t1020) - t8 * 
     #t1033) * t64 * t194 / 0.720D3 + (0.90D2 * t32 * t11 * (t1018 - t77
     #1 * t1020) - 0.180D3 * t55 * t1033) * t64 * t241 / 0.720D3
      t1050 = FJET(XB1, XB2, s, -t736, t738, t733, -t734, 0.0D0, t1049)
      t1052 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t808, -t804, -
     #t736, t733, t419)
      t1053 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t808, -t804, -
     #t736, t733, t419)
      t1062 = 0.90D2 * t32 * t11 * (-t1052 + t815 * t1053) + 0.180D3 * t
     #55 * t12 * t1053
      t1066 = FJET(XB1, XB2, s, -t736, t808, t733, -t804, t419, t1062 * 
     #t64 * t241 / 0.720D3)
      rrgq2qght9s2e1 = t298 * t297 + t407 * t406 + t465 * t464 + t619 * 
     #t618 + t729 * t728 + t784 * t783 + t829 * t825 * t832 / 0.720D3 + 
     #t906 * t905 + t940 * t939 + t1016 * t1015 + t1050 * t1049 + t1066 
     #* t1062 * t832 / 0.720D3

      end function



      doubleprecision function rrgq2qght9s2e0
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t8 * t18)
      t22 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t29 = pi * lh
      t30 = t3 * t6
      t31 = t30 * t22
      t33 = 0.180D3 * t29 * t31
      t35 = 0.1D1 / x3
      t37 = 0.1D1 / x1
      t40 = t4 * t6
      t41 = x2 * t7
      t44 = log(0.4D1 * t41 * t18)
      t47 = t11 * t17
      t48 = -0.1D1 + x2
      t52 = log(-0.4D1 * t41 * t14 * t47 * t48)
      t55 = 0.1D1 / x2
      t60 = t7 * t14
      t63 = log(0.4D1 * t60 * t47)
      t65 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t66 = t63 ** 2
      t78 = lh ** 2
      t80 = pi ** 2
      t82 = 0.180D3 * t78 - 0.30D2 * t80
      t83 = pi * t82
      t84 = t83 * t31
      t88 = x3 * t11
      t89 = t14 * t17
      t92 = log(0.4D1 * t88 * t89)
      t94 = t92 ** 2
      t110 = log(0.4D1 * t18)
      t111 = t110 * pi
      t114 = t110 ** 2
      t115 = t114 * pi
      t118 = (0.180D3 * t111 * lh + 0.45D2 * t115 + t83) * t3
      t119 = t6 * t24
      t122 = rrgq2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t140 = (-0.90D2 * t115 * lh + pi * (0.60D2 * lh * t80 - 0.240D3 * 
     #zeta3 - 0.120D3 * t78 * lh) - 0.15D2 * t114 * t110 * pi - t111 * t
     #82) * t3
      t141 = t6 * t22
      t147 = (-0.180D3 * t29 - 0.90D2 * t111) * t3
      t151 = x2 * x3
      t154 = log(0.4D1 * t151 * t18)
      t156 = t151 * t11
      t157 = t89 * t48
      t160 = log(-0.4D1 * t156 * t157)
      t167 = x2 * t11
      t170 = log(-0.4D1 * t167 * t157)
      t171 = t170 ** 2
      t174 = log(0.4D1 * t167 * t89)
      t175 = t174 ** 2
      t177 = t171 / 0.2D1 - t175 / 0.2D1
      t184 = -t170 + t174
      t189 = -(0.90D2 * t4 * t6 * (t21 * t22 - t24) + t33) * t35 * t37 /
     # 0.720D3 + t40 * (-t44 * t22 + t52 * t22) * t55 * t37 / 0.8D1 - (0
     #.90D2 * t4 * t6 * (t63 * t24 - t65 - t66 * t22 / 0.2D1) - 0.180D3 
     #* t29 * t30 * (-t24 + t63 * t22) - t84) * t37 / 0.720D3 + (0.90D2 
     #* t4 * t6 * (-t92 * t24 + t65 + t94 * t22 / 0.2D1) - 0.180D3 * t29
     # * t30 * (t24 - t92 * t22) + t84) * t35 / 0.1440D4 + t118 * t119 /
     # 0.1440D4 + t4 * t6 * t122 / 0.16D2 + t140 * t141 / 0.1440D4 + t14
     #7 * t6 * t65 / 0.1440D4 - t40 * (t154 * t22 - t160 * t22) * t35 * 
     #t55 / 0.16D2 - (0.90D2 * t4 * t141 * t177 + (0.90D2 * t4 * t119 - 
     #t33) * t184) * t55 / 0.1440D4
      t190 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t189)
      t192 = -0.1D1 + x1
      t193 = t2 * t192
      t194 = t2 * x1
      t195 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t194, 0.
     #0D0, -t193, 0.0D0)
      t197 = 0.1D1 / t9
      t199 = x1 * z
      t200 = -z - x1 + t199
      t201 = 0.1D1 / t200
      t202 = t192 ** 2
      t203 = t201 * t202
      t207 = log(-0.4D1 * t8 * t14 * t197 * t17 * t203)
      t208 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t194, 0.
     #0D0, -t193, 0.0D0)
      t214 = t30 * t208
      t216 = 0.180D3 * t29 * t214
      t222 = t55 * t37
      t227 = t14 * t197
      t231 = log(-0.4D1 * t41 * t17 * t227 * t203)
      t242 = t17 * t201
      t246 = log(-0.4D1 * t60 * t197 * t242 * t202)
      t247 = t246 ** 2
      t251 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t194, 0.
     #0D0, -t193, 0.0D0)
      t265 = -(0.90D2 * t4 * t6 * (t195 - t207 * t208) - t216) * t35 * t
     #37 / 0.720D3 - t40 * t208 * t35 * t222 / 0.8D1 + (0.90D2 * t4 * t6
     # * (t231 * t208 - t195) + t216) * t55 * t37 / 0.720D3 - (0.90D2 * 
     #t4 * t6 * (t247 * t208 / 0.2D1 - t246 * t195 + t251) - 0.180D3 * t
     #29 * t30 * (-t246 * t208 + t195) + t83 * t214) * t37 / 0.720D3
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t193, t194, 0.0D0, t265)
      t268 = x2 * x1
      t270 = t2 * t268 * t201
      t272 = t1 * x1
      t273 = t48 * s * t272
      t278 = s * t16 * x2 * x1 * t192 * t201
      t279 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t270, -t273, 0
     #.0D0, -t193, t278)
      t289 = log(0.4D1 * t41 * t227 * t242 * t202 * t48)
      t291 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t270, -t273, 0
     #.0D0, -t193, t278)
      t303 = t40 * t279 * t35 * t222 / 0.8D1 + (0.90D2 * t4 * t6 * (-t28
     #9 * t279 + t291) - 0.180D3 * t29 * t30 * t279) * t55 * t37 / 0.720
     #D3
      t304 = FJET(XB1, XB2, s, 0.0D0, -t270, -t193, -t273, t278, t303)
      t306 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t307 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t313 = t30 * t307
      t315 = 0.180D3 * t29 * t313
      t327 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t340 = t83 * t313
      t344 = rrgq2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t348 = t6 * t307
      t366 = t6 * t306
      t389 = -(0.90D2 * t4 * t6 * (-t306 + t21 * t307) + t315) * t35 * t
     #37 / 0.720D3 + t40 * (-t44 * t307 + t52 * t307) * t55 * t37 / 0.8D
     #1 - (0.90D2 * t4 * t6 * (-t327 + t63 * t306 - t66 * t307 / 0.2D1) 
     #- 0.180D3 * t29 * t30 * (-t306 + t63 * t307) - t340) * t37 / 0.720
     #D3 + t4 * t6 * t344 / 0.16D2 + t140 * t348 / 0.1440D4 + (0.90D2 * 
     #t4 * t6 * (t94 * t307 / 0.2D1 + t327 - t92 * t306) - 0.180D3 * t29
     # * t30 * (t306 - t92 * t307) + t340) * t35 / 0.1440D4 + t118 * t36
     #6 / 0.1440D4 + t147 * t6 * t327 / 0.1440D4 - t40 * (-t160 * t307 +
     # t154 * t307) * t35 * t55 / 0.16D2 - (0.90D2 * t4 * t348 * t177 + 
     #(0.90D2 * t4 * t366 - t315) * t184) * t55 / 0.1440D4
      t390 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t389)
      t392 = t2 * x3
      t393 = -0.1D1 + x3
      t394 = t2 * t393
      t395 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #392, -t394, 0.0D0)
      t400 = log(-0.4D1 * t8 * t17 * t15 * t393)
      t401 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #392, -t394, 0.0D0)
      t407 = t30 * t401
      t414 = t89 * t393
      t417 = log(-0.4D1 * t88 * t414)
      t419 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #392, -t394, 0.0D0)
      t420 = t417 ** 2
      t438 = log(-0.4D1 * t156 * t414)
      t444 = log(0.4D1 * t156 * t89 * t393 * t48)
      t451 = -(0.90D2 * t4 * t6 * (t395 - t400 * t401) - 0.180D3 * t29 *
     # t407) * t35 * t37 / 0.720D3 + (0.90D2 * t4 * t6 * (t417 * t395 - 
     #t419 - t420 * t401 / 0.2D1) - 0.180D3 * t29 * t30 * (-t395 + t417 
     #* t401) - t83 * t407) * t35 / 0.1440D4 - t40 * (-t438 * t401 + t44
     #4 * t401) * t35 * t55 / 0.16D2
      t452 = FJET(XB1, XB2, s, t392, 0.0D0, -t394, 0.0D0, 0.0D0, t451)
      t454 = t393 * s
      t455 = t1 * t192
      t456 = t454 * t455
      t457 = t454 * t272
      t459 = x3 * s * t455
      t460 = x3 * x1
      t461 = t2 * t460
      t468 = log(0.4D1 * t8 * t89 * t197 * t201 * t202 * t393)
      t469 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t461, -t457, -t
     #459, t456, 0.0D0)
      t471 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t461, -t457, -t
     #459, t456, 0.0D0)
      t487 = -(0.90D2 * t4 * t6 * (t468 * t469 - t471) + 0.180D3 * t29 *
     # t30 * t469) * t35 * t37 / 0.720D3 + t40 * t469 * t35 * t222 / 0.8
     #D1
      t488 = FJET(XB1, XB2, s, t456, -t457, -t459, t461, 0.0D0, t487)
      t492 = x3 * z
      t493 = t460 * z
      t494 = t151 * z
      t495 = t151 * x1
      t496 = t151 * t199
      t497 = cos(t12)
      t502 = Sqrt(-x3 * t48 * t200 * x2 * t393)
      t504 = 0.2D1 * t497 * t502
      t505 = z + x1 - t199 - x2 * z - t268 + t268 * z - t492 - t460 + t4
     #93 + t494 + t495 - t496 + t151 + t504
      t508 = t2 * x1 * t505 * t201
      t512 = t2 * x1 * (-t492 - t460 + t493 + t494 + t495 - t496 - x2 + 
     #t151 + t504) * t201
      t513 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t512, -t508, -t
     #459, t456, t278)
      t515 = t513 * t35 * t222
      t518 = FJET(XB1, XB2, s, t456, -t508, -t459, t512, t278, -t40 * t5
     #15 / 0.8D1)
      t523 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t194, 0.
     #0D0, -t193, 0.0D0)
      t525 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t194, 0.
     #0D0, -t193, 0.0D0)
      t530 = t30 * t523
      t532 = 0.180D3 * t29 * t530
      t552 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t194, 0.
     #0D0, -t193, 0.0D0)
      t567 = -(0.90D2 * t4 * t6 * (-t207 * t523 + t525) - t532) * t35 * 
     #t37 / 0.720D3 - t40 * t523 * t35 * t222 / 0.8D1 + (0.90D2 * t4 * t
     #6 * (-t525 + t231 * t523) + t532) * t55 * t37 / 0.720D3 - (0.90D2 
     #* t4 * t6 * (t247 * t523 / 0.2D1 + t552 - t246 * t525) - 0.180D3 *
     # t29 * t30 * (t525 - t246 * t523) + t83 * t530) * t37 / 0.720D3
      t568 = FJET(XB1, XB2, s, -t193, t194, 0.0D0, 0.0D0, 0.0D0, t567)
      t570 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t270, -t273, 0
     #.0D0, -t193, t278)
      t576 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t270, -t273, 0
     #.0D0, -t193, t278)
      t588 = t40 * t570 * t35 * t222 / 0.8D1 + (0.90D2 * t4 * t6 * (-t28
     #9 * t570 + t576) - 0.180D3 * t29 * t30 * t570) * t55 * t37 / 0.720
     #D3
      t589 = FJET(XB1, XB2, s, -t193, -t273, 0.0D0, -t270, t278, t588)
      t591 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #392, -t394, 0.0D0)
      t593 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #392, -t394, 0.0D0)
      t598 = t30 * t591
      t607 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #392, -t394, 0.0D0)
      t629 = -(0.90D2 * t4 * t6 * (-t400 * t591 + t593) - 0.180D3 * t29 
     #* t598) * t35 * t37 / 0.720D3 + (0.90D2 * t4 * t6 * (-t420 * t591 
     #/ 0.2D1 - t607 + t417 * t593) - 0.180D3 * t29 * t30 * (-t593 + t41
     #7 * t591) - t83 * t598) * t35 / 0.1440D4 - t40 * (-t438 * t591 + t
     #444 * t591) * t35 * t55 / 0.16D2
      t630 = FJET(XB1, XB2, s, -t394, 0.0D0, t392, 0.0D0, 0.0D0, t629)
      t632 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t461, -t457, -t
     #459, t456, 0.0D0)
      t633 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t461, -t457, -t
     #459, t456, 0.0D0)
      t650 = -(0.90D2 * t4 * t6 * (-t632 + t468 * t633) + 0.180D3 * t29 
     #* t30 * t633) * t35 * t37 / 0.720D3 + t40 * t633 * t35 * t222 / 0.
     #8D1
      t651 = FJET(XB1, XB2, s, -t459, t461, t456, -t457, 0.0D0, t650)
      t653 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t512, -t508, -t
     #459, t456, t278)
      t655 = t653 * t35 * t222
      t658 = FJET(XB1, XB2, s, -t459, t512, t456, -t508, t278, -t40 * t6
     #55 / 0.8D1)
      rrgq2qght9s2e0 = t190 * t189 + t266 * t265 + t304 * t303 + t390 * 
     #t389 + t452 * t451 + t488 * t487 - t518 * pi * t30 * t515 / 0.8D1 
     #+ t568 * t567 + t589 * t588 + t630 * t629 + t651 * t650 - t658 * p
     #i * t30 * t655 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s2em1
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t13 = Sin(x4 * pi)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t6
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x3
      t36 = x1 ** 2
      t37 = t36 * t14
      t41 = log(0.4D1 * t37 * t10 * t16)
      t48 = 0.1D1 / x1
      t51 = t4 * t6
      t56 = x2 * t10
      t57 = -0.1D1 + x2
      t61 = log(-0.4D1 * t56 * t17 * t57)
      t64 = log(0.4D1 * t56 * t17)
      t65 = -t61 + t64
      t67 = 0.1D1 / x2
      t71 = rrgq2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t79 = log(0.4D1 * t10 * t14 * t16)
      t80 = t79 * pi
      t83 = (-0.180D3 * t27 - 0.90D2 * t80) * t3
      t89 = t79 ** 2
      t92 = lh ** 2
      t94 = pi ** 2
      t99 = (0.180D3 * lh * t80 + 0.45D2 * t89 * pi + pi * (0.180D3 * t9
     #2 - 0.30D2 * t94)) * t3
      t103 = (0.90D2 * t4 * t6 * (t7 - t20 * t21) - t31) * t33 / 0.1440D
     #4 - (0.90D2 * t4 * t6 * (-t7 + t41 * t21) + t31) * t48 / 0.720D3 +
     # t51 * t21 * t33 * t48 / 0.8D1 - t51 * t21 * t65 * t67 / 0.16D2 + 
     #t4 * t6 * t71 / 0.16D2 + t83 * t6 * t7 / 0.1440D4 + t99 * t6 * t21
     # / 0.1440D4
      t104 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t103)
      t106 = -0.1D1 + x1
      t107 = t2 * t106
      t108 = t2 * x1
      t109 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t108, 0.
     #0D0, -t107, 0.0D0)
      t118 = 0.1D1 / (-z - x1 + x1 * z)
      t120 = t106 ** 2
      t124 = log(-0.4D1 * t37 / t8 * t16 * t118 * t120)
      t126 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t108, 0.
     #0D0, -t107, 0.0D0)
      t141 = -t51 * t109 * t67 * t48 / 0.8D1 - (0.90D2 * t4 * t6 * (-t12
     #4 * t109 + t126) - 0.180D3 * t27 * t28 * t109) * t48 / 0.720D3 - t
     #51 * t109 * t33 * t48 / 0.8D1
      t142 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t107, t108, 0.0D0, t141)
      t146 = t2 * x1 * x2 * t118
      t148 = t1 * x1
      t149 = t57 * s * t148
      t154 = s * t15 * x2 * x1 * t106 * t118
      t155 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t146, -t149, 0
     #.0D0, -t107, t154)
      t160 = FJET(XB1, XB2, s, 0.0D0, -t146, -t107, -t149, t154, t51 * t
     #155 * t67 * t48 / 0.8D1)
      t164 = t67 * t48
      t168 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t169 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t177 = 0.180D3 * t27 * t28 * t169
      t203 = rrgq2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t207 = (0.90D2 * t4 * t6 * (t168 - t20 * t169) - t177) * t33 / 0.1
     #440D4 - (0.90D2 * t4 * t6 * (-t168 + t41 * t169) + t177) * t48 / 0
     #.720D3 + t51 * t169 * t33 * t48 / 0.8D1 + t83 * t6 * t168 / 0.1440
     #D4 + t99 * t6 * t169 / 0.1440D4 - t51 * t169 * t65 * t67 / 0.16D2 
     #+ t4 * t6 * t203 / 0.16D2
      t208 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = t2 * x3
      t211 = -0.1D1 + x3
      t212 = t2 * t211
      t213 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #210, -t212, 0.0D0)
      t217 = log(-0.4D1 * t11 * t17 * t211)
      t218 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #210, -t212, 0.0D0)
      t234 = (0.90D2 * t4 * t6 * (-t213 + t217 * t218) + 0.180D3 * t27 *
     # t28 * t218) * t33 / 0.1440D4 - t51 * t218 * t33 * t48 / 0.8D1
      t235 = FJET(XB1, XB2, s, t210, 0.0D0, -t212, 0.0D0, 0.0D0, t234)
      t237 = t211 * s
      t238 = t1 * t106
      t239 = t237 * t238
      t240 = t237 * t148
      t242 = x3 * s * t238
      t244 = t2 * x1 * x3
      t245 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t244, -t240, -t
     #242, t239, 0.0D0)
      t250 = FJET(XB1, XB2, s, t239, -t240, -t242, t244, 0.0D0, t51 * t2
     #45 * t33 * t48 / 0.8D1)
      t254 = t33 * t48
      t258 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t108, 0.
     #0D0, -t107, 0.0D0)
      t263 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t108, 0.
     #0D0, -t107, 0.0D0)
      t279 = -t51 * t258 * t67 * t48 / 0.8D1 - (0.90D2 * t4 * t6 * (t263
     # - t124 * t258) - 0.180D3 * t27 * t28 * t258) * t48 / 0.720D3 - t5
     #1 * t258 * t33 * t48 / 0.8D1
      t280 = FJET(XB1, XB2, s, -t107, t108, 0.0D0, 0.0D0, 0.0D0, t279)
      t282 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t146, -t149, 0
     #.0D0, -t107, t154)
      t287 = FJET(XB1, XB2, s, -t107, -t149, 0.0D0, -t146, t154, t51 * t
     #282 * t67 * t48 / 0.8D1)
      t294 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #210, -t212, 0.0D0)
      t295 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #210, -t212, 0.0D0)
      t311 = (0.90D2 * t4 * t6 * (-t294 + t217 * t295) + 0.180D3 * t27 *
     # t28 * t295) * t33 / 0.1440D4 - t51 * t295 * t33 * t48 / 0.8D1
      t312 = FJET(XB1, XB2, s, -t212, 0.0D0, t210, 0.0D0, 0.0D0, t311)
      t314 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t244, -t240, -t
     #242, t239, 0.0D0)
      t319 = FJET(XB1, XB2, s, -t242, t244, t239, -t240, 0.0D0, t51 * t3
     #14 * t33 * t48 / 0.8D1)
      rrgq2qght9s2em1 = t104 * t103 + t142 * t141 + t160 * pi * t3 * t6 
     #* t155 * t164 / 0.8D1 + t207 * t208 + t235 * t234 + t250 * pi * t3
     # * t6 * t245 * t254 / 0.8D1 + t280 * t279 + t287 * pi * t3 * t6 * 
     #t282 * t164 / 0.8D1 + t312 * t311 + t319 * pi * t3 * t6 * t314 * t
     #254 / 0.8D1

      end function



      doubleprecision function rrgq2qght9s2em2
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x3
      t17 = rrgq2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t23 = z ** 2
      t27 = Sin(x4 * pi)
      t28 = t27 ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 / t23 / z * t28 * t31)
      t38 = (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3
      t41 = t4 * t8 * t9 / 0.8D1 + t4 * t8 * t13 / 0.16D2 + t4 * t6 * t1
     #7 / 0.16D2 + t38 * t8 / 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t45 = t2 * (-0.1D1 + x1)
      t46 = t2 * x1
      t47 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t46, 0.0D
     #0, -t45, 0.0D0)
      t49 = t6 * t47 * t9
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t46, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t57 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t58 = t6 * t57
      t65 = rrgq2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t71 = t4 * t58 * t9 / 0.8D1 + t4 * t58 * t13 / 0.16D2 + t4 * t6 * 
     #t65 / 0.16D2 + t38 * t58 / 0.1440D4
      t72 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t71)
      t74 = t2 * x3
      t76 = t2 * (-0.1D1 + x3)
      t77 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t7
     #4, -t76, 0.0D0)
      t79 = t6 * t77 * t13
      t82 = FJET(XB1, XB2, s, t74, 0.0D0, -t76, 0.0D0, 0.0D0, -t4 * t79 
     #/ 0.16D2)
      t87 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t46, 0.0D
     #0, -t45, 0.0D0)
      t89 = t6 * t87 * t9
      t92 = FJET(XB1, XB2, s, -t45, t46, 0.0D0, 0.0D0, 0.0D0, -t4 * t89 
     #/ 0.8D1)
      t97 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t7
     #4, -t76, 0.0D0)
      t99 = t6 * t97 * t13
      t102 = FJET(XB1, XB2, s, -t76, 0.0D0, t74, 0.0D0, 0.0D0, -t4 * t99
     # / 0.16D2)
      rrgq2qght9s2em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 + t72 * 
     #t71 - t82 * pi * t3 * t79 / 0.16D2 - t92 * pi * t3 * t89 / 0.8D1 -
     # t102 * pi * t3 * t99 / 0.16D2

      end function



      doubleprecision function rrgq2qght9s2em3
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

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
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrgq2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t6 * 
     #t7 / 0.16D2)
      t13 = t3 * t6
      t16 = rrgq2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t6 * 
     #t16 / 0.16D2)
      rrgq2qght9s2em3 = t11 * pi * t13 * t7 / 0.16D2 + t20 * pi * t13 * 
     #t16 / 0.16D2

      end function



      doubleprecision function rrgq2qght9s2em4
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
      doubleprecision rrgq2qgh91J1
      doubleprecision rrgq2qgh91J2
      doubleprecision rrgq2qgh91J3
      doubleprecision rrgq2qgh91J4
      doubleprecision rrgq2qgh91J5
      doubleprecision rrgq2qgh91J6
      doubleprecision rrgq2qgh91J7
      doubleprecision rrgq2qgh92J1
      doubleprecision rrgq2qgh92J2
      doubleprecision rrgq2qgh92J3
      doubleprecision rrgq2qgh92J4
      doubleprecision rrgq2qgh92J5
      doubleprecision rrgq2qgh92J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght9s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh91J1
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
      t2 = S24 ** 2
      t4 = S13 ** 2
      rrgq2qgh91J1 = -0.4D1 / 0.9D1 * S24 * wd * (S13 * t2 + t4 * S13) /
     # (S12 + S13 + S23) / S12 / z / pi

      end function
  
   
 

      doubleprecision function rrgq2qgh91J2
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
      t1 = S24 ** 2
      t2 = S13 * t1
      t3 = S13 ** 2
      t4 = t3 * S13
      rrgq2qgh91J2 = (-0.4D1 / 0.9D1 * S24 * (t2 + t4) - 0.4D1 / 0.9D1 *
     # S24 * (-t4 + 0.2D1 * t3 * S24 - t2)) / (S12 + S13 + S23) / S12 / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh91J3
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
      t2 = S14 ** 2
      t6 = S24 ** 2
      t7 = S13 * t6
      t8 = S13 ** 2
      t9 = t8 * S13
      rrgq2qgh91J3 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) + (-0.4D1 
     #/ 0.9D1 * S24 * (t7 + t9) - 0.8D1 / 0.9D1 * S24 * (-t9 + 0.2D1 * t
     #8 * S24 - t7)) / (S12 + S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh91J4
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
      t2 = S14 ** 2
      t6 = S24 ** 2
      t7 = S13 * t6
      t8 = S13 ** 2
      t9 = t8 * S13
      rrgq2qgh91J4 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) + (-0.4D1 
     #/ 0.9D1 * S24 * (t7 + t9) - 0.8D1 / 0.9D1 * S24 * (-t9 + 0.2D1 * t
     #8 * S24 - t7)) / (S12 + S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh91J5
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
      t2 = S14 ** 2
      t6 = S24 ** 2
      t7 = S13 * t6
      t8 = S13 ** 2
      t9 = t8 * S13
      rrgq2qgh91J5 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) + (-0.4D1 
     #/ 0.9D1 * S24 * (t7 + t9) - 0.8D1 / 0.9D1 * S24 * (-t9 + 0.2D1 * t
     #8 * S24 - t7)) / (S12 + S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh91J6
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
      t2 = S14 ** 2
      t6 = S13 ** 2
      t10 = S24 ** 2
      rrgq2qgh91J6 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) - 0.8D1 / 
     #0.9D1 * S24 * (-S13 * t6 + 0.2D1 * t6 * S24 - S13 * t10) / (S12 + 
     #S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh91J7
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
      t2 = S14 ** 2
      t5 = S13 ** 2
      t9 = S24 ** 2
      rrgq2qgh91J7 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) - 0.4D1 / 
     #0.9D1 * S24 * (-t5 * S13 + 0.2D1 * t5 * S24 - S13 * t9) / (S12 + S
     #13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh92J1
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
      t6 = S13 ** 2
      rrgq2qgh92J1 = (-0.4D1 * S24 * S12 - 0.8D1 * S13 * S24 - 0.4D1 / 0
     #.9D1 * S24 * (0.9D1 * t5 + 0.9D1 * t6) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh92J2
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
      t1 = S13 * S24
      t8 = S24 ** 2
      t9 = S13 ** 2
      rrgq2qgh92J2 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.18D2 * S24
     # - 0.27D2 * S13) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.9D1 * t9 - 0.27D2 * t1
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh92J3
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
      t1 = S13 * S24
      t8 = S24 ** 2
      t9 = S13 ** 2
      rrgq2qgh92J3 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.18D2 * S24
     # - 0.27D2 * S13) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.9D1 * t9 - 0.27D2 * t1
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh92J4
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
      t1 = S13 * S24
      t8 = S24 ** 2
      t9 = S13 ** 2
      rrgq2qgh92J4 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.18D2 * S24
     # - 0.27D2 * S13) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.9D1 * t9 - 0.27D2 * t1
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh92J5
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
      t1 = S13 * S24
      t8 = S24 ** 2
      t9 = S13 ** 2
      rrgq2qgh92J5 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.18D2 * S24
     # - 0.27D2 * S13) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.9D1 * t9 - 0.27D2 * t1
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh92J6
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
      t8 = S24 ** 2
      t10 = S13 ** 2
      rrgq2qgh92J6 = (0.4D1 * S24 * S12 - 0.4D1 / 0.9D1 * S24 * (-0.18D2
     # * S24 - 0.27D2 * S13) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.9D
     #1 * t10 - 0.27D2 * S13 * S24) / S12) / pi * wd / z

      end function
  
 