  
      subroutine rrqg2qght9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh91J1  
      doubleprecision rrqg2qgh91J2  
      doubleprecision rrqg2qgh91J3  
      doubleprecision rrqg2qgh91J4  
      doubleprecision rrqg2qgh91J5  
      doubleprecision rrqg2qgh91J6  
      doubleprecision rrqg2qgh91J7  
      doubleprecision rrqg2qgh92J1  
      doubleprecision rrqg2qgh92J2  
      doubleprecision rrqg2qgh92J3  
      doubleprecision rrqg2qgh92J4  
      doubleprecision rrqg2qgh92J5  
      doubleprecision rrqg2qgh92J6  
      doubleprecision rrqg2qght9s1e1  
      doubleprecision rrqg2qght9s1e0  
      doubleprecision rrqg2qght9s1em1  
      doubleprecision rrqg2qght9s1em2  
      doubleprecision rrqg2qght9s1em3  
      doubleprecision rrqg2qght9s1em4  
      doubleprecision rrqg2qght9s2e1  
      doubleprecision rrqg2qght9s2e0  
      doubleprecision rrqg2qght9s2em1  
      doubleprecision rrqg2qght9s2em2  
      doubleprecision rrqg2qght9s2em3  
      doubleprecision rrqg2qght9s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght9s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght9s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght9s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght9s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght9s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght9s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght9s1e1
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
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
      t33 = -0.60D2 * lh * t26 + 0.240D3 * zeta3 + 0.120D3 * t30 * lh
      t34 = pi * t33
      t36 = t22 * t21 * pi
      t38 = t21 * pi
      t41 = -0.180D3 * t30 + 0.30D2 * t26
      t44 = (0.90D2 * t23 * lh + t34 + 0.15D2 * t36 - t38 * t41) * t3
      t45 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t52 = pi * t41
      t54 = (-0.180D3 * t38 * lh - 0.45D2 * t23 + t52) * t3
      t55 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t59 = pi * lh
      t63 = (0.180D3 * t59 + 0.90D2 * t38) * t3
      t64 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t75 = t26 ** 2
      t76 = t30 ** 2
      t82 = t22 ** 2
      t86 = (-0.30D2 * t36 * lh + t23 * t41 / 0.2D1 - t38 * t33 + pi * (
     #-0.480D3 * lh * zeta3 - t75 - 0.60D2 * t76 + 0.60D2 * t30 * t26) -
     # 0.15D2 / 0.4D1 * t82 * pi) * t3
      t87 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t91 = x1 ** 2
      t92 = x3 * t91
      t95 = log(0.4D1 * t92 * t19)
      t96 = t95 ** 2
      t104 = t3 * t6
      t110 = t104 * t87
      t111 = t52 * t110
      t113 = 0.1D1 / x3
      t115 = 0.1D1 / x1
      t118 = t91 * t15
      t119 = t12 * t18
      t122 = log(0.4D1 * t118 * t119)
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
      t171 = t170 ** 2
      t191 = log(0.4D1 * x3 * t15 * t119)
      t196 = t191 ** 2
      t199 = t196 * t191
      t219 = log(0.4D1 * t149 * t19)
      t220 = t219 ** 2
      t240 = log(0.4D1 * x2 * t18 * t16)
      t245 = t240 ** 2
      t248 = t245 * t240
      t266 = t4 * t6 * t7 / 0.16D2 - t44 * t6 * t45 / 0.1440D4 - t54 * t
     #6 * t55 / 0.1440D4 - t63 * t6 * t64 / 0.1440D4 - t86 * t6 * t87 / 
     #0.1440D4 - (-0.90D2 * t4 * t6 * (t96 * t87 / 0.2D1 - t95 * t45 + t
     #55) + 0.180D3 * t59 * t104 * (-t95 * t87 + t45) + t111) * t113 * t
     #115 / 0.720D3 - (t52 * t104 * (-t122 * t87 + t45) - 0.90D2 * t4 * 
     #t6 * (t127 * t45 / 0.2D1 + t64 - t130 * t87 / 0.6D1 - t122 * t55) 
     #+ t138 + 0.180D3 * t59 * t104 * (-t122 * t45 + t55 + t127 * t87 / 
     #0.2D1)) * t115 / 0.720D3 - (-0.90D2 * t4 * t6 * (t45 - t153 * t87)
     # + 0.180D3 * t59 * t110) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 *
     # t6 * (t171 * t87 / 0.2D1 - t170 * t45 + t55) + 0.180D3 * t59 * t1
     #04 * (-t170 * t87 + t45) + t111) * t163 * t115 / 0.720D3 - (t52 * 
     #t104 * (t45 - t191 * t87) - 0.90D2 * t4 * t6 * (t196 * t45 / 0.2D1
     # + t64 - t199 * t87 / 0.6D1 - t191 * t55) + t138 + 0.180D3 * t59 *
     # t104 * (-t191 * t45 + t55 + t196 * t87 / 0.2D1)) * t113 / 0.1440D
     #4 - (-0.90D2 * t4 * t6 * (t220 * t87 / 0.2D1 - t219 * t45 + t55) +
     # 0.180D3 * t59 * t104 * (-t219 * t87 + t45) + t111) * t113 * t163 
     #/ 0.1440D4 + (t52 * t104 * (-t45 + t240 * t87) - 0.90D2 * t4 * t6 
     #* (-t245 * t45 / 0.2D1 - t64 + t248 * t87 / 0.6D1 + t240 * t55) - 
     #t138 + 0.180D3 * t59 * t104 * (t240 * t45 - t55 - t245 * t87 / 0.2
     #D1)) * t163 / 0.1440D4
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t266)
      t269 = -0.1D1 + x1
      t270 = t2 * t269
      t271 = t2 * x1
      t273 = x1 * z
      t274 = 0.1D1 - x1 + t273
      t275 = 0.1D1 / t274
      t276 = t269 ** 2
      t277 = t275 * t276
      t281 = log(0.4D1 * t92 * t15 * t119 * t277)
      t282 = t281 ** 2
      t283 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t286 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t287 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t298 = t104 * t283
      t299 = t52 * t298
      t303 = t118 * t12
      t304 = t18 * t275
      t308 = log(0.4D1 * t303 * t304 * t276)
      t313 = t308 ** 2
      t316 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t317 = t313 * t308
      t336 = t149 * t91 * t18
      t337 = t16 * t277
      t340 = log(0.4D1 * t336 * t337)
      t354 = log(0.4D1 * t167 * t18 * t337)
      t355 = t354 ** 2
      t372 = -(-0.90D2 * t4 * t6 * (-t282 * t283 / 0.2D1 - t286 + t281 *
     # t287) + 0.180D3 * t59 * t104 * (-t287 + t281 * t283) - t299) * t1
     #13 * t115 / 0.720D3 - (t52 * t104 * (-t287 + t308 * t283) - 0.90D2
     # * t4 * t6 * (-t313 * t287 / 0.2D1 - t316 + t317 * t283 / 0.6D1 + 
     #t308 * t286) - t34 * t298 + 0.180D3 * t59 * t104 * (t308 * t287 - 
     #t313 * t283 / 0.2D1 - t286)) * t115 / 0.720D3 - (-0.90D2 * t4 * t6
     # * (t340 * t283 - t287) - 0.180D3 * t59 * t298) * t113 * t164 / 0.
     #720D3 - (-0.90D2 * t4 * t6 * (-t355 * t283 / 0.2D1 + t354 * t287 -
     # t286) + 0.180D3 * t59 * t104 * (t354 * t283 - t287) - t299) * t16
     #3 * t115 / 0.720D3
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t270, t271, 0.0D0, t372)
      t375 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t377 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t380 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t390 = t104 * t377
      t391 = t52 * t390
      t403 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t410 = t34 * t390
      t421 = rrqg2qgh92J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t526 = -(-0.90D2 * t4 * t6 * (-t219 * t375 + t220 * t377 / 0.2D1 +
     # t380) + 0.180D3 * t59 * t104 * (t375 - t219 * t377) + t391) * t11
     #3 * t163 / 0.1440D4 + (t52 * t104 * (-t375 + t240 * t377) - 0.90D2
     # * t4 * t6 * (-t245 * t375 / 0.2D1 + t240 * t380 - t403 + t248 * t
     #377 / 0.6D1) - t410 + 0.180D3 * t59 * t104 * (-t245 * t377 / 0.2D1
     # - t380 + t240 * t375)) * t163 / 0.1440D4 + t4 * t6 * t421 / 0.16D
     #2 - (t52 * t104 * (t375 - t191 * t377) - 0.90D2 * t4 * t6 * (t196 
     #* t375 / 0.2D1 - t191 * t380 + t403 - t199 * t377 / 0.6D1) + t410 
     #+ 0.180D3 * t59 * t104 * (t196 * t377 / 0.2D1 + t380 - t191 * t375
     #)) * t113 / 0.1440D4 - t54 * t6 * t380 / 0.1440D4 - t63 * t6 * t40
     #3 / 0.1440D4 - t86 * t6 * t377 / 0.1440D4 - t44 * t6 * t375 / 0.14
     #40D4 - (-0.90D2 * t4 * t6 * (t380 - t95 * t375 + t96 * t377 / 0.2D
     #1) + 0.180D3 * t59 * t104 * (t375 - t95 * t377) + t391) * t113 * t
     #115 / 0.720D3 - (t52 * t104 * (t375 - t122 * t377) - 0.90D2 * t4 *
     # t6 * (-t122 * t380 + t403 + t127 * t375 / 0.2D1 - t130 * t377 / 0
     #.6D1) + t410 + 0.180D3 * t59 * t104 * (-t122 * t375 + t127 * t377 
     #/ 0.2D1 + t380)) * t115 / 0.720D3 - (-0.90D2 * t4 * t6 * (t375 - t
     #153 * t377) + 0.180D3 * t59 * t390) * t113 * t164 / 0.720D3 - (-0.
     #90D2 * t4 * t6 * (t380 - t170 * t375 + t171 * t377 / 0.2D1) + 0.18
     #0D3 * t59 * t104 * (-t170 * t377 + t375) + t391) * t163 * t115 / 0
     #.720D3
      t527 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t526)
      t529 = t2 * x3
      t530 = -0.1D1 + x3
      t531 = t2 * t530
      t532 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t534 = t16 * t530
      t537 = log(-0.4D1 * x3 * t18 * t534)
      t538 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t543 = t537 ** 2
      t546 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t548 = t543 * t537
      t551 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0.
     #0D0, 0.0D0, 0.0D0)
      t556 = t104 * t538
      t571 = log(-0.4D1 * t92 * t18 * t534)
      t572 = t571 ** 2
      t585 = t52 * t556
      t590 = t18 * t15
      t595 = log(-0.4D1 * t150 * t590 * t12 * t530)
      t610 = log(-0.4D1 * t149 * t18 * t534)
      t611 = t610 ** 2
      t628 = -(t52 * t104 * (-t532 + t537 * t538) - 0.90D2 * t4 * t6 * (
     #-t543 * t532 / 0.2D1 + t537 * t546 + t548 * t538 / 0.6D1 - t551) -
     # t34 * t556 + 0.180D3 * t59 * t104 * (t537 * t532 - t546 - t543 * 
     #t538 / 0.2D1)) * t113 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t572 * t
     #538 / 0.2D1 + t571 * t532 - t546) + 0.180D3 * t59 * t104 * (t571 *
     # t538 - t532) - t585) * t113 * t115 / 0.720D3 - (-0.90D2 * t4 * t6
     # * (t595 * t538 - t532) - 0.180D3 * t59 * t556) * t113 * t164 / 0.
     #720D3 - (-0.90D2 * t4 * t6 * (-t611 * t538 / 0.2D1 + t610 * t532 -
     # t546) + 0.180D3 * t59 * t104 * (-t532 + t610 * t538) - t585) * t1
     #13 * t163 / 0.1440D4
      t629 = FJET(XB1, XB2, s, t529, 0.0D0, -t531, 0.0D0, 0.0D0, t628)
      t631 = 0.2D1 * t149
      t632 = cos(t13)
      t633 = -0.1D1 + x2
      t637 = Sqrt(x2 * t633 * x3 * t530)
      t639 = 0.2D1 * t632 * t637
      t641 = t2 * (0.1D1 - x2 - x3 + t631 + t639)
      t643 = t2 * (-x3 + t631 - x2 + t639)
      t644 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t643, t641, 0.
     #0D0, 0.0D0, 0.0D0)
      t645 = t149 * t118
      t647 = t119 * t633 * t530
      t650 = log(0.4D1 * t645 * t647)
      t651 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t643, t641, 0.
     #0D0, 0.0D0, 0.0D0)
      t657 = t104 * t651
      t664 = t149 * t15
      t667 = log(0.4D1 * t664 * t647)
      t668 = t667 ** 2
      t671 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t643, t641, 0.
     #0D0, 0.0D0, 0.0D0)
      t687 = -(-0.90D2 * t4 * t6 * (t644 - t650 * t651) + 0.180D3 * t59 
     #* t657) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 * t6 * (t668 * t65
     #1 / 0.2D1 + t671 - t667 * t644) + 0.180D3 * t59 * t104 * (-t667 * 
     #t651 + t644) + t52 * t657) * t113 * t163 / 0.1440D4
      t688 = FJET(XB1, XB2, s, t641, 0.0D0, -t643, 0.0D0, 0.0D0, t687)
      t691 = x2 * s * t1
      t692 = t633 * s
      t693 = t692 * t1
      t694 = t18 * t633
      t698 = log(-0.4D1 * t150 * t16 * t694)
      t699 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0.
     #0D0, 0.0D0, 0.0D0)
      t701 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0.
     #0D0, 0.0D0, 0.0D0)
      t706 = t104 * t699
      t713 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0.
     #0D0, 0.0D0, 0.0D0)
      t715 = t119 * t633
      t718 = log(-0.4D1 * t167 * t15 * t715)
      t720 = t718 ** 2
      t732 = t52 * t706
      t739 = log(-0.4D1 * t664 * t715)
      t740 = t739 ** 2
      t760 = log(-0.4D1 * x2 * t15 * t715)
      t765 = t760 ** 2
      t769 = t765 * t760
      t772 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0.
     #0D0, 0.0D0, 0.0D0)
      t788 = -(-0.90D2 * t4 * t6 * (t698 * t699 - t701) - 0.180D3 * t59 
     #* t706) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t713 + t7
     #18 * t701 - t720 * t699 / 0.2D1) + 0.180D3 * t59 * t104 * (t718 * 
     #t699 - t701) - t732) * t163 * t115 / 0.720D3 - (-0.90D2 * t4 * t6 
     #* (-t740 * t699 / 0.2D1 + t739 * t701 - t713) + 0.180D3 * t59 * t1
     #04 * (-t701 + t739 * t699) - t732) * t113 * t163 / 0.1440D4 + (t52
     # * t104 * (t701 - t760 * t699) - 0.90D2 * t4 * t6 * (t765 * t701 /
     # 0.2D1 - t760 * t713 - t769 * t699 / 0.6D1 + t772) + t34 * t706 + 
     #0.180D3 * t59 * t104 * (-t760 * t701 + t713 + t765 * t699 / 0.2D1)
     #) * t163 / 0.1440D4
      t789 = FJET(XB1, XB2, s, t691, 0.0D0, -t693, 0.0D0, 0.0D0, t788)
      t791 = t1 * t269
      t792 = t692 * t791
      t795 = t2 * t269 * x2 * t275
      t800 = s * t17 * x2 * t269 * x1 * t275
      t805 = log(-0.4D1 * t645 * t119 * t277 * t633)
      t806 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t795, t792, 0.
     #0D0, t271, -t800)
      t808 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t795, t792, 0.
     #0D0, t271, -t800)
      t813 = t104 * t806
      t823 = log(-0.4D1 * t167 * t16 * t694 * t277)
      t825 = t823 ** 2
      t828 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t795, t792, 0.
     #0D0, t271, -t800)
      t843 = -(-0.90D2 * t4 * t6 * (-t805 * t806 + t808) + 0.180D3 * t59
     # * t813) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t823 * t
     #808 + t825 * t806 / 0.2D1 + t828) + 0.180D3 * t59 * t104 * (t808 -
     # t823 * t806) + t52 * t813) * t163 * t115 / 0.720D3
      t844 = FJET(XB1, XB2, s, t792, t271, -t795, 0.0D0, -t800, t843)
      t846 = t530 * s
      t847 = t846 * t791
      t849 = t846 * t1 * x1
      t851 = x3 * s * t791
      t852 = x3 * x1
      t853 = t2 * t852
      t860 = log(-0.4D1 * t92 * t590 * t12 * t275 * t276 * t530)
      t861 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t851, t847, t8
     #53, -t849, 0.0D0)
      t863 = t860 ** 2
      t864 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t851, t847, t8
     #53, -t849, 0.0D0)
      t867 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t851, t847, t8
     #53, -t849, 0.0D0)
      t877 = t104 * t864
      t886 = log(-0.4D1 * t336 * t16 * t277 * t530)
      t898 = -(-0.90D2 * t4 * t6 * (-t860 * t861 + t863 * t864 / 0.2D1 +
     # t867) + 0.180D3 * t59 * t104 * (t861 - t860 * t864) + t52 * t877)
     # * t113 * t115 / 0.720D3 - (-0.90D2 * t4 * t6 * (t861 - t886 * t86
     #4) + 0.180D3 * t59 * t877) * t113 * t164 / 0.720D3
      t899 = FJET(XB1, XB2, s, t847, -t849, -t851, t853, 0.0D0, t898)
      t901 = t852 * z
      t902 = t149 * x1
      t903 = t149 * t273
      t908 = Sqrt(x3 * t633 * t274 * x2 * t530)
      t910 = 0.2D1 * t632 * t908
      t914 = t2 * t269 * (-x3 + t852 - t901 + t631 - t902 + t903 - x2 + 
     #t910) * t275
      t915 = x2 * x1
      t917 = 0.1D1 - x1 + t273 - x2 + t915 - t915 * z - x3 + t852 - t901
     # + t631 - t902 + t903 + t910
      t920 = t2 * t269 * t917 * t275
      t927 = log(0.4D1 * t149 * t303 * t304 * t276 * t633 * t530)
      t928 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t914, -t920, t8
     #53, -t849, -t800)
      t930 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t914, -t920, t8
     #53, -t849, -t800)
      t938 = -0.90D2 * t4 * t6 * (t927 * t928 - t930) - 0.180D3 * t59 * 
     #t104 * t928
      t942 = FJET(XB1, XB2, s, t914, t853, -t920, -t849, -t800, -t938 * 
     #t113 * t164 / 0.720D3)
      t945 = t113 * t163 * t115
      t948 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t951 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t952 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t963 = t104 * t948
      t964 = t52 * t963
      t975 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t270, 0
     #.0D0, t271, 0.0D0)
      t1018 = -(-0.90D2 * t4 * t6 * (-t282 * t948 / 0.2D1 - t951 + t281 
     #* t952) + 0.180D3 * t59 * t104 * (t281 * t948 - t952) - t964) * t1
     #13 * t115 / 0.720D3 - (t52 * t104 * (t308 * t948 - t952) - 0.90D2 
     #* t4 * t6 * (-t313 * t952 / 0.2D1 + t308 * t951 - t975 + t317 * t9
     #48 / 0.6D1) - t34 * t963 + 0.180D3 * t59 * t104 * (t308 * t952 - t
     #313 * t948 / 0.2D1 - t951)) * t115 / 0.720D3 - (-0.90D2 * t4 * t6 
     #* (-t952 + t340 * t948) - 0.180D3 * t59 * t963) * t113 * t164 / 0.
     #720D3 - (-0.90D2 * t4 * t6 * (-t355 * t948 / 0.2D1 - t951 + t354 *
     # t952) + 0.180D3 * t59 * t104 * (-t952 + t354 * t948) - t964) * t1
     #63 * t115 / 0.720D3
      t1019 = FJET(XB1, XB2, s, -t270, t271, 0.0D0, 0.0D0, 0.0D0, t1018)
      t1021 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0
     #.0D0, 0.0D0, 0.0D0)
      t1022 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0
     #.0D0, 0.0D0, 0.0D0)
      t1027 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0
     #.0D0, 0.0D0, 0.0D0)
      t1031 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, t529, -t531, 0
     #.0D0, 0.0D0, 0.0D0)
      t1038 = t104 * t1022
      t1062 = t52 * t1038
      t1094 = -(t52 * t104 * (-t1021 + t537 * t1022) - 0.90D2 * t4 * t6 
     #* (t537 * t1027 + t548 * t1022 / 0.6D1 - t1031 - t543 * t1021 / 0.
     #2D1) - t34 * t1038 + 0.180D3 * t59 * t104 * (-t543 * t1022 / 0.2D1
     # - t1027 + t537 * t1021)) * t113 / 0.1440D4 - (-0.90D2 * t4 * t6 *
     # (t571 * t1021 - t572 * t1022 / 0.2D1 - t1027) + 0.180D3 * t59 * t
     #104 * (-t1021 + t571 * t1022) - t1062) * t113 * t115 / 0.720D3 - (
     #-0.90D2 * t4 * t6 * (-t1021 + t595 * t1022) - 0.180D3 * t59 * t103
     #8) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t1027 - t611 *
     # t1022 / 0.2D1 + t610 * t1021) + 0.180D3 * t59 * t104 * (-t1021 + 
     #t610 * t1022) - t1062) * t113 * t163 / 0.1440D4
      t1095 = FJET(XB1, XB2, s, -t531, 0.0D0, t529, 0.0D0, 0.0D0, t1094)
      t1097 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t643, t641, 0
     #.0D0, 0.0D0, 0.0D0)
      t1099 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t643, t641, 0
     #.0D0, 0.0D0, 0.0D0)
      t1104 = t104 * t1097
      t1114 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t643, t641, 0
     #.0D0, 0.0D0, 0.0D0)
      t1129 = -(-0.90D2 * t4 * t6 * (-t650 * t1097 + t1099) + 0.180D3 * 
     #t59 * t1104) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 * t6 * (t668 
     #* t1097 / 0.2D1 - t667 * t1099 + t1114) + 0.180D3 * t59 * t104 * (
     #t1099 - t667 * t1097) + t52 * t1104) * t113 * t163 / 0.1440D4
      t1130 = FJET(XB1, XB2, s, -t643, 0.0D0, t641, 0.0D0, 0.0D0, t1129)
      t1132 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0
     #.0D0, 0.0D0, 0.0D0)
      t1133 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0
     #.0D0, 0.0D0, 0.0D0)
      t1139 = t104 * t1133
      t1149 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0
     #.0D0, 0.0D0, 0.0D0)
      t1159 = t52 * t1139
      t1187 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, t691, -t693, 0
     #.0D0, 0.0D0, 0.0D0)
      t1205 = -(-0.90D2 * t4 * t6 * (-t1132 + t698 * t1133) - 0.180D3 * 
     #t59 * t1139) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 * t6 * (t718 
     #* t1132 - t720 * t1133 / 0.2D1 - t1149) + 0.180D3 * t59 * t104 * (
     #-t1132 + t718 * t1133) - t1159) * t163 * t115 / 0.720D3 - (-0.90D2
     # * t4 * t6 * (-t1149 - t740 * t1133 / 0.2D1 + t739 * t1132) + 0.18
     #0D3 * t59 * t104 * (-t1132 + t739 * t1133) - t1159) * t113 * t163 
     #/ 0.1440D4 + (t52 * t104 * (t1132 - t760 * t1133) - 0.90D2 * t4 * 
     #t6 * (-t760 * t1149 - t769 * t1133 / 0.6D1 + t1187 + t765 * t1132 
     #/ 0.2D1) + t34 * t1139 + 0.180D3 * t59 * t104 * (t765 * t1133 / 0.
     #2D1 + t1149 - t760 * t1132)) * t163 / 0.1440D4
      t1206 = FJET(XB1, XB2, s, -t693, 0.0D0, t691, 0.0D0, 0.0D0, t1205)
      t1208 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t851, t847, t
     #853, -t849, 0.0D0)
      t1211 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t851, t847, t
     #853, -t849, 0.0D0)
      t1212 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t851, t847, t
     #853, -t849, 0.0D0)
      t1223 = t104 * t1208
      t1239 = -(-0.90D2 * t4 * t6 * (t863 * t1208 / 0.2D1 + t1211 - t860
     # * t1212) + 0.180D3 * t59 * t104 * (t1212 - t860 * t1208) + t52 * 
     #t1223) * t113 * t115 / 0.720D3 - (-0.90D2 * t4 * t6 * (t1212 - t88
     #6 * t1208) + 0.180D3 * t59 * t1223) * t113 * t164 / 0.720D3
      t1240 = FJET(XB1, XB2, s, -t851, t853, t847, -t849, 0.0D0, t1239)
      t1242 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t795, t792, 0
     #.0D0, t271, -t800)
      t1244 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t795, t792, 0
     #.0D0, t271, -t800)
      t1249 = t104 * t1242
      t1258 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t795, t792, 0
     #.0D0, t271, -t800)
      t1273 = -(-0.90D2 * t4 * t6 * (-t805 * t1242 + t1244) + 0.180D3 * 
     #t59 * t1249) * t113 * t164 / 0.720D3 - (-0.90D2 * t4 * t6 * (t825 
     #* t1242 / 0.2D1 - t823 * t1244 + t1258) + 0.180D3 * t59 * t104 * (
     #-t823 * t1242 + t1244) + t52 * t1249) * t163 * t115 / 0.720D3
      t1274 = FJET(XB1, XB2, s, -t795, 0.0D0, t792, t271, -t800, t1273)
      t1276 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t914, -t920, t
     #853, -t849, -t800)
      t1278 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t914, -t920, t
     #853, -t849, -t800)
      t1286 = -0.90D2 * t4 * t6 * (t927 * t1276 - t1278) - 0.180D3 * t59
     # * t104 * t1276
      t1290 = FJET(XB1, XB2, s, -t920, -t849, t914, t853, -t800, -t1286 
     #* t113 * t164 / 0.720D3)
      rrqg2qght9s1e1 = t267 * t266 + t373 * t372 + t527 * t526 + t629 * 
     #t628 + t688 * t687 + t789 * t788 + t844 * t843 + t899 * t898 - t94
     #2 * t938 * t945 / 0.720D3 + t1019 * t1018 + t1095 * t1094 + t1130 
     #* t1129 + t1206 * t1205 + t1240 * t1239 + t1274 * t1273 - t1290 * 
     #t1286 * t945 / 0.720D3

      end function



      doubleprecision function rrqg2qght9s1e0
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t19 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = t18 ** 2
      t23 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t30 = pi * lh
      t31 = t3 * t6
      t37 = lh ** 2
      t39 = pi ** 2
      t41 = -0.180D3 * t37 + 0.30D2 * t39
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
      t60 = (-0.180D3 * t53 * lh - 0.45D2 * t57 + t42) * t3
      t64 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t82 = (0.90D2 * t57 * lh + pi * (-0.60D2 * lh * t39 + 0.240D3 * ze
     #ta3 + 0.120D3 * t37 * lh) + 0.15D2 * t56 * t52 * pi - t53 * t41) *
     # t3
      t89 = (0.180D3 * t30 + 0.90D2 * t53) * t3
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
      t166 = log(0.4D1 * t163 * t15)
      t168 = t166 ** 2
      t183 = -(-0.90D2 * t4 * t6 * (-t18 * t19 + t21 + t22 * t23 / 0.2D1
     #) + 0.180D3 * t30 * t31 * (t19 - t18 * t23) + t44) * t46 / 0.1440D
     #4 - t60 * t6 * t19 / 0.1440D4 + t4 * t6 * t64 / 0.16D2 - t82 * t6 
     #* t23 / 0.1440D4 - t89 * t6 * t21 / 0.1440D4 - (-0.90D2 * t4 * t6 
     #* (-t96 * t23 + t19) + t103) * t46 * t106 / 0.1440D4 + (-0.90D2 * 
     #t4 * t6 * (t112 * t19 - t21 - t114 * t23 / 0.2D1) + 0.180D3 * t30 
     #* t31 * (-t19 + t112 * t23) - t44) * t106 / 0.1440D4 - (-0.90D2 * 
     #t4 * t6 * (-t133 * t23 + t19) + t103) * t46 * t141 / 0.720D3 + t14
     #4 * t23 * t46 * t146 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t153 * t23 +
     # t19) + t103) * t106 * t141 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t16
     #6 * t19 + t21 + t168 * t23 / 0.2D1) + 0.180D3 * t30 * t31 * (-t166
     # * t23 + t19) + t44) * t141 / 0.720D3
      t184 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t183)
      t186 = -0.1D1 + x1
      t187 = t2 * t186
      t188 = t2 * x1
      t189 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t187, 0
     #.0D0, t188, 0.0D0)
      t191 = x1 * z
      t192 = 0.1D1 - x1 + t191
      t193 = 0.1D1 / t192
      t194 = t186 ** 2
      t195 = t193 * t194
      t199 = log(0.4D1 * t130 * t9 * t15 * t195)
      t200 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t187, 0
     #.0D0, t188, 0.0D0)
      t206 = t31 * t200
      t208 = 0.180D3 * t30 * t206
      t221 = log(0.4D1 * t150 * t14 * t49 * t195)
      t236 = log(0.4D1 * t163 * t12 * t14 * t193 * t194)
      t238 = t236 ** 2
      t241 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t187, 0
     #.0D0, t188, 0.0D0)
      t255 = -(-0.90D2 * t4 * t6 * (-t189 + t199 * t200) - t208) * t46 *
     # t141 / 0.720D3 - t144 * t200 * t46 * t146 / 0.8D1 - (-0.90D2 * t4
     # * t6 * (t221 * t200 - t189) - t208) * t106 * t141 / 0.720D3 - (-0
     #.90D2 * t4 * t6 * (t236 * t189 - t238 * t200 / 0.2D1 - t241) + 0.1
     #80D3 * t30 * t31 * (-t189 + t236 * t200) - t42 * t206) * t141 / 0.
     #720D3
      t256 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t187, t188, 0.0D0, t255)
      t258 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t261 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t262 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t273 = t31 * t258
      t274 = t42 * t273
      t278 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t294 = 0.180D3 * t30 * t273
      t354 = -(-0.90D2 * t4 * t6 * (t22 * t258 / 0.2D1 + t261 - t18 * t2
     #62) + 0.180D3 * t30 * t31 * (t262 - t18 * t258) + t274) * t46 / 0.
     #1440D4 + t4 * t6 * t278 / 0.16D2 - t89 * t6 * t261 / 0.1440D4 - t6
     #0 * t6 * t262 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t262 - t96 * t258
     #) + t294) * t46 * t106 / 0.1440D4 + (-0.90D2 * t4 * t6 * (-t114 * 
     #t258 / 0.2D1 - t261 + t112 * t262) + 0.180D3 * t30 * t31 * (-t262 
     #+ t112 * t258) - t274) * t106 / 0.1440D4 - t82 * t6 * t258 / 0.144
     #0D4 - (-0.90D2 * t4 * t6 * (t262 - t133 * t258) + t294) * t46 * t1
     #41 / 0.720D3 + t144 * t258 * t46 * t146 / 0.8D1 - (-0.90D2 * t4 * 
     #t6 * (-t153 * t258 + t262) + t294) * t106 * t141 / 0.720D3 - (-0.9
     #0D2 * t4 * t6 * (-t166 * t262 + t168 * t258 / 0.2D1 + t261) + 0.18
     #0D3 * t30 * t31 * (t262 - t166 * t258) + t274) * t141 / 0.720D3
      t355 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t354)
      t357 = t2 * x3
      t358 = -0.1D1 + x3
      t359 = t2 * t358
      t361 = t49 * t358
      t364 = log(-0.4D1 * x3 * t14 * t361)
      t365 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t367 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t368 = t364 ** 2
      t369 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t381 = t31 * t369
      t389 = log(-0.4D1 * t93 * t14 * t361)
      t396 = 0.180D3 * t30 * t381
      t404 = log(-0.4D1 * t130 * t14 * t361)
      t418 = -(-0.90D2 * t4 * t6 * (t364 * t365 - t367 - t368 * t369 / 0
     #.2D1) + 0.180D3 * t30 * t31 * (-t365 + t364 * t369) - t42 * t381) 
     #* t46 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t365 + t389 * t369) - t3
     #96) * t46 * t106 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t404 * t369 - 
     #t365) - t396) * t46 * t141 / 0.720D3 - t144 * t369 * t46 * t146 / 
     #0.8D1
      t419 = FJET(XB1, XB2, s, t357, 0.0D0, -t359, 0.0D0, 0.0D0, t418)
      t421 = 0.2D1 * t93
      t422 = cos(t7)
      t423 = -0.1D1 + x2
      t427 = Sqrt(x2 * t423 * x3 * t358)
      t429 = 0.2D1 * t422 * t427
      t431 = t2 * (0.1D1 - x2 - x3 + t421 + t429)
      t433 = t2 * (-x3 + t421 - x2 + t429)
      t434 = t93 * t9
      t439 = log(0.4D1 * t434 * t15 * t423 * t358)
      t440 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t433, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t442 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t433, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t458 = -(-0.90D2 * t4 * t6 * (-t439 * t440 + t442) + 0.180D3 * t30
     # * t31 * t440) * t46 * t106 / 0.1440D4 + t144 * t440 * t46 * t146 
     #/ 0.8D1
      t459 = FJET(XB1, XB2, s, t431, 0.0D0, -t433, 0.0D0, 0.0D0, t458)
      t462 = x2 * s * t1
      t463 = t423 * s
      t464 = t463 * t1
      t465 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t462, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t466 = t15 * t423
      t469 = log(-0.4D1 * t434 * t466)
      t470 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t462, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t476 = t31 * t470
      t478 = 0.180D3 * t30 * t476
      t486 = log(-0.4D1 * x2 * t9 * t466)
      t488 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t462, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t489 = t486 ** 2
      t512 = log(-0.4D1 * t150 * t9 * t466)
      t522 = -(-0.90D2 * t4 * t6 * (-t465 + t469 * t470) - t478) * t46 *
     # t106 / 0.1440D4 + (-0.90D2 * t4 * t6 * (-t486 * t465 + t488 + t48
     #9 * t470 / 0.2D1) + 0.180D3 * t30 * t31 * (t465 - t486 * t470) + t
     #42 * t476) * t106 / 0.1440D4 - t144 * t470 * t46 * t146 / 0.8D1 - 
     #(-0.90D2 * t4 * t6 * (t512 * t470 - t465) - t478) * t106 * t141 / 
     #0.720D3
      t523 = FJET(XB1, XB2, s, t462, 0.0D0, -t464, 0.0D0, 0.0D0, t522)
      t525 = t1 * t186
      t526 = t463 * t525
      t529 = t2 * t186 * x2 * t193
      t534 = s * t13 * x2 * t186 * x1 * t193
      t535 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t529, t526, 0.
     #0D0, t188, -t534)
      t540 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t529, t526, 0.
     #0D0, t188, -t534)
      t546 = log(-0.4D1 * t150 * t49 * t14 * t423 * t195)
      t559 = t144 * t535 * t46 * t146 / 0.8D1 - (-0.90D2 * t4 * t6 * (t5
     #40 - t546 * t535) + 0.180D3 * t30 * t31 * t535) * t106 * t141 / 0.
     #720D3
      t560 = FJET(XB1, XB2, s, t526, t188, -t529, 0.0D0, -t534, t559)
      t562 = t358 * s
      t563 = t562 * t525
      t565 = t562 * t1 * x1
      t567 = x3 * s * t525
      t568 = x3 * x1
      t569 = t2 * t568
      t570 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t567, t563, t5
     #69, -t565, 0.0D0)
      t578 = log(-0.4D1 * t130 * t14 * t9 * t12 * t193 * t194 * t358)
      t579 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t567, t563, t5
     #69, -t565, 0.0D0)
      t596 = -(-0.90D2 * t4 * t6 * (t570 - t578 * t579) + 0.180D3 * t30 
     #* t31 * t579) * t46 * t141 / 0.720D3 + t144 * t579 * t46 * t146 / 
     #0.8D1
      t597 = FJET(XB1, XB2, s, t563, -t565, -t567, t569, 0.0D0, t596)
      t599 = t568 * z
      t600 = t93 * x1
      t601 = t93 * t191
      t606 = Sqrt(x3 * t423 * t192 * x2 * t358)
      t608 = 0.2D1 * t422 * t606
      t612 = t2 * t186 * (-x3 + t568 - t599 + t421 - t600 + t601 - x2 + 
     #t608) * t193
      t613 = x2 * x1
      t615 = 0.1D1 - x1 + t191 - x2 + t613 - t613 * z - x3 + t568 - t599
     # + t421 - t600 + t601 + t608
      t618 = t2 * t186 * t615 * t193
      t619 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t612, -t618, t5
     #69, -t565, -t534)
      t621 = t619 * t46 * t146
      t624 = FJET(XB1, XB2, s, t612, t569, -t618, -t565, -t534, -t144 * 
     #t621 / 0.8D1)
      t629 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t187, 0
     #.0D0, t188, 0.0D0)
      t631 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t187, 0
     #.0D0, t188, 0.0D0)
      t636 = t31 * t629
      t638 = 0.180D3 * t30 * t636
      t659 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t187, 0
     #.0D0, t188, 0.0D0)
      t673 = -(-0.90D2 * t4 * t6 * (t199 * t629 - t631) - t638) * t46 * 
     #t141 / 0.720D3 - t144 * t629 * t46 * t146 / 0.8D1 - (-0.90D2 * t4 
     #* t6 * (-t631 + t221 * t629) - t638) * t106 * t141 / 0.720D3 - (-0
     #.90D2 * t4 * t6 * (t236 * t631 - t238 * t629 / 0.2D1 - t659) + 0.1
     #80D3 * t30 * t31 * (t236 * t629 - t631) - t42 * t636) * t141 / 0.7
     #20D3
      t674 = FJET(XB1, XB2, s, -t187, t188, 0.0D0, 0.0D0, 0.0D0, t673)
      t676 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t679 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t680 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t357, -t359, 0.
     #0D0, 0.0D0, 0.0D0)
      t691 = t31 * t676
      t702 = 0.180D3 * t30 * t691
      t720 = -(-0.90D2 * t4 * t6 * (-t368 * t676 / 0.2D1 - t679 + t364 *
     # t680) + 0.180D3 * t30 * t31 * (-t680 + t364 * t676) - t42 * t691)
     # * t46 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t680 + t389 * t676) - t
     #702) * t46 * t106 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t680 + t404 
     #* t676) - t702) * t46 * t141 / 0.720D3 - t144 * t676 * t46 * t146 
     #/ 0.8D1
      t721 = FJET(XB1, XB2, s, -t359, 0.0D0, t357, 0.0D0, 0.0D0, t720)
      t723 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t433, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t724 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t433, t431, 0.
     #0D0, 0.0D0, 0.0D0)
      t741 = -(-0.90D2 * t4 * t6 * (t723 - t439 * t724) + 0.180D3 * t30 
     #* t31 * t724) * t46 * t106 / 0.1440D4 + t144 * t724 * t46 * t146 /
     # 0.8D1
      t742 = FJET(XB1, XB2, s, -t433, 0.0D0, t431, 0.0D0, 0.0D0, t741)
      t744 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t462, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t745 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t462, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t751 = t31 * t745
      t753 = 0.180D3 * t30 * t751
      t760 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t462, -t464, 0.
     #0D0, 0.0D0, 0.0D0)
      t788 = -(-0.90D2 * t4 * t6 * (-t744 + t469 * t745) - t753) * t46 *
     # t106 / 0.1440D4 + (-0.90D2 * t4 * t6 * (t489 * t745 / 0.2D1 + t76
     #0 - t486 * t744) + 0.180D3 * t30 * t31 * (t744 - t486 * t745) + t4
     #2 * t751) * t106 / 0.1440D4 - t144 * t745 * t46 * t146 / 0.8D1 - (
     #-0.90D2 * t4 * t6 * (-t744 + t512 * t745) - t753) * t106 * t141 / 
     #0.720D3
      t789 = FJET(XB1, XB2, s, -t464, 0.0D0, t462, 0.0D0, 0.0D0, t788)
      t791 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t567, t563, t5
     #69, -t565, 0.0D0)
      t792 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t567, t563, t5
     #69, -t565, 0.0D0)
      t809 = -(-0.90D2 * t4 * t6 * (t791 - t578 * t792) + 0.180D3 * t30 
     #* t31 * t792) * t46 * t141 / 0.720D3 + t144 * t792 * t46 * t146 / 
     #0.8D1
      t810 = FJET(XB1, XB2, s, -t567, t569, t563, -t565, 0.0D0, t809)
      t812 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t529, t526, 0.
     #0D0, t188, -t534)
      t818 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t529, t526, 0.
     #0D0, t188, -t534)
      t830 = t144 * t812 * t46 * t146 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t
     #546 * t812 + t818) + 0.180D3 * t30 * t31 * t812) * t106 * t141 / 0
     #.720D3
      t831 = FJET(XB1, XB2, s, -t529, 0.0D0, t526, t188, -t534, t830)
      t833 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t612, -t618, t5
     #69, -t565, -t534)
      t835 = t833 * t46 * t146
      t838 = FJET(XB1, XB2, s, -t618, -t565, t612, t569, -t534, -t144 * 
     #t835 / 0.8D1)
      rrqg2qght9s1e0 = t184 * t183 + t256 * t255 + t355 * t354 + t419 * 
     #t418 + t459 * t458 + t523 * t522 + t560 * t559 + t597 * t596 - t62
     #4 * pi * t31 * t621 / 0.8D1 + t674 * t673 + t721 * t720 + t742 * t
     #741 + t789 * t788 + t810 * t809 + t831 * t830 - t838 * pi * t31 * 
     #t835 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s1em1
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * x3 * t10 * t16)
      t20 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = pi * lh
      t27 = t3 * t6
      t30 = 0.180D3 * t26 * t27 * t20
      t32 = 0.1D1 / x3
      t35 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t40 = t13 * t10
      t43 = log(0.4D1 * t40 * t15)
      t44 = t43 * pi
      t47 = (0.180D3 * t26 + 0.90D2 * t44) * t3
      t53 = t43 ** 2
      t56 = lh ** 2
      t58 = pi ** 2
      t63 = (-0.180D3 * t44 * lh - 0.45D2 * t53 * pi + pi * (-0.180D3 * 
     #t56 + 0.30D2 * t58)) * t3
      t67 = t4 * t6
      t68 = t20 * t32
      t69 = 0.1D1 / x2
      t76 = log(0.4D1 * x2 * t15 * t40)
      t86 = 0.1D1 / x1
      t90 = x1 ** 2
      t91 = t90 * t10
      t94 = log(0.4D1 * t91 * t16)
      t106 = -(-0.90D2 * t4 * t6 * (t7 - t19 * t20) + t30) * t32 / 0.144
     #0D4 + t4 * t6 * t35 / 0.16D2 - t47 * t6 * t7 / 0.1440D4 - t63 * t6
     # * t20 / 0.1440D4 + t67 * t68 * t69 / 0.16D2 + (-0.90D2 * t4 * t6 
     #* (-t7 + t76 * t20) - t30) * t69 / 0.1440D4 + t67 * t20 * t69 * t8
     #6 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t94 * t20 + t7) + t30) * t86 / 
     #0.720D3 + t67 * t68 * t86 / 0.8D1
      t107 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t106)
      t109 = -0.1D1 + x1
      t110 = t2 * t109
      t111 = t2 * x1
      t112 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, 0
     #.0D0, t111, 0.0D0)
      t117 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, 0
     #.0D0, t111, 0.0D0)
      t121 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t123 = t109 ** 2
      t127 = log(0.4D1 * t91 * t13 * t15 * t121 * t123)
      t143 = -t67 * t112 * t69 * t86 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t1
     #17 + t127 * t112) - 0.180D3 * t26 * t27 * t112) * t86 / 0.720D3 - 
     #t67 * t112 * t32 * t86 / 0.8D1
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t110, t111, 0.0D0, t143)
      t146 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t150 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t154 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t162 = 0.180D3 * t26 * t27 * t150
      t166 = t150 * t32
      t196 = t4 * t6 * t146 / 0.16D2 - t63 * t6 * t150 / 0.1440D4 - (-0.
     #90D2 * t4 * t6 * (t154 - t150 * t19) + t162) * t32 / 0.1440D4 + t6
     #7 * t166 * t69 / 0.16D2 + (-0.90D2 * t4 * t6 * (-t154 + t76 * t150
     #) - t162) * t69 / 0.1440D4 - t47 * t6 * t154 / 0.1440D4 + t67 * t1
     #50 * t69 * t86 / 0.8D1 - (-0.90D2 * t4 * t6 * (t154 - t94 * t150) 
     #+ t162) * t86 / 0.720D3 + t67 * t166 * t86 / 0.8D1
      t197 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t196)
      t199 = t2 * x3
      t200 = -0.1D1 + x3
      t201 = t2 * t200
      t202 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t207 = log(-0.4D1 * x3 * t15 * t40 * t200)
      t208 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t220 = t208 * t32
      t227 = -(-0.90D2 * t4 * t6 * (-t202 + t207 * t208) - 0.180D3 * t26
     # * t27 * t208) * t32 / 0.1440D4 - t67 * t220 * t69 / 0.16D2 - t67 
     #* t220 * t86 / 0.8D1
      t228 = FJET(XB1, XB2, s, t199, 0.0D0, -t201, 0.0D0, 0.0D0, t227)
      t231 = 0.2D1 * x2 * x3
      t232 = cos(t8)
      t233 = -0.1D1 + x2
      t237 = Sqrt(x2 * t233 * x3 * t200)
      t239 = 0.2D1 * t232 * t237
      t241 = t2 * (0.1D1 - x2 - x3 + t231 + t239)
      t243 = t2 * (-x3 + t231 - x2 + t239)
      t244 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t243, t241, 0.
     #0D0, 0.0D0, 0.0D0)
      t249 = FJET(XB1, XB2, s, t241, 0.0D0, -t243, 0.0D0, 0.0D0, t67 * t
     #244 * t32 * t69 / 0.16D2)
      t253 = t32 * t69
      t258 = x2 * s * t1
      t259 = t233 * s
      t260 = t259 * t1
      t261 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t266 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t271 = log(-0.4D1 * x2 * t10 * t16 * t233)
      t287 = -t67 * t261 * t32 * t69 / 0.16D2 + (-0.90D2 * t4 * t6 * (t2
     #66 - t271 * t261) + 0.180D3 * t26 * t27 * t261) * t69 / 0.1440D4 -
     # t67 * t261 * t69 * t86 / 0.8D1
      t288 = FJET(XB1, XB2, s, t258, 0.0D0, -t260, 0.0D0, 0.0D0, t287)
      t290 = t1 * t109
      t291 = t259 * t290
      t294 = t2 * t109 * x2 * t121
      t299 = s * t14 * x2 * t109 * x1 * t121
      t300 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t294, t291, 0.
     #0D0, t111, -t299)
      t305 = FJET(XB1, XB2, s, t291, t111, -t294, 0.0D0, -t299, t67 * t3
     #00 * t69 * t86 / 0.8D1)
      t309 = t69 * t86
      t313 = t200 * s
      t314 = t313 * t290
      t316 = t313 * t1 * x1
      t318 = x3 * s * t290
      t320 = t2 * x1 * x3
      t321 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t318, t314, t3
     #20, -t316, 0.0D0)
      t326 = FJET(XB1, XB2, s, t314, -t316, -t318, t320, 0.0D0, t67 * t3
     #21 * t32 * t86 / 0.8D1)
      t330 = t32 * t86
      t334 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, 0
     #.0D0, t111, 0.0D0)
      t340 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t110, 0
     #.0D0, t111, 0.0D0)
      t355 = -t67 * t334 * t69 * t86 / 0.8D1 - (-0.90D2 * t4 * t6 * (t12
     #7 * t334 - t340) - 0.180D3 * t26 * t27 * t334) * t86 / 0.720D3 - t
     #67 * t334 * t32 * t86 / 0.8D1
      t356 = FJET(XB1, XB2, s, -t110, t111, 0.0D0, 0.0D0, 0.0D0, t355)
      t358 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t359 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t199, -t201, 0.
     #0D0, 0.0D0, 0.0D0)
      t371 = t359 * t32
      t378 = -(-0.90D2 * t4 * t6 * (-t358 + t207 * t359) - 0.180D3 * t26
     # * t27 * t359) * t32 / 0.1440D4 - t67 * t371 * t69 / 0.16D2 - t67 
     #* t371 * t86 / 0.8D1
      t379 = FJET(XB1, XB2, s, -t201, 0.0D0, t199, 0.0D0, 0.0D0, t378)
      t381 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t243, t241, 0.
     #0D0, 0.0D0, 0.0D0)
      t386 = FJET(XB1, XB2, s, -t243, 0.0D0, t241, 0.0D0, 0.0D0, t67 * t
     #381 * t32 * t69 / 0.16D2)
      t393 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t398 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t258, -t260, 0.
     #0D0, 0.0D0, 0.0D0)
      t414 = -t67 * t393 * t32 * t69 / 0.16D2 + (-0.90D2 * t4 * t6 * (t3
     #98 - t271 * t393) + 0.180D3 * t26 * t27 * t393) * t69 / 0.1440D4 -
     # t67 * t393 * t69 * t86 / 0.8D1
      t415 = FJET(XB1, XB2, s, -t260, 0.0D0, t258, 0.0D0, 0.0D0, t414)
      t417 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t318, t314, t3
     #20, -t316, 0.0D0)
      t422 = FJET(XB1, XB2, s, -t318, t320, t314, -t316, 0.0D0, t67 * t4
     #17 * t32 * t86 / 0.8D1)
      t429 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t294, t291, 0.
     #0D0, t111, -t299)
      t434 = FJET(XB1, XB2, s, -t294, 0.0D0, t291, t111, -t299, t67 * t4
     #29 * t69 * t86 / 0.8D1)
      rrqg2qght9s1em1 = t107 * t106 + t144 * t143 + t197 * t196 + t228 *
     # t227 + t249 * pi * t3 * t6 * t244 * t253 / 0.16D2 + t288 * t287 +
     # t305 * pi * t3 * t6 * t300 * t309 / 0.8D1 + t326 * pi * t3 * t6 *
     # t321 * t330 / 0.8D1 + t356 * t355 + t379 * t378 + t386 * pi * t3 
     #* t6 * t381 * t253 / 0.16D2 + t415 * t414 + t422 * pi * t3 * t6 * 
     #t417 * t330 / 0.8D1 + t434 * pi * t3 * t6 * t429 * t309 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s1em2
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = z ** 2
      t30 = Sin(x4 * pi)
      t31 = t30 ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t31 * t34)
      t41 = (0.180D3 * pi * lh + 0.90D2 * t37 * pi) * t3
      t44 = t4 * t8 * t9 / 0.16D2 + t4 * t8 * t13 / 0.16D2 + t4 * t8 * t
     #17 / 0.8D1 + t4 * t6 * t21 / 0.16D2 - t41 * t8 / 0.1440D4
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t44)
      t48 = t2 * (-0.1D1 + x1)
      t49 = t2 * x1
      t50 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t48, 0.0
     #D0, t49, 0.0D0)
      t52 = t6 * t50 * t17
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t48, t49, 0.0D0, -t4 * t52 
     #/ 0.8D1)
      t60 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = t6 * t60
      t71 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t77 = t4 * t61 * t9 / 0.16D2 + t4 * t61 * t13 / 0.16D2 + t4 * t61 
     #* t17 / 0.8D1 + t4 * t6 * t71 / 0.16D2 - t41 * t61 / 0.1440D4
      t78 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t77)
      t80 = t2 * x3
      t82 = t2 * (-0.1D1 + x3)
      t83 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t82, 0.0D0
     #, 0.0D0, 0.0D0)
      t85 = t6 * t83 * t9
      t88 = FJET(XB1, XB2, s, t80, 0.0D0, -t82, 0.0D0, 0.0D0, -t4 * t85 
     #/ 0.16D2)
      t94 = x2 * s * t1
      t97 = (-0.1D1 + x2) * s * t1
      t98 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t94, -t97, 0.0D0
     #, 0.0D0, 0.0D0)
      t100 = t6 * t98 * t13
      t103 = FJET(XB1, XB2, s, t94, 0.0D0, -t97, 0.0D0, 0.0D0, -t4 * t10
     #0 / 0.16D2)
      t108 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t80, -t82, 0.0D
     #0, 0.0D0, 0.0D0)
      t110 = t6 * t108 * t9
      t113 = FJET(XB1, XB2, s, -t82, 0.0D0, t80, 0.0D0, 0.0D0, -t4 * t11
     #0 / 0.16D2)
      t118 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t94, -t97, 0.0D
     #0, 0.0D0, 0.0D0)
      t120 = t6 * t118 * t13
      t123 = FJET(XB1, XB2, s, -t97, 0.0D0, t94, 0.0D0, 0.0D0, -t4 * t12
     #0 / 0.16D2)
      t128 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t48, 0.
     #0D0, t49, 0.0D0)
      t130 = t6 * t128 * t17
      t133 = FJET(XB1, XB2, s, -t48, t49, 0.0D0, 0.0D0, 0.0D0, -t4 * t13
     #0 / 0.8D1)
      rrqg2qght9s1em2 = t45 * t44 - t55 * pi * t3 * t52 / 0.8D1 + t78 * 
     #t77 - t88 * pi * t3 * t85 / 0.16D2 - t103 * pi * t3 * t100 / 0.16D
     #2 - t113 * pi * t3 * t110 / 0.16D2 - t123 * pi * t3 * t120 / 0.16D
     #2 - t133 * pi * t3 * t130 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s1em3
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t6 * 
     #t7 / 0.16D2)
      t13 = t3 * t6
      t16 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t6 * 
     #t16 / 0.16D2)
      rrqg2qght9s1em3 = t11 * pi * t13 * t7 / 0.16D2 + t20 * pi * t13 * 
     #t16 / 0.16D2

      end function



      doubleprecision function rrqg2qght9s1em4
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght9s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght9s2e1
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = -0.180D3 * t3 + 0.30D2 * t5
      t8 = pi * t7
      t9 = 0.1D1 / t1
      t10 = s ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
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
      t27 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t32 = pi * t9
      t33 = t26 ** 2
      t36 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t37 = t33 * t26
      t40 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t51 = -0.60D2 * lh * t5 + 0.240D3 * zeta3 + 0.120D3 * t3 * lh
      t52 = pi * t51
      t53 = t12 * t27
      t54 = t52 * t53
      t55 = pi * lh
      t64 = 0.1D1 / x3
      t67 = x2 * x3
      t68 = t16 * t20
      t69 = t68 * t22
      t72 = log(0.4D1 * t67 * t69)
      t74 = t72 ** 2
      t77 = t67 * t16
      t78 = -0.1D1 + x2
      t79 = t23 * t78
      t82 = log(-0.4D1 * t77 * t79)
      t84 = t82 ** 2
      t99 = 0.1D1 / x2
      t102 = t11 * t13
      t108 = x2 * t16
      t111 = log(0.4D1 * t108 * t23)
      t112 = t111 ** 2
      t115 = log(-0.4D1 * t108 * t79)
      t116 = t115 ** 2
      t118 = t112 / 0.2D1 - t116 / 0.2D1
      t120 = t11 * t27
      t124 = t116 * t115 / 0.6D1 - t112 * t111 / 0.6D1
      t128 = t11 * t40
      t134 = t8 * t53
      t136 = -t111 + t115
      t141 = rrqg2qgh91J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t146 = log(0.4D1 * t69)
      t147 = t146 ** 2
      t148 = t147 * pi
      t152 = t147 * t146 * pi
      t154 = t146 * pi
      t157 = (0.90D2 * t148 * lh + t52 + 0.15D2 * t152 - t154 * t7) * t9
      t164 = (-0.180D3 * t154 * lh - 0.45D2 * t148 + t8) * t9
      t170 = (0.180D3 * t55 + 0.90D2 * t154) * t9
      t174 = x1 ** 2
      t175 = x3 * t174
      t178 = log(0.4D1 * t175 * t69)
      t180 = t178 ** 2
      t194 = 0.1D1 / x1
      t197 = t174 * t20
      t198 = t16 * t22
      t201 = log(0.4D1 * t197 * t198)
      t206 = t201 ** 2
      t209 = t206 * t201
      t227 = t32 * t11
      t228 = t67 * t174
      t231 = log(0.4D1 * t228 * t69)
      t237 = log(-0.4D1 * t228 * t68 * t22 * t78)
      t241 = t99 * t194
      t245 = x2 * t174
      t250 = log(-0.4D1 * t245 * t20 * t198 * t78)
      t252 = t250 ** 2
      t257 = log(0.4D1 * t245 * t69)
      t259 = t257 ** 2
      t283 = t5 ** 2
      t284 = t3 ** 2
      t290 = t147 ** 2
      t294 = (-0.30D2 * t152 * lh + t148 * t7 / 0.2D1 - t154 * t51 + pi 
     #* (-0.480D3 * lh * zeta3 - t283 - 0.60D2 * t284 + 0.60D2 * t3 * t5
     #) - 0.15D2 / 0.4D1 * t290 * pi) * t9
      t297 = -(t8 * t12 * (t13 - t26 * t27) - 0.90D2 * t32 * t11 * (t33 
     #* t13 / 0.2D1 + t36 - t37 * t27 / 0.6D1 - t26 * t40) + t54 + 0.180
     #D3 * t55 * t12 * (-t26 * t13 + t40 + t33 * t27 / 0.2D1)) * t64 / 0
     #.1440D4 - (-0.90D2 * t32 * t11 * (-t72 * t13 + t74 * t27 / 0.2D1 +
     # t82 * t13 - t84 * t27 / 0.2D1) + 0.180D3 * t55 * t12 * (-t72 * t2
     #7 + t82 * t27)) * t64 * t99 / 0.1440D4 - ((-0.90D2 * t32 * t102 + 
     #0.180D3 * t55 * t53) * t118 - 0.90D2 * t32 * t120 * t124 + (-0.90D
     #2 * t32 * t128 + 0.180D3 * t55 * t12 * t13 + t134) * t136) * t99 /
     # 0.1440D4 + t32 * t11 * t141 / 0.16D2 - t157 * t102 / 0.1440D4 - t
     #164 * t128 / 0.1440D4 - t170 * t11 * t36 / 0.1440D4 - (-0.90D2 * t
     #32 * t11 * (-t178 * t13 + t40 + t180 * t27 / 0.2D1) + 0.180D3 * t5
     #5 * t12 * (t13 - t178 * t27) + t134) * t64 * t194 / 0.720D3 - (t8 
     #* t12 * (-t201 * t27 + t13) - 0.90D2 * t32 * t11 * (t206 * t13 / 0
     #.2D1 + t36 - t209 * t27 / 0.6D1 - t201 * t40) + t54 + 0.180D3 * t5
     #5 * t12 * (-t201 * t13 + t40 + t206 * t27 / 0.2D1)) * t194 / 0.720
     #D3 + t227 * (-t231 * t27 + t237 * t27) * t64 * t241 / 0.8D1 - (-0.
     #90D2 * t32 * t11 * (t250 * t13 - t252 * t27 / 0.2D1 - t257 * t13 +
     # t259 * t27 / 0.2D1) + 0.180D3 * t55 * t12 * (t250 * t27 - t257 * 
     #t27)) * t99 * t194 / 0.720D3 - t294 * t120 / 0.1440D4
      t298 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t297)
      t300 = t2 * x1
      t301 = -0.1D1 + x1
      t302 = t2 * t301
      t304 = 0.1D1 / t14
      t305 = t304 * t22
      t306 = x1 * z
      t307 = -z - x1 + t306
      t308 = 0.1D1 / t307
      t309 = t301 ** 2
      t310 = t308 * t309
      t314 = log(-0.4D1 * t175 * t20 * t305 * t310)
      t315 = t314 ** 2
      t316 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t319 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t320 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t331 = t12 * t316
      t332 = t8 * t331
      t336 = t197 * t304
      t337 = t22 * t308
      t341 = log(-0.4D1 * t336 * t337 * t309)
      t346 = t341 ** 2
      t349 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t350 = t346 * t341
      t369 = t67 * t174 * t22
      t370 = t20 * t304
      t371 = t370 * t310
      t374 = log(-0.4D1 * t369 * t371)
      t388 = log(-0.4D1 * t245 * t22 * t371)
      t389 = t388 ** 2
      t406 = -(-0.90D2 * t32 * t11 * (-t315 * t316 / 0.2D1 - t319 + t314
     # * t320) + 0.180D3 * t55 * t12 * (-t320 + t314 * t316) - t332) * t
     #64 * t194 / 0.720D3 - (t8 * t12 * (-t320 + t341 * t316) - 0.90D2 *
     # t32 * t11 * (-t346 * t320 / 0.2D1 - t349 + t350 * t316 / 0.6D1 + 
     #t341 * t319) - t52 * t331 + 0.180D3 * t55 * t12 * (t341 * t320 - t
     #346 * t316 / 0.2D1 - t319)) * t194 / 0.720D3 - (-0.90D2 * t32 * t1
     #1 * (-t320 + t374 * t316) - 0.180D3 * t55 * t331) * t64 * t241 / 0
     #.720D3 - (-0.90D2 * t32 * t11 * (-t389 * t316 / 0.2D1 + t388 * t32
     #0 - t319) + 0.180D3 * t55 * t12 * (t388 * t316 - t320) - t332) * t
     #99 * t194 / 0.720D3
      t407 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t300, -t302, 0.0D0, t406)
      t409 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t411 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t414 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t424 = t12 * t411
      t425 = t8 * t424
      t435 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t444 = t52 * t424
      t482 = t11 * t411
      t508 = rrqg2qgh92J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t532 = t11 * t409
      t542 = t11 * t414
      t560 = -(-0.90D2 * t32 * t11 * (-t178 * t409 + t180 * t411 / 0.2D1
     # + t414) + 0.180D3 * t55 * t12 * (-t178 * t411 + t409) + t425) * t
     #64 * t194 / 0.720D3 - (t8 * t12 * (t409 - t201 * t411) - 0.90D2 * 
     #t32 * t11 * (-t201 * t414 + t435 + t206 * t409 / 0.2D1 - t209 * t4
     #11 / 0.6D1) + t444 + 0.180D3 * t55 * t12 * (-t201 * t409 + t206 * 
     #t411 / 0.2D1 + t414)) * t194 / 0.720D3 + t227 * (t237 * t411 - t23
     #1 * t411) * t64 * t241 / 0.8D1 - (-0.90D2 * t32 * t11 * (t259 * t4
     #11 / 0.2D1 + t250 * t409 - t252 * t411 / 0.2D1 - t257 * t409) + 0.
     #180D3 * t55 * t12 * (t250 * t411 - t257 * t411)) * t99 * t194 / 0.
     #720D3 - t294 * t482 / 0.1440D4 - (t8 * t12 * (t409 - t26 * t411) -
     # 0.90D2 * t32 * t11 * (t33 * t409 / 0.2D1 - t26 * t414 + t435 - t3
     #7 * t411 / 0.6D1) + t444 + 0.180D3 * t55 * t12 * (t33 * t411 / 0.2
     #D1 + t414 - t26 * t409)) * t64 / 0.1440D4 + t32 * t11 * t508 / 0.1
     #6D2 - (-0.90D2 * t32 * t11 * (-t72 * t409 + t74 * t411 / 0.2D1 + t
     #82 * t409 - t84 * t411 / 0.2D1) + 0.180D3 * t55 * t12 * (-t72 * t4
     #11 + t82 * t411)) * t64 * t99 / 0.1440D4 - ((-0.90D2 * t32 * t532 
     #+ 0.180D3 * t55 * t424) * t118 - 0.90D2 * t32 * t482 * t124 + (-0.
     #90D2 * t32 * t542 + 0.180D3 * t55 * t12 * t409 + t425) * t136) * t
     #99 / 0.1440D4 - t157 * t532 / 0.1440D4 - t164 * t542 / 0.1440D4 - 
     #t170 * t11 * t435 / 0.1440D4
      t561 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t560)
      t563 = t2 * x3
      t564 = -0.1D1 + x3
      t565 = t2 * t564
      t570 = log(-0.4D1 * t175 * t22 * t68 * t564)
      t571 = t570 ** 2
      t572 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t575 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t577 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t587 = t12 * t572
      t597 = log(-0.4D1 * t228 * t23 * t16 * t564)
      t599 = t67 * t197
      t600 = t78 * t564
      t604 = log(0.4D1 * t599 * t198 * t600)
      t614 = log(0.4D1 * t77 * t23 * t600)
      t616 = t614 ** 2
      t619 = t23 * t564
      t622 = log(-0.4D1 * t77 * t619)
      t624 = t622 ** 2
      t643 = log(-0.4D1 * t17 * t619)
      t648 = t643 ** 2
      t652 = t648 * t643
      t655 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t671 = -(-0.90D2 * t32 * t11 * (-t571 * t572 / 0.2D1 + t570 * t575
     # - t577) + 0.180D3 * t55 * t12 * (-t575 + t570 * t572) - t8 * t587
     #) * t64 * t194 / 0.720D3 + t227 * (t597 * t572 - t604 * t572) * t6
     #4 * t241 / 0.8D1 - (-0.90D2 * t32 * t11 * (-t614 * t575 + t616 * t
     #572 / 0.2D1 + t622 * t575 - t624 * t572 / 0.2D1) + 0.180D3 * t55 *
     # t12 * (-t614 * t572 + t622 * t572)) * t64 * t99 / 0.1440D4 - (t8 
     #* t12 * (-t575 + t643 * t572) - 0.90D2 * t32 * t11 * (-t648 * t575
     # / 0.2D1 + t643 * t577 + t652 * t572 / 0.6D1 - t655) - t52 * t587 
     #+ 0.180D3 * t55 * t12 * (t643 * t575 - t577 - t648 * t572 / 0.2D1)
     #) * t64 / 0.1440D4
      t672 = FJET(XB1, XB2, s, 0.0D0, t563, 0.0D0, -t565, 0.0D0, t671)
      t674 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t676 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t679 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t689 = t12 * t676
      t729 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #563, -t565, 0.0D0)
      t747 = -(-0.90D2 * t32 * t11 * (t570 * t674 - t571 * t676 / 0.2D1 
     #- t679) + 0.180D3 * t55 * t12 * (t570 * t676 - t674) - t8 * t689) 
     #* t64 * t194 / 0.720D3 + t227 * (t597 * t676 - t604 * t676) * t64 
     #* t241 / 0.8D1 - (-0.90D2 * t32 * t11 * (-t614 * t674 + t616 * t67
     #6 / 0.2D1 + t622 * t674 - t624 * t676 / 0.2D1) + 0.180D3 * t55 * t
     #12 * (-t614 * t676 + t622 * t676)) * t64 * t99 / 0.1440D4 - (t8 * 
     #t12 * (-t674 + t643 * t676) - 0.90D2 * t32 * t11 * (t643 * t679 + 
     #t652 * t676 / 0.6D1 - t729 - t648 * t674 / 0.2D1) - t52 * t689 + 0
     #.180D3 * t55 * t12 * (-t648 * t676 / 0.2D1 - t679 + t643 * t674)) 
     #* t64 / 0.1440D4
      t748 = FJET(XB1, XB2, s, 0.0D0, -t565, 0.0D0, t563, 0.0D0, t747)
      t750 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t752 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t755 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t765 = t12 * t752
      t766 = t8 * t765
      t777 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t300, 0.
     #0D0, -t302, 0.0D0)
      t820 = -(-0.90D2 * t32 * t11 * (t314 * t750 - t315 * t752 / 0.2D1 
     #- t755) + 0.180D3 * t55 * t12 * (-t750 + t314 * t752) - t766) * t6
     #4 * t194 / 0.720D3 - (t8 * t12 * (t341 * t752 - t750) - 0.90D2 * t
     #32 * t11 * (-t346 * t750 / 0.2D1 + t341 * t755 - t777 + t350 * t75
     #2 / 0.6D1) - t52 * t765 + 0.180D3 * t55 * t12 * (t341 * t750 - t34
     #6 * t752 / 0.2D1 - t755)) * t194 / 0.720D3 - (-0.90D2 * t32 * t11 
     #* (-t750 + t374 * t752) - 0.180D3 * t55 * t765) * t64 * t241 / 0.7
     #20D3 - (-0.90D2 * t32 * t11 * (-t389 * t752 / 0.2D1 - t755 + t388 
     #* t750) + 0.180D3 * t55 * t12 * (-t750 + t388 * t752) - t766) * t9
     #9 * t194 / 0.720D3
      t821 = FJET(XB1, XB2, s, t300, -t302, 0.0D0, 0.0D0, 0.0D0, t820)
      t823 = x3 * x1
      t824 = t2 * t823
      t826 = t1 * t301
      t827 = x3 * s * t826
      t828 = t564 * s
      t829 = t1 * x1
      t830 = t828 * t829
      t831 = t828 * t826
      t838 = log(0.4D1 * t175 * t23 * t304 * t308 * t309 * t564)
      t839 = t838 ** 2
      t840 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t824, -t830, -t
     #827, t831, 0.0D0)
      t843 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, t824, -t830, -t
     #827, t831, 0.0D0)
      t844 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t824, -t830, -t
     #827, t831, 0.0D0)
      t855 = t12 * t840
      t864 = log(0.4D1 * t369 * t370 * t310 * t564)
      t876 = -(-0.90D2 * t32 * t11 * (t839 * t840 / 0.2D1 + t843 - t838 
     #* t844) + 0.180D3 * t55 * t12 * (t844 - t838 * t840) + t8 * t855) 
     #* t64 * t194 / 0.720D3 - (-0.90D2 * t32 * t11 * (t844 - t864 * t84
     #0) + 0.180D3 * t55 * t855) * t64 * t241 / 0.720D3
      t877 = FJET(XB1, XB2, s, t824, -t827, -t830, t831, 0.0D0, t876)
      t879 = x3 * z
      t880 = t823 * z
      t881 = t67 * z
      t882 = t67 * x1
      t883 = t67 * t306
      t884 = cos(t18)
      t889 = Sqrt(-x3 * t78 * t307 * x2 * t564)
      t891 = 0.2D1 * t884 * t889
      t895 = t2 * x1 * (-t879 - t823 + t880 + t881 + t882 - t883 - x2 + 
     #t67 + t891) * t308
      t897 = x2 * x1
      t899 = z + x1 - t306 - x2 * z - t897 + t897 * z - t879 - t823 + t8
     #80 + t881 + t882 - t883 + t67 + t891
      t902 = t2 * x1 * t899 * t308
      t907 = s * t21 * x2 * x1 * t301 * t308
      t909 = t309 * t78
      t914 = log(-0.4D1 * t67 * t336 * t337 * t909 * t564)
      t915 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t895, -t902, -t
     #827, t831, t907)
      t917 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t895, -t902, -t
     #827, t831, t907)
      t925 = -0.90D2 * t32 * t11 * (t914 * t915 - t917) - 0.180D3 * t55 
     #* t12 * t915
      t929 = FJET(XB1, XB2, s, t895, -t827, -t902, t831, t907, -t925 * t
     #64 * t241 / 0.720D3)
      t932 = t64 * t99 * t194
      t936 = t78 * s * t829
      t938 = t2 * t897 * t308
      t939 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t938, -t936, 0
     #.0D0, -t302, t907)
      t944 = log(0.4D1 * t599 * t305 * t310 * t78)
      t945 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t938, -t936, 0
     #.0D0, -t302, t907)
      t951 = t12 * t945
      t961 = log(0.4D1 * t245 * t370 * t337 * t909)
      t962 = t961 ** 2
      t966 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, -t938, -t936, 0
     #.0D0, -t302, t907)
      t981 = -(-0.90D2 * t32 * t11 * (t939 - t944 * t945) + 0.180D3 * t5
     #5 * t951) * t64 * t241 / 0.720D3 - (-0.90D2 * t32 * t11 * (t962 * 
     #t945 / 0.2D1 - t961 * t939 + t966) + 0.180D3 * t55 * t12 * (-t961 
     #* t945 + t939) + t8 * t951) * t99 * t194 / 0.720D3
      t982 = FJET(XB1, XB2, s, -t936, -t302, -t938, 0.0D0, t907, t981)
      t984 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, t824, -t830, -t
     #827, t831, 0.0D0)
      t985 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t824, -t830, -t
     #827, t831, 0.0D0)
      t988 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t824, -t830, -t
     #827, t831, 0.0D0)
      t999 = t12 * t985
      t1015 = -(-0.90D2 * t32 * t11 * (t984 + t839 * t985 / 0.2D1 - t838
     # * t988) + 0.180D3 * t55 * t12 * (t988 - t838 * t985) + t8 * t999)
     # * t64 * t194 / 0.720D3 - (-0.90D2 * t32 * t11 * (-t864 * t985 + t
     #988) + 0.180D3 * t55 * t999) * t64 * t241 / 0.720D3
      t1016 = FJET(XB1, XB2, s, -t830, t831, t824, -t827, 0.0D0, t1015)
      t1018 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t938, -t936, 
     #0.0D0, -t302, t907)
      t1019 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t938, -t936, 
     #0.0D0, -t302, t907)
      t1025 = t12 * t1019
      t1034 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t938, -t936, 
     #0.0D0, -t302, t907)
      t1049 = -(-0.90D2 * t32 * t11 * (t1018 - t944 * t1019) + 0.180D3 *
     # t55 * t1025) * t64 * t241 / 0.720D3 - (-0.90D2 * t32 * t11 * (t96
     #2 * t1019 / 0.2D1 - t961 * t1018 + t1034) + 0.180D3 * t55 * t12 * 
     #(-t961 * t1019 + t1018) + t8 * t1025) * t99 * t194 / 0.720D3
      t1050 = FJET(XB1, XB2, s, -t938, 0.0D0, -t936, -t302, t907, t1049)
      t1052 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t895, -t902, -
     #t827, t831, t907)
      t1054 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t895, -t902, -
     #t827, t831, t907)
      t1062 = -0.90D2 * t32 * t11 * (t914 * t1052 - t1054) - 0.180D3 * t
     #55 * t12 * t1052
      t1066 = FJET(XB1, XB2, s, -t902, t831, t895, -t827, t907, -t1062 *
     # t64 * t241 / 0.720D3)
      rrqg2qght9s2e1 = t298 * t297 + t407 * t406 + t561 * t560 + t672 * 
     #t671 + t748 * t747 + t821 * t820 + t877 * t876 - t929 * t925 * t93
     #2 / 0.720D3 + t982 * t981 + t1016 * t1015 + t1050 * t1049 - t1066 
     #* t1062 * t932 / 0.720D3

      end function



      doubleprecision function rrqg2qght9s2e0
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t22 = log(0.4D1 * t9 * t19)
      t23 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t29 = pi * lh
      t30 = t3 * t6
      t31 = t30 * t23
      t33 = 0.180D3 * t29 * t31
      t35 = 0.1D1 / x3
      t37 = 0.1D1 / x1
      t40 = t4 * t6
      t41 = x2 * t8
      t43 = t12 * t18
      t44 = -0.1D1 + x2
      t48 = log(-0.4D1 * t41 * t15 * t43 * t44)
      t52 = log(0.4D1 * t41 * t19)
      t55 = 0.1D1 / x2
      t60 = t8 * t15
      t63 = log(0.4D1 * t60 * t43)
      t65 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t66 = t63 ** 2
      t78 = lh ** 2
      t80 = pi ** 2
      t82 = -0.180D3 * t78 + 0.30D2 * t80
      t83 = pi * t82
      t84 = t83 * t31
      t88 = x3 * t12
      t89 = t15 * t18
      t92 = log(0.4D1 * t88 * t89)
      t94 = t92 ** 2
      t110 = log(0.4D1 * t19)
      t111 = t110 * pi
      t114 = t110 ** 2
      t115 = t114 * pi
      t118 = (-0.180D3 * t111 * lh - 0.45D2 * t115 + t83) * t3
      t119 = t6 * t7
      t122 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t140 = (0.90D2 * t115 * lh + pi * (-0.60D2 * lh * t80 + 0.240D3 * 
     #zeta3 + 0.120D3 * t78 * lh) + 0.15D2 * t114 * t110 * pi - t111 * t
     #82) * t3
      t141 = t6 * t23
      t147 = (0.180D3 * t29 + 0.90D2 * t111) * t3
      t151 = x2 * x3
      t154 = log(0.4D1 * t151 * t19)
      t156 = t151 * t12
      t157 = t89 * t44
      t160 = log(-0.4D1 * t156 * t157)
      t167 = x2 * t12
      t170 = log(0.4D1 * t167 * t89)
      t171 = t170 ** 2
      t174 = log(-0.4D1 * t167 * t157)
      t175 = t174 ** 2
      t177 = t171 / 0.2D1 - t175 / 0.2D1
      t184 = -t170 + t174
      t189 = -(-0.90D2 * t4 * t6 * (t7 - t22 * t23) + t33) * t35 * t37 /
     # 0.720D3 + t40 * (t48 * t23 - t52 * t23) * t55 * t37 / 0.8D1 - (-0
     #.90D2 * t4 * t6 * (-t63 * t7 + t65 + t66 * t23 / 0.2D1) + 0.180D3 
     #* t29 * t30 * (-t63 * t23 + t7) + t84) * t37 / 0.720D3 - (-0.90D2 
     #* t4 * t6 * (-t92 * t7 + t65 + t94 * t23 / 0.2D1) + 0.180D3 * t29 
     #* t30 * (t7 - t92 * t23) + t84) * t35 / 0.1440D4 - t118 * t119 / 0
     #.1440D4 + t4 * t6 * t122 / 0.16D2 - t140 * t141 / 0.1440D4 - t147 
     #* t6 * t65 / 0.1440D4 + t40 * (-t154 * t23 + t160 * t23) * t35 * t
     #55 / 0.16D2 - (-0.90D2 * t4 * t141 * t177 + (-0.90D2 * t4 * t119 +
     # t33) * t184) * t55 / 0.1440D4
      t190 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t189)
      t192 = t2 * x1
      t193 = -0.1D1 + x1
      t194 = t2 * t193
      t195 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, 0.
     #0D0, -t194, 0.0D0)
      t197 = 0.1D1 / t10
      t199 = x1 * z
      t200 = -z - x1 + t199
      t201 = 0.1D1 / t200
      t202 = t193 ** 2
      t203 = t201 * t202
      t207 = log(-0.4D1 * t9 * t15 * t197 * t18 * t203)
      t208 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, 0.
     #0D0, -t194, 0.0D0)
      t214 = t30 * t208
      t216 = 0.180D3 * t29 * t214
      t222 = t55 * t37
      t227 = t15 * t197
      t231 = log(-0.4D1 * t41 * t18 * t227 * t203)
      t242 = t18 * t201
      t246 = log(-0.4D1 * t60 * t197 * t242 * t202)
      t248 = t246 ** 2
      t251 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, 0.
     #0D0, -t194, 0.0D0)
      t265 = -(-0.90D2 * t4 * t6 * (-t195 + t207 * t208) - t216) * t35 *
     # t37 / 0.720D3 - t40 * t208 * t35 * t222 / 0.8D1 - (-0.90D2 * t4 *
     # t6 * (t231 * t208 - t195) - t216) * t55 * t37 / 0.720D3 - (-0.90D
     #2 * t4 * t6 * (t246 * t195 - t248 * t208 / 0.2D1 - t251) + 0.180D3
     # * t29 * t30 * (-t195 + t246 * t208) - t83 * t214) * t37 / 0.720D3
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t192, -t194, 0.0D0, t265)
      t268 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t270 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t275 = t30 * t268
      t277 = 0.180D3 * t29 * t275
      t292 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t302 = t83 * t275
      t306 = t6 * t268
      t309 = t6 * t270
      t312 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t351 = -(-0.90D2 * t4 * t6 * (-t22 * t268 + t270) + t277) * t35 * 
     #t37 / 0.720D3 + t40 * (t48 * t268 - t52 * t268) * t55 * t37 / 0.8D
     #1 - (-0.90D2 * t4 * t6 * (-t63 * t270 + t66 * t268 / 0.2D1 + t292)
     # + 0.180D3 * t29 * t30 * (t270 - t63 * t268) + t302) * t37 / 0.720
     #D3 - t140 * t306 / 0.1440D4 - t118 * t309 / 0.1440D4 + t4 * t6 * t
     #312 / 0.16D2 - t147 * t6 * t292 / 0.1440D4 - (-0.90D2 * t4 * t6 * 
     #(t94 * t268 / 0.2D1 + t292 - t92 * t270) + 0.180D3 * t29 * t30 * (
     #t270 - t92 * t268) + t302) * t35 / 0.1440D4 + t40 * (-t154 * t268 
     #+ t160 * t268) * t35 * t55 / 0.16D2 - (-0.90D2 * t4 * t306 * t177 
     #+ (-0.90D2 * t4 * t309 + t277) * t184) * t55 / 0.1440D4
      t352 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t351)
      t354 = t2 * x3
      t355 = -0.1D1 + x3
      t356 = t2 * t355
      t357 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #354, -t356, 0.0D0)
      t362 = log(-0.4D1 * t9 * t18 * t16 * t355)
      t363 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #354, -t356, 0.0D0)
      t369 = t30 * t363
      t376 = t89 * t355
      t379 = log(-0.4D1 * t88 * t376)
      t381 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #354, -t356, 0.0D0)
      t382 = t379 ** 2
      t402 = log(0.4D1 * t156 * t89 * t355 * t44)
      t406 = log(-0.4D1 * t156 * t376)
      t413 = -(-0.90D2 * t4 * t6 * (-t357 + t362 * t363) - 0.180D3 * t29
     # * t369) * t35 * t37 / 0.720D3 - (-0.90D2 * t4 * t6 * (t379 * t357
     # - t381 - t382 * t363 / 0.2D1) + 0.180D3 * t29 * t30 * (-t357 + t3
     #79 * t363) - t83 * t369) * t35 / 0.1440D4 + t40 * (-t402 * t363 + 
     #t406 * t363) * t35 * t55 / 0.16D2
      t414 = FJET(XB1, XB2, s, 0.0D0, t354, 0.0D0, -t356, 0.0D0, t413)
      t416 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #354, -t356, 0.0D0)
      t418 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #354, -t356, 0.0D0)
      t423 = t30 * t416
      t432 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #354, -t356, 0.0D0)
      t454 = -(-0.90D2 * t4 * t6 * (t362 * t416 - t418) - 0.180D3 * t29 
     #* t423) * t35 * t37 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t382 * t416
     # / 0.2D1 - t432 + t379 * t418) + 0.180D3 * t29 * t30 * (-t418 + t3
     #79 * t416) - t83 * t423) * t35 / 0.1440D4 + t40 * (-t402 * t416 + 
     #t406 * t416) * t35 * t55 / 0.16D2
      t455 = FJET(XB1, XB2, s, 0.0D0, -t356, 0.0D0, t354, 0.0D0, t454)
      t457 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, 0.
     #0D0, -t194, 0.0D0)
      t458 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, 0.
     #0D0, -t194, 0.0D0)
      t464 = t30 * t458
      t466 = 0.180D3 * t29 * t464
      t487 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, 0.
     #0D0, -t194, 0.0D0)
      t501 = -(-0.90D2 * t4 * t6 * (-t457 + t207 * t458) - t466) * t35 *
     # t37 / 0.720D3 - t40 * t458 * t35 * t222 / 0.8D1 - (-0.90D2 * t4 *
     # t6 * (-t457 + t231 * t458) - t466) * t55 * t37 / 0.720D3 - (-0.90
     #D2 * t4 * t6 * (t246 * t457 - t248 * t458 / 0.2D1 - t487) + 0.180D
     #3 * t29 * t30 * (t246 * t458 - t457) - t83 * t464) * t37 / 0.720D3
      t502 = FJET(XB1, XB2, s, t192, -t194, 0.0D0, 0.0D0, 0.0D0, t501)
      t504 = x3 * x1
      t505 = t2 * t504
      t507 = t1 * t193
      t508 = x3 * s * t507
      t509 = t355 * s
      t510 = t1 * x1
      t511 = t509 * t510
      t512 = t509 * t507
      t513 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, t505, -t511, -t
     #508, t512, 0.0D0)
      t520 = log(0.4D1 * t9 * t89 * t197 * t201 * t202 * t355)
      t521 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t505, -t511, -t
     #508, t512, 0.0D0)
      t538 = -(-0.90D2 * t4 * t6 * (t513 - t520 * t521) + 0.180D3 * t29 
     #* t30 * t521) * t35 * t37 / 0.720D3 + t40 * t521 * t35 * t222 / 0.
     #8D1
      t539 = FJET(XB1, XB2, s, t505, -t508, -t511, t512, 0.0D0, t538)
      t541 = x3 * z
      t542 = t504 * z
      t543 = t151 * z
      t544 = t151 * x1
      t545 = t151 * t199
      t546 = cos(t13)
      t551 = Sqrt(-x3 * t44 * t200 * x2 * t355)
      t553 = 0.2D1 * t546 * t551
      t557 = t2 * x1 * (-t541 - t504 + t542 + t543 + t544 - t545 - x2 + 
     #t151 + t553) * t201
      t559 = x2 * x1
      t561 = z + x1 - t199 - x2 * z - t559 + t559 * z - t541 - t504 + t5
     #42 + t543 + t544 - t545 + t151 + t553
      t564 = t2 * x1 * t561 * t201
      t569 = s * t17 * x2 * x1 * t193 * t201
      t570 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t557, -t564, -t
     #508, t512, t569)
      t572 = t570 * t35 * t222
      t575 = FJET(XB1, XB2, s, t557, -t508, -t564, t512, t569, -t40 * t5
     #72 / 0.8D1)
      t581 = t44 * s * t510
      t583 = t2 * t559 * t201
      t584 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t583, -t581, 0
     #.0D0, -t194, t569)
      t594 = log(0.4D1 * t41 * t227 * t242 * t202 * t44)
      t596 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, -t583, -t581, 0
     #.0D0, -t194, t569)
      t608 = t40 * t584 * t35 * t222 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t5
     #94 * t584 + t596) + 0.180D3 * t29 * t30 * t584) * t55 * t37 / 0.72
     #0D3
      t609 = FJET(XB1, XB2, s, -t581, -t194, -t583, 0.0D0, t569, t608)
      t611 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, t505, -t511, -t
     #508, t512, 0.0D0)
      t612 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t505, -t511, -t
     #508, t512, 0.0D0)
      t629 = -(-0.90D2 * t4 * t6 * (t611 - t520 * t612) + 0.180D3 * t29 
     #* t30 * t612) * t35 * t37 / 0.720D3 + t40 * t612 * t35 * t222 / 0.
     #8D1
      t630 = FJET(XB1, XB2, s, -t511, t512, t505, -t508, 0.0D0, t629)
      t632 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t583, -t581, 0
     #.0D0, -t194, t569)
      t638 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t583, -t581, 0
     #.0D0, -t194, t569)
      t650 = t40 * t632 * t35 * t222 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t5
     #94 * t632 + t638) + 0.180D3 * t29 * t30 * t632) * t55 * t37 / 0.72
     #0D3
      t651 = FJET(XB1, XB2, s, -t583, 0.0D0, -t581, -t194, t569, t650)
      t653 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t557, -t564, -t
     #508, t512, t569)
      t655 = t653 * t35 * t222
      t658 = FJET(XB1, XB2, s, -t564, t512, t557, -t508, t569, -t40 * t6
     #55 / 0.8D1)
      rrqg2qght9s2e0 = t190 * t189 + t266 * t265 + t352 * t351 + t414 * 
     #t413 + t455 * t454 + t502 * t501 + t539 * t538 - t575 * pi * t30 *
     # t572 / 0.8D1 + t609 * t608 + t630 * t629 + t651 * t650 - t658 * p
     #i * t30 * t655 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s2em1
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
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
      t21 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
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
      t59 = log(0.4D1 * t56 * t17)
      t60 = -0.1D1 + x2
      t64 = log(-0.4D1 * t56 * t17 * t60)
      t65 = -t59 + t64
      t67 = 0.1D1 / x2
      t71 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t79 = log(0.4D1 * t10 * t14 * t16)
      t80 = t79 * pi
      t83 = (0.180D3 * t27 + 0.90D2 * t80) * t3
      t89 = t79 ** 2
      t92 = lh ** 2
      t94 = pi ** 2
      t99 = (-0.180D3 * lh * t80 - 0.45D2 * t89 * pi + pi * (-0.180D3 * 
     #t92 + 0.30D2 * t94)) * t3
      t103 = -(-0.90D2 * t4 * t6 * (t7 - t20 * t21) + t31) * t33 / 0.144
     #0D4 - (-0.90D2 * t4 * t6 * (-t41 * t21 + t7) + t31) * t48 / 0.720D
     #3 + t51 * t21 * t33 * t48 / 0.8D1 + t51 * t21 * t65 * t67 / 0.16D2
     # + t4 * t6 * t71 / 0.16D2 - t83 * t6 * t7 / 0.1440D4 - t99 * t6 * 
     #t21 / 0.1440D4
      t104 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t103)
      t106 = t2 * x1
      t107 = -0.1D1 + x1
      t108 = t2 * t107
      t109 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t106, 0.
     #0D0, -t108, 0.0D0)
      t114 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t106, 0.
     #0D0, -t108, 0.0D0)
      t119 = 0.1D1 / (-z - x1 + x1 * z)
      t121 = t107 ** 2
      t125 = log(-0.4D1 * t37 / t8 * t16 * t119 * t121)
      t141 = -t51 * t109 * t67 * t48 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t1
     #14 + t125 * t109) - 0.180D3 * t27 * t28 * t109) * t48 / 0.720D3 - 
     #t51 * t109 * t33 * t48 / 0.8D1
      t142 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t106, -t108, 0.0D0, t141)
      t144 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t145 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t153 = 0.180D3 * t27 * t28 * t145
      t176 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t183 = -(-0.90D2 * t4 * t6 * (t144 - t20 * t145) + t153) * t33 / 0
     #.1440D4 - (-0.90D2 * t4 * t6 * (t144 - t41 * t145) + t153) * t48 /
     # 0.720D3 + t51 * t145 * t33 * t48 / 0.8D1 - t99 * t6 * t145 / 0.14
     #40D4 + t51 * t145 * t65 * t67 / 0.16D2 + t4 * t6 * t176 / 0.16D2 -
     # t83 * t6 * t144 / 0.1440D4
      t184 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t183)
      t186 = t2 * x3
      t187 = -0.1D1 + x3
      t188 = t2 * t187
      t189 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #186, -t188, 0.0D0)
      t193 = log(-0.4D1 * t11 * t17 * t187)
      t194 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #186, -t188, 0.0D0)
      t210 = -(-0.90D2 * t4 * t6 * (-t189 + t193 * t194) - 0.180D3 * t27
     # * t28 * t194) * t33 / 0.1440D4 - t51 * t194 * t33 * t48 / 0.8D1
      t211 = FJET(XB1, XB2, s, 0.0D0, t186, 0.0D0, -t188, 0.0D0, t210)
      t213 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #186, -t188, 0.0D0)
      t214 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #186, -t188, 0.0D0)
      t230 = -(-0.90D2 * t4 * t6 * (-t213 + t193 * t214) - 0.180D3 * t27
     # * t28 * t214) * t33 / 0.1440D4 - t51 * t214 * t33 * t48 / 0.8D1
      t231 = FJET(XB1, XB2, s, 0.0D0, -t188, 0.0D0, t186, 0.0D0, t230)
      t233 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t106, 0.
     #0D0, -t108, 0.0D0)
      t239 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t106, 0.
     #0D0, -t108, 0.0D0)
      t254 = -t51 * t233 * t67 * t48 / 0.8D1 - (-0.90D2 * t4 * t6 * (t12
     #5 * t233 - t239) - 0.180D3 * t27 * t28 * t233) * t48 / 0.720D3 - t
     #51 * t233 * t33 * t48 / 0.8D1
      t255 = FJET(XB1, XB2, s, t106, -t108, 0.0D0, 0.0D0, 0.0D0, t254)
      t258 = t2 * x1 * x3
      t260 = t1 * t107
      t261 = x3 * s * t260
      t262 = t187 * s
      t263 = t1 * x1
      t264 = t262 * t263
      t265 = t262 * t260
      t266 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t264, -t
     #261, t265, 0.0D0)
      t271 = FJET(XB1, XB2, s, t258, -t261, -t264, t265, 0.0D0, t51 * t2
     #66 * t33 * t48 / 0.8D1)
      t275 = t33 * t48
      t280 = t60 * s * t263
      t283 = t2 * x1 * x2 * t119
      t288 = s * t15 * x2 * x1 * t107 * t119
      t289 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, -t283, -t280, 0
     #.0D0, -t108, t288)
      t294 = FJET(XB1, XB2, s, -t280, -t108, -t283, 0.0D0, t288, t51 * t
     #289 * t67 * t48 / 0.8D1)
      t298 = t67 * t48
      t302 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, t258, -t264, -t
     #261, t265, 0.0D0)
      t307 = FJET(XB1, XB2, s, -t264, t265, t258, -t261, 0.0D0, t51 * t3
     #02 * t33 * t48 / 0.8D1)
      t314 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t283, -t280, 0
     #.0D0, -t108, t288)
      t319 = FJET(XB1, XB2, s, -t283, 0.0D0, -t280, -t108, t288, t51 * t
     #314 * t67 * t48 / 0.8D1)
      rrqg2qght9s2em1 = t104 * t103 + t142 * t141 + t184 * t183 + t211 *
     # t210 + t231 * t230 + t255 * t254 + t271 * pi * t3 * t6 * t266 * t
     #275 / 0.8D1 + t294 * pi * t3 * t6 * t289 * t298 / 0.8D1 + t307 * p
     #i * t3 * t6 * t302 * t275 / 0.8D1 + t319 * pi * t3 * t6 * t314 * t
     #298 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s2em2
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x3
      t17 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t23 = z ** 2
      t27 = Sin(x4 * pi)
      t28 = t27 ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 / t23 / z * t28 * t31)
      t38 = (0.180D3 * pi * lh + 0.90D2 * t34 * pi) * t3
      t41 = t4 * t8 * t9 / 0.8D1 + t4 * t8 * t13 / 0.16D2 + t4 * t6 * t1
     #7 / 0.16D2 - t38 * t8 / 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = t2 * x1
      t46 = t2 * (-0.1D1 + x1)
      t47 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t44, 0.0D
     #0, -t46, 0.0D0)
      t49 = t6 * t47 * t9
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t44, -t46, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t57 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t58 = t6 * t57
      t65 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t71 = t4 * t58 * t9 / 0.8D1 + t4 * t58 * t13 / 0.16D2 + t4 * t6 * 
     #t65 / 0.16D2 - t38 * t58 / 0.1440D4
      t72 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t71)
      t74 = t2 * x3
      t76 = t2 * (-0.1D1 + x3)
      t77 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t7
     #4, -t76, 0.0D0)
      t79 = t6 * t77 * t13
      t82 = FJET(XB1, XB2, s, 0.0D0, t74, 0.0D0, -t76, 0.0D0, -t4 * t79 
     #/ 0.16D2)
      t87 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t44, 0.0D
     #0, -t46, 0.0D0)
      t89 = t6 * t87 * t9
      t92 = FJET(XB1, XB2, s, t44, -t46, 0.0D0, 0.0D0, 0.0D0, -t4 * t89 
     #/ 0.8D1)
      t97 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t7
     #4, -t76, 0.0D0)
      t99 = t6 * t97 * t13
      t102 = FJET(XB1, XB2, s, 0.0D0, -t76, 0.0D0, t74, 0.0D0, -t4 * t99
     # / 0.16D2)
      rrqg2qght9s2em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 + t72 * 
     #t71 - t82 * pi * t3 * t79 / 0.16D2 - t92 * pi * t3 * t89 / 0.8D1 -
     # t102 * pi * t3 * t99 / 0.16D2

      end function



      doubleprecision function rrqg2qght9s2em3
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

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
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t6 * 
     #t7 / 0.16D2)
      t13 = t3 * t6
      t16 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t6 * 
     #t16 / 0.16D2)
      rrqg2qght9s2em3 = t11 * pi * t13 * t7 / 0.16D2 + t20 * pi * t13 * 
     #t16 / 0.16D2

      end function



      doubleprecision function rrqg2qght9s2em4
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
      doubleprecision rrqg2qgh91J1
      doubleprecision rrqg2qgh91J2
      doubleprecision rrqg2qgh91J3
      doubleprecision rrqg2qgh91J4
      doubleprecision rrqg2qgh91J5
      doubleprecision rrqg2qgh91J6
      doubleprecision rrqg2qgh91J7
      doubleprecision rrqg2qgh92J1
      doubleprecision rrqg2qgh92J2
      doubleprecision rrqg2qgh92J3
      doubleprecision rrqg2qgh92J4
      doubleprecision rrqg2qgh92J5
      doubleprecision rrqg2qgh92J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght9s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh91J1
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
      rrqg2qgh91J1 = -0.4D1 / 0.9D1 * S24 * wd * (S13 * t2 + t4 * S13) /
     # (S12 + S13 + S23) / S12 / z / pi

      end function
  
   
 

      doubleprecision function rrqg2qgh91J2
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
      rrqg2qgh91J2 = (-0.4D1 / 0.9D1 * S24 * (t2 + t4) - 0.4D1 / 0.9D1 *
     # S24 * (-t4 + 0.2D1 * t3 * S24 - t2)) / (S12 + S13 + S23) / S12 / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh91J3
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
      rrqg2qgh91J3 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) + (-0.4D1 
     #/ 0.9D1 * S24 * (t7 + t9) - 0.8D1 / 0.9D1 * S24 * (-t9 + 0.2D1 * t
     #8 * S24 - t7)) / (S12 + S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh91J4
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
      rrqg2qgh91J4 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) + (-0.4D1 
     #/ 0.9D1 * S24 * (t7 + t9) - 0.8D1 / 0.9D1 * S24 * (-t9 + 0.2D1 * t
     #8 * S24 - t7)) / (S12 + S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh91J5
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
      rrqg2qgh91J5 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) + (-0.4D1 
     #/ 0.9D1 * S24 * (t7 + t9) - 0.8D1 / 0.9D1 * S24 * (-t9 + 0.2D1 * t
     #8 * S24 - t7)) / (S12 + S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh91J6
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
      rrqg2qgh91J6 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) - 0.8D1 / 
     #0.9D1 * S24 * (-S13 * t6 + 0.2D1 * t6 * S24 - S13 * t10) / (S12 + 
     #S13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh91J7
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
      rrqg2qgh91J7 = (-0.4D1 / 0.9D1 * S24 * (S24 * S14 + t2) - 0.4D1 / 
     #0.9D1 * S24 * (-t5 * S13 + 0.2D1 * t5 * S24 - S13 * t9) / (S12 + S
     #13 + S23)) / S12 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh92J1
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
      t5 = S13 ** 2
      t6 = S24 ** 2
      rrqg2qgh92J1 = (-0.4D1 * S24 * S12 - 0.8D1 * S13 * S24 - 0.4D1 / 0
     #.9D1 * S24 * (0.9D1 * t5 + 0.9D1 * t6) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh92J2
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
      t8 = S13 ** 2
      t9 = S24 ** 2
      rrqg2qgh92J2 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.27D2 * S13
     # - 0.18D2 * S24) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.27D2 * t1 - 0.9D1 * t9
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh92J3
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
      t8 = S13 ** 2
      t9 = S24 ** 2
      rrqg2qgh92J3 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.27D2 * S13
     # - 0.18D2 * S24) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.27D2 * t1 - 0.9D1 * t9
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh92J4
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
      t8 = S13 ** 2
      t9 = S24 ** 2
      rrqg2qgh92J4 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.27D2 * S13
     # - 0.18D2 * S24) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.27D2 * t1 - 0.9D1 * t9
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh92J5
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
      t8 = S13 ** 2
      t9 = S24 ** 2
      rrqg2qgh92J5 = (-0.8D1 * t1 - 0.4D1 / 0.9D1 * S24 * (-0.27D2 * S13
     # - 0.18D2 * S24) + (-0.4D1 / 0.9D1 * S24 * (0.9D1 * t8 + 0.9D1 * t
     #9) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.27D2 * t1 - 0.9D1 * t9
     #)) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh92J6
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
      t8 = S13 ** 2
      t12 = S24 ** 2
      rrqg2qgh92J6 = (0.4D1 * S24 * S12 - 0.4D1 / 0.9D1 * S24 * (-0.27D2
     # * S13 - 0.18D2 * S24) - 0.4D1 / 0.9D1 * S24 * (-0.9D1 * t8 - 0.27
     #D2 * S13 * S24 - 0.9D1 * t12) / S12) / pi * wd / z

      end function
  
 