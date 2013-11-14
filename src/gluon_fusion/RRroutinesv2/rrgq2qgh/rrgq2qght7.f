  
      subroutine rrgq2qght7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh71J1  
      doubleprecision rrgq2qgh71J2  
      doubleprecision rrgq2qgh71J3  
      doubleprecision rrgq2qgh71J4  
      doubleprecision rrgq2qgh71J5  
      doubleprecision rrgq2qgh71J6  
      doubleprecision rrgq2qgh72J1  
      doubleprecision rrgq2qgh72J2  
      doubleprecision rrgq2qgh72J3  
      doubleprecision rrgq2qgh72J4  
      doubleprecision rrgq2qgh72J5  
      doubleprecision rrgq2qgh72J6  
      doubleprecision rrgq2qgh73J1  
      doubleprecision rrgq2qgh73J2  
      doubleprecision rrgq2qgh73J3  
      doubleprecision rrgq2qgh73J4  
      doubleprecision rrgq2qgh73J5  
      doubleprecision rrgq2qgh73J6  
      doubleprecision rrgq2qgh74J1  
      doubleprecision rrgq2qgh74J2  
      doubleprecision rrgq2qgh74J3  
      doubleprecision rrgq2qgh74J4  
      doubleprecision rrgq2qgh74J5  
      doubleprecision rrgq2qgh74J6  
      doubleprecision rrgq2qght7s1e1  
      doubleprecision rrgq2qght7s1e0  
      doubleprecision rrgq2qght7s1em1  
      doubleprecision rrgq2qght7s1em2  
      doubleprecision rrgq2qght7s1em3  
      doubleprecision rrgq2qght7s1em4  
      doubleprecision rrgq2qght7s2e1  
      doubleprecision rrgq2qght7s2e0  
      doubleprecision rrgq2qght7s2em1  
      doubleprecision rrgq2qght7s2em2  
      doubleprecision rrgq2qght7s2em3  
      doubleprecision rrgq2qght7s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght7s1e1
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * pi
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t9 = t1 ** 2
      t10 = t9 ** 2
      t11 = t8 * t10
      t13 = log(0.4D1 * t11)
      t14 = t13 ** 2
      t15 = t14 * pi
      t18 = pi ** 2
      t22 = lh ** 2
      t25 = 0.60D2 * lh * t18 - 0.240D3 * zeta3 - 0.120D3 * t22 * lh
      t26 = pi * t25
      t28 = t14 * t13 * pi
      t30 = t13 * pi
      t33 = 0.180D3 * t22 - 0.30D2 * t18
      t36 = 0.1D1 / t1
      t37 = (-0.90D2 * t15 * lh + t26 - 0.15D2 * t28 - t30 * t33) * t36
      t38 = s ** 2
      t40 = 0.1D1 / t38 / s
      t41 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t42 = t40 * t41
      t48 = pi * t33
      t50 = (0.180D3 * t30 * lh + 0.45D2 * t15 + t48) * t36
      t51 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t52 = t40 * t51
      t55 = pi * lh
      t59 = (-0.180D3 * t55 - 0.90D2 * t30) * t36
      t60 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = t40 * t60
      t64 = pi * t36
      t65 = rrgq2qgh73J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t74 = t18 ** 2
      t75 = t22 ** 2
      t83 = t14 ** 2
      t87 = (0.30D2 * t28 * lh + t15 * t33 / 0.2D1 - t30 * t25 + pi * (t
     #74 + 0.60D2 * t75 + 0.480D3 * lh * zeta3 - 0.60D2 * t22 * t18) + 0
     #.15D2 / 0.4D1 * t83 * pi) * t36
      t88 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t89 = t40 * t88
      t92 = x1 ** 2
      t93 = x3 * t92
      t96 = log(0.4D1 * t93 * t11)
      t98 = t96 ** 2
      t101 = t93 * t7
      t102 = t4 * t10
      t103 = -0.1D1 + x3
      t104 = 0.1D1 / t103
      t105 = t102 * t104
      t108 = log(-0.4D1 * t101 * t105)
      t110 = t108 ** 2
      t114 = cos(t5)
      t116 = Sqrt(-x3 * t103)
      t120 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t114 * t116)
      t126 = t36 * t40
      t136 = t88 + t88 * t120
      t140 = 0.1D1 / x3
      t142 = 0.1D1 / x1
      t145 = t92 * t7
      t148 = log(0.4D1 * t145 * t102)
      t153 = t148 ** 2
      t156 = t153 * t148
      t164 = t126 * t88
      t165 = t26 * t164
      t176 = x2 ** 2
      t177 = x3 * t176
      t178 = t177 * t92
      t183 = log(-0.4D1 * t178 * t8 * t10 * t104)
      t189 = log(0.4D1 * t178 * t11)
      t196 = -t126 * t136
      t201 = 0.1D1 / x2
      t202 = t201 * t142
      t205 = t176 * t92
      t208 = log(0.4D1 * t205 * t11)
      t209 = t208 ** 2
      t222 = t48 * t164
      t227 = t126 * t41
      t233 = x3 * t7
      t236 = log(-0.4D1 * t233 * t105)
      t240 = log(0.4D1 * t233 * t102)
      t241 = t236 * t120 + t240
      t243 = t240 ** 2
      t245 = t236 ** 2
      t249 = t243 * t240 / 0.6D1 + t245 * t236 * t120 / 0.6D1
      t260 = -t120 - 0.1D1
      t269 = -t245 * t120 / 0.2D1 - t243 / 0.2D1
      t274 = t177 * t7
      t277 = log(-0.4D1 * t274 * t105)
      t279 = t277 ** 2
      t286 = log(0.4D1 * t177 * t11)
      t288 = t286 ** 2
      t308 = t176 * t7
      t311 = log(0.4D1 * t308 * t102)
      t316 = t311 ** 2
      t319 = t316 * t311
      t337 = t37 * t42 / 0.2880D4 + t50 * t52 / 0.2880D4 + t59 * t61 / 0
     #.2880D4 + t64 * t40 * t65 / 0.32D2 + t87 * t89 / 0.2880D4 + (0.90D
     #2 * t64 * t40 * (t51 - t96 * t41 + t98 * t88 / 0.2D1 - (t108 * t41
     # - t51 - t110 * t88 / 0.2D1) * t120) - 0.180D3 * t55 * t126 * (-t9
     #6 * t88 + t41 - (-t41 + t108 * t88) * t120) + t48 * t126 * t136) *
     # t140 * t142 / 0.1440D4 - (t48 * t126 * (-t41 + t148 * t88) + 0.90
     #D2 * t64 * t40 * (-t153 * t41 / 0.2D1 + t156 * t88 / 0.6D1 - t60 +
     # t148 * t51) - t165 - 0.180D3 * t55 * t126 * (-t51 - t153 * t88 / 
     #0.2D1 + t148 * t41)) * t142 / 0.1440D4 - (0.90D2 * t64 * t40 * ((-
     #t41 + t183 * t88) * t120 + t189 * t88 - t41) - 0.180D3 * t55 * t19
     #6) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t209 * t88 / 0
     #.2D1 - t208 * t41 + t51) - 0.180D3 * t55 * t126 * (-t208 * t88 + t
     #41) + t222) * t201 * t142 / 0.720D3 + ((0.180D3 * t55 * t227 - t22
     #2 - 0.90D2 * t64 * t52) * t241 - 0.90D2 * t64 * t89 * t249 + (-t48
     # * t227 - 0.90D2 * t64 * t61 - t165 + 0.180D3 * t55 * t126 * t51) 
     #* t260 + (-0.90D2 * t64 * t42 + 0.180D3 * t55 * t164) * t269) * t1
     #40 / 0.2880D4 - (0.90D2 * t64 * t40 * ((t277 * t41 - t51 - t279 * 
     #t88 / 0.2D1) * t120 + t286 * t41 - t51 - t288 * t88 / 0.2D1) - 0.1
     #80D3 * t55 * t126 * ((-t41 + t277 * t88) * t120 - t41 + t286 * t88
     #) + t48 * t196) * t140 * t201 / 0.1440D4 + (t48 * t126 * (-t311 * 
     #t88 + t41) + 0.90D2 * t64 * t40 * (t316 * t41 / 0.2D1 - t319 * t88
     # / 0.6D1 + t60 - t311 * t51) + t165 - 0.180D3 * t55 * t126 * (t316
     # * t88 / 0.2D1 - t311 * t41 + t51)) * t201 / 0.1440D4
      t338 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t337)
      t340 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t342 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t346 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t364 = -t342 - t342 * t120
      t365 = t126 * t364
      t378 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t385 = t126 * t342
      t386 = t26 * t385
      t397 = t40 * t340
      t400 = t40 * t346
      t403 = t40 * t378
      t406 = rrgq2qgh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t410 = t40 * t342
      t413 = t126 * t340
      t416 = t48 * t385
      t521 = -(0.90D2 * t64 * t40 * (t286 * t340 + (-t279 * t342 / 0.2D1
     # + t277 * t340 - t346) * t120 - t288 * t342 / 0.2D1 - t346) - 0.18
     #0D3 * t55 * t126 * (-t340 + (-t340 + t277 * t342) * t120 + t286 * 
     #t342) + t48 * t365) * t140 * t201 / 0.1440D4 + (t48 * t126 * (t340
     # - t311 * t342) + 0.90D2 * t64 * t40 * (t316 * t340 / 0.2D1 - t311
     # * t346 + t378 - t319 * t342 / 0.6D1) + t386 - 0.180D3 * t55 * t12
     #6 * (t346 + t316 * t342 / 0.2D1 - t311 * t340)) * t201 / 0.1440D4 
     #+ t37 * t397 / 0.2880D4 + t50 * t400 / 0.2880D4 + t59 * t403 / 0.2
     #880D4 + t64 * t40 * t406 / 0.32D2 + t87 * t410 / 0.2880D4 + ((0.18
     #0D3 * t55 * t413 - t416 - 0.90D2 * t64 * t400) * t241 - 0.90D2 * t
     #64 * t410 * t249 + (-t48 * t413 - 0.90D2 * t64 * t403 - t386 + 0.1
     #80D3 * t55 * t126 * t346) * t260 + (-0.90D2 * t64 * t397 + 0.180D3
     # * t55 * t385) * t269) * t140 / 0.2880D4 + (0.90D2 * t64 * t40 * (
     #t346 + t98 * t342 / 0.2D1 - (-t110 * t342 / 0.2D1 + t108 * t340 - 
     #t346) * t120 - t96 * t340) - 0.180D3 * t55 * t126 * (t340 - t96 * 
     #t342 - (-t340 + t108 * t342) * t120) - t48 * t126 * t364) * t140 *
     # t142 / 0.1440D4 - (t48 * t126 * (-t340 + t148 * t342) + 0.90D2 * 
     #t64 * t40 * (-t153 * t340 / 0.2D1 + t148 * t346 - t378 + t156 * t3
     #42 / 0.6D1) - t386 - 0.180D3 * t55 * t126 * (t148 * t340 - t346 - 
     #t153 * t342 / 0.2D1)) * t142 / 0.1440D4 - (0.90D2 * t64 * t40 * (-
     #t340 + (-t340 + t183 * t342) * t120 + t189 * t342) - 0.180D3 * t55
     # * t365) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t346 + t
     #209 * t342 / 0.2D1 - t208 * t340) - 0.180D3 * t55 * t126 * (t340 -
     # t208 * t342) + t416) * t201 * t142 / 0.720D3
      t522 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t521)
      t524 = t2 * x1
      t525 = -0.1D1 + x1
      t526 = t2 * t525
      t527 = x1 * z
      t528 = 0.1D1 - x1 + t527
      t529 = 0.1D1 / t528
      t530 = t525 ** 2
      t531 = t529 * t530
      t532 = t102 * t531
      t535 = log(0.4D1 * t101 * t532)
      t536 = t535 ** 2
      t537 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t541 = t10 * t529
      t546 = log(-0.4D1 * t93 * t8 * t541 * t530 * t104)
      t547 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t549 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t550 = t546 ** 2
      t554 = x3 * x1
      t555 = t554 * z
      t558 = x3 * t528
      t560 = Sqrt(-t558 * t103)
      t564 = 0.1D1 / (-0.2D1 * t555 + 0.2D1 * t554 - 0.1D1 + 0.2D1 * t11
     #4 * t560 - x3)
      t580 = -t537 - t537 * t564
      t587 = t145 * t4
      t588 = t541 * t530
      t591 = log(0.4D1 * t587 * t588)
      t596 = t591 ** 2
      t597 = t596 * t591
      t600 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t608 = t126 * t537
      t620 = t177 * t145
      t623 = log(0.4D1 * t620 * t532)
      t629 = log(-0.4D1 * t620 * t102 * t531 * t104)
      t645 = t205 * t7
      t648 = log(0.4D1 * t645 * t532)
      t650 = t648 ** 2
      t667 = (0.90D2 * t64 * t40 * (-t536 * t537 / 0.2D1 - (-t546 * t547
     # + t549 + t550 * t537 / 0.2D1) * t564 + t535 * t547 - t549) - 0.18
     #0D3 * t55 * t126 * (t535 * t537 - t547 - (t547 - t546 * t537) * t5
     #64) + t48 * t126 * t580) * t140 * t142 / 0.1440D4 - (t48 * t126 * 
     #(-t591 * t537 + t547) + 0.90D2 * t64 * t40 * (-t597 * t537 / 0.6D1
     # + t600 - t591 * t549 + t596 * t547 / 0.2D1) + t26 * t608 - 0.180D
     #3 * t55 * t126 * (t596 * t537 / 0.2D1 + t549 - t591 * t547)) * t14
     #2 / 0.1440D4 - (0.90D2 * t64 * t40 * (-t623 * t537 + (t547 - t629 
     #* t537) * t564 + t547) + 0.180D3 * t55 * t126 * t580) * t140 * t20
     #2 / 0.720D3 + (0.90D2 * t64 * t40 * (-t549 + t648 * t547 - t650 * 
     #t537 / 0.2D1) - 0.180D3 * t55 * t126 * (t648 * t537 - t547) - t48 
     #* t608) * t201 * t142 / 0.720D3
      t668 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t524, -t526, 0.0D0, t667)
      t670 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t675 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t677 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t694 = -t670 - t670 * t564
      t708 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 0
     #.0D0, t524, 0.0D0)
      t715 = t126 * t670
      t760 = (0.90D2 * t64 * t40 * (-t536 * t670 / 0.2D1 - (t550 * t670 
     #/ 0.2D1 - t546 * t675 + t677) * t564 + t535 * t675 - t677) - 0.180
     #D3 * t55 * t126 * (-(t675 - t546 * t670) * t564 - t675 + t535 * t6
     #70) + t48 * t126 * t694) * t140 * t142 / 0.1440D4 - (t48 * t126 * 
     #(t675 - t591 * t670) + 0.90D2 * t64 * t40 * (t596 * t675 / 0.2D1 -
     # t591 * t677 + t708 - t597 * t670 / 0.6D1) + t26 * t715 - 0.180D3 
     #* t55 * t126 * (-t591 * t675 + t677 + t596 * t670 / 0.2D1)) * t142
     # / 0.1440D4 - (0.90D2 * t64 * t40 * (t675 - t623 * t670 + (t675 - 
     #t629 * t670) * t564) + 0.180D3 * t55 * t126 * t694) * t140 * t202 
     #/ 0.720D3 + (0.90D2 * t64 * t40 * (-t677 + t648 * t675 - t650 * t6
     #70 / 0.2D1) - 0.180D3 * t55 * t126 * (-t675 + t648 * t670) - t48 *
     # t715) * t201 * t142 / 0.720D3
      t761 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t526, t524, 0.0D0, t760)
      t763 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t765 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t766 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t787 = t766 + t766 * t120
      t799 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t808 = t126 * t766
      t809 = t26 * t808
      t829 = -t126 * t787
      t848 = t48 * t808
      t853 = t40 * t765
      t856 = t40 * t799
      t907 = t40 * t763
      t910 = rrgq2qgh74J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t914 = t126 * t763
      t921 = t40 * t766
      t944 = (0.90D2 * t64 * t40 * (-t96 * t763 + t765 + t98 * t766 / 0.
     #2D1 - (t108 * t763 - t765 - t110 * t766 / 0.2D1) * t120) - 0.180D3
     # * t55 * t126 * (-t96 * t766 + t763 - (-t763 + t108 * t766) * t120
     #) + t48 * t126 * t787) * t140 * t142 / 0.1440D4 - (t48 * t126 * (-
     #t763 + t148 * t766) + 0.90D2 * t64 * t40 * (t148 * t765 - t799 - t
     #153 * t763 / 0.2D1 + t156 * t766 / 0.6D1) - t809 - 0.180D3 * t55 *
     # t126 * (t148 * t763 - t765 - t153 * t766 / 0.2D1)) * t142 / 0.144
     #0D4 - (0.90D2 * t64 * t40 * ((-t763 + t183 * t766) * t120 - t763 +
     # t189 * t766) - 0.180D3 * t55 * t829) * t140 * t202 / 0.720D3 + (0
     #.90D2 * t64 * t40 * (-t208 * t763 + t765 + t209 * t766 / 0.2D1) - 
     #0.180D3 * t55 * t126 * (-t208 * t766 + t763) + t848) * t201 * t142
     # / 0.720D3 + t50 * t853 / 0.2880D4 + t59 * t856 / 0.2880D4 - (0.90
     #D2 * t64 * t40 * ((t277 * t763 - t765 - t279 * t766 / 0.2D1) * t12
     #0 - t765 + t286 * t763 - t288 * t766 / 0.2D1) - 0.180D3 * t55 * t1
     #26 * ((-t763 + t277 * t766) * t120 + t286 * t766 - t763) + t48 * t
     #829) * t140 * t201 / 0.1440D4 + (t48 * t126 * (-t311 * t766 + t763
     #) + 0.90D2 * t64 * t40 * (t799 + t316 * t763 / 0.2D1 - t319 * t766
     # / 0.6D1 - t311 * t765) + t809 - 0.180D3 * t55 * t126 * (-t311 * t
     #763 + t316 * t766 / 0.2D1 + t765)) * t201 / 0.1440D4 + t37 * t907 
     #/ 0.2880D4 + t64 * t40 * t910 / 0.32D2 + ((0.180D3 * t55 * t914 - 
     #t848 - 0.90D2 * t64 * t853) * t241 - 0.90D2 * t64 * t921 * t249 + 
     #(-t48 * t914 - 0.90D2 * t64 * t856 - t809 + 0.180D3 * t55 * t126 *
     # t765) * t260 + (-0.90D2 * t64 * t907 + 0.180D3 * t55 * t808) * t2
     #69) * t140 / 0.2880D4 + t87 * t921 / 0.2880D4
      t945 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t944)
      t948 = x2 * s * t1
      t949 = -0.1D1 + x2
      t950 = t949 * s
      t951 = t950 * t1
      t952 = x2 * z
      t954 = 0.1D1 / (0.1D1 - x2 + t952)
      t955 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0.
     #0D0, 0.0D0, 0.0D0)
      t956 = t954 * t955
      t957 = t10 * t949
      t961 = log(-0.4D1 * t178 * t8 * t957)
      t962 = t961 * t954
      t963 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0.
     #0D0, 0.0D0, 0.0D0)
      t969 = t55 * t36
      t970 = t40 * t954
      t971 = t970 * t963
      t978 = t102 * t949
      t981 = log(-0.4D1 * t645 * t978)
      t982 = t981 * t954
      t984 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0.
     #0D0, 0.0D0, 0.0D0)
      t985 = t954 * t984
      t986 = t981 ** 2
      t987 = t986 * t954
      t999 = t48 * t36
      t1000 = t999 * t971
      t1007 = log(-0.4D1 * t274 * t978)
      t1008 = t1007 * t954
      t1010 = t1007 ** 2
      t1011 = t1010 * t954
      t1029 = log(-0.4D1 * t308 * t978)
      t1030 = t1029 * t954
      t1035 = t1029 ** 2
      t1036 = t1035 * t954
      t1039 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1042 = t1035 * t1029 * t954
      t1050 = t26 * t36
      t1062 = -(0.90D2 * t64 * t40 * (t956 - t962 * t963) - 0.180D3 * t9
     #69 * t971) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t982 *
     # t955 - t985 - t987 * t963 / 0.2D1) - 0.180D3 * t55 * t126 * (t982
     # * t963 - t956) - t1000) * t201 * t142 / 0.720D3 - (0.90D2 * t64 *
     # t40 * (-t1008 * t955 + t1011 * t963 / 0.2D1 + t985) - 0.180D3 * t
     #55 * t126 * (-t1008 * t963 + t956) + t1000) * t140 * t201 / 0.1440
     #D4 + (t48 * t126 * (-t956 + t1030 * t963) + 0.90D2 * t64 * t40 * (
     #-t1036 * t955 / 0.2D1 - t954 * t1039 + t1042 * t963 / 0.6D1 + t103
     #0 * t984) - t1050 * t971 - 0.180D3 * t55 * t126 * (-t985 + t1030 *
     # t955 - t1036 * t963 / 0.2D1)) * t201 / 0.1440D4
      t1063 = FJET(XB1, XB2, s, 0.0D0, t948, 0.0D0, -t951, 0.0D0, t1062)
      t1065 = x2 * x3
      t1068 = Sqrt(x3 * t949 * t103)
      t1069 = t114 * t1068
      t1071 = 0.2D1 * t1069 * x2
      t1073 = 0.1D1 - x3 + t1065
      t1074 = 0.1D1 / t1073
      t1076 = t2 * (0.1D1 - x3 - x2 + t1065 + t177 + t1071) * t1074
      t1077 = 0.2D1 * t1069
      t1081 = t2 * x2 * (-0.1D1 + t1065 + t1077) * t1074
      t1084 = t177 * z
      t1086 = 0.1D1 / (-0.1D1 - t1071 + x2 - x3 + 0.2D1 * t1069 * t952 +
     # t1077 + t1065 - t952 - t177 + t1084)
      t1087 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1088 = t1086 * t1087
      t1089 = t949 * t103
      t1090 = t1073 ** 2
      t1091 = 0.1D1 / t1090
      t1092 = t1089 * t1091
      t1096 = log(0.4D1 * t620 * t102 * t1092)
      t1097 = t1096 * t1086
      t1098 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1104 = t40 * t1086
      t1105 = t1104 * t1098
      t1117 = log(0.4D1 * t177 * t8 * t957 * t103 * t1091)
      t1118 = t1117 * t1086
      t1120 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1122 = t1117 ** 2
      t1123 = t1122 * t1086
      t1140 = -(0.90D2 * t64 * t40 * (t1088 - t1097 * t1098) - 0.180D3 *
     # t969 * t1105) * t140 * t202 / 0.720D3 - (0.90D2 * t64 * t40 * (-t
     #1118 * t1087 + t1086 * t1120 + t1123 * t1098 / 0.2D1) - 0.180D3 * 
     #t55 * t126 * (t1088 - t1118 * t1098) + t999 * t1105) * t140 * t201
     # / 0.1440D4
      t1141 = FJET(XB1, XB2, s, 0.0D0, t1076, 0.0D0, -t1081, 0.0D0, t114
     #0)
      t1143 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1144 = t954 * t1143
      t1145 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1151 = t970 * t1145
      t1159 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1160 = t954 * t1159
      t1172 = t999 * t1151
      t1199 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1219 = -(0.90D2 * t64 * t40 * (t1144 - t962 * t1145) - 0.180D3 * 
     #t969 * t1151) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t98
     #2 * t1143 - t1160 - t987 * t1145 / 0.2D1) - 0.180D3 * t55 * t126 *
     # (t982 * t1145 - t1144) - t1172) * t201 * t142 / 0.720D3 - (0.90D2
     # * t64 * t40 * (t1011 * t1145 / 0.2D1 - t1008 * t1143 + t1160) - 0
     #.180D3 * t55 * t126 * (t1144 - t1008 * t1145) + t1172) * t140 * t2
     #01 / 0.1440D4 + (t48 * t126 * (-t1144 + t1030 * t1145) + 0.90D2 * 
     #t64 * t40 * (-t1036 * t1143 / 0.2D1 - t954 * t1199 + t1030 * t1159
     # + t1042 * t1145 / 0.6D1) - t1050 * t1151 - 0.180D3 * t55 * t126 *
     # (-t1160 + t1030 * t1143 - t1036 * t1145 / 0.2D1)) * t201 / 0.1440
     #D4
      t1220 = FJET(XB1, XB2, s, 0.0D0, -t951, 0.0D0, t948, 0.0D0, t1219)
      t1222 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1224 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1225 = t1086 * t1224
      t1230 = t1104 * t1222
      t1238 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1256 = -(0.90D2 * t64 * t40 * (-t1097 * t1222 + t1225) - 0.180D3 
     #* t969 * t1230) * t140 * t202 / 0.720D3 - (0.90D2 * t64 * t40 * (-
     #t1118 * t1224 + t1086 * t1238 + t1123 * t1222 / 0.2D1) - 0.180D3 *
     # t55 * t126 * (t1225 - t1118 * t1222) + t999 * t1230) * t140 * t20
     #1 / 0.1440D4
      t1257 = FJET(XB1, XB2, s, 0.0D0, -t1081, 0.0D0, t1076, 0.0D0, t125
     #6)
      t1261 = t2 * t525 * x2 * t529
      t1263 = t950 * t1 * t525
      t1268 = s * t9 * x2 * x1 * t525 * t529
      t1273 = log(-0.4D1 * t620 * t102 * t531 * t949)
      t1274 = t1273 * t528
      t1275 = x2 * x1
      t1276 = t1275 * z
      t1278 = 0.1D1 / (-0.1D1 - t527 + x1 + x2 + t1276 - t1275 - t952)
      t1279 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1280 = t1278 * t1279
      t1282 = t528 * t1278
      t1283 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1284 = t1282 * t1283
      t1289 = t40 * t528
      t1290 = t1289 * t1280
      t1301 = log(-0.4D1 * t205 * t8 * t541 * t530 * t949)
      t1302 = t1301 * t528
      t1305 = t1301 ** 2
      t1306 = t1305 * t528
      t1309 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1325 = -(0.90D2 * t64 * t40 * (-t1274 * t1280 + t1284) - 0.180D3 
     #* t969 * t1290) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t
     #1302 * t1278 * t1283 - t1306 * t1280 / 0.2D1 - t1282 * t1309) - 0.
     #180D3 * t55 * t126 * (t1302 * t1280 - t1284) - t999 * t1290) * t20
     #1 * t142 / 0.720D3
      t1326 = FJET(XB1, XB2, s, 0.0D0, -t1261, t524, t1263, -t1268, t132
     #5)
      t1328 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1329 = t126 * t1328
      t1332 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1333 = t126 * t1332
      t1334 = t48 * t1333
      t1335 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1336 = t40 * t1335
      t1341 = t40 * t1332
      t1346 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1347 = t40 * t1346
      t1350 = t26 * t1333
      t1356 = t40 * t1328
      t1395 = t1332 + t1332 * t120
      t1434 = -t126 * t1395
      t1505 = rrgq2qgh72J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1509 = ((0.180D3 * t55 * t1329 - t1334 - 0.90D2 * t64 * t1336) * 
     #t241 - 0.90D2 * t64 * t1341 * t249 + (-t48 * t1329 - 0.90D2 * t64 
     #* t1347 - t1350 + 0.180D3 * t55 * t126 * t1335) * t260 + (-0.90D2 
     #* t64 * t1356 + 0.180D3 * t55 * t1333) * t269) * t140 / 0.2880D4 +
     # t87 * t1341 / 0.2880D4 + t37 * t1356 / 0.2880D4 + t50 * t1336 / 0
     #.2880D4 + t59 * t1347 / 0.2880D4 + (0.90D2 * t64 * t40 * (-t96 * t
     #1328 + t98 * t1332 / 0.2D1 + t1335 - (t108 * t1328 - t1335 - t110 
     #* t1332 / 0.2D1) * t120) - 0.180D3 * t55 * t126 * (t1328 - t96 * t
     #1332 - (-t1328 + t108 * t1332) * t120) + t48 * t126 * t1395) * t14
     #0 * t142 / 0.1440D4 - (t48 * t126 * (t148 * t1332 - t1328) + 0.90D
     #2 * t64 * t40 * (-t153 * t1328 / 0.2D1 - t1346 + t148 * t1335 + t1
     #56 * t1332 / 0.6D1) - t1350 - 0.180D3 * t55 * t126 * (t148 * t1328
     # - t1335 - t153 * t1332 / 0.2D1)) * t142 / 0.1440D4 - (0.90D2 * t6
     #4 * t40 * (t189 * t1332 - t1328 + (-t1328 + t183 * t1332) * t120) 
     #- 0.180D3 * t55 * t1434) * t140 * t202 / 0.720D3 + (0.90D2 * t64 *
     # t40 * (t1335 + t209 * t1332 / 0.2D1 - t208 * t1328) - 0.180D3 * t
     #55 * t126 * (t1328 - t208 * t1332) + t1334) * t201 * t142 / 0.720D
     #3 - (0.90D2 * t64 * t40 * (-t288 * t1332 / 0.2D1 - t1335 + t286 * 
     #t1328 + (t277 * t1328 - t1335 - t279 * t1332 / 0.2D1) * t120) - 0.
     #180D3 * t55 * t126 * (-t1328 + (-t1328 + t277 * t1332) * t120 + t2
     #86 * t1332) + t48 * t1434) * t140 * t201 / 0.1440D4 + (t48 * t126 
     #* (t1328 - t311 * t1332) + 0.90D2 * t64 * t40 * (-t319 * t1332 / 0
     #.6D1 + t1346 - t311 * t1335 + t316 * t1328 / 0.2D1) + t1350 - 0.18
     #0D3 * t55 * t126 * (t1335 + t316 * t1332 / 0.2D1 - t311 * t1328)) 
     #* t201 / 0.1440D4 + t64 * t40 * t1505 / 0.32D2
      t1510 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1509)
      t1512 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1513 = t1278 * t1512
      t1515 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1516 = t1282 * t1515
      t1521 = t1289 * t1513
      t1531 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1547 = -(0.90D2 * t64 * t40 * (-t1274 * t1513 + t1516) - 0.180D3 
     #* t969 * t1521) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t
     #1302 * t1278 * t1515 - t1306 * t1513 / 0.2D1 - t1282 * t1531) - 0.
     #180D3 * t55 * t126 * (-t1516 + t1302 * t1513) - t999 * t1521) * t2
     #01 * t142 / 0.720D3
      t1548 = FJET(XB1, XB2, s, t524, t1263, 0.0D0, -t1261, -t1268, t154
     #7)
      t1550 = t338 * t337 + t522 * t521 + t668 * t667 + t761 * t760 + t9
     #45 * t944 + t1063 * t1062 + t1141 * t1140 + t1220 * t1219 + t1257 
     #* t1256 + t1326 * t1325 + t1510 * t1509 + t1548 * t1547
      t1551 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1553 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1554 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1575 = -t1554 - t1554 * t564
      t1590 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1596 = t126 * t1554
      t1641 = (0.90D2 * t64 * t40 * (-(-t546 * t1551 + t1553 + t550 * t1
     #554 / 0.2D1) * t564 - t1553 - t536 * t1554 / 0.2D1 + t535 * t1551)
     # - 0.180D3 * t55 * t126 * (t535 * t1554 - t1551 - (t1551 - t546 * 
     #t1554) * t564) + t48 * t126 * t1575) * t140 * t142 / 0.1440D4 - (t
     #48 * t126 * (t1551 - t591 * t1554) + 0.90D2 * t64 * t40 * (t596 * 
     #t1551 / 0.2D1 - t597 * t1554 / 0.6D1 + t1590 - t591 * t1553) + t26
     # * t1596 - 0.180D3 * t55 * t126 * (t596 * t1554 / 0.2D1 + t1553 - 
     #t591 * t1551)) * t142 / 0.1440D4 - (0.90D2 * t64 * t40 * (t1551 - 
     #t623 * t1554 + (t1551 - t629 * t1554) * t564) + 0.180D3 * t55 * t1
     #26 * t1575) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (-t650
     # * t1554 / 0.2D1 + t648 * t1551 - t1553) - 0.180D3 * t55 * t126 * 
     #(-t1551 + t648 * t1554) - t48 * t1596) * t201 * t142 / 0.720D3
      t1642 = FJET(XB1, XB2, s, t524, -t526, 0.0D0, 0.0D0, 0.0D0, t1641)
      t1644 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1645 = t954 * t1644
      t1646 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1652 = t970 * t1646
      t1660 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1661 = t954 * t1660
      t1673 = t999 * t1652
      t1702 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t1720 = -(0.90D2 * t64 * t40 * (t1645 - t962 * t1646) - 0.180D3 * 
     #t969 * t1652) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t98
     #2 * t1644 - t1661 - t987 * t1646 / 0.2D1) - 0.180D3 * t55 * t126 *
     # (-t1645 + t982 * t1646) - t1673) * t201 * t142 / 0.720D3 - (0.90D
     #2 * t64 * t40 * (t1661 - t1008 * t1644 + t1011 * t1646 / 0.2D1) - 
     #0.180D3 * t55 * t126 * (t1645 - t1008 * t1646) + t1673) * t140 * t
     #201 / 0.1440D4 + (t48 * t126 * (-t1645 + t1030 * t1646) + 0.90D2 *
     # t64 * t40 * (-t1036 * t1644 / 0.2D1 + t1042 * t1646 / 0.6D1 - t95
     #4 * t1702 + t1030 * t1660) - t1050 * t1652 - 0.180D3 * t55 * t126 
     #* (t1030 * t1644 - t1661 - t1036 * t1646 / 0.2D1)) * t201 / 0.1440
     #D4
      t1721 = FJET(XB1, XB2, s, t948, 0.0D0, -t951, 0.0D0, 0.0D0, t1720)
      t1723 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1725 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1726 = t1086 * t1725
      t1731 = t1104 * t1723
      t1741 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t1757 = -(0.90D2 * t64 * t40 * (-t1097 * t1723 + t1726) - 0.180D3 
     #* t969 * t1731) * t140 * t202 / 0.720D3 - (0.90D2 * t64 * t40 * (-
     #t1118 * t1725 + t1123 * t1723 / 0.2D1 + t1086 * t1741) - 0.180D3 *
     # t55 * t126 * (t1726 - t1118 * t1723) + t999 * t1731) * t140 * t20
     #1 / 0.1440D4
      t1758 = FJET(XB1, XB2, s, t1076, 0.0D0, -t1081, 0.0D0, 0.0D0, t175
     #7)
      t1760 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1761 = t1278 * t1760
      t1763 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1764 = t1282 * t1763
      t1769 = t1289 * t1761
      t1779 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t1795 = -(0.90D2 * t64 * t40 * (-t1274 * t1761 + t1764) - 0.180D3 
     #* t969 * t1769) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (-
     #t1306 * t1761 / 0.2D1 + t1302 * t1278 * t1763 - t1282 * t1779) - 0
     #.180D3 * t55 * t126 * (t1302 * t1761 - t1764) - t999 * t1769) * t2
     #01 * t142 / 0.720D3
      t1796 = FJET(XB1, XB2, s, t1263, t524, -t1261, 0.0D0, -t1268, t179
     #5)
      t1799 = t524 * t1065 * t1074
      t1800 = t1065 * x1
      t1801 = t1065 * t527
      t1803 = Sqrt(t558 * t1089)
      t1804 = t114 * t1803
      t1805 = 0.2D1 * t1804
      t1810 = t526 * x2 * (t554 - t555 + t1065 - t1800 + t1801 - 0.1D1 +
     # t1805) * t529 * t1074
      t1814 = t103 * s * t1 * x1 * t1074
      t1816 = 0.2D1 * t1804 * x2
      t1817 = 0.1D1 - x1 + t527 - x2 + t1275 - t1276 - x3 + t554 - t555 
     #+ t1065 - t1800 + t1801 + t177 + t1816
      t1820 = t526 * t1817 * t529 * t1074
      t1833 = 0.1D1 - t1084 + 0.3D1 * t1800 + t1816 + 0.2D1 * t1804 * x1
     # + t178 - 0.4D1 * t93 * z - 0.2D1 * t93 * x2 + 0.2D1 * t93 * t3 - 
     #0.2D1 * t177 * x1 - t1065 + t177 + t1275 - t1805 + 0.2D1 * t93 + x
     #3 - x1
      t1861 = -x2 + 0.2D1 * t1804 * t1276 - 0.4D1 * t1801 + x3 * t3 * t1
     #275 + 0.4D1 * t93 * t952 - 0.2D1 * t93 * t3 * x2 + 0.3D1 * t177 * 
     #t527 - t177 * t3 * x1 - 0.2D1 * t177 * t92 * z + t177 * t92 * t3 -
     # 0.2D1 * t1804 * t527 - 0.2D1 * t1804 * t1275 - 0.2D1 * t1804 * t9
     #52 + 0.3D1 * t555 + t952 + t527 - 0.3D1 * t554 - t1276
      t1863 = 0.1D1 / (t1833 + t1861)
      t1864 = t528 * t1863
      t1865 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t1871 = log(0.4D1 * t177 * t587 * t588 * t1092)
      t1872 = t1871 * t528
      t1873 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t1874 = t1863 * t1873
      t1883 = 0.90D2 * t64 * t40 * (t1864 * t1865 - t1872 * t1874) - 0.1
     #80D3 * t969 * t1289 * t1874
      t1887 = FJET(XB1, XB2, s, t1799, t1810, -t1814, -t1820, -t1268, -t
     #1883 * t140 * t202 / 0.720D3)
      t1890 = t140 * t201 * t142
      t1893 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t1895 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t1896 = t1863 * t1895
      t1905 = 0.90D2 * t64 * t40 * (t1864 * t1893 - t1872 * t1896) - 0.1
     #80D3 * t969 * t1289 * t1896
      t1909 = FJET(XB1, XB2, s, t1810, t1799, -t1820, -t1814, -t1268, -t
     #1905 * t140 * t202 / 0.720D3)
      t1913 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1915 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1916 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1937 = -t1916 - t1916 * t564
      t1951 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t526, 
     #0.0D0, t524, 0.0D0)
      t1958 = t126 * t1916
      t2003 = (0.90D2 * t64 * t40 * (-(-t546 * t1913 + t1915 + t550 * t1
     #916 / 0.2D1) * t564 - t1915 - t536 * t1916 / 0.2D1 + t535 * t1913)
     # - 0.180D3 * t55 * t126 * (-(t1913 - t546 * t1916) * t564 + t535 *
     # t1916 - t1913) + t48 * t126 * t1937) * t140 * t142 / 0.1440D4 - (
     #t48 * t126 * (-t591 * t1916 + t1913) + 0.90D2 * t64 * t40 * (t596 
     #* t1913 / 0.2D1 - t591 * t1915 + t1951 - t597 * t1916 / 0.6D1) + t
     #26 * t1958 - 0.180D3 * t55 * t126 * (t596 * t1916 / 0.2D1 + t1915 
     #- t591 * t1913)) * t142 / 0.1440D4 - (0.90D2 * t64 * t40 * (-t623 
     #* t1916 + (t1913 - t629 * t1916) * t564 + t1913) + 0.180D3 * t55 *
     # t126 * t1937) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (-t
     #1915 - t650 * t1916 / 0.2D1 + t648 * t1913) - 0.180D3 * t55 * t126
     # * (t648 * t1916 - t1913) - t48 * t1958) * t201 * t142 / 0.720D3
      t2004 = FJET(XB1, XB2, s, -t526, t524, 0.0D0, 0.0D0, 0.0D0, t2003)
      t2006 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t2007 = t954 * t2006
      t2008 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t2014 = t970 * t2008
      t2024 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t2025 = t954 * t2024
      t2035 = t999 * t2014
      t2064 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, t948, -t951, 0
     #.0D0, 0.0D0, 0.0D0)
      t2082 = -(0.90D2 * t64 * t40 * (t2007 - t962 * t2008) - 0.180D3 * 
     #t969 * t2014) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (t98
     #2 * t2006 - t987 * t2008 / 0.2D1 - t2025) - 0.180D3 * t55 * t126 *
     # (-t2007 + t982 * t2008) - t2035) * t201 * t142 / 0.720D3 - (0.90D
     #2 * t64 * t40 * (t2025 + t1011 * t2008 / 0.2D1 - t1008 * t2006) - 
     #0.180D3 * t55 * t126 * (t2007 - t1008 * t2008) + t2035) * t140 * t
     #201 / 0.1440D4 + (t48 * t126 * (-t2007 + t1030 * t2008) + 0.90D2 *
     # t64 * t40 * (-t1036 * t2006 / 0.2D1 + t1042 * t2008 / 0.6D1 - t95
     #4 * t2064 + t1030 * t2024) - t1050 * t2014 - 0.180D3 * t55 * t126 
     #* (t1030 * t2006 - t2025 - t1036 * t2008 / 0.2D1)) * t201 / 0.1440
     #D4
      t2083 = FJET(XB1, XB2, s, -t951, 0.0D0, t948, 0.0D0, 0.0D0, t2082)
      t2085 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t2086 = t1086 * t2085
      t2087 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t2093 = t1104 * t2087
      t2103 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t1081, t1076,
     # 0.0D0, 0.0D0, 0.0D0)
      t2119 = -(0.90D2 * t64 * t40 * (t2086 - t1097 * t2087) - 0.180D3 *
     # t969 * t2093) * t140 * t202 / 0.720D3 - (0.90D2 * t64 * t40 * (-t
     #1118 * t2085 + t1123 * t2087 / 0.2D1 + t1086 * t2103) - 0.180D3 * 
     #t55 * t126 * (t2086 - t1118 * t2087) + t999 * t2093) * t140 * t201
     # / 0.1440D4
      t2120 = FJET(XB1, XB2, s, -t1081, 0.0D0, t1076, 0.0D0, 0.0D0, t211
     #9)
      t2122 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t2123 = t1282 * t2122
      t2124 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t2125 = t1278 * t2124
      t2131 = t1289 * t2125
      t2141 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t1261, t1263,
     # 0.0D0, t524, -t1268)
      t2157 = -(0.90D2 * t64 * t40 * (t2123 - t1274 * t2125) - 0.180D3 *
     # t969 * t2131) * t140 * t202 / 0.720D3 + (0.90D2 * t64 * t40 * (-t
     #1306 * t2125 / 0.2D1 + t1302 * t1278 * t2122 - t1282 * t2141) - 0.
     #180D3 * t55 * t126 * (t1302 * t2125 - t2123) - t999 * t2131) * t20
     #1 * t142 / 0.720D3
      t2158 = FJET(XB1, XB2, s, -t1261, 0.0D0, t1263, t524, -t1268, t215
     #7)
      t2160 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t2162 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t2163 = t1863 * t2162
      t2172 = 0.90D2 * t64 * t40 * (t1864 * t2160 - t1872 * t2163) - 0.1
     #80D3 * t969 * t1289 * t2163
      t2176 = FJET(XB1, XB2, s, -t1814, -t1820, t1799, t1810, -t1268, -t
     #2172 * t140 * t202 / 0.720D3)
      t2180 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t2182 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t1810, -t1820,
     # t1799, -t1814, -t1268)
      t2183 = t1863 * t2182
      t2192 = 0.90D2 * t64 * t40 * (t1864 * t2180 - t1872 * t2183) - 0.1
     #80D3 * t969 * t1289 * t2183
      t2196 = FJET(XB1, XB2, s, -t1820, -t1814, t1810, t1799, -t1268, -t
     #2192 * t140 * t202 / 0.720D3)
      t2200 = t1642 * t1641 + t1721 * t1720 + t1758 * t1757 + t1796 * t1
     #795 - t1887 * t1883 * t1890 / 0.720D3 - t1909 * t1905 * t1890 / 0.
     #720D3 + t2004 * t2003 + t2083 * t2082 + t2120 * t2119 + t2158 * t2
     #157 - t2176 * t2172 * t1890 / 0.720D3 - t2196 * t2192 * t1890 / 0.
     #720D3
      rrgq2qght7s1e1 = t1550 + t2200

      end function



      doubleprecision function rrgq2qght7s1e0
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t20 = 0.1D1 / t19
      t21 = t18 * t20
      t24 = log(-0.4D1 * t13 * t21)
      t25 = t24 ** 2
      t26 = cos(t10)
      t28 = Sqrt(-x3 * t19)
      t32 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t26 * t28)
      t36 = log(0.4D1 * t13 * t18)
      t37 = t36 ** 2
      t39 = -t25 * t32 / 0.2D1 - t37 / 0.2D1
      t43 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t44 = t7 * t43
      t47 = pi * lh
      t48 = t3 * t7
      t49 = t48 * t8
      t51 = 0.180D3 * t47 * t49
      t54 = t24 * t32 + t36
      t59 = lh ** 2
      t61 = pi ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t64 = pi * t63
      t65 = t64 * t49
      t66 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t67 = t7 * t66
      t71 = -t32 - 0.1D1
      t74 = 0.1D1 / x3
      t77 = t15 * t12
      t78 = t77 * t17
      t80 = log(0.4D1 * t78)
      t81 = t80 * pi
      t84 = t80 ** 2
      t85 = t84 * pi
      t88 = (0.180D3 * t81 * lh + 0.45D2 * t85 + t64) * t3
      t91 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t109 = (-0.90D2 * t85 * lh + pi * (0.60D2 * lh * t61 - 0.240D3 * z
     #eta3 - 0.120D3 * t59 * lh) - 0.15D2 * t84 * t80 * pi - t81 * t63) 
     #* t3
      t115 = (-0.180D3 * t47 - 0.90D2 * t81) * t3
      t118 = x2 ** 2
      t119 = x3 * t118
      t120 = t119 * t12
      t123 = log(-0.4D1 * t120 * t21)
      t129 = log(0.4D1 * t119 * t78)
      t136 = -t8 - t8 * t32
      t142 = 0.1D1 / x2
      t145 = t118 * t12
      t148 = log(0.4D1 * t145 * t18)
      t149 = t148 ** 2
      t165 = x1 ** 2
      t166 = x3 * t165
      t169 = log(0.4D1 * t166 * t78)
      t171 = t166 * t12
      t174 = log(-0.4D1 * t171 * t21)
      t188 = 0.1D1 / x1
      t191 = t4 * t7
      t193 = t142 * t188
      t197 = t118 * t165
      t200 = log(0.4D1 * t197 * t78)
      t210 = t165 * t12
      t213 = log(0.4D1 * t210 * t18)
      t214 = t213 ** 2
      t230 = (-0.90D2 * t4 * t9 * t39 + (-0.90D2 * t4 * t44 + t51) * t54
     # + (0.180D3 * t47 * t48 * t43 - t65 - 0.90D2 * t4 * t67) * t71) * 
     #t74 / 0.2880D4 + t88 * t44 / 0.2880D4 + t4 * t7 * t91 / 0.32D2 + t
     #109 * t9 / 0.2880D4 + t115 * t67 / 0.2880D4 - (0.90D2 * t4 * t7 * 
     #((-t43 + t123 * t8) * t32 - t43 + t129 * t8) - 0.180D3 * t47 * t48
     # * t136) * t74 * t142 / 0.1440D4 + (0.90D2 * t4 * t7 * (t149 * t8 
     #/ 0.2D1 - t148 * t43 + t66) - 0.180D3 * t47 * t48 * (-t148 * t8 + 
     #t43) + t65) * t142 / 0.1440D4 + (0.90D2 * t4 * t7 * (-t169 * t8 + 
     #t43 - (-t43 + t174 * t8) * t32) + 0.180D3 * t47 * t48 * t136) * t7
     #4 * t188 / 0.1440D4 - t191 * t136 * t74 * t193 / 0.8D1 + (0.90D2 *
     # t4 * t7 * (-t200 * t8 + t43) - t51) * t142 * t188 / 0.720D3 - (0.
     #90D2 * t4 * t7 * (-t66 - t214 * t8 / 0.2D1 + t213 * t43) - 0.180D3
     # * t47 * t48 * (-t43 + t213 * t8) - t65) * t188 / 0.1440D4
      t231 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t230)
      t233 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t234 = t7 * t233
      t238 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t239 = t7 * t238
      t242 = t48 * t233
      t244 = 0.180D3 * t47 * t242
      t250 = t64 * t242
      t251 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t252 = t7 * t251
      t262 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t279 = -t233 - t233 * t32
      t346 = (-0.90D2 * t4 * t234 * t39 + (-0.90D2 * t4 * t239 + t244) *
     # t54 + (0.180D3 * t47 * t48 * t238 - t250 - 0.90D2 * t4 * t252) * 
     #t71) * t74 / 0.2880D4 + t88 * t239 / 0.2880D4 + t4 * t7 * t262 / 0
     #.32D2 + t109 * t234 / 0.2880D4 + t115 * t252 / 0.2880D4 - (0.90D2 
     #* t4 * t7 * (-t238 + (-t238 + t123 * t233) * t32 + t129 * t233) - 
     #0.180D3 * t47 * t48 * t279) * t74 * t142 / 0.1440D4 + (0.90D2 * t4
     # * t7 * (t251 + t149 * t233 / 0.2D1 - t148 * t238) - 0.180D3 * t47
     # * t48 * (t238 - t148 * t233) + t250) * t142 / 0.1440D4 + (0.90D2 
     #* t4 * t7 * (t238 - t169 * t233 - (-t238 + t174 * t233) * t32) + 0
     #.180D3 * t47 * t48 * t279) * t74 * t188 / 0.1440D4 - t191 * t279 *
     # t74 * t193 / 0.8D1 + (0.90D2 * t4 * t7 * (t238 - t200 * t233) - t
     #244) * t142 * t188 / 0.720D3 - (0.90D2 * t4 * t7 * (t213 * t238 - 
     #t251 - t214 * t233 / 0.2D1) - 0.180D3 * t47 * t48 * (-t238 + t213 
     #* t233) - t250) * t188 / 0.1440D4
      t347 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t346)
      t349 = t2 * x1
      t350 = -0.1D1 + x1
      t351 = t2 * t350
      t352 = x1 * z
      t353 = 0.1D1 - x1 + t352
      t354 = 0.1D1 / t353
      t355 = t350 ** 2
      t357 = t18 * t354 * t355
      t360 = log(0.4D1 * t171 * t357)
      t361 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 0
     #.0D0, t349, 0.0D0)
      t363 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 0
     #.0D0, t349, 0.0D0)
      t365 = t17 * t354
      t370 = log(-0.4D1 * t166 * t77 * t365 * t355 * t20)
      t373 = x3 * x1
      t374 = t373 * z
      t377 = x3 * t353
      t379 = Sqrt(-t377 * t19)
      t383 = 0.1D1 / (-0.2D1 * t374 + 0.2D1 * t373 - 0.1D1 + 0.2D1 * t26
     # * t379 - x3)
      t390 = -t361 - t361 * t383
      t403 = t197 * t12
      t406 = log(0.4D1 * t403 * t357)
      t412 = t48 * t361
      t423 = log(0.4D1 * t210 * t15 * t365 * t355)
      t424 = t423 ** 2
      t427 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 0
     #.0D0, t349, 0.0D0)
      t442 = (0.90D2 * t4 * t7 * (t360 * t361 - t363 - (t363 - t370 * t3
     #61) * t383) - 0.180D3 * t47 * t48 * t390) * t74 * t188 / 0.1440D4 
     #+ t191 * t390 * t74 * t193 / 0.8D1 + (0.90D2 * t4 * t7 * (t406 * t
     #361 - t363) + 0.180D3 * t47 * t412) * t142 * t188 / 0.720D3 - (0.9
     #0D2 * t4 * t7 * (t424 * t361 / 0.2D1 + t427 - t423 * t363) - 0.180
     #D3 * t47 * t48 * (-t423 * t361 + t363) + t64 * t412) * t188 / 0.14
     #40D4
      t443 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t349, -t351, 0.0D0, t442)
      t445 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 0
     #.0D0, t349, 0.0D0)
      t446 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 0
     #.0D0, t349, 0.0D0)
      t456 = -t446 - t446 * t383
      t474 = t48 * t446
      t482 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 0
     #.0D0, t349, 0.0D0)
      t498 = (0.90D2 * t4 * t7 * (-(t445 - t370 * t446) * t383 - t445 + 
     #t360 * t446) - 0.180D3 * t47 * t48 * t456) * t74 * t188 / 0.1440D4
     # + t191 * t456 * t74 * t193 / 0.8D1 + (0.90D2 * t4 * t7 * (-t445 +
     # t406 * t446) + 0.180D3 * t47 * t474) * t142 * t188 / 0.720D3 - (0
     #.90D2 * t4 * t7 * (-t423 * t445 + t482 + t424 * t446 / 0.2D1) - 0.
     #180D3 * t47 * t48 * (t445 - t423 * t446) + t64 * t474) * t188 / 0.
     #1440D4
      t499 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t351, t349, 0.0D0, t498)
      t501 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t502 = t7 * t501
      t506 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t507 = t7 * t506
      t510 = t48 * t501
      t512 = 0.180D3 * t47 * t510
      t518 = t64 * t510
      t519 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t520 = t7 * t519
      t530 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t547 = -t501 - t501 * t32
      t614 = (-0.90D2 * t4 * t502 * t39 + (-0.90D2 * t4 * t507 + t512) *
     # t54 + (0.180D3 * t47 * t48 * t506 - t518 - 0.90D2 * t4 * t520) * 
     #t71) * t74 / 0.2880D4 + t88 * t507 / 0.2880D4 + t4 * t7 * t530 / 0
     #.32D2 + t109 * t502 / 0.2880D4 + t115 * t520 / 0.2880D4 - (0.90D2 
     #* t4 * t7 * ((-t506 + t123 * t501) * t32 + t129 * t501 - t506) - 0
     #.180D3 * t47 * t48 * t547) * t74 * t142 / 0.1440D4 + (0.90D2 * t4 
     #* t7 * (-t148 * t506 + t149 * t501 / 0.2D1 + t519) - 0.180D3 * t47
     # * t48 * (-t148 * t501 + t506) + t518) * t142 / 0.1440D4 + (0.90D2
     # * t4 * t7 * (-t169 * t501 + t506 - (-t506 + t174 * t501) * t32) +
     # 0.180D3 * t47 * t48 * t547) * t74 * t188 / 0.1440D4 - t191 * t547
     # * t74 * t193 / 0.8D1 + (0.90D2 * t4 * t7 * (-t200 * t501 + t506) 
     #- t512) * t142 * t188 / 0.720D3 - (0.90D2 * t4 * t7 * (t213 * t506
     # - t519 - t214 * t501 / 0.2D1) - 0.180D3 * t47 * t48 * (-t506 + t2
     #13 * t501) - t518) * t188 / 0.1440D4
      t615 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t614)
      t618 = x2 * s * t1
      t619 = -0.1D1 + x2
      t620 = t619 * s
      t621 = t620 * t1
      t622 = t18 * t619
      t625 = log(-0.4D1 * t120 * t622)
      t626 = x2 * z
      t628 = 0.1D1 / (0.1D1 - x2 + t626)
      t629 = t625 * t628
      t630 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0.
     #0D0, 0.0D0, 0.0D0)
      t632 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0.
     #0D0, 0.0D0, 0.0D0)
      t633 = t628 * t632
      t638 = t47 * t3
      t639 = t7 * t628
      t640 = t639 * t630
      t642 = 0.180D3 * t638 * t640
      t647 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0.
     #0D0, 0.0D0, 0.0D0)
      t651 = log(-0.4D1 * t145 * t622)
      t652 = t651 * t628
      t654 = t651 ** 2
      t655 = t654 * t628
      t667 = t64 * t3
      t672 = t4 * t639
      t679 = log(-0.4D1 * t403 * t622)
      t680 = t679 * t628
      t690 = -(0.90D2 * t4 * t7 * (-t629 * t630 + t633) - t642) * t74 * 
     #t142 / 0.1440D4 + (0.90D2 * t4 * t7 * (-t628 * t647 + t652 * t632 
     #- t655 * t630 / 0.2D1) - 0.180D3 * t47 * t48 * (-t633 + t652 * t63
     #0) - t667 * t640) * t142 / 0.1440D4 - t672 * t630 * t74 * t193 / 0
     #.8D1 + (0.90D2 * t4 * t7 * (t680 * t630 - t633) + t642) * t142 * t
     #188 / 0.720D3
      t691 = FJET(XB1, XB2, s, 0.0D0, t618, 0.0D0, -t621, 0.0D0, t690)
      t693 = x2 * x3
      t696 = Sqrt(x3 * t619 * t19)
      t697 = t26 * t696
      t699 = 0.2D1 * t697 * x2
      t701 = 0.1D1 - x3 + t693
      t702 = 0.1D1 / t701
      t704 = t2 * (0.1D1 - x3 - x2 + t693 + t119 + t699) * t702
      t705 = 0.2D1 * t697
      t709 = t2 * x2 * (-0.1D1 + t693 + t705) * t702
      t712 = t119 * z
      t714 = 0.1D1 / (-0.1D1 - t699 + x2 - x3 + 0.2D1 * t697 * t626 + t7
     #05 + t693 - t626 - t119 + t712)
      t715 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0.
     #0D0, 0.0D0, 0.0D0)
      t719 = t701 ** 2
      t725 = log(0.4D1 * t119 * t77 * t17 * t619 * t19 / t719)
      t726 = t725 * t714
      t727 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0.
     #0D0, 0.0D0, 0.0D0)
      t733 = t7 * t714
      t741 = t4 * t733
      t746 = -(0.90D2 * t4 * t7 * (t714 * t715 - t726 * t727) - 0.180D3 
     #* t638 * t733 * t727) * t74 * t142 / 0.1440D4 - t741 * t727 * t74 
     #* t193 / 0.8D1
      t747 = FJET(XB1, XB2, s, 0.0D0, t704, 0.0D0, -t709, 0.0D0, t746)
      t749 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0.
     #0D0, 0.0D0, 0.0D0)
      t750 = t628 * t749
      t751 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0.
     #0D0, 0.0D0, 0.0D0)
      t757 = t639 * t751
      t759 = 0.180D3 * t638 * t757
      t764 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0.
     #0D0, 0.0D0, 0.0D0)
      t795 = -(0.90D2 * t4 * t7 * (t750 - t629 * t751) - t759) * t74 * t
     #142 / 0.1440D4 + (0.90D2 * t4 * t7 * (-t628 * t764 + t652 * t749 -
     # t655 * t751 / 0.2D1) - 0.180D3 * t47 * t48 * (-t750 + t652 * t751
     #) - t667 * t757) * t142 / 0.1440D4 - t672 * t751 * t74 * t193 / 0.
     #8D1 + (0.90D2 * t4 * t7 * (t680 * t751 - t750) + t759) * t142 * t1
     #88 / 0.720D3
      t796 = FJET(XB1, XB2, s, 0.0D0, -t621, 0.0D0, t618, 0.0D0, t795)
      t798 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0.
     #0D0, 0.0D0, 0.0D0)
      t800 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0.
     #0D0, 0.0D0, 0.0D0)
      t817 = -(0.90D2 * t4 * t7 * (t714 * t798 - t726 * t800) - 0.180D3 
     #* t638 * t733 * t800) * t74 * t142 / 0.1440D4 - t741 * t800 * t74 
     #* t193 / 0.8D1
      t818 = FJET(XB1, XB2, s, 0.0D0, -t709, 0.0D0, t704, 0.0D0, t817)
      t822 = t2 * t350 * x2 * t354
      t824 = t620 * t1 * t350
      t829 = s * t16 * x2 * x1 * t350 * t354
      t830 = t7 * t353
      t831 = t4 * t830
      t832 = x2 * x1
      t833 = t832 * z
      t835 = 0.1D1 / (-0.1D1 - t352 + x1 + x2 + t833 - t832 - t626)
      t836 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0.
     #0D0, t349, -t829)
      t837 = t835 * t836
      t839 = t74 * t142 * t188
      t848 = log(-0.4D1 * t197 * t77 * t365 * t355 * t619)
      t849 = t848 * t353
      t851 = t353 * t835
      t852 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0.
     #0D0, t349, -t829)
      t865 = -t831 * t837 * t839 / 0.8D1 + (0.90D2 * t4 * t7 * (t849 * t
     #837 - t851 * t852) + 0.180D3 * t638 * t830 * t837) * t142 * t188 /
     # 0.720D3
      t866 = FJET(XB1, XB2, s, 0.0D0, -t822, t349, t824, -t829, t865)
      t868 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t869 = t7 * t868
      t873 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t874 = t7 * t873
      t877 = t48 * t868
      t879 = 0.180D3 * t47 * t877
      t885 = t64 * t877
      t886 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t887 = t7 * t886
      t895 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t912 = -t868 - t868 * t32
      t981 = (-0.90D2 * t4 * t869 * t39 + (-0.90D2 * t4 * t874 + t879) *
     # t54 + (0.180D3 * t47 * t48 * t873 - t885 - 0.90D2 * t4 * t887) * 
     #t71) * t74 / 0.2880D4 + t4 * t7 * t895 / 0.32D2 + t115 * t887 / 0.
     #2880D4 + t88 * t874 / 0.2880D4 - (0.90D2 * t4 * t7 * (-t873 + (-t8
     #73 + t123 * t868) * t32 + t129 * t868) - 0.180D3 * t47 * t48 * t91
     #2) * t74 * t142 / 0.1440D4 + (0.90D2 * t4 * t7 * (t886 + t149 * t8
     #68 / 0.2D1 - t148 * t873) - 0.180D3 * t47 * t48 * (t873 - t148 * t
     #868) + t885) * t142 / 0.1440D4 + t109 * t869 / 0.2880D4 + (0.90D2 
     #* t4 * t7 * (t873 - t169 * t868 - (-t873 + t174 * t868) * t32) + 0
     #.180D3 * t47 * t48 * t912) * t74 * t188 / 0.1440D4 - t191 * t912 *
     # t74 * t193 / 0.8D1 + (0.90D2 * t4 * t7 * (t873 - t200 * t868) - t
     #879) * t142 * t188 / 0.720D3 - (0.90D2 * t4 * t7 * (t213 * t873 - 
     #t886 - t214 * t868 / 0.2D1) - 0.180D3 * t47 * t48 * (t213 * t868 -
     # t873) - t885) * t188 / 0.1440D4
      t982 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t981)
      t984 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0.
     #0D0, t349, -t829)
      t985 = t835 * t984
      t989 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0.
     #0D0, t349, -t829)
      t1003 = -t831 * t985 * t839 / 0.8D1 + (0.90D2 * t4 * t7 * (-t851 *
     # t989 + t849 * t985) + 0.180D3 * t638 * t830 * t985) * t142 * t188
     # / 0.720D3
      t1004 = FJET(XB1, XB2, s, t349, t824, 0.0D0, -t822, -t829, t1003)
      t1006 = t231 * t230 + t347 * t346 + t443 * t442 + t499 * t498 + t6
     #15 * t614 + t691 * t690 + t747 * t746 + t796 * t795 + t818 * t817 
     #+ t866 * t865 + t982 * t981 + t1004 * t1003
      t1007 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 
     #0.0D0, t349, 0.0D0)
      t1009 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 
     #0.0D0, t349, 0.0D0)
      t1018 = -t1007 - t1007 * t383
      t1036 = t48 * t1007
      t1045 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 
     #0.0D0, t349, 0.0D0)
      t1060 = (0.90D2 * t4 * t7 * (t360 * t1007 - t1009 - (t1009 - t370 
     #* t1007) * t383) - 0.180D3 * t47 * t48 * t1018) * t74 * t188 / 0.1
     #440D4 + t191 * t1018 * t74 * t193 / 0.8D1 + (0.90D2 * t4 * t7 * (-
     #t1009 + t406 * t1007) + 0.180D3 * t47 * t1036) * t142 * t188 / 0.7
     #20D3 - (0.90D2 * t4 * t7 * (t424 * t1007 / 0.2D1 + t1045 - t423 * 
     #t1009) - 0.180D3 * t47 * t48 * (t1009 - t423 * t1007) + t64 * t103
     #6) * t188 / 0.1440D4
      t1061 = FJET(XB1, XB2, s, t349, -t351, 0.0D0, 0.0D0, 0.0D0, t1060)
      t1063 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0
     #.0D0, 0.0D0, 0.0D0)
      t1064 = t628 * t1063
      t1065 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0
     #.0D0, 0.0D0, 0.0D0)
      t1071 = t639 * t1065
      t1073 = 0.180D3 * t638 * t1071
      t1079 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0
     #.0D0, 0.0D0, 0.0D0)
      t1109 = -(0.90D2 * t4 * t7 * (t1064 - t629 * t1065) - t1073) * t74
     # * t142 / 0.1440D4 + (0.90D2 * t4 * t7 * (t652 * t1063 - t628 * t1
     #079 - t655 * t1065 / 0.2D1) - 0.180D3 * t47 * t48 * (-t1064 + t652
     # * t1065) - t667 * t1071) * t142 / 0.1440D4 - t672 * t1065 * t74 *
     # t193 / 0.8D1 + (0.90D2 * t4 * t7 * (-t1064 + t680 * t1065) + t107
     #3) * t142 * t188 / 0.720D3
      t1110 = FJET(XB1, XB2, s, t618, 0.0D0, -t621, 0.0D0, 0.0D0, t1109)
      t1112 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0
     #.0D0, 0.0D0, 0.0D0)
      t1114 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0
     #.0D0, 0.0D0, 0.0D0)
      t1131 = -(0.90D2 * t4 * t7 * (t714 * t1112 - t726 * t1114) - 0.180
     #D3 * t638 * t733 * t1114) * t74 * t142 / 0.1440D4 - t741 * t1114 *
     # t74 * t193 / 0.8D1
      t1132 = FJET(XB1, XB2, s, t704, 0.0D0, -t709, 0.0D0, 0.0D0, t1131)
      t1134 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0
     #.0D0, t349, -t829)
      t1135 = t835 * t1134
      t1140 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0
     #.0D0, t349, -t829)
      t1153 = -t831 * t1135 * t839 / 0.8D1 + (0.90D2 * t4 * t7 * (t849 *
     # t1135 - t851 * t1140) + 0.180D3 * t638 * t830 * t1135) * t142 * t
     #188 / 0.720D3
      t1154 = FJET(XB1, XB2, s, t824, t349, -t822, 0.0D0, -t829, t1153)
      t1157 = t349 * t693 * t702
      t1158 = t693 * x1
      t1159 = t693 * t352
      t1162 = Sqrt(t377 * t619 * t19)
      t1163 = t26 * t1162
      t1164 = 0.2D1 * t1163
      t1169 = t351 * x2 * (t373 - t374 + t693 - t1158 + t1159 - 0.1D1 + 
     #t1164) * t354 * t702
      t1173 = t19 * s * t1 * x1 * t702
      t1175 = 0.2D1 * t1163 * x2
      t1176 = 0.1D1 - x1 + t352 - x2 + t832 - t833 - x3 + t373 - t374 + 
     #t693 - t1158 + t1159 + t119 + t1175
      t1179 = t351 * t1176 * t354 * t702
      t1202 = 0.1D1 + 0.3D1 * t374 - x2 + t626 - t693 + t119 + x3 - 0.4D
     #1 * t1159 + x3 * t14 * t832 + 0.4D1 * t166 * t626 - 0.2D1 * t166 *
     # t14 * x2 + 0.3D1 * t119 * t352 - t119 * t14 * x1 - 0.2D1 * t119 *
     # t165 * z + t119 * t165 * t14 - 0.2D1 * t1163 * t352 - 0.2D1 * t11
     #63 * t832
      t1221 = -0.2D1 * t1163 * t626 + 0.3D1 * t1158 - t833 - x1 - t712 +
     # t1175 + 0.2D1 * t1163 * x1 + t119 * t165 - 0.4D1 * t166 * z - 0.2
     #D1 * t166 * x2 + 0.2D1 * t166 * t14 - 0.2D1 * t119 * x1 + t352 - 0
     #.3D1 * t373 + t832 - t1164 + 0.2D1 * t166 + 0.2D1 * t1163 * t833
      t1223 = 0.1D1 / (t1202 + t1221)
      t1224 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t1169, -t1179,
     # t1157, -t1173, -t829)
      t1226 = t1223 * t1224 * t839
      t1229 = FJET(XB1, XB2, s, t1157, t1169, -t1173, -t1179, -t829, -t8
     #31 * t1226 / 0.8D1)
      t1231 = t48 * t353
      t1235 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t1169, -t1179,
     # t1157, -t1173, -t829)
      t1237 = t1223 * t1235 * t839
      t1240 = FJET(XB1, XB2, s, t1169, t1157, -t1179, -t1173, -t829, -t8
     #31 * t1237 / 0.8D1)
      t1245 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 
     #0.0D0, t349, 0.0D0)
      t1246 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 
     #0.0D0, t349, 0.0D0)
      t1256 = -t1246 - t1246 * t383
      t1274 = t48 * t1246
      t1283 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t351, 
     #0.0D0, t349, 0.0D0)
      t1298 = (0.90D2 * t4 * t7 * (-(t1245 - t370 * t1246) * t383 + t360
     # * t1246 - t1245) - 0.180D3 * t47 * t48 * t1256) * t74 * t188 / 0.
     #1440D4 + t191 * t1256 * t74 * t193 / 0.8D1 + (0.90D2 * t4 * t7 * (
     #t406 * t1246 - t1245) + 0.180D3 * t47 * t1274) * t142 * t188 / 0.7
     #20D3 - (0.90D2 * t4 * t7 * (t424 * t1246 / 0.2D1 + t1283 - t423 * 
     #t1245) - 0.180D3 * t47 * t48 * (-t423 * t1246 + t1245) + t64 * t12
     #74) * t188 / 0.1440D4
      t1299 = FJET(XB1, XB2, s, -t351, t349, 0.0D0, 0.0D0, 0.0D0, t1298)
      t1301 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0
     #.0D0, 0.0D0, 0.0D0)
      t1302 = t628 * t1301
      t1303 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0
     #.0D0, 0.0D0, 0.0D0)
      t1309 = t639 * t1303
      t1311 = 0.180D3 * t638 * t1309
      t1317 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, t618, -t621, 0
     #.0D0, 0.0D0, 0.0D0)
      t1347 = -(0.90D2 * t4 * t7 * (t1302 - t629 * t1303) - t1311) * t74
     # * t142 / 0.1440D4 + (0.90D2 * t4 * t7 * (t652 * t1301 - t628 * t1
     #317 - t655 * t1303 / 0.2D1) - 0.180D3 * t47 * t48 * (-t1302 + t652
     # * t1303) - t667 * t1309) * t142 / 0.1440D4 - t672 * t1303 * t74 *
     # t193 / 0.8D1 + (0.90D2 * t4 * t7 * (-t1302 + t680 * t1303) + t131
     #1) * t142 * t188 / 0.720D3
      t1348 = FJET(XB1, XB2, s, -t621, 0.0D0, t618, 0.0D0, 0.0D0, t1347)
      t1350 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0
     #.0D0, 0.0D0, 0.0D0)
      t1352 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t709, t704, 0
     #.0D0, 0.0D0, 0.0D0)
      t1369 = -(0.90D2 * t4 * t7 * (t714 * t1350 - t726 * t1352) - 0.180
     #D3 * t638 * t733 * t1352) * t74 * t142 / 0.1440D4 - t741 * t1352 *
     # t74 * t193 / 0.8D1
      t1370 = FJET(XB1, XB2, s, -t709, 0.0D0, t704, 0.0D0, 0.0D0, t1369)
      t1372 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0
     #.0D0, t349, -t829)
      t1373 = t835 * t1372
      t1378 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t822, t824, 0
     #.0D0, t349, -t829)
      t1391 = -t831 * t1373 * t839 / 0.8D1 + (0.90D2 * t4 * t7 * (t849 *
     # t1373 - t851 * t1378) + 0.180D3 * t638 * t830 * t1373) * t142 * t
     #188 / 0.720D3
      t1392 = FJET(XB1, XB2, s, -t822, 0.0D0, t824, t349, -t829, t1391)
      t1394 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t1169, -t1179,
     # t1157, -t1173, -t829)
      t1396 = t1223 * t1394 * t839
      t1399 = FJET(XB1, XB2, s, -t1173, -t1179, t1157, t1169, -t829, -t8
     #31 * t1396 / 0.8D1)
      t1404 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t1169, -t1179,
     # t1157, -t1173, -t829)
      t1406 = t1223 * t1404 * t839
      t1409 = FJET(XB1, XB2, s, -t1179, -t1173, t1169, t1157, -t829, -t8
     #31 * t1406 / 0.8D1)
      t1414 = t1061 * t1060 + t1110 * t1109 + t1132 * t1131 + t1154 * t1
     #153 - t1229 * pi * t1231 * t1226 / 0.8D1 - t1240 * pi * t1231 * t1
     #237 / 0.8D1 + t1299 * t1298 + t1348 * t1347 + t1370 * t1369 + t139
     #2 * t1391 - t1399 * pi * t1231 * t1396 / 0.8D1 - t1409 * pi * t123
     #1 * t1406 / 0.8D1
      rrgq2qght7s1e0 = t1006 + t1414

      end function



      doubleprecision function rrgq2qght7s1em1
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t24 = log(-0.4D1 * t13 * t18 / t19)
      t25 = cos(t10)
      t27 = Sqrt(-x3 * t19)
      t31 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t25 * t27)
      t35 = log(0.4D1 * t13 * t18)
      t36 = t24 * t31 + t35
      t40 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t41 = t7 * t40
      t44 = pi * lh
      t45 = t3 * t7
      t48 = 0.180D3 * t44 * t45 * t8
      t50 = -t31 - 0.1D1
      t53 = 0.1D1 / x3
      t60 = log(0.4D1 * t15 * t12 * t17)
      t61 = t60 * pi
      t64 = (-0.180D3 * t44 - 0.90D2 * t61) * t3
      t69 = t60 ** 2
      t72 = lh ** 2
      t74 = pi ** 2
      t79 = (0.180D3 * lh * t61 + 0.45D2 * t69 * pi + pi * (0.180D3 * t7
     #2 - 0.30D2 * t74)) * t3
      t82 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t86 = t4 * t7
      t88 = -t8 - t8 * t31
      t90 = 0.1D1 / x2
      t94 = x2 ** 2
      t95 = t94 * t12
      t98 = log(0.4D1 * t95 * t18)
      t108 = 0.1D1 / x1
      t112 = x1 ** 2
      t113 = t112 * t12
      t116 = log(0.4D1 * t113 * t18)
      t130 = (-0.90D2 * t4 * t9 * t36 + (-0.90D2 * t4 * t41 + t48) * t50
     #) * t53 / 0.2880D4 + t64 * t41 / 0.2880D4 + t79 * t9 / 0.2880D4 + 
     #t4 * t7 * t82 / 0.32D2 - t86 * t88 * t53 * t90 / 0.16D2 + (0.90D2 
     #* t4 * t7 * (-t98 * t8 + t40) - t48) * t90 / 0.1440D4 + t86 * t8 *
     # t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (-t40 + t116 * t8) + t48
     #) * t108 / 0.1440D4 - t86 * t88 * t53 * t108 / 0.16D2
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t130)
      t133 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t134 = t7 * t133
      t138 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t139 = t7 * t138
      t144 = 0.180D3 * t44 * t45 * t133
      t154 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t159 = -t133 - t133 * t31
      t189 = (-0.90D2 * t4 * t134 * t36 + (-0.90D2 * t4 * t139 + t144) *
     # t50) * t53 / 0.2880D4 + t64 * t139 / 0.2880D4 + t79 * t134 / 0.28
     #80D4 + t4 * t7 * t154 / 0.32D2 - t86 * t159 * t53 * t90 / 0.16D2 +
     # (0.90D2 * t4 * t7 * (t138 - t98 * t133) - t144) * t90 / 0.1440D4 
     #+ t86 * t133 * t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (-t138 + t
     #116 * t133) + t144) * t108 / 0.1440D4 - t86 * t159 * t53 * t108 / 
     #0.16D2
      t190 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t189)
      t192 = t2 * x1
      t193 = -0.1D1 + x1
      t194 = t2 * t193
      t195 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t201 = x1 * z
      t202 = 0.1D1 - x1 + t201
      t203 = 0.1D1 / t202
      t205 = t193 ** 2
      t209 = log(0.4D1 * t113 * t15 * t17 * t203 * t205)
      t211 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t222 = x3 * x1
      t228 = Sqrt(-x3 * t202 * t19)
      t232 = 0.1D1 / (-0.2D1 * t222 * z + 0.2D1 * t222 - 0.1D1 + 0.2D1 *
     # t25 * t228 - x3)
      t239 = -t86 * t195 * t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (-t2
     #09 * t195 + t211) - 0.180D3 * t44 * t45 * t195) * t108 / 0.1440D4 
     #+ t86 * (-t195 - t195 * t232) * t53 * t108 / 0.16D2
      t240 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t192, -t194, 0.0D0, t239)
      t242 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t247 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t265 = -t86 * t242 * t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (t24
     #7 - t209 * t242) - 0.180D3 * t44 * t45 * t242) * t108 / 0.1440D4 +
     # t86 * (-t242 - t242 * t232) * t53 * t108 / 0.16D2
      t266 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t194, t192, 0.0D0, t265)
      t268 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t269 = t7 * t268
      t273 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t274 = t7 * t273
      t279 = 0.180D3 * t44 * t45 * t268
      t289 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t294 = -t268 - t268 * t31
      t324 = (-0.90D2 * t4 * t269 * t36 + (-0.90D2 * t4 * t274 + t279) *
     # t50) * t53 / 0.2880D4 + t64 * t274 / 0.2880D4 + t79 * t269 / 0.28
     #80D4 + t4 * t7 * t289 / 0.32D2 - t86 * t294 * t53 * t90 / 0.16D2 +
     # (0.90D2 * t4 * t7 * (-t98 * t268 + t273) - t279) * t90 / 0.1440D4
     # + t86 * t268 * t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (-t273 + 
     #t116 * t268) + t279) * t108 / 0.1440D4 - t86 * t294 * t53 * t108 /
     # 0.16D2
      t325 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t324)
      t328 = x2 * s * t1
      t329 = -0.1D1 + x2
      t330 = t329 * s
      t331 = t330 * t1
      t332 = x2 * z
      t334 = 0.1D1 / (0.1D1 - x2 + t332)
      t335 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t336 = t334 * t335
      t337 = t53 * t90
      t341 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t346 = log(-0.4D1 * t95 * t18 * t329)
      t347 = t346 * t334
      t353 = t44 * t3
      t354 = t7 * t334
      t361 = t90 * t108
      t365 = -t86 * t336 * t337 / 0.16D2 + (0.90D2 * t4 * t7 * (-t334 * 
     #t341 + t347 * t335) + 0.180D3 * t353 * t354 * t335) * t90 / 0.1440
     #D4 - t86 * t336 * t361 / 0.8D1
      t366 = FJET(XB1, XB2, s, 0.0D0, t328, 0.0D0, -t331, 0.0D0, t365)
      t368 = x2 * x3
      t369 = t94 * x3
      t372 = Sqrt(x3 * t329 * t19)
      t373 = t25 * t372
      t375 = 0.2D1 * t373 * x2
      t378 = 0.1D1 / (0.1D1 - x3 + t368)
      t380 = t2 * (0.1D1 - x3 - x2 + t368 + t369 + t375) * t378
      t381 = 0.2D1 * t373
      t385 = t2 * x2 * (-0.1D1 + t368 + t381) * t378
      t390 = 0.1D1 / (-0.1D1 - t375 + x2 - x3 + 0.2D1 * t373 * t332 + t3
     #81 + t368 - t332 - t369 + t369 * z)
      t391 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t385, t380, 0.
     #0D0, 0.0D0, 0.0D0)
      t393 = t390 * t391 * t337
      t396 = FJET(XB1, XB2, s, 0.0D0, t380, 0.0D0, -t385, 0.0D0, -t86 * 
     #t393 / 0.16D2)
      t401 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t402 = t334 * t401
      t406 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t422 = -t86 * t402 * t337 / 0.16D2 + (0.90D2 * t4 * t7 * (-t334 * 
     #t406 + t347 * t401) + 0.180D3 * t353 * t354 * t401) * t90 / 0.1440
     #D4 - t86 * t402 * t361 / 0.8D1
      t423 = FJET(XB1, XB2, s, 0.0D0, -t331, 0.0D0, t328, 0.0D0, t422)
      t425 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t385, t380, 0.
     #0D0, 0.0D0, 0.0D0)
      t427 = t390 * t425 * t337
      t430 = FJET(XB1, XB2, s, 0.0D0, -t385, 0.0D0, t380, 0.0D0, -t86 * 
     #t427 / 0.16D2)
      t437 = t2 * t193 * x2 * t203
      t439 = t330 * t1 * t193
      t444 = s * t16 * x2 * x1 * t193 * t203
      t446 = t4 * t7 * t202
      t447 = x2 * x1
      t450 = 0.1D1 / (-0.1D1 - t201 + x1 + x2 + t447 * z - t447 - t332)
      t451 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t437, t439, 0.
     #0D0, t192, -t444)
      t456 = FJET(XB1, XB2, s, 0.0D0, -t437, t192, t439, -t444, -t446 * 
     #t450 * t451 * t361 / 0.8D1)
      t459 = t202 * t450
      t465 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t466 = t7 * t465
      t469 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t473 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t474 = t7 * t473
      t482 = 0.180D3 * t44 * t45 * t473
      t489 = -t473 - t473 * t31
      t521 = t64 * t466 / 0.2880D4 + t4 * t7 * t469 / 0.32D2 + (-0.90D2 
     #* t4 * t474 * t36 + (-0.90D2 * t4 * t466 + t482) * t50) * t53 / 0.
     #2880D4 - t86 * t489 * t53 * t90 / 0.16D2 + (0.90D2 * t4 * t7 * (t4
     #65 - t98 * t473) - t482) * t90 / 0.1440D4 + t79 * t474 / 0.2880D4 
     #+ t86 * t473 * t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (t116 * t4
     #73 - t465) + t482) * t108 / 0.1440D4 - t86 * t489 * t53 * t108 / 0
     #.16D2
      t522 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t521)
      t524 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t437, t439, 0.
     #0D0, t192, -t444)
      t529 = FJET(XB1, XB2, s, t192, t439, 0.0D0, -t437, -t444, -t446 * 
     #t450 * t524 * t361 / 0.8D1)
      t537 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t542 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t560 = -t86 * t537 * t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (t54
     #2 - t209 * t537) - 0.180D3 * t44 * t45 * t537) * t108 / 0.1440D4 +
     # t86 * (-t537 - t537 * t232) * t53 * t108 / 0.16D2
      t561 = FJET(XB1, XB2, s, t192, -t194, 0.0D0, 0.0D0, 0.0D0, t560)
      t563 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t564 = t334 * t563
      t568 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t584 = -t86 * t564 * t337 / 0.16D2 + (0.90D2 * t4 * t7 * (-t334 * 
     #t568 + t347 * t563) + 0.180D3 * t353 * t354 * t563) * t90 / 0.1440
     #D4 - t86 * t564 * t361 / 0.8D1
      t585 = FJET(XB1, XB2, s, t328, 0.0D0, -t331, 0.0D0, 0.0D0, t584)
      t587 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t385, t380, 0.
     #0D0, 0.0D0, 0.0D0)
      t589 = t390 * t587 * t337
      t592 = FJET(XB1, XB2, s, t380, 0.0D0, -t385, 0.0D0, 0.0D0, -t86 * 
     #t589 / 0.16D2)
      t597 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t437, t439, 0.
     #0D0, t192, -t444)
      t602 = FJET(XB1, XB2, s, t439, t192, -t437, 0.0D0, -t444, -t446 * 
     #t450 * t597 * t361 / 0.8D1)
      t610 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t616 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t194, 0
     #.0D0, t192, 0.0D0)
      t633 = -t86 * t610 * t90 * t108 / 0.8D1 - (0.90D2 * t4 * t7 * (-t2
     #09 * t610 + t616) - 0.180D3 * t44 * t45 * t610) * t108 / 0.1440D4 
     #+ t86 * (-t610 - t610 * t232) * t53 * t108 / 0.16D2
      t634 = FJET(XB1, XB2, s, -t194, t192, 0.0D0, 0.0D0, 0.0D0, t633)
      t636 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t637 = t334 * t636
      t641 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t328, -t331, 0.
     #0D0, 0.0D0, 0.0D0)
      t657 = -t86 * t637 * t337 / 0.16D2 + (0.90D2 * t4 * t7 * (-t334 * 
     #t641 + t347 * t636) + 0.180D3 * t353 * t354 * t636) * t90 / 0.1440
     #D4 - t86 * t637 * t361 / 0.8D1
      t658 = FJET(XB1, XB2, s, -t331, 0.0D0, t328, 0.0D0, 0.0D0, t657)
      t660 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t385, t380, 0.
     #0D0, 0.0D0, 0.0D0)
      t662 = t390 * t660 * t337
      t665 = FJET(XB1, XB2, s, -t385, 0.0D0, t380, 0.0D0, 0.0D0, -t86 * 
     #t662 / 0.16D2)
      t670 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t437, t439, 0.
     #0D0, t192, -t444)
      t675 = FJET(XB1, XB2, s, -t437, 0.0D0, t439, t192, -t444, -t446 * 
     #t450 * t670 * t361 / 0.8D1)
      rrgq2qght7s1em1 = t131 * t130 + t190 * t189 + t240 * t239 + t266 *
     # t265 + t325 * t324 + t366 * t365 - t396 * pi * t45 * t393 / 0.16D
     #2 + t423 * t422 - t430 * pi * t45 * t427 / 0.16D2 - t456 * pi * t4
     #5 * t459 * t451 * t90 * t108 / 0.8D1 + t522 * t521 - t529 * pi * t
     #45 * t459 * t524 * t90 * t108 / 0.8D1 + t561 * t560 + t585 * t584 
     #- t592 * pi * t45 * t589 / 0.16D2 - t602 * pi * t45 * t459 * t597 
     #* t90 * t108 / 0.8D1 + t634 * t633 + t658 * t657 - t665 * pi * t45
     # * t662 / 0.16D2 - t675 * pi * t45 * t459 * t670 * t90 * t108 / 0.
     #8D1

      end function



      doubleprecision function rrgq2qght7s1em2
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = cos(t10)
      t14 = Sqrt(-x3 * (-0.1D1 + x3))
      t19 = -0.1D1 / (-0.1D1 - x3 + 0.2D1 * t11 * t14) - 0.1D1
      t21 = 0.1D1 / x3
      t25 = t7 * t9
      t26 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t34 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t40 = z ** 2
      t42 = Sin(t10)
      t43 = t42 ** 2
      t45 = t1 ** 2
      t46 = t45 ** 2
      t49 = log(0.4D1 / t40 * t43 * t46)
      t53 = (-0.180D3 * pi * lh - 0.90D2 * t49 * pi) * t3
      t56 = -t8 * t9 * t19 * t21 / 0.32D2 + t4 * t25 * t26 / 0.16D2 + t4
     # * t25 * t30 / 0.16D2 + t4 * t7 * t34 / 0.32D2 + t53 * t25 / 0.288
     #0D4
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t56)
      t59 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t64 = t7 * t59
      t71 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t77 = -t8 * t59 * t19 * t21 / 0.32D2 + t4 * t64 * t26 / 0.16D2 + t
     #4 * t64 * t30 / 0.16D2 + t4 * t7 * t71 / 0.32D2 + t53 * t64 / 0.28
     #80D4
      t78 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t77)
      t80 = t2 * x1
      t82 = t2 * (-0.1D1 + x1)
      t83 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.0
     #D0, t80, 0.0D0)
      t85 = t7 * t83 * t30
      t88 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t80, -t82, 0.0D0, -t4 * t85 
     #/ 0.16D2)
      t93 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.0
     #D0, t80, 0.0D0)
      t95 = t7 * t93 * t30
      t98 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t82, t80, 0.0D0, -t4 * t95 
     #/ 0.16D2)
      t103 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t108 = t7 * t103
      t115 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t121 = -t8 * t103 * t19 * t21 / 0.32D2 + t4 * t108 * t26 / 0.16D2 
     #+ t4 * t108 * t30 / 0.16D2 + t4 * t7 * t115 / 0.32D2 + t53 * t108 
     #/ 0.2880D4
      t122 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t125 = x2 * s * t1
      t128 = (-0.1D1 + x2) * s * t1
      t131 = 0.1D1 / (0.1D1 - x2 + x2 * z)
      t132 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t137 = FJET(XB1, XB2, s, 0.0D0, t125, 0.0D0, -t128, 0.0D0, -t8 * t
     #131 * t132 * t26 / 0.16D2)
      t140 = t7 * t131
      t145 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t150 = FJET(XB1, XB2, s, 0.0D0, -t128, 0.0D0, t125, 0.0D0, -t8 * t
     #131 * t145 * t26 / 0.16D2)
      t157 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t162 = t7 * t157
      t169 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t175 = -t8 * t157 * t19 * t21 / 0.32D2 + t4 * t162 * t26 / 0.16D2 
     #+ t4 * t162 * t30 / 0.16D2 + t4 * t7 * t169 / 0.32D2 + t53 * t162 
     #/ 0.2880D4
      t176 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t175)
      t178 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.
     #0D0, t80, 0.0D0)
      t180 = t7 * t178 * t30
      t183 = FJET(XB1, XB2, s, t80, -t82, 0.0D0, 0.0D0, 0.0D0, -t4 * t18
     #0 / 0.16D2)
      t188 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t193 = FJET(XB1, XB2, s, t125, 0.0D0, -t128, 0.0D0, 0.0D0, -t8 * t
     #131 * t188 * t26 / 0.16D2)
      t200 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t205 = FJET(XB1, XB2, s, -t128, 0.0D0, t125, 0.0D0, 0.0D0, -t8 * t
     #131 * t200 * t26 / 0.16D2)
      t212 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.
     #0D0, t80, 0.0D0)
      t214 = t7 * t212 * t30
      t217 = FJET(XB1, XB2, s, -t82, t80, 0.0D0, 0.0D0, 0.0D0, -t4 * t21
     #4 / 0.16D2)
      rrgq2qght7s1em2 = t57 * t56 + t78 * t77 - t88 * pi * t3 * t85 / 0.
     #16D2 - t98 * pi * t3 * t95 / 0.16D2 + t122 * t121 - t137 * pi * t3
     # * t140 * t132 * t26 / 0.16D2 - t150 * pi * t3 * t140 * t145 * t26
     # / 0.16D2 + t176 * t175 - t183 * pi * t3 * t180 / 0.16D2 - t193 * 
     #pi * t3 * t140 * t188 * t26 / 0.16D2 - t205 * pi * t3 * t140 * t20
     #0 * t26 / 0.16D2 - t217 * pi * t3 * t214 / 0.16D2

      end function



      doubleprecision function rrgq2qght7s1em3
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t7 * 
     #t8 / 0.32D2)
      t14 = t3 * t7
      t17 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t7 * 
     #t17 / 0.32D2)
      t25 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t29 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t25 / 0.32D2)
      t33 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t37 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t33 / 0.32D2)
      rrgq2qght7s1em3 = t12 * pi * t14 * t8 / 0.32D2 + t21 * pi * t14 * 
     #t17 / 0.32D2 + t29 * pi * t14 * t25 / 0.32D2 + t37 * pi * t14 * t3
     #3 / 0.32D2

      end function



      doubleprecision function rrgq2qght7s1em4
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght7s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght7s2e1
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t3 = pi * lh
      t4 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = t8 * t9
      t13 = lh ** 2
      t15 = pi ** 2
      t17 = 0.180D3 * t13 - 0.30D2 * t15
      t18 = pi * t17
      t19 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = t8 * t19
      t22 = pi * t4
      t23 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = t7 * t23
      t27 = -0.180D3 * t3 * t10 + t18 * t20 + 0.90D2 * t22 * t24
      t28 = z ** 2
      t30 = 0.1D1 / t28 / z
      t31 = x3 * t30
      t32 = x4 * pi
      t33 = Sin(t32)
      t34 = t33 ** 2
      t35 = t1 ** 2
      t36 = t35 ** 2
      t37 = t34 * t36
      t38 = -0.1D1 + x3
      t39 = 0.1D1 / t38
      t40 = t37 * t39
      t43 = log(-0.4D1 * t31 * t40)
      t44 = x3 * z
      t45 = 0.2D1 * t44
      t46 = cos(t32)
      t48 = Sqrt(-t44 * t38)
      t52 = 0.1D1 / (-t45 - 0.1D1 + 0.2D1 * t46 * t48 + x3)
      t56 = log(0.4D1 * t31 * t37)
      t57 = -t43 * t52 - t56
      t59 = t7 * t19
      t60 = t56 ** 2
      t62 = t43 ** 2
      t66 = -t60 * t56 / 0.6D1 - t62 * t43 * t52 / 0.6D1
      t71 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t72 = t7 * t71
      t80 = 0.60D2 * lh * t15 - 0.240D3 * zeta3 - 0.120D3 * t13 * lh
      t81 = pi * t80
      t82 = t81 * t20
      t87 = t52 + 0.1D1
      t89 = t7 * t9
      t94 = 0.90D2 * t22 * t89 - 0.180D3 * t3 * t20
      t97 = t62 * t52 / 0.2D1 + t60 / 0.2D1
      t100 = 0.1D1 / x3
      t103 = x2 ** 2
      t104 = x3 * t103
      t105 = t104 * t30
      t108 = log(-0.4D1 * t105 * t40)
      t110 = t108 ** 2
      t115 = t30 * t34
      t116 = t115 * t36
      t119 = log(0.4D1 * t104 * t116)
      t120 = t119 ** 2
      t123 = -0.1D1 + x2
      t124 = t37 * t123
      t127 = log(-0.4D1 * t105 * t124)
      t130 = t127 ** 2
      t146 = t18 * t4
      t147 = t59 * t52
      t151 = 0.1D1 / x2
      t154 = t30 * t103
      t157 = log(0.4D1 * t154 * t37)
      t158 = t157 ** 2
      t161 = log(-0.4D1 * t154 * t124)
      t162 = t161 ** 2
      t164 = t158 / 0.2D1 - t162 / 0.2D1
      t169 = t162 * t161 / 0.6D1 - t158 * t157 / 0.6D1
      t173 = -t157 + t161
      t179 = log(0.4D1 * t116)
      t180 = t179 ** 2
      t181 = t180 * pi
      t185 = t180 * t179 * pi
      t187 = t179 * pi
      t190 = (-0.90D2 * t181 * lh + t81 - 0.15D2 * t185 - t187 * t17) * 
     #t4
      t197 = (0.180D3 * t187 * lh + 0.45D2 * t181 + t18) * t4
      t203 = (-0.180D3 * t3 - 0.90D2 * t187) * t4
      t206 = rrgq2qgh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t210 = x1 ** 2
      t211 = x3 * t210
      t214 = log(0.4D1 * t211 * t116)
      t216 = t214 ** 2
      t219 = t211 * t34
      t220 = t30 * t36
      t224 = log(-0.4D1 * t219 * t220 * t39)
      t226 = t224 ** 2
      t249 = 0.1D1 / x1
      t252 = t210 * t34
      t255 = log(0.4D1 * t252 * t220)
      t260 = t255 ** 2
      t261 = t260 * t255
      t281 = t104 * t210
      t282 = t36 * t123
      t286 = log(-0.4D1 * t281 * t115 * t282)
      t290 = log(0.4D1 * t281 * t116)
      t296 = log(-0.4D1 * t281 * t115 * t36 * t39)
      t304 = t3 * t4
      t309 = t151 * t249
      t312 = t103 * t210
      t313 = t312 * t34
      t317 = log(-0.4D1 * t313 * t220 * t123)
      t321 = log(0.4D1 * t312 * t116)
      t322 = t321 ** 2
      t325 = t317 ** 2
      t348 = t15 ** 2
      t349 = t13 ** 2
      t357 = t180 ** 2
      t361 = (0.30D2 * t185 * lh + t181 * t17 / 0.2D1 - t187 * t80 + pi 
     #* (t348 + 0.60D2 * t349 + 0.480D3 * lh * zeta3 - 0.60D2 * t13 * t1
     #5) + 0.15D2 / 0.4D1 * t357 * pi) * t4
      t364 = (t27 * t57 + 0.90D2 * t22 * t59 * t66 + (t18 * t10 + 0.90D2
     # * t22 * t72 + t82 - 0.180D3 * t3 * t8 * t23) * t87 + t94 * t97) *
     # t100 / 0.2880D4 + (0.90D2 * t22 * t7 * ((t23 - t108 * t9 + t110 *
     # t19 / 0.2D1) * t52 + t120 * t19 / 0.2D1 + t127 * t9 - t119 * t9 -
     # t130 * t19 / 0.2D1) - 0.180D3 * t3 * t8 * (-t119 * t19 + (t9 - t1
     #08 * t19) * t52 + t127 * t19) + t146 * t147) * t100 * t151 / 0.144
     #0D4 + (t94 * t164 + 0.90D2 * t22 * t59 * t169 + t27 * t173) * t151
     # / 0.1440D4 + t190 * t89 / 0.2880D4 + t197 * t24 / 0.2880D4 + t203
     # * t72 / 0.2880D4 + t22 * t7 * t206 / 0.32D2 + (0.90D2 * t22 * t7 
     #* (t23 - t214 * t9 + t216 * t19 / 0.2D1 + (t23 - t224 * t9 + t226 
     #* t19 / 0.2D1) * t52) - 0.180D3 * t3 * t8 * (t9 - t214 * t19 + (t9
     # - t224 * t19) * t52) + t18 * t8 * (t19 + t19 * t52)) * t100 * t24
     #9 / 0.1440D4 + (t18 * t8 * (t9 - t255 * t19) + 0.90D2 * t22 * t7 *
     # (-t261 * t19 / 0.6D1 - t255 * t23 + t260 * t9 / 0.2D1 + t71) + t8
     #2 - 0.180D3 * t3 * t8 * (t23 - t255 * t9 + t260 * t19 / 0.2D1)) * 
     #t249 / 0.1440D4 + (0.90D2 * t22 * t7 * (t286 * t19 - t290 * t19 + 
     #(t9 - t296 * t19) * t52) - 0.180D3 * t304 * t147) * t100 * t309 / 
     #0.720D3 + (0.90D2 * t22 * t7 * (t317 * t9 + t322 * t19 / 0.2D1 - t
     #325 * t19 / 0.2D1 - t321 * t9) - 0.180D3 * t3 * t8 * (t317 * t19 -
     # t321 * t19)) * t151 * t249 / 0.720D3 + t361 * t59 / 0.2880D4
      t365 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t364)
      t367 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t368 = t7 * t367
      t371 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t372 = t8 * t371
      t375 = t8 * t367
      t377 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t378 = t7 * t377
      t381 = -0.180D3 * t3 * t372 + t18 * t375 + 0.90D2 * t22 * t378
      t387 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t388 = t7 * t387
      t391 = t81 * t375
      t397 = t7 * t371
      t402 = 0.90D2 * t22 * t397 - 0.180D3 * t3 * t375
      t431 = t368 * t52
      t537 = rrgq2qgh73J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t541 = t361 * t368 / 0.2880D4 + (t381 * t57 + 0.90D2 * t22 * t368 
     #* t66 + (t18 * t372 + 0.90D2 * t22 * t388 + t391 - 0.180D3 * t3 * 
     #t8 * t377) * t87 + t402 * t97) * t100 / 0.2880D4 + (0.90D2 * t22 *
     # t7 * ((t110 * t367 / 0.2D1 - t108 * t371 + t377) * t52 + t127 * t
     #371 - t119 * t371 - t130 * t367 / 0.2D1 + t120 * t367 / 0.2D1) - 0
     #.180D3 * t3 * t8 * (-t119 * t367 + (t371 - t108 * t367) * t52 + t1
     #27 * t367) + t146 * t431) * t100 * t151 / 0.1440D4 + (t402 * t164 
     #+ 0.90D2 * t22 * t368 * t169 + t381 * t173) * t151 / 0.1440D4 + t1
     #90 * t397 / 0.2880D4 + t197 * t378 / 0.2880D4 + t203 * t388 / 0.28
     #80D4 + (0.90D2 * t22 * t7 * (t216 * t367 / 0.2D1 + t377 - t214 * t
     #371 + (t226 * t367 / 0.2D1 - t224 * t371 + t377) * t52) - 0.180D3 
     #* t3 * t8 * (-t214 * t367 + t371 + (t371 - t224 * t367) * t52) + t
     #18 * t8 * (t367 + t367 * t52)) * t100 * t249 / 0.1440D4 + (t18 * t
     #8 * (t371 - t255 * t367) + 0.90D2 * t22 * t7 * (-t261 * t367 / 0.6
     #D1 + t387 + t260 * t371 / 0.2D1 - t255 * t377) + t391 - 0.180D3 * 
     #t3 * t8 * (t260 * t367 / 0.2D1 - t255 * t371 + t377)) * t249 / 0.1
     #440D4 + (0.90D2 * t22 * t7 * (-t290 * t367 + t286 * t367 + (t371 -
     # t296 * t367) * t52) - 0.180D3 * t304 * t431) * t100 * t309 / 0.72
     #0D3 + (0.90D2 * t22 * t7 * (-t325 * t367 / 0.2D1 + t322 * t367 / 0
     #.2D1 + t317 * t371 - t321 * t371) - 0.180D3 * t3 * t8 * (-t321 * t
     #367 + t317 * t367)) * t151 * t249 / 0.720D3 + t22 * t7 * t537 / 0.
     #32D2
      t542 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t541)
      t544 = t2 * x1
      t545 = -0.1D1 + x1
      t546 = t2 * t545
      t547 = 0.1D1 / t28
      t548 = t547 * t36
      t549 = x1 * z
      t550 = -z - x1 + t549
      t551 = 0.1D1 / t550
      t552 = t545 ** 2
      t553 = t551 * t552
      t554 = t548 * t553
      t557 = log(-0.4D1 * t219 * t554)
      t558 = t557 ** 2
      t559 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t562 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t563 = t34 * t547
      t565 = t36 * t551
      t570 = log(0.4D1 * t211 * t563 * t565 * t552 * t39)
      t571 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t573 = t570 ** 2
      t577 = x3 * x1
      t578 = t577 * z
      t581 = x3 * t550
      t583 = Sqrt(t581 * t38)
      t587 = 0.1D1 / (0.2D1 * t578 - 0.2D1 * t577 - t45 + x3 - 0.1D1 + 0
     #.2D1 * t46 * t583)
      t604 = t8 * (-t559 * t587 - t559)
      t610 = t252 * t547
      t611 = t565 * t552
      t614 = log(-0.4D1 * t610 * t611)
      t619 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t620 = t614 ** 2
      t623 = t620 * t614
      t631 = t8 * t559
      t643 = t104 * t252
      t648 = log(0.4D1 * t643 * t548 * t553 * t39)
      t654 = log(-0.4D1 * t643 * t554)
      t668 = log(-0.4D1 * t313 * t554)
      t669 = t668 ** 2
      t687 = (0.90D2 * t22 * t7 * (-t558 * t559 / 0.2D1 - (t562 - t570 *
     # t571 + t573 * t559 / 0.2D1) * t587 + t557 * t571 - t562) - 0.180D
     #3 * t3 * t8 * (-(t571 - t570 * t559) * t587 - t571 + t557 * t559) 
     #+ t18 * t604) * t100 * t249 / 0.1440D4 + (t18 * t8 * (-t571 + t614
     # * t559) + 0.90D2 * t22 * t7 * (-t619 - t620 * t571 / 0.2D1 + t623
     # * t559 / 0.6D1 + t614 * t562) - t81 * t631 - 0.180D3 * t3 * t8 * 
     #(-t562 + t614 * t571 - t620 * t559 / 0.2D1)) * t249 / 0.1440D4 + (
     #0.90D2 * t22 * t7 * (-(t571 - t648 * t559) * t587 + t654 * t559 - 
     #t571) - 0.180D3 * t3 * t604) * t100 * t309 / 0.720D3 + (0.90D2 * t
     #22 * t7 * (-t669 * t559 / 0.2D1 + t668 * t571 - t562) - 0.180D3 * 
     #t3 * t8 * (t668 * t559 - t571) - t18 * t631) * t151 * t249 / 0.720
     #D3
      t688 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t544, -t546, 0.0D0, t687)
      t690 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t693 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t695 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t715 = t8 * (-t690 * t587 - t690)
      t727 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t735 = t8 * t690
      t778 = (0.90D2 * t22 * t7 * (-t558 * t690 / 0.2D1 + t557 * t693 - 
     #t695 - (t573 * t690 / 0.2D1 - t570 * t693 + t695) * t587) - 0.180D
     #3 * t3 * t8 * (-(t693 - t570 * t690) * t587 - t693 + t557 * t690) 
     #+ t18 * t715) * t100 * t249 / 0.1440D4 + (t18 * t8 * (t614 * t690 
     #- t693) + 0.90D2 * t22 * t7 * (t623 * t690 / 0.6D1 - t727 - t620 *
     # t693 / 0.2D1 + t614 * t695) - t81 * t735 - 0.180D3 * t3 * t8 * (-
     #t620 * t690 / 0.2D1 - t695 + t614 * t693)) * t249 / 0.1440D4 + (0.
     #90D2 * t22 * t7 * (t654 * t690 - (t693 - t648 * t690) * t587 - t69
     #3) - 0.180D3 * t3 * t715) * t100 * t309 / 0.720D3 + (0.90D2 * t22 
     #* t7 * (-t669 * t690 / 0.2D1 - t695 + t668 * t693) - 0.180D3 * t3 
     #* t8 * (-t693 + t668 * t690) - t18 * t735) * t151 * t249 / 0.720D3
      t779 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t546, t544, 0.0D0, t778)
      t781 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t782 = t8 * t781
      t785 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t786 = t8 * t785
      t788 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t789 = t7 * t788
      t792 = -0.180D3 * t3 * t782 + t18 * t786 + 0.90D2 * t22 * t789
      t794 = t7 * t785
      t799 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t800 = t7 * t799
      t803 = t81 * t786
      t809 = t7 * t781
      t814 = 0.90D2 * t22 * t809 - 0.180D3 * t3 * t786
      t843 = t794 * t52
      t857 = rrgq2qgh72J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t955 = (t792 * t57 + 0.90D2 * t22 * t794 * t66 + (t18 * t782 + 0.9
     #0D2 * t22 * t800 + t803 - 0.180D3 * t3 * t8 * t788) * t87 + t814 *
     # t97) * t100 / 0.2880D4 + (0.90D2 * t22 * t7 * (t127 * t781 - t119
     # * t781 + (-t108 * t781 + t788 + t110 * t785 / 0.2D1) * t52 + t120
     # * t785 / 0.2D1 - t130 * t785 / 0.2D1) - 0.180D3 * t3 * t8 * ((t78
     #1 - t108 * t785) * t52 - t119 * t785 + t127 * t785) + t146 * t843)
     # * t100 * t151 / 0.1440D4 + (t814 * t164 + 0.90D2 * t22 * t794 * t
     #169 + t792 * t173) * t151 / 0.1440D4 + t22 * t7 * t857 / 0.32D2 + 
     #t361 * t794 / 0.2880D4 + t190 * t809 / 0.2880D4 + t197 * t789 / 0.
     #2880D4 + (0.90D2 * t22 * t7 * (-t214 * t781 + t216 * t785 / 0.2D1 
     #+ (-t224 * t781 + t788 + t226 * t785 / 0.2D1) * t52 + t788) - 0.18
     #0D3 * t3 * t8 * (-t214 * t785 + t781 + (t781 - t224 * t785) * t52)
     # + t18 * t8 * (t785 * t52 + t785)) * t100 * t249 / 0.1440D4 + (t18
     # * t8 * (t781 - t255 * t785) + 0.90D2 * t22 * t7 * (t799 + t260 * 
     #t781 / 0.2D1 - t261 * t785 / 0.6D1 - t255 * t788) + t803 - 0.180D3
     # * t3 * t8 * (t260 * t785 / 0.2D1 - t255 * t781 + t788)) * t249 / 
     #0.1440D4 + (0.90D2 * t22 * t7 * (-t290 * t785 + (t781 - t296 * t78
     #5) * t52 + t286 * t785) - 0.180D3 * t304 * t843) * t100 * t309 / 0
     #.720D3 + (0.90D2 * t22 * t7 * (-t321 * t781 + t322 * t785 / 0.2D1 
     #- t325 * t785 / 0.2D1 + t317 * t781) - 0.180D3 * t3 * t8 * (-t321 
     #* t785 + t317 * t785)) * t151 * t249 / 0.720D3 + t203 * t800 / 0.2
     #880D4
      t956 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t955)
      t958 = x2 * x3
      t959 = 0.1D1 - x3 + t958
      t960 = 0.1D1 / t959
      t961 = t958 * t960
      t962 = t2 * t961
      t964 = t2 * t38 * t960
      t965 = t958 * z
      t966 = t123 * t38
      t968 = Sqrt(t44 * t966)
      t972 = 0.1D1 / (-t45 + t965 - 0.1D1 + 0.2D1 * t46 * t968 + x3)
      t973 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #962, -t964, 0.0D0)
      t974 = t972 * t973
      t975 = t959 ** 2
      t976 = 0.1D1 / t975
      t977 = t966 * t976
      t981 = log(0.4D1 * t643 * t220 * t977)
      t982 = t981 * t972
      t983 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #962, -t964, 0.0D0)
      t989 = t7 * t972
      t990 = t989 * t983
      t1002 = log(0.4D1 * t104 * t115 * t282 * t38 * t976)
      t1003 = t1002 * t972
      t1005 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1007 = t1002 ** 2
      t1008 = t1007 * t972
      t1025 = (0.90D2 * t22 * t7 * (-t974 + t982 * t983) + 0.180D3 * t30
     #4 * t990) * t100 * t309 / 0.720D3 + (0.90D2 * t22 * t7 * (t1003 * 
     #t973 - t972 * t1005 - t1008 * t983 / 0.2D1) - 0.180D3 * t3 * t8 * 
     #(t1003 * t983 - t974) - t146 * t990) * t100 * t151 / 0.1440D4
      t1026 = FJET(XB1, XB2, s, 0.0D0, t962, 0.0D0, -t964, 0.0D0, t1025)
      t1028 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1030 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1033 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1040 = t972 * t1028
      t1045 = t989 * t1030
      t1062 = (0.90D2 * t22 * t7 * (t1003 * t1028 - t1008 * t1030 / 0.2D
     #1 - t972 * t1033) - 0.180D3 * t3 * t8 * (t1003 * t1030 - t1040) - 
     #t146 * t1045) * t100 * t151 / 0.1440D4 + (0.90D2 * t22 * t7 * (-t1
     #040 + t982 * t1030) + 0.180D3 * t304 * t1045) * t100 * t309 / 0.72
     #0D3
      t1063 = FJET(XB1, XB2, s, 0.0D0, -t964, 0.0D0, t962, 0.0D0, t1062)
      t1065 = x2 * x1
      t1067 = t2 * t1065 * t551
      t1070 = t123 * s * t1 * x1
      t1075 = s * t35 * x2 * x1 * t545 * t551
      t1076 = t1065 * z
      t1078 = 0.1D1 / (z + x1 - t549 - t1065 + t1076)
      t1079 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1080 = t1078 * t1079
      t1085 = log(0.4D1 * t643 * t548 * t553 * t123)
      t1086 = t1085 * t1078
      t1087 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1094 = t7 * t1078
      t1096 = t1094 * t1087 * t550
      t1107 = log(0.4D1 * t312 * t563 * t565 * t552 * t123)
      t1108 = t1107 * t1078
      t1110 = t1107 ** 2
      t1111 = t1110 * t1078
      t1114 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1132 = (-0.90D2 * t22 * t7 * (t1080 - t1086 * t1087) * t550 + 0.1
     #80D3 * t304 * t1096) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7
     # * (-t1108 * t1079 + t1111 * t1087 / 0.2D1 + t1078 * t1114) * t550
     # + 0.180D3 * t304 * t7 * (t1080 - t1108 * t1087) * t550 - t146 * t
     #1096) * t151 * t249 / 0.720D3
      t1133 = FJET(XB1, XB2, s, 0.0D0, -t1067, -t546, -t1070, t1075, t11
     #32)
      t1135 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1138 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1139 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1175 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1180 = t8 * t1139
      t1181 = t81 * t1180
      t1201 = t7 * t1139
      t1202 = t1201 * t52
      t1229 = t8 * t1135
      t1233 = t7 * t1138
      t1236 = -0.180D3 * t3 * t1229 + t18 * t1180 + 0.90D2 * t22 * t1233
      t1242 = t7 * t1175
      t1250 = t7 * t1135
      t1255 = 0.90D2 * t22 * t1250 - 0.180D3 * t3 * t1180
      t1303 = rrgq2qgh74J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1309 = (0.90D2 * t22 * t7 * (-t214 * t1135 + (-t224 * t1135 + t11
     #38 + t226 * t1139 / 0.2D1) * t52 + t1138 + t216 * t1139 / 0.2D1) -
     # 0.180D3 * t3 * t8 * (-t214 * t1139 + (t1135 - t224 * t1139) * t52
     # + t1135) + t18 * t8 * (t1139 + t1139 * t52)) * t100 * t249 / 0.14
     #40D4 + (t18 * t8 * (t1135 - t255 * t1139) + 0.90D2 * t22 * t7 * (t
     #260 * t1135 / 0.2D1 - t261 * t1139 / 0.6D1 - t255 * t1138 + t1175)
     # + t1181 - 0.180D3 * t3 * t8 * (t260 * t1139 / 0.2D1 - t255 * t113
     #5 + t1138)) * t249 / 0.1440D4 + (0.90D2 * t22 * t7 * (t286 * t1139
     # - t290 * t1139 + (t1135 - t296 * t1139) * t52) - 0.180D3 * t304 *
     # t1202) * t100 * t309 / 0.720D3 + (0.90D2 * t22 * t7 * (t322 * t11
     #39 / 0.2D1 - t325 * t1139 / 0.2D1 + t317 * t1135 - t321 * t1135) -
     # 0.180D3 * t3 * t8 * (-t321 * t1139 + t317 * t1139)) * t151 * t249
     # / 0.720D3 + (t1236 * t57 + 0.90D2 * t22 * t1201 * t66 + (t18 * t1
     #229 + 0.90D2 * t22 * t1242 + t1181 - 0.180D3 * t3 * t8 * t1138) * 
     #t87 + t1255 * t97) * t100 / 0.2880D4 + (0.90D2 * t22 * t7 * (t120 
     #* t1139 / 0.2D1 + (-t108 * t1135 + t1138 + t110 * t1139 / 0.2D1) *
     # t52 - t130 * t1139 / 0.2D1 + t127 * t1135 - t119 * t1135) - 0.180
     #D3 * t3 * t8 * (-t119 * t1139 + t127 * t1139 + (t1135 - t108 * t11
     #39) * t52) + t146 * t1202) * t100 * t151 / 0.1440D4 + (t1255 * t16
     #4 + 0.90D2 * t22 * t1201 * t169 + t1236 * t173) * t151 / 0.1440D4 
     #+ t197 * t1233 / 0.2880D4 + t203 * t1242 / 0.2880D4 + t190 * t1250
     # / 0.2880D4 + t22 * t7 * t1303 / 0.32D2 + t361 * t1201 / 0.2880D4
      t1310 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1309)
      t1312 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1315 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1316 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1337 = t8 * (-t1312 * t587 - t1312)
      t1348 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1357 = t8 * t1312
      t1400 = (0.90D2 * t22 * t7 * (-t558 * t1312 / 0.2D1 - t1315 - (-t5
     #70 * t1316 + t1315 + t573 * t1312 / 0.2D1) * t587 + t557 * t1316) 
     #- 0.180D3 * t3 * t8 * (-(t1316 - t570 * t1312) * t587 - t1316 + t5
     #57 * t1312) + t18 * t1337) * t100 * t249 / 0.1440D4 + (t18 * t8 * 
     #(-t1316 + t614 * t1312) + 0.90D2 * t22 * t7 * (t614 * t1315 - t134
     #8 + t623 * t1312 / 0.6D1 - t620 * t1316 / 0.2D1) - t81 * t1357 - 0
     #.180D3 * t3 * t8 * (t614 * t1316 - t1315 - t620 * t1312 / 0.2D1)) 
     #* t249 / 0.1440D4 + (0.90D2 * t22 * t7 * (-t1316 - (t1316 - t648 *
     # t1312) * t587 + t654 * t1312) - 0.180D3 * t3 * t1337) * t100 * t3
     #09 / 0.720D3 + (0.90D2 * t22 * t7 * (-t669 * t1312 / 0.2D1 - t1315
     # + t668 * t1316) - 0.180D3 * t3 * t8 * (t668 * t1312 - t1316) - t1
     #8 * t1357) * t151 * t249 / 0.720D3
      t1401 = FJET(XB1, XB2, s, t544, -t546, 0.0D0, 0.0D0, 0.0D0, t1400)
      t1403 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1404 = t972 * t1403
      t1405 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1411 = t989 * t1405
      t1418 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1437 = (0.90D2 * t22 * t7 * (-t1404 + t982 * t1405) + 0.180D3 * t
     #304 * t1411) * t100 * t309 / 0.720D3 + (0.90D2 * t22 * t7 * (-t972
     # * t1418 + t1003 * t1403 - t1008 * t1405 / 0.2D1) - 0.180D3 * t3 *
     # t8 * (t1003 * t1405 - t1404) - t146 * t1411) * t100 * t151 / 0.14
     #40D4
      t1438 = FJET(XB1, XB2, s, t962, 0.0D0, -t964, 0.0D0, 0.0D0, t1437)
      t1443 = t38 * s * t1 * t545 * t960
      t1444 = x2 * z
      t1445 = t958 * x1
      t1446 = t958 * t549
      t1448 = Sqrt(-t581 * t966)
      t1449 = t46 * t1448
      t1452 = z + x1 - t549 - t1444 - t1065 + t1076 - t44 - t577 + t578 
     #+ t965 + t1445 - t1446 + t104 + 0.2D1 * t1449 * x2
      t1455 = t544 * t1452 * t551 * t960
      t1456 = t546 * t961
      t1462 = t544 * x2 * (-t44 - t577 + t578 + t965 + t1445 - t1446 - 0
     #.1D1 + x3 + 0.2D1 * t1449) * t551 * t960
      t1471 = x3 * t28
      t1480 = -0.5D1 * t578 - z + 0.2D1 * t1449 * t1076 + t44 - t1445 + 
     #0.2D1 * t1449 * z + 0.2D1 * t1449 * x1 + t958 * t28 - t281 + 0.4D1
     # * t1471 * x1 + 0.4D1 * t211 * z + 0.2D1 * t211 * x2 - 0.2D1 * t21
     #1 * t28 - x1 + t549
      t1503 = t577 + t1065 - 0.2D1 * t211 - 0.2D1 * t1471 + 0.4D1 * t144
     #6 - 0.3D1 * t1471 * t1065 - 0.4D1 * t211 * t1444 + 0.2D1 * t211 * 
     #t28 * x2 - t104 * t549 + t104 * t28 * x1 + 0.2D1 * t104 * t210 * z
     # - t104 * t210 * t28 - 0.2D1 * t1449 * t549 - 0.2D1 * t1449 * t106
     #5 - t1076
      t1505 = 0.1D1 / (t1480 + t1503)
      t1506 = t550 * t1505
      t1507 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1513 = log(-0.4D1 * t104 * t610 * t611 * t977)
      t1514 = t1513 * t550
      t1515 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1516 = t1505 * t1515
      t1522 = t7 * t550
      t1526 = 0.90D2 * t22 * t7 * (-t1506 * t1507 + t1514 * t1516) + 0.1
     #80D3 * t304 * t1522 * t1516
      t1530 = FJET(XB1, XB2, s, t1443, -t1455, -t1456, t1462, t1075, t15
     #26 * t100 * t309 / 0.720D3)
      t1533 = t100 * t151 * t249
      t1536 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1537 = t1505 * t1536
      t1539 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1548 = 0.90D2 * t22 * t7 * (t1514 * t1537 - t1506 * t1539) + 0.18
     #0D3 * t304 * t1522 * t1537
      t1552 = FJET(XB1, XB2, s, t1462, -t1456, -t1455, t1443, t1075, t15
     #48 * t100 * t309 / 0.720D3)
      t1556 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1558 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1559 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1581 = t8 * (-t1559 * t587 - t1559)
      t1593 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1601 = t8 * t1559
      t1644 = (0.90D2 * t22 * t7 * (-(-t570 * t1556 + t1558 + t573 * t15
     #59 / 0.2D1) * t587 - t1558 - t558 * t1559 / 0.2D1 + t557 * t1556) 
     #- 0.180D3 * t3 * t8 * (-t1556 - (t1556 - t570 * t1559) * t587 + t5
     #57 * t1559) + t18 * t1581) * t100 * t249 / 0.1440D4 + (t18 * t8 * 
     #(-t1556 + t614 * t1559) + 0.90D2 * t22 * t7 * (t623 * t1559 / 0.6D
     #1 - t1593 + t614 * t1558 - t620 * t1556 / 0.2D1) - t81 * t1601 - 0
     #.180D3 * t3 * t8 * (t614 * t1556 - t1558 - t620 * t1559 / 0.2D1)) 
     #* t249 / 0.1440D4 + (0.90D2 * t22 * t7 * (-t1556 - (t1556 - t648 *
     # t1559) * t587 + t654 * t1559) - 0.180D3 * t3 * t1581) * t100 * t3
     #09 / 0.720D3 + (0.90D2 * t22 * t7 * (-t1558 + t668 * t1556 - t669 
     #* t1559 / 0.2D1) - 0.180D3 * t3 * t8 * (t668 * t1559 - t1556) - t1
     #8 * t1601) * t151 * t249 / 0.720D3
      t1645 = FJET(XB1, XB2, s, -t546, t544, 0.0D0, 0.0D0, 0.0D0, t1644)
      t1647 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1648 = t1078 * t1647
      t1649 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1657 = t1094 * t1649 * t550
      t1666 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1684 = (-0.90D2 * t22 * t7 * (t1648 - t1086 * t1649) * t550 + 0.1
     #80D3 * t304 * t1657) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7
     # * (-t1108 * t1647 + t1111 * t1649 / 0.2D1 + t1078 * t1666) * t550
     # + 0.180D3 * t304 * t7 * (t1648 - t1108 * t1649) * t550 - t146 * t
     #1657) * t151 * t249 / 0.720D3
      t1685 = FJET(XB1, XB2, s, -t546, -t1070, 0.0D0, -t1067, t1075, t16
     #84)
      t1687 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1689 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1690 = t972 * t1689
      t1695 = t989 * t1687
      t1702 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1721 = (0.90D2 * t22 * t7 * (t982 * t1687 - t1690) + 0.180D3 * t3
     #04 * t1695) * t100 * t309 / 0.720D3 + (0.90D2 * t22 * t7 * (-t972 
     #* t1702 + t1003 * t1689 - t1008 * t1687 / 0.2D1) - 0.180D3 * t3 * 
     #t8 * (t1003 * t1687 - t1690) - t146 * t1695) * t100 * t151 / 0.144
     #0D4
      t1722 = FJET(XB1, XB2, s, -t964, 0.0D0, t962, 0.0D0, 0.0D0, t1721)
      t1724 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1725 = t1078 * t1724
      t1726 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1734 = t1094 * t1726 * t550
      t1743 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1761 = (-0.90D2 * t22 * t7 * (t1725 - t1086 * t1726) * t550 + 0.1
     #80D3 * t304 * t1734) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7
     # * (-t1108 * t1724 + t1111 * t1726 / 0.2D1 + t1078 * t1743) * t550
     # + 0.180D3 * t304 * t7 * (t1725 - t1108 * t1726) * t550 - t146 * t
     #1734) * t151 * t249 / 0.720D3
      t1762 = FJET(XB1, XB2, s, -t1070, -t546, -t1067, 0.0D0, t1075, t17
     #61)
      t1764 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1765 = t1078 * t1764
      t1766 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1774 = t1094 * t1766 * t550
      t1783 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1801 = (-0.90D2 * t22 * t7 * (t1765 - t1086 * t1766) * t550 + 0.1
     #80D3 * t304 * t1774) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7
     # * (-t1108 * t1764 + t1111 * t1766 / 0.2D1 + t1078 * t1783) * t550
     # + 0.180D3 * t304 * t7 * (t1765 - t1108 * t1766) * t550 - t146 * t
     #1774) * t151 * t249 / 0.720D3
      t1802 = FJET(XB1, XB2, s, -t1067, 0.0D0, -t1070, -t546, t1075, t18
     #01)
      t1804 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1805 = t1505 * t1804
      t1807 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1816 = 0.90D2 * t22 * t7 * (t1514 * t1805 - t1506 * t1807) + 0.18
     #0D3 * t304 * t1522 * t1805
      t1820 = FJET(XB1, XB2, s, -t1455, t1443, t1462, -t1456, t1075, t18
     #16 * t100 * t309 / 0.720D3)
      t1824 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1825 = t1505 * t1824
      t1827 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t1462, -t1455,
     # -t1456, t1443, t1075)
      t1836 = 0.90D2 * t22 * t7 * (t1514 * t1825 - t1506 * t1827) + 0.18
     #0D3 * t304 * t1522 * t1825
      t1840 = FJET(XB1, XB2, s, -t1456, t1462, t1443, -t1455, t1075, t18
     #36 * t100 * t309 / 0.720D3)
      rrgq2qght7s2e1 = t365 * t364 + t542 * t541 + t688 * t687 + t779 * 
     #t778 + t956 * t955 + t1026 * t1025 + t1063 * t1062 + t1133 * t1132
     # + t1310 * t1309 + t1401 * t1400 + t1438 * t1437 + t1530 * t1526 *
     # t1533 / 0.720D3 + t1552 * t1548 * t1533 / 0.720D3 + t1645 * t1644
     # + t1685 * t1684 + t1722 * t1721 + t1762 * t1761 + t1802 * t1801 +
     # t1820 * t1816 * t1533 / 0.720D3 + t1840 * t1836 * t1533 / 0.720D3

      end function



      doubleprecision function rrgq2qght7s2e0
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = x4 * pi
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = t17 * t19
      t23 = log(0.4D1 * t10 * t20)
      t24 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t26 = t10 * t16
      t27 = t13 * t19
      t28 = -0.1D1 + x3
      t29 = 0.1D1 / t28
      t33 = log(-0.4D1 * t26 * t27 * t29)
      t36 = x3 * z
      t37 = 0.2D1 * t36
      t38 = cos(t14)
      t40 = Sqrt(-t36 * t28)
      t44 = 0.1D1 / (-t37 - 0.1D1 + 0.2D1 * t38 * t40 + x3)
      t50 = pi * lh
      t51 = t3 * t7
      t58 = 0.1D1 / x3
      t60 = 0.1D1 / x1
      t63 = t7 * t24
      t66 = 0.1D1 / x2
      t67 = t66 * t60
      t68 = t44 * t58 * t67
      t71 = t4 * t7
      t72 = x2 ** 2
      t73 = t72 * t9
      t74 = t73 * t16
      t75 = -0.1D1 + x2
      t79 = log(-0.4D1 * t74 * t27 * t75)
      t83 = log(0.4D1 * t73 * t20)
      t90 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t91 = t9 * t16
      t94 = log(0.4D1 * t91 * t27)
      t96 = t94 ** 2
      t108 = lh ** 2
      t110 = pi ** 2
      t112 = 0.180D3 * t108 - 0.30D2 * t110
      t113 = pi * t112
      t114 = t51 * t24
      t115 = t113 * t114
      t119 = x3 * t13
      t120 = t16 * t19
      t121 = t120 * t29
      t124 = log(-0.4D1 * t119 * t121)
      t125 = t124 ** 2
      t129 = log(0.4D1 * t119 * t120)
      t130 = t129 ** 2
      t132 = t125 * t44 / 0.2D1 + t130 / 0.2D1
      t136 = t7 * t8
      t141 = 0.90D2 * t4 * t136 - 0.180D3 * t50 * t114
      t143 = -t124 * t44 - t129
      t148 = t7 * t90
      t152 = t44 + 0.1D1
      t158 = log(0.4D1 * t20)
      t159 = t158 * pi
      t162 = t158 ** 2
      t163 = t162 * pi
      t166 = (0.180D3 * t159 * lh + 0.45D2 * t163 + t113) * t3
      t169 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t187 = (-0.90D2 * t163 * lh + pi * (0.60D2 * lh * t110 - 0.240D3 *
     # zeta3 - 0.120D3 * t108 * lh) - 0.15D2 * t162 * t158 * pi - t159 *
     # t112) * t3
      t193 = (-0.180D3 * t50 - 0.90D2 * t159) * t3
      t196 = t72 * x3
      t199 = log(0.4D1 * t196 * t20)
      t201 = t196 * t13
      t204 = log(-0.4D1 * t201 * t121)
      t208 = t120 * t75
      t211 = log(-0.4D1 * t201 * t208)
      t217 = t50 * t3
      t225 = t13 * t72
      t228 = log(0.4D1 * t225 * t120)
      t229 = t228 ** 2
      t232 = log(-0.4D1 * t225 * t208)
      t233 = t232 ** 2
      t235 = t229 / 0.2D1 - t233 / 0.2D1
      t239 = -t228 + t232
      t244 = (0.90D2 * t4 * t7 * (t8 - t23 * t24 + (t8 - t33 * t24) * t4
     #4) - 0.180D3 * t50 * t51 * (t24 + t24 * t44)) * t58 * t60 / 0.1440
     #D4 + t4 * t63 * t68 / 0.8D1 + t71 * (t79 * t24 - t83 * t24) * t66 
     #* t60 / 0.8D1 + (0.90D2 * t4 * t7 * (t90 - t94 * t8 + t96 * t24 / 
     #0.2D1) - 0.180D3 * t50 * t51 * (t8 - t94 * t24) + t115) * t60 / 0.
     #1440D4 + (0.90D2 * t4 * t63 * t132 + t141 * t143 + (-0.180D3 * t50
     # * t51 * t8 + t115 + 0.90D2 * t4 * t148) * t152) * t58 / 0.2880D4 
     #+ t166 * t136 / 0.2880D4 + t4 * t7 * t169 / 0.32D2 + t187 * t63 / 
     #0.2880D4 + t193 * t148 / 0.2880D4 + (0.90D2 * t4 * t7 * (-t199 * t
     #24 + (t8 - t204 * t24) * t44 + t211 * t24) - 0.180D3 * t217 * t63 
     #* t44) * t58 * t66 / 0.1440D4 + (0.90D2 * t4 * t63 * t235 + t141 *
     # t239) * t66 / 0.1440D4
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t244)
      t247 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t249 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t266 = t7 * t247
      t280 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t290 = t51 * t247
      t291 = t113 * t290
      t298 = t7 * t249
      t303 = 0.90D2 * t4 * t298 - 0.180D3 * t50 * t290
      t308 = t7 * t280
      t318 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t349 = (0.90D2 * t4 * t7 * (-t23 * t247 + t249 + (t249 - t33 * t24
     #7) * t44) - 0.180D3 * t50 * t51 * (t247 + t247 * t44)) * t58 * t60
     # / 0.1440D4 + t4 * t266 * t68 / 0.8D1 + t71 * (-t83 * t247 + t79 *
     # t247) * t66 * t60 / 0.8D1 + (0.90D2 * t4 * t7 * (t96 * t247 / 0.2
     #D1 - t94 * t249 + t280) - 0.180D3 * t50 * t51 * (t249 - t94 * t247
     #) + t291) * t60 / 0.1440D4 + (0.90D2 * t4 * t266 * t132 + t303 * t
     #143 + (-0.180D3 * t50 * t51 * t249 + t291 + 0.90D2 * t4 * t308) * 
     #t152) * t58 / 0.2880D4 + t166 * t298 / 0.2880D4 + t4 * t7 * t318 /
     # 0.32D2 + t187 * t266 / 0.2880D4 + t193 * t308 / 0.2880D4 + (0.90D
     #2 * t4 * t7 * (-t199 * t247 + (t249 - t204 * t247) * t44 + t211 * 
     #t247) - 0.180D3 * t217 * t266 * t44) * t58 * t66 / 0.1440D4 + (0.9
     #0D2 * t4 * t266 * t235 + t303 * t239) * t66 / 0.1440D4
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t349)
      t352 = t2 * x1
      t353 = -0.1D1 + x1
      t354 = t2 * t353
      t355 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t356 = 0.1D1 / t11
      t357 = t16 * t356
      t359 = x1 * z
      t360 = -z - x1 + t359
      t361 = 0.1D1 / t360
      t362 = t19 * t361
      t363 = t353 ** 2
      t368 = log(0.4D1 * t10 * t357 * t362 * t363 * t29)
      t369 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t372 = x3 * x1
      t373 = t372 * z
      t376 = x3 * t360
      t378 = Sqrt(t376 * t28)
      t382 = 0.1D1 / (0.2D1 * t373 - 0.2D1 * t372 - t37 + x3 - 0.1D1 + 0
     #.2D1 * t38 * t378)
      t386 = t356 * t19 * t361 * t363
      t389 = log(-0.4D1 * t26 * t386)
      t396 = -t369 * t382 - t369
      t410 = log(-0.4D1 * t74 * t386)
      t416 = t51 * t369
      t423 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t428 = log(-0.4D1 * t91 * t356 * t362 * t363)
      t430 = t428 ** 2
      t446 = (0.90D2 * t4 * t7 * (-(t355 - t368 * t369) * t382 - t355 + 
     #t389 * t369) - 0.180D3 * t50 * t51 * t396) * t58 * t60 / 0.1440D4 
     #+ t71 * t396 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (t410 * t36
     #9 - t355) + 0.180D3 * t50 * t416) * t66 * t60 / 0.720D3 + (0.90D2 
     #* t4 * t7 * (-t423 + t428 * t355 - t430 * t369 / 0.2D1) - 0.180D3 
     #* t50 * t51 * (-t355 + t428 * t369) - t113 * t416) * t60 / 0.1440D
     #4
      t447 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t352, -t354, 0.0D0, t446)
      t449 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t450 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t460 = -t450 * t382 - t450
      t477 = t51 * t450
      t486 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t501 = (0.90D2 * t4 * t7 * (-(t449 - t368 * t450) * t382 - t449 + 
     #t389 * t450) - 0.180D3 * t50 * t51 * t460) * t58 * t60 / 0.1440D4 
     #+ t71 * t460 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (-t449 + t4
     #10 * t450) + 0.180D3 * t50 * t477) * t66 * t60 / 0.720D3 + (0.90D2
     # * t4 * t7 * (-t430 * t450 / 0.2D1 - t486 + t428 * t449) - 0.180D3
     # * t50 * t51 * (t428 * t450 - t449) - t113 * t477) * t60 / 0.1440D
     #4
      t502 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t354, t352, 0.0D0, t501)
      t504 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t506 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t523 = t7 * t504
      t537 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t547 = t51 * t504
      t548 = t113 * t547
      t552 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t561 = t7 * t506
      t566 = 0.90D2 * t4 * t561 - 0.180D3 * t50 * t547
      t571 = t7 * t537
      t606 = (0.90D2 * t4 * t7 * (-t23 * t504 + t506 + (t506 - t33 * t50
     #4) * t44) - 0.180D3 * t50 * t51 * (t504 * t44 + t504)) * t58 * t60
     # / 0.1440D4 + t4 * t523 * t68 / 0.8D1 + t71 * (-t83 * t504 + t79 *
     # t504) * t66 * t60 / 0.8D1 + (0.90D2 * t4 * t7 * (t96 * t504 / 0.2
     #D1 - t94 * t506 + t537) - 0.180D3 * t50 * t51 * (t506 - t94 * t504
     #) + t548) * t60 / 0.1440D4 + t4 * t7 * t552 / 0.32D2 + t187 * t523
     # / 0.2880D4 + (0.90D2 * t4 * t523 * t132 + t566 * t143 + (-0.180D3
     # * t50 * t51 * t506 + t548 + 0.90D2 * t4 * t571) * t152) * t58 / 0
     #.2880D4 + t166 * t561 / 0.2880D4 + t193 * t571 / 0.2880D4 + (0.90D
     #2 * t4 * t7 * ((t506 - t204 * t504) * t44 - t199 * t504 + t211 * t
     #504) - 0.180D3 * t217 * t523 * t44) * t58 * t66 / 0.1440D4 + (0.90
     #D2 * t4 * t523 * t235 + t566 * t239) * t66 / 0.1440D4
      t607 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t606)
      t609 = x2 * x3
      t610 = 0.1D1 - x3 + t609
      t611 = 0.1D1 / t610
      t612 = t609 * t611
      t613 = t2 * t612
      t615 = t2 * t28 * t611
      t616 = t609 * z
      t617 = t75 * t28
      t619 = Sqrt(t36 * t617)
      t623 = 0.1D1 / (-t37 + t616 - 0.1D1 + 0.2D1 * t38 * t619 + x3)
      t624 = t7 * t623
      t625 = t4 * t624
      t626 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t633 = t610 ** 2
      t639 = log(0.4D1 * t196 * t17 * t19 * t75 * t28 / t633)
      t640 = t639 * t623
      t642 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t655 = -t625 * t626 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (t64
     #0 * t626 - t623 * t642) + 0.180D3 * t217 * t624 * t626) * t58 * t6
     #6 / 0.1440D4
      t656 = FJET(XB1, XB2, s, 0.0D0, t613, 0.0D0, -t615, 0.0D0, t655)
      t658 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t664 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t677 = -t625 * t658 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (t64
     #0 * t658 - t623 * t664) + 0.180D3 * t217 * t624 * t658) * t58 * t6
     #6 / 0.1440D4
      t678 = FJET(XB1, XB2, s, 0.0D0, -t615, 0.0D0, t613, 0.0D0, t677)
      t680 = x2 * x1
      t682 = t2 * t680 * t361
      t685 = t75 * s * t1 * x1
      t690 = s * t18 * x2 * x1 * t353 * t361
      t691 = t680 * z
      t693 = 0.1D1 / (z + x1 - t359 - t680 + t691)
      t694 = t7 * t693
      t695 = t4 * t694
      t696 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 0
     #.0D0, -t354, t690)
      t697 = t696 * t360
      t699 = t58 * t66 * t60
      t703 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 0
     #.0D0, -t354, t690)
      t710 = log(0.4D1 * t73 * t357 * t362 * t363 * t75)
      t711 = t710 * t693
      t725 = -t695 * t697 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t693 * 
     #t703 - t711 * t696) * t360 + 0.180D3 * t217 * t694 * t697) * t66 *
     # t60 / 0.720D3
      t726 = FJET(XB1, XB2, s, 0.0D0, -t682, -t354, -t685, t690, t725)
      t728 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t730 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t747 = t7 * t728
      t761 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t771 = t51 * t728
      t772 = t113 * t771
      t778 = t7 * t730
      t781 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t785 = t7 * t761
      t795 = 0.90D2 * t4 * t778 - 0.180D3 * t50 * t771
      t830 = (0.90D2 * t4 * t7 * (-t23 * t728 + (t730 - t33 * t728) * t4
     #4 + t730) - 0.180D3 * t50 * t51 * (t728 + t728 * t44)) * t58 * t60
     # / 0.1440D4 + t4 * t747 * t68 / 0.8D1 + t71 * (-t83 * t728 + t79 *
     # t728) * t66 * t60 / 0.8D1 + (0.90D2 * t4 * t7 * (t96 * t728 / 0.2
     #D1 - t94 * t730 + t761) - 0.180D3 * t50 * t51 * (t730 - t94 * t728
     #) + t772) * t60 / 0.1440D4 + t187 * t747 / 0.2880D4 + t166 * t778 
     #/ 0.2880D4 + t4 * t7 * t781 / 0.32D2 + t193 * t785 / 0.2880D4 + (0
     #.90D2 * t4 * t747 * t132 + t795 * t143 + (-0.180D3 * t50 * t51 * t
     #730 + t772 + 0.90D2 * t4 * t785) * t152) * t58 / 0.2880D4 + (0.90D
     #2 * t4 * t7 * (-t199 * t728 + t211 * t728 + (t730 - t204 * t728) *
     # t44) - 0.180D3 * t217 * t747 * t44) * t58 * t66 / 0.1440D4 + (0.9
     #0D2 * t4 * t747 * t235 + t795 * t239) * t66 / 0.1440D4
      t831 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t830)
      t833 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t834 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t844 = -t834 * t382 - t834
      t861 = t51 * t834
      t869 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t885 = (0.90D2 * t4 * t7 * (-(t833 - t368 * t834) * t382 - t833 + 
     #t389 * t834) - 0.180D3 * t50 * t51 * t844) * t58 * t60 / 0.1440D4 
     #+ t71 * t844 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (t410 * t83
     #4 - t833) + 0.180D3 * t50 * t861) * t66 * t60 / 0.720D3 + (0.90D2 
     #* t4 * t7 * (t428 * t833 - t869 - t430 * t834 / 0.2D1) - 0.180D3 *
     # t50 * t51 * (-t833 + t428 * t834) - t113 * t861) * t60 / 0.1440D4
      t886 = FJET(XB1, XB2, s, t352, -t354, 0.0D0, 0.0D0, 0.0D0, t885)
      t888 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t894 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t907 = -t625 * t888 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (t64
     #0 * t888 - t623 * t894) + 0.180D3 * t217 * t624 * t888) * t58 * t6
     #6 / 0.1440D4
      t908 = FJET(XB1, XB2, s, t613, 0.0D0, -t615, 0.0D0, 0.0D0, t907)
      t913 = t28 * s * t1 * t353 * t611
      t914 = x2 * z
      t915 = t609 * x1
      t916 = t609 * t359
      t918 = Sqrt(-t376 * t617)
      t919 = t38 * t918
      t922 = z + x1 - t359 - t914 - t680 + t691 - t36 - t372 + t373 + t6
     #16 + t915 - t916 + t196 + 0.2D1 * t919 * x2
      t925 = t352 * t922 * t361 * t611
      t926 = t354 * t612
      t932 = t352 * x2 * (-t36 - t372 + t373 + t616 + t915 - t916 - 0.1D
     #1 + x3 + 0.2D1 * t919) * t361 * t611
      t934 = t4 * t7 * t360
      t937 = x3 * t11
      t957 = t36 + t359 - 0.5D1 * t373 - z - x1 + 0.4D1 * t916 - 0.3D1 *
     # t937 * t680 - 0.4D1 * t10 * t914 + 0.2D1 * t10 * t11 * x2 - t196 
     #* t359 + t196 * t11 * x1 + 0.2D1 * t196 * t9 * z - t196 * t9 * t11
     # - 0.2D1 * t919 * t359 - 0.2D1 * t919 * t680
      t976 = -0.2D1 * t10 - 0.2D1 * t937 + t372 - t691 + t680 - t915 + 0
     #.2D1 * t919 * z + 0.2D1 * t919 * x1 + t609 * t11 - t196 * t9 + 0.4
     #D1 * t937 * x1 + 0.4D1 * t10 * z + 0.2D1 * t10 * x2 - 0.2D1 * t10 
     #* t11 + 0.2D1 * t919 * t691
      t978 = 0.1D1 / (t957 + t976)
      t979 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -t
     #926, t913, t690)
      t981 = t978 * t979 * t699
      t984 = FJET(XB1, XB2, s, t913, -t925, -t926, t932, t690, -t934 * t
     #981 / 0.8D1)
      t986 = t51 * t360
      t990 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -t
     #926, t913, t690)
      t992 = t978 * t990 * t699
      t995 = FJET(XB1, XB2, s, t932, -t926, -t925, t913, t690, -t934 * t
     #992 / 0.8D1)
      t1000 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0
     #.0D0, -t354, 0.0D0)
      t1001 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0
     #.0D0, -t354, 0.0D0)
      t1011 = -t1001 * t382 - t1001
      t1028 = t51 * t1001
      t1036 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0
     #.0D0, -t354, 0.0D0)
      t1052 = (0.90D2 * t4 * t7 * (-t1000 - (t1000 - t368 * t1001) * t38
     #2 + t389 * t1001) - 0.180D3 * t50 * t51 * t1011) * t58 * t60 / 0.1
     #440D4 + t71 * t1011 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (t41
     #0 * t1001 - t1000) + 0.180D3 * t50 * t1028) * t66 * t60 / 0.720D3 
     #+ (0.90D2 * t4 * t7 * (t428 * t1000 - t1036 - t430 * t1001 / 0.2D1
     #) - 0.180D3 * t50 * t51 * (-t1000 + t428 * t1001) - t113 * t1028) 
     #* t60 / 0.1440D4
      t1053 = FJET(XB1, XB2, s, -t354, t352, 0.0D0, 0.0D0, 0.0D0, t1052)
      t1055 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1056 = t1055 * t360
      t1060 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1075 = -t695 * t1056 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t693 
     #* t1060 - t711 * t1055) * t360 + 0.180D3 * t217 * t694 * t1056) * 
     #t66 * t60 / 0.720D3
      t1076 = FJET(XB1, XB2, s, -t354, -t685, 0.0D0, -t682, t690, t1075)
      t1078 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t613, -t615, 0.0D0)
      t1084 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t613, -t615, 0.0D0)
      t1097 = -t625 * t1078 * t58 * t67 / 0.8D1 + (0.90D2 * t4 * t7 * (t
     #640 * t1078 - t623 * t1084) + 0.180D3 * t217 * t624 * t1078) * t58
     # * t66 / 0.1440D4
      t1098 = FJET(XB1, XB2, s, -t615, 0.0D0, t613, 0.0D0, 0.0D0, t1097)
      t1100 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1101 = t1100 * t360
      t1105 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1120 = -t695 * t1101 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t693 
     #* t1105 - t711 * t1100) * t360 + 0.180D3 * t217 * t694 * t1101) * 
     #t66 * t60 / 0.720D3
      t1121 = FJET(XB1, XB2, s, -t685, -t354, -t682, 0.0D0, t690, t1120)
      t1123 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1124 = t1123 * t360
      t1128 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1143 = -t695 * t1124 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t693 
     #* t1128 - t711 * t1123) * t360 + 0.180D3 * t217 * t694 * t1124) * 
     #t66 * t60 / 0.720D3
      t1144 = FJET(XB1, XB2, s, -t682, 0.0D0, -t685, -t354, t690, t1143)
      t1146 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -
     #t926, t913, t690)
      t1148 = t978 * t1146 * t699
      t1151 = FJET(XB1, XB2, s, -t925, t913, t932, -t926, t690, -t934 * 
     #t1148 / 0.8D1)
      t1156 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -
     #t926, t913, t690)
      t1158 = t978 * t1156 * t699
      t1161 = FJET(XB1, XB2, s, -t926, t932, t913, -t925, t690, -t934 * 
     #t1158 / 0.8D1)
      rrgq2qght7s2e0 = t245 * t244 + t350 * t349 + t447 * t446 + t502 * 
     #t501 + t607 * t606 + t656 * t655 + t678 * t677 + t726 * t725 + t83
     #1 * t830 + t886 * t885 + t908 * t907 - t984 * pi * t986 * t981 / 0
     #.8D1 - t995 * pi * t986 * t992 / 0.8D1 + t1053 * t1052 + t1076 * t
     #1075 + t1098 * t1097 + t1121 * t1120 + t1144 * t1143 - t1151 * pi 
     #* t986 * t1148 / 0.8D1 - t1161 * pi * t986 * t1158 / 0.8D1

      end function



      doubleprecision function rrgq2qght7s2em1
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = t7 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * pi
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t20 = -0.1D1 + x3
      t25 = log(-0.4D1 * t13 * t19 / t20)
      t26 = x3 * z
      t27 = 0.2D1 * t26
      t28 = cos(t14)
      t30 = Sqrt(-t26 * t20)
      t34 = 0.1D1 / (-t27 - 0.1D1 + 0.2D1 * t28 * t30 + x3)
      t38 = log(0.4D1 * t13 * t19)
      t39 = -t25 * t34 - t38
      t43 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t44 = t7 * t43
      t47 = pi * lh
      t48 = t3 * t7
      t51 = 0.180D3 * t47 * t48 * t8
      t53 = t34 + 0.1D1
      t56 = 0.1D1 / x3
      t59 = x1 ** 2
      t60 = t59 * t16
      t64 = log(0.4D1 * t60 * t12 * t18)
      t71 = 0.1D1 / x1
      t74 = t4 * t7
      t75 = t8 * t34
      t81 = 0.1D1 / x2
      t82 = t56 * t81
      t86 = x2 ** 2
      t87 = t12 * t86
      t90 = log(0.4D1 * t87 * t19)
      t91 = -0.1D1 + x2
      t95 = log(-0.4D1 * t87 * t19 * t91)
      t96 = -t90 + t95
      t105 = log(0.4D1 * t12 * t16 * t18)
      t106 = t105 * pi
      t109 = (-0.180D3 * t47 - 0.90D2 * t106) * t3
      t114 = t105 ** 2
      t117 = lh ** 2
      t119 = pi ** 2
      t124 = (0.180D3 * t106 * lh + 0.45D2 * t114 * pi + pi * (0.180D3 *
     # t117 - 0.30D2 * t119)) * t3
      t127 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t131 = (0.90D2 * t4 * t9 * t39 + (0.90D2 * t4 * t44 - t51) * t53) 
     #* t56 / 0.2880D4 + (0.90D2 * t4 * t7 * (t43 - t64 * t8) - t51) * t
     #71 / 0.1440D4 + t74 * (t8 + t75) * t56 * t71 / 0.16D2 + t74 * t75 
     #* t82 / 0.16D2 + t74 * t8 * t96 * t81 / 0.16D2 + t109 * t44 / 0.28
     #80D4 + t124 * t9 / 0.2880D4 + t4 * t7 * t127 / 0.32D2
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t131)
      t134 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t135 = t7 * t134
      t139 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t140 = t7 * t139
      t145 = 0.180D3 * t47 * t48 * t134
      t159 = t134 * t34
      t176 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t180 = (0.90D2 * t4 * t135 * t39 + (0.90D2 * t4 * t140 - t145) * t
     #53) * t56 / 0.2880D4 + (0.90D2 * t4 * t7 * (t139 - t64 * t134) - t
     #145) * t71 / 0.1440D4 + t74 * (t134 + t159) * t56 * t71 / 0.16D2 +
     # t74 * t159 * t82 / 0.16D2 + t74 * t134 * t96 * t81 / 0.16D2 + t10
     #9 * t140 / 0.2880D4 + t124 * t135 / 0.2880D4 + t4 * t7 * t176 / 0.
     #32D2
      t181 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t180)
      t183 = t2 * x1
      t184 = -0.1D1 + x1
      t185 = t2 * t184
      t186 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t191 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t194 = x1 * z
      t195 = -z - x1 + t194
      t196 = 0.1D1 / t195
      t198 = t184 ** 2
      t202 = log(-0.4D1 * t60 / t10 * t18 * t196 * t198)
      t214 = x3 * x1
      t220 = Sqrt(x3 * t195 * t20)
      t224 = 0.1D1 / (0.2D1 * t214 * z - 0.2D1 * t214 - t27 + x3 - 0.1D1
     # + 0.2D1 * t28 * t220)
      t231 = -t74 * t186 * t81 * t71 / 0.8D1 + (0.90D2 * t4 * t7 * (-t19
     #1 + t202 * t186) + 0.180D3 * t47 * t48 * t186) * t71 / 0.1440D4 + 
     #t74 * (-t186 * t224 - t186) * t56 * t71 / 0.16D2
      t232 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t183, -t185, 0.0D0, t231)
      t234 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t240 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t257 = -t74 * t234 * t81 * t71 / 0.8D1 + (0.90D2 * t4 * t7 * (t202
     # * t234 - t240) + 0.180D3 * t47 * t48 * t234) * t71 / 0.1440D4 + t
     #74 * (-t234 * t224 - t234) * t56 * t71 / 0.16D2
      t258 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t185, t183, 0.0D0, t257)
      t260 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t261 = t7 * t260
      t265 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t266 = t7 * t265
      t271 = 0.180D3 * t47 * t48 * t260
      t285 = t260 * t34
      t293 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t306 = (0.90D2 * t4 * t261 * t39 + (0.90D2 * t4 * t266 - t271) * t
     #53) * t56 / 0.2880D4 + (0.90D2 * t4 * t7 * (t265 - t64 * t260) - t
     #271) * t71 / 0.1440D4 + t74 * (t285 + t260) * t56 * t71 / 0.16D2 +
     # t124 * t261 / 0.2880D4 + t4 * t7 * t293 / 0.32D2 + t74 * t285 * t
     #82 / 0.16D2 + t74 * t260 * t96 * t81 / 0.16D2 + t109 * t266 / 0.28
     #80D4
      t307 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t306)
      t309 = x2 * x3
      t311 = 0.1D1 / (0.1D1 - x3 + t309)
      t313 = t2 * t309 * t311
      t315 = t2 * t20 * t311
      t319 = Sqrt(t26 * t91 * t20)
      t323 = 0.1D1 / (-t27 + t309 * z - 0.1D1 + 0.2D1 * t28 * t319 + x3)
      t324 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t326 = t323 * t324 * t82
      t329 = FJET(XB1, XB2, s, 0.0D0, t313, 0.0D0, -t315, 0.0D0, -t74 * 
     #t326 / 0.16D2)
      t334 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t336 = t323 * t334 * t82
      t339 = FJET(XB1, XB2, s, 0.0D0, -t315, 0.0D0, t313, 0.0D0, -t74 * 
     #t336 / 0.16D2)
      t344 = x2 * x1
      t346 = t2 * t344 * t196
      t349 = t91 * s * t1 * x1
      t354 = s * t17 * x2 * x1 * t184 * t196
      t357 = 0.1D1 / (z + x1 - t194 - t344 + t344 * z)
      t359 = t4 * t7 * t357
      t360 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t362 = t81 * t71
      t366 = FJET(XB1, XB2, s, 0.0D0, -t346, -t185, -t349, t354, -t359 *
     # t360 * t195 * t362 / 0.8D1)
      t371 = t195 * t81 * t71
      t375 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t376 = t7 * t375
      t380 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t381 = t7 * t380
      t386 = 0.180D3 * t47 * t48 * t375
      t396 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t408 = t375 * t34
      t421 = (0.90D2 * t4 * t376 * t39 + (0.90D2 * t4 * t381 - t386) * t
     #53) * t56 / 0.2880D4 + t109 * t381 / 0.2880D4 + t124 * t376 / 0.28
     #80D4 + t4 * t7 * t396 / 0.32D2 + (0.90D2 * t4 * t7 * (t380 - t64 *
     # t375) - t386) * t71 / 0.1440D4 + t74 * (t375 + t408) * t56 * t71 
     #/ 0.16D2 + t74 * t408 * t82 / 0.16D2 + t74 * t375 * t96 * t81 / 0.
     #16D2
      t422 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t421)
      t424 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t429 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t447 = -t74 * t424 * t81 * t71 / 0.8D1 + (0.90D2 * t4 * t7 * (-t42
     #9 + t202 * t424) + 0.180D3 * t47 * t48 * t424) * t71 / 0.1440D4 + 
     #t74 * (-t424 * t224 - t424) * t56 * t71 / 0.16D2
      t448 = FJET(XB1, XB2, s, t183, -t185, 0.0D0, 0.0D0, 0.0D0, t447)
      t450 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t452 = t323 * t450 * t82
      t455 = FJET(XB1, XB2, s, t313, 0.0D0, -t315, 0.0D0, 0.0D0, -t74 * 
     #t452 / 0.16D2)
      t460 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t465 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t483 = -t74 * t460 * t81 * t71 / 0.8D1 + (0.90D2 * t4 * t7 * (-t46
     #5 + t202 * t460) + 0.180D3 * t47 * t48 * t460) * t71 / 0.1440D4 + 
     #t74 * (-t460 * t224 - t460) * t56 * t71 / 0.16D2
      t484 = FJET(XB1, XB2, s, -t185, t183, 0.0D0, 0.0D0, 0.0D0, t483)
      t486 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t491 = FJET(XB1, XB2, s, -t185, -t349, 0.0D0, -t346, t354, -t359 *
     # t486 * t195 * t362 / 0.8D1)
      t498 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t500 = t323 * t498 * t82
      t503 = FJET(XB1, XB2, s, -t315, 0.0D0, t313, 0.0D0, 0.0D0, -t74 * 
     #t500 / 0.16D2)
      t508 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t513 = FJET(XB1, XB2, s, -t349, -t185, -t346, 0.0D0, t354, -t359 *
     # t508 * t195 * t362 / 0.8D1)
      t520 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t525 = FJET(XB1, XB2, s, -t346, 0.0D0, -t349, -t185, t354, -t359 *
     # t520 * t195 * t362 / 0.8D1)
      rrgq2qght7s2em1 = t132 * t131 + t181 * t180 + t232 * t231 + t258 *
     # t257 + t307 * t306 - t329 * pi * t48 * t326 / 0.16D2 - t339 * pi 
     #* t48 * t336 / 0.16D2 - t366 * pi * t48 * t357 * t360 * t371 / 0.8
     #D1 + t422 * t421 + t448 * t447 - t455 * pi * t48 * t452 / 0.16D2 +
     # t484 * t483 - t491 * pi * t48 * t357 * t486 * t371 / 0.8D1 - t503
     # * pi * t48 * t500 / 0.16D2 - t513 * pi * t48 * t357 * t508 * t371
     # / 0.8D1 - t525 * pi * t48 * t357 * t520 * t371 / 0.8D1

      end function



      doubleprecision function rrgq2qght7s2em2
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = t4 * t7
      t15 = x3 * z
      t17 = x4 * pi
      t18 = cos(t17)
      t21 = Sqrt(-t15 * (-0.1D1 + x3))
      t26 = 0.1D1 / (-0.2D1 * t15 - 0.1D1 + 0.2D1 * t18 * t21 + x3) + 0.
     #1D1
      t28 = 0.1D1 / x3
      t32 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t38 = z ** 2
      t41 = Sin(t17)
      t42 = t41 ** 2
      t44 = t1 ** 2
      t45 = t44 ** 2
      t48 = log(0.4D1 / t38 / z * t42 * t45)
      t52 = (-0.180D3 * pi * lh - 0.90D2 * t48 * pi) * t3
      t55 = t4 * t9 * t10 / 0.16D2 + t14 * t8 * t26 * t28 / 0.32D2 + t4 
     #* t7 * t32 / 0.32D2 + t52 * t9 / 0.2880D4
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t55)
      t58 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t59 = t7 * t58
      t67 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t73 = t4 * t59 * t10 / 0.16D2 + t14 * t58 * t26 * t28 / 0.32D2 + t
     #4 * t7 * t67 / 0.32D2 + t52 * t59 / 0.2880D4
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t73)
      t76 = t2 * x1
      t78 = t2 * (-0.1D1 + x1)
      t79 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0D
     #0, -t78, 0.0D0)
      t81 = t7 * t79 * t10
      t84 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t76, -t78, 0.0D0, -t4 * t81 
     #/ 0.16D2)
      t89 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0D
     #0, -t78, 0.0D0)
      t91 = t7 * t89 * t10
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t78, t76, 0.0D0, -t4 * t91 
     #/ 0.16D2)
      t99 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t100 = t7 * t99
      t108 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t114 = t4 * t100 * t10 / 0.16D2 + t14 * t99 * t26 * t28 / 0.32D2 +
     # t4 * t7 * t108 / 0.32D2 + t52 * t100 / 0.2880D4
      t115 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t114)
      t117 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t118 = t7 * t117
      t126 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t132 = t4 * t118 * t10 / 0.16D2 + t14 * t117 * t26 * t28 / 0.32D2 
     #+ t4 * t7 * t126 / 0.32D2 + t52 * t118 / 0.2880D4
      t133 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t132)
      t135 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0
     #D0, -t78, 0.0D0)
      t137 = t7 * t135 * t10
      t140 = FJET(XB1, XB2, s, t76, -t78, 0.0D0, 0.0D0, 0.0D0, -t4 * t13
     #7 / 0.16D2)
      t145 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0
     #D0, -t78, 0.0D0)
      t147 = t7 * t145 * t10
      t150 = FJET(XB1, XB2, s, -t78, t76, 0.0D0, 0.0D0, 0.0D0, -t4 * t14
     #7 / 0.16D2)
      rrgq2qght7s2em2 = t56 * t55 + t74 * t73 - t84 * pi * t3 * t81 / 0.
     #16D2 - t94 * pi * t3 * t91 / 0.16D2 + t115 * t114 + t133 * t132 - 
     #t140 * pi * t3 * t137 / 0.16D2 - t150 * pi * t3 * t147 / 0.16D2

      end function



      doubleprecision function rrgq2qght7s2em3
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

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
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t7 * 
     #t8 / 0.32D2)
      t14 = t3 * t7
      t17 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t21 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t7 * 
     #t17 / 0.32D2)
      t25 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t29 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t25 / 0.32D2)
      t33 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t37 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t33 / 0.32D2)
      rrgq2qght7s2em3 = t12 * pi * t14 * t8 / 0.32D2 + t21 * pi * t14 * 
     #t17 / 0.32D2 + t29 * pi * t14 * t25 / 0.32D2 + t37 * pi * t14 * t3
     #3 / 0.32D2

      end function



      doubleprecision function rrgq2qght7s2em4
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
      doubleprecision rrgq2qgh71J1
      doubleprecision rrgq2qgh71J2
      doubleprecision rrgq2qgh71J3
      doubleprecision rrgq2qgh71J4
      doubleprecision rrgq2qgh71J5
      doubleprecision rrgq2qgh71J6
      doubleprecision rrgq2qgh72J1
      doubleprecision rrgq2qgh72J2
      doubleprecision rrgq2qgh72J3
      doubleprecision rrgq2qgh72J4
      doubleprecision rrgq2qgh72J5
      doubleprecision rrgq2qgh72J6
      doubleprecision rrgq2qgh73J1
      doubleprecision rrgq2qgh73J2
      doubleprecision rrgq2qgh73J3
      doubleprecision rrgq2qgh73J4
      doubleprecision rrgq2qgh73J5
      doubleprecision rrgq2qgh73J6
      doubleprecision rrgq2qgh74J1
      doubleprecision rrgq2qgh74J2
      doubleprecision rrgq2qgh74J3
      doubleprecision rrgq2qgh74J4
      doubleprecision rrgq2qgh74J5
      doubleprecision rrgq2qgh74J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght7s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh71J1
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
      t1 = S13 + S14 + S34
      t2 = S34 ** 2
      t10 = S14 ** 2
      t12 = S24 ** 2
      rrgq2qgh71J1 = (t1 * t2 * S34 + (-0.3D1 * S24 + 0.2D1 * S14) * t1 
     #* t2 + (0.2D1 * t10 + 0.3D1 * t12 - 0.4D1 * S24 * S14) * t1 * S34 
     #+ (-0.2D1 * t10 * S24 - t12 * S24 + 0.2D1 * S14 * t12) * t1) / S12
     # / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh71J2
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
      t1 = S13 + S14 + S34
      t3 = t1 * S24
      t12 = S34 ** 2
      t20 = S24 ** 2
      rrgq2qgh71J2 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (-t20 * S24 + 0.2D1 * S14 * t20) * t1) / S12) / pi * wd /
     # z

      end function
  
   
 

      doubleprecision function rrgq2qgh71J3
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
      t1 = S13 + S14 + S34
      t3 = t1 * S24
      t12 = S34 ** 2
      t20 = S24 ** 2
      rrgq2qgh71J3 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (-t20 * S24 + 0.2D1 * S14 * t20) * t1) / S12) / pi * wd /
     # z

      end function
  
   
 

      doubleprecision function rrgq2qgh71J4
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
      t1 = S13 + S14 + S34
      t3 = t1 * S24
      t12 = S34 ** 2
      t20 = S24 ** 2
      rrgq2qgh71J4 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (-t20 * S24 + 0.2D1 * S14 * t20) * t1) / S12) / pi * wd /
     # z

      end function
  
   
 

      doubleprecision function rrgq2qgh71J5
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
      t1 = S13 + S14 + S34
      t3 = t1 * S24
      t12 = S34 ** 2
      t20 = S24 ** 2
      rrgq2qgh71J5 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (-t20 * S24 + 0.2D1 * S14 * t20) * t1) / S12) / pi * wd /
     # z

      end function
  
   
 

      doubleprecision function rrgq2qgh71J6
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
      t1 = S13 + S14 + S34
      t3 = t1 * S24
      t12 = S14 ** 2
      t13 = t1 * t12
      rrgq2qgh71J6 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (-0.2D1 * t13 * S34 + 0.2D1 * t1
     #3 * S24) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh72J1
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
      t1 = S13 + S14 + S34
      t2 = S12 ** 2
      t5 = S34 ** 2
      t11 = S14 ** 2
      rrgq2qgh72J1 = (-0.4D1 * t1 * t2 - 0.4D1 * t1 * t5 - 0.8D1 * t1 * 
     #S14 * S34 - 0.4D1 * t1 * t11) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh72J2
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
      t1 = S13 + S14 + S34
      t4 = t1 * S14
      rrgq2qgh72J2 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh72J3
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
      t1 = S13 + S14 + S34
      t4 = t1 * S14
      rrgq2qgh72J3 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh72J4
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
      t1 = S13 + S14 + S34
      t4 = t1 * S14
      rrgq2qgh72J4 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh72J5
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
      t1 = S13 + S14 + S34
      t4 = t1 * S14
      rrgq2qgh72J5 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh72J6
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
      t1 = S13 + S14 + S34
      t2 = S12 ** 2
      t7 = t1 * S14
      t11 = S34 ** 2
      t16 = S14 ** 2
      rrgq2qgh72J6 = (0.4D1 * t1 * t2 + (0.12D2 * t1 * S34 + 0.8D1 * t7)
     # * S12 + 0.4D1 * t1 * t11 + 0.12D2 * t7 * S34 + 0.4D1 * t1 * t16) 
     #/ pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh73J1
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
      t1 = S23 + S24 + S34
      t2 = S34 ** 2
      t10 = S24 ** 2
      t12 = S14 ** 2
      rrgq2qgh73J1 = (t1 * t2 * S34 + (0.2D1 * S24 - 0.3D1 * S14) * t1 *
     # t2 + (0.2D1 * t10 + 0.3D1 * t12 - 0.4D1 * S24 * S14) * t1 * S34 +
     # (0.2D1 * t12 * S24 - 0.2D1 * S14 * t10 - S14 * t12) * t1) / S12 /
     # pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh73J2
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
      t1 = S23 + S24 + S34
      t7 = t1 * S24
      t12 = S34 ** 2
      t20 = S14 ** 2
      rrgq2qgh73J2 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (0.2D1 * S24 -
     # 0.3D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1 
     #* S34 + (0.2D1 * t20 * S24 - S14 * t20) * t1) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh73J3
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
      t1 = S23 + S24 + S34
      t7 = t1 * S24
      t12 = S34 ** 2
      t20 = S14 ** 2
      rrgq2qgh73J3 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (0.2D1 * S24 -
     # 0.3D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1 
     #* S34 + (0.2D1 * t20 * S24 - S14 * t20) * t1) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh73J4
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
      t1 = S23 + S24 + S34
      t7 = t1 * S24
      t12 = S34 ** 2
      t20 = S14 ** 2
      rrgq2qgh73J4 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (0.2D1 * S24 -
     # 0.3D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1 
     #* S34 + (0.2D1 * t20 * S24 - S14 * t20) * t1) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh73J5
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
      t1 = S23 + S24 + S34
      t7 = t1 * S24
      t12 = S34 ** 2
      t20 = S14 ** 2
      rrgq2qgh73J5 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (0.2D1 * S24 -
     # 0.3D1 * S14) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1 
     #* S34 + (0.2D1 * t20 * S24 - S14 * t20) * t1) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh73J6
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
      t1 = S23 + S24 + S34
      t3 = t1 * S14
      t7 = t1 * S24
      t12 = S24 ** 2
      rrgq2qgh73J6 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t7
     # * S34 - 0.2D1 * t7 * S14 + (-0.2D1 * t1 * t12 * S34 + 0.2D1 * t3 
     #* t12) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh74J1
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
      t1 = S23 + S24 + S34
      t2 = S12 ** 2
      t5 = S34 ** 2
      t11 = S24 ** 2
      rrgq2qgh74J1 = (-0.4D1 * t1 * t2 - 0.4D1 * t1 * t5 - 0.8D1 * t1 * 
     #S24 * S34 - 0.4D1 * t1 * t11) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh74J2
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
      t1 = S23 + S24 + S34
      t4 = t1 * S24
      rrgq2qgh74J2 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh74J3
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
      t1 = S23 + S24 + S34
      t4 = t1 * S24
      rrgq2qgh74J3 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh74J4
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
      t1 = S23 + S24 + S34
      t4 = t1 * S24
      rrgq2qgh74J4 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh74J5
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
      t1 = S23 + S24 + S34
      t4 = t1 * S24
      rrgq2qgh74J5 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh74J6
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
      t1 = S23 + S24 + S34
      t2 = S12 ** 2
      t7 = t1 * S24
      t11 = S34 ** 2
      t16 = S24 ** 2
      rrgq2qgh74J6 = (0.4D1 * t1 * t2 + (0.12D2 * t1 * S34 + 0.8D1 * t7)
     # * S12 + 0.4D1 * t1 * t11 + 0.12D2 * t7 * S34 + 0.4D1 * t1 * t16) 
     #/ pi * wd / z

      end function
  
 