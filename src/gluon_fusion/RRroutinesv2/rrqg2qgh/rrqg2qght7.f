  
      subroutine rrqg2qght7
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh71J1  
      doubleprecision rrqg2qgh71J2  
      doubleprecision rrqg2qgh71J3  
      doubleprecision rrqg2qgh71J4  
      doubleprecision rrqg2qgh71J5  
      doubleprecision rrqg2qgh71J6  
      doubleprecision rrqg2qgh72J1  
      doubleprecision rrqg2qgh72J2  
      doubleprecision rrqg2qgh72J3  
      doubleprecision rrqg2qgh72J4  
      doubleprecision rrqg2qgh72J5  
      doubleprecision rrqg2qgh72J6  
      doubleprecision rrqg2qgh73J1  
      doubleprecision rrqg2qgh73J2  
      doubleprecision rrqg2qgh73J3  
      doubleprecision rrqg2qgh73J4  
      doubleprecision rrqg2qgh73J5  
      doubleprecision rrqg2qgh73J6  
      doubleprecision rrqg2qgh74J1  
      doubleprecision rrqg2qgh74J2  
      doubleprecision rrqg2qgh74J3  
      doubleprecision rrqg2qgh74J4  
      doubleprecision rrqg2qgh74J5  
      doubleprecision rrqg2qgh74J6  
      doubleprecision rrqg2qght7s1e1  
      doubleprecision rrqg2qght7s1e0  
      doubleprecision rrqg2qght7s1em1  
      doubleprecision rrqg2qght7s1em2  
      doubleprecision rrqg2qght7s1em3  
      doubleprecision rrqg2qght7s1em4  
      doubleprecision rrqg2qght7s2e1  
      doubleprecision rrqg2qght7s2e0  
      doubleprecision rrqg2qght7s2em1  
      doubleprecision rrqg2qght7s2em2  
      doubleprecision rrqg2qght7s2em3  
      doubleprecision rrqg2qght7s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght7s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght7s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght7s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght7s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght7s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght7s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght7s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght7s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght7s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght7s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght7s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqg2qght7s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght7s1e1
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t25 = -0.60D2 * lh * t18 + 0.240D3 * zeta3 + 0.120D3 * t22 * lh
      t26 = pi * t25
      t28 = t14 * t13 * pi
      t30 = t13 * pi
      t33 = -0.180D3 * t22 + 0.30D2 * t18
      t36 = 0.1D1 / t1
      t37 = (0.90D2 * t15 * lh + t26 + 0.15D2 * t28 - t30 * t33) * t36
      t38 = s ** 2
      t40 = 0.1D1 / t38 / s
      t41 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t42 = t40 * t41
      t48 = pi * t33
      t50 = (-0.180D3 * t30 * lh - 0.45D2 * t15 + t48) * t36
      t51 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t52 = t40 * t51
      t55 = pi * lh
      t59 = (0.180D3 * t55 + 0.90D2 * t30) * t36
      t60 = rrqg2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = t40 * t60
      t64 = pi * t36
      t65 = rrqg2qgh73J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t76 = t18 ** 2
      t77 = t22 ** 2
      t83 = t14 ** 2
      t87 = (-0.30D2 * t28 * lh + t15 * t33 / 0.2D1 - t30 * t25 + pi * (
     #-0.480D3 * lh * zeta3 - t76 - 0.60D2 * t77 + 0.60D2 * t22 * t18) -
     # 0.15D2 / 0.4D1 * t83 * pi) * t36
      t88 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t89 = t40 * t88
      t92 = x1 ** 2
      t93 = x3 * t92
      t96 = log(0.4D1 * t93 * t11)
      t97 = t96 ** 2
      t100 = t93 * t7
      t101 = t4 * t10
      t102 = -0.1D1 + x3
      t103 = 0.1D1 / t102
      t104 = t101 * t103
      t107 = log(-0.4D1 * t100 * t104)
      t109 = t107 ** 2
      t113 = cos(t5)
      t115 = Sqrt(-x3 * t102)
      t119 = 0.1D1 / (-0.1D1 + 0.2D1 * t113 * t115 - x3)
      t126 = t36 * t40
      t136 = -t88 - t88 * t119
      t138 = t48 * t126 * t136
      t140 = 0.1D1 / x3
      t142 = 0.1D1 / x1
      t145 = t92 * t7
      t148 = log(0.4D1 * t145 * t101)
      t153 = t148 ** 2
      t156 = t153 * t148
      t164 = t126 * t88
      t165 = t26 * t164
      t176 = x2 ** 2
      t177 = x3 * t176
      t178 = t177 * t92
      t181 = log(0.4D1 * t178 * t11)
      t187 = log(-0.4D1 * t178 * t8 * t10 * t103)
      t201 = 0.1D1 / x2
      t202 = t201 * t142
      t205 = t176 * t92
      t208 = log(0.4D1 * t205 * t11)
      t209 = t208 ** 2
      t222 = t48 * t164
      t227 = t126 * t41
      t233 = x3 * t7
      t236 = log(-0.4D1 * t233 * t104)
      t240 = log(0.4D1 * t233 * t101)
      t241 = t236 * t119 + t240
      t243 = t240 ** 2
      t245 = t236 ** 2
      t249 = t243 * t240 / 0.6D1 + t245 * t236 * t119 / 0.6D1
      t260 = -t119 - 0.1D1
      t269 = -t245 * t119 / 0.2D1 - t243 / 0.2D1
      t276 = log(0.4D1 * t177 * t11)
      t277 = t276 ** 2
      t280 = t177 * t7
      t283 = log(-0.4D1 * t280 * t104)
      t285 = t283 ** 2
      t307 = t176 * t7
      t310 = log(0.4D1 * t307 * t101)
      t315 = t310 ** 2
      t318 = t315 * t310
      t336 = -t37 * t42 / 0.2880D4 - t50 * t52 / 0.2880D4 - t59 * t61 / 
     #0.2880D4 + t64 * t40 * t65 / 0.32D2 - t87 * t89 / 0.2880D4 + (-0.9
     #0D2 * t64 * t40 * (-t97 * t88 / 0.2D1 - t51 - (-t107 * t41 + t51 +
     # t109 * t88 / 0.2D1) * t119 + t96 * t41) + 0.180D3 * t55 * t126 * 
     #(-t41 + t96 * t88 - (t41 - t107 * t88) * t119) + t138) * t140 * t1
     #42 / 0.1440D4 - (t48 * t126 * (t41 - t148 * t88) - 0.90D2 * t64 * 
     #t40 * (t153 * t41 / 0.2D1 - t156 * t88 / 0.6D1 + t60 - t148 * t51)
     # + t165 + 0.180D3 * t55 * t126 * (t51 + t153 * t88 / 0.2D1 - t148 
     #* t41)) * t142 / 0.1440D4 - (-0.90D2 * t64 * t40 * (-t181 * t88 + 
     #(t41 - t187 * t88) * t119 + t41) - 0.180D3 * t55 * t126 * t136) * 
     #t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (t209 * t88 / 0.2D1
     # - t208 * t41 + t51) + 0.180D3 * t55 * t126 * (t41 - t208 * t88) +
     # t222) * t201 * t142 / 0.720D3 + ((0.180D3 * t55 * t227 + t222 - 0
     #.90D2 * t64 * t52) * t241 - 0.90D2 * t64 * t89 * t249 + (t48 * t22
     #7 - 0.90D2 * t64 * t61 + t165 + 0.180D3 * t55 * t126 * t51) * t260
     # + (-0.90D2 * t64 * t42 + 0.180D3 * t55 * t164) * t269) * t140 / 0
     #.2880D4 + (-0.90D2 * t64 * t40 * (-t51 - t277 * t88 / 0.2D1 - (-t2
     #83 * t41 + t51 + t285 * t88 / 0.2D1) * t119 + t276 * t41) + 0.180D
     #3 * t55 * t126 * (-(t41 - t283 * t88) * t119 - t41 + t276 * t88) +
     # t138) * t140 * t201 / 0.1440D4 - (t48 * t126 * (-t310 * t88 + t41
     #) - 0.90D2 * t64 * t40 * (t315 * t41 / 0.2D1 - t318 * t88 / 0.6D1 
     #+ t60 - t310 * t51) + t165 + 0.180D3 * t55 * t126 * (t315 * t88 / 
     #0.2D1 - t310 * t41 + t51)) * t201 / 0.1440D4
      t337 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t336)
      t339 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t342 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t344 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t363 = -t339 - t339 * t119
      t365 = t48 * t126 * t363
      t377 = rrqg2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t384 = t126 * t339
      t385 = t26 * t384
      t396 = t40 * t342
      t399 = t40 * t344
      t402 = t40 * t377
      t405 = rrqg2qgh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t409 = t40 * t339
      t412 = t126 * t342
      t415 = t48 * t384
      t519 = (-0.90D2 * t64 * t40 * (-t277 * t339 / 0.2D1 + t276 * t342 
     #- t344 - (t285 * t339 / 0.2D1 - t283 * t342 + t344) * t119) + 0.18
     #0D3 * t55 * t126 * (t276 * t339 - t342 - (t342 - t283 * t339) * t1
     #19) + t365) * t140 * t201 / 0.1440D4 - (t48 * t126 * (t342 - t310 
     #* t339) - 0.90D2 * t64 * t40 * (t315 * t342 / 0.2D1 - t310 * t344 
     #+ t377 - t318 * t339 / 0.6D1) + t385 + 0.180D3 * t55 * t126 * (t34
     #4 + t315 * t339 / 0.2D1 - t310 * t342)) * t201 / 0.1440D4 - t37 * 
     #t396 / 0.2880D4 - t50 * t399 / 0.2880D4 - t59 * t402 / 0.2880D4 + 
     #t64 * t40 * t405 / 0.32D2 - t87 * t409 / 0.2880D4 + ((0.180D3 * t5
     #5 * t412 + t415 - 0.90D2 * t64 * t399) * t241 - 0.90D2 * t64 * t40
     #9 * t249 + (t48 * t412 - 0.90D2 * t64 * t402 + t385 + 0.180D3 * t5
     #5 * t126 * t344) * t260 + (-0.90D2 * t64 * t396 + 0.180D3 * t55 * 
     #t384) * t269) * t140 / 0.2880D4 + (-0.90D2 * t64 * t40 * (-t344 - 
     #t97 * t339 / 0.2D1 + t96 * t342 - (t109 * t339 / 0.2D1 - t107 * t3
     #42 + t344) * t119) + 0.180D3 * t55 * t126 * (-(t342 - t107 * t339)
     # * t119 - t342 + t96 * t339) + t365) * t140 * t142 / 0.1440D4 - (t
     #48 * t126 * (t342 - t148 * t339) - 0.90D2 * t64 * t40 * (t153 * t3
     #42 / 0.2D1 - t148 * t344 + t377 - t156 * t339 / 0.6D1) + t385 + 0.
     #180D3 * t55 * t126 * (-t148 * t342 + t344 + t153 * t339 / 0.2D1)) 
     #* t142 / 0.1440D4 - (-0.90D2 * t64 * t40 * (t342 + (t342 - t187 * 
     #t339) * t119 - t181 * t339) - 0.180D3 * t55 * t126 * t363) * t140 
     #* t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (t344 + t209 * t339 / 0.
     #2D1 - t208 * t342) + 0.180D3 * t55 * t126 * (t342 - t208 * t339) +
     # t415) * t201 * t142 / 0.720D3
      t520 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t519)
      t522 = t2 * x1
      t523 = -0.1D1 + x1
      t524 = t2 * t523
      t525 = x1 * z
      t526 = 0.1D1 - x1 + t525
      t527 = 0.1D1 / t526
      t528 = t523 ** 2
      t529 = t527 * t528
      t530 = t101 * t529
      t533 = log(0.4D1 * t100 * t530)
      t534 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t537 = t10 * t527
      t542 = log(-0.4D1 * t93 * t8 * t537 * t528 * t103)
      t544 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t545 = t542 ** 2
      t546 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t550 = x3 * x1
      t551 = t550 * z
      t554 = x3 * t526
      t556 = Sqrt(-t554 * t102)
      t560 = 0.1D1 / (-0.2D1 * t551 + 0.2D1 * t550 - 0.1D1 - x3 + 0.2D1 
     #* t113 * t556)
      t562 = t533 ** 2
      t578 = t546 * t560 + t546
      t585 = t145 * t4
      t586 = t537 * t528
      t589 = log(0.4D1 * t585 * t586)
      t594 = t589 ** 2
      t595 = t594 * t589
      t598 = rrqg2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t606 = t126 * t546
      t618 = t177 * t145
      t621 = log(0.4D1 * t618 * t530)
      t627 = log(-0.4D1 * t618 * t101 * t529 * t103)
      t643 = t205 * t7
      t646 = log(0.4D1 * t643 * t530)
      t648 = t646 ** 2
      t665 = (-0.90D2 * t64 * t40 * (-t533 * t534 + (-t542 * t534 + t544
     # + t545 * t546 / 0.2D1) * t560 + t562 * t546 / 0.2D1 + t544) + 0.1
     #80D3 * t55 * t126 * (t534 - t533 * t546 + (t534 - t542 * t546) * t
     #560) + t48 * t126 * t578) * t140 * t142 / 0.1440D4 - (t48 * t126 *
     # (t589 * t546 - t534) - 0.90D2 * t64 * t40 * (t595 * t546 / 0.6D1 
     #- t598 + t589 * t544 - t594 * t534 / 0.2D1) - t26 * t606 + 0.180D3
     # * t55 * t126 * (-t594 * t546 / 0.2D1 - t544 + t589 * t534)) * t14
     #2 / 0.1440D4 - (-0.90D2 * t64 * t40 * (t621 * t546 - t534 - (t534 
     #- t627 * t546) * t560) - 0.180D3 * t55 * t126 * t578) * t140 * t20
     #2 / 0.720D3 - (-0.90D2 * t64 * t40 * (-t544 + t646 * t534 - t648 *
     # t546 / 0.2D1) + 0.180D3 * t55 * t126 * (-t534 + t646 * t546) - t4
     #8 * t606) * t201 * t142 / 0.720D3
      t666 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t522, -t524, 0.0D0, t665)
      t668 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t671 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t674 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t692 = t668 * t560 + t668
      t706 = rrqg2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 0
     #.0D0, t522, 0.0D0)
      t713 = t126 * t668
      t758 = (-0.90D2 * t64 * t40 * (t562 * t668 / 0.2D1 + t671 + (t545 
     #* t668 / 0.2D1 - t542 * t674 + t671) * t560 - t533 * t674) + 0.180
     #D3 * t55 * t126 * ((t674 - t542 * t668) * t560 + t674 - t533 * t66
     #8) + t48 * t126 * t692) * t140 * t142 / 0.1440D4 - (t48 * t126 * (
     #-t674 + t589 * t668) - 0.90D2 * t64 * t40 * (-t594 * t674 / 0.2D1 
     #+ t589 * t671 - t706 + t595 * t668 / 0.6D1) - t26 * t713 + 0.180D3
     # * t55 * t126 * (t589 * t674 - t671 - t594 * t668 / 0.2D1)) * t142
     # / 0.1440D4 - (-0.90D2 * t64 * t40 * (-t674 + t621 * t668 - (t674 
     #- t627 * t668) * t560) - 0.180D3 * t55 * t126 * t692) * t140 * t20
     #2 / 0.720D3 - (-0.90D2 * t64 * t40 * (-t671 - t648 * t668 / 0.2D1 
     #+ t646 * t674) + 0.180D3 * t55 * t126 * (-t674 + t646 * t668) - t4
     #8 * t713) * t201 * t142 / 0.720D3
      t759 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t524, t522, 0.0D0, t758)
      t761 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t763 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t764 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t785 = -t764 * t119 - t764
      t787 = t48 * t126 * t785
      t797 = rrqg2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t806 = t126 * t764
      t807 = t26 * t806
      t846 = t48 * t806
      t851 = t40 * t763
      t854 = t40 * t797
      t904 = t40 * t761
      t907 = rrqg2qgh74J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t911 = t126 * t761
      t918 = t40 * t764
      t941 = (-0.90D2 * t64 * t40 * (-(-t107 * t761 + t763 + t109 * t764
     # / 0.2D1) * t119 + t96 * t761 - t763 - t97 * t764 / 0.2D1) + 0.180
     #D3 * t55 * t126 * (-(t761 - t107 * t764) * t119 - t761 + t96 * t76
     #4) + t787) * t140 * t142 / 0.1440D4 - (t48 * t126 * (t761 - t148 *
     # t764) - 0.90D2 * t64 * t40 * (-t148 * t763 + t797 + t153 * t761 /
     # 0.2D1 - t156 * t764 / 0.6D1) + t807 + 0.180D3 * t55 * t126 * (-t1
     #48 * t761 + t763 + t153 * t764 / 0.2D1)) * t142 / 0.1440D4 - (-0.9
     #0D2 * t64 * t40 * (-t181 * t764 + (t761 - t187 * t764) * t119 + t7
     #61) - 0.180D3 * t55 * t126 * t785) * t140 * t202 / 0.720D3 - (-0.9
     #0D2 * t64 * t40 * (t209 * t764 / 0.2D1 + t763 - t208 * t761) + 0.1
     #80D3 * t55 * t126 * (-t208 * t764 + t761) + t846) * t201 * t142 / 
     #0.720D3 - t50 * t851 / 0.2880D4 - t59 * t854 / 0.2880D4 + (-0.90D2
     # * t64 * t40 * (-(-t283 * t761 + t763 + t285 * t764 / 0.2D1) * t11
     #9 + t276 * t761 - t763 - t277 * t764 / 0.2D1) + 0.180D3 * t55 * t1
     #26 * (-t761 + t276 * t764 - (t761 - t283 * t764) * t119) + t787) *
     # t140 * t201 / 0.1440D4 - (t48 * t126 * (-t310 * t764 + t761) - 0.
     #90D2 * t64 * t40 * (t797 + t315 * t761 / 0.2D1 - t318 * t764 / 0.6
     #D1 - t310 * t763) + t807 + 0.180D3 * t55 * t126 * (-t310 * t761 + 
     #t315 * t764 / 0.2D1 + t763)) * t201 / 0.1440D4 - t37 * t904 / 0.28
     #80D4 + t64 * t40 * t907 / 0.32D2 + ((0.180D3 * t55 * t911 + t846 -
     # 0.90D2 * t64 * t851) * t241 - 0.90D2 * t64 * t918 * t249 + (t48 *
     # t911 - 0.90D2 * t64 * t854 + t807 + 0.180D3 * t55 * t126 * t763) 
     #* t260 + (-0.90D2 * t64 * t904 + 0.180D3 * t55 * t806) * t269) * t
     #140 / 0.2880D4 - t87 * t918 / 0.2880D4
      t942 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t941)
      t945 = x2 * s * t1
      t946 = -0.1D1 + x2
      t947 = t946 * s
      t948 = t947 * t1
      t949 = t10 * t946
      t953 = log(-0.4D1 * t178 * t8 * t949)
      t954 = x2 * z
      t956 = 0.1D1 / (0.1D1 - x2 + t954)
      t957 = t953 * t956
      t958 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0.
     #0D0, 0.0D0, 0.0D0)
      t960 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0.
     #0D0, 0.0D0, 0.0D0)
      t961 = t956 * t960
      t966 = t55 * t36
      t967 = t40 * t956
      t968 = t967 * t958
      t975 = t101 * t946
      t978 = log(-0.4D1 * t643 * t975)
      t979 = t978 * t956
      t981 = t978 ** 2
      t982 = t981 * t956
      t985 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0.
     #0D0, 0.0D0, 0.0D0)
      t986 = t956 * t985
      t996 = t48 * t36
      t997 = t996 * t968
      t1004 = log(-0.4D1 * t280 * t975)
      t1005 = t1004 ** 2
      t1006 = t1005 * t956
      t1009 = t1004 * t956
      t1026 = log(-0.4D1 * t307 * t975)
      t1027 = t1026 * t956
      t1032 = t1026 ** 2
      t1033 = t1032 * t956
      t1036 = rrqg2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1039 = t1032 * t1026 * t956
      t1047 = t26 * t36
      t1059 = -(-0.90D2 * t64 * t40 * (t957 * t958 - t961) - 0.180D3 * t
     #966 * t968) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (t979
     # * t960 - t982 * t958 / 0.2D1 - t986) + 0.180D3 * t55 * t126 * (-t
     #961 + t979 * t958) - t997) * t201 * t142 / 0.720D3 + (-0.90D2 * t6
     #4 * t40 * (t986 + t1006 * t958 / 0.2D1 - t1009 * t960) + 0.180D3 *
     # t55 * t126 * (-t1009 * t958 + t961) + t997) * t140 * t201 / 0.144
     #0D4 - (t48 * t126 * (-t961 + t1027 * t958) - 0.90D2 * t64 * t40 * 
     #(-t1033 * t960 / 0.2D1 - t956 * t1036 + t1039 * t958 / 0.6D1 + t10
     #27 * t985) - t1047 * t968 + 0.180D3 * t55 * t126 * (-t986 + t1027 
     #* t960 - t1033 * t958 / 0.2D1)) * t201 / 0.1440D4
      t1060 = FJET(XB1, XB2, s, 0.0D0, t945, 0.0D0, -t948, 0.0D0, t1059)
      t1062 = x2 * x3
      t1065 = Sqrt(x3 * t946 * t102)
      t1066 = t113 * t1065
      t1068 = 0.2D1 * t1066 * x2
      t1070 = 0.1D1 - x3 + t1062
      t1071 = 0.1D1 / t1070
      t1073 = t2 * (0.1D1 - x3 - x2 + t1062 + t177 + t1068) * t1071
      t1074 = 0.2D1 * t1066
      t1078 = t2 * x2 * (-0.1D1 + t1062 + t1074) * t1071
      t1079 = t946 * t102
      t1080 = t1070 ** 2
      t1081 = 0.1D1 / t1080
      t1082 = t1079 * t1081
      t1086 = log(0.4D1 * t618 * t101 * t1082)
      t1087 = t177 * z
      t1091 = 0.1D1 / (-0.1D1 + t1074 + t1087 + t1062 - t954 + x2 - x3 -
     # t177 - t1068 + 0.2D1 * t1066 * t954)
      t1092 = t1086 * t1091
      t1093 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1095 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1096 = t1091 * t1095
      t1101 = t40 * t1091
      t1102 = t1101 * t1093
      t1109 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1116 = log(0.4D1 * t177 * t8 * t949 * t102 * t1081)
      t1117 = t1116 ** 2
      t1118 = t1117 * t1091
      t1121 = t1116 * t1091
      t1137 = -(-0.90D2 * t64 * t40 * (t1092 * t1093 - t1096) - 0.180D3 
     #* t966 * t1102) * t140 * t202 / 0.720D3 + (-0.90D2 * t64 * t40 * (
     #t1091 * t1109 + t1118 * t1093 / 0.2D1 - t1121 * t1095) + 0.180D3 *
     # t55 * t126 * (-t1121 * t1093 + t1096) + t996 * t1102) * t140 * t2
     #01 / 0.1440D4
      t1138 = FJET(XB1, XB2, s, 0.0D0, t1073, 0.0D0, -t1078, 0.0D0, t113
     #7)
      t1140 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1142 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1143 = t956 * t1142
      t1148 = t967 * t1140
      t1155 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1156 = t956 * t1155
      t1169 = t996 * t1148
      t1196 = rrqg2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1216 = -(-0.90D2 * t64 * t40 * (t957 * t1140 - t1143) - 0.180D3 *
     # t966 * t1148) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (-
     #t1156 + t979 * t1142 - t982 * t1140 / 0.2D1) + 0.180D3 * t55 * t12
     #6 * (t979 * t1140 - t1143) - t1169) * t201 * t142 / 0.720D3 + (-0.
     #90D2 * t64 * t40 * (t1156 - t1009 * t1142 + t1006 * t1140 / 0.2D1)
     # + 0.180D3 * t55 * t126 * (-t1009 * t1140 + t1143) + t1169) * t140
     # * t201 / 0.1440D4 - (t48 * t126 * (-t1143 + t1027 * t1140) - 0.90
     #D2 * t64 * t40 * (-t1033 * t1142 / 0.2D1 - t956 * t1196 + t1027 * 
     #t1155 + t1039 * t1140 / 0.6D1) - t1047 * t1148 + 0.180D3 * t55 * t
     #126 * (-t1156 + t1027 * t1142 - t1033 * t1140 / 0.2D1)) * t201 / 0
     #.1440D4
      t1217 = FJET(XB1, XB2, s, 0.0D0, -t948, 0.0D0, t945, 0.0D0, t1216)
      t1219 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1220 = t1091 * t1219
      t1221 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1227 = t1101 * t1221
      t1236 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1253 = -(-0.90D2 * t64 * t40 * (-t1220 + t1092 * t1221) - 0.180D3
     # * t966 * t1227) * t140 * t202 / 0.720D3 + (-0.90D2 * t64 * t40 * 
     #(t1118 * t1221 / 0.2D1 + t1091 * t1236 - t1121 * t1219) + 0.180D3 
     #* t55 * t126 * (t1220 - t1121 * t1221) + t996 * t1227) * t140 * t2
     #01 / 0.1440D4
      t1254 = FJET(XB1, XB2, s, 0.0D0, -t1078, 0.0D0, t1073, 0.0D0, t125
     #3)
      t1258 = t2 * t523 * x2 * t527
      t1260 = t947 * t1 * t523
      t1265 = s * t9 * x2 * t523 * x1 * t527
      t1266 = x2 * x1
      t1267 = t1266 * z
      t1269 = 0.1D1 / (-0.1D1 + t1267 - t1266 + x1 + x2 - t525 - t954)
      t1270 = t526 * t1269
      t1271 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1272 = t1270 * t1271
      t1277 = log(-0.4D1 * t618 * t101 * t529 * t946)
      t1278 = t1277 * t526
      t1279 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1280 = t1269 * t1279
      t1286 = t40 * t526
      t1287 = t1286 * t1280
      t1298 = log(-0.4D1 * t205 * t8 * t537 * t528 * t946)
      t1299 = t1298 ** 2
      t1300 = t1299 * t526
      t1303 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1305 = t1298 * t526
      t1322 = -(-0.90D2 * t64 * t40 * (-t1272 + t1278 * t1280) - 0.180D3
     # * t966 * t1287) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * 
     #(-t1300 * t1280 / 0.2D1 - t1270 * t1303 + t1305 * t1269 * t1271) +
     # 0.180D3 * t55 * t126 * (t1305 * t1280 - t1272) - t996 * t1287) * 
     #t201 * t142 / 0.720D3
      t1323 = FJET(XB1, XB2, s, 0.0D0, -t1258, t522, t1260, -t1265, t132
     #2)
      t1325 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1326 = t126 * t1325
      t1329 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1330 = t126 * t1329
      t1331 = t48 * t1330
      t1332 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1333 = t40 * t1332
      t1338 = t40 * t1329
      t1343 = rrqg2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1344 = t40 * t1343
      t1347 = t26 * t1330
      t1353 = t40 * t1325
      t1392 = -t1329 - t1329 * t119
      t1394 = t48 * t126 * t1392
      t1501 = rrqg2qgh72J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1505 = ((0.180D3 * t55 * t1326 + t1331 - 0.90D2 * t64 * t1333) * 
     #t241 - 0.90D2 * t64 * t1338 * t249 + (t48 * t1326 - 0.90D2 * t64 *
     # t1344 + t1347 + 0.180D3 * t55 * t126 * t1332) * t260 + (-0.90D2 *
     # t64 * t1353 + 0.180D3 * t55 * t1330) * t269) * t140 / 0.2880D4 - 
     #t87 * t1338 / 0.2880D4 - t37 * t1353 / 0.2880D4 - t50 * t1333 / 0.
     #2880D4 - t59 * t1344 / 0.2880D4 + (-0.90D2 * t64 * t40 * (-t1332 -
     # t97 * t1329 / 0.2D1 + t96 * t1325 - (-t107 * t1325 + t1332 + t109
     # * t1329 / 0.2D1) * t119) + 0.180D3 * t55 * t126 * (-(t1325 - t107
     # * t1329) * t119 + t96 * t1329 - t1325) + t1394) * t140 * t142 / 0
     #.1440D4 - (t48 * t126 * (-t148 * t1329 + t1325) - 0.90D2 * t64 * t
     #40 * (t153 * t1325 / 0.2D1 + t1343 - t148 * t1332 - t156 * t1329 /
     # 0.6D1) + t1347 + 0.180D3 * t55 * t126 * (-t148 * t1325 + t1332 + 
     #t153 * t1329 / 0.2D1)) * t142 / 0.1440D4 - (-0.90D2 * t64 * t40 * 
     #(t1325 - t181 * t1329 + (t1325 - t187 * t1329) * t119) - 0.180D3 *
     # t55 * t126 * t1392) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t4
     #0 * (-t208 * t1325 + t1332 + t209 * t1329 / 0.2D1) + 0.180D3 * t55
     # * t126 * (-t208 * t1329 + t1325) + t1331) * t201 * t142 / 0.720D3
     # + (-0.90D2 * t64 * t40 * (-(-t283 * t1325 + t1332 + t285 * t1329 
     #/ 0.2D1) * t119 - t1332 - t277 * t1329 / 0.2D1 + t276 * t1325) + 0
     #.180D3 * t55 * t126 * (-(t1325 - t283 * t1329) * t119 - t1325 + t2
     #76 * t1329) + t1394) * t140 * t201 / 0.1440D4 - (t48 * t126 * (t13
     #25 - t310 * t1329) - 0.90D2 * t64 * t40 * (-t318 * t1329 / 0.6D1 +
     # t1343 - t310 * t1332 + t315 * t1325 / 0.2D1) + t1347 + 0.180D3 * 
     #t55 * t126 * (t1332 + t315 * t1329 / 0.2D1 - t310 * t1325)) * t201
     # / 0.1440D4 + t64 * t40 * t1501 / 0.32D2
      t1506 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1505)
      t1508 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1509 = t1269 * t1508
      t1511 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1512 = t1270 * t1511
      t1517 = t1286 * t1509
      t1525 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1543 = -(-0.90D2 * t64 * t40 * (t1278 * t1509 - t1512) - 0.180D3 
     #* t966 * t1517) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (
     #-t1300 * t1509 / 0.2D1 - t1270 * t1525 + t1305 * t1269 * t1511) + 
     #0.180D3 * t55 * t126 * (t1305 * t1509 - t1512) - t996 * t1517) * t
     #201 * t142 / 0.720D3
      t1544 = FJET(XB1, XB2, s, t522, t1260, 0.0D0, -t1258, -t1265, t154
     #3)
      t1546 = t337 * t336 + t520 * t519 + t666 * t665 + t759 * t758 + t9
     #42 * t941 + t1060 * t1059 + t1138 * t1137 + t1217 * t1216 + t1254 
     #* t1253 + t1323 * t1322 + t1506 * t1505 + t1544 * t1543
      t1547 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1549 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1550 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1571 = t1550 + t1550 * t560
      t1586 = rrqg2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1592 = t126 * t1550
      t1637 = (-0.90D2 * t64 * t40 * ((-t542 * t1547 + t1549 + t545 * t1
     #550 / 0.2D1) * t560 + t562 * t1550 / 0.2D1 + t1549 - t533 * t1547)
     # + 0.180D3 * t55 * t126 * (t1547 + (t1547 - t542 * t1550) * t560 -
     # t533 * t1550) + t48 * t126 * t1571) * t140 * t142 / 0.1440D4 - (t
     #48 * t126 * (-t1547 + t589 * t1550) - 0.90D2 * t64 * t40 * (-t594 
     #* t1547 / 0.2D1 + t595 * t1550 / 0.6D1 - t1586 + t589 * t1549) - t
     #26 * t1592 + 0.180D3 * t55 * t126 * (-t594 * t1550 / 0.2D1 - t1549
     # + t589 * t1547)) * t142 / 0.1440D4 - (-0.90D2 * t64 * t40 * (t621
     # * t1550 - t1547 - (t1547 - t627 * t1550) * t560) - 0.180D3 * t55 
     #* t126 * t1571) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (
     #-t1549 - t648 * t1550 / 0.2D1 + t646 * t1547) + 0.180D3 * t55 * t1
     #26 * (-t1547 + t646 * t1550) - t48 * t1592) * t201 * t142 / 0.720D
     #3
      t1638 = FJET(XB1, XB2, s, t522, -t524, 0.0D0, 0.0D0, 0.0D0, t1637)
      t1640 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1641 = t956 * t1640
      t1642 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1648 = t967 * t1642
      t1655 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1656 = t956 * t1655
      t1669 = t996 * t1648
      t1698 = rrqg2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t1716 = -(-0.90D2 * t64 * t40 * (-t1641 + t957 * t1642) - 0.180D3 
     #* t966 * t1648) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (
     #-t1656 - t982 * t1642 / 0.2D1 + t979 * t1640) + 0.180D3 * t55 * t1
     #26 * (-t1641 + t979 * t1642) - t1669) * t201 * t142 / 0.720D3 + (-
     #0.90D2 * t64 * t40 * (t1656 + t1006 * t1642 / 0.2D1 - t1009 * t164
     #0) + 0.180D3 * t55 * t126 * (t1641 - t1009 * t1642) + t1669) * t14
     #0 * t201 / 0.1440D4 - (t48 * t126 * (-t1641 + t1027 * t1642) - 0.9
     #0D2 * t64 * t40 * (-t1033 * t1640 / 0.2D1 + t1039 * t1642 / 0.6D1 
     #- t956 * t1698 + t1027 * t1655) - t1047 * t1648 + 0.180D3 * t55 * 
     #t126 * (t1027 * t1640 - t1656 - t1033 * t1642 / 0.2D1)) * t201 / 0
     #.1440D4
      t1717 = FJET(XB1, XB2, s, t945, 0.0D0, -t948, 0.0D0, 0.0D0, t1716)
      t1719 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1721 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1722 = t1091 * t1721
      t1727 = t1101 * t1719
      t1736 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t1753 = -(-0.90D2 * t64 * t40 * (t1092 * t1719 - t1722) - 0.180D3 
     #* t966 * t1727) * t140 * t202 / 0.720D3 + (-0.90D2 * t64 * t40 * (
     #t1118 * t1719 / 0.2D1 + t1091 * t1736 - t1121 * t1721) + 0.180D3 *
     # t55 * t126 * (t1722 - t1121 * t1719) + t996 * t1727) * t140 * t20
     #1 / 0.1440D4
      t1754 = FJET(XB1, XB2, s, t1073, 0.0D0, -t1078, 0.0D0, 0.0D0, t175
     #3)
      t1756 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1757 = t1270 * t1756
      t1758 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1759 = t1269 * t1758
      t1765 = t1286 * t1759
      t1775 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t1791 = -(-0.90D2 * t64 * t40 * (-t1757 + t1278 * t1759) - 0.180D3
     # * t966 * t1765) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * 
     #(-t1300 * t1759 / 0.2D1 + t1305 * t1269 * t1756 - t1270 * t1775) +
     # 0.180D3 * t55 * t126 * (-t1757 + t1305 * t1759) - t996 * t1765) *
     # t201 * t142 / 0.720D3
      t1792 = FJET(XB1, XB2, s, t1260, t522, -t1258, 0.0D0, -t1265, t179
     #1)
      t1795 = t522 * t1062 * t1071
      t1796 = t1062 * x1
      t1797 = t1062 * t525
      t1799 = Sqrt(t554 * t1079)
      t1800 = t113 * t1799
      t1801 = 0.2D1 * t1800
      t1806 = t524 * x2 * (t550 - t551 + t1062 - t1796 + t1797 - 0.1D1 +
     # t1801) * t527 * t1071
      t1810 = t102 * s * t1 * x1 * t1071
      t1812 = 0.2D1 * t1800 * x2
      t1813 = 0.1D1 - x1 + t525 - x2 + t1266 - t1267 - x3 + t550 - t551 
     #+ t1062 - t1796 + t1797 + t177 + t1812
      t1816 = t524 * t1813 * t527 * t1071
      t1830 = 0.1D1 + 0.3D1 * t551 + 0.3D1 * t1796 - t1267 + 0.2D1 * t18
     #00 * t1267 + t525 - 0.3D1 * t550 + x3 - x1 - t1087 - t1801 + 0.2D1
     # * t93 + t1266 - 0.4D1 * t1797 - 0.2D1 * t1800 * t525 - 0.2D1 * t1
     #800 * t954 - 0.2D1 * t1800 * t1266
      t1857 = -t177 * t3 * x1 - 0.2D1 * t177 * t92 * z + t177 * t92 * t3
     # + x3 * t3 * t1266 + 0.4D1 * t93 * t954 - 0.2D1 * t93 * t3 * x2 + 
     #0.3D1 * t177 * t525 - x2 + t1812 - 0.2D1 * t93 * x2 - 0.2D1 * t177
     # * x1 + 0.2D1 * t1800 * x1 - 0.4D1 * t93 * z + 0.2D1 * t93 * t3 + 
     #t178 + t954 - t1062 + t177
      t1859 = 0.1D1 / (t1830 + t1857)
      t1860 = t526 * t1859
      t1861 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t1867 = log(0.4D1 * t177 * t585 * t586 * t1082)
      t1868 = t1867 * t526
      t1869 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t1870 = t1859 * t1869
      t1879 = -0.90D2 * t64 * t40 * (-t1860 * t1861 + t1868 * t1870) - 0
     #.180D3 * t966 * t1286 * t1870
      t1883 = FJET(XB1, XB2, s, t1795, t1806, -t1810, -t1816, -t1265, -t
     #1879 * t140 * t202 / 0.720D3)
      t1886 = t140 * t201 * t142
      t1889 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t1891 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t1892 = t1859 * t1891
      t1901 = -0.90D2 * t64 * t40 * (-t1860 * t1889 + t1868 * t1892) - 0
     #.180D3 * t966 * t1286 * t1892
      t1905 = FJET(XB1, XB2, s, t1806, t1795, -t1816, -t1810, -t1265, -t
     #1901 * t140 * t202 / 0.720D3)
      t1909 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1911 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1912 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1933 = t1912 + t1912 * t560
      t1947 = rrqg2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t524, 
     #0.0D0, t522, 0.0D0)
      t1954 = t126 * t1912
      t1999 = (-0.90D2 * t64 * t40 * ((-t542 * t1909 + t1911 + t545 * t1
     #912 / 0.2D1) * t560 - t533 * t1909 + t1911 + t562 * t1912 / 0.2D1)
     # + 0.180D3 * t55 * t126 * (-t533 * t1912 + t1909 + (t1909 - t542 *
     # t1912) * t560) + t48 * t126 * t1933) * t140 * t142 / 0.1440D4 - (
     #t48 * t126 * (t589 * t1912 - t1909) - 0.90D2 * t64 * t40 * (-t594 
     #* t1909 / 0.2D1 + t589 * t1911 - t1947 + t595 * t1912 / 0.6D1) - t
     #26 * t1954 + 0.180D3 * t55 * t126 * (-t594 * t1912 / 0.2D1 - t1911
     # + t589 * t1909)) * t142 / 0.1440D4 - (-0.90D2 * t64 * t40 * (-(t1
     #909 - t627 * t1912) * t560 - t1909 + t621 * t1912) - 0.180D3 * t55
     # * t126 * t1933) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * 
     #(-t648 * t1912 / 0.2D1 + t646 * t1909 - t1911) + 0.180D3 * t55 * t
     #126 * (-t1909 + t646 * t1912) - t48 * t1954) * t201 * t142 / 0.720
     #D3
      t2000 = FJET(XB1, XB2, s, -t524, t522, 0.0D0, 0.0D0, 0.0D0, t1999)
      t2002 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t2003 = t956 * t2002
      t2004 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t2010 = t967 * t2004
      t2018 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t2019 = t956 * t2018
      t2031 = t996 * t2010
      t2060 = rrqg2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, t945, -t948, 0
     #.0D0, 0.0D0, 0.0D0)
      t2078 = -(-0.90D2 * t64 * t40 * (-t2003 + t957 * t2004) - 0.180D3 
     #* t966 * t2010) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * (
     #t979 * t2002 - t2019 - t982 * t2004 / 0.2D1) + 0.180D3 * t55 * t12
     #6 * (t979 * t2004 - t2003) - t2031) * t201 * t142 / 0.720D3 + (-0.
     #90D2 * t64 * t40 * (-t1009 * t2002 + t1006 * t2004 / 0.2D1 + t2019
     #) + 0.180D3 * t55 * t126 * (-t1009 * t2004 + t2003) + t2031) * t14
     #0 * t201 / 0.1440D4 - (t48 * t126 * (-t2003 + t1027 * t2004) - 0.9
     #0D2 * t64 * t40 * (-t1033 * t2002 / 0.2D1 + t1039 * t2004 / 0.6D1 
     #- t956 * t2060 + t1027 * t2018) - t1047 * t2010 + 0.180D3 * t55 * 
     #t126 * (t1027 * t2002 - t2019 - t1033 * t2004 / 0.2D1)) * t201 / 0
     #.1440D4
      t2079 = FJET(XB1, XB2, s, -t948, 0.0D0, t945, 0.0D0, 0.0D0, t2078)
      t2081 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t2082 = t1091 * t2081
      t2083 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t2089 = t1101 * t2083
      t2098 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t1078, t1073,
     # 0.0D0, 0.0D0, 0.0D0)
      t2115 = -(-0.90D2 * t64 * t40 * (-t2082 + t1092 * t2083) - 0.180D3
     # * t966 * t2089) * t140 * t202 / 0.720D3 + (-0.90D2 * t64 * t40 * 
     #(t1118 * t2083 / 0.2D1 + t1091 * t2098 - t1121 * t2081) + 0.180D3 
     #* t55 * t126 * (t2082 - t1121 * t2083) + t996 * t2089) * t140 * t2
     #01 / 0.1440D4
      t2116 = FJET(XB1, XB2, s, -t1078, 0.0D0, t1073, 0.0D0, 0.0D0, t211
     #5)
      t2118 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t2119 = t1270 * t2118
      t2120 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t2121 = t1269 * t2120
      t2127 = t1286 * t2121
      t2135 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t1258, t1260,
     # 0.0D0, t522, -t1265)
      t2153 = -(-0.90D2 * t64 * t40 * (-t2119 + t1278 * t2121) - 0.180D3
     # * t966 * t2127) * t140 * t202 / 0.720D3 - (-0.90D2 * t64 * t40 * 
     #(-t1300 * t2121 / 0.2D1 - t1270 * t2135 + t1305 * t1269 * t2118) +
     # 0.180D3 * t55 * t126 * (-t2119 + t1305 * t2121) - t996 * t2127) *
     # t201 * t142 / 0.720D3
      t2154 = FJET(XB1, XB2, s, -t1258, 0.0D0, t1260, t522, -t1265, t215
     #3)
      t2156 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t2157 = t1859 * t2156
      t2159 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t2168 = -0.90D2 * t64 * t40 * (t1868 * t2157 - t1860 * t2159) - 0.
     #180D3 * t966 * t1286 * t2157
      t2172 = FJET(XB1, XB2, s, -t1810, -t1816, t1795, t1806, -t1265, -t
     #2168 * t140 * t202 / 0.720D3)
      t2176 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t2177 = t1859 * t2176
      t2179 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t1806, -t1816,
     # t1795, -t1810, -t1265)
      t2188 = -0.90D2 * t64 * t40 * (t1868 * t2177 - t1860 * t2179) - 0.
     #180D3 * t966 * t1286 * t2177
      t2192 = FJET(XB1, XB2, s, -t1816, -t1810, t1806, t1795, -t1265, -t
     #2188 * t140 * t202 / 0.720D3)
      t2196 = t1638 * t1637 + t1717 * t1716 + t1754 * t1753 + t1792 * t1
     #791 - t1883 * t1879 * t1886 / 0.720D3 - t1905 * t1901 * t1886 / 0.
     #720D3 + t2000 * t1999 + t2079 * t2078 + t2116 * t2115 + t2154 * t2
     #153 - t2172 * t2168 * t1886 / 0.720D3 - t2192 * t2188 * t1886 / 0.
     #720D3
      rrqg2qght7s1e1 = t1546 + t2196

      end function



      doubleprecision function rrqg2qght7s1e0
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t8 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
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
      t32 = 0.1D1 / (-0.1D1 + 0.2D1 * t26 * t28 - x3)
      t36 = log(0.4D1 * t13 * t18)
      t37 = t36 ** 2
      t39 = -t25 * t32 / 0.2D1 - t37 / 0.2D1
      t43 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t44 = t7 * t43
      t47 = pi * lh
      t48 = t3 * t7
      t49 = t48 * t8
      t51 = 0.180D3 * t47 * t49
      t54 = t24 * t32 + t36
      t59 = lh ** 2
      t61 = pi ** 2
      t63 = -0.180D3 * t59 + 0.30D2 * t61
      t64 = pi * t63
      t65 = t64 * t49
      t66 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
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
      t88 = (-0.180D3 * t81 * lh - 0.45D2 * t85 + t64) * t3
      t91 = rrqg2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t109 = (0.90D2 * t85 * lh + pi * (-0.60D2 * lh * t61 + 0.240D3 * z
     #eta3 + 0.120D3 * t59 * lh) + 0.15D2 * t84 * t80 * pi - t81 * t63) 
     #* t3
      t115 = (0.180D3 * t47 + 0.90D2 * t81) * t3
      t118 = x2 ** 2
      t119 = x3 * t118
      t120 = t119 * t12
      t123 = log(-0.4D1 * t120 * t21)
      t129 = log(0.4D1 * t119 * t78)
      t136 = -t8 - t8 * t32
      t139 = 0.180D3 * t47 * t48 * t136
      t142 = 0.1D1 / x2
      t145 = t118 * t12
      t148 = log(0.4D1 * t145 * t18)
      t149 = t148 ** 2
      t165 = x1 ** 2
      t166 = x3 * t165
      t169 = log(0.4D1 * t166 * t78)
      t171 = t166 * t12
      t174 = log(-0.4D1 * t171 * t21)
      t184 = 0.1D1 / x1
      t187 = t4 * t7
      t190 = t142 * t184
      t194 = t118 * t165
      t197 = log(0.4D1 * t194 * t78)
      t207 = t165 * t12
      t210 = log(0.4D1 * t207 * t18)
      t211 = t210 ** 2
      t227 = (-0.90D2 * t4 * t9 * t39 + (-0.90D2 * t4 * t44 + t51) * t54
     # + (0.180D3 * t47 * t48 * t43 + t65 - 0.90D2 * t4 * t67) * t71) * 
     #t74 / 0.2880D4 - t88 * t44 / 0.2880D4 + t4 * t7 * t91 / 0.32D2 - t
     #109 * t9 / 0.2880D4 - t115 * t67 / 0.2880D4 + (-0.90D2 * t4 * t7 *
     # (-(t43 - t123 * t8) * t32 - t43 + t129 * t8) + t139) * t74 * t142
     # / 0.1440D4 - (-0.90D2 * t4 * t7 * (t149 * t8 / 0.2D1 - t148 * t43
     # + t66) + 0.180D3 * t47 * t48 * (-t148 * t8 + t43) + t65) * t142 /
     # 0.1440D4 + (-0.90D2 * t4 * t7 * (-t43 + t169 * t8 - (t43 - t174 *
     # t8) * t32) + t139) * t74 * t184 / 0.1440D4 - t187 * t136 * t74 * 
     #t190 / 0.8D1 - (-0.90D2 * t4 * t7 * (t43 - t197 * t8) + t51) * t14
     #2 * t184 / 0.720D3 - (-0.90D2 * t4 * t7 * (t66 + t211 * t8 / 0.2D1
     # - t210 * t43) + 0.180D3 * t47 * t48 * (t43 - t210 * t8) + t65) * 
     #t184 / 0.1440D4
      t228 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t227)
      t230 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t231 = t7 * t230
      t235 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t236 = t7 * t235
      t239 = t48 * t230
      t241 = 0.180D3 * t47 * t239
      t247 = t64 * t239
      t248 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t249 = t7 * t248
      t259 = rrqg2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t276 = -t230 - t230 * t32
      t279 = 0.180D3 * t47 * t48 * t276
      t340 = (-0.90D2 * t4 * t231 * t39 + (-0.90D2 * t4 * t236 + t241) *
     # t54 + (0.180D3 * t47 * t48 * t235 + t247 - 0.90D2 * t4 * t249) * 
     #t71) * t74 / 0.2880D4 - t88 * t236 / 0.2880D4 + t4 * t7 * t259 / 0
     #.32D2 - t109 * t231 / 0.2880D4 - t115 * t249 / 0.2880D4 + (-0.90D2
     # * t4 * t7 * (t129 * t230 - t235 - (t235 - t123 * t230) * t32) + t
     #279) * t74 * t142 / 0.1440D4 - (-0.90D2 * t4 * t7 * (t248 + t149 *
     # t230 / 0.2D1 - t148 * t235) + 0.180D3 * t47 * t48 * (t235 - t148 
     #* t230) + t247) * t142 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-(t235 -
     # t174 * t230) * t32 - t235 + t169 * t230) + t279) * t74 * t184 / 0
     #.1440D4 - t187 * t276 * t74 * t190 / 0.8D1 - (-0.90D2 * t4 * t7 * 
     #(t235 - t197 * t230) + t241) * t142 * t184 / 0.720D3 - (-0.90D2 * 
     #t4 * t7 * (-t210 * t235 + t248 + t211 * t230 / 0.2D1) + 0.180D3 * 
     #t47 * t48 * (t235 - t210 * t230) + t247) * t184 / 0.1440D4
      t341 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t340)
      t343 = t2 * x1
      t344 = -0.1D1 + x1
      t345 = t2 * t344
      t346 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t347 = x1 * z
      t348 = 0.1D1 - x1 + t347
      t349 = 0.1D1 / t348
      t350 = t344 ** 2
      t352 = t18 * t349 * t350
      t355 = log(0.4D1 * t171 * t352)
      t356 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t359 = t17 * t349
      t364 = log(-0.4D1 * t166 * t77 * t359 * t350 * t20)
      t367 = x3 * x1
      t368 = t367 * z
      t371 = x3 * t348
      t373 = Sqrt(-t371 * t19)
      t377 = 0.1D1 / (-0.2D1 * t368 + 0.2D1 * t367 - 0.1D1 - x3 + 0.2D1 
     #* t26 * t373)
      t384 = t356 * t377 + t356
      t397 = t194 * t12
      t400 = log(0.4D1 * t397 * t352)
      t406 = t48 * t356
      t417 = log(0.4D1 * t207 * t15 * t359 * t350)
      t418 = t417 ** 2
      t421 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t436 = (-0.90D2 * t4 * t7 * (t346 - t355 * t356 + (t346 - t364 * t
     #356) * t377) + 0.180D3 * t47 * t48 * t384) * t74 * t184 / 0.1440D4
     # - t187 * t384 * t74 * t190 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t346 
     #+ t400 * t356) - 0.180D3 * t47 * t406) * t142 * t184 / 0.720D3 - (
     #-0.90D2 * t4 * t7 * (-t418 * t356 / 0.2D1 - t421 + t417 * t346) + 
     #0.180D3 * t47 * t48 * (t417 * t356 - t346) - t64 * t406) * t184 / 
     #0.1440D4
      t437 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t343, -t345, 0.0D0, t436)
      t439 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t440 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t450 = t440 * t377 + t440
      t468 = t48 * t440
      t476 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t492 = (-0.90D2 * t4 * t7 * ((t439 - t364 * t440) * t377 + t439 - 
     #t355 * t440) + 0.180D3 * t47 * t48 * t450) * t74 * t184 / 0.1440D4
     # - t187 * t450 * t74 * t190 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t439 
     #+ t400 * t440) - 0.180D3 * t47 * t468) * t142 * t184 / 0.720D3 - (
     #-0.90D2 * t4 * t7 * (t417 * t439 - t476 - t418 * t440 / 0.2D1) + 0
     #.180D3 * t47 * t48 * (-t439 + t417 * t440) - t64 * t468) * t184 / 
     #0.1440D4
      t493 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t345, t343, 0.0D0, t492)
      t495 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t496 = t7 * t495
      t500 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t501 = t7 * t500
      t504 = t48 * t495
      t506 = 0.180D3 * t47 * t504
      t512 = t64 * t504
      t513 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t514 = t7 * t513
      t524 = rrqg2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t541 = -t495 * t32 - t495
      t544 = 0.180D3 * t47 * t48 * t541
      t605 = (-0.90D2 * t4 * t496 * t39 + (-0.90D2 * t4 * t501 + t506) *
     # t54 + (0.180D3 * t47 * t48 * t500 + t512 - 0.90D2 * t4 * t514) * 
     #t71) * t74 / 0.2880D4 - t88 * t501 / 0.2880D4 + t4 * t7 * t524 / 0
     #.32D2 - t109 * t496 / 0.2880D4 - t115 * t514 / 0.2880D4 + (-0.90D2
     # * t4 * t7 * (-t500 + t129 * t495 - (t500 - t123 * t495) * t32) + 
     #t544) * t74 * t142 / 0.1440D4 - (-0.90D2 * t4 * t7 * (-t148 * t500
     # + t149 * t495 / 0.2D1 + t513) + 0.180D3 * t47 * t48 * (-t148 * t4
     #95 + t500) + t512) * t142 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-(t50
     #0 - t174 * t495) * t32 - t500 + t169 * t495) + t544) * t74 * t184 
     #/ 0.1440D4 - t187 * t541 * t74 * t190 / 0.8D1 - (-0.90D2 * t4 * t7
     # * (-t197 * t495 + t500) + t506) * t142 * t184 / 0.720D3 - (-0.90D
     #2 * t4 * t7 * (-t210 * t500 + t513 + t211 * t495 / 0.2D1) + 0.180D
     #3 * t47 * t48 * (t500 - t210 * t495) + t512) * t184 / 0.1440D4
      t606 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t605)
      t609 = x2 * s * t1
      t610 = -0.1D1 + x2
      t611 = t610 * s
      t612 = t611 * t1
      t613 = t18 * t610
      t616 = log(-0.4D1 * t120 * t613)
      t617 = x2 * z
      t619 = 0.1D1 / (0.1D1 - x2 + t617)
      t620 = t616 * t619
      t621 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0.
     #0D0, 0.0D0, 0.0D0)
      t623 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0.
     #0D0, 0.0D0, 0.0D0)
      t624 = t619 * t623
      t629 = t47 * t3
      t630 = t7 * t619
      t631 = t630 * t621
      t633 = 0.180D3 * t629 * t631
      t638 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0.
     #0D0, 0.0D0, 0.0D0)
      t642 = log(-0.4D1 * t145 * t613)
      t643 = t642 * t619
      t645 = t642 ** 2
      t646 = t645 * t619
      t658 = t64 * t3
      t663 = t4 * t630
      t670 = log(-0.4D1 * t397 * t613)
      t671 = t670 * t619
      t681 = (-0.90D2 * t4 * t7 * (-t620 * t621 + t624) + t633) * t74 * 
     #t142 / 0.1440D4 - (-0.90D2 * t4 * t7 * (-t619 * t638 + t643 * t623
     # - t646 * t621 / 0.2D1) + 0.180D3 * t47 * t48 * (-t624 + t643 * t6
     #21) - t658 * t631) * t142 / 0.1440D4 - t663 * t621 * t74 * t190 / 
     #0.8D1 - (-0.90D2 * t4 * t7 * (-t624 + t671 * t621) - t633) * t142 
     #* t184 / 0.720D3
      t682 = FJET(XB1, XB2, s, 0.0D0, t609, 0.0D0, -t612, 0.0D0, t681)
      t684 = x2 * x3
      t687 = Sqrt(x3 * t610 * t19)
      t688 = t26 * t687
      t690 = 0.2D1 * t688 * x2
      t692 = 0.1D1 - x3 + t684
      t693 = 0.1D1 / t692
      t695 = t2 * (0.1D1 - x3 - x2 + t684 + t119 + t690) * t693
      t696 = 0.2D1 * t688
      t700 = t2 * x2 * (-0.1D1 + t684 + t696) * t693
      t703 = t692 ** 2
      t709 = log(0.4D1 * t119 * t77 * t17 * t610 * t19 / t703)
      t710 = t119 * z
      t714 = 0.1D1 / (-0.1D1 + t696 + t710 + t684 - t617 + x2 - x3 - t11
     #9 - t690 + 0.2D1 * t688 * t617)
      t715 = t709 * t714
      t716 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t718 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t724 = t7 * t714
      t732 = t4 * t724
      t737 = (-0.90D2 * t4 * t7 * (-t715 * t716 + t714 * t718) + 0.180D3
     # * t629 * t724 * t716) * t74 * t142 / 0.1440D4 - t732 * t716 * t74
     # * t190 / 0.8D1
      t738 = FJET(XB1, XB2, s, 0.0D0, t695, 0.0D0, -t700, 0.0D0, t737)
      t740 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0.
     #0D0, 0.0D0, 0.0D0)
      t742 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0.
     #0D0, 0.0D0, 0.0D0)
      t743 = t619 * t742
      t748 = t630 * t740
      t750 = 0.180D3 * t629 * t748
      t755 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0.
     #0D0, 0.0D0, 0.0D0)
      t786 = (-0.90D2 * t4 * t7 * (-t620 * t740 + t743) + t750) * t74 * 
     #t142 / 0.1440D4 - (-0.90D2 * t4 * t7 * (-t619 * t755 + t643 * t742
     # - t646 * t740 / 0.2D1) + 0.180D3 * t47 * t48 * (-t743 + t643 * t7
     #40) - t658 * t748) * t142 / 0.1440D4 - t663 * t740 * t74 * t190 / 
     #0.8D1 - (-0.90D2 * t4 * t7 * (t671 * t740 - t743) - t750) * t142 *
     # t184 / 0.720D3
      t787 = FJET(XB1, XB2, s, 0.0D0, -t612, 0.0D0, t609, 0.0D0, t786)
      t789 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t791 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0.
     #0D0, 0.0D0, 0.0D0)
      t808 = (-0.90D2 * t4 * t7 * (t714 * t789 - t715 * t791) + 0.180D3 
     #* t629 * t724 * t791) * t74 * t142 / 0.1440D4 - t732 * t791 * t74 
     #* t190 / 0.8D1
      t809 = FJET(XB1, XB2, s, 0.0D0, -t700, 0.0D0, t695, 0.0D0, t808)
      t813 = t2 * t344 * x2 * t349
      t815 = t611 * t1 * t344
      t820 = s * t16 * x2 * t344 * x1 * t349
      t821 = t7 * t348
      t822 = t4 * t821
      t823 = x2 * x1
      t824 = t823 * z
      t826 = 0.1D1 / (-0.1D1 + t824 - t823 + x1 + x2 - t347 - t617)
      t827 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0.
     #0D0, t343, -t820)
      t828 = t826 * t827
      t830 = t74 * t142 * t184
      t839 = log(-0.4D1 * t194 * t77 * t359 * t350 * t610)
      t840 = t839 * t348
      t842 = t348 * t826
      t843 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0.
     #0D0, t343, -t820)
      t856 = -t822 * t828 * t830 / 0.8D1 - (-0.90D2 * t4 * t7 * (t840 * 
     #t828 - t842 * t843) - 0.180D3 * t629 * t821 * t828) * t142 * t184 
     #/ 0.720D3
      t857 = FJET(XB1, XB2, s, 0.0D0, -t813, t343, t815, -t820, t856)
      t859 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t860 = t7 * t859
      t864 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t865 = t7 * t864
      t868 = t48 * t859
      t870 = 0.180D3 * t47 * t868
      t876 = t64 * t868
      t877 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t878 = t7 * t877
      t886 = rrqg2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t903 = -t859 - t859 * t32
      t906 = 0.180D3 * t47 * t48 * t903
      t969 = (-0.90D2 * t4 * t860 * t39 + (-0.90D2 * t4 * t865 + t870) *
     # t54 + (0.180D3 * t47 * t48 * t864 + t876 - 0.90D2 * t4 * t878) * 
     #t71) * t74 / 0.2880D4 + t4 * t7 * t886 / 0.32D2 - t115 * t878 / 0.
     #2880D4 - t88 * t865 / 0.2880D4 + (-0.90D2 * t4 * t7 * (-(t864 - t1
     #23 * t859) * t32 - t864 + t129 * t859) + t906) * t74 * t142 / 0.14
     #40D4 - (-0.90D2 * t4 * t7 * (t877 + t149 * t859 / 0.2D1 - t148 * t
     #864) + 0.180D3 * t47 * t48 * (t864 - t148 * t859) + t876) * t142 /
     # 0.1440D4 - t109 * t860 / 0.2880D4 + (-0.90D2 * t4 * t7 * (-(t864 
     #- t174 * t859) * t32 + t169 * t859 - t864) + t906) * t74 * t184 / 
     #0.1440D4 - t187 * t903 * t74 * t190 / 0.8D1 - (-0.90D2 * t4 * t7 *
     # (-t197 * t859 + t864) + t870) * t142 * t184 / 0.720D3 - (-0.90D2 
     #* t4 * t7 * (-t210 * t864 + t877 + t211 * t859 / 0.2D1) + 0.180D3 
     #* t47 * t48 * (-t210 * t859 + t864) + t876) * t184 / 0.1440D4
      t970 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t969)
      t972 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0.
     #0D0, t343, -t820)
      t973 = t826 * t972
      t978 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0.
     #0D0, t343, -t820)
      t991 = -t822 * t973 * t830 / 0.8D1 - (-0.90D2 * t4 * t7 * (t840 * 
     #t973 - t842 * t978) - 0.180D3 * t629 * t821 * t973) * t142 * t184 
     #/ 0.720D3
      t992 = FJET(XB1, XB2, s, t343, t815, 0.0D0, -t813, -t820, t991)
      t994 = t228 * t227 + t341 * t340 + t437 * t436 + t493 * t492 + t60
     #6 * t605 + t682 * t681 + t738 * t737 + t787 * t786 + t809 * t808 +
     # t857 * t856 + t970 * t969 + t992 * t991
      t995 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t996 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 0
     #.0D0, t343, 0.0D0)
      t1006 = t996 + t996 * t377
      t1024 = t48 * t996
      t1033 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 
     #0.0D0, t343, 0.0D0)
      t1048 = (-0.90D2 * t4 * t7 * (t995 + (t995 - t364 * t996) * t377 -
     # t355 * t996) + 0.180D3 * t47 * t48 * t1006) * t74 * t184 / 0.1440
     #D4 - t187 * t1006 * t74 * t190 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t9
     #95 + t400 * t996) - 0.180D3 * t47 * t1024) * t142 * t184 / 0.720D3
     # - (-0.90D2 * t4 * t7 * (-t418 * t996 / 0.2D1 - t1033 + t417 * t99
     #5) + 0.180D3 * t47 * t48 * (-t995 + t417 * t996) - t64 * t1024) * 
     #t184 / 0.1440D4
      t1049 = FJET(XB1, XB2, s, t343, -t345, 0.0D0, 0.0D0, 0.0D0, t1048)
      t1051 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0
     #.0D0, 0.0D0, 0.0D0)
      t1052 = t619 * t1051
      t1053 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0
     #.0D0, 0.0D0, 0.0D0)
      t1059 = t630 * t1053
      t1061 = 0.180D3 * t629 * t1059
      t1067 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0
     #.0D0, 0.0D0, 0.0D0)
      t1097 = (-0.90D2 * t4 * t7 * (t1052 - t620 * t1053) + t1061) * t74
     # * t142 / 0.1440D4 - (-0.90D2 * t4 * t7 * (t643 * t1051 - t619 * t
     #1067 - t646 * t1053 / 0.2D1) + 0.180D3 * t47 * t48 * (-t1052 + t64
     #3 * t1053) - t658 * t1059) * t142 / 0.1440D4 - t663 * t1053 * t74 
     #* t190 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t1052 + t671 * t1053) - t1
     #061) * t142 * t184 / 0.720D3
      t1098 = FJET(XB1, XB2, s, t609, 0.0D0, -t612, 0.0D0, 0.0D0, t1097)
      t1100 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0
     #.0D0, 0.0D0, 0.0D0)
      t1102 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0
     #.0D0, 0.0D0, 0.0D0)
      t1119 = (-0.90D2 * t4 * t7 * (t714 * t1100 - t715 * t1102) + 0.180
     #D3 * t629 * t724 * t1102) * t74 * t142 / 0.1440D4 - t732 * t1102 *
     # t74 * t190 / 0.8D1
      t1120 = FJET(XB1, XB2, s, t695, 0.0D0, -t700, 0.0D0, 0.0D0, t1119)
      t1122 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0
     #.0D0, t343, -t820)
      t1123 = t826 * t1122
      t1127 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0
     #.0D0, t343, -t820)
      t1141 = -t822 * t1123 * t830 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t842
     # * t1127 + t840 * t1123) - 0.180D3 * t629 * t821 * t1123) * t142 *
     # t184 / 0.720D3
      t1142 = FJET(XB1, XB2, s, t815, t343, -t813, 0.0D0, -t820, t1141)
      t1145 = t343 * t684 * t693
      t1146 = t684 * x1
      t1147 = t684 * t347
      t1150 = Sqrt(t371 * t610 * t19)
      t1151 = t26 * t1150
      t1152 = 0.2D1 * t1151
      t1157 = t345 * x2 * (t367 - t368 + t684 - t1146 + t1147 - 0.1D1 + 
     #t1152) * t349 * t693
      t1161 = t19 * s * t1 * x1 * t693
      t1163 = 0.2D1 * t1151 * x2
      t1164 = 0.1D1 - x1 + t347 - x2 + t823 - t824 - x3 + t367 - t368 + 
     #t684 - t1146 + t1147 + t119 + t1163
      t1167 = t345 * t1164 * t349 * t693
      t1180 = 0.1D1 + t823 + 0.3D1 * t368 - t824 + 0.3D1 * t1146 - t1152
     # + 0.2D1 * t166 - t710 + t617 - t684 + t119 + x3 - 0.4D1 * t1147 -
     # 0.2D1 * t1151 * t347 - 0.2D1 * t1151 * t617 - 0.2D1 * t1151 * t82
     #3 - t119 * t14 * x1
      t1209 = -0.2D1 * t119 * t165 * z + t119 * t165 * t14 + x3 * t14 * 
     #t823 + 0.4D1 * t166 * t617 - 0.2D1 * t166 * t14 * x2 + 0.3D1 * t11
     #9 * t347 - x2 + t1163 - 0.2D1 * t166 * x2 - 0.2D1 * t119 * x1 + 0.
     #2D1 * t1151 * x1 - 0.4D1 * t166 * z + 0.2D1 * t166 * t14 + t119 * 
     #t165 + 0.2D1 * t1151 * t824 + t347 - 0.3D1 * t367 - x1
      t1211 = 0.1D1 / (t1180 + t1209)
      t1212 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1167,
     # t1145, -t1161, -t820)
      t1214 = t1211 * t1212 * t830
      t1217 = FJET(XB1, XB2, s, t1145, t1157, -t1161, -t1167, -t820, -t8
     #22 * t1214 / 0.8D1)
      t1219 = t48 * t348
      t1223 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1167,
     # t1145, -t1161, -t820)
      t1225 = t1211 * t1223 * t830
      t1228 = FJET(XB1, XB2, s, t1157, t1145, -t1167, -t1161, -t820, -t8
     #22 * t1225 / 0.8D1)
      t1233 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 
     #0.0D0, t343, 0.0D0)
      t1235 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 
     #0.0D0, t343, 0.0D0)
      t1244 = t1233 + t1233 * t377
      t1262 = t48 * t1233
      t1271 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t345, 
     #0.0D0, t343, 0.0D0)
      t1286 = (-0.90D2 * t4 * t7 * (-t355 * t1233 + t1235 + (t1235 - t36
     #4 * t1233) * t377) + 0.180D3 * t47 * t48 * t1244) * t74 * t184 / 0
     #.1440D4 - t187 * t1244 * t74 * t190 / 0.8D1 - (-0.90D2 * t4 * t7 *
     # (-t1235 + t400 * t1233) - 0.180D3 * t47 * t1262) * t142 * t184 / 
     #0.720D3 - (-0.90D2 * t4 * t7 * (-t418 * t1233 / 0.2D1 - t1271 + t4
     #17 * t1235) + 0.180D3 * t47 * t48 * (t417 * t1233 - t1235) - t64 *
     # t1262) * t184 / 0.1440D4
      t1287 = FJET(XB1, XB2, s, -t345, t343, 0.0D0, 0.0D0, 0.0D0, t1286)
      t1289 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0
     #.0D0, 0.0D0, 0.0D0)
      t1291 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0
     #.0D0, 0.0D0, 0.0D0)
      t1292 = t619 * t1291
      t1297 = t630 * t1289
      t1299 = 0.180D3 * t629 * t1297
      t1305 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, t609, -t612, 0
     #.0D0, 0.0D0, 0.0D0)
      t1335 = (-0.90D2 * t4 * t7 * (-t620 * t1289 + t1292) + t1299) * t7
     #4 * t142 / 0.1440D4 - (-0.90D2 * t4 * t7 * (t643 * t1291 - t619 * 
     #t1305 - t646 * t1289 / 0.2D1) + 0.180D3 * t47 * t48 * (-t1292 + t6
     #43 * t1289) - t658 * t1297) * t142 / 0.1440D4 - t663 * t1289 * t74
     # * t190 / 0.8D1 - (-0.90D2 * t4 * t7 * (t671 * t1289 - t1292) - t1
     #299) * t142 * t184 / 0.720D3
      t1336 = FJET(XB1, XB2, s, -t612, 0.0D0, t609, 0.0D0, 0.0D0, t1335)
      t1338 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0
     #.0D0, 0.0D0, 0.0D0)
      t1340 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t700, t695, 0
     #.0D0, 0.0D0, 0.0D0)
      t1357 = (-0.90D2 * t4 * t7 * (t714 * t1338 - t715 * t1340) + 0.180
     #D3 * t629 * t724 * t1340) * t74 * t142 / 0.1440D4 - t732 * t1340 *
     # t74 * t190 / 0.8D1
      t1358 = FJET(XB1, XB2, s, -t700, 0.0D0, t695, 0.0D0, 0.0D0, t1357)
      t1360 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0
     #.0D0, t343, -t820)
      t1361 = t826 * t1360
      t1365 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t813, t815, 0
     #.0D0, t343, -t820)
      t1379 = -t822 * t1361 * t830 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t842
     # * t1365 + t840 * t1361) - 0.180D3 * t629 * t821 * t1361) * t142 *
     # t184 / 0.720D3
      t1380 = FJET(XB1, XB2, s, -t813, 0.0D0, t815, t343, -t820, t1379)
      t1382 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1167,
     # t1145, -t1161, -t820)
      t1384 = t1211 * t1382 * t830
      t1387 = FJET(XB1, XB2, s, -t1161, -t1167, t1145, t1157, -t820, -t8
     #22 * t1384 / 0.8D1)
      t1392 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t1157, -t1167,
     # t1145, -t1161, -t820)
      t1394 = t1211 * t1392 * t830
      t1397 = FJET(XB1, XB2, s, -t1167, -t1161, t1157, t1145, -t820, -t8
     #22 * t1394 / 0.8D1)
      t1402 = t1049 * t1048 + t1098 * t1097 + t1120 * t1119 + t1142 * t1
     #141 - t1217 * pi * t1219 * t1214 / 0.8D1 - t1228 * pi * t1219 * t1
     #225 / 0.8D1 + t1287 * t1286 + t1336 * t1335 + t1358 * t1357 + t138
     #0 * t1379 - t1387 * pi * t1219 * t1384 / 0.8D1 - t1397 * pi * t121
     #9 * t1394 / 0.8D1
      rrqg2qght7s1e0 = t994 + t1402

      end function



      doubleprecision function rrqg2qght7s1em1
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t8 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
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
      t31 = 0.1D1 / (-0.1D1 + 0.2D1 * t25 * t27 - x3)
      t35 = log(0.4D1 * t13 * t18)
      t36 = t24 * t31 + t35
      t40 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t41 = t7 * t40
      t44 = pi * lh
      t45 = t3 * t7
      t48 = 0.180D3 * t44 * t45 * t8
      t50 = -t31 - 0.1D1
      t53 = 0.1D1 / x3
      t60 = log(0.4D1 * t15 * t12 * t17)
      t61 = t60 * pi
      t64 = (0.180D3 * t44 + 0.90D2 * t61) * t3
      t69 = t60 ** 2
      t72 = lh ** 2
      t74 = pi ** 2
      t79 = (-0.180D3 * lh * t61 - 0.45D2 * t69 * pi + pi * (-0.180D3 * 
     #t72 + 0.30D2 * t74)) * t3
      t82 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t86 = t4 * t7
      t89 = (-t8 - t8 * t31) * t53
      t90 = 0.1D1 / x2
      t94 = x2 ** 2
      t95 = t94 * t12
      t98 = log(0.4D1 * t95 * t18)
      t108 = 0.1D1 / x1
      t112 = x1 ** 2
      t113 = t112 * t12
      t116 = log(0.4D1 * t113 * t18)
      t128 = (-0.90D2 * t4 * t9 * t36 + (-0.90D2 * t4 * t41 + t48) * t50
     #) * t53 / 0.2880D4 - t64 * t41 / 0.2880D4 - t79 * t9 / 0.2880D4 + 
     #t4 * t7 * t82 / 0.32D2 - t86 * t89 * t90 / 0.16D2 - (-0.90D2 * t4 
     #* t7 * (-t98 * t8 + t40) + t48) * t90 / 0.1440D4 + t86 * t8 * t90 
     #* t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (t40 - t116 * t8) + t48) * t
     #108 / 0.1440D4 - t86 * t89 * t108 / 0.16D2
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t128)
      t131 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t132 = t7 * t131
      t136 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t137 = t7 * t136
      t142 = 0.180D3 * t44 * t45 * t131
      t152 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t158 = (-t131 - t131 * t31) * t53
      t185 = (-0.90D2 * t4 * t132 * t36 + (-0.90D2 * t4 * t137 + t142) *
     # t50) * t53 / 0.2880D4 - t64 * t137 / 0.2880D4 - t79 * t132 / 0.28
     #80D4 + t4 * t7 * t152 / 0.32D2 - t86 * t158 * t90 / 0.16D2 - (-0.9
     #0D2 * t4 * t7 * (t136 - t98 * t131) + t142) * t90 / 0.1440D4 + t86
     # * t131 * t90 * t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (t136 - t116 *
     # t131) + t142) * t108 / 0.1440D4 - t86 * t158 * t108 / 0.16D2
      t186 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t185)
      t188 = t2 * x1
      t189 = -0.1D1 + x1
      t190 = t2 * t189
      t191 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t197 = x1 * z
      t198 = 0.1D1 - x1 + t197
      t199 = 0.1D1 / t198
      t201 = t189 ** 2
      t205 = log(0.4D1 * t113 * t15 * t17 * t199 * t201)
      t207 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t218 = x3 * x1
      t224 = Sqrt(-x3 * t198 * t19)
      t228 = 0.1D1 / (-0.2D1 * t218 * z + 0.2D1 * t218 - 0.1D1 - x3 + 0.
     #2D1 * t25 * t224)
      t235 = -t86 * t191 * t90 * t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (t2
     #05 * t191 - t207) - 0.180D3 * t44 * t45 * t191) * t108 / 0.1440D4 
     #- t86 * (t191 * t228 + t191) * t53 * t108 / 0.16D2
      t236 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t188, -t190, 0.0D0, t235)
      t238 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t243 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t261 = -t86 * t238 * t90 * t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t
     #243 + t205 * t238) - 0.180D3 * t44 * t45 * t238) * t108 / 0.1440D4
     # - t86 * (t238 * t228 + t238) * t53 * t108 / 0.16D2
      t262 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t190, t188, 0.0D0, t261)
      t264 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t265 = t7 * t264
      t269 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t270 = t7 * t269
      t275 = 0.180D3 * t44 * t45 * t264
      t285 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t291 = (-t264 * t31 - t264) * t53
      t318 = (-0.90D2 * t4 * t265 * t36 + (-0.90D2 * t4 * t270 + t275) *
     # t50) * t53 / 0.2880D4 - t64 * t270 / 0.2880D4 - t79 * t265 / 0.28
     #80D4 + t4 * t7 * t285 / 0.32D2 - t86 * t291 * t90 / 0.16D2 - (-0.9
     #0D2 * t4 * t7 * (-t98 * t264 + t269) + t275) * t90 / 0.1440D4 + t8
     #6 * t264 * t90 * t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (t269 - t116 
     #* t264) + t275) * t108 / 0.1440D4 - t86 * t291 * t108 / 0.16D2
      t319 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t318)
      t322 = x2 * s * t1
      t323 = -0.1D1 + x2
      t324 = t323 * s
      t325 = t324 * t1
      t326 = x2 * z
      t328 = 0.1D1 / (0.1D1 - x2 + t326)
      t329 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t330 = t328 * t329
      t331 = t53 * t90
      t335 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t340 = log(-0.4D1 * t95 * t18 * t323)
      t341 = t340 * t328
      t347 = t44 * t3
      t348 = t7 * t328
      t355 = t90 * t108
      t359 = -t86 * t330 * t331 / 0.16D2 - (-0.90D2 * t4 * t7 * (-t328 *
     # t335 + t341 * t329) - 0.180D3 * t347 * t348 * t329) * t90 / 0.144
     #0D4 - t86 * t330 * t355 / 0.8D1
      t360 = FJET(XB1, XB2, s, 0.0D0, t322, 0.0D0, -t325, 0.0D0, t359)
      t362 = x2 * x3
      t363 = t94 * x3
      t366 = Sqrt(x3 * t323 * t19)
      t367 = t25 * t366
      t369 = 0.2D1 * t367 * x2
      t372 = 0.1D1 / (0.1D1 - x3 + t362)
      t374 = t2 * (0.1D1 - x3 - x2 + t362 + t363 + t369) * t372
      t375 = 0.2D1 * t367
      t379 = t2 * x2 * (-0.1D1 + t362 + t375) * t372
      t384 = 0.1D1 / (-0.1D1 + t375 + t363 * z + t362 - t326 + x2 - x3 -
     # t363 - t369 + 0.2D1 * t367 * t326)
      t385 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t379, t374, 0.
     #0D0, 0.0D0, 0.0D0)
      t387 = t384 * t385 * t331
      t390 = FJET(XB1, XB2, s, 0.0D0, t374, 0.0D0, -t379, 0.0D0, -t86 * 
     #t387 / 0.16D2)
      t395 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t396 = t328 * t395
      t400 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t416 = -t86 * t396 * t331 / 0.16D2 - (-0.90D2 * t4 * t7 * (-t328 *
     # t400 + t341 * t395) - 0.180D3 * t347 * t348 * t395) * t90 / 0.144
     #0D4 - t86 * t396 * t355 / 0.8D1
      t417 = FJET(XB1, XB2, s, 0.0D0, -t325, 0.0D0, t322, 0.0D0, t416)
      t419 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t379, t374, 0.
     #0D0, 0.0D0, 0.0D0)
      t421 = t384 * t419 * t331
      t424 = FJET(XB1, XB2, s, 0.0D0, -t379, 0.0D0, t374, 0.0D0, -t86 * 
     #t421 / 0.16D2)
      t431 = t2 * t189 * x2 * t199
      t433 = t324 * t1 * t189
      t438 = s * t16 * x2 * t189 * x1 * t199
      t440 = t4 * t7 * t198
      t441 = x2 * x1
      t444 = 0.1D1 / (-0.1D1 + t441 * z - t441 + x1 + x2 - t197 - t326)
      t445 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t431, t433, 0.
     #0D0, t188, -t438)
      t450 = FJET(XB1, XB2, s, 0.0D0, -t431, t188, t433, -t438, -t440 * 
     #t444 * t445 * t355 / 0.8D1)
      t453 = t198 * t444
      t459 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t460 = t7 * t459
      t463 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t467 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t468 = t7 * t467
      t476 = 0.180D3 * t44 * t45 * t467
      t484 = (-t467 - t467 * t31) * t53
      t513 = -t64 * t460 / 0.2880D4 + t4 * t7 * t463 / 0.32D2 + (-0.90D2
     # * t4 * t468 * t36 + (-0.90D2 * t4 * t460 + t476) * t50) * t53 / 0
     #.2880D4 - t86 * t484 * t90 / 0.16D2 - (-0.90D2 * t4 * t7 * (t459 -
     # t98 * t467) + t476) * t90 / 0.1440D4 - t79 * t468 / 0.2880D4 + t8
     #6 * t467 * t90 * t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t116 * t467
     # + t459) + t476) * t108 / 0.1440D4 - t86 * t484 * t108 / 0.16D2
      t514 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t513)
      t516 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t431, t433, 0.
     #0D0, t188, -t438)
      t521 = FJET(XB1, XB2, s, t188, t433, 0.0D0, -t431, -t438, -t440 * 
     #t444 * t516 * t355 / 0.8D1)
      t529 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t534 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t552 = -t86 * t529 * t90 * t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t
     #534 + t205 * t529) - 0.180D3 * t44 * t45 * t529) * t108 / 0.1440D4
     # - t86 * (t529 + t529 * t228) * t53 * t108 / 0.16D2
      t553 = FJET(XB1, XB2, s, t188, -t190, 0.0D0, 0.0D0, 0.0D0, t552)
      t555 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t556 = t328 * t555
      t560 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t576 = -t86 * t556 * t331 / 0.16D2 - (-0.90D2 * t4 * t7 * (-t328 *
     # t560 + t341 * t555) - 0.180D3 * t347 * t348 * t555) * t90 / 0.144
     #0D4 - t86 * t556 * t355 / 0.8D1
      t577 = FJET(XB1, XB2, s, t322, 0.0D0, -t325, 0.0D0, 0.0D0, t576)
      t579 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t379, t374, 0.
     #0D0, 0.0D0, 0.0D0)
      t581 = t384 * t579 * t331
      t584 = FJET(XB1, XB2, s, t374, 0.0D0, -t379, 0.0D0, 0.0D0, -t86 * 
     #t581 / 0.16D2)
      t589 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t431, t433, 0.
     #0D0, t188, -t438)
      t594 = FJET(XB1, XB2, s, t433, t188, -t431, 0.0D0, -t438, -t440 * 
     #t444 * t589 * t355 / 0.8D1)
      t602 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t608 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t190, 0
     #.0D0, t188, 0.0D0)
      t625 = -t86 * t602 * t90 * t108 / 0.8D1 - (-0.90D2 * t4 * t7 * (t2
     #05 * t602 - t608) - 0.180D3 * t44 * t45 * t602) * t108 / 0.1440D4 
     #- t86 * (t602 + t602 * t228) * t53 * t108 / 0.16D2
      t626 = FJET(XB1, XB2, s, -t190, t188, 0.0D0, 0.0D0, 0.0D0, t625)
      t628 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t629 = t328 * t628
      t633 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t322, -t325, 0.
     #0D0, 0.0D0, 0.0D0)
      t649 = -t86 * t629 * t331 / 0.16D2 - (-0.90D2 * t4 * t7 * (-t328 *
     # t633 + t341 * t628) - 0.180D3 * t347 * t348 * t628) * t90 / 0.144
     #0D4 - t86 * t629 * t355 / 0.8D1
      t650 = FJET(XB1, XB2, s, -t325, 0.0D0, t322, 0.0D0, 0.0D0, t649)
      t652 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t379, t374, 0.
     #0D0, 0.0D0, 0.0D0)
      t654 = t384 * t652 * t331
      t657 = FJET(XB1, XB2, s, -t379, 0.0D0, t374, 0.0D0, 0.0D0, -t86 * 
     #t654 / 0.16D2)
      t662 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t431, t433, 0.
     #0D0, t188, -t438)
      t667 = FJET(XB1, XB2, s, -t431, 0.0D0, t433, t188, -t438, -t440 * 
     #t444 * t662 * t355 / 0.8D1)
      rrqg2qght7s1em1 = t129 * t128 + t186 * t185 + t236 * t235 + t262 *
     # t261 + t319 * t318 + t360 * t359 - t390 * pi * t45 * t387 / 0.16D
     #2 + t417 * t416 - t424 * pi * t45 * t421 / 0.16D2 - t450 * pi * t4
     #5 * t453 * t445 * t90 * t108 / 0.8D1 + t514 * t513 - t521 * pi * t
     #45 * t453 * t516 * t90 * t108 / 0.8D1 + t553 * t552 + t577 * t576 
     #- t584 * pi * t45 * t581 / 0.16D2 - t594 * pi * t45 * t453 * t589 
     #* t90 * t108 / 0.8D1 + t626 * t625 + t650 * t649 - t657 * pi * t45
     # * t654 / 0.16D2 - t667 * pi * t45 * t453 * t662 * t90 * t108 / 0.
     #8D1

      end function



      doubleprecision function rrqg2qght7s1em2
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t9 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = cos(t10)
      t14 = Sqrt(-x3 * (-0.1D1 + x3))
      t19 = -0.1D1 / (-0.1D1 + 0.2D1 * t11 * t14 - x3) - 0.1D1
      t21 = 0.1D1 / x3
      t25 = t7 * t9
      t26 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t34 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t40 = z ** 2
      t42 = Sin(t10)
      t43 = t42 ** 2
      t45 = t1 ** 2
      t46 = t45 ** 2
      t49 = log(0.4D1 / t40 * t43 * t46)
      t53 = (0.180D3 * pi * lh + 0.90D2 * t49 * pi) * t3
      t56 = -t8 * t9 * t19 * t21 / 0.32D2 + t4 * t25 * t26 / 0.16D2 + t4
     # * t25 * t30 / 0.16D2 + t4 * t7 * t34 / 0.32D2 - t53 * t25 / 0.288
     #0D4
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t56)
      t59 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t64 = t7 * t59
      t71 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t77 = -t8 * t59 * t19 * t21 / 0.32D2 + t4 * t64 * t26 / 0.16D2 + t
     #4 * t64 * t30 / 0.16D2 + t4 * t7 * t71 / 0.32D2 - t53 * t64 / 0.28
     #80D4
      t78 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t77)
      t80 = t2 * x1
      t82 = t2 * (-0.1D1 + x1)
      t83 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.0
     #D0, t80, 0.0D0)
      t85 = t7 * t83 * t30
      t88 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t80, -t82, 0.0D0, -t4 * t85 
     #/ 0.16D2)
      t93 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.0
     #D0, t80, 0.0D0)
      t95 = t7 * t93 * t30
      t98 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t82, t80, 0.0D0, -t4 * t95 
     #/ 0.16D2)
      t103 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t108 = t7 * t103
      t115 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t121 = -t8 * t103 * t19 * t21 / 0.32D2 + t4 * t108 * t26 / 0.16D2 
     #+ t4 * t108 * t30 / 0.16D2 + t4 * t7 * t115 / 0.32D2 - t53 * t108 
     #/ 0.2880D4
      t122 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t121)
      t125 = x2 * s * t1
      t128 = (-0.1D1 + x2) * s * t1
      t131 = 0.1D1 / (0.1D1 - x2 + x2 * z)
      t132 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t137 = FJET(XB1, XB2, s, 0.0D0, t125, 0.0D0, -t128, 0.0D0, -t8 * t
     #131 * t132 * t26 / 0.16D2)
      t140 = t7 * t131
      t145 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t150 = FJET(XB1, XB2, s, 0.0D0, -t128, 0.0D0, t125, 0.0D0, -t8 * t
     #131 * t145 * t26 / 0.16D2)
      t157 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t162 = t7 * t157
      t169 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t175 = -t8 * t157 * t19 * t21 / 0.32D2 + t4 * t162 * t26 / 0.16D2 
     #+ t4 * t162 * t30 / 0.16D2 + t4 * t7 * t169 / 0.32D2 - t53 * t162 
     #/ 0.2880D4
      t176 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t175)
      t178 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.
     #0D0, t80, 0.0D0)
      t180 = t7 * t178 * t30
      t183 = FJET(XB1, XB2, s, t80, -t82, 0.0D0, 0.0D0, 0.0D0, -t4 * t18
     #0 / 0.16D2)
      t188 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t193 = FJET(XB1, XB2, s, t125, 0.0D0, -t128, 0.0D0, 0.0D0, -t8 * t
     #131 * t188 * t26 / 0.16D2)
      t200 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t125, -t128, 0.
     #0D0, 0.0D0, 0.0D0)
      t205 = FJET(XB1, XB2, s, -t128, 0.0D0, t125, 0.0D0, 0.0D0, -t8 * t
     #131 * t200 * t26 / 0.16D2)
      t212 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t82, 0.
     #0D0, t80, 0.0D0)
      t214 = t7 * t212 * t30
      t217 = FJET(XB1, XB2, s, -t82, t80, 0.0D0, 0.0D0, 0.0D0, -t4 * t21
     #4 / 0.16D2)
      rrqg2qght7s1em2 = t57 * t56 + t78 * t77 - t88 * pi * t3 * t85 / 0.
     #16D2 - t98 * pi * t3 * t95 / 0.16D2 + t122 * t121 - t137 * pi * t3
     # * t140 * t132 * t26 / 0.16D2 - t150 * pi * t3 * t140 * t145 * t26
     # / 0.16D2 + t176 * t175 - t183 * pi * t3 * t180 / 0.16D2 - t193 * 
     #pi * t3 * t140 * t188 * t26 / 0.16D2 - t205 * pi * t3 * t140 * t20
     #0 * t26 / 0.16D2 - t217 * pi * t3 * t214 / 0.16D2

      end function



      doubleprecision function rrqg2qght7s1em3
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t8 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t7 * 
     #t8 / 0.32D2)
      t14 = t3 * t7
      t17 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t7 * 
     #t17 / 0.32D2)
      t25 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t29 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t25 / 0.32D2)
      t33 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t37 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t33 / 0.32D2)
      rrqg2qght7s1em3 = t12 * pi * t14 * t8 / 0.32D2 + t21 * pi * t14 * 
     #t17 / 0.32D2 + t29 * pi * t14 * t25 / 0.32D2 + t37 * pi * t14 * t3
     #3 / 0.32D2

      end function



      doubleprecision function rrqg2qght7s1em4
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght7s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght7s2e1
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t9 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = t8 * t9
      t13 = lh ** 2
      t15 = pi ** 2
      t17 = -0.180D3 * t13 + 0.30D2 * t15
      t18 = pi * t17
      t19 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = t8 * t19
      t22 = pi * t4
      t23 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = t7 * t23
      t27 = 0.180D3 * t3 * t10 + t18 * t20 - 0.90D2 * t22 * t24
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
      t52 = 0.1D1 / (-t45 - 0.1D1 + x3 + 0.2D1 * t46 * t48)
      t56 = log(0.4D1 * t31 * t37)
      t57 = -t43 * t52 - t56
      t59 = t7 * t19
      t60 = t56 ** 2
      t62 = t43 ** 2
      t66 = -t60 * t56 / 0.6D1 - t62 * t43 * t52 / 0.6D1
      t71 = rrqg2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t72 = t7 * t71
      t80 = -0.60D2 * lh * t15 + 0.240D3 * zeta3 + 0.120D3 * t13 * lh
      t81 = pi * t80
      t82 = t81 * t20
      t87 = t52 + 0.1D1
      t89 = t7 * t9
      t94 = -0.90D2 * t22 * t89 + 0.180D3 * t3 * t20
      t97 = t62 * t52 / 0.2D1 + t60 / 0.2D1
      t100 = 0.1D1 / x3
      t103 = x2 ** 2
      t104 = x3 * t103
      t105 = t104 * t30
      t106 = -0.1D1 + x2
      t107 = t37 * t106
      t110 = log(-0.4D1 * t105 * t107)
      t112 = t110 ** 2
      t115 = t30 * t34
      t116 = t115 * t36
      t119 = log(0.4D1 * t104 * t116)
      t121 = t119 ** 2
      t126 = log(-0.4D1 * t105 * t40)
      t128 = t126 ** 2
      t146 = t18 * t4
      t147 = t59 * t52
      t151 = 0.1D1 / x2
      t154 = t30 * t103
      t157 = log(0.4D1 * t154 * t37)
      t158 = t157 ** 2
      t161 = log(-0.4D1 * t154 * t107)
      t162 = t161 ** 2
      t164 = t158 / 0.2D1 - t162 / 0.2D1
      t169 = t162 * t161 / 0.6D1 - t158 * t157 / 0.6D1
      t173 = -t157 + t161
      t179 = log(0.4D1 * t116)
      t180 = t179 ** 2
      t181 = t180 * pi
      t185 = t180 * t179 * pi
      t187 = t179 * pi
      t190 = (0.90D2 * t181 * lh + t81 + 0.15D2 * t185 - t187 * t17) * t
     #4
      t197 = (-0.180D3 * t187 * lh - 0.45D2 * t181 + t18) * t4
      t203 = (0.180D3 * t3 + 0.90D2 * t187) * t4
      t206 = rrqg2qgh71J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
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
      t286 = log(-0.4D1 * t281 * t115 * t36 * t39)
      t292 = log(0.4D1 * t281 * t116)
      t294 = t36 * t106
      t298 = log(-0.4D1 * t281 * t115 * t294)
      t304 = t3 * t4
      t309 = t151 * t249
      t312 = t103 * t210
      t315 = log(0.4D1 * t312 * t116)
      t316 = t315 ** 2
      t319 = t312 * t34
      t323 = log(-0.4D1 * t319 * t220 * t106)
      t325 = t323 ** 2
      t350 = t15 ** 2
      t351 = t13 ** 2
      t357 = t180 ** 2
      t361 = (-0.30D2 * t185 * lh + t181 * t17 / 0.2D1 - t187 * t80 + pi
     # * (-0.480D3 * lh * zeta3 - t350 - 0.60D2 * t351 + 0.60D2 * t13 * 
     #t15) - 0.15D2 / 0.4D1 * t357 * pi) * t4
      t364 = -(t27 * t57 - 0.90D2 * t22 * t59 * t66 + (t18 * t10 - 0.90D
     #2 * t22 * t72 + t82 + 0.180D3 * t3 * t8 * t23) * t87 + t94 * t97) 
     #* t100 / 0.2880D4 + (-0.90D2 * t22 * t7 * (-t110 * t9 + t112 * t19
     # / 0.2D1 + t119 * t9 - t121 * t19 / 0.2D1 - (t23 - t126 * t9 + t12
     #8 * t19 / 0.2D1) * t52) + 0.180D3 * t3 * t8 * (-t110 * t19 + t119 
     #* t19 - (t9 - t126 * t19) * t52) - t146 * t147) * t100 * t151 / 0.
     #1440D4 - (t94 * t164 - 0.90D2 * t22 * t59 * t169 + t27 * t173) * t
     #151 / 0.1440D4 - t190 * t89 / 0.2880D4 - t197 * t24 / 0.2880D4 - t
     #203 * t72 / 0.2880D4 + t22 * t7 * t206 / 0.32D2 + (-0.90D2 * t22 *
     # t7 * (-t23 + t214 * t9 - t216 * t19 / 0.2D1 - (t23 - t224 * t9 + 
     #t226 * t19 / 0.2D1) * t52) + 0.180D3 * t3 * t8 * (-t9 + t214 * t19
     # - (t9 - t224 * t19) * t52) + t18 * t8 * (-t19 - t19 * t52)) * t10
     #0 * t249 / 0.1440D4 - (t18 * t8 * (t9 - t255 * t19) - 0.90D2 * t22
     # * t7 * (-t261 * t19 / 0.6D1 - t255 * t23 + t260 * t9 / 0.2D1 + t7
     #1) + t82 + 0.180D3 * t3 * t8 * (t23 - t255 * t9 + t260 * t19 / 0.2
     #D1)) * t249 / 0.1440D4 + (-0.90D2 * t22 * t7 * (-(t9 - t286 * t19)
     # * t52 + t292 * t19 - t298 * t19) - 0.180D3 * t304 * t147) * t100 
     #* t309 / 0.720D3 + (-0.90D2 * t22 * t7 * (-t316 * t19 / 0.2D1 - t3
     #23 * t9 + t325 * t19 / 0.2D1 + t315 * t9) + 0.180D3 * t3 * t8 * (-
     #t323 * t19 + t315 * t19)) * t151 * t249 / 0.720D3 - t361 * t59 / 0
     #.2880D4
      t365 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t364)
      t367 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t368 = t7 * t367
      t371 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t372 = t8 * t371
      t375 = t8 * t367
      t377 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t378 = t7 * t377
      t381 = 0.180D3 * t3 * t372 + t18 * t375 - 0.90D2 * t22 * t378
      t387 = rrqg2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t388 = t7 * t387
      t391 = t81 * t375
      t397 = t7 * t371
      t402 = -0.90D2 * t22 * t397 + 0.180D3 * t3 * t375
      t431 = t368 * t52
      t537 = rrqg2qgh73J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t541 = -t361 * t368 / 0.2880D4 - (t381 * t57 - 0.90D2 * t22 * t368
     # * t66 + (t18 * t372 - 0.90D2 * t22 * t388 + t391 + 0.180D3 * t3 *
     # t8 * t377) * t87 + t402 * t97) * t100 / 0.2880D4 + (-0.90D2 * t22
     # * t7 * (-t110 * t371 + t112 * t367 / 0.2D1 + t119 * t371 - t121 *
     # t367 / 0.2D1 - (t128 * t367 / 0.2D1 - t126 * t371 + t377) * t52) 
     #+ 0.180D3 * t3 * t8 * (-t110 * t367 + t119 * t367 - (t371 - t126 *
     # t367) * t52) - t146 * t431) * t100 * t151 / 0.1440D4 - (t402 * t1
     #64 - 0.90D2 * t22 * t368 * t169 + t381 * t173) * t151 / 0.1440D4 -
     # t190 * t397 / 0.2880D4 - t197 * t378 / 0.2880D4 - t203 * t388 / 0
     #.2880D4 + (-0.90D2 * t22 * t7 * (-t216 * t367 / 0.2D1 - t377 + t21
     #4 * t371 - (t226 * t367 / 0.2D1 - t224 * t371 + t377) * t52) + 0.1
     #80D3 * t3 * t8 * (t214 * t367 - t371 - (t371 - t224 * t367) * t52)
     # + t18 * t8 * (-t367 - t367 * t52)) * t100 * t249 / 0.1440D4 - (t1
     #8 * t8 * (t371 - t255 * t367) - 0.90D2 * t22 * t7 * (-t261 * t367 
     #/ 0.6D1 + t387 + t260 * t371 / 0.2D1 - t255 * t377) + t391 + 0.180
     #D3 * t3 * t8 * (t260 * t367 / 0.2D1 - t255 * t371 + t377)) * t249 
     #/ 0.1440D4 + (-0.90D2 * t22 * t7 * (t292 * t367 - (t371 - t286 * t
     #367) * t52 - t298 * t367) - 0.180D3 * t304 * t431) * t100 * t309 /
     # 0.720D3 + (-0.90D2 * t22 * t7 * (-t316 * t367 / 0.2D1 - t323 * t3
     #71 + t315 * t371 + t325 * t367 / 0.2D1) + 0.180D3 * t3 * t8 * (t31
     #5 * t367 - t323 * t367)) * t151 * t249 / 0.720D3 + t22 * t7 * t537
     # / 0.32D2
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
      t559 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t562 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t563 = t34 * t547
      t565 = t36 * t551
      t570 = log(0.4D1 * t211 * t563 * t565 * t552 * t39)
      t571 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t573 = t570 ** 2
      t577 = x3 * x1
      t578 = t577 * z
      t581 = x3 * t550
      t583 = Sqrt(t581 * t38)
      t587 = 0.1D1 / (0.2D1 * t578 - 0.2D1 * t577 - t45 + x3 + 0.2D1 * t
     #46 * t583 - 0.1D1)
      t604 = t8 * (t559 * t587 + t559)
      t610 = t252 * t547
      t611 = t565 * t552
      t614 = log(-0.4D1 * t610 * t611)
      t619 = rrqg2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t620 = t614 ** 2
      t623 = t620 * t614
      t631 = t8 * t559
      t643 = t104 * t252
      t646 = log(-0.4D1 * t643 * t554)
      t652 = log(0.4D1 * t643 * t548 * t553 * t39)
      t668 = log(-0.4D1 * t319 * t554)
      t669 = t668 ** 2
      t687 = (-0.90D2 * t22 * t7 * (t558 * t559 / 0.2D1 + (t562 - t570 *
     # t571 + t573 * t559 / 0.2D1) * t587 - t557 * t571 + t562) + 0.180D
     #3 * t3 * t8 * ((t571 - t570 * t559) * t587 + t571 - t557 * t559) +
     # t18 * t604) * t100 * t249 / 0.1440D4 - (t18 * t8 * (-t571 + t614 
     #* t559) - 0.90D2 * t22 * t7 * (-t619 - t620 * t571 / 0.2D1 + t623 
     #* t559 / 0.6D1 + t614 * t562) - t81 * t631 + 0.180D3 * t3 * t8 * (
     #-t562 + t614 * t571 - t620 * t559 / 0.2D1)) * t249 / 0.1440D4 + (-
     #0.90D2 * t22 * t7 * (t571 - t646 * t559 - (-t571 + t652 * t559) * 
     #t587) + 0.180D3 * t3 * t604) * t100 * t309 / 0.720D3 + (-0.90D2 * 
     #t22 * t7 * (t669 * t559 / 0.2D1 - t668 * t571 + t562) + 0.180D3 * 
     #t3 * t8 * (-t668 * t559 + t571) + t18 * t631) * t151 * t249 / 0.72
     #0D3
      t688 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t544, -t546, 0.0D0, t687)
      t690 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t693 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t695 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t715 = t8 * (t690 * t587 + t690)
      t727 = rrqg2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0.
     #0D0, -t546, 0.0D0)
      t735 = t8 * t690
      t778 = (-0.90D2 * t22 * t7 * (t558 * t690 / 0.2D1 - t557 * t693 + 
     #t695 + (t573 * t690 / 0.2D1 - t570 * t693 + t695) * t587) + 0.180D
     #3 * t3 * t8 * ((t693 - t570 * t690) * t587 + t693 - t557 * t690) +
     # t18 * t715) * t100 * t249 / 0.1440D4 - (t18 * t8 * (t614 * t690 -
     # t693) - 0.90D2 * t22 * t7 * (t623 * t690 / 0.6D1 - t727 - t620 * 
     #t693 / 0.2D1 + t614 * t695) - t81 * t735 + 0.180D3 * t3 * t8 * (-t
     #620 * t690 / 0.2D1 - t695 + t614 * t693)) * t249 / 0.1440D4 + (-0.
     #90D2 * t22 * t7 * (-(-t693 + t652 * t690) * t587 - t646 * t690 + t
     #693) + 0.180D3 * t3 * t715) * t100 * t309 / 0.720D3 + (-0.90D2 * t
     #22 * t7 * (-t668 * t693 + t669 * t690 / 0.2D1 + t695) + 0.180D3 * 
     #t3 * t8 * (t693 - t668 * t690) + t18 * t735) * t151 * t249 / 0.720
     #D3
      t779 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t546, t544, 0.0D0, t778)
      t781 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t782 = t8 * t781
      t785 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t786 = t8 * t785
      t788 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t789 = t7 * t788
      t792 = 0.180D3 * t3 * t782 + t18 * t786 - 0.90D2 * t22 * t789
      t794 = t7 * t785
      t799 = rrqg2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t800 = t7 * t799
      t803 = t81 * t786
      t809 = t7 * t781
      t814 = -0.90D2 * t22 * t809 + 0.180D3 * t3 * t786
      t843 = t794 * t52
      t857 = rrqg2qgh72J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t955 = -(t792 * t57 - 0.90D2 * t22 * t794 * t66 + (t18 * t782 - 0.
     #90D2 * t22 * t800 + t803 + 0.180D3 * t3 * t8 * t788) * t87 + t814 
     #* t97) * t100 / 0.2880D4 + (-0.90D2 * t22 * t7 * (-t110 * t781 + t
     #112 * t785 / 0.2D1 + t119 * t781 - t121 * t785 / 0.2D1 - (-t126 * 
     #t781 + t788 + t128 * t785 / 0.2D1) * t52) + 0.180D3 * t3 * t8 * (-
     #t110 * t785 + t119 * t785 - (t781 - t126 * t785) * t52) - t146 * t
     #843) * t100 * t151 / 0.1440D4 - (t814 * t164 - 0.90D2 * t22 * t794
     # * t169 + t792 * t173) * t151 / 0.1440D4 + t22 * t7 * t857 / 0.32D
     #2 - t361 * t794 / 0.2880D4 - t190 * t809 / 0.2880D4 - t197 * t789 
     #/ 0.2880D4 + (-0.90D2 * t22 * t7 * (t214 * t781 - t216 * t785 / 0.
     #2D1 - (-t224 * t781 + t788 + t226 * t785 / 0.2D1) * t52 - t788) + 
     #0.180D3 * t3 * t8 * (t214 * t785 - t781 - (t781 - t224 * t785) * t
     #52) + t18 * t8 * (-t785 * t52 - t785)) * t100 * t249 / 0.1440D4 - 
     #(t18 * t8 * (t781 - t255 * t785) - 0.90D2 * t22 * t7 * (t799 + t26
     #0 * t781 / 0.2D1 - t261 * t785 / 0.6D1 - t255 * t788) + t803 + 0.1
     #80D3 * t3 * t8 * (t260 * t785 / 0.2D1 - t255 * t781 + t788)) * t24
     #9 / 0.1440D4 + (-0.90D2 * t22 * t7 * (-t298 * t785 + t292 * t785 -
     # (t781 - t286 * t785) * t52) - 0.180D3 * t304 * t843) * t100 * t30
     #9 / 0.720D3 + (-0.90D2 * t22 * t7 * (t315 * t781 - t316 * t785 / 0
     #.2D1 - t323 * t781 + t325 * t785 / 0.2D1) + 0.180D3 * t3 * t8 * (t
     #315 * t785 - t323 * t785)) * t151 * t249 / 0.720D3 - t203 * t800 /
     # 0.2880D4
      t956 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t955)
      t958 = x2 * x3
      t959 = 0.1D1 - x3 + t958
      t960 = 0.1D1 / t959
      t961 = t958 * t960
      t962 = t2 * t961
      t964 = t2 * t38 * t960
      t965 = t106 * t38
      t966 = t959 ** 2
      t967 = 0.1D1 / t966
      t968 = t965 * t967
      t972 = log(0.4D1 * t643 * t220 * t968)
      t973 = t958 * z
      t975 = Sqrt(t44 * t965)
      t979 = 0.1D1 / (-t45 + t973 - 0.1D1 + x3 + 0.2D1 * t46 * t975)
      t980 = t972 * t979
      t981 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #962, -t964, 0.0D0)
      t983 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #962, -t964, 0.0D0)
      t984 = t979 * t983
      t989 = t7 * t979
      t990 = t989 * t981
      t1002 = log(0.4D1 * t104 * t115 * t294 * t38 * t967)
      t1003 = t1002 * t979
      t1005 = t1002 ** 2
      t1006 = t1005 * t979
      t1009 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1025 = (-0.90D2 * t22 * t7 * (-t980 * t981 + t984) + 0.180D3 * t3
     #04 * t990) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7 * (-t1003
     # * t983 + t1006 * t981 / 0.2D1 + t979 * t1009) + 0.180D3 * t3 * t8
     # * (-t1003 * t981 + t984) + t146 * t990) * t100 * t151 / 0.1440D4
      t1026 = FJET(XB1, XB2, s, 0.0D0, t962, 0.0D0, -t964, 0.0D0, t1025)
      t1028 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1030 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1033 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1040 = t979 * t1033
      t1045 = t989 * t1030
      t1062 = (-0.90D2 * t22 * t7 * (t979 * t1028 + t1006 * t1030 / 0.2D
     #1 - t1003 * t1033) + 0.180D3 * t3 * t8 * (-t1003 * t1030 + t1040) 
     #+ t146 * t1045) * t100 * t151 / 0.1440D4 + (-0.90D2 * t22 * t7 * (
     #t1040 - t980 * t1030) + 0.180D3 * t304 * t1045) * t100 * t309 / 0.
     #720D3
      t1063 = FJET(XB1, XB2, s, 0.0D0, -t964, 0.0D0, t962, 0.0D0, t1062)
      t1065 = x2 * x1
      t1067 = t2 * t1065 * t551
      t1070 = t106 * s * t1 * x1
      t1075 = s * t35 * x2 * x1 * t545 * t551
      t1076 = t1065 * z
      t1078 = 0.1D1 / (z + t1076 - t549 - t1065 + x1)
      t1079 = t550 * t1078
      t1080 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1086 = log(0.4D1 * t643 * t548 * t553 * t106)
      t1087 = t1086 * t550
      t1088 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1089 = t1078 * t1088
      t1095 = t7 * t550
      t1096 = t1095 * t1089
      t1107 = log(0.4D1 * t312 * t563 * t565 * t552 * t106)
      t1108 = t1107 * t1078
      t1110 = t1107 ** 2
      t1111 = t1110 * t1078
      t1114 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1133 = (-0.90D2 * t22 * t7 * (t1079 * t1080 - t1087 * t1089) + 0.
     #180D3 * t304 * t1096) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t
     #7 * (-t1108 * t1080 + t1111 * t1088 / 0.2D1 + t1078 * t1114) * t55
     #0 + 0.180D3 * t304 * t7 * (t1078 * t1080 - t1108 * t1088) * t550 +
     # t146 * t1096) * t151 * t249 / 0.720D3
      t1134 = FJET(XB1, XB2, s, 0.0D0, -t1067, -t546, -t1070, t1075, t11
     #33)
      t1136 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1139 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1140 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1176 = rrqg2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1181 = t8 * t1140
      t1182 = t81 * t1181
      t1202 = t7 * t1140
      t1203 = t1202 * t52
      t1230 = t8 * t1136
      t1234 = t7 * t1139
      t1237 = 0.180D3 * t3 * t1230 + t18 * t1181 - 0.90D2 * t22 * t1234
      t1243 = t7 * t1176
      t1251 = t7 * t1136
      t1256 = -0.90D2 * t22 * t1251 + 0.180D3 * t3 * t1181
      t1304 = rrqg2qgh74J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t1310 = (-0.90D2 * t22 * t7 * (t214 * t1136 - (-t224 * t1136 + t11
     #39 + t226 * t1140 / 0.2D1) * t52 - t1139 - t216 * t1140 / 0.2D1) +
     # 0.180D3 * t3 * t8 * (t214 * t1140 - (t1136 - t224 * t1140) * t52 
     #- t1136) + t18 * t8 * (-t1140 - t1140 * t52)) * t100 * t249 / 0.14
     #40D4 - (t18 * t8 * (t1136 - t255 * t1140) - 0.90D2 * t22 * t7 * (t
     #260 * t1136 / 0.2D1 - t261 * t1140 / 0.6D1 - t255 * t1139 + t1176)
     # + t1182 + 0.180D3 * t3 * t8 * (t260 * t1140 / 0.2D1 - t255 * t113
     #6 + t1139)) * t249 / 0.1440D4 + (-0.90D2 * t22 * t7 * (t292 * t114
     #0 - t298 * t1140 - (t1136 - t286 * t1140) * t52) - 0.180D3 * t304 
     #* t1203) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7 * (t315 * t
     #1136 - t316 * t1140 / 0.2D1 - t323 * t1136 + t325 * t1140 / 0.2D1)
     # + 0.180D3 * t3 * t8 * (t315 * t1140 - t323 * t1140)) * t151 * t24
     #9 / 0.720D3 - (t1237 * t57 - 0.90D2 * t22 * t1202 * t66 + (t18 * t
     #1230 - 0.90D2 * t22 * t1243 + t1182 + 0.180D3 * t3 * t8 * t1139) *
     # t87 + t1256 * t97) * t100 / 0.2880D4 + (-0.90D2 * t22 * t7 * (t11
     #2 * t1140 / 0.2D1 - t110 * t1136 + t119 * t1136 - t121 * t1140 / 0
     #.2D1 - (-t126 * t1136 + t1139 + t128 * t1140 / 0.2D1) * t52) + 0.1
     #80D3 * t3 * t8 * (-t110 * t1140 + t119 * t1140 - (t1136 - t126 * t
     #1140) * t52) - t146 * t1203) * t100 * t151 / 0.1440D4 - (t1256 * t
     #164 - 0.90D2 * t22 * t1202 * t169 + t1237 * t173) * t151 / 0.1440D
     #4 - t197 * t1234 / 0.2880D4 - t203 * t1243 / 0.2880D4 - t190 * t12
     #51 / 0.2880D4 + t22 * t7 * t1304 / 0.32D2 - t361 * t1202 / 0.2880D
     #4
      t1311 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1310)
      t1313 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1316 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1317 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1338 = t8 * (t1313 * t587 + t1313)
      t1349 = rrqg2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1358 = t8 * t1313
      t1401 = (-0.90D2 * t22 * t7 * (t558 * t1313 / 0.2D1 + t1316 + (-t5
     #70 * t1317 + t1316 + t573 * t1313 / 0.2D1) * t587 - t557 * t1317) 
     #+ 0.180D3 * t3 * t8 * ((t1317 - t570 * t1313) * t587 + t1317 - t55
     #7 * t1313) + t18 * t1338) * t100 * t249 / 0.1440D4 - (t18 * t8 * (
     #-t1317 + t614 * t1313) - 0.90D2 * t22 * t7 * (t614 * t1316 - t1349
     # + t623 * t1313 / 0.6D1 - t620 * t1317 / 0.2D1) - t81 * t1358 + 0.
     #180D3 * t3 * t8 * (t614 * t1317 - t1316 - t620 * t1313 / 0.2D1)) *
     # t249 / 0.1440D4 + (-0.90D2 * t22 * t7 * (-(-t1317 + t652 * t1313)
     # * t587 + t1317 - t646 * t1313) + 0.180D3 * t3 * t1338) * t100 * t
     #309 / 0.720D3 + (-0.90D2 * t22 * t7 * (-t668 * t1317 + t1316 + t66
     #9 * t1313 / 0.2D1) + 0.180D3 * t3 * t8 * (t1317 - t668 * t1313) + 
     #t18 * t1358) * t151 * t249 / 0.720D3
      t1402 = FJET(XB1, XB2, s, t544, -t546, 0.0D0, 0.0D0, 0.0D0, t1401)
      t1404 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1405 = t979 * t1404
      t1406 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1412 = t989 * t1406
      t1420 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1438 = (-0.90D2 * t22 * t7 * (t1405 - t980 * t1406) + 0.180D3 * t
     #304 * t1412) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7 * (-t10
     #03 * t1404 + t979 * t1420 + t1006 * t1406 / 0.2D1) + 0.180D3 * t3 
     #* t8 * (t1405 - t1003 * t1406) + t146 * t1412) * t100 * t151 / 0.1
     #440D4
      t1439 = FJET(XB1, XB2, s, t962, 0.0D0, -t964, 0.0D0, 0.0D0, t1438)
      t1444 = t38 * s * t1 * t545 * t960
      t1445 = x2 * z
      t1446 = t958 * x1
      t1447 = t958 * t549
      t1449 = Sqrt(-t581 * t965)
      t1450 = t46 * t1449
      t1453 = z + x1 - t549 - t1445 - t1065 + t1076 - t44 - t577 + t578 
     #+ t973 + t1446 - t1447 + t104 + 0.2D1 * t1450 * x2
      t1456 = t544 * t1453 * t551 * t960
      t1457 = t546 * t961
      t1463 = t544 * x2 * (-t44 - t577 + t578 + t973 + t1446 - t1447 - 0
     #.1D1 + x3 + 0.2D1 * t1450) * t551 * t960
      t1468 = log(-0.4D1 * t104 * t610 * t611 * t968)
      t1469 = t1468 * t550
      t1485 = x3 * t28
      t1493 = 0.4D1 * t1447 + t549 + t577 + 0.2D1 * t1450 * t1076 - 0.5D
     #1 * t578 + t44 - z - 0.2D1 * t1450 * t549 - 0.2D1 * t1450 * t1065 
     #+ t104 * t28 * x1 + 0.2D1 * t104 * t210 * z - t104 * t210 * t28 - 
     #0.3D1 * t1485 * t1065 - 0.4D1 * t211 * t1445 + 0.2D1 * t211 * t28 
     #* x2
      t1510 = -t104 * t549 - t1446 + 0.2D1 * t211 * x2 + 0.4D1 * t1485 *
     # x1 + 0.4D1 * t211 * z - 0.2D1 * t211 * t28 + t958 * t28 - t281 + 
     #0.2D1 * t1450 * x1 + 0.2D1 * t1450 * z - 0.2D1 * t211 - 0.2D1 * t1
     #485 - x1 + t1065 - t1076
      t1512 = 0.1D1 / (t1493 + t1510)
      t1513 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1514 = t1512 * t1513
      t1516 = t550 * t1512
      t1517 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1526 = -0.90D2 * t22 * t7 * (-t1469 * t1514 + t1516 * t1517) + 0.
     #180D3 * t304 * t1095 * t1514
      t1530 = FJET(XB1, XB2, s, t1444, -t1456, -t1457, t1463, t1075, t15
     #26 * t100 * t309 / 0.720D3)
      t1533 = t100 * t151 * t249
      t1536 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1538 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1539 = t1512 * t1538
      t1548 = -0.90D2 * t22 * t7 * (t1516 * t1536 - t1469 * t1539) + 0.1
     #80D3 * t304 * t1095 * t1539
      t1552 = FJET(XB1, XB2, s, t1463, -t1457, -t1456, t1444, t1075, t15
     #48 * t100 * t309 / 0.720D3)
      t1556 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1558 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1559 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1581 = t8 * (t1559 * t587 + t1559)
      t1593 = rrqg2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t544, 0
     #.0D0, -t546, 0.0D0)
      t1601 = t8 * t1559
      t1644 = (-0.90D2 * t22 * t7 * ((-t570 * t1556 + t1558 + t573 * t15
     #59 / 0.2D1) * t587 + t1558 + t558 * t1559 / 0.2D1 - t557 * t1556) 
     #+ 0.180D3 * t3 * t8 * (t1556 + (t1556 - t570 * t1559) * t587 - t55
     #7 * t1559) + t18 * t1581) * t100 * t249 / 0.1440D4 - (t18 * t8 * (
     #-t1556 + t614 * t1559) - 0.90D2 * t22 * t7 * (t623 * t1559 / 0.6D1
     # - t1593 + t614 * t1558 - t620 * t1556 / 0.2D1) - t81 * t1601 + 0.
     #180D3 * t3 * t8 * (t614 * t1556 - t1558 - t620 * t1559 / 0.2D1)) *
     # t249 / 0.1440D4 + (-0.90D2 * t22 * t7 * (t1556 - (-t1556 + t652 *
     # t1559) * t587 - t646 * t1559) + 0.180D3 * t3 * t1581) * t100 * t3
     #09 / 0.720D3 + (-0.90D2 * t22 * t7 * (t669 * t1559 / 0.2D1 - t668 
     #* t1556 + t1558) + 0.180D3 * t3 * t8 * (t1556 - t668 * t1559) + t1
     #8 * t1601) * t151 * t249 / 0.720D3
      t1645 = FJET(XB1, XB2, s, -t546, t544, 0.0D0, 0.0D0, 0.0D0, t1644)
      t1647 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1649 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1650 = t1078 * t1649
      t1656 = t1095 * t1650
      t1665 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1684 = (-0.90D2 * t22 * t7 * (t1079 * t1647 - t1087 * t1650) + 0.
     #180D3 * t304 * t1656) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t
     #7 * (-t1108 * t1647 + t1111 * t1649 / 0.2D1 + t1078 * t1665) * t55
     #0 + 0.180D3 * t304 * t7 * (t1078 * t1647 - t1108 * t1649) * t550 +
     # t146 * t1656) * t151 * t249 / 0.720D3
      t1685 = FJET(XB1, XB2, s, -t546, -t1070, 0.0D0, -t1067, t1075, t16
     #84)
      t1687 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1688 = t979 * t1687
      t1689 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1695 = t989 * t1689
      t1703 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t962, -t964, 0.0D0)
      t1721 = (-0.90D2 * t22 * t7 * (t1688 - t980 * t1689) + 0.180D3 * t
     #304 * t1695) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t7 * (-t10
     #03 * t1687 + t979 * t1703 + t1006 * t1689 / 0.2D1) + 0.180D3 * t3 
     #* t8 * (t1688 - t1003 * t1689) + t146 * t1695) * t100 * t151 / 0.1
     #440D4
      t1722 = FJET(XB1, XB2, s, -t964, 0.0D0, t962, 0.0D0, 0.0D0, t1721)
      t1724 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1725 = t1078 * t1724
      t1727 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1733 = t1095 * t1725
      t1742 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1761 = (-0.90D2 * t22 * t7 * (-t1087 * t1725 + t1079 * t1727) + 0
     #.180D3 * t304 * t1733) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * 
     #t7 * (-t1108 * t1727 + t1111 * t1724 / 0.2D1 + t1078 * t1742) * t5
     #50 + 0.180D3 * t304 * t7 * (t1078 * t1727 - t1108 * t1724) * t550 
     #+ t146 * t1733) * t151 * t249 / 0.720D3
      t1762 = FJET(XB1, XB2, s, -t1070, -t546, -t1067, 0.0D0, t1075, t17
     #61)
      t1764 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1766 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1767 = t1078 * t1766
      t1773 = t1095 * t1767
      t1782 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, -t1067, -t1070
     #, 0.0D0, -t546, t1075)
      t1801 = (-0.90D2 * t22 * t7 * (t1079 * t1764 - t1087 * t1767) + 0.
     #180D3 * t304 * t1773) * t100 * t309 / 0.720D3 + (-0.90D2 * t22 * t
     #7 * (-t1108 * t1764 + t1111 * t1766 / 0.2D1 + t1078 * t1782) * t55
     #0 + 0.180D3 * t304 * t7 * (t1078 * t1764 - t1108 * t1766) * t550 +
     # t146 * t1773) * t151 * t249 / 0.720D3
      t1802 = FJET(XB1, XB2, s, -t1067, 0.0D0, -t1070, -t546, t1075, t18
     #01)
      t1804 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1806 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1807 = t1512 * t1806
      t1816 = -0.90D2 * t22 * t7 * (t1516 * t1804 - t1469 * t1807) + 0.1
     #80D3 * t304 * t1095 * t1807
      t1820 = FJET(XB1, XB2, s, -t1456, t1444, t1463, -t1457, t1075, t18
     #16 * t100 * t309 / 0.720D3)
      t1824 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1826 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t1463, -t1456,
     # -t1457, t1444, t1075)
      t1827 = t1512 * t1826
      t1836 = -0.90D2 * t22 * t7 * (t1516 * t1824 - t1469 * t1827) + 0.1
     #80D3 * t304 * t1095 * t1827
      t1840 = FJET(XB1, XB2, s, -t1457, t1463, t1444, -t1456, t1075, t18
     #36 * t100 * t309 / 0.720D3)
      rrqg2qght7s2e1 = t365 * t364 + t542 * t541 + t688 * t687 + t779 * 
     #t778 + t956 * t955 + t1026 * t1025 + t1063 * t1062 + t1134 * t1133
     # + t1311 * t1310 + t1402 * t1401 + t1439 * t1438 + t1530 * t1526 *
     # t1533 / 0.720D3 + t1552 * t1548 * t1533 / 0.720D3 + t1645 * t1644
     # + t1685 * t1684 + t1722 * t1721 + t1762 * t1761 + t1802 * t1801 +
     # t1820 * t1816 * t1533 / 0.720D3 + t1840 * t1836 * t1533 / 0.720D3

      end function



      doubleprecision function rrqg2qght7s2e0
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t8 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
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
      t24 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
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
      t44 = 0.1D1 / (-t37 - 0.1D1 + x3 + 0.2D1 * t38 * t40)
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
      t90 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t91 = t9 * t16
      t94 = log(0.4D1 * t91 * t27)
      t96 = t94 ** 2
      t108 = lh ** 2
      t110 = pi ** 2
      t112 = -0.180D3 * t108 + 0.30D2 * t110
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
      t141 = -0.90D2 * t4 * t136 + 0.180D3 * t50 * t114
      t143 = -t124 * t44 - t129
      t148 = t7 * t90
      t152 = t44 + 0.1D1
      t158 = log(0.4D1 * t20)
      t159 = t158 * pi
      t162 = t158 ** 2
      t163 = t162 * pi
      t166 = (-0.180D3 * t159 * lh - 0.45D2 * t163 + t113) * t3
      t169 = rrqg2qgh71J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t187 = (0.90D2 * t163 * lh + pi * (-0.60D2 * lh * t110 + 0.240D3 *
     # zeta3 + 0.120D3 * t108 * lh) + 0.15D2 * t162 * t158 * pi - t159 *
     # t112) * t3
      t193 = (0.180D3 * t50 + 0.90D2 * t159) * t3
      t196 = x3 * t72
      t197 = t196 * t13
      t198 = t120 * t75
      t201 = log(-0.4D1 * t197 * t198)
      t205 = log(0.4D1 * t196 * t20)
      t209 = log(-0.4D1 * t197 * t121)
      t217 = t50 * t3
      t225 = t13 * t72
      t228 = log(0.4D1 * t225 * t120)
      t229 = t228 ** 2
      t232 = log(-0.4D1 * t225 * t198)
      t233 = t232 ** 2
      t235 = t229 / 0.2D1 - t233 / 0.2D1
      t239 = -t228 + t232
      t244 = (-0.90D2 * t4 * t7 * (-t8 + t23 * t24 - (t8 - t33 * t24) * 
     #t44) + 0.180D3 * t50 * t51 * (-t24 - t24 * t44)) * t58 * t60 / 0.1
     #440D4 + t4 * t63 * t68 / 0.8D1 - t71 * (-t79 * t24 + t83 * t24) * 
     #t66 * t60 / 0.8D1 - (-0.90D2 * t4 * t7 * (t90 - t94 * t8 + t96 * t
     #24 / 0.2D1) + 0.180D3 * t50 * t51 * (t8 - t94 * t24) + t115) * t60
     # / 0.1440D4 - (-0.90D2 * t4 * t63 * t132 + t141 * t143 + (0.180D3 
     #* t50 * t51 * t8 + t115 - 0.90D2 * t4 * t148) * t152) * t58 / 0.28
     #80D4 - t166 * t136 / 0.2880D4 + t4 * t7 * t169 / 0.32D2 - t187 * t
     #63 / 0.2880D4 - t193 * t148 / 0.2880D4 + (-0.90D2 * t4 * t7 * (-t2
     #01 * t24 + t205 * t24 - (t8 - t209 * t24) * t44) - 0.180D3 * t217 
     #* t63 * t44) * t58 * t66 / 0.1440D4 - (-0.90D2 * t4 * t63 * t235 +
     # t141 * t239) * t66 / 0.1440D4
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t244)
      t247 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t249 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t266 = t7 * t247
      t280 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t290 = t51 * t247
      t291 = t113 * t290
      t298 = t7 * t249
      t303 = -0.90D2 * t4 * t298 + 0.180D3 * t50 * t290
      t308 = t7 * t280
      t318 = rrqg2qgh73J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t349 = (-0.90D2 * t4 * t7 * (t23 * t247 - t249 - (t249 - t33 * t24
     #7) * t44) + 0.180D3 * t50 * t51 * (-t247 - t247 * t44)) * t58 * t6
     #0 / 0.1440D4 + t4 * t266 * t68 / 0.8D1 - t71 * (t83 * t247 - t79 *
     # t247) * t66 * t60 / 0.8D1 - (-0.90D2 * t4 * t7 * (t96 * t247 / 0.
     #2D1 - t94 * t249 + t280) + 0.180D3 * t50 * t51 * (t249 - t94 * t24
     #7) + t291) * t60 / 0.1440D4 - (-0.90D2 * t4 * t266 * t132 + t303 *
     # t143 + (0.180D3 * t50 * t51 * t249 + t291 - 0.90D2 * t4 * t308) *
     # t152) * t58 / 0.2880D4 - t166 * t298 / 0.2880D4 + t4 * t7 * t318 
     #/ 0.32D2 - t187 * t266 / 0.2880D4 - t193 * t308 / 0.2880D4 + (-0.9
     #0D2 * t4 * t7 * (-t201 * t247 + t205 * t247 - (t249 - t209 * t247)
     # * t44) - 0.180D3 * t217 * t266 * t44) * t58 * t66 / 0.1440D4 - (-
     #0.90D2 * t4 * t266 * t235 + t303 * t239) * t66 / 0.1440D4
      t350 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t349)
      t352 = t2 * x1
      t353 = -0.1D1 + x1
      t354 = t2 * t353
      t355 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t356 = 0.1D1 / t11
      t357 = t16 * t356
      t359 = x1 * z
      t360 = -z - x1 + t359
      t361 = 0.1D1 / t360
      t362 = t19 * t361
      t363 = t353 ** 2
      t368 = log(0.4D1 * t10 * t357 * t362 * t363 * t29)
      t369 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t372 = x3 * x1
      t373 = t372 * z
      t376 = x3 * t360
      t378 = Sqrt(t376 * t28)
      t382 = 0.1D1 / (0.2D1 * t373 - 0.2D1 * t372 - t37 + x3 + 0.2D1 * t
     #38 * t378 - 0.1D1)
      t386 = t356 * t19 * t361 * t363
      t389 = log(-0.4D1 * t26 * t386)
      t396 = t369 * t382 + t369
      t410 = log(-0.4D1 * t74 * t386)
      t416 = t51 * t369
      t423 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t428 = log(-0.4D1 * t91 * t356 * t362 * t363)
      t430 = t428 ** 2
      t446 = (-0.90D2 * t4 * t7 * ((t355 - t368 * t369) * t382 + t355 - 
     #t389 * t369) + 0.180D3 * t50 * t51 * t396) * t58 * t60 / 0.1440D4 
     #- t71 * t396 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t410 * t
     #369 + t355) + 0.180D3 * t50 * t416) * t66 * t60 / 0.720D3 - (-0.90
     #D2 * t4 * t7 * (-t423 + t428 * t355 - t430 * t369 / 0.2D1) + 0.180
     #D3 * t50 * t51 * (-t355 + t428 * t369) - t113 * t416) * t60 / 0.14
     #40D4
      t447 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t352, -t354, 0.0D0, t446)
      t449 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t450 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t460 = t450 * t382 + t450
      t477 = t51 * t450
      t486 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t501 = (-0.90D2 * t4 * t7 * ((t449 - t368 * t450) * t382 + t449 - 
     #t389 * t450) + 0.180D3 * t50 * t51 * t460) * t58 * t60 / 0.1440D4 
     #- t71 * t460 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (t449 - t4
     #10 * t450) + 0.180D3 * t50 * t477) * t66 * t60 / 0.720D3 - (-0.90D
     #2 * t4 * t7 * (-t430 * t450 / 0.2D1 - t486 + t428 * t449) + 0.180D
     #3 * t50 * t51 * (t428 * t450 - t449) - t113 * t477) * t60 / 0.1440
     #D4
      t502 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t354, t352, 0.0D0, t501)
      t504 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t506 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t523 = t7 * t504
      t537 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t547 = t51 * t504
      t548 = t113 * t547
      t552 = rrqg2qgh72J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t561 = t7 * t506
      t566 = -0.90D2 * t4 * t561 + 0.180D3 * t50 * t547
      t571 = t7 * t537
      t606 = (-0.90D2 * t4 * t7 * (t23 * t504 - t506 - (t506 - t33 * t50
     #4) * t44) + 0.180D3 * t50 * t51 * (-t504 * t44 - t504)) * t58 * t6
     #0 / 0.1440D4 + t4 * t523 * t68 / 0.8D1 - t71 * (t83 * t504 - t79 *
     # t504) * t66 * t60 / 0.8D1 - (-0.90D2 * t4 * t7 * (t96 * t504 / 0.
     #2D1 - t94 * t506 + t537) + 0.180D3 * t50 * t51 * (t506 - t94 * t50
     #4) + t548) * t60 / 0.1440D4 + t4 * t7 * t552 / 0.32D2 - t187 * t52
     #3 / 0.2880D4 - (-0.90D2 * t4 * t523 * t132 + t566 * t143 + (0.180D
     #3 * t50 * t51 * t506 + t548 - 0.90D2 * t4 * t571) * t152) * t58 / 
     #0.2880D4 - t166 * t561 / 0.2880D4 - t193 * t571 / 0.2880D4 + (-0.9
     #0D2 * t4 * t7 * (-t201 * t504 + t205 * t504 - (t506 - t209 * t504)
     # * t44) - 0.180D3 * t217 * t523 * t44) * t58 * t66 / 0.1440D4 - (-
     #0.90D2 * t4 * t523 * t235 + t566 * t239) * t66 / 0.1440D4
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
      t623 = 0.1D1 / (-t37 + t616 - 0.1D1 + x3 + 0.2D1 * t38 * t619)
      t624 = t7 * t623
      t625 = t4 * t624
      t626 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t633 = t610 ** 2
      t639 = log(0.4D1 * t196 * t17 * t19 * t75 * t28 / t633)
      t640 = t639 * t623
      t642 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t655 = -t625 * t626 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t
     #640 * t626 + t623 * t642) + 0.180D3 * t217 * t624 * t626) * t58 * 
     #t66 / 0.1440D4
      t656 = FJET(XB1, XB2, s, 0.0D0, t613, 0.0D0, -t615, 0.0D0, t655)
      t658 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t664 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t677 = -t625 * t658 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t
     #640 * t658 + t623 * t664) + 0.180D3 * t217 * t624 * t658) * t58 * 
     #t66 / 0.1440D4
      t678 = FJET(XB1, XB2, s, 0.0D0, -t615, 0.0D0, t613, 0.0D0, t677)
      t680 = x2 * x1
      t682 = t2 * t680 * t361
      t685 = t75 * s * t1 * x1
      t690 = s * t18 * x2 * x1 * t353 * t361
      t691 = t7 * t360
      t692 = t4 * t691
      t693 = t680 * z
      t695 = 0.1D1 / (z + t693 - t359 - t680 + x1)
      t696 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 0
     #.0D0, -t354, t690)
      t697 = t695 * t696
      t699 = t58 * t66 * t60
      t703 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 0
     #.0D0, -t354, t690)
      t710 = log(0.4D1 * t73 * t357 * t362 * t363 * t75)
      t711 = t710 * t695
      t725 = -t692 * t697 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t695 * 
     #t703 - t711 * t696) * t360 + 0.180D3 * t217 * t691 * t697) * t66 *
     # t60 / 0.720D3
      t726 = FJET(XB1, XB2, s, 0.0D0, -t682, -t354, -t685, t690, t725)
      t728 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t730 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t747 = t7 * t728
      t761 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t771 = t51 * t728
      t772 = t113 * t771
      t778 = t7 * t730
      t781 = rrqg2qgh74J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t785 = t7 * t761
      t795 = -0.90D2 * t4 * t778 + 0.180D3 * t50 * t771
      t830 = (-0.90D2 * t4 * t7 * (t23 * t728 - (t730 - t33 * t728) * t4
     #4 - t730) + 0.180D3 * t50 * t51 * (-t728 - t728 * t44)) * t58 * t6
     #0 / 0.1440D4 + t4 * t747 * t68 / 0.8D1 - t71 * (t83 * t728 - t79 *
     # t728) * t66 * t60 / 0.8D1 - (-0.90D2 * t4 * t7 * (t96 * t728 / 0.
     #2D1 - t94 * t730 + t761) + 0.180D3 * t50 * t51 * (t730 - t94 * t72
     #8) + t772) * t60 / 0.1440D4 - t187 * t747 / 0.2880D4 - t166 * t778
     # / 0.2880D4 + t4 * t7 * t781 / 0.32D2 - t193 * t785 / 0.2880D4 - (
     #-0.90D2 * t4 * t747 * t132 + t795 * t143 + (0.180D3 * t50 * t51 * 
     #t730 + t772 - 0.90D2 * t4 * t785) * t152) * t58 / 0.2880D4 + (-0.9
     #0D2 * t4 * t7 * (-t201 * t728 + t205 * t728 - (t730 - t209 * t728)
     # * t44) - 0.180D3 * t217 * t747 * t44) * t58 * t66 / 0.1440D4 - (-
     #0.90D2 * t4 * t747 * t235 + t795 * t239) * t66 / 0.1440D4
      t831 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t830)
      t833 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t834 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t844 = t834 * t382 + t834
      t861 = t51 * t834
      t869 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t885 = (-0.90D2 * t4 * t7 * ((t833 - t368 * t834) * t382 + t833 - 
     #t389 * t834) + 0.180D3 * t50 * t51 * t844) * t58 * t60 / 0.1440D4 
     #- t71 * t844 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (t833 - t4
     #10 * t834) + 0.180D3 * t50 * t861) * t66 * t60 / 0.720D3 - (-0.90D
     #2 * t4 * t7 * (t428 * t833 - t869 - t430 * t834 / 0.2D1) + 0.180D3
     # * t50 * t51 * (-t833 + t428 * t834) - t113 * t861) * t60 / 0.1440
     #D4
      t886 = FJET(XB1, XB2, s, t352, -t354, 0.0D0, 0.0D0, 0.0D0, t885)
      t888 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t893 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #613, -t615, 0.0D0)
      t907 = -t625 * t888 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (t6
     #23 * t893 - t640 * t888) + 0.180D3 * t217 * t624 * t888) * t58 * t
     #66 / 0.1440D4
      t908 = FJET(XB1, XB2, s, t613, 0.0D0, -t615, 0.0D0, 0.0D0, t907)
      t913 = t28 * s * t1 * t353 * t611
      t914 = x2 * z
      t915 = t609 * x1
      t916 = t609 * t359
      t918 = Sqrt(-t376 * t617)
      t919 = t38 * t918
      t922 = z + x1 - t359 - t914 - t680 + t693 - t36 - t372 + t373 + t6
     #16 + t915 - t916 + t196 + 0.2D1 * t919 * x2
      t925 = t352 * t922 * t361 * t611
      t926 = t354 * t612
      t932 = t352 * x2 * (-t36 - t372 + t373 + t616 + t915 - t916 - 0.1D
     #1 + x3 + 0.2D1 * t919) * t361 * t611
      t934 = x3 * t11
      t952 = -x1 - 0.2D1 * t10 - 0.2D1 * t934 - z + 0.2D1 * t919 * t693 
     #- t915 + 0.2D1 * t10 * x2 + 0.4D1 * t934 * x1 + 0.4D1 * t10 * z - 
     #0.2D1 * t10 * t11 + t609 * t11 - t196 * t9 + 0.2D1 * t919 * x1 + 0
     #.2D1 * t919 * z + t359
      t974 = t372 + 0.4D1 * t916 - 0.2D1 * t919 * t359 - 0.2D1 * t919 * 
     #t680 + t196 * t11 * x1 + 0.2D1 * t196 * t9 * z - t196 * t9 * t11 -
     # 0.3D1 * t934 * t680 - 0.4D1 * t10 * t914 + 0.2D1 * t10 * t11 * x2
     # - t196 * t359 + t36 - 0.5D1 * t373 + t680 - t693
      t976 = 0.1D1 / (t952 + t974)
      t977 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -t
     #926, t913, t690)
      t979 = t976 * t977 * t699
      t982 = FJET(XB1, XB2, s, t913, -t925, -t926, t932, t690, -t692 * t
     #979 / 0.8D1)
      t984 = t51 * t360
      t988 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -t
     #926, t913, t690)
      t990 = t976 * t988 * t699
      t993 = FJET(XB1, XB2, s, t932, -t926, -t925, t913, t690, -t692 * t
     #990 / 0.8D1)
      t998 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t999 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0.
     #0D0, -t354, 0.0D0)
      t1009 = t999 * t382 + t999
      t1026 = t51 * t999
      t1034 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t352, 0
     #.0D0, -t354, 0.0D0)
      t1050 = (-0.90D2 * t4 * t7 * (t998 + (t998 - t368 * t999) * t382 -
     # t389 * t999) + 0.180D3 * t50 * t51 * t1009) * t58 * t60 / 0.1440D
     #4 - t71 * t1009 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (t998 -
     # t410 * t999) + 0.180D3 * t50 * t1026) * t66 * t60 / 0.720D3 - (-0
     #.90D2 * t4 * t7 * (t428 * t998 - t1034 - t430 * t999 / 0.2D1) + 0.
     #180D3 * t50 * t51 * (-t998 + t428 * t999) - t113 * t1026) * t60 / 
     #0.1440D4
      t1051 = FJET(XB1, XB2, s, -t354, t352, 0.0D0, 0.0D0, 0.0D0, t1050)
      t1053 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1054 = t695 * t1053
      t1058 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1073 = -t692 * t1054 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t695 
     #* t1058 - t711 * t1053) * t360 + 0.180D3 * t217 * t691 * t1054) * 
     #t66 * t60 / 0.720D3
      t1074 = FJET(XB1, XB2, s, -t354, -t685, 0.0D0, -t682, t690, t1073)
      t1076 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t613, -t615, 0.0D0)
      t1081 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t613, -t615, 0.0D0)
      t1095 = -t625 * t1076 * t58 * t67 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #t623 * t1081 - t640 * t1076) + 0.180D3 * t217 * t624 * t1076) * t5
     #8 * t66 / 0.1440D4
      t1096 = FJET(XB1, XB2, s, -t615, 0.0D0, t613, 0.0D0, 0.0D0, t1095)
      t1098 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1099 = t695 * t1098
      t1103 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1118 = -t692 * t1099 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t695 
     #* t1103 - t711 * t1098) * t360 + 0.180D3 * t217 * t691 * t1099) * 
     #t66 * t60 / 0.720D3
      t1119 = FJET(XB1, XB2, s, -t685, -t354, -t682, 0.0D0, t690, t1118)
      t1121 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1122 = t695 * t1121
      t1126 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, -t682, -t685, 
     #0.0D0, -t354, t690)
      t1141 = -t692 * t1122 * t699 / 0.8D1 + (-0.90D2 * t4 * t7 * (t695 
     #* t1126 - t711 * t1121) * t360 + 0.180D3 * t217 * t691 * t1122) * 
     #t66 * t60 / 0.720D3
      t1142 = FJET(XB1, XB2, s, -t682, 0.0D0, -t685, -t354, t690, t1141)
      t1144 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -
     #t926, t913, t690)
      t1146 = t976 * t1144 * t699
      t1149 = FJET(XB1, XB2, s, -t925, t913, t932, -t926, t690, -t692 * 
     #t1146 / 0.8D1)
      t1154 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, t932, -t925, -
     #t926, t913, t690)
      t1156 = t976 * t1154 * t699
      t1159 = FJET(XB1, XB2, s, -t926, t932, t913, -t925, t690, -t692 * 
     #t1156 / 0.8D1)
      rrqg2qght7s2e0 = t245 * t244 + t350 * t349 + t447 * t446 + t502 * 
     #t501 + t607 * t606 + t656 * t655 + t678 * t677 + t726 * t725 + t83
     #1 * t830 + t886 * t885 + t908 * t907 - t982 * pi * t984 * t979 / 0
     #.8D1 - t993 * pi * t984 * t990 / 0.8D1 + t1051 * t1050 + t1074 * t
     #1073 + t1096 * t1095 + t1119 * t1118 + t1142 * t1141 - t1149 * pi 
     #* t984 * t1146 / 0.8D1 - t1159 * pi * t984 * t1156 / 0.8D1

      end function



      doubleprecision function rrqg2qght7s2em1
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t8 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
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
      t34 = 0.1D1 / (-t27 - 0.1D1 + x3 + 0.2D1 * t28 * t30)
      t38 = log(0.4D1 * t13 * t19)
      t39 = -t25 * t34 - t38
      t43 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
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
      t109 = (0.180D3 * t47 + 0.90D2 * t106) * t3
      t114 = t105 ** 2
      t117 = lh ** 2
      t119 = pi ** 2
      t124 = (-0.180D3 * t106 * lh - 0.45D2 * t114 * pi + pi * (-0.180D3
     # * t117 + 0.30D2 * t119)) * t3
      t127 = rrqg2qgh71J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t131 = -(-0.90D2 * t4 * t9 * t39 + (-0.90D2 * t4 * t44 + t51) * t5
     #3) * t56 / 0.2880D4 - (-0.90D2 * t4 * t7 * (t43 - t64 * t8) + t51)
     # * t71 / 0.1440D4 - t74 * (-t8 - t75) * t56 * t71 / 0.16D2 + t74 *
     # t75 * t82 / 0.16D2 + t74 * t8 * t96 * t81 / 0.16D2 - t109 * t44 /
     # 0.2880D4 - t124 * t9 / 0.2880D4 + t4 * t7 * t127 / 0.32D2
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t131)
      t134 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t135 = t7 * t134
      t139 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t140 = t7 * t139
      t145 = 0.180D3 * t47 * t48 * t134
      t159 = t134 * t34
      t176 = rrqg2qgh73J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t180 = -(-0.90D2 * t4 * t135 * t39 + (-0.90D2 * t4 * t140 + t145) 
     #* t53) * t56 / 0.2880D4 - (-0.90D2 * t4 * t7 * (t139 - t64 * t134)
     # + t145) * t71 / 0.1440D4 - t74 * (-t134 - t159) * t56 * t71 / 0.1
     #6D2 + t74 * t159 * t82 / 0.16D2 + t74 * t134 * t96 * t81 / 0.16D2 
     #- t109 * t140 / 0.2880D4 - t124 * t135 / 0.2880D4 + t4 * t7 * t176
     # / 0.32D2
      t181 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t180)
      t183 = t2 * x1
      t184 = -0.1D1 + x1
      t185 = t2 * t184
      t186 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t191 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t194 = x1 * z
      t195 = -z - x1 + t194
      t196 = 0.1D1 / t195
      t198 = t184 ** 2
      t202 = log(-0.4D1 * t60 / t10 * t18 * t196 * t198)
      t214 = x3 * x1
      t220 = Sqrt(x3 * t195 * t20)
      t224 = 0.1D1 / (0.2D1 * t214 * z - 0.2D1 * t214 - t27 + x3 + 0.2D1
     # * t28 * t220 - 0.1D1)
      t231 = -t74 * t186 * t81 * t71 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t1
     #91 + t202 * t186) - 0.180D3 * t47 * t48 * t186) * t71 / 0.1440D4 -
     # t74 * (t186 * t224 + t186) * t56 * t71 / 0.16D2
      t232 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t183, -t185, 0.0D0, t231)
      t234 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t240 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t257 = -t74 * t234 * t81 * t71 / 0.8D1 - (-0.90D2 * t4 * t7 * (t20
     #2 * t234 - t240) - 0.180D3 * t47 * t48 * t234) * t71 / 0.1440D4 - 
     #t74 * (t234 * t224 + t234) * t56 * t71 / 0.16D2
      t258 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t185, t183, 0.0D0, t257)
      t260 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t261 = t7 * t260
      t265 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t266 = t7 * t265
      t271 = 0.180D3 * t47 * t48 * t260
      t285 = t260 * t34
      t293 = rrqg2qgh72J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t306 = -(-0.90D2 * t4 * t261 * t39 + (-0.90D2 * t4 * t266 + t271) 
     #* t53) * t56 / 0.2880D4 - (-0.90D2 * t4 * t7 * (t265 - t64 * t260)
     # + t271) * t71 / 0.1440D4 - t74 * (-t285 - t260) * t56 * t71 / 0.1
     #6D2 - t124 * t261 / 0.2880D4 + t4 * t7 * t293 / 0.32D2 + t74 * t28
     #5 * t82 / 0.16D2 + t74 * t260 * t96 * t81 / 0.16D2 - t109 * t266 /
     # 0.2880D4
      t307 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t306)
      t309 = x2 * x3
      t311 = 0.1D1 / (0.1D1 - x3 + t309)
      t313 = t2 * t309 * t311
      t315 = t2 * t20 * t311
      t319 = Sqrt(t26 * t91 * t20)
      t323 = 0.1D1 / (-t27 + t309 * z - 0.1D1 + x3 + 0.2D1 * t28 * t319)
      t324 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t326 = t323 * t324 * t82
      t329 = FJET(XB1, XB2, s, 0.0D0, t313, 0.0D0, -t315, 0.0D0, -t74 * 
     #t326 / 0.16D2)
      t334 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t336 = t323 * t334 * t82
      t339 = FJET(XB1, XB2, s, 0.0D0, -t315, 0.0D0, t313, 0.0D0, -t74 * 
     #t336 / 0.16D2)
      t344 = x2 * x1
      t346 = t2 * t344 * t196
      t349 = t91 * s * t1 * x1
      t354 = s * t17 * x2 * x1 * t184 * t196
      t356 = t4 * t7 * t195
      t359 = 0.1D1 / (z + t344 * z - t194 - t344 + x1)
      t360 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t362 = t81 * t71
      t366 = FJET(XB1, XB2, s, 0.0D0, -t346, -t185, -t349, t354, -t356 *
     # t359 * t360 * t362 / 0.8D1)
      t369 = t195 * t359
      t375 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t376 = t7 * t375
      t380 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t381 = t7 * t380
      t386 = 0.180D3 * t47 * t48 * t375
      t396 = rrqg2qgh74J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t408 = t375 * t34
      t421 = -(-0.90D2 * t4 * t376 * t39 + (-0.90D2 * t4 * t381 + t386) 
     #* t53) * t56 / 0.2880D4 - t109 * t381 / 0.2880D4 - t124 * t376 / 0
     #.2880D4 + t4 * t7 * t396 / 0.32D2 - (-0.90D2 * t4 * t7 * (t380 - t
     #64 * t375) + t386) * t71 / 0.1440D4 - t74 * (-t375 - t408) * t56 *
     # t71 / 0.16D2 + t74 * t408 * t82 / 0.16D2 + t74 * t375 * t96 * t81
     # / 0.16D2
      t422 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t421)
      t424 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t429 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t447 = -t74 * t424 * t81 * t71 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t4
     #29 + t202 * t424) - 0.180D3 * t47 * t48 * t424) * t71 / 0.1440D4 -
     # t74 * (t424 * t224 + t424) * t56 * t71 / 0.16D2
      t448 = FJET(XB1, XB2, s, t183, -t185, 0.0D0, 0.0D0, 0.0D0, t447)
      t450 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t452 = t323 * t450 * t82
      t455 = FJET(XB1, XB2, s, t313, 0.0D0, -t315, 0.0D0, 0.0D0, -t74 * 
     #t452 / 0.16D2)
      t460 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t465 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t183, 0.
     #0D0, -t185, 0.0D0)
      t483 = -t74 * t460 * t81 * t71 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t4
     #65 + t202 * t460) - 0.180D3 * t47 * t48 * t460) * t71 / 0.1440D4 -
     # t74 * (t460 * t224 + t460) * t56 * t71 / 0.16D2
      t484 = FJET(XB1, XB2, s, -t185, t183, 0.0D0, 0.0D0, 0.0D0, t483)
      t486 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t491 = FJET(XB1, XB2, s, -t185, -t349, 0.0D0, -t346, t354, -t356 *
     # t359 * t486 * t362 / 0.8D1)
      t499 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #313, -t315, 0.0D0)
      t501 = t323 * t499 * t82
      t504 = FJET(XB1, XB2, s, -t315, 0.0D0, t313, 0.0D0, 0.0D0, -t74 * 
     #t501 / 0.16D2)
      t509 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t514 = FJET(XB1, XB2, s, -t349, -t185, -t346, 0.0D0, t354, -t356 *
     # t359 * t509 * t362 / 0.8D1)
      t524 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, -t349, 0
     #.0D0, -t185, t354)
      t529 = FJET(XB1, XB2, s, -t346, 0.0D0, -t349, -t185, t354, -t4 * t
     #7 * t359 * t524 * t195 * t362 / 0.8D1)
      rrqg2qght7s2em1 = t132 * t131 + t181 * t180 + t232 * t231 + t258 *
     # t257 + t307 * t306 - t329 * pi * t48 * t326 / 0.16D2 - t339 * pi 
     #* t48 * t336 / 0.16D2 - t366 * pi * t48 * t369 * t360 * t81 * t71 
     #/ 0.8D1 + t422 * t421 + t448 * t447 - t455 * pi * t48 * t452 / 0.1
     #6D2 + t484 * t483 - t491 * pi * t48 * t369 * t486 * t81 * t71 / 0.
     #8D1 - t504 * pi * t48 * t501 / 0.16D2 - t514 * pi * t48 * t369 * t
     #509 * t81 * t71 / 0.8D1 - t529 * pi * t48 * t359 * t524 * t195 * t
     #81 * t71 / 0.8D1

      end function



      doubleprecision function rrqg2qght7s2em2
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t8 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = t4 * t7
      t15 = x3 * z
      t17 = x4 * pi
      t18 = cos(t17)
      t21 = Sqrt(-t15 * (-0.1D1 + x3))
      t26 = 0.1D1 / (-0.2D1 * t15 - 0.1D1 + x3 + 0.2D1 * t18 * t21) + 0.
     #1D1
      t28 = 0.1D1 / x3
      t32 = rrqg2qgh71J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t38 = z ** 2
      t41 = Sin(t17)
      t42 = t41 ** 2
      t44 = t1 ** 2
      t45 = t44 ** 2
      t48 = log(0.4D1 / t38 / z * t42 * t45)
      t52 = (0.180D3 * pi * lh + 0.90D2 * t48 * pi) * t3
      t55 = t4 * t9 * t10 / 0.16D2 + t14 * t8 * t26 * t28 / 0.32D2 + t4 
     #* t7 * t32 / 0.32D2 - t52 * t9 / 0.2880D4
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t55)
      t58 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t59 = t7 * t58
      t67 = rrqg2qgh73J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t73 = t4 * t59 * t10 / 0.16D2 + t14 * t58 * t26 * t28 / 0.32D2 + t
     #4 * t7 * t67 / 0.32D2 - t52 * t59 / 0.2880D4
      t74 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t73)
      t76 = t2 * x1
      t78 = t2 * (-0.1D1 + x1)
      t79 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0D
     #0, -t78, 0.0D0)
      t81 = t7 * t79 * t10
      t84 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t76, -t78, 0.0D0, -t4 * t81 
     #/ 0.16D2)
      t89 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0D
     #0, -t78, 0.0D0)
      t91 = t7 * t89 * t10
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t78, t76, 0.0D0, -t4 * t91 
     #/ 0.16D2)
      t99 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t100 = t7 * t99
      t108 = rrqg2qgh72J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t114 = t4 * t100 * t10 / 0.16D2 + t14 * t99 * t26 * t28 / 0.32D2 +
     # t4 * t7 * t108 / 0.32D2 - t52 * t100 / 0.2880D4
      t115 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t114)
      t117 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t118 = t7 * t117
      t126 = rrqg2qgh74J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t132 = t4 * t118 * t10 / 0.16D2 + t14 * t117 * t26 * t28 / 0.32D2 
     #+ t4 * t7 * t126 / 0.32D2 - t52 * t118 / 0.2880D4
      t133 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t132)
      t135 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0
     #D0, -t78, 0.0D0)
      t137 = t7 * t135 * t10
      t140 = FJET(XB1, XB2, s, t76, -t78, 0.0D0, 0.0D0, 0.0D0, -t4 * t13
     #7 / 0.16D2)
      t145 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t76, 0.0
     #D0, -t78, 0.0D0)
      t147 = t7 * t145 * t10
      t150 = FJET(XB1, XB2, s, -t78, t76, 0.0D0, 0.0D0, 0.0D0, -t4 * t14
     #7 / 0.16D2)
      rrqg2qght7s2em2 = t56 * t55 + t74 * t73 - t84 * pi * t3 * t81 / 0.
     #16D2 - t94 * pi * t3 * t91 / 0.16D2 + t115 * t114 + t133 * t132 - 
     #t140 * pi * t3 * t137 / 0.16D2 - t150 * pi * t3 * t147 / 0.16D2

      end function



      doubleprecision function rrqg2qght7s2em3
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

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
      t8 = rrqg2qgh71J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t7 * 
     #t8 / 0.32D2)
      t14 = t3 * t7
      t17 = rrqg2qgh73J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t21 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t7 * 
     #t17 / 0.32D2)
      t25 = rrqg2qgh72J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t29 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t25 / 0.32D2)
      t33 = rrqg2qgh74J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t37 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t33 / 0.32D2)
      rrqg2qght7s2em3 = t12 * pi * t14 * t8 / 0.32D2 + t21 * pi * t14 * 
     #t17 / 0.32D2 + t29 * pi * t14 * t25 / 0.32D2 + t37 * pi * t14 * t3
     #3 / 0.32D2

      end function



      doubleprecision function rrqg2qght7s2em4
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
      doubleprecision rrqg2qgh71J1
      doubleprecision rrqg2qgh71J2
      doubleprecision rrqg2qgh71J3
      doubleprecision rrqg2qgh71J4
      doubleprecision rrqg2qgh71J5
      doubleprecision rrqg2qgh71J6
      doubleprecision rrqg2qgh72J1
      doubleprecision rrqg2qgh72J2
      doubleprecision rrqg2qgh72J3
      doubleprecision rrqg2qgh72J4
      doubleprecision rrqg2qgh72J5
      doubleprecision rrqg2qgh72J6
      doubleprecision rrqg2qgh73J1
      doubleprecision rrqg2qgh73J2
      doubleprecision rrqg2qgh73J3
      doubleprecision rrqg2qgh73J4
      doubleprecision rrqg2qgh73J5
      doubleprecision rrqg2qgh73J6
      doubleprecision rrqg2qgh74J1
      doubleprecision rrqg2qgh74J2
      doubleprecision rrqg2qgh74J3
      doubleprecision rrqg2qgh74J4
      doubleprecision rrqg2qgh74J5
      doubleprecision rrqg2qgh74J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght7s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh71J1
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
      rrqg2qgh71J1 = (t1 * t2 * S34 + (-0.3D1 * S14 + 0.2D1 * S24) * t1 
     #* t2 + (0.2D1 * t10 + 0.3D1 * t12 - 0.4D1 * S24 * S14) * t1 * S34 
     #+ (-0.2D1 * t10 * S14 + 0.2D1 * S24 * t12 - t12 * S14) * t1) / S12
     # / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh71J2
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
      rrqg2qgh71J2 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (-0.3D1 * S14 
     #+ 0.2D1 * S24) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (0.2D1 * S24 * t20 - t20 * S14) * t1) / S12) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrqg2qgh71J3
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
      rrqg2qgh71J3 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (-0.3D1 * S14 
     #+ 0.2D1 * S24) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (0.2D1 * S24 * t20 - t20 * S14) * t1) / S12) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrqg2qgh71J4
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
      rrqg2qgh71J4 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (-0.3D1 * S14 
     #+ 0.2D1 * S24) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (0.2D1 * S24 * t20 - t20 * S14) * t1) / S12) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrqg2qgh71J5
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
      rrqg2qgh71J5 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (t1 * t12 * S34 + (-0.3D1 * S14 
     #+ 0.2D1 * S24) * t1 * t12 + (0.3D1 * t20 - 0.4D1 * S24 * S14) * t1
     # * S34 + (0.2D1 * S24 * t20 - t20 * S14) * t1) / S12) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrqg2qgh71J6
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
      t12 = S24 ** 2
      t13 = t1 * t12
      rrqg2qgh71J6 = ((0.2D1 * t1 * S34 - 0.2D1 * t1 * S14) * S12 + 0.2D
     #1 * t7 * S34 - 0.2D1 * t7 * S14 + (-0.2D1 * t13 * S34 + 0.2D1 * t1
     #3 * S14) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh72J1
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
      rrqg2qgh72J1 = (-0.4D1 * t1 * t2 - 0.4D1 * t1 * t5 - 0.8D1 * t1 * 
     #S24 * S34 - 0.4D1 * t1 * t11) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh72J2
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
      rrqg2qgh72J2 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh72J3
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
      rrqg2qgh72J3 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh72J4
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
      rrqg2qgh72J4 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh72J5
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
      rrqg2qgh72J5 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh72J6
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
      rrqg2qgh72J6 = (0.4D1 * t1 * t2 + (0.12D2 * t1 * S34 + 0.8D1 * t7)
     # * S12 + 0.4D1 * t1 * t11 + 0.12D2 * t7 * S34 + 0.4D1 * t1 * t16) 
     #/ pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh73J1
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
      t14 = S24 ** 2
      rrqg2qgh73J1 = (t1 * t2 * S34 + (-0.3D1 * S24 + 0.2D1 * S14) * t1 
     #* t2 + (0.2D1 * t10 - 0.4D1 * S24 * S14 + 0.3D1 * t14) * t1 * S34 
     #+ (-t14 * S24 - 0.2D1 * S24 * t10 + 0.2D1 * t14 * S14) * t1) / S12
     # / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh73J2
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
      t22 = S24 ** 2
      rrqg2qgh73J2 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (-0.4D1 * S24 * S14 + 0.3D1 * t22) * t
     #1 * S34 + (-t22 * S24 + 0.2D1 * t22 * S14) * t1) / S12) / pi * wd 
     #/ z

      end function
  
   
 

      doubleprecision function rrqg2qgh73J3
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
      t22 = S24 ** 2
      rrqg2qgh73J3 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (-0.4D1 * S24 * S14 + 0.3D1 * t22) * t
     #1 * S34 + (-t22 * S24 + 0.2D1 * t22 * S14) * t1) / S12) / pi * wd 
     #/ z

      end function
  
   
 

      doubleprecision function rrqg2qgh73J4
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
      t22 = S24 ** 2
      rrqg2qgh73J4 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (-0.4D1 * S24 * S14 + 0.3D1 * t22) * t
     #1 * S34 + (-t22 * S24 + 0.2D1 * t22 * S14) * t1) / S12) / pi * wd 
     #/ z

      end function
  
   
 

      doubleprecision function rrqg2qgh73J5
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
      t22 = S24 ** 2
      rrqg2qgh73J5 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (t1 * t12 * S34 + (-0.3D1 * S24 
     #+ 0.2D1 * S14) * t1 * t12 + (-0.4D1 * S24 * S14 + 0.3D1 * t22) * t
     #1 * S34 + (-t22 * S24 + 0.2D1 * t22 * S14) * t1) / S12) / pi * wd 
     #/ z

      end function
  
   
 

      doubleprecision function rrqg2qgh73J6
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
      rrqg2qgh73J6 = ((0.2D1 * t1 * S34 - 0.2D1 * t3) * S12 + 0.2D1 * t1
     # * S14 * S34 - 0.2D1 * t3 * S14 + (-0.2D1 * t1 * t12 * S34 + 0.2D1
     # * t3 * t12) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh74J1
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
      rrqg2qgh74J1 = (-0.4D1 * t1 * t2 - 0.4D1 * t1 * t5 - 0.8D1 * t1 * 
     #S14 * S34 - 0.4D1 * t1 * t11) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh74J2
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
      rrqg2qgh74J2 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh74J3
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
      rrqg2qgh74J3 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh74J4
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
      rrqg2qgh74J4 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh74J5
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
      rrqg2qgh74J5 = ((0.12D2 * t1 * S34 + 0.8D1 * t4) * S12 + 0.4D1 * t
     #4 * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh74J6
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
      rrqg2qgh74J6 = (0.4D1 * t1 * t2 + (0.12D2 * t1 * S34 + 0.8D1 * t7)
     # * S12 + 0.4D1 * t1 * t11 + 0.12D2 * t7 * S34 + 0.4D1 * t1 * t16) 
     #/ pi * wd / z

      end function
  
 