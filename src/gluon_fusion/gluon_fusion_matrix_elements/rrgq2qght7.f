  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * 0.3141592653589793D1
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t9 = t1 ** 2
      t10 = t9 ** 2
      t11 = t8 * t10
      t13 = log(0.4D1 * t11)
      t14 = t13 ** 2
      t15 = t14 * 0.3141592653589793D1
      t18 = 0.3141592653589793D1 ** 2
      t21 = lh ** 2
      t24 = 0.60D2 * lh * t18 - 0.2884936567583026D3 - 0.120D3 * t21 * l
     #h
      t25 = 0.3141592653589793D1 * t24
      t27 = t14 * t13 * 0.3141592653589793D1
      t29 = t13 * 0.3141592653589793D1
      t32 = 0.180D3 * t21 - 0.30D2 * t18
      t35 = 0.1D1 / t1
      t36 = (-0.90D2 * t15 * lh + t25 - 0.15D2 * t27 - t29 * t32) * t35
      t37 = s ** 2
      t39 = 0.1D1 / t37 / s
      t40 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t41 = t39 * t40
      t47 = 0.3141592653589793D1 * t32
      t49 = (0.180D3 * t29 * lh + 0.45D2 * t15 + t47) * t35
      t50 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t51 = t39 * t50
      t54 = 0.3141592653589793D1 * lh
      t58 = (-0.180D3 * t54 - 0.90D2 * t29) * t35
      t59 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t60 = t39 * t59
      t63 = 0.3141592653589793D1 * t35
      t64 = rrgq2qgh73J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t73 = t18 ** 2
      t74 = t21 ** 2
      t81 = t14 ** 2
      t85 = (0.30D2 * t27 * lh + t15 * t32 / 0.2D1 - t29 * t24 + 0.31415
     #92653589793D1 * (t73 + 0.60D2 * t74 + 0.5769873135166051D3 * lh - 
     #0.60D2 * t21 * t18) + 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D1)
     # * t35
      t86 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t87 = t39 * t86
      t90 = x1 ** 2
      t91 = x3 * t90
      t94 = log(0.4D1 * t91 * t11)
      t95 = t94 ** 2
      t98 = t91 * t7
      t99 = t4 * t10
      t100 = -0.1D1 + x3
      t101 = 0.1D1 / t100
      t102 = t99 * t101
      t105 = log(-0.4D1 * t98 * t102)
      t107 = t105 ** 2
      t111 = cos(t5)
      t113 = Sqrt(-x3 * t100)
      t117 = 0.1D1 / (-0.1D1 + 0.2D1 * t111 * t113 - x3)
      t124 = t35 * t39
      t134 = t86 + t86 * t117
      t138 = 0.1D1 / x3
      t140 = 0.1D1 / x1
      t143 = t90 * t7
      t146 = log(0.4D1 * t143 * t99)
      t151 = t146 ** 2
      t154 = t151 * t146
      t162 = t124 * t86
      t163 = t25 * t162
      t174 = x2 ** 2
      t175 = x3 * t174
      t176 = t175 * t90
      t181 = log(-0.4D1 * t176 * t8 * t10 * t101)
      t187 = log(0.4D1 * t176 * t11)
      t194 = -t124 * t134
      t199 = 0.1D1 / x2
      t200 = t199 * t140
      t203 = t174 * t90
      t206 = log(0.4D1 * t203 * t11)
      t208 = t206 ** 2
      t220 = t47 * t162
      t225 = t124 * t40
      t231 = x3 * t7
      t234 = log(0.4D1 * t231 * t99)
      t237 = log(-0.4D1 * t231 * t102)
      t239 = t234 + t237 * t117
      t241 = t237 ** 2
      t244 = t234 ** 2
      t247 = t241 * t237 * t117 / 0.6D1 + t244 * t234 / 0.6D1
      t258 = -0.1D1 - t117
      t267 = -t244 / 0.2D1 - t241 * t117 / 0.2D1
      t272 = t175 * t7
      t275 = log(-0.4D1 * t272 * t102)
      t277 = t275 ** 2
      t284 = log(0.4D1 * t175 * t11)
      t285 = t284 ** 2
      t306 = t174 * t7
      t309 = log(0.4D1 * t306 * t99)
      t314 = t309 ** 2
      t317 = t314 * t309
      t335 = t36 * t41 / 0.2880D4 + t49 * t51 / 0.2880D4 + t58 * t60 / 0
     #.2880D4 + t63 * t39 * t64 / 0.32D2 + t85 * t87 / 0.2880D4 + (0.90D
     #2 * t63 * t39 * (t50 + t95 * t86 / 0.2D1 + (-t105 * t40 + t50 + t1
     #07 * t86 / 0.2D1) * t117 - t94 * t40) - 0.180D3 * t54 * t124 * ((t
     #40 - t105 * t86) * t117 + t40 - t94 * t86) + t47 * t124 * t134) * 
     #t138 * t140 / 0.1440D4 + (t47 * t124 * (t40 - t146 * t86) + 0.90D2
     # * t63 * t39 * (t151 * t40 / 0.2D1 - t154 * t86 / 0.6D1 + t59 - t1
     #46 * t50) + t163 - 0.180D3 * t54 * t124 * (t50 + t151 * t86 / 0.2D
     #1 - t146 * t40)) * t140 / 0.1440D4 - (0.90D2 * t63 * t39 * (-t40 -
     # (t40 - t181 * t86) * t117 + t187 * t86) - 0.180D3 * t54 * t194) *
     # t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-t50 + t206 * t40 
     #- t208 * t86 / 0.2D1) - 0.180D3 * t54 * t124 * (t206 * t86 - t40) 
     #- t220) * t199 * t140 / 0.720D3 - ((-0.180D3 * t54 * t225 + t220 +
     # 0.90D2 * t63 * t51) * t239 + 0.90D2 * t63 * t87 * t247 + (t47 * t
     #225 + 0.90D2 * t63 * t60 + t163 - 0.180D3 * t54 * t124 * t50) * t2
     #58 + (0.90D2 * t63 * t41 - 0.180D3 * t54 * t162) * t267) * t138 / 
     #0.2880D4 - (0.90D2 * t63 * t39 * (-(-t275 * t40 + t50 + t277 * t86
     # / 0.2D1) * t117 - t285 * t86 / 0.2D1 + t284 * t40 - t50) - 0.180D
     #3 * t54 * t124 * (-(t40 - t275 * t86) * t117 + t284 * t86 - t40) +
     # t47 * t194) * t138 * t199 / 0.1440D4 - (t47 * t124 * (t309 * t86 
     #- t40) + 0.90D2 * t63 * t39 * (-t314 * t40 / 0.2D1 + t317 * t86 / 
     #0.6D1 - t59 + t309 * t50) - t163 - 0.180D3 * t54 * t124 * (-t314 *
     # t86 / 0.2D1 + t309 * t40 - t50)) * t199 / 0.1440D4
      t336 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t335)
      t338 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t340 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t341 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t362 = -t341 - t341 * t117
      t363 = t124 * t362
      t376 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t383 = t124 * t341
      t384 = t25 * t383
      t395 = t39 * t338
      t398 = t39 * t340
      t401 = t39 * t376
      t404 = rrgq2qgh71J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t408 = t39 * t341
      t411 = t124 * t338
      t414 = t47 * t383
      t519 = -(0.90D2 * t63 * t39 * (t284 * t338 - t340 - (t277 * t341 /
     # 0.2D1 - t275 * t338 + t340) * t117 - t285 * t341 / 0.2D1) - 0.180
     #D3 * t54 * t124 * (-(t338 - t275 * t341) * t117 + t284 * t341 - t3
     #38) + t47 * t363) * t138 * t199 / 0.1440D4 - (t47 * t124 * (-t338 
     #+ t309 * t341) + 0.90D2 * t63 * t39 * (-t314 * t338 / 0.2D1 + t309
     # * t340 - t376 + t317 * t341 / 0.6D1) - t384 - 0.180D3 * t54 * t12
     #4 * (-t340 - t314 * t341 / 0.2D1 + t309 * t338)) * t199 / 0.1440D4
     # + t36 * t395 / 0.2880D4 + t49 * t398 / 0.2880D4 + t58 * t401 / 0.
     #2880D4 + t63 * t39 * t404 / 0.32D2 + t85 * t408 / 0.2880D4 - ((-0.
     #180D3 * t54 * t411 + t414 + 0.90D2 * t63 * t398) * t239 + 0.90D2 *
     # t63 * t408 * t247 + (t47 * t411 + 0.90D2 * t63 * t401 + t384 - 0.
     #180D3 * t54 * t124 * t340) * t258 + (0.90D2 * t63 * t395 - 0.180D3
     # * t54 * t383) * t267) * t138 / 0.2880D4 + (0.90D2 * t63 * t39 * (
     #t95 * t341 / 0.2D1 - t94 * t338 + t340 + (t107 * t341 / 0.2D1 - t1
     #05 * t338 + t340) * t117) - 0.180D3 * t54 * t124 * (t338 + (t338 -
     # t105 * t341) * t117 - t94 * t341) - t47 * t124 * t362) * t138 * t
     #140 / 0.1440D4 + (t47 * t124 * (t338 - t146 * t341) + 0.90D2 * t63
     # * t39 * (t151 * t338 / 0.2D1 - t146 * t340 + t376 - t154 * t341 /
     # 0.6D1) + t384 - 0.180D3 * t54 * t124 * (-t146 * t338 + t340 + t15
     #1 * t341 / 0.2D1)) * t140 / 0.1440D4 - (0.90D2 * t63 * t39 * (t187
     # * t341 - t338 - (t338 - t181 * t341) * t117) - 0.180D3 * t54 * t3
     #63) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-t340 - t208 
     #* t341 / 0.2D1 + t206 * t338) - 0.180D3 * t54 * t124 * (-t338 + t2
     #06 * t341) - t414) * t199 * t140 / 0.720D3
      t520 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t519)
      t522 = t2 * x1
      t523 = -0.1D1 + x1
      t524 = t2 * t523
      t525 = -t523
      t526 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t527 = x1 * z
      t528 = 0.1D1 - x1 + t527
      t529 = 0.1D1 / t528
      t530 = t523 ** 2
      t531 = t529 * t530
      t532 = t99 * t531
      t535 = log(0.4D1 * t98 * t532)
      t536 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t539 = t10 * t529
      t544 = log(-0.4D1 * t91 * t8 * t539 * t530 * t101)
      t546 = t544 ** 2
      t547 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t551 = x3 * x1
      t552 = t551 * z
      t555 = x3 * t528
      t557 = Sqrt(-t555 * t100)
      t561 = 0.1D1 / (-0.2D1 * t552 + 0.2D1 * t551 - 0.1D1 - x3 + 0.2D1 
     #* t111 * t557)
      t563 = t535 ** 2
      t579 = -t547 * t561 - t547
      t586 = t143 * t4
      t587 = t539 * t530
      t590 = log(0.4D1 * t586 * t587)
      t595 = t590 ** 2
      t596 = t595 * t590
      t599 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t607 = t124 * t547
      t619 = t175 * t143
      t624 = log(-0.4D1 * t619 * t99 * t531 * t101)
      t630 = log(0.4D1 * t619 * t532)
      t644 = t203 * t7
      t647 = log(0.4D1 * t644 * t532)
      t649 = t647 ** 2
      t666 = (0.90D2 * t63 * t39 * (-t526 + t535 * t536 - (-t544 * t536 
     #+ t526 + t546 * t547 / 0.2D1) * t561 - t563 * t547 / 0.2D1) - 0.18
     #0D3 * t54 * t124 * (-t536 - (t536 - t544 * t547) * t561 + t535 * t
     #547) + t47 * t124 * t579) * t138 * t140 / 0.1440D4 + (t47 * t124 *
     # (t590 * t547 - t536) + 0.90D2 * t63 * t39 * (t596 * t547 / 0.6D1 
     #- t599 + t590 * t526 - t595 * t536 / 0.2D1) - t25 * t607 - 0.180D3
     # * t54 * t124 * (-t595 * t547 / 0.2D1 - t526 + t590 * t536)) * t14
     #0 / 0.1440D4 - (0.90D2 * t63 * t39 * ((t536 - t624 * t547) * t561 
     #+ t536 - t630 * t547) + 0.180D3 * t54 * t124 * t579) * t138 * t200
     # / 0.720D3 - (0.90D2 * t63 * t39 * (-t647 * t536 + t649 * t547 / 0
     #.2D1 + t526) - 0.180D3 * t54 * t124 * (t536 - t647 * t547) + t47 *
     # t607) * t199 * t140 / 0.720D3
      t667 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t522, -t524, 0.0D0, t666)
      t669 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t670 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t673 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t693 = -t670 * t561 - t670
      t707 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D0
     #, x4)
      t714 = t124 * t670
      t759 = (0.90D2 * t63 * t39 * (-t669 - (t546 * t670 / 0.2D1 - t544 
     #* t673 + t669) * t561 - t563 * t670 / 0.2D1 + t535 * t673) - 0.180
     #D3 * t54 * t124 * (-(t673 - t544 * t670) * t561 + t535 * t670 - t6
     #73) + t47 * t124 * t693) * t138 * t140 / 0.1440D4 + (t47 * t124 * 
     #(-t673 + t590 * t670) + 0.90D2 * t63 * t39 * (-t595 * t673 / 0.2D1
     # + t590 * t669 - t707 + t596 * t670 / 0.6D1) - t25 * t714 - 0.180D
     #3 * t54 * t124 * (t590 * t673 - t669 - t595 * t670 / 0.2D1)) * t14
     #0 / 0.1440D4 - (0.90D2 * t63 * t39 * (-t630 * t670 + (t673 - t624 
     #* t670) * t561 + t673) + 0.180D3 * t54 * t124 * t693) * t138 * t20
     #0 / 0.720D3 - (0.90D2 * t63 * t39 * (-t647 * t673 + t669 + t649 * 
     #t670 / 0.2D1) - 0.180D3 * t54 * t124 * (t673 - t647 * t670) + t47 
     #* t714) * t199 * t140 / 0.720D3
      t760 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t524, t522, 0.0D0, t759)
      t762 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t765 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t766 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t786 = t762 * t117 + t762
      t798 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t807 = t124 * t762
      t808 = t25 * t807
      t828 = -t124 * t786
      t847 = t47 * t807
      t852 = t39 * t765
      t855 = t39 * t798
      t906 = t39 * t766
      t909 = rrgq2qgh74J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t913 = t124 * t766
      t920 = t39 * t762
      t943 = (0.90D2 * t63 * t39 * (t95 * t762 / 0.2D1 + t765 + (-t105 *
     # t766 + t765 + t107 * t762 / 0.2D1) * t117 - t94 * t766) - 0.180D3
     # * t54 * t124 * (-t94 * t762 + t766 + (t766 - t105 * t762) * t117)
     # + t47 * t124 * t786) * t138 * t140 / 0.1440D4 + (t47 * t124 * (t7
     #66 - t146 * t762) + 0.90D2 * t63 * t39 * (-t146 * t765 + t798 + t1
     #51 * t766 / 0.2D1 - t154 * t762 / 0.6D1) + t808 - 0.180D3 * t54 * 
     #t124 * (-t146 * t766 + t765 + t151 * t762 / 0.2D1)) * t140 / 0.144
     #0D4 - (0.90D2 * t63 * t39 * (-t766 + t187 * t762 - (t766 - t181 * 
     #t762) * t117) - 0.180D3 * t54 * t828) * t138 * t200 / 0.720D3 - (0
     #.90D2 * t63 * t39 * (t206 * t766 - t208 * t762 / 0.2D1 - t765) - 0
     #.180D3 * t54 * t124 * (t206 * t762 - t766) - t847) * t199 * t140 /
     # 0.720D3 + t49 * t852 / 0.2880D4 + t58 * t855 / 0.2880D4 - (0.90D2
     # * t63 * t39 * (-(-t275 * t766 + t765 + t277 * t762 / 0.2D1) * t11
     #7 - t765 - t285 * t762 / 0.2D1 + t284 * t766) - 0.180D3 * t54 * t1
     #24 * (-(t766 - t275 * t762) * t117 - t766 + t284 * t762) + t47 * t
     #828) * t138 * t199 / 0.1440D4 - (t47 * t124 * (t309 * t762 - t766)
     # + 0.90D2 * t63 * t39 * (-t798 - t314 * t766 / 0.2D1 + t317 * t762
     # / 0.6D1 + t309 * t765) - t808 - 0.180D3 * t54 * t124 * (t309 * t7
     #66 - t314 * t762 / 0.2D1 - t765)) * t199 / 0.1440D4 + t36 * t906 /
     # 0.2880D4 + t63 * t39 * t909 / 0.32D2 - ((-0.180D3 * t54 * t913 + 
     #t847 + 0.90D2 * t63 * t852) * t239 + 0.90D2 * t63 * t920 * t247 + 
     #(t47 * t913 + 0.90D2 * t63 * t855 + t808 - 0.180D3 * t54 * t124 * 
     #t765) * t258 + (0.90D2 * t63 * t906 - 0.180D3 * t54 * t807) * t267
     #) * t138 / 0.2880D4 + t85 * t920 / 0.2880D4
      t944 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t943)
      t947 = x2 * s * t1
      t948 = -0.1D1 + x2
      t949 = t948 * s
      t950 = t949 * t1
      t951 = t10 * t948
      t955 = log(-0.4D1 * t176 * t8 * t951)
      t956 = x2 * z
      t958 = 0.1D1 / (0.1D1 + t956 - x2)
      t959 = t955 * t958
      t960 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t962 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t963 = t958 * t962
      t968 = t54 * t35
      t969 = t39 * t958
      t970 = t969 * t960
      t977 = t99 * t948
      t980 = log(-0.4D1 * t644 * t977)
      t981 = t980 * t958
      t983 = t980 ** 2
      t984 = t983 * t958
      t987 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t988 = t958 * t987
      t998 = t47 * t35
      t999 = t998 * t970
      t1006 = log(-0.4D1 * t272 * t977)
      t1007 = t1006 * t958
      t1009 = t1006 ** 2
      t1010 = t1009 * t958
      t1028 = log(-0.4D1 * t306 * t977)
      t1029 = t1028 * t958
      t1034 = t1028 ** 2
      t1035 = t1034 * t958
      t1038 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1041 = t1034 * t1028 * t958
      t1049 = t25 * t35
      t1061 = -(0.90D2 * t63 * t39 * (-t959 * t960 + t963) - 0.180D3 * t
     #968 * t970) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-t981
     # * t962 + t984 * t960 / 0.2D1 + t988) - 0.180D3 * t54 * t124 * (t9
     #63 - t981 * t960) + t999) * t199 * t140 / 0.720D3 - (0.90D2 * t63 
     #* t39 * (-t1007 * t962 + t988 + t1010 * t960 / 0.2D1) - 0.180D3 * 
     #t54 * t124 * (t963 - t1007 * t960) + t999) * t138 * t199 / 0.1440D
     #4 - (t47 * t124 * (t963 - t1029 * t960) + 0.90D2 * t63 * t39 * (t1
     #035 * t962 / 0.2D1 + t958 * t1038 - t1041 * t960 / 0.6D1 - t1029 *
     # t987) + t1049 * t970 - 0.180D3 * t54 * t124 * (t988 - t1029 * t96
     #2 + t1035 * t960 / 0.2D1)) * t199 / 0.1440D4
      t1062 = FJET(XB1, XB2, s, 0.0D0, t947, 0.0D0, -t950, 0.0D0, t1061)
      t1064 = x2 * x3
      t1067 = Sqrt(x3 * t948 * t100)
      t1068 = t111 * t1067
      t1070 = 0.2D1 * t1068 * x2
      t1072 = 0.1D1 - x3 + t1064
      t1073 = 0.1D1 / t1072
      t1075 = t2 * (0.1D1 - x3 - x2 + t1064 + t175 + t1070) * t1073
      t1076 = 0.2D1 * t1068
      t1080 = t2 * x2 * (-0.1D1 + t1064 + t1076) * t1073
      t1081 = t948 * t100
      t1082 = t1072 ** 2
      t1083 = 0.1D1 / t1082
      t1084 = t1081 * t1083
      t1088 = log(0.4D1 * t619 * t99 * t1084)
      t1089 = t175 * z
      t1093 = 0.1D1 / (-0.1D1 - t956 - t175 + t1064 - t1070 + t1076 + t1
     #089 + x2 - x3 + 0.2D1 * t1068 * t956)
      t1094 = t1088 * t1093
      t1095 = t1064 * t1073
      t1096 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1098 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1099 = t1093 * t1098
      t1104 = t39 * t1093
      t1105 = t1104 * t1096
      t1117 = log(0.4D1 * t175 * t8 * t951 * t100 * t1083)
      t1118 = t1117 * t1093
      t1120 = t1117 ** 2
      t1121 = t1120 * t1093
      t1124 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1140 = -(0.90D2 * t63 * t39 * (-t1094 * t1096 + t1099) - 0.180D3 
     #* t968 * t1105) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-
     #t1118 * t1098 + t1121 * t1096 / 0.2D1 + t1093 * t1124) - 0.180D3 *
     # t54 * t124 * (-t1118 * t1096 + t1099) + t998 * t1105) * t138 * t1
     #99 / 0.1440D4
      t1141 = FJET(XB1, XB2, s, 0.0D0, t1075, 0.0D0, -t1080, 0.0D0, t114
     #0)
      t1143 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1145 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1146 = t958 * t1145
      t1151 = t969 * t1143
      t1159 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1160 = t958 * t1159
      t1172 = t998 * t1151
      t1199 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1219 = -(0.90D2 * t63 * t39 * (-t959 * t1143 + t1146) - 0.180D3 *
     # t968 * t1151) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-t
     #981 * t1145 + t1160 + t984 * t1143 / 0.2D1) - 0.180D3 * t54 * t124
     # * (t1146 - t981 * t1143) + t1172) * t199 * t140 / 0.720D3 - (0.90
     #D2 * t63 * t39 * (-t1007 * t1145 + t1160 + t1010 * t1143 / 0.2D1) 
     #- 0.180D3 * t54 * t124 * (t1146 - t1007 * t1143) + t1172) * t138 *
     # t199 / 0.1440D4 - (t47 * t124 * (t1146 - t1029 * t1143) + 0.90D2 
     #* t63 * t39 * (t1035 * t1145 / 0.2D1 + t958 * t1199 - t1029 * t115
     #9 - t1041 * t1143 / 0.6D1) + t1049 * t1151 - 0.180D3 * t54 * t124 
     #* (t1160 - t1029 * t1145 + t1035 * t1143 / 0.2D1)) * t199 / 0.1440
     #D4
      t1220 = FJET(XB1, XB2, s, 0.0D0, -t950, 0.0D0, t947, 0.0D0, t1219)
      t1222 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1224 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1225 = t1093 * t1224
      t1230 = t1104 * t1222
      t1240 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1256 = -(0.90D2 * t63 * t39 * (-t1094 * t1222 + t1225) - 0.180D3 
     #* t968 * t1230) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (t
     #1121 * t1222 / 0.2D1 - t1118 * t1224 + t1093 * t1240) - 0.180D3 * 
     #t54 * t124 * (t1225 - t1118 * t1222) + t998 * t1230) * t138 * t199
     # / 0.1440D4
      t1257 = FJET(XB1, XB2, s, 0.0D0, -t1080, 0.0D0, t1075, 0.0D0, t125
     #6)
      t1261 = t2 * t523 * x2 * t529
      t1263 = t949 * t1 * t523
      t1268 = s * t9 * x2 * x1 * t523 * t529
      t1269 = x2 * x1
      t1270 = t1269 * z
      t1272 = 0.1D1 / (-0.1D1 - t956 - t1269 + t1270 - t527 + x2 + x1)
      t1273 = t528 * t1272
      t1274 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1275 = t1273 * t1274
      t1280 = log(-0.4D1 * t619 * t99 * t531 * t948)
      t1281 = t1280 * t528
      t1282 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1283 = t1272 * t1282
      t1289 = t39 * t528
      t1290 = t1289 * t1283
      t1296 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1303 = log(-0.4D1 * t203 * t8 * t539 * t530 * t948)
      t1304 = t1303 * t528
      t1307 = t1303 ** 2
      t1308 = t1307 * t528
      t1325 = -(0.90D2 * t63 * t39 * (t1275 - t1281 * t1283) - 0.180D3 *
     # t968 * t1290) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (t1
     #273 * t1296 - t1304 * t1272 * t1274 + t1308 * t1283 / 0.2D1) - 0.1
     #80D3 * t54 * t124 * (-t1304 * t1283 + t1275) + t998 * t1290) * t19
     #9 * t140 / 0.720D3
      t1326 = FJET(XB1, XB2, s, 0.0D0, -t1261, t522, t1263, -t1268, t132
     #5)
      t1328 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t1329 = t124 * t1328
      t1332 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t1333 = t124 * t1332
      t1334 = t47 * t1333
      t1335 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t1336 = t39 * t1335
      t1341 = t39 * t1332
      t1346 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t1347 = t39 * t1346
      t1350 = t25 * t1333
      t1356 = t39 * t1328
      t1395 = t1332 * t117 + t1332
      t1434 = -t124 * t1395
      t1505 = rrgq2qgh72J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.
     #0D0, x4)
      t1509 = -((-0.180D3 * t54 * t1329 + t1334 + 0.90D2 * t63 * t1336) 
     #* t239 + 0.90D2 * t63 * t1341 * t247 + (t47 * t1329 + 0.90D2 * t63
     # * t1347 + t1350 - 0.180D3 * t54 * t124 * t1335) * t258 + (0.90D2 
     #* t63 * t1356 - 0.180D3 * t54 * t1333) * t267) * t138 / 0.2880D4 +
     # t85 * t1341 / 0.2880D4 + t36 * t1356 / 0.2880D4 + t49 * t1336 / 0
     #.2880D4 + t58 * t1347 / 0.2880D4 + (0.90D2 * t63 * t39 * (t95 * t1
     #332 / 0.2D1 + (-t105 * t1328 + t1335 + t107 * t1332 / 0.2D1) * t11
     #7 + t1335 - t94 * t1328) - 0.180D3 * t54 * t124 * ((t1328 - t105 *
     # t1332) * t117 - t94 * t1332 + t1328) + t47 * t124 * t1395) * t138
     # * t140 / 0.1440D4 + (t47 * t124 * (-t146 * t1332 + t1328) + 0.90D
     #2 * t63 * t39 * (t151 * t1328 / 0.2D1 + t1346 - t146 * t1335 - t15
     #4 * t1332 / 0.6D1) + t1350 - 0.180D3 * t54 * t124 * (-t146 * t1328
     # + t1335 + t151 * t1332 / 0.2D1)) * t140 / 0.1440D4 - (0.90D2 * t6
     #3 * t39 * (-t1328 + t187 * t1332 - (t1328 - t181 * t1332) * t117) 
     #- 0.180D3 * t54 * t1434) * t138 * t200 / 0.720D3 - (0.90D2 * t63 *
     # t39 * (t206 * t1328 - t1335 - t208 * t1332 / 0.2D1) - 0.180D3 * t
     #54 * t124 * (t206 * t1332 - t1328) - t1334) * t199 * t140 / 0.720D
     #3 - (0.90D2 * t63 * t39 * (-(-t275 * t1328 + t1335 + t277 * t1332 
     #/ 0.2D1) * t117 - t285 * t1332 / 0.2D1 + t284 * t1328 - t1335) - 0
     #.180D3 * t54 * t124 * (-(t1328 - t275 * t1332) * t117 + t284 * t13
     #32 - t1328) + t47 * t1434) * t138 * t199 / 0.1440D4 - (t47 * t124 
     #* (-t1328 + t309 * t1332) + 0.90D2 * t63 * t39 * (t317 * t1332 / 0
     #.6D1 - t1346 + t309 * t1335 - t314 * t1328 / 0.2D1) - t1350 - 0.18
     #0D3 * t54 * t124 * (-t1335 - t314 * t1332 / 0.2D1 + t309 * t1328))
     # * t199 / 0.1440D4 + t63 * t39 * t1505 / 0.32D2
      t1510 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1509)
      t1512 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1513 = t1272 * t1512
      t1515 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1516 = t1273 * t1515
      t1521 = t1289 * t1513
      t1529 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1547 = -(0.90D2 * t63 * t39 * (-t1281 * t1513 + t1516) - 0.180D3 
     #* t968 * t1521) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-
     #t1304 * t1272 * t1515 + t1273 * t1529 + t1308 * t1513 / 0.2D1) - 0
     #.180D3 * t54 * t124 * (t1516 - t1304 * t1513) + t998 * t1521) * t1
     #99 * t140 / 0.720D3
      t1548 = FJET(XB1, XB2, s, t522, t1263, 0.0D0, -t1261, -t1268, t154
     #7)
      t1550 = t336 * t335 + t520 * t519 + t667 * t666 + t760 * t759 + t9
     #44 * t943 + t1062 * t1061 + t1141 * t1140 + t1220 * t1219 + t1257 
     #* t1256 + t1326 * t1325 + t1510 * t1509 + t1548 * t1547
      t1551 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1552 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1555 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1575 = -t1555 * t561 - t1555
      t1590 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1596 = t124 * t1555
      t1641 = (0.90D2 * t63 * t39 * (-t1551 + t535 * t1552 - (-t544 * t1
     #552 + t1551 + t546 * t1555 / 0.2D1) * t561 - t563 * t1555 / 0.2D1)
     # - 0.180D3 * t54 * t124 * (-(t1552 - t544 * t1555) * t561 + t535 *
     # t1555 - t1552) + t47 * t124 * t1575) * t138 * t140 / 0.1440D4 + (
     #t47 * t124 * (-t1552 + t590 * t1555) + 0.90D2 * t63 * t39 * (-t595
     # * t1552 / 0.2D1 + t596 * t1555 / 0.6D1 - t1590 + t590 * t1551) - 
     #t25 * t1596 - 0.180D3 * t54 * t124 * (-t595 * t1555 / 0.2D1 - t155
     #1 + t590 * t1552)) * t140 / 0.1440D4 - (0.90D2 * t63 * t39 * (t155
     #2 + (t1552 - t624 * t1555) * t561 - t630 * t1555) + 0.180D3 * t54 
     #* t124 * t1575) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (t
     #1551 + t649 * t1555 / 0.2D1 - t647 * t1552) - 0.180D3 * t54 * t124
     # * (-t647 * t1555 + t1552) + t47 * t1596) * t199 * t140 / 0.720D3
      t1642 = FJET(XB1, XB2, s, t522, -t524, 0.0D0, 0.0D0, 0.0D0, t1641)
      t1644 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1645 = t958 * t1644
      t1646 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1652 = t969 * t1646
      t1660 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1661 = t958 * t1660
      t1673 = t998 * t1652
      t1702 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1720 = -(0.90D2 * t63 * t39 * (t1645 - t959 * t1646) - 0.180D3 * 
     #t968 * t1652) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-t9
     #81 * t1644 + t1661 + t984 * t1646 / 0.2D1) - 0.180D3 * t54 * t124 
     #* (t1645 - t981 * t1646) + t1673) * t199 * t140 / 0.720D3 - (0.90D
     #2 * t63 * t39 * (-t1007 * t1644 + t1661 + t1010 * t1646 / 0.2D1) -
     # 0.180D3 * t54 * t124 * (-t1007 * t1646 + t1645) + t1673) * t138 *
     # t199 / 0.1440D4 - (t47 * t124 * (t1645 - t1029 * t1646) + 0.90D2 
     #* t63 * t39 * (t1035 * t1644 / 0.2D1 - t1041 * t1646 / 0.6D1 + t95
     #8 * t1702 - t1029 * t1660) + t1049 * t1652 - 0.180D3 * t54 * t124 
     #* (-t1029 * t1644 + t1661 + t1035 * t1646 / 0.2D1)) * t199 / 0.144
     #0D4
      t1721 = FJET(XB1, XB2, s, t947, 0.0D0, -t950, 0.0D0, 0.0D0, t1720)
      t1723 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1725 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1726 = t1093 * t1725
      t1731 = t1104 * t1723
      t1741 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t1757 = -(0.90D2 * t63 * t39 * (-t1094 * t1723 + t1726) - 0.180D3 
     #* t968 * t1731) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (t
     #1121 * t1723 / 0.2D1 - t1118 * t1725 + t1093 * t1741) - 0.180D3 * 
     #t54 * t124 * (-t1118 * t1723 + t1726) + t998 * t1731) * t138 * t19
     #9 / 0.1440D4
      t1758 = FJET(XB1, XB2, s, t1075, 0.0D0, -t1080, 0.0D0, 0.0D0, t175
     #7)
      t1760 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1761 = t1273 * t1760
      t1762 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1763 = t1272 * t1762
      t1769 = t1289 * t1763
      t1777 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t1795 = -(0.90D2 * t63 * t39 * (t1761 - t1281 * t1763) - 0.180D3 *
     # t968 * t1769) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-t
     #1304 * t1272 * t1760 + t1273 * t1777 + t1308 * t1763 / 0.2D1) - 0.
     #180D3 * t54 * t124 * (t1761 - t1304 * t1763) + t998 * t1769) * t19
     #9 * t140 / 0.720D3
      t1796 = FJET(XB1, XB2, s, t1263, t522, -t1261, 0.0D0, -t1268, t179
     #5)
      t1798 = t522 * t1095
      t1799 = t1064 * x1
      t1800 = t1064 * t527
      t1802 = Sqrt(t555 * t1081)
      t1803 = t111 * t1802
      t1804 = 0.2D1 * t1803
      t1809 = t524 * x2 * (t551 - t552 + t1064 - t1799 + t1800 - 0.1D1 +
     # t1804) * t529 * t1073
      t1813 = t100 * s * t1 * x1 * t1073
      t1815 = 0.2D1 * t1803 * x2
      t1816 = 0.1D1 - x1 + t527 - x2 + t1269 - t1270 - x3 + t551 - t552 
     #+ t1064 - t1799 + t1800 + t175 + t1815
      t1819 = t524 * t1816 * t529 * t1073
      t1831 = 0.1D1 - t1804 + 0.2D1 * t91 - x2 + x3 - x1 - t1089 + t1269
     # + 0.3D1 * t552 - 0.2D1 * t91 * x2 + 0.2D1 * t91 * t3 - 0.2D1 * t1
     #75 * x1 + t176 + 0.2D1 * t1803 * x1 - t1270 - 0.3D1 * t551 + t527
      t1860 = 0.2D1 * t1803 * t1270 + t956 + 0.3D1 * t1799 + t1815 - 0.4
     #D1 * t91 * z - t1064 + t175 - 0.4D1 * t1800 - t175 * t3 * x1 - 0.2
     #D1 * t175 * t90 * z + t175 * t90 * t3 - 0.2D1 * t1803 * t1269 - 0.
     #2D1 * t1803 * t956 + x3 * t3 * t1269 + 0.4D1 * t91 * t956 - 0.2D1 
     #* t91 * t3 * x2 + 0.3D1 * t175 * t527 - 0.2D1 * t1803 * t527
      t1862 = 0.1D1 / (t1831 + t1860)
      t1863 = t528 * t1862
      t1864 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t1870 = log(0.4D1 * t175 * t586 * t587 * t1084)
      t1871 = t1870 * t528
      t1872 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t1873 = t1862 * t1872
      t1882 = 0.90D2 * t63 * t39 * (t1863 * t1864 - t1871 * t1873) - 0.1
     #80D3 * t968 * t1289 * t1873
      t1886 = FJET(XB1, XB2, s, t1798, t1809, -t1813, -t1819, -t1268, -t
     #1882 * t138 * t200 / 0.720D3)
      t1889 = t138 * t199 * t140
      t1892 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t1894 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t1895 = t1862 * t1894
      t1904 = 0.90D2 * t63 * t39 * (t1863 * t1892 - t1871 * t1895) - 0.1
     #80D3 * t968 * t1289 * t1895
      t1908 = FJET(XB1, XB2, s, t1809, t1798, -t1819, -t1813, -t1268, -t
     #1904 * t138 * t200 / 0.720D3)
      t1912 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1914 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1915 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1936 = -t1915 * t561 - t1915
      t1950 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, t525, 0.0D0, 0.0D
     #0, x4)
      t1957 = t124 * t1915
      t2002 = (0.90D2 * t63 * t39 * (-(-t544 * t1912 + t1914 + t546 * t1
     #915 / 0.2D1) * t561 - t563 * t1915 / 0.2D1 + t535 * t1912 - t1914)
     # - 0.180D3 * t54 * t124 * (-(t1912 - t544 * t1915) * t561 + t535 *
     # t1915 - t1912) + t47 * t124 * t1936) * t138 * t140 / 0.1440D4 + (
     #t47 * t124 * (t590 * t1915 - t1912) + 0.90D2 * t63 * t39 * (-t595 
     #* t1912 / 0.2D1 + t590 * t1914 - t1950 + t596 * t1915 / 0.6D1) - t
     #25 * t1957 - 0.180D3 * t54 * t124 * (-t595 * t1915 / 0.2D1 - t1914
     # + t590 * t1912)) * t140 / 0.1440D4 - (0.90D2 * t63 * t39 * (-t630
     # * t1915 + (t1912 - t624 * t1915) * t561 + t1912) + 0.180D3 * t54 
     #* t124 * t1936) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (t
     #1914 + t649 * t1915 / 0.2D1 - t647 * t1912) - 0.180D3 * t54 * t124
     # * (-t647 * t1915 + t1912) + t47 * t1957) * t199 * t140 / 0.720D3
      t2003 = FJET(XB1, XB2, s, -t524, t522, 0.0D0, 0.0D0, 0.0D0, t2002)
      t2005 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t2007 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t2008 = t958 * t2007
      t2013 = t969 * t2005
      t2020 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t2021 = t958 * t2020
      t2034 = t998 * t2013
      t2063 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t2081 = -(0.90D2 * t63 * t39 * (-t959 * t2005 + t2008) - 0.180D3 *
     # t968 * t2013) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (t2
     #021 - t981 * t2007 + t984 * t2005 / 0.2D1) - 0.180D3 * t54 * t124 
     #* (t2008 - t981 * t2005) + t2034) * t199 * t140 / 0.720D3 - (0.90D
     #2 * t63 * t39 * (-t1007 * t2007 + t1010 * t2005 / 0.2D1 + t2021) -
     # 0.180D3 * t54 * t124 * (t2008 - t1007 * t2005) + t2034) * t138 * 
     #t199 / 0.1440D4 - (t47 * t124 * (t2008 - t1029 * t2005) + 0.90D2 *
     # t63 * t39 * (t1035 * t2007 / 0.2D1 - t1041 * t2005 / 0.6D1 + t958
     # * t2063 - t1029 * t2020) + t1049 * t2013 - 0.180D3 * t54 * t124 *
     # (-t1029 * t2007 + t2021 + t1035 * t2005 / 0.2D1)) * t199 / 0.1440
     #D4
      t2082 = FJET(XB1, XB2, s, -t950, 0.0D0, t947, 0.0D0, 0.0D0, t2081)
      t2084 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t2086 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t2087 = t1093 * t2086
      t2092 = t1104 * t2084
      t2102 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t1095
     #, x4)
      t2118 = -(0.90D2 * t63 * t39 * (-t1094 * t2084 + t2087) - 0.180D3 
     #* t968 * t2092) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-
     #t1118 * t2086 + t1121 * t2084 / 0.2D1 + t1093 * t2102) - 0.180D3 *
     # t54 * t124 * (-t1118 * t2084 + t2087) + t998 * t2092) * t138 * t1
     #99 / 0.1440D4
      t2119 = FJET(XB1, XB2, s, -t1080, 0.0D0, t1075, 0.0D0, 0.0D0, t211
     #8)
      t2121 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t2122 = t1273 * t2121
      t2123 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t2124 = t1272 * t2123
      t2130 = t1289 * t2124
      t2138 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, t525, x2, 0.0D0, 
     #x4)
      t2156 = -(0.90D2 * t63 * t39 * (t2122 - t1281 * t2124) - 0.180D3 *
     # t968 * t2130) * t138 * t200 / 0.720D3 - (0.90D2 * t63 * t39 * (-t
     #1304 * t1272 * t2121 + t1273 * t2138 + t1308 * t2124 / 0.2D1) - 0.
     #180D3 * t54 * t124 * (t2122 - t1304 * t2124) + t998 * t2130) * t19
     #9 * t140 / 0.720D3
      t2157 = FJET(XB1, XB2, s, -t1261, 0.0D0, t1263, t522, -t1268, t215
     #6)
      t2159 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t2161 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t2162 = t1862 * t2161
      t2171 = 0.90D2 * t63 * t39 * (t1863 * t2159 - t1871 * t2162) - 0.1
     #80D3 * t968 * t1289 * t2162
      t2175 = FJET(XB1, XB2, s, -t1813, -t1819, t1798, t1809, -t1268, -t
     #2171 * t138 * t200 / 0.720D3)
      t2179 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t2181 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t525, x2, t1095, 
     #x4)
      t2182 = t1862 * t2181
      t2191 = 0.90D2 * t63 * t39 * (t1863 * t2179 - t1871 * t2182) - 0.1
     #80D3 * t968 * t1289 * t2182
      t2195 = FJET(XB1, XB2, s, -t1819, -t1813, t1809, t1798, -t1268, -t
     #2191 * t138 * t200 / 0.720D3)
      t2199 = t1642 * t1641 + t1721 * t1720 + t1758 * t1757 + t1796 * t1
     #795 - t1886 * t1882 * t1889 / 0.720D3 - t1908 * t1904 * t1889 / 0.
     #720D3 + t2003 * t2002 + t2082 * t2081 + t2119 * t2118 + t2157 * t2
     #156 - t2175 * t2171 * t1889 / 0.720D3 - t2195 * t2191 * t1889 / 0.
     #720D3
      rrgq2qght7s1e1 = t1550 + t2199

      end function



      doubleprecision function rrgq2qght7s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t9 = t7 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = t21 ** 2
      t23 = -0.1D1 + x3
      t24 = 0.1D1 / t23
      t25 = t18 * t24
      t28 = log(-0.4D1 * t13 * t25)
      t29 = t28 ** 2
      t30 = cos(t10)
      t32 = Sqrt(-x3 * t23)
      t36 = 0.1D1 / (-0.1D1 + 0.2D1 * t30 * t32 - x3)
      t39 = -t22 / 0.2D1 - t29 * t36 / 0.2D1
      t43 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t44 = t7 * t43
      t47 = 0.3141592653589793D1 * lh
      t48 = t3 * t7
      t49 = t48 * t8
      t51 = 0.180D3 * t47 * t49
      t54 = t21 + t28 * t36
      t59 = lh ** 2
      t61 = 0.3141592653589793D1 ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t64 = 0.3141592653589793D1 * t63
      t65 = t64 * t49
      t66 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t67 = t7 * t66
      t71 = -0.1D1 - t36
      t74 = 0.1D1 / x3
      t77 = t15 * t12
      t78 = t77 * t17
      t80 = log(0.4D1 * t78)
      t81 = t80 * 0.3141592653589793D1
      t84 = t80 ** 2
      t85 = t84 * 0.3141592653589793D1
      t88 = (0.180D3 * t81 * lh + 0.45D2 * t85 + t64) * t3
      t91 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t108 = (-0.90D2 * t85 * lh + 0.3141592653589793D1 * (0.60D2 * lh *
     # t61 - 0.2884936567583026D3 - 0.120D3 * t59 * lh) - 0.15D2 * t84 *
     # t80 * 0.3141592653589793D1 - t81 * t63) * t3
      t114 = (-0.180D3 * t47 - 0.90D2 * t81) * t3
      t117 = x2 ** 2
      t118 = x3 * t117
      t119 = t118 * t12
      t122 = log(-0.4D1 * t119 * t25)
      t128 = log(0.4D1 * t118 * t78)
      t135 = -t8 - t8 * t36
      t141 = 0.1D1 / x2
      t144 = t117 * t12
      t147 = log(0.4D1 * t144 * t18)
      t148 = t147 ** 2
      t164 = x1 ** 2
      t165 = x3 * t164
      t166 = t165 * t12
      t169 = log(-0.4D1 * t166 * t25)
      t175 = log(0.4D1 * t165 * t78)
      t187 = 0.1D1 / x1
      t190 = t4 * t7
      t192 = t141 * t187
      t196 = t117 * t164
      t199 = log(0.4D1 * t196 * t78)
      t209 = t164 * t12
      t212 = log(0.4D1 * t209 * t18)
      t213 = t212 ** 2
      t229 = -(0.90D2 * t4 * t9 * t39 + (0.90D2 * t4 * t44 - t51) * t54 
     #+ (-0.180D3 * t47 * t48 * t43 + t65 + 0.90D2 * t4 * t67) * t71) * 
     #t74 / 0.2880D4 + t88 * t44 / 0.2880D4 + t4 * t7 * t91 / 0.32D2 + t
     #108 * t9 / 0.2880D4 + t114 * t67 / 0.2880D4 - (0.90D2 * t4 * t7 * 
     #(-(t43 - t122 * t8) * t36 + t128 * t8 - t43) - 0.180D3 * t47 * t48
     # * t135) * t74 * t141 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t148 * t8
     # / 0.2D1 + t147 * t43 - t66) - 0.180D3 * t47 * t48 * (t147 * t8 - 
     #t43) - t65) * t141 / 0.1440D4 + (0.90D2 * t4 * t7 * ((t43 - t169 *
     # t8) * t36 + t43 - t175 * t8) + 0.180D3 * t47 * t48 * t135) * t74 
     #* t187 / 0.1440D4 - t190 * t135 * t74 * t192 / 0.8D1 - (0.90D2 * t
     #4 * t7 * (t199 * t8 - t43) + t51) * t141 * t187 / 0.720D3 + (0.90D
     #2 * t4 * t7 * (t66 + t213 * t8 / 0.2D1 - t212 * t43) - 0.180D3 * t
     #47 * t48 * (t43 - t212 * t8) + t65) * t187 / 0.1440D4
      t230 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t229)
      t232 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t233 = t7 * t232
      t237 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t238 = t7 * t237
      t241 = t48 * t232
      t243 = 0.180D3 * t47 * t241
      t249 = t64 * t241
      t250 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t251 = t7 * t250
      t261 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t278 = -t232 - t232 * t36
      t345 = -(0.90D2 * t4 * t233 * t39 + (0.90D2 * t4 * t238 - t243) * 
     #t54 + (-0.180D3 * t47 * t48 * t237 + t249 + 0.90D2 * t4 * t251) * 
     #t71) * t74 / 0.2880D4 + t88 * t238 / 0.2880D4 + t4 * t7 * t261 / 0
     #.32D2 + t108 * t233 / 0.2880D4 + t114 * t251 / 0.2880D4 - (0.90D2 
     #* t4 * t7 * (-(t237 - t122 * t232) * t36 + t128 * t232 - t237) - 0
     #.180D3 * t47 * t48 * t278) * t74 * t141 / 0.1440D4 - (0.90D2 * t4 
     #* t7 * (-t250 - t148 * t232 / 0.2D1 + t147 * t237) - 0.180D3 * t47
     # * t48 * (-t237 + t147 * t232) - t249) * t141 / 0.1440D4 + (0.90D2
     # * t4 * t7 * (t237 + (t237 - t169 * t232) * t36 - t175 * t232) + 0
     #.180D3 * t47 * t48 * t278) * t74 * t187 / 0.1440D4 - t190 * t278 *
     # t74 * t192 / 0.8D1 - (0.90D2 * t4 * t7 * (-t237 + t199 * t232) + 
     #t243) * t141 * t187 / 0.720D3 + (0.90D2 * t4 * t7 * (-t212 * t237 
     #+ t250 + t213 * t232 / 0.2D1) - 0.180D3 * t47 * t48 * (t237 - t212
     # * t232) + t249) * t187 / 0.1440D4
      t346 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t345)
      t348 = t2 * x1
      t349 = -0.1D1 + x1
      t350 = t2 * t349
      t351 = -t349
      t352 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D0
     #, x4)
      t354 = x1 * z
      t355 = 0.1D1 - x1 + t354
      t356 = 0.1D1 / t355
      t357 = t17 * t356
      t358 = t349 ** 2
      t363 = log(-0.4D1 * t165 * t77 * t357 * t358 * t24)
      t364 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D0
     #, x4)
      t367 = x3 * x1
      t368 = t367 * z
      t371 = x3 * t355
      t373 = Sqrt(-t371 * t23)
      t377 = 0.1D1 / (-0.2D1 * t368 + 0.2D1 * t367 - 0.1D1 - x3 + 0.2D1 
     #* t30 * t373)
      t380 = t18 * t356 * t358
      t383 = log(0.4D1 * t166 * t380)
      t390 = -t364 * t377 - t364
      t403 = t196 * t12
      t406 = log(0.4D1 * t403 * t380)
      t412 = t48 * t364
      t423 = log(0.4D1 * t209 * t15 * t357 * t358)
      t424 = t423 ** 2
      t427 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D0
     #, x4)
      t442 = (0.90D2 * t4 * t7 * (-t352 - (t352 - t363 * t364) * t377 + 
     #t383 * t364) - 0.180D3 * t47 * t48 * t390) * t74 * t187 / 0.1440D4
     # + t190 * t390 * t74 * t192 / 0.8D1 - (0.90D2 * t4 * t7 * (t352 - 
     #t406 * t364) - 0.180D3 * t47 * t412) * t141 * t187 / 0.720D3 + (0.
     #90D2 * t4 * t7 * (-t424 * t364 / 0.2D1 - t427 + t423 * t352) - 0.1
     #80D3 * t47 * t48 * (t423 * t364 - t352) - t64 * t412) * t187 / 0.1
     #440D4
      t443 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t348, -t350, 0.0D0, t442)
      t445 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D0
     #, x4)
      t446 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D0
     #, x4)
      t456 = -t446 * t377 - t446
      t474 = t48 * t446
      t482 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D0
     #, x4)
      t498 = (0.90D2 * t4 * t7 * (-(t445 - t363 * t446) * t377 + t383 * 
     #t446 - t445) - 0.180D3 * t47 * t48 * t456) * t74 * t187 / 0.1440D4
     # + t190 * t456 * t74 * t192 / 0.8D1 - (0.90D2 * t4 * t7 * (t445 - 
     #t406 * t446) - 0.180D3 * t47 * t474) * t141 * t187 / 0.720D3 + (0.
     #90D2 * t4 * t7 * (t423 * t445 - t482 - t424 * t446 / 0.2D1) - 0.18
     #0D3 * t47 * t48 * (-t445 + t423 * t446) - t64 * t474) * t187 / 0.1
     #440D4
      t499 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t350, t348, 0.0D0, t498)
      t501 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t502 = t7 * t501
      t506 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t507 = t7 * t506
      t510 = t48 * t501
      t512 = 0.180D3 * t47 * t510
      t518 = t64 * t510
      t519 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t520 = t7 * t519
      t530 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t547 = -t501 * t36 - t501
      t614 = -(0.90D2 * t4 * t502 * t39 + (0.90D2 * t4 * t507 - t512) * 
     #t54 + (-0.180D3 * t47 * t48 * t506 + t518 + 0.90D2 * t4 * t520) * 
     #t71) * t74 / 0.2880D4 + t88 * t507 / 0.2880D4 + t4 * t7 * t530 / 0
     #.32D2 + t108 * t502 / 0.2880D4 + t114 * t520 / 0.2880D4 - (0.90D2 
     #* t4 * t7 * (-(t506 - t122 * t501) * t36 - t506 + t128 * t501) - 0
     #.180D3 * t47 * t48 * t547) * t74 * t141 / 0.1440D4 - (0.90D2 * t4 
     #* t7 * (t147 * t506 - t148 * t501 / 0.2D1 - t519) - 0.180D3 * t47 
     #* t48 * (t147 * t501 - t506) - t518) * t141 / 0.1440D4 + (0.90D2 *
     # t4 * t7 * (-t175 * t501 + t506 + (t506 - t169 * t501) * t36) + 0.
     #180D3 * t47 * t48 * t547) * t74 * t187 / 0.1440D4 - t190 * t547 * 
     #t74 * t192 / 0.8D1 - (0.90D2 * t4 * t7 * (t199 * t501 - t506) + t5
     #12) * t141 * t187 / 0.720D3 + (0.90D2 * t4 * t7 * (-t212 * t506 + 
     #t519 + t213 * t501 / 0.2D1) - 0.180D3 * t47 * t48 * (t506 - t212 *
     # t501) + t518) * t187 / 0.1440D4
      t615 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t614)
      t618 = x2 * s * t1
      t619 = -0.1D1 + x2
      t620 = t619 * s
      t621 = t620 * t1
      t622 = x2 * z
      t624 = 0.1D1 / (0.1D1 + t622 - x2)
      t625 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t626 = t624 * t625
      t627 = t18 * t619
      t630 = log(-0.4D1 * t119 * t627)
      t631 = t630 * t624
      t632 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t638 = t47 * t3
      t639 = t7 * t624
      t640 = t639 * t632
      t642 = 0.180D3 * t638 * t640
      t647 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t651 = log(-0.4D1 * t144 * t627)
      t652 = t651 * t624
      t654 = t651 ** 2
      t655 = t654 * t624
      t667 = t64 * t3
      t672 = t4 * t639
      t679 = log(-0.4D1 * t403 * t627)
      t680 = t679 * t624
      t690 = -(0.90D2 * t4 * t7 * (t626 - t631 * t632) - t642) * t74 * t
     #141 / 0.1440D4 - (0.90D2 * t4 * t7 * (t624 * t647 - t652 * t625 + 
     #t655 * t632 / 0.2D1) - 0.180D3 * t47 * t48 * (t626 - t652 * t632) 
     #+ t667 * t640) * t141 / 0.1440D4 - t672 * t632 * t74 * t192 / 0.8D
     #1 - (0.90D2 * t4 * t7 * (t626 - t680 * t632) - t642) * t141 * t187
     # / 0.720D3
      t691 = FJET(XB1, XB2, s, 0.0D0, t618, 0.0D0, -t621, 0.0D0, t690)
      t693 = x2 * x3
      t696 = Sqrt(x3 * t619 * t23)
      t697 = t30 * t696
      t699 = 0.2D1 * t697 * x2
      t701 = 0.1D1 - x3 + t693
      t702 = 0.1D1 / t701
      t704 = t2 * (0.1D1 - x3 - x2 + t693 + t118 + t699) * t702
      t705 = 0.2D1 * t697
      t709 = t2 * x2 * (-0.1D1 + t693 + t705) * t702
      t712 = t701 ** 2
      t718 = log(0.4D1 * t118 * t77 * t17 * t619 * t23 / t712)
      t719 = t118 * z
      t723 = 0.1D1 / (-0.1D1 - t622 - t118 + t693 - t699 + t705 + t719 +
     # x2 - x3 + 0.2D1 * t697 * t622)
      t724 = t718 * t723
      t725 = t693 * t702
      t726 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725, 
     #x4)
      t728 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725, 
     #x4)
      t734 = t7 * t723
      t742 = t4 * t734
      t747 = -(0.90D2 * t4 * t7 * (-t724 * t726 + t723 * t728) - 0.180D3
     # * t638 * t734 * t726) * t74 * t141 / 0.1440D4 - t742 * t726 * t74
     # * t192 / 0.8D1
      t748 = FJET(XB1, XB2, s, 0.0D0, t704, 0.0D0, -t709, 0.0D0, t747)
      t750 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t751 = t624 * t750
      t752 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t758 = t639 * t752
      t760 = 0.180D3 * t638 * t758
      t765 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t796 = -(0.90D2 * t4 * t7 * (t751 - t631 * t752) - t760) * t74 * t
     #141 / 0.1440D4 - (0.90D2 * t4 * t7 * (t624 * t765 - t652 * t750 + 
     #t655 * t752 / 0.2D1) - 0.180D3 * t47 * t48 * (t751 - t652 * t752) 
     #+ t667 * t758) * t141 / 0.1440D4 - t672 * t752 * t74 * t192 / 0.8D
     #1 - (0.90D2 * t4 * t7 * (t751 - t680 * t752) - t760) * t141 * t187
     # / 0.720D3
      t797 = FJET(XB1, XB2, s, 0.0D0, -t621, 0.0D0, t618, 0.0D0, t796)
      t799 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725, 
     #x4)
      t801 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725, 
     #x4)
      t818 = -(0.90D2 * t4 * t7 * (t723 * t799 - t724 * t801) - 0.180D3 
     #* t638 * t734 * t801) * t74 * t141 / 0.1440D4 - t742 * t801 * t74 
     #* t192 / 0.8D1
      t819 = FJET(XB1, XB2, s, 0.0D0, -t709, 0.0D0, t704, 0.0D0, t818)
      t823 = t2 * t349 * x2 * t356
      t825 = t620 * t1 * t349
      t830 = s * t16 * x2 * x1 * t349 * t356
      t831 = t7 * t355
      t832 = t4 * t831
      t833 = x2 * x1
      t834 = t833 * z
      t836 = 0.1D1 / (-0.1D1 - t622 - t833 + t834 - t354 + x2 + x1)
      t837 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, x
     #4)
      t838 = t836 * t837
      t840 = t74 * t141 * t187
      t849 = log(-0.4D1 * t196 * t77 * t357 * t358 * t619)
      t850 = t849 * t355
      t852 = t355 * t836
      t853 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, x
     #4)
      t866 = -t832 * t838 * t840 / 0.8D1 - (0.90D2 * t4 * t7 * (-t850 * 
     #t838 + t852 * t853) - 0.180D3 * t638 * t831 * t838) * t141 * t187 
     #/ 0.720D3
      t867 = FJET(XB1, XB2, s, 0.0D0, -t823, t348, t825, -t830, t866)
      t869 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t870 = t7 * t869
      t874 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t875 = t7 * t874
      t878 = t48 * t869
      t880 = 0.180D3 * t47 * t878
      t886 = t64 * t878
      t887 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t888 = t7 * t887
      t896 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t913 = -t869 - t869 * t36
      t982 = -(0.90D2 * t4 * t870 * t39 + (0.90D2 * t4 * t875 - t880) * 
     #t54 + (-0.180D3 * t47 * t48 * t874 + t886 + 0.90D2 * t4 * t888) * 
     #t71) * t74 / 0.2880D4 + t4 * t7 * t896 / 0.32D2 + t114 * t888 / 0.
     #2880D4 + t88 * t875 / 0.2880D4 - (0.90D2 * t4 * t7 * (-(t874 - t12
     #2 * t869) * t36 + t128 * t869 - t874) - 0.180D3 * t47 * t48 * t913
     #) * t74 * t141 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t887 - t148 * t8
     #69 / 0.2D1 + t147 * t874) - 0.180D3 * t47 * t48 * (-t874 + t147 * 
     #t869) - t886) * t141 / 0.1440D4 + t108 * t870 / 0.2880D4 + (0.90D2
     # * t4 * t7 * ((t874 - t169 * t869) * t36 - t175 * t869 + t874) + 0
     #.180D3 * t47 * t48 * t913) * t74 * t187 / 0.1440D4 - t190 * t913 *
     # t74 * t192 / 0.8D1 - (0.90D2 * t4 * t7 * (t199 * t869 - t874) + t
     #880) * t141 * t187 / 0.720D3 + (0.90D2 * t4 * t7 * (-t212 * t874 +
     # t887 + t213 * t869 / 0.2D1) - 0.180D3 * t47 * t48 * (-t212 * t869
     # + t874) + t886) * t187 / 0.1440D4
      t983 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t982)
      t985 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, x
     #4)
      t986 = t836 * t985
      t990 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, x
     #4)
      t1004 = -t832 * t986 * t840 / 0.8D1 - (0.90D2 * t4 * t7 * (t852 * 
     #t990 - t850 * t986) - 0.180D3 * t638 * t831 * t986) * t141 * t187 
     #/ 0.720D3
      t1005 = FJET(XB1, XB2, s, t348, t825, 0.0D0, -t823, -t830, t1004)
      t1007 = t230 * t229 + t346 * t345 + t443 * t442 + t499 * t498 + t6
     #15 * t614 + t691 * t690 + t748 * t747 + t797 * t796 + t819 * t818 
     #+ t867 * t866 + t983 * t982 + t1005 * t1004
      t1008 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D
     #0, x4)
      t1009 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D
     #0, x4)
      t1019 = -t1009 * t377 - t1009
      t1037 = t48 * t1009
      t1046 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D
     #0, x4)
      t1061 = (0.90D2 * t4 * t7 * (-(t1008 - t363 * t1009) * t377 + t383
     # * t1009 - t1008) - 0.180D3 * t47 * t48 * t1019) * t74 * t187 / 0.
     #1440D4 + t190 * t1019 * t74 * t192 / 0.8D1 - (0.90D2 * t4 * t7 * (
     #-t406 * t1009 + t1008) - 0.180D3 * t47 * t1037) * t141 * t187 / 0.
     #720D3 + (0.90D2 * t4 * t7 * (-t424 * t1009 / 0.2D1 - t1046 + t423 
     #* t1008) - 0.180D3 * t47 * t48 * (-t1008 + t423 * t1009) - t64 * t
     #1037) * t187 / 0.1440D4
      t1062 = FJET(XB1, XB2, s, t348, -t350, 0.0D0, 0.0D0, 0.0D0, t1061)
      t1064 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1066 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1067 = t624 * t1066
      t1072 = t639 * t1064
      t1074 = 0.180D3 * t638 * t1072
      t1080 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1110 = -(0.90D2 * t4 * t7 * (-t631 * t1064 + t1067) - t1074) * t7
     #4 * t141 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t652 * t1066 + t624 * 
     #t1080 + t655 * t1064 / 0.2D1) - 0.180D3 * t47 * t48 * (t1067 - t65
     #2 * t1064) + t667 * t1072) * t141 / 0.1440D4 - t672 * t1064 * t74 
     #* t192 / 0.8D1 - (0.90D2 * t4 * t7 * (t1067 - t680 * t1064) - t107
     #4) * t141 * t187 / 0.720D3
      t1111 = FJET(XB1, XB2, s, t618, 0.0D0, -t621, 0.0D0, 0.0D0, t1110)
      t1113 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725,
     # x4)
      t1115 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725,
     # x4)
      t1132 = -(0.90D2 * t4 * t7 * (-t724 * t1113 + t723 * t1115) - 0.18
     #0D3 * t638 * t734 * t1113) * t74 * t141 / 0.1440D4 - t742 * t1113 
     #* t74 * t192 / 0.8D1
      t1133 = FJET(XB1, XB2, s, t704, 0.0D0, -t709, 0.0D0, 0.0D0, t1132)
      t1135 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, 
     #x4)
      t1136 = t836 * t1135
      t1140 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, 
     #x4)
      t1154 = -t832 * t1136 * t840 / 0.8D1 - (0.90D2 * t4 * t7 * (t852 *
     # t1140 - t850 * t1136) - 0.180D3 * t638 * t831 * t1136) * t141 * t
     #187 / 0.720D3
      t1155 = FJET(XB1, XB2, s, t825, t348, -t823, 0.0D0, -t830, t1154)
      t1157 = t348 * t725
      t1158 = t693 * x1
      t1159 = t693 * t354
      t1162 = Sqrt(t371 * t619 * t23)
      t1163 = t30 * t1162
      t1164 = 0.2D1 * t1163
      t1169 = t350 * x2 * (t367 - t368 + t693 - t1158 + t1159 - 0.1D1 + 
     #t1164) * t356 * t702
      t1173 = t23 * s * t1 * x1 * t702
      t1175 = 0.2D1 * t1163 * x2
      t1176 = 0.1D1 - x1 + t354 - x2 + t833 - t834 - x3 + t367 - t368 + 
     #t693 - t1158 + t1159 + t118 + t1175
      t1179 = t350 * t1176 * t356 * t702
      t1204 = 0.1D1 - 0.4D1 * t1159 - t118 * t14 * x1 - 0.2D1 * t118 * t
     #164 * z + t118 * t164 * t14 - 0.2D1 * t1163 * t833 - 0.2D1 * t1163
     # * t622 + x3 * t14 * t833 + 0.4D1 * t165 * t622 - 0.2D1 * t165 * t
     #14 * x2 + 0.3D1 * t118 * t354 - 0.2D1 * t1163 * t354 - t834 - t719
     # + x3 - t1164 + 0.2D1 * t165
      t1221 = -x2 - x1 + 0.3D1 * t368 - t693 + t118 + t622 + t354 - 0.3D
     #1 * t367 + 0.2D1 * t1163 * t834 + 0.3D1 * t1158 + t1175 - 0.4D1 * 
     #t165 * z - 0.2D1 * t165 * x2 + 0.2D1 * t165 * t14 - 0.2D1 * t118 *
     # x1 + t118 * t164 + 0.2D1 * t1163 * x1 + t833
      t1223 = 0.1D1 / (t1204 + t1221)
      t1224 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, t725, x
     #4)
      t1226 = t1223 * t1224 * t840
      t1229 = FJET(XB1, XB2, s, t1157, t1169, -t1173, -t1179, -t830, -t8
     #32 * t1226 / 0.8D1)
      t1231 = t48 * t355
      t1235 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, t725, x
     #4)
      t1237 = t1223 * t1235 * t840
      t1240 = FJET(XB1, XB2, s, t1169, t1157, -t1179, -t1173, -t830, -t8
     #32 * t1237 / 0.8D1)
      t1245 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D
     #0, x4)
      t1246 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D
     #0, x4)
      t1256 = -t1246 * t377 - t1246
      t1274 = t48 * t1246
      t1283 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, t351, 0.0D0, 0.0D
     #0, x4)
      t1298 = (0.90D2 * t4 * t7 * (-(t1245 - t363 * t1246) * t377 + t383
     # * t1246 - t1245) - 0.180D3 * t47 * t48 * t1256) * t74 * t187 / 0.
     #1440D4 + t190 * t1256 * t74 * t192 / 0.8D1 - (0.90D2 * t4 * t7 * (
     #-t406 * t1246 + t1245) - 0.180D3 * t47 * t1274) * t141 * t187 / 0.
     #720D3 + (0.90D2 * t4 * t7 * (-t424 * t1246 / 0.2D1 - t1283 + t423 
     #* t1245) - 0.180D3 * t47 * t48 * (t423 * t1246 - t1245) - t64 * t1
     #274) * t187 / 0.1440D4
      t1299 = FJET(XB1, XB2, s, -t350, t348, 0.0D0, 0.0D0, 0.0D0, t1298)
      t1301 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1302 = t624 * t1301
      t1303 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1309 = t639 * t1303
      t1311 = 0.180D3 * t638 * t1309
      t1317 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1347 = -(0.90D2 * t4 * t7 * (t1302 - t631 * t1303) - t1311) * t74
     # * t141 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t652 * t1301 + t624 * t
     #1317 + t655 * t1303 / 0.2D1) - 0.180D3 * t47 * t48 * (t1302 - t652
     # * t1303) + t667 * t1309) * t141 / 0.1440D4 - t672 * t1303 * t74 *
     # t192 / 0.8D1 - (0.90D2 * t4 * t7 * (t1302 - t680 * t1303) - t1311
     #) * t141 * t187 / 0.720D3
      t1348 = FJET(XB1, XB2, s, -t621, 0.0D0, t618, 0.0D0, 0.0D0, t1347)
      t1350 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725,
     # x4)
      t1352 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t725,
     # x4)
      t1369 = -(0.90D2 * t4 * t7 * (-t724 * t1350 + t723 * t1352) - 0.18
     #0D3 * t638 * t734 * t1350) * t74 * t141 / 0.1440D4 - t742 * t1350 
     #* t74 * t192 / 0.8D1
      t1370 = FJET(XB1, XB2, s, -t709, 0.0D0, t704, 0.0D0, 0.0D0, t1369)
      t1372 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, 
     #x4)
      t1373 = t836 * t1372
      t1377 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, t351, x2, 0.0D0, 
     #x4)
      t1391 = -t832 * t1373 * t840 / 0.8D1 - (0.90D2 * t4 * t7 * (t852 *
     # t1377 - t850 * t1373) - 0.180D3 * t638 * t831 * t1373) * t141 * t
     #187 / 0.720D3
      t1392 = FJET(XB1, XB2, s, -t823, 0.0D0, t825, t348, -t830, t1391)
      t1394 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, t725, x
     #4)
      t1396 = t1223 * t1394 * t840
      t1399 = FJET(XB1, XB2, s, -t1173, -t1179, t1157, t1169, -t830, -t8
     #32 * t1396 / 0.8D1)
      t1404 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t351, x2, t725, x
     #4)
      t1406 = t1223 * t1404 * t840
      t1409 = FJET(XB1, XB2, s, -t1179, -t1173, t1169, t1157, -t830, -t8
     #32 * t1406 / 0.8D1)
      t1414 = t1062 * t1061 + t1111 * t1110 + t1133 * t1132 + t1155 * t1
     #154 - t1229 * 0.3141592653589793D1 * t1231 * t1226 / 0.8D1 - t1240
     # * 0.3141592653589793D1 * t1231 * t1237 / 0.8D1 + t1299 * t1298 + 
     #t1348 * t1347 + t1370 * t1369 + t1392 * t1391 - t1399 * 0.31415926
     #53589793D1 * t1231 * t1396 / 0.8D1 - t1409 * 0.3141592653589793D1 
     #* t1231 * t1406 / 0.8D1
      rrgq2qght7s1e0 = t1007 + t1414

      end function



      doubleprecision function rrgq2qght7s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t9 = t7 * t8
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = -0.1D1 + x3
      t27 = log(-0.4D1 * t13 * t18 / t22)
      t28 = cos(t10)
      t30 = Sqrt(-x3 * t22)
      t34 = 0.1D1 / (-0.1D1 + 0.2D1 * t28 * t30 - x3)
      t36 = t21 + t27 * t34
      t40 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t41 = t7 * t40
      t44 = 0.3141592653589793D1 * lh
      t45 = t3 * t7
      t48 = 0.180D3 * t44 * t45 * t8
      t50 = -0.1D1 - t34
      t53 = 0.1D1 / x3
      t60 = log(0.4D1 * t15 * t12 * t17)
      t61 = t60 * 0.3141592653589793D1
      t64 = (-0.180D3 * t44 - 0.90D2 * t61) * t3
      t69 = t60 ** 2
      t72 = lh ** 2
      t74 = 0.3141592653589793D1 ** 2
      t79 = (0.180D3 * lh * t61 + 0.45D2 * t69 * 0.3141592653589793D1 + 
     #0.3141592653589793D1 * (0.180D3 * t72 - 0.30D2 * t74)) * t3
      t82 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t86 = t4 * t7
      t88 = -t8 - t8 * t34
      t90 = 0.1D1 / x2
      t94 = x2 ** 2
      t95 = t94 * t12
      t98 = log(0.4D1 * t95 * t18)
      t108 = 0.1D1 / x1
      t112 = x1 ** 2
      t113 = t112 * t12
      t116 = log(0.4D1 * t113 * t18)
      t130 = -(0.90D2 * t4 * t9 * t36 + (0.90D2 * t4 * t41 - t48) * t50)
     # * t53 / 0.2880D4 + t64 * t41 / 0.2880D4 + t79 * t9 / 0.2880D4 + t
     #4 * t7 * t82 / 0.32D2 - t86 * t88 * t53 * t90 / 0.16D2 - (0.90D2 *
     # t4 * t7 * (t98 * t8 - t40) + t48) * t90 / 0.1440D4 + t86 * t8 * t
     #90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (t40 - t116 * t8) - t48) *
     # t108 / 0.1440D4 - t86 * t88 * t53 * t108 / 0.16D2
      t131 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t130)
      t133 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t134 = t7 * t133
      t138 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t139 = t7 * t138
      t144 = 0.180D3 * t44 * t45 * t133
      t154 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t159 = -t133 - t133 * t34
      t189 = -(0.90D2 * t4 * t134 * t36 + (0.90D2 * t4 * t139 - t144) * 
     #t50) * t53 / 0.2880D4 + t64 * t139 / 0.2880D4 + t79 * t134 / 0.288
     #0D4 + t4 * t7 * t154 / 0.32D2 - t86 * t159 * t53 * t90 / 0.16D2 - 
     #(0.90D2 * t4 * t7 * (-t138 + t98 * t133) + t144) * t90 / 0.1440D4 
     #+ t86 * t133 * t90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (t138 - t1
     #16 * t133) - t144) * t108 / 0.1440D4 - t86 * t159 * t53 * t108 / 0
     #.16D2
      t190 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t189)
      t192 = t2 * x1
      t193 = -0.1D1 + x1
      t194 = t2 * t193
      t195 = -t193
      t196 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t202 = x1 * z
      t203 = 0.1D1 - x1 + t202
      t204 = 0.1D1 / t203
      t206 = t193 ** 2
      t210 = log(0.4D1 * t113 * t15 * t17 * t204 * t206)
      t212 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t223 = x3 * x1
      t229 = Sqrt(-x3 * t203 * t22)
      t233 = 0.1D1 / (-0.2D1 * t223 * z + 0.2D1 * t223 - 0.1D1 - x3 + 0.
     #2D1 * t28 * t229)
      t240 = -t86 * t196 * t90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (t21
     #0 * t196 - t212) + 0.180D3 * t44 * t45 * t196) * t108 / 0.1440D4 +
     # t86 * (-t196 * t233 - t196) * t53 * t108 / 0.16D2
      t241 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t192, -t194, 0.0D0, t240)
      t243 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t248 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t266 = -t86 * t243 * t90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (-t2
     #48 + t210 * t243) + 0.180D3 * t44 * t45 * t243) * t108 / 0.1440D4 
     #+ t86 * (-t243 * t233 - t243) * t53 * t108 / 0.16D2
      t267 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t194, t192, 0.0D0, t266)
      t269 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t270 = t7 * t269
      t274 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t275 = t7 * t274
      t280 = 0.180D3 * t44 * t45 * t269
      t290 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t295 = -t269 * t34 - t269
      t325 = -(0.90D2 * t4 * t270 * t36 + (0.90D2 * t4 * t275 - t280) * 
     #t50) * t53 / 0.2880D4 + t64 * t275 / 0.2880D4 + t79 * t270 / 0.288
     #0D4 + t4 * t7 * t290 / 0.32D2 - t86 * t295 * t53 * t90 / 0.16D2 - 
     #(0.90D2 * t4 * t7 * (t98 * t269 - t274) + t280) * t90 / 0.1440D4 +
     # t86 * t269 * t90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (t274 - t11
     #6 * t269) - t280) * t108 / 0.1440D4 - t86 * t295 * t53 * t108 / 0.
     #16D2
      t326 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t325)
      t329 = x2 * s * t1
      t330 = -0.1D1 + x2
      t331 = t330 * s
      t332 = t331 * t1
      t333 = x2 * z
      t335 = 0.1D1 / (0.1D1 + t333 - x2)
      t336 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t337 = t335 * t336
      t338 = t53 * t90
      t342 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t347 = log(-0.4D1 * t95 * t18 * t330)
      t348 = t347 * t335
      t354 = t44 * t3
      t355 = t7 * t335
      t362 = t90 * t108
      t366 = -t86 * t337 * t338 / 0.16D2 - (0.90D2 * t4 * t7 * (t335 * t
     #342 - t348 * t336) - 0.180D3 * t354 * t355 * t336) * t90 / 0.1440D
     #4 - t86 * t337 * t362 / 0.8D1
      t367 = FJET(XB1, XB2, s, 0.0D0, t329, 0.0D0, -t332, 0.0D0, t366)
      t369 = x2 * x3
      t370 = t94 * x3
      t373 = Sqrt(x3 * t330 * t22)
      t374 = t28 * t373
      t376 = 0.2D1 * t374 * x2
      t379 = 0.1D1 / (0.1D1 - x3 + t369)
      t381 = t2 * (0.1D1 - x3 - x2 + t369 + t370 + t376) * t379
      t382 = 0.2D1 * t374
      t386 = t2 * x2 * (-0.1D1 + t369 + t382) * t379
      t391 = 0.1D1 / (-0.1D1 - t333 - t370 + t369 - t376 + t382 + t370 *
     # z + x2 - x3 + 0.2D1 * t374 * t333)
      t392 = t369 * t379
      t393 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t392, 
     #x4)
      t395 = t391 * t393 * t338
      t398 = FJET(XB1, XB2, s, 0.0D0, t381, 0.0D0, -t386, 0.0D0, -t86 * 
     #t395 / 0.16D2)
      t403 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t404 = t335 * t403
      t408 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t424 = -t86 * t404 * t338 / 0.16D2 - (0.90D2 * t4 * t7 * (t335 * t
     #408 - t348 * t403) - 0.180D3 * t354 * t355 * t403) * t90 / 0.1440D
     #4 - t86 * t404 * t362 / 0.8D1
      t425 = FJET(XB1, XB2, s, 0.0D0, -t332, 0.0D0, t329, 0.0D0, t424)
      t427 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t392, 
     #x4)
      t429 = t391 * t427 * t338
      t432 = FJET(XB1, XB2, s, 0.0D0, -t386, 0.0D0, t381, 0.0D0, -t86 * 
     #t429 / 0.16D2)
      t439 = t2 * t193 * x2 * t204
      t441 = t331 * t1 * t193
      t446 = s * t16 * x2 * x1 * t193 * t204
      t448 = t4 * t7 * t203
      t449 = x2 * x1
      t452 = 0.1D1 / (-0.1D1 - t333 - t449 + t449 * z - t202 + x2 + x1)
      t453 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t195, x2, 0.0D0, x
     #4)
      t458 = FJET(XB1, XB2, s, 0.0D0, -t439, t192, t441, -t446, -t448 * 
     #t452 * t453 * t362 / 0.8D1)
      t461 = t203 * t452
      t467 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t468 = t7 * t467
      t471 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t475 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t476 = t7 * t475
      t484 = 0.180D3 * t44 * t45 * t475
      t491 = -t475 - t475 * t34
      t523 = t64 * t468 / 0.2880D4 + t4 * t7 * t471 / 0.32D2 - (0.90D2 *
     # t4 * t476 * t36 + (0.90D2 * t4 * t468 - t484) * t50) * t53 / 0.28
     #80D4 - t86 * t491 * t53 * t90 / 0.16D2 - (0.90D2 * t4 * t7 * (-t46
     #7 + t98 * t475) + t484) * t90 / 0.1440D4 + t79 * t476 / 0.2880D4 +
     # t86 * t475 * t90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (-t116 * t4
     #75 + t467) - t484) * t108 / 0.1440D4 - t86 * t491 * t53 * t108 / 0
     #.16D2
      t524 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t523)
      t526 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t195, x2, 0.0D0, x
     #4)
      t531 = FJET(XB1, XB2, s, t192, t441, 0.0D0, -t439, -t446, -t448 * 
     #t452 * t526 * t362 / 0.8D1)
      t539 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t544 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t562 = -t86 * t539 * t90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (-t5
     #44 + t210 * t539) + 0.180D3 * t44 * t45 * t539) * t108 / 0.1440D4 
     #+ t86 * (-t539 * t233 - t539) * t53 * t108 / 0.16D2
      t563 = FJET(XB1, XB2, s, t192, -t194, 0.0D0, 0.0D0, 0.0D0, t562)
      t565 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t566 = t335 * t565
      t570 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t586 = -t86 * t566 * t338 / 0.16D2 - (0.90D2 * t4 * t7 * (t335 * t
     #570 - t348 * t565) - 0.180D3 * t354 * t355 * t565) * t90 / 0.1440D
     #4 - t86 * t566 * t362 / 0.8D1
      t587 = FJET(XB1, XB2, s, t329, 0.0D0, -t332, 0.0D0, 0.0D0, t586)
      t589 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t392, 
     #x4)
      t591 = t391 * t589 * t338
      t594 = FJET(XB1, XB2, s, t381, 0.0D0, -t386, 0.0D0, 0.0D0, -t86 * 
     #t591 / 0.16D2)
      t599 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t195, x2, 0.0D0, x
     #4)
      t604 = FJET(XB1, XB2, s, t441, t192, -t439, 0.0D0, -t446, -t448 * 
     #t452 * t599 * t362 / 0.8D1)
      t612 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t618 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, t195, 0.0D0, 0.0D0
     #, x4)
      t635 = -t86 * t612 * t90 * t108 / 0.8D1 + (0.90D2 * t4 * t7 * (t21
     #0 * t612 - t618) + 0.180D3 * t44 * t45 * t612) * t108 / 0.1440D4 +
     # t86 * (-t612 * t233 - t612) * t53 * t108 / 0.16D2
      t636 = FJET(XB1, XB2, s, -t194, t192, 0.0D0, 0.0D0, 0.0D0, t635)
      t638 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t639 = t335 * t638
      t643 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t659 = -t86 * t639 * t338 / 0.16D2 - (0.90D2 * t4 * t7 * (t335 * t
     #643 - t348 * t638) - 0.180D3 * t354 * t355 * t638) * t90 / 0.1440D
     #4 - t86 * t639 * t362 / 0.8D1
      t660 = FJET(XB1, XB2, s, -t332, 0.0D0, t329, 0.0D0, 0.0D0, t659)
      t662 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, t392, 
     #x4)
      t664 = t391 * t662 * t338
      t667 = FJET(XB1, XB2, s, -t386, 0.0D0, t381, 0.0D0, 0.0D0, -t86 * 
     #t664 / 0.16D2)
      t672 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t195, x2, 0.0D0, x
     #4)
      t677 = FJET(XB1, XB2, s, -t439, 0.0D0, t441, t192, -t446, -t448 * 
     #t452 * t672 * t362 / 0.8D1)
      rrgq2qght7s1em1 = t131 * t130 + t190 * t189 + t241 * t240 + t267 *
     # t266 + t326 * t325 + t367 * t366 - t398 * 0.3141592653589793D1 * 
     #t45 * t395 / 0.16D2 + t425 * t424 - t432 * 0.3141592653589793D1 * 
     #t45 * t429 / 0.16D2 - t458 * 0.3141592653589793D1 * t45 * t461 * t
     #453 * t90 * t108 / 0.8D1 + t524 * t523 - t531 * 0.3141592653589793
     #D1 * t45 * t461 * t526 * t90 * t108 / 0.8D1 + t563 * t562 + t587 *
     # t586 - t594 * 0.3141592653589793D1 * t45 * t591 / 0.16D2 - t604 *
     # 0.3141592653589793D1 * t45 * t461 * t599 * t90 * t108 / 0.8D1 + t
     #636 * t635 + t660 * t659 - t667 * 0.3141592653589793D1 * t45 * t66
     #4 / 0.16D2 - t677 * 0.3141592653589793D1 * t45 * t461 * t672 * t90
     # * t108 / 0.8D1

      end function



      doubleprecision function rrgq2qght7s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t10 = x4 * 0.3141592653589793D1
      t11 = cos(t10)
      t14 = Sqrt(-x3 * (-0.1D1 + x3))
      t19 = -0.1D1 - 0.1D1 / (-0.1D1 + 0.2D1 * t11 * t14 - x3)
      t21 = 0.1D1 / x3
      t25 = t7 * t9
      t26 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t34 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t40 = z ** 2
      t42 = Sin(t10)
      t43 = t42 ** 2
      t45 = t1 ** 2
      t46 = t45 ** 2
      t49 = log(0.4D1 / t40 * t43 * t46)
      t53 = (-0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t49 * 0.314
     #1592653589793D1) * t3
      t56 = -t8 * t9 * t19 * t21 / 0.32D2 + t4 * t25 * t26 / 0.16D2 + t4
     # * t25 * t30 / 0.16D2 + t4 * t7 * t34 / 0.32D2 + t53 * t25 / 0.288
     #0D4
      t57 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t56)
      t59 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t64 = t7 * t59
      t71 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t77 = -t8 * t59 * t19 * t21 / 0.32D2 + t4 * t64 * t26 / 0.16D2 + t
     #4 * t64 * t30 / 0.16D2 + t4 * t7 * t71 / 0.32D2 + t53 * t64 / 0.28
     #80D4
      t78 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t77)
      t80 = t2 * x1
      t81 = -0.1D1 + x1
      t82 = t2 * t81
      t83 = -t81
      t84 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, t83, 0.0D0, 0.0D0, 
     #x4)
      t86 = t7 * t84 * t30
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t80, -t82, 0.0D0, -t4 * t86 
     #/ 0.16D2)
      t94 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, t83, 0.0D0, 0.0D0, 
     #x4)
      t96 = t7 * t94 * t30
      t99 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t82, t80, 0.0D0, -t4 * t96 
     #/ 0.16D2)
      t104 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t109 = t7 * t104
      t116 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t122 = -t8 * t104 * t19 * t21 / 0.32D2 + t4 * t109 * t26 / 0.16D2 
     #+ t4 * t109 * t30 / 0.16D2 + t4 * t7 * t116 / 0.32D2 + t53 * t109 
     #/ 0.2880D4
      t123 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t122)
      t126 = x2 * s * t1
      t129 = (-0.1D1 + x2) * s * t1
      t132 = 0.1D1 / (0.1D1 + x2 * z - x2)
      t133 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t138 = FJET(XB1, XB2, s, 0.0D0, t126, 0.0D0, -t129, 0.0D0, -t8 * t
     #132 * t133 * t26 / 0.16D2)
      t141 = t7 * t132
      t146 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t151 = FJET(XB1, XB2, s, 0.0D0, -t129, 0.0D0, t126, 0.0D0, -t8 * t
     #132 * t146 * t26 / 0.16D2)
      t158 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t163 = t7 * t158
      t170 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t176 = -t8 * t158 * t19 * t21 / 0.32D2 + t4 * t163 * t26 / 0.16D2 
     #+ t4 * t163 * t30 / 0.16D2 + t4 * t7 * t170 / 0.32D2 + t53 * t163 
     #/ 0.2880D4
      t177 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t176)
      t179 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, t83, 0.0D0, 0.0D0,
     # x4)
      t181 = t7 * t179 * t30
      t184 = FJET(XB1, XB2, s, t80, -t82, 0.0D0, 0.0D0, 0.0D0, -t4 * t18
     #1 / 0.16D2)
      t189 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t194 = FJET(XB1, XB2, s, t126, 0.0D0, -t129, 0.0D0, 0.0D0, -t8 * t
     #132 * t189 * t26 / 0.16D2)
      t201 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t206 = FJET(XB1, XB2, s, -t129, 0.0D0, t126, 0.0D0, 0.0D0, -t8 * t
     #132 * t201 * t26 / 0.16D2)
      t213 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, t83, 0.0D0, 0.0D0,
     # x4)
      t215 = t7 * t213 * t30
      t218 = FJET(XB1, XB2, s, -t82, t80, 0.0D0, 0.0D0, 0.0D0, -t4 * t21
     #5 / 0.16D2)
      rrgq2qght7s1em2 = t57 * t56 + t78 * t77 - t89 * 0.3141592653589793
     #D1 * t3 * t86 / 0.16D2 - t99 * 0.3141592653589793D1 * t3 * t96 / 0
     #.16D2 + t123 * t122 - t138 * 0.3141592653589793D1 * t3 * t141 * t1
     #33 * t26 / 0.16D2 - t151 * 0.3141592653589793D1 * t3 * t141 * t146
     # * t26 / 0.16D2 + t177 * t176 - t184 * 0.3141592653589793D1 * t3 *
     # t181 / 0.16D2 - t194 * 0.3141592653589793D1 * t3 * t141 * t189 * 
     #t26 / 0.16D2 - t206 * 0.3141592653589793D1 * t3 * t141 * t201 * t2
     #6 / 0.16D2 - t218 * 0.3141592653589793D1 * t3 * t215 / 0.16D2

      end function



      doubleprecision function rrgq2qght7s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t7 * 
     #t8 / 0.32D2)
      t14 = t3 * t7
      t17 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t21 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t7 * 
     #t17 / 0.32D2)
      t25 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t29 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t25 / 0.32D2)
      t33 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t37 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t7 * 
     #t33 / 0.32D2)
      rrgq2qght7s1em3 = t12 * 0.3141592653589793D1 * t14 * t8 / 0.32D2 +
     # t21 * 0.3141592653589793D1 * t14 * t17 / 0.32D2 + t29 * 0.3141592
     #653589793D1 * t14 * t25 / 0.32D2 + t37 * 0.3141592653589793D1 * t1
     #4 * t33 / 0.32D2

      end function



      doubleprecision function rrgq2qght7s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgq2qght7s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght7s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t4 = t3 * 0.3141592653589793D1
      t7 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = t7 * 0.3141592653589793D1
      t10 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t11 = t10 * 0.3141592653589793D1
      t12 = lh ** 2
      t14 = 0.3141592653589793D1 ** 2
      t16 = 0.180D3 * t12 - 0.30D2 * t14
      t19 = 0.1D1 / t1
      t21 = s ** 2
      t23 = 0.1D1 / t21 / s
      t24 = z ** 2
      t26 = 0.1D1 / t24 / z
      t27 = x3 * t26
      t28 = x4 * 0.3141592653589793D1
      t29 = Sin(t28)
      t30 = t29 ** 2
      t31 = t1 ** 2
      t32 = t31 ** 2
      t33 = t30 * t32
      t34 = -0.1D1 + x3
      t35 = 0.1D1 / t34
      t36 = t33 * t35
      t39 = log(-0.4D1 * t27 * t36)
      t40 = x3 * z
      t41 = 0.2D1 * t40
      t42 = cos(t28)
      t44 = Sqrt(-t40 * t34)
      t48 = 0.1D1 / (-0.1D1 - t41 + x3 + 0.2D1 * t42 * t44)
      t52 = log(0.4D1 * t27 * t33)
      t54 = t23 * (t39 * t48 + t52)
      t56 = t19 * t23
      t57 = t52 ** 2
      t59 = t39 ** 2
      t64 = t56 * (t57 * t52 / 0.6D1 + t59 * t39 * t48 / 0.6D1)
      t73 = 0.60D2 * lh * t14 - 0.2884936567583026D3 - 0.120D3 * t12 * l
     #h
      t75 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t82 = t23 * (-t48 - 0.1D1)
      t92 = t23 * (-t59 * t48 / 0.2D1 - t57 / 0.2D1)
      t95 = 0.1D1 / x3
      t98 = t26 * t30
      t99 = t98 * t32
      t101 = log(0.4D1 * t99)
      t102 = t101 ** 2
      t105 = t102 * t101
      t123 = t14 ** 2
      t124 = t12 ** 2
      t129 = t123 + 0.60D2 * t124 + 0.5769873135166051D3 * lh - 0.60D2 *
     # t12 * t14
      t135 = rrgq2qgh71J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t137 = t102 ** 2
      t147 = 0.3141592653589793D1 * t19
      t148 = x1 ** 2
      t149 = x3 * t148
      t150 = t149 * t30
      t151 = t26 * t32
      t155 = log(-0.4D1 * t150 * t151 * t35)
      t157 = t155 ** 2
      t164 = log(0.4D1 * t149 * t99)
      t166 = t164 ** 2
      t173 = 0.3141592653589793D1 * lh
      t182 = 0.3141592653589793D1 * t16
      t183 = t10 * t48
      t189 = 0.1D1 / x1
      t192 = t148 * t30
      t195 = log(0.4D1 * t192 * t151)
      t200 = t195 ** 2
      t201 = t200 * t195
      t211 = 0.3141592653589793D1 * t73
      t224 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t225 = x2 ** 2
      t226 = x3 * t225
      t227 = t226 * t148
      t228 = -0.1D1 + x2
      t229 = t32 * t228
      t233 = log(-0.4D1 * t227 * t98 * t229)
      t234 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t240 = log(-0.4D1 * t227 * t98 * t32 * t35)
      t246 = log(0.4D1 * t227 * t99)
      t252 = t183 - t234 + t10
      t258 = 0.1D1 / x2
      t259 = t258 * t189
      t262 = t225 * t148
      t265 = log(0.4D1 * t262 * t99)
      t266 = t265 ** 2
      t269 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t270 = t262 * t30
      t274 = log(-0.4D1 * t270 * t151 * t228)
      t277 = t274 ** 2
      t290 = t10 - t234
      t297 = t226 * t26
      t298 = t33 * t228
      t301 = log(-0.4D1 * t297 * t298)
      t305 = log(-0.4D1 * t297 * t36)
      t307 = t305 ** 2
      t314 = log(0.4D1 * t226 * t99)
      t316 = t301 ** 2
      t319 = t314 ** 2
      t342 = t26 * t225
      t345 = log(-0.4D1 * t342 * t298)
      t349 = log(0.4D1 * t342 * t33)
      t354 = t349 ** 2
      t355 = t354 * t349
      t358 = t345 ** 2
      t362 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t363 = t358 * t345
      t389 = -((-0.180D3 * t4 * lh + 0.90D2 * t8 + t11 * t16) * t19 * t5
     #4 + 0.90D2 * t11 * t64 + (-0.180D3 * t8 * lh + t11 * t73 + 0.90D2 
     #* t75 * 0.3141592653589793D1 + t4 * t16) * t19 * t82 + (-0.180D3 *
     # t11 * lh + 0.90D2 * t4) * t19 * t92) * t95 / 0.2880D4 + (-0.180D3
     # * (t102 * t3 / 0.2D1 + t75 - t105 * t10 / 0.6D1 - t101 * t7) * 0.
     #3141592653589793D1 * lh + (t7 - t101 * t3 + t102 * t10 / 0.2D1) * 
     #0.3141592653589793D1 * t16 + (t3 - t101 * t10) * 0.314159265358979
     #3D1 * t73 + t11 * t129 + 0.90D2 * (t102 * t7 / 0.2D1 - t105 * t3 /
     # 0.6D1 + t135 - t101 * t75 + t137 * t10 / 0.24D2) * 0.314159265358
     #9793D1) * t19 * t23 / 0.2880D4 + (0.90D2 * t147 * t23 * ((t7 - t15
     #5 * t3 + t157 * t10 / 0.2D1) * t48 + t7 - t164 * t3 + t166 * t10 /
     # 0.2D1) - 0.180D3 * t173 * t56 * (t3 + (t3 - t155 * t10) * t48 - t
     #164 * t10) + t182 * t56 * (t10 + t183)) * t95 * t189 / 0.1440D4 - 
     #(t182 * t56 * (-t3 + t195 * t10) + 0.90D2 * t147 * t23 * (t201 * t
     #10 / 0.6D1 + t195 * t7 - t200 * t3 / 0.2D1 - t75) - t211 * t56 * t
     #10 - 0.180D3 * t173 * t56 * (-t7 + t195 * t3 - t200 * t10 / 0.2D1)
     #) * t189 / 0.1440D4 + (0.90D2 * t147 * t23 * (-t224 + t233 * t234 
     #+ (t3 - t240 * t10) * t48 - t246 * t10 + t3) - 0.180D3 * t173 * t5
     #6 * t252) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (t7 + t2
     #66 * t10 / 0.2D1 - t269 + t274 * t224 - t265 * t3 - t277 * t234 / 
     #0.2D1) - 0.180D3 * t173 * t56 * (t3 - t224 - t265 * t10 + t274 * t
     #234) + t182 * t56 * t290) * t258 * t189 / 0.720D3 - (0.90D2 * t147
     # * t23 * (-t7 - t301 * t224 + t269 - (t7 - t305 * t3 + t307 * t10 
     #/ 0.2D1) * t48 + t314 * t3 + t316 * t234 / 0.2D1 - t319 * t10 / 0.
     #2D1) - 0.180D3 * t173 * t56 * (t314 * t10 + t224 - (t3 - t305 * t1
     #0) * t48 - t3 - t301 * t234) - t182 * t56 * t252) * t95 * t258 / 0
     #.1440D4 - (t182 * t56 * (t224 - t3 - t345 * t234 + t349 * t10) + 0
     #.90D2 * t147 * t23 * (t355 * t10 / 0.6D1 + t358 * t224 / 0.2D1 + t
     #349 * t7 - t75 + t362 - t363 * t234 / 0.6D1 - t354 * t3 / 0.2D1 - 
     #t345 * t269) - t211 * t56 * t290 - 0.180D3 * t173 * t56 * (t269 + 
     #t358 * t234 / 0.2D1 - t7 + t349 * t3 - t345 * t224 - t354 * t10 / 
     #0.2D1)) * t258 / 0.1440D4
      t390 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t389)
      t392 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t393 = t392 * 0.3141592653589793D1
      t396 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t397 = t396 * 0.3141592653589793D1
      t399 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t400 = t399 * 0.3141592653589793D1
      t410 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t446 = rrgq2qgh73J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t481 = t399 * t48
      t514 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t519 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t525 = t399 + t481 - t519
      t534 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t550 = t399 - t519
      t593 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t624 = -((-0.180D3 * t393 * lh + 0.90D2 * t397 + t400 * t16) * t19
     # * t54 + 0.90D2 * t400 * t64 + (-0.180D3 * t397 * lh + t400 * t73 
     #+ 0.90D2 * t410 * 0.3141592653589793D1 + t393 * t16) * t19 * t82 +
     # (-0.180D3 * t400 * lh + 0.90D2 * t393) * t19 * t92) * t95 / 0.288
     #0D4 + (-0.180D3 * (-t105 * t399 / 0.6D1 + t102 * t392 / 0.2D1 + t4
     #10 - t101 * t396) * 0.3141592653589793D1 * lh + (t102 * t399 / 0.2
     #D1 - t101 * t392 + t396) * 0.3141592653589793D1 * t16 + (t392 - t1
     #01 * t399) * 0.3141592653589793D1 * t73 + t400 * t129 + 0.90D2 * (
     #t446 - t101 * t410 + t102 * t396 / 0.2D1 - t105 * t392 / 0.6D1 + t
     #137 * t399 / 0.24D2) * 0.3141592653589793D1) * t19 * t23 / 0.2880D
     #4 + (0.90D2 * t147 * t23 * (-t164 * t392 + (t157 * t399 / 0.2D1 - 
     #t155 * t392 + t396) * t48 + t166 * t399 / 0.2D1 + t396) - 0.180D3 
     #* t173 * t56 * (t392 + (t392 - t155 * t399) * t48 - t164 * t399) +
     # t182 * t56 * (t399 + t481)) * t95 * t189 / 0.1440D4 - (t182 * t56
     # * (-t392 + t195 * t399) + 0.90D2 * t147 * t23 * (t201 * t399 / 0.
     #6D1 - t410 - t200 * t392 / 0.2D1 + t195 * t396) - t211 * t56 * t39
     #9 - 0.180D3 * t173 * t56 * (-t200 * t399 / 0.2D1 + t195 * t392 - t
     #396)) * t189 / 0.1440D4 + (0.90D2 * t147 * t23 * (-t514 + (t392 - 
     #t240 * t399) * t48 + t392 - t246 * t399 + t233 * t519) - 0.180D3 *
     # t173 * t56 * t525) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 
     #* (-t265 * t392 + t396 - t534 + t274 * t514 + t266 * t399 / 0.2D1 
     #- t277 * t519 / 0.2D1) - 0.180D3 * t173 * t56 * (-t514 - t265 * t3
     #99 + t392 + t274 * t519) + t182 * t56 * t550) * t258 * t189 / 0.72
     #0D3 - (0.90D2 * t147 * t23 * (t534 - (t307 * t399 / 0.2D1 - t305 *
     # t392 + t396) * t48 - t396 + t316 * t519 / 0.2D1 - t301 * t514 - t
     #319 * t399 / 0.2D1 + t314 * t392) - 0.180D3 * t173 * t56 * (-t392 
     #+ t314 * t399 + t514 - t301 * t519 - (t392 - t305 * t399) * t48) -
     # t182 * t56 * t525) * t95 * t258 / 0.1440D4 - (t182 * t56 * (-t392
     # - t345 * t519 + t514 + t349 * t399) + 0.90D2 * t147 * t23 * (t593
     # - t410 + t349 * t396 + t358 * t514 / 0.2D1 - t354 * t392 / 0.2D1 
     #- t363 * t519 / 0.6D1 + t355 * t399 / 0.6D1 - t345 * t534) - t211 
     #* t56 * t550 - 0.180D3 * t173 * t56 * (-t345 * t514 + t534 - t396 
     #+ t349 * t392 + t358 * t519 / 0.2D1 - t354 * t399 / 0.2D1)) * t258
     # / 0.1440D4
      t625 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t624)
      t627 = t2 * x1
      t628 = -0.1D1 + x1
      t629 = t2 * t628
      t630 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t631 = 0.1D1 / t24
      t632 = t631 * t32
      t633 = x1 * z
      t634 = -z - x1 + t633
      t635 = 0.1D1 / t634
      t636 = t628 ** 2
      t637 = t635 * t636
      t638 = t632 * t637
      t641 = log(-0.4D1 * t150 * t638)
      t642 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t644 = t641 ** 2
      t645 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t648 = t30 * t631
      t650 = t32 * t635
      t655 = log(0.4D1 * t149 * t648 * t650 * t636 * t35)
      t657 = t655 ** 2
      t661 = x3 * x1
      t662 = t661 * z
      t665 = x3 * t634
      t667 = Sqrt(t665 * t34)
      t671 = 0.1D1 / (0.2D1 * t662 - 0.2D1 * t661 - t41 + x3 - 0.1D1 + 0
     #.2D1 * t42 * t667)
      t687 = t56 * (-t645 - t645 * t671)
      t693 = t192 * t631
      t694 = t650 * t636
      t697 = log(-0.4D1 * t693 * t694)
      t702 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t703 = t697 ** 2
      t706 = t703 * t697
      t714 = t56 * t645
      t726 = t226 * t192
      t731 = log(0.4D1 * t726 * t632 * t637 * t35)
      t737 = log(-0.4D1 * t726 * t638)
      t751 = log(-0.4D1 * t270 * t638)
      t752 = t751 ** 2
      t770 = (0.90D2 * t147 * t23 * (-t630 + t641 * t642 - t644 * t645 /
     # 0.2D1 - (t630 - t655 * t642 + t657 * t645 / 0.2D1) * t671) - 0.18
     #0D3 * t173 * t56 * (-t642 - (t642 - t655 * t645) * t671 + t641 * t
     #645) + t182 * t687) * t95 * t189 / 0.1440D4 - (t182 * t56 * (t642 
     #- t697 * t645) + 0.90D2 * t147 * t23 * (t702 + t703 * t642 / 0.2D1
     # - t706 * t645 / 0.6D1 - t697 * t630) + t211 * t714 - 0.180D3 * t1
     #73 * t56 * (t630 - t697 * t642 + t703 * t645 / 0.2D1)) * t189 / 0.
     #1440D4 + (0.90D2 * t147 * t23 * (-(t642 - t731 * t645) * t671 - t6
     #42 + t737 * t645) - 0.180D3 * t173 * t687) * t95 * t259 / 0.720D3 
     #+ (0.90D2 * t147 * t23 * (-t752 * t645 / 0.2D1 - t630 + t751 * t64
     #2) - 0.180D3 * t173 * t56 * (-t642 + t751 * t645) - t182 * t714) *
     # t258 * t189 / 0.720D3
      t771 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t627, -t629, 0.0D0, t770)
      t773 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t776 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t777 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t798 = t56 * (-t773 * t671 - t773)
      t810 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t818 = t56 * t773
      t861 = (0.90D2 * t147 * t23 * (-t644 * t773 / 0.2D1 - t776 + t641 
     #* t777 - (t657 * t773 / 0.2D1 - t655 * t777 + t776) * t671) - 0.18
     #0D3 * t173 * t56 * (t641 * t773 - (t777 - t655 * t773) * t671 - t7
     #77) + t182 * t798) * t95 * t189 / 0.1440D4 - (t182 * t56 * (-t697 
     #* t773 + t777) + 0.90D2 * t147 * t23 * (-t706 * t773 / 0.6D1 + t81
     #0 + t703 * t777 / 0.2D1 - t697 * t776) + t211 * t818 - 0.180D3 * t
     #173 * t56 * (t703 * t773 / 0.2D1 + t776 - t697 * t777)) * t189 / 0
     #.1440D4 + (0.90D2 * t147 * t23 * (-(t777 - t731 * t773) * t671 + t
     #737 * t773 - t777) - 0.180D3 * t173 * t798) * t95 * t259 / 0.720D3
     # + (0.90D2 * t147 * t23 * (t751 * t777 - t776 - t752 * t773 / 0.2D
     #1) - 0.180D3 * t173 * t56 * (t751 * t773 - t777) - t182 * t818) * 
     #t258 * t189 / 0.720D3
      t862 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t629, t627, 0.0D0, t861)
      t864 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t865 = t864 * 0.3141592653589793D1
      t868 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t869 = t868 * 0.3141592653589793D1
      t871 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t872 = t871 * 0.3141592653589793D1
      t882 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t925 = rrgq2qgh72J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t953 = t871 * t48
      t986 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t992 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t997 = t871 + t953 - t986
      t1009 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t1022 = -t986 + t871
      t1072 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t1096 = -((-0.180D3 * t865 * lh + 0.90D2 * t869 + t872 * t16) * t1
     #9 * t54 + 0.90D2 * t872 * t64 + (-0.180D3 * t869 * lh + t872 * t73
     # + 0.90D2 * t882 * 0.3141592653589793D1 + t865 * t16) * t19 * t82 
     #+ (-0.180D3 * t872 * lh + 0.90D2 * t865) * t19 * t92) * t95 / 0.28
     #80D4 + (-0.180D3 * (t102 * t864 / 0.2D1 - t105 * t871 / 0.6D1 + t8
     #82 - t101 * t868) * 0.3141592653589793D1 * lh + (-t101 * t864 + t8
     #68 + t102 * t871 / 0.2D1) * 0.3141592653589793D1 * t16 + (t864 - t
     #101 * t871) * 0.3141592653589793D1 * t73 + t872 * t129 + 0.90D2 * 
     #(-t105 * t864 / 0.6D1 + t102 * t868 / 0.2D1 - t101 * t882 + t137 *
     # t871 / 0.24D2 + t925) * 0.3141592653589793D1) * t19 * t23 / 0.288
     #0D4 + (0.90D2 * t147 * t23 * (t166 * t871 / 0.2D1 - t164 * t864 + 
     #(-t155 * t864 + t868 + t157 * t871 / 0.2D1) * t48 + t868) - 0.180D
     #3 * t173 * t56 * (-t164 * t871 + (t864 - t155 * t871) * t48 + t864
     #) + t182 * t56 * (t871 + t953)) * t95 * t189 / 0.1440D4 - (t182 * 
     #t56 * (-t864 + t195 * t871) + 0.90D2 * t147 * t23 * (-t882 - t200 
     #* t864 / 0.2D1 + t201 * t871 / 0.6D1 + t195 * t868) - t211 * t56 *
     # t871 - 0.180D3 * t173 * t56 * (-t200 * t871 / 0.2D1 + t195 * t864
     # - t868)) * t189 / 0.1440D4 + (0.90D2 * t147 * t23 * (t233 * t986 
     #- t246 * t871 + (t864 - t240 * t871) * t48 + t864 - t992) - 0.180D
     #3 * t173 * t56 * t997) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t
     #23 * (t266 * t871 / 0.2D1 - t277 * t986 / 0.2D1 - t1009 - t265 * t
     #864 + t868 + t274 * t992) - 0.180D3 * t173 * t56 * (t864 - t265 * 
     #t871 + t274 * t986 - t992) + t182 * t56 * t1022) * t258 * t189 / 0
     #.720D3 - (0.90D2 * t147 * t23 * (-(-t305 * t864 + t868 + t307 * t8
     #71 / 0.2D1) * t48 - t319 * t871 / 0.2D1 + t316 * t986 / 0.2D1 - t8
     #68 + t314 * t864 - t301 * t992 + t1009) - 0.180D3 * t173 * t56 * (
     #-t864 + t992 - t301 * t986 + t314 * t871 - (t864 - t305 * t871) * 
     #t48) - t182 * t56 * t997) * t95 * t258 / 0.1440D4 - (t182 * t56 * 
     #(t992 - t345 * t986 - t864 + t349 * t871) + 0.90D2 * t147 * t23 * 
     #(-t882 + t349 * t868 + t355 * t871 / 0.6D1 - t363 * t986 / 0.6D1 +
     # t358 * t992 / 0.2D1 + t1072 - t354 * t864 / 0.2D1 - t345 * t1009)
     # - t211 * t56 * t1022 - 0.180D3 * t173 * t56 * (-t345 * t992 + t10
     #09 + t349 * t864 - t868 + t358 * t986 / 0.2D1 - t354 * t871 / 0.2D
     #1)) * t258 / 0.1440D4
      t1097 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1096)
      t1099 = x2 * x3
      t1100 = 0.1D1 - x3 + t1099
      t1101 = 0.1D1 / t1100
      t1102 = t1099 * t1101
      t1103 = t2 * t1102
      t1105 = t2 * t34 * t1101
      t1106 = t1099 * z
      t1107 = t228 * t34
      t1109 = Sqrt(t40 * t1107)
      t1113 = 0.1D1 / (t1106 - t41 - 0.1D1 + x3 + 0.2D1 * t42 * t1109)
      t1114 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1115 = t1113 * t1114
      t1116 = t1100 ** 2
      t1117 = 0.1D1 / t1116
      t1118 = t1107 * t1117
      t1122 = log(0.4D1 * t726 * t151 * t1118)
      t1123 = t1122 * t1113
      t1124 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1130 = t173 * t19
      t1131 = t23 * t1113
      t1132 = t1131 * t1124
      t1144 = log(0.4D1 * t226 * t98 * t229 * t34 * t1117)
      t1145 = t1144 * t1113
      t1147 = t1144 ** 2
      t1148 = t1147 * t1113
      t1151 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1162 = t182 * t19
      t1168 = (0.90D2 * t147 * t23 * (-t1115 + t1123 * t1124) + 0.180D3 
     #* t1130 * t1132) * t95 * t259 / 0.720D3 - (0.90D2 * t147 * t23 * (
     #-t1145 * t1114 + t1148 * t1124 / 0.2D1 + t1113 * t1151) - 0.180D3 
     #* t173 * t56 * (t1115 - t1145 * t1124) + t1162 * t1132) * t95 * t2
     #58 / 0.1440D4
      t1169 = FJET(XB1, XB2, s, 0.0D0, t1103, 0.0D0, -t1105, 0.0D0, t116
     #8)
      t1171 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1172 = t1113 * t1171
      t1173 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1179 = t1131 * t1173
      t1189 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1205 = (0.90D2 * t147 * t23 * (-t1172 + t1123 * t1173) + 0.180D3 
     #* t1130 * t1179) * t95 * t259 / 0.720D3 - (0.90D2 * t147 * t23 * (
     #-t1145 * t1171 + t1148 * t1173 / 0.2D1 + t1113 * t1189) - 0.180D3 
     #* t173 * t56 * (t1172 - t1145 * t1173) + t1162 * t1179) * t95 * t2
     #58 / 0.1440D4
      t1206 = FJET(XB1, XB2, s, 0.0D0, -t1105, 0.0D0, t1103, 0.0D0, t120
     #5)
      t1208 = x2 * x1
      t1210 = t2 * t1208 * t635
      t1213 = t228 * s * t1 * x1
      t1218 = s * t31 * x2 * x1 * t628 * t635
      t1219 = t1208 * z
      t1221 = 0.1D1 / (z + x1 - t633 - t1208 + t1219)
      t1222 = t634 * t1221
      t1223 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1224 = t1222 * t1223
      t1229 = log(0.4D1 * t726 * t632 * t637 * t228)
      t1230 = t1229 * t634
      t1231 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1232 = t1221 * t1231
      t1238 = t23 * t634
      t1239 = t1238 * t1232
      t1250 = log(0.4D1 * t262 * t648 * t650 * t636 * t228)
      t1251 = t1250 * t634
      t1254 = t1250 ** 2
      t1255 = t1254 * t634
      t1258 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1274 = (0.90D2 * t147 * t23 * (-t1224 + t1230 * t1232) + 0.180D3 
     #* t1130 * t1239) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (
     #t1251 * t1221 * t1223 - t1255 * t1232 / 0.2D1 - t1222 * t1258) - 0
     #.180D3 * t173 * t56 * (-t1224 + t1251 * t1232) - t1162 * t1239) * 
     #t258 * t189 / 0.720D3
      t1275 = FJET(XB1, XB2, s, 0.0D0, -t1210, -t629, -t1213, t1218, t12
     #74)
      t1277 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t1280 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t1282 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t1283 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t1300 = t1283 * 0.3141592653589793D1
      t1309 = rrgq2qgh74J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0
     #D0, x4)
      t1337 = t1283 * t48
      t1374 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t1375 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t1381 = t1337 + t1283 - t1375
      t1395 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t1406 = t1283 - t1375
      t1413 = t1277 * 0.3141592653589793D1
      t1416 = t1280 * 0.3141592653589793D1
      t1484 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t1509 = (-0.180D3 * (t102 * t1277 / 0.2D1 - t101 * t1280 + t1282 -
     # t105 * t1283 / 0.6D1) * 0.3141592653589793D1 * lh + (-t101 * t127
     #7 + t1280 + t102 * t1283 / 0.2D1) * 0.3141592653589793D1 * t16 + (
     #t1277 - t101 * t1283) * 0.3141592653589793D1 * t73 + t1300 * t129 
     #+ 0.90D2 * (-t105 * t1277 / 0.6D1 - t101 * t1282 + t137 * t1283 / 
     #0.24D2 + t102 * t1280 / 0.2D1 + t1309) * 0.3141592653589793D1) * t
     #19 * t23 / 0.2880D4 + (0.90D2 * t147 * t23 * (t166 * t1283 / 0.2D1
     # + (-t155 * t1277 + t1280 + t157 * t1283 / 0.2D1) * t48 + t1280 - 
     #t164 * t1277) - 0.180D3 * t173 * t56 * (t1277 + (t1277 - t155 * t1
     #283) * t48 - t164 * t1283) + t182 * t56 * (t1283 + t1337)) * t95 *
     # t189 / 0.1440D4 - (t182 * t56 * (-t1277 + t195 * t1283) + 0.90D2 
     #* t147 * t23 * (-t200 * t1277 / 0.2D1 + t201 * t1283 / 0.6D1 + t19
     #5 * t1280 - t1282) - t211 * t56 * t1283 - 0.180D3 * t173 * t56 * (
     #-t200 * t1283 / 0.2D1 + t195 * t1277 - t1280)) * t189 / 0.1440D4 +
     # (0.90D2 * t147 * t23 * (t1277 + (t1277 - t240 * t1283) * t48 - t2
     #46 * t1283 - t1374 + t233 * t1375) - 0.180D3 * t173 * t56 * t1381)
     # * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (t1280 - t277 * t
     #1375 / 0.2D1 - t265 * t1277 + t266 * t1283 / 0.2D1 + t274 * t1374 
     #- t1395) - 0.180D3 * t173 * t56 * (t1277 + t274 * t1375 - t1374 - 
     #t265 * t1283) + t182 * t56 * t1406) * t258 * t189 / 0.720D3 - ((-0
     #.180D3 * t1413 * lh + 0.90D2 * t1416 + t1300 * t16) * t19 * t54 + 
     #0.90D2 * t1300 * t64 + (-0.180D3 * t1416 * lh + t1300 * t73 + 0.90
     #D2 * t1282 * 0.3141592653589793D1 + t1413 * t16) * t19 * t82 + (-0
     #.180D3 * t1300 * lh + 0.90D2 * t1413) * t19 * t92) * t95 / 0.2880D
     #4 - (0.90D2 * t147 * t23 * (t314 * t1277 - t1280 - (-t305 * t1277 
     #+ t1280 + t307 * t1283 / 0.2D1) * t48 - t301 * t1374 + t316 * t137
     #5 / 0.2D1 - t319 * t1283 / 0.2D1 + t1395) - 0.180D3 * t173 * t56 *
     # (-t1277 - t301 * t1375 + t314 * t1283 + t1374 - (t1277 - t305 * t
     #1283) * t48) - t182 * t56 * t1381) * t95 * t258 / 0.1440D4 - (t182
     # * t56 * (t1374 - t1277 - t345 * t1375 + t349 * t1283) + 0.90D2 * 
     #t147 * t23 * (-t354 * t1277 / 0.2D1 - t345 * t1395 - t363 * t1375 
     #/ 0.6D1 + t349 * t1280 - t1282 + t1484 + t358 * t1374 / 0.2D1 + t3
     #55 * t1283 / 0.6D1) - t211 * t56 * t1406 - 0.180D3 * t173 * t56 * 
     #(-t345 * t1374 + t358 * t1375 / 0.2D1 + t1395 + t349 * t1277 - t12
     #80 - t354 * t1283 / 0.2D1)) * t258 / 0.1440D4
      t1510 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1509)
      t1512 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1513 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1516 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1537 = t56 * (-t1513 - t1513 * t671)
      t1548 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1557 = t56 * t1513
      t1600 = (0.90D2 * t147 * t23 * (-t1512 - t644 * t1513 / 0.2D1 - (-
     #t655 * t1516 + t1512 + t657 * t1513 / 0.2D1) * t671 + t641 * t1516
     #) - 0.180D3 * t173 * t56 * (-(t1516 - t655 * t1513) * t671 + t641 
     #* t1513 - t1516) + t182 * t1537) * t95 * t189 / 0.1440D4 - (t182 *
     # t56 * (t1516 - t697 * t1513) + 0.90D2 * t147 * t23 * (-t697 * t15
     #12 + t1548 - t706 * t1513 / 0.6D1 + t703 * t1516 / 0.2D1) + t211 *
     # t1557 - 0.180D3 * t173 * t56 * (-t697 * t1516 + t1512 + t703 * t1
     #513 / 0.2D1)) * t189 / 0.1440D4 + (0.90D2 * t147 * t23 * (t737 * t
     #1513 - t1516 - (t1516 - t731 * t1513) * t671) - 0.180D3 * t173 * t
     #1537) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (-t752 * t15
     #13 / 0.2D1 + t751 * t1516 - t1512) - 0.180D3 * t173 * t56 * (t751 
     #* t1513 - t1516) - t182 * t1557) * t258 * t189 / 0.720D3
      t1601 = FJET(XB1, XB2, s, t627, -t629, 0.0D0, 0.0D0, 0.0D0, t1600)
      t1603 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1606 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1608 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1615 = t1113 * t1606
      t1620 = t1131 * t1603
      t1637 = -(0.90D2 * t147 * t23 * (t1148 * t1603 / 0.2D1 - t1145 * t
     #1606 + t1113 * t1608) - 0.180D3 * t173 * t56 * (-t1145 * t1603 + t
     #1615) + t1162 * t1620) * t95 * t258 / 0.1440D4 + (0.90D2 * t147 * 
     #t23 * (t1123 * t1603 - t1615) + 0.180D3 * t1130 * t1620) * t95 * t
     #259 / 0.720D3
      t1638 = FJET(XB1, XB2, s, t1103, 0.0D0, -t1105, 0.0D0, 0.0D0, t163
     #7)
      t1643 = t34 * s * t1 * t628 * t1101
      t1644 = x2 * z
      t1645 = t1099 * x1
      t1646 = t1099 * t633
      t1648 = Sqrt(-t665 * t1107)
      t1649 = t42 * t1648
      t1652 = z + x1 - t633 - t1644 - t1208 + t1219 - t40 - t661 + t662 
     #+ t1106 + t1645 - t1646 + t226 + 0.2D1 * t1649 * x2
      t1655 = t627 * t1652 * t635 * t1101
      t1656 = t629 * t1102
      t1662 = t627 * x2 * (-t40 - t661 + t662 + t1106 + t1645 - t1646 - 
     #0.1D1 + x3 + 0.2D1 * t1649) * t635 * t1101
      t1667 = log(-0.4D1 * t226 * t693 * t694 * t1118)
      t1668 = t1667 * t634
      t1673 = x3 * t24
      t1681 = t40 - z + 0.2D1 * t1649 * t1219 - 0.5D1 * t662 + t1208 - x
     #1 - 0.2D1 * t149 - 0.2D1 * t1673 - t1219 + t633 + t661 - t1645 + 0
     #.4D1 * t1673 * x1 + 0.4D1 * t149 * z + 0.2D1 * t149 * x2
      t1709 = -0.2D1 * t149 * t24 + 0.2D1 * t1649 * z + t1099 * t24 - t2
     #27 + 0.2D1 * t1649 * x1 + 0.4D1 * t1646 + t226 * t24 * x1 + 0.2D1 
     #* t226 * t148 * z - t226 * t148 * t24 - 0.2D1 * t1649 * t633 - 0.3
     #D1 * t1673 * t1208 - 0.4D1 * t149 * t1644 + 0.2D1 * t149 * t24 * x
     #2 - t226 * t633 - 0.2D1 * t1649 * t1208
      t1711 = 0.1D1 / (t1681 + t1709)
      t1712 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t1713 = t1711 * t1712
      t1715 = t634 * t1711
      t1716 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t1725 = 0.90D2 * t147 * t23 * (t1668 * t1713 - t1715 * t1716) + 0.
     #180D3 * t1130 * t1238 * t1713
      t1729 = FJET(XB1, XB2, s, t1643, -t1655, -t1656, t1662, t1218, t17
     #25 * t95 * t259 / 0.720D3)
      t1732 = t95 * t258 * t189
      t1735 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t1736 = t1711 * t1735
      t1738 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t1747 = 0.90D2 * t147 * t23 * (t1668 * t1736 - t1715 * t1738) + 0.
     #180D3 * t1130 * t1238 * t1736
      t1751 = FJET(XB1, XB2, s, t1662, -t1656, -t1655, t1643, t1218, t17
     #47 * t95 * t259 / 0.720D3)
      t1755 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1757 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1758 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1780 = t56 * (-t1758 - t1758 * t671)
      t1792 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1800 = t56 * t1758
      t1843 = (0.90D2 * t147 * t23 * (-(-t655 * t1755 + t1757 + t657 * t
     #1758 / 0.2D1) * t671 + t641 * t1755 - t644 * t1758 / 0.2D1 - t1757
     #) - 0.180D3 * t173 * t56 * (-(t1755 - t655 * t1758) * t671 - t1755
     # + t641 * t1758) + t182 * t1780) * t95 * t189 / 0.1440D4 - (t182 *
     # t56 * (t1755 - t697 * t1758) + 0.90D2 * t147 * t23 * (-t706 * t17
     #58 / 0.6D1 + t1792 - t697 * t1757 + t703 * t1755 / 0.2D1) + t211 *
     # t1800 - 0.180D3 * t173 * t56 * (-t697 * t1755 + t1757 + t703 * t1
     #758 / 0.2D1)) * t189 / 0.1440D4 + (0.90D2 * t147 * t23 * (t737 * t
     #1758 - (t1755 - t731 * t1758) * t671 - t1755) - 0.180D3 * t173 * t
     #1780) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (t751 * t175
     #5 - t752 * t1758 / 0.2D1 - t1757) - 0.180D3 * t173 * t56 * (t751 *
     # t1758 - t1755) - t182 * t1800) * t258 * t189 / 0.720D3
      t1844 = FJET(XB1, XB2, s, -t629, t627, 0.0D0, 0.0D0, 0.0D0, t1843)
      t1846 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1847 = t1222 * t1846
      t1848 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1849 = t1221 * t1848
      t1855 = t1238 * t1849
      t1863 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1881 = (0.90D2 * t147 * t23 * (-t1847 + t1230 * t1849) + 0.180D3 
     #* t1130 * t1855) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (
     #t1251 * t1221 * t1846 - t1222 * t1863 - t1255 * t1849 / 0.2D1) - 0
     #.180D3 * t173 * t56 * (-t1847 + t1251 * t1849) - t1162 * t1855) * 
     #t258 * t189 / 0.720D3
      t1882 = FJET(XB1, XB2, s, -t629, -t1213, 0.0D0, -t1210, t1218, t18
     #81)
      t1884 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1887 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1889 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t1102,
     # x4)
      t1896 = t1113 * t1887
      t1901 = t1131 * t1884
      t1918 = -(0.90D2 * t147 * t23 * (t1148 * t1884 / 0.2D1 - t1145 * t
     #1887 + t1113 * t1889) - 0.180D3 * t173 * t56 * (-t1145 * t1884 + t
     #1896) + t1162 * t1901) * t95 * t258 / 0.1440D4 + (0.90D2 * t147 * 
     #t23 * (t1123 * t1884 - t1896) + 0.180D3 * t1130 * t1901) * t95 * t
     #259 / 0.720D3
      t1919 = FJET(XB1, XB2, s, -t1105, 0.0D0, t1103, 0.0D0, 0.0D0, t191
     #8)
      t1921 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1922 = t1221 * t1921
      t1924 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1925 = t1222 * t1924
      t1930 = t1238 * t1922
      t1938 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1956 = (0.90D2 * t147 * t23 * (t1230 * t1922 - t1925) + 0.180D3 *
     # t1130 * t1930) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (t
     #1251 * t1221 * t1924 - t1222 * t1938 - t1255 * t1922 / 0.2D1) - 0.
     #180D3 * t173 * t56 * (-t1925 + t1251 * t1922) - t1162 * t1930) * t
     #258 * t189 / 0.720D3
      t1957 = FJET(XB1, XB2, s, -t1213, -t629, -t1210, 0.0D0, t1218, t19
     #56)
      t1959 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1960 = t1222 * t1959
      t1961 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1962 = t1221 * t1961
      t1968 = t1238 * t1962
      t1976 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1994 = (0.90D2 * t147 * t23 * (-t1960 + t1230 * t1962) + 0.180D3 
     #* t1130 * t1968) * t95 * t259 / 0.720D3 + (0.90D2 * t147 * t23 * (
     #t1251 * t1221 * t1959 - t1222 * t1976 - t1255 * t1962 / 0.2D1) - 0
     #.180D3 * t173 * t56 * (-t1960 + t1251 * t1962) - t1162 * t1968) * 
     #t258 * t189 / 0.720D3
      t1995 = FJET(XB1, XB2, s, -t1210, 0.0D0, -t1213, -t629, t1218, t19
     #94)
      t1997 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t1998 = t1711 * t1997
      t2000 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t2009 = 0.90D2 * t147 * t23 * (t1668 * t1998 - t1715 * t2000) + 0.
     #180D3 * t1130 * t1238 * t1998
      t2013 = FJET(XB1, XB2, s, -t1655, t1643, t1662, -t1656, t1218, t20
     #09 * t95 * t259 / 0.720D3)
      t2017 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t2018 = t1711 * t2017
      t2020 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t1102, x4
     #)
      t2029 = 0.90D2 * t147 * t23 * (t1668 * t2018 - t1715 * t2020) + 0.
     #180D3 * t1130 * t1238 * t2018
      t2033 = FJET(XB1, XB2, s, -t1656, t1662, t1643, -t1655, t1218, t20
     #29 * t95 * t259 / 0.720D3)
      rrgq2qght7s2e1 = t390 * t389 + t625 * t624 + t771 * t770 + t862 * 
     #t861 + t1097 * t1096 + t1169 * t1168 + t1206 * t1205 + t1275 * t12
     #74 + t1510 * t1509 + t1601 * t1600 + t1638 * t1637 + t1729 * t1725
     # * t1732 / 0.720D3 + t1751 * t1747 * t1732 / 0.720D3 + t1844 * t18
     #43 + t1882 * t1881 + t1919 * t1918 + t1957 * t1956 + t1995 * t1994
     # + t2013 * t2009 * t1732 / 0.720D3 + t2033 * t2029 * t1732 / 0.720
     #D3

      end function



      doubleprecision function rrgq2qght7s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t20 = -0.1D1 + x3
      t21 = 0.1D1 / t20
      t22 = t19 * t21
      t25 = log(-0.4D1 * t13 * t22)
      t26 = t25 ** 2
      t27 = x3 * z
      t28 = 0.2D1 * t27
      t29 = cos(t14)
      t31 = Sqrt(-t27 * t20)
      t35 = 0.1D1 / (-0.1D1 - t28 + x3 + 0.2D1 * t29 * t31)
      t39 = log(0.4D1 * t13 * t19)
      t40 = t39 ** 2
      t43 = t9 * (-t26 * t35 / 0.2D1 - t40 / 0.2D1)
      t48 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t49 = t48 * 0.3141592653589793D1
      t55 = t8 * (t25 * t35 + t39)
      t59 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t62 = lh ** 2
      t64 = 0.3141592653589793D1 ** 2
      t66 = 0.180D3 * t62 - 0.30D2 * t64
      t71 = t8 * (-t35 - 0.1D1)
      t74 = 0.1D1 / x3
      t77 = t12 * t16
      t78 = t77 * t18
      t80 = log(0.4D1 * t78)
      t82 = t80 ** 2
      t93 = 0.60D2 * lh * t64 - 0.2884936567583026D3 - 0.120D3 * t62 * l
     #h
      t97 = rrgq2qgh71J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t98 = t82 * t80
      t113 = 0.3141592653589793D1 * t5
      t114 = x2 ** 2
      t115 = t114 * x3
      t118 = log(0.4D1 * t115 * t78)
      t120 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t121 = t115 * t12
      t124 = log(-0.4D1 * t121 * t22)
      t128 = -0.1D1 + x2
      t129 = t19 * t128
      t132 = log(-0.4D1 * t121 * t129)
      t133 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t139 = 0.3141592653589793D1 * lh
      t140 = t3 * t35
      t141 = t133 - t140 - t3
      t147 = 0.1D1 / x2
      t150 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t151 = t12 * t114
      t154 = log(-0.4D1 * t151 * t129)
      t155 = t154 ** 2
      t160 = log(0.4D1 * t151 * t19)
      t163 = t160 ** 2
      t176 = 0.3141592653589793D1 * t66
      t177 = t133 - t3
      t183 = x1 ** 2
      t184 = x3 * t183
      t185 = t184 * t16
      t186 = t12 * t18
      t190 = log(-0.4D1 * t185 * t186 * t21)
      t196 = log(0.4D1 * t184 * t78)
      t208 = 0.1D1 / x1
      t211 = t113 * t8
      t214 = t147 * t208
      t218 = t114 * t183
      t221 = log(0.4D1 * t218 * t78)
      t223 = t218 * t16
      t227 = log(-0.4D1 * t223 * t186 * t128)
      t241 = t183 * t16
      t244 = log(0.4D1 * t241 * t186)
      t246 = t244 ** 2
      t263 = -(0.90D2 * t4 * t43 + (-0.180D3 * t4 * lh + 0.90D2 * t49) *
     # t5 * t55 + (-0.180D3 * t49 * lh + 0.90D2 * t59 * 0.31415926535897
     #93D1 + t4 * t66) * t5 * t71) * t74 / 0.2880D4 + (-0.180D3 * (t59 -
     # t80 * t48 + t82 * t3 / 0.2D1) * 0.3141592653589793D1 * lh + t4 * 
     #t93 + 0.90D2 * (t82 * t48 / 0.2D1 + t97 - t98 * t3 / 0.6D1 - t80 *
     # t59) * 0.3141592653589793D1 + (t48 - t80 * t3) * 0.31415926535897
     #93D1 * t66) * t5 * t8 / 0.2880D4 - (0.90D2 * t113 * t8 * (t118 * t
     #3 + t120 - (t48 - t124 * t3) * t35 - t48 - t132 * t133) - 0.180D3 
     #* t139 * t9 * t141) * t74 * t147 / 0.1440D4 - (0.90D2 * t113 * t8 
     #* (t150 + t155 * t133 / 0.2D1 - t59 + t160 * t48 - t154 * t120 - t
     #163 * t3 / 0.2D1) - 0.180D3 * t139 * t9 * (t120 - t48 - t154 * t13
     #3 + t160 * t3) + t176 * t9 * t177) * t147 / 0.1440D4 + (0.90D2 * t
     #113 * t8 * (t48 + (t48 - t190 * t3) * t35 - t196 * t3) - 0.180D3 *
     # t139 * t9 * (t3 + t140)) * t74 * t208 / 0.1440D4 - t211 * t141 * 
     #t74 * t214 / 0.8D1 + (0.90D2 * t113 * t8 * (t48 - t120 - t221 * t3
     # + t227 * t133) + 0.180D3 * t139 * t9 * t177) * t147 * t208 / 0.72
     #0D3 - (0.90D2 * t113 * t8 * (-t59 + t244 * t48 - t246 * t3 / 0.2D1
     #) - 0.180D3 * t139 * t9 * (-t48 + t244 * t3) - t176 * t9 * t3) * t
     #208 / 0.1440D4
      t264 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t263)
      t266 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t267 = t266 * 0.3141592653589793D1
      t272 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t273 = t272 * 0.3141592653589793D1
      t280 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t302 = rrgq2qgh73J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t316 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t317 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t326 = t266 * t35
      t327 = -t326 - t266 + t317
      t336 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t352 = t317 - t266
      t410 = -(0.90D2 * t267 * t43 + (-0.180D3 * t267 * lh + 0.90D2 * t2
     #73) * t5 * t55 + (-0.180D3 * t273 * lh + 0.90D2 * t280 * 0.3141592
     #653589793D1 + t267 * t66) * t5 * t71) * t74 / 0.2880D4 + (-0.180D3
     # * (t82 * t266 / 0.2D1 - t80 * t272 + t280) * 0.3141592653589793D1
     # * lh + t267 * t93 + 0.90D2 * (-t98 * t266 / 0.6D1 + t82 * t272 / 
     #0.2D1 + t302 - t80 * t280) * 0.3141592653589793D1 + (t272 - t80 * 
     #t266) * 0.3141592653589793D1 * t66) * t5 * t8 / 0.2880D4 - (0.90D2
     # * t113 * t8 * (-t272 + t118 * t266 + t316 - t132 * t317 - (t272 -
     # t124 * t266) * t35) - 0.180D3 * t139 * t9 * t327) * t74 * t147 / 
     #0.1440D4 - (0.90D2 * t113 * t8 * (-t154 * t316 + t336 - t280 + t16
     #0 * t272 + t155 * t317 / 0.2D1 - t163 * t266 / 0.2D1) - 0.180D3 * 
     #t139 * t9 * (-t272 - t154 * t317 + t316 + t160 * t266) + t176 * t9
     # * t352) * t147 / 0.1440D4 + (0.90D2 * t113 * t8 * (t272 + (t272 -
     # t190 * t266) * t35 - t196 * t266) - 0.180D3 * t139 * t9 * (t266 +
     # t326)) * t74 * t208 / 0.1440D4 - t211 * t327 * t74 * t214 / 0.8D1
     # + (0.90D2 * t113 * t8 * (-t316 - t221 * t266 + t272 + t227 * t317
     #) + 0.180D3 * t139 * t9 * t352) * t147 * t208 / 0.720D3 - (0.90D2 
     #* t113 * t8 * (-t246 * t266 / 0.2D1 + t244 * t272 - t280) - 0.180D
     #3 * t139 * t9 * (-t272 + t244 * t266) - t176 * t9 * t266) * t208 /
     # 0.1440D4
      t411 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t410)
      t413 = t2 * x1
      t414 = -0.1D1 + x1
      t415 = t2 * t414
      t416 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t417 = 0.1D1 / t10
      t418 = t16 * t417
      t420 = x1 * z
      t421 = -z - x1 + t420
      t422 = 0.1D1 / t421
      t423 = t18 * t422
      t424 = t414 ** 2
      t429 = log(0.4D1 * t184 * t418 * t423 * t424 * t21)
      t430 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t433 = x3 * x1
      t434 = t433 * z
      t437 = x3 * t421
      t439 = Sqrt(t437 * t20)
      t443 = 0.1D1 / (0.2D1 * t434 - 0.2D1 * t433 - t28 + x3 - 0.1D1 + 0
     #.2D1 * t29 * t439)
      t447 = t417 * t18 * t422 * t424
      t450 = log(-0.4D1 * t185 * t447)
      t457 = -t430 - t430 * t443
      t471 = log(-0.4D1 * t223 * t447)
      t477 = t9 * t430
      t484 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t489 = log(-0.4D1 * t241 * t417 * t423 * t424)
      t491 = t489 ** 2
      t507 = (0.90D2 * t113 * t8 * (-t416 - (t416 - t429 * t430) * t443 
     #+ t450 * t430) - 0.180D3 * t139 * t9 * t457) * t74 * t208 / 0.1440
     #D4 + t211 * t457 * t74 * t214 / 0.8D1 + (0.90D2 * t113 * t8 * (-t4
     #16 + t471 * t430) + 0.180D3 * t139 * t477) * t147 * t208 / 0.720D3
     # - (0.90D2 * t113 * t8 * (t484 - t489 * t416 + t491 * t430 / 0.2D1
     #) - 0.180D3 * t139 * t9 * (t416 - t489 * t430) + t176 * t477) * t2
     #08 / 0.1440D4
      t508 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t413, -t415, 0.0D0, t507)
      t510 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t512 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t521 = -t510 * t443 - t510
      t538 = t9 * t510
      t547 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t562 = (0.90D2 * t113 * t8 * (t450 * t510 - (t512 - t429 * t510) *
     # t443 - t512) - 0.180D3 * t139 * t9 * t521) * t74 * t208 / 0.1440D
     #4 + t211 * t521 * t74 * t214 / 0.8D1 + (0.90D2 * t113 * t8 * (t471
     # * t510 - t512) + 0.180D3 * t139 * t538) * t147 * t208 / 0.720D3 -
     # (0.90D2 * t113 * t8 * (t491 * t510 / 0.2D1 + t547 - t489 * t512) 
     #- 0.180D3 * t139 * t9 * (-t489 * t510 + t512) + t176 * t538) * t20
     #8 / 0.1440D4
      t563 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t415, t413, 0.0D0, t562)
      t565 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t566 = t565 * 0.3141592653589793D1
      t571 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t572 = t571 * 0.3141592653589793D1
      t579 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t601 = rrgq2qgh72J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t614 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t615 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t625 = t565 * t35
      t626 = t615 - t625 - t565
      t635 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t651 = t615 - t565
      t709 = -(0.90D2 * t566 * t43 + (-0.180D3 * t566 * lh + 0.90D2 * t5
     #72) * t5 * t55 + (-0.180D3 * t572 * lh + 0.90D2 * t579 * 0.3141592
     #653589793D1 + t566 * t66) * t5 * t71) * t74 / 0.2880D4 + (-0.180D3
     # * (-t80 * t571 + t579 + t82 * t565 / 0.2D1) * 0.3141592653589793D
     #1 * lh + t566 * t93 + 0.90D2 * (t82 * t571 / 0.2D1 - t98 * t565 / 
     #0.6D1 + t601 - t80 * t579) * 0.3141592653589793D1 + (t571 - t80 * 
     #t565) * 0.3141592653589793D1 * t66) * t5 * t8 / 0.2880D4 - (0.90D2
     # * t113 * t8 * (-t571 + t614 - t132 * t615 + t118 * t565 - (t571 -
     # t124 * t565) * t35) - 0.180D3 * t139 * t9 * t626) * t74 * t147 / 
     #0.1440D4 - (0.90D2 * t113 * t8 * (-t154 * t614 + t635 + t160 * t57
     #1 - t579 + t155 * t615 / 0.2D1 - t163 * t565 / 0.2D1) - 0.180D3 * 
     #t139 * t9 * (t614 - t154 * t615 - t571 + t160 * t565) + t176 * t9 
     #* t651) * t147 / 0.1440D4 + (0.90D2 * t113 * t8 * (-t196 * t565 + 
     #(t571 - t190 * t565) * t35 + t571) - 0.180D3 * t139 * t9 * (t565 +
     # t625)) * t74 * t208 / 0.1440D4 - t211 * t626 * t74 * t214 / 0.8D1
     # + (0.90D2 * t113 * t8 * (t571 - t221 * t565 + t227 * t615 - t614)
     # + 0.180D3 * t139 * t9 * t651) * t147 * t208 / 0.720D3 - (0.90D2 *
     # t113 * t8 * (-t246 * t565 / 0.2D1 + t244 * t571 - t579) - 0.180D3
     # * t139 * t9 * (-t571 + t244 * t565) - t176 * t9 * t565) * t208 / 
     #0.1440D4
      t710 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t709)
      t712 = x2 * x3
      t713 = 0.1D1 - x3 + t712
      t714 = 0.1D1 / t713
      t715 = t712 * t714
      t716 = t2 * t715
      t718 = t2 * t20 * t714
      t719 = t712 * z
      t720 = t128 * t20
      t722 = Sqrt(t27 * t720)
      t726 = 0.1D1 / (t719 - t28 - 0.1D1 + x3 + 0.2D1 * t29 * t722)
      t727 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, x
     #4)
      t731 = t713 ** 2
      t737 = log(0.4D1 * t115 * t77 * t18 * t128 * t20 / t731)
      t738 = t737 * t726
      t739 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, x
     #4)
      t745 = t139 * t5
      t746 = t8 * t726
      t754 = t113 * t746
      t759 = -(0.90D2 * t113 * t8 * (t726 * t727 - t738 * t739) - 0.180D
     #3 * t745 * t746 * t739) * t74 * t147 / 0.1440D4 - t754 * t739 * t7
     #4 * t214 / 0.8D1
      t760 = FJET(XB1, XB2, s, 0.0D0, t716, 0.0D0, -t718, 0.0D0, t759)
      t762 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, x
     #4)
      t764 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, x
     #4)
      t781 = -(0.90D2 * t113 * t8 * (t726 * t762 - t738 * t764) - 0.180D
     #3 * t745 * t746 * t764) * t74 * t147 / 0.1440D4 - t754 * t764 * t7
     #4 * t214 / 0.8D1
      t782 = FJET(XB1, XB2, s, 0.0D0, -t718, 0.0D0, t716, 0.0D0, t781)
      t784 = x2 * x1
      t786 = t2 * t784 * t422
      t789 = t128 * s * t1 * x1
      t794 = s * t17 * x2 * x1 * t414 * t422
      t795 = t8 * t421
      t796 = t113 * t795
      t797 = t784 * z
      t799 = 0.1D1 / (z + x1 - t420 - t784 + t797)
      t800 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t801 = t799 * t800
      t803 = t74 * t147 * t208
      t807 = t421 * t799
      t808 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t815 = log(0.4D1 * t218 * t418 * t423 * t424 * t128)
      t816 = t815 * t421
      t829 = -t796 * t801 * t803 / 0.8D1 + (0.90D2 * t113 * t8 * (-t807 
     #* t808 + t816 * t801) + 0.180D3 * t745 * t795 * t801) * t147 * t20
     #8 / 0.720D3
      t830 = FJET(XB1, XB2, s, 0.0D0, -t786, -t415, -t789, t794, t829)
      t832 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t834 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t835 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t842 = t835 * 0.3141592653589793D1
      t847 = rrgq2qgh74J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t861 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t864 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t872 = t835 * t35
      t873 = -t872 - t835 + t861
      t884 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t898 = t861 - t835
      t908 = t832 * 0.3141592653589793D1
      t976 = (-0.180D3 * (-t80 * t832 + t834 + t82 * t835 / 0.2D1) * 0.3
     #141592653589793D1 * lh + t842 * t93 + 0.90D2 * (t82 * t832 / 0.2D1
     # - t80 * t834 + t847 - t98 * t835 / 0.6D1) * 0.3141592653589793D1 
     #+ (t832 - t80 * t835) * 0.3141592653589793D1 * t66) * t5 * t8 / 0.
     #2880D4 - (0.90D2 * t113 * t8 * (-t832 - t132 * t861 + t118 * t835 
     #+ t864 - (t832 - t124 * t835) * t35) - 0.180D3 * t139 * t9 * t873)
     # * t74 * t147 / 0.1440D4 - (0.90D2 * t113 * t8 * (-t154 * t864 + t
     #155 * t861 / 0.2D1 + t884 + t160 * t832 - t834 - t163 * t835 / 0.2
     #D1) - 0.180D3 * t139 * t9 * (t864 - t832 - t154 * t861 + t160 * t8
     #35) + t176 * t9 * t898) * t147 / 0.1440D4 - (0.90D2 * t842 * t43 +
     # (-0.180D3 * t842 * lh + 0.90D2 * t908) * t5 * t55 + (-0.180D3 * t
     #908 * lh + 0.90D2 * t834 * 0.3141592653589793D1 + t842 * t66) * t5
     # * t71) * t74 / 0.2880D4 + (0.90D2 * t113 * t8 * (t832 + (t832 - t
     #190 * t835) * t35 - t196 * t835) - 0.180D3 * t139 * t9 * (t835 + t
     #872)) * t74 * t208 / 0.1440D4 - t211 * t873 * t74 * t214 / 0.8D1 +
     # (0.90D2 * t113 * t8 * (t832 + t227 * t861 - t864 - t221 * t835) +
     # 0.180D3 * t139 * t9 * t898) * t147 * t208 / 0.720D3 - (0.90D2 * t
     #113 * t8 * (-t246 * t835 / 0.2D1 + t244 * t832 - t834) - 0.180D3 *
     # t139 * t9 * (-t832 + t244 * t835) - t176 * t9 * t835) * t208 / 0.
     #1440D4
      t977 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t976)
      t979 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t980 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t990 = -t980 - t980 * t443
      t1007 = t9 * t980
      t1015 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1031 = (0.90D2 * t113 * t8 * (-(t979 - t429 * t980) * t443 + t450
     # * t980 - t979) - 0.180D3 * t139 * t9 * t990) * t74 * t208 / 0.144
     #0D4 + t211 * t990 * t74 * t214 / 0.8D1 + (0.90D2 * t113 * t8 * (t4
     #71 * t980 - t979) + 0.180D3 * t139 * t1007) * t147 * t208 / 0.720D
     #3 - (0.90D2 * t113 * t8 * (-t489 * t979 + t1015 + t491 * t980 / 0.
     #2D1) - 0.180D3 * t139 * t9 * (t979 - t489 * t980) + t176 * t1007) 
     #* t208 / 0.1440D4
      t1032 = FJET(XB1, XB2, s, t413, -t415, 0.0D0, 0.0D0, 0.0D0, t1031)
      t1034 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, 
     #x4)
      t1040 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, 
     #x4)
      t1053 = -t754 * t1034 * t74 * t214 / 0.8D1 - (0.90D2 * t113 * t8 *
     # (-t738 * t1034 + t726 * t1040) - 0.180D3 * t745 * t746 * t1034) *
     # t74 * t147 / 0.1440D4
      t1054 = FJET(XB1, XB2, s, t716, 0.0D0, -t718, 0.0D0, 0.0D0, t1053)
      t1059 = t20 * s * t1 * t414 * t714
      t1060 = x2 * z
      t1061 = t712 * x1
      t1062 = t712 * t420
      t1064 = Sqrt(-t437 * t720)
      t1065 = t29 * t1064
      t1068 = z + x1 - t420 - t1060 - t784 + t797 - t27 - t433 + t434 + 
     #t719 + t1061 - t1062 + t115 + 0.2D1 * t1065 * x2
      t1071 = t413 * t1068 * t422 * t714
      t1072 = t415 * t715
      t1078 = t413 * x2 * (-t27 - t433 + t434 + t719 + t1061 - t1062 - 0
     #.1D1 + x3 + 0.2D1 * t1065) * t422 * t714
      t1089 = x3 * t10
      t1102 = 0.4D1 * t1062 + t115 * t10 * x1 + 0.2D1 * t115 * t183 * z 
     #- t115 * t183 * t10 - 0.2D1 * t1065 * t420 - 0.3D1 * t1089 * t784 
     #- 0.4D1 * t184 * t1060 + 0.2D1 * t184 * t10 * x2 - t115 * t420 - 0
     #.2D1 * t1065 * t784 - 0.2D1 * t184 - 0.2D1 * t1089 + t784 + t27 + 
     #t420
      t1120 = t433 - x1 + 0.2D1 * t1065 * t797 - t1061 + 0.4D1 * t1089 *
     # x1 + 0.4D1 * t184 * z + 0.2D1 * t184 * x2 - 0.2D1 * t184 * t10 + 
     #0.2D1 * t1065 * z + t712 * t10 - t115 * t183 + 0.2D1 * t1065 * x1 
     #- 0.5D1 * t434 - z - t797
      t1122 = 0.1D1 / (t1102 + t1120)
      t1123 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t715, x4)
      t1125 = t1122 * t1123 * t803
      t1128 = FJET(XB1, XB2, s, t1059, -t1071, -t1072, t1078, t794, -t79
     #6 * t1125 / 0.8D1)
      t1130 = t9 * t421
      t1134 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t715, x4)
      t1136 = t1122 * t1134 * t803
      t1139 = FJET(XB1, XB2, s, t1078, -t1072, -t1071, t1059, t794, -t79
     #6 * t1136 / 0.8D1)
      t1144 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1145 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1155 = -t1145 - t1145 * t443
      t1172 = t9 * t1145
      t1180 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0,
     # x4)
      t1196 = (0.90D2 * t113 * t8 * (-(t1144 - t429 * t1145) * t443 - t1
     #144 + t450 * t1145) - 0.180D3 * t139 * t9 * t1155) * t74 * t208 / 
     #0.1440D4 + t211 * t1155 * t74 * t214 / 0.8D1 + (0.90D2 * t113 * t8
     # * (t471 * t1145 - t1144) + 0.180D3 * t139 * t1172) * t147 * t208 
     #/ 0.720D3 - (0.90D2 * t113 * t8 * (-t489 * t1144 + t1180 + t491 * 
     #t1145 / 0.2D1) - 0.180D3 * t139 * t9 * (t1144 - t489 * t1145) + t1
     #76 * t1172) * t208 / 0.1440D4
      t1197 = FJET(XB1, XB2, s, -t415, t413, 0.0D0, 0.0D0, 0.0D0, t1196)
      t1199 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1200 = t799 * t1199
      t1204 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1218 = -t796 * t1200 * t803 / 0.8D1 + (0.90D2 * t113 * t8 * (-t80
     #7 * t1204 + t816 * t1200) + 0.180D3 * t745 * t795 * t1200) * t147 
     #* t208 / 0.720D3
      t1219 = FJET(XB1, XB2, s, -t415, -t789, 0.0D0, -t786, t794, t1218)
      t1221 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, 
     #x4)
      t1227 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t715, 
     #x4)
      t1240 = -t754 * t1221 * t74 * t214 / 0.8D1 - (0.90D2 * t113 * t8 *
     # (-t738 * t1221 + t726 * t1227) - 0.180D3 * t745 * t746 * t1221) *
     # t74 * t147 / 0.1440D4
      t1241 = FJET(XB1, XB2, s, -t718, 0.0D0, t716, 0.0D0, 0.0D0, t1240)
      t1243 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1244 = t799 * t1243
      t1248 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1262 = -t796 * t1244 * t803 / 0.8D1 + (0.90D2 * t113 * t8 * (-t80
     #7 * t1248 + t816 * t1244) + 0.180D3 * t745 * t795 * t1244) * t147 
     #* t208 / 0.720D3
      t1263 = FJET(XB1, XB2, s, -t789, -t415, -t786, 0.0D0, t794, t1262)
      t1265 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1266 = t799 * t1265
      t1270 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1284 = -t796 * t1266 * t803 / 0.8D1 + (0.90D2 * t113 * t8 * (-t80
     #7 * t1270 + t816 * t1266) + 0.180D3 * t745 * t795 * t1266) * t147 
     #* t208 / 0.720D3
      t1285 = FJET(XB1, XB2, s, -t786, 0.0D0, -t789, -t415, t794, t1284)
      t1287 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t715, x4)
      t1289 = t1122 * t1287 * t803
      t1292 = FJET(XB1, XB2, s, -t1071, t1059, t1078, -t1072, t794, -t79
     #6 * t1289 / 0.8D1)
      t1297 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t715, x4)
      t1299 = t1122 * t1297 * t803
      t1302 = FJET(XB1, XB2, s, -t1072, t1078, t1059, -t1071, t794, -t79
     #6 * t1299 / 0.8D1)
      rrgq2qght7s2e0 = t264 * t263 + t411 * t410 + t508 * t507 + t563 * 
     #t562 + t710 * t709 + t760 * t759 + t782 * t781 + t830 * t829 + t97
     #7 * t976 + t1032 * t1031 + t1054 * t1053 - t1128 * 0.3141592653589
     #793D1 * t1130 * t1125 / 0.8D1 - t1139 * 0.3141592653589793D1 * t11
     #30 * t1136 / 0.8D1 + t1197 * t1196 + t1219 * t1218 + t1241 * t1240
     # + t1263 * t1262 + t1285 * t1284 - t1292 * 0.3141592653589793D1 * 
     #t1130 * t1289 / 0.8D1 - t1302 * 0.3141592653589793D1 * t1130 * t12
     #99 / 0.8D1

      end function



      doubleprecision function rrgq2qght7s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * 0.3141592653589793D1
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
      t34 = 0.1D1 / (-0.1D1 - t27 + x3 + 0.2D1 * t28 * t30)
      t38 = log(0.4D1 * t13 * t19)
      t40 = t9 * (t25 * t34 + t38)
      t45 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t51 = t8 * (-t34 - 0.1D1)
      t54 = 0.1D1 / x3
      t60 = log(0.4D1 * t12 * t16 * t18)
      t66 = rrgq2qgh71J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t68 = t60 ** 2
      t74 = lh ** 2
      t76 = 0.3141592653589793D1 ** 2
      t78 = 0.180D3 * t74 - 0.30D2 * t76
      t84 = 0.3141592653589793D1 * t5
      t85 = t84 * t8
      t86 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t87 = t3 * t34
      t90 = 0.1D1 / x2
      t94 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t95 = x2 ** 2
      t96 = t12 * t95
      t97 = -0.1D1 + x2
      t101 = log(-0.4D1 * t96 * t19 * t97)
      t105 = log(0.4D1 * t96 * t19)
      t111 = 0.3141592653589793D1 * lh
      t112 = t86 - t3
      t121 = 0.1D1 / x1
      t125 = x1 ** 2
      t126 = t125 * t16
      t130 = log(0.4D1 * t126 * t12 * t18)
      t147 = -(0.90D2 * t4 * t40 + (-0.180D3 * t4 * lh + 0.90D2 * t45 * 
     #0.3141592653589793D1) * t5 * t51) * t54 / 0.2880D4 + (-0.180D3 * (
     #t45 - t60 * t3) * 0.3141592653589793D1 * lh + 0.90D2 * (t66 - t60 
     #* t45 + t68 * t3 / 0.2D1) * 0.3141592653589793D1 + t4 * t78) * t5 
     #* t8 / 0.2880D4 - t85 * (t86 - t87 - t3) * t54 * t90 / 0.16D2 - (0
     #.90D2 * t84 * t8 * (t94 - t45 - t101 * t86 + t105 * t3) - 0.180D3 
     #* t111 * t9 * t112) * t90 / 0.1440D4 - t85 * t112 * t90 * t121 / 0
     #.8D1 - (0.90D2 * t84 * t8 * (-t45 + t130 * t3) + 0.180D3 * t111 * 
     #t9 * t3) * t121 / 0.1440D4 + t85 * (t3 + t87) * t54 * t121 / 0.16D
     #2
      t148 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t147)
      t150 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t151 = t150 * 0.3141592653589793D1
      t156 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t173 = rrgq2qgh73J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t182 = t150 * t34
      t183 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t190 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t196 = t183 - t150
      t224 = -(0.90D2 * t151 * t40 + (-0.180D3 * t151 * lh + 0.90D2 * t1
     #56 * 0.3141592653589793D1) * t5 * t51) * t54 / 0.2880D4 + (-0.180D
     #3 * (t156 - t60 * t150) * 0.3141592653589793D1 * lh + 0.90D2 * (t6
     #8 * t150 / 0.2D1 - t60 * t156 + t173) * 0.3141592653589793D1 + t15
     #1 * t78) * t5 * t8 / 0.2880D4 - t85 * (-t182 - t150 + t183) * t54 
     #* t90 / 0.16D2 - (0.90D2 * t84 * t8 * (-t156 - t101 * t183 + t190 
     #+ t105 * t150) - 0.180D3 * t111 * t9 * t196) * t90 / 0.1440D4 - t8
     #5 * t196 * t90 * t121 / 0.8D1 - (0.90D2 * t84 * t8 * (-t156 + t130
     # * t150) + 0.180D3 * t111 * t9 * t150) * t121 / 0.1440D4 + t85 * (
     #t150 + t182) * t54 * t121 / 0.16D2
      t225 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t224)
      t227 = t2 * x1
      t228 = -0.1D1 + x1
      t229 = t2 * t228
      t230 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t235 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t238 = x1 * z
      t239 = -z - x1 + t238
      t240 = 0.1D1 / t239
      t242 = t228 ** 2
      t246 = log(-0.4D1 * t126 / t10 * t18 * t240 * t242)
      t258 = x3 * x1
      t264 = Sqrt(x3 * t239 * t20)
      t268 = 0.1D1 / (0.2D1 * t258 * z - 0.2D1 * t258 - t27 + x3 - 0.1D1
     # + 0.2D1 * t28 * t264)
      t275 = -t85 * t230 * t90 * t121 / 0.8D1 - (0.90D2 * t84 * t8 * (t2
     #35 - t246 * t230) - 0.180D3 * t111 * t9 * t230) * t121 / 0.1440D4 
     #+ t85 * (-t230 - t230 * t268) * t54 * t121 / 0.16D2
      t276 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t227, -t229, 0.0D0, t275)
      t278 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t284 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t301 = -t85 * t278 * t90 * t121 / 0.8D1 - (0.90D2 * t84 * t8 * (-t
     #246 * t278 + t284) - 0.180D3 * t111 * t9 * t278) * t121 / 0.1440D4
     # + t85 * (-t278 * t268 - t278) * t54 * t121 / 0.16D2
      t302 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t229, t227, 0.0D0, t301)
      t304 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t305 = t304 * 0.3141592653589793D1
      t310 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t325 = rrgq2qgh72J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t336 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t337 = t304 * t34
      t343 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t350 = t336 - t304
      t378 = -(0.90D2 * t305 * t40 + (-0.180D3 * t305 * lh + 0.90D2 * t3
     #10 * 0.3141592653589793D1) * t5 * t51) * t54 / 0.2880D4 + (-0.180D
     #3 * (t310 - t60 * t304) * 0.3141592653589793D1 * lh + 0.90D2 * (-t
     #60 * t310 + t325 + t68 * t304 / 0.2D1) * 0.3141592653589793D1 + t3
     #05 * t78) * t5 * t8 / 0.2880D4 - t85 * (t336 - t337 - t304) * t54 
     #* t90 / 0.16D2 - (0.90D2 * t84 * t8 * (t343 - t101 * t336 - t310 +
     # t105 * t304) - 0.180D3 * t111 * t9 * t350) * t90 / 0.1440D4 - t85
     # * t350 * t90 * t121 / 0.8D1 - (0.90D2 * t84 * t8 * (-t310 + t130 
     #* t304) + 0.180D3 * t111 * t9 * t304) * t121 / 0.1440D4 + t85 * (t
     #304 + t337) * t54 * t121 / 0.16D2
      t379 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t378)
      t381 = x2 * x3
      t383 = 0.1D1 / (0.1D1 - x3 + t381)
      t384 = t381 * t383
      t385 = t2 * t384
      t387 = t2 * t20 * t383
      t391 = Sqrt(t26 * t97 * t20)
      t395 = 0.1D1 / (t381 * z - t27 - 0.1D1 + x3 + 0.2D1 * t28 * t391)
      t396 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t384, x
     #4)
      t398 = t54 * t90
      t399 = t395 * t396 * t398
      t402 = FJET(XB1, XB2, s, 0.0D0, t385, 0.0D0, -t387, 0.0D0, -t85 * 
     #t399 / 0.16D2)
      t407 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t384, x
     #4)
      t409 = t395 * t407 * t398
      t412 = FJET(XB1, XB2, s, 0.0D0, -t387, 0.0D0, t385, 0.0D0, -t85 * 
     #t409 / 0.16D2)
      t417 = x2 * x1
      t419 = t2 * t417 * t240
      t422 = t97 * s * t1 * x1
      t427 = s * t17 * x2 * x1 * t228 * t240
      t429 = t84 * t8 * t239
      t432 = 0.1D1 / (z + x1 - t238 - t417 + t417 * z)
      t433 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t435 = t90 * t121
      t439 = FJET(XB1, XB2, s, 0.0D0, -t419, -t229, -t422, t427, -t429 *
     # t432 * t433 * t435 / 0.8D1)
      t442 = t239 * t432
      t448 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t449 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t456 = rrgq2qgh74J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t462 = t449 * 0.3141592653589793D1
      t468 = t449 * t34
      t469 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t475 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t482 = t469 - t449
      t522 = (-0.180D3 * (t448 - t60 * t449) * 0.3141592653589793D1 * lh
     # + 0.90D2 * (-t60 * t448 + t456 + t68 * t449 / 0.2D1) * 0.31415926
     #53589793D1 + t462 * t78) * t5 * t8 / 0.2880D4 - t85 * (-t468 - t44
     #9 + t469) * t54 * t90 / 0.16D2 - (0.90D2 * t84 * t8 * (t475 - t448
     # - t101 * t469 + t105 * t449) - 0.180D3 * t111 * t9 * t482) * t90 
     #/ 0.1440D4 - (0.90D2 * t462 * t40 + (-0.180D3 * t462 * lh + 0.90D2
     # * t448 * 0.3141592653589793D1) * t5 * t51) * t54 / 0.2880D4 - t85
     # * t482 * t90 * t121 / 0.8D1 - (0.90D2 * t84 * t8 * (-t448 + t130 
     #* t449) + 0.180D3 * t111 * t9 * t449) * t121 / 0.1440D4 + t85 * (t
     #449 + t468) * t54 * t121 / 0.16D2
      t523 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t522)
      t525 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t530 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t548 = -t85 * t525 * t90 * t121 / 0.8D1 - (0.90D2 * t84 * t8 * (t5
     #30 - t246 * t525) - 0.180D3 * t111 * t9 * t525) * t121 / 0.1440D4 
     #+ t85 * (-t525 - t525 * t268) * t54 * t121 / 0.16D2
      t549 = FJET(XB1, XB2, s, t227, -t229, 0.0D0, 0.0D0, 0.0D0, t548)
      t551 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t384, x
     #4)
      t553 = t395 * t551 * t398
      t556 = FJET(XB1, XB2, s, t385, 0.0D0, -t387, 0.0D0, 0.0D0, -t85 * 
     #t553 / 0.16D2)
      t561 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t566 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t584 = -t85 * t561 * t90 * t121 / 0.8D1 - (0.90D2 * t84 * t8 * (t5
     #66 - t246 * t561) - 0.180D3 * t111 * t9 * t561) * t121 / 0.1440D4 
     #+ t85 * (-t561 - t561 * t268) * t54 * t121 / 0.16D2
      t585 = FJET(XB1, XB2, s, -t229, t227, 0.0D0, 0.0D0, 0.0D0, t584)
      t587 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t592 = FJET(XB1, XB2, s, -t229, -t422, 0.0D0, -t419, t427, -t429 *
     # t432 * t587 * t435 / 0.8D1)
      t600 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t384, x
     #4)
      t602 = t395 * t600 * t398
      t605 = FJET(XB1, XB2, s, -t387, 0.0D0, t385, 0.0D0, 0.0D0, -t85 * 
     #t602 / 0.16D2)
      t610 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t615 = FJET(XB1, XB2, s, -t422, -t229, -t419, 0.0D0, t427, -t429 *
     # t432 * t610 * t435 / 0.8D1)
      t623 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t628 = FJET(XB1, XB2, s, -t419, 0.0D0, -t422, -t229, t427, -t429 *
     # t432 * t623 * t435 / 0.8D1)
      rrgq2qght7s2em1 = t148 * t147 + t225 * t224 + t276 * t275 + t302 *
     # t301 + t379 * t378 - t402 * 0.3141592653589793D1 * t9 * t399 / 0.
     #16D2 - t412 * 0.3141592653589793D1 * t9 * t409 / 0.16D2 - t439 * 0
     #.3141592653589793D1 * t9 * t442 * t433 * t90 * t121 / 0.8D1 + t523
     # * t522 + t549 * t548 - t556 * 0.3141592653589793D1 * t9 * t553 / 
     #0.16D2 + t585 * t584 - t592 * 0.3141592653589793D1 * t9 * t442 * t
     #587 * t90 * t121 / 0.8D1 - t605 * 0.3141592653589793D1 * t9 * t602
     # / 0.16D2 - t615 * 0.3141592653589793D1 * t9 * t442 * t610 * t90 *
     # t121 / 0.8D1 - t628 * 0.3141592653589793D1 * t9 * t442 * t623 * t
     #90 * t121 / 0.8D1

      end function



      doubleprecision function rrgq2qght7s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t4 = t3 * 0.3141592653589793D1
      t5 = 0.1D1 / t1
      t7 = s ** 2
      t9 = 0.1D1 / t7 / s
      t10 = x3 * z
      t12 = x4 * 0.3141592653589793D1
      t13 = cos(t12)
      t16 = Sqrt(-t10 * (-0.1D1 + x3))
      t24 = t9 * (-0.1D1 / (-0.1D1 - 0.2D1 * t10 + x3 + 0.2D1 * t13 * t1
     #6) - 0.1D1) / x3
      t27 = 0.3141592653589793D1 * t5
      t28 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t31 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t37 = t5 * t9 * t36
      t42 = rrgq2qgh71J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t43 = z ** 2
      t46 = Sin(t12)
      t47 = t46 ** 2
      t49 = t1 ** 2
      t50 = t49 ** 2
      t53 = log(0.4D1 / t43 / z * t47 * t50)
      t62 = -t4 * t5 * t24 / 0.32D2 - t27 * t9 * (t28 - t3) * t31 / 0.16
     #D2 + t4 * t37 / 0.16D2 + (-0.180D3 * t4 * lh + 0.90D2 * (t42 - t53
     # * t3) * 0.3141592653589793D1) * t5 * t9 / 0.2880D4
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t62)
      t65 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t66 = t65 * 0.3141592653589793D1
      t70 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t80 = rrgq2qgh73J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t89 = -t66 * t5 * t24 / 0.32D2 - t27 * t9 * (t70 - t65) * t31 / 0.
     #16D2 + t66 * t37 / 0.16D2 + (-0.180D3 * t66 * lh + 0.90D2 * (t80 -
     # t53 * t65) * 0.3141592653589793D1) * t5 * t9 / 0.2880D4
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t89)
      t92 = t2 * x1
      t94 = t2 * (-0.1D1 + x1)
      t95 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, x
     #4)
      t97 = t9 * t95 * t36
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t92, -t94, 0.0D0, -t27 * t9
     #7 / 0.16D2)
      t105 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t107 = t9 * t105 * t36
      t110 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t94, t92, 0.0D0, -t27 * t1
     #07 / 0.16D2)
      t115 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t116 = t115 * 0.3141592653589793D1
      t120 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t130 = rrgq2qgh72J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t139 = -t116 * t5 * t24 / 0.32D2 - t27 * t9 * (t120 - t115) * t31 
     #/ 0.16D2 + t116 * t37 / 0.16D2 + (-0.180D3 * t116 * lh + 0.90D2 * 
     #(t130 - t53 * t115) * 0.3141592653589793D1) * t5 * t9 / 0.2880D4
      t140 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t139)
      t142 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t143 = t142 * 0.3141592653589793D1
      t147 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t159 = rrgq2qgh74J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t168 = -t143 * t5 * t24 / 0.32D2 - t27 * t9 * (t147 - t142) * t31 
     #/ 0.16D2 + t27 * t9 * t142 * t36 / 0.16D2 + (-0.180D3 * t143 * lh 
     #+ 0.90D2 * (t159 - t53 * t142) * 0.3141592653589793D1) * t5 * t9 /
     # 0.2880D4
      t169 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t168)
      t171 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t173 = t9 * t171 * t36
      t176 = FJET(XB1, XB2, s, t92, -t94, 0.0D0, 0.0D0, 0.0D0, -t27 * t1
     #73 / 0.16D2)
      t181 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t183 = t9 * t181 * t36
      t186 = FJET(XB1, XB2, s, -t94, t92, 0.0D0, 0.0D0, 0.0D0, -t27 * t1
     #83 / 0.16D2)
      rrgq2qght7s2em2 = t63 * t62 + t90 * t89 - t100 * 0.314159265358979
     #3D1 * t5 * t97 / 0.16D2 - t110 * 0.3141592653589793D1 * t5 * t107 
     #/ 0.16D2 + t140 * t139 + t169 * t168 - t176 * 0.3141592653589793D1
     # * t5 * t173 / 0.16D2 - t186 * 0.3141592653589793D1 * t5 * t183 / 
     #0.16D2

      end function



      doubleprecision function rrgq2qght7s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh71J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t3 * 0.314
     #1592653589793D1 * t9 / 0.32D2)
      t14 = 0.3141592653589793D1 * t5
      t15 = t14 * t8
      t17 = rrgq2qgh73J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t21 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t17 * 0.31
     #41592653589793D1 * t9 / 0.32D2)
      t24 = rrgq2qgh72J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t24 * 0.31
     #41592653589793D1 * t9 / 0.32D2)
      t31 = rrgq2qgh74J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t35 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t14 * t8 *
     # t31 / 0.32D2)
      rrgq2qght7s2em3 = t12 * t3 * t15 / 0.32D2 + t21 * t17 * t15 / 0.32
     #D2 + t28 * t24 * t15 / 0.32D2 + t35 * 0.3141592653589793D1 * t9 * 
     #t31 / 0.32D2

      end function



      doubleprecision function rrgq2qght7s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgq2qght7s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh71J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * x1
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t7 = t6 ** 2
      t10 = x2 ** 2
      t13 = x1 ** 2
      t14 = t13 * x1
      t15 = 0.1D1 - x1
      t16 = t15 ** 2
      t17 = t16 * t15
      t19 = z + t2
      t20 = t19 ** 2
      t22 = 0.1D1 / t20 / t19
      t26 = t5 * t6 * t1
      t28 = 0.1D1 / t20
      t29 = 0.1D1 - x3
      t30 = 0.1D1 - x2
      t35 = cos(x4 * 0.3141592653589793D1)
      t40 = Sqrt(x3 * t30 * t19 * x2 * t29)
      t43 = t29 * t30 * t19 + x2 * x3 + 0.2D1 * t35 * t40
      t44 = t43 ** 2
      t50 = t5 * t7
      t51 = t13 * t28
      t59 = t5 * t7 * t1
      t72 = 0.1D1 / t19
      t74 = t29 ** 2
      rrgq2qgh71J1 = t2 * wd * (-t5 * t7 * t6 * t10 * x2 * t14 * t17 * t
     #22 - 0.2D1 * t26 * t13 * t28 * t44 * t15 * t29 + 0.4D1 * t50 * t51
     # * t43 * x2 * t16 * t29 + 0.2D1 * t59 * t14 * t22 * t43 * t10 * t1
     #6 - 0.3D1 * t59 * t17 * t29 * t10 * t51 + 0.2D1 * t26 * x1 * t72 *
     # t43 * t16 * t74 - 0.2D1 * t50 * t14 * t22 * t44 * x2 * t15 - 0.3D
     #1 * t50 * t17 * t74 * x2 * x1 * t72 - t26 * t17 * t74 * t29) / z /
     # 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh71J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t7 = t6 ** 2
      t10 = x2 ** 2
      t13 = x1 ** 2
      t14 = t13 * x1
      t15 = 0.1D1 - x1
      t16 = t15 ** 2
      t17 = t16 * t15
      t20 = z + t1 * x1
      t21 = t20 ** 2
      t23 = 0.1D1 / t21 / t20
      t27 = t5 * t6 * t1
      t28 = t27 * t13
      t29 = 0.1D1 / t21
      t30 = 0.1D1 - x3
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t41 = Sqrt(x3 * t31 * t20 * x2 * t30)
      t44 = t30 * t31 * t20 + x2 * x3 + 0.2D1 * t36 * t41
      t45 = t44 ** 2
      t47 = t15 * t30
      t49 = t28 * t29 * t45 * t47
      t51 = t5 * t7
      t52 = t13 * t29
      t60 = t5 * t7 * t1
      t73 = 0.1D1 / t20
      t74 = t73 * t44
      t75 = t30 ** 2
      t82 = x2 * t15
      t84 = t51 * t14 * t23 * t45 * t82
      t101 = t5 * t6
      rrgq2qgh71J2 = -(-t2 * t3 * (-t5 * t7 * t6 * t10 * x2 * t14 * t17 
     #* t23 - 0.2D1 * t49 + 0.4D1 * t51 * t52 * t44 * x2 * t16 * t30 + 0
     #.2D1 * t60 * t14 * t23 * t44 * t10 * t16 - 0.3D1 * t60 * t17 * t30
     # * t10 * t52 + 0.2D1 * t27 * x1 * t74 * t16 * t75 - 0.2D1 * t84 - 
     #0.3D1 * t51 * t17 * t75 * x2 * x1 * t73 - t27 * t17 * t75 * t30) -
     # t2 * t3 * (0.2D1 * t49 + 0.2D1 * t28 * t29 * t44 * t82 + 0.2D1 * 
     #t101 * t15 * t30 * x1 * t74 - 0.2D1 * t5 * t1 * t47 - 0.2D1 * t101
     # * x2 * x1 * t15 * t73 + 0.2D1 * t84)) / s / z / 0.314159265358979
     #3D1

      end function
  
   
 

      doubleprecision function rrgq2qgh71J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t7 = t6 ** 2
      t10 = x2 ** 2
      t13 = x1 ** 2
      t14 = t13 * x1
      t15 = 0.1D1 - x1
      t16 = t15 ** 2
      t17 = t16 * t15
      t20 = z + t1 * x1
      t21 = t20 ** 2
      t23 = 0.1D1 / t21 / t20
      t27 = t5 * t6 * t1
      t28 = t27 * t13
      t29 = 0.1D1 / t21
      t30 = 0.1D1 - x3
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t41 = Sqrt(x3 * t31 * t20 * x2 * t30)
      t44 = t30 * t31 * t20 + x2 * x3 + 0.2D1 * t36 * t41
      t45 = t44 ** 2
      t47 = t15 * t30
      t49 = t28 * t29 * t45 * t47
      t51 = t5 * t7
      t52 = t13 * t29
      t60 = t5 * t7 * t1
      t73 = 0.1D1 / t20
      t74 = t73 * t44
      t75 = t30 ** 2
      t82 = x2 * t15
      t84 = t51 * t14 * t23 * t45 * t82
      t101 = t5 * t6
      rrgq2qgh71J3 = -(-t2 * t3 * (-t5 * t7 * t6 * t10 * x2 * t14 * t17 
     #* t23 - 0.2D1 * t49 + 0.4D1 * t51 * t52 * t44 * x2 * t16 * t30 + 0
     #.2D1 * t60 * t14 * t23 * t44 * t10 * t16 - 0.3D1 * t60 * t17 * t30
     # * t10 * t52 + 0.2D1 * t27 * x1 * t74 * t16 * t75 - 0.2D1 * t84 - 
     #0.3D1 * t51 * t17 * t75 * x2 * x1 * t73 - t27 * t17 * t75 * t30) -
     # t2 * t3 * (0.2D1 * t49 + 0.2D1 * t28 * t29 * t44 * t82 + 0.2D1 * 
     #t101 * t15 * t30 * x1 * t74 - 0.2D1 * t5 * t1 * t47 - 0.2D1 * t101
     # * x2 * x1 * t15 * t73 + 0.2D1 * t84)) / s / z / 0.314159265358979
     #3D1

      end function
  
   
 

      doubleprecision function rrgq2qgh71J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t7 = t6 ** 2
      t10 = x2 ** 2
      t13 = x1 ** 2
      t14 = t13 * x1
      t15 = 0.1D1 - x1
      t16 = t15 ** 2
      t17 = t16 * t15
      t20 = z + t1 * x1
      t21 = t20 ** 2
      t23 = 0.1D1 / t21 / t20
      t27 = t5 * t6 * t1
      t28 = t27 * t13
      t29 = 0.1D1 / t21
      t30 = 0.1D1 - x3
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t41 = Sqrt(x3 * t31 * t20 * x2 * t30)
      t44 = t30 * t31 * t20 + x2 * x3 + 0.2D1 * t36 * t41
      t45 = t44 ** 2
      t47 = t15 * t30
      t49 = t28 * t29 * t45 * t47
      t51 = t5 * t7
      t52 = t13 * t29
      t60 = t5 * t7 * t1
      t73 = 0.1D1 / t20
      t74 = t73 * t44
      t75 = t30 ** 2
      t82 = x2 * t15
      t84 = t51 * t14 * t23 * t45 * t82
      t101 = t5 * t6
      rrgq2qgh71J4 = -(-t2 * t3 * (-t5 * t7 * t6 * t10 * x2 * t14 * t17 
     #* t23 - 0.2D1 * t49 + 0.4D1 * t51 * t52 * t44 * x2 * t16 * t30 + 0
     #.2D1 * t60 * t14 * t23 * t44 * t10 * t16 - 0.3D1 * t60 * t17 * t30
     # * t10 * t52 + 0.2D1 * t27 * x1 * t74 * t16 * t75 - 0.2D1 * t84 - 
     #0.3D1 * t51 * t17 * t75 * x2 * x1 * t73 - t27 * t17 * t75 * t30) -
     # t2 * t3 * (0.2D1 * t49 + 0.2D1 * t28 * t29 * t44 * t82 + 0.2D1 * 
     #t101 * t15 * t30 * x1 * t74 - 0.2D1 * t5 * t1 * t47 - 0.2D1 * t101
     # * x2 * x1 * t15 * t73 + 0.2D1 * t84)) / s / z / 0.314159265358979
     #3D1

      end function
  
   
 

      doubleprecision function rrgq2qgh71J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t7 = t6 ** 2
      t10 = x2 ** 2
      t13 = x1 ** 2
      t14 = t13 * x1
      t15 = 0.1D1 - x1
      t16 = t15 ** 2
      t17 = t16 * t15
      t20 = z + t1 * x1
      t21 = t20 ** 2
      t23 = 0.1D1 / t21 / t20
      t27 = t5 * t6 * t1
      t28 = t27 * t13
      t29 = 0.1D1 / t21
      t30 = 0.1D1 - x3
      t31 = 0.1D1 - x2
      t36 = cos(x4 * 0.3141592653589793D1)
      t41 = Sqrt(x3 * t31 * t20 * x2 * t30)
      t44 = t30 * t31 * t20 + x2 * x3 + 0.2D1 * t36 * t41
      t45 = t44 ** 2
      t47 = t15 * t30
      t49 = t28 * t29 * t45 * t47
      t51 = t5 * t7
      t52 = t13 * t29
      t60 = t5 * t7 * t1
      t73 = 0.1D1 / t20
      t74 = t73 * t44
      t75 = t30 ** 2
      t82 = x2 * t15
      t84 = t51 * t14 * t23 * t45 * t82
      t101 = t5 * t6
      rrgq2qgh71J5 = -(-t2 * t3 * (-t5 * t7 * t6 * t10 * x2 * t14 * t17 
     #* t23 - 0.2D1 * t49 + 0.4D1 * t51 * t52 * t44 * x2 * t16 * t30 + 0
     #.2D1 * t60 * t14 * t23 * t44 * t10 * t16 - 0.3D1 * t60 * t17 * t30
     # * t10 * t52 + 0.2D1 * t27 * x1 * t74 * t16 * t75 - 0.2D1 * t84 - 
     #0.3D1 * t51 * t17 * t75 * x2 * x1 * t73 - t27 * t17 * t75 * t30) -
     # t2 * t3 * (0.2D1 * t49 + 0.2D1 * t28 * t29 * t44 * t82 + 0.2D1 * 
     #t101 * t15 * t30 * x1 * t74 - 0.2D1 * t5 * t1 * t47 - 0.2D1 * t101
     # * x2 * x1 * t15 * t73 + 0.2D1 * t84)) / s / z / 0.314159265358979
     #3D1

      end function
  
   
 

      doubleprecision function rrgq2qgh71J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * x1
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t9 = x1 ** 2
      t10 = t5 * t6 * t1 * t9
      t11 = z + t2
      t12 = t11 ** 2
      t13 = 0.1D1 / t12
      t14 = 0.1D1 - x3
      t15 = 0.1D1 - x2
      t20 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(x3 * t15 * t11 * x2 * t14)
      t28 = t14 * t15 * t11 + x2 * x3 + 0.2D1 * t20 * t25
      t29 = t28 ** 2
      t31 = 0.1D1 - x1
      t32 = t31 * t14
      t36 = x2 * t31
      t39 = t5 * t6
      t42 = 0.1D1 / t11
      t52 = t6 ** 2
      rrgq2qgh71J6 = t2 * wd * (0.2D1 * t10 * t13 * t29 * t32 + 0.2D1 * 
     #t10 * t13 * t28 * t36 + 0.2D1 * t39 * t31 * t14 * x1 * t42 * t28 -
     # 0.2D1 * t5 * t1 * t32 - 0.2D1 * t39 * x2 * x1 * t31 * t42 + 0.2D1
     # * t5 * t52 * t9 * x1 / t12 / t11 * t29 * t36) / z / 0.31415926535
     #89793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh72J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * x1
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t9 = x1 ** 2
      t11 = z + t2
      t12 = t11 ** 2
      t13 = 0.1D1 / t12
      t14 = 0.1D1 - x3
      t15 = 0.1D1 - x2
      t20 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(x3 * t15 * t11 * x2 * t14)
      t28 = t14 * t15 * t11 + x2 * x3 + 0.2D1 * t20 * t25
      t30 = 0.1D1 - x1
      t35 = t6 ** 2
      t37 = x2 ** 2
      t39 = t30 ** 2
      t46 = t28 ** 2
      rrgq2qgh72J1 = t2 * wd * (-0.8D1 * t5 * t6 * t1 * t9 * t13 * t28 *
     # x2 * t30 + 0.4D1 * t5 * t35 * t37 * t9 * t39 * t13 + 0.4D1 * t5 *
     # t6 * t9 * t13 * t46 + 0.4D1 * t5) / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh72J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t9 = x1 ** 2
      t12 = z + t1 * x1
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x3
      t16 = 0.1D1 - x2
      t21 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(x3 * t16 * t12 * x2 * t15)
      t29 = t15 * t16 * t12 + x2 * x3 + 0.2D1 * t21 * t26
      t31 = 0.1D1 - x1
      t34 = t5 * t6 * t1 * t9 * t14 * t29 * x2 * t31
      t36 = t6 ** 2
      t38 = x2 ** 2
      t40 = t31 ** 2
      t44 = 0.4D1 * t5 * t36 * t38 * t9 * t40 * t14
      t45 = t5 * t6
      t47 = t29 ** 2
      t50 = 0.4D1 * t45 * t9 * t14 * t47
      t51 = 0.4D1 * t5
      t56 = 0.1D1 / t12
      rrgq2qgh72J2 = -(-t2 * t3 * (-0.8D1 * t34 + t44 + t50 + t51) - t2 
     #* t3 * (0.8D1 * t5 * t1 * x1 * t56 * t29 - t50 - t44 + 0.12D2 * t3
     #4 - 0.12D2 * t45 * x2 * x1 * t31 * t56 - t51)) / s / z / 0.3141592
     #653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh72J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t9 = x1 ** 2
      t12 = z + t1 * x1
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x3
      t16 = 0.1D1 - x2
      t21 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(x3 * t16 * t12 * x2 * t15)
      t29 = t15 * t16 * t12 + x2 * x3 + 0.2D1 * t21 * t26
      t31 = 0.1D1 - x1
      t34 = t5 * t6 * t1 * t9 * t14 * t29 * x2 * t31
      t36 = t6 ** 2
      t38 = x2 ** 2
      t40 = t31 ** 2
      t44 = 0.4D1 * t5 * t36 * t38 * t9 * t40 * t14
      t45 = t5 * t6
      t47 = t29 ** 2
      t50 = 0.4D1 * t45 * t9 * t14 * t47
      t51 = 0.4D1 * t5
      t56 = 0.1D1 / t12
      rrgq2qgh72J3 = -(-t2 * t3 * (-0.8D1 * t34 + t44 + t50 + t51) - t2 
     #* t3 * (0.8D1 * t5 * t1 * x1 * t56 * t29 - t50 - t44 + 0.12D2 * t3
     #4 - 0.12D2 * t45 * x2 * x1 * t31 * t56 - t51)) / s / z / 0.3141592
     #653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh72J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t9 = x1 ** 2
      t12 = z + t1 * x1
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x3
      t16 = 0.1D1 - x2
      t21 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(x3 * t16 * t12 * x2 * t15)
      t29 = t15 * t16 * t12 + x2 * x3 + 0.2D1 * t21 * t26
      t31 = 0.1D1 - x1
      t34 = t5 * t6 * t1 * t9 * t14 * t29 * x2 * t31
      t36 = t6 ** 2
      t38 = x2 ** 2
      t40 = t31 ** 2
      t44 = 0.4D1 * t5 * t36 * t38 * t9 * t40 * t14
      t45 = t5 * t6
      t47 = t29 ** 2
      t50 = 0.4D1 * t45 * t9 * t14 * t47
      t51 = 0.4D1 * t5
      t56 = 0.1D1 / t12
      rrgq2qgh72J4 = -(-t2 * t3 * (-0.8D1 * t34 + t44 + t50 + t51) - t2 
     #* t3 * (0.8D1 * t5 * t1 * x1 * t56 * t29 - t50 - t44 + 0.12D2 * t3
     #4 - 0.12D2 * t45 * x2 * x1 * t31 * t56 - t51)) / s / z / 0.3141592
     #653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh72J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * wd
      t4 = s ** 2
      t5 = t4 * s
      t6 = t1 ** 2
      t9 = x1 ** 2
      t12 = z + t1 * x1
      t13 = t12 ** 2
      t14 = 0.1D1 / t13
      t15 = 0.1D1 - x3
      t16 = 0.1D1 - x2
      t21 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(x3 * t16 * t12 * x2 * t15)
      t29 = t15 * t16 * t12 + x2 * x3 + 0.2D1 * t21 * t26
      t31 = 0.1D1 - x1
      t34 = t5 * t6 * t1 * t9 * t14 * t29 * x2 * t31
      t36 = t6 ** 2
      t38 = x2 ** 2
      t40 = t31 ** 2
      t44 = 0.4D1 * t5 * t36 * t38 * t9 * t40 * t14
      t45 = t5 * t6
      t47 = t29 ** 2
      t50 = 0.4D1 * t45 * t9 * t14 * t47
      t51 = 0.4D1 * t5
      t56 = 0.1D1 / t12
      rrgq2qgh72J5 = -(-t2 * t3 * (-0.8D1 * t34 + t44 + t50 + t51) - t2 
     #* t3 * (0.8D1 * t5 * t1 * x1 * t56 * t29 - t50 - t44 + 0.12D2 * t3
     #4 - 0.12D2 * t45 * x2 * x1 * t31 * t56 - t51)) / s / z / 0.3141592
     #653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh72J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 * x1
      t4 = s ** 2
      t5 = t4 * s
      t7 = z + t2
      t8 = 0.1D1 / t7
      t10 = 0.1D1 - x3
      t11 = 0.1D1 - x2
      t16 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(x3 * t11 * t7 * x2 * t10)
      t24 = t10 * t11 * t7 + x2 * x3 + 0.2D1 * t16 * t21
      t28 = t1 ** 2
      t29 = t5 * t28
      t30 = x1 ** 2
      t31 = t7 ** 2
      t32 = 0.1D1 / t31
      t34 = t24 ** 2
      t38 = t28 ** 2
      t40 = x2 ** 2
      t42 = 0.1D1 - x1
      t43 = t42 ** 2
      rrgq2qgh72J6 = t2 * wd * (0.8D1 * t5 * t1 * x1 * t8 * t24 - 0.4D1 
     #* t29 * t30 * t32 * t34 - 0.4D1 * t5 * t38 * t40 * t30 * t43 * t32
     # + 0.12D2 * t5 * t28 * t1 * t30 * t32 * t24 * x2 * t42 - 0.12D2 * 
     #t29 * x2 * x1 * t42 * t8 - 0.4D1 * t5) / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh73J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x2
      t9 = z + t1 * x1
      t10 = 0.1D1 / t9
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t17 = t14 * t15 * t1
      t18 = x1 ** 2
      t20 = t9 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(x3 * t4 * t9 * x2 * t22)
      t35 = t22 * t4 * t9 + x2 * x3 + 0.2D1 * t27 * t32
      t36 = t35 ** 2
      t42 = t15 ** 2
      t44 = t14 * t42 * t1
      t45 = t18 * x1
      t48 = 0.1D1 / t20 / t9
      t50 = x2 ** 2
      t51 = t2 ** 2
      t56 = t14 * t42
      t57 = t18 * t21
      t68 = t51 * t2
      t82 = t22 ** 2
      rrgq2qgh73J1 = t1 * t2 * (z + x1 * t4 * t1) * t10 * wd * (0.2D1 * 
     #t17 * t18 * t21 * t36 * t2 * t22 - 0.3D1 * t44 * t45 * t48 * t35 *
     # t50 * t51 + 0.4D1 * t56 * t57 * t35 * x2 * t51 * t22 - t14 * t42 
     #* t15 * t50 * x2 * t45 * t68 * t48 + 0.2D1 * t44 * t68 * t22 * t50
     # * t57 - t17 * t45 * t48 * t36 * t35 - 0.2D1 * t56 * t68 * t82 * x
     #2 * x1 * t10 - 0.2D1 * t17 * x1 * t10 * t35 * t51 * t82 - 0.3D1 * 
     #t56 * t45 * t48 * t36 * x2 * t2) / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh73J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t5 = 0.1D1 - x2
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * t5 * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t17 = t14 * t15 * t1
      t18 = x1 ** 2
      t20 = t10 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(x3 * t5 * t10 * x2 * t22)
      t35 = t22 * t5 * t10 + x2 * x3 + 0.2D1 * t27 * t32
      t36 = t35 ** 2
      t42 = t15 ** 2
      t44 = t14 * t42 * t1
      t45 = t18 * x1
      t48 = 0.1D1 / t20 / t10
      t50 = x2 ** 2
      t51 = t3 ** 2
      t56 = t14 * t42
      t57 = t18 * t21
      t68 = t51 * t3
      t82 = t22 ** 2
      t84 = x1 * t11
      t86 = t56 * t68 * t82 * x2 * t84
      t89 = t11 * t35
      t92 = t17 * x1 * t89 * t51 * t82
      t111 = t14 * t15
      rrgq2qgh73J2 = -(-t4 * t12 * wd * (0.2D1 * t17 * t18 * t21 * t36 *
     # t3 * t22 - 0.3D1 * t44 * t45 * t48 * t35 * t50 * t51 + 0.4D1 * t5
     #6 * t57 * t35 * x2 * t51 * t22 - t14 * t42 * t15 * t50 * x2 * t45 
     #* t68 * t48 + 0.2D1 * t44 * t68 * t22 * t50 * t57 - t17 * t45 * t4
     #8 * t36 * t35 - 0.2D1 * t86 - 0.2D1 * t92 - 0.3D1 * t56 * t45 * t4
     #8 * t36 * x2 * t3) - t4 * t12 * wd * (0.2D1 * t86 + 0.2D1 * t17 * 
     #t51 * t22 * x2 * t84 + 0.2D1 * t92 - 0.2D1 * t14 * t1 * t84 * t35 
     #- 0.2D1 * t111 * x2 * x1 * t3 * t11 + 0.2D1 * t111 * t3 * t22 * x1
     # * t89)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh73J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t5 = 0.1D1 - x2
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * t5 * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t17 = t14 * t15 * t1
      t18 = x1 ** 2
      t20 = t10 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(x3 * t5 * t10 * x2 * t22)
      t35 = t22 * t5 * t10 + x2 * x3 + 0.2D1 * t27 * t32
      t36 = t35 ** 2
      t42 = t15 ** 2
      t44 = t14 * t42 * t1
      t45 = t18 * x1
      t48 = 0.1D1 / t20 / t10
      t50 = x2 ** 2
      t51 = t3 ** 2
      t56 = t14 * t42
      t57 = t18 * t21
      t68 = t51 * t3
      t82 = t22 ** 2
      t84 = x1 * t11
      t86 = t56 * t68 * t82 * x2 * t84
      t89 = t11 * t35
      t92 = t17 * x1 * t89 * t51 * t82
      t111 = t14 * t15
      rrgq2qgh73J3 = -(-t4 * t12 * wd * (0.2D1 * t17 * t18 * t21 * t36 *
     # t3 * t22 - 0.3D1 * t44 * t45 * t48 * t35 * t50 * t51 + 0.4D1 * t5
     #6 * t57 * t35 * x2 * t51 * t22 - t14 * t42 * t15 * t50 * x2 * t45 
     #* t68 * t48 + 0.2D1 * t44 * t68 * t22 * t50 * t57 - t17 * t45 * t4
     #8 * t36 * t35 - 0.2D1 * t86 - 0.2D1 * t92 - 0.3D1 * t56 * t45 * t4
     #8 * t36 * x2 * t3) - t4 * t12 * wd * (0.2D1 * t86 + 0.2D1 * t17 * 
     #t51 * t22 * x2 * t84 + 0.2D1 * t92 - 0.2D1 * t14 * t1 * t84 * t35 
     #- 0.2D1 * t111 * x2 * x1 * t3 * t11 + 0.2D1 * t111 * t3 * t22 * x1
     # * t89)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh73J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t5 = 0.1D1 - x2
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * t5 * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t17 = t14 * t15 * t1
      t18 = x1 ** 2
      t20 = t10 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(x3 * t5 * t10 * x2 * t22)
      t35 = t22 * t5 * t10 + x2 * x3 + 0.2D1 * t27 * t32
      t36 = t35 ** 2
      t42 = t15 ** 2
      t44 = t14 * t42 * t1
      t45 = t18 * x1
      t48 = 0.1D1 / t20 / t10
      t50 = x2 ** 2
      t51 = t3 ** 2
      t56 = t14 * t42
      t57 = t18 * t21
      t68 = t51 * t3
      t82 = t22 ** 2
      t84 = x1 * t11
      t86 = t56 * t68 * t82 * x2 * t84
      t89 = t11 * t35
      t92 = t17 * x1 * t89 * t51 * t82
      t111 = t14 * t15
      rrgq2qgh73J4 = -(-t4 * t12 * wd * (0.2D1 * t17 * t18 * t21 * t36 *
     # t3 * t22 - 0.3D1 * t44 * t45 * t48 * t35 * t50 * t51 + 0.4D1 * t5
     #6 * t57 * t35 * x2 * t51 * t22 - t14 * t42 * t15 * t50 * x2 * t45 
     #* t68 * t48 + 0.2D1 * t44 * t68 * t22 * t50 * t57 - t17 * t45 * t4
     #8 * t36 * t35 - 0.2D1 * t86 - 0.2D1 * t92 - 0.3D1 * t56 * t45 * t4
     #8 * t36 * x2 * t3) - t4 * t12 * wd * (0.2D1 * t86 + 0.2D1 * t17 * 
     #t51 * t22 * x2 * t84 + 0.2D1 * t92 - 0.2D1 * t14 * t1 * t84 * t35 
     #- 0.2D1 * t111 * x2 * x1 * t3 * t11 + 0.2D1 * t111 * t3 * t22 * x1
     # * t89)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh73J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t5 = 0.1D1 - x2
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * t5 * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t17 = t14 * t15 * t1
      t18 = x1 ** 2
      t20 = t10 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(x3 * t5 * t10 * x2 * t22)
      t35 = t22 * t5 * t10 + x2 * x3 + 0.2D1 * t27 * t32
      t36 = t35 ** 2
      t42 = t15 ** 2
      t44 = t14 * t42 * t1
      t45 = t18 * x1
      t48 = 0.1D1 / t20 / t10
      t50 = x2 ** 2
      t51 = t3 ** 2
      t56 = t14 * t42
      t57 = t18 * t21
      t68 = t51 * t3
      t82 = t22 ** 2
      t84 = x1 * t11
      t86 = t56 * t68 * t82 * x2 * t84
      t89 = t11 * t35
      t92 = t17 * x1 * t89 * t51 * t82
      t111 = t14 * t15
      rrgq2qgh73J5 = -(-t4 * t12 * wd * (0.2D1 * t17 * t18 * t21 * t36 *
     # t3 * t22 - 0.3D1 * t44 * t45 * t48 * t35 * t50 * t51 + 0.4D1 * t5
     #6 * t57 * t35 * x2 * t51 * t22 - t14 * t42 * t15 * t50 * x2 * t45 
     #* t68 * t48 + 0.2D1 * t44 * t68 * t22 * t50 * t57 - t17 * t45 * t4
     #8 * t36 * t35 - 0.2D1 * t86 - 0.2D1 * t92 - 0.3D1 * t56 * t45 * t4
     #8 * t36 * x2 * t3) - t4 * t12 * wd * (0.2D1 * t86 + 0.2D1 * t17 * 
     #t51 * t22 * x2 * t84 + 0.2D1 * t92 - 0.2D1 * t14 * t1 * t84 * t35 
     #- 0.2D1 * t111 * x2 * x1 * t3 * t11 + 0.2D1 * t111 * t3 * t22 * x1
     # * t89)) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh73J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x2
      t9 = z + t1 * x1
      t10 = 0.1D1 / t9
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = t2 ** 2
      t21 = 0.1D1 - x3
      t22 = t21 ** 2
      t24 = x1 * t10
      t28 = t14 * t15 * t1
      t38 = cos(x4 * 0.3141592653589793D1)
      t43 = Sqrt(x3 * t4 * t9 * x2 * t21)
      t46 = t21 * t4 * t9 + x2 * x3 + 0.2D1 * t38 * t43
      t47 = t10 * t46
      t54 = t14 * t15
      rrgq2qgh73J6 = t1 * t2 * (z + x1 * t4 * t1) * t10 * wd * (0.2D1 * 
     #t14 * t16 * t18 * t2 * t22 * x2 * t24 + 0.2D1 * t28 * t18 * t21 * 
     #x2 * t24 + 0.2D1 * t28 * x1 * t47 * t18 * t22 - 0.2D1 * t14 * t1 *
     # t24 * t46 - 0.2D1 * t54 * x2 * x1 * t2 * t10 + 0.2D1 * t54 * t2 *
     # t21 * x1 * t47) / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh74J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t9 = z + t1 * x1
      t10 = 0.1D1 / t9
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x2 ** 2
      t20 = x1 ** 2
      t21 = t2 ** 2
      t23 = t9 ** 2
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      rrgq2qgh74J1 = t1 * t2 * (z + x1 * (0.1D1 - x2) * t1) * t10 * wd *
     # (0.4D1 * t14 * t16 * t18 * t20 * t21 / t23 + 0.4D1 * t14 + 0.4D1 
     #* t14 * t15 * t21 * t31 - 0.8D1 * t14 * t15 * t1 * t21 * t30 * x2 
     #* x1 * t10) / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh74J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * (0.1D1 - x2) * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x2 ** 2
      t20 = x1 ** 2
      t21 = t3 ** 2
      t23 = t10 ** 2
      t27 = 0.4D1 * t14 * t16 * t18 * t20 * t21 / t23
      t28 = 0.4D1 * t14
      t29 = t14 * t15
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      t34 = 0.4D1 * t29 * t21 * t31
      t41 = t14 * t15 * t1 * t21 * t30 * x2 * x1 * t11
      rrgq2qgh74J2 = -(-t4 * t12 * wd * (t27 + t28 + t34 - 0.8D1 * t41) 
     #- t4 * t12 * wd * (-t34 - t27 - t28 - 0.12D2 * t29 * x2 * x1 * t3 
     #* t11 + 0.8D1 * t14 * t1 * t3 * t30 + 0.12D2 * t41)) / s / z / 0.3
     #141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh74J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * (0.1D1 - x2) * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x2 ** 2
      t20 = x1 ** 2
      t21 = t3 ** 2
      t23 = t10 ** 2
      t27 = 0.4D1 * t14 * t16 * t18 * t20 * t21 / t23
      t28 = 0.4D1 * t14
      t29 = t14 * t15
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      t34 = 0.4D1 * t29 * t21 * t31
      t41 = t14 * t15 * t1 * t21 * t30 * x2 * x1 * t11
      rrgq2qgh74J3 = -(-t4 * t12 * wd * (t27 + t28 + t34 - 0.8D1 * t41) 
     #- t4 * t12 * wd * (-t34 - t27 - t28 - 0.12D2 * t29 * x2 * x1 * t3 
     #* t11 + 0.8D1 * t14 * t1 * t3 * t30 + 0.12D2 * t41)) / s / z / 0.3
     #141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh74J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * (0.1D1 - x2) * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x2 ** 2
      t20 = x1 ** 2
      t21 = t3 ** 2
      t23 = t10 ** 2
      t27 = 0.4D1 * t14 * t16 * t18 * t20 * t21 / t23
      t28 = 0.4D1 * t14
      t29 = t14 * t15
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      t34 = 0.4D1 * t29 * t21 * t31
      t41 = t14 * t15 * t1 * t21 * t30 * x2 * x1 * t11
      rrgq2qgh74J4 = -(-t4 * t12 * wd * (t27 + t28 + t34 - 0.8D1 * t41) 
     #- t4 * t12 * wd * (-t34 - t27 - t28 - 0.12D2 * t29 * x2 * x1 * t3 
     #* t11 + 0.8D1 * t14 * t1 * t3 * t30 + 0.12D2 * t41)) / s / z / 0.3
     #141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh74J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t3 = 0.1D1 - x1
      t4 = s * t1 * t3
      t10 = z + t1 * x1
      t11 = 0.1D1 / t10
      t12 = (z + x1 * (0.1D1 - x2) * t1) * t11
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x2 ** 2
      t20 = x1 ** 2
      t21 = t3 ** 2
      t23 = t10 ** 2
      t27 = 0.4D1 * t14 * t16 * t18 * t20 * t21 / t23
      t28 = 0.4D1 * t14
      t29 = t14 * t15
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      t34 = 0.4D1 * t29 * t21 * t31
      t41 = t14 * t15 * t1 * t21 * t30 * x2 * x1 * t11
      rrgq2qgh74J5 = -(-t4 * t12 * wd * (t27 + t28 + t34 - 0.8D1 * t41) 
     #- t4 * t12 * wd * (-t34 - t27 - t28 - 0.12D2 * t29 * x2 * x1 * t3 
     #* t11 + 0.8D1 * t14 * t1 * t3 * t30 + 0.12D2 * t41)) / s / z / 0.3
     #141592653589793D1

      end function
  
   
 

      doubleprecision function rrgq2qgh74J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t9 = z + t1 * x1
      t10 = 0.1D1 / t9
      t13 = s ** 2
      t14 = t13 * s
      t15 = t1 ** 2
      t16 = t14 * t15
      t17 = t2 ** 2
      t18 = 0.1D1 - x3
      t19 = t18 ** 2
      t23 = t15 ** 2
      t25 = x2 ** 2
      t27 = x1 ** 2
      t29 = t9 ** 2
      rrgq2qgh74J6 = t1 * t2 * (z + x1 * (0.1D1 - x2) * t1) * t10 * wd *
     # (-0.4D1 * t16 * t17 * t19 - 0.4D1 * t14 * t23 * t25 * t27 * t17 /
     # t29 - 0.4D1 * t14 - 0.12D2 * t16 * x2 * x1 * t2 * t10 + 0.8D1 * t
     #14 * t1 * t2 * t18 + 0.12D2 * t14 * t15 * t1 * t17 * t18 * x2 * x1
     # * t10) / z / 0.3141592653589793D1

      end function
  
 