  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqg2qgh91J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t21 = log(0.4D1 * t19)
      t22 = t21 ** 2
      t23 = t22 * 0.3141592653589793D1
      t26 = 0.3141592653589793D1 ** 2
      t29 = lh ** 2
      t32 = -0.60D2 * lh * t26 + 0.2884936567583026D3 + 0.120D3 * t29 * 
     #lh
      t33 = 0.3141592653589793D1 * t32
      t35 = t22 * t21 * 0.3141592653589793D1
      t37 = t21 * 0.3141592653589793D1
      t40 = -0.180D3 * t29 + 0.30D2 * t26
      t43 = (0.90D2 * t23 * lh + t33 + 0.15D2 * t35 - t37 * t40) * t3
      t44 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t51 = 0.3141592653589793D1 * t40
      t53 = (-0.180D3 * t37 * lh - 0.45D2 * t23 + t51) * t3
      t54 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t58 = 0.3141592653589793D1 * lh
      t62 = (0.180D3 * t58 + 0.90D2 * t37) * t3
      t63 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t73 = t26 ** 2
      t74 = t29 ** 2
      t80 = t22 ** 2
      t84 = (-0.30D2 * t35 * lh + t23 * t40 / 0.2D1 - t37 * t32 + 0.3141
     #592653589793D1 * (-0.5769873135166051D3 * lh - t73 - 0.60D2 * t74 
     #+ 0.60D2 * t29 * t26) - 0.15D2 / 0.4D1 * t80 * 0.3141592653589793D
     #1) * t3
      t85 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t89 = x1 ** 2
      t90 = x3 * t89
      t93 = log(0.4D1 * t90 * t19)
      t94 = t93 ** 2
      t102 = t3 * t6
      t108 = t102 * t85
      t109 = t51 * t108
      t111 = 0.1D1 / x3
      t113 = 0.1D1 / x1
      t116 = t89 * t15
      t117 = t12 * t18
      t120 = log(0.4D1 * t116 * t117)
      t125 = t120 ** 2
      t128 = t125 * t120
      t136 = t33 * t108
      t147 = x2 * x3
      t148 = t147 * t89
      t151 = log(0.4D1 * t148 * t19)
      t161 = 0.1D1 / x2
      t162 = t161 * t113
      t165 = x2 * t89
      t168 = log(0.4D1 * t165 * t19)
      t170 = t168 ** 2
      t189 = log(0.4D1 * x3 * t15 * t117)
      t194 = t189 ** 2
      t197 = t194 * t189
      t217 = log(0.4D1 * t147 * t19)
      t219 = t217 ** 2
      t238 = log(0.4D1 * x2 * t18 * t16)
      t243 = t238 ** 2
      t246 = t243 * t238
      t264 = t4 * t6 * t7 / 0.16D2 - t43 * t6 * t44 / 0.1440D4 - t53 * t
     #6 * t54 / 0.1440D4 - t62 * t6 * t63 / 0.1440D4 - t84 * t6 * t85 / 
     #0.1440D4 - (-0.90D2 * t4 * t6 * (t94 * t85 / 0.2D1 - t93 * t44 + t
     #54) + 0.180D3 * t58 * t102 * (-t93 * t85 + t44) + t109) * t111 * t
     #113 / 0.720D3 - (t51 * t102 * (-t120 * t85 + t44) - 0.90D2 * t4 * 
     #t6 * (t125 * t44 / 0.2D1 + t63 - t128 * t85 / 0.6D1 - t120 * t54) 
     #+ t136 + 0.180D3 * t58 * t102 * (-t120 * t44 + t54 + t125 * t85 / 
     #0.2D1)) * t113 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t151 * t85 + t44
     #) + 0.180D3 * t58 * t108) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 
     #* t6 * (-t168 * t44 + t54 + t170 * t85 / 0.2D1) + 0.180D3 * t58 * 
     #t102 * (t44 - t168 * t85) + t109) * t161 * t113 / 0.720D3 - (t51 *
     # t102 * (t44 - t189 * t85) - 0.90D2 * t4 * t6 * (t194 * t44 / 0.2D
     #1 + t63 - t197 * t85 / 0.6D1 - t189 * t54) + t136 + 0.180D3 * t58 
     #* t102 * (-t189 * t44 + t54 + t194 * t85 / 0.2D1)) * t111 / 0.1440
     #D4 - (-0.90D2 * t4 * t6 * (-t217 * t44 + t54 + t219 * t85 / 0.2D1)
     # + 0.180D3 * t58 * t102 * (t44 - t217 * t85) + t109) * t111 * t161
     # / 0.1440D4 - (t51 * t102 * (-t238 * t85 + t44) - 0.90D2 * t4 * t6
     # * (t243 * t44 / 0.2D1 + t63 - t246 * t85 / 0.6D1 - t238 * t54) + 
     #t136 + 0.180D3 * t58 * t102 * (-t238 * t44 + t54 + t243 * t85 / 0.
     #2D1)) * t161 / 0.1440D4
      t265 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t264)
      t267 = -0.1D1 + x1
      t268 = t2 * t267
      t269 = t2 * x1
      t271 = x1 * z
      t272 = 0.1D1 - x1 + t271
      t273 = 0.1D1 / t272
      t274 = t267 ** 2
      t275 = t273 * t274
      t279 = log(0.4D1 * t90 * t15 * t117 * t275)
      t280 = t279 ** 2
      t281 = -t267
      t282 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t285 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t287 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t297 = t102 * t282
      t298 = t51 * t297
      t302 = t116 * t12
      t303 = t18 * t273
      t307 = log(0.4D1 * t302 * t303 * t274)
      t312 = t307 ** 2
      t315 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t316 = t312 * t307
      t335 = t147 * t89 * t18
      t336 = t16 * t275
      t339 = log(0.4D1 * t335 * t336)
      t353 = log(0.4D1 * t165 * t18 * t336)
      t354 = t353 ** 2
      t371 = -(-0.90D2 * t4 * t6 * (-t280 * t282 / 0.2D1 + t279 * t285 -
     # t287) + 0.180D3 * t58 * t102 * (t279 * t282 - t285) - t298) * t11
     #1 * t113 / 0.720D3 - (t51 * t102 * (-t285 + t307 * t282) - 0.90D2 
     #* t4 * t6 * (-t312 * t285 / 0.2D1 - t315 + t316 * t282 / 0.6D1 + t
     #307 * t287) - t33 * t297 + 0.180D3 * t58 * t102 * (t307 * t285 - t
     #312 * t282 / 0.2D1 - t287)) * t113 / 0.720D3 - (-0.90D2 * t4 * t6 
     #* (-t285 + t339 * t282) - 0.180D3 * t58 * t297) * t111 * t162 / 0.
     #720D3 - (-0.90D2 * t4 * t6 * (-t354 * t282 / 0.2D1 + t353 * t285 -
     # t287) + 0.180D3 * t58 * t102 * (t353 * t282 - t285) - t298) * t16
     #1 * t113 / 0.720D3
      t372 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t268, t269, 0.0D0, t371)
      t374 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t377 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t378 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t389 = t102 * t374
      t390 = t51 * t389
      t401 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t409 = t33 * t389
      t420 = rrqg2qgh92J5(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t525 = -(-0.90D2 * t4 * t6 * (t219 * t374 / 0.2D1 + t377 - t217 * 
     #t378) + 0.180D3 * t58 * t102 * (t378 - t217 * t374) + t390) * t111
     # * t161 / 0.1440D4 - (t51 * t102 * (t378 - t238 * t374) - 0.90D2 *
     # t4 * t6 * (t243 * t378 / 0.2D1 + t401 - t246 * t374 / 0.6D1 - t23
     #8 * t377) + t409 + 0.180D3 * t58 * t102 * (t243 * t374 / 0.2D1 + t
     #377 - t238 * t378)) * t161 / 0.1440D4 + t4 * t6 * t420 / 0.16D2 - 
     #(t51 * t102 * (t378 - t189 * t374) - 0.90D2 * t4 * t6 * (t194 * t3
     #78 / 0.2D1 - t189 * t377 + t401 - t197 * t374 / 0.6D1) + t409 + 0.
     #180D3 * t58 * t102 * (t194 * t374 / 0.2D1 + t377 - t189 * t378)) *
     # t111 / 0.1440D4 - t53 * t6 * t377 / 0.1440D4 - t62 * t6 * t401 / 
     #0.1440D4 - t84 * t6 * t374 / 0.1440D4 - t43 * t6 * t378 / 0.1440D4
     # - (-0.90D2 * t4 * t6 * (t377 - t93 * t378 + t94 * t374 / 0.2D1) +
     # 0.180D3 * t58 * t102 * (-t93 * t374 + t378) + t390) * t111 * t113
     # / 0.720D3 - (t51 * t102 * (t378 - t120 * t374) - 0.90D2 * t4 * t6
     # * (-t120 * t377 + t401 + t125 * t378 / 0.2D1 - t128 * t374 / 0.6D
     #1) + t409 + 0.180D3 * t58 * t102 * (-t120 * t378 + t125 * t374 / 0
     #.2D1 + t377)) * t113 / 0.720D3 - (-0.90D2 * t4 * t6 * (t378 - t151
     # * t374) + 0.180D3 * t58 * t389) * t111 * t162 / 0.720D3 - (-0.90D
     #2 * t4 * t6 * (t377 - t168 * t378 + t170 * t374 / 0.2D1) + 0.180D3
     # * t58 * t102 * (t378 - t168 * t374) + t390) * t161 * t113 / 0.720
     #D3
      t526 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t525)
      t528 = t2 * x3
      t529 = -0.1D1 + x3
      t530 = t2 * t529
      t531 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t533 = t16 * t529
      t536 = log(-0.4D1 * x3 * t18 * t533)
      t537 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t542 = t536 ** 2
      t545 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t547 = t542 * t536
      t550 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t555 = t102 * t537
      t570 = log(-0.4D1 * t90 * t18 * t533)
      t572 = t570 ** 2
      t584 = t51 * t555
      t589 = t18 * t15
      t594 = log(-0.4D1 * t148 * t589 * t12 * t529)
      t609 = log(-0.4D1 * t147 * t18 * t533)
      t611 = t609 ** 2
      t627 = -(t51 * t102 * (-t531 + t536 * t537) - 0.90D2 * t4 * t6 * (
     #-t542 * t531 / 0.2D1 + t536 * t545 + t547 * t537 / 0.6D1 - t550) -
     # t33 * t555 + 0.180D3 * t58 * t102 * (t536 * t531 - t545 - t542 * 
     #t537 / 0.2D1)) * t111 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t545 + t
     #570 * t531 - t572 * t537 / 0.2D1) + 0.180D3 * t58 * t102 * (t570 *
     # t537 - t531) - t584) * t111 * t113 / 0.720D3 - (-0.90D2 * t4 * t6
     # * (t594 * t537 - t531) - 0.180D3 * t58 * t555) * t111 * t162 / 0.
     #720D3 - (-0.90D2 * t4 * t6 * (t609 * t531 - t545 - t611 * t537 / 0
     #.2D1) + 0.180D3 * t58 * t102 * (t609 * t537 - t531) - t584) * t111
     # * t161 / 0.1440D4
      t628 = FJET(XB1, XB2, s, t528, 0.0D0, -t530, 0.0D0, 0.0D0, t627)
      t630 = 0.2D1 * t147
      t631 = cos(t13)
      t632 = -0.1D1 + x2
      t636 = Sqrt(x2 * t632 * x3 * t529)
      t638 = 0.2D1 * t631 * t636
      t640 = t2 * (0.1D1 - x2 - x3 + t630 + t638)
      t642 = t2 * (-x3 + t630 - x2 + t638)
      t643 = t147 * t116
      t645 = t117 * t632 * t529
      t648 = log(0.4D1 * t643 * t645)
      t649 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t651 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t656 = t102 * t649
      t663 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t664 = t147 * t15
      t667 = log(0.4D1 * t664 * t645)
      t669 = t667 ** 2
      t686 = -(-0.90D2 * t4 * t6 * (-t648 * t649 + t651) + 0.180D3 * t58
     # * t656) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 * t6 * (t663 - t6
     #67 * t651 + t669 * t649 / 0.2D1) + 0.180D3 * t58 * t102 * (t651 - 
     #t667 * t649) + t51 * t656) * t111 * t161 / 0.1440D4
      t687 = FJET(XB1, XB2, s, t640, 0.0D0, -t642, 0.0D0, 0.0D0, t686)
      t690 = x2 * s * t1
      t691 = t632 * s
      t692 = t691 * t1
      t693 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t694 = t18 * t632
      t698 = log(-0.4D1 * t148 * t16 * t694)
      t699 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t705 = t102 * t699
      t713 = t117 * t632
      t716 = log(-0.4D1 * t165 * t15 * t713)
      t717 = t716 ** 2
      t721 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t731 = t51 * t705
      t738 = log(-0.4D1 * t664 * t713)
      t739 = t738 ** 2
      t759 = log(-0.4D1 * x2 * t15 * t713)
      t764 = t759 ** 2
      t767 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t768 = t764 * t759
      t787 = -(-0.90D2 * t4 * t6 * (-t693 + t698 * t699) - 0.180D3 * t58
     # * t705) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t717 * t
     #699 / 0.2D1 + t716 * t693 - t721) + 0.180D3 * t58 * t102 * (t716 *
     # t699 - t693) - t731) * t161 * t113 / 0.720D3 - (-0.90D2 * t4 * t6
     # * (-t721 - t739 * t699 / 0.2D1 + t738 * t693) + 0.180D3 * t58 * t
     #102 * (-t693 + t738 * t699) - t731) * t111 * t161 / 0.1440D4 - (t5
     #1 * t102 * (-t693 + t759 * t699) - 0.90D2 * t4 * t6 * (-t764 * t69
     #3 / 0.2D1 - t767 + t768 * t699 / 0.6D1 + t759 * t721) - t33 * t705
     # + 0.180D3 * t58 * t102 * (-t764 * t699 / 0.2D1 - t721 + t759 * t6
     #93)) * t161 / 0.1440D4
      t788 = FJET(XB1, XB2, s, t690, 0.0D0, -t692, 0.0D0, 0.0D0, t787)
      t790 = t1 * t267
      t791 = t691 * t790
      t794 = t2 * t267 * x2 * t273
      t799 = s * t17 * x2 * x1 * t267 * t273
      t804 = log(-0.4D1 * t643 * t117 * t275 * t632)
      t805 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t281, x2, 0.0D0, x
     #4)
      t807 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t281, x2, 0.0D0, x
     #4)
      t812 = t102 * t805
      t818 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t281, x2, 0.0D0, x
     #4)
      t823 = log(-0.4D1 * t165 * t16 * t694 * t275)
      t824 = t823 ** 2
      t842 = -(-0.90D2 * t4 * t6 * (-t804 * t805 + t807) + 0.180D3 * t58
     # * t812) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 * t6 * (t818 + t8
     #24 * t805 / 0.2D1 - t823 * t807) + 0.180D3 * t58 * t102 * (t807 - 
     #t823 * t805) + t51 * t812) * t161 * t113 / 0.720D3
      t843 = FJET(XB1, XB2, s, t791, t269, -t794, 0.0D0, -t799, t842)
      t845 = t529 * s
      t846 = t845 * t790
      t848 = t845 * t1 * x1
      t850 = x3 * s * t790
      t851 = x3 * x1
      t852 = t2 * t851
      t859 = log(-0.4D1 * t90 * t589 * t12 * t273 * t274 * t529)
      t860 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, x3, x
     #4)
      t862 = t859 ** 2
      t863 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, x3, x
     #4)
      t866 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, x3, x
     #4)
      t876 = t102 * t863
      t885 = log(-0.4D1 * t335 * t16 * t275 * t529)
      t897 = -(-0.90D2 * t4 * t6 * (-t859 * t860 + t862 * t863 / 0.2D1 +
     # t866) + 0.180D3 * t58 * t102 * (t860 - t859 * t863) + t51 * t876)
     # * t111 * t113 / 0.720D3 - (-0.90D2 * t4 * t6 * (t860 - t885 * t86
     #3) + 0.180D3 * t58 * t876) * t111 * t162 / 0.720D3
      t898 = FJET(XB1, XB2, s, t846, -t848, -t850, t852, 0.0D0, t897)
      t900 = t851 * z
      t901 = t147 * x1
      t902 = t147 * t271
      t907 = Sqrt(x3 * t632 * t272 * x2 * t529)
      t909 = 0.2D1 * t631 * t907
      t913 = t2 * t267 * (-x3 + t851 - t900 + t630 - t901 + t902 - x2 + 
     #t909) * t273
      t914 = x2 * x1
      t916 = 0.1D1 - x1 + t271 - x2 + t914 - t914 * z - x3 + t851 - t900
     # + t630 - t901 + t902 + t909
      t919 = t2 * t267 * t916 * t273
      t926 = log(0.4D1 * t147 * t302 * t303 * t274 * t632 * t529)
      t927 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t281, x2, x3, x4)
      t929 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t281, x2, x3, x4)
      t937 = -0.90D2 * t4 * t6 * (t926 * t927 - t929) - 0.180D3 * t58 * 
     #t102 * t927
      t941 = FJET(XB1, XB2, s, t913, t852, -t919, -t848, -t799, -t937 * 
     #t111 * t162 / 0.720D3)
      t944 = t111 * t161 * t113
      t947 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t950 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t951 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t962 = t102 * t947
      t963 = t51 * t962
      t974 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, 0.0D0
     #, x4)
      t1017 = -(-0.90D2 * t4 * t6 * (-t280 * t947 / 0.2D1 - t950 + t279 
     #* t951) + 0.180D3 * t58 * t102 * (-t951 + t279 * t947) - t963) * t
     #111 * t113 / 0.720D3 - (t51 * t102 * (t307 * t947 - t951) - 0.90D2
     # * t4 * t6 * (-t312 * t951 / 0.2D1 + t307 * t950 - t974 + t316 * t
     #947 / 0.6D1) - t33 * t962 + 0.180D3 * t58 * t102 * (t307 * t951 - 
     #t312 * t947 / 0.2D1 - t950)) * t113 / 0.720D3 - (-0.90D2 * t4 * t6
     # * (-t951 + t339 * t947) - 0.180D3 * t58 * t962) * t111 * t162 / 0
     #.720D3 - (-0.90D2 * t4 * t6 * (-t950 - t354 * t947 / 0.2D1 + t353 
     #* t951) + 0.180D3 * t58 * t102 * (t353 * t947 - t951) - t963) * t1
     #61 * t113 / 0.720D3
      t1018 = FJET(XB1, XB2, s, -t268, t269, 0.0D0, 0.0D0, 0.0D0, t1017)
      t1020 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t1021 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t1026 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t1030 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3
     #, x4)
      t1037 = t102 * t1021
      t1061 = t51 * t1037
      t1093 = -(t51 * t102 * (-t1020 + t536 * t1021) - 0.90D2 * t4 * t6 
     #* (t536 * t1026 + t547 * t1021 / 0.6D1 - t1030 - t542 * t1020 / 0.
     #2D1) - t33 * t1037 + 0.180D3 * t58 * t102 * (-t542 * t1021 / 0.2D1
     # - t1026 + t536 * t1020)) * t111 / 0.1440D4 - (-0.90D2 * t4 * t6 *
     # (t570 * t1020 - t572 * t1021 / 0.2D1 - t1026) + 0.180D3 * t58 * t
     #102 * (-t1020 + t570 * t1021) - t1061) * t111 * t113 / 0.720D3 - (
     #-0.90D2 * t4 * t6 * (-t1020 + t594 * t1021) - 0.180D3 * t58 * t103
     #7) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t611 * t1021 /
     # 0.2D1 - t1026 + t609 * t1020) + 0.180D3 * t58 * t102 * (-t1020 + 
     #t609 * t1021) - t1061) * t111 * t161 / 0.1440D4
      t1094 = FJET(XB1, XB2, s, -t530, 0.0D0, t528, 0.0D0, 0.0D0, t1093)
      t1096 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x
     #4)
      t1097 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x
     #4)
      t1103 = t102 * t1097
      t1110 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x
     #4)
      t1128 = -(-0.90D2 * t4 * t6 * (t1096 - t648 * t1097) + 0.180D3 * t
     #58 * t1103) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 * t6 * (t1110 
     #+ t669 * t1097 / 0.2D1 - t667 * t1096) + 0.180D3 * t58 * t102 * (t
     #1096 - t667 * t1097) + t51 * t1103) * t111 * t161 / 0.1440D4
      t1129 = FJET(XB1, XB2, s, -t642, 0.0D0, t640, 0.0D0, 0.0D0, t1128)
      t1131 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1132 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1138 = t102 * t1132
      t1147 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1158 = t51 * t1138
      t1185 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0
     #, x4)
      t1204 = -(-0.90D2 * t4 * t6 * (-t1131 + t698 * t1132) - 0.180D3 * 
     #t58 * t1138) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t717
     # * t1132 / 0.2D1 - t1147 + t716 * t1131) + 0.180D3 * t58 * t102 * 
     #(-t1131 + t716 * t1132) - t1158) * t161 * t113 / 0.720D3 - (-0.90D
     #2 * t4 * t6 * (-t739 * t1132 / 0.2D1 - t1147 + t738 * t1131) + 0.1
     #80D3 * t58 * t102 * (-t1131 + t738 * t1132) - t1158) * t111 * t161
     # / 0.1440D4 - (t51 * t102 * (-t1131 + t759 * t1132) - 0.90D2 * t4 
     #* t6 * (-t764 * t1131 / 0.2D1 - t1185 + t768 * t1132 / 0.6D1 + t75
     #9 * t1147) - t33 * t1138 + 0.180D3 * t58 * t102 * (t759 * t1131 - 
     #t1147 - t764 * t1132 / 0.2D1)) * t161 / 0.1440D4
      t1205 = FJET(XB1, XB2, s, -t692, 0.0D0, t690, 0.0D0, 0.0D0, t1204)
      t1207 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, x3, 
     #x4)
      t1210 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, x3, 
     #x4)
      t1212 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t281, 0.0D0, x3, 
     #x4)
      t1222 = t102 * t1207
      t1238 = -(-0.90D2 * t4 * t6 * (t862 * t1207 / 0.2D1 - t859 * t1210
     # + t1212) + 0.180D3 * t58 * t102 * (-t859 * t1207 + t1210) + t51 *
     # t1222) * t111 * t113 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t885 * t1
     #207 + t1210) + 0.180D3 * t58 * t1222) * t111 * t162 / 0.720D3
      t1239 = FJET(XB1, XB2, s, -t850, t852, t846, -t848, 0.0D0, t1238)
      t1241 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t281, x2, 0.0D0, 
     #x4)
      t1242 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t281, x2, 0.0D0, 
     #x4)
      t1248 = t102 * t1242
      t1254 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t281, x2, 0.0D0, 
     #x4)
      t1272 = -(-0.90D2 * t4 * t6 * (t1241 - t804 * t1242) + 0.180D3 * t
     #58 * t1248) * t111 * t162 / 0.720D3 - (-0.90D2 * t4 * t6 * (t1254 
     #- t823 * t1241 + t824 * t1242 / 0.2D1) + 0.180D3 * t58 * t102 * (t
     #1241 - t823 * t1242) + t51 * t1248) * t161 * t113 / 0.720D3
      t1273 = FJET(XB1, XB2, s, -t794, 0.0D0, t791, t269, -t799, t1272)
      t1275 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t281, x2, x3, x4)
      t1276 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t281, x2, x3, x4)
      t1285 = -0.90D2 * t4 * t6 * (-t1275 + t926 * t1276) - 0.180D3 * t5
     #8 * t102 * t1276
      t1289 = FJET(XB1, XB2, s, -t919, -t848, t913, t852, -t799, -t1285 
     #* t111 * t162 / 0.720D3)
      rrqg2qght9s1e1 = t265 * t264 + t372 * t371 + t526 * t525 + t628 * 
     #t627 + t687 * t686 + t788 * t787 + t843 * t842 + t898 * t897 - t94
     #1 * t937 * t944 / 0.720D3 + t1018 * t1017 + t1094 * t1093 + t1129 
     #* t1128 + t1205 * t1204 + t1239 * t1238 + t1273 * t1272 - t1289 * 
     #t1285 * t944 / 0.720D3

      end function



      doubleprecision function rrqg2qght9s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t1 ** 2
      t14 = t13 ** 2
      t15 = t12 * t14
      t18 = log(0.4D1 * x3 * t9 * t15)
      t19 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t21 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t22 = t18 ** 2
      t23 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t30 = 0.3141592653589793D1 * lh
      t31 = t3 * t6
      t37 = lh ** 2
      t39 = 0.3141592653589793D1 ** 2
      t41 = -0.180D3 * t37 + 0.30D2 * t39
      t42 = 0.3141592653589793D1 * t41
      t43 = t31 * t23
      t44 = t42 * t43
      t46 = 0.1D1 / x3
      t49 = t12 * t9
      t50 = t49 * t14
      t52 = log(0.4D1 * t50)
      t53 = t52 * 0.3141592653589793D1
      t56 = t52 ** 2
      t57 = t56 * 0.3141592653589793D1
      t60 = (-0.180D3 * t53 * lh - 0.45D2 * t57 + t42) * t3
      t64 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t81 = (0.90D2 * t57 * lh + 0.3141592653589793D1 * (-0.60D2 * lh * 
     #t39 + 0.2884936567583026D3 + 0.120D3 * t37 * lh) + 0.15D2 * t56 * 
     #t52 * 0.3141592653589793D1 - t53 * t41) * t3
      t88 = (0.180D3 * t30 + 0.90D2 * t53) * t3
      t92 = x2 * x3
      t95 = log(0.4D1 * t92 * t50)
      t102 = 0.180D3 * t30 * t43
      t105 = 0.1D1 / x2
      t111 = log(0.4D1 * x2 * t14 * t49)
      t113 = t111 ** 2
      t128 = x1 ** 2
      t129 = x3 * t128
      t132 = log(0.4D1 * t129 * t50)
      t140 = 0.1D1 / x1
      t143 = t4 * t6
      t145 = t105 * t140
      t149 = x2 * t128
      t152 = log(0.4D1 * t149 * t50)
      t162 = t128 * t9
      t165 = log(0.4D1 * t162 * t15)
      t167 = t165 ** 2
      t182 = -(-0.90D2 * t4 * t6 * (-t18 * t19 + t21 + t22 * t23 / 0.2D1
     #) + 0.180D3 * t30 * t31 * (t19 - t18 * t23) + t44) * t46 / 0.1440D
     #4 - t60 * t6 * t19 / 0.1440D4 + t4 * t6 * t64 / 0.16D2 - t81 * t6 
     #* t23 / 0.1440D4 - t88 * t6 * t21 / 0.1440D4 - (-0.90D2 * t4 * t6 
     #* (t19 - t95 * t23) + t102) * t46 * t105 / 0.1440D4 - (-0.90D2 * t
     #4 * t6 * (-t111 * t19 + t21 + t113 * t23 / 0.2D1) + 0.180D3 * t30 
     #* t31 * (-t111 * t23 + t19) + t44) * t105 / 0.1440D4 - (-0.90D2 * 
     #t4 * t6 * (-t132 * t23 + t19) + t102) * t46 * t140 / 0.720D3 + t14
     #3 * t23 * t46 * t145 / 0.8D1 - (-0.90D2 * t4 * t6 * (t19 - t152 * 
     #t23) + t102) * t105 * t140 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t165
     # * t19 + t21 + t167 * t23 / 0.2D1) + 0.180D3 * t30 * t31 * (-t165 
     #* t23 + t19) + t44) * t140 / 0.720D3
      t183 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t182)
      t185 = -0.1D1 + x1
      t186 = t2 * t185
      t187 = t2 * x1
      t189 = x1 * z
      t190 = 0.1D1 - x1 + t189
      t191 = 0.1D1 / t190
      t192 = t185 ** 2
      t193 = t191 * t192
      t197 = log(0.4D1 * t129 * t9 * t15 * t193)
      t198 = -t185
      t199 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t201 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t206 = t31 * t199
      t208 = 0.180D3 * t30 * t206
      t221 = log(0.4D1 * t149 * t14 * t49 * t193)
      t236 = log(0.4D1 * t162 * t12 * t14 * t191 * t192)
      t238 = t236 ** 2
      t241 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t255 = -(-0.90D2 * t4 * t6 * (t197 * t199 - t201) - t208) * t46 * 
     #t140 / 0.720D3 - t143 * t199 * t46 * t145 / 0.8D1 - (-0.90D2 * t4 
     #* t6 * (t221 * t199 - t201) - t208) * t105 * t140 / 0.720D3 - (-0.
     #90D2 * t4 * t6 * (t236 * t201 - t238 * t199 / 0.2D1 - t241) + 0.18
     #0D3 * t30 * t31 * (-t201 + t236 * t199) - t42 * t206) * t140 / 0.7
     #20D3
      t256 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t186, t187, 0.0D0, t255)
      t258 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t261 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t262 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t273 = t31 * t258
      t274 = t42 * t273
      t278 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t294 = 0.180D3 * t30 * t273
      t354 = -(-0.90D2 * t4 * t6 * (t22 * t258 / 0.2D1 + t261 - t18 * t2
     #62) + 0.180D3 * t30 * t31 * (t262 - t18 * t258) + t274) * t46 / 0.
     #1440D4 + t4 * t6 * t278 / 0.16D2 - t88 * t6 * t261 / 0.1440D4 - t6
     #0 * t6 * t262 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t262 - t95 * t258
     #) + t294) * t46 * t105 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t113 * t
     #258 / 0.2D1 + t261 - t111 * t262) + 0.180D3 * t30 * t31 * (t262 - 
     #t111 * t258) + t274) * t105 / 0.1440D4 - t81 * t6 * t258 / 0.1440D
     #4 - (-0.90D2 * t4 * t6 * (-t132 * t258 + t262) + t294) * t46 * t14
     #0 / 0.720D3 + t143 * t258 * t46 * t145 / 0.8D1 - (-0.90D2 * t4 * t
     #6 * (t262 - t152 * t258) + t294) * t105 * t140 / 0.720D3 - (-0.90D
     #2 * t4 * t6 * (-t165 * t262 + t167 * t258 / 0.2D1 + t261) + 0.180D
     #3 * t30 * t31 * (t262 - t165 * t258) + t274) * t140 / 0.720D3
      t355 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t354)
      t357 = t2 * x3
      t358 = -0.1D1 + x3
      t359 = t2 * t358
      t361 = t49 * t358
      t364 = log(-0.4D1 * x3 * t14 * t361)
      t365 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t367 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t368 = t364 ** 2
      t369 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t381 = t31 * t369
      t389 = log(-0.4D1 * t92 * t14 * t361)
      t396 = 0.180D3 * t30 * t381
      t404 = log(-0.4D1 * t129 * t14 * t361)
      t418 = -(-0.90D2 * t4 * t6 * (t364 * t365 - t367 - t368 * t369 / 0
     #.2D1) + 0.180D3 * t30 * t31 * (-t365 + t364 * t369) - t42 * t381) 
     #* t46 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t389 * t369 - t365) - t39
     #6) * t46 * t105 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t404 * t369 - t
     #365) - t396) * t46 * t140 / 0.720D3 - t143 * t369 * t46 * t145 / 0
     #.8D1
      t419 = FJET(XB1, XB2, s, t357, 0.0D0, -t359, 0.0D0, 0.0D0, t418)
      t421 = 0.2D1 * t92
      t422 = cos(t7)
      t423 = -0.1D1 + x2
      t427 = Sqrt(x2 * t423 * x3 * t358)
      t429 = 0.2D1 * t422 * t427
      t431 = t2 * (0.1D1 - x2 - x3 + t421 + t429)
      t433 = t2 * (-x3 + t421 - x2 + t429)
      t434 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t435 = t92 * t9
      t440 = log(0.4D1 * t435 * t15 * t358 * t423)
      t441 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t458 = -(-0.90D2 * t4 * t6 * (t434 - t440 * t441) + 0.180D3 * t30 
     #* t31 * t441) * t46 * t105 / 0.1440D4 + t143 * t441 * t46 * t145 /
     # 0.8D1
      t459 = FJET(XB1, XB2, s, t431, 0.0D0, -t433, 0.0D0, 0.0D0, t458)
      t462 = x2 * s * t1
      t463 = t423 * s
      t464 = t463 * t1
      t465 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t466 = t15 * t423
      t469 = log(-0.4D1 * t435 * t466)
      t470 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t476 = t31 * t470
      t478 = 0.180D3 * t30 * t476
      t486 = log(-0.4D1 * x2 * t9 * t466)
      t487 = t486 ** 2
      t490 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t512 = log(-0.4D1 * t149 * t9 * t466)
      t522 = -(-0.90D2 * t4 * t6 * (-t465 + t469 * t470) - t478) * t46 *
     # t105 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t487 * t470 / 0.2D1 - t4
     #90 + t486 * t465) + 0.180D3 * t30 * t31 * (-t465 + t486 * t470) - 
     #t42 * t476) * t105 / 0.1440D4 - t143 * t470 * t46 * t145 / 0.8D1 -
     # (-0.90D2 * t4 * t6 * (t512 * t470 - t465) - t478) * t105 * t140 /
     # 0.720D3
      t523 = FJET(XB1, XB2, s, t462, 0.0D0, -t464, 0.0D0, 0.0D0, t522)
      t525 = t1 * t185
      t526 = t463 * t525
      t529 = t2 * t185 * x2 * t191
      t534 = s * t13 * x2 * x1 * t185 * t191
      t535 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t540 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t546 = log(-0.4D1 * t149 * t49 * t14 * t423 * t193)
      t559 = t143 * t535 * t46 * t145 / 0.8D1 - (-0.90D2 * t4 * t6 * (t5
     #40 - t546 * t535) + 0.180D3 * t30 * t31 * t535) * t105 * t140 / 0.
     #720D3
      t560 = FJET(XB1, XB2, s, t526, t187, -t529, 0.0D0, -t534, t559)
      t562 = t358 * s
      t563 = t562 * t525
      t565 = t562 * t1 * x1
      t567 = x3 * s * t525
      t568 = x3 * x1
      t569 = t2 * t568
      t570 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t578 = log(-0.4D1 * t129 * t14 * t9 * t12 * t191 * t192 * t358)
      t579 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t596 = -(-0.90D2 * t4 * t6 * (t570 - t578 * t579) + 0.180D3 * t30 
     #* t31 * t579) * t46 * t140 / 0.720D3 + t143 * t579 * t46 * t145 / 
     #0.8D1
      t597 = FJET(XB1, XB2, s, t563, -t565, -t567, t569, 0.0D0, t596)
      t599 = t568 * z
      t600 = t92 * x1
      t601 = t92 * t189
      t606 = Sqrt(x3 * t423 * t190 * x2 * t358)
      t608 = 0.2D1 * t422 * t606
      t612 = t2 * t185 * (-x3 + t568 - t599 + t421 - t600 + t601 - x2 + 
     #t608) * t191
      t613 = x2 * x1
      t615 = 0.1D1 - x1 + t189 - x2 + t613 - t613 * z - x3 + t568 - t599
     # + t421 - t600 + t601 + t608
      t618 = t2 * t185 * t615 * t191
      t619 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, x3, x4)
      t621 = t619 * t46 * t145
      t624 = FJET(XB1, XB2, s, t612, t569, -t618, -t565, -t534, -t143 * 
     #t621 / 0.8D1)
      t629 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t630 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t636 = t31 * t630
      t638 = 0.180D3 * t30 * t636
      t659 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, 0.0D0
     #, x4)
      t673 = -(-0.90D2 * t4 * t6 * (-t629 + t197 * t630) - t638) * t46 *
     # t140 / 0.720D3 - t143 * t630 * t46 * t145 / 0.8D1 - (-0.90D2 * t4
     # * t6 * (t221 * t630 - t629) - t638) * t105 * t140 / 0.720D3 - (-0
     #.90D2 * t4 * t6 * (t236 * t629 - t238 * t630 / 0.2D1 - t659) + 0.1
     #80D3 * t30 * t31 * (t236 * t630 - t629) - t42 * t636) * t140 / 0.7
     #20D3
      t674 = FJET(XB1, XB2, s, -t186, t187, 0.0D0, 0.0D0, 0.0D0, t673)
      t676 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t679 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t680 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t691 = t31 * t676
      t702 = 0.180D3 * t30 * t691
      t720 = -(-0.90D2 * t4 * t6 * (-t368 * t676 / 0.2D1 - t679 + t364 *
     # t680) + 0.180D3 * t30 * t31 * (-t680 + t364 * t676) - t42 * t691)
     # * t46 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t680 + t389 * t676) - t
     #702) * t46 * t105 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t680 + t404 
     #* t676) - t702) * t46 * t140 / 0.720D3 - t143 * t676 * t46 * t145 
     #/ 0.8D1
      t721 = FJET(XB1, XB2, s, -t359, 0.0D0, t357, 0.0D0, 0.0D0, t720)
      t723 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t724 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t741 = -(-0.90D2 * t4 * t6 * (t723 - t440 * t724) + 0.180D3 * t30 
     #* t31 * t724) * t46 * t105 / 0.1440D4 + t143 * t724 * t46 * t145 /
     # 0.8D1
      t742 = FJET(XB1, XB2, s, -t433, 0.0D0, t431, 0.0D0, 0.0D0, t741)
      t744 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t745 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t751 = t31 * t745
      t753 = 0.180D3 * t30 * t751
      t759 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t788 = -(-0.90D2 * t4 * t6 * (-t744 + t469 * t745) - t753) * t46 *
     # t105 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t486 * t744 - t759 - t487
     # * t745 / 0.2D1) + 0.180D3 * t30 * t31 * (-t744 + t486 * t745) - t
     #42 * t751) * t105 / 0.1440D4 - t143 * t745 * t46 * t145 / 0.8D1 - 
     #(-0.90D2 * t4 * t6 * (-t744 + t512 * t745) - t753) * t105 * t140 /
     # 0.720D3
      t789 = FJET(XB1, XB2, s, -t464, 0.0D0, t462, 0.0D0, 0.0D0, t788)
      t791 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t793 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t198, 0.0D0, x3, x
     #4)
      t809 = -(-0.90D2 * t4 * t6 * (-t578 * t791 + t793) + 0.180D3 * t30
     # * t31 * t791) * t46 * t140 / 0.720D3 + t143 * t791 * t46 * t145 /
     # 0.8D1
      t810 = FJET(XB1, XB2, s, -t567, t569, t563, -t565, 0.0D0, t809)
      t812 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t817 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t198, x2, 0.0D0, x
     #4)
      t830 = t143 * t812 * t46 * t145 / 0.8D1 - (-0.90D2 * t4 * t6 * (t8
     #17 - t546 * t812) + 0.180D3 * t30 * t31 * t812) * t105 * t140 / 0.
     #720D3
      t831 = FJET(XB1, XB2, s, -t529, 0.0D0, t526, t187, -t534, t830)
      t833 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t198, x2, x3, x4)
      t835 = t833 * t46 * t145
      t838 = FJET(XB1, XB2, s, -t618, -t565, t612, t569, -t534, -t143 * 
     #t835 / 0.8D1)
      rrqg2qght9s1e0 = t183 * t182 + t256 * t255 + t355 * t354 + t419 * 
     #t418 + t459 * t458 + t523 * t522 + t560 * t559 + t597 * t596 - t62
     #4 * 0.3141592653589793D1 * t31 * t621 / 0.8D1 + t674 * t673 + t721
     # * t720 + t742 * t741 + t789 * t788 + t810 * t809 + t831 * t830 - 
     #t838 * 0.3141592653589793D1 * t31 * t835 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * x3 * t10 * t16)
      t20 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t26 = 0.3141592653589793D1 * lh
      t27 = t3 * t6
      t30 = 0.180D3 * t26 * t27 * t20
      t32 = 0.1D1 / x3
      t35 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t40 = t13 * t10
      t43 = log(0.4D1 * t40 * t15)
      t44 = t43 * 0.3141592653589793D1
      t47 = (0.180D3 * t26 + 0.90D2 * t44) * t3
      t53 = t43 ** 2
      t56 = lh ** 2
      t58 = 0.3141592653589793D1 ** 2
      t63 = (-0.180D3 * t44 * lh - 0.45D2 * t53 * 0.3141592653589793D1 +
     # 0.3141592653589793D1 * (-0.180D3 * t56 + 0.30D2 * t58)) * t3
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
     # * t20 / 0.1440D4 + t67 * t68 * t69 / 0.16D2 - (-0.90D2 * t4 * t6 
     #* (-t76 * t20 + t7) + t30) * t69 / 0.1440D4 + t67 * t20 * t69 * t8
     #6 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t94 * t20 + t7) + t30) * t86 / 
     #0.720D3 + t67 * t68 * t86 / 0.8D1
      t107 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t106)
      t109 = -0.1D1 + x1
      t110 = t2 * t109
      t111 = t2 * x1
      t112 = -t109
      t113 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t112, 0.0D0, 0.0D0
     #, x4)
      t118 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, t112, 0.0D0, 0.0D0
     #, x4)
      t122 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t124 = t109 ** 2
      t128 = log(0.4D1 * t91 * t13 * t15 * t122 * t124)
      t144 = -t67 * t113 * t69 * t86 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t1
     #18 + t128 * t113) - 0.180D3 * t26 * t27 * t113) * t86 / 0.720D3 - 
     #t67 * t113 * t32 * t86 / 0.8D1
      t145 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t110, t111, 0.0D0, t144)
      t147 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t151 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t155 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0
     #D0, x4)
      t163 = 0.180D3 * t26 * t27 * t151
      t167 = t151 * t32
      t197 = t4 * t6 * t147 / 0.16D2 - t63 * t6 * t151 / 0.1440D4 - (-0.
     #90D2 * t4 * t6 * (t155 - t19 * t151) + t163) * t32 / 0.1440D4 + t6
     #7 * t167 * t69 / 0.16D2 - (-0.90D2 * t4 * t6 * (t155 - t76 * t151)
     # + t163) * t69 / 0.1440D4 - t47 * t6 * t155 / 0.1440D4 + t67 * t15
     #1 * t69 * t86 / 0.8D1 - (-0.90D2 * t4 * t6 * (t155 - t94 * t151) +
     # t163) * t86 / 0.720D3 + t67 * t167 * t86 / 0.8D1
      t198 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t197)
      t200 = t2 * x3
      t201 = -0.1D1 + x3
      t202 = t2 * t201
      t203 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t208 = log(-0.4D1 * x3 * t15 * t40 * t201)
      t209 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t221 = t209 * t32
      t228 = -(-0.90D2 * t4 * t6 * (-t203 + t208 * t209) - 0.180D3 * t26
     # * t27 * t209) * t32 / 0.1440D4 - t67 * t221 * t69 / 0.16D2 - t67 
     #* t221 * t86 / 0.8D1
      t229 = FJET(XB1, XB2, s, t200, 0.0D0, -t202, 0.0D0, 0.0D0, t228)
      t232 = 0.2D1 * x2 * x3
      t233 = cos(t8)
      t234 = -0.1D1 + x2
      t238 = Sqrt(x2 * t234 * x3 * t201)
      t240 = 0.2D1 * t233 * t238
      t242 = t2 * (0.1D1 - x2 - x3 + t232 + t240)
      t244 = t2 * (-x3 + t232 - x2 + t240)
      t245 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t250 = FJET(XB1, XB2, s, t242, 0.0D0, -t244, 0.0D0, 0.0D0, t67 * t
     #245 * t32 * t69 / 0.16D2)
      t254 = t32 * t69
      t259 = x2 * s * t1
      t260 = t234 * s
      t261 = t260 * t1
      t262 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t267 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t272 = log(-0.4D1 * x2 * t10 * t16 * t234)
      t288 = -t67 * t262 * t32 * t69 / 0.16D2 - (-0.90D2 * t4 * t6 * (-t
     #267 + t272 * t262) - 0.180D3 * t26 * t27 * t262) * t69 / 0.1440D4 
     #- t67 * t262 * t69 * t86 / 0.8D1
      t289 = FJET(XB1, XB2, s, t259, 0.0D0, -t261, 0.0D0, 0.0D0, t288)
      t291 = t1 * t109
      t292 = t260 * t291
      t295 = t2 * t109 * x2 * t122
      t300 = s * t14 * x2 * x1 * t109 * t122
      t301 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t112, x2, 0.0D0, x
     #4)
      t306 = FJET(XB1, XB2, s, t292, t111, -t295, 0.0D0, -t300, t67 * t3
     #01 * t69 * t86 / 0.8D1)
      t310 = t69 * t86
      t314 = t201 * s
      t315 = t314 * t291
      t317 = t314 * t1 * x1
      t319 = x3 * s * t291
      t321 = t2 * x1 * x3
      t322 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t112, 0.0D0, x3, x
     #4)
      t327 = FJET(XB1, XB2, s, t315, -t317, -t319, t321, 0.0D0, t67 * t3
     #22 * t32 * t86 / 0.8D1)
      t331 = t32 * t86
      t335 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t112, 0.0D0, 0.0D0
     #, x4)
      t341 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, t112, 0.0D0, 0.0D0
     #, x4)
      t356 = -t67 * t335 * t69 * t86 / 0.8D1 - (-0.90D2 * t4 * t6 * (t12
     #8 * t335 - t341) - 0.180D3 * t26 * t27 * t335) * t86 / 0.720D3 - t
     #67 * t335 * t32 * t86 / 0.8D1
      t357 = FJET(XB1, XB2, s, -t110, t111, 0.0D0, 0.0D0, 0.0D0, t356)
      t359 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t360 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t372 = t360 * t32
      t379 = -(-0.90D2 * t4 * t6 * (-t359 + t208 * t360) - 0.180D3 * t26
     # * t27 * t360) * t32 / 0.1440D4 - t67 * t372 * t69 / 0.16D2 - t67 
     #* t372 * t86 / 0.8D1
      t380 = FJET(XB1, XB2, s, -t202, 0.0D0, t200, 0.0D0, 0.0D0, t379)
      t382 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t387 = FJET(XB1, XB2, s, -t244, 0.0D0, t242, 0.0D0, 0.0D0, t67 * t
     #382 * t32 * t69 / 0.16D2)
      t394 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t399 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t415 = -t67 * t394 * t32 * t69 / 0.16D2 - (-0.90D2 * t4 * t6 * (-t
     #399 + t272 * t394) - 0.180D3 * t26 * t27 * t394) * t69 / 0.1440D4 
     #- t67 * t394 * t69 * t86 / 0.8D1
      t416 = FJET(XB1, XB2, s, -t261, 0.0D0, t259, 0.0D0, 0.0D0, t415)
      t418 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t112, 0.0D0, x3, x
     #4)
      t423 = FJET(XB1, XB2, s, -t319, t321, t315, -t317, 0.0D0, t67 * t4
     #18 * t32 * t86 / 0.8D1)
      t430 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t112, x2, 0.0D0, x
     #4)
      t435 = FJET(XB1, XB2, s, -t295, 0.0D0, t292, t111, -t300, t67 * t4
     #30 * t69 * t86 / 0.8D1)
      rrqg2qght9s1em1 = t107 * t106 + t145 * t144 + t198 * t197 + t229 *
     # t228 + t250 * 0.3141592653589793D1 * t3 * t6 * t245 * t254 / 0.16
     #D2 + t289 * t288 + t306 * 0.3141592653589793D1 * t3 * t6 * t301 * 
     #t310 / 0.8D1 + t327 * 0.3141592653589793D1 * t3 * t6 * t322 * t331
     # / 0.8D1 + t357 * t356 + t380 * t379 + t387 * 0.3141592653589793D1
     # * t3 * t6 * t382 * t254 / 0.16D2 + t416 * t415 + t423 * 0.3141592
     #653589793D1 * t3 * t6 * t418 * t331 / 0.8D1 + t435 * 0.31415926535
     #89793D1 * t3 * t6 * t430 * t310 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t27 = z ** 2
      t30 = Sin(x4 * 0.3141592653589793D1)
      t31 = t30 ** 2
      t33 = t1 ** 2
      t34 = t33 ** 2
      t37 = log(0.4D1 / t27 * t31 * t34)
      t41 = (0.180D3 * 0.3141592653589793D1 * lh + 0.90D2 * t37 * 0.3141
     #592653589793D1) * t3
      t44 = t4 * t8 * t9 / 0.16D2 + t4 * t8 * t13 / 0.16D2 + t4 * t8 * t
     #17 / 0.8D1 + t4 * t6 * t21 / 0.16D2 - t41 * t8 / 0.1440D4
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t44)
      t47 = -0.1D1 + x1
      t48 = t2 * t47
      t49 = t2 * x1
      t50 = -t47
      t51 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, t50, 0.0D0, 0.0D0, 
     #x4)
      t53 = t6 * t51 * t17
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t48, t49, 0.0D0, -t4 * t53 
     #/ 0.8D1)
      t61 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t62 = t6 * t61
      t72 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t78 = t4 * t62 * t9 / 0.16D2 + t4 * t62 * t13 / 0.16D2 + t4 * t62 
     #* t17 / 0.8D1 + t4 * t6 * t72 / 0.16D2 - t41 * t62 / 0.1440D4
      t79 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t78)
      t81 = t2 * x3
      t83 = t2 * (-0.1D1 + x3)
      t84 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t86 = t6 * t84 * t9
      t89 = FJET(XB1, XB2, s, t81, 0.0D0, -t83, 0.0D0, 0.0D0, -t4 * t86 
     #/ 0.16D2)
      t95 = x2 * s * t1
      t98 = (-0.1D1 + x2) * s * t1
      t99 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0, 
     #x4)
      t101 = t6 * t99 * t13
      t104 = FJET(XB1, XB2, s, t95, 0.0D0, -t98, 0.0D0, 0.0D0, -t4 * t10
     #1 / 0.16D2)
      t109 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3,
     # x4)
      t111 = t6 * t109 * t9
      t114 = FJET(XB1, XB2, s, -t83, 0.0D0, t81, 0.0D0, 0.0D0, -t4 * t11
     #1 / 0.16D2)
      t119 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0.0D0,
     # x4)
      t121 = t6 * t119 * t13
      t124 = FJET(XB1, XB2, s, -t98, 0.0D0, t95, 0.0D0, 0.0D0, -t4 * t12
     #1 / 0.16D2)
      t129 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, t50, 0.0D0, 0.0D0,
     # x4)
      t131 = t6 * t129 * t17
      t134 = FJET(XB1, XB2, s, -t48, t49, 0.0D0, 0.0D0, 0.0D0, -t4 * t13
     #1 / 0.8D1)
      rrqg2qght9s1em2 = t45 * t44 - t56 * 0.3141592653589793D1 * t3 * t5
     #3 / 0.8D1 + t79 * t78 - t89 * 0.3141592653589793D1 * t3 * t86 / 0.
     #16D2 - t104 * 0.3141592653589793D1 * t3 * t101 / 0.16D2 - t114 * 0
     #.3141592653589793D1 * t3 * t111 / 0.16D2 - t124 * 0.31415926535897
     #93D1 * t3 * t121 / 0.16D2 - t134 * 0.3141592653589793D1 * t3 * t13
     #1 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D0
     #, x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t6 * 
     #t7 / 0.16D2)
      t13 = t3 * t6
      t16 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, 0.0D
     #0, x4)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t4 * t6 * 
     #t16 / 0.16D2)
      rrqg2qght9s1em3 = t11 * 0.3141592653589793D1 * t13 * t7 / 0.16D2 +
     # t20 * 0.3141592653589793D1 * t13 * t16 / 0.16D2

      end function



      doubleprecision function rrqg2qght9s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrqg2qght9s1em4 = 0.0D0

      end function


      doubleprecision function rrqg2qght9s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqg2qgh91J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t13 * t16
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = t17 * t19
      t22 = log(0.4D1 * t20)
      t23 = t22 ** 2
      t24 = t23 * 0.3141592653589793D1
      t27 = 0.3141592653589793D1 ** 2
      t30 = lh ** 2
      t33 = -0.60D2 * lh * t27 + 0.2884936567583026D3 + 0.120D3 * t30 * 
     #lh
      t34 = 0.3141592653589793D1 * t33
      t36 = t23 * t22 * 0.3141592653589793D1
      t38 = t22 * 0.3141592653589793D1
      t41 = -0.180D3 * t30 + 0.30D2 * t27
      t44 = (0.90D2 * t24 * lh + t34 + 0.15D2 * t36 - t38 * t41) * t3
      t45 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t52 = 0.3141592653589793D1 * t41
      t54 = (-0.180D3 * t38 * lh - 0.45D2 * t24 + t52) * t3
      t55 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t59 = 0.3141592653589793D1 * lh
      t63 = (0.180D3 * t59 + 0.90D2 * t38) * t3
      t64 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t74 = t27 ** 2
      t75 = t30 ** 2
      t81 = t23 ** 2
      t85 = (-0.30D2 * t36 * lh + t24 * t41 / 0.2D1 - t38 * t33 + 0.3141
     #592653589793D1 * (-0.5769873135166051D3 * lh - t74 - 0.60D2 * t75 
     #+ 0.60D2 * t30 * t27) - 0.15D2 / 0.4D1 * t81 * 0.3141592653589793D
     #1) * t3
      t86 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t90 = x1 ** 2
      t91 = x3 * t90
      t94 = log(0.4D1 * t91 * t20)
      t96 = t94 ** 2
      t103 = t3 * t6
      t109 = t103 * t86
      t112 = 0.1D1 / x3
      t114 = 0.1D1 / x1
      t117 = t90 * t16
      t118 = t13 * t19
      t121 = log(0.4D1 * t117 * t118)
      t126 = t121 ** 2
      t129 = t126 * t121
      t137 = t34 * t109
      t148 = x2 * x3
      t149 = t148 * t90
      t152 = log(0.4D1 * t149 * t20)
      t154 = -0.1D1 + x2
      t159 = log(-0.4D1 * t149 * t17 * t19 * t154)
      t160 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t162 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t167 = t86 - t160
      t168 = t103 * t167
      t173 = 0.1D1 / x2
      t174 = t173 * t114
      t177 = x2 * t90
      t180 = log(0.4D1 * t177 * t20)
      t181 = t180 ** 2
      t188 = log(-0.4D1 * t177 * t16 * t118 * t154)
      t191 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t192 = t188 ** 2
      t210 = x3 * t13
      t211 = t16 * t19
      t214 = log(0.4D1 * t210 * t211)
      t219 = t214 ** 2
      t222 = t219 * t214
      t242 = log(0.4D1 * t148 * t20)
      t243 = t242 ** 2
      t247 = t148 * t13
      t248 = t211 * t154
      t251 = log(-0.4D1 * t247 * t248)
      t253 = t251 ** 2
      t273 = x2 * t13
      t276 = log(-0.4D1 * t273 * t248)
      t280 = log(0.4D1 * t273 * t211)
      t285 = t280 ** 2
      t286 = t285 * t280
      t290 = t276 ** 2
      t293 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t294 = t290 * t276
      t318 = t4 * t6 * t7 / 0.16D2 - t44 * t6 * t45 / 0.1440D4 - t54 * t
     #6 * t55 / 0.1440D4 - t63 * t6 * t64 / 0.1440D4 - t85 * t6 * t86 / 
     #0.1440D4 - (-0.90D2 * t4 * t6 * (-t94 * t45 + t55 + t96 * t86 / 0.
     #2D1) + 0.180D3 * t59 * t103 * (t45 - t94 * t86) + t52 * t109) * t1
     #12 * t114 / 0.720D3 - (t52 * t103 * (t45 - t121 * t86) - 0.90D2 * 
     #t4 * t6 * (t126 * t45 / 0.2D1 + t64 - t129 * t86 / 0.6D1 - t121 * 
     #t55) + t137 + 0.180D3 * t59 * t103 * (-t121 * t45 + t55 + t126 * t
     #86 / 0.2D1)) * t114 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t152 * t86 
     #+ t45 + t159 * t160 - t162) + 0.180D3 * t59 * t168) * t112 * t174 
     #/ 0.720D3 - (-0.90D2 * t4 * t6 * (t181 * t86 / 0.2D1 + t188 * t162
     # - t180 * t45 + t55 - t191 - t192 * t160 / 0.2D1) + 0.180D3 * t59 
     #* t103 * (-t180 * t86 + t45 + t188 * t160 - t162) + t52 * t168) * 
     #t173 * t114 / 0.720D3 + (t52 * t103 * (t214 * t86 - t45) - 0.90D2 
     #* t4 * t6 * (-t219 * t45 / 0.2D1 - t64 + t222 * t86 / 0.6D1 + t214
     # * t55) - t137 + 0.180D3 * t59 * t103 * (t214 * t45 - t55 - t219 *
     # t86 / 0.2D1)) * t112 / 0.1440D4 + (-0.90D2 * t4 * t6 * (-t243 * t
     #86 / 0.2D1 + t242 * t45 - t55 - t251 * t162 + t191 + t253 * t160 /
     # 0.2D1) + 0.180D3 * t59 * t103 * (t242 * t86 + t162 - t45 - t251 *
     # t160) - t52 * t103 * t167) * t112 * t173 / 0.1440D4 - (t52 * t103
     # * (-t162 + t276 * t160 + t45 - t280 * t86) - 0.90D2 * t4 * t6 * (
     #t64 - t286 * t86 / 0.6D1 - t280 * t55 - t290 * t162 / 0.2D1 - t293
     # + t294 * t160 / 0.6D1 + t285 * t45 / 0.2D1 + t276 * t191) + t34 *
     # t168 + 0.180D3 * t59 * t103 * (-t290 * t160 / 0.2D1 + t276 * t162
     # - t191 - t280 * t45 + t55 + t285 * t86 / 0.2D1)) * t173 / 0.1440D
     #4
      t319 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t318)
      t321 = t2 * x1
      t322 = -0.1D1 + x1
      t323 = t2 * t322
      t325 = 0.1D1 / t11
      t326 = t325 * t19
      t327 = x1 * z
      t328 = -z - x1 + t327
      t329 = 0.1D1 / t328
      t330 = t322 ** 2
      t331 = t329 * t330
      t335 = log(-0.4D1 * t91 * t16 * t326 * t331)
      t336 = t335 ** 2
      t337 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t340 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t342 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t352 = t103 * t337
      t353 = t52 * t352
      t357 = t117 * t325
      t358 = t19 * t329
      t362 = log(-0.4D1 * t357 * t358 * t330)
      t367 = t362 ** 2
      t368 = t367 * t362
      t373 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t390 = t148 * t90 * t19
      t391 = t16 * t325
      t392 = t391 * t331
      t395 = log(-0.4D1 * t390 * t392)
      t409 = log(-0.4D1 * t177 * t19 * t392)
      t410 = t409 ** 2
      t427 = -(-0.90D2 * t4 * t6 * (-t336 * t337 / 0.2D1 + t335 * t340 -
     # t342) + 0.180D3 * t59 * t103 * (t335 * t337 - t340) - t353) * t11
     #2 * t114 / 0.720D3 - (t52 * t103 * (t362 * t337 - t340) - 0.90D2 *
     # t4 * t6 * (t368 * t337 / 0.6D1 - t367 * t340 / 0.2D1 - t373 + t36
     #2 * t342) - t34 * t352 + 0.180D3 * t59 * t103 * (-t367 * t337 / 0.
     #2D1 + t362 * t340 - t342)) * t114 / 0.720D3 - (-0.90D2 * t4 * t6 *
     # (t395 * t337 - t340) - 0.180D3 * t59 * t352) * t112 * t174 / 0.72
     #0D3 - (-0.90D2 * t4 * t6 * (-t410 * t337 / 0.2D1 - t342 + t409 * t
     #340) + 0.180D3 * t59 * t103 * (t409 * t337 - t340) - t353) * t173 
     #* t114 / 0.720D3
      t428 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t321, -t323, 0.0D0, t427)
      t430 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t433 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t434 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t436 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t437 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t439 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t452 = t439 - t430
      t467 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t473 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t481 = -t103 * t452
      t496 = rrqg2qgh92J5(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t513 = t103 * t430
      t514 = t34 * t513
      t610 = (-0.90D2 * t4 * t6 * (-t243 * t430 / 0.2D1 - t433 + t242 * 
     #t434 + t436 - t251 * t437 + t253 * t439 / 0.2D1) + 0.180D3 * t59 *
     # t103 * (t242 * t430 + t437 - t434 - t251 * t439) + t52 * t103 * t
     #452) * t112 * t173 / 0.1440D4 - (t52 * t103 * (-t437 + t276 * t439
     # + t434 - t280 * t430) - 0.90D2 * t4 * t6 * (t285 * t434 / 0.2D1 +
     # t276 * t436 + t467 - t286 * t430 / 0.6D1 - t280 * t433 - t290 * t
     #437 / 0.2D1 - t473 + t294 * t439 / 0.6D1) + t34 * t481 + 0.180D3 *
     # t59 * t103 * (-t290 * t439 / 0.2D1 - t436 + t276 * t437 + t433 - 
     #t280 * t434 + t285 * t430 / 0.2D1)) * t173 / 0.1440D4 + t4 * t6 * 
     #t496 / 0.16D2 + (t52 * t103 * (-t434 + t214 * t430) - 0.90D2 * t4 
     #* t6 * (-t219 * t434 / 0.2D1 - t467 + t222 * t430 / 0.6D1 + t214 *
     # t433) - t514 + 0.180D3 * t59 * t103 * (-t219 * t430 / 0.2D1 - t43
     #3 + t214 * t434)) * t112 / 0.1440D4 - t54 * t6 * t433 / 0.1440D4 -
     # t63 * t6 * t467 / 0.1440D4 - t85 * t6 * t430 / 0.1440D4 - t44 * t
     #6 * t434 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t433 - t94 * t434 + t9
     #6 * t430 / 0.2D1) + 0.180D3 * t59 * t103 * (t434 - t94 * t430) + t
     #52 * t513) * t112 * t114 / 0.720D3 - (t52 * t103 * (t434 - t121 * 
     #t430) - 0.90D2 * t4 * t6 * (t126 * t434 / 0.2D1 - t129 * t430 / 0.
     #6D1 - t121 * t433 + t467) + t514 + 0.180D3 * t59 * t103 * (t433 - 
     #t121 * t434 + t126 * t430 / 0.2D1)) * t114 / 0.720D3 - (-0.90D2 * 
     #t4 * t6 * (t434 - t152 * t430 - t437 + t159 * t439) + 0.180D3 * t5
     #9 * t481) * t112 * t174 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t436 + 
     #t433 + t188 * t437 + t181 * t430 / 0.2D1 - t192 * t439 / 0.2D1 - t
     #180 * t434) + 0.180D3 * t59 * t103 * (t434 - t180 * t430 - t437 + 
     #t188 * t439) + t52 * t481) * t173 * t114 / 0.720D3
      t611 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t610)
      t613 = t2 * x3
      t614 = -0.1D1 + x3
      t615 = t2 * t614
      t616 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t617 = t211 * t614
      t620 = log(-0.4D1 * t210 * t617)
      t621 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t626 = t620 ** 2
      t629 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t630 = t626 * t620
      t633 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t639 = t103 * t621
      t655 = log(-0.4D1 * t91 * t19 * t17 * t614)
      t657 = t655 ** 2
      t674 = t148 * t117
      t675 = t154 * t614
      t679 = log(0.4D1 * t674 * t118 * t675)
      t680 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t682 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t687 = log(-0.4D1 * t149 * t211 * t13 * t614)
      t693 = t680 - t621
      t704 = log(0.4D1 * t247 * t211 * t675)
      t705 = t704 ** 2
      t709 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t712 = log(-0.4D1 * t247 * t617)
      t713 = t712 ** 2
      t734 = (t52 * t103 * (t616 - t620 * t621) - 0.90D2 * t4 * t6 * (t6
     #26 * t616 / 0.2D1 + t629 - t630 * t621 / 0.6D1 - t620 * t633) + t3
     #4 * t639 + 0.180D3 * t59 * t103 * (t626 * t621 / 0.2D1 + t633 - t6
     #20 * t616)) * t112 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t655 * t616 
     #- t657 * t621 / 0.2D1 - t633) + 0.180D3 * t59 * t103 * (-t616 + t6
     #55 * t621) - t52 * t639) * t112 * t114 / 0.720D3 - (-0.90D2 * t4 *
     # t6 * (-t679 * t680 - t616 + t682 + t687 * t621) + 0.180D3 * t59 *
     # t103 * t693) * t112 * t174 / 0.720D3 + (-0.90D2 * t4 * t6 * (-t70
     #5 * t680 / 0.2D1 + t704 * t682 - t709 + t713 * t621 / 0.2D1 + t633
     # - t712 * t616) + 0.180D3 * t59 * t103 * (-t682 + t616 - t712 * t6
     #21 + t704 * t680) - t52 * t103 * t693) * t112 * t173 / 0.1440D4
      t735 = FJET(XB1, XB2, s, 0.0D0, t613, 0.0D0, -t615, 0.0D0, t734)
      t737 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t738 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t745 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t748 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t754 = t103 * t738
      t784 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t785 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t791 = t785 - t738
      t801 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t823 = (t52 * t103 * (t737 - t620 * t738) - 0.90D2 * t4 * t6 * (t6
     #26 * t737 / 0.2D1 + t745 - t630 * t738 / 0.6D1 - t620 * t748) + t3
     #4 * t754 + 0.180D3 * t59 * t103 * (-t620 * t737 + t748 + t626 * t7
     #38 / 0.2D1)) * t112 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t748 + t65
     #5 * t737 - t657 * t738 / 0.2D1) + 0.180D3 * t59 * t103 * (t655 * t
     #738 - t737) - t52 * t754) * t112 * t114 / 0.720D3 - (-0.90D2 * t4 
     #* t6 * (t687 * t738 - t737 + t784 - t679 * t785) + 0.180D3 * t59 *
     # t103 * t791) * t112 * t174 / 0.720D3 + (-0.90D2 * t4 * t6 * (-t70
     #5 * t785 / 0.2D1 - t801 + t704 * t784 + t748 + t713 * t738 / 0.2D1
     # - t712 * t737) + 0.180D3 * t59 * t103 * (-t784 + t704 * t785 + t7
     #37 - t712 * t738) - t52 * t103 * t791) * t112 * t173 / 0.1440D4
      t824 = FJET(XB1, XB2, s, 0.0D0, -t615, 0.0D0, t613, 0.0D0, t823)
      t826 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t829 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t830 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t841 = t103 * t826
      t842 = t52 * t841
      t850 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t896 = -(-0.90D2 * t4 * t6 * (-t336 * t826 / 0.2D1 - t829 + t335 *
     # t830) + 0.180D3 * t59 * t103 * (-t830 + t335 * t826) - t842) * t1
     #12 * t114 / 0.720D3 - (t52 * t103 * (-t830 + t362 * t826) - 0.90D2
     # * t4 * t6 * (-t850 - t367 * t830 / 0.2D1 + t368 * t826 / 0.6D1 + 
     #t362 * t829) - t34 * t841 + 0.180D3 * t59 * t103 * (-t367 * t826 /
     # 0.2D1 - t829 + t362 * t830)) * t114 / 0.720D3 - (-0.90D2 * t4 * t
     #6 * (t395 * t826 - t830) - 0.180D3 * t59 * t841) * t112 * t174 / 0
     #.720D3 - (-0.90D2 * t4 * t6 * (-t410 * t826 / 0.2D1 - t829 + t409 
     #* t830) + 0.180D3 * t59 * t103 * (-t830 + t409 * t826) - t842) * t
     #173 * t114 / 0.720D3
      t897 = FJET(XB1, XB2, s, t321, -t323, 0.0D0, 0.0D0, 0.0D0, t896)
      t899 = x3 * x1
      t900 = t2 * t899
      t902 = t1 * t322
      t903 = x3 * s * t902
      t904 = t614 * s
      t905 = t1 * x1
      t906 = t904 * t905
      t907 = t904 * t902
      t914 = log(0.4D1 * t91 * t211 * t325 * t329 * t330 * t614)
      t915 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t917 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t918 = t914 ** 2
      t919 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t931 = t103 * t919
      t940 = log(0.4D1 * t390 * t391 * t331 * t614)
      t952 = -(-0.90D2 * t4 * t6 * (-t914 * t915 + t917 + t918 * t919 / 
     #0.2D1) + 0.180D3 * t59 * t103 * (t915 - t914 * t919) + t52 * t931)
     # * t112 * t114 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t940 * t919 + t9
     #15) + 0.180D3 * t59 * t931) * t112 * t174 / 0.720D3
      t953 = FJET(XB1, XB2, s, t900, -t903, -t906, t907, 0.0D0, t952)
      t955 = x3 * z
      t956 = t899 * z
      t957 = t148 * z
      t958 = t148 * x1
      t959 = t148 * t327
      t960 = cos(t14)
      t965 = Sqrt(-x3 * t154 * t328 * x2 * t614)
      t967 = 0.2D1 * t960 * t965
      t971 = t2 * x1 * (-t955 - t899 + t956 + t957 + t958 - t959 - x2 + 
     #t148 + t967) * t329
      t973 = x2 * x1
      t975 = z + x1 - t327 - x2 * z - t973 + t973 * z - t955 - t899 + t9
     #56 + t957 + t958 - t959 + t148 + t967
      t978 = t2 * x1 * t975 * t329
      t983 = s * t18 * x2 * x1 * t322 * t329
      t984 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t986 = t330 * t154
      t991 = log(-0.4D1 * t148 * t357 * t358 * t986 * t614)
      t992 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t1001 = -0.90D2 * t4 * t6 * (-t984 + t991 * t992) - 0.180D3 * t59 
     #* t103 * t992
      t1005 = FJET(XB1, XB2, s, t971, -t903, -t978, t907, t983, -t1001 *
     # t112 * t174 / 0.720D3)
      t1008 = t112 * t173 * t114
      t1012 = t154 * s * t905
      t1014 = t2 * t973 * t329
      t1015 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1020 = log(0.4D1 * t674 * t326 * t331 * t154)
      t1021 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1027 = t103 * t1021
      t1037 = log(0.4D1 * t177 * t391 * t358 * t986)
      t1039 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1040 = t1037 ** 2
      t1057 = -(-0.90D2 * t4 * t6 * (t1015 - t1020 * t1021) + 0.180D3 * 
     #t59 * t1027) * t112 * t174 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t103
     #7 * t1015 + t1039 + t1040 * t1021 / 0.2D1) + 0.180D3 * t59 * t103 
     #* (t1015 - t1037 * t1021) + t52 * t1027) * t173 * t114 / 0.720D3
      t1058 = FJET(XB1, XB2, s, -t1012, -t323, -t1014, 0.0D0, t983, t105
     #7)
      t1060 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4
     #)
      t1063 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4
     #)
      t1064 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4
     #)
      t1075 = t103 * t1060
      t1091 = -(-0.90D2 * t4 * t6 * (t918 * t1060 / 0.2D1 + t1063 - t914
     # * t1064) + 0.180D3 * t59 * t103 * (t1064 - t914 * t1060) + t52 * 
     #t1075) * t112 * t114 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t940 * t10
     #60 + t1064) + 0.180D3 * t59 * t1075) * t112 * t174 / 0.720D3
      t1092 = FJET(XB1, XB2, s, -t906, t907, t900, -t903, 0.0D0, t1091)
      t1094 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1096 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1101 = t103 * t1094
      t1108 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4
     #)
      t1125 = -(-0.90D2 * t4 * t6 * (-t1020 * t1094 + t1096) + 0.180D3 *
     # t59 * t1101) * t112 * t174 / 0.720D3 - (-0.90D2 * t4 * t6 * (-t10
     #37 * t1096 + t1108 + t1040 * t1094 / 0.2D1) + 0.180D3 * t59 * t103
     # * (t1096 - t1037 * t1094) + t52 * t1101) * t173 * t114 / 0.720D3
      t1126 = FJET(XB1, XB2, s, -t1014, 0.0D0, -t1012, -t323, t983, t112
     #5)
      t1128 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t1129 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t1138 = -0.90D2 * t4 * t6 * (-t1128 + t991 * t1129) - 0.180D3 * t5
     #9 * t103 * t1129
      t1142 = FJET(XB1, XB2, s, -t978, t907, t971, -t903, t983, -t1138 *
     # t112 * t174 / 0.720D3)
      rrqg2qght9s2e1 = t319 * t318 + t428 * t427 + t611 * t610 + t735 * 
     #t734 + t824 * t823 + t897 * t896 + t953 * t952 - t1005 * t1001 * t
     #1008 / 0.720D3 + t1058 * t1057 + t1092 * t1091 + t1126 * t1125 - t
     #1142 * t1138 * t1008 / 0.720D3

      end function



      doubleprecision function rrqg2qght9s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * t10 * t16)
      t20 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t22 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t23 = t19 ** 2
      t24 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t31 = 0.3141592653589793D1 * lh
      t32 = t3 * t6
      t38 = lh ** 2
      t40 = 0.3141592653589793D1 ** 2
      t42 = -0.180D3 * t38 + 0.30D2 * t40
      t43 = 0.3141592653589793D1 * t42
      t44 = t32 * t24
      t45 = t43 * t44
      t47 = 0.1D1 / x3
      t50 = t9 * t13
      t51 = t50 * t15
      t53 = log(0.4D1 * t51)
      t54 = t53 * 0.3141592653589793D1
      t57 = t53 ** 2
      t58 = t57 * 0.3141592653589793D1
      t61 = (-0.180D3 * t54 * lh - 0.45D2 * t58 + t43) * t3
      t65 = rrqg2qgh91J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t82 = (0.90D2 * t58 * lh + 0.3141592653589793D1 * (-0.60D2 * lh * 
     #t40 + 0.2884936567583026D3 + 0.120D3 * t38 * lh) + 0.15D2 * t57 * 
     #t53 * 0.3141592653589793D1 - t54 * t42) * t3
      t89 = (0.180D3 * t31 + 0.90D2 * t54) * t3
      t93 = x2 * x3
      t96 = log(0.4D1 * t93 * t51)
      t98 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t99 = t93 * t9
      t100 = -0.1D1 + x2
      t101 = t16 * t100
      t104 = log(-0.4D1 * t99 * t101)
      t105 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t111 = t105 - t24
      t117 = 0.1D1 / x2
      t120 = x2 * t9
      t123 = log(-0.4D1 * t120 * t101)
      t124 = t123 ** 2
      t128 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t131 = log(0.4D1 * t120 * t16)
      t133 = t131 ** 2
      t146 = -t111
      t147 = t32 * t146
      t152 = x1 ** 2
      t153 = x3 * t152
      t156 = log(0.4D1 * t153 * t51)
      t166 = 0.1D1 / x1
      t169 = t4 * t6
      t171 = t117 * t166
      t175 = x2 * t152
      t178 = log(0.4D1 * t175 * t51)
      t181 = t9 * t15
      t185 = log(-0.4D1 * t175 * t13 * t181 * t100)
      t197 = t152 * t13
      t200 = log(0.4D1 * t197 * t181)
      t202 = t200 ** 2
      t217 = (-0.90D2 * t4 * t6 * (t19 * t20 - t22 - t23 * t24 / 0.2D1) 
     #+ 0.180D3 * t31 * t32 * (t19 * t24 - t20) - t45) * t47 / 0.1440D4 
     #- t61 * t6 * t20 / 0.1440D4 + t4 * t6 * t65 / 0.16D2 - t82 * t6 * 
     #t24 / 0.1440D4 - t89 * t6 * t22 / 0.1440D4 + (-0.90D2 * t4 * t6 * 
     #(t96 * t24 + t98 - t20 - t104 * t105) + 0.180D3 * t31 * t32 * t111
     #) * t47 * t117 / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t124 * t105 / 0
     #.2D1 + t123 * t98 - t128 - t131 * t20 + t22 + t133 * t24 / 0.2D1) 
     #+ 0.180D3 * t31 * t32 * (-t98 + t123 * t105 + t20 - t131 * t24) + 
     #t43 * t147) * t117 / 0.1440D4 - (-0.90D2 * t4 * t6 * (t20 - t156 *
     # t24) + 0.180D3 * t31 * t44) * t47 * t166 / 0.720D3 + t169 * t146 
     #* t47 * t171 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t178 * t24 + t20 + t
     #185 * t105 - t98) + 0.180D3 * t31 * t147) * t117 * t166 / 0.720D3 
     #- (-0.90D2 * t4 * t6 * (-t200 * t20 + t22 + t202 * t24 / 0.2D1) + 
     #0.180D3 * t31 * t32 * (t20 - t200 * t24) + t45) * t166 / 0.720D3
      t218 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t217)
      t220 = t2 * x1
      t221 = -0.1D1 + x1
      t222 = t2 * t221
      t224 = 0.1D1 / t7
      t226 = x1 * z
      t227 = -z - x1 + t226
      t228 = 0.1D1 / t227
      t229 = t221 ** 2
      t230 = t228 * t229
      t234 = log(-0.4D1 * t153 * t13 * t224 * t15 * t230)
      t235 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t237 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t242 = t32 * t235
      t244 = 0.180D3 * t31 * t242
      t254 = t13 * t224
      t258 = log(-0.4D1 * t175 * t15 * t254 * t230)
      t269 = t15 * t228
      t273 = log(-0.4D1 * t197 * t224 * t269 * t229)
      t274 = t273 ** 2
      t278 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t292 = -(-0.90D2 * t4 * t6 * (t234 * t235 - t237) - t244) * t47 * 
     #t166 / 0.720D3 - t169 * t235 * t47 * t171 / 0.8D1 - (-0.90D2 * t4 
     #* t6 * (t258 * t235 - t237) - t244) * t117 * t166 / 0.720D3 - (-0.
     #90D2 * t4 * t6 * (-t274 * t235 / 0.2D1 + t273 * t237 - t278) + 0.1
     #80D3 * t31 * t32 * (t273 * t235 - t237) - t43 * t242) * t166 / 0.7
     #20D3
      t293 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t220, -t222, 0.0D0, t292)
      t295 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t298 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t299 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t310 = t32 * t295
      t311 = t43 * t310
      t315 = rrqg2qgh92J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t326 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t327 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t333 = t327 - t295
      t343 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t358 = -t333
      t359 = t32 * t358
      t409 = (-0.90D2 * t4 * t6 * (-t23 * t295 / 0.2D1 - t298 + t19 * t2
     #99) + 0.180D3 * t31 * t32 * (-t299 + t19 * t295) - t311) * t47 / 0
     #.1440D4 + t4 * t6 * t315 / 0.16D2 - t89 * t6 * t298 / 0.1440D4 - t
     #61 * t6 * t299 / 0.1440D4 + (-0.90D2 * t4 * t6 * (t96 * t295 + t32
     #6 - t299 - t104 * t327) + 0.180D3 * t31 * t32 * t333) * t47 * t117
     # / 0.1440D4 - (-0.90D2 * t4 * t6 * (-t124 * t327 / 0.2D1 - t343 + 
     #t123 * t326 + t298 - t131 * t299 + t133 * t295 / 0.2D1) + 0.180D3 
     #* t31 * t32 * (-t326 + t123 * t327 + t299 - t131 * t295) + t43 * t
     #359) * t117 / 0.1440D4 - t82 * t6 * t295 / 0.1440D4 - (-0.90D2 * t
     #4 * t6 * (t299 - t156 * t295) + 0.180D3 * t31 * t310) * t47 * t166
     # / 0.720D3 + t169 * t358 * t47 * t171 / 0.8D1 - (-0.90D2 * t4 * t6
     # * (t299 - t178 * t295 - t326 + t185 * t327) + 0.180D3 * t31 * t35
     #9) * t117 * t166 / 0.720D3 - (-0.90D2 * t4 * t6 * (t298 - t200 * t
     #299 + t202 * t295 / 0.2D1) + 0.180D3 * t31 * t32 * (t299 - t200 * 
     #t295) + t311) * t166 / 0.720D3
      t410 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t409)
      t412 = t2 * x3
      t413 = -0.1D1 + x3
      t414 = t2 * t413
      t415 = t16 * t413
      t418 = log(-0.4D1 * t10 * t415)
      t419 = t418 ** 2
      t420 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t423 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t424 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t435 = t32 * t420
      t440 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t443 = log(-0.4D1 * t99 * t415)
      t449 = log(0.4D1 * t99 * t16 * t100 * t413)
      t450 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t456 = t420 - t450
      t468 = log(-0.4D1 * t153 * t15 * t50 * t413)
      t485 = (-0.90D2 * t4 * t6 * (t419 * t420 / 0.2D1 + t423 - t418 * t
     #424) + 0.180D3 * t31 * t32 * (t424 - t418 * t420) + t43 * t435) * 
     #t47 / 0.1440D4 + (-0.90D2 * t4 * t6 * (-t440 + t424 - t443 * t420 
     #+ t449 * t450) + 0.180D3 * t31 * t32 * t456) * t47 * t117 / 0.1440
     #D4 - (-0.90D2 * t4 * t6 * (-t424 + t468 * t420) - 0.180D3 * t31 * 
     #t435) * t47 * t166 / 0.720D3 - t169 * t456 * t47 * t171 / 0.8D1
      t486 = FJET(XB1, XB2, s, 0.0D0, t412, 0.0D0, -t414, 0.0D0, t485)
      t488 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t490 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t491 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t503 = t32 * t491
      t508 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t509 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t516 = t491 - t509
      t540 = (-0.90D2 * t4 * t6 * (-t418 * t488 + t490 + t419 * t491 / 0
     #.2D1) + 0.180D3 * t31 * t32 * (t488 - t418 * t491) + t43 * t503) *
     # t47 / 0.1440D4 + (-0.90D2 * t4 * t6 * (-t508 + t449 * t509 + t488
     # - t443 * t491) + 0.180D3 * t31 * t32 * t516) * t47 * t117 / 0.144
     #0D4 - (-0.90D2 * t4 * t6 * (t468 * t491 - t488) - 0.180D3 * t31 * 
     #t503) * t47 * t166 / 0.720D3 - t169 * t516 * t47 * t171 / 0.8D1
      t541 = FJET(XB1, XB2, s, 0.0D0, -t414, 0.0D0, t412, 0.0D0, t540)
      t543 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t544 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t550 = t32 * t544
      t552 = 0.180D3 * t31 * t550
      t572 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t587 = -(-0.90D2 * t4 * t6 * (-t543 + t234 * t544) - t552) * t47 *
     # t166 / 0.720D3 - t169 * t544 * t47 * t171 / 0.8D1 - (-0.90D2 * t4
     # * t6 * (-t543 + t258 * t544) - t552) * t117 * t166 / 0.720D3 - (-
     #0.90D2 * t4 * t6 * (-t274 * t544 / 0.2D1 - t572 + t273 * t543) + 0
     #.180D3 * t31 * t32 * (-t543 + t273 * t544) - t43 * t550) * t166 / 
     #0.720D3
      t588 = FJET(XB1, XB2, s, t220, -t222, 0.0D0, 0.0D0, 0.0D0, t587)
      t590 = x3 * x1
      t591 = t2 * t590
      t593 = t1 * t221
      t594 = x3 * s * t593
      t595 = t413 * s
      t596 = t1 * x1
      t597 = t595 * t596
      t598 = t595 * t593
      t599 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t606 = log(0.4D1 * t153 * t16 * t224 * t228 * t229 * t413)
      t607 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t624 = -(-0.90D2 * t4 * t6 * (t599 - t606 * t607) + 0.180D3 * t31 
     #* t32 * t607) * t47 * t166 / 0.720D3 + t169 * t607 * t47 * t171 / 
     #0.8D1
      t625 = FJET(XB1, XB2, s, t591, -t594, -t597, t598, 0.0D0, t624)
      t627 = x3 * z
      t628 = t590 * z
      t629 = t93 * z
      t630 = t93 * x1
      t631 = t93 * t226
      t632 = cos(t11)
      t637 = Sqrt(-x3 * t100 * t227 * x2 * t413)
      t639 = 0.2D1 * t632 * t637
      t643 = t2 * x1 * (-t627 - t590 + t628 + t629 + t630 - t631 - x2 + 
     #t93 + t639) * t228
      t645 = x2 * x1
      t647 = z + x1 - t226 - x2 * z - t645 + t645 * z - t627 - t590 + t6
     #28 + t629 + t630 - t631 + t93 + t639
      t650 = t2 * x1 * t647 * t228
      t655 = s * t14 * x2 * x1 * t221 * t228
      t656 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t658 = t656 * t47 * t171
      t661 = FJET(XB1, XB2, s, t643, -t594, -t650, t598, t655, -t169 * t
     #658 / 0.8D1)
      t667 = t100 * s * t596
      t669 = t2 * t645 * t228
      t670 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t675 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t681 = log(0.4D1 * t175 * t254 * t269 * t229 * t100)
      t694 = t169 * t670 * t47 * t171 / 0.8D1 - (-0.90D2 * t4 * t6 * (t6
     #75 - t681 * t670) + 0.180D3 * t31 * t32 * t670) * t117 * t166 / 0.
     #720D3
      t695 = FJET(XB1, XB2, s, -t667, -t222, -t669, 0.0D0, t655, t694)
      t697 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t698 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t715 = -(-0.90D2 * t4 * t6 * (t697 - t606 * t698) + 0.180D3 * t31 
     #* t32 * t698) * t47 * t166 / 0.720D3 + t169 * t698 * t47 * t171 / 
     #0.8D1
      t716 = FJET(XB1, XB2, s, -t597, t598, t591, -t594, 0.0D0, t715)
      t718 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t723 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t736 = t169 * t718 * t47 * t171 / 0.8D1 - (-0.90D2 * t4 * t6 * (t7
     #23 - t681 * t718) + 0.180D3 * t31 * t32 * t718) * t117 * t166 / 0.
     #720D3
      t737 = FJET(XB1, XB2, s, -t669, 0.0D0, -t667, -t222, t655, t736)
      t739 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t741 = t739 * t47 * t171
      t744 = FJET(XB1, XB2, s, -t650, t598, t643, -t594, t655, -t169 * t
     #741 / 0.8D1)
      rrqg2qght9s2e0 = t218 * t217 + t293 * t292 + t410 * t409 + t486 * 
     #t485 + t541 * t540 + t588 * t587 + t625 * t624 - t661 * 0.31415926
     #53589793D1 * t32 * t658 / 0.8D1 + t695 * t694 + t716 * t715 + t737
     # * t736 - t744 * 0.3141592653589793D1 * t32 * t741 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * 0.3141592653589793D1)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * t10 * t16)
      t20 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t22 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t27 = 0.3141592653589793D1 * lh
      t28 = t3 * t6
      t31 = 0.180D3 * t27 * t28 * t20
      t33 = 0.1D1 / x3
      t36 = rrqg2qgh91J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t44 = log(0.4D1 * t9 * t13 * t15)
      t45 = t44 * 0.3141592653589793D1
      t48 = (0.180D3 * t27 + 0.90D2 * t45) * t3
      t54 = t44 ** 2
      t57 = lh ** 2
      t59 = 0.3141592653589793D1 ** 2
      t64 = (-0.180D3 * t45 * lh - 0.45D2 * t54 * 0.3141592653589793D1 +
     # 0.3141592653589793D1 * (-0.180D3 * t57 + 0.30D2 * t59)) * t3
      t68 = t4 * t6
      t69 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t70 = t69 - t20
      t72 = 0.1D1 / x2
      t76 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t77 = x2 * t9
      t78 = -0.1D1 + x2
      t82 = log(-0.4D1 * t77 * t16 * t78)
      t86 = log(0.4D1 * t77 * t16)
      t92 = -t70
      t100 = 0.1D1 / x1
      t104 = x1 ** 2
      t105 = t104 * t13
      t109 = log(0.4D1 * t105 * t9 * t15)
      t122 = (-0.90D2 * t4 * t6 * (t19 * t20 - t22) - t31) * t33 / 0.144
     #0D4 + t4 * t6 * t36 / 0.16D2 - t48 * t6 * t22 / 0.1440D4 - t64 * t
     #6 * t20 / 0.1440D4 - t68 * t70 * t33 * t72 / 0.16D2 - (-0.90D2 * t
     #4 * t6 * (-t76 + t82 * t69 + t22 - t86 * t20) + 0.180D3 * t27 * t2
     #8 * t92) * t72 / 0.1440D4 + t68 * t92 * t72 * t100 / 0.8D1 - (-0.9
     #0D2 * t4 * t6 * (t22 - t109 * t20) + t31) * t100 / 0.720D3 + t68 *
     # t20 * t33 * t100 / 0.8D1
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t122)
      t125 = t2 * x1
      t126 = -0.1D1 + x1
      t127 = t2 * t126
      t128 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t137 = 0.1D1 / (-z - x1 + x1 * z)
      t139 = t126 ** 2
      t143 = log(-0.4D1 * t105 / t7 * t15 * t137 * t139)
      t145 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t160 = -t68 * t128 * t72 * t100 / 0.8D1 - (-0.90D2 * t4 * t6 * (t1
     #43 * t128 - t145) - 0.180D3 * t27 * t28 * t128) * t100 / 0.720D3 -
     # t68 * t128 * t33 * t100 / 0.8D1
      t161 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t125, -t127, 0.0D0, t160)
      t163 = rrqg2qgh92J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t167 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t171 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D
     #0, x4)
      t179 = 0.180D3 * t27 * t28 * t167
      t183 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t184 = t183 - t167
      t189 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, 
     #x4)
      t196 = -t184
      t222 = t4 * t6 * t163 / 0.16D2 - t64 * t6 * t167 / 0.1440D4 + (-0.
     #90D2 * t4 * t6 * (-t171 + t19 * t167) - t179) * t33 / 0.1440D4 - t
     #68 * t184 * t33 * t72 / 0.16D2 - (-0.90D2 * t4 * t6 * (-t189 + t82
     # * t183 + t171 - t86 * t167) + 0.180D3 * t27 * t28 * t196) * t72 /
     # 0.1440D4 - t48 * t6 * t171 / 0.1440D4 + t68 * t196 * t72 * t100 /
     # 0.8D1 - (-0.90D2 * t4 * t6 * (t171 - t109 * t167) + t179) * t100 
     #/ 0.720D3 + t68 * t167 * t33 * t100 / 0.8D1
      t223 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t222)
      t225 = t2 * x3
      t226 = -0.1D1 + x3
      t227 = t2 * t226
      t228 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t232 = log(-0.4D1 * t10 * t16 * t226)
      t233 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t245 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t255 = (-0.90D2 * t4 * t6 * (t228 - t232 * t233) + 0.180D3 * t27 *
     # t28 * t233) * t33 / 0.1440D4 - t68 * (t233 - t245) * t33 * t72 / 
     #0.16D2 - t68 * t233 * t33 * t100 / 0.8D1
      t256 = FJET(XB1, XB2, s, 0.0D0, t225, 0.0D0, -t227, 0.0D0, t255)
      t258 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t259 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t271 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t281 = (-0.90D2 * t4 * t6 * (t258 - t232 * t259) + 0.180D3 * t27 *
     # t28 * t259) * t33 / 0.1440D4 - t68 * (t259 - t271) * t33 * t72 / 
     #0.16D2 - t68 * t259 * t33 * t100 / 0.8D1
      t282 = FJET(XB1, XB2, s, 0.0D0, -t227, 0.0D0, t225, 0.0D0, t281)
      t284 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t289 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t305 = -t68 * t284 * t72 * t100 / 0.8D1 - (-0.90D2 * t4 * t6 * (-t
     #289 + t143 * t284) - 0.180D3 * t27 * t28 * t284) * t100 / 0.720D3 
     #- t68 * t284 * t33 * t100 / 0.8D1
      t306 = FJET(XB1, XB2, s, t125, -t127, 0.0D0, 0.0D0, 0.0D0, t305)
      t309 = t2 * x1 * x3
      t311 = t1 * t126
      t312 = x3 * s * t311
      t313 = t226 * s
      t314 = t1 * x1
      t315 = t313 * t314
      t316 = t313 * t311
      t317 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t322 = FJET(XB1, XB2, s, t309, -t312, -t315, t316, 0.0D0, t68 * t3
     #17 * t33 * t100 / 0.8D1)
      t326 = t33 * t100
      t331 = t78 * s * t314
      t334 = t2 * x1 * x2 * t137
      t339 = s * t14 * x2 * x1 * t126 * t137
      t340 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t345 = FJET(XB1, XB2, s, -t331, -t127, -t334, 0.0D0, t339, t68 * t
     #340 * t72 * t100 / 0.8D1)
      t349 = t72 * t100
      t353 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t358 = FJET(XB1, XB2, s, -t315, t316, t309, -t312, 0.0D0, t68 * t3
     #53 * t33 * t100 / 0.8D1)
      t365 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t370 = FJET(XB1, XB2, s, -t334, 0.0D0, -t331, -t127, t339, t68 * t
     #365 * t72 * t100 / 0.8D1)
      rrqg2qght9s2em1 = t123 * t122 + t161 * t160 + t223 * t222 + t256 *
     # t255 + t282 * t281 + t306 * t305 + t322 * 0.3141592653589793D1 * 
     #t3 * t6 * t317 * t326 / 0.8D1 + t345 * 0.3141592653589793D1 * t3 *
     # t6 * t340 * t349 / 0.8D1 + t358 * 0.3141592653589793D1 * t3 * t6 
     #* t353 * t326 / 0.8D1 + t370 * 0.3141592653589793D1 * t3 * t6 * t3
     #65 * t349 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t16 = 0.1D1 / x2
      t20 = 0.1D1 / x1
      t24 = rrqg2qgh91J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t30 = z ** 2
      t34 = Sin(x4 * 0.3141592653589793D1)
      t35 = t34 ** 2
      t37 = t1 ** 2
      t38 = t37 ** 2
      t41 = log(0.4D1 / t30 / z * t35 * t38)
      t45 = (0.180D3 * 0.3141592653589793D1 * lh + 0.90D2 * 0.3141592653
     #589793D1 * t41) * t3
      t48 = t4 * t8 * t9 / 0.16D2 + t4 * t6 * (t7 - t13) * t16 / 0.16D2 
     #+ t4 * t8 * t20 / 0.8D1 + t4 * t6 * t24 / 0.16D2 - t45 * t8 / 0.14
     #40D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t51 = t2 * x1
      t53 = t2 * (-0.1D1 + x1)
      t54 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, x
     #4)
      t56 = t6 * t54 * t20
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t51, -t53, 0.0D0, -t4 * t56 
     #/ 0.8D1)
      t64 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t65 = t6 * t64
      t69 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t78 = rrqg2qgh92J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t84 = t4 * t65 * t9 / 0.16D2 + t4 * t6 * (-t69 + t64) * t16 / 0.16
     #D2 + t4 * t65 * t20 / 0.8D1 + t4 * t6 * t78 / 0.16D2 - t45 * t65 /
     # 0.1440D4
      t85 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t84)
      t87 = t2 * x3
      t89 = t2 * (-0.1D1 + x3)
      t90 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t92 = t6 * t90 * t9
      t95 = FJET(XB1, XB2, s, 0.0D0, t87, 0.0D0, -t89, 0.0D0, -t4 * t92 
     #/ 0.16D2)
      t100 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, 
     #x4)
      t102 = t6 * t100 * t9
      t105 = FJET(XB1, XB2, s, 0.0D0, -t89, 0.0D0, t87, 0.0D0, -t4 * t10
     #2 / 0.16D2)
      t110 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.0D0, 
     #x4)
      t112 = t6 * t110 * t20
      t115 = FJET(XB1, XB2, s, t51, -t53, 0.0D0, 0.0D0, 0.0D0, -t4 * t11
     #2 / 0.8D1)
      rrqg2qght9s2em2 = t49 * t48 - t59 * 0.3141592653589793D1 * t3 * t5
     #6 / 0.8D1 + t85 * t84 - t95 * 0.3141592653589793D1 * t3 * t92 / 0.
     #16D2 - t105 * 0.3141592653589793D1 * t3 * t102 / 0.16D2 - t115 * 0
     #.3141592653589793D1 * t3 * t112 / 0.8D1

      end function



      doubleprecision function rrqg2qght9s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = 0.3141592653589793D1 * t3
      t5 = s ** 2
      t6 = 0.1D1 / t5
      t7 = rrqg2qgh91J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0,
     # x4)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t6 * 
     #t7 / 0.16D2)
      t13 = t3 * t6
      t16 = rrqg2qgh92J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 0.0D0
     #, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t6 * 
     #t16 / 0.16D2)
      rrqg2qght9s2em3 = t11 * 0.3141592653589793D1 * t13 * t7 / 0.16D2 +
     # t20 * 0.3141592653589793D1 * t13 * t16 / 0.16D2

      end function



      doubleprecision function rrqg2qght9s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrqg2qght9s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh91J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x3
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t17 = x3 * (0.1D1 - x2)
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t4)
      t28 = t17 * t14 + x2 * t4 - 0.2D1 * t21 * t25
      t30 = t2 ** 2
      t31 = t4 ** 2
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t46 = s * t1
      rrqg2qgh91J1 = 0.4D1 / 0.9D1 * t1 * t2 * t4 * wd * (-t11 * x1 * t1
     #5 * t28 * t30 * t31 - t11 * t35 * x1 / t37 / t14 * t41 * t28) / (s
     # - t46 * x1 * t15 * t28 - t46 * t2 * x3) / z / 0.3141592653589793D
     #1

      end function
  
   
 

      doubleprecision function rrqg2qgh91J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t17 = x3 * (0.1D1 - x2)
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t28 = t17 * t14 + x2 * t5 - 0.2D1 * t21 * t25
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      rrqg2qgh91J2 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - 0.2D1 * t11 * t35 / t37 * t41 * t3 * t5 + t34)) / (s - t
     #2 * x1 * t15 * t28 - t2 * t3 * x3) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh91J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t27 = 0.2D1 * t21 * t25
      t28 = t17 * t14 + x2 * t5 - t27
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      t49 = 0.1D1 / t37
      t54 = 0.2D1 * t11 * t35 * t49 * t41 * t3 * t5
      t63 = s - t2 * x1 * t15 * t28 - t2 * t3 * x3
      t64 = t63 * t7
      t71 = t5 * t16 * t14 + x2 * x3 + t27
      t77 = t71 ** 2
      rrqg2qgh91J3 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - t54 + t34) - t4 * t6 * (-t54 + t44 + t34 + t64 * t9 * t3
     # * t5 * x1 * t15 * t71 + t64 * t9 * t35 * t49 * t77)) / t63 / s / 
     #z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh91J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t27 = 0.2D1 * t21 * t25
      t28 = t17 * t14 + x2 * t5 - t27
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      t49 = 0.1D1 / t37
      t54 = 0.2D1 * t11 * t35 * t49 * t41 * t3 * t5
      t63 = s - t2 * x1 * t15 * t28 - t2 * t3 * x3
      t64 = t63 * t7
      t71 = t5 * t16 * t14 + x2 * x3 + t27
      t77 = t71 ** 2
      rrqg2qgh91J4 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - t54 + t34) - t4 * t6 * (-t54 + t44 + t34 + t64 * t9 * t3
     # * t5 * x1 * t15 * t71 + t64 * t9 * t35 * t49 * t77)) / t63 / s / 
     #z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh91J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t14 = z + t1 * x1
      t15 = 0.1D1 / t14
      t16 = 0.1D1 - x2
      t17 = x3 * t16
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t17 * t14 * x2 * t5)
      t27 = 0.2D1 * t21 * t25
      t28 = t17 * t14 + x2 * t5 - t27
      t30 = t3 ** 2
      t31 = t5 ** 2
      t34 = t11 * x1 * t15 * t28 * t30 * t31
      t35 = x1 ** 2
      t37 = t14 ** 2
      t41 = t28 ** 2
      t44 = t11 * t35 * x1 / t37 / t14 * t41 * t28
      t49 = 0.1D1 / t37
      t54 = 0.2D1 * t11 * t35 * t49 * t41 * t3 * t5
      t63 = s - t2 * x1 * t15 * t28 - t2 * t3 * x3
      t64 = t63 * t7
      t71 = t5 * t16 * t14 + x2 * x3 + t27
      t77 = t71 ** 2
      rrqg2qgh91J5 = -0.4D1 / 0.9D1 * (-t4 * t6 * (-t34 - t44) - t4 * t6
     # * (t44 - t54 + t34) - t4 * t6 * (-t54 + t44 + t34 + t64 * t9 * t3
     # * t5 * x1 * t15 * t71 + t64 * t9 * t35 * t49 * t77)) / t63 / s / 
     #z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh91J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t12 = x1 ** 2
      t15 = z + t1 * x1
      t16 = t15 ** 2
      t20 = 0.1D1 - x2
      t21 = x3 * t20
      t25 = cos(x4 * 0.3141592653589793D1)
      t29 = Sqrt(t21 * t15 * x2 * t5)
      t31 = 0.2D1 * t25 * t29
      t32 = t21 * t15 + x2 * t5 - t31
      t33 = t32 ** 2
      t36 = t11 * t12 * x1 / t16 / t15 * t33 * t32
      t38 = 0.1D1 / t16
      t43 = 0.2D1 * t11 * t12 * t38 * t33 * t3 * t5
      t45 = 0.1D1 / t15
      t47 = t3 ** 2
      t48 = t5 ** 2
      t51 = t11 * x1 * t45 * t32 * t47 * t48
      t60 = s - t2 * x1 * t45 * t32 - t2 * t3 * x3
      t61 = t60 * t7
      t68 = t5 * t20 * t15 + x2 * x3 + t31
      t74 = t68 ** 2
      rrqg2qgh91J6 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t36 - t43 + t51) - t4
     # * t6 * (-t43 + t36 + t51 + t61 * t9 * t3 * t5 * x1 * t45 * t68 + 
     #t61 * t9 * t12 * t38 * t74)) / t60 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh91J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x3
      t7 = s ** 2
      t9 = t1 ** 2
      t11 = t7 * s * t9 * t1
      t12 = x1 ** 2
      t15 = z + t1 * x1
      t16 = t15 ** 2
      t17 = 0.1D1 / t16
      t18 = 0.1D1 - x2
      t19 = x3 * t18
      t23 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(t19 * t15 * x2 * t4)
      t29 = 0.2D1 * t23 * t27
      t30 = t19 * t15 + x2 * t4 - t29
      t31 = t30 ** 2
      t45 = 0.1D1 / t15
      t47 = t2 ** 2
      t48 = t4 ** 2
      t52 = s * t1
      t58 = s - t52 * x1 * t45 * t30 - t52 * t2 * x3
      t59 = t58 * t7
      t66 = t4 * t18 * t15 + x2 * x3 + t29
      t72 = t66 ** 2
      rrqg2qgh91J7 = 0.4D1 / 0.9D1 * t1 * t2 * t4 * wd * (-0.2D1 * t11 *
     # t12 * t17 * t31 * t2 * t4 + t11 * t12 * x1 / t16 / t15 * t31 * t3
     #0 + t11 * x1 * t45 * t30 * t47 * t48 + t59 * t9 * t2 * t4 * x1 * t
     #45 * t66 + t59 * t9 * t12 * t17 * t72) / t58 / z / 0.3141592653589
     #793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh92J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t4 = 0.1D1 - x3
      t7 = s * t1
      t9 = z + t1 * x1
      t13 = x3 * (0.1D1 - x2)
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t13 * t9 * x2 * t4)
      t24 = t13 * t9 + x2 * t4 - 0.2D1 * t17 * t21
      t25 = x1 / t9 * t24
      t29 = s - t7 * t25 - t7 * t2 * x3
      t30 = s ** 2
      t31 = t29 * t30
      t32 = t1 ** 2
      t34 = x1 ** 2
      t35 = t9 ** 2
      t38 = t24 ** 2
      t42 = t2 ** 2
      t44 = t4 ** 2
      rrqg2qgh92J1 = 0.4D1 / 0.9D1 * t1 * t2 * t4 * wd * (0.9D1 * t31 * 
     #t32 * t34 / t35 * t38 + 0.9D1 * t31 * t32 * t42 * t44 + 0.9D1 * t3
     #1 - 0.18D2 * t31 * t1 * t25) / t29 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh92J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t33 = x1 ** 2
      t34 = t8 ** 2
      t37 = t23 ** 2
      t40 = 0.9D1 * t30 * t31 * t33 / t34 * t37
      t41 = t3 ** 2
      t43 = t5 ** 2
      t46 = 0.9D1 * t30 * t31 * t41 * t43
      t47 = 0.9D1 * t30
      t49 = t30 * t1 * t24
      rrqg2qgh92J2 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t40 + t46 + t47 - 0.1
     #8D2 * t49) - t4 * t6 * (0.18D2 * t30 * t1 * t3 * t5 - t47 - 0.27D2
     # * t30 * t31 * t3 * t5 * x1 * t9 * t23 - t46 - t40 + 0.27D2 * t49)
     #) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh92J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t33 = x1 ** 2
      t34 = t8 ** 2
      t37 = t23 ** 2
      t40 = 0.9D1 * t30 * t31 * t33 / t34 * t37
      t41 = t3 ** 2
      t43 = t5 ** 2
      t46 = 0.9D1 * t30 * t31 * t41 * t43
      t47 = 0.9D1 * t30
      t49 = t30 * t1 * t24
      rrqg2qgh92J3 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t40 + t46 + t47 - 0.1
     #8D2 * t49) - t4 * t6 * (0.18D2 * t30 * t1 * t3 * t5 - t47 - 0.27D2
     # * t30 * t31 * t3 * t5 * x1 * t9 * t23 - t46 - t40 + 0.27D2 * t49)
     #) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh92J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t33 = x1 ** 2
      t34 = t8 ** 2
      t37 = t23 ** 2
      t40 = 0.9D1 * t30 * t31 * t33 / t34 * t37
      t41 = t3 ** 2
      t43 = t5 ** 2
      t46 = 0.9D1 * t30 * t31 * t41 * t43
      t47 = 0.9D1 * t30
      t49 = t30 * t1 * t24
      rrqg2qgh92J4 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t40 + t46 + t47 - 0.1
     #8D2 * t49) - t4 * t6 * (0.18D2 * t30 * t1 * t3 * t5 - t47 - 0.27D2
     # * t30 * t31 * t3 * t5 * x1 * t9 * t23 - t46 - t40 + 0.27D2 * t49)
     #) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh92J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = 0.1D1 - x1
      t4 = t2 * t3
      t5 = 0.1D1 - x3
      t6 = t5 * wd
      t8 = z + t1 * x1
      t9 = 0.1D1 / t8
      t12 = x3 * (0.1D1 - x2)
      t16 = cos(x4 * 0.3141592653589793D1)
      t20 = Sqrt(t12 * t8 * x2 * t5)
      t23 = t12 * t8 + x2 * t5 - 0.2D1 * t16 * t20
      t24 = x1 * t9 * t23
      t28 = s - t2 * t24 - t2 * t3 * x3
      t29 = s ** 2
      t30 = t28 * t29
      t31 = t1 ** 2
      t33 = x1 ** 2
      t34 = t8 ** 2
      t37 = t23 ** 2
      t40 = 0.9D1 * t30 * t31 * t33 / t34 * t37
      t41 = t3 ** 2
      t43 = t5 ** 2
      t46 = 0.9D1 * t30 * t31 * t41 * t43
      t47 = 0.9D1 * t30
      t49 = t30 * t1 * t24
      rrqg2qgh92J5 = -0.4D1 / 0.9D1 * (-t4 * t6 * (t40 + t46 + t47 - 0.1
     #8D2 * t49) - t4 * t6 * (0.18D2 * t30 * t1 * t3 * t5 - t47 - 0.27D2
     # * t30 * t31 * t3 * t5 * x1 * t9 * t23 - t46 - t40 + 0.27D2 * t49)
     #) / t28 / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqg2qgh92J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = 0.1D1 - x1
      t3 = t1 * t2
      t4 = 0.1D1 - x3
      t7 = s * t1
      t9 = z + t1 * x1
      t10 = 0.1D1 / t9
      t13 = x3 * (0.1D1 - x2)
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t13 * t9 * x2 * t4)
      t24 = t13 * t9 + x2 * t4 - 0.2D1 * t17 * t21
      t25 = x1 * t10 * t24
      t29 = s - t7 * t25 - t7 * t2 * x3
      t30 = s ** 2
      t31 = t29 * t30
      t36 = t1 ** 2
      t44 = t2 ** 2
      t46 = t4 ** 2
      t51 = x1 ** 2
      t52 = t9 ** 2
      t55 = t24 ** 2
      rrqg2qgh92J6 = 0.4D1 / 0.9D1 * t3 * t4 * wd * (0.18D2 * t31 * t3 *
     # t4 - 0.9D1 * t31 - 0.27D2 * t31 * t36 * t2 * t4 * x1 * t10 * t24 
     #- 0.9D1 * t31 * t36 * t44 * t46 - 0.9D1 * t31 * t36 * t51 / t52 * 
     #t55 + 0.27D2 * t31 * t1 * t25) / t29 / z / 0.3141592653589793D1

      end function
  
 