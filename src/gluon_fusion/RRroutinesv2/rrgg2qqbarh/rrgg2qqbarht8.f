  
      subroutine rrgg2qqbarht8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh81J1  
      doubleprecision rrgg2qqbarh81J2  
      doubleprecision rrgg2qqbarh81J3  
      doubleprecision rrgg2qqbarh81J4  
      doubleprecision rrgg2qqbarh81J5  
      doubleprecision rrgg2qqbarh81J6  
      doubleprecision rrgg2qqbarht8s1e1  
      doubleprecision rrgg2qqbarht8s1e0  
      doubleprecision rrgg2qqbarht8s1em1  
      doubleprecision rrgg2qqbarht8s1em2  
      doubleprecision rrgg2qqbarht8s1em3  
      doubleprecision rrgg2qqbarht8s1em4  
      doubleprecision rrgg2qqbarht8s2e1  
      doubleprecision rrgg2qqbarht8s2e0  
      doubleprecision rrgg2qqbarht8s2em1  
      doubleprecision rrgg2qqbarht8s2em2  
      doubleprecision rrgg2qqbarht8s2em3  
      doubleprecision rrgg2qqbarht8s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht8s1e1
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

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
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = s ** 2
      t20 = 0.1D1 / t19
      t21 = pi * t20
      t22 = Sin(t5)
      t23 = t22 ** 2
      t24 = x2 * t23
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t31 = log(0.4D1 * t27 * t10 * t7)
      t32 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t34 = t31 ** 2
      t35 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t38 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t42 = pi * lh
      t48 = lh ** 2
      t50 = pi ** 2
      t52 = 0.180D3 * t48 - 0.30D2 * t50
      t53 = pi * t52
      t54 = t20 * t35
      t57 = 0.1D1 / x3
      t59 = 0.1D1 / x2
      t62 = x1 ** 2
      t63 = t3 * t62
      t64 = t26 * t23
      t69 = log(0.4D1 * t63 * t64 * t7 * t9)
      t78 = 0.1D1 / x1
      t79 = t59 * t78
      t82 = -(0.90D2 * t21 * (-t31 * t32 + t34 * t35 / 0.2D1 + t38) - 0.
     #180D3 * t42 * t20 * (t32 - t31 * t35) + t53 * t54) * t57 * t59 / 0
     #.1440D4 - (0.90D2 * t21 * (t32 - t69 * t35) - 0.180D3 * t42 * t54)
     # * t57 * t79 / 0.720D3
      t83 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t82)
      t85 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t86 = x3 * t26
      t89 = log(0.4D1 * t86 * t23)
      t90 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t95 = t89 ** 2
      t98 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
      t102 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t112 = 0.60D2 * lh * t50 - 0.240D3 * zeta3 - 0.120D3 * t48 * lh
      t113 = pi * t112
      t114 = t20 * t90
      t115 = t113 * t114
      t127 = log(0.4D1 * t64)
      t128 = t127 ** 2
      t129 = t128 * pi
      t133 = t128 * t127 * pi
      t135 = t127 * pi
      t154 = rrgg2qqbarh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t164 = t50 ** 2
      t165 = t48 ** 2
      t171 = t128 ** 2
      t178 = x3 * t62
      t181 = log(0.4D1 * t178 * t64)
      t183 = t181 ** 2
      t194 = t53 * t114
      t199 = t62 * t26
      t200 = t199 * t23
      t202 = log(0.4D1 * t200)
      t207 = t202 ** 2
      t229 = log(0.4D1 * t3 * t200)
      t240 = x2 * t62
      t241 = t240 * t64
      t243 = log(0.4D1 * t241)
      t245 = t243 ** 2
      t262 = log(0.4D1 * t3 * t64)
      t264 = t262 ** 2
      t280 = log(0.4D1 * t27)
      t285 = t280 ** 2
      t305 = -(t53 * t20 * (t85 - t89 * t90) + 0.90D2 * t21 * (t95 * t85
     # / 0.2D1 + t98 - t95 * t89 * t90 / 0.6D1 - t89 * t102) + t115 - 0.
     #180D3 * t42 * t20 * (-t89 * t85 + t95 * t90 / 0.2D1 + t102)) * t57
     # / 0.1440D4 - (-0.90D2 * t129 * lh + t113 - 0.15D2 * t133 - t135 *
     # t52) * t20 * t85 / 0.1440D4 - (0.180D3 * t135 * lh + 0.45D2 * t12
     #9 + t53) * t20 * t102 / 0.1440D4 - (-0.180D3 * t42 - 0.90D2 * t135
     #) * t20 * t98 / 0.1440D4 - t21 * t154 / 0.16D2 - (0.30D2 * t133 * 
     #lh + t129 * t52 / 0.2D1 - t135 * t112 + pi * (0.480D3 * lh * zeta3
     # + t164 + 0.60D2 * t165 - 0.60D2 * t48 * t50) + 0.15D2 / 0.4D1 * t
     #171 * pi) * t20 * t90 / 0.1440D4 - (0.90D2 * t21 * (-t181 * t85 + 
     #t183 * t90 / 0.2D1 + t102) - 0.180D3 * t42 * t20 * (t85 - t181 * t
     #90) + t194) * t57 * t78 / 0.720D3 - (t53 * t20 * (t85 - t202 * t90
     #) + 0.90D2 * t21 * (t207 * t85 / 0.2D1 + t98 - t207 * t202 * t90 /
     # 0.6D1 - t202 * t102) + t115 - 0.180D3 * t42 * t20 * (-t202 * t85 
     #+ t207 * t90 / 0.2D1 + t102)) * t78 / 0.720D3 - (0.90D2 * t21 * (t
     #85 - t229 * t90) - 0.180D3 * t42 * t114) * t57 * t79 / 0.720D3 - (
     #0.90D2 * t21 * (-t243 * t85 + t245 * t90 / 0.2D1 + t102) - 0.180D3
     # * t42 * t20 * (t85 - t243 * t90) + t194) * t59 * t78 / 0.720D3 - 
     #(0.90D2 * t21 * (-t262 * t85 + t264 * t90 / 0.2D1 + t102) - 0.180D
     #3 * t42 * t20 * (t85 - t262 * t90) + t194) * t57 * t59 / 0.1440D4 
     #- (t53 * t20 * (t85 - t280 * t90) + 0.90D2 * t21 * (t285 * t85 / 0
     #.2D1 + t98 - t285 * t280 * t90 / 0.6D1 - t280 * t102) + t115 - 0.1
     #80D3 * t42 * t20 * (-t280 * t85 + t285 * t90 / 0.2D1 + t102)) * t5
     #9 / 0.1440D4
      t306 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t305)
      t309 = -0.1D1 + x1
      t310 = t1 * t309
      t311 = t7 * s * t310
      t312 = t2 * x1
      t314 = x1 * z
      t315 = 0.1D1 - x1 + t314
      t316 = 0.1D1 / t315
      t318 = t2 * t309 * x2 * t316
      t319 = t1 ** 2
      t324 = s * t319 * x2 * x1 * t309 * t316
      t325 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t318, t311,
     # 0.0D0, t312, -t324)
      t327 = t3 * t62 * t23
      t328 = t26 * t316
      t329 = t309 ** 2
      t330 = t329 * t7
      t331 = t328 * t330
      t334 = log(-0.4D1 * t327 * t331)
      t335 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t318, t311,
     # 0.0D0, t312, -t324)
      t340 = t20 * t335
      t346 = t240 * t23
      t349 = log(-0.4D1 * t346 * t331)
      t351 = t349 ** 2
      t354 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t318, t311,
     # 0.0D0, t312, -t324)
      t368 = -(0.90D2 * t21 * (t325 - t334 * t335) - 0.180D3 * t42 * t34
     #0) * t57 * t79 / 0.720D3 - (0.90D2 * t21 * (-t349 * t325 + t351 * 
     #t335 / 0.2D1 + t354) - 0.180D3 * t42 * t20 * (t325 - t349 * t335) 
     #+ t53 * t340) * t59 * t78 / 0.720D3
      t369 = FJET(XB1, XB2, s, t311, t312, -t318, 0.0D0, -t324, t368)
      t371 = t9 * s
      t372 = t371 * t310
      t374 = t371 * t1 * x1
      t376 = x3 * s * t310
      t377 = x3 * x1
      t378 = t2 * t377
      t379 = t316 * t329
      t380 = t379 * t10
      t383 = log(-0.4D1 * t200 * t380)
      t384 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t376, t372,
     # t378, -t374, 0.0D0)
      t386 = t383 ** 2
      t387 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t376, t372,
     # t378, -t374, 0.0D0)
      t390 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t376, t372,
     # t378, -t374, 0.0D0)
      t399 = t20 * t387
      t406 = log(-0.4D1 * t241 * t380)
      t417 = -(0.90D2 * t21 * (-t383 * t384 + t386 * t387 / 0.2D1 + t390
     #) - 0.180D3 * t42 * t20 * (t384 - t383 * t387) + t53 * t399) * t57
     # * t78 / 0.720D3 - (0.90D2 * t21 * (t384 - t406 * t387) - 0.180D3 
     #* t42 * t399) * t57 * t79 / 0.720D3
      t418 = FJET(XB1, XB2, s, t372, -t374, -t376, t378, 0.0D0, t417)
      t421 = t7 * t1 * s
      t423 = x2 * t1 * s
      t424 = t64 * t7
      t427 = log(-0.4D1 * t3 * t424)
      t428 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t423, -t421,
     # 0.0D0, 0.0D0, 0.0D0)
      t430 = t427 ** 2
      t431 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t423, -t421,
     # 0.0D0, 0.0D0, 0.0D0)
      t434 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, t423, -t421,
     # 0.0D0, 0.0D0, 0.0D0)
      t443 = t20 * t431
      t444 = t53 * t443
      t448 = (0.90D2 * t21 * (t427 * t428 - t430 * t431 / 0.2D1 - t434) 
     #- 0.180D3 * t42 * t20 * (-t428 + t427 * t431) - t444) * t57 * t59 
     #/ 0.1440D4
      t452 = log(-0.4D1 * t24 * t26 * t7)
      t454 = -t428 + t452 * t431
      t457 = t452 ** 2
      t460 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, t423, -t421,
     # 0.0D0, 0.0D0, 0.0D0)
      t465 = -t457 * t428 / 0.2D1 - t460 + t457 * t452 * t431 / 0.6D1 + 
     #t452 * t434
      t468 = t113 * t443
      t472 = t452 * t428 - t457 * t431 / 0.2D1 - t434
      t481 = log(-0.4D1 * t63 * t424)
      t491 = (0.90D2 * t21 * (-t428 + t481 * t431) + 0.180D3 * t42 * t44
     #3) * t57 * t79 / 0.720D3
      t494 = log(-0.4D1 * t240 * t424)
      t496 = t494 ** 2
      t510 = (0.90D2 * t21 * (t494 * t428 - t496 * t431 / 0.2D1 - t434) 
     #- 0.180D3 * t42 * t20 * (-t428 + t494 * t431) - t444) * t59 * t78 
     #/ 0.720D3
      t511 = -t448 - (t53 * t20 * t454 + 0.90D2 * t21 * t465 - t468 - 0.
     #180D3 * t42 * t20 * t472) * t59 / 0.1440D4 - t491 - t510
      t512 = FJET(XB1, XB2, s, 0.0D0, -t421, 0.0D0, t423, 0.0D0, t511)
      t514 = t2 * t309
      t517 = t23 * t329 * t316
      t520 = log(0.4D1 * t178 * t26 * t517)
      t521 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t514
     #, 0.0D0, t312, 0.0D0)
      t523 = t520 ** 2
      t524 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t514
     #, 0.0D0, t312, 0.0D0)
      t527 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t514
     #, 0.0D0, t312, 0.0D0)
      t536 = t20 * t524
      t537 = t53 * t536
      t540 = (0.90D2 * t21 * (t520 * t521 - t523 * t524 / 0.2D1 - t527) 
     #- 0.180D3 * t42 * t20 * (-t521 + t520 * t524) - t537) * t57 * t78
      t543 = log(0.4D1 * t199 * t517)
      t545 = t521 - t543 * t524
      t548 = t543 ** 2
      t551 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t514
     #, 0.0D0, t312, 0.0D0)
      t556 = t548 * t521 / 0.2D1 + t551 - t548 * t543 * t524 / 0.6D1 - t
     #543 * t527
      t559 = t113 * t536
      t563 = -t543 * t521 + t548 * t524 / 0.2D1 + t527
      t572 = log(0.4D1 * t63 * t64 * t379)
      t581 = (0.90D2 * t21 * (-t521 + t572 * t524) + 0.180D3 * t42 * t53
     #6) * t57 * t79
      t585 = log(0.4D1 * t346 * t328 * t329)
      t587 = t585 ** 2
      t600 = (0.90D2 * t21 * (t585 * t521 - t587 * t524 / 0.2D1 - t527) 
     #- 0.180D3 * t42 * t20 * (-t521 + t585 * t524) - t537) * t59 * t78
      t602 = -t540 / 0.720D3 - (-t53 * t20 * t545 - 0.90D2 * t21 * t556 
     #- t559 + 0.180D3 * t42 * t20 * t563) * t78 / 0.720D3 - t581 / 0.72
     #0D3 - t600 / 0.720D3
      t603 = FJET(XB1, XB2, s, t312, -t514, 0.0D0, 0.0D0, 0.0D0, t602)
      t605 = FJET(XB1, XB2, s, -t318, 0.0D0, t311, t312, -t324, t368)
      t607 = t2 * x3
      t608 = t2 * t9
      t612 = log(-0.4D1 * t178 * t64 * t9)
      t613 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t607, -t608,
     # 0.0D0, 0.0D0, 0.0D0)
      t615 = t612 ** 2
      t616 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t607, -t608,
     # 0.0D0, 0.0D0, 0.0D0)
      t619 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, t607, -t608,
     # 0.0D0, 0.0D0, 0.0D0)
      t628 = t20 * t616
      t629 = t53 * t628
      t633 = (0.90D2 * t21 * (t612 * t613 - t615 * t616 / 0.2D1 - t619) 
     #- 0.180D3 * t42 * t20 * (-t613 + t612 * t616) - t629) * t57 * t78 
     #/ 0.720D3
      t634 = t86 * t9
      t637 = log(-0.4D1 * t346 * t634)
      t647 = (0.90D2 * t21 * (-t613 + t637 * t616) + 0.180D3 * t42 * t62
     #8) * t57 * t79 / 0.720D3
      t650 = log(-0.4D1 * t64 * t10)
      t652 = -t613 + t650 * t616
      t655 = t650 ** 2
      t658 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, t607, -t608,
     # 0.0D0, 0.0D0, 0.0D0)
      t663 = -t655 * t613 / 0.2D1 - t658 + t655 * t650 * t616 / 0.6D1 + 
     #t650 * t619
      t666 = t113 * t628
      t670 = t650 * t613 - t655 * t616 / 0.2D1 - t619
      t679 = log(-0.4D1 * t24 * t634)
      t681 = t679 ** 2
      t684 = t679 * t613 - t681 * t616 / 0.2D1 - t619
      t688 = -t613 + t679 * t616
      t695 = (0.90D2 * t21 * t684 - 0.180D3 * t42 * t20 * t688 - t629) *
     # t57 * t59 / 0.1440D4
      t696 = -t633 - t647 - (t53 * t20 * t652 + 0.90D2 * t21 * t663 - t6
     #66 - 0.180D3 * t42 * t20 * t670) * t57 / 0.1440D4 - t695
      t697 = FJET(XB1, XB2, s, 0.0D0, t607, 0.0D0, -t608, 0.0D0, t696)
      t699 = FJET(XB1, XB2, s, 0.0D0, t18, 0.0D0, -t16, 0.0D0, t82)
      t701 = FJET(XB1, XB2, s, t18, 0.0D0, -t16, 0.0D0, 0.0D0, t82)
      t703 = FJET(XB1, XB2, s, -t376, t378, t372, -t374, 0.0D0, t417)
      t717 = (t53 * t20 * t652 + 0.90D2 * t21 * t663 - t666 - 0.180D3 * 
     #t42 * t20 * t670) * t57 / 0.1440D4
      t718 = -t717 - t633 - t647 - t695
      t719 = FJET(XB1, XB2, s, t607, 0.0D0, -t608, 0.0D0, 0.0D0, t718)
      t721 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t82)
      t736 = -t540 / 0.720D3 - (-t53 * t20 * t545 - 0.90D2 * t21 * t556 
     #- t559 + 0.180D3 * t42 * t20 * t563) * t78 / 0.720D3 - t581 / 0.72
     #0D3 - t600 / 0.720D3
      t737 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t312, -t514, 0.0D0, t736)
      t752 = -t448 - (t53 * t20 * t454 + 0.90D2 * t21 * t465 - t468 - 0.
     #180D3 * t42 * t20 * t472) * t59 / 0.1440D4 - t491 - t510
      t753 = FJET(XB1, XB2, s, t423, 0.0D0, -t421, 0.0D0, 0.0D0, t752)
      t755 = FJET(XB1, XB2, s, -t421, 0.0D0, t423, 0.0D0, 0.0D0, t752)
      t757 = t83 * t82 + t306 * t305 + t369 * t368 + t418 * t417 + t512 
     #* t511 + t603 * t602 + t605 * t368 + t697 * t696 + t699 * t82 + t7
     #01 * t82 + t703 * t417 + t719 * t718 + t721 * t82 + t737 * t736 + 
     #t753 * t752 + t755 * t752
      t758 = FJET(XB1, XB2, s, 0.0D0, t423, 0.0D0, -t421, 0.0D0, t511)
      t760 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t305)
      t762 = FJET(XB1, XB2, s, t312, t311, 0.0D0, -t318, -t324, t368)
      t764 = FJET(XB1, XB2, s, -t374, t372, t378, -t376, 0.0D0, t417)
      t766 = FJET(XB1, XB2, s, 0.0D0, -t318, t312, t311, -t324, t368)
      t768 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t514, t312, 0.0D0, t736)
      t781 = -t717 - t633 - t647 - (0.90D2 * t21 * t684 - 0.180D3 * t42 
     #* t20 * t688 - t629) * t57 * t59 / 0.1440D4
      t782 = FJET(XB1, XB2, s, -t608, 0.0D0, t607, 0.0D0, 0.0D0, t781)
      t784 = FJET(XB1, XB2, s, t378, -t376, -t374, t372, 0.0D0, t417)
      t786 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t305)
      t788 = FJET(XB1, XB2, s, 0.0D0, -t608, 0.0D0, t607, 0.0D0, t696)
      t790 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t305)
      t792 = FJET(XB1, XB2, s, -t514, t312, 0.0D0, 0.0D0, 0.0D0, t602)
      t794 = t377 * z
      t795 = t3 * x1
      t796 = t3 * t314
      t801 = Sqrt(x3 * t7 * t315 * x2 * t9)
      t803 = 0.2D1 * t6 * t801
      t807 = t2 * t309 * (-x3 + t377 - t794 + t4 - t795 + t796 - x2 + t8
     #03) * t316
      t808 = x2 * x1
      t810 = 0.1D1 - x1 + t314 - x2 + t808 - t808 * z - x3 + t377 - t794
     # + t4 - t795 + t796 + t803
      t813 = t2 * t309 * t810 * t316
      t814 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t807, -t813,
     # t378, -t374, -t324)
      t819 = log(0.4D1 * t327 * t328 * t330 * t9)
      t820 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t807, -t813,
     # t378, -t374, -t324)
      t822 = -t814 + t819 * t820
      t827 = 0.180D3 * t42 * t20 * t820
      t828 = 0.90D2 * t21 * t822 + t827
      t831 = t828 * t57 * t79 / 0.720D3
      t832 = FJET(XB1, XB2, s, t378, t807, -t374, -t813, -t324, -t831)
      t835 = t57 * t59 * t78
      t838 = FJET(XB1, XB2, s, -t374, -t813, t378, t807, -t324, -t831)
      t845 = 0.90D2 * t21 * t822 + t827
      t849 = FJET(XB1, XB2, s, -t813, -t374, t807, t378, -t324, -t845 * 
     #t57 * t79 / 0.720D3)
      t853 = FJET(XB1, XB2, s, t807, t378, -t813, -t374, -t324, -t831)
      t857 = t758 * t511 + t760 * t305 + t762 * t368 + t764 * t417 + t76
     #6 * t368 + t768 * t736 + t782 * t781 + t784 * t417 + t786 * t305 +
     # t788 * t696 + t790 * t305 + t792 * t602 - t832 * t828 * t835 / 0.
     #720D3 - t838 * t828 * t835 / 0.720D3 - t849 * t845 * t835 / 0.720D
     #3 - t853 * t828 * t835 / 0.720D3
      rrgg2qqbarht8s1e1 = t757 + t857

      end function



      doubleprecision function rrgg2qqbarht8s1e0
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

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
      t3 = -0.1D1 + x1
      t4 = x1 * z
      t5 = x2 * x1
      t7 = x3 * x1
      t8 = t7 * z
      t9 = x2 * x3
      t10 = 0.2D1 * t9
      t11 = t9 * x1
      t12 = t9 * t4
      t13 = x4 * pi
      t14 = cos(t13)
      t15 = -0.1D1 + x2
      t17 = 0.1D1 - x1 + t4
      t19 = -0.1D1 + x3
      t22 = Sqrt(x3 * t15 * t17 * x2 * t19)
      t24 = 0.2D1 * t14 * t22
      t25 = 0.1D1 - x1 + t4 - x2 + t5 - t5 * z - x3 + t7 - t8 + t10 - t1
     #1 + t12 + t24
      t27 = 0.1D1 / t17
      t29 = t2 * t3 * t25 * t27
      t30 = t19 * s
      t32 = t30 * t1 * x1
      t36 = t2 * t3 * (-x3 + t7 - t8 + t10 - t11 + t12 - x2 + t24) * t27
      t37 = t2 * t7
      t38 = t1 ** 2
      t43 = s * t38 * x2 * x1 * t3 * t27
      t44 = s ** 2
      t45 = 0.1D1 / t44
      t46 = pi * t45
      t47 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t36, -t29, t3
     #7, -t32, -t43)
      t49 = 0.1D1 / x3
      t50 = 0.1D1 / x2
      t52 = 0.1D1 / x1
      t53 = t49 * t50 * t52
      t55 = t46 * t47 * t53 / 0.8D1
      t56 = FJET(XB1, XB2, s, -t29, -t32, t36, t37, -t43, t55)
      t61 = t47 * t49 * t50 * t52
      t64 = FJET(XB1, XB2, s, t36, t37, -t29, -t32, -t43, t55)
      t69 = FJET(XB1, XB2, s, -t32, -t29, t37, t36, -t43, t55)
      t74 = FJET(XB1, XB2, s, t37, t36, -t32, -t29, -t43, t55)
      t80 = t1 * t3
      t81 = x3 * s * t80
      t82 = t30 * t80
      t83 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t81, t82, t3
     #7, -t32, 0.0D0)
      t84 = x1 ** 2
      t85 = z ** 2
      t86 = 0.1D1 / t85
      t87 = t84 * t86
      t88 = Sin(t13)
      t89 = t88 ** 2
      t90 = t87 * t89
      t91 = t3 ** 2
      t92 = t27 * t91
      t93 = x3 * t19
      t97 = log(-0.4D1 * t90 * t92 * t93)
      t98 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t81, t82, t3
     #7, -t32, 0.0D0)
      t103 = pi * lh
      t114 = -(0.90D2 * t46 * (t83 - t97 * t98) - 0.180D3 * t103 * t45 *
     # t98) * t49 * t52 / 0.720D3 - t46 * t98 * t53 / 0.8D1
      t115 = FJET(XB1, XB2, s, t37, -t81, -t32, t82, 0.0D0, t114)
      t117 = t2 * x1
      t118 = t2 * t3
      t119 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t118
     #, 0.0D0, t117, 0.0D0)
      t120 = x3 * t84
      t123 = t89 * t91 * t27
      t126 = log(0.4D1 * t120 * t86 * t123)
      t127 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t118
     #, 0.0D0, t117, 0.0D0)
      t132 = t45 * t127
      t134 = 0.180D3 * t103 * t132
      t138 = (0.90D2 * t46 * (-t119 + t126 * t127) + t134) * t49 * t52 /
     # 0.720D3
      t141 = t46 * t127 * t53 / 0.8D1
      t142 = x2 * t84
      t143 = t142 * t89
      t148 = log(0.4D1 * t143 * t86 * t27 * t91)
      t156 = (0.90D2 * t46 * (-t119 + t148 * t127) + t134) * t50 * t52 /
     # 0.720D3
      t159 = log(0.4D1 * t87 * t123)
      t161 = t159 ** 2
      t164 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t118
     #, 0.0D0, t117, 0.0D0)
      t165 = t159 * t119 - t161 * t127 / 0.2D1 - t164
      t169 = -t119 + t159 * t127
      t173 = lh ** 2
      t175 = pi ** 2
      t177 = 0.180D3 * t173 - 0.30D2 * t175
      t178 = pi * t177
      t179 = t178 * t132
      t183 = -t138 + t141 - t156 - (0.90D2 * t46 * t165 - 0.180D3 * t103
     # * t45 * t169 - t179) * t52 / 0.720D3
      t184 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t117, -t118, 0.0D0, t183)
      t187 = x2 * t1 * s
      t189 = t15 * t1 * s
      t190 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t187, -t189,
     # 0.0D0, 0.0D0, 0.0D0)
      t191 = t89 * t86
      t192 = t191 * t15
      t195 = log(-0.4D1 * t9 * t192)
      t196 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t187, -t189,
     # 0.0D0, 0.0D0, 0.0D0)
      t201 = t45 * t196
      t203 = 0.180D3 * t103 * t201
      t207 = (0.90D2 * t46 * (-t190 + t195 * t196) + t203) * t49 * t50 /
     # 0.1440D4
      t208 = x2 * t89
      t209 = t86 * t15
      t212 = log(-0.4D1 * t208 * t209)
      t214 = t212 ** 2
      t217 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, t187, -t189,
     # 0.0D0, 0.0D0, 0.0D0)
      t218 = -t212 * t190 + t214 * t196 / 0.2D1 + t217
      t222 = t190 - t212 * t196
      t226 = t178 * t201
      t232 = t46 * t196 * t53 / 0.8D1
      t235 = log(-0.4D1 * t142 * t192)
      t243 = (0.90D2 * t46 * (-t190 + t235 * t196) + t203) * t50 * t52 /
     # 0.720D3
      t244 = -t207 - (-0.90D2 * t46 * t218 + 0.180D3 * t103 * t45 * t222
     # - t226) * t50 / 0.1440D4 + t232 - t243
      t245 = FJET(XB1, XB2, s, t187, 0.0D0, -t189, 0.0D0, 0.0D0, t244)
      t247 = x3 * t86
      t250 = log(0.4D1 * t247 * t89)
      t251 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t253 = t250 ** 2
      t254 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t257 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t266 = t45 * t254
      t267 = t178 * t266
      t272 = log(0.4D1 * t191)
      t273 = t272 * pi
      t276 = t272 ** 2
      t277 = t276 * pi
      t283 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t311 = log(0.4D1 * t9 * t191)
      t317 = 0.180D3 * t103 * t266
      t322 = t208 * t86
      t324 = log(0.4D1 * t322)
      t326 = t324 ** 2
      t342 = log(0.4D1 * t120 * t191)
      t356 = log(0.4D1 * t142 * t191)
      t366 = log(0.4D1 * t90)
      t368 = t366 ** 2
      t382 = -(0.90D2 * t46 * (-t250 * t251 + t253 * t254 / 0.2D1 + t257
     #) - 0.180D3 * t103 * t45 * (t251 - t250 * t254) + t267) * t49 / 0.
     #1440D4 - (0.180D3 * t273 * lh + 0.45D2 * t277 + t178) * t45 * t251
     # / 0.1440D4 - t46 * t283 / 0.16D2 - (-0.90D2 * t277 * lh + pi * (0
     #.60D2 * lh * t175 - 0.240D3 * zeta3 - 0.120D3 * t173 * lh) - 0.15D
     #2 * t276 * t272 * pi - t273 * t177) * t45 * t254 / 0.1440D4 - (-0.
     #180D3 * t103 - 0.90D2 * t273) * t45 * t257 / 0.1440D4 - (0.90D2 * 
     #t46 * (t251 - t311 * t254) - t317) * t49 * t50 / 0.1440D4 - (0.90D
     #2 * t46 * (-t324 * t251 + t326 * t254 / 0.2D1 + t257) - 0.180D3 * 
     #t103 * t45 * (t251 - t324 * t254) + t267) * t50 / 0.1440D4 - (0.90
     #D2 * t46 * (t251 - t342 * t254) - t317) * t49 * t52 / 0.720D3 - t4
     #6 * t254 * t53 / 0.8D1 - (0.90D2 * t46 * (t251 - t356 * t254) - t3
     #17) * t50 * t52 / 0.720D3 - (0.90D2 * t46 * (-t366 * t251 + t368 *
     # t254 / 0.2D1 + t257) - 0.180D3 * t103 * t45 * (t251 - t366 * t254
     #) + t267) * t52 / 0.720D3
      t383 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t382)
      t385 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t382)
      t389 = Sqrt(x2 * t15 * t93)
      t391 = 0.2D1 * t14 * t389
      t393 = t2 * (0.1D1 - x2 - x3 + t10 + t391)
      t395 = t2 * (-x3 + t10 - x2 + t391)
      t396 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t395, t393,
     # 0.0D0, 0.0D0, 0.0D0)
      t400 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t395, t393,
     # 0.0D0, 0.0D0, 0.0D0)
      t404 = log(0.4D1 * t322 * t93 * t15)
      t416 = -t46 * t396 * t53 / 0.8D1 - (0.90D2 * t46 * (t400 - t404 * 
     #t396) - 0.180D3 * t103 * t45 * t396) * t49 * t50 / 0.1440D4
      t417 = FJET(XB1, XB2, s, 0.0D0, t393, 0.0D0, -t395, 0.0D0, t416)
      t419 = FJET(XB1, XB2, s, -t32, t82, t37, -t81, 0.0D0, t114)
      t421 = FJET(XB1, XB2, s, -t395, 0.0D0, t393, 0.0D0, 0.0D0, t416)
      t423 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t382)
      t426 = t15 * s * t80
      t429 = t2 * t3 * x2 * t27
      t430 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t429, t426,
     # 0.0D0, t117, -t43)
      t434 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t429, t426,
     # 0.0D0, t117, -t43)
      t438 = log(-0.4D1 * t143 * t209 * t92)
      t450 = -t46 * t430 * t53 / 0.8D1 - (0.90D2 * t46 * (t434 - t438 * 
     #t430) - 0.180D3 * t103 * t45 * t430) * t50 * t52 / 0.720D3
      t451 = FJET(XB1, XB2, s, t426, t117, -t429, 0.0D0, -t43, t450)
      t463 = -t207 - (-0.90D2 * t46 * t218 + 0.180D3 * t103 * t45 * t222
     # - t226) * t50 / 0.1440D4 + t232 - t243
      t464 = FJET(XB1, XB2, s, 0.0D0, t187, 0.0D0, -t189, 0.0D0, t463)
      t466 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t382)
      t468 = t56 * pi * t45 * t61 / 0.8D1 + t64 * pi * t45 * t61 / 0.8D1
     # + t69 * pi * t45 * t61 / 0.8D1 + t74 * pi * t45 * t61 / 0.8D1 + t
     #115 * t114 + t184 * t183 + t245 * t244 + t383 * t382 + t385 * t382
     # + t417 * t416 + t419 * t114 + t421 * t416 + t423 * t382 + t451 * 
     #t450 + t464 * t463 + t466 * t382
      t469 = FJET(XB1, XB2, s, 0.0D0, -t429, t117, t426, -t43, t450)
      t471 = FJET(XB1, XB2, s, 0.0D0, -t189, 0.0D0, t187, 0.0D0, t463)
      t473 = t2 * x3
      t474 = t2 * t19
      t477 = log(-0.4D1 * t191 * t93)
      t478 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t473, -t474,
     # 0.0D0, 0.0D0, 0.0D0)
      t480 = t477 ** 2
      t481 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t473, -t474,
     # 0.0D0, 0.0D0, 0.0D0)
      t484 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, t473, -t474,
     # 0.0D0, 0.0D0, 0.0D0)
      t485 = -t477 * t478 + t480 * t481 / 0.2D1 + t484
      t489 = t478 - t477 * t481
      t493 = t45 * t481
      t494 = t178 * t493
      t497 = (-0.90D2 * t46 * t485 + 0.180D3 * t103 * t45 * t489 - t494)
     # * t49 / 0.1440D4
      t501 = log(-0.4D1 * t208 * t247 * t19)
      t503 = -t478 + t501 * t481
      t507 = 0.180D3 * t103 * t493
      t511 = (0.90D2 * t46 * t503 + t507) * t49 * t50 / 0.1440D4
      t515 = log(-0.4D1 * t120 * t191 * t19)
      t523 = (0.90D2 * t46 * (-t478 + t515 * t481) + t507) * t49 * t52 /
     # 0.720D3
      t526 = t46 * t481 * t53 / 0.8D1
      t527 = -t497 - t511 - t523 + t526
      t528 = FJET(XB1, XB2, s, t473, 0.0D0, -t474, 0.0D0, 0.0D0, t527)
      t537 = -t497 - (0.90D2 * t46 * t503 + t507) * t49 * t50 / 0.1440D4
     # - t523 + t526
      t538 = FJET(XB1, XB2, s, -t474, 0.0D0, t473, 0.0D0, 0.0D0, t537)
      t540 = FJET(XB1, XB2, s, -t429, 0.0D0, t426, t117, -t43, t450)
      t542 = FJET(XB1, XB2, s, -t189, 0.0D0, t187, 0.0D0, 0.0D0, t244)
      t544 = FJET(XB1, XB2, s, t82, -t32, -t81, t37, 0.0D0, t114)
      t556 = -t138 + t141 - t156 - (0.90D2 * t46 * t165 - 0.180D3 * t103
     # * t45 * t169 - t179) * t52 / 0.720D3
      t557 = FJET(XB1, XB2, s, -t118, t117, 0.0D0, 0.0D0, 0.0D0, t556)
      t559 = FJET(XB1, XB2, s, t117, t426, 0.0D0, -t429, -t43, t450)
      t561 = FJET(XB1, XB2, s, 0.0D0, -t395, 0.0D0, t393, 0.0D0, t416)
      t573 = -t511 - (-0.90D2 * t46 * t485 + 0.180D3 * t103 * t45 * t489
     # - t494) * t49 / 0.1440D4 - t523 + t526
      t574 = FJET(XB1, XB2, s, 0.0D0, -t474, 0.0D0, t473, 0.0D0, t573)
      t576 = FJET(XB1, XB2, s, t117, -t118, 0.0D0, 0.0D0, 0.0D0, t556)
      t578 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t118, t117, 0.0D0, t183)
      t580 = FJET(XB1, XB2, s, 0.0D0, t473, 0.0D0, -t474, 0.0D0, t573)
      t582 = FJET(XB1, XB2, s, -t81, t37, t82, -t32, 0.0D0, t114)
      t584 = FJET(XB1, XB2, s, t393, 0.0D0, -t395, 0.0D0, 0.0D0, t416)
      t586 = t469 * t450 + t471 * t463 + t528 * t527 + t538 * t537 + t54
     #0 * t450 + t542 * t244 + t544 * t114 + t557 * t556 + t559 * t450 +
     # t561 * t416 + t574 * t573 + t576 * t556 + t578 * t183 + t580 * t5
     #73 + t582 * t114 + t584 * t416
      rrgg2qqbarht8s1e0 = t468 + t586

      end function



      doubleprecision function rrgg2qqbarht8s1em1
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

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
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t18 = t2 * (-x3 + t4 - x2 + t14)
      t19 = s ** 2
      t20 = 0.1D1 / t19
      t21 = pi * t20
      t22 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t18, t16, 0.
     #0D0, 0.0D0, 0.0D0)
      t23 = 0.1D1 / x3
      t25 = 0.1D1 / x2
      t26 = t22 * t23 * t25
      t28 = t21 * t26 / 0.16D2
      t29 = FJET(XB1, XB2, s, 0.0D0, t16, 0.0D0, -t18, 0.0D0, -t28)
      t35 = -0.1D1 + x1
      t36 = t1 * t35
      t37 = t7 * s * t36
      t38 = t2 * x1
      t42 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t44 = t2 * t35 * x2 * t42
      t45 = t1 ** 2
      t50 = s * t45 * x2 * x1 * t35 * t42
      t51 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t44, t37, 0.
     #0D0, t38, -t50)
      t53 = 0.1D1 / x1
      t54 = t51 * t25 * t53
      t56 = t21 * t54 / 0.8D1
      t57 = FJET(XB1, XB2, s, t37, t38, -t44, 0.0D0, -t50, -t56)
      t62 = t9 * s
      t63 = t62 * t36
      t65 = t62 * t1 * x1
      t67 = x3 * s * t36
      t69 = t2 * x1 * x3
      t70 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t67, t63, t6
     #9, -t65, 0.0D0)
      t72 = t70 * t23 * t53
      t74 = t21 * t72 / 0.8D1
      t75 = FJET(XB1, XB2, s, t63, -t65, -t67, t69, 0.0D0, -t74)
      t80 = FJET(XB1, XB2, s, 0.0D0, -t18, 0.0D0, t16, 0.0D0, -t28)
      t85 = FJET(XB1, XB2, s, t16, 0.0D0, -t18, 0.0D0, 0.0D0, -t28)
      t90 = FJET(XB1, XB2, s, -t44, 0.0D0, t37, t38, -t50, -t56)
      t95 = FJET(XB1, XB2, s, -t65, t63, t69, -t67, 0.0D0, -t74)
      t100 = FJET(XB1, XB2, s, t38, t37, 0.0D0, -t44, -t50, -t56)
      t105 = t2 * t35
      t106 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t105
     #, 0.0D0, t38, 0.0D0)
      t110 = t21 * t106 * t25 * t53 / 0.8D1
      t111 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t105
     #, 0.0D0, t38, 0.0D0)
      t112 = x1 ** 2
      t113 = z ** 2
      t114 = 0.1D1 / t113
      t115 = t112 * t114
      t116 = Sin(t5)
      t117 = t116 ** 2
      t119 = t35 ** 2
      t123 = log(0.4D1 * t115 * t117 * t42 * t119)
      t125 = -t111 + t123 * t106
      t128 = pi * lh
      t131 = 0.180D3 * t128 * t20 * t106
      t138 = t21 * t106 * t23 * t53 / 0.8D1
      t139 = t110 - (0.90D2 * t21 * t125 + t131) * t53 / 0.720D3 + t138
      t140 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t105, t38, 0.0D0, t139)
      t142 = FJET(XB1, XB2, s, 0.0D0, -t44, t38, t37, -t50, -t56)
      t147 = FJET(XB1, XB2, s, -t67, t69, t63, -t65, 0.0D0, -t74)
      t152 = t2 * t9
      t153 = t2 * x3
      t154 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t153, -t152,
     # 0.0D0, 0.0D0, 0.0D0)
      t155 = t114 * t117
      t158 = log(-0.4D1 * t155 * t10)
      t159 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t153, -t152,
     # 0.0D0, 0.0D0, 0.0D0)
      t161 = t154 - t158 * t159
      t166 = 0.180D3 * t128 * t20 * t159
      t170 = t159 * t23
      t173 = t21 * t170 * t25 / 0.16D2
      t176 = t21 * t170 * t53 / 0.8D1
      t177 = -(-0.90D2 * t21 * t161 + t166) * t23 / 0.1440D4 + t173 + t1
     #76
      t178 = FJET(XB1, XB2, s, -t152, 0.0D0, t153, 0.0D0, 0.0D0, t177)
      t180 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t184 = log(0.4D1 * x3 * t114 * t117)
      t185 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t192 = 0.180D3 * t128 * t20 * t185
      t198 = log(0.4D1 * t155)
      t199 = t198 * pi
      t207 = t198 ** 2
      t210 = lh ** 2
      t212 = pi ** 2
      t220 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0
     #.0D0, 0.0D0, 0.0D0)
      t223 = t185 * t23
      t227 = x2 * t117
      t230 = log(0.4D1 * t227 * t114)
      t244 = log(0.4D1 * t115 * t117)
      t255 = -(0.90D2 * t21 * (t180 - t184 * t185) - t192) * t23 / 0.144
     #0D4 - (-0.180D3 * t128 - 0.90D2 * t199) * t20 * t180 / 0.1440D4 - 
     #(0.180D3 * t199 * lh + 0.45D2 * t207 * pi + pi * (0.180D3 * t210 -
     # 0.30D2 * t212)) * t20 * t185 / 0.1440D4 - t21 * t220 / 0.16D2 - t
     #21 * t223 * t25 / 0.16D2 - (0.90D2 * t21 * (t180 - t230 * t185) - 
     #t192) * t25 / 0.1440D4 - t21 * t185 * t25 * t53 / 0.8D1 - (0.90D2 
     #* t21 * (t180 - t244 * t185) - t192) * t53 / 0.720D3 - t21 * t223 
     #* t53 / 0.8D1
      t256 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t255)
      t258 = FJET(XB1, XB2, s, t69, -t67, -t65, t63, 0.0D0, -t74)
      t263 = -t29 * pi * t20 * t26 / 0.16D2 - t57 * pi * t20 * t54 / 0.8
     #D1 - t75 * pi * t20 * t72 / 0.8D1 - t80 * pi * t20 * t26 / 0.16D2 
     #- t85 * pi * t20 * t26 / 0.16D2 - t90 * pi * t20 * t54 / 0.8D1 - t
     #95 * pi * t20 * t72 / 0.8D1 - t100 * pi * t20 * t54 / 0.8D1 + t140
     # * t139 - t142 * pi * t20 * t54 / 0.8D1 - t147 * pi * t20 * t72 / 
     #0.8D1 + t178 * t177 + t256 * t255 - t258 * pi * t20 * t72 / 0.8D1
      t264 = FJET(XB1, XB2, s, -t18, 0.0D0, t16, 0.0D0, 0.0D0, -t28)
      t269 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t38, -t105, 0.0D0, t139)
      t277 = t110 - (0.90D2 * t21 * t125 + t131) * t53 / 0.720D3 + t138
      t278 = FJET(XB1, XB2, s, t38, -t105, 0.0D0, 0.0D0, 0.0D0, t277)
      t286 = -(-0.90D2 * t21 * t161 + t166) * t23 / 0.1440D4 + t173 + t1
     #76
      t287 = FJET(XB1, XB2, s, 0.0D0, -t152, 0.0D0, t153, 0.0D0, t286)
      t289 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t255)
      t291 = FJET(XB1, XB2, s, 0.0D0, t153, 0.0D0, -t152, 0.0D0, t286)
      t293 = FJET(XB1, XB2, s, t153, 0.0D0, -t152, 0.0D0, 0.0D0, t177)
      t295 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t255)
      t298 = x2 * t1 * s
      t300 = t7 * t1 * s
      t301 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t298, -t300,
     # 0.0D0, 0.0D0, 0.0D0)
      t305 = t21 * t301 * t23 * t25 / 0.16D2
      t306 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t298, -t300,
     # 0.0D0, 0.0D0, 0.0D0)
      t310 = log(-0.4D1 * t227 * t114 * t7)
      t312 = t306 - t310 * t301
      t317 = 0.180D3 * t128 * t20 * t301
      t324 = t21 * t301 * t25 * t53 / 0.8D1
      t325 = t305 - (-0.90D2 * t21 * t312 + t317) * t25 / 0.1440D4 + t32
     #4
      t326 = FJET(XB1, XB2, s, t298, 0.0D0, -t300, 0.0D0, 0.0D0, t325)
      t334 = t305 - (-0.90D2 * t21 * t312 + t317) * t25 / 0.1440D4 + t32
     #4
      t335 = FJET(XB1, XB2, s, 0.0D0, t298, 0.0D0, -t300, 0.0D0, t334)
      t337 = FJET(XB1, XB2, s, -t105, t38, 0.0D0, 0.0D0, 0.0D0, t277)
      t339 = FJET(XB1, XB2, s, -t300, 0.0D0, t298, 0.0D0, 0.0D0, t325)
      t341 = FJET(XB1, XB2, s, 0.0D0, -t300, 0.0D0, t298, 0.0D0, t334)
      t343 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t255)
      t345 = -t264 * pi * t20 * t26 / 0.16D2 + t269 * t139 + t278 * t277
     # + t287 * t286 + t289 * t255 + t291 * t286 + t293 * t177 + t295 * 
     #t255 + t326 * t325 + t335 * t334 + t337 * t277 + t339 * t325 + t34
     #1 * t334 + t343 * t255
      rrgg2qqbarht8s1em1 = t263 + t345

      end function



      doubleprecision function rrgg2qqbarht8s1em2
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

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
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t7 = 0.1D1 / x3
      t11 = 0.1D1 / x2
      t15 = 0.1D1 / x1
      t19 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.
     #0D0, 0.0D0, 0.0D0)
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
      t46 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t45, 
     #0.0D0, t43, 0.0D0)
      t49 = t5 * t46 * t15 / 0.8D1
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t43, -t45, 0.0D0, t49)
      t53 = t4 * t46 * t15
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t45, t43, 0.0D0, t49)
      t60 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t38)
      t62 = t2 * x3
      t64 = t2 * (-0.1D1 + x3)
      t65 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t62, -t64, 0.
     #0D0, 0.0D0, 0.0D0)
      t68 = t5 * t65 * t7 / 0.16D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t62, 0.0D0, -t64, 0.0D0, t68)
      t72 = t4 * t65 * t7
      t76 = x2 * t1 * s
      t79 = (-0.1D1 + x2) * t1 * s
      t80 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t76, -t79, 0.
     #0D0, 0.0D0, 0.0D0)
      t83 = t5 * t80 * t11 / 0.16D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t76, 0.0D0, -t79, 0.0D0, t83)
      t87 = t4 * t80 * t11
      t90 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t76, 0.0D0, t83)
      t94 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t62, 0.0D0, t68)
      t98 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t38)
      t100 = FJET(XB1, XB2, s, t43, -t45, 0.0D0, 0.0D0, 0.0D0, t49)
      t104 = FJET(XB1, XB2, s, t62, 0.0D0, -t64, 0.0D0, 0.0D0, t68)
      t108 = FJET(XB1, XB2, s, t76, 0.0D0, -t79, 0.0D0, 0.0D0, t83)
      t112 = FJET(XB1, XB2, s, -t64, 0.0D0, t62, 0.0D0, 0.0D0, t68)
      t116 = FJET(XB1, XB2, s, -t79, 0.0D0, t76, 0.0D0, 0.0D0, t83)
      t120 = FJET(XB1, XB2, s, -t45, t43, 0.0D0, 0.0D0, 0.0D0, t49)
      rrgg2qqbarht8s1em2 = t39 * t38 + t41 * t38 + t50 * pi * t53 / 0.8D
     #1 + t56 * pi * t53 / 0.8D1 + t60 * t38 + t69 * pi * t72 / 0.16D2 +
     # t84 * pi * t87 / 0.16D2 + t90 * pi * t87 / 0.16D2 + t94 * pi * t7
     #2 / 0.16D2 + t98 * t38 + t100 * pi * t53 / 0.8D1 + t104 * pi * t72
     # / 0.16D2 + t108 * pi * t87 / 0.16D2 + t112 * pi * t72 / 0.16D2 + 
     #t116 * pi * t87 / 0.16D2 + t120 * pi * t53 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht8s1em3
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht8s1em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0.
     #16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht8s1em4
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht8s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht8s2e1
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

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
      t6 = x1 ** 2
      t7 = x3 * t6
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t17 = log(0.4D1 * t7 * t14)
      t18 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t24 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t28 = pi * lh
      t34 = lh ** 2
      t36 = pi ** 2
      t38 = 0.180D3 * t34 - 0.30D2 * t36
      t39 = pi * t38
      t40 = t4 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t49 = t6 * t10 * t13
      t51 = log(0.4D1 * t49)
      t56 = t51 ** 2
      t59 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t72 = 0.60D2 * lh * t36 - 0.240D3 * zeta3 - 0.120D3 * t34 * lh
      t73 = pi * t72
      t74 = t73 * t40
      t85 = x2 * x3
      t88 = log(0.4D1 * t85 * t49)
      t90 = t85 * t6
      t91 = -0.1D1 + x2
      t92 = t14 * t91
      t95 = log(-0.4D1 * t90 * t92)
      t99 = 0.1D1 / x2
      t101 = t43 * t99 * t45
      t104 = x2 * t6
      t107 = log(0.4D1 * t104 * t14)
      t109 = t107 ** 2
      t114 = log(-0.4D1 * t104 * t92)
      t116 = t114 ** 2
      t132 = x3 * t10
      t135 = log(0.4D1 * t132 * t13)
      t140 = t135 ** 2
      t162 = log(0.4D1 * t85 * t14)
      t164 = t162 ** 2
      t169 = log(-0.4D1 * t85 * t92)
      t171 = t169 ** 2
      t192 = x2 * t10
      t195 = log(0.4D1 * t192 * t13)
      t196 = t195 ** 2
      t200 = log(-0.4D1 * t192 * t13 * t91)
      t201 = t200 ** 2
      t224 = log(0.4D1 * t14)
      t225 = t224 ** 2
      t226 = t225 * pi
      t230 = t225 * t224 * pi
      t232 = t224 * pi
      t251 = rrgg2qqbarh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t261 = t36 ** 2
      t262 = t34 ** 2
      t268 = t225 ** 2
      t275 = -(0.90D2 * t5 * (-t17 * t18 + t20 * t21 / 0.2D1 + t24) - 0.
     #180D3 * t28 * t4 * (t18 - t17 * t21) + t41) * t43 * t45 / 0.720D3 
     #- (t39 * t4 * (t18 - t51 * t21) + 0.90D2 * t5 * (t56 * t18 / 0.2D1
     # + t59 - t56 * t51 * t21 / 0.6D1 - t51 * t24) + t74 - 0.180D3 * t2
     #8 * t4 * (-t51 * t18 + t56 * t21 / 0.2D1 + t24)) * t45 / 0.720D3 -
     # t5 * (-t88 * t21 + t95 * t21) * t101 / 0.8D1 - (0.90D2 * t5 * (-t
     #107 * t18 + t109 * t21 / 0.2D1 + t114 * t18 - t116 * t21 / 0.2D1) 
     #- 0.180D3 * t28 * t4 * (-t107 * t21 + t114 * t21)) * t99 * t45 / 0
     #.720D3 + (t39 * t4 * (-t18 + t135 * t21) + 0.90D2 * t5 * (-t140 * 
     #t18 / 0.2D1 - t59 + t140 * t135 * t21 / 0.6D1 + t135 * t24) - t74 
     #- 0.180D3 * t28 * t4 * (t135 * t18 - t140 * t21 / 0.2D1 - t24)) * 
     #t43 / 0.1440D4 - (0.90D2 * t5 * (-t162 * t18 + t164 * t21 / 0.2D1 
     #+ t169 * t18 - t171 * t21 / 0.2D1) - 0.180D3 * t28 * t4 * (-t162 *
     # t21 + t169 * t21)) * t43 * t99 / 0.1440D4 - ((0.90D2 * t5 * t18 -
     # 0.180D3 * t28 * t40) * (t196 / 0.2D1 - t201 / 0.2D1) + 0.90D2 * t
     #5 * t21 * (t201 * t200 / 0.6D1 - t196 * t195 / 0.6D1) + (-0.180D3 
     #* t28 * t4 * t18 + t41 + 0.90D2 * t5 * t24) * (-t195 + t200)) * t9
     #9 / 0.1440D4 - (-0.90D2 * t226 * lh + t73 - 0.15D2 * t230 - t232 *
     # t38) * t4 * t18 / 0.1440D4 - (0.180D3 * t232 * lh + 0.45D2 * t226
     # + t39) * t4 * t24 / 0.1440D4 - (-0.180D3 * t28 - 0.90D2 * t232) *
     # t4 * t59 / 0.1440D4 - t5 * t251 / 0.16D2 - (0.30D2 * t230 * lh + 
     #t226 * t38 / 0.2D1 - t232 * t72 + pi * (0.480D3 * lh * zeta3 + t26
     #1 + 0.60D2 * t262 - 0.60D2 * t34 * t36) + 0.15D2 / 0.4D1 * t268 * 
     #pi) * t4 * t21 / 0.1440D4
      t276 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t275)
      t278 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t275)
      t280 = t2 * x1
      t281 = -0.1D1 + x1
      t282 = t2 * t281
      t283 = 0.1D1 / t8
      t285 = t281 ** 2
      t287 = x1 * z
      t288 = -z - x1 + t287
      t289 = 0.1D1 / t288
      t290 = t13 * t285 * t289
      t293 = log(-0.4D1 * t7 * t283 * t290)
      t294 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280,
     # 0.0D0, -t282, 0.0D0)
      t296 = t293 ** 2
      t297 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280,
     # 0.0D0, -t282, 0.0D0)
      t300 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280,
     # 0.0D0, -t282, 0.0D0)
      t309 = t4 * t297
      t310 = t39 * t309
      t313 = (0.90D2 * t5 * (t293 * t294 - t296 * t297 / 0.2D1 - t300) -
     # 0.180D3 * t28 * t4 * (-t294 + t293 * t297) - t310) * t43 * t45
      t317 = log(-0.4D1 * t6 * t283 * t290)
      t319 = -t294 + t317 * t297
      t322 = t317 ** 2
      t325 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t280,
     # 0.0D0, -t282, 0.0D0)
      t330 = -t322 * t294 / 0.2D1 - t325 + t322 * t317 * t297 / 0.6D1 + 
     #t317 * t300
      t333 = t73 * t309
      t337 = t317 * t294 - t322 * t297 / 0.2D1 - t300
      t343 = t13 * t283
      t344 = t285 * t289
      t348 = log(-0.4D1 * t90 * t343 * t344)
      t357 = t99 * t45
      t358 = (0.90D2 * t5 * (-t294 + t348 * t297) + 0.180D3 * t28 * t309
     #) * t43 * t357
      t359 = t104 * t13
      t364 = log(-0.4D1 * t359 * t283 * t285 * t289)
      t366 = t364 ** 2
      t379 = (0.90D2 * t5 * (t364 * t294 - t366 * t297 / 0.2D1 - t300) -
     # 0.180D3 * t28 * t4 * (-t294 + t364 * t297) - t310) * t99 * t45
      t381 = -t313 / 0.720D3 - (t39 * t4 * t319 + 0.90D2 * t5 * t330 - t
     #333 - 0.180D3 * t28 * t4 * t337) * t45 / 0.720D3 - t358 / 0.720D3 
     #- t379 / 0.720D3
      t382 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t280, -t282, 0.0D0, t381)
      t384 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t282, t280, 0.0D0, t381)
      t386 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t275)
      t388 = t2 * x3
      t389 = -0.1D1 + x3
      t390 = t2 * t389
      t394 = log(-0.4D1 * t7 * t14 * t389)
      t395 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t388, -t390, 0.0D0)
      t397 = t394 ** 2
      t398 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t388, -t390, 0.0D0)
      t401 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t388, -t390, 0.0D0)
      t410 = t4 * t398
      t419 = log(-0.4D1 * t359 * t132 * t389)
      t425 = log(0.4D1 * t90 * t14 * t91 * t389)
      t431 = t13 * t389
      t434 = log(-0.4D1 * t132 * t431)
      t439 = t434 ** 2
      t442 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t388, -t390, 0.0D0)
      t465 = log(-0.4D1 * t192 * t13 * x3 * t389)
      t467 = t465 ** 2
      t474 = log(0.4D1 * t85 * t10 * t431 * t91)
      t476 = t474 ** 2
      t492 = -(0.90D2 * t5 * (t394 * t395 - t397 * t398 / 0.2D1 - t401) 
     #- 0.180D3 * t28 * t4 * (-t395 + t394 * t398) - t39 * t410) * t43 *
     # t45 / 0.720D3 - t5 * (t419 * t398 - t425 * t398) * t101 / 0.8D1 +
     # (t39 * t4 * (t395 - t434 * t398) + 0.90D2 * t5 * (t439 * t395 / 0
     #.2D1 + t442 - t439 * t434 * t398 / 0.6D1 - t434 * t401) + t73 * t4
     #10 - 0.180D3 * t28 * t4 * (-t434 * t395 + t439 * t398 / 0.2D1 + t4
     #01)) * t43 / 0.1440D4 - (0.90D2 * t5 * (t465 * t395 - t467 * t398 
     #/ 0.2D1 - t474 * t395 + t476 * t398 / 0.2D1) - 0.180D3 * t28 * t4 
     #* (t465 * t398 - t474 * t398)) * t43 * t99 / 0.1440D4
      t493 = FJET(XB1, XB2, s, 0.0D0, t388, 0.0D0, -t390, 0.0D0, t492)
      t495 = FJET(XB1, XB2, s, 0.0D0, -t390, 0.0D0, t388, 0.0D0, t492)
      t497 = x2 * x1
      t499 = t2 * t497 * t289
      t501 = t1 * x1
      t502 = t91 * s * t501
      t503 = t1 ** 2
      t508 = s * t503 * x2 * x1 * t281 * t289
      t509 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t499, -t502
     #, 0.0D0, -t282, t508)
      t510 = t6 * t13
      t511 = t85 * t510
      t512 = t283 * t289
      t513 = t285 * t91
      t514 = t512 * t513
      t517 = log(0.4D1 * t511 * t514)
      t518 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t499, -t502
     #, 0.0D0, -t282, t508)
      t523 = t4 * t518
      t531 = log(0.4D1 * t359 * t514)
      t533 = t531 ** 2
      t536 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t499, -t502
     #, 0.0D0, -t282, t508)
      t550 = -(0.90D2 * t5 * (t509 - t517 * t518) - 0.180D3 * t28 * t523
     #) * t43 * t357 / 0.720D3 - (0.90D2 * t5 * (-t531 * t509 + t533 * t
     #518 / 0.2D1 + t536) - 0.180D3 * t28 * t4 * (t509 - t531 * t518) + 
     #t39 * t523) * t99 * t45 / 0.720D3
      t551 = FJET(XB1, XB2, s, 0.0D0, -t499, -t282, -t502, t508, t550)
      t553 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t275)
      t568 = -t313 / 0.720D3 - (t39 * t4 * t319 + 0.90D2 * t5 * t330 - t
     #333 - 0.180D3 * t28 * t4 * t337) * t45 / 0.720D3 - t358 / 0.720D3 
     #- t379 / 0.720D3
      t569 = FJET(XB1, XB2, s, t280, -t282, 0.0D0, 0.0D0, 0.0D0, t568)
      t571 = FJET(XB1, XB2, s, t388, 0.0D0, -t390, 0.0D0, 0.0D0, t492)
      t573 = x3 * x1
      t574 = t2 * t573
      t576 = t1 * t281
      t577 = x3 * s * t576
      t578 = t389 * s
      t579 = t578 * t501
      t580 = t578 * t576
      t582 = x3 * t389
      t586 = log(0.4D1 * t510 * t283 * t582 * t344)
      t587 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t574, -t579,
     # -t577, t580, 0.0D0)
      t589 = t586 ** 2
      t590 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t574, -t579,
     # -t577, t580, 0.0D0)
      t593 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, t574, -t579,
     # -t577, t580, 0.0D0)
      t602 = t4 * t590
      t613 = log(0.4D1 * t289 * t6 * t343 * t285 * x2 * t582)
      t624 = -(0.90D2 * t5 * (-t586 * t587 + t589 * t590 / 0.2D1 + t593)
     # - 0.180D3 * t28 * t4 * (t587 - t586 * t590) + t39 * t602) * t43 *
     # t45 / 0.720D3 - (0.90D2 * t5 * (t587 - t613 * t590) - 0.180D3 * t
     #28 * t602) * t43 * t357 / 0.720D3
      t625 = FJET(XB1, XB2, s, t574, -t577, -t579, t580, 0.0D0, t624)
      t627 = t276 * t275 + t278 * t275 + t382 * t381 + t384 * t381 + t38
     #6 * t275 + t493 * t492 + t495 * t492 + t551 * t550 + t553 * t275 +
     # t569 * t568 + t571 * t492 + t625 * t624
      t628 = FJET(XB1, XB2, s, t580, -t579, -t577, t574, 0.0D0, t624)
      t632 = x3 * z
      t633 = t573 * z
      t634 = t85 * z
      t635 = t85 * x1
      t636 = t85 * t287
      t637 = cos(t11)
      t642 = Sqrt(-x3 * t91 * t288 * x2 * t389)
      t644 = 0.2D1 * t637 * t642
      t645 = z + x1 - t287 - x2 * z - t497 + t497 * z - t632 - t573 + t6
     #33 + t634 + t635 - t636 + t85 + t644
      t648 = t2 * x1 * t645 * t289
      t652 = t2 * x1 * (-t632 - t573 + t633 + t634 + t635 - t636 - x2 + 
     #t85 + t644) * t289
      t653 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t652, -t648,
     # -t577, t580, t508)
      t658 = log(-0.4D1 * t511 * t512 * t513 * t389)
      t659 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t652, -t648,
     # -t577, t580, t508)
      t661 = -t653 + t658 * t659
      t666 = 0.180D3 * t28 * t4 * t659
      t667 = 0.90D2 * t5 * t661 + t666
      t670 = t667 * t43 * t357 / 0.720D3
      t671 = FJET(XB1, XB2, s, t580, -t648, -t577, t652, t508, -t670)
      t675 = FJET(XB1, XB2, s, t652, -t577, -t648, t580, t508, -t670)
      t679 = FJET(XB1, XB2, s, -t282, t280, 0.0D0, 0.0D0, 0.0D0, t568)
      t681 = FJET(XB1, XB2, s, -t282, -t502, 0.0D0, -t499, t508, t550)
      t683 = FJET(XB1, XB2, s, -t390, 0.0D0, t388, 0.0D0, 0.0D0, t492)
      t685 = FJET(XB1, XB2, s, -t577, t574, t580, -t579, 0.0D0, t624)
      t687 = FJET(XB1, XB2, s, -t577, t652, t580, -t648, t508, -t670)
      t691 = FJET(XB1, XB2, s, -t502, -t282, -t499, 0.0D0, t508, t550)
      t693 = FJET(XB1, XB2, s, -t579, t580, t574, -t577, 0.0D0, t624)
      t695 = FJET(XB1, XB2, s, -t499, 0.0D0, -t502, -t282, t508, t550)
      t700 = 0.90D2 * t5 * t661 + t666
      t704 = FJET(XB1, XB2, s, -t648, t580, t652, -t577, t508, -t700 * t
     #43 * t357 / 0.720D3)
      t708 = t628 * t624 - t671 * t667 * t101 / 0.720D3 - t675 * t667 * 
     #t101 / 0.720D3 + t679 * t568 + t681 * t550 + t683 * t492 + t685 * 
     #t624 - t687 * t667 * t101 / 0.720D3 + t691 * t550 + t693 * t624 + 
     #t695 * t550 - t704 * t700 * t101 / 0.720D3
      rrgg2qqbarht8s2e1 = t627 + t708

      end function



      doubleprecision function rrgg2qqbarht8s2e0
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

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
      t6 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t7 = x1 ** 2
      t8 = x3 * t7
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t18 = log(0.4D1 * t8 * t15)
      t19 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
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
      t46 = 0.1D1 / x2
      t54 = log(0.4D1 * t7 * t11 * t14)
      t56 = t54 ** 2
      t59 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t68 = lh ** 2
      t70 = pi ** 2
      t72 = 0.180D3 * t68 - 0.30D2 * t70
      t73 = pi * t72
      t74 = t73 * t25
      t78 = x3 * t11
      t81 = log(0.4D1 * t78 * t14)
      t83 = t81 ** 2
      t98 = log(0.4D1 * t15)
      t99 = t98 * pi
      t102 = t98 ** 2
      t103 = t102 * pi
      t109 = rrgg2qqbarh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, 0.0D0, t2, 0.0D0)
      t135 = x2 * x3
      t138 = log(0.4D1 * t135 * t15)
      t142 = log(-0.4D1 * t135 * t40)
      t149 = x2 * t11
      t152 = log(0.4D1 * t149 * t14)
      t153 = t152 ** 2
      t157 = log(-0.4D1 * t149 * t14 * t39)
      t158 = t157 ** 2
      t172 = -(0.90D2 * t5 * (t6 - t18 * t19) - t27) * t29 * t31 / 0.720
     #D3 - t5 * (-t37 * t19 + t43 * t19) * t46 * t31 / 0.8D1 - (0.90D2 *
     # t5 * (-t54 * t6 + t56 * t19 / 0.2D1 + t59) - 0.180D3 * t24 * t4 *
     # (t6 - t54 * t19) + t74) * t31 / 0.720D3 + (0.90D2 * t5 * (t81 * t
     #6 - t83 * t19 / 0.2D1 - t59) - 0.180D3 * t24 * t4 * (-t6 + t81 * t
     #19) - t74) * t29 / 0.1440D4 - (0.180D3 * t99 * lh + 0.45D2 * t103 
     #+ t73) * t4 * t6 / 0.1440D4 - t5 * t109 / 0.16D2 - (-0.90D2 * t103
     # * lh + pi * (0.60D2 * lh * t70 - 0.240D3 * zeta3 - 0.120D3 * t68 
     #* lh) - 0.15D2 * t102 * t98 * pi - t99 * t72) * t4 * t19 / 0.1440D
     #4 - (-0.180D3 * t24 - 0.90D2 * t99) * t4 * t59 / 0.1440D4 - t5 * (
     #-t138 * t19 + t142 * t19) * t29 * t46 / 0.16D2 - (0.90D2 * t5 * t1
     #9 * (t153 / 0.2D1 - t158 / 0.2D1) + (0.90D2 * t5 * t6 - t27) * (-t
     #152 + t157)) * t46 / 0.1440D4
      t173 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t172)
      t175 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t172)
      t177 = t2 * x1
      t178 = -0.1D1 + x1
      t179 = t2 * t178
      t180 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t177,
     # 0.0D0, -t179, 0.0D0)
      t181 = 0.1D1 / t9
      t183 = t178 ** 2
      t185 = x1 * z
      t186 = -z - x1 + t185
      t187 = 0.1D1 / t186
      t188 = t14 * t183 * t187
      t191 = log(-0.4D1 * t8 * t181 * t188)
      t192 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t177,
     # 0.0D0, -t179, 0.0D0)
      t197 = t4 * t192
      t199 = 0.180D3 * t24 * t197
      t203 = (0.90D2 * t5 * (-t180 + t191 * t192) + t199) * t29 * t31 / 
     #0.720D3
      t206 = t29 * t46 * t31
      t208 = t5 * t192 * t206 / 0.8D1
      t209 = t34 * t14
      t214 = log(-0.4D1 * t209 * t181 * t183 * t187)
      t222 = (0.90D2 * t5 * (-t180 + t214 * t192) + t199) * t46 * t31 / 
     #0.720D3
      t226 = log(-0.4D1 * t7 * t181 * t188)
      t228 = t226 ** 2
      t231 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t177,
     # 0.0D0, -t179, 0.0D0)
      t232 = t226 * t180 - t228 * t192 / 0.2D1 - t231
      t236 = -t180 + t226 * t192
      t240 = t73 * t197
      t244 = -t203 + t208 - t222 - (0.90D2 * t5 * t232 - 0.180D3 * t24 *
     # t4 * t236 - t240) * t31 / 0.720D3
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t177, -t179, 0.0D0, t244)
      t247 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t179, t177, 0.0D0, t244)
      t249 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t172)
      t251 = t2 * x3
      t252 = -0.1D1 + x3
      t253 = t2 * t252
      t254 = t14 * t252
      t257 = log(-0.4D1 * t78 * t254)
      t258 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t251, -t253, 0.0D0)
      t260 = t257 ** 2
      t261 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t251, -t253, 0.0D0)
      t264 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t251, -t253, 0.0D0)
      t273 = t4 * t261
      t281 = log(-0.4D1 * t8 * t15 * t252)
      t296 = log(-0.4D1 * t149 * t14 * x3 * t252)
      t302 = log(0.4D1 * t135 * t11 * t254 * t39)
      t309 = (0.90D2 * t5 * (-t257 * t258 + t260 * t261 / 0.2D1 + t264) 
     #- 0.180D3 * t24 * t4 * (t258 - t257 * t261) + t73 * t273) * t29 / 
     #0.1440D4 - (0.90D2 * t5 * (-t258 + t281 * t261) + 0.180D3 * t24 * 
     #t273) * t29 * t31 / 0.720D3 - t5 * (t296 * t261 - t302 * t261) * t
     #29 * t46 / 0.16D2
      t310 = FJET(XB1, XB2, s, 0.0D0, t251, 0.0D0, -t253, 0.0D0, t309)
      t312 = FJET(XB1, XB2, s, 0.0D0, -t253, 0.0D0, t251, 0.0D0, t309)
      t314 = x2 * x1
      t316 = t2 * t314 * t187
      t318 = t1 * x1
      t319 = t39 * s * t318
      t320 = t1 ** 2
      t325 = s * t320 * x2 * x1 * t178 * t187
      t326 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t316, -t319
     #, 0.0D0, -t179, t325)
      t330 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t316, -t319
     #, 0.0D0, -t179, t325)
      t336 = log(0.4D1 * t209 * t181 * t187 * t183 * t39)
      t348 = -t5 * t326 * t206 / 0.8D1 - (0.90D2 * t5 * (t330 - t336 * t
     #326) - 0.180D3 * t24 * t4 * t326) * t46 * t31 / 0.720D3
      t349 = FJET(XB1, XB2, s, 0.0D0, -t316, -t179, -t319, t325, t348)
      t351 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t172)
      t363 = -t203 + t208 - t222 - (0.90D2 * t5 * t232 - 0.180D3 * t24 *
     # t4 * t236 - t240) * t31 / 0.720D3
      t364 = FJET(XB1, XB2, s, t177, -t179, 0.0D0, 0.0D0, 0.0D0, t363)
      t366 = FJET(XB1, XB2, s, t251, 0.0D0, -t253, 0.0D0, 0.0D0, t309)
      t368 = x3 * x1
      t369 = t2 * t368
      t371 = t1 * t178
      t372 = x3 * s * t371
      t373 = t252 * s
      t374 = t373 * t318
      t375 = t373 * t371
      t376 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, t369, -t374,
     # -t372, t375, 0.0D0)
      t384 = log(0.4D1 * t7 * t14 * t181 * x3 * t252 * t187 * t183)
      t385 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t369, -t374,
     # -t372, t375, 0.0D0)
      t400 = -(0.90D2 * t5 * (t376 - t384 * t385) - 0.180D3 * t24 * t4 *
     # t385) * t29 * t31 / 0.720D3 - t5 * t385 * t206 / 0.8D1
      t401 = FJET(XB1, XB2, s, t369, -t372, -t374, t375, 0.0D0, t400)
      t403 = t173 * t172 + t175 * t172 + t245 * t244 + t247 * t244 + t24
     #9 * t172 + t310 * t309 + t312 * t309 + t349 * t348 + t351 * t172 +
     # t364 * t363 + t366 * t309 + t401 * t400
      t404 = FJET(XB1, XB2, s, t375, -t374, -t372, t369, 0.0D0, t400)
      t408 = x3 * z
      t409 = t368 * z
      t410 = t135 * z
      t411 = t135 * x1
      t412 = t135 * t185
      t413 = cos(t12)
      t418 = Sqrt(-x3 * t39 * t186 * x2 * t252)
      t420 = 0.2D1 * t413 * t418
      t421 = z + x1 - t185 - x2 * z - t314 + t314 * z - t408 - t368 + t4
     #09 + t410 + t411 - t412 + t135 + t420
      t424 = t2 * x1 * t421 * t187
      t428 = t2 * x1 * (-t408 - t368 + t409 + t410 + t411 - t412 - x2 + 
     #t135 + t420) * t187
      t429 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t428, -t424,
     # -t372, t375, t325)
      t432 = t5 * t429 * t206 / 0.8D1
      t433 = FJET(XB1, XB2, s, t375, -t424, -t372, t428, t325, t432)
      t438 = t429 * t29 * t46 * t31
      t441 = FJET(XB1, XB2, s, t428, -t372, -t424, t375, t325, t432)
      t446 = FJET(XB1, XB2, s, -t179, t177, 0.0D0, 0.0D0, 0.0D0, t363)
      t448 = FJET(XB1, XB2, s, -t179, -t319, 0.0D0, -t316, t325, t348)
      t450 = FJET(XB1, XB2, s, -t253, 0.0D0, t251, 0.0D0, 0.0D0, t309)
      t452 = FJET(XB1, XB2, s, -t372, t369, t375, -t374, 0.0D0, t400)
      t454 = FJET(XB1, XB2, s, -t372, t428, t375, -t424, t325, t432)
      t459 = FJET(XB1, XB2, s, -t319, -t179, -t316, 0.0D0, t325, t348)
      t461 = FJET(XB1, XB2, s, -t374, t375, t369, -t372, 0.0D0, t400)
      t463 = FJET(XB1, XB2, s, -t316, 0.0D0, -t319, -t179, t325, t348)
      t465 = FJET(XB1, XB2, s, -t424, t375, t428, -t372, t325, t432)
      t470 = t404 * t400 + t433 * pi * t4 * t438 / 0.8D1 + t441 * pi * t
     #4 * t438 / 0.8D1 + t446 * t363 + t448 * t348 + t450 * t309 + t452 
     #* t400 + t454 * pi * t4 * t438 / 0.8D1 + t459 * t348 + t461 * t400
     # + t463 * t348 + t465 * pi * t4 * t438 / 0.8D1
      rrgg2qqbarht8s2e0 = t403 + t470

      end function



      doubleprecision function rrgg2qqbarht8s2em1
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

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
      t6 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x3 * t9
      t12 = Sin(x4 * pi)
      t13 = t12 ** 2
      t16 = log(0.4D1 * t10 * t13)
      t17 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t4 * t17
      t27 = 0.1D1 / x3
      t30 = x1 ** 2
      t34 = log(0.4D1 * t30 * t9 * t13)
      t40 = 0.1D1 / x1
      t47 = x2 * t9
      t50 = log(0.4D1 * t47 * t13)
      t51 = -0.1D1 + x2
      t55 = log(-0.4D1 * t47 * t13 * t51)
      t58 = 0.1D1 / x2
      t65 = log(0.4D1 * t9 * t13)
      t66 = t65 * pi
      t74 = t65 ** 2
      t77 = lh ** 2
      t79 = pi ** 2
      t87 = rrgg2qqbarh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t90 = (0.90D2 * t5 * (-t6 + t16 * t17) + t25) * t27 / 0.1440D4 - (
     #0.90D2 * t5 * (t6 - t34 * t17) - t25) * t40 / 0.720D3 - t5 * t17 *
     # t27 * t40 / 0.8D1 - t5 * t17 * (-t50 + t55) * t58 / 0.16D2 - (-0.
     #180D3 * t22 - 0.90D2 * t66) * t4 * t6 / 0.1440D4 - (0.180D3 * t66 
     #* lh + 0.45D2 * t74 * pi + pi * (0.180D3 * t77 - 0.30D2 * t79)) * 
     #t4 * t17 / 0.1440D4 - t5 * t87 / 0.16D2
      t91 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t90)
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t90)
      t95 = t2 * x1
      t96 = -0.1D1 + x1
      t97 = t2 * t96
      t98 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t95, 0
     #.0D0, -t97, 0.0D0)
      t102 = t5 * t98 * t58 * t40 / 0.8D1
      t103 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t95, 
     #0.0D0, -t97, 0.0D0)
      t108 = 0.1D1 / (-z - x1 + x1 * z)
      t110 = t96 ** 2
      t114 = log(-0.4D1 * t30 / t7 * t13 * t108 * t110)
      t116 = -t103 + t114 * t98
      t121 = 0.180D3 * t22 * t4 * t98
      t128 = t5 * t98 * t27 * t40 / 0.8D1
      t129 = t102 - (0.90D2 * t5 * t116 + t121) * t40 / 0.720D3 + t128
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t95, -t97, 0.0D0, t129)
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t97, t95, 0.0D0, t129)
      t134 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t90)
      t136 = t2 * x3
      t137 = -0.1D1 + x3
      t138 = t2 * t137
      t139 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t136, -t138, 0.0D0)
      t144 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t136, -t138, 0.0D0)
      t148 = log(-0.4D1 * t10 * t13 * t137)
      t159 = t5 * t139 * t27 * t40 / 0.8D1 + (0.90D2 * t5 * (t144 - t148
     # * t139) - 0.180D3 * t22 * t4 * t139) * t27 / 0.1440D4
      t160 = FJET(XB1, XB2, s, 0.0D0, t136, 0.0D0, -t138, 0.0D0, t159)
      t162 = FJET(XB1, XB2, s, 0.0D0, -t138, 0.0D0, t136, 0.0D0, t159)
      t166 = t2 * x1 * x2 * t108
      t168 = t1 * x1
      t169 = t51 * s * t168
      t170 = t1 ** 2
      t175 = s * t170 * x2 * x1 * t96 * t108
      t176 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t166, -t169
     #, 0.0D0, -t97, t175)
      t178 = t176 * t58 * t40
      t180 = t5 * t178 / 0.8D1
      t181 = FJET(XB1, XB2, s, 0.0D0, -t166, -t97, -t169, t175, -t180)
      t186 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t90)
      t194 = t102 - (0.90D2 * t5 * t116 + t121) * t40 / 0.720D3 + t128
      t195 = FJET(XB1, XB2, s, t95, -t97, 0.0D0, 0.0D0, 0.0D0, t194)
      t197 = FJET(XB1, XB2, s, t136, 0.0D0, -t138, 0.0D0, 0.0D0, t159)
      t200 = t2 * x1 * x3
      t202 = t1 * t96
      t203 = x3 * s * t202
      t204 = t137 * s
      t205 = t204 * t168
      t206 = t204 * t202
      t207 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, t200, -t205,
     # -t203, t206, 0.0D0)
      t209 = t207 * t27 * t40
      t211 = t5 * t209 / 0.8D1
      t212 = FJET(XB1, XB2, s, t200, -t203, -t205, t206, 0.0D0, -t211)
      t217 = FJET(XB1, XB2, s, t206, -t205, -t203, t200, 0.0D0, -t211)
      t222 = FJET(XB1, XB2, s, -t97, t95, 0.0D0, 0.0D0, 0.0D0, t194)
      t224 = FJET(XB1, XB2, s, -t97, -t169, 0.0D0, -t166, t175, -t180)
      t229 = FJET(XB1, XB2, s, -t138, 0.0D0, t136, 0.0D0, 0.0D0, t159)
      t231 = FJET(XB1, XB2, s, -t203, t200, t206, -t205, 0.0D0, -t211)
      t236 = FJET(XB1, XB2, s, -t169, -t97, -t166, 0.0D0, t175, -t180)
      t241 = FJET(XB1, XB2, s, -t205, t206, t200, -t203, 0.0D0, -t211)
      t246 = FJET(XB1, XB2, s, -t166, 0.0D0, -t169, -t97, t175, -t180)
      rrgg2qqbarht8s2em1 = t91 * t90 + t93 * t90 + t130 * t129 + t132 * 
     #t129 + t134 * t90 + t160 * t159 + t162 * t159 - t181 * pi * t4 * t
     #178 / 0.8D1 + t186 * t90 + t195 * t194 + t197 * t159 - t212 * pi *
     # t4 * t209 / 0.8D1 - t217 * pi * t4 * t209 / 0.8D1 + t222 * t194 -
     # t224 * pi * t4 * t178 / 0.8D1 + t229 * t159 - t231 * pi * t4 * t2
     #09 / 0.8D1 - t236 * pi * t4 * t178 / 0.8D1 - t241 * pi * t4 * t209
     # / 0.8D1 - t246 * pi * t4 * t178 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht8s2em2
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = pi * t4
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t7 = 0.1D1 / x1
      t11 = 0.1D1 / x3
      t15 = rrgg2qqbarh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # 0.0D0, t2, 0.0D0)
      t20 = z ** 2
      t24 = Sin(x4 * pi)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t35 = -t5 * t6 * t7 / 0.8D1 - t5 * t6 * t11 / 0.16D2 - t5 * t15 / 
     #0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t28 * pi) * t4 * t6 / 0.14
     #40D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t40 = t2 * x1
      t42 = t2 * (-0.1D1 + x1)
      t43 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t40, 0
     #.0D0, -t42, 0.0D0)
      t46 = t5 * t43 * t7 / 0.8D1
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t40, -t42, 0.0D0, t46)
      t50 = t4 * t43 * t7
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t42, t40, 0.0D0, t46)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t59 = t2 * x3
      t61 = t2 * (-0.1D1 + x3)
      t62 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t59, -t61, 0.0D0)
      t65 = t5 * t62 * t11 / 0.16D2
      t66 = FJET(XB1, XB2, s, 0.0D0, t59, 0.0D0, -t61, 0.0D0, t65)
      t69 = t4 * t62 * t11
      t72 = FJET(XB1, XB2, s, 0.0D0, -t61, 0.0D0, t59, 0.0D0, t65)
      t76 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t78 = FJET(XB1, XB2, s, t40, -t42, 0.0D0, 0.0D0, 0.0D0, t46)
      t82 = FJET(XB1, XB2, s, t59, 0.0D0, -t61, 0.0D0, 0.0D0, t65)
      t86 = FJET(XB1, XB2, s, -t42, t40, 0.0D0, 0.0D0, 0.0D0, t46)
      t90 = FJET(XB1, XB2, s, -t61, 0.0D0, t59, 0.0D0, 0.0D0, t65)
      rrgg2qqbarht8s2em2 = t36 * t35 + t38 * t35 + t47 * pi * t50 / 0.8D
     #1 + t53 * pi * t50 / 0.8D1 + t57 * t35 + t66 * pi * t69 / 0.16D2 +
     # t72 * pi * t69 / 0.16D2 + t76 * t35 + t78 * pi * t50 / 0.8D1 + t8
     #2 * pi * t69 / 0.16D2 + t86 * pi * t50 / 0.8D1 + t90 * pi * t69 / 
     #0.16D2

      end function



      doubleprecision function rrgg2qqbarht8s2em3
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t6 = rrgg2qqbarh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #0.0D0, t2, 0.0D0)
      t8 = pi * t4 * t6 / 0.16D2
      t9 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t8)
      t11 = t4 * t6
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t8)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t8)
      t19 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t8)
      rrgg2qqbarht8s2em3 = -t9 * pi * t11 / 0.16D2 - t13 * pi * t11 / 0.
     #16D2 - t16 * pi * t11 / 0.16D2 - t19 * pi * t11 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht8s2em4
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
      doubleprecision rrgg2qqbarh81J1
      doubleprecision rrgg2qqbarh81J2
      doubleprecision rrgg2qqbarh81J3
      doubleprecision rrgg2qqbarh81J4
      doubleprecision rrgg2qqbarh81J5
      doubleprecision rrgg2qqbarh81J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht8s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh81J1
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
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t8 = S14 ** 2
      t11 = S13 ** 2
      rrgg2qqbarh81J1 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8D1 *
     # t5 * t2 * nf + 0.3D1 / 0.8D1 * t5 * (-t8 + 0.2D1 * S13 * S14 - t1
     #1) * nf / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J2
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
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t11 = S14 ** 2
      t14 = S13 ** 2
      t17 = S24 ** 2
      rrgg2qqbarh81J2 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8D1 *
     # (0.2D1 * t5 * t2 - 0.2D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (0.2D
     #1 * t5 * (-t11 + 0.2D1 * S13 * S14 - t14) - 0.2D1 * t5 * t17) * nf
     # / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J3
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
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t12 = S14 ** 2
      t15 = S13 ** 2
      t19 = S24 ** 2
      rrgg2qqbarh81J3 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8D1 *
     # (0.3D1 * t5 * t2 - 0.4D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (0.3D
     #1 * t5 * (-t12 + 0.2D1 * S13 * S14 - t15) - 0.4D1 * t5 * t19) * nf
     # / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J4
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
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t12 = S14 ** 2
      t15 = S13 ** 2
      t19 = S24 ** 2
      rrgg2qqbarh81J4 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8D1 *
     # (0.4D1 * t5 * t2 - 0.6D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (0.4D
     #1 * t5 * (-t12 + 0.2D1 * S13 * S14 - t15) - 0.6D1 * t5 * t19) * nf
     # / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J5
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
      t1 = -S13 + S14
      t2 = 0.2D1 * t1
      t5 = -t1
      t12 = S14 ** 2
      t15 = S13 ** 2
      t19 = S24 ** 2
      rrgg2qqbarh81J5 = (0.3D1 / 0.8D1 * t2 * nf * S12 + 0.3D1 / 0.8D1 *
     # (0.5D1 * t5 * t2 - 0.8D1 * t5 * S24) * nf + 0.3D1 / 0.8D1 * (0.5D
     #1 * t5 * (-t12 + 0.2D1 * S13 * S14 - t15) - 0.8D1 * t5 * t19) * nf
     # / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh81J6
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
      t1 = S13 - S14
      t9 = S24 ** 2
      rrgg2qqbarh81J6 = (0.15D2 / 0.4D1 * t1 * nf * S12 - 0.15D2 / 0.4D1
     # * t1 * S24 * nf - 0.15D2 / 0.4D1 * t1 * t9 * nf / S12) / pi * wd 
     #/ z

      end function
  
 