      subroutine rrgg2qqbarht6
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt6
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt6
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhard61J1  
      doubleprecision rrgg2qqbarhhard61J2  
      doubleprecision rrgg2qqbarhhard61J3  
      doubleprecision rrgg2qqbarhhard61J4  
      doubleprecision rrgg2qqbarhhard61J5  
      doubleprecision rrgg2qqbarhhard61J6  
      doubleprecision rrgg2qqbarhhard61J7  
      doubleprecision rrgg2qqbarhhardt6s1e1  
      doubleprecision rrgg2qqbarhhardt6s1e0  
      doubleprecision rrgg2qqbarhhardt6s1em1  
      doubleprecision rrgg2qqbarhhardt6s1em2  
      doubleprecision rrgg2qqbarhhardt6s1em3  
      doubleprecision rrgg2qqbarhhardt6s1em4  
      doubleprecision rrgg2qqbarhhardt6s2e1  
      doubleprecision rrgg2qqbarhhardt6s2e0  
      doubleprecision rrgg2qqbarhhardt6s2em1  
      doubleprecision rrgg2qqbarhhardt6s2em2  
      doubleprecision rrgg2qqbarhhardt6s2em3  
      doubleprecision rrgg2qqbarhhardt6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt6s1e1
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t3 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t6 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t8 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t9 = lh ** 2
      t11 = pi ** 2
      t13 = 0.180D3 * t9 - 0.30D2 * t11
      t16 = s ** 2
      t18 = 0.1D1 / t16 / s
      t20 = x4 * pi
      t21 = Sin(t20)
      t22 = t21 ** 2
      t23 = x3 * t22
      t24 = z ** 2
      t25 = 0.1D1 / t24
      t28 = log(0.4D1 * t23 * t25)
      t29 = -0.1D1 + x3
      t30 = 0.1D1 / t29
      t34 = log(-0.4D1 * t23 * t25 * t30)
      t35 = cos(t20)
      t37 = Sqrt(-x3 * t29)
      t41 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t35 * t37)
      t47 = t34 ** 2
      t50 = t28 ** 2
      t64 = 0.60D2 * lh * t11 - 0.240D3 * zeta3 - 0.120D3 * t9 * lh
      t66 = rrgg2qqbarhhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t85 = 0.1D1 / x3
      t88 = t25 * t22
      t90 = log(0.4D1 * t88)
      t91 = t90 ** 2
      t94 = t91 * t90
      t109 = t11 ** 2
      t110 = t9 ** 2
      t118 = rrgg2qqbarhhard61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t126 = t91 ** 2
      t133 = t18 * pi
      t134 = x1 ** 2
      t135 = x3 * t134
      t136 = t88 * t30
      t139 = log(-0.4D1 * t135 * t136)
      t141 = t139 ** 2
      t148 = log(0.4D1 * t135 * t88)
      t150 = t148 ** 2
      t156 = lh * t18
      t165 = t13 * t18
      t167 = t8 + t8 * t41
      t168 = pi * t167
      t172 = 0.1D1 / x1
      t175 = t134 * t22
      t176 = t175 * t25
      t178 = log(0.4D1 * t176)
      t183 = t178 ** 2
      t193 = t64 * t18
      t194 = pi * t8
      t195 = t193 * t194
      t206 = x2 ** 2
      t207 = x3 * t206
      t208 = t207 * t134
      t211 = log(-0.4D1 * t208 * t136)
      t215 = t207 * t176
      t217 = log(0.4D1 * t215)
      t226 = 0.1D1 / x2
      t227 = t226 * t172
      t230 = t206 * t134
      t233 = log(0.4D1 * t230 * t88)
      t235 = t233 ** 2
      t253 = log(-0.4D1 * t207 * t136)
      t255 = t253 ** 2
      t262 = log(0.4D1 * t207 * t88)
      t264 = t262 ** 2
      t285 = t206 * t22
      t288 = log(0.4D1 * t285 * t25)
      t293 = t288 ** 2
      t313 = ((-0.180D3 * t3 * lh + 0.90D2 * t6 + t8 * t13) * t18 * pi *
     # (t28 + t34 * t41) + 0.90D2 * t8 * t18 * pi * (t47 * t34 * t41 / 0
     #.6D1 + t50 * t28 / 0.6D1) + (-0.180D3 * t6 * lh + t8 * t64 + 0.90D
     #2 * t66 + t3 * t13) * t18 * pi * (-0.1D1 - t41) + (-0.180D3 * t8 *
     # lh + 0.90D2 * t3) * t18 * pi * (-t50 / 0.2D1 - t47 * t41 / 0.2D1)
     #) * t85 / 0.2880D4 - (-0.180D3 * (t91 * t3 / 0.2D1 + t66 - t94 * t
     #8 / 0.6D1 - t90 * t6) * lh + (t6 - t90 * t3 + t91 * t8 / 0.2D1) * 
     #t13 + (t3 - t90 * t8) * t64 + t8 * (t109 + 0.60D2 * t110 + 0.480D3
     # * lh * zeta3 - 0.60D2 * t9 * t11) + 0.90D2 * t118 - 0.15D2 * t94 
     #* t3 + 0.45D2 * t91 * t6 - 0.90D2 * t90 * t66 + 0.15D2 / 0.4D1 * t
     #126 * t8) * t18 * pi / 0.2880D4 - (0.90D2 * t133 * ((t6 - t139 * t
     #3 + t141 * t8 / 0.2D1) * t41 + t6 - t148 * t3 + t150 * t8 / 0.2D1)
     # - 0.180D3 * t156 * pi * ((t3 - t139 * t8) * t41 + t3 - t148 * t8)
     # + t165 * t168) * t85 * t172 / 0.1440D4 + (t165 * pi * (-t3 + t178
     # * t8) + 0.90D2 * t133 * (-t183 * t3 / 0.2D1 - t66 + t183 * t178 *
     # t8 / 0.6D1 + t178 * t6) - t195 - 0.180D3 * t156 * pi * (-t6 + t17
     #8 * t3 - t183 * t8 / 0.2D1)) * t172 / 0.1440D4 - (0.90D2 * t133 * 
     #((t3 - t211 * t8) * t41 + t3 - t217 * t8) - 0.180D3 * t156 * t168)
     # * t85 * t227 / 0.720D3 - (0.90D2 * t133 * (t6 - t233 * t3 + t235 
     #* t8 / 0.2D1) - 0.180D3 * t156 * pi * (t3 - t233 * t8) + t165 * t1
     #94) * t226 * t172 / 0.720D3 + (0.90D2 * t133 * (-(t6 - t253 * t3 +
     # t255 * t8 / 0.2D1) * t41 - t6 + t262 * t3 - t264 * t8 / 0.2D1) - 
     #0.180D3 * t156 * pi * (-(t3 - t253 * t8) * t41 - t3 + t262 * t8) -
     # t165 * pi * t167) * t85 * t226 / 0.1440D4 - (t165 * pi * (t3 - t2
     #88 * t8) + 0.90D2 * t133 * (t293 * t3 / 0.2D1 + t66 - t293 * t288 
     #* t8 / 0.6D1 - t288 * t6) + t195 - 0.180D3 * t156 * pi * (t6 - t28
     #8 * t3 + t293 * t8 / 0.2D1)) * t226 / 0.1440D4
      t314 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t313)
      t316 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t313)
      t318 = t2 * x1
      t319 = -0.1D1 + x1
      t320 = t2 * t319
      t321 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t320, 0.0D0, t318, 0.0D0)
      t322 = t135 * t22
      t323 = x1 * z
      t324 = 0.1D1 - x1 + t323
      t325 = 0.1D1 / t324
      t326 = t25 * t325
      t327 = t319 ** 2
      t328 = t326 * t327
      t331 = log(0.4D1 * t322 * t328)
      t332 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t320, 0.0D0, t318, 0.0D0)
      t334 = t331 ** 2
      t335 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t320, 0.0D0, t318, 0.0D0)
      t339 = t326 * t327 * t30
      t342 = log(-0.4D1 * t322 * t339)
      t344 = t342 ** 2
      t348 = x3 * x1
      t349 = t348 * z
      t352 = x3 * t324
      t354 = Sqrt(-t352 * t29)
      t358 = 0.1D1 / (-0.2D1 * t349 + 0.2D1 * t348 - 0.1D1 - x3 + 0.2D1 
     #* t35 * t354)
      t373 = pi * (-t335 * t358 - t335)
      t381 = log(0.4D1 * t175 * t328)
      t386 = t381 ** 2
      t389 = rrgg2qqbarhhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t320, 0.0D0, t318, 0.0D0)
      t397 = pi * t335
      t409 = t207 * t175
      t412 = log(-0.4D1 * t409 * t339)
      t416 = t325 * t327
      t420 = log(0.4D1 * t208 * t88 * t416)
      t431 = t230 * t22
      t434 = log(0.4D1 * t431 * t328)
      t436 = t434 ** 2
      t452 = -(0.90D2 * t133 * (-t321 + t331 * t332 - t334 * t335 / 0.2D
     #1 - (t321 - t342 * t332 + t344 * t335 / 0.2D1) * t358) - 0.180D3 *
     # t156 * pi * (-t332 + t331 * t335 - (t332 - t342 * t335) * t358) +
     # t165 * t373) * t85 * t172 / 0.1440D4 + (t165 * pi * (t332 - t381 
     #* t335) + 0.90D2 * t133 * (t386 * t332 / 0.2D1 + t389 - t386 * t38
     #1 * t335 / 0.6D1 - t381 * t321) + t193 * t397 - 0.180D3 * t156 * p
     #i * (t321 - t381 * t332 + t386 * t335 / 0.2D1)) * t172 / 0.1440D4 
     #- (0.90D2 * t133 * (-(t332 - t412 * t335) * t358 - t332 + t420 * t
     #335) - 0.180D3 * t156 * t373) * t85 * t227 / 0.720D3 - (0.90D2 * t
     #133 * (-t321 + t434 * t332 - t436 * t335 / 0.2D1) - 0.180D3 * t156
     # * pi * (-t332 + t434 * t335) - t165 * t397) * t226 * t172 / 0.720
     #D3
      t453 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t318, -t320, 0.0D0, t452)
      t455 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t320, t318, 0.0D0, t452)
      t457 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t313)
      t460 = x2 * t1 * s
      t461 = -0.1D1 + x2
      t463 = t461 * t1 * s
      t464 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t460, -t
     #463, 0.0D0, 0.0D0, 0.0D0)
      t465 = t88 * t461
      t468 = log(-0.4D1 * t208 * t465)
      t469 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t460, -t
     #463, 0.0D0, 0.0D0, 0.0D0)
      t472 = x2 * z
      t474 = 0.1D1 / (0.1D1 + t472 - x2)
      t479 = pi * t469 * t474
      t486 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, t460, -t
     #463, 0.0D0, 0.0D0, 0.0D0)
      t489 = log(-0.4D1 * t230 * t465)
      t491 = t489 ** 2
      t504 = t165 * t479
      t511 = log(-0.4D1 * t207 * t465)
      t513 = t511 ** 2
      t530 = t25 * t461
      t533 = log(-0.4D1 * t285 * t530)
      t539 = t533 ** 2
      t542 = rrgg2qqbarhhard61J4(s, XB1, XB2, z, lh, wd, nf, s, t460, -t
     #463, 0.0D0, 0.0D0, 0.0D0)
      t563 = -(-0.90D2 * t133 * (t464 - t468 * t469) * t474 + 0.180D3 * 
     #t156 * t479) * t85 * t227 / 0.720D3 - (-0.90D2 * t133 * (t486 - t4
     #89 * t464 + t491 * t469 / 0.2D1) * t474 + 0.180D3 * t156 * pi * (t
     #464 - t489 * t469) * t474 - t504) * t226 * t172 / 0.720D3 + (0.90D
     #2 * t133 * (t486 - t511 * t464 + t513 * t469 / 0.2D1) * t474 - 0.1
     #80D3 * t156 * pi * (t464 - t511 * t469) * t474 + t504) * t85 * t22
     #6 / 0.1440D4 - (-t165 * pi * (t464 - t533 * t469) * t474 - 0.90D2 
     #* t133 * (t539 * t464 / 0.2D1 + t542 - t539 * t533 * t469 / 0.6D1 
     #- t533 * t486) * t474 - t193 * t479 + 0.180D3 * t156 * pi * (t486 
     #- t533 * t464 + t539 * t469 / 0.2D1) * t474) * t226 / 0.1440D4
      t564 = FJET(XB1, XB2, s, 0.0D0, t460, 0.0D0, -t463, 0.0D0, t563)
      t566 = x2 * x3
      t569 = Sqrt(x3 * t461 * t29)
      t570 = t35 * t569
      t572 = 0.2D1 * t570 * x2
      t574 = 0.1D1 - x3 + t566
      t575 = 0.1D1 / t574
      t577 = t2 * (0.1D1 - x3 - x2 + t566 + t207 + t572) * t575
      t578 = 0.2D1 * t570
      t582 = t2 * x2 * (-0.1D1 + t566 + t578) * t575
      t583 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t582, t
     #577, 0.0D0, 0.0D0, 0.0D0)
      t584 = t574 ** 2
      t585 = 0.1D1 / t584
      t587 = t530 * t29 * t585
      t590 = log(0.4D1 * t409 * t587)
      t591 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t582, t
     #577, 0.0D0, 0.0D0, 0.0D0)
      t596 = t207 * z
      t598 = 0.1D1 / (-0.1D1 + 0.2D1 * t570 * t472 + t596 - t207 + t566 
     #- t472 + x2 - x3 - t572 + t578)
      t603 = pi * t591 * t598
      t610 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, -t582, t
     #577, 0.0D0, 0.0D0, 0.0D0)
      t614 = log(0.4D1 * t207 * t22 * t587)
      t616 = t614 ** 2
      t634 = -(-0.90D2 * t133 * (t583 - t590 * t591) * t598 + 0.180D3 * 
     #t156 * t603) * t85 * t227 / 0.720D3 + (0.90D2 * t133 * (t610 - t61
     #4 * t583 + t616 * t591 / 0.2D1) * t598 - 0.180D3 * t156 * pi * (t5
     #83 - t614 * t591) * t598 + t165 * t603) * t85 * t226 / 0.1440D4
      t635 = FJET(XB1, XB2, s, 0.0D0, t577, 0.0D0, -t582, 0.0D0, t634)
      t637 = FJET(XB1, XB2, s, 0.0D0, -t463, 0.0D0, t460, 0.0D0, t563)
      t639 = FJET(XB1, XB2, s, 0.0D0, -t582, 0.0D0, t577, 0.0D0, t634)
      t643 = t2 * t319 * x2 * t325
      t646 = t461 * s * t1 * t319
      t647 = t1 ** 2
      t652 = s * t647 * x2 * x1 * t319 * t325
      t653 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t643, t
     #646, 0.0D0, t318, -t652)
      t654 = t324 * t653
      t656 = t326 * t327 * t461
      t659 = log(-0.4D1 * t409 * t656)
      t661 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t643, t
     #646, 0.0D0, t318, -t652)
      t664 = x2 * x1
      t665 = t664 * z
      t667 = 0.1D1 / (-0.1D1 + t665 - t472 - t664 + x1 + x2 - t323)
      t671 = t156 * pi
      t673 = t324 * t661 * t667
      t679 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, -t643, t
     #646, 0.0D0, t318, -t652)
      t683 = log(-0.4D1 * t431 * t656)
      t684 = t683 * t324
      t686 = t683 ** 2
      t706 = -(-0.90D2 * t133 * (t654 - t659 * t324 * t661) * t667 + 0.1
     #80D3 * t671 * t673) * t85 * t227 / 0.720D3 - (-0.90D2 * t133 * (t3
     #24 * t679 - t684 * t653 + t686 * t324 * t661 / 0.2D1) * t667 + 0.1
     #80D3 * t156 * pi * (t654 - t684 * t661) * t667 - t165 * pi * t673)
     # * t226 * t172 / 0.720D3
      t707 = FJET(XB1, XB2, s, 0.0D0, -t643, t318, t646, -t652, t706)
      t709 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t313)
      t711 = FJET(XB1, XB2, s, t318, t646, 0.0D0, -t643, -t652, t706)
      t713 = t314 * t313 + t316 * t313 + t453 * t452 + t455 * t452 + t45
     #7 * t313 + t564 * t563 + t635 * t634 + t637 * t563 + t639 * t634 +
     # t707 * t706 + t709 * t313 + t711 * t706
      t714 = FJET(XB1, XB2, s, t318, -t320, 0.0D0, 0.0D0, 0.0D0, t452)
      t716 = FJET(XB1, XB2, s, t460, 0.0D0, -t463, 0.0D0, 0.0D0, t563)
      t718 = FJET(XB1, XB2, s, t577, 0.0D0, -t582, 0.0D0, 0.0D0, t634)
      t720 = FJET(XB1, XB2, s, t646, t318, -t643, 0.0D0, -t652, t706)
      t723 = t318 * t566 * t575
      t724 = t566 * x1
      t725 = t566 * t323
      t726 = t461 * t29
      t728 = Sqrt(t352 * t726)
      t729 = t35 * t728
      t730 = 0.2D1 * t729
      t735 = t320 * x2 * (t348 - t349 + t566 - t724 + t725 - 0.1D1 + t73
     #0) * t325 * t575
      t739 = t29 * s * t1 * x1 * t575
      t741 = 0.2D1 * t729 * x2
      t742 = 0.1D1 - x1 + t323 - x2 + t664 - t665 - x3 + t348 - t349 + t
     #566 - t724 + t725 + t207 + t741
      t745 = t320 * t742 * t325 * t575
      t746 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t735, -t
     #745, t723, -t739, -t652)
      t752 = log(0.4D1 * t215 * t416 * t726 * t585)
      t754 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t735, -t
     #745, t723, -t739, -t652)
      t763 = 0.1D1 - x2 - t730 + 0.2D1 * t135 + t472 - t566 + t207 + t32
     #3 - 0.3D1 * t348 - t596 + t664 - x1 + 0.3D1 * t349 - t665 + 0.3D1 
     #* t724 + t741 - 0.2D1 * t135 * x2
      t797 = -0.2D1 * t207 * x1 + t208 + 0.2D1 * t729 * x1 - 0.4D1 * t13
     #5 * z + 0.2D1 * t135 * t24 - 0.4D1 * t725 - 0.2D1 * t729 * t472 - 
     #0.2D1 * t729 * t664 + x3 * t24 * t664 + 0.4D1 * t135 * t472 - 0.2D
     #1 * t135 * t24 * x2 + 0.3D1 * t207 * t323 - 0.2D1 * t729 * t323 - 
     #t207 * t24 * x1 - 0.2D1 * t207 * t134 * z + t207 * t134 * t24 + x3
     # + 0.2D1 * t729 * t665
      t799 = 0.1D1 / (t763 + t797)
      t807 = -0.90D2 * t133 * (t324 * t746 - t752 * t324 * t754) * t799 
     #+ 0.180D3 * t671 * t324 * t754 * t799
      t810 = t807 * t85 * t227 / 0.720D3
      t811 = FJET(XB1, XB2, s, t723, t735, -t739, -t745, -t652, -t810)
      t814 = t85 * t226 * t172
      t817 = FJET(XB1, XB2, s, t735, t723, -t745, -t739, -t652, -t810)
      t821 = FJET(XB1, XB2, s, -t320, t318, 0.0D0, 0.0D0, 0.0D0, t452)
      t823 = FJET(XB1, XB2, s, -t463, 0.0D0, t460, 0.0D0, 0.0D0, t563)
      t825 = FJET(XB1, XB2, s, -t582, 0.0D0, t577, 0.0D0, 0.0D0, t634)
      t827 = FJET(XB1, XB2, s, -t643, 0.0D0, t646, t318, -t652, t706)
      t829 = FJET(XB1, XB2, s, -t739, -t745, t723, t735, -t652, -t810)
      t833 = FJET(XB1, XB2, s, -t745, -t739, t735, t723, -t652, -t810)
      t837 = t714 * t452 + t716 * t563 + t718 * t634 + t720 * t706 - t81
     #1 * t807 * t814 / 0.720D3 - t817 * t807 * t814 / 0.720D3 + t821 * 
     #t452 + t823 * t563 + t825 * t634 + t827 * t706 - t829 * t807 * t81
     #4 / 0.720D3 - t833 * t807 * t814 / 0.720D3
      rrgg2qqbarhhardt6s1e1 = t713 + t837

      end function



      doubleprecision function rrgg2qqbarhhardt6s1e0
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t3 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = t16 ** 2
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t23 = log(-0.4D1 * t11 * t13 * t19)
      t24 = t23 ** 2
      t25 = cos(t8)
      t27 = Sqrt(-x3 * t18)
      t31 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t25 * t27)
      t40 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t50 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t52 = lh ** 2
      t54 = pi ** 2
      t56 = 0.180D3 * t52 - 0.30D2 * t54
      t64 = 0.1D1 / x3
      t67 = t13 * t10
      t69 = log(0.4D1 * t67)
      t71 = t69 ** 2
      t86 = rrgg2qqbarhhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t100 = t6 * pi
      t101 = x2 ** 2
      t102 = t101 * x3
      t103 = t67 * t19
      t106 = log(-0.4D1 * t102 * t103)
      t112 = log(0.4D1 * t102 * t67)
      t117 = lh * t6
      t119 = -t3 * t31 - t3
      t125 = 0.1D1 / x2
      t128 = t101 * t10
      t131 = log(0.4D1 * t128 * t13)
      t133 = t131 ** 2
      t144 = t56 * t6
      t145 = pi * t3
      t146 = t144 * t145
      t150 = x1 ** 2
      t151 = x3 * t150
      t154 = log(-0.4D1 * t151 * t103)
      t160 = log(0.4D1 * t151 * t67)
      t165 = -t119
      t171 = 0.1D1 / x1
      t176 = t64 * t125 * t171
      t179 = t101 * t150
      t182 = log(0.4D1 * t179 * t67)
      t193 = t150 * t10
      t196 = log(0.4D1 * t193 * t13)
      t198 = t196 ** 2
      t212 = (0.90D2 * t3 * t6 * pi * (-t17 / 0.2D1 - t24 * t31 / 0.2D1)
     # + (-0.180D3 * t3 * lh + 0.90D2 * t40) * t6 * pi * (t16 + t23 * t3
     #1) + (-0.180D3 * t40 * lh + 0.90D2 * t50 + t3 * t56) * t6 * pi * (
     #-0.1D1 - t31)) * t64 / 0.2880D4 - (-0.180D3 * (t50 - t69 * t40 + t
     #71 * t3 / 0.2D1) * lh + t3 * (0.60D2 * lh * t54 - 0.240D3 * zeta3 
     #- 0.120D3 * t52 * lh) + 0.45D2 * t71 * t40 + 0.90D2 * t86 - 0.15D2
     # * t71 * t69 * t3 - 0.90D2 * t69 * t50 + (t40 - t69 * t3) * t56) *
     # t6 * pi / 0.2880D4 + (0.90D2 * t100 * (-(t40 - t106 * t3) * t31 -
     # t40 + t112 * t3) - 0.180D3 * t117 * pi * t119) * t64 * t125 / 0.1
     #440D4 - (0.90D2 * t100 * (t50 - t131 * t40 + t133 * t3 / 0.2D1) - 
     #0.180D3 * t117 * pi * (t40 - t131 * t3) + t146) * t125 / 0.1440D4 
     #- (0.90D2 * t100 * ((t40 - t154 * t3) * t31 + t40 - t160 * t3) - 0
     #.180D3 * t117 * pi * t165) * t64 * t171 / 0.1440D4 - t100 * t165 *
     # t176 / 0.8D1 - (0.90D2 * t100 * (t40 - t182 * t3) - 0.180D3 * t11
     #7 * t145) * t125 * t171 / 0.720D3 + (0.90D2 * t100 * (-t50 + t196 
     #* t40 - t198 * t3 / 0.2D1) - 0.180D3 * t117 * pi * (-t40 + t196 * 
     #t3) - t146) * t171 / 0.1440D4
      t213 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t212)
      t215 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t212)
      t217 = t2 * x1
      t218 = -0.1D1 + x1
      t219 = t2 * t218
      t220 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t219, 0.0D0, t217, 0.0D0)
      t221 = t151 * t10
      t222 = x1 * z
      t223 = 0.1D1 - x1 + t222
      t224 = 0.1D1 / t223
      t225 = t13 * t224
      t226 = t218 ** 2
      t227 = t225 * t226
      t230 = log(0.4D1 * t221 * t227)
      t231 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t219, 0.0D0, t217, 0.0D0)
      t237 = log(-0.4D1 * t221 * t225 * t226 * t19)
      t240 = x3 * x1
      t241 = t240 * z
      t244 = x3 * t223
      t246 = Sqrt(-t244 * t18)
      t250 = 0.1D1 / (-0.2D1 * t241 + 0.2D1 * t240 - 0.1D1 - x3 + 0.2D1 
     #* t25 * t246)
      t256 = -t231 * t250 - t231
      t267 = t179 * t10
      t270 = log(0.4D1 * t267 * t227)
      t275 = pi * t231
      t282 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t219, 0.0D0, t217, 0.0D0)
      t285 = log(0.4D1 * t193 * t227)
      t287 = t285 ** 2
      t302 = -(0.90D2 * t100 * (-t220 + t230 * t231 - (t220 - t237 * t23
     #1) * t250) - 0.180D3 * t117 * pi * t256) * t64 * t171 / 0.1440D4 -
     # t100 * t256 * t176 / 0.8D1 - (0.90D2 * t100 * (-t220 + t270 * t23
     #1) + 0.180D3 * t117 * t275) * t125 * t171 / 0.720D3 + (0.90D2 * t1
     #00 * (t282 - t285 * t220 + t287 * t231 / 0.2D1) - 0.180D3 * t117 *
     # pi * (t220 - t285 * t231) + t144 * t275) * t171 / 0.1440D4
      t303 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t217, -t219, 0.0D0, t302)
      t305 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t219, t217, 0.0D0, t302)
      t307 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t212)
      t310 = x2 * t1 * s
      t311 = -0.1D1 + x2
      t313 = t311 * t1 * s
      t314 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t310, -t
     #313, 0.0D0, 0.0D0, 0.0D0)
      t315 = t67 * t311
      t318 = log(-0.4D1 * t102 * t315)
      t319 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t310, -t
     #313, 0.0D0, 0.0D0, 0.0D0)
      t322 = x2 * z
      t324 = 0.1D1 / (0.1D1 + t322 - x2)
      t329 = pi * t319 * t324
      t331 = 0.180D3 * t117 * t329
      t336 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, t310, -t
     #313, 0.0D0, 0.0D0, 0.0D0)
      t337 = t13 * t311
      t340 = log(-0.4D1 * t128 * t337)
      t342 = t340 ** 2
      t361 = t125 * t171
      t367 = log(-0.4D1 * t179 * t315)
      t377 = (0.90D2 * t100 * (t314 - t318 * t319) * t324 - t331) * t64 
     #* t125 / 0.1440D4 - (-0.90D2 * t100 * (t336 - t340 * t314 + t342 *
     # t319 / 0.2D1) * t324 + 0.180D3 * t117 * pi * (t314 - t340 * t319)
     # * t324 - t144 * t329) * t125 / 0.1440D4 + t100 * t319 * t324 * t6
     #4 * t361 / 0.8D1 - (-0.90D2 * t100 * (t314 - t367 * t319) * t324 +
     # t331) * t125 * t171 / 0.720D3
      t378 = FJET(XB1, XB2, s, 0.0D0, t310, 0.0D0, -t313, 0.0D0, t377)
      t380 = x2 * x3
      t383 = Sqrt(x3 * t311 * t18)
      t384 = t25 * t383
      t386 = 0.2D1 * t384 * x2
      t388 = 0.1D1 - x3 + t380
      t389 = 0.1D1 / t388
      t391 = t2 * (0.1D1 - x3 - x2 + t380 + t102 + t386) * t389
      t392 = 0.2D1 * t384
      t396 = t2 * x2 * (-0.1D1 + t380 + t392) * t389
      t397 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t396, t
     #391, 0.0D0, 0.0D0, 0.0D0)
      t399 = t388 ** 2
      t405 = log(0.4D1 * t102 * t10 * t337 * t18 / t399)
      t406 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t396, t
     #391, 0.0D0, 0.0D0, 0.0D0)
      t411 = t102 * z
      t413 = 0.1D1 / (-0.1D1 + 0.2D1 * t384 * t322 + t411 - t102 + t380 
     #- t322 + x2 - x3 - t386 + t392)
      t430 = (0.90D2 * t100 * (t397 - t405 * t406) * t413 - 0.180D3 * t1
     #17 * pi * t406 * t413) * t64 * t125 / 0.1440D4 + t100 * t406 * t41
     #3 * t64 * t361 / 0.8D1
      t431 = FJET(XB1, XB2, s, 0.0D0, t391, 0.0D0, -t396, 0.0D0, t430)
      t433 = FJET(XB1, XB2, s, 0.0D0, -t313, 0.0D0, t310, 0.0D0, t377)
      t435 = FJET(XB1, XB2, s, 0.0D0, -t396, 0.0D0, t391, 0.0D0, t430)
      t439 = t2 * t218 * x2 * t224
      t442 = t311 * s * t1 * t218
      t443 = t1 ** 2
      t448 = s * t443 * x2 * x1 * t218 * t224
      t449 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t439, t
     #442, 0.0D0, t217, -t448)
      t450 = t223 * t449
      t452 = x2 * x1
      t453 = t452 * z
      t455 = 0.1D1 / (-0.1D1 + t453 - t322 - t452 + x1 + x2 - t222)
      t460 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t439, t
     #442, 0.0D0, t217, -t448)
      t466 = log(-0.4D1 * t267 * t225 * t226 * t311)
      t481 = t100 * t450 * t455 * t64 * t361 / 0.8D1 - (-0.90D2 * t100 *
     # (t223 * t460 - t466 * t223 * t449) * t455 + 0.180D3 * t117 * pi *
     # t450 * t455) * t125 * t171 / 0.720D3
      t482 = FJET(XB1, XB2, s, 0.0D0, -t439, t217, t442, -t448, t481)
      t484 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t212)
      t486 = FJET(XB1, XB2, s, t217, t442, 0.0D0, -t439, -t448, t481)
      t488 = t213 * t212 + t215 * t212 + t303 * t302 + t305 * t302 + t30
     #7 * t212 + t378 * t377 + t431 * t430 + t433 * t377 + t435 * t430 +
     # t482 * t481 + t484 * t212 + t486 * t481
      t489 = FJET(XB1, XB2, s, t217, -t219, 0.0D0, 0.0D0, 0.0D0, t302)
      t491 = FJET(XB1, XB2, s, t310, 0.0D0, -t313, 0.0D0, 0.0D0, t377)
      t493 = FJET(XB1, XB2, s, t391, 0.0D0, -t396, 0.0D0, 0.0D0, t430)
      t495 = FJET(XB1, XB2, s, t442, t217, -t439, 0.0D0, -t448, t481)
      t498 = t217 * t380 * t389
      t499 = t380 * x1
      t500 = t380 * t222
      t503 = Sqrt(t244 * t311 * t18)
      t504 = t25 * t503
      t505 = 0.2D1 * t504
      t510 = t219 * x2 * (t240 - t241 + t380 - t499 + t500 - 0.1D1 + t50
     #5) * t224 * t389
      t514 = t18 * s * t1 * x1 * t389
      t516 = 0.2D1 * t504 * x2
      t517 = 0.1D1 - x1 + t222 - x2 + t452 - t453 - x3 + t240 - t241 + t
     #380 - t499 + t500 + t102 + t516
      t520 = t219 * t517 * t224 * t389
      t521 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t510, -t
     #520, t498, -t514, -t448)
      t537 = 0.1D1 + t516 - 0.2D1 * t151 * x2 - 0.2D1 * t102 * x1 + t102
     # * t150 + 0.2D1 * t504 * x1 - 0.4D1 * t151 * z + 0.2D1 * t151 * t1
     #2 + 0.3D1 * t499 + t222 - 0.3D1 * t240 - t453 - t411 + x3 - t380 +
     # t102 - x1
      t565 = -t505 + 0.2D1 * t151 + 0.3D1 * t241 + t322 + 0.2D1 * t504 *
     # t453 - x2 - 0.4D1 * t500 - 0.2D1 * t504 * t322 - 0.2D1 * t504 * t
     #452 + x3 * t12 * t452 + 0.4D1 * t151 * t322 - 0.2D1 * t151 * t12 *
     # x2 + 0.3D1 * t102 * t222 - 0.2D1 * t504 * t222 - t102 * t12 * x1 
     #- 0.2D1 * t102 * t150 * z + t102 * t150 * t12 + t452
      t567 = 0.1D1 / (t537 + t565)
      t571 = t100 * t223 * t521 * t567 * t64 * t361 / 0.8D1
      t572 = FJET(XB1, XB2, s, t498, t510, -t514, -t520, -t448, t571)
      t574 = pi * t223
      t577 = t521 * t567 * t176
      t580 = FJET(XB1, XB2, s, t510, t498, -t520, -t514, -t448, t571)
      t585 = FJET(XB1, XB2, s, -t219, t217, 0.0D0, 0.0D0, 0.0D0, t302)
      t587 = FJET(XB1, XB2, s, -t313, 0.0D0, t310, 0.0D0, 0.0D0, t377)
      t589 = FJET(XB1, XB2, s, -t396, 0.0D0, t391, 0.0D0, 0.0D0, t430)
      t591 = FJET(XB1, XB2, s, -t439, 0.0D0, t442, t217, -t448, t481)
      t593 = FJET(XB1, XB2, s, -t514, -t520, t498, t510, -t448, t571)
      t598 = FJET(XB1, XB2, s, -t520, -t514, t510, t498, -t448, t571)
      t603 = t489 * t302 + t491 * t377 + t493 * t430 + t495 * t481 + t57
     #2 * t6 * t574 * t577 / 0.8D1 + t580 * t6 * t574 * t577 / 0.8D1 + t
     #585 * t302 + t587 * t377 + t589 * t430 + t591 * t481 + t593 * t6 *
     # t574 * t577 / 0.8D1 + t598 * t6 * t574 * t577 / 0.8D1
      rrgg2qqbarhhardt6s1e0 = t488 + t603

      end function



      doubleprecision function rrgg2qqbarhhardt6s1em1
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t3 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t16 = log(0.4D1 * t11 * t13)
      t17 = -0.1D1 + x3
      t22 = log(-0.4D1 * t11 * t13 / t17)
      t23 = cos(t8)
      t25 = Sqrt(-x3 * t17)
      t29 = 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t23 * t25)
      t37 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t45 = 0.1D1 / x3
      t50 = log(0.4D1 * t13 * t10)
      t55 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t59 = t50 ** 2
      t62 = lh ** 2
      t64 = pi ** 2
      t72 = t6 * pi
      t74 = -t3 * t29 - t3
      t76 = 0.1D1 / x2
      t80 = x2 ** 2
      t81 = t80 * t10
      t84 = log(0.4D1 * t81 * t13)
      t89 = lh * t6
      t92 = 0.180D3 * t89 * pi * t3
      t97 = 0.1D1 / x1
      t101 = x1 ** 2
      t102 = t101 * t10
      t105 = log(0.4D1 * t102 * t13)
      t118 = (0.90D2 * t7 * pi * (t16 + t22 * t29) + (-0.180D3 * t3 * lh
     # + 0.90D2 * t37) * t6 * pi * (-0.1D1 - t29)) * t45 / 0.2880D4 - (-
     #0.180D3 * (t37 - t50 * t3) * lh + 0.90D2 * t55 - 0.90D2 * t50 * t3
     #7 + 0.45D2 * t59 * t3 + t3 * (0.180D3 * t62 - 0.30D2 * t64)) * t6 
     #* pi / 0.2880D4 + t72 * t74 * t45 * t76 / 0.16D2 - (0.90D2 * t72 *
     # (t37 - t84 * t3) - t92) * t76 / 0.1440D4 - t7 * pi * t76 * t97 / 
     #0.8D1 + (0.90D2 * t72 * (-t37 + t105 * t3) + t92) * t97 / 0.1440D4
     # + t72 * t74 * t45 * t97 / 0.16D2
      t119 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t118)
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t118)
      t123 = t2 * x1
      t124 = -0.1D1 + x1
      t125 = t2 * t124
      t126 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t125, 0.0D0, t123, 0.0D0)
      t131 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t125, 0.0D0, t123, 0.0D0)
      t132 = x1 * z
      t133 = 0.1D1 - x1 + t132
      t134 = 0.1D1 / t133
      t136 = t124 ** 2
      t140 = log(0.4D1 * t102 * t13 * t134 * t136)
      t151 = x3 * x1
      t157 = Sqrt(-x3 * t133 * t17)
      t168 = t72 * t126 * t76 * t97 / 0.8D1 + (0.90D2 * t72 * (t131 - t1
     #40 * t126) - 0.180D3 * t89 * pi * t126) * t97 / 0.1440D4 - t72 * (
     #-t126 / (-0.2D1 * t151 * z + 0.2D1 * t151 - 0.1D1 - x3 + 0.2D1 * t
     #23 * t157) - t126) * t45 * t97 / 0.16D2
      t169 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t123, -t125, 0.0D0, t168)
      t171 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t125, t123, 0.0D0, t168)
      t173 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t118)
      t176 = x2 * t1 * s
      t177 = -0.1D1 + x2
      t179 = t177 * t1 * s
      t180 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t176, -t
     #179, 0.0D0, 0.0D0, 0.0D0)
      t181 = t72 * t180
      t182 = x2 * z
      t184 = 0.1D1 / (0.1D1 + t182 - x2)
      t189 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t176, -t
     #179, 0.0D0, 0.0D0, 0.0D0)
      t193 = log(-0.4D1 * t81 * t13 * t177)
      t210 = t181 * t184 * t45 * t76 / 0.16D2 - (-0.90D2 * t72 * (t189 -
     # t193 * t180) * t184 + 0.180D3 * t89 * pi * t180 * t184) * t76 / 0
     #.1440D4 + t181 * t184 * t76 * t97 / 0.8D1
      t211 = FJET(XB1, XB2, s, 0.0D0, t176, 0.0D0, -t179, 0.0D0, t210)
      t213 = x2 * x3
      t214 = t80 * x3
      t217 = Sqrt(x3 * t177 * t17)
      t218 = t23 * t217
      t220 = 0.2D1 * t218 * x2
      t223 = 0.1D1 / (0.1D1 - x3 + t213)
      t225 = t2 * (0.1D1 - x3 - x2 + t213 + t214 + t220) * t223
      t226 = 0.2D1 * t218
      t230 = t2 * x2 * (-0.1D1 + t213 + t226) * t223
      t231 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t230, t
     #225, 0.0D0, 0.0D0, 0.0D0)
      t237 = 0.1D1 / (-0.1D1 + 0.2D1 * t218 * t182 + t214 * z - t214 + t
     #213 - t182 + x2 - x3 - t220 + t226)
      t241 = t72 * t231 * t237 * t45 * t76 / 0.16D2
      t242 = FJET(XB1, XB2, s, 0.0D0, t225, 0.0D0, -t230, 0.0D0, t241)
      t247 = t231 * t237 * t45 * t76
      t250 = FJET(XB1, XB2, s, 0.0D0, -t179, 0.0D0, t176, 0.0D0, t210)
      t252 = FJET(XB1, XB2, s, 0.0D0, -t230, 0.0D0, t225, 0.0D0, t241)
      t259 = t2 * t124 * x2 * t134
      t262 = t177 * s * t1 * t124
      t263 = t1 ** 2
      t268 = s * t263 * x2 * x1 * t124 * t134
      t270 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t259, t
     #262, 0.0D0, t123, -t268)
      t271 = x2 * x1
      t277 = t270 / (-0.1D1 + t271 * z - t182 - t271 + x1 + x2 - t132) *
     # t76 * t97
      t279 = t72 * t133 * t277 / 0.8D1
      t280 = FJET(XB1, XB2, s, 0.0D0, -t259, t123, t262, -t268, t279)
      t282 = pi * t133
      t286 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t118)
      t288 = FJET(XB1, XB2, s, t123, t262, 0.0D0, -t259, -t268, t279)
      t293 = FJET(XB1, XB2, s, t123, -t125, 0.0D0, 0.0D0, 0.0D0, t168)
      t295 = FJET(XB1, XB2, s, t176, 0.0D0, -t179, 0.0D0, 0.0D0, t210)
      t297 = FJET(XB1, XB2, s, t225, 0.0D0, -t230, 0.0D0, 0.0D0, t241)
      t302 = FJET(XB1, XB2, s, t262, t123, -t259, 0.0D0, -t268, t279)
      t307 = FJET(XB1, XB2, s, -t125, t123, 0.0D0, 0.0D0, 0.0D0, t168)
      t309 = FJET(XB1, XB2, s, -t179, 0.0D0, t176, 0.0D0, 0.0D0, t210)
      t311 = FJET(XB1, XB2, s, -t259, 0.0D0, t262, t123, -t268, t279)
      t316 = FJET(XB1, XB2, s, -t230, 0.0D0, t225, 0.0D0, 0.0D0, t241)
      rrgg2qqbarhhardt6s1em1 = t119 * t118 + t121 * t118 + t169 * t168 +
     # t171 * t168 + t173 * t118 + t211 * t210 + t242 * t6 * pi * t247 /
     # 0.16D2 + t250 * t210 + t252 * t6 * pi * t247 / 0.16D2 + t280 * t6
     # * t282 * t277 / 0.8D1 + t286 * t118 + t288 * t6 * t282 * t277 / 0
     #.8D1 + t293 * t168 + t295 * t210 + t297 * t6 * pi * t247 / 0.16D2 
     #+ t302 * t6 * t282 * t277 / 0.8D1 + t307 * t168 + t309 * t210 + t3
     #11 * t6 * t282 * t277 / 0.8D1 + t316 * t6 * pi * t247 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt6s1em2
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t3 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t7 = t3 * t6
      t8 = x4 * pi
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t23 = 0.1D1 / x2
      t27 = 0.1D1 / x1
      t33 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t35 = z ** 2
      t37 = Sin(t8)
      t38 = t37 ** 2
      t41 = log(0.4D1 / t35 * t38)
      t48 = t7 * pi * (-0.1D1 - 0.1D1 / (-0.1D1 - x3 + 0.2D1 * t9 * t12)
     #) / x3 / 0.32D2 - t7 * pi * t23 / 0.16D2 - t7 * pi * t27 / 0.16D2 
     #- (-0.180D3 * t3 * lh + 0.90D2 * t33 - 0.90D2 * t41 * t3) * t6 * p
     #i / 0.2880D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t48)
      t53 = t2 * x1
      t55 = t2 * (-0.1D1 + x1)
      t56 = t6 * pi
      t57 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #55, 0.0D0, t53, 0.0D0)
      t60 = t56 * t57 * t27 / 0.16D2
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t53, -t55, 0.0D0, t60)
      t64 = pi * t57 * t27
      t67 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t55, t53, 0.0D0, t60)
      t71 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t48)
      t74 = x2 * t1 * s
      t77 = (-0.1D1 + x2) * t1 * s
      t78 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t74, -t77
     #, 0.0D0, 0.0D0, 0.0D0)
      t83 = t78 / (0.1D1 + x2 * z - x2) * t23
      t85 = t56 * t83 / 0.16D2
      t86 = FJET(XB1, XB2, s, 0.0D0, t74, 0.0D0, -t77, 0.0D0, t85)
      t91 = FJET(XB1, XB2, s, 0.0D0, -t77, 0.0D0, t74, 0.0D0, t85)
      t96 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t98 = FJET(XB1, XB2, s, t53, -t55, 0.0D0, 0.0D0, 0.0D0, t60)
      t102 = FJET(XB1, XB2, s, t74, 0.0D0, -t77, 0.0D0, 0.0D0, t85)
      t107 = FJET(XB1, XB2, s, -t77, 0.0D0, t74, 0.0D0, 0.0D0, t85)
      t112 = FJET(XB1, XB2, s, -t55, t53, 0.0D0, 0.0D0, 0.0D0, t60)
      rrgg2qqbarhhardt6s1em2 = t49 * t48 + t51 * t48 + t61 * t6 * t64 / 
     #0.16D2 + t67 * t6 * t64 / 0.16D2 + t71 * t48 + t86 * t6 * pi * t83
     # / 0.16D2 + t91 * t6 * pi * t83 / 0.16D2 + t96 * t48 + t98 * t6 * 
     #t64 / 0.16D2 + t102 * t6 * pi * t83 / 0.16D2 + t107 * t6 * pi * t8
     #3 / 0.16D2 + t112 * t6 * t64 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt6s1em3
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t4 = s ** 2
      t6 = 0.1D1 / t4 / s
      t9 = t3 * t6 * pi / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = t6 * pi
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2qqbarhhardt6s1em3 = -t10 * t3 * t12 / 0.32D2 - t14 * t3 * t12
     # / 0.32D2 - t17 * t3 * t12 / 0.32D2 - t20 * t3 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarhhardt6s1em4
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt6s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhhardt6s2e1
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = t12 * t15
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t22 = log(-0.4D1 * t9 * t19)
      t23 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t25 = t22 ** 2
      t26 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t30 = x3 * z
      t31 = 0.2D1 * t30
      t32 = cos(t10)
      t34 = Sqrt(-t30 * t17)
      t38 = 0.1D1 / (-0.1D1 - t31 + 0.2D1 * t32 * t34 + x3)
      t42 = log(0.4D1 * t9 * t16)
      t44 = t42 ** 2
      t50 = lh * t5
      t59 = lh ** 2
      t60 = 0.180D3 * t59
      t61 = pi ** 2
      t62 = 0.30D2 * t61
      t63 = t60 - t62
      t64 = t63 * t5
      t70 = 0.1D1 / x3
      t72 = 0.1D1 / x1
      t75 = t8 * t12
      t76 = t75 * t15
      t78 = log(0.4D1 * t76)
      t83 = t78 ** 2
      t86 = rrgg2qqbarhhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t95 = 0.60D2 * lh * t61
      t96 = 0.240D3 * zeta3
      t98 = 0.120D3 * t59 * lh
      t99 = t95 - t96 - t98
      t100 = t99 * t5
      t101 = pi * t26
      t113 = x2 ** 2
      t114 = t113 * x3
      t117 = log(0.4D1 * t114 * t76)
      t119 = t114 * t8
      t122 = log(-0.4D1 * t119 * t19)
      t126 = -0.1D1 + x2
      t127 = t16 * t126
      t130 = log(-0.4D1 * t119 * t127)
      t135 = t101 * t38
      t140 = 0.1D1 / x2
      t141 = t140 * t72
      t144 = t113 * t8
      t147 = log(0.4D1 * t144 * t16)
      t149 = t147 ** 2
      t154 = log(-0.4D1 * t144 * t127)
      t156 = t154 ** 2
      t178 = x3 * t15
      t182 = log(-0.4D1 * t178 * t12 * t18)
      t186 = log(0.4D1 * t178 * t12)
      t190 = t186 ** 2
      t192 = t182 ** 2
      t225 = log(-0.4D1 * t114 * t127)
      t227 = t225 ** 2
      t232 = log(-0.4D1 * t114 * t19)
      t234 = t232 ** 2
      t241 = log(0.4D1 * t114 * t16)
      t243 = t241 ** 2
      t268 = t15 * t113
      t269 = t12 * t126
      t272 = log(-0.4D1 * t268 * t269)
      t273 = t272 ** 2
      t276 = log(0.4D1 * t268 * t12)
      t277 = t276 ** 2
      t290 = pi * t23
      t300 = rrgg2qqbarhhard61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, 0.0D0, t2, 0.0D0)
      t304 = log(0.4D1 * t16)
      t305 = t304 ** 2
      t308 = t305 * t304
      t335 = t61 ** 2
      t336 = t59 ** 2
      t342 = t305 ** 2
      t348 = -(0.90D2 * t6 * ((t7 - t22 * t23 + t25 * t26 / 0.2D1) * t38
     # + t7 - t42 * t23 + t44 * t26 / 0.2D1) - 0.180D3 * t50 * pi * ((t2
     #3 - t22 * t26) * t38 + t23 - t42 * t26) + t64 * pi * (t26 + t26 * 
     #t38)) * t70 * t72 / 0.1440D4 + (t64 * pi * (-t23 + t78 * t26) + 0.
     #90D2 * t6 * (-t83 * t23 / 0.2D1 - t86 + t83 * t78 * t26 / 0.6D1 + 
     #t78 * t7) - t100 * t101 - 0.180D3 * t50 * pi * (-t7 + t78 * t23 - 
     #t83 * t26 / 0.2D1)) * t72 / 0.1440D4 + (0.90D2 * t6 * (t117 * t26 
     #- (t23 - t122 * t26) * t38 - t130 * t26) + 0.180D3 * t50 * t135) *
     # t70 * t141 / 0.720D3 + (0.90D2 * t6 * (t147 * t23 - t149 * t26 / 
     #0.2D1 - t154 * t23 + t156 * t26 / 0.2D1) - 0.180D3 * t50 * pi * (t
     #147 * t26 - t154 * t26)) * t140 * t72 / 0.720D3 - ((-0.180D3 * t23
     # * lh + 0.90D2 * t7 + t26 * t63) * t5 * pi * (-t182 * t38 - t186) 
     #+ 0.90D2 * t6 * t26 * (-t190 * t186 / 0.6D1 - t192 * t182 * t38 / 
     #0.6D1) + (-0.180D3 * t7 * lh + t26 * t99 + 0.90D2 * t86 + t23 * t6
     #3) * t5 * pi * (t38 + 0.1D1) + (-0.180D3 * t26 * lh + 0.90D2 * t23
     #) * t5 * pi * (t192 * t38 / 0.2D1 + t190 / 0.2D1)) * t70 / 0.2880D
     #4 + (0.90D2 * t6 * (-t225 * t23 + t227 * t26 / 0.2D1 - (t7 - t232 
     #* t23 + t234 * t26 / 0.2D1) * t38 + t241 * t23 - t243 * t26 / 0.2D
     #1) - 0.180D3 * t50 * pi * (-t225 * t26 - (t23 - t232 * t26) * t38 
     #+ t241 * t26) - t64 * t135) * t70 * t140 / 0.1440D4 + ((0.90D2 * t
     #6 * t23 - 0.180D3 * t50 * t101) * (t273 / 0.2D1 - t277 / 0.2D1) + 
     #0.90D2 * t6 * t26 * (t277 * t276 / 0.6D1 - t273 * t272 / 0.6D1) + 
     #(0.90D2 * t6 * t7 - 0.180D3 * t50 * t290 + t64 * t101) * (-t272 + 
     #t276)) * t140 / 0.1440D4 - t6 * t300 / 0.32D2 - (-0.90D2 * t305 * 
     #lh + t95 - t96 - t98 - 0.15D2 * t308 - t304 * t63) * t5 * t290 / 0
     #.2880D4 - (0.180D3 * t304 * lh + 0.45D2 * t305 + t60 - t62) * t5 *
     # pi * t7 / 0.2880D4 - (-0.180D3 * lh - 0.90D2 * t304) * t5 * pi * 
     #t86 / 0.2880D4 - (0.30D2 * t308 * lh + t305 * t63 / 0.2D1 - t304 *
     # t99 + t335 + 0.60D2 * t336 + 0.480D3 * lh * zeta3 - 0.60D2 * t59 
     #* t61 + 0.15D2 / 0.4D1 * t342) * t5 * t101 / 0.2880D4
      t349 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t348)
      t351 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t348)
      t353 = t2 * x1
      t354 = -0.1D1 + x1
      t355 = t2 * t354
      t356 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #353, 0.0D0, -t355, 0.0D0)
      t357 = t9 * t12
      t358 = 0.1D1 / t13
      t359 = x1 * z
      t360 = -z - x1 + t359
      t361 = 0.1D1 / t360
      t362 = t358 * t361
      t363 = t354 ** 2
      t365 = t362 * t363 * t18
      t368 = log(0.4D1 * t357 * t365)
      t369 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #353, 0.0D0, -t355, 0.0D0)
      t371 = t368 ** 2
      t372 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #353, 0.0D0, -t355, 0.0D0)
      t376 = x3 * x1
      t377 = t376 * z
      t380 = x3 * t360
      t382 = Sqrt(t380 * t17)
      t386 = 0.1D1 / (0.2D1 * t377 - 0.2D1 * t376 - t31 + x3 - 0.1D1 + 0
     #.2D1 * t32 * t382)
      t388 = t362 * t363
      t391 = log(-0.4D1 * t357 * t388)
      t393 = t391 ** 2
      t408 = -t372 * t386 - t372
      t417 = log(-0.4D1 * t75 * t388)
      t422 = t417 ** 2
      t425 = rrgg2qqbarhhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #353, 0.0D0, -t355, 0.0D0)
      t433 = pi * t372
      t445 = t114 * t75
      t448 = log(0.4D1 * t445 * t365)
      t453 = t361 * t363
      t457 = log(-0.4D1 * t119 * t12 * t358 * t453)
      t470 = t144 * t12
      t473 = log(-0.4D1 * t470 * t388)
      t475 = t473 ** 2
      t491 = -(0.90D2 * t6 * (-(t356 - t368 * t369 + t371 * t372 / 0.2D1
     #) * t386 - t356 + t391 * t369 - t393 * t372 / 0.2D1) - 0.180D3 * t
     #50 * pi * (-(t369 - t368 * t372) * t386 - t369 + t391 * t372) + t6
     #4 * pi * t408) * t70 * t72 / 0.1440D4 + (t64 * pi * (t369 - t417 *
     # t372) + 0.90D2 * t6 * (t422 * t369 / 0.2D1 + t425 - t422 * t417 *
     # t372 / 0.6D1 - t417 * t356) + t100 * t433 - 0.180D3 * t50 * pi * 
     #(t356 - t417 * t369 + t422 * t372 / 0.2D1)) * t72 / 0.1440D4 + (0.
     #90D2 * t6 * ((t369 - t448 * t372) * t386 + t369 - t457 * t372) + 0
     #.180D3 * t50 * pi * t408) * t70 * t141 / 0.720D3 + (0.90D2 * t6 * 
     #(t356 - t473 * t369 + t475 * t372 / 0.2D1) - 0.180D3 * t50 * pi * 
     #(t369 - t473 * t372) + t64 * t433) * t140 * t72 / 0.720D3
      t492 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t353, -t355, 0.0D0, t491)
      t494 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t355, t353, 0.0D0, t491)
      t496 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t348)
      t498 = x2 * x3
      t499 = 0.1D1 - x3 + t498
      t500 = 0.1D1 / t499
      t501 = t498 * t500
      t502 = t2 * t501
      t504 = t2 * t17 * t500
      t505 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t502, -t504, 0.0D0)
      t507 = t499 ** 2
      t508 = 0.1D1 / t507
      t509 = t17 * t508
      t513 = log(0.4D1 * t114 * t15 * t269 * t509)
      t514 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t502, -t504, 0.0D0)
      t516 = t513 ** 2
      t517 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t502, -t504, 0.0D0)
      t521 = t498 * z
      t522 = t126 * t17
      t524 = Sqrt(t30 * t522)
      t528 = 0.1D1 / (t521 - t31 - 0.1D1 + 0.2D1 * t32 * t524 + x3)
      t539 = pi * t517 * t528
      t549 = log(0.4D1 * t445 * t15 * t126 * t509)
      t561 = (0.90D2 * t6 * (t505 - t513 * t514 + t516 * t517 / 0.2D1) *
     # t528 - 0.180D3 * t50 * pi * (t514 - t513 * t517) * t528 + t64 * t
     #539) * t70 * t140 / 0.1440D4 + (0.90D2 * t6 * (t514 - t549 * t517)
     # * t528 - 0.180D3 * t50 * t539) * t70 * t141 / 0.720D3
      t562 = FJET(XB1, XB2, s, 0.0D0, t502, 0.0D0, -t504, 0.0D0, t561)
      t564 = FJET(XB1, XB2, s, 0.0D0, -t504, 0.0D0, t502, 0.0D0, t561)
      t566 = x2 * x1
      t568 = t2 * t566 * t361
      t571 = t126 * s * t1 * x1
      t572 = t1 ** 2
      t577 = s * t572 * x2 * x1 * t354 * t361
      t578 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t568, -
     #t571, 0.0D0, -t355, t577)
      t579 = t360 * t578
      t581 = t362 * t363 * t126
      t584 = log(0.4D1 * t445 * t581)
      t586 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t568, -
     #t571, 0.0D0, -t355, t577)
      t589 = t566 * z
      t591 = 0.1D1 / (z + t589 - t566 + x1 - t359)
      t595 = t50 * pi
      t597 = t360 * t586 * t591
      t603 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, -t568, -
     #t571, 0.0D0, -t355, t577)
      t607 = log(0.4D1 * t470 * t581)
      t608 = t607 * t360
      t610 = t607 ** 2
      t630 = (0.90D2 * t6 * (t579 - t584 * t360 * t586) * t591 - 0.180D3
     # * t595 * t597) * t70 * t141 / 0.720D3 + (0.90D2 * t6 * (t360 * t6
     #03 - t608 * t578 + t610 * t360 * t586 / 0.2D1) * t591 - 0.180D3 * 
     #t50 * pi * (t579 - t608 * t586) * t591 + t64 * pi * t597) * t140 *
     # t72 / 0.720D3
      t631 = FJET(XB1, XB2, s, 0.0D0, -t568, -t355, -t571, t577, t630)
      t633 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t348)
      t635 = FJET(XB1, XB2, s, t353, -t355, 0.0D0, 0.0D0, 0.0D0, t491)
      t637 = FJET(XB1, XB2, s, t502, 0.0D0, -t504, 0.0D0, 0.0D0, t561)
      t642 = t17 * s * t1 * t354 * t500
      t643 = x2 * z
      t644 = t498 * x1
      t645 = t498 * t359
      t647 = Sqrt(-t380 * t522)
      t648 = t32 * t647
      t651 = z + x1 - t359 - t643 - t566 + t589 - t30 - t376 + t377 + t5
     #21 + t644 - t645 + t114 + 0.2D1 * t648 * x2
      t654 = t353 * t651 * t361 * t500
      t655 = t355 * t501
      t661 = t353 * x2 * (-t30 - t376 + t377 + t521 + t644 - t645 - 0.1D
     #1 + x3 + 0.2D1 * t648) * t361 * t500
      t662 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, t661, -t
     #654, -t655, t642, t577)
      t670 = log(-0.4D1 * t114 * t75 * t358 * t453 * t522 * t508)
      t672 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t661, -t
     #654, -t655, t642, t577)
      t676 = x3 * t13
      t698 = 0.4D1 * t645 - 0.3D1 * t676 * t566 - 0.4D1 * t9 * t643 + 0.
     #2D1 * t9 * t13 * x2 - t114 * t359 - 0.2D1 * t648 * t566 - 0.2D1 * 
     #t648 * t359 + t114 * t13 * x1 + 0.2D1 * t114 * t8 * z - t114 * t8 
     #* t13 + 0.2D1 * t648 * t589 - t589 + t359 + t376 + t566
      t715 = t30 - 0.5D1 * t377 - 0.2D1 * t9 - 0.2D1 * t676 + 0.2D1 * t9
     # * x2 + t498 * t13 - t119 + 0.4D1 * t676 * x1 + 0.4D1 * t9 * z - 0
     #.2D1 * t9 * t13 + 0.2D1 * t648 * z + 0.2D1 * t648 * x1 - z - x1 - 
     #t644
      t717 = 0.1D1 / (t698 + t715)
      t725 = 0.90D2 * t6 * (t360 * t662 - t670 * t360 * t672) * t717 - 0
     #.180D3 * t595 * t360 * t672 * t717
      t728 = t725 * t70 * t141 / 0.720D3
      t729 = FJET(XB1, XB2, s, t642, -t654, -t655, t661, t577, t728)
      t732 = t70 * t140 * t72
      t735 = FJET(XB1, XB2, s, t661, -t655, -t654, t642, t577, t728)
      t739 = FJET(XB1, XB2, s, -t355, t353, 0.0D0, 0.0D0, 0.0D0, t491)
      t741 = FJET(XB1, XB2, s, -t355, -t571, 0.0D0, -t568, t577, t630)
      t743 = FJET(XB1, XB2, s, -t571, -t355, -t568, 0.0D0, t577, t630)
      t745 = FJET(XB1, XB2, s, -t504, 0.0D0, t502, 0.0D0, 0.0D0, t561)
      t747 = FJET(XB1, XB2, s, -t568, 0.0D0, -t571, -t355, t577, t630)
      t749 = FJET(XB1, XB2, s, -t654, t642, t661, -t655, t577, t728)
      t753 = FJET(XB1, XB2, s, -t655, t661, t642, -t654, t577, t728)
      rrgg2qqbarhhardt6s2e1 = t349 * t348 + t351 * t348 + t492 * t491 + 
     #t494 * t491 + t496 * t348 + t562 * t561 + t564 * t561 + t631 * t63
     #0 + t633 * t348 + t635 * t491 + t637 * t561 + t729 * t725 * t732 /
     # 0.720D3 + t735 * t725 * t732 / 0.720D3 + t739 * t491 + t741 * t63
     #0 + t743 * t630 + t745 * t561 + t747 * t630 + t749 * t725 * t732 /
     # 0.720D3 + t753 * t725 * t732 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarhhardt6s2e0
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = t12 * t15
      t17 = -0.1D1 + x3
      t18 = 0.1D1 / t17
      t19 = t16 * t18
      t22 = log(-0.4D1 * t9 * t19)
      t23 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t26 = x3 * z
      t27 = 0.2D1 * t26
      t28 = cos(t10)
      t30 = Sqrt(-t26 * t17)
      t34 = 0.1D1 / (-0.1D1 - t27 + 0.2D1 * t28 * t30 + x3)
      t38 = log(0.4D1 * t9 * t16)
      t43 = lh * t5
      t50 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t57 = 0.1D1 / x2
      t58 = t57 * t52
      t62 = x2 ** 2
      t63 = t62 * t8
      t66 = log(0.4D1 * t63 * t16)
      t68 = -0.1D1 + x2
      t69 = t16 * t68
      t72 = log(-0.4D1 * t63 * t69)
      t79 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t80 = t8 * t12
      t83 = log(0.4D1 * t80 * t15)
      t85 = t83 ** 2
      t96 = lh ** 2
      t97 = 0.180D3 * t96
      t98 = pi ** 2
      t99 = 0.30D2 * t98
      t100 = t97 - t99
      t101 = t100 * t5
      t102 = pi * t23
      t107 = x3 * t15
      t111 = log(-0.4D1 * t107 * t12 * t18)
      t112 = t111 ** 2
      t116 = log(0.4D1 * t107 * t12)
      t117 = t116 ** 2
      t145 = log(0.4D1 * t16)
      t148 = t145 ** 2
      t155 = rrgg2qqbarhhard61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, 0.0D0, t2, 0.0D0)
      t179 = t62 * x3
      t182 = log(-0.4D1 * t179 * t69)
      t186 = log(-0.4D1 * t179 * t19)
      t192 = log(0.4D1 * t179 * t16)
      t204 = t15 * t62
      t205 = t12 * t68
      t208 = log(-0.4D1 * t204 * t205)
      t209 = t208 ** 2
      t212 = log(0.4D1 * t204 * t12)
      t213 = t212 ** 2
      t229 = -(0.90D2 * t6 * ((t7 - t22 * t23) * t34 + t7 - t38 * t23) -
     # 0.180D3 * t43 * pi * (t23 + t23 * t34)) * t50 * t52 / 0.1440D4 - 
     #t6 * t23 * t34 * t50 * t58 / 0.8D1 + t6 * (t66 * t23 - t72 * t23) 
     #* t57 * t52 / 0.8D1 + (0.90D2 * t6 * (-t79 + t83 * t7 - t85 * t23 
     #/ 0.2D1) - 0.180D3 * t43 * pi * (-t7 + t83 * t23) - t101 * t102) *
     # t52 / 0.1440D4 - (0.90D2 * t6 * t23 * (t112 * t34 / 0.2D1 + t117 
     #/ 0.2D1) + (-0.180D3 * t23 * lh + 0.90D2 * t7) * t5 * pi * (-t111 
     #* t34 - t116) + (-0.180D3 * t7 * lh + 0.90D2 * t79 + t23 * t100) *
     # t5 * pi * (t34 + 0.1D1)) * t50 / 0.2880D4 - (0.180D3 * t145 * lh 
     #+ 0.45D2 * t148 + t97 - t99) * t5 * pi * t7 / 0.2880D4 - t6 * t155
     # / 0.32D2 - (-0.90D2 * t148 * lh + 0.60D2 * lh * t98 - 0.240D3 * z
     #eta3 - 0.120D3 * t96 * lh - 0.15D2 * t148 * t145 - t145 * t100) * 
     #t5 * t102 / 0.2880D4 - (-0.180D3 * lh - 0.90D2 * t145) * t5 * pi *
     # t79 / 0.2880D4 + (0.90D2 * t6 * (-t182 * t23 - (t7 - t186 * t23) 
     #* t34 + t192 * t23) + 0.180D3 * t43 * t102 * t34) * t50 * t57 / 0.
     #1440D4 + (0.90D2 * t6 * t23 * (t209 / 0.2D1 - t213 / 0.2D1) + (0.9
     #0D2 * t6 * t7 - 0.180D3 * t43 * t102) * (-t208 + t212)) * t57 / 0.
     #1440D4
      t230 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t229)
      t232 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t229)
      t234 = t2 * x1
      t235 = -0.1D1 + x1
      t236 = t2 * t235
      t237 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #234, 0.0D0, -t236, 0.0D0)
      t238 = t9 * t12
      t240 = x1 * z
      t241 = -z - x1 + t240
      t242 = 0.1D1 / t241
      t243 = 0.1D1 / t13 * t242
      t244 = t235 ** 2
      t249 = log(0.4D1 * t238 * t243 * t244 * t18)
      t250 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #234, 0.0D0, -t236, 0.0D0)
      t253 = x3 * x1
      t254 = t253 * z
      t257 = x3 * t241
      t259 = Sqrt(t257 * t17)
      t263 = 0.1D1 / (0.2D1 * t254 - 0.2D1 * t253 - t27 + x3 - 0.1D1 + 0
     #.2D1 * t28 * t259)
      t265 = t243 * t244
      t268 = log(-0.4D1 * t238 * t265)
      t274 = -t250 * t263 - t250
      t285 = t50 * t57 * t52
      t288 = t63 * t12
      t291 = log(-0.4D1 * t288 * t265)
      t296 = pi * t250
      t303 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #234, 0.0D0, -t236, 0.0D0)
      t306 = log(-0.4D1 * t80 * t265)
      t308 = t306 ** 2
      t323 = -(0.90D2 * t6 * (-(t237 - t249 * t250) * t263 - t237 + t268
     # * t250) - 0.180D3 * t43 * pi * t274) * t50 * t52 / 0.1440D4 - t6 
     #* t274 * t285 / 0.8D1 + (0.90D2 * t6 * (t237 - t291 * t250) - 0.18
     #0D3 * t43 * t296) * t57 * t52 / 0.720D3 + (0.90D2 * t6 * (t303 - t
     #306 * t237 + t308 * t250 / 0.2D1) - 0.180D3 * t43 * pi * (t237 - t
     #306 * t250) + t101 * t296) * t52 / 0.1440D4
      t324 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t234, -t236, 0.0D0, t323)
      t326 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t236, t234, 0.0D0, t323)
      t328 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t229)
      t330 = x2 * x3
      t331 = 0.1D1 - x3 + t330
      t332 = 0.1D1 / t331
      t333 = t330 * t332
      t334 = t2 * t333
      t336 = t2 * t17 * t332
      t337 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t334, -t336, 0.0D0)
      t339 = t330 * z
      t340 = t68 * t17
      t342 = Sqrt(t26 * t340)
      t346 = 0.1D1 / (t339 - t27 - 0.1D1 + 0.2D1 * t28 * t342 + x3)
      t351 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t334, -t336, 0.0D0)
      t353 = t331 ** 2
      t359 = log(0.4D1 * t179 * t15 * t205 * t17 / t353)
      t373 = t6 * t337 * t346 * t50 * t58 / 0.8D1 + (0.90D2 * t6 * (t351
     # - t359 * t337) * t346 - 0.180D3 * t43 * pi * t337 * t346) * t50 *
     # t57 / 0.1440D4
      t374 = FJET(XB1, XB2, s, 0.0D0, t334, 0.0D0, -t336, 0.0D0, t373)
      t376 = FJET(XB1, XB2, s, 0.0D0, -t336, 0.0D0, t334, 0.0D0, t373)
      t378 = x2 * x1
      t380 = t2 * t378 * t242
      t383 = t68 * s * t1 * x1
      t384 = t1 ** 2
      t389 = s * t384 * x2 * x1 * t235 * t242
      t390 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t380, -
     #t383, 0.0D0, -t236, t389)
      t391 = t241 * t390
      t393 = t378 * z
      t395 = 0.1D1 / (z + t393 - t378 + x1 - t240)
      t400 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, -t380, -
     #t383, 0.0D0, -t236, t389)
      t406 = log(0.4D1 * t288 * t243 * t244 * t68)
      t421 = t6 * t391 * t395 * t50 * t58 / 0.8D1 + (0.90D2 * t6 * (t241
     # * t400 - t406 * t241 * t390) * t395 - 0.180D3 * t43 * pi * t391 *
     # t395) * t57 * t52 / 0.720D3
      t422 = FJET(XB1, XB2, s, 0.0D0, -t380, -t236, -t383, t389, t421)
      t424 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t229)
      t426 = FJET(XB1, XB2, s, t234, -t236, 0.0D0, 0.0D0, 0.0D0, t323)
      t428 = FJET(XB1, XB2, s, t334, 0.0D0, -t336, 0.0D0, 0.0D0, t373)
      t433 = t17 * s * t1 * t235 * t332
      t434 = x2 * z
      t435 = t330 * x1
      t436 = t330 * t240
      t438 = Sqrt(-t257 * t340)
      t439 = t28 * t438
      t442 = z + x1 - t240 - t434 - t378 + t393 - t26 - t253 + t254 + t3
     #39 + t435 - t436 + t179 + 0.2D1 * t439 * x2
      t445 = t234 * t442 * t242 * t332
      t446 = t236 * t333
      t452 = t234 * x2 * (-t26 - t253 + t254 + t339 + t435 - t436 - 0.1D
     #1 + x3 + 0.2D1 * t439) * t242 * t332
      t453 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, t452, -t
     #445, -t446, t433, t389)
      t457 = x3 * t13
      t479 = -z - x1 + 0.4D1 * t436 - 0.3D1 * t457 * t378 - 0.4D1 * t9 *
     # t434 + 0.2D1 * t9 * t13 * x2 - t179 * t240 - 0.2D1 * t439 * t378 
     #- 0.2D1 * t439 * t240 + t179 * t13 * x1 + 0.2D1 * t179 * t8 * z - 
     #t179 * t8 * t13 + t378 - 0.2D1 * t9 - 0.2D1 * t457
      t497 = -0.5D1 * t254 - t393 - t435 + 0.2D1 * t9 * x2 + t330 * t13 
     #- t179 * t8 + 0.4D1 * t457 * x1 + 0.4D1 * t9 * z - 0.2D1 * t9 * t1
     #3 + 0.2D1 * t439 * z + 0.2D1 * t439 * x1 + 0.2D1 * t439 * t393 + t
     #26 + t240 + t253
      t499 = 0.1D1 / (t479 + t497)
      t503 = t6 * t241 * t453 * t499 * t50 * t58 / 0.8D1
      t504 = FJET(XB1, XB2, s, t433, -t445, -t446, t452, t389, t503)
      t506 = pi * t241
      t509 = t453 * t499 * t285
      t512 = FJET(XB1, XB2, s, t452, -t446, -t445, t433, t389, t503)
      t517 = FJET(XB1, XB2, s, -t236, t234, 0.0D0, 0.0D0, 0.0D0, t323)
      t519 = FJET(XB1, XB2, s, -t236, -t383, 0.0D0, -t380, t389, t421)
      t521 = FJET(XB1, XB2, s, -t383, -t236, -t380, 0.0D0, t389, t421)
      t523 = FJET(XB1, XB2, s, -t336, 0.0D0, t334, 0.0D0, 0.0D0, t373)
      t525 = FJET(XB1, XB2, s, -t380, 0.0D0, -t383, -t236, t389, t421)
      t527 = FJET(XB1, XB2, s, -t445, t433, t452, -t446, t389, t503)
      t532 = FJET(XB1, XB2, s, -t446, t452, t433, -t445, t389, t503)
      rrgg2qqbarhhardt6s2e0 = t230 * t229 + t232 * t229 + t324 * t323 + 
     #t326 * t323 + t328 * t229 + t374 * t373 + t376 * t373 + t422 * t42
     #1 + t424 * t229 + t426 * t323 + t428 * t373 + t504 * t5 * t506 * t
     #509 / 0.8D1 + t512 * t5 * t506 * t509 / 0.8D1 + t517 * t323 + t519
     # * t421 + t521 * t421 + t523 * t373 + t525 * t421 + t527 * t5 * t5
     #06 * t509 / 0.8D1 + t532 * t5 * t506 * t509 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt6s2em1
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = -0.1D1 + x3
      t20 = log(-0.4D1 * t11 * t14 / t15)
      t21 = x3 * z
      t22 = 0.2D1 * t21
      t23 = cos(t12)
      t25 = Sqrt(-t21 * t15)
      t29 = 0.1D1 / (-0.1D1 - t22 + 0.2D1 * t23 * t25 + x3)
      t33 = log(0.4D1 * t11 * t14)
      t40 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t48 = 0.1D1 / x3
      t51 = x1 ** 2
      t52 = t51 * t14
      t55 = log(0.4D1 * t52 * t10)
      t60 = lh * t5
      t61 = pi * t7
      t65 = 0.1D1 / x1
      t76 = 0.1D1 / x2
      t80 = x2 ** 2
      t81 = t10 * t80
      t82 = -0.1D1 + x2
      t86 = log(-0.4D1 * t81 * t14 * t82)
      t89 = log(0.4D1 * t81 * t14)
      t95 = rrgg2qqbarhhard61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t101 = log(0.4D1 * t10 * t14)
      t110 = t101 ** 2
      t112 = lh ** 2
      t114 = pi ** 2
      t120 = -(0.90D2 * t6 * t7 * (-t20 * t29 - t33) + (-0.180D3 * t7 * 
     #lh + 0.90D2 * t40) * t5 * pi * (t29 + 0.1D1)) * t48 / 0.2880D4 + (
     #0.90D2 * t6 * (-t40 + t55 * t7) + 0.180D3 * t60 * t61) * t65 / 0.1
     #440D4 - t6 * (t7 + t7 * t29) * t48 * t65 / 0.16D2 - t6 * t7 * t29 
     #* t48 * t76 / 0.16D2 + t6 * t7 * (-t86 + t89) * t76 / 0.16D2 - t6 
     #* t95 / 0.32D2 - (-0.180D3 * lh - 0.90D2 * t101) * t5 * pi * t40 /
     # 0.2880D4 - (0.180D3 * t101 * lh + 0.45D2 * t110 + 0.180D3 * t112 
     #- 0.30D2 * t114) * t5 * t61 / 0.2880D4
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t120)
      t123 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t120)
      t125 = t2 * x1
      t126 = -0.1D1 + x1
      t127 = t2 * t126
      t128 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #125, 0.0D0, -t127, 0.0D0)
      t133 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #125, 0.0D0, -t127, 0.0D0)
      t135 = x1 * z
      t136 = -z - x1 + t135
      t137 = 0.1D1 / t136
      t139 = t126 ** 2
      t143 = log(-0.4D1 * t52 / t8 * t137 * t139)
      t154 = x3 * x1
      t160 = Sqrt(x3 * t136 * t15)
      t171 = t6 * t128 * t76 * t65 / 0.8D1 + (0.90D2 * t6 * (t133 - t143
     # * t128) - 0.180D3 * t60 * pi * t128) * t65 / 0.1440D4 - t6 * (-t1
     #28 / (0.2D1 * t154 * z - 0.2D1 * t154 - t22 + x3 - 0.1D1 + 0.2D1 *
     # t23 * t160) - t128) * t48 * t65 / 0.16D2
      t172 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t125, -t127, 0.0D0, t171)
      t174 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t127, t125, 0.0D0, t171)
      t176 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t120)
      t178 = x2 * x3
      t180 = 0.1D1 / (0.1D1 - x3 + t178)
      t182 = t2 * t178 * t180
      t184 = t2 * t15 * t180
      t185 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t182, -t184, 0.0D0)
      t190 = Sqrt(t21 * t82 * t15)
      t194 = 0.1D1 / (t178 * z - t22 - 0.1D1 + 0.2D1 * t23 * t190 + x3)
      t198 = t6 * t185 * t194 * t48 * t76 / 0.16D2
      t199 = FJET(XB1, XB2, s, 0.0D0, t182, 0.0D0, -t184, 0.0D0, t198)
      t204 = t185 * t194 * t48 * t76
      t207 = FJET(XB1, XB2, s, 0.0D0, -t184, 0.0D0, t182, 0.0D0, t198)
      t212 = x2 * x1
      t214 = t2 * t212 * t137
      t217 = t82 * s * t1 * x1
      t218 = t1 ** 2
      t223 = s * t218 * x2 * x1 * t126 * t137
      t225 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, -t214, -
     #t217, 0.0D0, -t127, t223)
      t231 = t225 / (z + t212 * z - t212 + x1 - t135) * t76 * t65
      t233 = t6 * t136 * t231 / 0.8D1
      t234 = FJET(XB1, XB2, s, 0.0D0, -t214, -t127, -t217, t223, t233)
      t236 = pi * t136
      t240 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t120)
      t242 = FJET(XB1, XB2, s, t125, -t127, 0.0D0, 0.0D0, 0.0D0, t171)
      t244 = FJET(XB1, XB2, s, t182, 0.0D0, -t184, 0.0D0, 0.0D0, t198)
      t249 = FJET(XB1, XB2, s, -t127, t125, 0.0D0, 0.0D0, 0.0D0, t171)
      t251 = FJET(XB1, XB2, s, -t127, -t217, 0.0D0, -t214, t223, t233)
      t256 = FJET(XB1, XB2, s, -t217, -t127, -t214, 0.0D0, t223, t233)
      t261 = FJET(XB1, XB2, s, -t214, 0.0D0, -t217, -t127, t223, t233)
      t266 = FJET(XB1, XB2, s, -t184, 0.0D0, t182, 0.0D0, 0.0D0, t198)
      rrgg2qqbarhhardt6s2em1 = t121 * t120 + t123 * t120 + t172 * t171 +
     # t174 * t171 + t176 * t120 + t199 * t5 * pi * t204 / 0.16D2 + t207
     # * t5 * pi * t204 / 0.16D2 + t234 * t5 * t236 * t231 / 0.8D1 + t24
     #0 * t120 + t242 * t171 + t244 * t5 * pi * t204 / 0.16D2 + t249 * t
     #171 + t251 * t5 * t236 * t231 / 0.8D1 + t256 * t5 * t236 * t231 / 
     #0.8D1 + t261 * t5 * t236 * t231 / 0.8D1 + t266 * t5 * pi * t204 / 
     #0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt6s2em2
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t5 = 0.1D1 / t3 / s
      t6 = t5 * pi
      t7 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = x3 * z
      t14 = x4 * pi
      t15 = cos(t14)
      t18 = Sqrt(-t12 * (-0.1D1 + x3))
      t29 = rrgg2qqbarhhard61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, 0.0D0, t2, 0.0D0)
      t33 = z ** 2
      t36 = Sin(t14)
      t37 = t36 ** 2
      t40 = log(0.4D1 / t33 / z * t37)
      t47 = -t6 * t7 * t8 / 0.16D2 - t6 * t7 * (0.1D1 / (-0.1D1 - 0.2D1 
     #* t12 + 0.2D1 * t15 * t18 + x3) + 0.1D1) / x3 / 0.32D2 - t6 * t29 
     #/ 0.32D2 - (-0.180D3 * lh - 0.90D2 * t40) * t5 * pi * t7 / 0.2880D
     #4
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t47)
      t50 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t47)
      t52 = t2 * x1
      t54 = t2 * (-0.1D1 + x1)
      t55 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t5
     #2, 0.0D0, -t54, 0.0D0)
      t58 = t6 * t55 * t8 / 0.16D2
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t52, -t54, 0.0D0, t58)
      t62 = pi * t55 * t8
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t54, t52, 0.0D0, t58)
      t69 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t47)
      t71 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t47)
      t73 = FJET(XB1, XB2, s, t52, -t54, 0.0D0, 0.0D0, 0.0D0, t58)
      t77 = FJET(XB1, XB2, s, -t54, t52, 0.0D0, 0.0D0, 0.0D0, t58)
      rrgg2qqbarhhardt6s2em2 = t48 * t47 + t50 * t47 + t59 * t5 * t62 / 
     #0.16D2 + t65 * t5 * t62 / 0.16D2 + t69 * t47 + t71 * t47 + t73 * t
     #5 * t62 / 0.16D2 + t77 * t5 * t62 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt6s2em3
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

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
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2qqbarhhard61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, 0.0D0, t2, 0.0D0)
      t9 = t5 * pi * t7 / 0.32D2
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t9)
      t12 = pi * t7
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t9)
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t9)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t9)
      rrgg2qqbarhhardt6s2em3 = -t10 * t5 * t12 / 0.32D2 - t14 * t5 * t12
     # / 0.32D2 - t17 * t5 * t12 / 0.32D2 - t20 * t5 * t12 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarhhardt6s2em4
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
      doubleprecision rrgg2qqbarhhard61J1
      doubleprecision rrgg2qqbarhhard61J2
      doubleprecision rrgg2qqbarhhard61J3
      doubleprecision rrgg2qqbarhhard61J4
      doubleprecision rrgg2qqbarhhard61J5
      doubleprecision rrgg2qqbarhhard61J6
      doubleprecision rrgg2qqbarhhard61J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarhhard61J1
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
      t2 = S12 ** 2
      t4 = S34 ** 2
      rrgg2qqbarhhard61J1 = (S34 * nf * t2 / 0.6D1 + t4 * S34 * nf / 0.6
     #D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard61J2
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      rrgg2qqbarhhard61J2 = (S34 * nf * t2 / 0.6D1 + t5 * nf * S12 / 0.3
     #D1 + t5 * S34 * nf / 0.6D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard61J3
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
      t1 = S34 ** 2
      t4 = S13 + S14 + S34
      t7 = S23 + S24 + S34
      t13 = S24 ** 2
      t17 = S14 ** 2
      rrgg2qqbarhhard61J3 = ((t1 * nf + S24 * nf * t4 / 0.6D1 + t7 * S14
     # * nf / 0.6D1) * S12 + t13 * nf * t4 / 0.6D1 + t7 * t17 * nf / 0.6
     #D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard61J4
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarhhard61J4 = (-S34 * nf * t2 / 0.6D1 + (0.5D1 / 0.3D1 * t
     #5 * nf + S24 * nf * t9 / 0.3D1 + t12 * S14 * nf / 0.3D1) * S12 - t
     #5 * S34 * nf / 0.6D1 + t21 * nf * t9 / 0.3D1 + t12 * t25 * nf / 0.
     #3D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard61J5
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarhhard61J5 = (-S34 * nf * t2 / 0.3D1 + (0.7D1 / 0.3D1 * t
     #5 * nf + S24 * nf * t9 / 0.2D1 + t12 * S14 * nf / 0.2D1) * S12 - t
     #5 * S34 * nf / 0.3D1 + t21 * nf * t9 / 0.2D1 + t12 * t25 * nf / 0.
     #2D1) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard61J6
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarhhard61J6 = (-0.3D1 / 0.2D1 * S34 * nf * t2 + (0.3D1 * t
     #5 * nf + 0.2D1 / 0.3D1 * S24 * nf * t9 + 0.2D1 / 0.3D1 * t12 * S14
     # * nf) * S12 - 0.3D1 / 0.2D1 * t5 * S34 * nf + 0.2D1 / 0.3D1 * t21
     # * nf * t9 + 0.2D1 / 0.3D1 * t12 * t25 * nf) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard61J7
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
      t2 = S12 ** 2
      t5 = S34 ** 2
      t9 = S13 + S14 + S34
      t12 = S23 + S24 + S34
      t21 = S24 ** 2
      t25 = S14 ** 2
      rrgg2qqbarhhard61J7 = (-0.5D1 / 0.6D1 * S34 * nf * t2 + (0.5D1 / 0
     #.3D1 * t5 * nf + 0.5D1 / 0.6D1 * S24 * nf * t9 + 0.5D1 / 0.6D1 * t
     #12 * S14 * nf) * S12 - 0.5D1 / 0.6D1 * t5 * S34 * nf + 0.5D1 / 0.6
     #D1 * t21 * nf * t9 + 0.5D1 / 0.6D1 * t12 * t25 * nf) / pi * wd / z

      end function
  
   
      subroutine rrgg2qqbarhsoftt6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt6s1e1  
      doubleprecision rrgg2qqbarhsoftt6s1e0  
      doubleprecision rrgg2qqbarhsoftt6s1em1  
      doubleprecision rrgg2qqbarhsoftt6s1em2  
      doubleprecision rrgg2qqbarhsoftt6s1em3  
      doubleprecision rrgg2qqbarhsoftt6s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt6s1e1
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
      rrgg2qqbarhsoftt6s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt6s1e0
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
      rrgg2qqbarhsoftt6s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt6s1em1
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
      rrgg2qqbarhsoftt6s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt6s1em2
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
      rrgg2qqbarhsoftt6s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt6s1em3
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
      rrgg2qqbarhsoftt6s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt6s1em4
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
      rrgg2qqbarhsoftt6s1em4 = 0.0D0

      end function
