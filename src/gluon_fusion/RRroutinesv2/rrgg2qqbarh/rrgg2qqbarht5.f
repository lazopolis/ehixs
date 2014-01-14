      subroutine rrgg2qqbarht5
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt5
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt5
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhard51J1  
      doubleprecision rrgg2qqbarhhard51J2  
      doubleprecision rrgg2qqbarhhard51J3  
      doubleprecision rrgg2qqbarhhard51J4  
      doubleprecision rrgg2qqbarhhard51J5  
      doubleprecision rrgg2qqbarhhard51J6  
      doubleprecision rrgg2qqbarhhard51J7  
      doubleprecision rrgg2qqbarhhardt5s1e1  
      doubleprecision rrgg2qqbarhhardt5s1e0  
      doubleprecision rrgg2qqbarhhardt5s1em1  
      doubleprecision rrgg2qqbarhhardt5s1em2  
      doubleprecision rrgg2qqbarhhardt5s1em3  
      doubleprecision rrgg2qqbarhhardt5s1em4  
      doubleprecision rrgg2qqbarhhardt5s2e1  
      doubleprecision rrgg2qqbarhhardt5s2e0  
      doubleprecision rrgg2qqbarhhardt5s2em1  
      doubleprecision rrgg2qqbarhhardt5s2em2  
      doubleprecision rrgg2qqbarhhardt5s2em3  
      doubleprecision rrgg2qqbarhhardt5s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt5s1e1
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t10 = lh * t5
      t11 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t12 = pi * t11
      t15 = lh ** 2
      t17 = pi ** 2
      t19 = 0.180D3 * t15 - 0.30D2 * t17
      t20 = t19 * t5
      t21 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t22 = pi * t21
      t25 = x4 * pi
      t26 = Sin(t25)
      t27 = t26 ** 2
      t28 = x3 * t27
      t29 = z ** 2
      t30 = 0.1D1 / t29
      t31 = -0.1D1 + x3
      t32 = 0.1D1 / t31
      t36 = log(-0.4D1 * t28 * t30 * t32)
      t37 = cos(t25)
      t39 = Sqrt(-x3 * t31)
      t44 = 0.1D1 / (-z - x3 + 0.2D1 * t37 * t39 * z)
      t48 = log(0.4D1 * t28 * t30)
      t49 = 0.1D1 / z
      t53 = t48 ** 2
      t56 = t36 ** 2
      t65 = rrgg2qqbarhhard51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t73 = 0.60D2 * lh * t17 - 0.240D3 * zeta3 - 0.120D3 * t15 * lh
      t74 = t73 * t5
      t93 = 0.1D1 / x3
      t96 = t30 * t27
      t98 = log(0.4D1 * t96)
      t99 = t98 ** 2
      t102 = t99 * t98
      t117 = t17 ** 2
      t118 = t15 ** 2
      t126 = rrgg2qqbarhhard51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t
     #2, 0.0D0, 0.0D0, 0.0D0)
      t134 = t99 ** 2
      t139 = t49 * pi
      t142 = x1 ** 2
      t143 = x3 * t142
      t144 = t96 * t32
      t147 = log(-0.4D1 * t143 * t144)
      t149 = t147 ** 2
      t156 = log(0.4D1 * t143 * t96)
      t158 = t156 ** 2
      t179 = pi * (-t21 * t49 - t21 * t44)
      t180 = t20 * t179
      t183 = 0.1D1 / x1
      t186 = t142 * t27
      t187 = t186 * t30
      t189 = log(0.4D1 * t187)
      t194 = t5 * t49
      t195 = t189 ** 2
      t206 = t139 * t21
      t207 = t74 * t206
      t218 = x2 ** 2
      t219 = t218 * x3
      t220 = t219 * t187
      t222 = log(0.4D1 * t220)
      t226 = t219 * t142
      t229 = log(-0.4D1 * t226 * t144)
      t240 = 0.1D1 / x2
      t241 = t240 * t183
      t244 = t218 * t142
      t247 = log(0.4D1 * t244 * t96)
      t249 = t247 ** 2
      t269 = log(-0.4D1 * t219 * t144)
      t271 = t269 ** 2
      t278 = log(0.4D1 * t219 * t96)
      t280 = t278 ** 2
      t302 = t218 * t27
      t305 = log(0.4D1 * t302 * t30)
      t311 = t305 ** 2
      t333 = ((0.90D2 * t6 * t7 - 0.180D3 * t10 * t12 + t20 * t22) * (t3
     #6 * t44 + t48 * t49) + 0.90D2 * t6 * t21 * (t53 * t48 * t49 / 0.6D
     #1 + t56 * t36 * t44 / 0.6D1) + (t20 * t12 + 0.90D2 * t6 * t65 + t7
     #4 * t22 - 0.180D3 * t10 * pi * t7) * (-t44 - t49) + (0.90D2 * t6 *
     # t11 - 0.180D3 * t10 * t22) * (-t56 * t44 / 0.2D1 - t53 * t49 / 0.
     #2D1)) * t93 / 0.2880D4 - (-0.180D3 * (t99 * t11 / 0.2D1 + t65 - t1
     #02 * t21 / 0.6D1 - t98 * t7) * lh + (t7 - t98 * t11 + t99 * t21 / 
     #0.2D1) * t19 + (t11 - t98 * t21) * t73 + t21 * (t117 + 0.60D2 * t1
     #18 + 0.480D3 * lh * zeta3 - 0.60D2 * t15 * t17) + 0.90D2 * t126 - 
     #0.15D2 * t102 * t11 + 0.45D2 * t99 * t7 - 0.90D2 * t98 * t65 + 0.1
     #5D2 / 0.4D1 * t134 * t21) * t5 * t139 / 0.2880D4 + (0.90D2 * t6 * 
     #(-(t7 - t147 * t11 + t149 * t21 / 0.2D1) * t44 - (t7 - t156 * t11 
     #+ t158 * t21 / 0.2D1) * t49) - 0.180D3 * t10 * pi * (-(t11 - t147 
     #* t21) * t44 - (t11 - t156 * t21) * t49) + t180) * t93 * t183 / 0.
     #1440D4 + (t20 * t139 * (-t11 + t189 * t21) + 0.90D2 * t194 * pi * 
     #(-t195 * t11 / 0.2D1 - t65 + t195 * t189 * t21 / 0.6D1 + t189 * t7
     #) - t207 - 0.180D3 * t10 * t139 * (-t7 + t189 * t11 - t195 * t21 /
     # 0.2D1)) * t183 / 0.1440D4 + (0.90D2 * t6 * (-(t11 - t222 * t21) *
     # t49 - (t11 - t229 * t21) * t44) - 0.180D3 * t10 * t179) * t93 * t
     #241 / 0.720D3 + (-0.90D2 * t6 * (t7 - t247 * t11 + t249 * t21 / 0.
     #2D1) * t49 + 0.180D3 * t10 * pi * (t11 - t247 * t21) * t49 - t20 *
     # t206) * t240 * t183 / 0.720D3 + (0.90D2 * t6 * (-(t7 - t269 * t11
     # + t271 * t21 / 0.2D1) * t44 - (t7 - t278 * t11 + t280 * t21 / 0.2
     #D1) * t49) - 0.180D3 * t10 * pi * (-(t11 - t269 * t21) * t44 - (t1
     #1 - t278 * t21) * t49) + t180) * t93 * t240 / 0.1440D4 + (-t20 * p
     #i * (t11 - t305 * t21) * t49 - 0.90D2 * t6 * (t311 * t11 / 0.2D1 +
     # t65 - t311 * t305 * t21 / 0.6D1 - t305 * t7) * t49 - t207 + 0.180
     #D3 * t10 * pi * (t7 - t305 * t11 + t311 * t21 / 0.2D1) * t49) * t2
     #40 / 0.1440D4
      t334 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t333)
      t336 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t333)
      t338 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t333)
      t340 = t2 * x1
      t341 = -0.1D1 + x1
      t342 = x1 * z
      t343 = 0.1D1 - x1 + t342
      t344 = 0.1D1 / t343
      t346 = t2 * t341 * t344
      t347 = t1 ** 2
      t348 = s * t347
      t350 = t341 * x1 * t344
      t351 = t348 * t350
      t352 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t346, t340, 0.0D0, -t351)
      t353 = t49 * t352
      t354 = t143 * t27
      t355 = t30 * t344
      t356 = t341 ** 2
      t357 = t355 * t356
      t360 = log(0.4D1 * t354 * t357)
      t361 = t360 * t49
      t362 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t346, t340, 0.0D0, -t351)
      t364 = t360 ** 2
      t366 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t346, t340, 0.0D0, -t351)
      t371 = t355 * t356 * t32
      t374 = log(-0.4D1 * t354 * t371)
      t375 = t374 * t343
      t377 = t374 ** 2
      t382 = x3 * x1
      t383 = t382 * z
      t384 = 0.3D1 * t383
      t385 = 0.2D1 * t382
      t386 = x1 * t29
      t387 = x3 * t29
      t388 = t387 * x1
      t390 = 0.2D1 * t143 * z
      t391 = t143 * t29
      t392 = x3 * t343
      t394 = Sqrt(-t392 * t31)
      t398 = -z + t342 - t384 + t385 - x3 - t386 + t388 + t390 - t391 + 
     #0.2D1 * t37 * t394 * z - t143
      t399 = 0.1D1 / t398
      t404 = t49 * t362
      t406 = t343 * t362
      t418 = pi * (t49 * t366 + t343 * t366 * t399)
      t426 = log(0.4D1 * t186 * t357)
      t431 = t426 ** 2
      t434 = rrgg2qqbarhhard51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t346, t340, 0.0D0, -t351)
      t443 = t139 * t366
      t455 = t344 * t356
      t459 = log(0.4D1 * t226 * t96 * t455)
      t462 = t219 * t186
      t465 = log(-0.4D1 * t462 * t371)
      t479 = t244 * t27
      t482 = log(0.4D1 * t479 * t357)
      t483 = t482 * t49
      t485 = t482 ** 2
      t502 = (0.90D2 * t6 * (t353 - t361 * t362 + t364 * t49 * t366 / 0.
     #2D1 + (t343 * t352 - t375 * t362 + t377 * t343 * t366 / 0.2D1) * t
     #399) - 0.180D3 * t10 * pi * (t404 - t361 * t366 + (t406 - t375 * t
     #366) * t399) + t20 * t418) * t93 * t183 / 0.1440D4 + (t20 * t139 *
     # (t362 - t426 * t366) + 0.90D2 * t194 * pi * (t431 * t362 / 0.2D1 
     #+ t434 - t431 * t426 * t366 / 0.6D1 - t426 * t352) + t74 * t443 - 
     #0.180D3 * t10 * t139 * (t352 - t426 * t362 + t431 * t366 / 0.2D1))
     # * t183 / 0.1440D4 + (0.90D2 * t6 * (t404 - t459 * t49 * t366 + (t
     #406 - t465 * t343 * t366) * t399) - 0.180D3 * t10 * t418) * t93 * 
     #t241 / 0.720D3 + (0.90D2 * t6 * (t353 - t483 * t362 + t485 * t49 *
     # t366 / 0.2D1) - 0.180D3 * t10 * pi * (t404 - t483 * t366) + t20 *
     # t443) * t240 * t183 / 0.720D3
      t503 = FJET(XB1, XB2, s, 0.0D0, t340, -t346, 0.0D0, -t351, t502)
      t506 = x2 * t1 * s
      t507 = -0.1D1 + x2
      t509 = t507 * t1 * s
      t510 = x2 * z
      t512 = 0.1D1 / (-x2 + t510 - z)
      t513 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, t506, -t
     #509, 0.0D0, 0.0D0, 0.0D0)
      t514 = t512 * t513
      t515 = t96 * t507
      t518 = log(-0.4D1 * t226 * t515)
      t520 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t506, -t
     #509, 0.0D0, 0.0D0, 0.0D0)
      t526 = pi * t512 * t520
      t532 = (0.90D2 * t6 * (-t514 + t518 * t512 * t520) + 0.180D3 * t10
     # * t526) * t93 * t241 / 0.720D3
      t533 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, t506, -t
     #509, 0.0D0, 0.0D0, 0.0D0)
      t534 = t512 * t533
      t537 = log(-0.4D1 * t244 * t515)
      t538 = t537 * t512
      t540 = t537 ** 2
      t552 = t20 * t526
      t556 = (0.90D2 * t6 * (-t534 + t538 * t513 - t540 * t512 * t520 / 
     #0.2D1) - 0.180D3 * t10 * pi * (-t514 + t538 * t520) - t552) * t240
     # * t183 / 0.720D3
      t559 = log(-0.4D1 * t219 * t515)
      t560 = t559 * t512
      t562 = t559 ** 2
      t577 = (0.90D2 * t6 * (-t534 + t560 * t513 - t562 * t512 * t520 / 
     #0.2D1) - 0.180D3 * t10 * pi * (-t514 + t560 * t520) - t552) * t93 
     #* t240 / 0.1440D4
      t578 = t30 * t507
      t581 = log(-0.4D1 * t302 * t578)
      t582 = t581 * t512
      t584 = -t514 + t582 * t520
      t587 = t581 ** 2
      t588 = t587 * t512
      t591 = rrgg2qqbarhhard51J4(s, XB1, XB2, z, lh, wd, nf, s, t506, -t
     #509, 0.0D0, 0.0D0, 0.0D0)
      t598 = -t588 * t513 / 0.2D1 - t512 * t591 + t587 * t581 * t512 * t
     #520 / 0.6D1 + t582 * t533
      t601 = t74 * t526
      t605 = -t534 + t582 * t513 - t588 * t520 / 0.2D1
      t612 = t532 + t556 + t577 + (t20 * pi * t584 + 0.90D2 * t6 * t598 
     #- t601 - 0.180D3 * t10 * pi * t605) * t240 / 0.1440D4
      t613 = FJET(XB1, XB2, s, 0.0D0, t506, 0.0D0, -t509, 0.0D0, t612)
      t615 = x2 * x3
      t618 = Sqrt(x3 * t507 * t31)
      t619 = t37 * t618
      t621 = 0.2D1 * t619 * x2
      t623 = 0.1D1 - x3 + t615
      t624 = 0.1D1 / t623
      t626 = t2 * (0.1D1 - x3 - x2 + t615 + t219 + t621) * t624
      t631 = t2 * x2 * (-0.1D1 + t615 + 0.2D1 * t619) * t624
      t632 = t615 * z
      t633 = t219 * z
      t639 = 0.1D1 / (x2 - t510 + z - t219 + x3 - t632 + t633 - t621 - 0
     #.2D1 * t619 * z + 0.2D1 * t619 * t510)
      t640 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, -t631, t
     #626, 0.0D0, 0.0D0, 0.0D0)
      t641 = t639 * t640
      t642 = t623 ** 2
      t643 = 0.1D1 / t642
      t645 = t578 * t31 * t643
      t648 = log(0.4D1 * t462 * t645)
      t650 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, -t631, t
     #626, 0.0D0, 0.0D0, 0.0D0)
      t656 = pi * t639 * t650
      t662 = (0.90D2 * t6 * (-t641 + t648 * t639 * t650) + 0.180D3 * t10
     # * t656) * t93 * t241 / 0.720D3
      t663 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, -t631, t
     #626, 0.0D0, 0.0D0, 0.0D0)
      t668 = log(0.4D1 * t219 * t27 * t645)
      t669 = t668 * t639
      t671 = t668 ** 2
      t675 = -t639 * t663 + t669 * t640 - t671 * t639 * t650 / 0.2D1
      t679 = -t641 + t669 * t650
      t683 = t20 * t656
      t688 = t662 + (0.90D2 * t6 * t675 - 0.180D3 * t10 * pi * t679 - t6
     #83) * t93 * t240 / 0.1440D4
      t689 = FJET(XB1, XB2, s, 0.0D0, t626, 0.0D0, -t631, 0.0D0, t688)
      t692 = t1 * t341
      t694 = t507 * s * t692 * t344
      t696 = x2 * s * t692
      t698 = t348 * t507 * t350
      t699 = x2 * x1
      t700 = t699 * z
      t702 = 0.1D1 / (x2 + t700 - t699 + z - t510)
      t703 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, -t696, t
     #694, t340, 0.0D0, t698)
      t704 = t702 * t703
      t706 = t355 * t356 * t507
      t709 = log(-0.4D1 * t462 * t706)
      t711 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, -t696, t
     #694, t340, 0.0D0, t698)
      t717 = pi * t702 * t711
      t722 = (0.90D2 * t6 * (-t704 + t709 * t702 * t711) + 0.180D3 * t10
     # * t717) * t93 * t241
      t723 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, -t696, t
     #694, t340, 0.0D0, t698)
      t727 = log(-0.4D1 * t479 * t706)
      t728 = t727 * t702
      t730 = t727 ** 2
      t734 = -t702 * t723 + t728 * t703 - t730 * t702 * t711 / 0.2D1
      t738 = -t704 + t728 * t711
      t742 = t20 * t717
      t747 = t722 / 0.720D3 + (0.90D2 * t6 * t734 - 0.180D3 * t10 * pi *
     # t738 - t742) * t240 * t183 / 0.720D3
      t748 = FJET(XB1, XB2, s, 0.0D0, t694, t340, -t696, t698, t747)
      t750 = FJET(XB1, XB2, s, 0.0D0, -t509, 0.0D0, t506, 0.0D0, t612)
      t752 = FJET(XB1, XB2, s, 0.0D0, -t346, t340, 0.0D0, -t351, t502)
      t754 = FJET(XB1, XB2, s, 0.0D0, -t631, 0.0D0, t626, 0.0D0, t688)
      t756 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t333)
      t758 = FJET(XB1, XB2, s, t340, 0.0D0, 0.0D0, -t346, -t351, t502)
      t760 = t334 * t333 + t336 * t333 + t338 * t333 + t503 * t502 + t61
     #3 * t612 + t689 * t688 + t748 * t747 + t750 * t612 + t752 * t502 +
     # t754 * t688 + t756 * t333 + t758 * t502
      t761 = FJET(XB1, XB2, s, t340, -t696, 0.0D0, t694, t698, t747)
      t776 = t532 + t556 + t577 + (t20 * pi * t584 + 0.90D2 * t6 * t598 
     #- t601 - 0.180D3 * t10 * pi * t605) * t240 / 0.1440D4
      t777 = FJET(XB1, XB2, s, t506, 0.0D0, -t509, 0.0D0, 0.0D0, t776)
      t779 = FJET(XB1, XB2, s, t626, 0.0D0, -t631, 0.0D0, 0.0D0, t688)
      t781 = FJET(XB1, XB2, s, t694, 0.0D0, -t696, t340, t698, t747)
      t784 = t340 * t615 * t624
      t785 = t2 * t341
      t786 = t219 * x1
      t787 = t219 * t342
      t788 = t507 * t31
      t790 = Sqrt(t392 * t788)
      t791 = t37 * t790
      t793 = 0.2D1 * t791 * x2
      t797 = t785 * (-t786 + t219 - x2 + t615 + t787 + t793 + 0.1D1 - x3
     #) * t344 * t624
      t801 = t31 * s * t1 * x1 * t624
      t807 = t785 * x2 * (-0.1D1 + t615 + x1 - t382 - t342 + t383 + 0.2D
     #1 * t791) * t344 * t624
      t816 = t29 * x2
      t822 = 0.2D1 * t791 * t700 - 0.3D1 * t700 + 0.2D1 * t615 * t342 - 
     #t387 * t699 - 0.2D1 * t143 * t510 + t143 * t816 - 0.2D1 * t791 * t
     #510 - 0.2D1 * t791 * t699 - x2 + t787 + t385 - t386 - t143 - t633 
     #+ t632 + t342 - x3
      t823 = t142 * x2
      t834 = -t823 + 0.2D1 * t699 - z + t510 - t384 + t388 + t390 - t391
     # + 0.2D1 * t823 * z - t142 * t29 * x2 + t816 * x1 - t615 * x1 + t1
     #43 * x2 + 0.2D1 * t791 * z - t786 + t219 + t793
      t836 = 0.1D1 / (t822 + t834)
      t837 = t343 * t836
      t838 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, t807, -t
     #797, -t801, t784, t698)
      t844 = log(0.4D1 * t220 * t455 * t788 * t643)
      t846 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t807, -t
     #797, -t801, t784, t698)
      t849 = -t837 * t838 + t844 * t343 * t836 * t846
      t855 = 0.180D3 * t10 * pi * t837 * t846
      t856 = 0.90D2 * t6 * t849 + t855
      t859 = t856 * t93 * t241 / 0.720D3
      t860 = FJET(XB1, XB2, s, t784, -t797, -t801, t807, t698, t859)
      t863 = t93 * t240 * t183
      t866 = FJET(XB1, XB2, s, t807, -t801, -t797, t784, t698, t859)
      t870 = FJET(XB1, XB2, s, -t509, 0.0D0, t506, 0.0D0, 0.0D0, t776)
      t872 = FJET(XB1, XB2, s, -t346, 0.0D0, 0.0D0, t340, -t351, t502)
      t885 = t722 / 0.720D3 + (0.90D2 * t6 * t734 - 0.180D3 * t10 * pi *
     # t738 - t742) * t240 * t183 / 0.720D3
      t886 = FJET(XB1, XB2, s, -t696, t340, t694, 0.0D0, t698, t885)
      t899 = t662 + (0.90D2 * t6 * t675 - 0.180D3 * t10 * pi * t679 - t6
     #83) * t93 * t240 / 0.1440D4
      t900 = FJET(XB1, XB2, s, -t631, 0.0D0, t626, 0.0D0, 0.0D0, t899)
      t905 = 0.90D2 * t6 * t849 + t855
      t908 = t905 * t93 * t241 / 0.720D3
      t909 = FJET(XB1, XB2, s, -t801, t807, t784, -t797, t698, t908)
      t913 = FJET(XB1, XB2, s, -t797, t784, t807, -t801, t698, t908)
      t917 = t761 * t747 + t777 * t776 + t779 * t688 + t781 * t747 + t86
     #0 * t856 * t863 / 0.720D3 + t866 * t856 * t863 / 0.720D3 + t870 * 
     #t776 + t872 * t502 + t886 * t885 + t900 * t899 + t909 * t905 * t86
     #3 / 0.720D3 + t913 * t905 * t863 / 0.720D3
      rrgg2qqbarhhardt5s1e1 = t760 + t917

      end function



      doubleprecision function rrgg2qqbarhhardt5s1e0
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = -0.1D1 + x3
      t15 = 0.1D1 / t14
      t19 = log(-0.4D1 * t11 * t13 * t15)
      t20 = t19 ** 2
      t21 = cos(t8)
      t23 = Sqrt(-x3 * t14)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t23 * z)
      t32 = log(0.4D1 * t11 * t13)
      t33 = t32 ** 2
      t34 = 0.1D1 / z
      t41 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t44 = lh * t5
      t45 = pi * t7
      t53 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t59 = lh ** 2
      t61 = pi ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t64 = t63 * t5
      t70 = 0.1D1 / x3
      t73 = t13 * t10
      t75 = log(0.4D1 * t73)
      t77 = t75 ** 2
      t92 = rrgg2qqbarhhard51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t104 = t34 * pi
      t107 = x2 ** 2
      t108 = t107 * x3
      t109 = t73 * t15
      t112 = log(-0.4D1 * t108 * t109)
      t118 = log(0.4D1 * t108 * t73)
      t127 = -t7 * t34 - t7 * t28
      t130 = 0.180D3 * t44 * pi * t127
      t133 = 0.1D1 / x2
      t136 = t107 * t10
      t139 = log(0.4D1 * t136 * t13)
      t141 = t139 ** 2
      t154 = t45 * t34
      t155 = t64 * t154
      t159 = x1 ** 2
      t160 = x3 * t159
      t163 = log(-0.4D1 * t160 * t109)
      t169 = log(0.4D1 * t160 * t73)
      t178 = 0.1D1 / x1
      t183 = t70 * t133 * t178
      t186 = t107 * t159
      t189 = log(0.4D1 * t186 * t73)
      t201 = t5 * t34
      t202 = t159 * t10
      t205 = log(0.4D1 * t202 * t13)
      t207 = t205 ** 2
      t222 = (0.90D2 * t6 * t7 * (-t20 * t28 / 0.2D1 - t33 * t34 / 0.2D1
     #) + (0.90D2 * t6 * t41 - 0.180D3 * t44 * t45) * (t19 * t28 + t32 *
     # t34) + (0.90D2 * t6 * t53 - 0.180D3 * t44 * pi * t41 + t64 * t45)
     # * (-t28 - t34)) * t70 / 0.2880D4 - (-0.180D3 * (t53 - t75 * t41 +
     # t77 * t7 / 0.2D1) * lh + t7 * (0.60D2 * lh * t61 - 0.240D3 * zeta
     #3 - 0.120D3 * t59 * lh) + 0.45D2 * t77 * t41 + 0.90D2 * t92 - 0.15
     #D2 * t77 * t75 * t7 - 0.90D2 * t75 * t53 + (t41 - t75 * t7) * t63)
     # * t5 * t104 / 0.2880D4 + (0.90D2 * t6 * (-(t41 - t112 * t7) * t28
     # - (t41 - t118 * t7) * t34) - t130) * t70 * t133 / 0.1440D4 + (-0.
     #90D2 * t6 * (t53 - t139 * t41 + t141 * t7 / 0.2D1) * t34 + 0.180D3
     # * t44 * pi * (t41 - t139 * t7) * t34 - t155) * t133 / 0.1440D4 + 
     #(0.90D2 * t6 * (-(t41 - t163 * t7) * t28 - (t41 - t169 * t7) * t34
     #) - t130) * t70 * t178 / 0.1440D4 + t6 * t127 * t183 / 0.8D1 + (-0
     #.90D2 * t6 * (t41 - t189 * t7) * t34 + 0.180D3 * t44 * t154) * t13
     #3 * t178 / 0.720D3 + (0.90D2 * t201 * pi * (-t53 + t205 * t41 - t2
     #07 * t7 / 0.2D1) - 0.180D3 * t44 * t104 * (-t41 + t205 * t7) - t15
     #5) * t178 / 0.1440D4
      t223 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t222)
      t225 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t222)
      t227 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t222)
      t229 = t2 * x1
      t230 = -0.1D1 + x1
      t231 = x1 * z
      t232 = 0.1D1 - x1 + t231
      t233 = 0.1D1 / t232
      t235 = t2 * t230 * t233
      t236 = t1 ** 2
      t237 = s * t236
      t239 = t230 * x1 * t233
      t240 = t237 * t239
      t241 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t235, t229, 0.0D0, -t240)
      t242 = t34 * t241
      t243 = t160 * t10
      t244 = t13 * t233
      t245 = t230 ** 2
      t246 = t244 * t245
      t249 = log(0.4D1 * t243 * t246)
      t251 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t235, t229, 0.0D0, -t240)
      t258 = log(-0.4D1 * t243 * t244 * t245 * t15)
      t262 = x3 * x1
      t263 = t262 * z
      t264 = 0.3D1 * t263
      t265 = 0.2D1 * t262
      t266 = x1 * t12
      t267 = x3 * t12
      t268 = t267 * x1
      t270 = 0.2D1 * t160 * z
      t271 = t160 * t12
      t272 = x3 * t232
      t274 = Sqrt(-t272 * t14)
      t278 = -z + t231 - t264 + t265 - x3 - t266 + t268 + t270 - t271 + 
     #0.2D1 * t21 * t274 * z - t160
      t279 = 0.1D1 / t278
      t287 = t34 * t251 + t232 * t251 * t279
      t298 = t186 * t10
      t301 = log(0.4D1 * t298 * t246)
      t307 = t104 * t251
      t314 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t235, t229, 0.0D0, -t240)
      t317 = log(0.4D1 * t202 * t246)
      t319 = t317 ** 2
      t335 = (0.90D2 * t6 * (t242 - t249 * t34 * t251 + (t232 * t241 - t
     #258 * t232 * t251) * t279) - 0.180D3 * t44 * pi * t287) * t70 * t1
     #78 / 0.1440D4 + t6 * t287 * t183 / 0.8D1 + (0.90D2 * t6 * (t242 - 
     #t301 * t34 * t251) - 0.180D3 * t44 * t307) * t133 * t178 / 0.720D3
     # + (0.90D2 * t201 * pi * (t314 - t317 * t241 + t319 * t251 / 0.2D1
     #) - 0.180D3 * t44 * t104 * (t241 - t317 * t251) + t64 * t307) * t1
     #78 / 0.1440D4
      t336 = FJET(XB1, XB2, s, 0.0D0, t229, -t235, 0.0D0, -t240, t335)
      t339 = x2 * t1 * s
      t340 = -0.1D1 + x2
      t342 = t340 * t1 * s
      t343 = x2 * z
      t345 = 0.1D1 / (-x2 + t343 - z)
      t346 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, t339, -t
     #342, 0.0D0, 0.0D0, 0.0D0)
      t347 = t345 * t346
      t348 = t73 * t340
      t351 = log(-0.4D1 * t108 * t348)
      t353 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t339, -t
     #342, 0.0D0, 0.0D0, 0.0D0)
      t359 = pi * t345 * t353
      t361 = 0.180D3 * t44 * t359
      t365 = (0.90D2 * t6 * (-t347 + t351 * t345 * t353) + t361) * t70 *
     # t133 / 0.1440D4
      t366 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, t339, -t
     #342, 0.0D0, 0.0D0, 0.0D0)
      t368 = t13 * t340
      t371 = log(-0.4D1 * t136 * t368)
      t372 = t371 * t345
      t374 = t371 ** 2
      t378 = -t345 * t366 + t372 * t346 - t374 * t345 * t353 / 0.2D1
      t382 = -t347 + t372 * t353
      t386 = t64 * t359
      t392 = t133 * t178
      t395 = t6 * t345 * t353 * t70 * t392 / 0.8D1
      t398 = log(-0.4D1 * t186 * t348)
      t407 = (0.90D2 * t6 * (-t347 + t398 * t345 * t353) + t361) * t133 
     #* t178 / 0.720D3
      t408 = t365 + (0.90D2 * t6 * t378 - 0.180D3 * t44 * pi * t382 - t3
     #86) * t133 / 0.1440D4 - t395 + t407
      t409 = FJET(XB1, XB2, s, 0.0D0, t339, 0.0D0, -t342, 0.0D0, t408)
      t411 = x2 * x3
      t414 = Sqrt(x3 * t340 * t14)
      t415 = t21 * t414
      t417 = 0.2D1 * t415 * x2
      t419 = 0.1D1 - x3 + t411
      t420 = 0.1D1 / t419
      t422 = t2 * (0.1D1 - x3 - x2 + t411 + t108 + t417) * t420
      t427 = t2 * x2 * (-0.1D1 + t411 + 0.2D1 * t415) * t420
      t428 = t411 * z
      t429 = t108 * z
      t435 = 0.1D1 / (x2 - t343 + z - t108 + x3 - t428 + t429 - t417 - 0
     #.2D1 * t415 * z + 0.2D1 * t415 * t343)
      t436 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, -t427, t
     #422, 0.0D0, 0.0D0, 0.0D0)
      t439 = t419 ** 2
      t445 = log(0.4D1 * t108 * t10 * t368 * t14 / t439)
      t447 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, -t427, t
     #422, 0.0D0, 0.0D0, 0.0D0)
      t449 = -t435 * t436 + t445 * t435 * t447
      t455 = 0.180D3 * t44 * pi * t435 * t447
      t464 = t6 * t435 * t447 * t70 * t392 / 0.8D1
      t465 = (0.90D2 * t6 * t449 + t455) * t70 * t133 / 0.1440D4 - t464
      t466 = FJET(XB1, XB2, s, 0.0D0, t422, 0.0D0, -t427, 0.0D0, t465)
      t469 = t1 * t230
      t471 = t340 * s * t469 * t233
      t473 = x2 * s * t469
      t475 = t237 * t340 * t239
      t476 = x2 * x1
      t477 = t476 * z
      t479 = 0.1D1 / (x2 + t477 - t476 + z - t343)
      t481 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, -t473, t
     #471, t229, 0.0D0, t475)
      t485 = t6 * t479 * t481 * t70 * t392 / 0.8D1
      t486 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, -t473, t
     #471, t229, 0.0D0, t475)
      t492 = log(-0.4D1 * t298 * t244 * t245 * t340)
      t495 = -t479 * t486 + t492 * t479 * t481
      t501 = 0.180D3 * t44 * pi * t479 * t481
      t506 = -t485 + (0.90D2 * t6 * t495 + t501) * t133 * t178 / 0.720D3
      t507 = FJET(XB1, XB2, s, 0.0D0, t471, t229, -t473, t475, t506)
      t509 = FJET(XB1, XB2, s, 0.0D0, -t342, 0.0D0, t339, 0.0D0, t408)
      t511 = FJET(XB1, XB2, s, 0.0D0, -t235, t229, 0.0D0, -t240, t335)
      t513 = FJET(XB1, XB2, s, 0.0D0, -t427, 0.0D0, t422, 0.0D0, t465)
      t515 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t222)
      t517 = FJET(XB1, XB2, s, t229, 0.0D0, 0.0D0, -t235, -t240, t335)
      t519 = t223 * t222 + t225 * t222 + t227 * t222 + t336 * t335 + t40
     #9 * t408 + t466 * t465 + t507 * t506 + t509 * t408 + t511 * t335 +
     # t513 * t465 + t515 * t222 + t517 * t335
      t520 = FJET(XB1, XB2, s, t229, -t473, 0.0D0, t471, t475, t506)
      t532 = t365 + (0.90D2 * t6 * t378 - 0.180D3 * t44 * pi * t382 - t3
     #86) * t133 / 0.1440D4 - t395 + t407
      t533 = FJET(XB1, XB2, s, t339, 0.0D0, -t342, 0.0D0, 0.0D0, t532)
      t535 = FJET(XB1, XB2, s, t422, 0.0D0, -t427, 0.0D0, 0.0D0, t465)
      t537 = FJET(XB1, XB2, s, t471, 0.0D0, -t473, t229, t475, t506)
      t540 = t229 * t411 * t420
      t541 = t2 * t230
      t542 = t108 * x1
      t543 = t108 * t231
      t546 = Sqrt(t272 * t340 * t14)
      t547 = t21 * t546
      t549 = 0.2D1 * t547 * x2
      t553 = t541 * (-t542 + t108 - x2 + t411 + t543 + t549 + 0.1D1 - x3
     #) * t233 * t420
      t557 = t14 * s * t1 * x1 * t420
      t563 = t541 * x2 * (-0.1D1 + t411 + x1 - t262 - t231 + t263 + 0.2D
     #1 * t547) * t233 * t420
      t564 = t159 * x2
      t571 = -z - x3 - t564 + t108 + 0.2D1 * t476 - x2 - t264 + t268 + t
     #270 - t271 + t549 + t428 - t429 - 0.3D1 * t477 + 0.2D1 * t547 * t4
     #77 + t543 + 0.2D1 * t411 * t231
      t575 = t12 * x2
      t590 = -t267 * t476 - 0.2D1 * t160 * t343 + t160 * t575 - 0.2D1 * 
     #t547 * t343 - 0.2D1 * t547 * t476 - t542 + 0.2D1 * t564 * z - t159
     # * t12 * x2 + t575 * x1 - t411 * x1 + t160 * x2 + 0.2D1 * t547 * z
     # + t231 + t265 - t266 - t160 + t343
      t592 = 0.1D1 / (t571 + t590)
      t595 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t563, -t
     #553, -t557, t540, t475)
      t599 = t6 * t232 * t592 * t595 * t70 * t392 / 0.8D1
      t600 = FJET(XB1, XB2, s, t540, -t553, -t557, t563, t475, -t599)
      t602 = pi * t232
      t605 = t592 * t595 * t183
      t608 = FJET(XB1, XB2, s, t563, -t557, -t553, t540, t475, -t599)
      t613 = FJET(XB1, XB2, s, -t342, 0.0D0, t339, 0.0D0, 0.0D0, t532)
      t615 = FJET(XB1, XB2, s, -t235, 0.0D0, 0.0D0, t229, -t240, t335)
      t624 = -t485 + (0.90D2 * t6 * t495 + t501) * t133 * t178 / 0.720D3
      t625 = FJET(XB1, XB2, s, -t473, t229, t471, 0.0D0, t475, t624)
      t634 = (0.90D2 * t6 * t449 + t455) * t70 * t133 / 0.1440D4 - t464
      t635 = FJET(XB1, XB2, s, -t427, 0.0D0, t422, 0.0D0, 0.0D0, t634)
      t637 = FJET(XB1, XB2, s, -t557, t563, t540, -t553, t475, -t599)
      t642 = FJET(XB1, XB2, s, -t553, t540, t563, -t557, t475, -t599)
      t647 = t520 * t506 + t533 * t532 + t535 * t465 + t537 * t506 - t60
     #0 * t5 * t602 * t605 / 0.8D1 - t608 * t5 * t602 * t605 / 0.8D1 + t
     #613 * t532 + t615 * t335 + t625 * t624 + t635 * t634 - t637 * t5 *
     # t602 * t605 / 0.8D1 - t642 * t5 * t602 * t605 / 0.8D1
      rrgg2qqbarhhardt5s1e0 = t519 + t647

      end function



      doubleprecision function rrgg2qqbarhhardt5s1em1
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = x3 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = -0.1D1 + x3
      t19 = log(-0.4D1 * t11 * t13 / t14)
      t20 = cos(t8)
      t22 = Sqrt(-x3 * t14)
      t27 = 0.1D1 / (-z - x3 + 0.2D1 * t20 * t22 * z)
      t31 = log(0.4D1 * t11 * t13)
      t32 = 0.1D1 / z
      t38 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t41 = lh * t5
      t42 = pi * t7
      t49 = 0.1D1 / x3
      t54 = log(0.4D1 * t13 * t10)
      t59 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t63 = t54 ** 2
      t66 = lh ** 2
      t68 = pi ** 2
      t74 = t32 * pi
      t80 = (-t7 * t32 - t7 * t27) * t49
      t81 = 0.1D1 / x2
      t85 = x2 ** 2
      t86 = t85 * t10
      t89 = log(0.4D1 * t86 * t13)
      t97 = 0.180D3 * t41 * t42 * t32
      t103 = 0.1D1 / x1
      t107 = t5 * t32
      t108 = x1 ** 2
      t109 = t108 * t10
      t112 = log(0.4D1 * t109 * t13)
      t124 = (0.90D2 * t6 * t7 * (t19 * t27 + t31 * t32) + (0.90D2 * t6 
     #* t38 - 0.180D3 * t41 * t42) * (-t27 - t32)) * t49 / 0.2880D4 - (-
     #0.180D3 * (t38 - t54 * t7) * lh + 0.90D2 * t59 - 0.90D2 * t54 * t3
     #8 + 0.45D2 * t63 * t7 + t7 * (0.180D3 * t66 - 0.30D2 * t68)) * t5 
     #* t74 / 0.2880D4 + t6 * t80 * t81 / 0.16D2 + (-0.90D2 * t6 * (t38 
     #- t89 * t7) * t32 + t97) * t81 / 0.1440D4 - t6 * t7 * t32 * t81 * 
     #t103 / 0.8D1 + (0.90D2 * t107 * pi * (-t38 + t112 * t7) + t97) * t
     #103 / 0.1440D4 + t6 * t80 * t103 / 0.16D2
      t125 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t124)
      t127 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t124)
      t129 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t124)
      t131 = t2 * x1
      t132 = -0.1D1 + x1
      t133 = x1 * z
      t134 = 0.1D1 - x1 + t133
      t135 = 0.1D1 / t134
      t137 = t2 * t132 * t135
      t138 = t1 ** 2
      t139 = s * t138
      t141 = t132 * x1 * t135
      t142 = t139 * t141
      t144 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t137, t131, 0.0D0, -t142)
      t149 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t137, t131, 0.0D0, -t142)
      t151 = t132 ** 2
      t155 = log(0.4D1 * t109 * t13 * t135 * t151)
      t169 = x3 * x1
      t176 = x3 * t108
      t182 = Sqrt(-x3 * t134 * t14)
      t186 = -z + t133 - 0.3D1 * t169 * z + 0.2D1 * t169 - x3 - x1 * t12
     # + x3 * t12 * x1 + 0.2D1 * t176 * z - t176 * t12 + 0.2D1 * t20 * t
     #182 * z - t176
      t194 = t6 * t32 * t144 * t81 * t103 / 0.8D1 + (0.90D2 * t107 * pi 
     #* (t149 - t155 * t144) - 0.180D3 * t41 * t74 * t144) * t103 / 0.14
     #40D4 + t6 * (t32 * t144 + t134 * t144 / t186) * t49 * t103 / 0.16D
     #2
      t195 = FJET(XB1, XB2, s, 0.0D0, t131, -t137, 0.0D0, -t142, t194)
      t198 = x2 * t1 * s
      t199 = -0.1D1 + x2
      t201 = t199 * t1 * s
      t202 = x2 * z
      t204 = 0.1D1 / (-x2 + t202 - z)
      t205 = t6 * t204
      t206 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t198, -t
     #201, 0.0D0, 0.0D0, 0.0D0)
      t210 = t205 * t206 * t49 * t81 / 0.16D2
      t211 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, t198, -t
     #201, 0.0D0, 0.0D0, 0.0D0)
      t216 = log(-0.4D1 * t86 * t13 * t199)
      t219 = -t204 * t211 + t216 * t204 * t206
      t225 = 0.180D3 * t41 * pi * t204 * t206
      t232 = t205 * t206 * t81 * t103 / 0.8D1
      t233 = -t210 + (0.90D2 * t6 * t219 + t225) * t81 / 0.1440D4 - t232
      t234 = FJET(XB1, XB2, s, 0.0D0, t198, 0.0D0, -t201, 0.0D0, t233)
      t236 = x2 * x3
      t237 = t85 * x3
      t240 = Sqrt(x3 * t199 * t14)
      t241 = t20 * t240
      t243 = 0.2D1 * t241 * x2
      t246 = 0.1D1 / (0.1D1 - x3 + t236)
      t248 = t2 * (0.1D1 - x3 - x2 + t236 + t237 + t243) * t246
      t253 = t2 * x2 * (-0.1D1 + t236 + 0.2D1 * t241) * t246
      t261 = 0.1D1 / (x2 - t202 + z - t237 + x3 - t236 * z + t237 * z - 
     #t243 - 0.2D1 * t241 * z + 0.2D1 * t241 * t202)
      t263 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, -t253, t
     #248, 0.0D0, 0.0D0, 0.0D0)
      t267 = t6 * t261 * t263 * t49 * t81 / 0.16D2
      t268 = FJET(XB1, XB2, s, 0.0D0, t248, 0.0D0, -t253, 0.0D0, -t267)
      t273 = t261 * t263 * t49 * t81
      t277 = t1 * t132
      t279 = t199 * s * t277 * t135
      t281 = x2 * s * t277
      t283 = t139 * t199 * t141
      t284 = x2 * x1
      t287 = 0.1D1 / (x2 + t284 * z - t284 + z - t202)
      t289 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, -t281, t
     #279, t131, 0.0D0, t283)
      t293 = t6 * t287 * t289 * t81 * t103 / 0.8D1
      t294 = FJET(XB1, XB2, s, 0.0D0, t279, t131, -t281, t283, -t293)
      t299 = t287 * t289 * t81 * t103
      t302 = FJET(XB1, XB2, s, 0.0D0, -t201, 0.0D0, t198, 0.0D0, t233)
      t304 = FJET(XB1, XB2, s, 0.0D0, -t137, t131, 0.0D0, -t142, t194)
      t306 = FJET(XB1, XB2, s, 0.0D0, -t253, 0.0D0, t248, 0.0D0, -t267)
      t311 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t124)
      t313 = FJET(XB1, XB2, s, t131, 0.0D0, 0.0D0, -t137, -t142, t194)
      t315 = FJET(XB1, XB2, s, t131, -t281, 0.0D0, t279, t283, -t293)
      t326 = -t210 + (0.90D2 * t6 * t219 + t225) * t81 / 0.1440D4 - t232
      t327 = FJET(XB1, XB2, s, t198, 0.0D0, -t201, 0.0D0, 0.0D0, t326)
      t329 = FJET(XB1, XB2, s, t248, 0.0D0, -t253, 0.0D0, 0.0D0, -t267)
      t334 = FJET(XB1, XB2, s, t279, 0.0D0, -t281, t131, t283, -t293)
      t339 = FJET(XB1, XB2, s, -t201, 0.0D0, t198, 0.0D0, 0.0D0, t326)
      t341 = FJET(XB1, XB2, s, -t137, 0.0D0, 0.0D0, t131, -t142, t194)
      t343 = FJET(XB1, XB2, s, -t281, t131, t279, 0.0D0, t283, -t293)
      t348 = FJET(XB1, XB2, s, -t253, 0.0D0, t248, 0.0D0, 0.0D0, -t267)
      rrgg2qqbarhhardt5s1em1 = t125 * t124 + t127 * t124 + t129 * t124 +
     # t195 * t194 + t234 * t233 - t268 * t5 * pi * t273 / 0.16D2 - t294
     # * t5 * pi * t299 / 0.8D1 + t302 * t233 + t304 * t194 - t306 * t5 
     #* pi * t273 / 0.16D2 + t311 * t124 + t313 * t194 - t315 * t5 * pi 
     #* t299 / 0.8D1 + t327 * t326 - t329 * t5 * pi * t273 / 0.16D2 - t3
     #34 * t5 * pi * t299 / 0.8D1 + t339 * t326 + t341 * t194 - t343 * t
     #5 * pi * t299 / 0.8D1 - t348 * t5 * pi * t273 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt5s1em2
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = cos(t8)
      t12 = Sqrt(-x3 * (-0.1D1 + x3))
      t18 = 0.1D1 / z
      t25 = t7 * t18
      t26 = 0.1D1 / x2
      t30 = 0.1D1 / x1
      t36 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2
     #, 0.0D0, 0.0D0, 0.0D0)
      t38 = z ** 2
      t40 = Sin(t8)
      t41 = t40 ** 2
      t44 = log(0.4D1 / t38 * t41)
      t52 = t6 * t7 * (-0.1D1 / (-z - x3 + 0.2D1 * t9 * t12 * z) - t18) 
     #/ x3 / 0.32D2 - t6 * t25 * t26 / 0.16D2 - t6 * t25 * t30 / 0.16D2 
     #- (-0.180D3 * t7 * lh + 0.90D2 * t36 - 0.90D2 * t44 * t7) * t5 * t
     #18 * pi / 0.2880D4
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t52)
      t55 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t52)
      t57 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t52)
      t59 = t2 * x1
      t60 = -0.1D1 + x1
      t63 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t65 = t2 * t60 * t63
      t66 = t1 ** 2
      t70 = s * t66 * t60 * x1 * t63
      t71 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #65, t59, 0.0D0, -t70)
      t73 = t18 * t71 * t30
      t75 = t6 * t73 / 0.16D2
      t76 = FJET(XB1, XB2, s, 0.0D0, t59, -t65, 0.0D0, -t70, t75)
      t82 = x2 * t1 * s
      t85 = (-0.1D1 + x2) * t1 * s
      t89 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t82, -t85
     #, 0.0D0, 0.0D0, 0.0D0)
      t91 = 0.1D1 / (-x2 + x2 * z - z) * t89 * t26
      t93 = t6 * t91 / 0.16D2
      t94 = FJET(XB1, XB2, s, 0.0D0, t82, 0.0D0, -t85, 0.0D0, -t93)
      t99 = FJET(XB1, XB2, s, 0.0D0, -t85, 0.0D0, t82, 0.0D0, -t93)
      t104 = FJET(XB1, XB2, s, 0.0D0, -t65, t59, 0.0D0, -t70, t75)
      t109 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t52)
      t111 = FJET(XB1, XB2, s, t59, 0.0D0, 0.0D0, -t65, -t70, t75)
      t116 = FJET(XB1, XB2, s, t82, 0.0D0, -t85, 0.0D0, 0.0D0, -t93)
      t121 = FJET(XB1, XB2, s, -t85, 0.0D0, t82, 0.0D0, 0.0D0, -t93)
      t126 = FJET(XB1, XB2, s, -t65, 0.0D0, 0.0D0, t59, -t70, t75)
      rrgg2qqbarhhardt5s1em2 = t53 * t52 + t55 * t52 + t57 * t52 + t76 *
     # t5 * pi * t73 / 0.16D2 - t94 * t5 * pi * t91 / 0.16D2 - t99 * t5 
     #* pi * t91 / 0.16D2 + t104 * t5 * pi * t73 / 0.16D2 + t109 * t52 +
     # t111 * t5 * pi * t73 / 0.16D2 - t116 * t5 * pi * t91 / 0.16D2 - t
     #121 * t5 * pi * t91 / 0.16D2 + t126 * t5 * pi * t73 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt5s1em3
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2,
     # 0.0D0, 0.0D0, 0.0D0)
      t8 = 0.1D1 / z
      t11 = t5 * pi * t7 * t8 / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = pi * t7 * t8
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2qqbarhhardt5s1em3 = -t12 * t5 * t15 / 0.32D2 - t17 * t5 * t15
     # / 0.32D2 - t20 * t5 * t15 / 0.32D2 - t23 * t5 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarhhardt5s1em4
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt5s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhhardt5s2e1
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
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
      t23 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t25 = t22 ** 2
      t26 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t30 = cos(t10)
      t31 = x3 * z
      t33 = Sqrt(-t31 * t17)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t33)
      t39 = 0.1D1 / z
      t40 = t39 * t7
      t43 = log(0.4D1 * t9 * t16)
      t44 = t43 * t39
      t46 = t43 ** 2
      t53 = lh * t5
      t57 = t39 * t23
      t63 = lh ** 2
      t65 = pi ** 2
      t67 = 0.180D3 * t63 - 0.30D2 * t65
      t68 = t67 * t5
      t69 = t39 * t26
      t75 = 0.1D1 / x3
      t77 = 0.1D1 / x1
      t80 = t39 * pi
      t81 = t8 * t12
      t82 = t81 * t15
      t84 = log(0.4D1 * t82)
      t89 = t5 * t39
      t90 = t84 ** 2
      t93 = rrgg2qqbarhhard51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t107 = 0.60D2 * lh * t65 - 0.240D3 * zeta3 - 0.120D3 * t63 * lh
      t108 = t107 * t5
      t121 = x2 ** 2
      t122 = x3 * t121
      t125 = log(0.4D1 * t122 * t82)
      t128 = t122 * t8
      t131 = log(-0.4D1 * t128 * t19)
      t135 = -0.1D1 + x2
      t136 = t16 * t135
      t139 = log(-0.4D1 * t128 * t136)
      t145 = pi * t26
      t146 = t145 * t37
      t151 = 0.1D1 / x2
      t152 = t151 * t77
      t155 = t121 * t8
      t158 = log(-0.4D1 * t155 * t136)
      t159 = t158 * t39
      t161 = t158 ** 2
      t167 = log(0.4D1 * t155 * t16)
      t168 = t167 * t39
      t170 = t167 ** 2
      t189 = pi * t23
      t194 = x3 * t15
      t198 = log(-0.4D1 * t194 * t12 * t18)
      t202 = log(0.4D1 * t194 * t12)
      t206 = t202 ** 2
      t209 = t198 ** 2
      t242 = log(-0.4D1 * t122 * t136)
      t243 = t242 * t39
      t245 = t242 ** 2
      t251 = log(-0.4D1 * t122 * t19)
      t253 = t251 ** 2
      t260 = log(0.4D1 * t122 * t16)
      t261 = t260 * t39
      t263 = t260 ** 2
      t289 = t15 * t121
      t292 = log(0.4D1 * t289 * t12)
      t293 = t292 ** 2
      t294 = t12 * t135
      t297 = log(-0.4D1 * t289 * t294)
      t298 = t297 ** 2
      t323 = log(0.4D1 * t16)
      t324 = t323 ** 2
      t325 = t324 * t39
      t330 = t324 * t323 * t39
      t333 = t323 * t39
      t346 = t65 ** 2
      t347 = t63 ** 2
      t355 = rrgg2qqbarhhard51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t364 = t324 ** 2
      t372 = (0.90D2 * t6 * (-(t7 - t22 * t23 + t25 * t26 / 0.2D1) * t37
     # - t40 + t44 * t23 - t46 * t39 * t26 / 0.2D1) - 0.180D3 * t53 * pi
     # * (-(t23 - t22 * t26) * t37 - t57 + t44 * t26) + t68 * pi * (-t69
     # - t26 * t37)) * t75 * t77 / 0.1440D4 - (t68 * t80 * (t23 - t84 * 
     #t26) + 0.90D2 * t89 * pi * (t90 * t23 / 0.2D1 + t93 - t90 * t84 * 
     #t26 / 0.6D1 - t84 * t7) + t108 * t80 * t26 - 0.180D3 * t53 * t80 *
     # (t7 - t84 * t23 + t90 * t26 / 0.2D1)) * t77 / 0.1440D4 - (0.90D2 
     #* t6 * (-t125 * t39 * t26 + (t23 - t131 * t26) * t37 + t139 * t39 
     #* t26) - 0.180D3 * t53 * t146) * t75 * t152 / 0.720D3 + (0.90D2 * 
     #t6 * (-t159 * t23 + t161 * t39 * t26 / 0.2D1 + t168 * t23 - t170 *
     # t39 * t26 / 0.2D1) - 0.180D3 * t53 * pi * (-t159 * t26 + t168 * t
     #26)) * t151 * t77 / 0.720D3 + ((0.90D2 * t6 * t7 - 0.180D3 * t53 *
     # t189 + t68 * t145) * (t198 * t37 + t202 * t39) + 0.90D2 * t6 * t2
     #6 * (t206 * t202 * t39 / 0.6D1 + t209 * t198 * t37 / 0.6D1) + (t68
     # * t189 + 0.90D2 * t6 * t93 + t108 * t145 - 0.180D3 * t53 * pi * t
     #7) * (-t37 - t39) + (0.90D2 * t6 * t23 - 0.180D3 * t53 * t145) * (
     #-t209 * t37 / 0.2D1 - t206 * t39 / 0.2D1)) * t75 / 0.2880D4 - (0.9
     #0D2 * t6 * (t243 * t23 - t245 * t39 * t26 / 0.2D1 + (t7 - t251 * t
     #23 + t253 * t26 / 0.2D1) * t37 - t261 * t23 + t263 * t39 * t26 / 0
     #.2D1) - 0.180D3 * t53 * pi * (t243 * t26 + (t23 - t251 * t26) * t3
     #7 - t261 * t26) + t68 * t146) * t75 * t151 / 0.1440D4 - ((-0.180D3
     # * t69 * lh + 0.90D2 * t57) * t5 * pi * (t293 / 0.2D1 - t298 / 0.2
     #D1) + 0.90D2 * t69 * t6 * (t298 * t297 / 0.6D1 - t293 * t292 / 0.6
     #D1) + (-0.180D3 * t57 * lh + 0.90D2 * t40 + t69 * t67) * t5 * pi *
     # (-t292 + t297)) * t151 / 0.1440D4 - (-0.180D3 * (t325 * t23 / 0.2
     #D1 + t39 * t93 - t330 * t26 / 0.6D1 - t333 * t7) * lh + (t40 - t33
     #3 * t23 + t325 * t26 / 0.2D1) * t67 + (t57 - t333 * t26) * t107 + 
     #t69 * (t346 + 0.60D2 * t347 + 0.480D3 * lh * zeta3 - 0.60D2 * t63 
     #* t65) + 0.90D2 * t39 * t355 - 0.15D2 * t330 * t23 + 0.45D2 * t325
     # * t7 - 0.90D2 * t333 * t93 + 0.15D2 / 0.4D1 * t364 * t39 * t26) *
     # t5 * pi / 0.2880D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t372)
      t375 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t372)
      t377 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t372)
      t380 = x1 * z
      t381 = -z - x1 + t380
      t382 = 0.1D1 / t381
      t384 = t2 * x1 * t135 * t382
      t385 = -0.1D1 + x1
      t386 = t2 * t385
      t389 = x2 * s * t1 * x1
      t390 = t1 ** 2
      t391 = s * t390
      t394 = t385 * x1 * t382
      t395 = t391 * t135 * t394
      t396 = x2 * x1
      t397 = t396 * z
      t399 = 0.1D1 / (-z - t396 + t397)
      t400 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, t389, t3
     #84, -t386, 0.0D0, -t395)
      t401 = t399 * t400
      t402 = t122 * t81
      t403 = 0.1D1 / t13
      t404 = t403 * t382
      t405 = t385 ** 2
      t407 = t404 * t405 * t135
      t410 = log(0.4D1 * t402 * t407)
      t412 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t389, t3
     #84, -t386, 0.0D0, -t395)
      t418 = pi * t399 * t412
      t424 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, t389, t3
     #84, -t386, 0.0D0, -t395)
      t426 = t155 * t12
      t429 = log(0.4D1 * t426 * t407)
      t430 = t429 * t399
      t432 = t429 ** 2
      t449 = -(0.90D2 * t6 * (-t401 + t410 * t399 * t412) + 0.180D3 * t5
     #3 * t418) * t75 * t152 / 0.720D3 + (0.90D2 * t6 * (t399 * t424 - t
     #430 * t400 + t432 * t399 * t412 / 0.2D1) - 0.180D3 * t53 * pi * (t
     #401 - t430 * t412) + t68 * t418) * t151 * t77 / 0.720D3
      t450 = FJET(XB1, XB2, s, 0.0D0, t384, -t386, t389, -t395, t449)
      t452 = x2 * x3
      t453 = 0.1D1 - x3 + t452
      t454 = 0.1D1 / t453
      t455 = t452 * t454
      t456 = t2 * t455
      t458 = t2 * t17 * t454
      t459 = t135 * t17
      t461 = Sqrt(t31 * t459)
      t465 = 0.1D1 / (-z - x3 + t452 + 0.2D1 * t30 * t461)
      t466 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t458, t456, 0.0D0)
      t469 = t453 ** 2
      t470 = 0.1D1 / t469
      t471 = t17 * t470
      t475 = log(0.4D1 * t122 * t15 * t294 * t471)
      t476 = t475 * t465
      t477 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t458, t456, 0.0D0)
      t479 = t475 ** 2
      t481 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t458, t456, 0.0D0)
      t484 = -t465 * t466 + t476 * t477 - t479 * t465 * t481 / 0.2D1
      t487 = t465 * t477
      t489 = -t487 + t476 * t481
      t494 = pi * t465 * t481
      t495 = t68 * t494
      t504 = log(0.4D1 * t402 * t15 * t135 * t471)
      t515 = (0.90D2 * t6 * (-t487 + t504 * t465 * t481) + 0.180D3 * t53
     # * t494) * t75 * t152 / 0.720D3
      t516 = -(0.90D2 * t6 * t484 - 0.180D3 * t53 * pi * t489 - t495) * 
     #t75 * t151 / 0.1440D4 - t515
      t517 = FJET(XB1, XB2, s, 0.0D0, t456, 0.0D0, -t458, 0.0D0, t516)
      t520 = t2 * x1 * t382
      t521 = t391 * t394
      t522 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t520, -t386, 0.0D0, t521)
      t523 = t39 * t522
      t524 = t9 * t12
      t525 = t404 * t405
      t528 = log(-0.4D1 * t524 * t525)
      t529 = t528 * t39
      t530 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t520, -t386, 0.0D0, t521)
      t532 = t528 ** 2
      t534 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t520, -t386, 0.0D0, t521)
      t539 = t404 * t405 * t18
      t542 = log(0.4D1 * t524 * t539)
      t543 = t542 * t381
      t545 = t542 ** 2
      t550 = x3 * x1
      t551 = t550 * z
      t553 = 0.2D1 * t9 * z
      t554 = t9 * t13
      t555 = x1 * t13
      t556 = x3 * t13
      t557 = t556 * x1
      t558 = x3 * t381
      t560 = Sqrt(t558 * t17)
      t565 = 0.1D1 / (-t380 - t551 - t9 + t553 - t554 + t555 + t557 - t3
     #1 + 0.2D1 * t30 * t560 * z - t13)
      t570 = t39 * t530
      t572 = t381 * t530
      t583 = t39 * t534 - t381 * t534 * t565
      t589 = (0.90D2 * t6 * (t523 - t529 * t530 + t532 * t39 * t534 / 0.
     #2D1 - (t381 * t522 - t543 * t530 + t545 * t381 * t534 / 0.2D1) * t
     #565) - 0.180D3 * t53 * pi * (t570 - t529 * t534 - (t572 - t543 * t
     #534) * t565) + t68 * pi * t583) * t75 * t77 / 0.1440D4
      t592 = log(-0.4D1 * t81 * t525)
      t594 = -t530 + t592 * t534
      t597 = t592 ** 2
      t600 = rrgg2qqbarhhard51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t520, -t386, 0.0D0, t521)
      t605 = -t597 * t530 / 0.2D1 - t600 + t597 * t592 * t534 / 0.6D1 + 
     #t592 * t522
      t609 = t80 * t534
      t610 = t108 * t609
      t614 = -t522 + t592 * t530 - t597 * t534 / 0.2D1
      t623 = log(0.4D1 * t402 * t539)
      t629 = t382 * t405
      t633 = log(-0.4D1 * t128 * t12 * t403 * t629)
      t646 = (0.90D2 * t6 * ((t572 - t623 * t381 * t534) * t565 - t570 +
     # t633 * t39 * t534) + 0.180D3 * t53 * pi * t583) * t75 * t152 / 0.
     #720D3
      t649 = log(-0.4D1 * t426 * t525)
      t650 = t649 * t39
      t652 = t649 ** 2
      t668 = (0.90D2 * t6 * (t523 - t650 * t530 + t652 * t39 * t534 / 0.
     #2D1) - 0.180D3 * t53 * pi * (t570 - t650 * t534) + t68 * t609) * t
     #151 * t77 / 0.720D3
      t669 = t589 - (t68 * t80 * t594 + 0.90D2 * t89 * pi * t605 - t610 
     #- 0.180D3 * t53 * t80 * t614) * t77 / 0.1440D4 - t646 + t668
      t670 = FJET(XB1, XB2, s, 0.0D0, -t386, -t520, 0.0D0, t521, t669)
      t672 = FJET(XB1, XB2, s, 0.0D0, -t520, -t386, 0.0D0, t521, t669)
      t674 = FJET(XB1, XB2, s, 0.0D0, -t458, 0.0D0, t456, 0.0D0, t516)
      t676 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t372)
      t678 = FJET(XB1, XB2, s, t389, -t386, t384, 0.0D0, -t395, t449)
      t680 = FJET(XB1, XB2, s, t384, 0.0D0, t389, -t386, -t395, t449)
      t693 = -(0.90D2 * t6 * t484 - 0.180D3 * t53 * pi * t489 - t495) * 
     #t75 * t151 / 0.1440D4 - t515
      t694 = FJET(XB1, XB2, s, t456, 0.0D0, -t458, 0.0D0, 0.0D0, t693)
      t699 = t17 * s * t1 * t385 * t454
      t700 = t2 * x1
      t702 = Sqrt(-t558 * t459)
      t703 = t30 * t702
      t709 = t700 * x2 * (-x3 + t452 - z + t31 - x1 + t550 + t380 - t551
     # + 0.2D1 * t703) * t382 * t454
      t710 = t386 * t455
      t713 = t122 * x1
      t715 = t122 * t380
      t719 = t700 * (0.2D1 * t703 * x2 + t713 + t122 * z - x2 + t452 - t
     #715 + 0.1D1 - x3) * t382 * t454
      t728 = t13 * x2
      t732 = t8 * x2
      t739 = -0.2D1 * t703 * t396 - 0.2D1 * t452 * t380 + t556 * t396 + 
     #0.2D1 * t9 * x2 * z - t9 * t728 + t397 + 0.2D1 * t703 * t397 + t71
     #5 + t13 - 0.2D1 * t732 * z + t8 * t13 * x2 - t728 * x1 - t452 * z
      t744 = t551 - t553 + t554 - t557 - t713 + t452 * x1 - t9 * x2 - 0.
     #2D1 * t703 * z + t9 - t555 + t31 + t732 + t380
      t746 = 0.1D1 / (t739 + t744)
      t747 = t381 * t746
      t748 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, t709, -t
     #719, t699, -t710, -t395)
      t756 = log(-0.4D1 * t122 * t81 * t403 * t629 * t459 * t470)
      t758 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t709, -t
     #719, t699, -t710, -t395)
      t768 = 0.90D2 * t6 * (t747 * t748 - t756 * t381 * t746 * t758) - 0
     #.180D3 * t53 * pi * t747 * t758
      t771 = t768 * t75 * t152 / 0.720D3
      t772 = FJET(XB1, XB2, s, t699, t709, -t710, -t719, -t395, -t771)
      t775 = t75 * t151 * t77
      t778 = FJET(XB1, XB2, s, t709, t699, -t719, -t710, -t395, -t771)
      t796 = t589 - (t68 * t80 * t594 + 0.90D2 * t89 * pi * t605 - t610 
     #- 0.180D3 * t53 * t80 * t614) * t77 / 0.1440D4 - t646 + t668
      t797 = FJET(XB1, XB2, s, -t386, 0.0D0, 0.0D0, -t520, t521, t796)
      t799 = FJET(XB1, XB2, s, -t386, t389, 0.0D0, t384, -t395, t449)
      t801 = FJET(XB1, XB2, s, -t520, 0.0D0, 0.0D0, -t386, t521, t796)
      t803 = FJET(XB1, XB2, s, -t458, 0.0D0, t456, 0.0D0, 0.0D0, t693)
      t805 = FJET(XB1, XB2, s, -t719, -t710, t709, t699, -t395, -t771)
      t809 = FJET(XB1, XB2, s, -t710, -t719, t699, t709, -t395, -t771)
      rrgg2qqbarhhardt5s2e1 = t373 * t372 + t375 * t372 + t377 * t372 + 
     #t450 * t449 + t517 * t516 + t670 * t669 + t672 * t669 + t674 * t51
     #6 + t676 * t372 + t678 * t449 + t680 * t449 + t694 * t693 - t772 *
     # t768 * t775 / 0.720D3 - t778 * t768 * t775 / 0.720D3 + t797 * t79
     #6 + t799 * t449 + t801 * t796 + t803 * t693 - t805 * t768 * t775 /
     # 0.720D3 - t809 * t768 * t775 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarhhardt5s2e0
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
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
      t23 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t26 = cos(t10)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t17)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t35 = 0.1D1 / z
      t36 = t35 * t7
      t39 = log(0.4D1 * t9 * t16)
      t45 = lh * t5
      t46 = t35 * t23
      t53 = 0.1D1 / x3
      t55 = 0.1D1 / x1
      t60 = 0.1D1 / x2
      t61 = t60 * t55
      t65 = x2 ** 2
      t66 = t65 * t8
      t67 = -0.1D1 + x2
      t68 = t16 * t67
      t71 = log(-0.4D1 * t66 * t68)
      t76 = log(0.4D1 * t66 * t16)
      t84 = t5 * t35
      t85 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t86 = t8 * t12
      t89 = log(0.4D1 * t86 * t15)
      t91 = t89 ** 2
      t98 = t35 * pi
      t104 = lh ** 2
      t106 = pi ** 2
      t108 = 0.180D3 * t104 - 0.30D2 * t106
      t109 = t108 * t5
      t115 = x3 * t15
      t119 = log(-0.4D1 * t115 * t12 * t18)
      t120 = t119 ** 2
      t124 = log(0.4D1 * t115 * t12)
      t125 = t124 ** 2
      t134 = pi * t23
      t156 = log(0.4D1 * t16)
      t157 = t156 * t35
      t159 = t156 ** 2
      t160 = t159 * t35
      t175 = rrgg2qqbarhhard51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t191 = x3 * t65
      t194 = log(-0.4D1 * t191 * t68)
      t199 = log(-0.4D1 * t191 * t19)
      t205 = log(0.4D1 * t191 * t16)
      t218 = t15 * t65
      t221 = log(0.4D1 * t218 * t12)
      t222 = t221 ** 2
      t223 = t12 * t67
      t226 = log(-0.4D1 * t218 * t223)
      t227 = t226 ** 2
      t244 = (0.90D2 * t6 * (-(t7 - t22 * t23) * t33 - t36 + t39 * t35 *
     # t23) - 0.180D3 * t45 * pi * (-t46 - t23 * t33)) * t53 * t55 / 0.1
     #440D4 - t6 * t23 * t33 * t53 * t61 / 0.8D1 + t6 * (-t71 * t35 * t2
     #3 + t76 * t35 * t23) * t60 * t55 / 0.8D1 - (0.90D2 * t84 * pi * (t
     #85 - t89 * t7 + t91 * t23 / 0.2D1) - 0.180D3 * t45 * t98 * (t7 - t
     #89 * t23) + t109 * t98 * t23) * t55 / 0.1440D4 + (0.90D2 * t6 * t2
     #3 * (-t120 * t33 / 0.2D1 - t125 * t35 / 0.2D1) + (0.90D2 * t6 * t7
     # - 0.180D3 * t45 * t134) * (t119 * t33 + t124 * t35) + (0.90D2 * t
     #6 * t85 - 0.180D3 * t45 * pi * t7 + t109 * t134) * (-t33 - t35)) *
     # t53 / 0.2880D4 - (-0.180D3 * (t35 * t85 - t157 * t7 + t160 * t23 
     #/ 0.2D1) * lh + t46 * (0.60D2 * lh * t106 - 0.240D3 * zeta3 - 0.12
     #0D3 * t104 * lh) + 0.45D2 * t160 * t7 + 0.90D2 * t35 * t175 - 0.15
     #D2 * t159 * t156 * t35 * t23 - 0.90D2 * t157 * t85 + (t36 - t157 *
     # t23) * t108) * t5 * pi / 0.2880D4 - (0.90D2 * t6 * (t194 * t35 * 
     #t23 + (t7 - t199 * t23) * t33 - t205 * t35 * t23) - 0.180D3 * t45 
     #* t134 * t33) * t53 * t60 / 0.1440D4 - (0.90D2 * t46 * t6 * (t222 
     #/ 0.2D1 - t227 / 0.2D1) + (-0.180D3 * t46 * lh + 0.90D2 * t36) * t
     #5 * pi * (-t221 + t226)) * t60 / 0.1440D4
      t245 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t244)
      t247 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t244)
      t249 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t244)
      t252 = x1 * z
      t253 = -z - x1 + t252
      t254 = 0.1D1 / t253
      t256 = t2 * x1 * t67 * t254
      t257 = -0.1D1 + x1
      t258 = t2 * t257
      t261 = x2 * s * t1 * x1
      t262 = t1 ** 2
      t263 = s * t262
      t266 = t257 * x1 * t254
      t267 = t263 * t67 * t266
      t268 = x2 * x1
      t269 = t268 * z
      t271 = 0.1D1 / (-z - t268 + t269)
      t273 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t261, t2
     #56, -t258, 0.0D0, -t267)
      t278 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, t261, t2
     #56, -t258, 0.0D0, -t267)
      t280 = t66 * t12
      t282 = 0.1D1 / t13 * t254
      t283 = t257 ** 2
      t288 = log(0.4D1 * t280 * t282 * t283 * t67)
      t302 = t6 * t271 * t273 * t53 * t61 / 0.8D1 + (0.90D2 * t6 * (t271
     # * t278 - t288 * t271 * t273) - 0.180D3 * t45 * pi * t271 * t273) 
     #* t60 * t55 / 0.720D3
      t303 = FJET(XB1, XB2, s, 0.0D0, t256, -t258, t261, -t267, t302)
      t305 = x2 * x3
      t306 = 0.1D1 - x3 + t305
      t307 = 0.1D1 / t306
      t308 = t305 * t307
      t309 = t2 * t308
      t311 = t2 * t17 * t307
      t312 = t67 * t17
      t314 = Sqrt(t27 * t312)
      t318 = 0.1D1 / (-z - x3 + t305 + 0.2D1 * t26 * t314)
      t320 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t311, t309, 0.0D0)
      t324 = t6 * t318 * t320 * t53 * t61 / 0.8D1
      t325 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t311, t309, 0.0D0)
      t328 = t306 ** 2
      t334 = log(0.4D1 * t191 * t15 * t223 * t17 / t328)
      t337 = -t318 * t325 + t334 * t318 * t320
      t343 = 0.180D3 * t45 * pi * t318 * t320
      t348 = t324 - (0.90D2 * t6 * t337 + t343) * t53 * t60 / 0.1440D4
      t349 = FJET(XB1, XB2, s, 0.0D0, t309, 0.0D0, -t311, 0.0D0, t348)
      t352 = t2 * x1 * t254
      t353 = t263 * t266
      t354 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t352, -t258, 0.0D0, t353)
      t355 = t35 * t354
      t356 = t9 * t12
      t357 = t282 * t283
      t360 = log(-0.4D1 * t356 * t357)
      t362 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t352, -t258, 0.0D0, t353)
      t369 = log(0.4D1 * t356 * t282 * t283 * t18)
      t373 = x3 * x1
      t374 = t373 * z
      t376 = 0.2D1 * t9 * z
      t377 = t9 * t13
      t378 = x1 * t13
      t379 = x3 * t13
      t380 = t379 * x1
      t381 = x3 * t253
      t383 = Sqrt(t381 * t17)
      t388 = 0.1D1 / (-t252 - t374 - t9 + t376 - t377 + t378 + t380 - t2
     #7 + 0.2D1 * t26 * t383 * z - t13)
      t396 = t35 * t362 - t253 * t362 * t388
      t403 = (0.90D2 * t6 * (t355 - t360 * t35 * t362 - (t253 * t354 - t
     #369 * t253 * t362) * t388) - 0.180D3 * t45 * pi * t396) * t53 * t5
     #5 / 0.1440D4
      t407 = t53 * t60 * t55
      t409 = -t6 * t396 * t407 / 0.8D1
      t412 = log(-0.4D1 * t280 * t357)
      t418 = t98 * t362
      t424 = (0.90D2 * t6 * (t355 - t412 * t35 * t362) - 0.180D3 * t45 *
     # t418) * t60 * t55 / 0.720D3
      t425 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t352, -t258, 0.0D0, t353)
      t428 = log(-0.4D1 * t86 * t357)
      t430 = t428 ** 2
      t433 = -t425 + t428 * t354 - t430 * t362 / 0.2D1
      t438 = -t354 + t428 * t362
      t442 = t109 * t418
      t446 = t403 - t409 + t424 - (0.90D2 * t84 * pi * t433 - 0.180D3 * 
     #t45 * t98 * t438 - t442) * t55 / 0.1440D4
      t447 = FJET(XB1, XB2, s, 0.0D0, -t258, -t352, 0.0D0, t353, t446)
      t449 = FJET(XB1, XB2, s, 0.0D0, -t352, -t258, 0.0D0, t353, t446)
      t451 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t309, 0.0D0, t348)
      t453 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t244)
      t455 = FJET(XB1, XB2, s, t261, -t258, t256, 0.0D0, -t267, t302)
      t457 = FJET(XB1, XB2, s, t256, 0.0D0, t261, -t258, -t267, t302)
      t466 = t324 - (0.90D2 * t6 * t337 + t343) * t53 * t60 / 0.1440D4
      t467 = FJET(XB1, XB2, s, t309, 0.0D0, -t311, 0.0D0, 0.0D0, t466)
      t472 = t17 * s * t1 * t257 * t307
      t473 = t2 * x1
      t475 = Sqrt(-t381 * t312)
      t476 = t26 * t475
      t482 = t473 * x2 * (-x3 + t305 - z + t27 - x1 + t373 + t252 - t374
     # + 0.2D1 * t476) * t254 * t307
      t483 = t258 * t308
      t486 = t191 * x1
      t488 = t191 * t252
      t492 = t473 * (0.2D1 * t476 * x2 + t486 + t191 * z - x2 + t305 - t
     #488 + 0.1D1 - x3) * t254 * t307
      t499 = 0.2D1 * t476 * t269 + t269 + t374 - t376 + t377 - t380 + t2
     #52 + t9 - t378 + t27 + t488 - 0.2D1 * t476 * t268 - 0.2D1 * t305 *
     # t252
      t504 = t13 * x2
      t506 = t8 * x2
      t517 = t379 * t268 + 0.2D1 * t9 * x2 * z - t9 * t504 - t486 - 0.2D
     #1 * t506 * z + t8 * t13 * x2 - t504 * x1 - t305 * z + t305 * x1 - 
     #t9 * x2 - 0.2D1 * t476 * z + t13 + t506
      t519 = 0.1D1 / (t499 + t517)
      t522 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t482, -t
     #492, t472, -t483, -t267)
      t526 = t6 * t253 * t519 * t522 * t53 * t61 / 0.8D1
      t527 = FJET(XB1, XB2, s, t472, t482, -t483, -t492, -t267, -t526)
      t529 = pi * t253
      t532 = t519 * t522 * t407
      t535 = FJET(XB1, XB2, s, t482, t472, -t492, -t483, -t267, -t526)
      t551 = t403 - t409 + t424 - (0.90D2 * t84 * pi * t433 - 0.180D3 * 
     #t45 * t98 * t438 - t442) * t55 / 0.1440D4
      t552 = FJET(XB1, XB2, s, -t258, 0.0D0, 0.0D0, -t352, t353, t551)
      t554 = FJET(XB1, XB2, s, -t258, t261, 0.0D0, t256, -t267, t302)
      t556 = FJET(XB1, XB2, s, -t352, 0.0D0, 0.0D0, -t258, t353, t551)
      t558 = FJET(XB1, XB2, s, -t311, 0.0D0, t309, 0.0D0, 0.0D0, t466)
      t560 = FJET(XB1, XB2, s, -t492, -t483, t482, t472, -t267, -t526)
      t565 = FJET(XB1, XB2, s, -t483, -t492, t472, t482, -t267, -t526)
      rrgg2qqbarhhardt5s2e0 = t245 * t244 + t247 * t244 + t249 * t244 + 
     #t303 * t302 + t349 * t348 + t447 * t446 + t449 * t446 + t451 * t34
     #8 + t453 * t244 + t455 * t302 + t457 * t302 + t467 * t466 - t527 *
     # t5 * t529 * t532 / 0.8D1 - t535 * t5 * t529 * t532 / 0.8D1 + t552
     # * t551 + t554 * t302 + t556 * t551 + t558 * t466 - t560 * t5 * t5
     #29 * t532 / 0.8D1 - t565 * t5 * t529 * t532 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarhhardt5s2em1
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t7 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = -0.1D1 + x3
      t20 = log(-0.4D1 * t11 * t14 / t15)
      t21 = cos(t12)
      t22 = x3 * z
      t24 = Sqrt(-t22 * t15)
      t28 = 0.1D1 / (-z - x3 + 0.2D1 * t21 * t24)
      t32 = log(0.4D1 * t11 * t14)
      t33 = 0.1D1 / z
      t39 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t42 = lh * t5
      t50 = 0.1D1 / x3
      t53 = t5 * t33
      t54 = x1 ** 2
      t55 = t54 * t14
      t58 = log(0.4D1 * t55 * t10)
      t64 = t33 * pi
      t69 = 0.1D1 / x1
      t72 = t33 * t7
      t81 = 0.1D1 / x2
      t86 = x2 ** 2
      t87 = t10 * t86
      t90 = log(0.4D1 * t87 * t14)
      t91 = -0.1D1 + x2
      t95 = log(-0.4D1 * t87 * t14 * t91)
      t104 = log(0.4D1 * t10 * t14)
      t105 = t104 * t33
      t110 = rrgg2qqbarhhard51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, t2, 0.0D0, 0.0D0)
      t115 = t104 ** 2
      t119 = lh ** 2
      t121 = pi ** 2
      t129 = (0.90D2 * t6 * t7 * (t20 * t28 + t32 * t33) + (0.90D2 * t6 
     #* t39 - 0.180D3 * t42 * pi * t7) * (-t28 - t33)) * t50 / 0.2880D4 
     #- (0.90D2 * t53 * pi * (t39 - t58 * t7) - 0.180D3 * t42 * t64 * t7
     #) * t69 / 0.1440D4 + t6 * (-t72 - t7 * t28) * t50 * t69 / 0.16D2 -
     # t6 * t7 * t28 * t50 * t81 / 0.16D2 - t72 * t5 * pi * (-t90 + t95)
     # * t81 / 0.16D2 - (-0.180D3 * (t33 * t39 - t105 * t7) * lh + 0.90D
     #2 * t33 * t110 - 0.90D2 * t105 * t39 + 0.45D2 * t115 * t33 * t7 + 
     #t72 * (0.180D3 * t119 - 0.30D2 * t121)) * t5 * pi / 0.2880D4
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t129)
      t132 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t129)
      t134 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t129)
      t137 = x1 * z
      t138 = -z - x1 + t137
      t139 = 0.1D1 / t138
      t141 = t2 * x1 * t91 * t139
      t142 = -0.1D1 + x1
      t143 = t2 * t142
      t146 = x2 * s * t1 * x1
      t147 = t1 ** 2
      t148 = s * t147
      t151 = t142 * x1 * t139
      t152 = t148 * t91 * t151
      t153 = x2 * x1
      t156 = 0.1D1 / (-z - t153 + t153 * z)
      t158 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, t146, t1
     #41, -t143, 0.0D0, -t152)
      t162 = t6 * t156 * t158 * t81 * t69 / 0.8D1
      t163 = FJET(XB1, XB2, s, 0.0D0, t141, -t143, t146, -t152, t162)
      t168 = t156 * t158 * t81 * t69
      t171 = x2 * x3
      t173 = 0.1D1 / (0.1D1 - x3 + t171)
      t175 = t2 * t171 * t173
      t177 = t2 * t15 * t173
      t180 = Sqrt(t22 * t91 * t15)
      t184 = 0.1D1 / (-z - x3 + t171 + 0.2D1 * t21 * t180)
      t186 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0
     #.0D0, -t177, t175, 0.0D0)
      t190 = t6 * t184 * t186 * t50 * t81 / 0.16D2
      t191 = FJET(XB1, XB2, s, 0.0D0, t175, 0.0D0, -t177, 0.0D0, t190)
      t196 = t184 * t186 * t50 * t81
      t200 = t2 * x1 * t139
      t201 = t148 * t151
      t203 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t200, -t143, 0.0D0, t201)
      t207 = t6 * t33 * t203 * t81 * t69 / 0.8D1
      t208 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -
     #t200, -t143, 0.0D0, t201)
      t211 = t142 ** 2
      t215 = log(-0.4D1 * t55 / t8 * t139 * t211)
      t217 = -t208 + t215 * t203
      t223 = 0.180D3 * t42 * t64 * t203
      t231 = x3 * t54
      t240 = Sqrt(x3 * t138 * t15)
      t251 = t6 * (t33 * t203 - t138 * t203 / (-t137 - x3 * x1 * z - t23
     #1 + 0.2D1 * t231 * z - t231 * t8 + x1 * t8 + x3 * t8 * x1 - t22 + 
     #0.2D1 * t21 * t240 * z - t8)) * t50 * t69 / 0.16D2
      t252 = t207 - (0.90D2 * t53 * pi * t217 + t223) * t69 / 0.1440D4 +
     # t251
      t253 = FJET(XB1, XB2, s, 0.0D0, -t143, -t200, 0.0D0, t201, t252)
      t255 = FJET(XB1, XB2, s, 0.0D0, -t200, -t143, 0.0D0, t201, t252)
      t257 = FJET(XB1, XB2, s, 0.0D0, -t177, 0.0D0, t175, 0.0D0, t190)
      t262 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t129)
      t264 = FJET(XB1, XB2, s, t146, -t143, t141, 0.0D0, -t152, t162)
      t269 = FJET(XB1, XB2, s, t141, 0.0D0, t146, -t143, -t152, t162)
      t274 = FJET(XB1, XB2, s, t175, 0.0D0, -t177, 0.0D0, 0.0D0, t190)
      t286 = t207 - (0.90D2 * t53 * pi * t217 + t223) * t69 / 0.1440D4 +
     # t251
      t287 = FJET(XB1, XB2, s, -t143, 0.0D0, 0.0D0, -t200, t201, t286)
      t289 = FJET(XB1, XB2, s, -t143, t146, 0.0D0, t141, -t152, t162)
      t294 = FJET(XB1, XB2, s, -t200, 0.0D0, 0.0D0, -t143, t201, t286)
      t296 = FJET(XB1, XB2, s, -t177, 0.0D0, t175, 0.0D0, 0.0D0, t190)
      rrgg2qqbarhhardt5s2em1 = t130 * t129 + t132 * t129 + t134 * t129 +
     # t163 * t5 * pi * t168 / 0.8D1 + t191 * t5 * pi * t196 / 0.16D2 + 
     #t253 * t252 + t255 * t252 + t257 * t5 * pi * t196 / 0.16D2 + t262 
     #* t129 + t264 * t5 * pi * t168 / 0.8D1 + t269 * t5 * pi * t168 / 0
     #.8D1 + t274 * t5 * pi * t196 / 0.16D2 + t287 * t286 + t289 * t5 * 
     #pi * t168 / 0.8D1 + t294 * t286 + t296 * t5 * pi * t196 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt5s2em2
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

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
      t3 = 0.1D1 / z
      t4 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t5 = t3 * t4
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t8 * pi
      t10 = 0.1D1 / x1
      t14 = x4 * pi
      t15 = cos(t14)
      t19 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t32 = rrgg2qqbarhhard51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.
     #0D0, t2, 0.0D0, 0.0D0)
      t35 = z ** 2
      t38 = Sin(t14)
      t39 = t38 ** 2
      t42 = log(0.4D1 / t35 / z * t39)
      t50 = -t5 * t9 * t10 / 0.16D2 + t9 * t4 * (-0.1D1 / (-z - x3 + 0.2
     #D1 * t15 * t19) - t3) / x3 / 0.32D2 - (-0.180D3 * lh * t5 + 0.90D2
     # * t3 * t32 - 0.90D2 * t42 * t3 * t4) * t8 * pi / 0.2880D4
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t50)
      t53 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t50)
      t55 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t50)
      t57 = -0.1D1 + x1
      t58 = t2 * t57
      t61 = 0.1D1 / (-z - x1 + x1 * z)
      t63 = t2 * x1 * t61
      t64 = t1 ** 2
      t68 = s * t64 * x1 * t57 * t61
      t69 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t
     #63, -t58, 0.0D0, t68)
      t71 = t3 * t69 * t10
      t73 = t9 * t71 / 0.16D2
      t74 = FJET(XB1, XB2, s, 0.0D0, -t58, -t63, 0.0D0, t68, t73)
      t79 = FJET(XB1, XB2, s, 0.0D0, -t63, -t58, 0.0D0, t68, t73)
      t84 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t50)
      t86 = FJET(XB1, XB2, s, -t58, 0.0D0, 0.0D0, -t63, t68, t73)
      t91 = FJET(XB1, XB2, s, -t63, 0.0D0, 0.0D0, -t58, t68, t73)
      rrgg2qqbarhhardt5s2em2 = t51 * t50 + t53 * t50 + t55 * t50 + t74 *
     # t8 * pi * t71 / 0.16D2 + t79 * t8 * pi * t71 / 0.16D2 + t84 * t50
     # + t86 * t8 * pi * t71 / 0.16D2 + t91 * t8 * pi * t71 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarhhardt5s2em3
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = 0.1D1 / z
      t4 = rrgg2qqbarhhard51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0
     #D0, t2, 0.0D0, 0.0D0)
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t11 = t3 * t4 * t8 * pi / 0.32D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t11)
      t15 = t4 * t8 * pi
      t17 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t11)
      t23 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrgg2qqbarhhardt5s2em3 = -t12 * t3 * t15 / 0.32D2 - t17 * t3 * t15
     # / 0.32D2 - t20 * t3 * t15 / 0.32D2 - t23 * t3 * t15 / 0.32D2

      end function



      doubleprecision function rrgg2qqbarhhardt5s2em4
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
      doubleprecision rrgg2qqbarhhard51J1
      doubleprecision rrgg2qqbarhhard51J2
      doubleprecision rrgg2qqbarhhard51J3
      doubleprecision rrgg2qqbarhhard51J4
      doubleprecision rrgg2qqbarhhard51J5
      doubleprecision rrgg2qqbarhhard51J6
      doubleprecision rrgg2qqbarhhard51J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt5s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarhhard51J1
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
      t2 = S13 + S14 + S34
      t4 = S23 + S24 + S34
      t5 = t4 * S24
      t10 = S23 * S13
      t18 = 0.1D1 / S12
      t22 = -S24 - S13
      t24 = 0.144D3 * t22 * nf
      t25 = S12 ** 2
      t28 = -t22
      t42 = S24 ** 2
      t43 = S13 * S14
      t44 = S13 ** 2
      t45 = S23 * S24
      t46 = t42 - t43 + t44 - t45
      t52 = S34 ** 2
      t82 = S24 * S14
      t90 = t42 * S24
      t92 = t44 * S14
      t94 = S14 ** 2
      t95 = S13 * t94
      t97 = S23 ** 2
      t98 = t97 * S24
      t100 = t44 * S13
      t102 = S23 * t42
      t187 = t97 * S23 * S24
      t189 = S23 * t90
      t191 = t42 ** 2
      t193 = t44 ** 2
      t200 = t94 * S14 * S13
      t202 = S14 * t100
      rrgg2qqbarhhard51J1 = (((0.4D1 / 0.3D1 * S13 * nf * t2 + 0.4D1 / 0
     #.3D1 * t5 * nf) * S34 - 0.4D1 / 0.3D1 * t10 * nf * t2 - 0.4D1 / 0.
     #3D1 * t5 * S14 * nf) * t18 * s * z - t24 * t25 / 0.96D2 + (-0.7D1 
     #/ 0.4D1 * t28 * nf * S34 - (-0.56D2 * S24 - 0.312D3 * S13) * nf * 
     #t2 / 0.96D2 - (-0.56D2 * S13 - 0.312D3 * S24) * nf * t4 / 0.96D2 -
     # 0.9D1 / 0.4D1 * t46 * nf) * S12 - t24 * t52 / 0.96D2 + (-(0.128D3
     # * S13 - 0.400D3 * S24) * nf * t2 / 0.96D2 - (-0.400D3 * S13 + 0.1
     #28D3 * S24) * nf * t4 / 0.96D2 + 0.7D1 / 0.6D1 * t46 * nf) * S34 -
     # (0.240D3 * t44 - 0.128D3 * t10 - 0.880D3 * t43 - 0.584D3 * t45 - 
     #0.328D3 * t42) * nf * t2 / 0.96D2 - (-0.880D3 * t45 + 0.240D3 * t4
     #2 - 0.128D3 * t82 - 0.328D3 * t44 - 0.584D3 * t43) * nf * t4 / 0.9
     #6D2 - (-0.144D3 * t90 + 0.288D3 * t92 - 0.144D3 * t95 - 0.144D3 * 
     #t98 - 0.144D3 * t100 + 0.288D3 * t102) * nf / 0.96D2 + (-0.7D1 / 0
     #.24D2 * t28 * nf * t52 * S34 + (-(-0.47D2 * S13 - 0.192D3 * S24) *
     # nf * t2 / 0.96D2 - (-0.192D3 * S13 - 0.47D2 * S24) * nf * t4 / 0.
     #96D2 - 0.3D1 / 0.8D1 * t46 * nf) * t52 + (-(0.96D2 * t43 + 0.94D2 
     #* t10 - 0.530D3 * t45 + 0.148D3 * t44 - 0.182D3 * t42) * nf * t2 /
     # 0.96D2 - (-0.182D3 * t44 + 0.96D2 * t45 + 0.148D3 * t42 + 0.94D2 
     #* t82 - 0.530D3 * t43) * nf * t4 / 0.96D2 - (0.28D2 * t98 - 0.56D2
     # * t102 - 0.56D2 * t92 + 0.28D2 * t90 + 0.28D2 * t100 + 0.28D2 * t
     #95) * nf / 0.96D2) * S34 - (-0.748D3 * t95 - 0.16D2 * t92 - 0.148D
     #3 * t44 * S23 - 0.430D3 * t98 - 0.96D2 * t43 * S23 - 0.328D3 * t10
     #2 + 0.12D2 * t100 - 0.47D2 * S13 * t97 - 0.10D2 * t90) * nf * t2 /
     # 0.96D2 - (-0.748D3 * t98 + 0.12D2 * t90 - 0.430D3 * t95 - 0.328D3
     # * t92 - 0.96D2 * t82 * S23 - 0.47D2 * S24 * t94 - 0.16D2 * t102 -
     # 0.148D3 * t42 * S14 - 0.10D2 * t100) * nf * t4 / 0.96D2 - (-0.36D
     #2 * t187 - 0.108D3 * t189 + 0.36D2 * t191 + 0.36D2 * t193 + 0.108D
     #3 * t42 * t97 + 0.108D3 * t44 * t94 - 0.36D2 * t200 - 0.108D3 * t2
     #02) * nf / 0.96D2) * t18 + (-(-0.144D3 * t43 * t97 - 0.288D3 * t92
     # * S23 - 0.144D3 * t200 - 0.144D3 * t202) * nf * t2 / 0.96D2 - (-0
     #.144D3 * t45 * t94 - 0.288D3 * t102 * S14 - 0.144D3 * t187 - 0.144
     #D3 * t189) * nf * t4 / 0.96D2) / t25) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard51J2
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
      t1 = -S24 - S13
      t4 = S12 ** 2
      t7 = S13 + S14 + S34
      t10 = S23 + S24 + S34
      t32 = S13 * S14
      t33 = S24 ** 2
      t34 = S23 * S24
      t35 = S13 ** 2
      t36 = -t32 + t33 - t34 + t35
      t44 = S34 ** 2
      t67 = S23 * S13
      t71 = 0.128D3 * S14 * S23
      t73 = S24 * S14
      t89 = t33 * S24
      t91 = t35 * S14
      t92 = 0.576D3 * t91
      t93 = S14 ** 2
      t94 = S13 * t93
      t96 = S23 ** 2
      t97 = t96 * S24
      t99 = t35 * S13
      t101 = S23 * t33
      t102 = 0.576D3 * t101
      t152 = t73 * S23
      t156 = t33 * S14
      t158 = t35 * S23
      t160 = S13 * t96
      t162 = t32 * S23
      t166 = S24 * t93
      t169 = t92 + 0.40D2 * t99 - 0.52D2 * t101 - 0.256D3 * t152 - 0.318
     #D3 * t97 - 0.904D3 * t94 - 0.256D3 * t156 - 0.80D2 * t158 - 0.40D2
     # * t160 + 0.448D3 * t162 + 0.64D2 * S23 * t93 - 0.136D3 * t166 + 0
     #.42D2 * t89
      t186 = -0.318D3 * t94 - 0.40D2 * t166 + t102 - 0.80D2 * t156 - 0.9
     #04D3 * t97 + 0.42D2 * t99 - 0.256D3 * t162 - 0.136D3 * t160 + 0.44
     #8D3 * t152 + 0.64D2 * t96 * S14 + 0.40D2 * t89 - 0.256D3 * t158 - 
     #0.52D2 * t91
      t191 = t96 * S23 * S24
      t193 = S23 * t89
      t195 = t33 ** 2
      t197 = t35 ** 2
      t199 = t35 * t93
      t201 = t33 * t96
      t204 = t93 * S14 * S13
      t206 = S14 * t99
      rrgg2qqbarhhard51J2 = (-0.3D1 / 0.2D1 * t1 * nf * t4 + ((0.2D1 / 0
     #.3D1 * nf * t7 + 0.2D1 / 0.3D1 * nf * t10 - t1 * nf / 0.12D2) * S3
     #4 - (-0.136D3 * S24 + 0.64D2 * S23 - 0.360D3 * S13) * nf * t7 / 0.
     #96D2 - (-0.136D3 * S13 - 0.360D3 * S24 + 0.64D2 * S14) * nf * t10 
     #/ 0.96D2 - 0.15D2 / 0.4D1 * t36 * nf) * S12 - t1 * nf * t44 / 0.3D
     #1 + (-(-0.512D3 * S24 - 0.240D3 * S13 - 0.128D3 * S14) * nf * t7 /
     # 0.96D2 - (-0.512D3 * S13 - 0.128D3 * S23 - 0.240D3 * S24) * nf * 
     #t10 / 0.96D2 + 0.5D1 / 0.6D1 * t36 * nf) * S34 - (-0.1120D4 * t32 
     #+ 0.544D3 * t35 + 0.240D3 * t67 - 0.624D3 * t34 + t71 - 0.400D3 * 
     #t33 - 0.216D3 * t73) * nf * t7 / 0.96D2 - (-0.1120D4 * t34 - 0.624
     #D3 * t32 + 0.544D3 * t33 - 0.400D3 * t35 - 0.216D3 * t67 + t71 + 0
     #.240D3 * t73) * nf * t10 / 0.96D2 - (-0.288D3 * t89 + t92 - 0.288D
     #3 * t94 - 0.288D3 * t97 - 0.288D3 * t99 + t102) * nf / 0.96D2 + ((
     #-(-0.40D2 * S13 - 0.354D3 * S24) * nf * t7 / 0.96D2 - (-0.40D2 * S
     #24 - 0.354D3 * S13) * nf * t10 / 0.96D2) * t44 + (-(-0.448D3 * t32
     # - 0.256D3 * t33 - 0.256D3 * t73 - 0.64D2 * t93 - 0.616D3 * t34 + 
     #0.80D2 * t35 + 0.80D2 * t67) * nf * t7 / 0.96D2 - (-0.256D3 * t67 
     #+ 0.80D2 * t73 + 0.80D2 * t33 - 0.64D2 * t96 - 0.256D3 * t35 - 0.6
     #16D3 * t32 - 0.448D3 * t34) * nf * t10 / 0.96D2 - (0.56D2 * t99 + 
     #0.56D2 * t89 + 0.56D2 * t94 - 0.112D3 * t91 + 0.56D2 * t97 - 0.112
     #D3 * t101) * nf / 0.96D2) * S34 - t169 * nf * t7 / 0.96D2 - t186 *
     # nf * t10 / 0.96D2 - (-0.72D2 * t191 - 0.216D3 * t193 + 0.72D2 * t
     #195 + 0.72D2 * t197 + 0.216D3 * t199 + 0.216D3 * t201 - 0.72D2 * t
     #204 - 0.216D3 * t206) * nf / 0.96D2) / S12 + (-(0.432D3 * t94 * S2
     #3 - 0.144D3 * t204 + 0.288D3 * t199 - 0.144D3 * t91 * S23 - 0.144D
     #3 * t206 - 0.144D3 * t32 * t96) * nf * t7 / 0.96D2 - (-0.144D3 * t
     #193 + 0.432D3 * t97 * S14 - 0.144D3 * t34 * t93 + 0.288D3 * t201 -
     # 0.144D3 * t191 - 0.144D3 * t101 * S14) * nf * t10 / 0.96D2) / t4)
     # / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard51J3
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.4D1 * t12 - 0.4D1 * t14
      t22 = S34 ** 2
      t39 = -S24 - S13
      t42 = S12 ** 2
      t66 = S23 * S24
      t67 = S13 * S14
      t68 = S24 ** 2
      t69 = S13 ** 2
      t70 = -t66 - t67 + t68 + t69
      t95 = -t70
      t103 = S23 * S13
      t106 = S14 * S23
      t107 = 0.320D3 * t106
      t109 = S24 * S14
      t111 = S23 ** 2
      t119 = S14 ** 2
      t129 = t68 * S24
      t131 = t69 * S14
      t133 = S13 * t119
      t135 = t111 * S24
      t137 = t69 * S13
      t139 = S23 * t68
      t172 = 0.64D2 * t106
      t205 = t109 * S23
      t209 = t111 * S14
      t211 = t69 * S23
      t213 = S13 * t111
      t216 = t68 * S14
      t219 = t111 * S23
      t221 = t67 * S23
      t223 = S23 * t119
      t225 = S24 * t119
      t228 = 0.1040D4 * t131 - 0.25776D5 * t205 - 0.28414D5 * t135 - 0.2
     #7696D5 * t139 + 0.32D2 * t209 - 0.12D2 * t211 - 0.37D2 * t213 - 0.
     #1124D4 * t133 - 0.25808D5 * t216 + 0.4D1 * t137 + 0.16D2 * t219 + 
     #0.992D3 * t221 + 0.128D3 * t223 - 0.272D3 * t225 + 0.398D3 * t129
      t239 = t119 * S14
      t248 = -0.28414D5 * t133 + 0.4D1 * t129 + 0.1040D4 * t139 - 0.12D2
     # * t216 - 0.1124D4 * t135 + 0.32D2 * t223 - 0.25776D5 * t221 + 0.1
     #6D2 * t239 + 0.992D3 * t205 + 0.398D3 * t137 + 0.128D3 * t209 - 0.
     #272D3 * t213 - 0.27696D5 * t131 - 0.25808D5 * t211 - 0.37D2 * t225
      t252 = t219 * S24
      t254 = S23 * t129
      t256 = t68 ** 2
      t258 = t69 ** 2
      t260 = t69 * t119
      t262 = t239 * S13
      t264 = t68 * t111
      t266 = S14 * t137
      t297 = 0.4D1 / 0.3D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S34
     # * t1 * t3 * t6 + (-t16 * t22 + (-(-0.384D3 * S14 - 0.384D3 * S13)
     # * nf * t11 / 0.96D2 - (-0.384D3 * S23 - 0.384D3 * S24) * nf * t13
     # / 0.96D2) * S34) * t1 * s * z - 0.3D1 / 0.2D1 * t39 * nf * t42 + 
     #((0.8D1 / 0.3D1 * t12 + 0.8D1 / 0.3D1 * t14 - 0.5D1 / 0.12D2 * t39
     # * nf) * S34 - (0.256D3 * S23 - 0.472D3 * S13 - 0.248D3 * S24) * n
     #f * t11 / 0.96D2 - (-0.248D3 * S13 - 0.472D3 * S24 + 0.256D3 * S14
     #) * nf * t13 / 0.96D2 - 0.21D2 / 0.4D1 * t70 * nf) * S12 + (-t12 -
     # t14 - 0.13D2 / 0.6D1 * t39 * nf) * t22 + (-(-0.320D3 * S14 - 0.74
     #912D5 * S24 - 0.600D3 * S13 - 0.192D3 * S23) * nf * t11 / 0.96D2 -
     # (-0.600D3 * S24 - 0.74912D5 * S13 - 0.192D3 * S14 - 0.320D3 * S23
     #) * nf * t13 / 0.96D2 - t95 * nf / 0.2D1) * S34 - (-0.1488D4 * t67
     # + 0.720D3 * t69 + 0.600D3 * t103 - 0.74872D5 * t66 + t107 - 0.747
     #92D5 * t68 - 0.512D3 * t109 + 0.96D2 * t111) * nf * t11 / 0.96D2 -
     # (-0.1488D4 * t66 - 0.512D3 * t103 + 0.96D2 * t119 - 0.74792D5 * t
     #69 - 0.74872D5 * t67 + t107 + 0.600D3 * t109 + 0.720D3 * t68) * nf
     # * t13 / 0.96D2 - (-0.432D3 * t129 + 0.864D3 * t131 - 0.432D3 * t1
     #33 - 0.432D3 * t135 - 0.432D3 * t137 + 0.864D3 * t139) * nf / 0.96
     #D2 + ((-0.7D1 / 0.6D1 * t12 - 0.7D1 / 0.6D1 * t14 + 0.29D2 / 0.24D
     #2 * t39 * nf) * t22 * S34 + (-(0.288D3 * S14 + 0.48D2 * S23 + 0.21
     #9D3 * S13 + 0.28276D5 * S24) * nf * t11 / 0.96D2 - (0.219D3 * S24 
     #+ 0.48D2 * S14 + 0.288D3 * S23 + 0.28276D5 * S13) * nf * t13 / 0.9
     #6D2 - 0.3D1 / 0.8D1 * t95 * nf) * t22 + (-(-t172 + 0.74D2 * t103 -
     # 0.166D3 * t66 + 0.140D3 * t69 - 0.48D2 * t111 + 0.28798D5 * t68 -
     # 0.25760D5 * t109 - 0.736D3 * t67) * nf * t11 / 0.96D2 - (-0.736D3
     # * t66 - t172 + 0.140D3 * t68 + 0.74D2 * t109 + 0.28798D5 * t69 - 
     #0.48D2 * t119 - 0.25760D5 * t103 - 0.166D3 * t67) * nf * t13 / 0.9
     #6D2 - (-0.168D3 * t131 + 0.84D2 * t129 + 0.84D2 * t135 + 0.84D2 * 
     #t133 - 0.168D3 * t139 + 0.84D2 * t137) * nf / 0.96D2) * S34 - t228
     # * nf * t11 / 0.96D2 - t248 * nf * t13 / 0.96D2 - (-0.108D3 * t252
     # - 0.324D3 * t254 + 0.108D3 * t256 + 0.108D3 * t258 + 0.324D3 * t2
     #60 - 0.108D3 * t262 + 0.324D3 * t264 - 0.324D3 * t266) * nf / 0.96
     #D2) * t1 + (-(-0.144D3 * t262 + 0.576D3 * t260 - 0.144D3 * t266 - 
     #0.144D3 * t67 * t111 + 0.864D3 * t133 * S23) * nf * t11 / 0.96D2 -
     # (-0.144D3 * t252 - 0.144D3 * t66 * t119 + 0.576D3 * t264 - 0.144D
     #3 * t254 + 0.864D3 * t135 * S14) * nf * t13 / 0.96D2) / t42
      rrgg2qqbarhhard51J3 = t297 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard51J4
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.8D1 * t12 - 0.8D1 * t14
      t22 = S34 ** 2
      t39 = -S24 - S13
      t42 = S12 ** 2
      t66 = S24 ** 2
      t67 = S23 * S24
      t68 = S13 ** 2
      t69 = S13 * S14
      t70 = t66 - t67 + t68 - t69
      t97 = -t70
      t105 = S23 * S13
      t108 = S14 * S23
      t109 = 0.512D3 * t108
      t111 = S24 * S14
      t113 = S23 ** 2
      t121 = S14 ** 2
      t131 = t66 * S24
      t133 = t68 * S14
      t135 = S13 * t121
      t137 = t113 * S24
      t139 = t68 * S13
      t141 = S23 * t66
      t178 = 0.128D3 * t108
      t209 = t111 * S23
      t213 = t113 * S14
      t215 = S23 * t68
      t217 = S13 * t113
      t220 = t66 * S14
      t223 = t113 * S23
      t225 = t69 * S23
      t227 = S23 * t121
      t229 = S24 * t121
      t232 = 0.1504D4 * t133 - 0.51296D5 * t209 - 0.56510D5 * t137 - 0.5
     #5340D5 * t141 + 0.64D2 * t213 + 0.56D2 * t215 - 0.34D2 * t217 - 0.
     #1344D4 * t135 - 0.51360D5 * t220 - 0.32D2 * t139 + 0.32D2 * t223 +
     # 0.1536D4 * t225 + 0.192D3 * t227 - 0.408D3 * t229 + 0.754D3 * t13
     #1
      t243 = t121 * S14
      t252 = -0.56510D5 * t135 - 0.32D2 * t131 + 0.1504D4 * t141 + 0.56D
     #2 * t220 - 0.1344D4 * t137 + 0.64D2 * t227 - 0.51296D5 * t225 + 0.
     #32D2 * t243 + 0.1536D4 * t209 + 0.754D3 * t139 + 0.192D3 * t213 - 
     #0.408D3 * t217 - 0.55340D5 * t133 - 0.51360D5 * t215 - 0.34D2 * t2
     #29
      t257 = 0.144D3 * t223 * S24
      t258 = S23 * t131
      t260 = t66 ** 2
      t262 = t68 ** 2
      t264 = t68 * t121
      t267 = 0.144D3 * t243 * S13
      t268 = t66 * t113
      t270 = S14 * t139
      t303 = 0.8D1 / 0.3D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S34
     # * t1 * t3 * t6 + (-t16 * t22 + (-(-0.768D3 * S14 - 0.768D3 * S13)
     # * nf * t11 / 0.96D2 - (-0.768D3 * S24 - 0.768D3 * S23) * nf * t13
     # / 0.96D2) * S34) * t1 * s * z - 0.3D1 / 0.2D1 * t39 * nf * t42 + 
     #((0.14D2 / 0.3D1 * t12 + 0.14D2 / 0.3D1 * t14 - 0.3D1 / 0.4D1 * t3
     #9 * nf) * S34 - (-0.584D3 * S13 + 0.448D3 * S23 - 0.360D3 * S24) *
     # nf * t11 / 0.96D2 - (-0.584D3 * S24 - 0.360D3 * S13 + 0.448D3 * S
     #14) * nf * t13 / 0.96D2 - 0.27D2 / 0.4D1 * t70 * nf) * S12 + (-0.2
     #D1 * t12 - 0.2D1 * t14 - 0.4D1 * t39 * nf) * t22 + (-(-0.512D3 * S
     #14 - 0.149312D6 * S24 - 0.960D3 * S13 - 0.384D3 * S23) * nf * t11 
     #/ 0.96D2 - (-0.960D3 * S24 - 0.149312D6 * S13 - 0.384D3 * S14 - 0.
     #512D3 * S23) * nf * t13 / 0.96D2 - t97 * nf / 0.6D1) * S34 - (-0.1
     #856D4 * t69 + 0.896D3 * t68 + 0.960D3 * t105 - 0.149120D6 * t67 + 
     #t109 - 0.149184D6 * t66 - 0.808D3 * t111 + 0.192D3 * t113) * nf * 
     #t11 / 0.96D2 - (-0.1856D4 * t67 - 0.808D3 * t105 + 0.192D3 * t121 
     #- 0.149184D6 * t68 - 0.149120D6 * t69 + t109 + 0.960D3 * t111 + 0.
     #896D3 * t66) * nf * t13 / 0.96D2 - (-0.576D3 * t131 + 0.1152D4 * t
     #133 - 0.576D3 * t135 - 0.576D3 * t137 - 0.576D3 * t139 + 0.1152D4 
     #* t141) * nf / 0.96D2 + ((-0.7D1 / 0.3D1 * t12 - 0.7D1 / 0.3D1 * t
     #14 + 0.29D2 / 0.12D2 * t39 * nf) * t22 * S34 + (-(0.576D3 * S14 + 
     #0.96D2 * S23 + 0.478D3 * S13 + 0.56906D5 * S24) * nf * t11 / 0.96D
     #2 - (0.478D3 * S24 + 0.96D2 * S14 + 0.576D3 * S23 + 0.56906D5 * S1
     #3) * nf * t13 / 0.96D2 - 0.3D1 / 0.4D1 * t97 * nf) * t22 + (-(0.20
     #0D3 * t68 + 0.64D2 * t121 + 0.68D2 * t105 + 0.57852D5 * t66 - t178
     # - 0.51264D5 * t111 + 0.284D3 * t67 - 0.1024D4 * t69 - 0.96D2 * t1
     #13) * nf * t11 / 0.96D2 - (0.200D3 * t66 + 0.57852D5 * t68 + 0.284
     #D3 * t69 + 0.64D2 * t113 - t178 - 0.51264D5 * t105 + 0.68D2 * t111
     # - 0.96D2 * t121 - 0.1024D4 * t67) * nf * t13 / 0.96D2 - (-0.224D3
     # * t133 - 0.224D3 * t141 + 0.112D3 * t135 + 0.112D3 * t139 + 0.112
     #D3 * t131 + 0.112D3 * t137) * nf / 0.96D2) * S34 - t232 * nf * t11
     # / 0.96D2 - t252 * nf * t13 / 0.96D2 - (-t257 - 0.432D3 * t258 + 0
     #.144D3 * t260 + 0.144D3 * t262 + 0.432D3 * t264 - t267 + 0.432D3 *
     # t268 - 0.432D3 * t270) * nf / 0.96D2) * t1 + (-(0.1296D4 * t135 *
     # S23 + 0.864D3 * t264 - t267 - 0.144D3 * t270 + 0.144D3 * t133 * S
     #23 - 0.144D3 * t69 * t113) * nf * t11 / 0.96D2 - (-0.144D3 * t67 *
     # t121 + 0.144D3 * t141 * S14 + 0.1296D4 * S14 * t137 - 0.144D3 * t
     #258 + 0.864D3 * t268 - t257) * nf * t13 / 0.96D2) / t42
      rrgg2qqbarhhard51J4 = t303 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard51J5
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.12D2 * t12 - 0.12D2 * t14
      t22 = S34 ** 2
      t39 = -S24 - S13
      t42 = S12 ** 2
      t66 = S24 ** 2
      t67 = S23 * S24
      t68 = S13 ** 2
      t69 = S13 * S14
      t70 = t66 - t67 + t68 - t69
      t104 = S23 * S13
      t107 = S14 * S23
      t108 = 0.704D3 * t107
      t110 = S24 * S14
      t112 = S23 ** 2
      t120 = S14 ** 2
      t130 = t66 * S24
      t132 = t68 * S14
      t134 = S13 * t120
      t136 = t112 * S24
      t138 = t68 * S13
      t140 = S23 * t66
      t178 = 0.192D3 * t107
      t209 = t110 * S23
      t213 = t112 * S14
      t215 = S23 * t68
      t217 = S13 * t112
      t220 = t66 * S14
      t223 = t112 * S23
      t225 = t69 * S23
      t227 = S23 * t120
      t229 = S24 * t120
      t232 = 0.1968D4 * t132 - 0.76816D5 * t209 - 0.84606D5 * t136 - 0.8
     #2984D5 * t140 + 0.96D2 * t213 + 0.124D3 * t215 - 0.31D2 * t217 - 0
     #.1564D4 * t134 - 0.76912D5 * t220 - 0.68D2 * t138 + 0.48D2 * t223 
     #+ 0.2080D4 * t225 + 0.256D3 * t227 - 0.544D3 * t229 + 0.1110D4 * t
     #130
      t243 = t120 * S14
      t252 = -0.84606D5 * t134 - 0.68D2 * t130 + 0.1968D4 * t140 + 0.124
     #D3 * t220 - 0.1564D4 * t136 + 0.96D2 * t227 - 0.76816D5 * t225 + 0
     #.48D2 * t243 + 0.2080D4 * t209 + 0.1110D4 * t138 + 0.256D3 * t213 
     #- 0.544D3 * t217 - 0.82984D5 * t132 - 0.76912D5 * t215 - 0.31D2 * 
     #t229
      t256 = t223 * S24
      t258 = S23 * t130
      t260 = t66 ** 2
      t262 = t68 ** 2
      t264 = t68 * t120
      t266 = t243 * S13
      t268 = t66 * t112
      t270 = S14 * t138
      t305 = 0.4D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S34 * t1 * 
     #t3 * t6 + (-t16 * t22 + (-(-0.1152D4 * S14 - 0.1152D4 * S13) * nf 
     #* t11 / 0.96D2 - (-0.1152D4 * S24 - 0.1152D4 * S23) * nf * t13 / 0
     #.96D2) * S34) * t1 * s * z - 0.3D1 / 0.2D1 * t39 * nf * t42 + ((0.
     #20D2 / 0.3D1 * t12 + 0.20D2 / 0.3D1 * t14 - 0.13D2 / 0.12D2 * t39 
     #* nf) * S34 - (-0.696D3 * S13 + 0.640D3 * S23 - 0.472D3 * S24) * n
     #f * t11 / 0.96D2 - (-0.696D3 * S24 - 0.472D3 * S13 + 0.640D3 * S14
     #) * nf * t13 / 0.96D2 - 0.33D2 / 0.4D1 * t70 * nf) * S12 + (-0.3D1
     # * t12 - 0.3D1 * t14 - 0.35D2 / 0.6D1 * t39 * nf) * t22 + (-(-0.70
     #4D3 * S14 - 0.223712D6 * S24 - 0.1320D4 * S13 - 0.576D3 * S23) * n
     #f * t11 / 0.96D2 - (-0.1320D4 * S24 - 0.223712D6 * S13 - 0.576D3 *
     # S14 - 0.704D3 * S23) * nf * t13 / 0.96D2 - t70 * nf / 0.6D1) * S3
     #4 - (-0.2224D4 * t69 + 0.1072D4 * t68 + 0.1320D4 * t104 - 0.223368
     #D6 * t67 + t108 - 0.223576D6 * t66 - 0.1104D4 * t110 + 0.288D3 * t
     #112) * nf * t11 / 0.96D2 - (-0.2224D4 * t67 - 0.1104D4 * t104 + 0.
     #288D3 * t120 - 0.223576D6 * t68 - 0.223368D6 * t69 + t108 + 0.1320
     #D4 * t110 + 0.1072D4 * t66) * nf * t13 / 0.96D2 - (-0.720D3 * t130
     # + 0.1440D4 * t132 - 0.720D3 * t134 - 0.720D3 * t136 - 0.720D3 * t
     #138 + 0.1440D4 * t140) * nf / 0.96D2 + ((-0.7D1 / 0.2D1 * t12 - 0.
     #7D1 / 0.2D1 * t14 + 0.29D2 / 0.8D1 * t39 * nf) * t22 * S34 + (-(0.
     #864D3 * S14 + 0.144D3 * S23 + 0.737D3 * S13 + 0.85536D5 * S24) * n
     #f * t11 / 0.96D2 - (0.737D3 * S24 + 0.144D3 * S14 + 0.864D3 * S23 
     #+ 0.85536D5 * S13) * nf * t13 / 0.96D2 + 0.9D1 / 0.8D1 * t70 * nf)
     # * t22 + (-(0.260D3 * t68 + 0.128D3 * t120 + 0.62D2 * t104 + 0.869
     #06D5 * t66 - t178 - 0.76768D5 * t110 + 0.734D3 * t67 - 0.1312D4 * 
     #t69 - 0.144D3 * t112) * nf * t11 / 0.96D2 - (0.260D3 * t66 + 0.869
     #06D5 * t68 + 0.734D3 * t69 + 0.128D3 * t112 - t178 - 0.76768D5 * t
     #104 + 0.62D2 * t110 - 0.144D3 * t120 - 0.1312D4 * t67) * nf * t13 
     #/ 0.96D2 - (-0.280D3 * t132 - 0.280D3 * t140 + 0.140D3 * t134 + 0.
     #140D3 * t138 + 0.140D3 * t130 + 0.140D3 * t136) * nf / 0.96D2) * S
     #34 - t232 * nf * t11 / 0.96D2 - t252 * nf * t13 / 0.96D2 - (-0.180
     #D3 * t256 - 0.540D3 * t258 + 0.180D3 * t260 + 0.180D3 * t262 + 0.5
     #40D3 * t264 - 0.180D3 * t266 + 0.540D3 * t268 - 0.540D3 * t270) * 
     #nf / 0.96D2) * t1 + (-(0.1728D4 * t134 * S23 + 0.1152D4 * t264 - 0
     #.144D3 * t266 - 0.144D3 * t270 + 0.288D3 * t132 * S23 - 0.144D3 * 
     #t69 * t112) * nf * t11 / 0.96D2 - (-0.144D3 * t67 * t120 + 0.288D3
     # * t140 * S14 + 0.1728D4 * t136 * S14 - 0.144D3 * t258 + 0.1152D4 
     #* t268 - 0.144D3 * t256) * nf * t13 / 0.96D2) / t42
      rrgg2qqbarhhard51J5 = t305 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard51J6
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.16D2 * t12 - 0.16D2 * t14
      t22 = S34 ** 2
      t37 = S23 * S13
      t48 = S13 + S24
      t51 = S12 ** 2
      t76 = S13 ** 2
      t77 = S13 * S14
      t78 = S24 ** 2
      t79 = S23 * S24
      t80 = -t76 + t77 - t78 + t79
      t82 = 0.360D3 * t80 * nf
      t118 = S23 ** 2
      t120 = S14 * S23
      t121 = 0.896D3 * t120
      t122 = S24 * S14
      t132 = S14 ** 2
      t166 = 0.256D3 * t120
      t167 = 0.192D3 * t132
      t169 = 0.192D3 * t118
      t189 = t76 * S14
      t191 = t76 * S13
      t193 = t118 * S24
      t195 = S23 * t78
      t197 = t118 * S14
      t199 = S13 * t118
      t201 = t122 * S23
      t203 = S13 * t132
      t205 = t78 * S14
      t207 = t76 * S23
      t209 = t118 * S23
      t211 = t77 * S23
      t213 = t132 * S23
      t215 = S24 * t132
      t217 = t78 * S24
      t219 = 0.2528D4 * t189 - 0.176D3 * t191 - 0.110122D6 * t193 - 0.10
     #8660D6 * t195 + 0.128D3 * t197 + 0.254D3 * t199 - 0.102336D6 * t20
     #1 + 0.2704D4 * t203 - 0.102464D6 * t205 + 0.1080D4 * t207 + 0.64D2
     # * t209 + 0.3200D4 * t211 + 0.320D3 * t213 - 0.680D3 * t215 + 0.15
     #26D4 * t217
      t237 = t132 * S14
      t239 = 0.320D3 * t197 - 0.176D3 * t217 - 0.110122D6 * t203 - 0.108
     #660D6 * t189 + 0.2704D4 * t193 + 0.254D3 * t215 + 0.2528D4 * t195 
     #+ 0.1080D4 * t205 + 0.3200D4 * t201 + 0.1526D4 * t191 + 0.128D3 * 
     #t213 - 0.680D3 * t199 - 0.102336D6 * t211 - 0.102464D6 * t207 + 0.
     #64D2 * t237
      t279 = 0.16D2 / 0.3D1 * t1 * S34 * nf * t3 * s * t6 * z + t16 * S3
     #4 * t1 * t3 * t6 + (-t16 * t22 + (-(-0.768D3 * S13 - 0.1536D4 * S1
     #4) * nf * t11 / 0.96D2 - (-0.768D3 * S24 - 0.1536D4 * S23) * nf * 
     #t13 / 0.96D2) * S34 + 0.8D1 * t37 * t12 + 0.8D1 * t13 * S24 * S14 
     #* nf) * t1 * s * z - 0.15D2 / 0.2D1 * t48 * nf * t51 + ((0.26D2 / 
     #0.3D1 * t12 + 0.26D2 / 0.3D1 * t14 + 0.143D3 / 0.12D2 * t48 * nf) 
     #* S34 - (-0.248D3 * S24 + 0.832D3 * S23 + 0.1064D4 * S13) * nf * t
     #11 / 0.96D2 - (-0.248D3 * S13 + 0.832D3 * S14 + 0.1064D4 * S24) * 
     #nf * t13 / 0.96D2 - t82 / 0.96D2) * S12 + (-0.4D1 * t12 - 0.4D1 * 
     #t14 - 0.4D1 / 0.3D1 * t48 * nf) * t22 + (-(-0.768D3 * S23 - 0.2957
     #12D6 * S24 - 0.2448D4 * S13 - 0.896D3 * S14) * nf * t11 / 0.96D2 -
     # (-0.2448D4 * S24 - 0.896D3 * S23 - 0.768D3 * S14 - 0.295712D6 * S
     #13) * nf * t13 / 0.96D2 + 0.15D2 / 0.2D1 * t80 * nf) * S34 - (0.26
     #88D4 * t77 - 0.296000D6 * t78 - 0.294112D6 * t79 + 0.2448D4 * t37 
     #- 0.192D3 * t76 + 0.384D3 * t118 + t121 - 0.1400D4 * t122) * nf * 
     #t11 / 0.96D2 - (0.2448D4 * t122 + 0.2688D4 * t79 + t121 - 0.192D3 
     #* t78 - 0.294112D6 * t77 + 0.384D3 * t132 - 0.296000D6 * t76 - 0.1
     #400D4 * t37) * nf * t13 / 0.96D2 + ((-0.14D2 / 0.3D1 * t12 - 0.14D
     #2 / 0.3D1 * t14 - 0.37D2 / 0.12D2 * t48 * nf) * t22 * S34 + (-(0.1
     #15318D6 * S24 + 0.192D3 * S23 + 0.1152D4 * S14 + 0.1278D4 * S13) *
     # nf * t11 / 0.96D2 - (0.115318D6 * S13 + 0.192D3 * S14 + 0.1152D4 
     #* S23 + 0.1278D4 * S24) * nf * t13 / 0.96D2 - t82 / 0.96D2) * t22 
     #+ (-(-0.2176D4 * t77 - t166 + t167 - 0.102272D6 * t122 - t169 - 0.
     #568D3 * t76 - 0.508D3 * t37 + 0.4364D4 * t79 + 0.117052D6 * t78) *
     # nf * t11 / 0.96D2 - (-0.2176D4 * t79 + t169 + 0.4364D4 * t77 - t1
     #66 - 0.102272D6 * t37 - 0.568D3 * t78 + 0.117052D6 * t76 - t167 - 
     #0.508D3 * t122) * nf * t13 / 0.96D2) * S34 - t219 * nf * t11 / 0.9
     #6D2 - t239 * nf * t13 / 0.96D2) * t1 + (-(0.1440D4 * t76 * t132 + 
     #0.2160D4 * t189 * S23 + 0.720D3 * t77 * t118 + 0.720D3 * t237 * S1
     #3 + 0.2160D4 * t203 * S23 + 0.720D3 * S14 * t191) * nf * t11 / 0.9
     #6D2 - (0.1440D4 * t78 * t118 + 0.720D3 * t79 * t132 + 0.2160D4 * t
     #193 * S14 + 0.720D3 * t209 * S24 + 0.2160D4 * t195 * S14 + 0.720D3
     # * S23 * t217) * nf * t13 / 0.96D2) / t51
      rrgg2qqbarhhard51J6 = t279 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarhhard51J7
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t6 = z ** 2
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = nf * t13
      t16 = -0.20D2 * t12 - 0.20D2 * t14
      t22 = S34 ** 2
      t37 = S23 * S13
      t50 = S13 + S24
      t53 = 0.15D2 / 0.2D1 * t50 * nf
      t97 = S23 * S24
      t99 = 0.8D1 * t37
      t100 = S24 ** 2
      t103 = 0.64D2 * S14 * S23
      t104 = S13 * S14
      t106 = S13 ** 2
      t107 = 0.128D3 * t106
      t108 = S23 ** 2
      t110 = S24 * S14
      t116 = 0.8D1 * t110
      t117 = S14 ** 2
      t120 = 0.128D3 * t100
      t171 = S13 * t117
      t173 = t100 * S24
      t175 = t106 * S13
      t177 = t106 * S14
      t181 = t108 * S24
      t185 = S23 * t100
      t193 = -0.64D2 * t171 + 0.304D3 * t173 - 0.64D2 * t175 - 0.128D3 *
     # t177 + 0.32D2 * t108 * S14 - 0.28208D5 * t181 - 0.25264D5 * t110 
     #* S23 - 0.27920D5 * t185 + 0.16D2 * t108 * S23 - 0.25296D5 * S14 *
     # t100 - 0.4D1 * S13 * t108
      t213 = -0.64D2 * t181 + 0.32D2 * S23 * t117 - 0.28208D5 * t171 - 0
     #.27920D5 * t177 + 0.16D2 * t117 * S14 - 0.4D1 * S24 * t117 - 0.128
     #D3 * t185 + 0.304D3 * t175 - 0.64D2 * t173 - 0.25296D5 * t106 * S2
     #3 - 0.25264D5 * t104 * S23
      rrgg2qqbarhhard51J7 = (0.20D2 / 0.3D1 * t1 * S34 * nf * t3 * s * t
     #6 * z + t16 * S34 * t1 * t3 * t6 + (-t16 * t22 + (-0.5D1 / 0.96D2 
     #* (-0.512D3 * S13 - 0.384D3 * S14) * nf * t11 - 0.5D1 / 0.96D2 * (
     #-0.512D3 * S24 - 0.384D3 * S23) * nf * t13) * S34 - 0.20D2 / 0.3D1
     # * t37 * t12 - 0.20D2 / 0.3D1 * t13 * S24 * S14 * nf) * t1 * s * z
     # + ((0.20D2 / 0.3D1 * t12 + 0.20D2 / 0.3D1 * t14 - t53) * S34 - 0.
     #5D1 / 0.96D2 * (-0.32D2 * S24 - 0.64D2 * S13 + 0.128D3 * S23) * nf
     # * t11 - 0.5D1 / 0.96D2 * (0.128D3 * S14 - 0.32D2 * S13 - 0.64D2 *
     # S24) * nf * t13) * S12 + (-0.5D1 * t12 - 0.5D1 * t14 + 0.15D2 * t
     #50 * nf) * t22 + (-0.5D1 / 0.96D2 * (0.8D1 * S13 - 0.64D2 * S14 - 
     #0.74288D5 * S24 - 0.192D3 * S23) * nf * t11 - 0.5D1 / 0.96D2 * (-0
     #.74288D5 * S13 + 0.8D1 * S24 - 0.192D3 * S14 - 0.64D2 * S23) * nf 
     #* t13) * S34 - 0.5D1 / 0.96D2 * (-0.74208D5 * t97 - t99 - 0.74320D
     #5 * t100 + t103 - 0.128D3 * t104 - t107 + 0.96D2 * t108 - 0.80D2 *
     # t110) * nf * t11 - 0.5D1 / 0.96D2 * (-t116 + 0.96D2 * t117 - 0.12
     #8D3 * t97 - t120 - 0.74208D5 * t104 - 0.80D2 * t37 - 0.74320D5 * t
     #106 + t103) * nf * t13 + ((-0.35D2 / 0.6D1 * t12 - 0.35D2 / 0.6D1 
     #* t14 - t53) * t22 * S34 + (-0.5D1 / 0.96D2 * (0.288D3 * S14 + 0.2
     #8792D5 * S24 + 0.48D2 * S23 + 0.252D3 * S13) * nf * t11 - 0.5D1 / 
     #0.96D2 * (0.48D2 * S14 + 0.252D3 * S24 + 0.28792D5 * S13 + 0.288D3
     # * S23) * nf * t13) * t22 + (-0.5D1 / 0.96D2 * (-0.25248D5 * t110 
     #+ 0.256D3 * t104 - 0.48D2 * t108 + 0.29128D5 * t100 + 0.536D3 * t9
     #7 - t103 + t99 + t107 + 0.128D3 * t117) * nf * t11 - 0.5D1 / 0.96D
     #2 * (t116 + 0.536D3 * t104 + 0.29128D5 * t106 - 0.48D2 * t117 + t1
     #20 + 0.256D3 * t97 + 0.128D3 * t108 - t103 - 0.25248D5 * t37) * nf
     # * t13) * S34 - 0.5D1 / 0.96D2 * t193 * nf * t11 - 0.5D1 / 0.96D2 
     #* t213 * nf * t13) * t1) / pi * wd / z

      end function
  
   
      subroutine rrgg2qqbarhsoftt5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt5s1e1  
      doubleprecision rrgg2qqbarhsoftt5s1e0  
      doubleprecision rrgg2qqbarhsoftt5s1em1  
      doubleprecision rrgg2qqbarhsoftt5s1em2  
      doubleprecision rrgg2qqbarhsoftt5s1em3  
      doubleprecision rrgg2qqbarhsoftt5s1em4  
      doubleprecision rrgg2qqbarhsoftt5s2e1  
      doubleprecision rrgg2qqbarhsoftt5s2e0  
      doubleprecision rrgg2qqbarhsoftt5s2em1  
      doubleprecision rrgg2qqbarhsoftt5s2em2  
      doubleprecision rrgg2qqbarhsoftt5s2em3  
      doubleprecision rrgg2qqbarhsoftt5s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt5s1e1
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
      rrgg2qqbarhsoftt5s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s1e0
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
      rrgg2qqbarhsoftt5s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s1em1
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
      rrgg2qqbarhsoftt5s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s1em2
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
      rrgg2qqbarhsoftt5s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s1em3
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
      rrgg2qqbarhsoftt5s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s1em4
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
      rrgg2qqbarhsoftt5s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhsoftt5s2e1
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
      rrgg2qqbarhsoftt5s2e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s2e0
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
      rrgg2qqbarhsoftt5s2e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s2em1
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
      rrgg2qqbarhsoftt5s2em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s2em2
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
      rrgg2qqbarhsoftt5s2em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s2em3
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
      rrgg2qqbarhsoftt5s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt5s2em4
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
      rrgg2qqbarhsoftt5s2em4 = 0.0D0

      end function
