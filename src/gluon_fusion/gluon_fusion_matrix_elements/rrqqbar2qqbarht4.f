  
      subroutine rrqqbar2qqbarht4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2qqbarh41J1  
      doubleprecision rrqqbar2qqbarh41J2  
      doubleprecision rrqqbar2qqbarh41J3  
      doubleprecision rrqqbar2qqbarht4s1e1  
      doubleprecision rrqqbar2qqbarht4s1e0  
      doubleprecision rrqqbar2qqbarht4s1em1  
      doubleprecision rrqqbar2qqbarht4s1em2  
      doubleprecision rrqqbar2qqbarht4s1em3  
      doubleprecision rrqqbar2qqbarht4s1em4  
      doubleprecision rrqqbar2qqbarht4s2e1  
      doubleprecision rrqqbar2qqbarht4s2e0  
      doubleprecision rrqqbar2qqbarht4s2em1  
      doubleprecision rrqqbar2qqbarht4s2em2  
      doubleprecision rrqqbar2qqbarht4s2em3  
      doubleprecision rrqqbar2qqbarht4s2em4  
      doubleprecision rrqqbar2qqbarht4s3e1  
      doubleprecision rrqqbar2qqbarht4s3e0  
      doubleprecision rrqqbar2qqbarht4s3em1  
      doubleprecision rrqqbar2qqbarht4s3em2  
      doubleprecision rrqqbar2qqbarht4s3em3  
      doubleprecision rrqqbar2qqbarht4s3em4  
      doubleprecision rrqqbar2qqbarht4s4e1  
      doubleprecision rrqqbar2qqbarht4s4e0  
      doubleprecision rrqqbar2qqbarht4s4em1  
      doubleprecision rrqqbar2qqbarht4s4em2  
      doubleprecision rrqqbar2qqbarht4s4em3  
      doubleprecision rrqqbar2qqbarht4s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht4s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2qqbarht4s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2qqbarht4s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht4s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2qqbarht4s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2qqbarht4s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht4s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2qqbarht4s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2qqbarht4s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht4s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2qqbarht4s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2qqbarht4s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht4s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2qqbarht4s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2qqbarht4s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2qqbarht4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2qqbarht4s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqqbar2qqbarht4s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqqbar2qqbarht4s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2qqbarht4s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1
     #, 0.10D1, x4)
      t6 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1
     #, 0.10D1, x4)
      t8 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1
     #, 0.10D1, x4)
      t9 = lh ** 2
      t11 = 0.3141592653589793D1 ** 2
      t13 = 0.180D3 * t9 - 0.30D2 * t11
      t16 = s ** 2
      t17 = 0.1D1 / t16
      t19 = 0.1D1 / t1
      t20 = t19 * 0.3141592653589793D1
      t21 = z ** 2
      t22 = 0.1D1 / t21
      t23 = x3 * t22
      t24 = x4 * 0.3141592653589793D1
      t25 = Sin(t24)
      t26 = t25 ** 2
      t27 = t1 ** 2
      t28 = t27 ** 2
      t29 = t26 * t28
      t32 = log(0.4D1 * t23 * t29)
      t33 = -0.1D1 + x3
      t34 = 0.1D1 / t33
      t35 = t29 * t34
      t38 = log(-0.4D1 * t23 * t35)
      t40 = cos(t24)
      t42 = Sqrt(-x3 * t33)
      t47 = 0.1D1 / (-z - x3 + 0.2D1 * t40 * t42 * z)
      t53 = t38 ** 2
      t57 = t32 ** 2
      t70 = 0.60D2 * lh * t11 - 0.2884936567583026D3 - 0.120D3 * t9 * lh
      t91 = 0.1D1 / x3
      t94 = t22 * t26
      t95 = t94 * t28
      t97 = log(0.4D1 * t95)
      t99 = t97 ** 2
      t102 = t99 * t97
      t117 = t11 ** 2
      t118 = t9 ** 2
      t128 = t99 ** 2
      t135 = t17 * t19
      t136 = z * t6
      t137 = x1 ** 2
      t138 = t23 * t137
      t141 = log(-0.4D1 * t138 * t35)
      t142 = t141 * z
      t144 = t141 ** 2
      t150 = t137 * t26
      t154 = log(0.4D1 * t23 * t150 * t28)
      t156 = t154 ** 2
      t163 = lh * t17
      t164 = z * t3
      t173 = t13 * t17
      t176 = t8 + z * t8 * t47
      t181 = 0.1D1 / x1
      t184 = t22 * t137
      t187 = log(0.4D1 * t184 * t29)
      t193 = t187 ** 2
      t203 = t70 * t17
      t204 = t20 * t8
      t205 = t203 * t204
      t216 = x2 ** 2
      t217 = x3 * t216
      t218 = t217 * t137
      t223 = log(-0.4D1 * t218 * t94 * t28 * t34)
      t230 = log(0.4D1 * t218 * t95)
      t237 = -t20 * t176
      t242 = 0.1D1 / x2
      t243 = t242 * t181
      t246 = t216 * t137
      t249 = log(0.4D1 * t246 * t95)
      t251 = t249 ** 2
      t270 = log(0.4D1 * t217 * t95)
      t272 = t270 ** 2
      t275 = t217 * t26
      t276 = t22 * t28
      t280 = log(-0.4D1 * t275 * t276 * t34)
      t281 = t280 * z
      t283 = t280 ** 2
      t306 = t216 * t26
      t309 = log(0.4D1 * t306 * t276)
      t315 = t309 ** 2
      t335 = ((-0.180D3 * t3 * lh + 0.90D2 * t6 + t8 * t13) * t17 * t20 
     #* (-t32 - t38 * z * t47) + 0.90D2 * t8 * t17 * t20 * (-t53 * t38 *
     # z * t47 / 0.6D1 - t57 * t32 / 0.6D1) + (-0.180D3 * t6 * lh + t8 *
     # t70 + t3 * t13) * t17 * t20 * (0.1D1 + z * t47) + (-0.180D3 * t8 
     #* lh + 0.90D2 * t3) * t17 * t20 * (t57 / 0.2D1 + t53 * z * t47 / 0
     #.2D1)) * t91 / 0.2880D4 + (-0.180D3 * (-t97 * t6 + t99 * t3 / 0.2D
     #1 - t102 * t8 / 0.6D1) * lh + (t6 - t97 * t3 + t99 * t8 / 0.2D1) *
     # t13 + (t3 - t97 * t8) * t70 + t8 * (0.5769873135166051D3 * lh + t
     #117 + 0.60D2 * t118 - 0.60D2 * t9 * t11) + 0.45D2 * t99 * t6 - 0.1
     #5D2 * t102 * t3 + 0.15D2 / 0.4D1 * t128 * t8) * t17 * t20 / 0.2880
     #D4 + (0.90D2 * t135 * 0.3141592653589793D1 * ((t136 - t142 * t3 + 
     #t144 * z * t8 / 0.2D1) * t47 + t6 - t154 * t3 + t156 * t8 / 0.2D1)
     # - 0.180D3 * t163 * t20 * ((t164 - t142 * t8) * t47 + t3 - t154 * 
     #t8) + t173 * t20 * t176) * t91 * t181 / 0.1440D4 + (t173 * t20 * (
     #t3 - t187 * t8) + 0.90D2 * t135 * 0.3141592653589793D1 * (-t187 * 
     #t6 + t193 * t3 / 0.2D1 - t193 * t187 * t8 / 0.6D1) + t205 - 0.180D
     #3 * t163 * t20 * (t6 - t187 * t3 + t193 * t8 / 0.2D1)) * t181 / 0.
     #1440D4 - (0.90D2 * t135 * 0.3141592653589793D1 * (-(t164 - t223 * 
     #z * t8) * t47 - t3 + t230 * t8) - 0.180D3 * t163 * t237) * t91 * t
     #243 / 0.720D3 + (0.90D2 * t135 * 0.3141592653589793D1 * (t6 - t249
     # * t3 + t251 * t8 / 0.2D1) - 0.180D3 * t163 * t20 * (t3 - t249 * t
     #8) + t173 * t204) * t242 * t181 / 0.720D3 - (0.90D2 * t135 * 0.314
     #1592653589793D1 * (-t6 + t270 * t3 - t272 * t8 / 0.2D1 - (t136 - t
     #281 * t3 + t283 * z * t8 / 0.2D1) * t47) - 0.180D3 * t163 * t20 * 
     #(-t3 + t270 * t8 - (t164 - t281 * t8) * t47) + t173 * t237) * t91 
     #* t242 / 0.1440D4 + (t173 * t20 * (t3 - t309 * t8) + 0.90D2 * t135
     # * 0.3141592653589793D1 * (-t309 * t6 + t315 * t3 / 0.2D1 - t315 *
     # t309 * t8 / 0.6D1) + t205 - 0.180D3 * t163 * t20 * (t6 - t309 * t
     #3 + t315 * t8 / 0.2D1)) * t242 / 0.1440D4
      t336 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t335)
      t339 = x2 * t1 * s
      t340 = -0.1D1 + x2
      t342 = t340 * t1 * s
      t343 = -t340
      t344 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t343
     #, 0.10D1, x4)
      t345 = t28 * t340
      t349 = log(-0.4D1 * t218 * t94 * t345)
      t350 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t343
     #, 0.10D1, x4)
      t356 = t20 * t350
      t362 = (0.90D2 * t135 * 0.3141592653589793D1 * (t344 - t349 * t350
     #) - 0.180D3 * t163 * t356) * t91 * t243 / 0.720D3
      t363 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t343
     #, 0.10D1, x4)
      t364 = t246 * t26
      t365 = t276 * t340
      t368 = log(-0.4D1 * t364 * t365)
      t370 = t368 ** 2
      t382 = t173 * t356
      t386 = (0.90D2 * t135 * 0.3141592653589793D1 * (-t363 + t368 * t34
     #4 - t370 * t350 / 0.2D1) - 0.180D3 * t163 * t20 * (-t344 + t368 * 
     #t350) - t382) * t242 * t181 / 0.720D3
      t389 = log(-0.4D1 * t275 * t365)
      t391 = t389 ** 2
      t406 = (0.90D2 * t135 * 0.3141592653589793D1 * (t363 - t389 * t344
     # + t391 * t350 / 0.2D1) - 0.180D3 * t163 * t20 * (t344 - t389 * t3
     #50) + t382) * t91 * t242 / 0.1440D4
      t409 = log(-0.4D1 * t306 * t365)
      t411 = -t344 + t409 * t350
      t415 = t409 ** 2
      t421 = t409 * t363 - t415 * t344 / 0.2D1 + t415 * t409 * t350 / 0.
     #6D1
      t425 = t203 * t356
      t429 = -t363 + t409 * t344 - t415 * t350 / 0.2D1
      t436 = -t362 + t386 - t406 + (t173 * t20 * t411 + 0.90D2 * t135 * 
     #0.3141592653589793D1 * t421 - t425 - 0.180D3 * t163 * t20 * t429) 
     #* t242 / 0.1440D4
      t437 = FJET(XB1, XB2, s, 0.0D0, t339, 0.0D0, -t342, 0.0D0, t436)
      t439 = x2 * x3
      t442 = Sqrt(x3 * t340 * t33)
      t443 = t40 * t442
      t447 = 0.1D1 - x3 + t439
      t448 = 0.1D1 / t447
      t450 = t2 * x2 * (-0.1D1 + t439 + 0.2D1 * t443) * t448
      t452 = 0.2D1 * t443 * x2
      t455 = t2 * (0.1D1 - x3 - x2 + t439 + t217 + t452) * t448
      t456 = t135 * 0.3141592653589793D1
      t457 = t33 * t448
      t458 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t343
     #, -t457, x4)
      t459 = t447 ** 2
      t460 = 0.1D1 / t459
      t467 = log(0.4D1 * t460 * t137 * t94 * t345 * t217 * t33)
      t468 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t343
     #, -t457, x4)
      t471 = x2 * z
      t472 = t439 * z
      t473 = t217 * z
      t479 = 0.1D1 / (z - t471 - t472 + t473 + x2 + x3 - t217 - 0.2D1 * 
     #t443 * z + 0.2D1 * t443 * t471 - t452)
      t481 = -z + t471 - x2
      t485 = t163 * t19
      t487 = t479 * t481
      t488 = 0.3141592653589793D1 * t468 * t487
      t495 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t343
     #, -t457, x4)
      t501 = log(0.4D1 * t217 * t94 * t345 * t33 * t460)
      t503 = t501 ** 2
      t523 = -(0.90D2 * t456 * (t458 - t467 * t468) * t479 * t481 - 0.18
     #0D3 * t485 * t488) * t91 * t243 / 0.720D3 - (0.90D2 * t456 * (t495
     # - t501 * t458 + t503 * t468 / 0.2D1) * t479 * t481 - 0.180D3 * t4
     #85 * 0.3141592653589793D1 * (t458 - t501 * t468) * t487 + t173 * t
     #19 * t488) * t91 * t242 / 0.1440D4
      t524 = FJET(XB1, XB2, s, 0.0D0, -t450, 0.0D0, t455, 0.0D0, t523)
      t526 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t335)
      t529 = x1 * t1 * s
      t530 = -0.1D1 + x1
      t531 = x1 * z
      t532 = 0.1D1 - x1 + t531
      t533 = 0.1D1 / t532
      t535 = t2 * t530 * t533
      t536 = s * t27
      t538 = x1 * t530 * t533
      t539 = t536 * t538
      t540 = -t530
      t541 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, t540, 0.10D1
     #, 0.10D1, x4)
      t544 = t28 * t533
      t545 = t530 ** 2
      t550 = log(-0.4D1 * t23 * t150 * t544 * t545 * t34)
      t551 = t550 * z
      t552 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t540, 0.10D1
     #, 0.10D1, x4)
      t554 = t550 ** 2
      t556 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t540, 0.10D1
     #, 0.10D1, x4)
      t561 = x3 * x1
      t562 = t561 * z
      t563 = 0.3D1 * t562
      t564 = x1 * t21
      t565 = x3 * t21
      t566 = t565 * x1
      t567 = x3 * t137
      t569 = 0.2D1 * t567 * z
      t570 = t567 * t21
      t571 = x3 * t532
      t573 = Sqrt(-t571 * t33)
      t577 = 0.2D1 * t561
      t578 = -z - x3 + t531 - t563 - t564 + t566 + t569 - t570 + 0.2D1 *
     # t40 * t573 * z + t577 - t567
      t579 = 0.1D1 / t578
      t581 = t533 * t545
      t585 = log(0.4D1 * t138 * t29 * t581)
      t587 = t585 ** 2
      t594 = z * t552
      t607 = -t556 - z * t556 * t532 * t579
      t613 = (0.90D2 * t135 * 0.3141592653589793D1 * (-(z * t541 - t551 
     #* t552 + t554 * z * t556 / 0.2D1) * t532 * t579 - t541 + t585 * t5
     #52 - t587 * t556 / 0.2D1) - 0.180D3 * t163 * t20 * (-(t594 - t551 
     #* t556) * t532 * t579 - t552 + t585 * t556) + t173 * t20 * t607) *
     # t91 * t181 / 0.1440D4
      t614 = t184 * t26
      t618 = log(0.4D1 * t614 * t544 * t545)
      t624 = t618 ** 2
      t634 = t20 * t556
      t645 = (-t173 * t20 * (t552 - t618 * t556) - 0.90D2 * t135 * 0.314
     #1592653589793D1 * (-t618 * t541 + t624 * t552 / 0.2D1 - t624 * t61
     #8 * t556 / 0.6D1) - t203 * t634 + 0.180D3 * t163 * t20 * (t541 - t
     #618 * t552 + t624 * t556 / 0.2D1)) * t181 / 0.1440D4
      t647 = t545 * t137 * t94
      t654 = log(-0.4D1 * t647 * t28 * t216 * t34 * t533 * x3)
      t661 = t276 * t581
      t664 = log(0.4D1 * t217 * t150 * t661)
      t677 = (0.90D2 * t135 * 0.3141592653589793D1 * ((t594 - t654 * z *
     # t556) * t532 * t579 + t552 - t664 * t556) + 0.180D3 * t163 * t20 
     #* t607) * t91 * t243 / 0.720D3
      t680 = log(0.4D1 * t364 * t661)
      t682 = t680 ** 2
      t685 = -t541 + t680 * t552 - t682 * t556 / 0.2D1
      t690 = -t552 + t680 * t556
      t694 = t173 * t634
      t699 = t613 + t645 - t677 + (0.90D2 * t135 * 0.3141592653589793D1 
     #* t685 - 0.180D3 * t163 * t20 * t690 - t694) * t242 * t181 / 0.720
     #D3
      t700 = FJET(XB1, XB2, s, t529, 0.0D0, 0.0D0, -t535, -t539, t699)
      t703 = x2 * t530 * t2
      t706 = t340 * t530 * t2 * t533
      t708 = t536 * t340 * t538
      t709 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t540, t343, 
     #0.10D1, x4)
      t715 = log(-0.4D1 * t647 * t345 * t533 * t216 * x3)
      t716 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t540, t343, 
     #0.10D1, x4)
      t722 = t20 * t716
      t728 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, t540, t343, 
     #0.10D1, x4)
      t734 = log(-0.4D1 * t246 * t94 * t544 * t545 * t340)
      t736 = t734 ** 2
      t753 = -(0.90D2 * t135 * 0.3141592653589793D1 * (-t709 + t715 * t7
     #16) + 0.180D3 * t163 * t722) * t91 * t243 / 0.720D3 + (0.90D2 * t1
     #35 * 0.3141592653589793D1 * (t728 - t734 * t709 + t736 * t716 / 0.
     #2D1) - 0.180D3 * t163 * t20 * (t709 - t734 * t716) + t173 * t722) 
     #* t242 * t181 / 0.720D3
      t754 = FJET(XB1, XB2, s, t529, -t703, 0.0D0, t706, t708, t753)
      t756 = FJET(XB1, XB2, s, t455, 0.0D0, -t450, 0.0D0, 0.0D0, t523)
      t758 = FJET(XB1, XB2, s, t706, 0.0D0, -t703, t529, t708, t753)
      t774 = -t362 + t386 - t406 + (t173 * t20 * t411 + 0.90D2 * t135 * 
     #0.3141592653589793D1 * t421 - t425 - 0.180D3 * t163 * t20 * t429) 
     #* t242 / 0.1440D4
      t775 = FJET(XB1, XB2, s, -t342, 0.0D0, t339, 0.0D0, 0.0D0, t774)
      t789 = t613 + t645 - t677 + (0.90D2 * t135 * 0.3141592653589793D1 
     #* t685 - 0.180D3 * t163 * t20 * t690 - t694) * t242 * t181 / 0.720
     #D3
      t790 = FJET(XB1, XB2, s, -t535, 0.0D0, 0.0D0, t529, -t539, t789)
      t794 = t33 * x1 * t2 * t448
      t795 = t2 * t530
      t798 = Sqrt(t571 * t340 * t33)
      t799 = t40 * t798
      t805 = t795 * x2 * (-0.1D1 + t439 + x1 - t561 - t531 + t562 + 0.2D
     #1 * t799) * t533 * t448
      t807 = t529 * t439 * t448
      t808 = t217 * t531
      t809 = t217 * x1
      t811 = 0.2D1 * t799 * x2
      t815 = t795 * (0.1D1 - x3 + t808 - x2 + t439 - t809 + t811 + t217)
     # * t533 * t448
      t816 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t540, t343, 
     #-t457, x4)
      t825 = log(0.4D1 * t460 * t545 * t614 * t345 * t216 * t533 * x3 * 
     #t33)
      t826 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t540, t343, 
     #-t457, x4)
      t829 = x2 * x1
      t830 = t829 * z
      t835 = x2 * t137
      t844 = -t809 + t811 - 0.3D1 * t830 - t439 * x1 + t567 * x2 + t829 
     #* t21 + 0.2D1 * t835 * z - t835 * t21 + 0.2D1 * t799 * z + t531 - 
     #t564 + t577 - t567 + 0.2D1 * t829 - t835 + t808 - 0.2D1 * t799 * t
     #471
      t856 = -0.2D1 * t799 * t829 + 0.2D1 * t439 * t531 - t565 * t829 - 
     #0.2D1 * t567 * t471 + t567 * t21 * x2 - t563 + t566 + t569 - t570 
     #- x2 + t472 - t473 + 0.2D1 * t799 * t830 + t217 + t471 - z - x3
      t858 = 0.1D1 / (t844 + t856)
      t861 = t532 * (x2 - t829 + z - t471 + t830)
      t870 = -0.90D2 * t456 * (t816 - t825 * t826) * t858 * t861 + 0.180
     #D3 * t163 * t20 * t826 * t858 * t861
      t873 = t870 * t91 * t243 / 0.720D3
      t874 = FJET(XB1, XB2, s, -t794, t805, t807, -t815, t708, -t873)
      t877 = t91 * t242 * t181
      t880 = FJET(XB1, XB2, s, -t815, t807, t805, -t794, t708, -t873)
      rrqqbar2qqbarht4s1e1 = t336 * t335 + t437 * t436 + t524 * t523 + t
     #526 * t335 + t700 * t699 + t754 * t753 + t756 * t523 + t758 * t753
     # + t775 * t774 + t790 * t789 - t874 * t870 * t877 / 0.720D3 - t880
     # * t870 * t877 / 0.720D3

      end function



      doubleprecision function rrqqbar2qqbarht4s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1
     #, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = 0.1D1 / t1
      t8 = t7 * 0.3141592653589793D1
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = t20 ** 2
      t22 = -0.1D1 + x3
      t23 = 0.1D1 / t22
      t24 = t17 * t23
      t27 = log(-0.4D1 * t11 * t24)
      t28 = t27 ** 2
      t30 = cos(t12)
      t32 = Sqrt(-x3 * t22)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t32 * z)
      t46 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D
     #1, 0.10D1, x4)
      t57 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D
     #1, 0.10D1, x4)
      t59 = lh ** 2
      t61 = 0.3141592653589793D1 ** 2
      t63 = 0.180D3 * t59 - 0.30D2 * t61
      t72 = 0.1D1 / x3
      t75 = t10 * t14
      t76 = t75 * t16
      t78 = log(0.4D1 * t76)
      t80 = t78 ** 2
      t106 = t5 * t7
      t107 = x2 ** 2
      t108 = t107 * x3
      t111 = log(0.4D1 * t108 * t76)
      t113 = z * t46
      t114 = t108 * t14
      t115 = t10 * t16
      t119 = log(-0.4D1 * t114 * t115 * t23)
      t128 = lh * t5
      t131 = -t3 - z * t3 * t37
      t137 = 0.1D1 / x2
      t140 = t107 * t14
      t143 = log(0.4D1 * t140 * t115)
      t145 = t143 ** 2
      t157 = t63 * t5
      t158 = t8 * t3
      t159 = t157 * t158
      t163 = x1 ** 2
      t164 = t11 * t163
      t167 = log(-0.4D1 * t164 * t24)
      t172 = t163 * t14
      t176 = log(0.4D1 * t11 * t172 * t16)
      t188 = 0.1D1 / x1
      t191 = t106 * 0.3141592653589793D1
      t193 = t137 * t188
      t197 = t107 * t163
      t200 = log(0.4D1 * t197 * t76)
      t212 = t10 * t163
      t215 = log(0.4D1 * t212 * t17)
      t217 = t215 ** 2
      t232 = (0.90D2 * t3 * t5 * t8 * (t21 / 0.2D1 + t28 * z * t37 / 0.2
     #D1) + (-0.180D3 * t3 * lh + 0.90D2 * t46) * t5 * t8 * (-t20 - t27 
     #* z * t37) + (-0.180D3 * t46 * lh + 0.90D2 * t57 + t3 * t63) * t5 
     #* t8 * (0.1D1 + z * t37)) * t72 / 0.2880D4 + (-0.180D3 * (t57 - t7
     #8 * t46 + t80 * t3 / 0.2D1) * lh + t3 * (0.60D2 * lh * t61 - 0.288
     #4936567583026D3 - 0.120D3 * t59 * lh) - 0.90D2 * t78 * t57 + 0.45D
     #2 * t80 * t46 - 0.15D2 * t80 * t78 * t3 + (t46 - t78 * t3) * t63) 
     #* t5 * t8 / 0.2880D4 - (0.90D2 * t106 * 0.3141592653589793D1 * (-t
     #46 + t111 * t3 - (t113 - t119 * z * t3) * t37) - 0.180D3 * t128 * 
     #t8 * t131) * t72 * t137 / 0.1440D4 + (0.90D2 * t106 * 0.3141592653
     #589793D1 * (t57 - t143 * t46 + t145 * t3 / 0.2D1) - 0.180D3 * t128
     # * t8 * (t46 - t143 * t3) + t159) * t137 / 0.1440D4 + (0.90D2 * t1
     #06 * 0.3141592653589793D1 * ((t113 - t167 * z * t3) * t37 + t46 - 
     #t176 * t3) + 0.180D3 * t128 * t8 * t131) * t72 * t188 / 0.1440D4 -
     # t191 * t131 * t72 * t193 / 0.8D1 + (0.90D2 * t106 * 0.31415926535
     #89793D1 * (t46 - t200 * t3) - 0.180D3 * t128 * t158) * t137 * t188
     # / 0.720D3 + (0.90D2 * t106 * 0.3141592653589793D1 * (t57 - t215 *
     # t46 + t217 * t3 / 0.2D1) - 0.180D3 * t128 * t8 * (t46 - t215 * t3
     #) + t159) * t188 / 0.1440D4
      t233 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t232)
      t236 = x2 * t1 * s
      t237 = -0.1D1 + x2
      t239 = t237 * t1 * s
      t240 = -t237
      t241 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t240
     #, 0.10D1, x4)
      t242 = t115 * t237
      t245 = log(-0.4D1 * t114 * t242)
      t246 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t240
     #, 0.10D1, x4)
      t252 = t8 * t246
      t254 = 0.180D3 * t128 * t252
      t258 = (0.90D2 * t106 * 0.3141592653589793D1 * (t241 - t245 * t246
     #) - t254) * t72 * t137 / 0.1440D4
      t259 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t240
     #, 0.10D1, x4)
      t262 = log(-0.4D1 * t140 * t242)
      t264 = t262 ** 2
      t267 = -t259 + t262 * t241 - t264 * t246 / 0.2D1
      t272 = -t241 + t262 * t246
      t276 = t157 * t252
      t283 = t191 * t246 * t72 * t193 / 0.8D1
      t284 = t197 * t14
      t287 = log(-0.4D1 * t284 * t242)
      t296 = (0.90D2 * t106 * 0.3141592653589793D1 * (-t241 + t287 * t24
     #6) + t254) * t137 * t188 / 0.720D3
      t297 = -t258 + (0.90D2 * t106 * 0.3141592653589793D1 * t267 - 0.18
     #0D3 * t128 * t8 * t272 - t276) * t137 / 0.1440D4 - t283 + t296
      t298 = FJET(XB1, XB2, s, 0.0D0, t236, 0.0D0, -t239, 0.0D0, t297)
      t300 = x2 * x3
      t303 = Sqrt(x3 * t237 * t22)
      t304 = t30 * t303
      t308 = 0.1D1 - x3 + t300
      t309 = 0.1D1 / t308
      t311 = t2 * x2 * (-0.1D1 + t300 + 0.2D1 * t304) * t309
      t313 = 0.2D1 * t304 * x2
      t316 = t2 * (0.1D1 - x3 - x2 + t300 + t108 + t313) * t309
      t317 = t22 * t309
      t318 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t240
     #, -t317, x4)
      t321 = t308 ** 2
      t327 = log(0.4D1 * t108 * t75 * t16 * t237 * t22 / t321)
      t328 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t240
     #, -t317, x4)
      t331 = x2 * z
      t332 = t300 * z
      t333 = t108 * z
      t339 = 0.1D1 / (z - t331 - t332 + t333 + x2 + x3 - t108 - 0.2D1 * 
     #t304 * z + 0.2D1 * t304 * t331 - t313)
      t341 = -z + t331 - x2
      t346 = 0.3141592653589793D1 * t328
      t347 = t339 * t341
      t357 = t72 * t137 * t188
      t361 = -(0.90D2 * t191 * (t318 - t327 * t328) * t339 * t341 - 0.18
     #0D3 * t128 * t7 * t346 * t347) * t72 * t137 / 0.1440D4 - t106 * t3
     #46 * t347 * t357 / 0.8D1
      t362 = FJET(XB1, XB2, s, 0.0D0, -t311, 0.0D0, t316, 0.0D0, t361)
      t364 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t232)
      t367 = x1 * t1 * s
      t368 = -0.1D1 + x1
      t369 = x1 * z
      t370 = 0.1D1 - x1 + t369
      t371 = 0.1D1 / t370
      t373 = t2 * t368 * t371
      t374 = s * t15
      t376 = x1 * t368 * t371
      t377 = t374 * t376
      t378 = -t368
      t379 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t378, 0.10D1
     #, 0.10D1, x4)
      t382 = t16 * t371
      t383 = t368 ** 2
      t388 = log(-0.4D1 * t11 * t172 * t382 * t383 * t23)
      t390 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t378, 0.10D1
     #, 0.10D1, x4)
      t394 = x3 * x1
      t395 = t394 * z
      t396 = 0.3D1 * t395
      t397 = x1 * t9
      t398 = x3 * t9
      t399 = t398 * x1
      t400 = x3 * t163
      t402 = 0.2D1 * t400 * z
      t403 = t400 * t9
      t404 = x3 * t370
      t406 = Sqrt(-t404 * t22)
      t410 = 0.2D1 * t394
      t411 = -z - x3 + t369 - t396 - t397 + t399 + t402 - t403 + 0.2D1 *
     # t30 * t406 * z + t410 - t400
      t412 = 0.1D1 / t411
      t414 = t371 * t383
      t418 = log(0.4D1 * t164 * t17 * t414)
      t427 = -t390 - z * t390 * t370 * t412
      t434 = (0.90D2 * t106 * 0.3141592653589793D1 * (-(z * t379 - t388 
     #* z * t390) * t370 * t412 - t379 + t418 * t390) - 0.180D3 * t128 *
     # t8 * t427) * t72 * t188 / 0.1440D4
      t439 = -t191 * t427 * t72 * t193 / 0.8D1
      t443 = log(0.4D1 * t284 * t115 * t414)
      t445 = -t379 + t443 * t390
      t449 = t8 * t390
      t451 = 0.180D3 * t128 * t449
      t456 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, t378, 0.10D1
     #, 0.10D1, x4)
      t461 = log(0.4D1 * t212 * t14 * t382 * t383)
      t463 = t461 ** 2
      t478 = (-0.90D2 * t106 * 0.3141592653589793D1 * (t456 - t461 * t37
     #9 + t463 * t390 / 0.2D1) + 0.180D3 * t128 * t8 * (t379 - t461 * t3
     #90) - t157 * t449) * t188 / 0.1440D4
      t479 = t434 - t439 + (0.90D2 * t106 * 0.3141592653589793D1 * t445 
     #+ t451) * t137 * t188 / 0.720D3 + t478
      t480 = FJET(XB1, XB2, s, t367, 0.0D0, 0.0D0, -t373, -t377, t479)
      t483 = x2 * t368 * t2
      t486 = t237 * t368 * t2 * t371
      t488 = t374 * t237 * t376
      t489 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t378, t240, 
     #0.10D1, x4)
      t494 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t378, t240, 
     #0.10D1, x4)
      t500 = log(-0.4D1 * t197 * t75 * t382 * t383 * t237)
      t513 = t191 * t489 * t72 * t193 / 0.8D1 + (0.90D2 * t106 * 0.31415
     #92653589793D1 * (t494 - t500 * t489) - 0.180D3 * t128 * t8 * t489)
     # * t137 * t188 / 0.720D3
      t514 = FJET(XB1, XB2, s, t367, -t483, 0.0D0, t486, t488, t513)
      t516 = FJET(XB1, XB2, s, t316, 0.0D0, -t311, 0.0D0, 0.0D0, t361)
      t518 = FJET(XB1, XB2, s, t486, 0.0D0, -t483, t367, t488, t513)
      t531 = -t258 + (0.90D2 * t106 * 0.3141592653589793D1 * t267 - 0.18
     #0D3 * t128 * t8 * t272 - t276) * t137 / 0.1440D4 - t283 + t296
      t532 = FJET(XB1, XB2, s, -t239, 0.0D0, t236, 0.0D0, 0.0D0, t531)
      t542 = t434 - t439 + (0.90D2 * t106 * 0.3141592653589793D1 * t445 
     #+ t451) * t137 * t188 / 0.720D3 + t478
      t543 = FJET(XB1, XB2, s, -t373, 0.0D0, 0.0D0, t367, -t377, t542)
      t547 = t22 * x1 * t2 * t309
      t548 = t2 * t368
      t551 = Sqrt(t404 * t237 * t22)
      t552 = t30 * t551
      t558 = t548 * x2 * (-0.1D1 + t300 + x1 - t394 - t369 + t395 + 0.2D
     #1 * t552) * t371 * t309
      t560 = t367 * t300 * t309
      t561 = t108 * t369
      t562 = t108 * x1
      t564 = 0.2D1 * t552 * x2
      t568 = t548 * (0.1D1 - x3 + t561 - x2 + t300 - t562 + t564 + t108)
     # * t371 * t309
      t569 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t378, t240, 
     #-t317, x4)
      t571 = x2 * x1
      t572 = t571 * z
      t577 = x2 * t163
      t583 = -t396 + t399 + t402 - t403 - t562 + t564 - 0.3D1 * t572 - t
     #300 * x1 + t400 * x2 + t571 * t9 + 0.2D1 * t577 * z - t577 * t9 + 
     #0.2D1 * z * t552 + t108 + t331 - z - x3
      t598 = t561 - 0.2D1 * t552 * t331 - 0.2D1 * t552 * t571 + 0.2D1 * 
     #t300 * t369 - t398 * t571 - 0.2D1 * t400 * t331 + t400 * t9 * x2 +
     # 0.2D1 * t552 * t572 - x2 + t332 - t333 + 0.2D1 * t571 - t577 + t3
     #69 - t397 + t410 - t400
      t600 = 0.1D1 / (t583 + t598)
      t603 = x2 - t571 + z - t331 + t572
      t607 = t106 * 0.3141592653589793D1 * t569 * t600 * t370 * t603 * t
     #357 / 0.8D1
      t608 = FJET(XB1, XB2, s, -t547, t558, t560, -t568, t488, t607)
      t610 = t8 * t569
      t614 = t600 * t370 * t603 * t357
      t617 = FJET(XB1, XB2, s, -t568, t560, t558, -t547, t488, t607)
      rrqqbar2qqbarht4s1e0 = t233 * t232 + t298 * t297 + t362 * t361 + t
     #364 * t232 + t480 * t479 + t514 * t513 + t516 * t361 + t518 * t513
     # + t532 * t531 + t543 * t542 + t608 * t5 * t610 * t614 / 0.8D1 + t
     #617 * t5 * t610 * t614 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht4s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1
     #, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = 0.1D1 / t1
      t8 = t7 * 0.3141592653589793D1
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = -0.1D1 + x3
      t26 = log(-0.4D1 * t11 * t17 / t21)
      t28 = cos(t12)
      t30 = Sqrt(-x3 * t21)
      t35 = 0.1D1 / (-z - x3 + 0.2D1 * t28 * t30 * z)
      t43 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D
     #1, 0.10D1, x4)
      t52 = 0.1D1 / x3
      t58 = log(0.4D1 * t10 * t14 * t16)
      t63 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D
     #1, 0.10D1, x4)
      t67 = t58 ** 2
      t70 = lh ** 2
      t72 = 0.3141592653589793D1 ** 2
      t80 = t5 * t7
      t81 = t80 * 0.3141592653589793D1
      t84 = -t3 - z * t3 * t35
      t86 = 0.1D1 / x2
      t90 = x2 ** 2
      t91 = t90 * t14
      t92 = t10 * t16
      t95 = log(0.4D1 * t91 * t92)
      t101 = lh * t5
      t104 = 0.180D3 * t101 * t8 * t3
      t110 = 0.1D1 / x1
      t114 = x1 ** 2
      t115 = t10 * t114
      t118 = log(0.4D1 * t115 * t17)
      t132 = (0.90D2 * t6 * t8 * (-t20 - t26 * z * t35) + (-0.180D3 * t3
     # * lh + 0.90D2 * t43) * t5 * t8 * (0.1D1 + z * t35)) * t52 / 0.288
     #0D4 + (-0.180D3 * (t43 - t58 * t3) * lh + 0.90D2 * t63 - 0.90D2 * 
     #t58 * t43 + 0.45D2 * t67 * t3 + t3 * (0.180D3 * t70 - 0.30D2 * t72
     #)) * t5 * t8 / 0.2880D4 - t81 * t84 * t52 * t86 / 0.16D2 + (0.90D2
     # * t80 * 0.3141592653589793D1 * (t43 - t95 * t3) - t104) * t86 / 0
     #.1440D4 + t6 * t7 * 0.3141592653589793D1 * t86 * t110 / 0.8D1 + (0
     #.90D2 * t80 * 0.3141592653589793D1 * (t43 - t118 * t3) - t104) * t
     #110 / 0.1440D4 - t81 * t84 * t52 * t110 / 0.16D2
      t133 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t132)
      t136 = x2 * t1 * s
      t137 = -0.1D1 + x2
      t139 = t137 * t1 * s
      t140 = -t137
      t141 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t140
     #, 0.10D1, x4)
      t145 = t81 * t141 * t52 * t86 / 0.16D2
      t146 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t140
     #, 0.10D1, x4)
      t150 = log(-0.4D1 * t91 * t92 * t137)
      t152 = -t146 + t150 * t141
      t158 = 0.180D3 * t101 * t8 * t141
      t165 = t81 * t141 * t86 * t110 / 0.8D1
      t166 = -t145 + (0.90D2 * t80 * 0.3141592653589793D1 * t152 + t158)
     # * t86 / 0.1440D4 - t165
      t167 = FJET(XB1, XB2, s, 0.0D0, t136, 0.0D0, -t139, 0.0D0, t166)
      t169 = x2 * x3
      t172 = Sqrt(x3 * t137 * t21)
      t173 = t28 * t172
      t178 = 0.1D1 / (0.1D1 - x3 + t169)
      t180 = t2 * x2 * (-0.1D1 + t169 + 0.2D1 * t173) * t178
      t181 = t90 * x3
      t183 = 0.2D1 * t173 * x2
      t186 = t2 * (0.1D1 - x3 - x2 + t169 + t181 + t183) * t178
      t188 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, t140
     #, -t21 * t178, x4)
      t191 = x2 * z
      t199 = 0.1D1 / (z - t191 - t169 * z + t181 * z + x2 + x3 - t181 - 
     #0.2D1 * t173 * z + 0.2D1 * t173 * t191 - t183)
      t200 = -z + t191 - x2
      t205 = t80 * 0.3141592653589793D1 * t188 * t199 * t200 * t52 * t86
     # / 0.16D2
      t206 = FJET(XB1, XB2, s, 0.0D0, -t180, 0.0D0, t186, 0.0D0, -t205)
      t212 = t188 * t199 * t200 * t52 * t86
      t215 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t132)
      t218 = x1 * t1 * s
      t219 = -0.1D1 + x1
      t220 = x1 * z
      t221 = 0.1D1 - x1 + t220
      t222 = 0.1D1 / t221
      t224 = t2 * t219 * t222
      t225 = s * t15
      t227 = x1 * t219 * t222
      t228 = t225 * t227
      t229 = -t219
      t230 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t229, 0.10D1
     #, 0.10D1, x4)
      t235 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t229, 0.10D1
     #, 0.10D1, x4)
      t238 = t219 ** 2
      t242 = log(0.4D1 * t115 * t14 * t16 * t222 * t238)
      t255 = x3 * x1
      t261 = x3 * t114
      t267 = Sqrt(-x3 * t221 * t21)
      t272 = -z - x3 + t220 - 0.3D1 * t255 * z - x1 * t9 + x3 * t9 * x1 
     #+ 0.2D1 * t261 * z - t261 * t9 + 0.2D1 * t28 * t267 * z + 0.2D1 * 
     #t255 - t261
      t281 = -t81 * t230 * t86 * t110 / 0.8D1 + (-0.90D2 * t80 * 0.31415
     #92653589793D1 * (t235 - t242 * t230) + 0.180D3 * t101 * t8 * t230)
     # * t110 / 0.1440D4 + t81 * (-t230 - z * t230 * t221 / t272) * t52 
     #* t110 / 0.16D2
      t282 = FJET(XB1, XB2, s, t218, 0.0D0, 0.0D0, -t224, -t228, t281)
      t285 = x2 * t219 * t2
      t288 = t137 * t219 * t2 * t222
      t290 = t225 * t137 * t227
      t291 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t229, t140, 
     #0.10D1, x4)
      t295 = t81 * t291 * t86 * t110 / 0.8D1
      t296 = FJET(XB1, XB2, s, t218, -t285, 0.0D0, t288, t290, t295)
      t301 = 0.3141592653589793D1 * t291 * t86 * t110
      t304 = FJET(XB1, XB2, s, t186, 0.0D0, -t180, 0.0D0, 0.0D0, -t205)
      t309 = FJET(XB1, XB2, s, t288, 0.0D0, -t285, t218, t290, t295)
      t321 = -t145 + (0.90D2 * t80 * 0.3141592653589793D1 * t152 + t158)
     # * t86 / 0.1440D4 - t165
      t322 = FJET(XB1, XB2, s, -t139, 0.0D0, t136, 0.0D0, 0.0D0, t321)
      t324 = FJET(XB1, XB2, s, -t224, 0.0D0, 0.0D0, t218, -t228, t281)
      rrqqbar2qqbarht4s1em1 = t133 * t132 + t167 * t166 - t206 * t5 * t8
     # * t212 / 0.16D2 + t215 * t132 + t282 * t281 + t296 * t5 * t7 * t3
     #01 / 0.8D1 - t304 * t5 * t8 * t212 / 0.16D2 + t309 * t5 * t7 * t30
     #1 / 0.8D1 + t322 * t321 + t324 * t281

      end function



      doubleprecision function rrqqbar2qqbarht4s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1
     #, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = 0.1D1 / t1
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t26 = t7 * 0.3141592653589793D1
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t37 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D
     #1, 0.10D1, x4)
      t39 = z ** 2
      t41 = Sin(t9)
      t42 = t41 ** 2
      t44 = t1 ** 2
      t45 = t44 ** 2
      t48 = log(0.4D1 / t39 * t42 * t45)
      t55 = t6 * t7 * 0.3141592653589793D1 * (0.1D1 + z / (-z - x3 + 0.2
     #D1 * t10 * t13 * z)) / x3 / 0.32D2 + t6 * t26 * t27 / 0.16D2 + t6 
     #* t26 * t31 / 0.16D2 + (-0.180D3 * t3 * lh + 0.90D2 * t37 - 0.90D2
     # * t48 * t3) * t5 * t26 / 0.2880D4
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t55)
      t59 = x2 * t1 * s
      t60 = -0.1D1 + x2
      t62 = t60 * t1 * s
      t63 = t5 * t7
      t65 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, -t60,
     # 0.10D1, x4)
      t67 = 0.3141592653589793D1 * t65 * t27
      t69 = t63 * t67 / 0.16D2
      t70 = FJET(XB1, XB2, s, 0.0D0, t59, 0.0D0, -t62, 0.0D0, -t69)
      t75 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t55)
      t78 = x1 * t1 * s
      t79 = -0.1D1 + x1
      t82 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t84 = t2 * t79 * t82
      t88 = s * t44 * x1 * t79 * t82
      t90 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, -t79, 0.10D1,
     # 0.10D1, x4)
      t92 = 0.3141592653589793D1 * t90 * t31
      t94 = t63 * t92 / 0.16D2
      t95 = FJET(XB1, XB2, s, t78, 0.0D0, 0.0D0, -t84, -t88, -t94)
      t100 = FJET(XB1, XB2, s, -t62, 0.0D0, t59, 0.0D0, 0.0D0, -t69)
      t105 = FJET(XB1, XB2, s, -t84, 0.0D0, 0.0D0, t78, -t88, -t94)
      rrqqbar2qqbarht4s1em2 = t56 * t55 - t70 * t5 * t7 * t67 / 0.16D2 +
     # t75 * t55 - t95 * t5 * t7 * t92 / 0.16D2 - t100 * t5 * t7 * t67 /
     # 0.16D2 - t105 * t5 * t7 * t92 / 0.16D2

      end function



      doubleprecision function rrqqbar2qqbarht4s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.10D1
     #, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t7 = 0.1D1 / t1
      t10 = t3 * t5 * t7 * 0.3141592653589793D1 / 0.32D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = t5 * t7 * 0.3141592653589793D1
      t16 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2qqbarht4s1em3 = t11 * t3 * t14 / 0.32D2 + t16 * t3 * t14 /
     # 0.32D2

      end function



      doubleprecision function rrqqbar2qqbarht4s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      rrqqbar2qqbarht4s1em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2qqbarht4s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = t4 - t6
      t8 = s ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = 0.1D1 / t1
      t12 = t11 * 0.3141592653589793D1
      t13 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t15
      t17 = x4 * 0.3141592653589793D1
      t18 = Sin(t17)
      t19 = t18 ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t19 * t21
      t25 = log(0.4D1 * t16 * t22)
      t26 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t31 = t9 * t11
      t32 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t34 = t25 ** 2
      t45 = 0.60D2 * lh * t5
      t47 = 0.120D3 * t3 * lh
      t48 = t45 - 0.2884936567583026D3 - t47
      t49 = t48 * t9
      t50 = t12 * t26
      t51 = t49 * t50
      t52 = lh * t9
      t61 = 0.1D1 / x3
      t64 = t15 * t19
      t65 = t64 * t21
      t67 = log(0.4D1 * t65)
      t70 = t67 ** 2
      t79 = t70 * t67
      t93 = t5 ** 2
      t94 = t3 ** 2
      t98 = t70 ** 2
      t104 = x1 ** 2
      t105 = t104 * t19
      t109 = log(0.4D1 * t16 * t105 * t21)
      t111 = t109 ** 2
      t123 = t10 * t50
      t126 = 0.1D1 / x1
      t129 = t15 * t104
      t132 = log(0.4D1 * t129 * t22)
      t138 = t132 ** 2
      t158 = x3 * t104
      t159 = t158 * x2
      t162 = log(0.4D1 * t159 * t65)
      t172 = 0.1D1 / x2
      t173 = t172 * t126
      t176 = x2 * t104
      t179 = log(0.4D1 * t176 * t65)
      t181 = t179 ** 2
      t197 = x2 * x3
      t200 = log(0.4D1 * t197 * t65)
      t202 = t200 ** 2
      t218 = x2 * t19
      t219 = t15 * t21
      t222 = log(0.4D1 * t218 * t219)
      t228 = t222 ** 2
      t248 = (t10 * t12 * (t13 - t25 * t26) + 0.90D2 * t31 * 0.314159265
     #3589793D1 * (-t25 * t32 + t34 * t13 / 0.2D1 - t34 * t25 * t26 / 0.
     #6D1) + t51 - 0.180D3 * t52 * t12 * (t32 - t25 * t13 + t34 * t26 / 
     #0.2D1)) * t61 / 0.1440D4 + (0.180D3 * t67 * lh + 0.45D2 * t70 + t4
     # - t6) * t9 * t12 * t32 / 0.1440D4 + (-0.90D2 * t70 * lh + t45 - 0
     #.2884936567583026D3 - t47 - 0.15D2 * t79 - t67 * t7) * t9 * t12 * 
     #t13 / 0.1440D4 + (0.30D2 * t79 * lh + t70 * t7 / 0.2D1 - t67 * t48
     # + 0.5769873135166051D3 * lh + t93 + 0.60D2 * t94 - 0.60D2 * t3 * 
     #t5 + 0.15D2 / 0.4D1 * t98) * t9 * t50 / 0.1440D4 + (0.90D2 * t31 *
     # 0.3141592653589793D1 * (t32 - t109 * t13 + t111 * t26 / 0.2D1) - 
     #0.180D3 * t52 * t12 * (t13 - t109 * t26) + t123) * t61 * t126 / 0.
     #720D3 + (t10 * t12 * (t13 - t132 * t26) + 0.90D2 * t31 * 0.3141592
     #653589793D1 * (-t132 * t32 + t138 * t13 / 0.2D1 - t138 * t132 * t2
     #6 / 0.6D1) + t51 - 0.180D3 * t52 * t12 * (t32 - t132 * t13 + t138 
     #* t26 / 0.2D1)) * t126 / 0.720D3 + (0.90D2 * t31 * 0.3141592653589
     #793D1 * (t13 - t162 * t26) - 0.180D3 * t52 * t50) * t61 * t173 / 0
     #.720D3 + (0.90D2 * t31 * 0.3141592653589793D1 * (t32 - t179 * t13 
     #+ t181 * t26 / 0.2D1) - 0.180D3 * t52 * t12 * (t13 - t179 * t26) +
     # t123) * t172 * t126 / 0.720D3 + (0.90D2 * t31 * 0.314159265358979
     #3D1 * (t32 - t200 * t13 + t202 * t26 / 0.2D1) - 0.180D3 * t52 * t1
     #2 * (t13 - t200 * t26) + t123) * t61 * t172 / 0.1440D4 - (t10 * t1
     #2 * (-t13 + t222 * t26) + 0.90D2 * t31 * 0.3141592653589793D1 * (t
     #222 * t32 - t228 * t13 / 0.2D1 + t228 * t222 * t26 / 0.6D1) - t51 
     #- 0.180D3 * t52 * t12 * (-t32 + t222 * t13 - t228 * t26 / 0.2D1)) 
     #* t172 / 0.1440D4
      t249 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t248)
      t251 = -0.1D1 + x1
      t253 = t251 * t1 * s
      t255 = x1 * t1 * s
      t256 = -t251
      t257 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, t256, 0.0D0,
     # 0.10D1, x4)
      t259 = x1 * z
      t260 = 0.1D1 - x1 + t259
      t261 = 0.1D1 / t260
      t262 = t251 ** 2
      t263 = t261 * t262
      t267 = log(0.4D1 * t16 * t104 * t22 * t263)
      t268 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t256, 0.0D0,
     # 0.10D1, x4)
      t270 = t267 ** 2
      t271 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t256, 0.0D0,
     # 0.10D1, x4)
      t283 = t12 * t271
      t284 = t10 * t283
      t287 = (0.90D2 * t31 * 0.3141592653589793D1 * (-t257 + t267 * t268
     # - t270 * t271 / 0.2D1) - 0.180D3 * t52 * t12 * (-t268 + t267 * t2
     #71) - t284) * t61 * t126
      t289 = t21 * t261
      t293 = log(0.4D1 * t129 * t19 * t289 * t262)
      t295 = -t268 + t293 * t271
      t299 = t293 ** 2
      t305 = t293 * t257 - t299 * t268 / 0.2D1 + t299 * t293 * t271 / 0.
     #6D1
      t309 = t49 * t283
      t313 = -t257 + t293 * t268 - t299 * t271 / 0.2D1
      t319 = t197 * t105
      t320 = t219 * t263
      t323 = log(0.4D1 * t319 * t320)
      t333 = (0.90D2 * t31 * 0.3141592653589793D1 * (-t268 + t323 * t271
     #) + 0.180D3 * t52 * t283) * t61 * t173
      t334 = t176 * t19
      t337 = log(0.4D1 * t334 * t320)
      t339 = t337 ** 2
      t353 = (0.90D2 * t31 * 0.3141592653589793D1 * (-t257 + t337 * t268
     # - t339 * t271 / 0.2D1) - 0.180D3 * t52 * t12 * (-t268 + t337 * t2
     #71) - t284) * t172 * t126
      t355 = t287 / 0.720D3 + (t10 * t12 * t295 + 0.90D2 * t31 * 0.31415
     #92653589793D1 * t305 - t309 - 0.180D3 * t52 * t12 * t313) * t126 /
     # 0.720D3 + t333 / 0.720D3 + t353 / 0.720D3
      t356 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t253, t255, 0.0D0, t355)
      t358 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t248)
      t360 = -0.1D1 + x3
      t361 = t2 * t360
      t362 = t2 * x3
      t363 = -t360
      t364 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t363, x4)
      t366 = t219 * t360
      t369 = log(-0.4D1 * x3 * t19 * t366)
      t370 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t363, x4)
      t375 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t363, x4)
      t377 = t369 ** 2
      t387 = t12 * t370
      t399 = t197 * t19
      t402 = log(-0.4D1 * t399 * t366)
      t404 = t402 ** 2
      t416 = t10 * t387
      t424 = log(-0.4D1 * t158 * t19 * t366)
      t426 = t424 ** 2
      t446 = log(-0.4D1 * t159 * t64 * t21 * t360)
      t458 = (-t10 * t12 * (t364 - t369 * t370) - 0.90D2 * t31 * 0.31415
     #92653589793D1 * (-t369 * t375 + t377 * t364 / 0.2D1 - t377 * t369 
     #* t370 / 0.6D1) - t49 * t387 + 0.180D3 * t52 * t12 * (t375 - t369 
     #* t364 + t377 * t370 / 0.2D1)) * t61 / 0.1440D4 + (0.90D2 * t31 * 
     #0.3141592653589793D1 * (-t375 + t402 * t364 - t404 * t370 / 0.2D1)
     # - 0.180D3 * t52 * t12 * (-t364 + t402 * t370) - t416) * t61 * t17
     #2 / 0.1440D4 + (0.90D2 * t31 * 0.3141592653589793D1 * (-t375 + t42
     #4 * t364 - t426 * t370 / 0.2D1) - 0.180D3 * t52 * t12 * (-t364 + t
     #424 * t370) - t416) * t61 * t126 / 0.720D3 + (0.90D2 * t31 * 0.314
     #1592653589793D1 * (-t364 + t446 * t370) + 0.180D3 * t52 * t387) * 
     #t61 * t173 / 0.720D3
      t459 = FJET(XB1, XB2, s, 0.0D0, -t361, 0.0D0, t362, 0.0D0, t458)
      t461 = -0.1D1 + x2
      t463 = t461 * t1 * s
      t465 = x2 * t1 * s
      t466 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t467 = t461 ** 2
      t468 = t219 * t467
      t471 = log(0.4D1 * t399 * t468)
      t472 = t471 ** 2
      t473 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t476 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t487 = t12 * t473
      t488 = t10 * t487
      t495 = log(0.4D1 * t218 * t468)
      t501 = t495 ** 2
      t522 = t21 * t467
      t526 = log(0.4D1 * t159 * t64 * t522)
      t540 = log(0.4D1 * t334 * t468)
      t542 = t540 ** 2
      t558 = (0.90D2 * t31 * 0.3141592653589793D1 * (-t466 - t472 * t473
     # / 0.2D1 + t471 * t476) - 0.180D3 * t52 * t12 * (-t476 + t471 * t4
     #73) - t488) * t61 * t172 / 0.1440D4 - (t10 * t12 * (t476 - t495 * 
     #t473) + 0.90D2 * t31 * 0.3141592653589793D1 * (-t495 * t466 + t501
     # * t476 / 0.2D1 - t501 * t495 * t473 / 0.6D1) + t49 * t487 - 0.180
     #D3 * t52 * t12 * (t466 - t495 * t476 + t501 * t473 / 0.2D1)) * t17
     #2 / 0.1440D4 + (0.90D2 * t31 * 0.3141592653589793D1 * (-t476 + t52
     #6 * t473) + 0.180D3 * t52 * t487) * t61 * t173 / 0.720D3 + (0.90D2
     # * t31 * 0.3141592653589793D1 * (-t466 + t540 * t476 - t542 * t473
     # / 0.2D1) - 0.180D3 * t52 * t12 * (-t476 + t540 * t473) - t488) * 
     #t172 * t126 / 0.720D3
      t559 = FJET(XB1, XB2, s, 0.0D0, -t463, 0.0D0, t465, 0.0D0, t558)
      t561 = cos(t17)
      t562 = t197 * t360
      t563 = Sqrt(-t562)
      t564 = t561 * t563
      t565 = 0.2D1 * t564
      t568 = t197 - 0.1D1
      t569 = 0.1D1 / t568
      t571 = t2 * t461 * (-t197 - 0.1D1 + x3 + t565) * t569
      t572 = 0.3D1 * t197
      t573 = x2 ** 2
      t574 = t573 * x3
      t576 = 0.2D1 * t564 * x2
      t579 = t2 * (-x2 - x3 + t572 - t574 - t565 + t576) * t569
      t580 = x2 * z
      t581 = 0.1D1 - x2 + t580
      t582 = t360 * t569
      t583 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #t582, x4)
      t586 = t568 ** 2
      t587 = 0.1D1 / t586
      t592 = log(-0.4D1 * t197 * t64 * t522 * t360 * t587)
      t593 = t592 * t581
      t594 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #t582, x4)
      t596 = t592 ** 2
      t598 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #t582, x4)
      t603 = t574 * z
      t604 = t197 * z
      t607 = 0.2D1 * t197
      t609 = 0.1D1 / (-t603 + t574 - t580 + t604 + 0.2D1 * t564 * t580 +
     # x2 - t607 - t576 - 0.1D1 + t565)
      t613 = t52 * t11
      t614 = t581 * t594
      t624 = 0.3141592653589793D1 * t581 * t598 * t609
      t635 = log(-0.4D1 * t319 * t219 * t467 * t360 * t587)
      t649 = (-0.90D2 * t31 * 0.3141592653589793D1 * (t581 * t583 - t593
     # * t594 + t596 * t581 * t598 / 0.2D1) * t609 + 0.180D3 * t613 * 0.
     #3141592653589793D1 * (t614 - t593 * t598) * t609 - t10 * t11 * t62
     #4) * t61 * t172 / 0.1440D4 + (-0.90D2 * t31 * 0.3141592653589793D1
     # * (t614 - t635 * t581 * t598) * t609 + 0.180D3 * t613 * t624) * t
     #61 * t173 / 0.720D3
      t650 = FJET(XB1, XB2, s, 0.0D0, -t571, 0.0D0, t579, 0.0D0, t649)
      t652 = FJET(XB1, XB2, s, t362, 0.0D0, -t361, 0.0D0, 0.0D0, t458)
      t655 = t461 * t251 * t2
      t658 = t2 * t251 * x2 * t261
      t663 = s * t20 * x2 * x1 * t251 * t261
      t664 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t256, x2, 0.
     #10D1, x4)
      t669 = log(0.4D1 * t319 * t219 * t263 * t467)
      t670 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t256, x2, 0.
     #10D1, x4)
      t676 = t12 * t670
      t682 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, t256, x2, 0.
     #10D1, x4)
      t688 = log(0.4D1 * t176 * t64 * t289 * t262 * t467)
      t690 = t688 ** 2
      t707 = (0.90D2 * t31 * 0.3141592653589793D1 * (t664 - t669 * t670)
     # - 0.180D3 * t52 * t676) * t61 * t173 / 0.720D3 + (0.90D2 * t31 * 
     #0.3141592653589793D1 * (t682 - t688 * t664 + t690 * t670 / 0.2D1) 
     #- 0.180D3 * t52 * t12 * (t664 - t688 * t670) + t10 * t676) * t172 
     #* t126 / 0.720D3
      t708 = FJET(XB1, XB2, s, t255, t655, 0.0D0, -t658, -t663, t707)
      t724 = t287 / 0.720D3 + (t10 * t12 * t295 + 0.90D2 * t31 * 0.31415
     #92653589793D1 * t305 - t309 - 0.180D3 * t52 * t12 * t313) * t126 /
     # 0.720D3 + t333 / 0.720D3 + t353 / 0.720D3
      t725 = FJET(XB1, XB2, s, t255, -t253, 0.0D0, 0.0D0, 0.0D0, t724)
      t727 = FJET(XB1, XB2, s, t465, 0.0D0, -t463, 0.0D0, 0.0D0, t558)
      t729 = FJET(XB1, XB2, s, t579, 0.0D0, -t571, 0.0D0, 0.0D0, t649)
      t731 = t360 * x1
      t733 = t731 * t2 * t569
      t734 = x3 * x1
      t735 = t734 * z
      t739 = Sqrt(-x3 * t260 * x2 * t360)
      t740 = t561 * t739
      t741 = 0.2D1 * t740
      t746 = t253 * t461 * (-t197 - 0.1D1 + x3 + x1 - t734 - t259 + t735
     # + t741) * t261 * t569
      t749 = t255 * x3 * t461 * t569
      t750 = t197 * t259
      t752 = t574 * t259
      t753 = t197 * x1
      t755 = t574 * x1
      t757 = 0.2D1 * t740 * x2
      t758 = -x2 - x3 + 0.2D1 * t750 - t752 + t572 - t735 - 0.2D1 * t753
     # + t755 - t741 + t757 + t734 - t574
      t761 = t253 * t758 * t261 * t569
      t763 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t256, x2, t5
     #82, x4)
      t764 = t262 * t104
      t774 = log(-0.4D1 * t764 * t467 * t19 * t15 * t289 * x2 * x3 * t36
     #0 * t587)
      t775 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t256, x2, t5
     #82, x4)
      t779 = x2 * x1
      t780 = t779 * z
      t796 = 0.1D1 + 0.4D1 * t750 - t752 - x3 * t14 * t779 - 0.2D1 * t15
     #8 * t580 + t158 * t14 * x2 - 0.2D1 * t740 * t580 - 0.2D1 * t740 * 
     #t779 - 0.2D1 * t740 * t259 - 0.2D1 * x1 - x2 + t607 - t574 + t580 
     #- t741 + t104
      t812 = t603 - t604 - 0.3D1 * t753 + t755 + t757 - 0.3D1 * t780 + t
     #159 + t779 * t14 + 0.2D1 * t176 * z - t176 * t14 + 0.2D1 * t740 * 
     #x1 + 0.2D1 * t740 * t780 + 0.2D1 * t259 + 0.2D1 * t779 - 0.2D1 * t
     #104 * z + t104 * t14 - t176
      t815 = (x1 - t259 - t779 + t780 - t580 - 0.1D1 + x2) / (t796 + t81
     #2)
      t824 = 0.90D2 * t31 * 0.3141592653589793D1 * (t763 - t774 * t775) 
     #* t260 * t815 - 0.180D3 * t52 * t12 * t775 * t260 * t815
      t827 = t824 * t61 * t173 / 0.720D3
      t828 = FJET(XB1, XB2, s, t733, t746, t749, -t761, -t663, t827)
      t831 = t61 * t172 * t126
      t835 = t2 * t251 * x3
      t836 = t2 * t734
      t838 = t360 * t251 * t2
      t839 = t731 * t2
      t840 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, t256, 0.0D0,
     # t363, x4)
      t846 = log(-0.4D1 * t158 * t64 * t289 * t262 * t360)
      t847 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t256, 0.0D0,
     # t363, x4)
      t849 = t846 ** 2
      t850 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t256, 0.0D0,
     # t363, x4)
      t862 = t12 * t850
      t872 = log(-0.4D1 * t764 * t261 * t19 * t219 * t562)
      t884 = (0.90D2 * t31 * 0.3141592653589793D1 * (t840 - t846 * t847 
     #+ t849 * t850 / 0.2D1) - 0.180D3 * t52 * t12 * (t847 - t846 * t850
     #) + t10 * t862) * t61 * t126 / 0.720D3 + (0.90D2 * t31 * 0.3141592
     #653589793D1 * (t847 - t872 * t850) - 0.180D3 * t52 * t862) * t61 *
     # t173 / 0.720D3
      t885 = FJET(XB1, XB2, s, -t835, t836, t838, -t839, 0.0D0, t884)
      t887 = FJET(XB1, XB2, s, -t839, t838, t836, -t835, 0.0D0, t884)
      t889 = FJET(XB1, XB2, s, -t658, 0.0D0, t655, t255, -t663, t707)
      t891 = FJET(XB1, XB2, s, -t761, t749, t746, t733, -t663, t827)
      rrqqbar2qqbarht4s2e1 = t249 * t248 + t356 * t355 + t358 * t248 + t
     #459 * t458 + t559 * t558 + t650 * t649 + t652 * t458 + t708 * t707
     # + t725 * t724 + t727 * t558 + t729 * t649 + t828 * t824 * t831 / 
     #0.720D3 + t885 * t884 + t887 * t884 + t889 * t707 + t891 * t824 * 
     #t831 / 0.720D3

      end function



      doubleprecision function rrqqbar2qqbarht4s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0,
     # 0.10D1, x4)
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t10 = x3 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * t10 * t16)
      t20 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t22 = t19 ** 2
      t23 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t30 = lh * t4
      t31 = t5 * 0.3141592653589793D1
      t37 = lh ** 2
      t38 = 0.180D3 * t37
      t39 = 0.3141592653589793D1 ** 2
      t40 = 0.30D2 * t39
      t41 = t38 - t40
      t42 = t41 * t4
      t43 = t31 * t23
      t44 = t42 * t43
      t46 = 0.1D1 / x3
      t50 = t9 * t13
      t51 = t50 * t15
      t53 = log(0.4D1 * t51)
      t62 = t53 ** 2
      t82 = x2 * x3
      t85 = log(0.4D1 * t82 * t51)
      t92 = 0.180D3 * t30 * t43
      t95 = 0.1D1 / x2
      t98 = x2 * t13
      t99 = t9 * t15
      t102 = log(0.4D1 * t98 * t99)
      t104 = t102 ** 2
      t119 = x1 ** 2
      t124 = log(0.4D1 * t10 * t119 * t13 * t15)
      t132 = 0.1D1 / x1
      t135 = t6 * 0.3141592653589793D1
      t137 = t95 * t132
      t141 = x2 * t119
      t144 = log(0.4D1 * t141 * t51)
      t154 = t9 * t119
      t157 = log(0.4D1 * t154 * t16)
      t159 = t157 ** 2
      t174 = (0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t19 * t20 + t22
     # * t23 / 0.2D1) - 0.180D3 * t30 * t31 * (t20 - t19 * t23) + t44) *
     # t46 / 0.1440D4 + (-0.180D3 * lh - 0.90D2 * t53) * t4 * t31 * t7 /
     # 0.1440D4 + (0.180D3 * t53 * lh + 0.45D2 * t62 + t38 - t40) * t4 *
     # t31 * t20 / 0.1440D4 + (-0.90D2 * t62 * lh + 0.60D2 * lh * t39 - 
     #0.2884936567583026D3 - 0.120D3 * t37 * lh - 0.15D2 * t62 * t53 - t
     #53 * t41) * t4 * t43 / 0.1440D4 + (0.90D2 * t6 * 0.314159265358979
     #3D1 * (t20 - t85 * t23) - t92) * t46 * t95 / 0.1440D4 - (0.90D2 * 
     #t6 * 0.3141592653589793D1 * (-t7 + t102 * t20 - t104 * t23 / 0.2D1
     #) - 0.180D3 * t30 * t31 * (-t20 + t102 * t23) - t44) * t95 / 0.144
     #0D4 + (0.90D2 * t6 * 0.3141592653589793D1 * (t20 - t124 * t23) - t
     #92) * t46 * t132 / 0.720D3 + t135 * t23 * t46 * t137 / 0.8D1 + (0.
     #90D2 * t6 * 0.3141592653589793D1 * (t20 - t144 * t23) - t92) * t95
     # * t132 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t1
     #57 * t20 + t159 * t23 / 0.2D1) - 0.180D3 * t30 * t31 * (t20 - t157
     # * t23) + t44) * t132 / 0.720D3
      t175 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t174)
      t177 = -0.1D1 + x1
      t179 = t177 * t1 * s
      t181 = x1 * t1 * s
      t182 = -t177
      t183 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t182, 0.0D0,
     # 0.10D1, x4)
      t185 = x1 * z
      t186 = 0.1D1 - x1 + t185
      t187 = 0.1D1 / t186
      t188 = t177 ** 2
      t189 = t187 * t188
      t193 = log(0.4D1 * t10 * t119 * t16 * t189)
      t194 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t182, 0.0D0,
     # 0.10D1, x4)
      t200 = t31 * t194
      t202 = 0.180D3 * t30 * t200
      t206 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t183 + t193 * t194)
     # + t202) * t46 * t132 / 0.720D3
      t210 = t135 * t194 * t46 * t137 / 0.8D1
      t211 = t141 * t13
      t215 = log(0.4D1 * t211 * t99 * t189)
      t224 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t183 + t215 * t194)
     # + t202) * t95 * t132 / 0.720D3
      t225 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, t182, 0.0D0,
     # 0.10D1, x4)
      t227 = t15 * t187
      t231 = log(0.4D1 * t154 * t13 * t227 * t188)
      t233 = t231 ** 2
      t236 = -t225 + t231 * t183 - t233 * t194 / 0.2D1
      t241 = -t183 + t231 * t194
      t245 = t42 * t200
      t249 = t206 - t210 + t224 + (0.90D2 * t6 * 0.3141592653589793D1 * 
     #t236 - 0.180D3 * t30 * t31 * t241 - t245) * t132 / 0.720D3
      t250 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t179, t181, 0.0D0, t249)
      t252 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t174)
      t254 = -0.1D1 + x3
      t255 = t2 * t254
      t256 = t2 * x3
      t257 = -t254
      t258 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t257, x4)
      t260 = t99 * t254
      t263 = log(-0.4D1 * x3 * t13 * t260)
      t264 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t257, x4)
      t266 = t263 ** 2
      t267 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t257, x4)
      t279 = t31 * t267
      t284 = x3 * t119
      t288 = log(-0.4D1 * t284 * t13 * t260)
      t295 = 0.180D3 * t30 * t279
      t304 = t82 * t13
      t307 = log(-0.4D1 * t304 * t260)
      t317 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t258 - t263 * t264 
     #+ t266 * t267 / 0.2D1) + 0.180D3 * t30 * t31 * (t264 - t263 * t267
     #) - t42 * t279) * t46 / 0.1440D4 + (0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t264 + t288 * t267) + t295) * t46 * t132 / 0.720D3 - t135
     # * t267 * t46 * t137 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1
     # * (-t264 + t307 * t267) + t295) * t46 * t95 / 0.1440D4
      t318 = FJET(XB1, XB2, s, 0.0D0, -t255, 0.0D0, t256, 0.0D0, t317)
      t320 = -0.1D1 + x2
      t322 = t320 * t1 * s
      t324 = x2 * t1 * s
      t325 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t330 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t331 = t320 ** 2
      t332 = t99 * t331
      t335 = log(0.4D1 * t211 * t332)
      t341 = t31 * t325
      t343 = 0.180D3 * t30 * t341
      t350 = log(0.4D1 * t304 * t332)
      t360 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t363 = log(0.4D1 * t98 * t332)
      t365 = t363 ** 2
      t381 = -t135 * t325 * t46 * t137 / 0.8D1 + (0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t330 + t335 * t325) + t343) * t95 * t132 / 0.720D
     #3 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t330 + t350 * t325) + 
     #t343) * t46 * t95 / 0.1440D4 - (0.90D2 * t6 * 0.3141592653589793D1
     # * (t360 - t363 * t330 + t365 * t325 / 0.2D1) - 0.180D3 * t30 * t3
     #1 * (t330 - t363 * t325) + t42 * t341) * t95 / 0.1440D4
      t382 = FJET(XB1, XB2, s, 0.0D0, -t322, 0.0D0, t324, 0.0D0, t381)
      t384 = cos(t11)
      t386 = Sqrt(-t82 * t254)
      t387 = t384 * t386
      t388 = 0.2D1 * t387
      t391 = t82 - 0.1D1
      t392 = 0.1D1 / t391
      t394 = t2 * t320 * (-t82 - 0.1D1 + x3 + t388) * t392
      t395 = 0.3D1 * t82
      t396 = x2 ** 2
      t397 = t396 * x3
      t399 = 0.2D1 * t387 * x2
      t402 = t2 * (-x2 - x3 + t395 - t397 - t388 + t399) * t392
      t403 = x2 * z
      t404 = 0.1D1 - x2 + t403
      t405 = 0.3141592653589793D1 * t404
      t407 = t254 * t392
      t408 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #t407, x4)
      t409 = t397 * z
      t410 = t82 * z
      t413 = 0.2D1 * t82
      t415 = 0.1D1 / (-t409 + t397 - t403 + t410 + 0.2D1 * t387 * t403 +
     # x2 - t413 - t399 - 0.1D1 + t388)
      t416 = t408 * t415
      t418 = t46 * t95 * t132
      t422 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #t407, x4)
      t426 = t391 ** 2
      t432 = log(-0.4D1 * t82 * t50 * t15 * t331 * t254 / t426)
      t448 = -t6 * t405 * t416 * t418 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t404 * t422 - t432 * t404 * t408) * t415 + 0.180D3
     # * t30 * t5 * t405 * t416) * t46 * t95 / 0.1440D4
      t449 = FJET(XB1, XB2, s, 0.0D0, -t394, 0.0D0, t402, 0.0D0, t448)
      t451 = FJET(XB1, XB2, s, t256, 0.0D0, -t255, 0.0D0, 0.0D0, t317)
      t454 = t320 * t177 * t2
      t457 = t2 * t177 * x2 * t187
      t462 = s * t14 * x2 * x1 * t177 * t187
      t463 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t182, x2, 0.
     #10D1, x4)
      t468 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t182, x2, 0.
     #10D1, x4)
      t474 = log(0.4D1 * t141 * t50 * t227 * t188 * t331)
      t487 = t135 * t463 * t46 * t137 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (t468 - t474 * t463) - 0.180D3 * t30 * t31 * t463) *
     # t95 * t132 / 0.720D3
      t488 = FJET(XB1, XB2, s, t181, t454, 0.0D0, -t457, -t462, t487)
      t501 = t206 - t210 + t224 + (0.90D2 * t6 * 0.3141592653589793D1 * 
     #t236 - 0.180D3 * t30 * t31 * t241 - t245) * t132 / 0.720D3
      t502 = FJET(XB1, XB2, s, t181, -t179, 0.0D0, 0.0D0, 0.0D0, t501)
      t504 = FJET(XB1, XB2, s, t324, 0.0D0, -t322, 0.0D0, 0.0D0, t381)
      t506 = FJET(XB1, XB2, s, t402, 0.0D0, -t394, 0.0D0, 0.0D0, t448)
      t508 = t254 * x1
      t510 = t508 * t2 * t392
      t511 = x3 * x1
      t512 = t511 * z
      t516 = Sqrt(-x3 * t186 * x2 * t254)
      t517 = t384 * t516
      t518 = 0.2D1 * t517
      t523 = t179 * t320 * (-t82 - 0.1D1 + x3 + x1 - t511 - t185 + t512 
     #+ t518) * t187 * t392
      t526 = t181 * x3 * t320 * t392
      t527 = t82 * t185
      t529 = t397 * t185
      t530 = t82 * x1
      t532 = t397 * x1
      t534 = 0.2D1 * t517 * x2
      t535 = -x2 - x3 + 0.2D1 * t527 - t529 + t395 - t512 - 0.2D1 * t530
     # + t532 - t518 + t534 + t511 - t397
      t538 = t179 * t535 * t187 * t392
      t539 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t182, x2, t4
     #07, x4)
      t543 = x2 * x1
      t544 = t543 * z
      t545 = x1 - t185 - t543 + t544 - t403 - 0.1D1 + x2
      t558 = 0.1D1 - t410 + t409 - t397 - t529 + 0.4D1 * t527 - 0.2D1 * 
     #t284 * t403 - 0.2D1 * t119 * z + t119 * t8 - t141 - x3 * t8 * t543
     # + t284 * t8 * x2 + 0.2D1 * t543 + t534 + t532 - 0.3D1 * t530
      t577 = -0.2D1 * t517 * t185 - 0.2D1 * t517 * t543 - 0.2D1 * t517 *
     # t403 + t543 * t8 + 0.2D1 * t141 * z - t141 * t8 + 0.2D1 * t517 * 
     #x1 + t119 + t413 + 0.2D1 * t517 * t544 + 0.2D1 * t185 - x2 - 0.2D1
     # * x1 + t403 + t284 * x2 - t518 - 0.3D1 * t544
      t579 = 0.1D1 / (t558 + t577)
      t583 = t6 * 0.3141592653589793D1 * t539 * t186 * t545 * t579 * t41
     #8 / 0.8D1
      t584 = FJET(XB1, XB2, s, t510, t523, t526, -t538, -t462, t583)
      t586 = t31 * t539
      t590 = t186 * t545 * t579 * t418
      t594 = t2 * t177 * x3
      t595 = t2 * t511
      t597 = t254 * t177 * t2
      t598 = t508 * t2
      t599 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t182, 0.0D0,
     # t257, x4)
      t605 = log(-0.4D1 * t284 * t50 * t227 * t188 * t254)
      t606 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t182, 0.0D0,
     # t257, x4)
      t623 = (0.90D2 * t6 * 0.3141592653589793D1 * (t599 - t605 * t606) 
     #- 0.180D3 * t30 * t31 * t606) * t46 * t132 / 0.720D3 + t135 * t606
     # * t46 * t137 / 0.8D1
      t624 = FJET(XB1, XB2, s, -t594, t595, t597, -t598, 0.0D0, t623)
      t626 = FJET(XB1, XB2, s, -t598, t597, t595, -t594, 0.0D0, t623)
      t628 = FJET(XB1, XB2, s, -t457, 0.0D0, t454, t181, -t462, t487)
      t630 = FJET(XB1, XB2, s, -t538, t526, t523, t510, -t462, t583)
      rrqqbar2qqbarht4s2e0 = t175 * t174 + t250 * t249 + t252 * t174 + t
     #318 * t317 + t382 * t381 + t449 * t448 + t451 * t317 + t488 * t487
     # + t502 * t501 + t504 * t381 + t506 * t448 + t584 * t4 * t586 * t5
     #90 / 0.8D1 + t624 * t623 + t626 * t623 + t628 * t487 + t630 * t4 *
     # t586 * t590 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht4s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0,
     # 0.10D1, x4)
      t8 = z ** 2
      t9 = 0.1D1 / t8
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * x3 * t9 * t16)
      t20 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t26 = lh * t4
      t27 = t5 * 0.3141592653589793D1
      t28 = t27 * t20
      t30 = 0.180D3 * t26 * t28
      t32 = 0.1D1 / x3
      t35 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t43 = log(0.4D1 * t9 * t13 * t15)
      t52 = t43 ** 2
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t62 = t6 * 0.3141592653589793D1
      t63 = t20 * t32
      t64 = 0.1D1 / x2
      t68 = x2 * t13
      t69 = t9 * t15
      t72 = log(0.4D1 * t68 * t69)
      t82 = 0.1D1 / x1
      t86 = x1 ** 2
      t87 = t9 * t86
      t90 = log(0.4D1 * t87 * t16)
      t102 = (0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t19 * t20) - t3
     #0) * t32 / 0.1440D4 + t6 * 0.3141592653589793D1 * t35 / 0.16D2 + (
     #-0.180D3 * lh - 0.90D2 * t43) * t4 * t27 * t7 / 0.1440D4 + (0.180D
     #3 * t43 * lh + 0.45D2 * t52 + 0.180D3 * t54 - 0.30D2 * t56) * t4 *
     # t28 / 0.1440D4 + t62 * t63 * t64 / 0.16D2 - (0.90D2 * t6 * 0.3141
     #592653589793D1 * (-t7 + t72 * t20) + t30) * t64 / 0.1440D4 + t62 *
     # t20 * t64 * t82 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (
     #t7 - t90 * t20) - t30) * t82 / 0.720D3 + t62 * t63 * t82 / 0.8D1
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t102)
      t105 = -0.1D1 + x1
      t107 = t105 * t1 * s
      t109 = x1 * t1 * s
      t110 = -t105
      t111 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t110, 0.0D0,
     # 0.10D1, x4)
      t115 = t62 * t111 * t64 * t82 / 0.8D1
      t116 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, t110, 0.0D0,
     # 0.10D1, x4)
      t120 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t122 = t105 ** 2
      t126 = log(0.4D1 * t87 * t13 * t15 * t120 * t122)
      t128 = -t116 + t126 * t111
      t134 = 0.180D3 * t26 * t27 * t111
      t141 = t62 * t111 * t32 * t82 / 0.8D1
      t142 = -t115 + (0.90D2 * t6 * 0.3141592653589793D1 * t128 + t134) 
     #* t82 / 0.720D3 - t141
      t143 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t107, t109, 0.0D0, t142)
      t145 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t102)
      t147 = -0.1D1 + x3
      t148 = t2 * t147
      t149 = t2 * x3
      t150 = -t147
      t151 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t150, x4)
      t156 = log(-0.4D1 * x3 * t13 * t69 * t147)
      t157 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D
     #0, t150, x4)
      t169 = t157 * t32
      t176 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t151 - t156 * t157)
     # + 0.180D3 * t26 * t27 * t157) * t32 / 0.1440D4 - t62 * t169 * t82
     # / 0.8D1 - t62 * t169 * t64 / 0.16D2
      t177 = FJET(XB1, XB2, s, 0.0D0, -t148, 0.0D0, t149, 0.0D0, t176)
      t179 = -0.1D1 + x2
      t181 = t179 * t1 * s
      t183 = x2 * t1 * s
      t184 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t193 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #0.10D1, x4)
      t194 = t179 ** 2
      t198 = log(0.4D1 * t68 * t69 * t194)
      t210 = -t62 * t184 * t64 * t82 / 0.8D1 - t62 * t184 * t32 * t64 / 
     #0.16D2 - (0.90D2 * t6 * 0.3141592653589793D1 * (t193 - t198 * t184
     #) - 0.180D3 * t26 * t27 * t184) * t64 / 0.1440D4
      t211 = FJET(XB1, XB2, s, 0.0D0, -t181, 0.0D0, t183, 0.0D0, t210)
      t213 = x2 * x3
      t214 = cos(t11)
      t216 = Sqrt(-t213 * t147)
      t217 = t214 * t216
      t218 = 0.2D1 * t217
      t222 = 0.1D1 / (t213 - 0.1D1)
      t224 = t2 * t179 * (-t213 - 0.1D1 + x3 + t218) * t222
      t226 = x2 ** 2
      t227 = t226 * x3
      t229 = 0.2D1 * t217 * x2
      t232 = t2 * (-x2 - x3 + 0.3D1 * t213 - t227 - t218 + t229) * t222
      t233 = x2 * z
      t234 = 0.1D1 - x2 + t233
      t238 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 
     #t147 * t222, x4)
      t245 = 0.1D1 / (-t227 * z + t227 - t233 + t213 * z + 0.2D1 * t217 
     #* t233 + x2 - 0.2D1 * t213 - t229 - 0.1D1 + t218)
      t250 = t6 * 0.3141592653589793D1 * t234 * t238 * t245 * t32 * t64 
     #/ 0.16D2
      t251 = FJET(XB1, XB2, s, 0.0D0, -t224, 0.0D0, t232, 0.0D0, -t250)
      t257 = t234 * t238 * t245 * t32 * t64
      t260 = FJET(XB1, XB2, s, t149, 0.0D0, -t148, 0.0D0, 0.0D0, t176)
      t263 = t179 * t105 * t2
      t266 = t2 * t105 * x2 * t120
      t271 = s * t14 * x2 * x1 * t105 * t120
      t272 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t110, x2, 0.
     #10D1, x4)
      t276 = t62 * t272 * t64 * t82 / 0.8D1
      t277 = FJET(XB1, XB2, s, t109, t263, 0.0D0, -t266, -t271, t276)
      t282 = 0.3141592653589793D1 * t272 * t64 * t82
      t292 = -t115 + (0.90D2 * t6 * 0.3141592653589793D1 * t128 + t134) 
     #* t82 / 0.720D3 - t141
      t293 = FJET(XB1, XB2, s, t109, -t107, 0.0D0, 0.0D0, 0.0D0, t292)
      t295 = FJET(XB1, XB2, s, t183, 0.0D0, -t181, 0.0D0, 0.0D0, t210)
      t297 = FJET(XB1, XB2, s, t232, 0.0D0, -t224, 0.0D0, 0.0D0, -t250)
      t303 = t2 * t105 * x3
      t305 = t2 * x1 * x3
      t307 = t147 * t105 * t2
      t309 = t147 * x1 * t2
      t310 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, t110, 0.0D0,
     # t150, x4)
      t314 = t62 * t310 * t32 * t82 / 0.8D1
      t315 = FJET(XB1, XB2, s, -t303, t305, t307, -t309, 0.0D0, t314)
      t320 = 0.3141592653589793D1 * t310 * t32 * t82
      t323 = FJET(XB1, XB2, s, -t309, t307, t305, -t303, 0.0D0, t314)
      t328 = FJET(XB1, XB2, s, -t266, 0.0D0, t263, t109, -t271, t276)
      rrqqbar2qqbarht4s2em1 = t103 * t102 + t143 * t142 + t145 * t102 + 
     #t177 * t176 + t211 * t210 - t251 * t4 * t27 * t257 / 0.16D2 + t260
     # * t176 + t277 * t4 * t5 * t282 / 0.8D1 + t293 * t292 + t295 * t21
     #0 - t297 * t4 * t27 * t257 / 0.16D2 + t315 * t4 * t5 * t320 / 0.8D
     #1 + t323 * t4 * t5 * t320 / 0.8D1 + t328 * t4 * t5 * t282 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht4s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0,
     # 0.10D1, x4)
      t8 = 0.3141592653589793D1 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t21 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, 0.10D1, x4)
      t26 = z ** 2
      t29 = Sin(x4 * 0.3141592653589793D1)
      t30 = t29 ** 2
      t32 = t1 ** 2
      t33 = t32 ** 2
      t36 = log(0.4D1 / t26 * t30 * t33)
      t44 = t6 * t8 * t9 / 0.16D2 + t6 * t8 * t13 / 0.16D2 + t6 * t8 * t
     #17 / 0.8D1 + t6 * 0.3141592653589793D1 * t21 / 0.16D2 + (-0.180D3 
     #* lh - 0.90D2 * t36) * t4 * t5 * 0.3141592653589793D1 * t7 / 0.144
     #0D4
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t44)
      t47 = -0.1D1 + x1
      t49 = t47 * t1 * s
      t51 = x1 * t1 * s
      t53 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, -t47, 0.0D0, 
     #0.10D1, x4)
      t55 = 0.3141592653589793D1 * t53 * t17
      t57 = t6 * t55 / 0.8D1
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t49, t51, 0.0D0, -t57)
      t63 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t44)
      t65 = -0.1D1 + x3
      t66 = t2 * t65
      t67 = t2 * x3
      t69 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0
     #, -t65, x4)
      t71 = 0.3141592653589793D1 * t69 * t9
      t73 = t6 * t71 / 0.16D2
      t74 = FJET(XB1, XB2, s, 0.0D0, -t66, 0.0D0, t67, 0.0D0, -t73)
      t81 = (-0.1D1 + x2) * t1 * s
      t83 = x2 * t1 * s
      t84 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, 0
     #.10D1, x4)
      t86 = 0.3141592653589793D1 * t84 * t13
      t88 = t6 * t86 / 0.16D2
      t89 = FJET(XB1, XB2, s, 0.0D0, -t81, 0.0D0, t83, 0.0D0, -t88)
      t94 = FJET(XB1, XB2, s, t67, 0.0D0, -t66, 0.0D0, 0.0D0, -t73)
      t99 = FJET(XB1, XB2, s, t83, 0.0D0, -t81, 0.0D0, 0.0D0, -t88)
      t104 = FJET(XB1, XB2, s, t51, -t49, 0.0D0, 0.0D0, 0.0D0, -t57)
      rrqqbar2qqbarht4s2em2 = t45 * t44 - t58 * t4 * t5 * t55 / 0.8D1 + 
     #t63 * t44 - t74 * t4 * t5 * t71 / 0.16D2 - t89 * t4 * t5 * t86 / 0
     #.16D2 - t94 * t4 * t5 * t71 / 0.16D2 - t99 * t4 * t5 * t86 / 0.16D
     #2 - t104 * t4 * t5 * t55 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht4s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t7 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0,
     # 0.10D1, x4)
      t10 = t4 * t5 * 0.3141592653589793D1 * t7 / 0.16D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t10)
      t14 = t5 * 0.3141592653589793D1 * t7
      t16 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2qqbarht4s2em3 = t11 * t4 * t14 / 0.16D2 + t16 * t4 * t14 /
     # 0.16D2

      end function



      doubleprecision function rrqqbar2qqbarht4s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      rrqqbar2qqbarht4s2em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2qqbarht4s3e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1,
     # 0.10D1, x4)
      t11 = lh * t4
      t12 = t5 * 0.3141592653589793D1
      t13 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1
     #, 0.10D1, x4)
      t14 = t12 * t13
      t17 = lh ** 2
      t18 = 0.180D3 * t17
      t19 = 0.3141592653589793D1 ** 2
      t20 = 0.30D2 * t19
      t21 = t18 - t20
      t22 = t21 * t4
      t23 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1
     #, 0.10D1, x4)
      t24 = t12 * t23
      t27 = z ** 2
      t29 = 0.1D1 / t27 / z
      t30 = x3 * t29
      t31 = x4 * 0.3141592653589793D1
      t32 = Sin(t31)
      t33 = t32 ** 2
      t34 = t1 ** 2
      t35 = t34 ** 2
      t36 = t33 * t35
      t37 = -0.1D1 + x3
      t38 = 0.1D1 / t37
      t39 = t36 * t38
      t42 = log(-0.4D1 * t30 * t39)
      t44 = cos(t31)
      t45 = x3 * z
      t47 = Sqrt(-t45 * t37)
      t51 = 0.1D1 / (-z - x3 + 0.2D1 * t44 * t47)
      t55 = log(0.4D1 * t30 * t36)
      t59 = t55 ** 2
      t61 = t42 ** 2
      t70 = t12 * t7
      t75 = 0.60D2 * lh * t19
      t77 = 0.120D3 * t17 * lh
      t78 = t75 - 0.2884936567583026D3 - t77
      t79 = t78 * t4
      t80 = t79 * t24
      t97 = 0.1D1 / x3
      t100 = t29 * t33
      t101 = t100 * t35
      t103 = log(0.4D1 * t101)
      t106 = t103 ** 2
      t114 = t106 * t103
      t127 = t19 ** 2
      t128 = t17 ** 2
      t132 = t106 ** 2
      t138 = x1 ** 2
      t139 = x3 * t138
      t142 = log(0.4D1 * t139 * t101)
      t144 = t142 ** 2
      t147 = z * t7
      t148 = t139 * t33
      t149 = t29 * t35
      t153 = log(-0.4D1 * t148 * t149 * t38)
      t154 = t153 * z
      t156 = t153 ** 2
      t167 = z * t13
      t176 = z * t23 * t51
      t182 = 0.1D1 / x1
      t185 = t138 * t33
      t188 = log(0.4D1 * t185 * t149)
      t194 = t188 ** 2
      t215 = x2 ** 2
      t216 = t35 * t215
      t221 = log(-0.4D1 * t185 * t29 * t216 * x3 * t38)
      t226 = x3 * t215
      t227 = t226 * t138
      t230 = log(0.4D1 * t227 * t101)
      t232 = 0.1D1 - x2
      t233 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t232,
     # 0.10D1, x4)
      t234 = -t232
      t239 = log(-0.4D1 * t227 * t100 * t35 * t234)
      t240 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t232,
     # 0.10D1, x4)
      t247 = t12 * (-t240 + t176 + t23)
      t252 = 0.1D1 / x2
      t253 = t252 * t182
      t256 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t232,
     # 0.10D1, x4)
      t257 = t215 * t138
      t258 = t257 * t33
      t262 = log(-0.4D1 * t258 * t149 * t234)
      t264 = t262 ** 2
      t269 = log(0.4D1 * t257 * t101)
      t271 = t269 ** 2
      t285 = t12 * (t23 - t240)
      t291 = t226 * t29
      t292 = t36 * t234
      t295 = log(-0.4D1 * t291 * t292)
      t297 = t295 ** 2
      t302 = log(-0.4D1 * t291 * t39)
      t303 = t302 * z
      t305 = t302 ** 2
      t313 = log(0.4D1 * t226 * t101)
      t315 = t313 ** 2
      t336 = t29 * t215
      t339 = log(0.4D1 * t336 * t36)
      t343 = log(-0.4D1 * t336 * t292)
      t349 = t343 ** 2
      t356 = t339 ** 2
      t380 = -((0.90D2 * t6 * 0.3141592653589793D1 * t7 - 0.180D3 * t11 
     #* t14 + t22 * t24) * (t42 * z * t51 + t55) + 0.90D2 * t6 * 0.31415
     #92653589793D1 * t23 * (t59 * t55 / 0.6D1 + t61 * t42 * z * t51 / 0
     #.6D1) + (-0.180D3 * t11 * t70 + t22 * t14 + t80) * (-z * t51 - 0.1
     #D1) + (0.90D2 * t6 * 0.3141592653589793D1 * t13 - 0.180D3 * t11 * 
     #t24) * (-t61 * z * t51 / 0.2D1 - t59 / 0.2D1)) * t97 / 0.2880D4 + 
     #(0.180D3 * t103 * lh + 0.45D2 * t106 + t18 - t20) * t4 * t70 / 0.2
     #880D4 + (-0.90D2 * t106 * lh + t75 - 0.2884936567583026D3 - t77 - 
     #0.15D2 * t114 - t103 * t21) * t4 * t14 / 0.2880D4 + (0.30D2 * t114
     # * lh + t106 * t21 / 0.2D1 - t103 * t78 + 0.5769873135166051D3 * l
     #h + t127 + 0.60D2 * t128 - 0.60D2 * t17 * t19 + 0.15D2 / 0.4D1 * t
     #132) * t4 * t24 / 0.2880D4 + (0.90D2 * t6 * 0.3141592653589793D1 *
     # (t7 - t142 * t13 + t144 * t23 / 0.2D1 + (t147 - t154 * t13 + t156
     # * z * t23 / 0.2D1) * t51) - 0.180D3 * t11 * t12 * (t13 - t142 * t
     #23 + (t167 - t154 * t23) * t51) + t22 * t12 * (t23 + t176)) * t97 
     #* t182 / 0.1440D4 + (t22 * t12 * (t13 - t188 * t23) + 0.90D2 * t6 
     #* 0.3141592653589793D1 * (-t188 * t7 + t194 * t13 / 0.2D1 - t194 *
     # t188 * t23 / 0.6D1) + t80 - 0.180D3 * t11 * t12 * (t7 - t188 * t1
     #3 + t194 * t23 / 0.2D1)) * t182 / 0.1440D4 + (0.90D2 * t6 * 0.3141
     #592653589793D1 * ((t167 - t221 * z * t23) * t51 + t13 - t230 * t23
     # - t233 + t239 * t240) - 0.180D3 * t11 * t247) * t97 * t253 / 0.72
     #0D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t256 + t262 * t233 -
     # t264 * t240 / 0.2D1 + t7 - t269 * t13 + t271 * t23 / 0.2D1) - 0.1
     #80D3 * t11 * t12 * (-t233 + t262 * t240 + t13 - t269 * t23) + t22 
     #* t285) * t252 * t182 / 0.720D3 + (0.90D2 * t6 * 0.314159265358979
     #3D1 * (-t256 + t295 * t233 - t297 * t240 / 0.2D1 + (t147 - t303 * 
     #t13 + t305 * z * t23 / 0.2D1) * t51 + t7 - t313 * t13 + t315 * t23
     # / 0.2D1) - 0.180D3 * t11 * t12 * (-t233 + t295 * t240 + (t167 - t
     #303 * t23) * t51 + t13 - t313 * t23) + t22 * t247) * t97 * t252 / 
     #0.1440D4 + (t22 * t12 * (t13 - t339 * t23 - t233 + t343 * t240) + 
     #0.90D2 * t6 * 0.3141592653589793D1 * (t343 * t256 - t349 * t233 / 
     #0.2D1 + t349 * t343 * t240 / 0.6D1 - t339 * t7 + t356 * t13 / 0.2D
     #1 - t356 * t339 * t23 / 0.6D1) + t79 * t285 - 0.180D3 * t11 * t12 
     #* (t7 - t339 * t13 + t356 * t23 / 0.2D1 - t256 + t343 * t233 - t34
     #9 * t240 / 0.2D1)) * t252 / 0.1440D4
      t381 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t380)
      t383 = x2 * x3
      t384 = 0.1D1 - x3 + t383
      t385 = 0.1D1 / t384
      t386 = t383 * t385
      t387 = t2 * t386
      t388 = t37 * t385
      t389 = t2 * t388
      t390 = t234 * t37
      t392 = Sqrt(t45 * t390)
      t396 = 0.1D1 / (-z - x3 + t383 + 0.2D1 * t44 * t392)
      t397 = t396 * z
      t398 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t232,
     # -t388, x4)
      t399 = t397 * t398
      t400 = t384 ** 2
      t401 = 0.1D1 / t400
      t405 = x3 * t37 * t234
      t409 = log(0.4D1 * t401 * t138 * t100 * t216 * t405)
      t411 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t232,
     # -t388, x4)
      t412 = z * t411
      t420 = 0.3141592653589793D1 * t396 * t412
      t427 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t232,
     # -t388, x4)
      t434 = log(0.4D1 * t401 * t33 * t149 * t226 * t390)
      t435 = t434 ** 2
      t439 = t434 * t396
      t457 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t399 + t409 * t396 
     #* t412) + 0.180D3 * t11 * t5 * t420) * t97 * t253 / 0.720D3 + (0.9
     #0D2 * t6 * 0.3141592653589793D1 * (-t397 * t427 - t435 * t396 * t4
     #12 / 0.2D1 + t439 * z * t398) - 0.180D3 * t11 * t12 * (t439 * t412
     # - t399) - t22 * t5 * t420) * t97 * t252 / 0.1440D4
      t458 = FJET(XB1, XB2, s, 0.0D0, t387, 0.0D0, -t389, 0.0D0, t457)
      t460 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t380)
      t463 = x1 * z
      t464 = -z - x1 + t463
      t465 = 0.1D1 / t464
      t467 = t2 * x1 * t234 * t465
      t468 = x2 * x1
      t469 = t468 * t2
      t470 = -0.1D1 + x1
      t472 = t470 * t1 * s
      t473 = s * t34
      t476 = x1 * t470 * t465
      t477 = t473 * t234 * t476
      t478 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, t232, 0.
     #10D1, x4)
      t479 = t226 * t185
      t480 = 0.1D1 / t27
      t481 = t480 * t35
      t482 = t470 ** 2
      t483 = t482 * t465
      t488 = log(0.4D1 * t479 * t481 * t483 * t234)
      t489 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, t232, 0.
     #10D1, x4)
      t495 = t12 * t489
      t501 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, t232, 0.
     #10D1, x4)
      t502 = t33 * t480
      t504 = t35 * t465
      t509 = log(0.4D1 * t257 * t502 * t504 * t482 * t234)
      t511 = t509 ** 2
      t528 = (0.90D2 * t6 * 0.3141592653589793D1 * (t478 - t488 * t489) 
     #- 0.180D3 * t11 * t495) * t97 * t253 / 0.720D3 + (0.90D2 * t6 * 0.
     #3141592653589793D1 * (t501 - t509 * t478 + t511 * t489 / 0.2D1) - 
     #0.180D3 * t11 * t12 * (t478 - t509 * t489) + t22 * t495) * t252 * 
     #t182 / 0.720D3
      t529 = FJET(XB1, XB2, s, t467, 0.0D0, t469, -t472, -t477, t528)
      t533 = t37 * t470 * t2 * t385
      t534 = t2 * x1
      t535 = x3 * x1
      t536 = t535 * z
      t537 = x3 * t464
      t539 = Sqrt(-t537 * t390)
      t540 = t44 * t539
      t546 = t534 * x2 * (-x3 + t383 - z + t45 - x1 + t535 + t463 - t536
     # + 0.2D1 * t540) * t465 * t385
      t547 = t472 * t386
      t550 = t226 * t463
      t551 = t226 * x1
      t556 = t534 * (0.1D1 - x3 + 0.2D1 * t540 * x2 - t550 - x2 + t383 +
     # t551 + t226 * z) * t465 * t385
      t559 = t468 * z
      t562 = x3 * t27
      t563 = t562 * x1
      t565 = 0.2D1 * t139 * z
      t567 = t139 * t27
      t569 = x2 * t138
      t572 = t27 + t536 - t551 - 0.2D1 * t540 * z + t559 - t383 * z + t3
     #83 * x1 - t563 - t565 - t139 * x2 + t567 - t468 * t27 - 0.2D1 * t5
     #69 * z
      t586 = x1 * t27
      t587 = t569 * t27 + 0.2D1 * t540 * t559 + t463 + t550 - 0.2D1 * t3
     #83 * t463 + t562 * t468 + 0.2D1 * t139 * x2 * z - t139 * t27 * x2 
     #- 0.2D1 * t540 * t468 + t45 + t139 - t586 + t569
      t589 = 0.1D1 / (t572 + t587)
      t590 = t589 * t464
      t591 = -z - t468 + t559
      t592 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, t232, -t
     #388, x4)
      t603 = log(-0.4D1 * t401 * t482 * t138 * t465 * t33 * t481 * t215 
     #* t405)
      t606 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, t232, -t
     #388, x4)
      t618 = 0.90D2 * t6 * 0.3141592653589793D1 * (-t590 * t591 * t592 +
     # t603 * t589 * t464 * t591 * t606) + 0.180D3 * t11 * t12 * t590 * 
     #t591 * t606
      t621 = t618 * t97 * t253 / 0.720D3
      t622 = FJET(XB1, XB2, s, t533, t546, -t547, -t556, -t477, t621)
      t625 = t97 * t252 * t182
      t629 = t2 * x1 * t465
      t630 = t473 * t476
      t631 = z * t464
      t632 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t640 = log(0.4D1 * t139 * t502 * t35 * t482 * t465 * t38)
      t641 = t640 * z
      t642 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t645 = t640 ** 2
      t647 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t648 = t464 * t647
      t653 = Sqrt(t537 * t37)
      t658 = 0.1D1 / (-t463 - t536 + t565 + t586 + t563 - t567 - t45 - t
     #139 - t27 + 0.2D1 * t44 * t653 * z)
      t660 = t481 * t483
      t663 = log(-0.4D1 * t148 * t660)
      t665 = t663 ** 2
      t672 = t631 * t642
      t684 = t12 * (-t647 + t631 * t647 * t658)
      t689 = (0.90D2 * t6 * 0.3141592653589793D1 * ((t631 * t632 - t641 
     #* t464 * t642 + t645 * z * t648 / 0.2D1) * t658 - t632 + t663 * t6
     #42 - t665 * t647 / 0.2D1) - 0.180D3 * t11 * t12 * ((t672 - t641 * 
     #t648) * t658 - t642 + t663 * t647) + t22 * t684) * t97 * t182 / 0.
     #1440D4
      t694 = log(-0.4D1 * t185 * t480 * t504 * t482)
      t700 = t694 ** 2
      t710 = t12 * t647
      t721 = (-t22 * t12 * (t642 - t694 * t647) - 0.90D2 * t6 * 0.314159
     #2653589793D1 * (-t694 * t632 + t700 * t642 / 0.2D1 - t700 * t694 *
     # t647 / 0.6D1) - t79 * t710 + 0.180D3 * t11 * t12 * (t632 - t694 *
     # t642 + t700 * t647 / 0.2D1)) * t182 / 0.1440D4
      t724 = log(-0.4D1 * t479 * t660)
      t730 = log(0.4D1 * t479 * t481 * t483 * t38)
      t744 = (0.90D2 * t6 * 0.3141592653589793D1 * (-t642 + t724 * t647 
     #+ (t672 - t730 * z * t648) * t658) - 0.180D3 * t11 * t684) * t97 *
     # t253 / 0.720D3
      t747 = log(-0.4D1 * t258 * t660)
      t749 = t747 ** 2
      t752 = -t632 + t747 * t642 - t749 * t647 / 0.2D1
      t757 = -t642 + t747 * t647
      t761 = t22 * t710
      t766 = t689 + t721 + t744 + (0.90D2 * t6 * 0.3141592653589793D1 * 
     #t752 - 0.180D3 * t11 * t12 * t757 - t761) * t252 * t182 / 0.720D3
      t767 = FJET(XB1, XB2, s, -t472, 0.0D0, 0.0D0, -t629, t630, t766)
      t769 = FJET(XB1, XB2, s, -t472, t469, 0.0D0, t467, -t477, t528)
      t783 = t689 + t721 + t744 + (0.90D2 * t6 * 0.3141592653589793D1 * 
     #t752 - 0.180D3 * t11 * t12 * t757 - t761) * t252 * t182 / 0.720D3
      t784 = FJET(XB1, XB2, s, -t629, 0.0D0, 0.0D0, -t472, t630, t783)
      t786 = FJET(XB1, XB2, s, -t389, 0.0D0, t387, 0.0D0, 0.0D0, t457)
      t788 = FJET(XB1, XB2, s, -t556, -t547, t546, t533, -t477, t621)
      rrqqbar2qqbarht4s3e1 = t381 * t380 + t458 * t457 + t460 * t380 + t
     #529 * t528 + t622 * t618 * t625 / 0.720D3 + t767 * t766 + t769 * t
     #528 + t784 * t783 + t786 * t457 + t788 * t618 * t625 / 0.720D3

      end function



      doubleprecision function rrqqbar2qqbarht4s3e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1,
     # 0.10D1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t20 = 0.1D1 / t19
      t21 = t18 * t20
      t24 = log(-0.4D1 * t12 * t21)
      t25 = t24 ** 2
      t27 = cos(t13)
      t28 = x3 * z
      t30 = Sqrt(-t28 * t19)
      t34 = 0.1D1 / (-z - x3 + 0.2D1 * t27 * t30)
      t38 = log(0.4D1 * t12 * t18)
      t39 = t38 ** 2
      t45 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1
     #, 0.10D1, x4)
      t49 = lh * t4
      t50 = t5 * 0.3141592653589793D1
      t51 = t50 * t7
      t59 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1
     #, 0.10D1, x4)
      t63 = t50 * t45
      t66 = lh ** 2
      t67 = 0.180D3 * t66
      t68 = 0.3141592653589793D1 ** 2
      t69 = 0.30D2 * t68
      t70 = t67 - t69
      t71 = t70 * t4
      t72 = t71 * t51
      t78 = 0.1D1 / x3
      t83 = t11 * t15 * t17
      t85 = log(0.4D1 * t83)
      t94 = t85 ** 2
      t113 = 0.1D1 - x2
      t114 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t113,
     # 0.10D1, x4)
      t115 = x2 ** 2
      t116 = x3 * t115
      t117 = t116 * t11
      t118 = -t113
      t119 = t18 * t118
      t122 = log(-0.4D1 * t117 * t119)
      t123 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t113,
     # 0.10D1, x4)
      t125 = z * t45
      t128 = log(-0.4D1 * t117 * t21)
      t135 = log(0.4D1 * t116 * t83)
      t142 = z * t7 * t34
      t143 = -t123 + t142 + t7
      t149 = 0.1D1 / x2
      t152 = t11 * t115
      t155 = log(0.4D1 * t152 * t18)
      t157 = t155 ** 2
      t160 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t113,
     # 0.10D1, x4)
      t163 = log(-0.4D1 * t152 * t119)
      t165 = t163 ** 2
      t179 = t50 * (t7 - t123)
      t184 = x1 ** 2
      t185 = x3 * t184
      t188 = log(0.4D1 * t185 * t83)
      t190 = t185 * t15
      t191 = t11 * t17
      t195 = log(-0.4D1 * t190 * t191 * t20)
      t210 = 0.1D1 / x1
      t213 = t6 * 0.3141592653589793D1
      t215 = t149 * t210
      t219 = t115 * t184
      t220 = t219 * t15
      t224 = log(-0.4D1 * t220 * t191 * t118)
      t228 = log(0.4D1 * t219 * t83)
      t240 = t184 * t15
      t243 = log(0.4D1 * t240 * t191)
      t245 = t243 ** 2
      t260 = -(0.90D2 * t6 * 0.3141592653589793D1 * t7 * (-t25 * z * t34
     # / 0.2D1 - t39 / 0.2D1) + (0.90D2 * t6 * 0.3141592653589793D1 * t4
     #5 - 0.180D3 * t49 * t51) * (t24 * z * t34 + t38) + (0.90D2 * t6 * 
     #0.3141592653589793D1 * t59 - 0.180D3 * t49 * t63 + t72) * (-z * t3
     #4 - 0.1D1)) * t78 / 0.2880D4 + (-0.180D3 * lh - 0.90D2 * t85) * t4
     # * t50 * t59 / 0.2880D4 + (0.180D3 * t85 * lh + 0.45D2 * t94 + t67
     # - t69) * t4 * t63 / 0.2880D4 + (-0.90D2 * t94 * lh + 0.60D2 * lh 
     #* t68 - 0.2884936567583026D3 - 0.120D3 * t66 * lh - 0.15D2 * t94 *
     # t85 - t85 * t70) * t4 * t51 / 0.2880D4 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (-t114 + t122 * t123 + (t125 - t128 * z * t7) * t34 
     #+ t45 - t135 * t7) - 0.180D3 * t49 * t50 * t143) * t78 * t149 / 0.
     #1440D4 + (0.90D2 * t6 * 0.3141592653589793D1 * (t59 - t155 * t45 +
     # t157 * t7 / 0.2D1 - t160 + t163 * t114 - t165 * t123 / 0.2D1) - 0
     #.180D3 * t49 * t50 * (t45 - t155 * t7 - t114 + t163 * t123) + t71 
     #* t179) * t149 / 0.1440D4 + (0.90D2 * t6 * 0.3141592653589793D1 * 
     #(t45 - t188 * t7 + (t125 - t195 * z * t7) * t34) - 0.180D3 * t49 *
     # t50 * (t7 + t142)) * t78 * t210 / 0.1440D4 + t213 * t143 * t78 * 
     #t215 / 0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (-t114 + t224
     # * t123 + t45 - t228 * t7) - 0.180D3 * t49 * t179) * t149 * t210 /
     # 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t59 - t243 * t45
     # + t245 * t7 / 0.2D1) - 0.180D3 * t49 * t50 * (t45 - t243 * t7) + 
     #t72) * t210 / 0.1440D4
      t261 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t260)
      t263 = x2 * x3
      t264 = 0.1D1 - x3 + t263
      t265 = 0.1D1 / t264
      t266 = t263 * t265
      t267 = t2 * t266
      t268 = t19 * t265
      t269 = t2 * t268
      t270 = t264 ** 2
      t274 = t19 * t118
      t278 = log(0.4D1 / t270 * t15 * t191 * t116 * t274)
      t280 = Sqrt(t28 * t274)
      t284 = 0.1D1 / (-z - x3 + t263 + 0.2D1 * t27 * t280)
      t286 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t113,
     # -t268, x4)
      t287 = z * t286
      t290 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t113,
     # -t268, x4)
      t297 = 0.3141592653589793D1 * t284
      t307 = t78 * t149 * t210
      t311 = (0.90D2 * t6 * 0.3141592653589793D1 * (t278 * t284 * t287 -
     # t284 * z * t290) + 0.180D3 * t49 * t5 * t297 * t287) * t78 * t149
     # / 0.1440D4 - t6 * t297 * t287 * t307 / 0.8D1
      t312 = FJET(XB1, XB2, s, 0.0D0, t267, 0.0D0, -t269, 0.0D0, t311)
      t314 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t260)
      t317 = x1 * z
      t318 = -z - x1 + t317
      t319 = 0.1D1 / t318
      t321 = t2 * x1 * t118 * t319
      t322 = x2 * x1
      t323 = t322 * t2
      t324 = -0.1D1 + x1
      t326 = t324 * t1 * s
      t327 = s * t16
      t330 = x1 * t324 * t319
      t331 = t327 * t118 * t330
      t332 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, t113, 0.
     #10D1, x4)
      t337 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, t113, 0.
     #10D1, x4)
      t338 = 0.1D1 / t9
      t339 = t15 * t338
      t341 = t17 * t319
      t342 = t324 ** 2
      t347 = log(0.4D1 * t219 * t339 * t341 * t342 * t118)
      t360 = t213 * t332 * t78 * t215 / 0.8D1 + (0.90D2 * t6 * 0.3141592
     #653589793D1 * (t337 - t347 * t332) - 0.180D3 * t49 * t50 * t332) *
     # t149 * t210 / 0.720D3
      t361 = FJET(XB1, XB2, s, t321, 0.0D0, t323, -t326, -t331, t360)
      t365 = t19 * t324 * t2 * t265
      t366 = t2 * x1
      t367 = x3 * x1
      t368 = t367 * z
      t369 = x3 * t318
      t371 = Sqrt(-t369 * t274)
      t372 = t27 * t371
      t378 = t366 * x2 * (-x3 + t263 - z + t28 - x1 + t367 + t317 - t368
     # + 0.2D1 * t372) * t319 * t265
      t379 = t326 * t266
      t382 = t116 * t317
      t383 = t116 * x1
      t388 = t366 * (0.1D1 - x3 + 0.2D1 * t372 * x2 - t382 - x2 + t263 +
     # t383 + t116 * z) * t319 * t265
      t389 = t322 * z
      t399 = x3 * t9
      t407 = 0.2D1 * t372 * t389 + t368 - t383 - 0.2D1 * t372 * z - t185
     # * t9 * x2 + 0.2D1 * t185 * x2 * z + t399 * t322 - 0.2D1 * t263 * 
     #t317 + t382 + t389 - 0.2D1 * t372 * t322 + t263 * x1 - t263 * z
      t408 = t399 * x1
      t409 = t185 * t9
      t412 = 0.2D1 * t185 * z
      t414 = x2 * t184
      t418 = x1 * t9
      t419 = -t408 + t409 - t185 * x2 - t412 - t322 * t9 - 0.2D1 * t414 
     #* z + t414 * t9 + t185 - t418 + t414 + t28 + t9 + t317
      t421 = 0.1D1 / (t407 + t419)
      t425 = -z - t322 + t389
      t426 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, t113, -t
     #268, x4)
      t430 = t6 * 0.3141592653589793D1 * t421 * t318 * t425 * t426 * t30
     #7 / 0.8D1
      t431 = FJET(XB1, XB2, s, t365, t378, -t379, -t388, -t331, -t430)
      t433 = t50 * t421
      t437 = t318 * t425 * t426 * t307
      t441 = t2 * x1 * t319
      t442 = t327 * t330
      t443 = z * t318
      t444 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t452 = log(0.4D1 * t185 * t339 * t17 * t342 * t319 * t20)
      t454 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t459 = Sqrt(t369 * t19)
      t464 = 0.1D1 / (-t317 - t368 + t412 + t418 + t408 - t409 - t28 - t
     #185 - t9 + 0.2D1 * t27 * t459 * z)
      t468 = t338 * t17 * t342 * t319
      t471 = log(-0.4D1 * t190 * t468)
      t479 = -t454 + t443 * t454 * t464
      t486 = (0.90D2 * t6 * 0.3141592653589793D1 * ((t443 * t444 - t452 
     #* z * t318 * t454) * t464 - t444 + t471 * t454) - 0.180D3 * t49 * 
     #t50 * t479) * t78 * t210 / 0.1440D4
      t490 = t213 * t479 * t78 * t215 / 0.8D1
      t493 = log(-0.4D1 * t220 * t468)
      t495 = -t444 + t493 * t454
      t499 = t50 * t454
      t501 = 0.180D3 * t49 * t499
      t506 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t511 = log(-0.4D1 * t240 * t338 * t341 * t342)
      t513 = t511 ** 2
      t528 = (-0.90D2 * t6 * 0.3141592653589793D1 * (t506 - t511 * t444 
     #+ t513 * t454 / 0.2D1) + 0.180D3 * t49 * t50 * (t444 - t511 * t454
     #) - t71 * t499) * t210 / 0.1440D4
      t529 = t486 + t490 + (0.90D2 * t6 * 0.3141592653589793D1 * t495 + 
     #t501) * t149 * t210 / 0.720D3 + t528
      t530 = FJET(XB1, XB2, s, -t326, 0.0D0, 0.0D0, -t441, t442, t529)
      t532 = FJET(XB1, XB2, s, -t326, t323, 0.0D0, t321, -t331, t360)
      t542 = t486 + t490 + (0.90D2 * t6 * 0.3141592653589793D1 * t495 + 
     #t501) * t149 * t210 / 0.720D3 + t528
      t543 = FJET(XB1, XB2, s, -t441, 0.0D0, 0.0D0, -t326, t442, t542)
      t545 = FJET(XB1, XB2, s, -t269, 0.0D0, t267, 0.0D0, 0.0D0, t311)
      t547 = FJET(XB1, XB2, s, -t388, -t379, t378, t365, -t331, -t430)
      rrqqbar2qqbarht4s3e0 = t261 * t260 + t312 * t311 + t314 * t260 + t
     #361 * t360 - t431 * t4 * t433 * t437 / 0.8D1 + t530 * t529 + t532 
     #* t360 + t543 * t542 + t545 * t311 - t547 * t4 * t433 * t437 / 0.8
     #D1

      end function



      doubleprecision function rrqqbar2qqbarht4s3em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1,
     # 0.10D1, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = -0.1D1 + x3
      t24 = log(-0.4D1 * t12 * t18 / t19)
      t26 = cos(t13)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t19)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t12 * t18)
      t42 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1
     #, 0.10D1, x4)
      t46 = lh * t4
      t47 = t5 * 0.3141592653589793D1
      t48 = t47 * t7
      t50 = 0.180D3 * t46 * t48
      t56 = 0.1D1 / x3
      t59 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1
     #, 0.10D1, x4)
      t67 = log(0.4D1 * t11 * t15 * t17)
      t76 = t67 ** 2
      t78 = lh ** 2
      t80 = 0.3141592653589793D1 ** 2
      t86 = t6 * 0.3141592653589793D1
      t87 = 0.1D1 - x2
      t88 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t87, 0
     #.10D1, x4)
      t90 = z * t7 * t33
      t93 = 0.1D1 / x2
      t97 = x2 ** 2
      t98 = t11 * t97
      t101 = log(0.4D1 * t98 * t18)
      t103 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t87, 
     #0.10D1, x4)
      t104 = -t87
      t108 = log(-0.4D1 * t98 * t18 * t104)
      t114 = t7 - t88
      t122 = 0.1D1 / x1
      t126 = x1 ** 2
      t127 = t126 * t15
      t131 = log(0.4D1 * t127 * t11 * t17)
      t145 = -(0.90D2 * t6 * 0.3141592653589793D1 * t7 * (t24 * z * t33 
     #+ t37) + (0.90D2 * t6 * 0.3141592653589793D1 * t42 - t50) * (-z * 
     #t33 - 0.1D1)) * t56 / 0.2880D4 + t6 * 0.3141592653589793D1 * t59 /
     # 0.32D2 + (-0.180D3 * lh - 0.90D2 * t67) * t4 * t47 * t42 / 0.2880
     #D4 + (0.180D3 * t67 * lh + 0.45D2 * t76 + 0.180D3 * t78 - 0.30D2 *
     # t80) * t4 * t48 / 0.2880D4 + t86 * (-t88 + t90 + t7) * t56 * t93 
     #/ 0.16D2 + (0.90D2 * t6 * 0.3141592653589793D1 * (t42 - t101 * t7 
     #- t103 + t108 * t88) - 0.180D3 * t46 * t47 * t114) * t93 / 0.1440D
     #4 + t86 * t114 * t93 * t122 / 0.8D1 + (0.90D2 * t6 * 0.31415926535
     #89793D1 * (t42 - t131 * t7) - t50) * t122 / 0.1440D4 + t86 * (t7 +
     # t90) * t56 * t122 / 0.16D2
      t146 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t145)
      t148 = x2 * x3
      t150 = 0.1D1 / (0.1D1 - x3 + t148)
      t152 = t2 * t148 * t150
      t153 = t19 * t150
      t154 = t2 * t153
      t157 = Sqrt(t27 * t104 * t19)
      t161 = 0.1D1 / (-z - x3 + t148 + 0.2D1 * t26 * t157)
      t164 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, t87, 
     #-t153, x4)
      t169 = t6 * 0.3141592653589793D1 * t161 * z * t164 * t56 * t93 / 0
     #.16D2
      t170 = FJET(XB1, XB2, s, 0.0D0, t152, 0.0D0, -t154, 0.0D0, -t169)
      t176 = t161 * z * t164 * t56 * t93
      t179 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t145)
      t182 = x1 * z
      t183 = -z - x1 + t182
      t184 = 0.1D1 / t183
      t186 = t2 * x1 * t104 * t184
      t188 = x2 * x1 * t2
      t189 = -0.1D1 + x1
      t191 = t189 * t1 * s
      t192 = s * t16
      t195 = x1 * t189 * t184
      t196 = t192 * t104 * t195
      t197 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, t87, 0.1
     #0D1, x4)
      t201 = t86 * t197 * t93 * t122 / 0.8D1
      t202 = FJET(XB1, XB2, s, t186, 0.0D0, t188, -t191, -t196, t201)
      t207 = 0.3141592653589793D1 * t197 * t93 * t122
      t211 = t2 * x1 * t184
      t212 = t192 * t195
      t213 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t218 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 
     #0.10D1, x4)
      t222 = t189 ** 2
      t226 = log(-0.4D1 * t127 / t9 * t17 * t184 * t222)
      t241 = x3 * t126
      t250 = Sqrt(x3 * t183 * t19)
      t263 = -t86 * t213 * t93 * t122 / 0.8D1 + (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t218 - t226 * t213) + 0.180D3 * t46 * t47 * t213) 
     #* t122 / 0.1440D4 + t86 * (-t213 + z * t183 * t213 / (-t182 - x3 *
     # x1 * z + 0.2D1 * t241 * z + x1 * t9 + x3 * t9 * x1 - t241 * t9 - 
     #t27 - t241 - t9 + 0.2D1 * t26 * t250 * z)) * t56 * t122 / 0.16D2
      t264 = FJET(XB1, XB2, s, -t191, 0.0D0, 0.0D0, -t211, t212, t263)
      t266 = FJET(XB1, XB2, s, -t191, t188, 0.0D0, t186, -t196, t201)
      t271 = FJET(XB1, XB2, s, -t211, 0.0D0, 0.0D0, -t191, t212, t263)
      t273 = FJET(XB1, XB2, s, -t154, 0.0D0, t152, 0.0D0, 0.0D0, -t169)
      rrqqbar2qqbarht4s3em1 = t146 * t145 - t170 * t4 * t47 * t176 / 0.1
     #6D2 + t179 * t145 + t202 * t4 * t5 * t207 / 0.8D1 + t264 * t263 + 
     #t266 * t4 * t5 * t207 / 0.8D1 + t271 * t263 - t273 * t4 * t47 * t1
     #76 / 0.16D2

      end function



      doubleprecision function rrqqbar2qqbarht4s3em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t8 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1,
     # 0.10D1, x4)
      t9 = x4 * 0.3141592653589793D1
      t10 = cos(t9)
      t14 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t27 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.1D1 
     #- x2, 0.10D1, x4)
      t35 = 0.1D1 / x1
      t39 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1
     #, 0.10D1, x4)
      t44 = z ** 2
      t47 = Sin(t9)
      t48 = t47 ** 2
      t50 = t1 ** 2
      t51 = t50 ** 2
      t54 = log(0.4D1 / t44 / z * t48 * t51)
      t62 = -t6 * 0.3141592653589793D1 * t8 * (-z / (-z - x3 + 0.2D1 * t
     #10 * t14) - 0.1D1) / x3 / 0.32D2 + t6 * 0.3141592653589793D1 * (t8
     # - t27) / x2 / 0.16D2 + t6 * 0.3141592653589793D1 * t8 * t35 / 0.1
     #6D2 + t6 * 0.3141592653589793D1 * t39 / 0.32D2 + (-0.180D3 * lh - 
     #0.90D2 * t54) * t4 * t5 * 0.3141592653589793D1 * t8 / 0.2880D4
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t62)
      t65 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t62)
      t67 = -0.1D1 + x1
      t69 = t67 * t1 * s
      t72 = 0.1D1 / (-z - x1 + x1 * z)
      t74 = t2 * x1 * t72
      t78 = s * t50 * x1 * t67 * t72
      t79 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.10D1, 0
     #.10D1, x4)
      t81 = 0.3141592653589793D1 * t79 * t35
      t83 = t6 * t81 / 0.16D2
      t84 = FJET(XB1, XB2, s, -t69, 0.0D0, 0.0D0, -t74, t78, -t83)
      t89 = FJET(XB1, XB2, s, -t74, 0.0D0, 0.0D0, -t69, t78, -t83)
      rrqqbar2qqbarht4s3em2 = t63 * t62 + t65 * t62 - t84 * t4 * t5 * t8
     #1 / 0.16D2 - t89 * t4 * t5 * t81 / 0.16D2

      end function



      doubleprecision function rrqqbar2qqbarht4s3em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t7 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.10D1,
     # 0.10D1, x4)
      t10 = t4 * t5 * 0.3141592653589793D1 * t7 / 0.32D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = t5 * 0.3141592653589793D1 * t7
      t16 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2qqbarht4s3em3 = t11 * t4 * t14 / 0.32D2 + t16 * t4 * t14 /
     # 0.32D2

      end function



      doubleprecision function rrqqbar2qqbarht4s3em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      rrqqbar2qqbarht4s3em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2qqbarht4s4e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = lh ** 2
      t4 = 0.180D3 * t3
      t5 = 0.3141592653589793D1 ** 2
      t6 = 0.30D2 * t5
      t7 = t4 - t6
      t8 = s ** 2
      t9 = 0.1D1 / t8
      t10 = t7 * t9
      t11 = 0.1D1 / t1
      t12 = t11 * 0.3141592653589793D1
      t13 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = x3 * t16
      t18 = x4 * 0.3141592653589793D1
      t19 = Sin(t18)
      t20 = t19 ** 2
      t21 = t1 ** 2
      t22 = t21 ** 2
      t23 = t20 * t22
      t26 = log(0.4D1 * t17 * t23)
      t27 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t32 = t9 * t11
      t33 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t35 = t26 ** 2
      t46 = 0.60D2 * lh * t5
      t48 = 0.120D3 * t3 * lh
      t49 = t46 - 0.2884936567583026D3 - t48
      t50 = t49 * t9
      t51 = t12 * t27
      t52 = t50 * t51
      t53 = lh * t9
      t62 = 0.1D1 / x3
      t65 = t16 * t20
      t66 = t65 * t22
      t68 = log(0.4D1 * t66)
      t71 = t68 ** 2
      t80 = t71 * t68
      t94 = t5 ** 2
      t95 = t3 ** 2
      t99 = t71 ** 2
      t105 = x1 ** 2
      t106 = x3 * t105
      t109 = log(0.4D1 * t106 * t66)
      t111 = t109 ** 2
      t126 = 0.1D1 / x1
      t129 = t105 * t20
      t130 = t16 * t22
      t133 = log(0.4D1 * t129 * t130)
      t139 = t133 ** 2
      t159 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t160 = t106 * x2
      t161 = -0.1D1 + x2
      t162 = t161 ** 2
      t164 = t65 * t22 * t162
      t167 = log(0.4D1 * t160 * t164)
      t168 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t172 = log(0.4D1 * t160 * t66)
      t178 = t27 - t168
      t179 = t12 * t178
      t184 = 0.1D1 / x2
      t185 = t184 * t126
      t188 = x2 * t105
      t191 = log(0.4D1 * t188 * t66)
      t193 = t191 ** 2
      t196 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t197 = t188 * t20
      t201 = log(0.4D1 * t197 * t130 * t162)
      t203 = t201 ** 2
      t218 = -t10 * t12 * t178
      t223 = x2 * x3
      t224 = t223 * t16
      t225 = t23 * t162
      t228 = log(0.4D1 * t224 * t225)
      t230 = t228 ** 2
      t235 = log(0.4D1 * t223 * t66)
      t237 = t235 ** 2
      t254 = x2 * t16
      t257 = log(0.4D1 * t254 * t23)
      t261 = log(0.4D1 * t254 * t225)
      t267 = t261 ** 2
      t274 = t257 ** 2
      t298 = -(t10 * t12 * (-t13 + t26 * t27) + 0.90D2 * t32 * 0.3141592
     #653589793D1 * (t26 * t33 - t35 * t13 / 0.2D1 + t35 * t26 * t27 / 0
     #.6D1) - t52 - 0.180D3 * t53 * t12 * (-t33 + t26 * t13 - t35 * t27 
     #/ 0.2D1)) * t62 / 0.1440D4 + (0.180D3 * lh * t68 + 0.45D2 * t71 + 
     #t4 - t6) * t9 * t12 * t33 / 0.1440D4 + (-0.90D2 * t71 * lh + t46 -
     # 0.2884936567583026D3 - t48 - 0.15D2 * t80 - t68 * t7) * t9 * t12 
     #* t13 / 0.1440D4 + (0.30D2 * t80 * lh + t71 * t7 / 0.2D1 - t68 * t
     #49 + 0.5769873135166051D3 * lh + t94 + 0.60D2 * t95 - 0.60D2 * t3 
     #* t5 + 0.15D2 / 0.4D1 * t99) * t9 * t51 / 0.1440D4 - (0.90D2 * t32
     # * 0.3141592653589793D1 * (-t33 + t109 * t13 - t111 * t27 / 0.2D1)
     # - 0.180D3 * t53 * t12 * (-t13 + t109 * t27) - t10 * t51) * t62 * 
     #t126 / 0.720D3 + (t10 * t12 * (t13 - t133 * t27) + 0.90D2 * t32 * 
     #0.3141592653589793D1 * (-t133 * t33 + t139 * t13 / 0.2D1 - t139 * 
     #t133 * t27 / 0.6D1) + t52 - 0.180D3 * t53 * t12 * (t33 - t133 * t1
     #3 + t139 * t27 / 0.2D1)) * t126 / 0.720D3 + (0.90D2 * t32 * 0.3141
     #592653589793D1 * (-t159 + t167 * t168 + t13 - t172 * t27) - 0.180D
     #3 * t53 * t179) * t62 * t185 / 0.720D3 - (0.90D2 * t32 * 0.3141592
     #653589793D1 * (-t33 + t191 * t13 - t193 * t27 / 0.2D1 + t196 - t20
     #1 * t159 + t203 * t168 / 0.2D1) - 0.180D3 * t53 * t12 * (-t13 + t1
     #91 * t27 + t159 - t201 * t168) + t218) * t184 * t126 / 0.720D3 - (
     #0.90D2 * t32 * 0.3141592653589793D1 * (t196 - t228 * t159 + t230 *
     # t168 / 0.2D1 - t33 + t235 * t13 - t237 * t27 / 0.2D1) - 0.180D3 *
     # t53 * t12 * (t159 - t228 * t168 - t13 + t235 * t27) + t218) * t62
     # * t184 / 0.1440D4 + (t10 * t12 * (t13 - t257 * t27 - t159 + t261 
     #* t168) + 0.90D2 * t32 * 0.3141592653589793D1 * (t261 * t196 - t26
     #7 * t159 / 0.2D1 + t267 * t261 * t168 / 0.6D1 - t257 * t33 + t274 
     #* t13 / 0.2D1 - t274 * t257 * t27 / 0.6D1) + t50 * t179 - 0.180D3 
     #* t53 * t12 * (t33 - t257 * t13 + t274 * t27 / 0.2D1 - t196 + t261
     # * t159 - t267 * t168 / 0.2D1)) * t184 / 0.1440D4
      t299 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t298)
      t302 = x1 * t1 * s
      t303 = -0.1D1 + x1
      t305 = t303 * t1 * s
      t306 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t307 = t106 * t20
      t308 = 0.1D1 / t14
      t309 = t308 * t22
      t310 = t303 ** 2
      t311 = x1 * z
      t312 = -z - x1 + t311
      t313 = 0.1D1 / t312
      t314 = t310 * t313
      t315 = t309 * t314
      t318 = log(-0.4D1 * t307 * t315)
      t319 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t321 = t318 ** 2
      t322 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t334 = t12 * t322
      t335 = t10 * t334
      t338 = (0.90D2 * t32 * 0.3141592653589793D1 * (t306 - t318 * t319 
     #+ t321 * t322 / 0.2D1) - 0.180D3 * t53 * t12 * (t319 - t318 * t322
     #) + t335) * t62 * t126
      t339 = t129 * t308
      t340 = t22 * t313
      t341 = t340 * t310
      t344 = log(-0.4D1 * t339 * t341)
      t346 = -t319 + t344 * t322
      t350 = t344 ** 2
      t356 = t344 * t306 - t350 * t319 / 0.2D1 + t350 * t344 * t322 / 0.
     #6D1
      t360 = t50 * t334
      t364 = -t306 + t344 * t319 - t350 * t322 / 0.2D1
      t370 = t223 * t129
      t373 = log(-0.4D1 * t370 * t315)
      t383 = (0.90D2 * t32 * 0.3141592653589793D1 * (-t319 + t373 * t322
     #) + 0.180D3 * t53 * t334) * t62 * t185
      t386 = log(-0.4D1 * t197 * t315)
      t388 = t386 ** 2
      t402 = (0.90D2 * t32 * 0.3141592653589793D1 * (t306 - t386 * t319 
     #+ t388 * t322 / 0.2D1) - 0.180D3 * t53 * t12 * (t319 - t386 * t322
     #) + t335) * t184 * t126
      t404 = -t338 / 0.720D3 + (t10 * t12 * t346 + 0.90D2 * t32 * 0.3141
     #592653589793D1 * t356 - t360 - 0.180D3 * t53 * t12 * t364) * t126 
     #/ 0.720D3 + t383 / 0.720D3 - t402 / 0.720D3
      t405 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t302, -t305, 0.0D0, t404)
      t407 = t2 * x3
      t408 = -0.1D1 + x3
      t409 = t2 * t408
      t410 = -t408
      t411 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t410, x4)
      t415 = log(-0.4D1 * t307 * t130 * t408)
      t416 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t410, x4)
      t418 = t415 ** 2
      t419 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t410, x4)
      t431 = t12 * t419
      t432 = t10 * t431
      t441 = log(-0.4D1 * t160 * t65 * t22 * t408)
      t453 = t23 * t408
      t456 = log(-0.4D1 * t17 * t453)
      t462 = t456 ** 2
      t485 = log(-0.4D1 * t224 * t453)
      t487 = t485 ** 2
      t503 = -(0.90D2 * t32 * 0.3141592653589793D1 * (t411 - t415 * t416
     # + t418 * t419 / 0.2D1) - 0.180D3 * t53 * t12 * (t416 - t415 * t41
     #9) + t432) * t62 * t126 / 0.720D3 + (0.90D2 * t32 * 0.314159265358
     #9793D1 * (-t416 + t441 * t419) + 0.180D3 * t53 * t431) * t62 * t18
     #5 / 0.720D3 - (t10 * t12 * (t416 - t456 * t419) + 0.90D2 * t32 * 0
     #.3141592653589793D1 * (-t456 * t411 + t462 * t416 / 0.2D1 - t462 *
     # t456 * t419 / 0.6D1) + t50 * t431 - 0.180D3 * t53 * t12 * (t411 -
     # t456 * t416 + t462 * t419 / 0.2D1)) * t62 / 0.1440D4 - (0.90D2 * 
     #t32 * 0.3141592653589793D1 * (t411 - t485 * t416 + t487 * t419 / 0
     #.2D1) - 0.180D3 * t53 * t12 * (t416 - t485 * t419) + t432) * t62 *
     # t184 / 0.1440D4
      t504 = FJET(XB1, XB2, s, 0.0D0, t407, 0.0D0, -t409, 0.0D0, t503)
      t507 = t223 - 0.1D1
      t508 = 0.1D1 / t507
      t509 = x3 * t161 * t508
      t510 = t2 * t509
      t511 = t408 * t508
      t512 = t2 * t511
      t513 = t32 * 0.3141592653589793D1
      t514 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #511, x4)
      t515 = t507 ** 2
      t516 = 0.1D1 / t515
      t521 = log(-0.4D1 * t164 * t223 * t408 * t516)
      t522 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #511, x4)
      t524 = t521 ** 2
      t525 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #511, x4)
      t529 = cos(t18)
      t530 = x3 * z
      t531 = x2 * t408
      t533 = Sqrt(-t530 * t531)
      t537 = 0.1D1 / (-z - t223 + 0.2D1 * t529 * t533)
      t542 = t53 * t11
      t546 = t537 * z
      t552 = 0.3141592653589793D1 * t525 * t546
      t561 = x3 * t408
      t566 = log(-0.4D1 * t105 * t162 * t65 * t22 * x2 * t561 * t516)
      t579 = -(0.90D2 * t513 * (t514 - t521 * t522 + t524 * t525 / 0.2D1
     #) * t537 * z - 0.180D3 * t542 * 0.3141592653589793D1 * (t522 - t52
     #1 * t525) * t546 + t10 * t11 * t552) * t62 * t184 / 0.1440D4 + (-0
     #.90D2 * t513 * (t522 - t566 * t525) * t537 * z + 0.180D3 * t542 * 
     #t552) * t62 * t185 / 0.720D3
      t580 = FJET(XB1, XB2, s, 0.0D0, t510, 0.0D0, t512, 0.0D0, t579)
      t582 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t298)
      t584 = x3 * x1
      t585 = t2 * t584
      t587 = t2 * t303 * x3
      t589 = t408 * x1 * t2
      t590 = t408 * t303
      t591 = t590 * t2
      t592 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t
     #410, x4)
      t599 = log(0.4D1 * t313 * t20 * t309 * t105 * t310 * t561)
      t600 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t
     #410, x4)
      t602 = t599 ** 2
      t603 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t
     #410, x4)
      t615 = t12 * t603
      t621 = t20 * t308
      t629 = log(0.4D1 * x2 * t313 * t621 * t22 * t105 * t310 * x3 * t40
     #8)
      t641 = -(0.90D2 * t32 * 0.3141592653589793D1 * (-t592 + t599 * t60
     #0 - t602 * t603 / 0.2D1) - 0.180D3 * t53 * t12 * (-t600 + t599 * t
     #603) - t10 * t615) * t62 * t126 / 0.720D3 + (0.90D2 * t32 * 0.3141
     #592653589793D1 * (t600 - t629 * t603) - 0.180D3 * t53 * t615) * t6
     #2 * t185 / 0.720D3
      t642 = FJET(XB1, XB2, s, t585, -t587, -t589, t591, 0.0D0, t641)
      t644 = FJET(XB1, XB2, s, t512, 0.0D0, t510, 0.0D0, 0.0D0, t579)
      t646 = FJET(XB1, XB2, s, t591, -t589, -t587, t585, 0.0D0, t641)
      t648 = FJET(XB1, XB2, s, -t409, 0.0D0, t407, 0.0D0, 0.0D0, t503)
      t664 = -t338 / 0.720D3 + (t10 * t12 * t346 + 0.90D2 * t32 * 0.3141
     #592653589793D1 * t356 - t360 - 0.180D3 * t53 * t12 * t364) * t126 
     #/ 0.720D3 + t383 / 0.720D3 - t402 / 0.720D3
      t665 = FJET(XB1, XB2, s, -t305, t302, 0.0D0, 0.0D0, 0.0D0, t664)
      t668 = t2 * x1 * t161
      t669 = x2 * x1
      t671 = t2 * t669 * t313
      t676 = s * t21 * x2 * x1 * t303 * t313
      t677 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t682 = log(-0.4D1 * t370 * t309 * t314 * t162)
      t683 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t689 = t12 * t683
      t695 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t701 = log(-0.4D1 * t188 * t621 * t340 * t310 * t162)
      t703 = t701 ** 2
      t720 = (0.90D2 * t32 * 0.3141592653589793D1 * (t677 - t682 * t683)
     # - 0.180D3 * t53 * t689) * t62 * t185 / 0.720D3 - (-0.90D2 * t32 *
     # 0.3141592653589793D1 * (t695 - t701 * t677 + t703 * t683 / 0.2D1)
     # + 0.180D3 * t53 * t12 * (t677 - t701 * t683) - t10 * t689) * t184
     # * t126 / 0.720D3
      t721 = FJET(XB1, XB2, s, -t305, -t668, 0.0D0, -t671, t676, t720)
      t723 = FJET(XB1, XB2, s, -t671, 0.0D0, -t668, -t305, t676, t720)
      t726 = t590 * t2 * t508
      t727 = t584 * z
      t730 = Sqrt(x3 * t312 * t531)
      t731 = t529 * t730
      t732 = 0.2D1 * t731
      t737 = t302 * t161 * (-t223 - z + t530 - x1 + t584 + t311 - t727 +
     # t732) * t313 * t508
      t738 = t305 * t509
      t741 = x2 ** 2
      t742 = x3 * t741
      t743 = t742 * t311
      t746 = t223 * z
      t748 = t223 * x1
      t750 = t742 * x1
      t752 = -x2 - t732 - 0.2D1 * t223 * t311 + t743 + t223 + 0.2D1 * t7
     #31 * x2 + t727 + 0.2D1 * t746 + 0.2D1 * t748 - t750 - t742 * z - t
     #530 - t584
      t755 = t302 * t752 * t313 * t508
      t756 = t669 * z
      t757 = z + x1 - t311 - t669 + t756
      t758 = t312 * t757
      t759 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t511
     #, x4)
      t767 = log(0.4D1 * t223 * t339 * t341 * t162 * t408 * t516)
      t769 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t511
     #, x4)
      t784 = -t14 - 0.2D1 * t311 - t105 + 0.2D1 * t731 * t756 + x3 * t14
     # * t669 + 0.2D1 * t106 * x2 * z - t106 * t14 * x2 - t743 + t756 - 
     #t746 - t748 - t160
      t802 = t750 - t669 * t14 - 0.2D1 * t188 * z + t188 * t14 - 0.2D1 *
     # t731 * t669 - 0.2D1 * t731 * t311 + 0.2D1 * x1 * t14 + 0.2D1 * t1
     #05 * z - t105 * t14 + t188 + 0.2D1 * t731 * z + 0.2D1 * t731 * x1
      t804 = 0.1D1 / (t784 + t802)
      t813 = -0.90D2 * t32 * 0.3141592653589793D1 * (t758 * t759 - t767 
     #* t312 * t757 * t769) * t804 + 0.180D3 * t53 * t12 * t758 * t769 *
     # t804
      t816 = t813 * t62 * t185 / 0.720D3
      t817 = FJET(XB1, XB2, s, -t726, t737, -t738, -t755, t676, t816)
      t820 = t62 * t184 * t126
      t823 = FJET(XB1, XB2, s, -t755, -t738, t737, -t726, t676, t816)
      rrqqbar2qqbarht4s4e1 = t299 * t298 + t405 * t404 + t504 * t503 + t
     #580 * t579 + t582 * t298 + t642 * t641 + t644 * t579 + t646 * t641
     # + t648 * t503 + t665 * t664 + t721 * t720 + t723 * t720 + t817 * 
     #t813 * t820 / 0.720D3 + t823 * t813 * t820 / 0.720D3

      end function



      doubleprecision function rrqqbar2qqbarht4s4e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 
     #0.10D1, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t23 = t20 ** 2
      t24 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t31 = lh * t4
      t32 = t5 * 0.3141592653589793D1
      t38 = lh ** 2
      t39 = 0.180D3 * t38
      t40 = 0.3141592653589793D1 ** 2
      t41 = 0.30D2 * t40
      t42 = t39 - t41
      t43 = t42 * t4
      t44 = t32 * t24
      t45 = t43 * t44
      t47 = 0.1D1 / x3
      t52 = t10 * t14 * t16
      t54 = log(0.4D1 * t52)
      t63 = t54 ** 2
      t83 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t84 = x2 * x3
      t85 = t84 * t10
      t86 = -0.1D1 + x2
      t87 = t86 ** 2
      t88 = t17 * t87
      t91 = log(0.4D1 * t85 * t88)
      t92 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t96 = log(0.4D1 * t84 * t52)
      t102 = t92 - t24
      t105 = 0.180D3 * t31 * t32 * t102
      t108 = 0.1D1 / x2
      t111 = x2 * t10
      t114 = log(0.4D1 * t111 * t17)
      t116 = t114 ** 2
      t119 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0
     #.10D1, x4)
      t122 = log(0.4D1 * t111 * t88)
      t124 = t122 ** 2
      t137 = -t102
      t143 = x1 ** 2
      t144 = x3 * t143
      t147 = log(0.4D1 * t144 * t52)
      t157 = 0.1D1 / x1
      t160 = t6 * 0.3141592653589793D1
      t162 = t108 * t157
      t166 = x2 * t143
      t169 = log(0.4D1 * t166 * t52)
      t171 = t166 * t14
      t172 = t10 * t16
      t176 = log(0.4D1 * t171 * t172 * t87)
      t186 = t143 * t14
      t189 = log(0.4D1 * t186 * t172)
      t191 = t189 ** 2
      t206 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t7 + t20 * t21 - t
     #23 * t24 / 0.2D1) - 0.180D3 * t31 * t32 * (-t21 + t20 * t24) - t45
     #) * t47 / 0.1440D4 + (-0.180D3 * lh - 0.90D2 * t54) * t4 * t32 * t
     #7 / 0.1440D4 + (0.180D3 * t54 * lh + 0.45D2 * t63 + t39 - t41) * t
     #4 * t32 * t21 / 0.1440D4 + (-0.90D2 * t63 * lh + 0.60D2 * lh * t40
     # - 0.2884936567583026D3 - 0.120D3 * t38 * lh - 0.15D2 * t63 * t54 
     #- t54 * t42) * t4 * t44 / 0.1440D4 - (0.90D2 * t6 * 0.314159265358
     #9793D1 * (t83 - t91 * t92 - t21 + t96 * t24) - t105) * t47 * t108 
     #/ 0.1440D4 + (0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t114 * t2
     #1 + t116 * t24 / 0.2D1 - t119 + t122 * t83 - t124 * t92 / 0.2D1) -
     # 0.180D3 * t31 * t32 * (t21 - t114 * t24 - t83 + t122 * t92) + t43
     # * t32 * t137) * t108 / 0.1440D4 - (0.90D2 * t6 * 0.31415926535897
     #93D1 * (-t21 + t147 * t24) + 0.180D3 * t31 * t44) * t47 * t157 / 0
     #.720D3 + t160 * t137 * t47 * t162 / 0.8D1 - (0.90D2 * t6 * 0.31415
     #92653589793D1 * (-t21 + t169 * t24 + t83 - t176 * t92) - t105) * t
     #108 * t157 / 0.720D3 + (0.90D2 * t6 * 0.3141592653589793D1 * (t7 -
     # t189 * t21 + t191 * t24 / 0.2D1) - 0.180D3 * t31 * t32 * (t21 - t
     #189 * t24) + t45) * t157 / 0.720D3
      t207 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t206)
      t210 = x1 * t1 * s
      t211 = -0.1D1 + x1
      t213 = t211 * t1 * s
      t214 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t215 = t144 * t14
      t216 = 0.1D1 / t8
      t217 = t216 * t16
      t218 = t211 ** 2
      t219 = x1 * z
      t220 = -z - x1 + t219
      t221 = 0.1D1 / t220
      t223 = t217 * t218 * t221
      t226 = log(-0.4D1 * t215 * t223)
      t227 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t233 = t32 * t227
      t235 = 0.180D3 * t31 * t233
      t239 = (0.90D2 * t6 * 0.3141592653589793D1 * (t214 - t226 * t227) 
     #- t235) * t47 * t157 / 0.720D3
      t243 = t160 * t227 * t47 * t162 / 0.8D1
      t246 = log(-0.4D1 * t171 * t223)
      t255 = (0.90D2 * t6 * 0.3141592653589793D1 * (t214 - t246 * t227) 
     #- t235) * t108 * t157 / 0.720D3
      t256 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t258 = t16 * t221
      t262 = log(-0.4D1 * t186 * t216 * t258 * t218)
      t264 = t262 ** 2
      t267 = -t256 + t262 * t214 - t264 * t227 / 0.2D1
      t272 = -t214 + t262 * t227
      t276 = t43 * t233
      t280 = -t239 - t243 - t255 + (0.90D2 * t6 * 0.3141592653589793D1 *
     # t267 - 0.180D3 * t31 * t32 * t272 - t276) * t157 / 0.720D3
      t281 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t210, -t213, 0.0D0, t280)
      t283 = t2 * x3
      t284 = -0.1D1 + x3
      t285 = t2 * t284
      t286 = -t284
      t287 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t286, x4)
      t288 = t17 * t284
      t291 = log(-0.4D1 * t85 * t288)
      t292 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t286, x4)
      t298 = t32 * t292
      t300 = 0.180D3 * t31 * t298
      t305 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t286, x4)
      t308 = log(-0.4D1 * t11 * t288)
      t310 = t308 ** 2
      t329 = log(-0.4D1 * t215 * t172 * t284)
      t343 = -(0.90D2 * t6 * 0.3141592653589793D1 * (t287 - t291 * t292)
     # - t300) * t47 * t108 / 0.1440D4 - (0.90D2 * t6 * 0.31415926535897
     #93D1 * (t305 - t308 * t287 + t310 * t292 / 0.2D1) - 0.180D3 * t31 
     #* t32 * (t287 - t308 * t292) + t43 * t298) * t47 / 0.1440D4 - (0.9
     #0D2 * t6 * 0.3141592653589793D1 * (t287 - t329 * t292) - t300) * t
     #47 * t157 / 0.720D3 - t160 * t292 * t47 * t162 / 0.8D1
      t344 = FJET(XB1, XB2, s, 0.0D0, t283, 0.0D0, -t285, 0.0D0, t343)
      t347 = t84 - 0.1D1
      t348 = 0.1D1 / t347
      t349 = x3 * t86 * t348
      t350 = t2 * t349
      t351 = t284 * t348
      t352 = t2 * t351
      t353 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #351, x4)
      t354 = 0.3141592653589793D1 * t353
      t356 = cos(t12)
      t357 = x3 * z
      t358 = x2 * t284
      t360 = Sqrt(-t357 * t358)
      t364 = 0.1D1 / (-z - t84 + 0.2D1 * t356 * t360)
      t365 = t364 * z
      t367 = t47 * t108 * t157
      t371 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #351, x4)
      t374 = t347 ** 2
      t380 = log(-0.4D1 * t87 * t14 * t172 * t84 * t284 / t374)
      t395 = -t6 * t354 * t365 * t367 / 0.8D1 - (0.90D2 * t160 * (t371 -
     # t380 * t353) * t364 * z - 0.180D3 * t31 * t5 * t354 * t365) * t47
     # * t108 / 0.1440D4
      t396 = FJET(XB1, XB2, s, 0.0D0, t350, 0.0D0, t352, 0.0D0, t395)
      t398 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t206)
      t400 = x3 * x1
      t401 = t2 * t400
      t403 = t2 * t211 * x3
      t405 = t284 * x1 * t2
      t406 = t284 * t211
      t407 = t406 * t2
      t408 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t
     #286, x4)
      t416 = log(0.4D1 * t221 * t14 * t217 * t143 * t218 * x3 * t284)
      t417 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t
     #286, x4)
      t434 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t408 + t416 * t417
     #) + 0.180D3 * t31 * t32 * t417) * t47 * t157 / 0.720D3 + t160 * t4
     #17 * t47 * t162 / 0.8D1
      t435 = FJET(XB1, XB2, s, t401, -t403, -t405, t407, 0.0D0, t434)
      t437 = FJET(XB1, XB2, s, t352, 0.0D0, t350, 0.0D0, 0.0D0, t395)
      t439 = FJET(XB1, XB2, s, t407, -t405, -t403, t401, 0.0D0, t434)
      t441 = FJET(XB1, XB2, s, -t285, 0.0D0, t283, 0.0D0, 0.0D0, t343)
      t454 = -t239 - t243 - t255 + (0.90D2 * t6 * 0.3141592653589793D1 *
     # t267 - 0.180D3 * t31 * t32 * t272 - t276) * t157 / 0.720D3
      t455 = FJET(XB1, XB2, s, -t213, t210, 0.0D0, 0.0D0, 0.0D0, t454)
      t458 = t2 * x1 * t86
      t459 = x2 * x1
      t461 = t2 * t459 * t221
      t466 = s * t15 * x2 * x1 * t211 * t221
      t467 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t472 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t479 = log(-0.4D1 * t166 * t14 * t216 * t258 * t218 * t87)
      t492 = t160 * t467 * t47 * t162 / 0.8D1 - (-0.90D2 * t6 * 0.314159
     #2653589793D1 * (t472 - t479 * t467) + 0.180D3 * t31 * t32 * t467) 
     #* t108 * t157 / 0.720D3
      t493 = FJET(XB1, XB2, s, -t213, -t458, 0.0D0, -t461, t466, t492)
      t495 = FJET(XB1, XB2, s, -t461, 0.0D0, -t458, -t213, t466, t492)
      t498 = t406 * t2 * t348
      t499 = t400 * z
      t502 = Sqrt(x3 * t220 * t358)
      t503 = t356 * t502
      t504 = 0.2D1 * t503
      t509 = t210 * t86 * (-t84 - z + t357 - x1 + t400 + t219 - t499 + t
     #504) * t221 * t348
      t510 = t213 * t349
      t513 = x2 ** 2
      t514 = x3 * t513
      t515 = t514 * t219
      t518 = t84 * z
      t520 = t84 * x1
      t522 = t514 * x1
      t524 = -x2 - t504 - 0.2D1 * t84 * t219 + t515 + t84 + 0.2D1 * t503
     # * x2 + t499 + 0.2D1 * t518 + 0.2D1 * t520 - t522 - t514 * z - t35
     #7 - t400
      t527 = t210 * t524 * t221 * t348
      t529 = t459 * z
      t530 = z + x1 - t219 - t459 + t529
      t533 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t351
     #, x4)
      t545 = -t8 - 0.2D1 * t219 - t143 + 0.2D1 * t503 * t529 + x3 * t8 *
     # t459 + 0.2D1 * t144 * x2 * z - t144 * t8 * x2 - t515 + t529 - t51
     #8 - t520 - t144 * x2
      t563 = t522 - t459 * t8 - 0.2D1 * t166 * z + t166 * t8 - 0.2D1 * t
     #503 * t459 - 0.2D1 * t503 * t219 + 0.2D1 * x1 * t8 + 0.2D1 * t143 
     #* z - t143 * t8 + t166 + 0.2D1 * t503 * z + 0.2D1 * t503 * x1
      t565 = 0.1D1 / (t545 + t563)
      t569 = t6 * 0.3141592653589793D1 * t220 * t530 * t533 * t565 * t36
     #7 / 0.8D1
      t570 = FJET(XB1, XB2, s, -t498, t509, -t510, -t527, t466, -t569)
      t572 = t32 * t220
      t576 = t530 * t533 * t565 * t367
      t579 = FJET(XB1, XB2, s, -t527, -t510, t509, -t498, t466, -t569)
      rrqqbar2qqbarht4s4e0 = t207 * t206 + t281 * t280 + t344 * t343 + t
     #396 * t395 + t398 * t206 + t435 * t434 + t437 * t395 + t439 * t434
     # + t441 * t343 + t455 * t454 + t493 * t492 + t495 * t492 - t570 * 
     #t4 * t572 * t576 / 0.8D1 - t579 * t4 * t572 * t576 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht4s4em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 
     #0.10D1, x4)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t27 = lh * t4
      t28 = t5 * 0.3141592653589793D1
      t29 = t28 * t21
      t31 = 0.180D3 * t27 * t29
      t33 = 0.1D1 / x3
      t36 = rrqqbar2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t44 = log(0.4D1 * t10 * t14 * t16)
      t53 = t44 ** 2
      t55 = lh ** 2
      t57 = 0.3141592653589793D1 ** 2
      t63 = t6 * 0.3141592653589793D1
      t64 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t65 = t64 - t21
      t67 = 0.1D1 / x2
      t71 = x2 * t10
      t74 = log(0.4D1 * t71 * t17)
      t76 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t77 = -0.1D1 + x2
      t78 = t77 ** 2
      t82 = log(0.4D1 * t71 * t17 * t78)
      t96 = 0.1D1 / x1
      t100 = x1 ** 2
      t101 = t100 * t14
      t105 = log(0.4D1 * t101 * t10 * t16)
      t118 = -(0.90D2 * t6 * 0.3141592653589793D1 * (-t7 + t20 * t21) + 
     #t31) * t33 / 0.1440D4 + t6 * 0.3141592653589793D1 * t36 / 0.16D2 +
     # (-0.180D3 * lh - 0.90D2 * t44) * t4 * t28 * t7 / 0.1440D4 + (0.18
     #0D3 * t44 * lh + 0.45D2 * t53 + 0.180D3 * t55 - 0.30D2 * t57) * t4
     # * t29 / 0.1440D4 - t63 * t65 * t33 * t67 / 0.16D2 + (0.90D2 * t6 
     #* 0.3141592653589793D1 * (t7 - t74 * t21 - t76 + t82 * t64) + 0.18
     #0D3 * t27 * t28 * t65) * t67 / 0.1440D4 - t63 * t65 * t67 * t96 / 
     #0.8D1 + (0.90D2 * t6 * 0.3141592653589793D1 * (t7 - t105 * t21) - 
     #t31) * t96 / 0.720D3 + t63 * t21 * t33 * t96 / 0.8D1
      t119 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t118)
      t122 = x1 * t1 * s
      t123 = -0.1D1 + x1
      t125 = t123 * t1 * s
      t126 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t130 = t63 * t126 * t67 * t96 / 0.8D1
      t131 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0
     #.10D1, x4)
      t136 = 0.1D1 / (-z - x1 + x1 * z)
      t138 = t123 ** 2
      t142 = log(-0.4D1 * t101 / t8 * t16 * t136 * t138)
      t144 = -t131 + t142 * t126
      t150 = 0.180D3 * t27 * t28 * t126
      t157 = t63 * t126 * t33 * t96 / 0.8D1
      t158 = -t130 + (0.90D2 * t6 * 0.3141592653589793D1 * t144 + t150) 
     #* t96 / 0.720D3 - t157
      t159 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t122, -t125, 0.0D0, t158)
      t161 = t2 * x3
      t162 = -0.1D1 + x3
      t163 = t2 * t162
      t164 = -t162
      t165 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t164, x4)
      t166 = t165 * t33
      t170 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0
     #, t164, x4)
      t174 = log(-0.4D1 * t11 * t17 * t162)
      t189 = -t63 * t166 * t67 / 0.16D2 - (0.90D2 * t6 * 0.3141592653589
     #793D1 * (t170 - t174 * t165) - 0.180D3 * t27 * t28 * t165) * t33 /
     # 0.1440D4 - t63 * t166 * t96 / 0.8D1
      t190 = FJET(XB1, XB2, s, 0.0D0, t161, 0.0D0, -t163, 0.0D0, t189)
      t193 = x2 * x3
      t195 = 0.1D1 / (t193 - 0.1D1)
      t197 = t2 * x3 * t77 * t195
      t198 = t162 * t195
      t199 = t2 * t198
      t200 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t
     #198, x4)
      t203 = cos(t12)
      t207 = Sqrt(-x3 * z * x2 * t162)
      t211 = 0.1D1 / (-z - t193 + 0.2D1 * t203 * t207)
      t216 = t6 * 0.3141592653589793D1 * t200 * t211 * z * t33 * t67 / 0
     #.16D2
      t217 = FJET(XB1, XB2, s, 0.0D0, t197, 0.0D0, t199, 0.0D0, -t216)
      t223 = t200 * t211 * z * t33 * t67
      t226 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t118)
      t229 = t2 * x1 * x3
      t231 = t2 * t123 * x3
      t233 = t162 * x1 * t2
      t235 = t162 * t123 * t2
      t236 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, t
     #164, x4)
      t240 = t63 * t236 * t33 * t96 / 0.8D1
      t241 = FJET(XB1, XB2, s, t229, -t231, -t233, t235, 0.0D0, t240)
      t246 = 0.3141592653589793D1 * t236 * t33 * t96
      t249 = FJET(XB1, XB2, s, t199, 0.0D0, t197, 0.0D0, 0.0D0, -t216)
      t254 = FJET(XB1, XB2, s, t235, -t233, -t231, t229, 0.0D0, t240)
      t259 = FJET(XB1, XB2, s, -t163, 0.0D0, t161, 0.0D0, 0.0D0, t189)
      t268 = -t130 + (0.90D2 * t6 * 0.3141592653589793D1 * t144 + t150) 
     #* t96 / 0.720D3 - t157
      t269 = FJET(XB1, XB2, s, -t125, t122, 0.0D0, 0.0D0, 0.0D0, t268)
      t272 = t2 * x1 * t77
      t275 = t2 * x1 * x2 * t136
      t280 = s * t15 * x2 * x1 * t123 * t136
      t281 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10
     #D1, x4)
      t285 = t63 * t281 * t67 * t96 / 0.8D1
      t286 = FJET(XB1, XB2, s, -t125, -t272, 0.0D0, -t275, t280, t285)
      t291 = 0.3141592653589793D1 * t281 * t67 * t96
      t294 = FJET(XB1, XB2, s, -t275, 0.0D0, -t272, -t125, t280, t285)
      rrqqbar2qqbarht4s4em1 = t119 * t118 + t159 * t158 + t190 * t189 - 
     #t217 * t4 * t28 * t223 / 0.16D2 + t226 * t118 + t241 * t4 * t5 * t
     #246 / 0.8D1 - t249 * t4 * t28 * t223 / 0.16D2 + t254 * t4 * t5 * t
     #246 / 0.8D1 + t259 * t189 + t269 * t268 + t286 * t4 * t5 * t291 / 
     #0.8D1 + t294 * t4 * t5 * t291 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht4s4em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t6 = t4 * t5
      t7 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 
     #0.10D1, x4)
      t8 = 0.3141592653589793D1 * t7
      t9 = 0.1D1 / x3
      t13 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.
     #10D1, x4)
      t20 = 0.1D1 / x1
      t24 = rrqqbar2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # 0.10D1, x4)
      t29 = z ** 2
      t33 = Sin(x4 * 0.3141592653589793D1)
      t34 = t33 ** 2
      t36 = t1 ** 2
      t37 = t36 ** 2
      t40 = log(0.4D1 / t29 / z * t34 * t37)
      t48 = t6 * t8 * t9 / 0.16D2 + t6 * 0.3141592653589793D1 * (t7 - t1
     #3) / x2 / 0.16D2 + t6 * t8 * t20 / 0.8D1 + t6 * 0.3141592653589793
     #D1 * t24 / 0.16D2 + (-0.180D3 * lh - 0.90D2 * t40) * t4 * t5 * 0.3
     #141592653589793D1 * t7 / 0.1440D4
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t48)
      t52 = x1 * t1 * s
      t55 = (-0.1D1 + x1) * t1 * s
      t56 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, 0.
     #10D1, x4)
      t58 = 0.3141592653589793D1 * t56 * t20
      t60 = t6 * t58 / 0.8D1
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t52, -t55, 0.0D0, -t60)
      t66 = t2 * x3
      t67 = -0.1D1 + x3
      t68 = t2 * t67
      t70 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0,
     # -t67, x4)
      t72 = 0.3141592653589793D1 * t70 * t9
      t74 = t6 * t72 / 0.16D2
      t75 = FJET(XB1, XB2, s, 0.0D0, t66, 0.0D0, -t68, 0.0D0, -t74)
      t80 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t48)
      t82 = FJET(XB1, XB2, s, -t68, 0.0D0, t66, 0.0D0, 0.0D0, -t74)
      t87 = FJET(XB1, XB2, s, -t55, t52, 0.0D0, 0.0D0, 0.0D0, -t60)
      rrqqbar2qqbarht4s4em2 = t49 * t48 - t61 * t4 * t5 * t58 / 0.8D1 - 
     #t75 * t4 * t5 * t72 / 0.16D2 + t80 * t48 - t82 * t4 * t5 * t72 / 0
     #.16D2 - t87 * t4 * t5 * t58 / 0.8D1

      end function



      doubleprecision function rrqqbar2qqbarht4s4em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = 0.1D1 / t1
      t7 = rrqqbar2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, 
     #0.10D1, x4)
      t10 = t4 * t5 * 0.3141592653589793D1 * t7 / 0.16D2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t10)
      t14 = t5 * 0.3141592653589793D1 * t7
      t16 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t10)
      rrqqbar2qqbarht4s4em3 = t11 * t4 * t14 / 0.16D2 + t16 * t4 * t14 /
     # 0.16D2

      end function



      doubleprecision function rrqqbar2qqbarht4s4em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2qqbarh41J1
      doubleprecision rrqqbar2qqbarh41J2
      doubleprecision rrqqbar2qqbarh41J3
      rrqqbar2qqbarht4s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2qqbarh41J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 ** 2
      t5 = z + x1 * t1
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t18 = Sqrt(x3 * t8 * t5 * x2 * t7)
      t21 = t7 * t8 * t5 + x2 * x3 + 0.2D1 * t13 * t18
      t23 = 0.1D1 - x1
      t27 = s ** 2
      t28 = t27 * t2
      t29 = x1 ** 2
      t30 = t5 ** 2
      t33 = t21 ** 2
      t36 = t23 ** 2
      t37 = x3 ** 2
      rrqqbar2qqbarh41J1 = 0.32D2 / 0.27D2 * t2 * x1 / t5 * t21 * t23 * 
     #x3 * wd * (-t28 * t29 / t30 * t33 - t28 * t36 * t37) / z / 0.31415
     #92653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh41J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 ** 2
      t5 = z + x1 * t1
      t6 = 0.1D1 / t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t5 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t5 + x2 * x3 + t20
      t22 = t6 * t21
      t23 = 0.1D1 - x1
      t27 = s ** 2
      t28 = t27 * t2
      t29 = x1 ** 2
      t30 = t5 ** 2
      t33 = t21 ** 2
      t36 = t23 ** 2
      t37 = x3 ** 2
      t45 = t27 * t1
      rrqqbar2qqbarh41J2 = 0.32D2 / 0.27D2 * t2 * x1 * t22 * t23 * x3 * 
     #wd * (t28 * t29 / t30 * t33 + t28 * t36 * t37 + 0.2D1 * t28 * x1 *
     # t22 * t23 * x3 + t45 * x1 * t6 * (t14 * t5 + x2 * t7 - t20) + t45
     # * t23 * t7) / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2qqbarh41J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = t1 ** 2
      t5 = z + x1 * t1
      t6 = 0.1D1 / t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t5 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t5 + x2 * x3 + t20
      t22 = t6 * t21
      t23 = 0.1D1 - x1
      t27 = s ** 2
      t28 = t27 * t2
      t29 = t28 * x1
      t30 = t23 * t7
      t33 = x1 ** 2
      t35 = t5 ** 2
      t36 = 0.1D1 / t35
      t40 = t14 * t5 + x2 * t7 - t20
      t43 = t27 * t1
      t50 = x1 * t6
      t53 = t23 ** 2
      t54 = x3 ** 2
      t60 = t23 * x3
      t65 = t21 ** 2
      t71 = t29 * t22 * t30 + t28 * t33 * t36 * t21 * t40 - t43 * t30 + 
     #t28 * t23 * x3 * x1 * t6 * t40 - t43 * t50 * t40 + t28 * t53 * t54
     # + t28 * t53 * x3 * t7 + 0.2D1 * t29 * t22 * t60 + t28 * t33 * t36
     # * t65 - t43 * t60 - t43 * t50 * t21
      rrqqbar2qqbarh41J3 = 0.32D2 / 0.27D2 * t2 * x1 * t22 * t23 * x3 * 
     #wd * t71 / z / 0.3141592653589793D1

      end function
  
 