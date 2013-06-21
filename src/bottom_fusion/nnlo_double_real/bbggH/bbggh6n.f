  
      subroutine bbggh6n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh61J1  
      doubleprecision bbggh61J2  
      doubleprecision bbggh62J1  
      doubleprecision bbggh62J2  
      doubleprecision bbggh6n1e1  
      doubleprecision bbggh6n1e0  
      doubleprecision bbggh6n1em1  
      doubleprecision bbggh6n1em2  
      doubleprecision bbggh6n1em3  
      doubleprecision bbggh6n1em4  
      doubleprecision bbggh6n2e1  
      doubleprecision bbggh6n2e0  
      doubleprecision bbggh6n2em1  
      doubleprecision bbggh6n2em2  
      doubleprecision bbggh6n2em3  
      doubleprecision bbggh6n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh6n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh6n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh6n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh6n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh6n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh6n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh6n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh6n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh6n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh6n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh6n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bbggh6n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh6n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x1 * x3
      t4 = t2 * t3
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t11 = t9 * t1 * x1
      t12 = t1 * t5
      t13 = t9 * t12
      t14 = s ** 2
      t15 = 0.1D1 / t14
      t16 = lh * t15
      t17 = -t5
      t18 = bbggh61J2(s, XB1, XB2, z, lh, wd, t17, 0.0D0, x3, x4)
      t19 = x1 ** 2
      t20 = x3 * t19
      t21 = x4 * 0.3141592653589793D1
      t22 = Sin(t21)
      t23 = t22 ** 2
      t24 = t20 * t23
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = x1 * z
      t28 = 0.1D1 - x1 + t27
      t29 = 0.1D1 / t28
      t30 = t26 * t29
      t31 = t5 ** 2
      t33 = t30 * t31 * t8
      t36 = log(-0.4D1 * t24 * t33)
      t37 = bbggh61J1(s, XB1, XB2, z, lh, wd, t17, 0.0D0, x3, x4)
      t43 = t36 ** 2
      t49 = lh ** 2
      t51 = 0.3141592653589793D1 ** 2
      t53 = 0.180D3 * t49 - 0.30D2 * t51
      t54 = t53 * t15
      t57 = 0.1D1 / x3
      t59 = 0.1D1 / x1
      t61 = x2 * x3
      t62 = t19 * t23
      t63 = t61 * t62
      t66 = log(-0.4D1 * t63 * t33)
      t75 = 0.1D1 / x2
      t76 = t75 * t59
      t79 = -(-0.180D3 * t16 * (t18 - t36 * t37) + 0.90D2 * t15 * (-t36 
     #* t18 + t43 * t37 / 0.2D1) + t54 * t37) * t57 * t59 / 0.2880D4 - (
     #0.90D2 * t15 * (-t66 * t37 + t18) - 0.180D3 * t16 * t37) * t57 * t
     #76 / 0.2880D4
      t80 = FJET(XB1, XB2, s, t4, -t7, -t11, t13, 0.0D0, t79)
      t82 = t2 * x3
      t83 = t2 * t8
      t84 = t23 * t26
      t85 = t84 * t8
      t88 = log(-0.4D1 * t20 * t85)
      t89 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t91 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t96 = t88 ** 2
      t102 = t54 * t89
      t107 = t61 * t19
      t110 = log(-0.4D1 * t107 * t85)
      t123 = log(-0.4D1 * t61 * t85)
      t129 = t123 ** 2
      t139 = x3 * t23
      t143 = log(-0.4D1 * t139 * t26 * t8)
      t145 = t143 ** 2
      t156 = t145 * t143
      t163 = 0.120D3 * t49 * lh
      t165 = 0.60D2 * lh * t51
      t166 = -0.2884936567583026D3 - t163 + t165
      t167 = t166 * t15
      t172 = -(-0.180D3 * t16 * (t88 * t89 - t91) + 0.90D2 * t15 * (t88 
     #* t91 - t96 * t89 / 0.2D1) - t102) * t57 * t59 / 0.2880D4 - (0.90D
     #2 * t15 * (t110 * t89 - t91) + 0.180D3 * t16 * t89) * t57 * t76 / 
     #0.2880D4 - (-0.180D3 * t16 * (-t91 + t123 * t89) + 0.90D2 * t15 * 
     #(t123 * t91 - t129 * t89 / 0.2D1) - t102) * t57 * t75 / 0.5760D4 +
     # (-0.180D3 * t16 * (-t143 * t91 + t145 * t89 / 0.2D1) + t54 * (t91
     # - t143 * t89) + 0.90D2 * t15 * (t145 * t91 / 0.2D1 - t156 * t89 /
     # 0.6D1) + t167 * t89) * t57 / 0.5760D4
      t173 = FJET(XB1, XB2, s, t82, 0.0D0, -t83, 0.0D0, 0.0D0, t172)
      t175 = bbggh62J2(s, XB1, XB2, z, lh, wd, t17, 0.0D0, x3, x4)
      t176 = bbggh62J1(s, XB1, XB2, z, lh, wd, t17, 0.0D0, x3, x4)
      t201 = -(-0.180D3 * t16 * (t175 - t36 * t176) + 0.90D2 * t15 * (t4
     #3 * t176 / 0.2D1 - t36 * t175) + t54 * t176) * t57 * t59 / 0.2880D
     #4 - (0.90D2 * t15 * (-t66 * t176 + t175) - 0.180D3 * t16 * t176) *
     # t57 * t76 / 0.2880D4
      t202 = FJET(XB1, XB2, s, -t11, t13, t4, -t7, 0.0D0, t201)
      t204 = t2 * x1
      t205 = t2 * t5
      t206 = t30 * t31
      t209 = log(0.4D1 * t62 * t206)
      t210 = bbggh62J2(s, XB1, XB2, z, lh, wd, t17, 0.0D0, 0.0D0, x4)
      t212 = t209 ** 2
      t213 = bbggh62J1(s, XB1, XB2, z, lh, wd, t17, 0.0D0, 0.0D0, x4)
      t224 = t212 * t209
      t235 = log(0.4D1 * t24 * t206)
      t240 = t235 ** 2
      t247 = t54 * t213
      t255 = log(0.4D1 * t107 * t84 * t29 * t31)
      t265 = x2 * t19
      t266 = t265 * t23
      t269 = log(0.4D1 * t266 * t206)
      t274 = t269 ** 2
      t285 = -(-0.180D3 * t16 * (t209 * t210 - t212 * t213 / 0.2D1) + t5
     #4 * (-t210 + t209 * t213) + 0.90D2 * t15 * (-t212 * t210 / 0.2D1 +
     # t224 * t213 / 0.6D1) - t167 * t213) * t59 / 0.2880D4 - (-0.180D3 
     #* t16 * (-t210 + t235 * t213) + 0.90D2 * t15 * (-t240 * t213 / 0.2
     #D1 + t235 * t210) - t247) * t57 * t59 / 0.2880D4 - (0.90D2 * t15 *
     # (t255 * t213 - t210) + 0.180D3 * t16 * t213) * t57 * t76 / 0.2880
     #D4 + (-0.180D3 * t16 * (-t269 * t213 + t210) + 0.90D2 * t15 * (t27
     #4 * t213 / 0.2D1 - t269 * t210) + t247) * t75 * t59 / 0.2880D4
      t286 = FJET(XB1, XB2, s, t204, -t205, 0.0D0, 0.0D0, 0.0D0, t285)
      t288 = 0.2D1 * t61
      t289 = cos(t21)
      t290 = -0.1D1 + x2
      t292 = x3 * t8
      t294 = Sqrt(x2 * t290 * t292)
      t296 = 0.2D1 * t289 * t294
      t298 = t2 * (0.1D1 - x2 - x3 + t288 + t296)
      t300 = t2 * (-x3 + t288 - x2 + t296)
      t301 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t306 = log(0.4D1 * t107 * t84 * t290 * t8)
      t307 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t318 = x2 * t23
      t319 = t318 * t26
      t323 = log(0.4D1 * t319 * t292 * t290)
      t328 = t323 ** 2
      t340 = -(0.90D2 * t15 * (t301 - t306 * t307) - 0.180D3 * t16 * t30
     #7) * t57 * t76 / 0.2880D4 - (-0.180D3 * t16 * (-t323 * t307 + t301
     #) + 0.90D2 * t15 * (t328 * t307 / 0.2D1 - t323 * t301) + t54 * t30
     #7) * t57 * t75 / 0.5760D4
      t341 = FJET(XB1, XB2, s, t298, 0.0D0, -t300, 0.0D0, 0.0D0, t340)
      t343 = x2 * x1
      t345 = t3 * z
      t346 = t61 * x1
      t347 = t61 * t27
      t352 = Sqrt(x3 * t290 * t28 * x2 * t8)
      t354 = 0.2D1 * t289 * t352
      t355 = 0.1D1 - x1 + t27 - x2 + t343 - t343 * z - x3 + t3 - t345 + 
     #t288 - t346 + t347 + t354
      t358 = t2 * t5 * t355 * t29
      t362 = t2 * t5 * (-x3 + t3 - t345 + t288 - t346 + t347 - x2 + t354
     #) * t29
      t363 = t1 ** 2
      t368 = s * t363 * x2 * x1 * t5 * t29
      t369 = t31 * t290
      t374 = log(0.4D1 * t63 * t30 * t369 * t8)
      t375 = bbggh62J1(s, XB1, XB2, z, lh, wd, t17, x2, x3, x4)
      t377 = bbggh62J2(s, XB1, XB2, z, lh, wd, t17, x2, x3, x4)
      t383 = 0.90D2 * t15 * (t374 * t375 - t377) + 0.180D3 * t16 * t375
      t386 = t383 * t57 * t76 / 0.2880D4
      t387 = FJET(XB1, XB2, s, -t11, -t358, t4, t362, -t368, -t386)
      t390 = t57 * t75 * t59
      t393 = bbggh61J2(s, XB1, XB2, z, lh, wd, t17, x2, x3, x4)
      t394 = bbggh61J1(s, XB1, XB2, z, lh, wd, t17, x2, x3, x4)
      t401 = 0.90D2 * t15 * (-t393 + t374 * t394) + 0.180D3 * t16 * t394
      t404 = t401 * t57 * t76 / 0.2880D4
      t405 = FJET(XB1, XB2, s, t4, t362, -t11, -t358, -t368, -t404)
      t409 = FJET(XB1, XB2, s, -t358, -t11, t362, t4, -t368, -t386)
      t413 = FJET(XB1, XB2, s, t362, t4, -t358, -t11, -t368, -t404)
      t417 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t419 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t429 = t54 * t417
      t478 = -(-0.180D3 * t16 * (t88 * t417 - t419) + 0.90D2 * t15 * (-t
     #96 * t417 / 0.2D1 + t88 * t419) - t429) * t57 * t59 / 0.2880D4 - (
     #0.90D2 * t15 * (-t419 + t110 * t417) + 0.180D3 * t16 * t417) * t57
     # * t76 / 0.2880D4 - (-0.180D3 * t16 * (t123 * t417 - t419) + 0.90D
     #2 * t15 * (t123 * t419 - t129 * t417 / 0.2D1) - t429) * t57 * t75 
     #/ 0.5760D4 + (-0.180D3 * t16 * (-t143 * t419 + t145 * t417 / 0.2D1
     #) + t54 * (-t143 * t417 + t419) + 0.90D2 * t15 * (t145 * t419 / 0.
     #2D1 - t156 * t417 / 0.6D1) + t167 * t417) * t57 / 0.5760D4
      t479 = FJET(XB1, XB2, s, 0.0D0, -t83, 0.0D0, t82, 0.0D0, t478)
      t481 = FJET(XB1, XB2, s, -t83, 0.0D0, t82, 0.0D0, 0.0D0, t478)
      t484 = x2 * s * t1
      t485 = t290 * s
      t486 = t485 * t1
      t487 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t488 = t84 * t290
      t491 = log(-0.4D1 * t107 * t488)
      t492 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t505 = log(-0.4D1 * t265 * t488)
      t511 = t505 ** 2
      t517 = t54 * t492
      t525 = log(-0.4D1 * t318 * t26 * t290)
      t527 = t525 ** 2
      t538 = t527 * t525
      t550 = log(-0.4D1 * t61 * t488)
      t555 = t550 ** 2
      t566 = -(0.90D2 * t15 * (-t487 + t491 * t492) + 0.180D3 * t16 * t4
     #92) * t57 * t76 / 0.2880D4 + (-0.180D3 * t16 * (t487 - t505 * t492
     #) + 0.90D2 * t15 * (-t505 * t487 + t511 * t492 / 0.2D1) + t517) * 
     #t75 * t59 / 0.2880D4 + (-0.180D3 * t16 * (-t525 * t487 + t527 * t4
     #92 / 0.2D1) + t54 * (t487 - t525 * t492) + 0.90D2 * t15 * (t527 * 
     #t487 / 0.2D1 - t538 * t492 / 0.6D1) + t167 * t492) * t75 / 0.5760D
     #4 - (-0.180D3 * t16 * (t550 * t492 - t487) + 0.90D2 * t15 * (-t555
     # * t492 / 0.2D1 + t550 * t487) - t517) * t57 * t75 / 0.5760D4
      t567 = FJET(XB1, XB2, s, 0.0D0, t484, 0.0D0, -t486, 0.0D0, t566)
      t569 = FJET(XB1, XB2, s, -t7, t4, t13, -t11, 0.0D0, t79)
      t571 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t572 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t598 = -(-0.180D3 * t16 * (t571 - t323 * t572) + 0.90D2 * t15 * (-
     #t323 * t571 + t328 * t572 / 0.2D1) + t54 * t572) * t57 * t75 / 0.5
     #760D4 - (0.90D2 * t15 * (t571 - t306 * t572) - 0.180D3 * t16 * t57
     #2) * t57 * t76 / 0.2880D4
      t599 = FJET(XB1, XB2, s, 0.0D0, -t300, 0.0D0, t298, 0.0D0, t598)
      t601 = bbggh61J2(s, XB1, XB2, z, lh, wd, t17, 0.0D0, 0.0D0, x4)
      t603 = bbggh61J1(s, XB1, XB2, z, lh, wd, t17, 0.0D0, 0.0D0, x4)
      t632 = t54 * t603
      t659 = -(-0.180D3 * t16 * (t209 * t601 - t212 * t603 / 0.2D1) + t5
     #4 * (-t601 + t209 * t603) + 0.90D2 * t15 * (-t212 * t601 / 0.2D1 +
     # t224 * t603 / 0.6D1) - t167 * t603) * t59 / 0.2880D4 - (-0.180D3 
     #* t16 * (t235 * t603 - t601) + 0.90D2 * t15 * (-t240 * t603 / 0.2D
     #1 + t235 * t601) - t632) * t57 * t59 / 0.2880D4 - (0.90D2 * t15 * 
     #(t255 * t603 - t601) + 0.180D3 * t16 * t603) * t57 * t76 / 0.2880D
     #4 + (-0.180D3 * t16 * (t601 - t269 * t603) + 0.90D2 * t15 * (t274 
     #* t603 / 0.2D1 - t269 * t601) + t632) * t75 * t59 / 0.2880D4
      t660 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t205, t204, 0.0D0, t659)
      t664 = t2 * t5 * x2 * t29
      t665 = t485 * t12
      t666 = bbggh61J2(s, XB1, XB2, z, lh, wd, t17, x2, 0.0D0, x4)
      t667 = t30 * t369
      t670 = log(-0.4D1 * t63 * t667)
      t671 = bbggh61J1(s, XB1, XB2, z, lh, wd, t17, x2, 0.0D0, x4)
      t683 = log(-0.4D1 * t266 * t667)
      t688 = t683 ** 2
      t700 = -(0.90D2 * t15 * (t666 - t670 * t671) - 0.180D3 * t16 * t67
     #1) * t57 * t76 / 0.2880D4 + (-0.180D3 * t16 * (-t666 + t683 * t671
     #) + 0.90D2 * t15 * (-t688 * t671 / 0.2D1 + t683 * t666) - t54 * t6
     #71) * t75 * t59 / 0.2880D4
      t701 = FJET(XB1, XB2, s, 0.0D0, -t664, t204, t665, -t368, t700)
      t703 = t80 * t79 + t173 * t172 + t202 * t201 + t286 * t285 + t341 
     #* t340 - t387 * t383 * t390 / 0.2880D4 - t405 * t401 * t390 / 0.28
     #80D4 - t409 * t383 * t390 / 0.2880D4 - t413 * t401 * t390 / 0.2880
     #D4 + t479 * t478 + t481 * t478 + t567 * t566 + t569 * t79 + t599 *
     # t598 + t660 * t659 + t701 * t700
      t704 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t706 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t736 = t54 * t706
      t765 = (-0.180D3 * t16 * (-t525 * t704 + t527 * t706 / 0.2D1) + t5
     #4 * (-t525 * t706 + t704) + 0.90D2 * t15 * (t527 * t704 / 0.2D1 - 
     #t538 * t706 / 0.6D1) + t167 * t706) * t75 / 0.5760D4 - (-0.180D3 *
     # t16 * (t550 * t706 - t704) + 0.90D2 * t15 * (t550 * t704 - t555 *
     # t706 / 0.2D1) - t736) * t57 * t75 / 0.5760D4 - (0.90D2 * t15 * (t
     #491 * t706 - t704) + 0.180D3 * t16 * t706) * t57 * t76 / 0.2880D4 
     #+ (-0.180D3 * t16 * (t704 - t505 * t706) + 0.90D2 * t15 * (-t505 *
     # t704 + t511 * t706 / 0.2D1) + t736) * t75 * t59 / 0.2880D4
      t766 = FJET(XB1, XB2, s, -t486, 0.0D0, t484, 0.0D0, 0.0D0, t765)
      t768 = FJET(XB1, XB2, s, -t664, 0.0D0, t665, t204, -t368, t700)
      t770 = t62 * t26
      t772 = log(0.4D1 * t770)
      t773 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t775 = t772 ** 2
      t776 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t787 = t775 * t772
      t793 = t167 * t776
      t799 = log(0.4D1 * t20 * t84)
      t805 = t799 ** 2
      t811 = t54 * t776
      t818 = log(0.4D1 * t61 * t770)
      t831 = log(0.4D1 * t265 * t84)
      t837 = t831 ** 2
      t849 = log(0.4D1 * t139 * t26)
      t851 = t849 ** 2
      t862 = t851 * t849
      t872 = log(0.4D1 * t319)
      t874 = t872 ** 2
      t885 = t874 * t872
      t896 = log(0.4D1 * t61 * t84)
      t902 = t896 ** 2
      t913 = log(0.4D1 * t84)
      t915 = t913 ** 2
      t918 = t915 * t913
      t921 = (-t913 * t53 - 0.90D2 * t915 * lh - 0.2884936567583026D3 - 
     #t163 + t165 - 0.15D2 * t918) * t15
      t924 = t915 ** 2
      t928 = t51 ** 2
      t929 = t49 ** 2
      t938 = (0.15D2 / 0.4D1 * t924 - t913 * t166 + 0.5769873135166051D3
     # * lh + t928 + 0.60D2 * t929 - 0.60D2 * t49 * t51 + t915 * t53 / 0
     #.2D1 + 0.30D2 * t918 * lh) * t15
      t941 = -(-0.180D3 * t16 * (-t772 * t773 + t775 * t776 / 0.2D1) + t
     #54 * (t773 - t772 * t776) + 0.90D2 * t15 * (t775 * t773 / 0.2D1 - 
     #t787 * t776 / 0.6D1) + t793) * t59 / 0.2880D4 - (-0.180D3 * t16 * 
     #(t773 - t799 * t776) + 0.90D2 * t15 * (-t799 * t773 + t805 * t776 
     #/ 0.2D1) + t811) * t57 * t59 / 0.2880D4 - (0.90D2 * t15 * (t773 - 
     #t818 * t776) - 0.180D3 * t16 * t776) * t57 * t76 / 0.2880D4 + (-0.
     #180D3 * t16 * (t831 * t776 - t773) + 0.90D2 * t15 * (t831 * t773 -
     # t837 * t776 / 0.2D1) - t811) * t75 * t59 / 0.2880D4 + (-0.180D3 *
     # t16 * (t849 * t773 - t851 * t776 / 0.2D1) + t54 * (-t773 + t849 *
     # t776) + 0.90D2 * t15 * (-t851 * t773 / 0.2D1 + t862 * t776 / 0.6D
     #1) - t793) * t57 / 0.5760D4 + (-0.180D3 * t16 * (t872 * t773 - t87
     #4 * t776 / 0.2D1) + t54 * (-t773 + t872 * t776) + 0.90D2 * t15 * (
     #-t874 * t773 / 0.2D1 + t885 * t776 / 0.6D1) - t793) * t75 / 0.5760
     #D4 - (-0.180D3 * t16 * (-t896 * t776 + t773) + 0.90D2 * t15 * (-t8
     #96 * t773 + t902 * t776 / 0.2D1) + t811) * t57 * t75 / 0.5760D4 - 
     #t921 * t773 / 0.5760D4 - t938 * t776 / 0.5760D4
      t942 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t941)
      t944 = FJET(XB1, XB2, s, 0.0D0, -t486, 0.0D0, t484, 0.0D0, t765)
      t946 = FJET(XB1, XB2, s, -t205, t204, 0.0D0, 0.0D0, 0.0D0, t285)
      t948 = FJET(XB1, XB2, s, 0.0D0, t82, 0.0D0, -t83, 0.0D0, t172)
      t950 = bbggh62J2(s, XB1, XB2, z, lh, wd, t17, x2, 0.0D0, x4)
      t951 = bbggh62J1(s, XB1, XB2, z, lh, wd, t17, x2, 0.0D0, x4)
      t976 = -(0.90D2 * t15 * (t950 - t670 * t951) - 0.180D3 * t16 * t95
     #1) * t57 * t76 / 0.2880D4 + (-0.180D3 * t16 * (t683 * t951 - t950)
     # + 0.90D2 * t15 * (t683 * t950 - t688 * t951 / 0.2D1) - t54 * t951
     #) * t75 * t59 / 0.2880D4
      t977 = FJET(XB1, XB2, s, t204, t665, 0.0D0, -t664, -t368, t976)
      t979 = FJET(XB1, XB2, s, t665, t204, -t664, 0.0D0, -t368, t976)
      t981 = FJET(XB1, XB2, s, t484, 0.0D0, -t486, 0.0D0, 0.0D0, t566)
      t983 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t941)
      t985 = FJET(XB1, XB2, s, 0.0D0, t298, 0.0D0, -t300, 0.0D0, t340)
      t987 = FJET(XB1, XB2, s, t13, -t11, -t7, t4, 0.0D0, t201)
      t989 = FJET(XB1, XB2, s, -t300, 0.0D0, t298, 0.0D0, 0.0D0, t598)
      t991 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t204, -t205, 0.0D0, t659)
      t993 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t996 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t1011 = t167 * t993
      t1025 = t54 * t993
      t1110 = -(-0.180D3 * t16 * (t775 * t993 / 0.2D1 - t772 * t996) + t
     #54 * (t996 - t772 * t993) + 0.90D2 * t15 * (-t787 * t993 / 0.6D1 +
     # t775 * t996 / 0.2D1) + t1011) * t59 / 0.2880D4 - (-0.180D3 * t16 
     #* (t996 - t799 * t993) + 0.90D2 * t15 * (-t799 * t996 + t805 * t99
     #3 / 0.2D1) + t1025) * t57 * t59 / 0.2880D4 - (0.90D2 * t15 * (t996
     # - t818 * t993) - 0.180D3 * t16 * t993) * t57 * t76 / 0.2880D4 + (
     #-0.180D3 * t16 * (-t996 + t831 * t993) + 0.90D2 * t15 * (-t837 * t
     #993 / 0.2D1 + t831 * t996) - t1025) * t75 * t59 / 0.2880D4 - t921 
     #* t996 / 0.5760D4 - t938 * t993 / 0.5760D4 + (-0.180D3 * t16 * (-t
     #851 * t993 / 0.2D1 + t849 * t996) + t54 * (-t996 + t849 * t993) + 
     #0.90D2 * t15 * (t862 * t993 / 0.6D1 - t851 * t996 / 0.2D1) - t1011
     #) * t57 / 0.5760D4 + (-0.180D3 * t16 * (-t874 * t993 / 0.2D1 + t87
     #2 * t996) + t54 * (-t996 + t872 * t993) + 0.90D2 * t15 * (t885 * t
     #993 / 0.6D1 - t874 * t996 / 0.2D1) - t1011) * t75 / 0.5760D4 - (-0
     #.180D3 * t16 * (t996 - t896 * t993) + 0.90D2 * t15 * (-t896 * t996
     # + t902 * t993 / 0.2D1) + t1025) * t57 * t75 / 0.5760D4
      t1111 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1110)
      t1113 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1110)
      t1115 = t766 * t765 + t768 * t700 + t942 * t941 + t944 * t765 + t9
     #46 * t285 + t948 * t172 + t977 * t976 + t979 * t976 + t981 * t566 
     #+ t983 * t941 + t985 * t340 + t987 * t201 + t989 * t598 + t991 * t
     #659 + t1111 * t1110 + t1113 * t1110
      bbggh6n1e1 = t703 + t1115

      end function



      doubleprecision function bbggh6n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x2
      t5 = t4 * s
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t8 = t5 * t7
      t10 = x1 * z
      t11 = 0.1D1 - x1 + t10
      t12 = 0.1D1 / t11
      t14 = t2 * t6 * x2 * t12
      t15 = t1 ** 2
      t20 = s * t15 * x2 * x1 * t6 * t12
      t21 = s ** 2
      t22 = 0.1D1 / t21
      t23 = -t6
      t24 = bbggh62J1(s, XB1, XB2, z, lh, wd, t23, x2, 0.0D0, x4)
      t26 = 0.1D1 / x3
      t27 = 0.1D1 / x2
      t29 = 0.1D1 / x1
      t30 = t26 * t27 * t29
      t33 = x1 ** 2
      t34 = x2 * t33
      t35 = x4 * 0.3141592653589793D1
      t36 = Sin(t35)
      t37 = t36 ** 2
      t38 = t34 * t37
      t39 = z ** 2
      t40 = 0.1D1 / t39
      t41 = t40 * t12
      t42 = t6 ** 2
      t47 = log(-0.4D1 * t38 * t41 * t42 * t4)
      t49 = bbggh62J2(s, XB1, XB2, z, lh, wd, t23, x2, 0.0D0, x4)
      t53 = lh * t22
      t60 = -t22 * t24 * t30 / 0.32D2 + (0.90D2 * t22 * (t47 * t24 - t49
     #) + 0.180D3 * t53 * t24) * t27 * t29 / 0.2880D4
      t61 = FJET(XB1, XB2, s, t3, t8, 0.0D0, -t14, -t20, t60)
      t63 = -0.1D1 + x3
      t64 = t2 * t63
      t65 = t2 * x3
      t66 = x2 * x3
      t67 = t37 * t40
      t68 = t67 * t63
      t71 = log(-0.4D1 * t66 * t68)
      t72 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t74 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t79 = 0.180D3 * t53 * t72
      t84 = x3 * t33
      t87 = log(-0.4D1 * t84 * t68)
      t99 = x3 * t37
      t103 = log(-0.4D1 * t99 * t40 * t63)
      t109 = t103 ** 2
      t115 = lh ** 2
      t116 = 0.180D3 * t115
      t117 = 0.3141592653589793D1 ** 2
      t118 = 0.30D2 * t117
      t119 = t116 - t118
      t120 = t119 * t22
      t125 = -(0.90D2 * t22 * (t71 * t72 - t74) + t79) * t26 * t27 / 0.5
     #760D4 - (0.90D2 * t22 * (t87 * t72 - t74) + t79) * t26 * t29 / 0.2
     #880D4 + t22 * t72 * t30 / 0.32D2 + (-0.180D3 * t53 * (-t103 * t72 
     #+ t74) + 0.90D2 * t22 * (-t103 * t74 + t109 * t72 / 0.2D1) + t120 
     #* t72) * t26 / 0.5760D4
      t126 = FJET(XB1, XB2, s, 0.0D0, -t64, 0.0D0, t65, 0.0D0, t125)
      t128 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t130 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t135 = 0.180D3 * t53 * t128
      t165 = -(0.90D2 * t22 * (t87 * t128 - t130) + t135) * t26 * t29 / 
     #0.2880D4 + t22 * t128 * t30 / 0.32D2 + (-0.180D3 * t53 * (t130 - t
     #103 * t128) + 0.90D2 * t22 * (-t103 * t130 + t109 * t128 / 0.2D1) 
     #+ t120 * t128) * t26 / 0.5760D4 - (0.90D2 * t22 * (-t130 + t71 * t
     #128) + t135) * t26 * t27 / 0.5760D4
      t166 = FJET(XB1, XB2, s, t65, 0.0D0, -t64, 0.0D0, 0.0D0, t165)
      t168 = FJET(XB1, XB2, s, -t64, 0.0D0, t65, 0.0D0, 0.0D0, t125)
      t170 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t173 = log(0.4D1 * t84 * t67)
      t174 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t180 = 0.180D3 * t53 * t174
      t190 = log(0.4D1 * t34 * t67)
      t199 = t33 * t37
      t202 = log(0.4D1 * t199 * t40)
      t207 = t202 ** 2
      t214 = t120 * t174
      t220 = log(0.4D1 * t99 * t40)
      t225 = t220 ** 2
      t237 = log(0.4D1 * t66 * t67)
      t246 = x2 * t37
      t247 = t246 * t40
      t249 = log(0.4D1 * t247)
      t254 = t249 ** 2
      t265 = log(0.4D1 * t67)
      t268 = t265 ** 2
      t271 = (0.180D3 * t265 * lh + t116 - t118 + 0.45D2 * t268) * t22
      t284 = (-t265 * t119 - 0.90D2 * t268 * lh - 0.2884936567583026D3 -
     # 0.120D3 * t115 * lh + 0.60D2 * lh * t117 - 0.15D2 * t268 * t265) 
     #* t22
      t287 = -(0.90D2 * t22 * (t170 - t173 * t174) - t180) * t26 * t29 /
     # 0.2880D4 - t22 * t174 * t30 / 0.32D2 + (0.90D2 * t22 * (-t170 + t
     #190 * t174) + t180) * t27 * t29 / 0.2880D4 - (-0.180D3 * t53 * (t1
     #70 - t202 * t174) + 0.90D2 * t22 * (t207 * t174 / 0.2D1 - t202 * t
     #170) + t214) * t29 / 0.2880D4 + (-0.180D3 * t53 * (-t170 + t220 * 
     #t174) + 0.90D2 * t22 * (-t225 * t174 / 0.2D1 + t220 * t170) - t214
     #) * t26 / 0.5760D4 - (0.90D2 * t22 * (t170 - t237 * t174) - t180) 
     #* t26 * t27 / 0.5760D4 + (-0.180D3 * t53 * (-t170 + t249 * t174) +
     # 0.90D2 * t22 * (-t254 * t174 / 0.2D1 + t249 * t170) - t214) * t27
     # / 0.5760D4 - t271 * t170 / 0.5760D4 - t284 * t174 / 0.5760D4
      t288 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t287)
      t290 = t5 * t1
      t292 = x2 * s * t1
      t293 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t297 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t298 = t67 * t4
      t301 = log(-0.4D1 * t34 * t298)
      t307 = 0.180D3 * t53 * t293
      t314 = log(-0.4D1 * t66 * t298)
      t326 = log(-0.4D1 * t246 * t40 * t4)
      t332 = t326 ** 2
      t342 = t22 * t293 * t30 / 0.32D2 + (0.90D2 * t22 * (t297 - t301 * 
     #t293) - t307) * t27 * t29 / 0.2880D4 - (0.90D2 * t22 * (t314 * t29
     #3 - t297) + t307) * t26 * t27 / 0.5760D4 + (-0.180D3 * t53 * (-t32
     #6 * t293 + t297) + 0.90D2 * t22 * (-t326 * t297 + t332 * t293 / 0.
     #2D1) + t120 * t293) * t27 / 0.5760D4
      t343 = FJET(XB1, XB2, s, -t290, 0.0D0, t292, 0.0D0, 0.0D0, t342)
      t345 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t347 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t352 = 0.180D3 * t53 * t345
      t382 = -(0.90D2 * t22 * (t314 * t345 - t347) + t352) * t26 * t27 /
     # 0.5760D4 + (-0.180D3 * t53 * (t347 - t326 * t345) + 0.90D2 * t22 
     #* (-t326 * t347 + t332 * t345 / 0.2D1) + t120 * t345) * t27 / 0.57
     #60D4 + t22 * t345 * t30 / 0.32D2 + (0.90D2 * t22 * (t347 - t301 * 
     #t345) - t352) * t27 * t29 / 0.2880D4
      t383 = FJET(XB1, XB2, s, t292, 0.0D0, -t290, 0.0D0, 0.0D0, t382)
      t385 = t63 * s
      t387 = t385 * t1 * x1
      t388 = t385 * t7
      t389 = x1 * x3
      t390 = t2 * t389
      t392 = t2 * t6 * x3
      t393 = bbggh62J2(s, XB1, XB2, z, lh, wd, t23, 0.0D0, x3, x4)
      t394 = t84 * t37
      t399 = log(-0.4D1 * t394 * t41 * t42 * t63)
      t400 = bbggh62J1(s, XB1, XB2, z, lh, wd, t23, 0.0D0, x3, x4)
      t414 = -(0.90D2 * t22 * (t393 - t399 * t400) - 0.180D3 * t53 * t40
     #0) * t26 * t29 / 0.2880D4 - t22 * t400 * t30 / 0.32D2
      t415 = FJET(XB1, XB2, s, -t387, t388, t390, -t392, 0.0D0, t414)
      t417 = FJET(XB1, XB2, s, t8, t3, -t14, 0.0D0, -t20, t60)
      t419 = FJET(XB1, XB2, s, t388, -t387, -t392, t390, 0.0D0, t414)
      t421 = t2 * t6
      t422 = bbggh62J2(s, XB1, XB2, z, lh, wd, t23, 0.0D0, 0.0D0, x4)
      t423 = t41 * t42
      t426 = log(0.4D1 * t394 * t423)
      t427 = bbggh62J1(s, XB1, XB2, z, lh, wd, t23, 0.0D0, 0.0D0, x4)
      t433 = 0.180D3 * t53 * t427
      t443 = log(0.4D1 * t38 * t423)
      t454 = log(0.4D1 * t199 * t423)
      t460 = t454 ** 2
      t470 = -(0.90D2 * t22 * (-t422 + t426 * t427) + t433) * t26 * t29 
     #/ 0.2880D4 + t22 * t427 * t30 / 0.32D2 + (0.90D2 * t22 * (-t443 * 
     #t427 + t422) - t433) * t27 * t29 / 0.2880D4 - (-0.180D3 * t53 * (-
     #t422 + t454 * t427) + 0.90D2 * t22 * (t454 * t422 - t460 * t427 / 
     #0.2D1) - t120 * t427) * t29 / 0.2880D4
      t471 = FJET(XB1, XB2, s, -t421, t3, 0.0D0, 0.0D0, 0.0D0, t470)
      t473 = bbggh61J2(s, XB1, XB2, z, lh, wd, t23, 0.0D0, x3, x4)
      t474 = bbggh61J1(s, XB1, XB2, z, lh, wd, t23, 0.0D0, x3, x4)
      t488 = -(0.90D2 * t22 * (t473 - t399 * t474) - 0.180D3 * t53 * t47
     #4) * t26 * t29 / 0.2880D4 - t22 * t474 * t30 / 0.32D2
      t489 = FJET(XB1, XB2, s, t390, -t392, -t387, t388, 0.0D0, t488)
      t491 = FJET(XB1, XB2, s, -t392, t390, t388, -t387, 0.0D0, t488)
      t493 = x2 * x1
      t495 = t389 * z
      t496 = 0.2D1 * t66
      t497 = t66 * x1
      t498 = t66 * t10
      t499 = cos(t35)
      t504 = Sqrt(x3 * t4 * t11 * x2 * t63)
      t506 = 0.2D1 * t499 * t504
      t507 = 0.1D1 - x1 + t10 - x2 + t493 - t493 * z - x3 + t389 - t495 
     #+ t496 - t497 + t498 + t506
      t510 = t2 * t6 * t507 * t12
      t514 = t2 * t6 * (-x3 + t389 - t495 + t496 - t497 + t498 - x2 + t5
     #06) * t12
      t515 = bbggh62J1(s, XB1, XB2, z, lh, wd, t23, x2, x3, x4)
      t518 = t22 * t515 * t30 / 0.32D2
      t519 = FJET(XB1, XB2, s, -t387, -t510, t390, t514, -t20, t518)
      t524 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t525 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t531 = 0.180D3 * t53 * t525
      t557 = t120 * t525
      t599 = -(0.90D2 * t22 * (t524 - t173 * t525) - t531) * t26 * t29 /
     # 0.2880D4 - t22 * t525 * t30 / 0.32D2 + (0.90D2 * t22 * (t190 * t5
     #25 - t524) + t531) * t27 * t29 / 0.2880D4 - (-0.180D3 * t53 * (t52
     #4 - t202 * t525) + 0.90D2 * t22 * (-t202 * t524 + t207 * t525 / 0.
     #2D1) + t557) * t29 / 0.2880D4 - t271 * t524 / 0.5760D4 - t284 * t5
     #25 / 0.5760D4 + (-0.180D3 * t53 * (-t524 + t220 * t525) + 0.90D2 *
     # t22 * (t220 * t524 - t225 * t525 / 0.2D1) - t557) * t26 / 0.5760D
     #4 - (0.90D2 * t22 * (-t237 * t525 + t524) - t531) * t26 * t27 / 0.
     #5760D4 + (-0.180D3 * t53 * (-t524 + t249 * t525) + 0.90D2 * t22 * 
     #(t249 * t524 - t254 * t525 / 0.2D1) - t557) * t27 / 0.5760D4
      t600 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t599)
      t602 = FJET(XB1, XB2, s, -t510, -t387, t514, t390, -t20, t518)
      t607 = t61 * t60 + t126 * t125 + t166 * t165 + t168 * t125 + t288 
     #* t287 + t343 * t342 + t383 * t382 + t415 * t414 + t417 * t60 + t4
     #19 * t414 + t471 * t470 + t489 * t488 + t491 * t488 + t519 * t22 *
     # t515 * t30 / 0.32D2 + t600 * t599 + t602 * t22 * t515 * t30 / 0.3
     #2D2
      t608 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t287)
      t610 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t599)
      t612 = bbggh61J1(s, XB1, XB2, z, lh, wd, t23, x2, 0.0D0, x4)
      t616 = bbggh61J2(s, XB1, XB2, z, lh, wd, t23, x2, 0.0D0, x4)
      t627 = -t22 * t612 * t30 / 0.32D2 + (0.90D2 * t22 * (-t616 + t47 *
     # t612) + 0.180D3 * t53 * t612) * t27 * t29 / 0.2880D4
      t628 = FJET(XB1, XB2, s, 0.0D0, -t14, t3, t8, -t20, t627)
      t630 = FJET(XB1, XB2, s, 0.0D0, t65, 0.0D0, -t64, 0.0D0, t165)
      t632 = FJET(XB1, XB2, s, t3, -t421, 0.0D0, 0.0D0, 0.0D0, t470)
      t634 = FJET(XB1, XB2, s, 0.0D0, -t290, 0.0D0, t292, 0.0D0, t342)
      t636 = bbggh61J1(s, XB1, XB2, z, lh, wd, t23, 0.0D0, 0.0D0, x4)
      t638 = bbggh61J2(s, XB1, XB2, z, lh, wd, t23, 0.0D0, 0.0D0, x4)
      t643 = 0.180D3 * t53 * t636
      t673 = -(0.90D2 * t22 * (t426 * t636 - t638) + t643) * t26 * t29 /
     # 0.2880D4 + t22 * t636 * t30 / 0.32D2 + (0.90D2 * t22 * (t638 - t4
     #43 * t636) - t643) * t27 * t29 / 0.2880D4 - (-0.180D3 * t53 * (-t6
     #38 + t454 * t636) + 0.90D2 * t22 * (t454 * t638 - t460 * t636 / 0.
     #2D1) - t120 * t636) * t29 / 0.2880D4
      t674 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t421, 0.0D0, t673)
      t676 = FJET(XB1, XB2, s, 0.0D0, t292, 0.0D0, -t290, 0.0D0, t382)
      t679 = x3 * t63
      t681 = Sqrt(x2 * t4 * t679)
      t683 = 0.2D1 * t499 * t681
      t685 = t2 * (0.1D1 - x2 - x3 + t496 + t683)
      t687 = t2 * (-x3 + t496 - x2 + t683)
      t688 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t695 = log(0.4D1 * t247 * t679 * t4)
      t697 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t707 = -t22 * t688 * t30 / 0.32D2 - (0.90D2 * t22 * (-t695 * t688 
     #+ t697) - 0.180D3 * t53 * t688) * t26 * t27 / 0.5760D4
      t708 = FJET(XB1, XB2, s, t685, 0.0D0, -t687, 0.0D0, 0.0D0, t707)
      t710 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t421, t3, 0.0D0, t673)
      t712 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t716 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t727 = -t22 * t712 * t30 / 0.32D2 - (0.90D2 * t22 * (t716 - t695 *
     # t712) - 0.180D3 * t53 * t712) * t26 * t27 / 0.5760D4
      t728 = FJET(XB1, XB2, s, 0.0D0, -t687, 0.0D0, t685, 0.0D0, t727)
      t730 = bbggh61J1(s, XB1, XB2, z, lh, wd, t23, x2, x3, x4)
      t733 = t22 * t730 * t30 / 0.32D2
      t734 = FJET(XB1, XB2, s, t514, t390, -t510, -t387, -t20, t733)
      t739 = FJET(XB1, XB2, s, t390, t514, -t387, -t510, -t20, t733)
      t744 = FJET(XB1, XB2, s, 0.0D0, t685, 0.0D0, -t687, 0.0D0, t707)
      t746 = FJET(XB1, XB2, s, -t14, 0.0D0, t8, t3, -t20, t627)
      t748 = FJET(XB1, XB2, s, -t687, 0.0D0, t685, 0.0D0, 0.0D0, t727)
      t750 = t608 * t287 + t610 * t599 + t628 * t627 + t630 * t165 + t63
     #2 * t470 + t634 * t342 + t674 * t673 + t676 * t382 + t708 * t707 +
     # t710 * t673 + t728 * t727 + t734 * t22 * t730 * t30 / 0.32D2 + t7
     #39 * t22 * t730 * t30 / 0.32D2 + t744 * t707 + t746 * t627 + t748 
     #* t727
      bbggh6n1e0 = t607 + t750

      end function



      doubleprecision function bbggh6n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = t2 * x1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = -t3
      t9 = bbggh61J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.0D0, x4)
      t10 = x1 ** 2
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t19 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t21 = t3 ** 2
      t25 = log(0.4D1 * t14 * t16 * t19 * t21)
      t26 = bbggh61J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.0D0, x4)
      t31 = lh * t7
      t35 = 0.1D1 / x1
      t38 = t7 * t26
      t39 = 0.1D1 / x3
      t40 = t39 * t35
      t43 = 0.1D1 / x2
      t44 = t43 * t35
      t47 = -(0.90D2 * t7 * (-t9 + t25 * t26) + 0.180D3 * t31 * t26) * t
     #35 / 0.2880D4 + t38 * t40 / 0.32D2 + t38 * t44 / 0.32D2
      t48 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t47)
      t51 = x2 * s * t1
      t52 = -0.1D1 + x2
      t53 = t52 * s
      t54 = t53 * t1
      t55 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t56 = t7 * t55
      t57 = t39 * t43
      t60 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t61 = x2 * t13
      t65 = log(-0.4D1 * t61 * t16 * t52)
      t77 = t56 * t57 / 0.64D2 + (0.90D2 * t7 * (t60 - t65 * t55) - 0.18
     #0D3 * t31 * t55) * t43 / 0.5760D4 + t56 * t44 / 0.32D2
      t78 = FJET(XB1, XB2, s, t51, 0.0D0, -t54, 0.0D0, 0.0D0, t77)
      t80 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t83 = log(0.4D1 * t14 * t16)
      t84 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t90 = 0.180D3 * t31 * t84
      t94 = t7 * t84
      t103 = log(0.4D1 * t61 * t16)
      t114 = log(0.4D1 * t16 * t13)
      t117 = (-0.180D3 * lh - 0.90D2 * t114) * t7
      t122 = lh ** 2
      t124 = 0.3141592653589793D1 ** 2
      t126 = t114 ** 2
      t129 = (0.180D3 * t114 * lh + 0.180D3 * t122 - 0.30D2 * t124 + 0.4
     #5D2 * t126) * t7
      t132 = x3 * t13
      t135 = log(0.4D1 * t132 * t16)
      t143 = -(0.90D2 * t7 * (t80 - t83 * t84) - t90) * t35 / 0.2880D4 -
     # t94 * t40 / 0.32D2 - t94 * t44 / 0.32D2 - t94 * t57 / 0.64D2 + (0
     #.90D2 * t7 * (-t80 + t103 * t84) + t90) * t43 / 0.5760D4 - t117 * 
     #t80 / 0.5760D4 - t129 * t84 / 0.5760D4 + (0.90D2 * t7 * (-t80 + t1
     #35 * t84) + t90) * t39 / 0.5760D4
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t143)
      t146 = bbggh62J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.0D0, x4)
      t147 = bbggh62J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.0D0, x4)
      t157 = t7 * t147
      t162 = -(0.90D2 * t7 * (-t146 + t25 * t147) + 0.180D3 * t31 * t147
     #) * t35 / 0.2880D4 + t157 * t40 / 0.32D2 + t157 * t44 / 0.32D2
      t163 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t162)
      t165 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t162)
      t167 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t47)
      t169 = t2 * x3
      t170 = -0.1D1 + x3
      t171 = t2 * t170
      t172 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t176 = log(-0.4D1 * t132 * t16 * t170)
      t177 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t187 = t7 * t177
      t192 = (0.90D2 * t7 * (t172 - t176 * t177) - 0.180D3 * t31 * t177)
     # * t39 / 0.5760D4 + t187 * t40 / 0.32D2 + t187 * t57 / 0.64D2
      t193 = FJET(XB1, XB2, s, t169, 0.0D0, -t171, 0.0D0, 0.0D0, t192)
      t197 = t2 * t3 * x2 * t19
      t198 = t1 * t3
      t199 = t53 * t198
      t200 = t1 ** 2
      t205 = s * t200 * x2 * x1 * t3 * t19
      t206 = bbggh61J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.0D0, x4)
      t209 = t7 * t206 * t44 / 0.32D2
      t210 = FJET(XB1, XB2, s, -t197, 0.0D0, t199, t5, -t205, -t209)
      t213 = t206 * t43 * t35
      t217 = 0.2D1 * x2 * x3
      t218 = cos(t11)
      t222 = Sqrt(x2 * t52 * x3 * t170)
      t224 = 0.2D1 * t218 * t222
      t226 = t2 * (0.1D1 - x2 - x3 + t217 + t224)
      t228 = t2 * (-x3 + t217 - x2 + t224)
      t229 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t232 = t7 * t229 * t57 / 0.64D2
      t233 = FJET(XB1, XB2, s, 0.0D0, t226, 0.0D0, -t228, 0.0D0, -t232)
      t236 = t229 * t39 * t43
      t239 = t170 * s
      t240 = t239 * t198
      t242 = t239 * t1 * x1
      t244 = t2 * t3 * x3
      t246 = t2 * x1 * x3
      t247 = bbggh62J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, x3, x4)
      t250 = t7 * t247 * t40 / 0.32D2
      t251 = FJET(XB1, XB2, s, t240, -t242, -t244, t246, 0.0D0, -t250)
      t254 = t247 * t39 * t35
      t257 = bbggh62J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.0D0, x4)
      t260 = t7 * t257 * t44 / 0.32D2
      t261 = FJET(XB1, XB2, s, t199, t5, -t197, 0.0D0, -t205, -t260)
      t264 = t257 * t43 * t35
      t267 = bbggh61J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, x3, x4)
      t270 = t7 * t267 * t40 / 0.32D2
      t271 = FJET(XB1, XB2, s, t246, -t244, -t242, t240, 0.0D0, -t270)
      t274 = t267 * t39 * t35
      t277 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t280 = t7 * t277 * t57 / 0.64D2
      t281 = FJET(XB1, XB2, s, 0.0D0, -t228, 0.0D0, t226, 0.0D0, -t280)
      t284 = t277 * t39 * t43
      t287 = FJET(XB1, XB2, s, -t242, t240, t246, -t244, 0.0D0, -t250)
      t291 = t48 * t47 + t78 * t77 + t144 * t143 + t163 * t162 + t165 * 
     #t162 + t167 * t47 + t193 * t192 - t210 * t7 * t213 / 0.32D2 - t233
     # * t7 * t236 / 0.64D2 - t251 * t7 * t254 / 0.32D2 - t261 * t7 * t2
     #64 / 0.32D2 - t271 * t7 * t274 / 0.32D2 - t281 * t7 * t284 / 0.64D
     #2 - t287 * t7 * t254 / 0.32D2
      t292 = FJET(XB1, XB2, s, -t244, t246, t240, -t242, 0.0D0, -t270)
      t296 = FJET(XB1, XB2, s, -t228, 0.0D0, t226, 0.0D0, 0.0D0, -t280)
      t300 = FJET(XB1, XB2, s, 0.0D0, -t197, t5, t199, -t205, -t209)
      t304 = FJET(XB1, XB2, s, t5, t199, 0.0D0, -t197, -t205, -t260)
      t308 = FJET(XB1, XB2, s, t226, 0.0D0, -t228, 0.0D0, 0.0D0, -t232)
      t312 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t313 = t7 * t312
      t319 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t328 = t313 * t40 / 0.32D2 + t313 * t57 / 0.64D2 + (0.90D2 * t7 * 
     #(-t176 * t312 + t319) - 0.180D3 * t31 * t312) * t39 / 0.5760D4
      t329 = FJET(XB1, XB2, s, 0.0D0, -t171, 0.0D0, t169, 0.0D0, t328)
      t331 = FJET(XB1, XB2, s, 0.0D0, t169, 0.0D0, -t171, 0.0D0, t192)
      t333 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t334 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t340 = 0.180D3 * t31 * t334
      t344 = t7 * t334
      t369 = -(0.90D2 * t7 * (t333 - t83 * t334) - t340) * t35 / 0.2880D
     #4 - t344 * t40 / 0.32D2 - t344 * t44 / 0.32D2 - t129 * t334 / 0.57
     #60D4 + (0.90D2 * t7 * (-t333 + t135 * t334) + t340) * t39 / 0.5760
     #D4 - t344 * t57 / 0.64D2 + (0.90D2 * t7 * (-t333 + t103 * t334) + 
     #t340) * t43 / 0.5760D4 - t117 * t333 / 0.5760D4
      t370 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t369)
      t372 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t369)
      t374 = FJET(XB1, XB2, s, -t171, 0.0D0, t169, 0.0D0, 0.0D0, t328)
      t376 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t377 = t7 * t376
      t383 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t392 = t377 * t44 / 0.32D2 + t377 * t57 / 0.64D2 + (0.90D2 * t7 * 
     #(-t65 * t376 + t383) - 0.180D3 * t31 * t376) * t43 / 0.5760D4
      t393 = FJET(XB1, XB2, s, 0.0D0, -t54, 0.0D0, t51, 0.0D0, t392)
      t395 = FJET(XB1, XB2, s, -t54, 0.0D0, t51, 0.0D0, 0.0D0, t392)
      t397 = FJET(XB1, XB2, s, 0.0D0, t51, 0.0D0, -t54, 0.0D0, t77)
      t399 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t143)
      t401 = -t292 * t7 * t274 / 0.32D2 - t296 * t7 * t284 / 0.64D2 - t3
     #00 * t7 * t213 / 0.32D2 - t304 * t7 * t264 / 0.32D2 - t308 * t7 * 
     #t236 / 0.64D2 + t329 * t328 + t331 * t192 + t370 * t369 + t372 * t
     #369 + t374 * t328 + t393 * t392 + t395 * t392 + t397 * t77 + t399 
     #* t143
      bbggh6n1em1 = t291 + t401

      end function



      doubleprecision function bbggh6n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t17 = z ** 2
      t20 = Sin(x4 * 0.3141592653589793D1)
      t21 = t20 ** 2
      t24 = log(0.4D1 / t17 * t21)
      t27 = (-0.180D3 * lh - 0.90D2 * t24) * t4
      t30 = 0.1D1 / x3
      t33 = -t6 * t7 / 0.32D2 - t6 * t10 / 0.64D2 - t4 * t13 / 0.64D2 - 
     #t27 * t5 / 0.5760D4 - t6 * t30 / 0.64D2
      t34 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t33)
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t33)
      t38 = t2 * x1
      t39 = -0.1D1 + x1
      t40 = t2 * t39
      t41 = -t39
      t42 = bbggh61J1(s, XB1, XB2, z, lh, wd, t41, 0.0D0, 0.0D0, x4)
      t45 = t4 * t42 * t7 / 0.32D2
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t38, -t40, 0.0D0, t45)
      t48 = t42 * t7
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t40, t38, 0.0D0, t45)
      t55 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t56 = t4 * t55
      t65 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t68 = -t56 * t7 / 0.32D2 - t27 * t55 / 0.5760D4 - t56 * t30 / 0.64
     #D2 - t56 * t10 / 0.64D2 - t4 * t65 / 0.64D2
      t69 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t68)
      t71 = t2 * x3
      t73 = t2 * (-0.1D1 + x3)
      t74 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t77 = t4 * t74 * t30 / 0.64D2
      t78 = FJET(XB1, XB2, s, 0.0D0, t71, 0.0D0, -t73, 0.0D0, t77)
      t80 = t74 * t30
      t84 = x2 * s * t1
      t87 = (-0.1D1 + x2) * s * t1
      t88 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t91 = t4 * t88 * t10 / 0.64D2
      t92 = FJET(XB1, XB2, s, 0.0D0, t84, 0.0D0, -t87, 0.0D0, t91)
      t94 = t88 * t10
      t97 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, x3, x4)
      t100 = t4 * t97 * t30 / 0.64D2
      t101 = FJET(XB1, XB2, s, 0.0D0, -t73, 0.0D0, t71, 0.0D0, t100)
      t103 = t97 * t30
      t106 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t109 = t4 * t106 * t10 / 0.64D2
      t110 = FJET(XB1, XB2, s, 0.0D0, -t87, 0.0D0, t84, 0.0D0, t109)
      t112 = t106 * t10
      t115 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t68)
      t117 = bbggh62J1(s, XB1, XB2, z, lh, wd, t41, 0.0D0, 0.0D0, x4)
      t120 = t4 * t117 * t7 / 0.32D2
      t121 = FJET(XB1, XB2, s, t38, -t40, 0.0D0, 0.0D0, 0.0D0, t120)
      t123 = t117 * t7
      t126 = FJET(XB1, XB2, s, t71, 0.0D0, -t73, 0.0D0, 0.0D0, t77)
      t130 = FJET(XB1, XB2, s, t84, 0.0D0, -t87, 0.0D0, 0.0D0, t91)
      t134 = FJET(XB1, XB2, s, -t40, t38, 0.0D0, 0.0D0, 0.0D0, t120)
      t138 = FJET(XB1, XB2, s, -t87, 0.0D0, t84, 0.0D0, 0.0D0, t109)
      t142 = FJET(XB1, XB2, s, -t73, 0.0D0, t71, 0.0D0, 0.0D0, t100)
      bbggh6n1em2 = t34 * t33 + t36 * t33 + t46 * t4 * t48 / 0.32D2 + t5
     #1 * t4 * t48 / 0.32D2 + t69 * t68 + t78 * t4 * t80 / 0.64D2 + t92 
     #* t4 * t94 / 0.64D2 + t101 * t4 * t103 / 0.64D2 + t110 * t4 * t112
     # / 0.64D2 + t115 * t68 + t121 * t4 * t123 / 0.32D2 + t126 * t4 * t
     #80 / 0.64D2 + t130 * t4 * t94 / 0.64D2 + t134 * t4 * t123 / 0.32D2
     # + t138 * t4 * t112 / 0.64D2 + t142 * t4 * t103 / 0.64D2

      end function



      doubleprecision function bbggh6n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t14 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.0D0, x4)
      t16 = t4 * t14 / 0.64D2
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t16)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t16)
      bbggh6n1em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2 - t1
     #7 * t4 * t14 / 0.64D2 - t20 * t4 * t14 / 0.64D2

      end function



      doubleprecision function bbggh6n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      bbggh6n1em4 = 0.0D0

      end function


      doubleprecision function bbggh6n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = lh * t4
      t6 = x1 ** 2
      t7 = x4 * 0.3141592653589793D1
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t14 = t10 * t13
      t16 = log(0.4D1 * t14)
      t17 = t16 ** 2
      t18 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t21 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t26 = lh ** 2
      t28 = 0.3141592653589793D1 ** 2
      t30 = 0.180D3 * t26 - 0.30D2 * t28
      t31 = t30 * t4
      t35 = t17 * t16
      t44 = 0.120D3 * t26 * lh
      t46 = 0.60D2 * lh * t28
      t47 = -0.2884936567583026D3 - t44 + t46
      t48 = t47 * t4
      t49 = t48 * t18
      t51 = 0.1D1 / x1
      t54 = x3 * t6
      t55 = t13 * t9
      t58 = log(0.4D1 * t54 * t55)
      t63 = t58 ** 2
      t72 = 0.1D1 / x3
      t76 = x2 * x3
      t79 = log(0.4D1 * t76 * t14)
      t81 = t76 * t6
      t82 = -0.1D1 + x2
      t83 = t55 * t82
      t86 = log(-0.4D1 * t81 * t83)
      t87 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t89 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t93 = -t18 + t87
      t98 = 0.1D1 / x2
      t99 = t98 * t51
      t102 = x2 * t6
      t105 = log(0.4D1 * t102 * t55)
      t109 = log(-0.4D1 * t102 * t83)
      t114 = t105 ** 2
      t119 = t109 ** 2
      t125 = -t93
      t132 = log(0.4D1 * t55)
      t134 = t132 ** 2
      t137 = t134 * t132
      t140 = (-t132 * t30 - 0.90D2 * t134 * lh - 0.2884936567583026D3 - 
     #t44 + t46 - 0.15D2 * t137) * t4
      t143 = t134 ** 2
      t147 = t28 ** 2
      t148 = t26 ** 2
      t157 = (0.15D2 / 0.4D1 * t143 - t132 * t47 + 0.5769873135166051D3 
     #* lh + t147 + 0.60D2 * t148 - 0.60D2 * t26 * t28 + t134 * t30 / 0.
     #2D1 + 0.30D2 * t137 * lh) * t4
      t163 = log(0.4D1 * x3 * t13 * t9)
      t164 = t163 ** 2
      t174 = t164 * t163
      t185 = x2 * t13
      t186 = t9 * t82
      t189 = log(-0.4D1 * t185 * t186)
      t191 = t189 ** 2
      t196 = log(0.4D1 * t185 * t9)
      t198 = t196 ** 2
      t210 = t191 * t189
      t215 = t198 * t196
      t227 = log(-0.4D1 * t76 * t83)
      t231 = log(0.4D1 * t76 * t55)
      t236 = t227 ** 2
      t241 = t231 ** 2
      t252 = -(-0.180D3 * t5 * (t17 * t18 / 0.2D1 - t16 * t21) + t31 * (
     #t21 - t16 * t18) + 0.90D2 * t4 * (-t35 * t18 / 0.6D1 + t17 * t21 /
     # 0.2D1) + t49) * t51 / 0.2880D4 + (-0.180D3 * t5 * (t58 * t18 - t2
     #1) + 0.90D2 * t4 * (-t63 * t18 / 0.2D1 + t58 * t21) - t31 * t18) *
     # t72 * t51 / 0.2880D4 + (0.90D2 * t4 * (t79 * t18 - t86 * t87 + t8
     #9 - t21) - 0.180D3 * t5 * t93) * t72 * t99 / 0.2880D4 - (-0.180D3 
     #* t5 * (-t105 * t18 - t89 + t109 * t87 + t21) + 0.90D2 * t4 * (t11
     #4 * t18 / 0.2D1 - t105 * t21 + t109 * t89 - t119 * t87 / 0.2D1) + 
     #t31 * t125) * t98 * t51 / 0.2880D4 - t140 * t21 / 0.5760D4 - t157 
     #* t18 / 0.5760D4 - (-0.180D3 * t5 * (t164 * t18 / 0.2D1 - t163 * t
     #21) + t31 * (t21 - t163 * t18) + 0.90D2 * t4 * (-t174 * t18 / 0.6D
     #1 + t164 * t21 / 0.2D1) + t49) * t72 / 0.5760D4 - (-0.180D3 * t5 *
     # (t189 * t89 - t191 * t87 / 0.2D1 - t196 * t21 + t198 * t18 / 0.2D
     #1) + t31 * (t21 - t196 * t18 - t89 + t189 * t87) + 0.90D2 * t4 * (
     #-t191 * t89 / 0.2D1 + t210 * t87 / 0.6D1 + t198 * t21 / 0.2D1 - t2
     #15 * t18 / 0.6D1) + t48 * t125) * t98 / 0.5760D4 + (-0.180D3 * t5 
     #* (-t227 * t87 - t21 + t231 * t18 + t89) + 0.90D2 * t4 * (t236 * t
     #87 / 0.2D1 - t227 * t89 + t231 * t21 - t241 * t18 / 0.2D1) + t31 *
     # t93) * t72 * t98 / 0.5760D4
      t253 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t252)
      t255 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t252)
      t257 = t2 * x1
      t258 = -0.1D1 + x1
      t259 = t2 * t258
      t260 = 0.1D1 / t11
      t261 = x1 * z
      t262 = -z - x1 + t261
      t263 = 0.1D1 / t262
      t264 = t260 * t263
      t265 = t258 ** 2
      t266 = t264 * t265
      t269 = log(-0.4D1 * t10 * t266)
      t270 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t272 = t269 ** 2
      t273 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t284 = t272 * t269
      t293 = t54 * t9
      t296 = log(-0.4D1 * t293 * t266)
      t302 = t296 ** 2
      t308 = t31 * t273
      t317 = log(-0.4D1 * t81 * t9 * t260 * t265 * t263)
      t327 = t102 * t9
      t330 = log(-0.4D1 * t327 * t266)
      t335 = t330 ** 2
      t346 = -(-0.180D3 * t5 * (t269 * t270 - t272 * t273 / 0.2D1) + t31
     # * (t269 * t273 - t270) + 0.90D2 * t4 * (-t272 * t270 / 0.2D1 + t2
     #84 * t273 / 0.6D1) - t48 * t273) * t51 / 0.2880D4 + (-0.180D3 * t5
     # * (t270 - t296 * t273) + 0.90D2 * t4 * (-t296 * t270 + t302 * t27
     #3 / 0.2D1) + t308) * t72 * t51 / 0.2880D4 + (0.90D2 * t4 * (-t317 
     #* t273 + t270) - 0.180D3 * t5 * t273) * t72 * t99 / 0.2880D4 - (-0
     #.180D3 * t5 * (-t270 + t330 * t273) + 0.90D2 * t4 * (-t335 * t273 
     #/ 0.2D1 + t330 * t270) - t308) * t98 * t51 / 0.2880D4
      t347 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t257, -t259, 0.0D0, t346)
      t349 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t259, t257, 0.0D0, t346)
      t351 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t353 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t369 = t48 * t353
      t388 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t390 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t395 = t388 - t353
      t416 = -t395
      t492 = -(-0.180D3 * t5 * (-t16 * t351 + t17 * t353 / 0.2D1) + t31 
     #* (-t16 * t353 + t351) + 0.90D2 * t4 * (t17 * t351 / 0.2D1 - t35 *
     # t353 / 0.6D1) + t369) * t51 / 0.2880D4 + (-0.180D3 * t5 * (t58 * 
     #t353 - t351) + 0.90D2 * t4 * (t58 * t351 - t63 * t353 / 0.2D1) - t
     #31 * t353) * t72 * t51 / 0.2880D4 + (0.90D2 * t4 * (-t351 - t86 * 
     #t388 + t390 + t79 * t353) - 0.180D3 * t5 * t395) * t72 * t99 / 0.2
     #880D4 - (-0.180D3 * t5 * (t351 - t105 * t353 + t109 * t388 - t390)
     # + 0.90D2 * t4 * (-t105 * t351 - t119 * t388 / 0.2D1 + t114 * t353
     # / 0.2D1 + t109 * t390) + t31 * t416) * t98 * t51 / 0.2880D4 - (-0
     #.180D3 * t5 * (-t163 * t351 + t164 * t353 / 0.2D1) + t31 * (-t163 
     #* t353 + t351) + 0.90D2 * t4 * (t164 * t351 / 0.2D1 - t174 * t353 
     #/ 0.6D1) + t369) * t72 / 0.5760D4 - (-0.180D3 * t5 * (t189 * t390 
     #- t191 * t388 / 0.2D1 - t196 * t351 + t198 * t353 / 0.2D1) + t31 *
     # (-t196 * t353 - t390 + t351 + t189 * t388) + 0.90D2 * t4 * (-t191
     # * t390 / 0.2D1 + t210 * t388 / 0.6D1 + t198 * t351 / 0.2D1 - t215
     # * t353 / 0.6D1) + t48 * t416) * t98 / 0.5760D4 + (-0.180D3 * t5 *
     # (t231 * t353 + t390 - t351 - t227 * t388) + 0.90D2 * t4 * (-t227 
     #* t390 + t236 * t388 / 0.2D1 - t241 * t353 / 0.2D1 + t231 * t351) 
     #+ t31 * t395) * t72 * t98 / 0.5760D4 - t140 * t351 / 0.5760D4 - t1
     #57 * t353 / 0.5760D4
      t493 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t492)
      t495 = t2 * x3
      t496 = -0.1D1 + x3
      t497 = t2 * t496
      t498 = t55 * t496
      t501 = log(-0.4D1 * t54 * t498)
      t502 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t504 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t508 = t501 ** 2
      t522 = log(-0.4D1 * t81 * t498)
      t528 = log(0.4D1 * t81 * t55 * t82 * t496)
      t529 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t531 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t535 = t502 - t529
      t546 = log(0.4D1 * t76 * t13 * t186 * t496)
      t550 = log(-0.4D1 * t76 * t498)
      t555 = t550 ** 2
      t560 = t546 ** 2
      t574 = log(-0.4D1 * t55 * x3 * t496)
      t576 = t574 ** 2
      t587 = t576 * t574
      t597 = (-0.180D3 * t5 * (-t501 * t502 + t504) + 0.90D2 * t4 * (t50
     #8 * t502 / 0.2D1 - t501 * t504) + t31 * t502) * t72 * t51 / 0.2880
     #D4 + (0.90D2 * t4 * (-t522 * t502 + t504 + t528 * t529 - t531) - 0
     #.180D3 * t5 * t535) * t72 * t99 / 0.2880D4 + (-0.180D3 * t5 * (t50
     #4 - t531 + t546 * t529 - t550 * t502) + 0.90D2 * t4 * (t555 * t502
     # / 0.2D1 + t546 * t531 - t550 * t504 - t560 * t529 / 0.2D1) + t31 
     #* t535) * t72 * t98 / 0.5760D4 - (-0.180D3 * t5 * (t574 * t504 - t
     #576 * t502 / 0.2D1) + t31 * (-t504 + t574 * t502) + 0.90D2 * t4 * 
     #(-t576 * t504 / 0.2D1 + t587 * t502 / 0.6D1) - t48 * t502) * t72 /
     # 0.5760D4
      t598 = FJET(XB1, XB2, s, 0.0D0, t495, 0.0D0, -t497, 0.0D0, t597)
      t600 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t601 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t617 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t618 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t624 = t601 - t618
      t670 = (-0.180D3 * t5 * (t600 - t501 * t601) + 0.90D2 * t4 * (-t50
     #1 * t600 + t508 * t601 / 0.2D1) + t31 * t601) * t72 * t51 / 0.2880
     #D4 + (0.90D2 * t4 * (-t617 + t600 + t528 * t618 - t522 * t601) - 0
     #.180D3 * t5 * t624) * t72 * t99 / 0.2880D4 + (-0.180D3 * t5 * (t60
     #0 + t546 * t618 - t617 - t550 * t601) + 0.90D2 * t4 * (t546 * t617
     # - t550 * t600 - t560 * t618 / 0.2D1 + t555 * t601 / 0.2D1) + t31 
     #* t624) * t72 * t98 / 0.5760D4 - (-0.180D3 * t5 * (t574 * t600 - t
     #576 * t601 / 0.2D1) + t31 * (-t600 + t574 * t601) + 0.90D2 * t4 * 
     #(-t576 * t600 / 0.2D1 + t587 * t601 / 0.6D1) - t48 * t601) * t72 /
     # 0.5760D4
      t671 = FJET(XB1, XB2, s, 0.0D0, -t497, 0.0D0, t495, 0.0D0, t670)
      t673 = x2 * x1
      t675 = t2 * t673 * t263
      t677 = t1 * x1
      t678 = t82 * s * t677
      t679 = t1 ** 2
      t684 = s * t679 * x2 * x1 * t258 * t263
      t685 = t76 * t10
      t686 = t265 * t82
      t687 = t264 * t686
      t690 = log(0.4D1 * t685 * t687)
      t691 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t693 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t704 = log(0.4D1 * t327 * t687)
      t709 = t704 ** 2
      t721 = (0.90D2 * t4 * (t690 * t691 - t693) + 0.180D3 * t5 * t691) 
     #* t72 * t99 / 0.2880D4 - (-0.180D3 * t5 * (t693 - t704 * t691) + 0
     #.90D2 * t4 * (t709 * t691 / 0.2D1 - t704 * t693) + t31 * t691) * t
     #98 * t51 / 0.2880D4
      t722 = FJET(XB1, XB2, s, 0.0D0, -t675, -t259, -t678, t684, t721)
      t724 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t492)
      t726 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t728 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t757 = t31 * t728
      t784 = -(-0.180D3 * t5 * (t269 * t726 - t272 * t728 / 0.2D1) + t31
     # * (t269 * t728 - t726) + 0.90D2 * t4 * (-t272 * t726 / 0.2D1 + t2
     #84 * t728 / 0.6D1) - t48 * t728) * t51 / 0.2880D4 + (-0.180D3 * t5
     # * (t726 - t296 * t728) + 0.90D2 * t4 * (t302 * t728 / 0.2D1 - t29
     #6 * t726) + t757) * t72 * t51 / 0.2880D4 + (0.90D2 * t4 * (t726 - 
     #t317 * t728) - 0.180D3 * t5 * t728) * t72 * t99 / 0.2880D4 - (-0.1
     #80D3 * t5 * (-t726 + t330 * t728) + 0.90D2 * t4 * (-t335 * t728 / 
     #0.2D1 + t330 * t726) - t757) * t98 * t51 / 0.2880D4
      t785 = FJET(XB1, XB2, s, t257, -t259, 0.0D0, 0.0D0, 0.0D0, t784)
      t787 = FJET(XB1, XB2, s, t495, 0.0D0, -t497, 0.0D0, 0.0D0, t597)
      t789 = x1 * x3
      t790 = t2 * t789
      t792 = t2 * t258 * x3
      t793 = t496 * s
      t794 = t793 * t677
      t796 = t793 * t1 * t258
      t798 = t264 * t265 * t496
      t801 = log(0.4D1 * t293 * t798)
      t802 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t804 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t809 = t801 ** 2
      t821 = log(0.4D1 * t685 * t798)
      t832 = (-0.180D3 * t5 * (t801 * t802 - t804) + 0.90D2 * t4 * (t801
     # * t804 - t809 * t802 / 0.2D1) - t31 * t802) * t72 * t51 / 0.2880D
     #4 + (0.90D2 * t4 * (-t804 + t821 * t802) + 0.180D3 * t5 * t802) * 
     #t72 * t99 / 0.2880D4
      t833 = FJET(XB1, XB2, s, t790, -t792, -t794, t796, 0.0D0, t832)
      t835 = t253 * t252 + t255 * t252 + t347 * t346 + t349 * t346 + t49
     #3 * t492 + t598 * t597 + t671 * t670 + t722 * t721 + t724 * t492 +
     # t785 * t784 + t787 * t597 + t833 * t832
      t836 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t838 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t862 = (-0.180D3 * t5 * (t801 * t836 - t838) + 0.90D2 * t4 * (t801
     # * t838 - t809 * t836 / 0.2D1) - t31 * t836) * t72 * t51 / 0.2880D
     #4 + (0.90D2 * t4 * (t821 * t836 - t838) + 0.180D3 * t5 * t836) * t
     #72 * t99 / 0.2880D4
      t863 = FJET(XB1, XB2, s, t796, -t794, -t792, t790, 0.0D0, t862)
      t867 = x3 * z
      t868 = t789 * z
      t869 = t76 * z
      t870 = t76 * x1
      t871 = t76 * t261
      t872 = cos(t7)
      t877 = Sqrt(-x3 * t82 * t262 * x2 * t496)
      t879 = 0.2D1 * t872 * t877
      t880 = z + x1 - t261 - x2 * z - t673 + t673 * z - t867 - t789 + t8
     #68 + t869 + t870 - t871 + t76 + t879
      t883 = t2 * x1 * t880 * t263
      t887 = t2 * x1 * (-t867 - t789 + t868 + t869 + t870 - t871 - x2 + 
     #t76 + t879) * t263
      t892 = log(-0.4D1 * t685 * t264 * t686 * t496)
      t893 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t895 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t901 = 0.90D2 * t4 * (-t892 * t893 + t895) - 0.180D3 * t5 * t893
      t904 = t901 * t72 * t99 / 0.2880D4
      t905 = FJET(XB1, XB2, s, t796, -t883, -t792, t887, t684, t904)
      t908 = t72 * t98 * t51
      t911 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t913 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t919 = 0.90D2 * t4 * (-t892 * t911 + t913) - 0.180D3 * t5 * t911
      t922 = t919 * t72 * t99 / 0.2880D4
      t923 = FJET(XB1, XB2, s, t887, -t792, -t883, t796, t684, t922)
      t927 = FJET(XB1, XB2, s, -t259, t257, 0.0D0, 0.0D0, 0.0D0, t784)
      t929 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t930 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t955 = (0.90D2 * t4 * (-t929 + t690 * t930) + 0.180D3 * t5 * t930)
     # * t72 * t99 / 0.2880D4 - (-0.180D3 * t5 * (-t704 * t930 + t929) +
     # 0.90D2 * t4 * (-t704 * t929 + t709 * t930 / 0.2D1) + t31 * t930) 
     #* t98 * t51 / 0.2880D4
      t956 = FJET(XB1, XB2, s, -t259, -t678, 0.0D0, -t675, t684, t955)
      t958 = FJET(XB1, XB2, s, -t497, 0.0D0, t495, 0.0D0, 0.0D0, t670)
      t960 = FJET(XB1, XB2, s, -t792, t790, t796, -t794, 0.0D0, t832)
      t962 = FJET(XB1, XB2, s, -t792, t887, t796, -t883, t684, t922)
      t966 = FJET(XB1, XB2, s, -t678, -t259, -t675, 0.0D0, t684, t955)
      t968 = FJET(XB1, XB2, s, -t794, t796, t790, -t792, 0.0D0, t862)
      t970 = FJET(XB1, XB2, s, -t675, 0.0D0, -t678, -t259, t684, t721)
      t972 = FJET(XB1, XB2, s, -t883, t796, t887, -t792, t684, t904)
      t976 = t863 * t862 + t905 * t901 * t908 / 0.2880D4 + t923 * t919 *
     # t908 / 0.2880D4 + t927 * t784 + t956 * t955 + t958 * t670 + t960 
     #* t832 + t962 * t919 * t908 / 0.2880D4 + t966 * t955 + t968 * t862
     # + t970 * t721 + t972 * t901 * t908 / 0.2880D4
      bbggh6n2e1 = t835 + t976

      end function



      doubleprecision function bbggh6n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = x1 ** 2
      t6 = x3 * t5
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t16 = log(0.4D1 * t6 * t13)
      t17 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t19 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t23 = lh * t4
      t27 = 0.1D1 / x3
      t29 = 0.1D1 / x1
      t32 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t33 = -t17 + t32
      t35 = 0.1D1 / x2
      t37 = t27 * t35 * t29
      t40 = x2 * t5
      t43 = log(0.4D1 * t40 * t13)
      t45 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t46 = -0.1D1 + x2
      t47 = t13 * t46
      t50 = log(-0.4D1 * t40 * t47)
      t55 = -t33
      t62 = t5 * t12
      t65 = log(0.4D1 * t62 * t9)
      t70 = t65 ** 2
      t77 = lh ** 2
      t78 = 0.180D3 * t77
      t79 = 0.3141592653589793D1 ** 2
      t80 = 0.30D2 * t79
      t81 = t78 - t80
      t82 = t81 * t4
      t83 = t82 * t17
      t90 = log(0.4D1 * x3 * t9 * t12)
      t95 = t90 ** 2
      t105 = x2 * x3
      t108 = log(-0.4D1 * t105 * t47)
      t112 = log(0.4D1 * t105 * t13)
      t123 = x2 * t9
      t126 = log(0.4D1 * t123 * t12)
      t128 = t12 * t46
      t131 = log(-0.4D1 * t123 * t128)
      t137 = t131 ** 2
      t141 = t126 ** 2
      t152 = log(0.4D1 * t13)
      t155 = t152 ** 2
      t158 = (0.180D3 * t152 * lh + t78 - t80 + 0.45D2 * t155) * t4
      t171 = (-t152 * t81 - 0.90D2 * t155 * lh - 0.2884936567583026D3 - 
     #0.120D3 * t77 * lh + 0.60D2 * lh * t79 - 0.15D2 * t155 * t152) * t
     #4
      t174 = (0.90D2 * t4 * (t17 * t16 - t19) + 0.180D3 * t23 * t17) * t
     #27 * t29 / 0.2880D4 + t4 * t33 * t37 / 0.32D2 - (0.90D2 * t4 * (-t
     #43 * t17 - t45 + t50 * t32 + t19) - 0.180D3 * t23 * t55) * t35 * t
     #29 / 0.2880D4 - (-0.180D3 * t23 * (t19 - t65 * t17) + 0.90D2 * t4 
     #* (t70 * t17 / 0.2D1 - t65 * t19) + t83) * t29 / 0.2880D4 - (-0.18
     #0D3 * t23 * (t19 - t90 * t17) + 0.90D2 * t4 * (t95 * t17 / 0.2D1 -
     # t90 * t19) + t83) * t27 / 0.5760D4 + (0.90D2 * t4 * (-t108 * t32 
     #- t19 + t112 * t17 + t45) - 0.180D3 * t23 * t33) * t27 * t35 / 0.5
     #760D4 - (-0.180D3 * t23 * (t19 - t126 * t17 - t45 + t131 * t32) + 
     #0.90D2 * t4 * (t131 * t45 - t137 * t32 / 0.2D1 - t126 * t19 + t141
     # * t17 / 0.2D1) + t55 * t82) * t35 / 0.5760D4 - t158 * t19 / 0.576
     #0D4 - t171 * t17 / 0.5760D4
      t175 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t174)
      t177 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t174)
      t179 = t2 * x1
      t180 = -0.1D1 + x1
      t181 = t2 * t180
      t182 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t183 = t6 * t12
      t185 = x1 * z
      t186 = -z - x1 + t185
      t187 = 0.1D1 / t186
      t188 = 0.1D1 / t7 * t187
      t189 = t180 ** 2
      t190 = t188 * t189
      t193 = log(-0.4D1 * t183 * t190)
      t194 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t200 = 0.180D3 * t23 * t194
      t208 = t40 * t12
      t211 = log(-0.4D1 * t208 * t190)
      t222 = log(-0.4D1 * t62 * t190)
      t228 = t222 ** 2
      t238 = (0.90D2 * t4 * (t182 - t193 * t194) - t200) * t27 * t29 / 0
     #.2880D4 + t4 * t194 * t37 / 0.32D2 - (0.90D2 * t4 * (-t182 + t211 
     #* t194) + t200) * t35 * t29 / 0.2880D4 - (-0.180D3 * t23 * (t222 *
     # t194 - t182) + 0.90D2 * t4 * (t222 * t182 - t228 * t194 / 0.2D1) 
     #- t82 * t194) * t29 / 0.2880D4
      t239 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t179, -t181, 0.0D0, t238)
      t241 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t181, t179, 0.0D0, t238)
      t243 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t245 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t255 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t256 = t255 - t243
      t262 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t266 = -t256
      t283 = t82 * t243
      t333 = (0.90D2 * t4 * (t16 * t243 - t245) + 0.180D3 * t23 * t243) 
     #* t27 * t29 / 0.2880D4 + t4 * t256 * t37 / 0.32D2 - (0.90D2 * t4 *
     # (t245 - t43 * t243 + t50 * t255 - t262) - 0.180D3 * t23 * t266) *
     # t35 * t29 / 0.2880D4 - (-0.180D3 * t23 * (-t65 * t243 + t245) + 0
     #.90D2 * t4 * (-t65 * t245 + t70 * t243 / 0.2D1) + t283) * t29 / 0.
     #2880D4 - t158 * t245 / 0.5760D4 - t171 * t243 / 0.5760D4 - (-0.180
     #D3 * t23 * (-t90 * t243 + t245) + 0.90D2 * t4 * (-t90 * t245 + t95
     # * t243 / 0.2D1) + t283) * t27 / 0.5760D4 + (0.90D2 * t4 * (t112 *
     # t243 + t262 - t245 - t108 * t255) - 0.180D3 * t23 * t256) * t27 *
     # t35 / 0.5760D4 - (-0.180D3 * t23 * (-t126 * t243 - t262 + t245 + 
     #t131 * t255) + 0.90D2 * t4 * (t131 * t262 - t137 * t255 / 0.2D1 - 
     #t126 * t245 + t141 * t243 / 0.2D1) + t82 * t266) * t35 / 0.5760D4
      t334 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t333)
      t336 = t2 * x3
      t337 = -0.1D1 + x3
      t338 = t2 * t337
      t339 = t13 * t337
      t342 = log(-0.4D1 * t6 * t339)
      t343 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t345 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t355 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t356 = t343 - t355
      t363 = log(-0.4D1 * t13 * x3 * t337)
      t369 = t363 ** 2
      t379 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t384 = log(0.4D1 * t105 * t9 * t128 * t337)
      t388 = log(-0.4D1 * t105 * t339)
      t399 = (0.90D2 * t4 * (-t342 * t343 + t345) - 0.180D3 * t23 * t343
     #) * t27 * t29 / 0.2880D4 + t4 * t356 * t37 / 0.32D2 - (-0.180D3 * 
     #t23 * (-t345 + t363 * t343) + 0.90D2 * t4 * (t363 * t345 - t369 * 
     #t343 / 0.2D1) - t82 * t343) * t27 / 0.5760D4 + (0.90D2 * t4 * (t34
     #5 - t379 + t384 * t355 - t388 * t343) - 0.180D3 * t23 * t356) * t2
     #7 * t35 / 0.5760D4
      t400 = FJET(XB1, XB2, s, 0.0D0, t336, 0.0D0, -t338, 0.0D0, t399)
      t402 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t403 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t414 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t415 = t403 - t414
      t434 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t445 = (0.90D2 * t4 * (t402 - t342 * t403) - 0.180D3 * t23 * t403)
     # * t27 * t29 / 0.2880D4 + t4 * t415 * t37 / 0.32D2 - (-0.180D3 * t
     #23 * (-t402 + t363 * t403) + 0.90D2 * t4 * (t363 * t402 - t369 * t
     #403 / 0.2D1) - t82 * t403) * t27 / 0.5760D4 + (0.90D2 * t4 * (t402
     # + t384 * t414 - t434 - t388 * t403) - 0.180D3 * t23 * t415) * t27
     # * t35 / 0.5760D4
      t446 = FJET(XB1, XB2, s, 0.0D0, -t338, 0.0D0, t336, 0.0D0, t445)
      t448 = x2 * x1
      t450 = t2 * t448 * t187
      t452 = t1 * x1
      t453 = t46 * s * t452
      t454 = t1 ** 2
      t459 = s * t454 * x2 * x1 * t180 * t187
      t460 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t464 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t469 = log(0.4D1 * t208 * t188 * t189 * t46)
      t480 = -t4 * t460 * t37 / 0.32D2 - (0.90D2 * t4 * (t464 - t469 * t
     #460) - 0.180D3 * t23 * t460) * t35 * t29 / 0.2880D4
      t481 = FJET(XB1, XB2, s, 0.0D0, -t450, -t181, -t453, t459, t480)
      t483 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t333)
      t485 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t486 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t492 = 0.180D3 * t23 * t486
      t522 = (0.90D2 * t4 * (t485 - t193 * t486) - t492) * t27 * t29 / 0
     #.2880D4 + t4 * t486 * t37 / 0.32D2 - (0.90D2 * t4 * (-t485 + t211 
     #* t486) + t492) * t35 * t29 / 0.2880D4 - (-0.180D3 * t23 * (t222 *
     # t486 - t485) + 0.90D2 * t4 * (t222 * t485 - t228 * t486 / 0.2D1) 
     #- t82 * t486) * t29 / 0.2880D4
      t523 = FJET(XB1, XB2, s, t179, -t181, 0.0D0, 0.0D0, 0.0D0, t522)
      t525 = FJET(XB1, XB2, s, t336, 0.0D0, -t338, 0.0D0, 0.0D0, t399)
      t527 = x1 * x3
      t528 = t2 * t527
      t530 = t2 * t180 * x3
      t531 = t337 * s
      t532 = t531 * t452
      t534 = t531 * t1 * t180
      t539 = log(0.4D1 * t183 * t188 * t189 * t337)
      t540 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t542 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t555 = (0.90D2 * t4 * (t539 * t540 - t542) + 0.180D3 * t23 * t540)
     # * t27 * t29 / 0.2880D4 - t4 * t540 * t37 / 0.32D2
      t556 = FJET(XB1, XB2, s, t528, -t530, -t532, t534, 0.0D0, t555)
      t558 = t175 * t174 + t177 * t174 + t239 * t238 + t241 * t238 + t33
     #4 * t333 + t400 * t399 + t446 * t445 + t481 * t480 + t483 * t333 +
     # t523 * t522 + t525 * t399 + t556 * t555
      t559 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t561 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t574 = (0.90D2 * t4 * (t539 * t559 - t561) + 0.180D3 * t23 * t559)
     # * t27 * t29 / 0.2880D4 - t4 * t559 * t37 / 0.32D2
      t575 = FJET(XB1, XB2, s, t534, -t532, -t530, t528, 0.0D0, t574)
      t579 = x3 * z
      t580 = t527 * z
      t581 = t105 * z
      t582 = t105 * x1
      t583 = t105 * t185
      t584 = cos(t10)
      t589 = Sqrt(-x3 * t46 * t186 * x2 * t337)
      t591 = 0.2D1 * t584 * t589
      t592 = z + x1 - t185 - x2 * z - t448 + t448 * z - t579 - t527 + t5
     #80 + t581 + t582 - t583 + t105 + t591
      t595 = t2 * x1 * t592 * t187
      t599 = t2 * x1 * (-t579 - t527 + t580 + t581 + t582 - t583 - x2 + 
     #t105 + t591) * t187
      t600 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t603 = t4 * t600 * t37 / 0.32D2
      t604 = FJET(XB1, XB2, s, t534, -t595, -t530, t599, t459, t603)
      t609 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t612 = t4 * t609 * t37 / 0.32D2
      t613 = FJET(XB1, XB2, s, t599, -t530, -t595, t534, t459, t612)
      t618 = FJET(XB1, XB2, s, -t181, t179, 0.0D0, 0.0D0, 0.0D0, t522)
      t620 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t625 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t635 = -t4 * t620 * t37 / 0.32D2 - (0.90D2 * t4 * (-t469 * t620 + 
     #t625) - 0.180D3 * t23 * t620) * t35 * t29 / 0.2880D4
      t636 = FJET(XB1, XB2, s, -t181, -t453, 0.0D0, -t450, t459, t635)
      t638 = FJET(XB1, XB2, s, -t338, 0.0D0, t336, 0.0D0, 0.0D0, t445)
      t640 = FJET(XB1, XB2, s, -t530, t528, t534, -t532, 0.0D0, t555)
      t642 = FJET(XB1, XB2, s, -t530, t599, t534, -t595, t459, t612)
      t647 = FJET(XB1, XB2, s, -t453, -t181, -t450, 0.0D0, t459, t635)
      t649 = FJET(XB1, XB2, s, -t532, t534, t528, -t530, 0.0D0, t574)
      t651 = FJET(XB1, XB2, s, -t450, 0.0D0, -t453, -t181, t459, t480)
      t653 = FJET(XB1, XB2, s, -t595, t534, t599, -t530, t459, t603)
      t658 = t575 * t574 + t604 * t4 * t600 * t37 / 0.32D2 + t613 * t4 *
     # t609 * t37 / 0.32D2 + t618 * t522 + t636 * t635 + t638 * t445 + t
     #640 * t555 + t642 * t4 * t609 * t37 / 0.32D2 + t647 * t635 + t649 
     #* t574 + t651 * t480 + t653 * t4 * t600 * t37 / 0.32D2
      bbggh6n2e0 = t558 + t658

      end function



      doubleprecision function bbggh6n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t6 = x1 ** 2
      t8 = Sin(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t16 = log(0.4D1 * t10 * t13)
      t17 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t22 = lh * t4
      t24 = 0.180D3 * t22 * t17
      t26 = 0.1D1 / x1
      t30 = 0.1D1 / x3
      t31 = t30 * t26
      t34 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t35 = -t34 + t17
      t37 = 0.1D1 / x2
      t38 = t37 * t26
      t43 = t30 * t37
      t46 = x2 * t13
      t49 = log(0.4D1 * t46 * t9)
      t51 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t52 = -0.1D1 + x2
      t56 = log(-0.4D1 * t46 * t9 * t52)
      t67 = t13 * t9
      t69 = log(0.4D1 * t67)
      t72 = (-0.180D3 * lh - 0.90D2 * t69) * t4
      t77 = lh ** 2
      t79 = 0.3141592653589793D1 ** 2
      t81 = t69 ** 2
      t84 = (0.180D3 * t69 * lh + 0.180D3 * t77 - 0.30D2 * t79 + 0.45D2 
     #* t81) * t4
      t90 = log(0.4D1 * x3 * t13 * t9)
      t98 = -(0.90D2 * t4 * (t5 - t17 * t16) - t24) * t26 / 0.2880D4 - t
     #4 * t17 * t31 / 0.32D2 - t4 * t35 * t38 / 0.32D2 - t4 * t35 * t43 
     #/ 0.64D2 - (0.90D2 * t4 * (t5 - t49 * t17 - t51 + t56 * t34) - 0.1
     #80D3 * t22 * t35) * t37 / 0.5760D4 - t72 * t5 / 0.5760D4 - t84 * t
     #17 / 0.5760D4 - (0.90D2 * t4 * (t5 - t90 * t17) - t24) * t30 / 0.5
     #760D4
      t99 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t98)
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t98)
      t103 = t2 * x1
      t104 = -0.1D1 + x1
      t105 = t2 * t104
      t109 = 0.1D1 / (-z - x1 + x1 * z)
      t111 = t104 ** 2
      t115 = log(-0.4D1 * t10 / t11 * t109 * t111)
      t116 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t118 = bbggh61J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t127 = t4 * t116
      t132 = -(0.90D2 * t4 * (t115 * t116 - t118) + 0.180D3 * t22 * t116
     #) * t26 / 0.2880D4 + t127 * t31 / 0.32D2 + t127 * t38 / 0.32D2
      t133 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t103, -t105, 0.0D0, t132)
      t135 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t105, t103, 0.0D0, t132)
      t137 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t139 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t144 = 0.180D3 * t22 * t137
      t151 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t152 = -t151 + t137
      t170 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t182 = -(0.90D2 * t4 * (-t16 * t137 + t139) - t144) * t26 / 0.2880
     #D4 - t4 * t137 * t31 / 0.32D2 - t4 * t152 * t38 / 0.32D2 - t84 * t
     #137 / 0.5760D4 - (0.90D2 * t4 * (-t90 * t137 + t139) - t144) * t30
     # / 0.5760D4 - t4 * t152 * t43 / 0.64D2 - (0.90D2 * t4 * (-t49 * t1
     #37 - t170 + t139 + t56 * t151) - 0.180D3 * t22 * t152) * t37 / 0.5
     #760D4 - t72 * t139 / 0.5760D4
      t183 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t182)
      t185 = t2 * x3
      t186 = -0.1D1 + x3
      t187 = t2 * t186
      t188 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t192 = log(-0.4D1 * t67 * x3 * t186)
      t193 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t206 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t211 = -(0.90D2 * t4 * (-t188 + t192 * t193) + 0.180D3 * t22 * t19
     #3) * t30 / 0.5760D4 + t4 * t193 * t31 / 0.32D2 + t4 * (t193 - t206
     #) * t43 / 0.64D2
      t212 = FJET(XB1, XB2, s, 0.0D0, t185, 0.0D0, -t187, 0.0D0, t211)
      t214 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t218 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t228 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t233 = t4 * t214 * t31 / 0.32D2 - (0.90D2 * t4 * (-t218 + t192 * t
     #214) + 0.180D3 * t22 * t214) * t30 / 0.5760D4 + t4 * (t214 - t228)
     # * t43 / 0.64D2
      t234 = FJET(XB1, XB2, s, 0.0D0, -t187, 0.0D0, t185, 0.0D0, t233)
      t238 = t2 * x1 * x2 * t109
      t240 = t1 * x1
      t241 = t52 * s * t240
      t242 = t1 ** 2
      t247 = s * t242 * x2 * x1 * t104 * t109
      t248 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t251 = t4 * t248 * t38 / 0.32D2
      t252 = FJET(XB1, XB2, s, 0.0D0, -t238, -t105, -t241, t247, -t251)
      t255 = t248 * t37 * t26
      t258 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t182)
      t260 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t262 = bbggh62J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t271 = t4 * t260
      t276 = -(0.90D2 * t4 * (t115 * t260 - t262) + 0.180D3 * t22 * t260
     #) * t26 / 0.2880D4 + t271 * t31 / 0.32D2 + t271 * t38 / 0.32D2
      t277 = FJET(XB1, XB2, s, t103, -t105, 0.0D0, 0.0D0, 0.0D0, t276)
      t279 = FJET(XB1, XB2, s, t185, 0.0D0, -t187, 0.0D0, 0.0D0, t211)
      t282 = t2 * x1 * x3
      t284 = t2 * t104 * x3
      t285 = t186 * s
      t286 = t285 * t240
      t288 = t285 * t1 * t104
      t289 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t292 = t4 * t289 * t31 / 0.32D2
      t293 = FJET(XB1, XB2, s, t282, -t284, -t286, t288, 0.0D0, -t292)
      t296 = t289 * t30 * t26
      t299 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, x3, x4)
      t302 = t4 * t299 * t31 / 0.32D2
      t303 = FJET(XB1, XB2, s, t288, -t286, -t284, t282, 0.0D0, -t302)
      t306 = t299 * t30 * t26
      t309 = FJET(XB1, XB2, s, -t105, t103, 0.0D0, 0.0D0, 0.0D0, t276)
      t311 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t314 = t4 * t311 * t38 / 0.32D2
      t315 = FJET(XB1, XB2, s, -t105, -t241, 0.0D0, -t238, t247, -t314)
      t318 = t311 * t37 * t26
      t321 = FJET(XB1, XB2, s, -t187, 0.0D0, t185, 0.0D0, 0.0D0, t233)
      t323 = FJET(XB1, XB2, s, -t284, t282, t288, -t286, 0.0D0, -t292)
      t327 = FJET(XB1, XB2, s, -t241, -t105, -t238, 0.0D0, t247, -t314)
      t331 = FJET(XB1, XB2, s, -t286, t288, t282, -t284, 0.0D0, -t302)
      t335 = FJET(XB1, XB2, s, -t238, 0.0D0, -t241, -t105, t247, -t251)
      bbggh6n2em1 = t99 * t98 + t101 * t98 + t133 * t132 + t135 * t132 +
     # t183 * t182 + t212 * t211 + t234 * t233 - t252 * t4 * t255 / 0.32
     #D2 + t258 * t182 + t277 * t276 + t279 * t211 - t293 * t4 * t296 / 
     #0.32D2 - t303 * t4 * t306 / 0.32D2 + t309 * t276 - t315 * t4 * t31
     #8 / 0.32D2 + t321 * t233 - t323 * t4 * t296 / 0.32D2 - t327 * t4 *
     # t318 / 0.32D2 - t331 * t4 * t306 / 0.32D2 - t335 * t4 * t255 / 0.
     #32D2

      end function



      doubleprecision function bbggh6n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t13 = 0.1D1 / x2
      t16 = bbggh61J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t20 = z ** 2
      t24 = Sin(x4 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t28 = log(0.4D1 / t20 / z * t25)
      t31 = (-0.180D3 * lh - 0.90D2 * t28) * t4
      t34 = 0.1D1 / x3
      t37 = -t6 * t7 / 0.32D2 - t4 * (-t10 + t5) * t13 / 0.64D2 - t4 * t
     #16 / 0.64D2 - t31 * t5 / 0.5760D4 - t6 * t34 / 0.64D2
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t37)
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t37)
      t42 = t2 * x1
      t44 = t2 * (-0.1D1 + x1)
      t45 = bbggh61J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t48 = t4 * t45 * t7 / 0.32D2
      t49 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t42, -t44, 0.0D0, t48)
      t51 = t45 * t7
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t44, t42, 0.0D0, t48)
      t58 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t59 = t4 * t58
      t66 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t71 = bbggh62J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t74 = -t59 * t7 / 0.32D2 - t31 * t58 / 0.5760D4 - t59 * t34 / 0.64
     #D2 - t4 * (-t66 + t58) * t13 / 0.64D2 - t4 * t71 / 0.64D2
      t75 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t74)
      t77 = t2 * x3
      t79 = t2 * (-0.1D1 + x3)
      t80 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t83 = t4 * t80 * t34 / 0.64D2
      t84 = FJET(XB1, XB2, s, 0.0D0, t77, 0.0D0, -t79, 0.0D0, t83)
      t86 = t80 * t34
      t89 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, x3, x4)
      t92 = t4 * t89 * t34 / 0.64D2
      t93 = FJET(XB1, XB2, s, 0.0D0, -t79, 0.0D0, t77, 0.0D0, t92)
      t95 = t89 * t34
      t98 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t74)
      t100 = bbggh62J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.0D0, x4)
      t103 = t4 * t100 * t7 / 0.32D2
      t104 = FJET(XB1, XB2, s, t42, -t44, 0.0D0, 0.0D0, 0.0D0, t103)
      t106 = t100 * t7
      t109 = FJET(XB1, XB2, s, t77, 0.0D0, -t79, 0.0D0, 0.0D0, t83)
      t113 = FJET(XB1, XB2, s, -t44, t42, 0.0D0, 0.0D0, 0.0D0, t103)
      t117 = FJET(XB1, XB2, s, -t79, 0.0D0, t77, 0.0D0, 0.0D0, t92)
      bbggh6n2em2 = t38 * t37 + t40 * t37 + t49 * t4 * t51 / 0.32D2 + t5
     #4 * t4 * t51 / 0.32D2 + t75 * t74 + t84 * t4 * t86 / 0.64D2 + t93 
     #* t4 * t95 / 0.64D2 + t98 * t74 + t104 * t4 * t106 / 0.32D2 + t109
     # * t4 * t86 / 0.64D2 + t113 * t4 * t106 / 0.32D2 + t117 * t4 * t95
     # / 0.64D2

      end function



      doubleprecision function bbggh6n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bbggh61J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t7 = t4 * t5 / 0.64D2
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t7)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t7)
      t14 = bbggh62J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.0D0, x4)
      t16 = t4 * t14 / 0.64D2
      t17 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t16)
      t20 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t16)
      bbggh6n2em3 = -t8 * t4 * t5 / 0.64D2 - t11 * t4 * t5 / 0.64D2 - t1
     #7 * t4 * t14 / 0.64D2 - t20 * t4 * t14 / 0.64D2

      end function



      doubleprecision function bbggh6n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bbggh61J1
      doubleprecision bbggh61J2
      doubleprecision bbggh62J1
      doubleprecision bbggh62J2
      bbggh6n2em4 = 0.0D0

      end function
  
 

      doubleprecision function bbggh61J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t7 = 0.1D1 - x3
      t8 = t7 ** 2
      t11 = t1 * z
      t16 = z ** 2
      t19 = x1 ** 2
      t21 = z + t2 * x1
      t22 = t21 ** 2
      t25 = 0.1D1 - x2
      t30 = cos(x4 * 0.3141592653589793D1)
      t35 = Sqrt(x3 * t25 * t21 * x2 * t7)
      t38 = t7 * t25 * t21 + x2 * x3 + 0.2D1 * t30 * t35
      t39 = t38 ** 2
      t43 = 0.1D1 / t21
      bbggh61J1 = -0.48D2 * wd * (-t4 * t6 * t8 - 0.2D1 * t11 * t2 * t5 
     #* t7 - 0.2D1 * t1 * t16 - t4 * t19 / t22 * t39 - 0.2D1 * t11 * t2 
     #* x1 * t43 * t38 - 0.2D1 * t4 * t5 * t7 * x1 * t43 * t38)

      end function
  
   
 

      doubleprecision function bbggh61J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t7 = 0.1D1 - x3
      t8 = t7 ** 2
      t11 = x1 ** 2
      t13 = z + t2 * x1
      t14 = t13 ** 2
      t17 = 0.1D1 - x2
      t22 = cos(x4 * 0.3141592653589793D1)
      t27 = Sqrt(x3 * t17 * t13 * x2 * t7)
      t30 = t7 * t17 * t13 + x2 * x3 + 0.2D1 * t22 * t27
      t31 = t30 ** 2
      bbggh61J2 = -0.48D2 * wd * (t4 * t6 * t8 + t4 * t11 / t14 * t31 + 
     #0.3D1 * t4 * t5 * t7 * x1 / t13 * t30)

      end function
  
   
 

      doubleprecision function bbggh62J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t7 = 0.1D1 - x3
      t10 = z + t2 * x1
      t11 = 0.1D1 / t10
      t13 = x3 * (0.1D1 - x2)
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t13 * t10 * x2 * t7)
      t24 = t13 * t10 + x2 * t7 - 0.2D1 * t17 * t21
      t29 = z ** 2
      t32 = x1 ** 2
      t33 = t10 ** 2
      t36 = t24 ** 2
      t39 = t5 ** 2
      t40 = t7 ** 2
      t43 = t1 * z
      bbggh62J1 = -0.48D2 * wd * (0.2D1 * t4 * t5 * t7 * x1 * t11 * t24 
     #- 0.2D1 * t1 * t29 - t4 * t32 / t33 * t36 - t4 * t39 * t40 - 0.2D1
     # * t43 * t2 * t5 * t7 + 0.2D1 * t43 * t2 * x1 * t11 * t24)

      end function
  
   
 

      doubleprecision function bbggh62J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - x1
      t6 = t5 ** 2
      t7 = 0.1D1 - x3
      t8 = t7 ** 2
      t11 = x1 ** 2
      t13 = z + t2 * x1
      t14 = t13 ** 2
      t18 = x3 * (0.1D1 - x2)
      t22 = cos(x4 * 0.3141592653589793D1)
      t26 = Sqrt(t18 * t13 * x2 * t7)
      t29 = t18 * t13 + x2 * t7 - 0.2D1 * t22 * t26
      t30 = t29 ** 2
      bbggh62J2 = -0.48D2 * wd * (t4 * t6 * t8 + t4 * t11 / t14 * t30 - 
     #0.3D1 * t4 * t5 * t7 * x1 / t13 * t29)

      end function
  
 