  
      subroutine bggbH5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bggbH51J1  
      doubleprecision bggbH51J2  
      doubleprecision bggbH51J3  
      doubleprecision bggbH52J1  
      doubleprecision bggbH52J2  
      doubleprecision bggbH53J1  
      doubleprecision bggbH53J2  
      doubleprecision bggbH53J3  
      doubleprecision bggbH54J1  
      doubleprecision bggbH54J2  
      doubleprecision bggbH5n1e1  
      doubleprecision bggbH5n1e0  
      doubleprecision bggbH5n1em1  
      doubleprecision bggbH5n1em2  
      doubleprecision bggbH5n1em3  
      doubleprecision bggbH5n1em4  
      doubleprecision bggbH5n2e1  
      doubleprecision bggbH5n2e0  
      doubleprecision bggbH5n2em1  
      doubleprecision bggbH5n2em2  
      doubleprecision bggbH5n2em3  
      doubleprecision bggbH5n2em4  
      doubleprecision bggbH5n3e1  
      doubleprecision bggbH5n3e0  
      doubleprecision bggbH5n3em1  
      doubleprecision bggbH5n3em2  
      doubleprecision bggbH5n3em3  
      doubleprecision bggbH5n3em4  
      doubleprecision bggbH5n4e1  
      doubleprecision bggbH5n4e0  
      doubleprecision bggbH5n4em1  
      doubleprecision bggbH5n4em2  
      doubleprecision bggbH5n4em3  
      doubleprecision bggbH5n4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bggbH5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH5n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bggbH5n3e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bggbH5n4e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bggbH5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH5n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bggbH5n3e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bggbH5n4e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bggbH5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH5n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bggbH5n3em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bggbH5n4em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bggbH5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH5n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bggbH5n3em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bggbH5n4em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bggbH5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH5n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bggbH5n3em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bggbH5n4em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bggbH5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=bggbH5n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=bggbH5n3em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=bggbH5n4em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bggbH5n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t4 = 0.1D1 / t3
      t5 = x4 * 0.3141592653589793D1
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = t4 * t7
      t10 = log(0.4D1 * t8)
      t11 = s ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = lh ** 2
      t16 = 0.3141592653589793D1 ** 2
      t18 = 0.180D3 * t14 - 0.30D2 * t16
      t20 = t10 ** 2
      t21 = t20 * t12
      t28 = -0.2884936567583026D3 - 0.120D3 * t14 * lh + 0.60D2 * lh * t
     #16
      t29 = t12 * t28
      t31 = t20 * t10 * t12
      t33 = -t13 * t18 - 0.90D2 * t21 * lh + t29 - 0.15D2 * t31
      t34 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t37 = t20 ** 2
      t42 = t16 ** 2
      t43 = t14 ** 2
      t53 = 0.15D2 / 0.4D1 * t37 * t12 - t13 * t28 + t12 * (0.5769873135
     #166051D3 * lh + t42 + 0.60D2 * t43 - 0.60D2 * t14 * t16) + t21 * t
     #18 / 0.2D1 + 0.30D2 * t31 * lh
      t54 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t57 = t54 * t12
      t60 = t34 * t12
      t63 = x3 * t7
      t66 = log(0.4D1 * t63 * t4)
      t67 = t66 ** 2
      t68 = -0.1D1 + x3
      t69 = 0.1D1 / t68
      t73 = log(-0.4D1 * t63 * t4 * t69)
      t74 = t73 ** 2
      t76 = cos(t5)
      t78 = Sqrt(-x3 * t68)
      t83 = 0.1D1 / (-z - x3 + 0.2D1 * t76 * t78 * z)
      t86 = -t67 / 0.2D1 - t74 * z * t83 / 0.2D1
      t90 = t12 * t18
      t91 = t90 * t54
      t95 = t73 * z * t83 + t66
      t102 = t67 * t66 / 0.6D1 + t74 * t73 * z * t83 / 0.6D1
      t106 = t29 * t54
      t109 = -z * t83 - 0.1D1
      t112 = 0.1D1 / x3
      t115 = t12 * lh
      t116 = x1 ** 2
      t117 = t116 * t7
      t118 = t117 * t4
      t120 = log(0.4D1 * t118)
      t122 = t120 ** 2
      t133 = t122 * t120
      t140 = 0.1D1 / x1
      t143 = z * t34
      t144 = t116 * x3
      t145 = t8 * t69
      t148 = log(-0.4D1 * t144 * t145)
      t149 = t148 * z
      t155 = log(0.4D1 * t144 * t8)
      t161 = t148 ** 2
      t162 = t161 * z
      t168 = t155 ** 2
      t176 = z * t54 * t83 + t54
      t177 = t90 * t176
      t182 = x2 ** 2
      t183 = x3 * t182
      t184 = t183 * t116
      t187 = log(-0.4D1 * t184 * t145)
      t188 = t187 * z
      t192 = t183 * t118
      t194 = log(0.4D1 * t192)
      t203 = 0.1D1 / x2
      t204 = t203 * t140
      t207 = t182 * t116
      t210 = log(0.4D1 * t207 * t8)
      t216 = t210 ** 2
      t226 = t182 * t7
      t229 = log(0.4D1 * t226 * t4)
      t231 = t229 ** 2
      t240 = t231 * t229
      t253 = log(0.4D1 * t183 * t8)
      t257 = log(-0.4D1 * t183 * t145)
      t258 = t257 * z
      t266 = t257 ** 2
      t267 = t266 * z
      t273 = t253 ** 2
      t283 = -t33 * t34 / 0.11520D5 - t53 * t54 / 0.11520D5 - ((0.180D3 
     #* t57 * lh - 0.90D2 * t60) * t86 + (0.180D3 * t60 * lh - t91) * t9
     #5 - 0.90D2 * t57 * t102 + (-t60 * t18 - t106) * t109) * t112 / 0.1
     #1520D5 - (-0.180D3 * t115 * (-t120 * t34 + t122 * t54 / 0.2D1) + t
     #90 * (t34 - t120 * t54) + 0.90D2 * t12 * (t122 * t34 / 0.2D1 - t13
     #3 * t54 / 0.6D1) + t106) * t140 / 0.5760D4 - (-0.180D3 * t115 * (-
     #(-t143 + t149 * t54) * t83 + t34 - t155 * t54) + 0.90D2 * t12 * (-
     #(t149 * t34 - t162 * t54 / 0.2D1) * t83 - t155 * t34 + t168 * t54 
     #/ 0.2D1) + t177) * t112 * t140 / 0.5760D4 - (0.90D2 * t12 * (-(-t1
     #43 + t188 * t54) * t83 - t194 * t54 + t34) - 0.180D3 * t115 * t176
     #) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (-t210 * t54 + t34
     #) + 0.90D2 * t12 * (-t210 * t34 + t216 * t54 / 0.2D1) + t91) * t20
     #3 * t140 / 0.2880D4 - (-0.180D3 * t115 * (-t229 * t34 + t231 * t54
     # / 0.2D1) + t90 * (t34 - t229 * t54) + 0.90D2 * t12 * (-t240 * t54
     # / 0.6D1 + t231 * t34 / 0.2D1) + t106) * t203 / 0.5760D4 - (-0.180
     #D3 * t115 * (t34 - t253 * t54 - (-t143 + t258 * t54) * t83) + 0.90
     #D2 * t12 * (-(t258 * t34 - t267 * t54 / 0.2D1) * t83 - t253 * t34 
     #+ t273 * t54 / 0.2D1) + t177) * t112 * t203 / 0.5760D4
      t284 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t283)
      t286 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t288 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t289 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t306 = t29 * t289
      t310 = z * t286
      t318 = z * t288
      t332 = t289 + z * t289 * t83
      t333 = t90 * t332
      t361 = t90 * t289
      t371 = 0.180D3 * t13 * lh + t90 + 0.45D2 * t21
      t418 = t289 * t12
      t421 = t286 * t12
      t427 = t288 * t12
      t441 = -(-0.180D3 * t115 * (-t120 * t286 + t288 + t122 * t289 / 0.
     #2D1) + t90 * (t286 - t120 * t289) + 0.90D2 * t12 * (-t120 * t288 +
     # t122 * t286 / 0.2D1 - t133 * t289 / 0.6D1) + t306) * t140 / 0.576
     #0D4 - (-0.180D3 * t115 * (-(-t310 + t149 * t289) * t83 - t155 * t2
     #89 + t286) + 0.90D2 * t12 * (-(-t318 + t149 * t286 - t162 * t289 /
     # 0.2D1) * t83 + t288 - t155 * t286 + t168 * t289 / 0.2D1) + t333) 
     #* t112 * t140 / 0.5760D4 - (0.90D2 * t12 * (t286 - (-t310 + t188 *
     # t289) * t83 - t194 * t289) - 0.180D3 * t115 * t332) * t112 * t204
     # / 0.2880D4 - (-0.180D3 * t115 * (-t210 * t289 + t286) + 0.90D2 * 
     #t12 * (t216 * t289 / 0.2D1 - t210 * t286 + t288) + t361) * t203 * 
     #t140 / 0.2880D4 - t53 * t289 / 0.11520D5 - t371 * t288 / 0.11520D5
     # - t33 * t286 / 0.11520D5 - (-0.180D3 * t115 * (t231 * t289 / 0.2D
     #1 - t229 * t286 + t288) + t90 * (t286 - t229 * t289) + 0.90D2 * t1
     #2 * (-t229 * t288 - t240 * t289 / 0.6D1 + t231 * t286 / 0.2D1) + t
     #306) * t203 / 0.5760D4 - (-0.180D3 * t115 * (-t253 * t289 + t286 -
     # (-t310 + t258 * t289) * t83) + 0.90D2 * t12 * (-(-t318 + t258 * t
     #286 - t267 * t289 / 0.2D1) * t83 - t253 * t286 + t288 + t273 * t28
     #9 / 0.2D1) + t333) * t112 * t203 / 0.5760D4 - ((0.180D3 * t418 * l
     #h - 0.90D2 * t421) * t86 + (0.180D3 * t421 * lh - t361 - 0.90D2 * 
     #t427) * t95 - 0.90D2 * t418 * t102 + (-t421 * t18 + 0.180D3 * t427
     # * lh - t306) * t109) * t112 / 0.11520D5
      t442 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t441)
      t444 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t447 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t450 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t470 = t29 * t450
      t474 = z * t447
      t484 = z * t444
      t496 = z * t450 * t83 + t450
      t497 = t90 * t496
      t567 = t90 * t450
      t572 = t450 * t12
      t575 = t447 * t12
      t581 = t444 * t12
      t595 = -t371 * t444 / 0.11520D5 - t33 * t447 / 0.11520D5 - t53 * t
     #450 / 0.11520D5 - (-0.180D3 * t115 * (-t229 * t447 + t444 + t231 *
     # t450 / 0.2D1) + t90 * (t447 - t229 * t450) + 0.90D2 * t12 * (-t22
     #9 * t444 + t231 * t447 / 0.2D1 - t240 * t450 / 0.6D1) + t470) * t2
     #03 / 0.5760D4 - (-0.180D3 * t115 * (-(-t474 + t258 * t450) * t83 +
     # t447 - t253 * t450) + 0.90D2 * t12 * (t273 * t450 / 0.2D1 + t444 
     #- (-t484 + t258 * t447 - t267 * t450 / 0.2D1) * t83 - t253 * t447)
     # + t497) * t112 * t203 / 0.5760D4 - (-0.180D3 * t115 * (t122 * t45
     #0 / 0.2D1 + t444 - t120 * t447) + t90 * (-t120 * t450 + t447) + 0.
     #90D2 * t12 * (-t120 * t444 + t122 * t447 / 0.2D1 - t133 * t450 / 0
     #.6D1) + t470) * t140 / 0.5760D4 - (-0.180D3 * t115 * (-t155 * t450
     # + t447 - (-t474 + t149 * t450) * t83) + 0.90D2 * t12 * (-(-t484 +
     # t149 * t447 - t162 * t450 / 0.2D1) * t83 - t155 * t447 + t168 * t
     #450 / 0.2D1 + t444) + t497) * t112 * t140 / 0.5760D4 - (0.90D2 * t
     #12 * (-(-t474 + t188 * t450) * t83 - t194 * t450 + t447) - 0.180D3
     # * t115 * t496) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (-t2
     #10 * t450 + t447) + 0.90D2 * t12 * (t444 + t216 * t450 / 0.2D1 - t
     #210 * t447) + t567) * t203 * t140 / 0.2880D4 - ((0.180D3 * t572 * 
     #lh - 0.90D2 * t575) * t86 + (0.180D3 * t575 * lh - t567 - 0.90D2 *
     # t581) * t95 - 0.90D2 * t572 * t102 + (-t575 * t18 + 0.180D3 * t58
     #1 * lh - t470) * t109) * t112 / 0.11520D5
      t596 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t595)
      t598 = t2 * x1
      t599 = -0.1D1 + x1
      t600 = x1 * z
      t601 = 0.1D1 - x1 + t600
      t602 = 0.1D1 / t601
      t604 = t2 * t599 * t602
      t605 = t1 ** 2
      t606 = s * t605
      t608 = x1 * t599 * t602
      t609 = t606 * t608
      t610 = -t599
      t611 = bggbH51J3(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4)
      t612 = t4 * t602
      t613 = t599 ** 2
      t614 = t612 * t613
      t617 = log(0.4D1 * t117 * t614)
      t618 = t617 ** 2
      t619 = bggbH51J1(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4)
      t622 = bggbH51J2(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4)
      t631 = t618 * t617
      t643 = z * t601
      t644 = t643 * t622
      t645 = t144 * t7
      t647 = t612 * t613 * t69
      t650 = log(-0.4D1 * t645 * t647)
      t651 = t650 * z
      t652 = t601 * t619
      t655 = x1 * x3
      t656 = 0.2D1 * t655
      t657 = x3 * t601
      t659 = Sqrt(-t657 * t68)
      t663 = x1 * t3
      t664 = x3 * t3
      t665 = t664 * x1
      t667 = 0.2D1 * t144 * z
      t668 = t144 * t3
      t669 = t655 * z
      t670 = 0.3D1 * t669
      t671 = -t144 - z + t656 + 0.2D1 * t76 * t659 * z - x3 + t600 - t66
     #3 + t665 + t667 - t668 - t670
      t672 = 0.1D1 / t671
      t676 = log(0.4D1 * t645 * t614)
      t682 = t676 ** 2
      t688 = t650 ** 2
      t689 = t688 * z
      t699 = -t643 * t619 * t672 - t619
      t705 = t602 * t613
      t709 = log(0.4D1 * t184 * t8 * t705)
      t711 = t183 * t117
      t714 = log(-0.4D1 * t711 * t647)
      t715 = t714 * z
      t728 = t207 * t7
      t731 = log(0.4D1 * t728 * t614)
      t737 = t731 ** 2
      t748 = -(-0.180D3 * t115 * (-t611 - t618 * t619 / 0.2D1 + t617 * t
     #622) + t90 * (-t622 + t617 * t619) + 0.90D2 * t12 * (t617 * t611 +
     # t631 * t619 / 0.6D1 - t618 * t622 / 0.2D1) - t29 * t619) * t140 /
     # 0.5760D4 - (-0.180D3 * t115 * (-t622 - (t644 - t651 * t652) * t67
     #2 + t676 * t619) + 0.90D2 * t12 * (-t611 + t676 * t622 - t682 * t6
     #19 / 0.2D1 - (t643 * t611 - t651 * t601 * t622 + t689 * t652 / 0.2
     #D1) * t672) + t90 * t699) * t112 * t140 / 0.5760D4 - (0.90D2 * t12
     # * (t709 * t619 - (t644 - t715 * t652) * t672 - t622) - 0.180D3 * 
     #t115 * t699) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (-t622 
     #+ t731 * t619) + 0.90D2 * t12 * (-t611 + t731 * t622 - t737 * t619
     # / 0.2D1) - t90 * t619) * t203 * t140 / 0.2880D4
      t749 = FJET(XB1, XB2, s, 0.0D0, t598, -t604, 0.0D0, -t609, t748)
      t751 = x2 * s
      t752 = t751 * t1
      t753 = -0.1D1 + x2
      t754 = t2 * t753
      t755 = t8 * t753
      t758 = log(-0.4D1 * t184 * t755)
      t759 = -t753
      t760 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4)
      t762 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4)
      t774 = log(-0.4D1 * t207 * t755)
      t779 = t774 ** 2
      t786 = t90 * t760
      t791 = t4 * t753
      t794 = log(-0.4D1 * t226 * t791)
      t795 = t794 ** 2
      t807 = t795 * t794
      t819 = log(-0.4D1 * t183 * t755)
      t824 = t819 ** 2
      t835 = -(0.90D2 * t12 * (t758 * t760 - t762) + 0.180D3 * t115 * t7
     #60) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t774 * t760 - t
     #762) + 0.90D2 * t12 * (-t779 * t760 / 0.2D1 + t774 * t762) - t786)
     # * t203 * t140 / 0.2880D4 - (-0.180D3 * t115 * (-t795 * t760 / 0.2
     #D1 + t794 * t762) + t90 * (t794 * t760 - t762) + 0.90D2 * t12 * (-
     #t795 * t762 / 0.2D1 + t807 * t760 / 0.6D1) - t29 * t760) * t203 / 
     #0.5760D4 - (-0.180D3 * t115 * (t819 * t760 - t762) + 0.90D2 * t12 
     #* (-t824 * t760 / 0.2D1 + t819 * t762) - t786) * t112 * t203 / 0.5
     #760D4
      t836 = FJET(XB1, XB2, s, 0.0D0, t752, 0.0D0, -t754, 0.0D0, t835)
      t838 = x2 * x3
      t841 = Sqrt(x3 * t753 * t68)
      t842 = t76 * t841
      t844 = 0.2D1 * t842 * x2
      t846 = 0.1D1 - x3 + t838
      t847 = 0.1D1 / t846
      t849 = t2 * (0.1D1 - x2 - x3 + t838 + t183 + t844) * t847
      t854 = t2 * x2 * (t838 - 0.1D1 + 0.2D1 * t842) * t847
      t856 = t846 ** 2
      t857 = 0.1D1 / t856
      t859 = t791 * t68 * t857
      t862 = log(0.4D1 * t183 * t7 * t859)
      t863 = x2 * z
      t864 = -x2 - z + t863
      t865 = t862 * t864
      t866 = t183 * z
      t867 = t838 * z
      t873 = 0.1D1 / (x2 + z - t863 + t866 - t867 + x3 - t183 - t844 - 0
     #.2D1 * t842 * z + 0.2D1 * t842 * t863)
      t874 = t68 * t847
      t875 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t876 = t873 * t875
      t878 = t864 * t873
      t879 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t880 = t878 * t879
      t886 = t862 ** 2
      t887 = t886 * t864
      t890 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t895 = t878 * t875
      t903 = log(0.4D1 * t711 * t859)
      t904 = t903 * t864
      t915 = -(-0.180D3 * t115 * (t865 * t876 - t880) + 0.90D2 * t12 * (
     #t865 * t873 * t879 - t887 * t876 / 0.2D1 - t878 * t890) - t90 * t8
     #95) * t112 * t203 / 0.5760D4 - (0.90D2 * t12 * (-t880 + t904 * t87
     #6) + 0.180D3 * t115 * t895) * t112 * t204 / 0.2880D4
      t916 = FJET(XB1, XB2, s, 0.0D0, t849, 0.0D0, -t854, 0.0D0, t915)
      t919 = t1 * t599
      t921 = t753 * s * t919 * t602
      t922 = t751 * t919
      t924 = t606 * t753 * t608
      t926 = t612 * t613 * t753
      t929 = log(-0.4D1 * t711 * t926)
      t930 = bggbH53J1(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t932 = bggbH53J2(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t943 = log(-0.4D1 * t728 * t926)
      t949 = bggbH53J3(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t950 = t943 ** 2
      t961 = -(0.90D2 * t12 * (-t929 * t930 + t932) - 0.180D3 * t115 * t
     #930) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t932 - t943 * 
     #t930) + 0.90D2 * t12 * (-t943 * t932 + t949 + t950 * t930 / 0.2D1)
     # + t90 * t930) * t203 * t140 / 0.2880D4
      t962 = FJET(XB1, XB2, s, 0.0D0, t921, t598, -t922, t924, t961)
      t964 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4)
      t966 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4)
      t982 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4)
      t987 = t90 * t964
      t1027 = -(0.90D2 * t12 * (t758 * t964 - t966) + 0.180D3 * t115 * t
     #964) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t774 * t964 - 
     #t966) + 0.90D2 * t12 * (-t779 * t964 / 0.2D1 - t982 + t774 * t966)
     # - t987) * t203 * t140 / 0.2880D4 - (-0.180D3 * t115 * (-t982 - t7
     #95 * t964 / 0.2D1 + t794 * t966) + t90 * (-t966 + t794 * t964) + 0
     #.90D2 * t12 * (t794 * t982 - t795 * t966 / 0.2D1 + t807 * t964 / 0
     #.6D1) - t29 * t964) * t203 / 0.5760D4 - (-0.180D3 * t115 * (-t966 
     #+ t819 * t964) + 0.90D2 * t12 * (t819 * t966 - t982 - t824 * t964 
     #/ 0.2D1) - t987) * t112 * t203 / 0.5760D4
      t1028 = FJET(XB1, XB2, s, 0.0D0, -t754, 0.0D0, t752, 0.0D0, t1027)
      t1030 = bggbH53J2(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4
     #)
      t1032 = bggbH53J1(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4
     #)
      t1035 = bggbH53J3(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4
     #)
      t1054 = t643 * t1030
      t1055 = t601 * t1032
      t1078 = -t1032 - t643 * t1032 * t672
      t1112 = -(-0.180D3 * t115 * (t617 * t1030 - t618 * t1032 / 0.2D1 -
     # t1035) + t90 * (t617 * t1032 - t1030) + 0.90D2 * t12 * (t617 * t1
     #035 + t631 * t1032 / 0.6D1 - t618 * t1030 / 0.2D1) - t29 * t1032) 
     #* t140 / 0.5760D4 - (-0.180D3 * t115 * (-t1030 - (t1054 - t651 * t
     #1055) * t672 + t676 * t1032) + 0.90D2 * t12 * (-t682 * t1032 / 0.2
     #D1 + t676 * t1030 - (t643 * t1035 - t651 * t601 * t1030 + t689 * t
     #1055 / 0.2D1) * t672 - t1035) + t90 * t1078) * t112 * t140 / 0.576
     #0D4 - (0.90D2 * t12 * (t709 * t1032 - t1030 - (t1054 - t715 * t105
     #5) * t672) - 0.180D3 * t115 * t1078) * t112 * t204 / 0.2880D4 - (-
     #0.180D3 * t115 * (t731 * t1032 - t1030) + 0.90D2 * t12 * (-t1035 +
     # t731 * t1030 - t737 * t1032 / 0.2D1) - t90 * t1032) * t203 * t140
     # / 0.2880D4
      t1113 = FJET(XB1, XB2, s, 0.0D0, -t604, t598, 0.0D0, -t609, t1112)
      t1115 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t1116 = t873 * t1115
      t1118 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t1119 = t878 * t1118
      t1123 = t878 * t1115
      t1146 = -(0.90D2 * t12 * (t904 * t1116 - t1119) + 0.180D3 * t115 *
     # t1123) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t865 * t111
     #6 - t1119) + 0.90D2 * t12 * (-t887 * t1116 / 0.2D1 + t865 * t873 *
     # t1118) - t90 * t1123) * t112 * t203 / 0.5760D4
      t1147 = FJET(XB1, XB2, s, 0.0D0, -t854, 0.0D0, t849, 0.0D0, t1146)
      t1149 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, 
     #x4)
      t1151 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, 
     #x4)
      t1167 = t29 * t1151
      t1172 = z * t1149
      t1192 = t1151 + z * t1151 * t83
      t1193 = t90 * t1192
      t1221 = t90 * t1151
      t1267 = t1151 * t12
      t1270 = t1149 * t12
      t1290 = -(-0.180D3 * t115 * (-t120 * t1149 + t122 * t1151 / 0.2D1)
     # + t90 * (t1149 - t120 * t1151) + 0.90D2 * t12 * (t122 * t1149 / 0
     #.2D1 - t133 * t1151 / 0.6D1) + t1167) * t140 / 0.5760D4 - (-0.180D
     #3 * t115 * (-t155 * t1151 + t1149 - (-t1172 + t149 * t1151) * t83)
     # + 0.90D2 * t12 * (-(t149 * t1149 - t162 * t1151 / 0.2D1) * t83 - 
     #t155 * t1149 + t168 * t1151 / 0.2D1) + t1193) * t112 * t140 / 0.57
     #60D4 - (0.90D2 * t12 * (-(-t1172 + t188 * t1151) * t83 - t194 * t1
     #151 + t1149) - 0.180D3 * t115 * t1192) * t112 * t204 / 0.2880D4 - 
     #(-0.180D3 * t115 * (-t210 * t1151 + t1149) + 0.90D2 * t12 * (-t210
     # * t1149 + t216 * t1151 / 0.2D1) + t1221) * t203 * t140 / 0.2880D4
     # - (-0.180D3 * t115 * (-t229 * t1149 + t231 * t1151 / 0.2D1) + t90
     # * (-t229 * t1151 + t1149) + 0.90D2 * t12 * (t231 * t1149 / 0.2D1 
     #- t240 * t1151 / 0.6D1) + t1167) * t203 / 0.5760D4 - (-0.180D3 * t
     #115 * (-(-t1172 + t258 * t1151) * t83 + t1149 - t253 * t1151) + 0.
     #90D2 * t12 * (-t253 * t1149 + t273 * t1151 / 0.2D1 - (t258 * t1149
     # - t267 * t1151 / 0.2D1) * t83) + t1193) * t112 * t203 / 0.5760D4 
     #- ((0.180D3 * t1267 * lh - 0.90D2 * t1270) * t86 + (0.180D3 * t127
     #0 * lh - t1221) * t95 - 0.90D2 * t1267 * t102 + (-t1270 * t18 - t1
     #167) * t109) * t112 / 0.11520D5 - t53 * t1151 / 0.11520D5 - t33 * 
     #t1149 / 0.11520D5
      t1291 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1290)
      t1293 = bggbH54J1(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4
     #)
      t1296 = bggbH54J2(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4
     #)
      t1316 = t643 * t1296
      t1317 = t601 * t1293
      t1338 = -t1293 - t643 * t1293 * t672
      t1372 = -(-0.180D3 * t115 * (-t618 * t1293 / 0.2D1 + t617 * t1296)
     # + t90 * (t617 * t1293 - t1296) + 0.90D2 * t12 * (t631 * t1293 / 0
     #.6D1 - t618 * t1296 / 0.2D1) - t29 * t1293) * t140 / 0.5760D4 - (-
     #0.180D3 * t115 * (t676 * t1293 - t1296 - (t1316 - t651 * t1317) * 
     #t672) + 0.90D2 * t12 * (-t682 * t1293 / 0.2D1 + t676 * t1296 - (-t
     #651 * t601 * t1296 + t689 * t1317 / 0.2D1) * t672) + t90 * t1338) 
     #* t112 * t140 / 0.5760D4 - (0.90D2 * t12 * (-(t1316 - t715 * t1317
     #) * t672 - t1296 + t709 * t1293) - 0.180D3 * t115 * t1338) * t112 
     #* t204 / 0.2880D4 - (-0.180D3 * t115 * (-t1296 + t731 * t1293) + 0
     #.90D2 * t12 * (-t737 * t1293 / 0.2D1 + t731 * t1296) - t90 * t1293
     #) * t203 * t140 / 0.2880D4
      t1373 = FJET(XB1, XB2, s, t598, 0.0D0, 0.0D0, -t604, -t609, t1372)
      t1375 = t284 * t283 + t442 * t441 + t596 * t595 + t749 * t748 + t8
     #36 * t835 + t916 * t915 + t962 * t961 + t1028 * t1027 + t1113 * t1
     #112 + t1147 * t1146 + t1291 * t1290 + t1373 * t1372
      t1376 = bggbH54J1(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t1378 = bggbH54J2(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t1402 = -(0.90D2 * t12 * (-t929 * t1376 + t1378) - 0.180D3 * t115 
     #* t1376) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (-t943 * t1
     #376 + t1378) + 0.90D2 * t12 * (-t943 * t1378 + t950 * t1376 / 0.2D
     #1) + t90 * t1376) * t203 * t140 / 0.2880D4
      t1403 = FJET(XB1, XB2, s, t598, -t922, 0.0D0, t921, t924, t1402)
      t1405 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4
     #)
      t1407 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4
     #)
      t1421 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4
     #)
      t1428 = t90 * t1405
      t1468 = -(0.90D2 * t12 * (t758 * t1405 - t1407) + 0.180D3 * t115 *
     # t1405) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t774 * t140
     #5 - t1407) + 0.90D2 * t12 * (-t1421 + t774 * t1407 - t779 * t1405 
     #/ 0.2D1) - t1428) * t203 * t140 / 0.2880D4 - (-0.180D3 * t115 * (-
     #t1421 - t795 * t1405 / 0.2D1 + t794 * t1407) + t90 * (t794 * t1405
     # - t1407) + 0.90D2 * t12 * (t794 * t1421 - t795 * t1407 / 0.2D1 + 
     #t807 * t1405 / 0.6D1) - t29 * t1405) * t203 / 0.5760D4 - (-0.180D3
     # * t115 * (t819 * t1405 - t1407) + 0.90D2 * t12 * (-t1421 - t824 *
     # t1405 / 0.2D1 + t819 * t1407) - t1428) * t112 * t203 / 0.5760D4
      t1469 = FJET(XB1, XB2, s, t752, 0.0D0, -t754, 0.0D0, 0.0D0, t1468)
      t1471 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t1472 = t878 * t1471
      t1473 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t1474 = t873 * t1473
      t1479 = t878 * t1473
      t1502 = -(0.90D2 * t12 * (-t1472 + t904 * t1474) + 0.180D3 * t115 
     #* t1479) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t865 * t14
     #74 - t1472) + 0.90D2 * t12 * (t865 * t873 * t1471 - t887 * t1474 /
     # 0.2D1) - t90 * t1479) * t112 * t203 / 0.5760D4
      t1503 = FJET(XB1, XB2, s, t849, 0.0D0, -t854, 0.0D0, 0.0D0, t1502)
      t1505 = bggbH52J1(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t1507 = bggbH52J2(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t1531 = -(0.90D2 * t12 * (-t929 * t1505 + t1507) - 0.180D3 * t115 
     #* t1505) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t1507 - t9
     #43 * t1505) + 0.90D2 * t12 * (-t943 * t1507 + t950 * t1505 / 0.2D1
     #) + t90 * t1505) * t203 * t140 / 0.2880D4
      t1532 = FJET(XB1, XB2, s, t921, 0.0D0, -t922, t598, t924, t1531)
      t1535 = t598 * t838 * t847
      t1536 = t2 * t599
      t1537 = t183 * x1
      t1538 = t183 * t600
      t1539 = t753 * t68
      t1541 = Sqrt(t657 * t1539)
      t1542 = t76 * t1541
      t1544 = 0.2D1 * t1542 * x2
      t1548 = t1536 * (t183 - x2 + t838 + 0.1D1 - x3 - t1537 + t1538 + t
     #1544) * t602 * t847
      t1552 = t68 * s * t1 * x1 * t847
      t1558 = t1536 * x2 * (-0.1D1 + t838 + x1 - t655 - t600 + t669 + 0.
     #2D1 * t1542) * t602 * t847
      t1559 = x2 * x1
      t1560 = t1559 * z
      t1561 = z - t863 - t1559 + t1560 + x2
      t1562 = t601 * t1561
      t1578 = x2 * t116
      t1582 = t665 + t667 - t668 - t670 - 0.2D1 * t1542 * t863 - 0.2D1 *
     # t1542 * t1559 - t664 * t1559 - 0.2D1 * t144 * t863 + t144 * t3 * 
     #x2 + 0.2D1 * t838 * t600 + t1538 - 0.3D1 * t1560 + t144 * x2 - t83
     #8 * x1 + t1559 * t3 + 0.2D1 * t1578 * z - t1578 * t3
      t1588 = 0.2D1 * t1542 * z + t183 - t1537 + t1544 - x3 - z + 0.2D1 
     #* t1542 * t1560 - t866 + t867 + t600 - t144 + t656 - t663 + t863 -
     # x2 + 0.2D1 * t1559 - t1578
      t1590 = 0.1D1 / (t1582 + t1588)
      t1591 = bggbH53J2(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1598 = log(0.4D1 * t192 * t705 * t1539 * t857)
      t1599 = t1598 * t601
      t1600 = t1561 * t1590
      t1601 = bggbH53J1(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1602 = t1600 * t1601
      t1607 = t115 * t601
      t1610 = 0.90D2 * t12 * (t1562 * t1590 * t1591 - t1599 * t1602) - 0
     #.180D3 * t1607 * t1602
      t1614 = FJET(XB1, XB2, s, t1535, -t1548, -t1552, t1558, t924, -t16
     #10 * t112 * t204 / 0.2880D4)
      t1617 = t112 * t203 * t140
      t1620 = bggbH51J2(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1623 = bggbH51J1(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1624 = t1600 * t1623
      t1631 = 0.90D2 * t12 * (t1562 * t1590 * t1620 - t1599 * t1624) - 0
     #.180D3 * t1607 * t1624
      t1635 = FJET(XB1, XB2, s, t1558, -t1552, -t1548, t1535, t924, -t16
     #31 * t112 * t204 / 0.2880D4)
      t1639 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4
     #)
      t1640 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, 0.10D1, x4
     #)
      t1661 = t90 * t1640
      t1700 = -(0.90D2 * t12 * (-t1639 + t758 * t1640) + 0.180D3 * t115 
     #* t1640) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (-t1639 + t
     #774 * t1640) + 0.90D2 * t12 * (-t779 * t1640 / 0.2D1 + t774 * t163
     #9) - t1661) * t203 * t140 / 0.2880D4 - (-0.180D3 * t115 * (-t795 *
     # t1640 / 0.2D1 + t794 * t1639) + t90 * (t794 * t1640 - t1639) + 0.
     #90D2 * t12 * (-t795 * t1639 / 0.2D1 + t807 * t1640 / 0.6D1) - t29 
     #* t1640) * t203 / 0.5760D4 - (-0.180D3 * t115 * (t819 * t1640 - t1
     #639) + 0.90D2 * t12 * (t819 * t1639 - t824 * t1640 / 0.2D1) - t166
     #1) * t112 * t203 / 0.5760D4
      t1701 = FJET(XB1, XB2, s, -t754, 0.0D0, t752, 0.0D0, 0.0D0, t1700)
      t1703 = bggbH52J1(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4
     #)
      t1706 = bggbH52J2(s, XB1, XB2, z, lh, wd, t610, 0.10D1, 0.10D1, x4
     #)
      t1726 = t643 * t1706
      t1727 = t601 * t1703
      t1748 = -t1703 - t643 * t1703 * t672
      t1782 = -(-0.180D3 * t115 * (-t618 * t1703 / 0.2D1 + t617 * t1706)
     # + t90 * (-t1706 + t617 * t1703) + 0.90D2 * t12 * (t631 * t1703 / 
     #0.6D1 - t618 * t1706 / 0.2D1) - t29 * t1703) * t140 / 0.5760D4 - (
     #-0.180D3 * t115 * (t676 * t1703 - t1706 - (t1726 - t651 * t1727) *
     # t672) + 0.90D2 * t12 * (-t682 * t1703 / 0.2D1 + t676 * t1706 - (-
     #t651 * t601 * t1706 + t689 * t1727 / 0.2D1) * t672) + t90 * t1748)
     # * t112 * t140 / 0.5760D4 - (0.90D2 * t12 * (t709 * t1703 - t1706 
     #- (t1726 - t715 * t1727) * t672) - 0.180D3 * t115 * t1748) * t112 
     #* t204 / 0.2880D4 - (-0.180D3 * t115 * (-t1706 + t731 * t1703) + 0
     #.90D2 * t12 * (t731 * t1706 - t737 * t1703 / 0.2D1) - t90 * t1703)
     # * t203 * t140 / 0.2880D4
      t1783 = FJET(XB1, XB2, s, -t604, 0.0D0, 0.0D0, t598, -t609, t1782)
      t1785 = bggbH51J2(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t1786 = bggbH51J1(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t1803 = bggbH51J3(s, XB1, XB2, z, lh, wd, t610, t759, 0.10D1, x4)
      t1812 = -(0.90D2 * t12 * (t1785 - t929 * t1786) - 0.180D3 * t115 *
     # t1786) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (t1785 - t94
     #3 * t1786) + 0.90D2 * t12 * (-t943 * t1785 + t950 * t1786 / 0.2D1 
     #+ t1803) + t90 * t1786) * t203 * t140 / 0.2880D4
      t1813 = FJET(XB1, XB2, s, -t922, t598, t921, 0.0D0, t924, t1812)
      t1815 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t1816 = t873 * t1815
      t1818 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t1819 = t878 * t1818
      t1823 = t878 * t1815
      t1836 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, t759, -t874, x4)
      t1848 = -(0.90D2 * t12 * (t904 * t1816 - t1819) + 0.180D3 * t115 *
     # t1823) * t112 * t204 / 0.2880D4 - (-0.180D3 * t115 * (-t1819 + t8
     #65 * t1816) + 0.90D2 * t12 * (-t887 * t1816 / 0.2D1 - t878 * t1836
     # + t865 * t873 * t1818) - t90 * t1823) * t112 * t203 / 0.5760D4
      t1849 = FJET(XB1, XB2, s, -t854, 0.0D0, t849, 0.0D0, 0.0D0, t1848)
      t1851 = bggbH54J2(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1854 = bggbH54J1(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1855 = t1600 * t1854
      t1862 = 0.90D2 * t12 * (t1562 * t1590 * t1851 - t1599 * t1855) - 0
     #.180D3 * t1607 * t1855
      t1866 = FJET(XB1, XB2, s, -t1552, t1558, t1535, -t1548, t924, -t18
     #62 * t112 * t204 / 0.2880D4)
      t1870 = bggbH52J2(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1873 = bggbH52J1(s, XB1, XB2, z, lh, wd, t610, t759, -t874, x4)
      t1874 = t1600 * t1873
      t1881 = 0.90D2 * t12 * (t1562 * t1590 * t1870 - t1599 * t1874) - 0
     #.180D3 * t1607 * t1874
      t1885 = FJET(XB1, XB2, s, -t1548, t1535, t1558, -t1552, t924, -t18
     #81 * t112 * t204 / 0.2880D4)
      t1889 = t1403 * t1402 + t1469 * t1468 + t1503 * t1502 + t1532 * t1
     #531 - t1614 * t1610 * t1617 / 0.2880D4 - t1635 * t1631 * t1617 / 0
     #.2880D4 + t1701 * t1700 + t1783 * t1782 + t1813 * t1812 + t1849 * 
     #t1848 - t1866 * t1862 * t1617 / 0.2880D4 - t1885 * t1881 * t1617 /
     # 0.2880D4
      bggbH5n1e1 = t1375 + t1889

      end function



      doubleprecision function bggbH5n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t6 = z * t5
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = -0.1D1 + x3
      t16 = 0.1D1 / t15
      t17 = t14 * t16
      t20 = log(-0.4D1 * t8 * t17)
      t21 = t20 * z
      t22 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t25 = cos(t9)
      t27 = Sqrt(-x3 * t15)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t8 * t14)
      t41 = t4 * lh
      t44 = z * t22 * t32 + t22
      t46 = 0.180D3 * t41 * t44
      t48 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t54 = 0.1D1 / x2
      t56 = t48 * t54 * t50
      t59 = x2 ** 2
      t60 = t59 * t7
      t63 = log(0.4D1 * t60 * t14)
      t68 = t22 * t4
      t70 = 0.180D3 * t68 * lh
      t75 = t7 * t11
      t78 = log(0.4D1 * t75 * t13)
      t84 = t78 ** 2
      t90 = lh ** 2
      t92 = 0.3141592653589793D1 ** 2
      t94 = 0.180D3 * t90 - 0.30D2 * t92
      t95 = t4 * t94
      t96 = t95 * t22
      t100 = t5 * t4
      t103 = x3 * t11
      t107 = log(-0.4D1 * t103 * t13 * t16)
      t112 = log(0.4D1 * t103 * t13)
      t113 = t107 * z * t32 + t112
      t115 = t112 ** 2
      t116 = t107 ** 2
      t120 = -t115 / 0.2D1 - t116 * z * t32 / 0.2D1
      t127 = -z * t32 - 0.1D1
      t133 = log(0.4D1 * t14)
      t134 = t133 * t4
      t137 = t133 ** 2
      t138 = t137 * t4
      t140 = 0.180D3 * t134 * lh + t95 + 0.45D2 * t138
      t143 = x3 * t59
      t146 = log(0.4D1 * t143 * t14)
      t150 = log(-0.4D1 * t143 * t17)
      t151 = t150 * z
      t162 = t59 * t11
      t165 = log(0.4D1 * t162 * t13)
      t171 = t165 ** 2
      t192 = -t134 * t94 - 0.90D2 * t138 * lh + t4 * (-0.288493656758302
     #6D3 - 0.120D3 * t90 * lh + 0.60D2 * lh * t92) - 0.15D2 * t137 * t1
     #33 * t4
      t195 = -(0.90D2 * t4 * (-(-t6 + t21 * t22) * t32 + t5 - t36 * t22)
     # - t46) * t48 * t50 / 0.5760D4 - t4 * t44 * t56 / 0.32D2 - (0.90D2
     # * t4 * (-t63 * t22 + t5) - t70) * t54 * t50 / 0.2880D4 - (-0.180D
     #3 * t41 * (t5 - t78 * t22) + 0.90D2 * t4 * (-t78 * t5 + t84 * t22 
     #/ 0.2D1) + t96) * t50 / 0.5760D4 - ((t70 - 0.90D2 * t100) * t113 -
     # 0.90D2 * t68 * t120 + (0.180D3 * t100 * lh - t96) * t127) * t48 /
     # 0.11520D5 - t140 * t5 / 0.11520D5 - (0.90D2 * t4 * (t5 - t146 * t
     #22 - (-t6 + t151 * t22) * t32) - t46) * t48 * t54 / 0.5760D4 - (-0
     #.180D3 * t41 * (t5 - t165 * t22) + 0.90D2 * t4 * (-t165 * t5 + t17
     #1 * t22 / 0.2D1) + t96) * t54 / 0.5760D4 - t192 * t22 / 0.11520D5
      t196 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t195)
      t198 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t199 = z * t198
      t200 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t210 = t200 + z * t200 * t32
      t212 = 0.180D3 * t41 * t210
      t224 = t200 * t4
      t226 = 0.180D3 * t224 * lh
      t236 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t242 = t95 * t200
      t248 = t198 * t4
      t291 = -0.180D3 * t41 - 0.90D2 * t134
      t294 = -(0.90D2 * t4 * (-(-t199 + t21 * t200) * t32 - t36 * t200 +
     # t198) - t212) * t48 * t50 / 0.5760D4 - t4 * t210 * t56 / 0.32D2 -
     # (0.90D2 * t4 * (-t63 * t200 + t198) - t226) * t54 * t50 / 0.2880D
     #4 - (-0.180D3 * t41 * (t198 - t78 * t200) + 0.90D2 * t4 * (-t78 * 
     #t198 + t236 + t84 * t200 / 0.2D1) + t242) * t50 / 0.5760D4 - t192 
     #* t200 / 0.11520D5 - ((t226 - 0.90D2 * t248) * t113 - 0.90D2 * t22
     #4 * t120 + (0.180D3 * t248 * lh - t242 - 0.90D2 * t236 * t4) * t12
     #7) * t48 / 0.11520D5 - t140 * t198 / 0.11520D5 - (0.90D2 * t4 * (-
     #t146 * t200 + t198 - (-t199 + t151 * t200) * t32) - t212) * t48 * 
     #t54 / 0.5760D4 - (-0.180D3 * t41 * (t198 - t165 * t200) + 0.90D2 *
     # t4 * (t171 * t200 / 0.2D1 - t165 * t198 + t236) + t242) * t54 / 0
     #.5760D4 - t291 * t236 / 0.11520D5
      t295 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t294)
      t297 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t299 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t300 = z * t299
      t309 = z * t297 * t32 + t297
      t311 = 0.180D3 * t41 * t309
      t323 = t297 * t4
      t325 = 0.180D3 * t323 * lh
      t336 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t341 = t95 * t297
      t347 = t299 * t4
      t390 = -(0.90D2 * t4 * (-t36 * t297 + t299 - (-t300 + t21 * t297) 
     #* t32) - t311) * t48 * t50 / 0.5760D4 - t4 * t309 * t56 / 0.32D2 -
     # (0.90D2 * t4 * (-t63 * t297 + t299) - t325) * t54 * t50 / 0.2880D
     #4 - (-0.180D3 * t41 * (-t78 * t297 + t299) + 0.90D2 * t4 * (t84 * 
     #t297 / 0.2D1 + t336 - t78 * t299) + t341) * t50 / 0.5760D4 - t192 
     #* t297 / 0.11520D5 - ((t325 - 0.90D2 * t347) * t113 - 0.90D2 * t32
     #3 * t120 + (0.180D3 * t347 * lh - t341 - 0.90D2 * t336 * t4) * t12
     #7) * t48 / 0.11520D5 - t140 * t299 / 0.11520D5 - (0.90D2 * t4 * (-
     #(-t300 + t151 * t297) * t32 + t299 - t146 * t297) - t311) * t48 * 
     #t54 / 0.5760D4 - (-0.180D3 * t41 * (t299 - t165 * t297) + 0.90D2 *
     # t4 * (-t165 * t299 + t336 + t171 * t297 / 0.2D1) + t341) * t54 / 
     #0.5760D4 - t291 * t336 / 0.11520D5
      t391 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t390)
      t393 = t2 * x1
      t394 = -0.1D1 + x1
      t395 = x1 * z
      t396 = 0.1D1 - x1 + t395
      t397 = 0.1D1 / t396
      t399 = t2 * t394 * t397
      t400 = t1 ** 2
      t401 = s * t400
      t403 = x1 * t394 * t397
      t404 = t401 * t403
      t405 = -t394
      t406 = bggbH51J2(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t407 = z * t396
      t409 = t8 * t11
      t410 = t13 * t397
      t411 = t394 ** 2
      t416 = log(-0.4D1 * t409 * t410 * t411 * t16)
      t417 = t416 * z
      t418 = bggbH51J1(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t422 = x1 * x3
      t423 = 0.2D1 * t422
      t424 = x3 * t396
      t426 = Sqrt(-t424 * t15)
      t430 = x1 * t12
      t431 = x3 * t12
      t432 = t431 * x1
      t434 = 0.2D1 * t8 * z
      t435 = t8 * t12
      t436 = t422 * z
      t437 = 0.3D1 * t436
      t438 = -t8 - z + t423 + 0.2D1 * t25 * t426 * z - x3 + t395 - t430 
     #+ t432 + t434 - t435 - t437
      t439 = 0.1D1 / t438
      t441 = t410 * t411
      t444 = log(0.4D1 * t409 * t441)
      t451 = -t407 * t418 * t439 - t418
      t461 = t60 * t11
      t464 = log(0.4D1 * t461 * t441)
      t477 = log(0.4D1 * t75 * t441)
      t482 = bggbH51J3(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t483 = t477 ** 2
      t494 = -(0.90D2 * t4 * (-t406 - (t407 * t406 - t417 * t396 * t418)
     # * t439 + t444 * t418) - 0.180D3 * t41 * t451) * t48 * t50 / 0.576
     #0D4 - t4 * t451 * t56 / 0.32D2 - (0.90D2 * t4 * (-t406 + t464 * t4
     #18) + 0.180D3 * t41 * t418) * t54 * t50 / 0.2880D4 - (-0.180D3 * t
     #41 * (-t406 + t477 * t418) + 0.90D2 * t4 * (-t482 - t483 * t418 / 
     #0.2D1 + t477 * t406) - t95 * t418) * t50 / 0.5760D4
      t495 = FJET(XB1, XB2, s, 0.0D0, t393, -t399, 0.0D0, -t404, t494)
      t497 = x2 * s
      t498 = t497 * t1
      t499 = -0.1D1 + x2
      t500 = t2 * t499
      t501 = t14 * t499
      t504 = log(-0.4D1 * t143 * t501)
      t505 = -t499
      t506 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t508 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t513 = 0.180D3 * t41 * t506
      t518 = t13 * t499
      t521 = log(-0.4D1 * t162 * t518)
      t526 = t521 ** 2
      t542 = log(-0.4D1 * t60 * t501)
      t551 = -(0.90D2 * t4 * (t504 * t506 - t508) + t513) * t48 * t54 / 
     #0.5760D4 - (-0.180D3 * t41 * (t521 * t506 - t508) + 0.90D2 * t4 * 
     #(-t526 * t506 / 0.2D1 + t521 * t508) - t95 * t506) * t54 / 0.5760D
     #4 + t4 * t506 * t56 / 0.32D2 - (0.90D2 * t4 * (t542 * t506 - t508)
     # + t513) * t54 * t50 / 0.2880D4
      t552 = FJET(XB1, XB2, s, 0.0D0, t498, 0.0D0, -t500, 0.0D0, t551)
      t554 = x2 * x3
      t557 = Sqrt(x3 * t499 * t15)
      t558 = t25 * t557
      t560 = 0.2D1 * t558 * x2
      t562 = 0.1D1 - x3 + t554
      t563 = 0.1D1 / t562
      t565 = t2 * (0.1D1 - x2 - x3 + t554 + t143 + t560) * t563
      t570 = t2 * x2 * (t554 - 0.1D1 + 0.2D1 * t558) * t563
      t572 = t562 ** 2
      t578 = log(0.4D1 * t143 * t11 * t518 * t15 / t572)
      t579 = x2 * z
      t580 = -x2 - z + t579
      t581 = t578 * t580
      t582 = t143 * z
      t583 = t554 * z
      t589 = 0.1D1 / (x2 + z - t579 + t582 - t583 + x3 - t143 - t560 - 0
     #.2D1 * t558 * z + 0.2D1 * t558 * t579)
      t590 = t15 * t563
      t591 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t594 = t580 * t589
      t595 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t608 = t4 * t580 * t589
      t610 = t54 * t50
      t614 = -(0.90D2 * t4 * (t581 * t589 * t591 - t594 * t595) + 0.180D
     #3 * t41 * t594 * t591) * t48 * t54 / 0.5760D4 + t608 * t591 * t48 
     #* t610 / 0.32D2
      t615 = FJET(XB1, XB2, s, 0.0D0, t565, 0.0D0, -t570, 0.0D0, t614)
      t618 = t1 * t394
      t620 = t499 * s * t618 * t397
      t621 = t497 * t618
      t623 = t401 * t499 * t403
      t624 = bggbH53J1(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t628 = bggbH53J2(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t633 = log(-0.4D1 * t461 * t410 * t411 * t499)
      t644 = -t4 * t624 * t56 / 0.32D2 - (0.90D2 * t4 * (t628 - t633 * t
     #624) - 0.180D3 * t41 * t624) * t54 * t50 / 0.2880D4
      t645 = FJET(XB1, XB2, s, 0.0D0, t620, t393, -t621, t623, t644)
      t647 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t652 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t657 = 0.180D3 * t41 * t647
      t674 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t685 = t4 * t647 * t56 / 0.32D2 - (0.90D2 * t4 * (t542 * t647 - t6
     #52) + t657) * t54 * t50 / 0.2880D4 - (0.90D2 * t4 * (-t652 + t504 
     #* t647) + t657) * t48 * t54 / 0.5760D4 - (-0.180D3 * t41 * (-t652 
     #+ t521 * t647) + 0.90D2 * t4 * (-t674 - t526 * t647 / 0.2D1 + t521
     # * t652) - t95 * t647) * t54 / 0.5760D4
      t686 = FJET(XB1, XB2, s, 0.0D0, -t500, 0.0D0, t498, 0.0D0, t685)
      t688 = bggbH53J2(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t690 = bggbH53J1(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t701 = -t690 - t407 * t690 * t439
      t728 = bggbH53J3(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t736 = -(0.90D2 * t4 * (-t688 - (t407 * t688 - t417 * t396 * t690)
     # * t439 + t444 * t690) - 0.180D3 * t41 * t701) * t48 * t50 / 0.576
     #0D4 - t4 * t701 * t56 / 0.32D2 - (0.90D2 * t4 * (t464 * t690 - t68
     #8) + 0.180D3 * t41 * t690) * t54 * t50 / 0.2880D4 - (-0.180D3 * t4
     #1 * (t477 * t690 - t688) + 0.90D2 * t4 * (t477 * t688 - t483 * t69
     #0 / 0.2D1 - t728) - t95 * t690) * t50 / 0.5760D4
      t737 = FJET(XB1, XB2, s, 0.0D0, -t399, t393, 0.0D0, -t404, t736)
      t739 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t742 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t758 = -(0.90D2 * t4 * (t581 * t589 * t739 - t594 * t742) + 0.180D
     #3 * t41 * t594 * t739) * t48 * t54 / 0.5760D4 + t608 * t739 * t48 
     #* t610 / 0.32D2
      t759 = FJET(XB1, XB2, s, 0.0D0, -t570, 0.0D0, t565, 0.0D0, t758)
      t761 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t763 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t764 = z * t763
      t773 = t761 + z * t761 * t32
      t775 = 0.180D3 * t41 * t773
      t787 = t761 * t4
      t789 = 0.180D3 * t787 * lh
      t804 = t95 * t761
      t810 = t763 * t4
      t849 = -(0.90D2 * t4 * (-t36 * t761 + t763 - (-t764 + t21 * t761) 
     #* t32) - t775) * t48 * t50 / 0.5760D4 - t4 * t773 * t56 / 0.32D2 -
     # (0.90D2 * t4 * (-t63 * t761 + t763) - t789) * t54 * t50 / 0.2880D
     #4 - (-0.180D3 * t41 * (t763 - t78 * t761) + 0.90D2 * t4 * (-t78 * 
     #t763 + t84 * t761 / 0.2D1) + t804) * t50 / 0.5760D4 - t140 * t763 
     #/ 0.11520D5 - ((t789 - 0.90D2 * t810) * t113 - 0.90D2 * t787 * t12
     #0 + (0.180D3 * t810 * lh - t804) * t127) * t48 / 0.11520D5 - (0.90
     #D2 * t4 * (-(-t764 + t151 * t761) * t32 + t763 - t146 * t761) - t7
     #75) * t48 * t54 / 0.5760D4 - (-0.180D3 * t41 * (-t165 * t761 + t76
     #3) + 0.90D2 * t4 * (-t165 * t763 + t171 * t761 / 0.2D1) + t804) * 
     #t54 / 0.5760D4 - t192 * t761 / 0.11520D5
      t850 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t849)
      t852 = bggbH54J1(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t854 = bggbH54J2(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4)
      t865 = -t852 - t407 * t852 * t439
      t899 = -(0.90D2 * t4 * (t444 * t852 - t854 - (t407 * t854 - t417 *
     # t396 * t852) * t439) - 0.180D3 * t41 * t865) * t48 * t50 / 0.5760
     #D4 - t4 * t865 * t56 / 0.32D2 - (0.90D2 * t4 * (-t854 + t464 * t85
     #2) + 0.180D3 * t41 * t852) * t54 * t50 / 0.2880D4 - (-0.180D3 * t4
     #1 * (t477 * t852 - t854) + 0.90D2 * t4 * (-t483 * t852 / 0.2D1 + t
     #477 * t854) - t95 * t852) * t50 / 0.5760D4
      t900 = FJET(XB1, XB2, s, t393, 0.0D0, 0.0D0, -t399, -t404, t899)
      t902 = t196 * t195 + t295 * t294 + t391 * t390 + t495 * t494 + t55
     #2 * t551 + t615 * t614 + t645 * t644 + t686 * t685 + t737 * t736 +
     # t759 * t758 + t850 * t849 + t900 * t899
      t903 = bggbH54J1(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t908 = bggbH54J2(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t918 = -t4 * t903 * t56 / 0.32D2 - (0.90D2 * t4 * (-t633 * t903 + 
     #t908) - 0.180D3 * t41 * t903) * t54 * t50 / 0.2880D4
      t919 = FJET(XB1, XB2, s, t393, -t621, 0.0D0, t620, t623, t918)
      t921 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t926 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t931 = 0.180D3 * t41 * t921
      t948 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4)
      t959 = t4 * t921 * t56 / 0.32D2 - (0.90D2 * t4 * (t542 * t921 - t9
     #26) + t931) * t54 * t50 / 0.2880D4 - (0.90D2 * t4 * (t504 * t921 -
     # t926) + t931) * t48 * t54 / 0.5760D4 - (-0.180D3 * t41 * (t521 * 
     #t921 - t926) + 0.90D2 * t4 * (-t948 - t526 * t921 / 0.2D1 + t521 *
     # t926) - t95 * t921) * t54 / 0.5760D4
      t960 = FJET(XB1, XB2, s, t498, 0.0D0, -t500, 0.0D0, 0.0D0, t959)
      t962 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t969 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t981 = t608 * t962 * t48 * t610 / 0.32D2 - (0.90D2 * t4 * (t581 * 
     #t589 * t962 - t594 * t969) + 0.180D3 * t41 * t594 * t962) * t48 * 
     #t54 / 0.5760D4
      t982 = FJET(XB1, XB2, s, t565, 0.0D0, -t570, 0.0D0, 0.0D0, t981)
      t984 = bggbH52J1(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t988 = bggbH52J2(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t999 = -t4 * t984 * t56 / 0.32D2 - (0.90D2 * t4 * (t988 - t633 * t
     #984) - 0.180D3 * t41 * t984) * t54 * t50 / 0.2880D4
      t1000 = FJET(XB1, XB2, s, t620, 0.0D0, -t621, t393, t623, t999)
      t1003 = t393 * t554 * t563
      t1004 = t2 * t394
      t1005 = t143 * x1
      t1006 = t143 * t395
      t1009 = Sqrt(t424 * t499 * t15)
      t1010 = t25 * t1009
      t1012 = 0.2D1 * t1010 * x2
      t1016 = t1004 * (t143 - x2 + t554 + 0.1D1 - x3 - t1005 + t1006 + t
     #1012) * t397 * t563
      t1020 = t15 * s * t1 * x1 * t563
      t1026 = t1004 * x2 * (-0.1D1 + t554 + x1 - t422 - t395 + t436 + 0.
     #2D1 * t1010) * t397 * t563
      t1028 = x2 * x1
      t1029 = t1028 * z
      t1030 = z - t579 - t1028 + t1029 + x2
      t1033 = x2 * t7
      t1044 = -x2 - t554 * x1 + t1028 * t12 + 0.2D1 * t1033 * z - t1033 
     #* t12 + 0.2D1 * t1010 * z + 0.2D1 * t1010 * t1029 - t582 + t583 - 
     #t1005 + t1012 - 0.3D1 * t1029 + t8 * x2 + 0.2D1 * t1028 - t1033 + 
     #t432 + t434
      t1056 = -t435 - t437 - x3 - z + t395 - t8 + t423 - t430 - t431 * t
     #1028 - 0.2D1 * t8 * t579 + t8 * t12 * x2 + 0.2D1 * t554 * t395 + t
     #1006 - 0.2D1 * t1010 * t579 - 0.2D1 * t1010 * t1028 + t143 + t579
      t1058 = 0.1D1 / (t1044 + t1056)
      t1060 = t4 * t396 * t1030 * t1058
      t1061 = bggbH53J1(s, XB1, XB2, z, lh, wd, t405, t505, -t590, x4)
      t1066 = FJET(XB1, XB2, s, t1003, -t1016, -t1020, t1026, t623, -t10
     #60 * t1061 * t48 * t610 / 0.32D2)
      t1068 = t396 * t1030
      t1074 = bggbH51J1(s, XB1, XB2, z, lh, wd, t405, t505, -t590, x4)
      t1079 = FJET(XB1, XB2, s, t1026, -t1020, -t1016, t1003, t623, -t10
     #60 * t1074 * t48 * t610 / 0.32D2)
      t1086 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4
     #)
      t1090 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, 0.10D1, x4
     #)
      t1096 = 0.180D3 * t41 * t1086
      t1123 = t4 * t1086 * t56 / 0.32D2 - (0.90D2 * t4 * (-t1090 + t542 
     #* t1086) + t1096) * t54 * t50 / 0.2880D4 - (0.90D2 * t4 * (t504 * 
     #t1086 - t1090) + t1096) * t48 * t54 / 0.5760D4 - (-0.180D3 * t41 *
     # (t521 * t1086 - t1090) + 0.90D2 * t4 * (-t526 * t1086 / 0.2D1 + t
     #521 * t1090) - t95 * t1086) * t54 / 0.5760D4
      t1124 = FJET(XB1, XB2, s, -t500, 0.0D0, t498, 0.0D0, 0.0D0, t1123)
      t1126 = bggbH52J1(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4
     #)
      t1128 = bggbH52J2(s, XB1, XB2, z, lh, wd, t405, 0.10D1, 0.10D1, x4
     #)
      t1139 = -t1126 - t407 * t1126 * t439
      t1173 = -(0.90D2 * t4 * (t444 * t1126 - t1128 - (t407 * t1128 - t4
     #17 * t396 * t1126) * t439) - 0.180D3 * t41 * t1139) * t48 * t50 / 
     #0.5760D4 - t4 * t1139 * t56 / 0.32D2 - (0.90D2 * t4 * (-t1128 + t4
     #64 * t1126) + 0.180D3 * t41 * t1126) * t54 * t50 / 0.2880D4 - (-0.
     #180D3 * t41 * (-t1128 + t477 * t1126) + 0.90D2 * t4 * (-t483 * t11
     #26 / 0.2D1 + t477 * t1128) - t95 * t1126) * t50 / 0.5760D4
      t1174 = FJET(XB1, XB2, s, -t399, 0.0D0, 0.0D0, t393, -t404, t1173)
      t1176 = bggbH51J1(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t1180 = bggbH51J2(s, XB1, XB2, z, lh, wd, t405, t505, 0.10D1, x4)
      t1191 = -t4 * t1176 * t56 / 0.32D2 - (0.90D2 * t4 * (t1180 - t633 
     #* t1176) - 0.180D3 * t41 * t1176) * t54 * t50 / 0.2880D4
      t1192 = FJET(XB1, XB2, s, -t621, t393, t620, 0.0D0, t623, t1191)
      t1194 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t1199 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t505, -t590, x4)
      t1213 = t608 * t1194 * t48 * t610 / 0.32D2 - (0.90D2 * t4 * (-t594
     # * t1199 + t581 * t589 * t1194) + 0.180D3 * t41 * t594 * t1194) * 
     #t48 * t54 / 0.5760D4
      t1214 = FJET(XB1, XB2, s, -t570, 0.0D0, t565, 0.0D0, 0.0D0, t1213)
      t1216 = bggbH54J1(s, XB1, XB2, z, lh, wd, t405, t505, -t590, x4)
      t1221 = FJET(XB1, XB2, s, -t1020, t1026, t1003, -t1016, t623, -t10
     #60 * t1216 * t48 * t610 / 0.32D2)
      t1228 = bggbH52J1(s, XB1, XB2, z, lh, wd, t405, t505, -t590, x4)
      t1233 = FJET(XB1, XB2, s, -t1016, t1003, t1026, -t1020, t623, -t10
     #60 * t1228 * t48 * t610 / 0.32D2)
      t1240 = t919 * t918 + t960 * t959 + t982 * t981 + t1000 * t999 - t
     #1066 * t4 * t1068 * t1058 * t1061 * t56 / 0.32D2 - t1079 * t4 * t1
     #068 * t1058 * t1074 * t56 / 0.32D2 + t1124 * t1123 + t1174 * t1173
     # + t1192 * t1191 + t1214 * t1213 - t1221 * t4 * t1068 * t1058 * t1
     #216 * t56 / 0.32D2 - t1233 * t4 * t1068 * t1058 * t1228 * t56 / 0.
     #32D2
      bggbH5n1e0 = t902 + t1240

      end function



      doubleprecision function bggbH5n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t7 = z ** 2
      t8 = 0.1D1 / t7
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t14 = log(0.4D1 * t8 * t11)
      t15 = t14 * t4
      t17 = -0.180D3 * t5 - 0.90D2 * t15
      t18 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t21 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t23 = cos(t9)
      t24 = -0.1D1 + x3
      t26 = Sqrt(-x3 * t24)
      t31 = 0.1D1 / (-z - x3 + 0.2D1 * t23 * t26 * z)
      t34 = t4 * (z * t21 * t31 + t21)
      t35 = 0.1D1 / x3
      t36 = 0.1D1 / x2
      t37 = t35 * t36
      t40 = x2 ** 2
      t41 = t40 * t11
      t44 = log(0.4D1 * t41 * t8)
      t49 = t21 * t4
      t51 = 0.180D3 * t49 * lh
      t57 = lh ** 2
      t59 = 0.3141592653589793D1 ** 2
      t63 = t14 ** 2
      t66 = 0.180D3 * t15 * lh + t4 * (0.180D3 * t57 - 0.30D2 * t59) + 0
     #.45D2 * t63 * t4
      t69 = x1 ** 2
      t70 = t69 * t11
      t73 = log(0.4D1 * t70 * t8)
      t79 = 0.1D1 / x1
      t82 = t35 * t79
      t85 = t36 * t79
      t88 = x3 * t11
      t93 = log(-0.4D1 * t88 * t8 / t24)
      t98 = log(0.4D1 * t88 * t8)
      t99 = t93 * z * t31 + t98
      t106 = -z * t31 - 0.1D1
      t111 = -t17 * t18 / 0.11520D5 - t34 * t37 / 0.64D2 - (0.90D2 * t4 
     #* (t18 - t44 * t21) - t51) * t36 / 0.5760D4 - t66 * t21 / 0.11520D
     #5 - (0.90D2 * t4 * (t18 - t73 * t21) - t51) * t79 / 0.5760D4 - t34
     # * t82 / 0.64D2 - t49 * t85 / 0.32D2 - (-0.90D2 * t49 * t99 + (t51
     # - 0.90D2 * t18 * t4) * t106) * t35 / 0.11520D5
      t112 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t111)
      t114 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t115 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t120 = t115 * t4
      t122 = 0.180D3 * t120 * lh
      t129 = t4 * (t115 + z * t115 * t31)
      t147 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t159 = -(0.90D2 * t4 * (t114 - t73 * t115) - t122) * t79 / 0.5760D
     #4 - t129 * t82 / 0.64D2 - t120 * t85 / 0.32D2 - t17 * t114 / 0.115
     #20D5 - t129 * t37 / 0.64D2 - (0.90D2 * t4 * (t114 - t44 * t115) - 
     #t122) * t36 / 0.5760D4 - t66 * t115 / 0.11520D5 - t147 * t4 / 0.12
     #8D3 - (-0.90D2 * t120 * t99 + (t122 - 0.90D2 * t114 * t4) * t106) 
     #* t35 / 0.11520D5
      t160 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t159)
      t162 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t165 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t170 = t165 * t4
      t172 = 0.180D3 * t170 * lh
      t179 = t4 * (z * t165 * t31 + t165)
      t186 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t207 = -t17 * t162 / 0.11520D5 - (0.90D2 * t4 * (-t73 * t165 + t16
     #2) - t172) * t79 / 0.5760D4 - t179 * t82 / 0.64D2 - t170 * t85 / 0
     #.32D2 - t66 * t165 / 0.11520D5 - t186 * t4 / 0.128D3 - t179 * t37 
     #/ 0.64D2 - (0.90D2 * t4 * (t162 - t44 * t165) - t172) * t36 / 0.57
     #60D4 - (-0.90D2 * t170 * t99 + (t172 - 0.90D2 * t162 * t4) * t106)
     # * t35 / 0.11520D5
      t208 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = t2 * x1
      t211 = -0.1D1 + x1
      t212 = x1 * z
      t213 = 0.1D1 - x1 + t212
      t214 = 0.1D1 / t213
      t216 = t2 * t211 * t214
      t217 = t1 ** 2
      t218 = s * t217
      t220 = x1 * t211 * t214
      t221 = t218 * t220
      t222 = -t211
      t223 = bggbH51J2(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t225 = t211 ** 2
      t229 = log(0.4D1 * t70 * t8 * t214 * t225)
      t230 = bggbH51J1(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t240 = z * t213
      t241 = t69 * x3
      t242 = x1 * x3
      t246 = Sqrt(-x3 * t213 * t24)
      t258 = -t241 - z + 0.2D1 * t242 + 0.2D1 * t23 * t246 * z - x3 + t2
     #12 - x1 * t7 + x3 * t7 * x1 + 0.2D1 * t241 * z - t241 * t7 - 0.3D1
     # * t242 * z
      t259 = 0.1D1 / t258
      t269 = -(0.90D2 * t4 * (-t223 + t229 * t230) + 0.180D3 * t5 * t230
     #) * t79 / 0.5760D4 - t4 * (-t240 * t230 * t259 - t230) * t82 / 0.6
     #4D2 + t4 * t230 * t85 / 0.32D2
      t270 = FJET(XB1, XB2, s, 0.0D0, t210, -t216, 0.0D0, -t221, t269)
      t272 = x2 * s
      t273 = t272 * t1
      t274 = -0.1D1 + x2
      t275 = t2 * t274
      t276 = -t274
      t277 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t278 = t4 * t277
      t286 = log(-0.4D1 * t41 * t8 * t274)
      t288 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t297 = t278 * t85 / 0.32D2 + t278 * t37 / 0.64D2 - (0.90D2 * t4 * 
     #(t286 * t277 - t288) + 0.180D3 * t5 * t277) * t36 / 0.5760D4
      t298 = FJET(XB1, XB2, s, 0.0D0, t273, 0.0D0, -t275, 0.0D0, t297)
      t300 = x2 * x3
      t301 = x3 * t40
      t304 = Sqrt(x3 * t274 * t24)
      t305 = t23 * t304
      t307 = 0.2D1 * t305 * x2
      t310 = 0.1D1 / (0.1D1 - x3 + t300)
      t312 = t2 * (0.1D1 - x2 - x3 + t300 + t301 + t307) * t310
      t317 = t2 * x2 * (t300 - 0.1D1 + 0.2D1 * t305) * t310
      t318 = x2 * z
      t319 = -x2 - z + t318
      t328 = 0.1D1 / (x2 + z - t318 + t301 * z - t300 * z + x3 - t301 - 
     #t307 - 0.2D1 * t305 * z + 0.2D1 * t305 * t318)
      t329 = t4 * t319 * t328
      t330 = t24 * t310
      t331 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, -t330, x4)
      t336 = FJET(XB1, XB2, s, 0.0D0, t312, 0.0D0, -t317, 0.0D0, t329 * 
     #t331 * t35 * t36 / 0.64D2)
      t344 = t1 * t211
      t346 = t274 * s * t344 * t214
      t347 = t272 * t344
      t349 = t218 * t274 * t220
      t350 = bggbH53J1(s, XB1, XB2, z, lh, wd, t222, t276, 0.10D1, x4)
      t354 = FJET(XB1, XB2, s, 0.0D0, t346, t210, -t347, t349, -t4 * t35
     #0 * t85 / 0.32D2)
      t360 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t361 = t4 * t360
      t364 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t376 = t361 * t37 / 0.64D2 - (0.90D2 * t4 * (-t364 + t286 * t360) 
     #+ 0.180D3 * t5 * t360) * t36 / 0.5760D4 + t361 * t85 / 0.32D2
      t377 = FJET(XB1, XB2, s, 0.0D0, -t275, 0.0D0, t273, 0.0D0, t376)
      t379 = bggbH53J1(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t381 = bggbH53J2(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t399 = -(0.90D2 * t4 * (t229 * t379 - t381) + 0.180D3 * t5 * t379)
     # * t79 / 0.5760D4 - t4 * (-t379 - t240 * t379 * t259) * t82 / 0.64
     #D2 + t4 * t379 * t85 / 0.32D2
      t400 = FJET(XB1, XB2, s, 0.0D0, -t216, t210, 0.0D0, -t221, t399)
      t402 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, -t330, x4)
      t407 = FJET(XB1, XB2, s, 0.0D0, -t317, 0.0D0, t312, 0.0D0, t329 * 
     #t402 * t35 * t36 / 0.64D2)
      t414 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t415 = t414 * t4
      t419 = 0.180D3 * t415 * lh
      t420 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t442 = t4 * (t414 + z * t414 * t31)
      t456 = -(-0.90D2 * t415 * t99 + (t419 - 0.90D2 * t420 * t4) * t106
     #) * t35 / 0.11520D5 - t66 * t414 / 0.11520D5 - t17 * t420 / 0.1152
     #0D5 - (0.90D2 * t4 * (t420 - t73 * t414) - t419) * t79 / 0.5760D4 
     #- t442 * t82 / 0.64D2 - t415 * t85 / 0.32D2 - t442 * t37 / 0.64D2 
     #- (0.90D2 * t4 * (-t44 * t414 + t420) - t419) * t36 / 0.5760D4
      t457 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t456)
      t459 = bggbH54J1(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t461 = bggbH54J2(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t479 = -(0.90D2 * t4 * (t229 * t459 - t461) + 0.180D3 * t5 * t459)
     # * t79 / 0.5760D4 - t4 * (-t459 - t240 * t459 * t259) * t82 / 0.64
     #D2 + t4 * t459 * t85 / 0.32D2
      t480 = FJET(XB1, XB2, s, t210, 0.0D0, 0.0D0, -t216, -t221, t479)
      t482 = bggbH54J1(s, XB1, XB2, z, lh, wd, t222, t276, 0.10D1, x4)
      t486 = FJET(XB1, XB2, s, t210, -t347, 0.0D0, t346, t349, -t4 * t48
     #2 * t85 / 0.32D2)
      t492 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t493 = t4 * t492
      t499 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t508 = t493 * t85 / 0.32D2 + t493 * t37 / 0.64D2 - (0.90D2 * t4 * 
     #(t286 * t492 - t499) + 0.180D3 * t5 * t492) * t36 / 0.5760D4
      t509 = FJET(XB1, XB2, s, t273, 0.0D0, -t275, 0.0D0, 0.0D0, t508)
      t511 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, -t330, x4)
      t516 = FJET(XB1, XB2, s, t312, 0.0D0, -t317, 0.0D0, 0.0D0, t329 * 
     #t511 * t35 * t36 / 0.64D2)
      t523 = bggbH52J1(s, XB1, XB2, z, lh, wd, t222, t276, 0.10D1, x4)
      t527 = FJET(XB1, XB2, s, t346, 0.0D0, -t347, t210, t349, -t4 * t52
     #3 * t85 / 0.32D2)
      t533 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t534 = t4 * t533
      t540 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, t276, 0.10D1, x4)
      t549 = t534 * t85 / 0.32D2 + t534 * t37 / 0.64D2 - (0.90D2 * t4 * 
     #(t286 * t533 - t540) + 0.180D3 * t5 * t533) * t36 / 0.5760D4
      t550 = FJET(XB1, XB2, s, -t275, 0.0D0, t273, 0.0D0, 0.0D0, t549)
      t552 = bggbH52J2(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t553 = bggbH52J1(s, XB1, XB2, z, lh, wd, t222, 0.10D1, 0.10D1, x4)
      t572 = -(0.90D2 * t4 * (-t552 + t229 * t553) + 0.180D3 * t5 * t553
     #) * t79 / 0.5760D4 - t4 * (-t553 - t240 * t553 * t259) * t82 / 0.6
     #4D2 + t4 * t553 * t85 / 0.32D2
      t573 = FJET(XB1, XB2, s, -t216, 0.0D0, 0.0D0, t210, -t221, t572)
      t575 = bggbH51J1(s, XB1, XB2, z, lh, wd, t222, t276, 0.10D1, x4)
      t579 = FJET(XB1, XB2, s, -t347, t210, t346, 0.0D0, t349, -t4 * t57
     #5 * t85 / 0.32D2)
      t585 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t276, -t330, x4)
      t590 = FJET(XB1, XB2, s, -t317, 0.0D0, t312, 0.0D0, 0.0D0, t329 * 
     #t585 * t35 * t36 / 0.64D2)
      bggbH5n1em1 = t112 * t111 + t160 * t159 + t208 * t207 + t270 * t26
     #9 + t298 * t297 + t336 * t4 * t319 * t328 * t331 * t37 / 0.64D2 - 
     #t354 * t4 * t350 * t36 * t79 / 0.32D2 + t377 * t376 + t400 * t399 
     #+ t407 * t4 * t319 * t328 * t402 * t37 / 0.64D2 + t457 * t456 + t4
     #80 * t479 - t486 * t4 * t482 * t36 * t79 / 0.32D2 + t509 * t508 + 
     #t516 * t4 * t319 * t328 * t511 * t37 / 0.64D2 - t527 * t4 * t523 *
     # t36 * t79 / 0.32D2 + t550 * t549 + t573 * t572 - t579 * t4 * t575
     # * t36 * t79 / 0.32D2 + t590 * t4 * t319 * t328 * t585 * t37 / 0.6
     #4D2

      end function



      doubleprecision function bggbH5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t18 = z ** 2
      t20 = x4 * 0.3141592653589793D1
      t21 = Sin(t20)
      t22 = t21 ** 2
      t25 = log(0.4D1 / t18 * t22)
      t28 = -0.180D3 * t5 * lh - 0.90D2 * t25 * t5
      t31 = cos(t20)
      t34 = Sqrt(-x3 * (-0.1D1 + x3))
      t43 = (-z / (-z - x3 + 0.2D1 * t31 * t34 * z) - 0.1D1) / x3
      t46 = -t6 * t7 / 0.64D2 - t6 * t10 / 0.64D2 - t13 * t5 / 0.128D3 -
     # t28 * t3 / 0.11520D5 + t6 * t43 / 0.128D3
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t46)
      t49 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t50 = t49 * t5
      t55 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t62 = -t50 * t7 / 0.64D2 - t50 * t10 / 0.64D2 - t55 * t5 / 0.128D3
     # - t28 * t49 / 0.11520D5 + t50 * t43 / 0.128D3
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t62)
      t65 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t66 = t65 * t5
      t71 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t78 = -t66 * t7 / 0.64D2 - t66 * t10 / 0.64D2 - t71 * t5 / 0.128D3
     # - t28 * t65 / 0.11520D5 + t66 * t43 / 0.128D3
      t79 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t78)
      t81 = t2 * x1
      t82 = -0.1D1 + x1
      t85 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t87 = t2 * t82 * t85
      t88 = t1 ** 2
      t92 = s * t88 * x1 * t82 * t85
      t93 = -t82
      t94 = bggbH51J1(s, XB1, XB2, z, lh, wd, t93, 0.10D1, 0.10D1, x4)
      t98 = FJET(XB1, XB2, s, 0.0D0, t81, -t87, 0.0D0, -t92, t5 * t94 * 
     #t7 / 0.64D2)
      t104 = x2 * s * t1
      t105 = -0.1D1 + x2
      t106 = t2 * t105
      t107 = -t105
      t108 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, t107, 0.10D1, x4)
      t112 = FJET(XB1, XB2, s, 0.0D0, t104, 0.0D0, -t106, 0.0D0, t5 * t1
     #08 * t10 / 0.64D2)
      t117 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, t107, 0.10D1, x4)
      t121 = FJET(XB1, XB2, s, 0.0D0, -t106, 0.0D0, t104, 0.0D0, t5 * t1
     #17 * t10 / 0.64D2)
      t126 = bggbH53J1(s, XB1, XB2, z, lh, wd, t93, 0.10D1, 0.10D1, x4)
      t130 = FJET(XB1, XB2, s, 0.0D0, -t87, t81, 0.0D0, -t92, t5 * t126 
     #* t7 / 0.64D2)
      t135 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t136 = t5 * t135
      t145 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x
     #4)
      t148 = -t136 * t7 / 0.64D2 - t28 * t135 / 0.11520D5 + t136 * t43 /
     # 0.128D3 - t136 * t10 / 0.64D2 - t145 * t5 / 0.128D3
      t149 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t148)
      t151 = bggbH54J1(s, XB1, XB2, z, lh, wd, t93, 0.10D1, 0.10D1, x4)
      t155 = FJET(XB1, XB2, s, t81, 0.0D0, 0.0D0, -t87, -t92, t5 * t151 
     #* t7 / 0.64D2)
      t160 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, t107, 0.10D1, x4)
      t164 = FJET(XB1, XB2, s, t104, 0.0D0, -t106, 0.0D0, 0.0D0, t5 * t1
     #60 * t10 / 0.64D2)
      t169 = bggbH52J1(s, XB1, XB2, z, lh, wd, t93, 0.10D1, 0.10D1, x4)
      t173 = FJET(XB1, XB2, s, -t87, 0.0D0, 0.0D0, t81, -t92, t5 * t169 
     #* t7 / 0.64D2)
      t178 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, t107, 0.10D1, x4)
      t182 = FJET(XB1, XB2, s, -t106, 0.0D0, t104, 0.0D0, 0.0D0, t5 * t1
     #78 * t10 / 0.64D2)
      bggbH5n1em2 = t47 * t46 + t63 * t62 + t79 * t78 + t98 * t5 * t94 *
     # t7 / 0.64D2 + t112 * t5 * t108 * t10 / 0.64D2 + t121 * t5 * t117 
     #* t10 / 0.64D2 + t130 * t5 * t126 * t7 / 0.64D2 + t149 * t148 + t1
     #55 * t5 * t151 * t7 / 0.64D2 + t164 * t5 * t160 * t10 / 0.64D2 + t
     #173 * t5 * t169 * t7 / 0.64D2 + t182 * t5 * t178 * t10 / 0.64D2

      end function



      doubleprecision function bggbH5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t2 = s * (-0.1D1 + z)
      t3 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4)
      t4 = s ** 2
      t5 = 0.1D1 / t4
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t3 * t5 / 
     #0.128D3)
      t11 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t11 * t5 
     #/ 0.128D3)
      t17 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t17 * t5 
     #/ 0.128D3)
      t23 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.10D1, 0.10D1, x4
     #)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t5 * t23 
     #/ 0.128D3)
      bggbH5n1em3 = -t8 * t3 * t5 / 0.128D3 - t14 * t11 * t5 / 0.128D3 -
     # t20 * t17 * t5 / 0.128D3 - t26 * t5 * t23 / 0.128D3

      end function



      doubleprecision function bggbH5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      bggbH5n1em4 = 0.0D0

      end function


      doubleprecision function bggbH5n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x2
      t4 = x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x3
      t9 = Sqrt(-t4 * t7)
      t10 = t6 * t9
      t11 = 0.2D1 * t10
      t14 = t4 - 0.1D1
      t15 = 0.1D1 / t14
      t17 = t2 * t3 * (-t4 - 0.1D1 + x3 + t11) * t15
      t18 = 0.3D1 * t4
      t19 = x2 ** 2
      t20 = x3 * t19
      t22 = 0.2D1 * t10 * x2
      t25 = t2 * (-x2 + t18 - t20 - x3 - t11 + t22) * t15
      t26 = s ** 2
      t27 = 0.1D1 / t26
      t28 = x2 * z
      t29 = 0.1D1 - x2 + t28
      t30 = t20 * z
      t33 = t4 * z
      t34 = 0.2D1 * t4
      t36 = 0.1D1 / (t20 - t30 + 0.2D1 * t10 * t28 - t22 - t28 + t33 + x
     #2 - t34 - 0.1D1 + t11)
      t37 = t29 * t36
      t38 = t7 * t15
      t39 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t40 = t37 * t39
      t41 = x1 ** 2
      t42 = Sin(t5)
      t43 = t42 ** 2
      t44 = t41 * t43
      t45 = t4 * t44
      t46 = z ** 2
      t47 = 0.1D1 / t46
      t48 = t3 ** 2
      t49 = t47 * t48
      t50 = t14 ** 2
      t51 = 0.1D1 / t50
      t53 = t49 * t7 * t51
      t56 = log(-0.4D1 * t45 * t53)
      t57 = t56 * t29
      t58 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t59 = t36 * t58
      t64 = t27 * lh
      t65 = t37 * t58
      t69 = 0.1D1 / x3
      t71 = 0.1D1 / x2
      t72 = 0.1D1 / x1
      t73 = t71 * t72
      t79 = log(-0.4D1 * t4 * t43 * t53)
      t80 = t79 * t29
      t87 = t79 ** 2
      t88 = t87 * t29
      t91 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t96 = lh ** 2
      t98 = 0.3141592653589793D1 ** 2
      t100 = 0.180D3 * t96 - 0.30D2 * t98
      t101 = t27 * t100
      t107 = -(0.90D2 * t27 * (-t40 + t57 * t59) + 0.180D3 * t64 * t65) 
     #* t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (t40 - t80 * t59) + 0.9
     #0D2 * t27 * (-t80 * t36 * t39 + t88 * t59 / 0.2D1 + t37 * t91) + t
     #101 * t65) * t69 * t71 / 0.5760D4
      t108 = FJET(XB1, XB2, s, -t17, 0.0D0, t25, 0.0D0, 0.0D0, t107)
      t110 = -0.1D1 + x1
      t111 = t2 * t110
      t112 = t2 * x1
      t113 = x1 * z
      t114 = 0.1D1 - x1 + t113
      t115 = 0.1D1 / t114
      t116 = t47 * t115
      t117 = t110 ** 2
      t118 = t116 * t117
      t121 = log(0.4D1 * t44 * t118)
      t122 = t121 ** 2
      t123 = -t110
      t124 = bggbH52J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t127 = bggbH52J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t135 = t122 * t121
      t147 = -0.2884936567583026D3 - 0.120D3 * t96 * lh + 0.60D2 * lh * 
     #t98
      t148 = t27 * t147
      t152 = x3 * t41
      t153 = t152 * t43
      t156 = log(0.4D1 * t153 * t118)
      t161 = t156 ** 2
      t168 = t101 * t124
      t172 = t152 * x2
      t173 = t47 * t43
      t174 = t115 * t117
      t178 = log(0.4D1 * t172 * t173 * t174)
      t188 = x2 * t41
      t189 = t188 * t43
      t192 = log(0.4D1 * t189 * t118)
      t197 = t192 ** 2
      t208 = (-0.180D3 * t64 * (t122 * t124 / 0.2D1 - t121 * t127) + t10
     #1 * (-t121 * t124 + t127) + 0.90D2 * t27 * (-t135 * t124 / 0.6D1 +
     # t122 * t127 / 0.2D1) + t148 * t124) * t72 / 0.2880D4 - (-0.180D3 
     #* t64 * (-t127 + t156 * t124) + 0.90D2 * t27 * (-t161 * t124 / 0.2
     #D1 + t156 * t127) - t168) * t69 * t72 / 0.2880D4 - (0.90D2 * t27 *
     # (-t127 + t178 * t124) + 0.180D3 * t64 * t124) * t69 * t73 / 0.288
     #0D4 + (-0.180D3 * t64 * (-t192 * t124 + t127) + 0.90D2 * t27 * (t1
     #97 * t124 / 0.2D1 - t192 * t127) + t168) * t71 * t72 / 0.2880D4
      t209 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t111, t112, 0.0D0, t208)
      t212 = t1 * t110
      t213 = t3 * s * t212
      t216 = t2 * t110 * x2 * t115
      t217 = t1 ** 2
      t222 = s * t217 * x2 * t110 * x1 * t115
      t224 = t116 * t117 * t48
      t227 = log(0.4D1 * t45 * t224)
      t228 = bggbH51J1(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t230 = bggbH51J2(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t241 = log(0.4D1 * t189 * t224)
      t247 = bggbH51J3(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t248 = t241 ** 2
      t259 = -(0.90D2 * t27 * (-t227 * t228 + t230) - 0.180D3 * t64 * t2
     #28) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (t241 * t228 - t230
     #) + 0.90D2 * t27 * (t241 * t230 - t247 - t248 * t228 / 0.2D1) - t1
     #01 * t228) * t71 * t72 / 0.2880D4
      t260 = FJET(XB1, XB2, s, t213, t112, -t216, 0.0D0, -t222, t259)
      t263 = t2 * t110 * x3
      t264 = x1 * x3
      t265 = t2 * t264
      t266 = t7 * s
      t267 = t266 * t212
      t268 = t1 * x1
      t269 = t266 * t268
      t270 = -t7
      t271 = bggbH52J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t273 = t116 * t117 * t7
      t276 = log(-0.4D1 * t153 * t273)
      t277 = bggbH52J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t282 = t276 ** 2
      t295 = log(-0.4D1 * t45 * t273)
      t306 = -(-0.180D3 * t64 * (t271 - t276 * t277) + 0.90D2 * t27 * (t
     #282 * t277 / 0.2D1 - t276 * t271) + t101 * t277) * t69 * t72 / 0.2
     #880D4 - (0.90D2 * t27 * (t271 - t295 * t277) - 0.180D3 * t64 * t27
     #7) * t69 * t73 / 0.2880D4
      t307 = FJET(XB1, XB2, s, -t263, t265, t267, -t269, 0.0D0, t306)
      t309 = t2 * t7
      t310 = t2 * x3
      t311 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t312 = t173 * t7
      t315 = log(-0.4D1 * t152 * t312)
      t316 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t321 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t322 = t315 ** 2
      t329 = t101 * t316
      t336 = log(-0.4D1 * t172 * t312)
      t349 = log(-0.4D1 * t4 * t312)
      t355 = t349 ** 2
      t365 = x3 * t43
      t369 = log(-0.4D1 * t365 * t47 * t7)
      t370 = t369 ** 2
      t383 = t370 * t369
      t393 = -(-0.180D3 * t64 * (-t311 + t315 * t316) + 0.90D2 * t27 * (
     #-t321 - t322 * t316 / 0.2D1 + t315 * t311) - t329) * t69 * t72 / 0
     #.2880D4 - (0.90D2 * t27 * (t336 * t316 - t311) + 0.180D3 * t64 * t
     #316) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t349 * t316 + t3
     #11) + 0.90D2 * t27 * (-t349 * t311 + t321 + t355 * t316 / 0.2D1) +
     # t329) * t69 * t71 / 0.5760D4 + (-0.180D3 * t64 * (t370 * t316 / 0
     #.2D1 - t369 * t311 + t321) + t101 * (-t369 * t316 + t311) + 0.90D2
     # * t27 * (t370 * t311 / 0.2D1 - t369 * t321 - t383 * t316 / 0.6D1)
     # + t148 * t316) * t69 / 0.5760D4
      t394 = FJET(XB1, XB2, s, -t309, 0.0D0, t310, 0.0D0, 0.0D0, t393)
      t396 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t397 = t44 * t47
      t399 = log(0.4D1 * t397)
      t400 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t402 = t399 ** 2
      t403 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t413 = t402 * t399
      t421 = t148 * t403
      t427 = log(0.4D1 * t152 * t173)
      t433 = t427 ** 2
      t439 = t101 * t403
      t444 = t4 * t397
      t446 = log(0.4D1 * t444)
      t459 = log(0.4D1 * t188 * t173)
      t465 = t459 ** 2
      t476 = log(0.4D1 * t173)
      t477 = t476 * t27
      t480 = t476 ** 2
      t481 = t480 * t27
      t483 = 0.180D3 * t477 * lh + t101 + 0.45D2 * t481
      t490 = t480 * t476 * t27
      t492 = -t477 * t100 - 0.90D2 * t481 * lh + t148 - 0.15D2 * t490
      t495 = t480 ** 2
      t500 = t98 ** 2
      t501 = t96 ** 2
      t511 = 0.15D2 / 0.4D1 * t495 * t27 - t477 * t147 + t27 * (0.576987
     #3135166051D3 * lh + t500 + 0.60D2 * t501 - 0.60D2 * t96 * t98) + t
     #481 * t100 / 0.2D1 + 0.30D2 * t490 * lh
      t516 = log(0.4D1 * t365 * t47)
      t518 = t516 ** 2
      t528 = t518 * t516
      t539 = x2 * t43
      t542 = log(0.4D1 * t539 * t47)
      t544 = t542 ** 2
      t556 = t544 * t542
      t567 = log(0.4D1 * t4 * t173)
      t573 = t567 ** 2
      t583 = (-0.180D3 * t64 * (-t396 + t399 * t400 - t402 * t403 / 0.2D
     #1) + t101 * (-t400 + t399 * t403) + 0.90D2 * t27 * (t399 * t396 + 
     #t413 * t403 / 0.6D1 - t402 * t400 / 0.2D1) - t421) * t72 / 0.2880D
     #4 - (-0.180D3 * t64 * (t400 - t427 * t403) + 0.90D2 * t27 * (t396 
     #- t427 * t400 + t433 * t403 / 0.2D1) + t439) * t69 * t72 / 0.2880D
     #4 - (0.90D2 * t27 * (t400 - t446 * t403) - 0.180D3 * t64 * t403) *
     # t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t400 + t459 * t403) + 
     #0.90D2 * t27 * (-t396 + t459 * t400 - t465 * t403 / 0.2D1) - t439)
     # * t71 * t72 / 0.2880D4 - t483 * t396 / 0.5760D4 - t492 * t400 / 0
     #.5760D4 - t511 * t403 / 0.5760D4 + (-0.180D3 * t64 * (-t396 + t516
     # * t400 - t518 * t403 / 0.2D1) + t101 * (-t400 + t516 * t403) + 0.
     #90D2 * t27 * (t516 * t396 + t528 * t403 / 0.6D1 - t518 * t400 / 0.
     #2D1) - t421) * t69 / 0.5760D4 - (-0.180D3 * t64 * (t396 - t542 * t
     #400 + t544 * t403 / 0.2D1) + t101 * (t400 - t542 * t403) + 0.90D2 
     #* t27 * (-t542 * t396 + t544 * t400 / 0.2D1 - t556 * t403 / 0.6D1)
     # + t421) * t71 / 0.5760D4 + (-0.180D3 * t64 * (-t400 + t567 * t403
     #) + 0.90D2 * t27 * (-t396 + t567 * t400 - t573 * t403 / 0.2D1) - t
     #439) * t69 * t71 / 0.5760D4
      t584 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t583)
      t586 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t587 = t36 * t586
      t589 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t590 = t37 * t589
      t598 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t603 = t37 * t586
      t619 = (-0.180D3 * t64 * (-t80 * t587 + t590) + 0.90D2 * t27 * (-t
     #80 * t36 * t589 + t88 * t587 / 0.2D1 + t37 * t598) + t101 * t603) 
     #* t69 * t71 / 0.5760D4 - (0.90D2 * t27 * (t57 * t587 - t590) + 0.1
     #80D3 * t64 * t603) * t69 * t73 / 0.2880D4
      t620 = FJET(XB1, XB2, s, 0.0D0, t25, 0.0D0, -t17, 0.0D0, t619)
      t622 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t623 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t626 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t642 = t148 * t623
      t656 = t101 * t623
      t745 = (-0.180D3 * t64 * (-t622 - t402 * t623 / 0.2D1 + t399 * t62
     #6) + t101 * (t399 * t623 - t626) + 0.90D2 * t27 * (t413 * t623 / 0
     #.6D1 + t399 * t622 - t402 * t626 / 0.2D1) - t642) * t72 / 0.2880D4
     # - (-0.180D3 * t64 * (t626 - t427 * t623) + 0.90D2 * t27 * (t433 *
     # t623 / 0.2D1 - t427 * t626 + t622) + t656) * t69 * t72 / 0.2880D4
     # - (0.90D2 * t27 * (-t446 * t623 + t626) - 0.180D3 * t64 * t623) *
     # t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t626 + t459 * t623) + 
     #0.90D2 * t27 * (-t465 * t623 / 0.2D1 - t622 + t459 * t626) - t656)
     # * t71 * t72 / 0.2880D4 - (-0.180D3 * t64 * (t622 - t542 * t626 + 
     #t544 * t623 / 0.2D1) + t101 * (t626 - t542 * t623) + 0.90D2 * t27 
     #* (-t542 * t622 + t544 * t626 / 0.2D1 - t556 * t623 / 0.6D1) + t64
     #2) * t71 / 0.5760D4 + (-0.180D3 * t64 * (-t626 + t567 * t623) + 0.
     #90D2 * t27 * (-t573 * t623 / 0.2D1 + t567 * t626 - t622) - t656) *
     # t69 * t71 / 0.5760D4 + (-0.180D3 * t64 * (-t622 - t518 * t623 / 0
     #.2D1 + t516 * t626) + t101 * (t516 * t623 - t626) + 0.90D2 * t27 *
     # (-t518 * t626 / 0.2D1 + t516 * t622 + t528 * t623 / 0.6D1) - t642
     #) * t69 / 0.5760D4 - t483 * t622 / 0.5760D4 - t511 * t623 / 0.5760
     #D4 - t492 * t626 / 0.5760D4
      t746 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t745)
      t748 = t2 * t3
      t750 = x2 * s * t1
      t751 = t173 * t48
      t754 = log(0.4D1 * t172 * t751)
      t755 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t757 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t769 = log(0.4D1 * t188 * t751)
      t774 = t769 ** 2
      t777 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t782 = t101 * t755
      t789 = log(0.4D1 * t539 * t49)
      t791 = t789 ** 2
      t803 = t791 * t789
      t815 = log(0.4D1 * t4 * t751)
      t820 = t815 ** 2
      t831 = -(0.90D2 * t27 * (t754 * t755 - t757) + 0.180D3 * t64 * t75
     #5) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t769 * t755 + t757
     #) + 0.90D2 * t27 * (t774 * t755 / 0.2D1 + t777 - t769 * t757) + t7
     #82) * t71 * t72 / 0.2880D4 - (-0.180D3 * t64 * (-t777 + t789 * t75
     #7 - t791 * t755 / 0.2D1) + t101 * (t789 * t755 - t757) + 0.90D2 * 
     #t27 * (t789 * t777 - t791 * t757 / 0.2D1 + t803 * t755 / 0.6D1) - 
     #t148 * t755) * t71 / 0.5760D4 + (-0.180D3 * t64 * (t757 - t815 * t
     #755) + 0.90D2 * t27 * (t820 * t755 / 0.2D1 - t815 * t757 + t777) +
     # t782) * t69 * t71 / 0.5760D4
      t832 = FJET(XB1, XB2, s, -t748, 0.0D0, t750, 0.0D0, 0.0D0, t831)
      t834 = bggbH51J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t837 = bggbH51J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t839 = bggbH51J3(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t867 = t101 * t834
      t894 = (-0.180D3 * t64 * (t122 * t834 / 0.2D1 - t121 * t837 + t839
     #) + t101 * (-t121 * t834 + t837) + 0.90D2 * t27 * (-t135 * t834 / 
     #0.6D1 + t122 * t837 / 0.2D1 - t121 * t839) + t148 * t834) * t72 / 
     #0.2880D4 - (-0.180D3 * t64 * (t156 * t834 - t837) + 0.90D2 * t27 *
     # (-t161 * t834 / 0.2D1 - t839 + t156 * t837) - t867) * t69 * t72 /
     # 0.2880D4 - (0.90D2 * t27 * (t178 * t834 - t837) + 0.180D3 * t64 *
     # t834) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t192 * t834 + 
     #t837) + 0.90D2 * t27 * (t839 - t192 * t837 + t197 * t834 / 0.2D1) 
     #+ t867) * t71 * t72 / 0.2880D4
      t895 = FJET(XB1, XB2, s, -t111, t112, 0.0D0, 0.0D0, 0.0D0, t894)
      t897 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t899 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t909 = t101 * t897
      t958 = -(-0.180D3 * t64 * (t315 * t897 - t899) + 0.90D2 * t27 * (t
     #315 * t899 - t322 * t897 / 0.2D1) - t909) * t69 * t72 / 0.2880D4 -
     # (0.90D2 * t27 * (t336 * t897 - t899) + 0.180D3 * t64 * t897) * t6
     #9 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t349 * t897 + t899) + 0.9
     #0D2 * t27 * (-t349 * t899 + t355 * t897 / 0.2D1) + t909) * t69 * t
     #71 / 0.5760D4 + (-0.180D3 * t64 * (t370 * t897 / 0.2D1 - t369 * t8
     #99) + t101 * (t899 - t369 * t897) + 0.90D2 * t27 * (-t383 * t897 /
     # 0.6D1 + t370 * t899 / 0.2D1) + t148 * t897) * t69 / 0.5760D4
      t959 = FJET(XB1, XB2, s, 0.0D0, -t309, 0.0D0, t310, 0.0D0, t958)
      t961 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t962 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t980 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t984 = t101 * t962
      t1024 = -(0.90D2 * t27 * (-t961 + t754 * t962) + 0.180D3 * t64 * t
     #962) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t769 * t962 + t9
     #61) + 0.90D2 * t27 * (t774 * t962 / 0.2D1 - t769 * t961 + t980) + 
     #t984) * t71 * t72 / 0.2880D4 - (-0.180D3 * t64 * (-t980 - t791 * t
     #962 / 0.2D1 + t789 * t961) + t101 * (t789 * t962 - t961) + 0.90D2 
     #* t27 * (t789 * t980 + t803 * t962 / 0.6D1 - t791 * t961 / 0.2D1) 
     #- t148 * t962) * t71 / 0.5760D4 + (-0.180D3 * t64 * (t961 - t815 *
     # t962) + 0.90D2 * t27 * (-t815 * t961 + t820 * t962 / 0.2D1 + t980
     #) + t984) * t69 * t71 / 0.5760D4
      t1025 = FJET(XB1, XB2, s, 0.0D0, t750, 0.0D0, -t748, 0.0D0, t1024)
      t1027 = bggbH54J1(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t1029 = bggbH54J2(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t1053 = -(0.90D2 * t27 * (-t227 * t1027 + t1029) - 0.180D3 * t64 *
     # t1027) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t1029 + t241 
     #* t1027) + 0.90D2 * t27 * (t241 * t1029 - t248 * t1027 / 0.2D1) - 
     #t101 * t1027) * t71 * t72 / 0.2880D4
      t1054 = FJET(XB1, XB2, s, t112, t213, 0.0D0, -t216, -t222, t1053)
      t1056 = bggbH52J1(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t1058 = bggbH52J2(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t1082 = -(0.90D2 * t27 * (-t227 * t1056 + t1058) - 0.180D3 * t64 *
     # t1056) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t1058 + t241 
     #* t1056) + 0.90D2 * t27 * (-t248 * t1056 / 0.2D1 + t241 * t1058) -
     # t101 * t1056) * t71 * t72 / 0.2880D4
      t1083 = FJET(XB1, XB2, s, -t216, 0.0D0, t213, t112, -t222, t1082)
      t1085 = bggbH54J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t1088 = bggbH54J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t1116 = t101 * t1085
      t1143 = (-0.180D3 * t64 * (t122 * t1085 / 0.2D1 - t121 * t1088) + 
     #t101 * (-t121 * t1085 + t1088) + 0.90D2 * t27 * (-t135 * t1085 / 0
     #.6D1 + t122 * t1088 / 0.2D1) + t148 * t1085) * t72 / 0.2880D4 - (-
     #0.180D3 * t64 * (-t1088 + t156 * t1085) + 0.90D2 * t27 * (t156 * t
     #1088 - t161 * t1085 / 0.2D1) - t1116) * t69 * t72 / 0.2880D4 - (0.
     #90D2 * t27 * (t178 * t1085 - t1088) + 0.180D3 * t64 * t1085) * t69
     # * t73 / 0.2880D4 + (-0.180D3 * t64 * (t1088 - t192 * t1085) + 0.9
     #0D2 * t27 * (-t192 * t1088 + t197 * t1085 / 0.2D1) + t1116) * t71 
     #* t72 / 0.2880D4
      t1144 = FJET(XB1, XB2, s, t112, -t111, 0.0D0, 0.0D0, 0.0D0, t1143)
      t1146 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t1147 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t1154 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t1159 = t101 * t1147
      t1209 = -(-0.180D3 * t64 * (-t1146 + t315 * t1147) + 0.90D2 * t27 
     #* (-t322 * t1147 / 0.2D1 - t1154 + t315 * t1146) - t1159) * t69 * 
     #t72 / 0.2880D4 - (0.90D2 * t27 * (t336 * t1147 - t1146) + 0.180D3 
     #* t64 * t1147) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (t1146 -
     # t349 * t1147) + 0.90D2 * t27 * (t1154 + t355 * t1147 / 0.2D1 - t3
     #49 * t1146) + t1159) * t69 * t71 / 0.5760D4 + (-0.180D3 * t64 * (t
     #1154 + t370 * t1147 / 0.2D1 - t369 * t1146) + t101 * (t1146 - t369
     # * t1147) + 0.90D2 * t27 * (t370 * t1146 / 0.2D1 - t369 * t1154 - 
     #t383 * t1147 / 0.6D1) + t148 * t1147) * t69 / 0.5760D4
      t1210 = FJET(XB1, XB2, s, 0.0D0, t310, 0.0D0, -t309, 0.0D0, t1209)
      t1212 = t108 * t107 + t209 * t208 + t260 * t259 + t307 * t306 + t3
     #94 * t393 + t584 * t583 + t620 * t619 + t746 * t745 + t832 * t831 
     #+ t895 * t894 + t959 * t958 + t1025 * t1024 + t1054 * t1053 + t108
     #3 * t1082 + t1144 * t1143 + t1210 * t1209
      t1213 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1215 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1235 = t101 * t1213
      t1274 = -(0.90D2 * t27 * (t754 * t1213 - t1215) + 0.180D3 * t64 * 
     #t1213) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (t1215 - t769 * 
     #t1213) + 0.90D2 * t27 * (t774 * t1213 / 0.2D1 - t769 * t1215) + t1
     #235) * t71 * t72 / 0.2880D4 - (-0.180D3 * t64 * (-t791 * t1213 / 0
     #.2D1 + t789 * t1215) + t101 * (t789 * t1213 - t1215) + 0.90D2 * t2
     #7 * (t803 * t1213 / 0.6D1 - t791 * t1215 / 0.2D1) - t148 * t1213) 
     #* t71 / 0.5760D4 + (-0.180D3 * t64 * (-t815 * t1213 + t1215) + 0.9
     #0D2 * t27 * (-t815 * t1215 + t820 * t1213 / 0.2D1) + t1235) * t69 
     #* t71 / 0.5760D4
      t1275 = FJET(XB1, XB2, s, 0.0D0, -t748, 0.0D0, t750, 0.0D0, t1274)
      t1277 = bggbH53J1(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t1279 = bggbH53J2(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t1295 = bggbH53J3(s, XB1, XB2, z, lh, wd, t123, x2, 0.10D1, x4)
      t1304 = -(0.90D2 * t27 * (-t227 * t1277 + t1279) - 0.180D3 * t64 *
     # t1277) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (t241 * t1277 -
     # t1279) + 0.90D2 * t27 * (t241 * t1279 - t248 * t1277 / 0.2D1 - t1
     #295) - t101 * t1277) * t71 * t72 / 0.2880D4
      t1305 = FJET(XB1, XB2, s, 0.0D0, -t216, t112, t213, -t222, t1304)
      t1307 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1310 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1329 = t148 * t1310
      t1362 = t101 * t1310
      t1424 = -t492 * t1307 / 0.5760D4 - t511 * t1310 / 0.5760D4 + (-0.1
     #80D3 * t64 * (-t518 * t1310 / 0.2D1 + t516 * t1307) + t101 * (t516
     # * t1310 - t1307) + 0.90D2 * t27 * (t528 * t1310 / 0.6D1 - t518 * 
     #t1307 / 0.2D1) - t1329) * t69 / 0.5760D4 + (-0.180D3 * t64 * (-t40
     #2 * t1310 / 0.2D1 + t399 * t1307) + t101 * (t399 * t1310 - t1307) 
     #+ 0.90D2 * t27 * (t413 * t1310 / 0.6D1 - t402 * t1307 / 0.2D1) - t
     #1329) * t72 / 0.2880D4 - (-0.180D3 * t64 * (-t427 * t1310 + t1307)
     # + 0.90D2 * t27 * (t433 * t1310 / 0.2D1 - t427 * t1307) + t1362) *
     # t69 * t72 / 0.2880D4 - (0.90D2 * t27 * (-t446 * t1310 + t1307) - 
     #0.180D3 * t64 * t1310) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * 
     #(t459 * t1310 - t1307) + 0.90D2 * t27 * (-t465 * t1310 / 0.2D1 + t
     #459 * t1307) - t1362) * t71 * t72 / 0.2880D4 - (-0.180D3 * t64 * (
     #-t542 * t1307 + t544 * t1310 / 0.2D1) + t101 * (t1307 - t542 * t13
     #10) + 0.90D2 * t27 * (t544 * t1307 / 0.2D1 - t556 * t1310 / 0.6D1)
     # + t1329) * t71 / 0.5760D4 + (-0.180D3 * t64 * (t567 * t1310 - t13
     #07) + 0.90D2 * t27 * (-t573 * t1310 / 0.2D1 + t567 * t1307) - t136
     #2) * t69 * t71 / 0.5760D4
      t1425 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1424)
      t1427 = bggbH53J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t1428 = bggbH53J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t1434 = bggbH53J3(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t1454 = -(-0.180D3 * t64 * (t1427 - t276 * t1428) + 0.90D2 * t27 *
     # (-t276 * t1427 + t1434 + t282 * t1428 / 0.2D1) + t101 * t1428) * 
     #t69 * t72 / 0.2880D4 - (0.90D2 * t27 * (-t295 * t1428 + t1427) - 0
     #.180D3 * t64 * t1428) * t69 * t73 / 0.2880D4
      t1455 = FJET(XB1, XB2, s, t265, -t263, -t269, t267, 0.0D0, t1454)
      t1458 = t266 * t268 * t15
      t1459 = t264 * z
      t1463 = Sqrt(-x3 * t114 * x2 * t7)
      t1464 = t6 * t1463
      t1465 = 0.2D1 * t1464
      t1470 = t111 * t3 * (-t4 - 0.1D1 + x3 + x1 - t264 - t113 + t1459 +
     # t1465) * t115 * t15
      t1473 = t112 * x3 * t3 * t15
      t1474 = t20 * x1
      t1475 = t4 * x1
      t1477 = t20 * t113
      t1478 = t4 * t113
      t1481 = 0.2D1 * t1464 * x2
      t1482 = t264 - t20 - t1465 + t18 - x2 - x3 + t1474 - t1459 - 0.2D1
     # * t1475 - t1477 + 0.2D1 * t1478 + t1481
      t1485 = t111 * t1482 * t115 * t15
      t1491 = log(-0.4D1 * t444 * t174 * t48 * t7 * t51)
      t1492 = t1491 * t114
      t1493 = x2 * x1
      t1494 = t1493 * z
      t1495 = -t113 + t1494 + x1 - t1493 - 0.1D1 - t28 + x2
      t1512 = 0.1D1 - x2 - t1477 + 0.4D1 * t1478 - x3 * t46 * t1493 - 0.
     #2D1 * t152 * t28 + t152 * t46 * x2 - 0.2D1 * t1464 * t28 - 0.2D1 *
     # t1464 * t1493 - 0.2D1 * t1464 * t113 - 0.2D1 * x1 + 0.2D1 * t1464
     # * t1494 + t34 - t20 + t28 + t41
      t1526 = 0.2D1 * t113 + t1474 - 0.3D1 * t1475 + t1481 - 0.3D1 * t14
     #94 + 0.2D1 * t1464 * x1 + t172 + t1493 * t46 + 0.2D1 * t188 * z - 
     #t188 * t46 + t30 - t33 - t1465 + 0.2D1 * t1493 - 0.2D1 * t41 * z +
     # t41 * t46 - t188
      t1528 = 0.1D1 / (t1512 + t1526)
      t1529 = t1495 * t1528
      t1530 = bggbH54J1(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t1531 = t1529 * t1530
      t1533 = t114 * t1495
      t1534 = bggbH54J2(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t1540 = t64 * t114
      t1543 = 0.90D2 * t27 * (-t1492 * t1531 + t1533 * t1528 * t1534) - 
     #0.180D3 * t1540 * t1531
      t1547 = FJET(XB1, XB2, s, t1458, t1470, t1473, -t1485, -t222, -t15
     #43 * t69 * t73 / 0.2880D4)
      t1550 = t69 * t71 * t72
      t1553 = bggbH54J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t1554 = bggbH54J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t1579 = -(-0.180D3 * t64 * (t1553 - t276 * t1554) + 0.90D2 * t27 *
     # (t282 * t1554 / 0.2D1 - t276 * t1553) + t101 * t1554) * t69 * t72
     # / 0.2880D4 - (0.90D2 * t27 * (-t295 * t1554 + t1553) - 0.180D3 * 
     #t64 * t1554) * t69 * t73 / 0.2880D4
      t1580 = FJET(XB1, XB2, s, -t269, t267, t265, -t263, 0.0D0, t1579)
      t1582 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t1583 = t36 * t1582
      t1585 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t1586 = t37 * t1585
      t1590 = t37 * t1582
      t1613 = -(0.90D2 * t27 * (t57 * t1583 - t1586) + 0.180D3 * t64 * t
     #1590) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t80 * t1583 + t
     #1586) + 0.90D2 * t27 * (-t80 * t36 * t1585 + t88 * t1583 / 0.2D1) 
     #+ t101 * t1590) * t69 * t71 / 0.5760D4
      t1614 = FJET(XB1, XB2, s, 0.0D0, -t17, 0.0D0, t25, 0.0D0, t1613)
      t1616 = bggbH53J3(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t1617 = bggbH53J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t1620 = bggbH53J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, 0.10D1, x4)
      t1649 = t101 * t1617
      t1676 = (-0.180D3 * t64 * (t1616 + t122 * t1617 / 0.2D1 - t121 * t
     #1620) + t101 * (-t121 * t1617 + t1620) + 0.90D2 * t27 * (t122 * t1
     #620 / 0.2D1 - t121 * t1616 - t135 * t1617 / 0.6D1) + t148 * t1617)
     # * t72 / 0.2880D4 - (-0.180D3 * t64 * (-t1620 + t156 * t1617) + 0.
     #90D2 * t27 * (-t161 * t1617 / 0.2D1 - t1616 + t156 * t1620) - t164
     #9) * t69 * t72 / 0.2880D4 - (0.90D2 * t27 * (t178 * t1617 - t1620)
     # + 0.180D3 * t64 * t1617) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64
     # * (-t192 * t1617 + t1620) + 0.90D2 * t27 * (t197 * t1617 / 0.2D1 
     #+ t1616 - t192 * t1620) + t1649) * t71 * t72 / 0.2880D4
      t1677 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t112, -t111, 0.0D0, t1676)
      t1679 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1681 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1701 = t101 * t1679
      t1740 = -(0.90D2 * t27 * (t754 * t1679 - t1681) + 0.180D3 * t64 * 
     #t1679) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t769 * t1679 +
     # t1681) + 0.90D2 * t27 * (-t769 * t1681 + t774 * t1679 / 0.2D1) + 
     #t1701) * t71 * t72 / 0.2880D4 - (-0.180D3 * t64 * (t789 * t1681 - 
     #t791 * t1679 / 0.2D1) + t101 * (t789 * t1679 - t1681) + 0.90D2 * t
     #27 * (-t791 * t1681 / 0.2D1 + t803 * t1679 / 0.6D1) - t148 * t1679
     #) * t71 / 0.5760D4 + (-0.180D3 * t64 * (t1681 - t815 * t1679) + 0.
     #90D2 * t27 * (t820 * t1679 / 0.2D1 - t815 * t1681) + t1701) * t69 
     #* t71 / 0.5760D4
      t1741 = FJET(XB1, XB2, s, t750, 0.0D0, -t748, 0.0D0, 0.0D0, t1740)
      t1743 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t1744 = t37 * t1743
      t1745 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t38, x4)
      t1746 = t36 * t1745
      t1751 = t37 * t1745
      t1774 = -(0.90D2 * t27 * (-t1744 + t57 * t1746) + 0.180D3 * t64 * 
     #t1751) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (t1744 - t80 * t
     #1746) + 0.90D2 * t27 * (t88 * t1746 / 0.2D1 - t80 * t36 * t1743) +
     # t101 * t1751) * t69 * t71 / 0.5760D4
      t1775 = FJET(XB1, XB2, s, t25, 0.0D0, -t17, 0.0D0, 0.0D0, t1774)
      t1777 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1780 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1797 = t148 * t1780
      t1811 = t101 * t1780
      t1894 = -t492 * t1777 / 0.5760D4 + (-0.180D3 * t64 * (-t402 * t178
     #0 / 0.2D1 + t399 * t1777) + t101 * (-t1777 + t399 * t1780) + 0.90D
     #2 * t27 * (-t402 * t1777 / 0.2D1 + t413 * t1780 / 0.6D1) - t1797) 
     #* t72 / 0.2880D4 - (-0.180D3 * t64 * (-t427 * t1780 + t1777) + 0.9
     #0D2 * t27 * (t433 * t1780 / 0.2D1 - t427 * t1777) + t1811) * t69 *
     # t72 / 0.2880D4 - (0.90D2 * t27 * (t1777 - t446 * t1780) - 0.180D3
     # * t64 * t1780) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t1777
     # + t459 * t1780) + 0.90D2 * t27 * (-t465 * t1780 / 0.2D1 + t459 * 
     #t1777) - t1811) * t71 * t72 / 0.2880D4 - t511 * t1780 / 0.5760D4 -
     # (-0.180D3 * t64 * (-t542 * t1777 + t544 * t1780 / 0.2D1) + t101 *
     # (-t542 * t1780 + t1777) + 0.90D2 * t27 * (t544 * t1777 / 0.2D1 - 
     #t556 * t1780 / 0.6D1) + t1797) * t71 / 0.5760D4 + (-0.180D3 * t64 
     #* (t567 * t1780 - t1777) + 0.90D2 * t27 * (t567 * t1777 - t573 * t
     #1780 / 0.2D1) - t1811) * t69 * t71 / 0.5760D4 + (-0.180D3 * t64 * 
     #(t516 * t1777 - t518 * t1780 / 0.2D1) + t101 * (t516 * t1780 - t17
     #77) + 0.90D2 * t27 * (-t518 * t1777 / 0.2D1 + t528 * t1780 / 0.6D1
     #) - t1797) * t69 / 0.5760D4
      t1895 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1894)
      t1897 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t1898 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t270, x4)
      t1909 = t101 * t1898
      t1958 = -(-0.180D3 * t64 * (-t1897 + t315 * t1898) + 0.90D2 * t27 
     #* (t315 * t1897 - t322 * t1898 / 0.2D1) - t1909) * t69 * t72 / 0.2
     #880D4 - (0.90D2 * t27 * (t336 * t1898 - t1897) + 0.180D3 * t64 * t
     #1898) * t69 * t73 / 0.2880D4 + (-0.180D3 * t64 * (-t349 * t1898 + 
     #t1897) + 0.90D2 * t27 * (-t349 * t1897 + t355 * t1898 / 0.2D1) + t
     #1909) * t69 * t71 / 0.5760D4 + (-0.180D3 * t64 * (t370 * t1898 / 0
     #.2D1 - t369 * t1897) + t101 * (-t369 * t1898 + t1897) + 0.90D2 * t
     #27 * (t370 * t1897 / 0.2D1 - t383 * t1898 / 0.6D1) + t148 * t1898)
     # * t69 / 0.5760D4
      t1959 = FJET(XB1, XB2, s, t310, 0.0D0, -t309, 0.0D0, 0.0D0, t1958)
      t1961 = bggbH53J1(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t1962 = t1529 * t1961
      t1964 = bggbH53J2(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t1972 = 0.90D2 * t27 * (-t1492 * t1962 + t1533 * t1528 * t1964) - 
     #0.180D3 * t1540 * t1962
      t1976 = FJET(XB1, XB2, s, t1473, -t1485, t1458, t1470, -t222, -t19
     #72 * t69 * t73 / 0.2880D4)
      t1980 = bggbH52J2(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t1983 = bggbH52J1(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t1984 = t1529 * t1983
      t1991 = 0.90D2 * t27 * (t1533 * t1528 * t1980 - t1492 * t1984) - 0
     #.180D3 * t1540 * t1984
      t1995 = FJET(XB1, XB2, s, -t1485, t1473, t1470, t1458, -t222, -t19
     #91 * t69 * t73 / 0.2880D4)
      t1999 = bggbH51J1(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t2001 = bggbH51J2(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t2007 = bggbH51J3(s, XB1, XB2, z, lh, wd, t123, 0.0D0, t270, x4)
      t2026 = -(-0.180D3 * t64 * (-t276 * t1999 + t2001) + 0.90D2 * t27 
     #* (t282 * t1999 / 0.2D1 + t2007 - t276 * t2001) + t101 * t1999) * 
     #t69 * t72 / 0.2880D4 - (0.90D2 * t27 * (-t295 * t1999 + t2001) - 0
     #.180D3 * t64 * t1999) * t69 * t73 / 0.2880D4
      t2027 = FJET(XB1, XB2, s, t267, -t269, -t263, t265, 0.0D0, t2026)
      t2029 = bggbH51J2(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t2032 = bggbH51J1(s, XB1, XB2, z, lh, wd, t123, x2, t38, x4)
      t2033 = t1529 * t2032
      t2040 = 0.90D2 * t27 * (t1533 * t1528 * t2029 - t1492 * t2033) - 0
     #.180D3 * t1540 * t2033
      t2044 = FJET(XB1, XB2, s, t1470, t1458, -t1485, t1473, -t222, -t20
     #40 * t69 * t73 / 0.2880D4)
      t2048 = t1275 * t1274 + t1305 * t1304 + t1425 * t1424 + t1455 * t1
     #454 - t1547 * t1543 * t1550 / 0.2880D4 + t1580 * t1579 + t1614 * t
     #1613 + t1677 * t1676 + t1741 * t1740 + t1775 * t1774 + t1895 * t18
     #94 + t1959 * t1958 - t1976 * t1972 * t1550 / 0.2880D4 - t1995 * t1
     #991 * t1550 / 0.2880D4 + t2027 * t2026 - t2044 * t2040 * t1550 / 0
     #.2880D4
      bggbH5n2e1 = t1212 + t2048

      end function



      doubleprecision function bggbH5n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = t2 * x1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = -t3
      t9 = bggbH52J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t10 = x1 ** 2
      t11 = x3 * t10
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t11 * t14
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t18 = x1 * z
      t19 = 0.1D1 - x1 + t18
      t20 = 0.1D1 / t19
      t21 = t17 * t20
      t22 = t3 ** 2
      t23 = t21 * t22
      t26 = log(0.4D1 * t15 * t23)
      t27 = bggbH52J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t32 = t7 * lh
      t34 = 0.180D3 * t32 * t27
      t36 = 0.1D1 / x3
      t38 = 0.1D1 / x1
      t42 = 0.1D1 / x2
      t44 = t36 * t42 * t38
      t47 = t10 * x2
      t48 = t47 * t14
      t51 = log(0.4D1 * t48 * t23)
      t60 = t10 * t14
      t63 = log(0.4D1 * t60 * t23)
      t68 = t63 ** 2
      t75 = lh ** 2
      t77 = 0.3141592653589793D1 ** 2
      t79 = 0.180D3 * t75 - 0.30D2 * t77
      t80 = t7 * t79
      t85 = -(0.90D2 * t7 * (-t9 + t26 * t27) + t34) * t36 * t38 / 0.288
     #0D4 + t7 * t27 * t44 / 0.32D2 + (0.90D2 * t7 * (-t51 * t27 + t9) -
     # t34) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 * (-t63 * t27 + t9)
     # + 0.90D2 * t7 * (t68 * t27 / 0.2D1 - t63 * t9) + t80 * t27) * t38
     # / 0.2880D4
      t86 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t85)
      t88 = -0.1D1 + x3
      t89 = t88 * s
      t90 = t1 * x1
      t91 = t89 * t90
      t92 = t1 * t3
      t93 = t89 * t92
      t94 = x1 * x3
      t95 = t2 * t94
      t97 = t2 * t3 * x3
      t98 = -t88
      t99 = bggbH54J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t104 = log(-0.4D1 * t15 * t21 * t22 * t88)
      t105 = bggbH54J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t119 = -(0.90D2 * t7 * (t99 - t104 * t105) - 0.180D3 * t32 * t105)
     # * t36 * t38 / 0.2880D4 - t7 * t105 * t44 / 0.32D2
      t120 = FJET(XB1, XB2, s, -t91, t93, t95, -t97, 0.0D0, t119)
      t122 = -0.1D1 + x2
      t123 = t2 * t122
      t125 = x2 * s * t1
      t126 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t130 = t14 * t17
      t131 = t122 ** 2
      t132 = t130 * t131
      t135 = log(0.4D1 * t47 * t132)
      t137 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t142 = 0.180D3 * t32 * t126
      t147 = x2 * x3
      t150 = log(0.4D1 * t147 * t132)
      t159 = x2 * t14
      t160 = t17 * t131
      t163 = log(0.4D1 * t159 * t160)
      t168 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t170 = t163 ** 2
      t180 = t7 * t126 * t44 / 0.32D2 + (0.90D2 * t7 * (-t135 * t126 + t
     #137) - t142) * t42 * t38 / 0.2880D4 + (0.90D2 * t7 * (t137 - t150 
     #* t126) - t142) * t36 * t42 / 0.5760D4 - (-0.180D3 * t32 * (t163 *
     # t126 - t137) + 0.90D2 * t7 * (-t168 + t163 * t137 - t170 * t126 /
     # 0.2D1) - t80 * t126) * t42 / 0.5760D4
      t181 = FJET(XB1, XB2, s, -t123, 0.0D0, t125, 0.0D0, 0.0D0, t180)
      t185 = log(0.4D1 * t11 * t130)
      t186 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t188 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t193 = 0.180D3 * t32 * t186
      t203 = log(0.4D1 * t47 * t130)
      t214 = log(0.4D1 * t60 * t17)
      t219 = t214 ** 2
      t226 = t80 * t186
      t231 = log(0.4D1 * t130)
      t232 = t231 * t7
      t235 = t231 ** 2
      t236 = t235 * t7
      t238 = 0.180D3 * t232 * lh + t80 + 0.45D2 * t236
      t243 = log(0.4D1 * t147 * t130)
      t254 = log(0.4D1 * t159 * t17)
      t260 = t254 ** 2
      t281 = -t232 * t79 - 0.90D2 * t236 * lh + t7 * (-0.288493656758302
     #6D3 - 0.120D3 * t75 * lh + 0.60D2 * lh * t77) - 0.15D2 * t235 * t2
     #31 * t7
      t284 = x3 * t14
      t287 = log(0.4D1 * t284 * t17)
      t293 = t287 ** 2
      t302 = -(0.90D2 * t7 * (-t185 * t186 + t188) - t193) * t36 * t38 /
     # 0.2880D4 - t7 * t186 * t44 / 0.32D2 + (0.90D2 * t7 * (-t188 + t20
     #3 * t186) + t193) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 * (-t18
     #8 + t214 * t186) + 0.90D2 * t7 * (-t219 * t186 / 0.2D1 + t214 * t1
     #88) - t226) * t38 / 0.2880D4 - t238 * t188 / 0.5760D4 + (0.90D2 * 
     #t7 * (t243 * t186 - t188) + t193) * t36 * t42 / 0.5760D4 - (-0.180
     #D3 * t32 * (-t254 * t186 + t188) + 0.90D2 * t7 * (-t254 * t188 + t
     #260 * t186 / 0.2D1) + t226) * t42 / 0.5760D4 - t281 * t186 / 0.576
     #0D4 + (-0.180D3 * t32 * (t287 * t186 - t188) + 0.90D2 * t7 * (t287
     # * t188 - t293 * t186 / 0.2D1) - t226) * t36 / 0.5760D4
      t303 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t302)
      t305 = cos(t12)
      t307 = Sqrt(-t147 * t88)
      t308 = t305 * t307
      t309 = 0.2D1 * t308
      t312 = t147 - 0.1D1
      t313 = 0.1D1 / t312
      t315 = t2 * t122 * (-t147 - 0.1D1 + x3 + t309) * t313
      t316 = 0.3D1 * t147
      t317 = x2 ** 2
      t318 = x3 * t317
      t320 = 0.2D1 * t308 * x2
      t323 = t2 * (-x2 + t316 - t318 - x3 - t309 + t320) * t313
      t324 = x2 * z
      t325 = 0.1D1 - x2 + t324
      t327 = t318 * z
      t330 = t147 * z
      t331 = 0.2D1 * t147
      t333 = 0.1D1 / (t318 - t327 + 0.2D1 * t308 * t324 - t320 - t324 + 
     #t330 + x2 - t331 - 0.1D1 + t309)
      t334 = t7 * t325 * t333
      t335 = t88 * t313
      t336 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t338 = t42 * t38
      t342 = t325 * t333
      t343 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t346 = t312 ** 2
      t352 = log(-0.4D1 * t147 * t14 * t160 * t88 / t346)
      t353 = t352 * t325
      t366 = t334 * t336 * t36 * t338 / 0.32D2 + (0.90D2 * t7 * (t342 * 
     #t343 - t353 * t333 * t336) - 0.180D3 * t32 * t342 * t336) * t36 * 
     #t42 / 0.5760D4
      t367 = FJET(XB1, XB2, s, -t315, 0.0D0, t323, 0.0D0, 0.0D0, t366)
      t370 = t122 * s * t92
      t373 = t2 * t3 * x2 * t20
      t374 = t1 ** 2
      t379 = s * t374 * x2 * t3 * x1 * t20
      t380 = bggbH54J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t384 = bggbH54J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t389 = log(0.4D1 * t48 * t21 * t22 * t131)
      t400 = -t7 * t380 * t44 / 0.32D2 + (0.90D2 * t7 * (-t384 + t389 * 
     #t380) + 0.180D3 * t32 * t380) * t42 * t38 / 0.2880D4
      t401 = FJET(XB1, XB2, s, t5, t370, 0.0D0, -t373, -t379, t400)
      t403 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t407 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t413 = 0.180D3 * t32 * t403
      t440 = t7 * t403 * t44 / 0.32D2 + (0.90D2 * t7 * (t407 - t135 * t4
     #03) - t413) * t42 * t38 / 0.2880D4 + (0.90D2 * t7 * (-t150 * t403 
     #+ t407) - t413) * t36 * t42 / 0.5760D4 - (-0.180D3 * t32 * (t163 *
     # t403 - t407) + 0.90D2 * t7 * (-t170 * t403 / 0.2D1 + t163 * t407)
     # - t80 * t403) * t42 / 0.5760D4
      t441 = FJET(XB1, XB2, s, 0.0D0, -t123, 0.0D0, t125, 0.0D0, t440)
      t443 = bggbH53J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t444 = bggbH53J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t450 = 0.180D3 * t32 * t444
      t470 = bggbH53J3(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t481 = -(0.90D2 * t7 * (-t443 + t26 * t444) + t450) * t36 * t38 / 
     #0.2880D4 + t7 * t444 * t44 / 0.32D2 + (0.90D2 * t7 * (-t51 * t444 
     #+ t443) - t450) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 * (-t63 *
     # t444 + t443) + 0.90D2 * t7 * (t470 + t68 * t444 / 0.2D1 - t63 * t
     #443) + t80 * t444) * t38 / 0.2880D4
      t482 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t481)
      t484 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t489 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t503 = t334 * t484 * t36 * t338 / 0.32D2 + (0.90D2 * t7 * (t342 * 
     #t489 - t353 * t333 * t484) - 0.180D3 * t32 * t342 * t484) * t36 * 
     #t42 / 0.5760D4
      t504 = FJET(XB1, XB2, s, t323, 0.0D0, -t315, 0.0D0, 0.0D0, t503)
      t506 = bggbH54J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t507 = bggbH54J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t513 = 0.180D3 * t32 * t507
      t543 = -(0.90D2 * t7 * (-t506 + t26 * t507) + t513) * t36 * t38 / 
     #0.2880D4 + t7 * t507 * t44 / 0.32D2 + (0.90D2 * t7 * (t506 - t51 *
     # t507) - t513) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 * (-t63 * 
     #t507 + t506) + 0.90D2 * t7 * (t68 * t507 / 0.2D1 - t63 * t506) + t
     #80 * t507) * t38 / 0.2880D4
      t544 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t543)
      t546 = t2 * x3
      t547 = t2 * t88
      t548 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t549 = t130 * t88
      t552 = log(-0.4D1 * t11 * t549)
      t553 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t559 = 0.180D3 * t32 * t553
      t570 = log(-0.4D1 * t284 * t17 * t88)
      t575 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t576 = t570 ** 2
      t589 = log(-0.4D1 * t147 * t549)
      t598 = -(0.90D2 * t7 * (-t548 + t552 * t553) + t559) * t36 * t38 /
     # 0.2880D4 + t7 * t553 * t44 / 0.32D2 + (-0.180D3 * t32 * (t548 - t
     #570 * t553) + 0.90D2 * t7 * (t575 + t576 * t553 / 0.2D1 - t570 * t
     #548) + t80 * t553) * t36 / 0.5760D4 + (0.90D2 * t7 * (t548 - t589 
     #* t553) - t559) * t36 * t42 / 0.5760D4
      t599 = FJET(XB1, XB2, s, 0.0D0, t546, 0.0D0, -t547, 0.0D0, t598)
      t601 = bggbH51J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t603 = bggbH51J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t616 = -(0.90D2 * t7 * (-t104 * t601 + t603) - 0.180D3 * t32 * t60
     #1) * t36 * t38 / 0.2880D4 - t7 * t601 * t44 / 0.32D2
      t617 = FJET(XB1, XB2, s, t93, -t91, -t97, t95, 0.0D0, t616)
      t619 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t626 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t638 = t334 * t619 * t36 * t338 / 0.32D2 + (0.90D2 * t7 * (-t353 *
     # t333 * t619 + t342 * t626) - 0.180D3 * t32 * t342 * t619) * t36 *
     # t42 / 0.5760D4
      t639 = FJET(XB1, XB2, s, 0.0D0, -t315, 0.0D0, t323, 0.0D0, t638)
      t641 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t642 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t648 = 0.180D3 * t32 * t642
      t678 = -(0.90D2 * t7 * (-t641 + t552 * t642) + t648) * t36 * t38 /
     # 0.2880D4 + t7 * t642 * t44 / 0.32D2 + (0.90D2 * t7 * (-t589 * t64
     #2 + t641) - t648) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (-t57
     #0 * t642 + t641) + 0.90D2 * t7 * (t576 * t642 / 0.2D1 - t570 * t64
     #1) + t80 * t642) * t36 / 0.5760D4
      t679 = FJET(XB1, XB2, s, t546, 0.0D0, -t547, 0.0D0, 0.0D0, t678)
      t681 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t682 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t688 = 0.180D3 * t32 * t682
      t708 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t715 = t80 * t682
      t721 = -0.180D3 * t32 - 0.90D2 * t232
      t762 = -(0.90D2 * t7 * (t681 - t185 * t682) - t688) * t36 * t38 / 
     #0.2880D4 - t7 * t682 * t44 / 0.32D2 + (0.90D2 * t7 * (-t681 + t203
     # * t682) + t688) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 * (t214 
     #* t682 - t681) + 0.90D2 * t7 * (-t708 - t219 * t682 / 0.2D1 + t214
     # * t681) - t715) * t38 / 0.2880D4 - t721 * t708 / 0.5760D4 - t281 
     #* t682 / 0.5760D4 - t238 * t681 / 0.5760D4 + (0.90D2 * t7 * (-t681
     # + t243 * t682) + t688) * t36 * t42 / 0.5760D4 - (-0.180D3 * t32 *
     # (t681 - t254 * t682) + 0.90D2 * t7 * (t708 - t254 * t681 + t260 *
     # t682 / 0.2D1) + t715) * t42 / 0.5760D4 + (-0.180D3 * t32 * (t287 
     #* t682 - t681) + 0.90D2 * t7 * (-t708 - t293 * t682 / 0.2D1 + t287
     # * t681) - t715) * t36 / 0.5760D4
      t763 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t762)
      t765 = bggbH53J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t766 = bggbH53J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t780 = -(0.90D2 * t7 * (t765 - t104 * t766) - 0.180D3 * t32 * t766
     #) * t36 * t38 / 0.2880D4 - t7 * t766 * t44 / 0.32D2
      t781 = FJET(XB1, XB2, s, t95, -t97, -t91, t93, 0.0D0, t780)
      t783 = t86 * t85 + t120 * t119 + t181 * t180 + t303 * t302 + t367 
     #* t366 + t401 * t400 + t441 * t440 + t482 * t481 + t504 * t503 + t
     #544 * t543 + t599 * t598 + t617 * t616 + t639 * t638 + t679 * t678
     # + t763 * t762 + t781 * t780
      t784 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t785 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t791 = 0.180D3 * t32 * t785
      t814 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t822 = -(0.90D2 * t7 * (-t784 + t552 * t785) + t791) * t36 * t38 /
     # 0.2880D4 + t7 * t785 * t44 / 0.32D2 + (0.90D2 * t7 * (-t589 * t78
     #5 + t784) - t791) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (-t57
     #0 * t785 + t784) + 0.90D2 * t7 * (t576 * t785 / 0.2D1 - t570 * t78
     #4 + t814) + t80 * t785) * t36 / 0.5760D4
      t823 = FJET(XB1, XB2, s, -t547, 0.0D0, t546, 0.0D0, 0.0D0, t822)
      t825 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t826 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t832 = 0.180D3 * t32 * t826
      t852 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t859 = t80 * t826
      t903 = -(0.90D2 * t7 * (t825 - t185 * t826) - t832) * t36 * t38 / 
     #0.2880D4 - t7 * t826 * t44 / 0.32D2 + (0.90D2 * t7 * (-t825 + t203
     # * t826) + t832) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 * (-t825
     # + t214 * t826) + 0.90D2 * t7 * (-t852 + t214 * t825 - t219 * t826
     # / 0.2D1) - t859) * t38 / 0.2880D4 + (-0.180D3 * t32 * (-t825 + t2
     #87 * t826) + 0.90D2 * t7 * (-t852 + t287 * t825 - t293 * t826 / 0.
     #2D1) - t859) * t36 / 0.5760D4 + (0.90D2 * t7 * (-t825 + t243 * t82
     #6) + t832) * t36 * t42 / 0.5760D4 - (-0.180D3 * t32 * (t825 - t254
     # * t826) + 0.90D2 * t7 * (t852 - t254 * t825 + t260 * t826 / 0.2D1
     #) + t859) * t42 / 0.5760D4 - t721 * t852 / 0.5760D4 - t238 * t825 
     #/ 0.5760D4 - t281 * t826 / 0.5760D4
      t904 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t903)
      t906 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t908 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t98, x4)
      t913 = 0.180D3 * t32 * t906
      t943 = -(0.90D2 * t7 * (t552 * t906 - t908) + t913) * t36 * t38 / 
     #0.2880D4 + t7 * t906 * t44 / 0.32D2 + (0.90D2 * t7 * (-t589 * t906
     # + t908) - t913) * t36 * t42 / 0.5760D4 + (-0.180D3 * t32 * (t908 
     #- t570 * t906) + 0.90D2 * t7 * (t576 * t906 / 0.2D1 - t570 * t908)
     # + t80 * t906) * t36 / 0.5760D4
      t944 = FJET(XB1, XB2, s, 0.0D0, -t547, 0.0D0, t546, 0.0D0, t943)
      t946 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t951 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t956 = 0.180D3 * t32 * t946
      t983 = t7 * t946 * t44 / 0.32D2 + (0.90D2 * t7 * (-t135 * t946 + t
     #951) - t956) * t42 * t38 / 0.2880D4 + (0.90D2 * t7 * (t951 - t150 
     #* t946) - t956) * t36 * t42 / 0.5760D4 - (-0.180D3 * t32 * (t163 *
     # t946 - t951) + 0.90D2 * t7 * (t163 * t951 - t170 * t946 / 0.2D1) 
     #- t80 * t946) * t42 / 0.5760D4
      t984 = FJET(XB1, XB2, s, t125, 0.0D0, -t123, 0.0D0, 0.0D0, t983)
      t986 = bggbH52J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t990 = bggbH52J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t1001 = -t7 * t986 * t44 / 0.32D2 + (0.90D2 * t7 * (-t990 + t389 *
     # t986) + 0.180D3 * t32 * t986) * t42 * t38 / 0.2880D4
      t1002 = FJET(XB1, XB2, s, -t373, 0.0D0, t370, t5, -t379, t1001)
      t1004 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1009 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1014 = 0.180D3 * t32 * t1004
      t1031 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t1042 = t7 * t1004 * t44 / 0.32D2 + (0.90D2 * t7 * (-t135 * t1004 
     #+ t1009) - t1014) * t42 * t38 / 0.2880D4 + (0.90D2 * t7 * (t1009 -
     # t150 * t1004) - t1014) * t36 * t42 / 0.5760D4 - (-0.180D3 * t32 *
     # (t163 * t1004 - t1009) + 0.90D2 * t7 * (-t1031 - t170 * t1004 / 0
     #.2D1 + t163 * t1009) - t80 * t1004) * t42 / 0.5760D4
      t1043 = FJET(XB1, XB2, s, 0.0D0, t125, 0.0D0, -t123, 0.0D0, t1042)
      t1045 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1047 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x
     #4)
      t1052 = 0.180D3 * t32 * t1045
      t1078 = t80 * t1045
      t1120 = -(0.90D2 * t7 * (-t185 * t1045 + t1047) - t1052) * t36 * t
     #38 / 0.2880D4 - t7 * t1045 * t44 / 0.32D2 + (0.90D2 * t7 * (t203 *
     # t1045 - t1047) + t1052) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 
     #* (t214 * t1045 - t1047) + 0.90D2 * t7 * (-t219 * t1045 / 0.2D1 + 
     #t214 * t1047) - t1078) * t38 / 0.2880D4 + (-0.180D3 * t32 * (t287 
     #* t1045 - t1047) + 0.90D2 * t7 * (-t293 * t1045 / 0.2D1 + t287 * t
     #1047) - t1078) * t36 / 0.5760D4 - t238 * t1047 / 0.5760D4 + (0.90D
     #2 * t7 * (t243 * t1045 - t1047) + t1052) * t36 * t42 / 0.5760D4 - 
     #(-0.180D3 * t32 * (t1047 - t254 * t1045) + 0.90D2 * t7 * (-t254 * 
     #t1047 + t260 * t1045 / 0.2D1) + t1078) * t42 / 0.5760D4 - t281 * t
     #1045 / 0.5760D4
      t1121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1120)
      t1123 = bggbH52J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t1124 = bggbH52J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, t98, x4)
      t1138 = -(0.90D2 * t7 * (t1123 - t104 * t1124) - 0.180D3 * t32 * t
     #1124) * t36 * t38 / 0.2880D4 - t7 * t1124 * t44 / 0.32D2
      t1139 = FJET(XB1, XB2, s, -t97, t95, t93, -t91, 0.0D0, t1138)
      t1141 = bggbH53J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t1146 = bggbH53J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t1156 = -t7 * t1141 * t44 / 0.32D2 + (0.90D2 * t7 * (t389 * t1141 
     #- t1146) + 0.180D3 * t32 * t1141) * t42 * t38 / 0.2880D4
      t1157 = FJET(XB1, XB2, s, 0.0D0, -t373, t5, t370, -t379, t1156)
      t1159 = bggbH51J1(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t1164 = bggbH51J2(s, XB1, XB2, z, lh, wd, t8, x2, 0.10D1, x4)
      t1174 = -t7 * t1159 * t44 / 0.32D2 + (0.90D2 * t7 * (t389 * t1159 
     #- t1164) + 0.180D3 * t32 * t1159) * t42 * t38 / 0.2880D4
      t1175 = FJET(XB1, XB2, s, t370, t5, -t373, 0.0D0, -t379, t1174)
      t1179 = t5 * x3 * t122 * t313
      t1183 = Sqrt(-x3 * t19 * x2 * t88)
      t1184 = t305 * t1183
      t1185 = 0.2D1 * t1184
      t1186 = t318 * x1
      t1187 = t94 * z
      t1188 = t147 * x1
      t1190 = t318 * t18
      t1191 = t147 * t18
      t1194 = 0.2D1 * t1184 * x2
      t1195 = t94 - t318 - t1185 + t316 - x2 - x3 + t1186 - t1187 - 0.2D
     #1 * t1188 - t1190 + 0.2D1 * t1191 + t1194
      t1198 = t4 * t1195 * t20 * t313
      t1200 = t89 * t90 * t313
      t1205 = t4 * t122 * (-t147 - 0.1D1 + x3 + x1 - t94 - t18 + t1187 +
     # t1185) * t20 * t313
      t1207 = x2 * x1
      t1208 = t1207 * z
      t1209 = -t18 + t1208 + x1 - t1207 - 0.1D1 - t324 + x2
      t1219 = 0.1D1 - 0.2D1 * x1 - x2 + 0.2D1 * t18 + t1186 - 0.3D1 * t1
     #188 + t1194 + 0.2D1 * t1207 - 0.2D1 * t10 * z + t10 * t16 - t47 - 
     #t1185 + 0.2D1 * t1184 * t1208 + t10 + t331 - t318
      t1241 = t324 + t327 - t330 - t1190 + 0.4D1 * t1191 - x3 * t16 * t1
     #207 - 0.2D1 * t11 * t324 + t11 * t16 * x2 - 0.2D1 * t1184 * t324 -
     # 0.2D1 * t1184 * t1207 - 0.2D1 * t1184 * t18 - 0.3D1 * t1208 + 0.2
     #D1 * t1184 * x1 + t11 * x2 + t1207 * t16 + 0.2D1 * t47 * z - t47 *
     # t16
      t1243 = 0.1D1 / (t1219 + t1241)
      t1245 = t7 * t19 * t1209 * t1243
      t1246 = bggbH53J1(s, XB1, XB2, z, lh, wd, t8, x2, t335, x4)
      t1251 = FJET(XB1, XB2, s, t1179, -t1198, t1200, t1205, -t379, -t12
     #45 * t1246 * t36 * t338 / 0.32D2)
      t1253 = t19 * t1209
      t1259 = bggbH54J1(s, XB1, XB2, z, lh, wd, t8, x2, t335, x4)
      t1264 = FJET(XB1, XB2, s, t1200, t1205, t1179, -t1198, -t379, -t12
     #45 * t1259 * t36 * t338 / 0.32D2)
      t1271 = bggbH51J1(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t1273 = bggbH51J2(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t1278 = 0.180D3 * t32 * t1271
      t1301 = bggbH51J3(s, XB1, XB2, z, lh, wd, t8, 0.0D0, 0.10D1, x4)
      t1309 = -(0.90D2 * t7 * (t26 * t1271 - t1273) + t1278) * t36 * t38
     # / 0.2880D4 + t7 * t1271 * t44 / 0.32D2 + (0.90D2 * t7 * (-t51 * t
     #1271 + t1273) - t1278) * t42 * t38 / 0.2880D4 + (-0.180D3 * t32 * 
     #(-t63 * t1271 + t1273) + 0.90D2 * t7 * (t68 * t1271 / 0.2D1 - t63 
     #* t1273 + t1301) + t80 * t1271) * t38 / 0.2880D4
      t1310 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t1309)
      t1312 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t1319 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t335, x4)
      t1331 = t334 * t1312 * t36 * t338 / 0.32D2 + (0.90D2 * t7 * (-t353
     # * t333 * t1312 + t342 * t1319) - 0.180D3 * t32 * t342 * t1312) * 
     #t36 * t42 / 0.5760D4
      t1332 = FJET(XB1, XB2, s, 0.0D0, t323, 0.0D0, -t315, 0.0D0, t1331)
      t1334 = bggbH52J1(s, XB1, XB2, z, lh, wd, t8, x2, t335, x4)
      t1339 = FJET(XB1, XB2, s, -t1198, t1179, t1205, t1200, -t379, -t12
     #45 * t1334 * t36 * t338 / 0.32D2)
      t1346 = bggbH51J1(s, XB1, XB2, z, lh, wd, t8, x2, t335, x4)
      t1351 = FJET(XB1, XB2, s, t1205, t1200, -t1198, t1179, -t379, -t12
     #45 * t1346 * t36 * t338 / 0.32D2)
      t1358 = t823 * t822 + t904 * t903 + t944 * t943 + t984 * t983 + t1
     #002 * t1001 + t1043 * t1042 + t1121 * t1120 + t1139 * t1138 + t115
     #7 * t1156 + t1175 * t1174 - t1251 * t7 * t1253 * t1243 * t1246 * t
     #44 / 0.32D2 - t1264 * t7 * t1253 * t1243 * t1259 * t44 / 0.32D2 + 
     #t1310 * t1309 + t1332 * t1331 - t1339 * t7 * t1253 * t1243 * t1334
     # * t44 / 0.32D2 - t1351 * t7 * t1253 * t1243 * t1346 * t44 / 0.32D
     #2
      bggbH5n2e0 = t783 + t1358

      end function



      doubleprecision function bggbH5n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x2
      t4 = x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x3
      t9 = Sqrt(-t4 * t7)
      t10 = t6 * t9
      t11 = 0.2D1 * t10
      t15 = 0.1D1 / (t4 - 0.1D1)
      t17 = t2 * t3 * (-t4 - 0.1D1 + x3 + t11) * t15
      t19 = x2 ** 2
      t20 = x3 * t19
      t22 = 0.2D1 * t10 * x2
      t25 = t2 * (-x2 + 0.3D1 * t4 - t20 - x3 - t11 + t22) * t15
      t26 = s ** 2
      t27 = 0.1D1 / t26
      t28 = x2 * z
      t29 = 0.1D1 - x2 + t28
      t37 = 0.1D1 / (t20 - t20 * z + 0.2D1 * t10 * t28 - t22 - t28 + t4 
     #* z + x2 - 0.2D1 * t4 - 0.1D1 + t11)
      t38 = t27 * t29 * t37
      t39 = t7 * t15
      t40 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t39, x4)
      t41 = 0.1D1 / x3
      t43 = 0.1D1 / x2
      t47 = FJET(XB1, XB2, s, -t17, 0.0D0, t25, 0.0D0, 0.0D0, t38 * t40 
     #* t41 * t43 / 0.64D2)
      t51 = t41 * t43
      t55 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t39, x4)
      t60 = FJET(XB1, XB2, s, 0.0D0, t25, 0.0D0, -t17, 0.0D0, t38 * t55 
     #* t41 * t43 / 0.64D2)
      t67 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t39, x4)
      t72 = FJET(XB1, XB2, s, 0.0D0, -t17, 0.0D0, t25, 0.0D0, t38 * t67 
     #* t41 * t43 / 0.64D2)
      t79 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, t39, x4)
      t84 = FJET(XB1, XB2, s, t25, 0.0D0, -t17, 0.0D0, 0.0D0, t38 * t79 
     #* t41 * t43 / 0.64D2)
      t91 = t27 * lh
      t93 = z ** 2
      t94 = 0.1D1 / t93
      t95 = Sin(t5)
      t96 = t95 ** 2
      t99 = log(0.4D1 * t94 * t96)
      t100 = t99 * t27
      t102 = -0.180D3 * t91 - 0.90D2 * t100
      t103 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t106 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t107 = t27 * t106
      t110 = x2 * t96
      t113 = log(0.4D1 * t110 * t94)
      t119 = 0.180D3 * t91 * t106
      t125 = lh ** 2
      t127 = 0.3141592653589793D1 ** 2
      t131 = t99 ** 2
      t134 = 0.180D3 * t100 * lh + t27 * (0.180D3 * t125 - 0.30D2 * t127
     #) + 0.45D2 * t131 * t27
      t137 = x1 ** 2
      t138 = t137 * t96
      t141 = log(0.4D1 * t138 * t94)
      t147 = 0.1D1 / x1
      t150 = t41 * t147
      t153 = t43 * t147
      t156 = x3 * t96
      t159 = log(0.4D1 * t156 * t94)
      t167 = -t102 * t103 / 0.5760D4 - t107 * t51 / 0.64D2 - (0.90D2 * t
     #27 * (t103 - t113 * t106) - t119) * t43 / 0.5760D4 - t134 * t106 /
     # 0.5760D4 + (0.90D2 * t27 * (t141 * t106 - t103) + t119) * t147 / 
     #0.2880D4 - t107 * t150 / 0.32D2 - t107 * t153 / 0.32D2 + (0.90D2 *
     # t27 * (t159 * t106 - t103) + t119) * t41 / 0.5760D4
      t168 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t167)
      t170 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t173 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t176 = t27 * t173
      t184 = 0.180D3 * t91 * t173
      t206 = -t102 * t170 / 0.5760D4 - t134 * t173 / 0.5760D4 - t176 * t
     #51 / 0.64D2 - (0.90D2 * t27 * (-t113 * t173 + t170) - t184) * t43 
     #/ 0.5760D4 + (0.90D2 * t27 * (-t170 + t141 * t173) + t184) * t147 
     #/ 0.2880D4 - t176 * t150 / 0.32D2 - t176 * t153 / 0.32D2 + (0.90D2
     # * t27 * (t159 * t173 - t170) + t184) * t41 / 0.5760D4
      t207 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t206)
      t210 = x2 * s * t1
      t211 = t2 * t3
      t212 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t213 = t27 * t212
      t218 = t3 ** 2
      t222 = log(0.4D1 * t110 * t94 * t218)
      t224 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t233 = t213 * t153 / 0.32D2 + t213 * t51 / 0.64D2 - (0.90D2 * t27 
     #* (t222 * t212 - t224) + 0.180D3 * t91 * t212) * t43 / 0.5760D4
      t234 = FJET(XB1, XB2, s, 0.0D0, t210, 0.0D0, -t211, 0.0D0, t233)
      t236 = t2 * x3
      t237 = t2 * t7
      t238 = -t7
      t239 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t240 = t27 * t239
      t245 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t249 = log(-0.4D1 * t156 * t94 * t7)
      t259 = t240 * t150 / 0.32D2 + t240 * t51 / 0.64D2 + (0.90D2 * t27 
     #* (t245 - t249 * t239) - 0.180D3 * t91 * t239) * t41 / 0.5760D4
      t260 = FJET(XB1, XB2, s, 0.0D0, t236, 0.0D0, -t237, 0.0D0, t259)
      t262 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t263 = t27 * t262
      t269 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t278 = t263 * t153 / 0.32D2 + t263 * t51 / 0.64D2 - (0.90D2 * t27 
     #* (t222 * t262 - t269) + 0.180D3 * t91 * t262) * t43 / 0.5760D4
      t279 = FJET(XB1, XB2, s, -t211, 0.0D0, t210, 0.0D0, 0.0D0, t278)
      t281 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t282 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t288 = 0.180D3 * t91 * t282
      t292 = t27 * t282
      t306 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t320 = (0.90D2 * t27 * (-t281 + t141 * t282) + t288) * t147 / 0.28
     #80D4 - t292 * t150 / 0.32D2 - t292 * t153 / 0.32D2 - t292 * t51 / 
     #0.64D2 - (0.90D2 * t27 * (t281 - t113 * t282) - t288) * t43 / 0.57
     #60D4 - t27 * t306 / 0.64D2 - t102 * t281 / 0.5760D4 - t134 * t282 
     #/ 0.5760D4 + (0.90D2 * t27 * (-t281 + t159 * t282) + t288) * t41 /
     # 0.5760D4
      t321 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t320)
      t323 = t2 * x1
      t324 = -0.1D1 + x1
      t325 = t2 * t324
      t328 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t330 = t324 ** 2
      t334 = log(0.4D1 * t138 * t94 * t328 * t330)
      t335 = -t324
      t336 = bggbH53J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t338 = bggbH53J2(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t347 = t27 * t336
      t352 = (0.90D2 * t27 * (-t334 * t336 + t338) - 0.180D3 * t91 * t33
     #6) * t147 / 0.2880D4 + t347 * t150 / 0.32D2 + t347 * t153 / 0.32D2
      t353 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t323, -t325, 0.0D0, t352)
      t355 = bggbH54J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t357 = bggbH54J2(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t366 = t27 * t355
      t371 = (0.90D2 * t27 * (-t334 * t355 + t357) - 0.180D3 * t91 * t35
     #5) * t147 / 0.2880D4 + t366 * t150 / 0.32D2 + t366 * t153 / 0.32D2
      t372 = FJET(XB1, XB2, s, t323, -t325, 0.0D0, 0.0D0, 0.0D0, t371)
      t374 = bggbH52J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t376 = bggbH52J2(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t385 = t27 * t374
      t390 = (0.90D2 * t27 * (-t334 * t374 + t376) - 0.180D3 * t91 * t37
     #4) * t147 / 0.2880D4 + t385 * t150 / 0.32D2 + t385 * t153 / 0.32D2
      t391 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t325, t323, 0.0D0, t390)
      t393 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t396 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t400 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t405 = 0.180D3 * t91 * t396
      t409 = t27 * t396
      t432 = -t27 * t393 / 0.64D2 - t134 * t396 / 0.5760D4 + (0.90D2 * t
     #27 * (t141 * t396 - t400) + t405) * t147 / 0.2880D4 - t409 * t150 
     #/ 0.32D2 - t409 * t153 / 0.32D2 - t409 * t51 / 0.64D2 - (0.90D2 * 
     #t27 * (t400 - t113 * t396) - t405) * t43 / 0.5760D4 - t102 * t400 
     #/ 0.5760D4 + (0.90D2 * t27 * (t159 * t396 - t400) + t405) * t41 / 
     #0.5760D4
      t433 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t432)
      t435 = t47 * t27 * t29 * t37 * t40 * t51 / 0.64D2 + t60 * t27 * t2
     #9 * t37 * t55 * t51 / 0.64D2 + t72 * t27 * t29 * t37 * t67 * t51 /
     # 0.64D2 + t84 * t27 * t29 * t37 * t79 * t51 / 0.64D2 + t168 * t167
     # + t207 * t206 + t234 * t233 + t260 * t259 + t279 * t278 + t321 * 
     #t320 + t353 * t352 + t372 * t371 + t391 * t390 + t433 * t432
      t436 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t437 = t27 * t436
      t443 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t452 = t437 * t150 / 0.32D2 + t437 * t51 / 0.64D2 + (0.90D2 * t27 
     #* (-t249 * t436 + t443) - 0.180D3 * t91 * t436) * t41 / 0.5760D4
      t453 = FJET(XB1, XB2, s, t236, 0.0D0, -t237, 0.0D0, 0.0D0, t452)
      t455 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t456 = t27 * t455
      t462 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t471 = t456 * t153 / 0.32D2 + t456 * t51 / 0.64D2 - (0.90D2 * t27 
     #* (t222 * t455 - t462) + 0.180D3 * t91 * t455) * t43 / 0.5760D4
      t472 = FJET(XB1, XB2, s, 0.0D0, -t211, 0.0D0, t210, 0.0D0, t471)
      t474 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t475 = t27 * t474
      t481 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t490 = t475 * t150 / 0.32D2 + t475 * t51 / 0.64D2 + (0.90D2 * t27 
     #* (-t249 * t474 + t481) - 0.180D3 * t91 * t474) * t41 / 0.5760D4
      t491 = FJET(XB1, XB2, s, -t237, 0.0D0, t236, 0.0D0, 0.0D0, t490)
      t493 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t494 = t27 * t493
      t499 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t238, x4)
      t509 = t494 * t150 / 0.32D2 + t494 * t51 / 0.64D2 + (0.90D2 * t27 
     #* (t499 - t249 * t493) - 0.180D3 * t91 * t493) * t41 / 0.5760D4
      t510 = FJET(XB1, XB2, s, 0.0D0, -t237, 0.0D0, t236, 0.0D0, t509)
      t512 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t513 = t27 * t512
      t519 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t528 = t513 * t153 / 0.32D2 + t513 * t51 / 0.64D2 - (0.90D2 * t27 
     #* (t222 * t512 - t519) + 0.180D3 * t91 * t512) * t43 / 0.5760D4
      t529 = FJET(XB1, XB2, s, t210, 0.0D0, -t211, 0.0D0, 0.0D0, t528)
      t531 = bggbH51J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t533 = bggbH51J2(s, XB1, XB2, z, lh, wd, t335, 0.0D0, 0.10D1, x4)
      t542 = t27 * t531
      t547 = (0.90D2 * t27 * (-t334 * t531 + t533) - 0.180D3 * t91 * t53
     #1) * t147 / 0.2880D4 + t542 * t150 / 0.32D2 + t542 * t153 / 0.32D2
      t548 = FJET(XB1, XB2, s, -t325, t323, 0.0D0, 0.0D0, 0.0D0, t547)
      t550 = t7 * s
      t551 = t1 * t324
      t552 = t550 * t551
      t554 = t550 * t1 * x1
      t556 = t2 * t324 * x3
      t558 = t2 * x1 * x3
      t559 = bggbH51J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, t238, x4)
      t563 = FJET(XB1, XB2, s, t552, -t554, -t556, t558, 0.0D0, -t27 * t
     #559 * t150 / 0.32D2)
      t570 = t3 * s * t551
      t573 = t2 * t324 * x2 * t328
      t574 = t1 ** 2
      t579 = s * t574 * x2 * t324 * x1 * t328
      t580 = bggbH51J1(s, XB1, XB2, z, lh, wd, t335, x2, 0.10D1, x4)
      t584 = FJET(XB1, XB2, s, t570, t323, -t573, 0.0D0, -t579, -t27 * t
     #580 * t153 / 0.32D2)
      t590 = bggbH54J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, t238, x4)
      t594 = FJET(XB1, XB2, s, -t554, t552, t558, -t556, 0.0D0, -t27 * t
     #590 * t150 / 0.32D2)
      t600 = bggbH52J1(s, XB1, XB2, z, lh, wd, t335, x2, 0.10D1, x4)
      t604 = FJET(XB1, XB2, s, -t573, 0.0D0, t570, t323, -t579, -t27 * t
     #600 * t153 / 0.32D2)
      t610 = bggbH52J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, t238, x4)
      t614 = FJET(XB1, XB2, s, -t556, t558, t552, -t554, 0.0D0, -t27 * t
     #610 * t150 / 0.32D2)
      t620 = bggbH53J1(s, XB1, XB2, z, lh, wd, t335, x2, 0.10D1, x4)
      t624 = FJET(XB1, XB2, s, 0.0D0, -t573, t323, t570, -t579, -t27 * t
     #620 * t153 / 0.32D2)
      t630 = bggbH54J1(s, XB1, XB2, z, lh, wd, t335, x2, 0.10D1, x4)
      t634 = FJET(XB1, XB2, s, t323, t570, 0.0D0, -t573, -t579, -t27 * t
     #630 * t153 / 0.32D2)
      t640 = bggbH53J1(s, XB1, XB2, z, lh, wd, t335, 0.0D0, t238, x4)
      t644 = FJET(XB1, XB2, s, t558, -t556, -t554, t552, 0.0D0, -t27 * t
     #640 * t150 / 0.32D2)
      t650 = t453 * t452 + t472 * t471 + t491 * t490 + t510 * t509 + t52
     #9 * t528 + t548 * t547 - t563 * t27 * t559 * t41 * t147 / 0.32D2 -
     # t584 * t27 * t580 * t43 * t147 / 0.32D2 - t594 * t27 * t590 * t41
     # * t147 / 0.32D2 - t604 * t27 * t600 * t43 * t147 / 0.32D2 - t614 
     #* t27 * t610 * t41 * t147 / 0.32D2 - t624 * t27 * t620 * t43 * t14
     #7 / 0.32D2 - t634 * t27 * t630 * t43 * t147 / 0.32D2 - t644 * t27 
     #* t640 * t41 * t147 / 0.32D2
      bggbH5n2em1 = t435 + t650

      end function



      doubleprecision function bggbH5n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 / x2
      t13 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t18 = z ** 2
      t21 = Sin(x4 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t25 = log(0.4D1 / t18 * t22)
      t28 = -0.180D3 * t4 * lh - 0.90D2 * t25 * t4
      t31 = 0.1D1 / x3
      t34 = -t6 * t7 / 0.32D2 - t6 * t10 / 0.64D2 - t4 * t13 / 0.64D2 - 
     #t28 * t5 / 0.5760D4 - t6 * t31 / 0.64D2
      t35 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t34)
      t37 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t38 = t4 * t37
      t43 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t50 = -t38 * t7 / 0.32D2 - t38 * t10 / 0.64D2 - t4 * t43 / 0.64D2 
     #- t28 * t37 / 0.5760D4 - t38 * t31 / 0.64D2
      t51 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t50)
      t53 = t2 * x1
      t54 = -0.1D1 + x1
      t55 = t2 * t54
      t56 = -t54
      t57 = bggbH53J1(s, XB1, XB2, z, lh, wd, t56, 0.0D0, 0.10D1, x4)
      t61 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t53, -t55, 0.0D0, t4 * t57 *
     # t7 / 0.32D2)
      t66 = bggbH52J1(s, XB1, XB2, z, lh, wd, t56, 0.0D0, 0.10D1, x4)
      t70 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t55, t53, 0.0D0, t4 * t66 *
     # t7 / 0.32D2)
      t75 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t76 = t4 * t75
      t85 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t88 = -t76 * t7 / 0.32D2 - t28 * t75 / 0.5760D4 - t76 * t31 / 0.64
     #D2 - t76 * t10 / 0.64D2 - t4 * t85 / 0.64D2
      t89 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t88)
      t91 = t2 * x3
      t92 = -0.1D1 + x3
      t93 = t2 * t92
      t94 = -t92
      t95 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t94, x4)
      t99 = FJET(XB1, XB2, s, 0.0D0, t91, 0.0D0, -t93, 0.0D0, t4 * t95 *
     # t31 / 0.64D2)
      t105 = x2 * s * t1
      t107 = t2 * (-0.1D1 + x2)
      t108 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t112 = FJET(XB1, XB2, s, 0.0D0, t105, 0.0D0, -t107, 0.0D0, t4 * t1
     #08 * t10 / 0.64D2)
      t117 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t121 = FJET(XB1, XB2, s, 0.0D0, -t107, 0.0D0, t105, 0.0D0, t4 * t1
     #17 * t10 / 0.64D2)
      t126 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t94, x4)
      t130 = FJET(XB1, XB2, s, 0.0D0, -t93, 0.0D0, t91, 0.0D0, t4 * t126
     # * t31 / 0.64D2)
      t135 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t138 = t4 * t135
      t145 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4
     #)
      t148 = -t28 * t135 / 0.5760D4 - t138 * t31 / 0.64D2 - t138 * t7 / 
     #0.32D2 - t138 * t10 / 0.64D2 - t4 * t145 / 0.64D2
      t149 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t148)
      t151 = bggbH54J1(s, XB1, XB2, z, lh, wd, t56, 0.0D0, 0.10D1, x4)
      t155 = FJET(XB1, XB2, s, t53, -t55, 0.0D0, 0.0D0, 0.0D0, t4 * t151
     # * t7 / 0.32D2)
      t160 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t94, x4)
      t164 = FJET(XB1, XB2, s, t91, 0.0D0, -t93, 0.0D0, 0.0D0, t4 * t160
     # * t31 / 0.64D2)
      t169 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t173 = FJET(XB1, XB2, s, t105, 0.0D0, -t107, 0.0D0, 0.0D0, t4 * t1
     #69 * t10 / 0.64D2)
      t178 = bggbH51J1(s, XB1, XB2, z, lh, wd, t56, 0.0D0, 0.10D1, x4)
      t182 = FJET(XB1, XB2, s, -t55, t53, 0.0D0, 0.0D0, 0.0D0, t4 * t178
     # * t7 / 0.32D2)
      t187 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.10D1, x4)
      t191 = FJET(XB1, XB2, s, -t107, 0.0D0, t105, 0.0D0, 0.0D0, t4 * t1
     #87 * t10 / 0.64D2)
      t196 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, t94, x4)
      t200 = FJET(XB1, XB2, s, -t93, 0.0D0, t91, 0.0D0, 0.0D0, t4 * t196
     # * t31 / 0.64D2)
      bggbH5n2em2 = t35 * t34 + t51 * t50 + t61 * t4 * t57 * t7 / 0.32D2
     # + t70 * t4 * t66 * t7 / 0.32D2 + t89 * t88 + t99 * t4 * t95 * t31
     # / 0.64D2 + t112 * t4 * t108 * t10 / 0.64D2 + t121 * t4 * t117 * t
     #10 / 0.64D2 + t130 * t4 * t126 * t31 / 0.64D2 + t149 * t148 + t155
     # * t4 * t151 * t7 / 0.32D2 + t164 * t4 * t160 * t31 / 0.64D2 + t17
     #3 * t4 * t169 * t10 / 0.64D2 + t182 * t4 * t178 * t7 / 0.32D2 + t1
     #91 * t4 * t187 * t10 / 0.64D2 + t200 * t4 * t196 * t31 / 0.64D2

      end function



      doubleprecision function bggbH5n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t4 * t5 / 
     #0.64D2)
      t11 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t4 * t11 
     #/ 0.64D2)
      t17 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t4 * t17 
     #/ 0.64D2)
      t23 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.10D1, 0.0D0, 0.10D1, x4)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t4 * t23 
     #/ 0.64D2)
      bggbH5n2em3 = -t8 * t4 * t5 / 0.64D2 - t14 * t4 * t11 / 0.64D2 - t
     #20 * t4 * t17 / 0.64D2 - t26 * t4 * t23 / 0.64D2

      end function



      doubleprecision function bggbH5n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      bggbH5n2em4 = 0.0D0

      end function


      doubleprecision function bggbH5n3e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = z ** 2
      t5 = 0.1D1 / t3 / z
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t5 * t8
      t11 = log(0.4D1 * t9)
      t12 = s ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = lh ** 2
      t17 = 0.3141592653589793D1 ** 2
      t19 = 0.180D3 * t15 - 0.30D2 * t17
      t21 = t11 ** 2
      t22 = t21 * t13
      t29 = -0.2884936567583026D3 - 0.120D3 * t15 * lh + 0.60D2 * lh * t
     #17
      t30 = t13 * t29
      t32 = t21 * t11 * t13
      t34 = -t14 * t19 - 0.90D2 * t22 * lh + t30 - 0.15D2 * t32
      t35 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t38 = t21 ** 2
      t43 = t17 ** 2
      t44 = t15 ** 2
      t54 = 0.15D2 / 0.4D1 * t38 * t13 - t14 * t29 + t13 * (0.5769873135
     #166051D3 * lh + t43 + 0.60D2 * t44 - 0.60D2 * t15 * t17) + t22 * t
     #19 / 0.2D1 + 0.30D2 * t32 * lh
      t55 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t60 = t13 * lh
      t64 = x3 * t5
      t67 = log(0.4D1 * t64 * t8)
      t68 = t67 ** 2
      t69 = -0.1D1 + x3
      t70 = 0.1D1 / t69
      t74 = log(-0.4D1 * t64 * t8 * t70)
      t75 = t74 ** 2
      t77 = cos(t6)
      t78 = x3 * z
      t80 = Sqrt(-t78 * t69)
      t84 = 0.1D1 / (-z - x3 + 0.2D1 * t77 * t80)
      t87 = -t68 / 0.2D1 - t75 * z * t84 / 0.2D1
      t91 = t13 * t19
      t96 = t74 * z * t84 + t67
      t104 = t68 * t67 / 0.6D1 + t75 * t74 * z * t84 / 0.6D1
      t108 = t30 * t55
      t111 = -z * t84 - 0.1D1
      t114 = 0.1D1 / x3
      t117 = x1 ** 2
      t118 = t117 * t8
      t119 = t118 * t5
      t121 = log(0.4D1 * t119)
      t123 = t121 ** 2
      t132 = t123 * t121
      t141 = 0.1D1 / x1
      t144 = z * t35
      t145 = t117 * x3
      t146 = t9 * t70
      t149 = log(-0.4D1 * t145 * t146)
      t150 = t149 * z
      t156 = log(0.4D1 * t145 * t9)
      t162 = t156 ** 2
      t166 = t149 ** 2
      t167 = t166 * z
      t176 = z * t55 * t84
      t183 = x2 ** 2
      t184 = x3 * t183
      t185 = t184 * t117
      t186 = -0.1D1 + x2
      t187 = t9 * t186
      t190 = log(-0.4D1 * t185 * t187)
      t191 = -t186
      t192 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t196 = log(0.4D1 * t184 * t119)
      t198 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t201 = log(-0.4D1 * t185 * t146)
      t202 = t201 * z
      t209 = -t192 + t55 + t176
      t214 = 0.1D1 / x2
      t215 = t214 * t141
      t218 = t183 * t117
      t221 = log(0.4D1 * t218 * t9)
      t225 = log(-0.4D1 * t218 * t187)
      t230 = t221 ** 2
      t235 = t225 ** 2
      t241 = -t55 + t192
      t247 = t5 * t183
      t250 = log(0.4D1 * t247 * t8)
      t251 = t250 ** 2
      t255 = t8 * t186
      t258 = log(-0.4D1 * t247 * t255)
      t260 = t258 ** 2
      t274 = t251 * t250
      t277 = t260 * t258
      t289 = log(-0.4D1 * t184 * t187)
      t293 = log(-0.4D1 * t184 * t146)
      t294 = t293 * z
      t300 = log(0.4D1 * t184 * t9)
      t306 = t289 ** 2
      t309 = t300 ** 2
      t314 = t293 ** 2
      t315 = t314 * z
      t328 = -t34 * t35 / 0.11520D5 - t54 * t55 / 0.11520D5 + ((0.90D2 *
     # t13 * t35 - 0.180D3 * t60 * t55) * t87 + (-0.180D3 * t60 * t35 + 
     #t91 * t55) * t96 + 0.90D2 * t13 * t55 * t104 + (t91 * t35 + t108) 
     #* t111) * t114 / 0.11520D5 + (-0.180D3 * t60 * (t121 * t35 - t123 
     #* t55 / 0.2D1) + t91 * (-t35 + t121 * t55) + 0.90D2 * t13 * (t132 
     #* t55 / 0.6D1 - t123 * t35 / 0.2D1) - t108) * t141 / 0.5760D4 + (-
     #0.180D3 * t60 * (-t35 - (t144 - t150 * t55) * t84 + t156 * t55) + 
     #0.90D2 * t13 * (t156 * t35 - t162 * t55 / 0.2D1 - (-t150 * t35 + t
     #167 * t55 / 0.2D1) * t84) + t91 * (-t176 - t55)) * t114 * t141 / 0
     #.5760D4 - (0.90D2 * t13 * (t190 * t192 - t196 * t55 + t35 - t198 +
     # (t144 - t202 * t55) * t84) - 0.180D3 * t60 * t209) * t114 * t215 
     #/ 0.2880D4 + (-0.180D3 * t60 * (t221 * t55 + t198 - t35 - t225 * t
     #192) + 0.90D2 * t13 * (-t230 * t55 / 0.2D1 + t221 * t35 - t225 * t
     #198 + t235 * t192 / 0.2D1) + t91 * t241) * t214 * t141 / 0.2880D4 
     #+ (-0.180D3 * t60 * (-t251 * t55 / 0.2D1 + t250 * t35 - t258 * t19
     #8 + t260 * t192 / 0.2D1) + t91 * (-t35 - t258 * t192 + t198 + t250
     # * t55) + 0.90D2 * t13 * (-t251 * t35 / 0.2D1 + t260 * t198 / 0.2D
     #1 + t274 * t55 / 0.6D1 - t277 * t192 / 0.6D1) + t30 * t241) * t214
     # / 0.5760D4 - (-0.180D3 * t60 * (-t198 + t289 * t192 + t35 + (t144
     # - t294 * t55) * t84 - t300 * t55) + 0.90D2 * t13 * (-t300 * t35 -
     # t306 * t192 / 0.2D1 + t309 * t55 / 0.2D1 + t289 * t198 + (-t294 *
     # t35 + t315 * t55 / 0.2D1) * t84) + t91 * t209) * t114 * t214 / 0.
     #5760D4
      t329 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t328)
      t331 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t334 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t336 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t351 = t30 * t331
      t355 = z * t334
      t366 = z * t336
      t376 = z * t331 * t84
      t384 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t386 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t393 = -t384 + t376 + t331
      t408 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t415 = t384 - t331
      t426 = 0.180D3 * t14 * lh + t91 + 0.45D2 * t22
      t512 = (-0.180D3 * t60 * (-t123 * t331 / 0.2D1 + t121 * t334 - t33
     #6) + t91 * (-t334 + t121 * t331) + 0.90D2 * t13 * (t121 * t336 + t
     #132 * t331 / 0.6D1 - t123 * t334 / 0.2D1) - t351) * t141 / 0.5760D
     #4 + (-0.180D3 * t60 * (-t334 - (t355 - t150 * t331) * t84 + t156 *
     # t331) + 0.90D2 * t13 * (-t336 + t156 * t334 - t162 * t331 / 0.2D1
     # - (t366 - t150 * t334 + t167 * t331 / 0.2D1) * t84) + t91 * (-t33
     #1 - t376)) * t114 * t141 / 0.5760D4 - (0.90D2 * t13 * (t334 - t196
     # * t331 + t190 * t384 - t386 + (t355 - t202 * t331) * t84) - 0.180
     #D3 * t60 * t393) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (-t3
     #34 + t386 + t221 * t331 - t225 * t384) + 0.90D2 * t13 * (-t230 * t
     #331 / 0.2D1 + t221 * t334 + t408 + t235 * t384 / 0.2D1 - t336 - t2
     #25 * t386) + t91 * t415) * t214 * t141 / 0.2880D4 - t54 * t331 / 0
     #.11520D5 - t426 * t336 / 0.11520D5 - t34 * t334 / 0.11520D5 + (-0.
     #180D3 * t60 * (-t336 + t250 * t334 + t408 - t258 * t386 - t251 * t
     #331 / 0.2D1 + t260 * t384 / 0.2D1) + t91 * (t386 - t334 + t250 * t
     #331 - t258 * t384) + 0.90D2 * t13 * (t250 * t336 - t251 * t334 / 0
     #.2D1 + t260 * t386 / 0.2D1 + t274 * t331 / 0.6D1 - t277 * t384 / 0
     #.6D1 - t258 * t408) + t30 * t415) * t214 / 0.5760D4 - (-0.180D3 * 
     #t60 * ((t355 - t294 * t331) * t84 - t386 + t334 + t289 * t384 - t3
     #00 * t331) + 0.90D2 * t13 * (-t306 * t384 / 0.2D1 - t408 - t300 * 
     #t334 + t309 * t331 / 0.2D1 + t336 + t289 * t386 + (t366 - t294 * t
     #334 + t315 * t331 / 0.2D1) * t84) + t91 * t393) * t114 * t214 / 0.
     #5760D4 + ((0.90D2 * t13 * t334 - 0.180D3 * t60 * t331) * t87 + (0.
     #90D2 * t13 * t336 - 0.180D3 * t60 * t334 + t91 * t331) * t96 + 0.9
     #0D2 * t13 * t331 * t104 + (-0.180D3 * t60 * t336 + t91 * t334 + t3
     #51) * t111) * t114 / 0.11520D5
      t513 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t512)
      t515 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t518 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t521 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t524 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t527 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t530 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t553 = -t521 + t530
      t558 = z * t518
      t571 = z * t515
      t583 = z * t521 * t84
      t584 = -t530 + t521 + t583
      t607 = t30 * t521
      t692 = -t426 * t515 / 0.11520D5 - t34 * t518 / 0.11520D5 - t54 * t
     #521 / 0.11520D5 + (-0.180D3 * t60 * (-t515 + t524 - t251 * t521 / 
     #0.2D1 - t258 * t527 + t250 * t518 + t260 * t530 / 0.2D1) + t91 * (
     #-t258 * t530 - t518 + t527 + t250 * t521) + 0.90D2 * t13 * (t250 *
     # t515 - t251 * t518 / 0.2D1 + t260 * t527 / 0.2D1 + t274 * t521 / 
     #0.6D1 - t277 * t530 / 0.6D1 - t258 * t524) + t30 * t553) * t214 / 
     #0.5760D4 - (-0.180D3 * t60 * ((t558 - t294 * t521) * t84 - t527 + 
     #t289 * t530 + t518 - t300 * t521) + 0.90D2 * t13 * (t289 * t527 - 
     #t300 * t518 + t515 - t306 * t530 / 0.2D1 + (t571 - t294 * t518 + t
     #315 * t521 / 0.2D1) * t84 - t524 + t309 * t521 / 0.2D1) + t91 * t5
     #84) * t114 * t214 / 0.5760D4 + (-0.180D3 * t60 * (-t515 - t123 * t
     #521 / 0.2D1 + t121 * t518) + t91 * (-t518 + t121 * t521) + 0.90D2 
     #* t13 * (t121 * t515 - t123 * t518 / 0.2D1 + t132 * t521 / 0.6D1) 
     #- t607) * t141 / 0.5760D4 + (-0.180D3 * t60 * (-(t558 - t150 * t52
     #1) * t84 + t156 * t521 - t518) + 0.90D2 * t13 * (-t515 + t156 * t5
     #18 - t162 * t521 / 0.2D1 - (t571 - t150 * t518 + t167 * t521 / 0.2
     #D1) * t84) + t91 * (-t583 - t521)) * t114 * t141 / 0.5760D4 - (0.9
     #0D2 * t13 * (-t527 - t196 * t521 + t518 + (t558 - t202 * t521) * t
     #84 + t190 * t530) - 0.180D3 * t60 * t584) * t114 * t215 / 0.2880D4
     # + (-0.180D3 * t60 * (-t225 * t530 - t518 + t527 + t221 * t521) + 
     #0.90D2 * t13 * (t524 + t221 * t518 - t225 * t527 - t230 * t521 / 0
     #.2D1 + t235 * t530 / 0.2D1 - t515) + t91 * t553) * t214 * t141 / 0
     #.2880D4 + ((0.90D2 * t13 * t518 - 0.180D3 * t60 * t521) * t87 + (0
     #.90D2 * t13 * t515 - 0.180D3 * t60 * t518 + t91 * t521) * t96 + 0.
     #90D2 * t13 * t521 * t104 + (-0.180D3 * t60 * t515 + t91 * t518 + t
     #607) * t111) * t114 / 0.11520D5
      t693 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t692)
      t695 = x2 * x3
      t696 = 0.1D1 - x3 + t695
      t697 = 0.1D1 / t696
      t698 = t695 * t697
      t699 = t2 * t698
      t700 = t69 * t697
      t701 = t2 * t700
      t702 = t184 * t118
      t704 = t696 ** 2
      t705 = 0.1D1 / t704
      t706 = t69 * t705
      t710 = log(0.4D1 * t702 * t5 * t186 * t706)
      t711 = t710 * z
      t712 = t186 * t69
      t714 = Sqrt(t78 * t712)
      t718 = 0.1D1 / (-z - x3 + t695 + 0.2D1 * t77 * t714)
      t719 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t720 = t718 * t719
      t722 = z * t718
      t723 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t724 = t722 * t723
      t728 = t722 * t719
      t739 = log(0.4D1 * t184 * t5 * t255 * t706)
      t740 = t739 * z
      t745 = t739 ** 2
      t746 = t745 * z
      t759 = -(0.90D2 * t13 * (t711 * t720 - t724) + 0.180D3 * t60 * t72
     #8) * t114 * t215 / 0.2880D4 - (-0.180D3 * t60 * (-t724 + t740 * t7
     #20) + 0.90D2 * t13 * (-t746 * t720 / 0.2D1 + t740 * t718 * t723) -
     # t91 * t728) * t114 * t214 / 0.5760D4
      t760 = FJET(XB1, XB2, s, 0.0D0, t699, 0.0D0, -t701, 0.0D0, t759)
      t763 = t1 * x1
      t764 = x1 * z
      t765 = -z - x1 + t764
      t766 = 0.1D1 / t765
      t768 = t186 * s * t763 * t766
      t769 = -0.1D1 + x1
      t770 = t2 * t769
      t772 = x2 * s * t763
      t773 = t1 ** 2
      t774 = s * t773
      t777 = x1 * t769 * t766
      t778 = t774 * t186 * t777
      t779 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t780 = 0.1D1 / t3
      t781 = t780 * t766
      t782 = t769 ** 2
      t784 = t781 * t782 * t186
      t787 = log(0.4D1 * t702 * t784)
      t788 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t798 = t218 * t8
      t801 = log(0.4D1 * t798 * t784)
      t806 = t801 ** 2
      t809 = bggbH53J3(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t819 = -(0.90D2 * t13 * (t779 - t787 * t788) - 0.180D3 * t60 * t78
     #8) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (t801 * t788 - t77
     #9) + 0.90D2 * t13 * (-t806 * t788 / 0.2D1 - t809 + t801 * t779) - 
     #t91 * t788) * t214 * t141 / 0.2880D4
      t820 = FJET(XB1, XB2, s, 0.0D0, t768, -t770, t772, -t778, t819)
      t823 = t2 * x1 * t766
      t824 = t774 * t777
      t825 = bggbH51J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t826 = t781 * t782
      t829 = log(-0.4D1 * t118 * t826)
      t830 = t829 ** 2
      t831 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t834 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t845 = t830 * t829
      t855 = t145 * t8
      t858 = log(-0.4D1 * t855 * t826)
      t860 = z * t765
      t861 = t860 * t834
      t863 = t781 * t782 * t70
      t866 = log(0.4D1 * t855 * t863)
      t867 = t866 * z
      t868 = t765 * t831
      t871 = x1 * x3
      t872 = t871 * z
      t874 = 0.2D1 * t145 * z
      t875 = x1 * t3
      t876 = x3 * t3
      t877 = t876 * x1
      t878 = t145 * t3
      t879 = x3 * t765
      t881 = Sqrt(t879 * t69)
      t886 = 0.1D1 / (-t764 - t872 - t78 - t145 + t874 + t875 + t877 - t
     #878 + 0.2D1 * t77 * t881 * z - t3)
      t895 = t866 ** 2
      t896 = t895 * z
      t901 = t858 ** 2
      t909 = t831 - t860 * t831 * t886
      t917 = log(0.4D1 * t702 * t863)
      t918 = t917 * z
      t923 = t766 * t782
      t927 = log(-0.4D1 * t185 * t8 * t780 * t923)
      t941 = log(-0.4D1 * t798 * t826)
      t946 = t941 ** 2
      t958 = (-0.180D3 * t60 * (t825 + t830 * t831 / 0.2D1 - t829 * t834
     #) + t91 * (t834 - t829 * t831) + 0.90D2 * t13 * (-t829 * t825 + t8
     #30 * t834 / 0.2D1 - t845 * t831 / 0.6D1) + t30 * t831) * t141 / 0.
     #5760D4 + (-0.180D3 * t60 * (t834 - t858 * t831 - (t861 - t867 * t8
     #68) * t886) + 0.90D2 * t13 * (-t858 * t834 - (t860 * t825 - t867 *
     # t765 * t834 + t896 * t868 / 0.2D1) * t886 + t825 + t901 * t831 / 
     #0.2D1) + t91 * t909) * t114 * t141 / 0.5760D4 - (0.90D2 * t13 * (-
     #t834 + (t861 - t918 * t868) * t886 + t927 * t831) + 0.180D3 * t60 
     #* t909) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (t834 - t941 
     #* t831) + 0.90D2 * t13 * (t946 * t831 / 0.2D1 - t941 * t834 + t825
     #) + t91 * t831) * t214 * t141 / 0.2880D4
      t959 = FJET(XB1, XB2, s, 0.0D0, -t770, -t823, 0.0D0, t824, t958)
      t961 = bggbH53J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t962 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t965 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t986 = t860 * t965
      t987 = t765 * t962
      t1009 = t962 - t860 * t962 * t886
      t1044 = (-0.180D3 * t60 * (t961 + t830 * t962 / 0.2D1 - t829 * t96
     #5) + t91 * (t965 - t829 * t962) + 0.90D2 * t13 * (t830 * t965 / 0.
     #2D1 - t845 * t962 / 0.6D1 - t829 * t961) + t30 * t962) * t141 / 0.
     #5760D4 + (-0.180D3 * t60 * (t965 - t858 * t962 - (t986 - t867 * t9
     #87) * t886) + 0.90D2 * t13 * (-t858 * t965 + t961 - (t860 * t961 -
     # t867 * t765 * t965 + t896 * t987 / 0.2D1) * t886 + t901 * t962 / 
     #0.2D1) + t91 * t1009) * t114 * t141 / 0.5760D4 - (0.90D2 * t13 * (
     #-t965 + (t986 - t918 * t987) * t886 + t927 * t962) + 0.180D3 * t60
     # * t1009) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (t965 - t94
     #1 * t962) + 0.90D2 * t13 * (-t941 * t965 + t946 * t962 / 0.2D1 + t
     #961) + t91 * t962) * t214 * t141 / 0.2880D4
      t1045 = FJET(XB1, XB2, s, 0.0D0, -t823, -t770, 0.0D0, t824, t1044)
      t1047 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1048 = t722 * t1047
      t1049 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1050 = t718 * t1049
      t1059 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1064 = t722 * t1049
      t1080 = -(-0.180D3 * t60 * (-t1048 + t740 * t1050) + 0.90D2 * t13 
     #* (t740 * t718 * t1047 - t746 * t1050 / 0.2D1 - t722 * t1059) - t9
     #1 * t1064) * t114 * t214 / 0.5760D4 - (0.90D2 * t13 * (-t1048 + t7
     #11 * t1050) + 0.180D3 * t60 * t1064) * t114 * t215 / 0.2880D4
      t1081 = FJET(XB1, XB2, s, 0.0D0, -t701, 0.0D0, t699, 0.0D0, t1080)
      t1083 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x
     #4)
      t1086 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x
     #4)
      t1101 = t30 * t1083
      t1106 = z * t1086
      t1125 = z * t1083 * t84
      t1136 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t1137 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, 0.10D1, x4)
      t1142 = t1125 - t1137 + t1083
      t1163 = t1137 - t1083
      t1248 = (-0.180D3 * t60 * (-t123 * t1083 / 0.2D1 + t121 * t1086) +
     # t91 * (-t1086 + t121 * t1083) + 0.90D2 * t13 * (t132 * t1083 / 0.
     #6D1 - t123 * t1086 / 0.2D1) - t1101) * t141 / 0.5760D4 + (-0.180D3
     # * t60 * (-t1086 + t156 * t1083 - (t1106 - t150 * t1083) * t84) + 
     #0.90D2 * t13 * (-t162 * t1083 / 0.2D1 - (-t150 * t1086 + t167 * t1
     #083 / 0.2D1) * t84 + t156 * t1086) + t91 * (-t1125 - t1083)) * t11
     #4 * t141 / 0.5760D4 - (0.90D2 * t13 * (t1086 - t196 * t1083 + (t11
     #06 - t202 * t1083) * t84 - t1136 + t190 * t1137) - 0.180D3 * t60 *
     # t1142) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (-t225 * t113
     #7 - t1086 + t221 * t1083 + t1136) + 0.90D2 * t13 * (-t225 * t1136 
     #+ t221 * t1086 + t235 * t1137 / 0.2D1 - t230 * t1083 / 0.2D1) + t9
     #1 * t1163) * t214 * t141 / 0.2880D4 + ((0.90D2 * t13 * t1086 - 0.1
     #80D3 * t60 * t1083) * t87 + (-0.180D3 * t60 * t1086 + t91 * t1083)
     # * t96 + 0.90D2 * t13 * t1083 * t104 + (t91 * t1086 + t1101) * t11
     #1) * t114 / 0.11520D5 - t54 * t1083 / 0.11520D5 + (-0.180D3 * t60 
     #* (t260 * t1137 / 0.2D1 - t251 * t1083 / 0.2D1 - t258 * t1136 + t2
     #50 * t1086) + t91 * (-t258 * t1137 + t250 * t1083 - t1086 + t1136)
     # + 0.90D2 * t13 * (-t251 * t1086 / 0.2D1 + t260 * t1136 / 0.2D1 + 
     #t274 * t1083 / 0.6D1 - t277 * t1137 / 0.6D1) + t30 * t1163) * t214
     # / 0.5760D4 - (-0.180D3 * t60 * (-t300 * t1083 + t1086 + t289 * t1
     #137 - t1136 + (t1106 - t294 * t1083) * t84) + 0.90D2 * t13 * (-t30
     #6 * t1137 / 0.2D1 + t309 * t1083 / 0.2D1 - t300 * t1086 + t289 * t
     #1136 + (-t294 * t1086 + t315 * t1083 / 0.2D1) * t84) + t91 * t1142
     #) * t114 * t214 / 0.5760D4 - t34 * t1086 / 0.11520D5
      t1249 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1248)
      t1251 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t1252 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t1267 = bggbH51J3(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t1278 = -(0.90D2 * t13 * (t1251 - t787 * t1252) - 0.180D3 * t60 * 
     #t1252) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (-t1251 + t801
     # * t1252) + 0.90D2 * t13 * (t801 * t1251 - t1267 - t806 * t1252 / 
     #0.2D1) - t91 * t1252) * t214 * t141 / 0.2880D4
      t1279 = FJET(XB1, XB2, s, t772, -t770, t768, 0.0D0, -t778, t1278)
      t1281 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1282 = t718 * t1281
      t1284 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1285 = t722 * t1284
      t1289 = t722 * t1281
      t1300 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1314 = -(0.90D2 * t13 * (t711 * t1282 - t1285) + 0.180D3 * t60 * 
     #t1289) * t114 * t215 / 0.2880D4 - (-0.180D3 * t60 * (-t1285 + t740
     # * t1282) + 0.90D2 * t13 * (-t722 * t1300 + t740 * t718 * t1284 - 
     #t746 * t1282 / 0.2D1) - t91 * t1289) * t114 * t214 / 0.5760D4
      t1315 = FJET(XB1, XB2, s, t699, 0.0D0, -t701, 0.0D0, 0.0D0, t1314)
      t1317 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t1318 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t1343 = -(0.90D2 * t13 * (t1317 - t787 * t1318) - 0.180D3 * t60 * 
     #t1318) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (t801 * t1318 
     #- t1317) + 0.90D2 * t13 * (-t806 * t1318 / 0.2D1 + t801 * t1317) -
     # t91 * t1318) * t214 * t141 / 0.2880D4
      t1344 = FJET(XB1, XB2, s, t768, 0.0D0, t772, -t770, -t778, t1343)
      t1349 = t69 * s * t1 * t769 * t697
      t1350 = t2 * x1
      t1352 = Sqrt(-t879 * t712)
      t1353 = t77 * t1352
      t1359 = t1350 * x2 * (-x3 + t695 - z + t78 - x1 + t871 + t764 - t8
     #72 + 0.2D1 * t1353) * t766 * t697
      t1360 = t770 * t698
      t1363 = t184 * x1
      t1365 = t184 * t764
      t1369 = t1350 * (-x2 + t695 + 0.2D1 * t1353 * x2 + 0.1D1 - x3 + t1
     #363 + t184 * z - t1365) * t766 * t697
      t1370 = x2 * x1
      t1371 = t1370 * z
      t1372 = -z - t1370 + t1371
      t1373 = t765 * t1372
      t1376 = x2 * t117
      t1387 = t78 + t145 - t875 + t764 + 0.2D1 * t1353 * t1371 + t3 + t1
     #376 - 0.2D1 * t1353 * t1370 + t1365 + t876 * t1370 + 0.2D1 * t145 
     #* x2 * z - 0.2D1 * t695 * t764 - t145 * t3 * x2
      t1397 = t1371 - 0.2D1 * t1353 * z - t145 * x2 - t695 * z + t695 * 
     #x1 - t1370 * t3 - 0.2D1 * t1376 * z + t1376 * t3 - t1363 - t877 + 
     #t878 + t872 - t874
      t1399 = 0.1D1 / (t1387 + t1397)
      t1400 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1409 = log(-0.4D1 * t184 * t118 * t780 * t923 * t712 * t705)
      t1410 = t1409 * t765
      t1411 = t1372 * t1399
      t1412 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1413 = t1411 * t1412
      t1418 = t60 * t765
      t1421 = 0.90D2 * t13 * (-t1373 * t1399 * t1400 + t1410 * t1413) + 
     #0.180D3 * t1418 * t1413
      t1425 = FJET(XB1, XB2, s, t1349, t1359, -t1360, -t1369, -t778, -t1
     #421 * t114 * t215 / 0.2880D4)
      t1428 = t114 * t214 * t141
      t1431 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1434 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1435 = t1411 * t1434
      t1442 = 0.90D2 * t13 * (-t1373 * t1399 * t1431 + t1410 * t1435) + 
     #0.180D3 * t1418 * t1435
      t1446 = FJET(XB1, XB2, s, t1359, t1349, -t1369, -t1360, -t778, -t1
     #442 * t114 * t215 / 0.2880D4)
      t1450 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1453 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1473 = t860 * t1453
      t1474 = t765 * t1450
      t1495 = -t860 * t1450 * t886 + t1450
      t1530 = (-0.180D3 * t60 * (t830 * t1450 / 0.2D1 - t829 * t1453) + 
     #t91 * (t1453 - t829 * t1450) + 0.90D2 * t13 * (t830 * t1453 / 0.2D
     #1 - t845 * t1450 / 0.6D1) + t30 * t1450) * t141 / 0.5760D4 + (-0.1
     #80D3 * t60 * (-t858 * t1450 + t1453 - (t1473 - t867 * t1474) * t88
     #6) + 0.90D2 * t13 * (t901 * t1450 / 0.2D1 - (-t867 * t765 * t1453 
     #+ t896 * t1474 / 0.2D1) * t886 - t858 * t1453) + t91 * t1495) * t1
     #14 * t141 / 0.5760D4 - (0.90D2 * t13 * (-t1453 + t927 * t1450 + (t
     #1473 - t918 * t1474) * t886) + 0.180D3 * t60 * t1495) * t114 * t21
     #5 / 0.2880D4 + (-0.180D3 * t60 * (t1453 - t941 * t1450) + 0.90D2 *
     # t13 * (t946 * t1450 / 0.2D1 - t941 * t1453) + t91 * t1450) * t214
     # * t141 / 0.2880D4
      t1531 = FJET(XB1, XB2, s, -t770, 0.0D0, 0.0D0, -t823, t824, t1530)
      t1533 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t1534 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, t191, 0.10D1, x4)
      t1559 = -(0.90D2 * t13 * (t1533 - t787 * t1534) - 0.180D3 * t60 * 
     #t1534) * t114 * t215 / 0.2880D4 + (-0.180D3 * t60 * (-t1533 + t801
     # * t1534) + 0.90D2 * t13 * (-t806 * t1534 / 0.2D1 + t801 * t1533) 
     #- t91 * t1534) * t214 * t141 / 0.2880D4
      t1560 = FJET(XB1, XB2, s, -t770, t772, 0.0D0, t768, -t778, t1559)
      t1562 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1565 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1584 = t860 * t1565
      t1585 = t765 * t1562
      t1607 = -t860 * t1562 * t886 + t1562
      t1642 = (-0.180D3 * t60 * (t830 * t1562 / 0.2D1 - t829 * t1565) + 
     #t91 * (t1565 - t829 * t1562) + 0.90D2 * t13 * (t830 * t1565 / 0.2D
     #1 - t845 * t1562 / 0.6D1) + t30 * t1562) * t141 / 0.5760D4 + (-0.1
     #80D3 * t60 * (-(t1584 - t867 * t1585) * t886 + t1565 - t858 * t156
     #2) + 0.90D2 * t13 * (t901 * t1562 / 0.2D1 - (-t867 * t765 * t1565 
     #+ t896 * t1585 / 0.2D1) * t886 - t858 * t1565) + t91 * t1607) * t1
     #14 * t141 / 0.5760D4 - (0.90D2 * t13 * (-t1565 + (t1584 - t918 * t
     #1585) * t886 + t927 * t1562) + 0.180D3 * t60 * t1607) * t114 * t21
     #5 / 0.2880D4 + (-0.180D3 * t60 * (-t941 * t1562 + t1565) + 0.90D2 
     #* t13 * (t946 * t1562 / 0.2D1 - t941 * t1565) + t91 * t1562) * t21
     #4 * t141 / 0.2880D4
      t1643 = FJET(XB1, XB2, s, -t823, 0.0D0, 0.0D0, -t770, t824, t1642)
      t1645 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1646 = t718 * t1645
      t1648 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t191, -t700, x4)
      t1649 = t722 * t1648
      t1653 = t722 * t1645
      t1676 = -(0.90D2 * t13 * (t711 * t1646 - t1649) + 0.180D3 * t60 * 
     #t1653) * t114 * t215 / 0.2880D4 - (-0.180D3 * t60 * (-t1649 + t740
     # * t1646) + 0.90D2 * t13 * (t740 * t718 * t1648 - t746 * t1646 / 0
     #.2D1) - t91 * t1653) * t114 * t214 / 0.5760D4
      t1677 = FJET(XB1, XB2, s, -t701, 0.0D0, t699, 0.0D0, 0.0D0, t1676)
      t1679 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1680 = t1411 * t1679
      t1682 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1690 = 0.90D2 * t13 * (t1410 * t1680 - t1373 * t1399 * t1682) + 0
     #.180D3 * t1418 * t1680
      t1694 = FJET(XB1, XB2, s, -t1369, -t1360, t1359, t1349, -t778, -t1
     #690 * t114 * t215 / 0.2880D4)
      t1698 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1699 = t1411 * t1698
      t1701 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, t191, -t700, x4)
      t1709 = 0.90D2 * t13 * (t1410 * t1699 - t1373 * t1399 * t1701) + 0
     #.180D3 * t1418 * t1699
      t1713 = FJET(XB1, XB2, s, -t1360, -t1369, t1349, t1359, -t778, -t1
     #709 * t114 * t215 / 0.2880D4)
      bggbH5n3e1 = t329 * t328 + t513 * t512 + t693 * t692 + t760 * t759
     # + t820 * t819 + t959 * t958 + t1045 * t1044 + t1081 * t1080 + t12
     #49 * t1248 + t1279 * t1278 + t1315 * t1314 + t1344 * t1343 - t1425
     # * t1421 * t1428 / 0.2880D4 - t1446 * t1442 * t1428 / 0.2880D4 + t
     #1531 * t1530 + t1560 * t1559 + t1643 * t1642 + t1677 * t1676 - t16
     #94 * t1690 * t1428 / 0.2880D4 - t1713 * t1709 * t1428 / 0.2880D4

      end function



      doubleprecision function bggbH5n3e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t6 = z * t5
      t7 = x1 ** 2
      t8 = t7 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t14 = 0.1D1 / t12 / z
      t15 = t11 * t14
      t16 = -0.1D1 + x3
      t17 = 0.1D1 / t16
      t18 = t15 * t17
      t21 = log(-0.4D1 * t8 * t18)
      t22 = t21 * z
      t23 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = cos(t9)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t16)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t8 * t15)
      t42 = t4 * lh
      t44 = z * t23 * t33
      t49 = 0.1D1 / x3
      t51 = 0.1D1 / x1
      t54 = 0.1D1 - x2
      t55 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t56 = -t55 + t23 + t44
      t58 = 0.1D1 / x2
      t60 = t49 * t58 * t51
      t63 = x2 ** 2
      t64 = t63 * t7
      t67 = log(0.4D1 * t64 * t15)
      t69 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t70 = -t54
      t71 = t15 * t70
      t74 = log(-0.4D1 * t64 * t71)
      t79 = -t23 + t55
      t86 = t7 * t11
      t89 = log(0.4D1 * t86 * t14)
      t95 = t89 ** 2
      t101 = lh ** 2
      t103 = 0.3141592653589793D1 ** 2
      t105 = 0.180D3 * t101 - 0.30D2 * t103
      t106 = t4 * t105
      t107 = t106 * t23
      t116 = x3 * t14
      t120 = log(-0.4D1 * t116 * t11 * t17)
      t125 = log(0.4D1 * t116 * t11)
      t126 = t120 * z * t33 + t125
      t129 = t125 ** 2
      t130 = t120 ** 2
      t134 = -t129 / 0.2D1 - t130 * z * t33 / 0.2D1
      t141 = -z * t33 - 0.1D1
      t147 = log(0.4D1 * t15)
      t148 = t147 * t4
      t151 = t147 ** 2
      t152 = t151 * t4
      t154 = 0.180D3 * t148 * lh + t106 + 0.45D2 * t152
      t157 = x3 * t63
      t160 = log(-0.4D1 * t157 * t71)
      t164 = log(-0.4D1 * t157 * t18)
      t165 = t164 * z
      t171 = log(0.4D1 * t157 * t15)
      t182 = t14 * t63
      t183 = t11 * t70
      t186 = log(-0.4D1 * t182 * t183)
      t190 = log(0.4D1 * t182 * t11)
      t195 = t190 ** 2
      t200 = t186 ** 2
      t222 = -t148 * t105 - 0.90D2 * t152 * lh + t4 * (-0.28849365675830
     #26D3 - 0.120D3 * t101 * lh + 0.60D2 * lh * t103) - 0.15D2 * t151 *
     # t147 * t4
      t225 = (0.90D2 * t4 * (-t5 - (t6 - t22 * t23) * t33 + t37 * t23) -
     # 0.180D3 * t42 * (-t44 - t23)) * t49 * t51 / 0.5760D4 - t4 * t56 *
     # t60 / 0.32D2 + (0.90D2 * t4 * (t67 * t23 + t69 - t5 - t74 * t55) 
     #- 0.180D3 * t42 * t79) * t58 * t51 / 0.2880D4 + (-0.180D3 * t42 * 
     #(-t5 + t89 * t23) + 0.90D2 * t4 * (t89 * t5 - t95 * t23 / 0.2D1) -
     # t107) * t51 / 0.5760D4 + ((0.90D2 * t4 * t5 - 0.180D3 * t42 * t23
     #) * t126 + 0.90D2 * t4 * t23 * t134 + (-0.180D3 * t42 * t5 + t107)
     # * t141) * t49 / 0.11520D5 - t154 * t5 / 0.11520D5 - (0.90D2 * t4 
     #* (-t69 + t160 * t55 + t5 + (t6 - t165 * t23) * t33 - t171 * t23) 
     #- 0.180D3 * t42 * t56) * t49 * t58 / 0.5760D4 + (-0.180D3 * t42 * 
     #(-t5 - t186 * t55 + t69 + t190 * t23) + 0.90D2 * t4 * (-t195 * t23
     # / 0.2D1 + t190 * t5 - t186 * t69 + t200 * t55 / 0.2D1) + t106 * t
     #79) * t58 / 0.5760D4 - t222 * t23 / 0.11520D5
      t226 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t225)
      t228 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t229 = z * t228
      t230 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t239 = z * t230 * t33
      t247 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t248 = -t247 + t239 + t230
      t252 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t258 = t247 - t230
      t272 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t276 = t106 * t230
      t322 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t337 = -0.180D3 * t42 - 0.90D2 * t148
      t340 = (0.90D2 * t4 * (-t228 - (t229 - t22 * t230) * t33 + t37 * t
     #230) - 0.180D3 * t42 * (-t230 - t239)) * t49 * t51 / 0.5760D4 - t4
     # * t248 * t60 / 0.32D2 + (0.90D2 * t4 * (-t228 + t252 + t67 * t230
     # - t74 * t247) - 0.180D3 * t42 * t258) * t58 * t51 / 0.2880D4 + (-
     #0.180D3 * t42 * (-t228 + t89 * t230) + 0.90D2 * t4 * (-t95 * t230 
     #/ 0.2D1 + t89 * t228 - t272) - t276) * t51 / 0.5760D4 - t222 * t23
     #0 / 0.11520D5 + ((0.90D2 * t4 * t228 - 0.180D3 * t42 * t230) * t12
     #6 + 0.90D2 * t4 * t230 * t134 + (0.90D2 * t4 * t272 - 0.180D3 * t4
     #2 * t228 + t276) * t141) * t49 / 0.11520D5 - t154 * t228 / 0.11520
     #D5 - (0.90D2 * t4 * ((t229 - t165 * t230) * t33 - t252 + t228 + t1
     #60 * t247 - t171 * t230) - 0.180D3 * t42 * t248) * t49 * t58 / 0.5
     #760D4 + (-0.180D3 * t42 * (t252 - t228 + t190 * t230 - t186 * t247
     #) + 0.90D2 * t4 * (-t272 + t190 * t228 + t322 - t186 * t252 - t195
     # * t230 / 0.2D1 + t200 * t247 / 0.2D1) + t106 * t258) * t58 / 0.57
     #60D4 - t337 * t272 / 0.11520D5
      t341 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t340)
      t343 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t344 = z * t343
      t345 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t354 = z * t345 * t33
      t362 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t363 = -t362 + t345 + t354
      t368 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t373 = -t345 + t362
      t384 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t391 = t106 * t345
      t436 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t452 = (0.90D2 * t4 * (-(t344 - t22 * t345) * t33 + t37 * t345 - t
     #343) - 0.180D3 * t42 * (-t354 - t345)) * t49 * t51 / 0.5760D4 - t4
     # * t363 * t60 / 0.32D2 + (0.90D2 * t4 * (-t74 * t362 - t343 + t368
     # + t67 * t345) - 0.180D3 * t42 * t373) * t58 * t51 / 0.2880D4 + (-
     #0.180D3 * t42 * (-t343 + t89 * t345) + 0.90D2 * t4 * (-t384 - t95 
     #* t345 / 0.2D1 + t89 * t343) - t391) * t51 / 0.5760D4 - t222 * t34
     #5 / 0.11520D5 + ((0.90D2 * t4 * t343 - 0.180D3 * t42 * t345) * t12
     #6 + 0.90D2 * t4 * t345 * t134 + (0.90D2 * t4 * t384 - 0.180D3 * t4
     #2 * t343 + t391) * t141) * t49 / 0.11520D5 - t154 * t343 / 0.11520
     #D5 - (0.90D2 * t4 * ((t344 - t165 * t345) * t33 - t368 + t160 * t3
     #62 + t343 - t171 * t345) - 0.180D3 * t42 * t363) * t49 * t58 / 0.5
     #760D4 + (-0.180D3 * t42 * (-t186 * t362 - t343 + t368 + t190 * t34
     #5) + 0.90D2 * t4 * (-t384 + t436 - t195 * t345 / 0.2D1 - t186 * t3
     #68 + t190 * t343 + t200 * t362 / 0.2D1) + t106 * t373) * t58 / 0.5
     #760D4 - t337 * t384 / 0.11520D5
      t453 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t452)
      t455 = x2 * x3
      t456 = 0.1D1 - x3 + t455
      t457 = 0.1D1 / t456
      t458 = t455 * t457
      t459 = t2 * t458
      t460 = t16 * t457
      t461 = t2 * t460
      t463 = t70 * t16
      t465 = Sqrt(t27 * t463)
      t469 = 0.1D1 / (-z - x3 + t455 + 0.2D1 * t26 * t465)
      t470 = t4 * z * t469
      t471 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t473 = t58 * t51
      t477 = z * t469
      t478 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t481 = t456 ** 2
      t487 = log(0.4D1 * t157 * t14 * t183 * t16 / t481)
      t488 = t487 * z
      t501 = t470 * t471 * t49 * t473 / 0.32D2 - (0.90D2 * t4 * (-t477 *
     # t478 + t488 * t469 * t471) + 0.180D3 * t42 * t477 * t471) * t49 *
     # t58 / 0.5760D4
      t502 = FJET(XB1, XB2, s, 0.0D0, t459, 0.0D0, -t461, 0.0D0, t501)
      t505 = t1 * x1
      t506 = x1 * z
      t507 = -z - x1 + t506
      t508 = 0.1D1 / t507
      t510 = t70 * s * t505 * t508
      t511 = -0.1D1 + x1
      t512 = t2 * t511
      t514 = x2 * s * t505
      t515 = t1 ** 2
      t516 = s * t515
      t519 = x1 * t511 * t508
      t520 = t516 * t70 * t519
      t521 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t525 = t64 * t11
      t527 = 0.1D1 / t12 * t508
      t528 = t511 ** 2
      t533 = log(0.4D1 * t525 * t527 * t528 * t70)
      t535 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t545 = -t4 * t521 * t60 / 0.32D2 + (0.90D2 * t4 * (t533 * t521 - t
     #535) + 0.180D3 * t42 * t521) * t58 * t51 / 0.2880D4
      t546 = FJET(XB1, XB2, s, 0.0D0, t510, -t512, t514, -t520, t545)
      t549 = t2 * x1 * t508
      t550 = t516 * t519
      t551 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t552 = t8 * t11
      t553 = t527 * t528
      t556 = log(-0.4D1 * t552 * t553)
      t557 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t559 = z * t507
      t565 = log(0.4D1 * t552 * t527 * t528 * t17)
      t566 = t565 * z
      t570 = x1 * x3
      t571 = t570 * z
      t573 = 0.2D1 * t8 * z
      t574 = x1 * t12
      t575 = x3 * t12
      t576 = t575 * x1
      t577 = t8 * t12
      t578 = x3 * t507
      t580 = Sqrt(t578 * t16)
      t585 = 0.1D1 / (-t506 - t571 - t27 - t8 + t573 + t574 + t576 - t57
     #7 + 0.2D1 * t26 * t580 * z - t12)
      t592 = t557 - t559 * t557 * t585
      t605 = log(-0.4D1 * t525 * t553)
      t618 = log(-0.4D1 * t86 * t553)
      t623 = bggbH51J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t624 = t618 ** 2
      t635 = (0.90D2 * t4 * (t551 - t556 * t557 - (t559 * t551 - t566 * 
     #t507 * t557) * t585) - 0.180D3 * t42 * t592) * t49 * t51 / 0.5760D
     #4 + t4 * t592 * t60 / 0.32D2 + (0.90D2 * t4 * (t551 - t605 * t557)
     # - 0.180D3 * t42 * t557) * t58 * t51 / 0.2880D4 + (-0.180D3 * t42 
     #* (t551 - t618 * t557) + 0.90D2 * t4 * (t623 + t624 * t557 / 0.2D1
     # - t618 * t551) + t106 * t557) * t51 / 0.5760D4
      t636 = FJET(XB1, XB2, s, 0.0D0, -t512, -t549, 0.0D0, t550, t635)
      t638 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t639 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t651 = t639 - t559 * t639 * t585
      t676 = bggbH53J3(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t687 = (0.90D2 * t4 * (t638 - t556 * t639 - (t559 * t638 - t566 * 
     #t507 * t639) * t585) - 0.180D3 * t42 * t651) * t49 * t51 / 0.5760D
     #4 + t4 * t651 * t60 / 0.32D2 + (0.90D2 * t4 * (t638 - t605 * t639)
     # - 0.180D3 * t42 * t639) * t58 * t51 / 0.2880D4 + (-0.180D3 * t42 
     #* (t638 - t618 * t639) + 0.90D2 * t4 * (t676 + t624 * t639 / 0.2D1
     # - t618 * t638) + t106 * t639) * t51 / 0.5760D4
      t688 = FJET(XB1, XB2, s, 0.0D0, -t549, -t512, 0.0D0, t550, t687)
      t690 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t695 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t709 = t470 * t690 * t49 * t473 / 0.32D2 - (0.90D2 * t4 * (-t477 *
     # t695 + t488 * t469 * t690) + 0.180D3 * t42 * t477 * t690) * t49 *
     # t58 / 0.5760D4
      t710 = FJET(XB1, XB2, s, 0.0D0, -t461, 0.0D0, t459, 0.0D0, t709)
      t712 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t713 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t715 = z * t712
      t723 = z * t713 * t33
      t731 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t732 = t723 - t731 + t713
      t738 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, 0.10D1, x4)
      t742 = t731 - t713
      t759 = t106 * t713
      t815 = (0.90D2 * t4 * (-t712 + t37 * t713 - (t715 - t22 * t713) * 
     #t33) - 0.180D3 * t42 * (-t723 - t713)) * t49 * t51 / 0.5760D4 - t4
     # * t732 * t60 / 0.32D2 + (0.90D2 * t4 * (-t74 * t731 - t712 + t67 
     #* t713 + t738) - 0.180D3 * t42 * t742) * t58 * t51 / 0.2880D4 + (-
     #0.180D3 * t42 * (-t712 + t89 * t713) + 0.90D2 * t4 * (-t95 * t713 
     #/ 0.2D1 + t89 * t712) - t759) * t51 / 0.5760D4 - (0.90D2 * t4 * (-
     #t171 * t713 + t712 + t160 * t731 - t738 + (t715 - t165 * t713) * t
     #33) - 0.180D3 * t42 * t732) * t49 * t58 / 0.5760D4 + (-0.180D3 * t
     #42 * (-t186 * t731 + t190 * t713 - t712 + t738) + 0.90D2 * t4 * (t
     #200 * t731 / 0.2D1 - t195 * t713 / 0.2D1 - t186 * t738 + t190 * t7
     #12) + t106 * t742) * t58 / 0.5760D4 - t222 * t713 / 0.11520D5 + ((
     #0.90D2 * t4 * t712 - 0.180D3 * t42 * t713) * t126 + 0.90D2 * t4 * 
     #t713 * t134 + (-0.180D3 * t42 * t712 + t759) * t141) * t49 / 0.115
     #20D5 - t154 * t712 / 0.11520D5
      t816 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t815)
      t818 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t822 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t833 = -t4 * t818 * t60 / 0.32D2 + (0.90D2 * t4 * (-t822 + t533 * 
     #t818) + 0.180D3 * t42 * t818) * t58 * t51 / 0.2880D4
      t834 = FJET(XB1, XB2, s, t514, -t512, t510, 0.0D0, -t520, t833)
      t836 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t841 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t855 = t470 * t836 * t49 * t473 / 0.32D2 - (0.90D2 * t4 * (-t477 *
     # t841 + t488 * t469 * t836) + 0.180D3 * t42 * t477 * t836) * t49 *
     # t58 / 0.5760D4
      t856 = FJET(XB1, XB2, s, t459, 0.0D0, -t461, 0.0D0, 0.0D0, t855)
      t858 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t863 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t873 = -t4 * t858 * t60 / 0.32D2 + (0.90D2 * t4 * (t533 * t858 - t
     #863) + 0.180D3 * t42 * t858) * t58 * t51 / 0.2880D4
      t874 = FJET(XB1, XB2, s, t510, 0.0D0, t514, -t512, -t520, t873)
      t879 = t16 * s * t1 * t511 * t457
      t880 = t2 * x1
      t882 = Sqrt(-t578 * t463)
      t883 = t26 * t882
      t889 = t880 * x2 * (-x3 + t455 - z + t27 - x1 + t570 + t506 - t571
     # + 0.2D1 * t883) * t508 * t457
      t890 = t512 * t458
      t893 = t157 * x1
      t895 = t157 * t506
      t899 = t880 * (-x2 + t455 + 0.2D1 * t883 * x2 + 0.1D1 - x3 + t893 
     #+ t157 * z - t895) * t508 * t457
      t901 = x2 * x1
      t902 = t901 * z
      t903 = -z - t901 + t902
      t904 = x2 * t7
      t918 = t12 + t904 - t893 + t902 - 0.2D1 * t883 * z - t8 * x2 - t45
     #5 * z + t455 * x1 - t901 * t12 - 0.2D1 * t904 * z + t904 * t12 + t
     #575 * t901 + 0.2D1 * t8 * x2 * z
      t927 = -t8 * t12 * x2 - 0.2D1 * t455 * t506 + t895 - 0.2D1 * t883 
     #* t901 + 0.2D1 * t883 * t902 + t506 + t27 + t8 - t574 + t571 - t57
     #3 - t576 + t577
      t929 = 0.1D1 / (t918 + t927)
      t931 = t4 * t507 * t903 * t929
      t932 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, t54, -t460, x4)
      t937 = FJET(XB1, XB2, s, t879, t889, -t890, -t899, -t520, t931 * t
     #932 * t49 * t473 / 0.32D2)
      t939 = t507 * t903
      t945 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, t54, -t460, x4)
      t950 = FJET(XB1, XB2, s, t889, t879, -t899, -t890, -t520, t931 * t
     #945 * t49 * t473 / 0.32D2)
      t957 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t959 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t970 = -t559 * t957 * t585 + t957
      t1005 = (0.90D2 * t4 * (-t556 * t957 + t959 - (t559 * t959 - t566 
     #* t507 * t957) * t585) - 0.180D3 * t42 * t970) * t49 * t51 / 0.576
     #0D4 + t4 * t970 * t60 / 0.32D2 + (0.90D2 * t4 * (t959 - t605 * t95
     #7) - 0.180D3 * t42 * t957) * t58 * t51 / 0.2880D4 + (-0.180D3 * t4
     #2 * (t959 - t618 * t957) + 0.90D2 * t4 * (t624 * t957 / 0.2D1 - t6
     #18 * t959) + t106 * t957) * t51 / 0.5760D4
      t1006 = FJET(XB1, XB2, s, -t512, 0.0D0, 0.0D0, -t549, t550, t1005)
      t1008 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t1012 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, t54, 0.10D1, x4)
      t1023 = -t4 * t1008 * t60 / 0.32D2 + (0.90D2 * t4 * (-t1012 + t533
     # * t1008) + 0.180D3 * t42 * t1008) * t58 * t51 / 0.2880D4
      t1024 = FJET(XB1, XB2, s, -t512, t514, 0.0D0, t510, -t520, t1023)
      t1026 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1028 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t1039 = -t559 * t1028 * t585 + t1028
      t1074 = (0.90D2 * t4 * (-(t559 * t1026 - t566 * t507 * t1028) * t5
     #85 + t1026 - t556 * t1028) - 0.180D3 * t42 * t1039) * t49 * t51 / 
     #0.5760D4 + t4 * t1039 * t60 / 0.32D2 + (0.90D2 * t4 * (-t605 * t10
     #28 + t1026) - 0.180D3 * t42 * t1028) * t58 * t51 / 0.2880D4 + (-0.
     #180D3 * t42 * (t1026 - t618 * t1028) + 0.90D2 * t4 * (t624 * t1028
     # / 0.2D1 - t618 * t1026) + t106 * t1028) * t51 / 0.5760D4
      t1075 = FJET(XB1, XB2, s, -t549, 0.0D0, 0.0D0, -t512, t550, t1074)
      t1077 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t1082 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t54, -t460, x4)
      t1096 = t470 * t1077 * t49 * t473 / 0.32D2 - (0.90D2 * t4 * (-t477
     # * t1082 + t488 * t469 * t1077) + 0.180D3 * t42 * t477 * t1077) * 
     #t49 * t58 / 0.5760D4
      t1097 = FJET(XB1, XB2, s, -t461, 0.0D0, t459, 0.0D0, 0.0D0, t1096)
      t1099 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, t54, -t460, x4)
      t1104 = FJET(XB1, XB2, s, -t899, -t890, t889, t879, -t520, t931 * 
     #t1099 * t49 * t473 / 0.32D2)
      t1111 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, t54, -t460, x4)
      t1116 = FJET(XB1, XB2, s, -t890, -t899, t879, t889, -t520, t931 * 
     #t1111 * t49 * t473 / 0.32D2)
      bggbH5n3e0 = t226 * t225 + t341 * t340 + t453 * t452 + t502 * t501
     # + t546 * t545 + t636 * t635 + t688 * t687 + t710 * t709 + t816 * 
     #t815 + t834 * t833 + t856 * t855 + t874 * t873 + t937 * t4 * t939 
     #* t929 * t932 * t60 / 0.32D2 + t950 * t4 * t939 * t929 * t945 * t6
     #0 / 0.32D2 + t1006 * t1005 + t1024 * t1023 + t1075 * t1074 + t1097
     # * t1096 + t1104 * t4 * t939 * t929 * t1099 * t60 / 0.32D2 + t1116
     # * t4 * t939 * t929 * t1111 * t60 / 0.32D2

      end function



      doubleprecision function bggbH5n3em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t15 = log(0.4D1 * t9 * t12)
      t16 = t15 * t4
      t18 = -0.180D3 * t5 - 0.90D2 * t16
      t19 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t22 = 0.1D1 - x2
      t23 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t24 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = cos(t10)
      t27 = x3 * z
      t28 = -0.1D1 + x3
      t30 = Sqrt(-t27 * t28)
      t34 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t30)
      t35 = z * t24 * t34
      t38 = 0.1D1 / x3
      t39 = 0.1D1 / x2
      t40 = t38 * t39
      t43 = x2 ** 2
      t44 = t9 * t43
      t45 = -t22
      t49 = log(-0.4D1 * t44 * t12 * t45)
      t51 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t54 = log(0.4D1 * t44 * t12)
      t59 = -t24 + t23
      t67 = lh ** 2
      t69 = 0.3141592653589793D1 ** 2
      t73 = t15 ** 2
      t76 = 0.180D3 * t16 * lh + t4 * (0.180D3 * t67 - 0.30D2 * t69) + 0
     #.45D2 * t73 * t4
      t79 = x1 ** 2
      t80 = t79 * t12
      t83 = log(0.4D1 * t80 * t9)
      t89 = 0.180D3 * t5 * t24
      t91 = 0.1D1 / x1
      t96 = t38 * t91
      t100 = t39 * t91
      t104 = x3 * t9
      t109 = log(-0.4D1 * t104 * t12 / t28)
      t114 = log(0.4D1 * t104 * t12)
      t115 = t109 * z * t34 + t114
      t122 = -z * t34 - 0.1D1
      t127 = -t18 * t19 / 0.11520D5 - t4 * (-t23 + t24 + t35) * t40 / 0.
     #64D2 + (0.90D2 * t4 * (-t19 - t49 * t23 + t51 + t54 * t24) - 0.180
     #D3 * t5 * t59) * t39 / 0.5760D4 - t76 * t24 / 0.11520D5 + (0.90D2 
     #* t4 * (-t19 + t83 * t24) + t89) * t91 / 0.5760D4 + t4 * (-t35 - t
     #24) * t96 / 0.64D2 + t4 * t59 * t100 / 0.32D2 + (0.90D2 * t4 * t24
     # * t115 + (0.90D2 * t4 * t19 - t89) * t122) * t38 / 0.11520D5
      t128 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t127)
      t130 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t131 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t137 = 0.180D3 * t5 * t131
      t142 = z * t131 * t34
      t147 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t148 = t147 - t131
      t158 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t171 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t184 = (0.90D2 * t4 * (-t130 + t83 * t131) + t137) * t91 / 0.5760D
     #4 + t4 * (-t131 - t142) * t96 / 0.64D2 + t4 * t148 * t100 / 0.32D2
     # - t18 * t130 / 0.11520D5 - t4 * (-t147 + t142 + t131) * t40 / 0.6
     #4D2 + (0.90D2 * t4 * (t158 - t130 + t54 * t131 - t49 * t147) - 0.1
     #80D3 * t5 * t148) * t39 / 0.5760D4 - t76 * t131 / 0.11520D5 - t4 *
     # t171 / 0.128D3 + (0.90D2 * t4 * t131 * t115 + (0.90D2 * t4 * t130
     # - t137) * t122) * t38 / 0.11520D5
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t184)
      t187 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t190 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t196 = 0.180D3 * t190 * t5
      t201 = z * t190 * t34
      t206 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t207 = -t190 + t206
      t213 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t221 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t241 = -t18 * t187 / 0.11520D5 + (0.90D2 * t4 * (-t187 + t83 * t19
     #0) + t196) * t91 / 0.5760D4 + t4 * (-t201 - t190) * t96 / 0.64D2 +
     # t4 * t207 * t100 / 0.32D2 - t76 * t190 / 0.11520D5 - t4 * t213 / 
     #0.128D3 - t4 * (-t206 + t190 + t201) * t40 / 0.64D2 + (0.90D2 * t4
     # * (-t49 * t206 - t187 + t221 + t54 * t190) - 0.180D3 * t5 * t207)
     # * t39 / 0.5760D4 + (0.90D2 * t4 * t190 * t115 + (0.90D2 * t4 * t1
     #87 - t196) * t122) * t38 / 0.11520D5
      t242 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t241)
      t244 = x2 * x3
      t246 = 0.1D1 / (0.1D1 - x3 + t244)
      t248 = t2 * t244 * t246
      t249 = t28 * t246
      t250 = t2 * t249
      t254 = Sqrt(t27 * t45 * t28)
      t258 = 0.1D1 / (-z - x3 + t244 + 0.2D1 * t26 * t254)
      t259 = t4 * z * t258
      t260 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, -t249, x4)
      t265 = FJET(XB1, XB2, s, 0.0D0, t248, 0.0D0, -t250, 0.0D0, t259 * 
     #t260 * t38 * t39 / 0.64D2)
      t273 = t1 * x1
      t274 = x1 * z
      t275 = -z - x1 + t274
      t276 = 0.1D1 / t275
      t278 = t45 * s * t273 * t276
      t279 = -0.1D1 + x1
      t280 = t2 * t279
      t282 = x2 * s * t273
      t283 = t1 ** 2
      t284 = s * t283
      t287 = x1 * t279 * t276
      t288 = t284 * t45 * t287
      t289 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, t22, 0.10D1, x4)
      t293 = FJET(XB1, XB2, s, 0.0D0, t278, -t280, t282, -t288, -t4 * t2
     #89 * t100 / 0.32D2)
      t300 = t2 * x1 * t276
      t301 = t284 * t287
      t302 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t305 = t279 ** 2
      t309 = log(-0.4D1 * t80 / t7 * t276 * t305)
      t310 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t320 = z * t275
      t323 = t79 * x3
      t332 = Sqrt(x3 * t275 * t28)
      t337 = 0.1D1 / (-t274 - x3 * x1 * z - t27 - t323 + 0.2D1 * t323 * 
     #z + x1 * t7 + x3 * t7 * x1 - t323 * t7 + 0.2D1 * t26 * t332 * z - 
     #t7)
      t347 = (0.90D2 * t4 * (t302 - t309 * t310) - 0.180D3 * t5 * t310) 
     #* t91 / 0.5760D4 + t4 * (t310 - t320 * t310 * t337) * t96 / 0.64D2
     # + t4 * t310 * t100 / 0.32D2
      t348 = FJET(XB1, XB2, s, 0.0D0, -t280, -t300, 0.0D0, t301, t347)
      t350 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t351 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t370 = (0.90D2 * t4 * (t350 - t309 * t351) - 0.180D3 * t5 * t351) 
     #* t91 / 0.5760D4 + t4 * (t351 - t320 * t351 * t337) * t96 / 0.64D2
     # + t4 * t351 * t100 / 0.32D2
      t371 = FJET(XB1, XB2, s, 0.0D0, -t300, -t280, 0.0D0, t301, t370)
      t373 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, -t249, x4)
      t378 = FJET(XB1, XB2, s, 0.0D0, -t250, 0.0D0, t248, 0.0D0, t259 * 
     #t373 * t38 * t39 / 0.64D2)
      t385 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t386 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t392 = 0.180D3 * t5 * t386
      t397 = z * t386 * t34
      t402 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t403 = t402 - t386
      t415 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, t22, 0.10D1, x4)
      t436 = (0.90D2 * t4 * (-t385 + t83 * t386) + t392) * t91 / 0.5760D
     #4 + t4 * (-t397 - t386) * t96 / 0.64D2 + t4 * t403 * t100 / 0.32D2
     # - t18 * t385 / 0.11520D5 - t4 * (t397 - t402 + t386) * t40 / 0.64
     #D2 + (0.90D2 * t4 * (-t49 * t402 + t54 * t386 - t385 + t415) - 0.1
     #80D3 * t5 * t403) * t39 / 0.5760D4 - t76 * t386 / 0.11520D5 + (0.9
     #0D2 * t4 * t386 * t115 + (0.90D2 * t4 * t385 - t392) * t122) * t38
     # / 0.11520D5
      t437 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t436)
      t439 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, t22, 0.10D1, x4)
      t443 = FJET(XB1, XB2, s, t282, -t280, t278, 0.0D0, -t288, -t4 * t4
     #39 * t100 / 0.32D2)
      t449 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, -t249, x4)
      t454 = FJET(XB1, XB2, s, t248, 0.0D0, -t250, 0.0D0, 0.0D0, t259 * 
     #t449 * t38 * t39 / 0.64D2)
      t461 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, t22, 0.10D1, x4)
      t465 = FJET(XB1, XB2, s, t278, 0.0D0, t282, -t280, -t288, -t4 * t4
     #61 * t100 / 0.32D2)
      t471 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t472 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t491 = (0.90D2 * t4 * (t471 - t309 * t472) - 0.180D3 * t5 * t472) 
     #* t91 / 0.5760D4 + t4 * (-t320 * t472 * t337 + t472) * t96 / 0.64D
     #2 + t4 * t472 * t100 / 0.32D2
      t492 = FJET(XB1, XB2, s, -t280, 0.0D0, 0.0D0, -t300, t301, t491)
      t494 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, t22, 0.10D1, x4)
      t498 = FJET(XB1, XB2, s, -t280, t282, 0.0D0, t278, -t288, -t4 * t4
     #94 * t100 / 0.32D2)
      t504 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t505 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t524 = (0.90D2 * t4 * (t504 - t309 * t505) - 0.180D3 * t5 * t505) 
     #* t91 / 0.5760D4 + t4 * (-t320 * t505 * t337 + t505) * t96 / 0.64D
     #2 + t4 * t505 * t100 / 0.32D2
      t525 = FJET(XB1, XB2, s, -t300, 0.0D0, 0.0D0, -t280, t301, t524)
      t527 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t22, -t249, x4)
      t532 = FJET(XB1, XB2, s, -t250, 0.0D0, t248, 0.0D0, 0.0D0, t259 * 
     #t527 * t38 * t39 / 0.64D2)
      bggbH5n3em1 = t128 * t127 + t185 * t184 + t242 * t241 + t265 * t4 
     #* z * t258 * t260 * t40 / 0.64D2 - t293 * t4 * t289 * t39 * t91 / 
     #0.32D2 + t348 * t347 + t371 * t370 + t378 * t4 * z * t258 * t373 *
     # t40 / 0.64D2 + t437 * t436 - t443 * t4 * t439 * t39 * t91 / 0.32D
     #2 + t454 * t4 * z * t258 * t449 * t40 / 0.64D2 - t465 * t4 * t461 
     #* t39 * t91 / 0.32D2 + t492 * t491 - t498 * t4 * t494 * t39 * t91 
     #/ 0.32D2 + t525 * t524 + t532 * t4 * z * t258 * t527 * t40 / 0.64D
     #2

      end function



      doubleprecision function bggbH5n3em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = 0.1D1 - x2
      t11 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t14 = 0.1D1 / x2
      t17 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t22 = z ** 2
      t25 = x4 * 0.3141592653589793D1
      t26 = Sin(t25)
      t27 = t26 ** 2
      t30 = log(0.4D1 / t22 / z * t27)
      t33 = -0.180D3 * t4 * lh - 0.90D2 * t30 * t4
      t36 = cos(t25)
      t40 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t48 = (-z / (-z - x3 + 0.2D1 * t36 * t40) - 0.1D1) / x3
      t51 = -t6 * t7 / 0.64D2 + t4 * (-t5 + t11) * t14 / 0.64D2 - t4 * t
     #17 / 0.128D3 - t33 * t5 / 0.11520D5 + t6 * t48 / 0.128D3
      t52 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t51)
      t54 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t55 = t4 * t54
      t58 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t63 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t70 = -t55 * t7 / 0.64D2 + t4 * (t58 - t54) * t14 / 0.64D2 - t4 * 
     #t63 / 0.128D3 - t33 * t54 / 0.11520D5 + t55 * t48 / 0.128D3
      t71 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t70)
      t73 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t74 = t73 * t4
      t77 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t82 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t89 = -t74 * t7 / 0.64D2 + t4 * (-t73 + t77) * t14 / 0.64D2 - t4 *
     # t82 / 0.128D3 - t33 * t73 / 0.11520D5 + t74 * t48 / 0.128D3
      t90 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = -0.1D1 + x1
      t93 = t2 * t92
      t96 = 0.1D1 / (-z - x1 + x1 * z)
      t98 = t2 * x1 * t96
      t99 = t1 ** 2
      t103 = s * t99 * x1 * t92 * t96
      t104 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t108 = FJET(XB1, XB2, s, 0.0D0, -t93, -t98, 0.0D0, t103, t4 * t104
     # * t7 / 0.64D2)
      t113 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t117 = FJET(XB1, XB2, s, 0.0D0, -t98, -t93, 0.0D0, t103, t4 * t113
     # * t7 / 0.64D2)
      t122 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t123 = t4 * t122
      t130 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, t10, 0.10D1, x4)
      t135 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4
     #)
      t138 = -t123 * t7 / 0.64D2 - t33 * t122 / 0.11520D5 + t123 * t48 /
     # 0.128D3 + t4 * (t130 - t122) * t14 / 0.64D2 - t4 * t135 / 0.128D3
      t139 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t138)
      t141 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t145 = FJET(XB1, XB2, s, -t93, 0.0D0, 0.0D0, -t98, t103, t4 * t141
     # * t7 / 0.64D2)
      t150 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.10D1, 0.10D1, x4)
      t154 = FJET(XB1, XB2, s, -t98, 0.0D0, 0.0D0, -t93, t103, t4 * t150
     # * t7 / 0.64D2)
      bggbH5n3em2 = t52 * t51 + t71 * t70 + t90 * t89 + t108 * t4 * t104
     # * t7 / 0.64D2 + t117 * t4 * t113 * t7 / 0.64D2 + t139 * t138 + t1
     #45 * t4 * t141 * t7 / 0.64D2 + t154 * t4 * t150 * t7 / 0.64D2

      end function



      doubleprecision function bggbH5n3em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t4 * t5 / 
     #0.128D3)
      t11 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t4 * t11 
     #/ 0.128D3)
      t17 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t4 * t17 
     #/ 0.128D3)
      t23 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.10D1, 0.10D1, x4)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t4 * t23 
     #/ 0.128D3)
      bggbH5n3em3 = -t8 * t4 * t5 / 0.128D3 - t14 * t4 * t11 / 0.128D3 -
     # t20 * t4 * t17 / 0.128D3 - t26 * t4 * t23 / 0.128D3

      end function



      doubleprecision function bggbH5n3em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      bggbH5n3em4 = 0.0D0

      end function


      doubleprecision function bggbH5n4e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * lh
      t9 = x1 ** 2
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x1 * z
      t17 = -z - x1 + t16
      t18 = 0.1D1 / t17
      t19 = t15 * t18
      t20 = t4 ** 2
      t21 = t19 * t20
      t24 = log(-0.4D1 * t13 * t21)
      t25 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t27 = t24 ** 2
      t28 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t34 = lh ** 2
      t36 = 0.3141592653589793D1 ** 2
      t38 = 0.180D3 * t34 - 0.30D2 * t36
      t39 = t7 * t38
      t45 = t27 * t24
      t55 = -0.2884936567583026D3 - 0.120D3 * t34 * lh + 0.60D2 * lh * t
     #36
      t56 = t7 * t55
      t59 = 0.1D1 / x1
      t61 = x3 * t9
      t62 = t61 * t12
      t65 = log(-0.4D1 * t62 * t21)
      t71 = t65 ** 2
      t77 = t39 * t28
      t79 = 0.1D1 / x3
      t82 = t61 * x2
      t84 = t20 * t18
      t88 = log(-0.4D1 * t82 * t12 * t15 * t84)
      t97 = 0.1D1 / x2
      t98 = t97 * t59
      t100 = x2 * t9
      t101 = t100 * t12
      t104 = log(-0.4D1 * t101 * t21)
      t110 = t104 ** 2
      t120 = (-0.180D3 * t8 * (-t24 * t25 + t27 * t28 / 0.2D1) + t39 * (
     #-t24 * t28 + t25) + 0.90D2 * t7 * (t27 * t25 / 0.2D1 - t45 * t28 /
     # 0.6D1) + t56 * t28) * t59 / 0.2880D4 + (-0.180D3 * t8 * (t25 - t6
     #5 * t28) + 0.90D2 * t7 * (-t65 * t25 + t71 * t28 / 0.2D1) + t77) *
     # t79 * t59 / 0.2880D4 + (0.90D2 * t7 * (-t88 * t28 + t25) - 0.180D
     #3 * t8 * t28) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t104 * t
     #28 + t25) + 0.90D2 * t7 * (-t104 * t25 + t110 * t28 / 0.2D1) + t77
     #) * t97 * t59 / 0.2880D4
      t121 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t120)
      t123 = -0.1D1 + x2
      t125 = x2 * x3
      t126 = t125 - 0.1D1
      t127 = 0.1D1 / t126
      t128 = x3 * t123 * t127
      t129 = t2 * t128
      t130 = -0.1D1 + x3
      t131 = t130 * t127
      t132 = t2 * t131
      t133 = cos(t10)
      t134 = x3 * z
      t135 = x2 * t130
      t137 = Sqrt(-t134 * t135)
      t141 = 0.1D1 / (-z - t125 + 0.2D1 * t133 * t137)
      t142 = z * t141
      t143 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t144 = t142 * t143
      t146 = 0.1D1 / t14 / z
      t148 = t123 ** 2
      t149 = t12 * t148
      t150 = t126 ** 2
      t151 = 0.1D1 / t150
      t152 = t130 * t151
      t156 = log(-0.4D1 * t125 * t146 * t149 * t152)
      t157 = t156 * z
      t158 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t159 = t141 * t158
      t164 = t156 ** 2
      t165 = t164 * z
      t173 = t142 * t158
      t179 = t125 * t13
      t184 = log(-0.4D1 * t179 * t146 * t148 * t152)
      t185 = t184 * z
      t196 = (-0.180D3 * t8 * (t144 - t157 * t159) + 0.90D2 * t7 * (t165
     # * t159 / 0.2D1 - t157 * t141 * t143) + t39 * t173) * t79 * t97 / 
     #0.5760D4 + (0.90D2 * t7 * (t144 - t185 * t159) - 0.180D3 * t8 * t1
     #73) * t79 * t98 / 0.2880D4
      t197 = FJET(XB1, XB2, s, 0.0D0, t129, 0.0D0, t132, 0.0D0, t196)
      t199 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t200 = t142 * t199
      t201 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t202 = t141 * t201
      t207 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t216 = t142 * t201
      t232 = (-0.180D3 * t8 * (t200 - t157 * t202) + 0.90D2 * t7 * (t142
     # * t207 - t157 * t141 * t199 + t165 * t202 / 0.2D1) + t39 * t216) 
     #* t79 * t97 / 0.5760D4 + (0.90D2 * t7 * (-t185 * t202 + t200) - 0.
     #180D3 * t8 * t216) * t79 * t98 / 0.2880D4
      t233 = FJET(XB1, XB2, s, 0.0D0, t132, 0.0D0, t129, 0.0D0, t232)
      t235 = t2 * t130
      t236 = t2 * x3
      t237 = t12 * t146
      t238 = t237 * t130
      t241 = log(-0.4D1 * t61 * t238)
      t242 = -t130
      t243 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t245 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t250 = t241 ** 2
      t253 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t257 = t39 * t243
      t264 = log(-0.4D1 * t82 * t238)
      t277 = log(-0.4D1 * t125 * t238)
      t282 = t277 ** 2
      t296 = log(-0.4D1 * t237 * x3 * t130)
      t297 = t296 ** 2
      t310 = t297 * t296
      t320 = (-0.180D3 * t8 * (-t241 * t243 + t245) + 0.90D2 * t7 * (-t2
     #41 * t245 + t250 * t243 / 0.2D1 + t253) + t257) * t79 * t59 / 0.28
     #80D4 + (0.90D2 * t7 * (-t264 * t243 + t245) - 0.180D3 * t8 * t243)
     # * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t277 * t243 + t245) +
     # 0.90D2 * t7 * (t282 * t243 / 0.2D1 + t253 - t277 * t245) + t257) 
     #* t79 * t97 / 0.5760D4 - (-0.180D3 * t8 * (-t253 - t297 * t243 / 0
     #.2D1 + t296 * t245) + t39 * (-t245 + t296 * t243) + 0.90D2 * t7 * 
     #(t296 * t253 - t297 * t245 / 0.2D1 + t310 * t243 / 0.6D1) - t56 * 
     #t243) * t79 / 0.5760D4
      t321 = FJET(XB1, XB2, s, 0.0D0, -t235, 0.0D0, t236, 0.0D0, t320)
      t323 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t325 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t335 = t39 * t323
      t384 = (-0.180D3 * t8 * (-t241 * t323 + t325) + 0.90D2 * t7 * (t25
     #0 * t323 / 0.2D1 - t241 * t325) + t335) * t79 * t59 / 0.2880D4 + (
     #0.90D2 * t7 * (-t264 * t323 + t325) - 0.180D3 * t8 * t323) * t79 *
     # t98 / 0.2880D4 - (-0.180D3 * t8 * (-t297 * t323 / 0.2D1 + t296 * 
     #t325) + t39 * (-t325 + t296 * t323) + 0.90D2 * t7 * (-t297 * t325 
     #/ 0.2D1 + t310 * t323 / 0.6D1) - t56 * t323) * t79 / 0.5760D4 + (-
     #0.180D3 * t8 * (-t277 * t323 + t325) + 0.90D2 * t7 * (t282 * t323 
     #/ 0.2D1 - t277 * t325) + t335) * t79 * t97 / 0.5760D4
      t385 = FJET(XB1, XB2, s, -t235, 0.0D0, t236, 0.0D0, 0.0D0, t384)
      t387 = t130 * s
      t388 = t1 * x1
      t389 = t387 * t388
      t390 = t1 * t4
      t391 = t387 * t390
      t392 = x1 * x3
      t393 = t2 * t392
      t395 = t2 * t4 * x3
      t397 = t19 * t20 * t130
      t400 = log(0.4D1 * t62 * t397)
      t401 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t403 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t407 = t400 ** 2
      t410 = bggbH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t421 = log(0.4D1 * t179 * t397)
      t432 = (-0.180D3 * t8 * (t400 * t401 - t403) + 0.90D2 * t7 * (-t40
     #7 * t401 / 0.2D1 - t410 + t400 * t403) - t39 * t401) * t79 * t59 /
     # 0.2880D4 + (0.90D2 * t7 * (t421 * t401 - t403) + 0.180D3 * t8 * t
     #401) * t79 * t98 / 0.2880D4
      t433 = FJET(XB1, XB2, s, -t389, t391, t393, -t395, 0.0D0, t432)
      t435 = t5 * t128
      t438 = Sqrt(x3 * t17 * t135)
      t439 = t133 * t438
      t440 = 0.2D1 * t439
      t441 = x2 ** 2
      t442 = t441 * x3
      t443 = t442 * x1
      t445 = t392 * z
      t446 = t125 * z
      t448 = t125 * x1
      t452 = t442 * t16
      t455 = -t134 - t392 + t125 - t440 - x2 - t443 - t442 * z + t445 + 
     #0.2D1 * t446 + 0.2D1 * t448 + 0.2D1 * t439 * x2 + t452 - 0.2D1 * t
     #125 * t16
      t458 = t3 * t455 * t18 * t127
      t460 = t387 * t390 * t127
      t465 = t3 * t123 * (-t125 - z + t134 - x1 + t392 + t16 - t445 + t4
     #40) * t18 * t127
      t466 = t1 ** 2
      t471 = s * t466 * x2 * x1 * t4 * t18
      t472 = x2 * x1
      t473 = t472 * z
      t474 = z + x1 - t16 - t472 + t473
      t475 = t17 * t474
      t486 = 0.2D1 * t439 * z + 0.2D1 * t439 * x1 - t9 - 0.2D1 * t16 - t
     #82 + t443 + t473 - t446 - t448 + 0.2D1 * t439 * t473 - t472 * t14 
     #- 0.2D1 * t100 * z
      t504 = t100 * t14 - 0.2D1 * t439 * t472 - 0.2D1 * t439 * t16 + 0.2
     #D1 * x1 * t14 + 0.2D1 * t9 * z - t9 * t14 + t100 - t14 + x3 * t14 
     #* t472 + 0.2D1 * t61 * x2 * z - t61 * t14 * x2 - t452
      t506 = 0.1D1 / (t486 + t504)
      t507 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t517 = log(0.4D1 * t125 * t13 * t15 * t84 * t148 * t130 * t151)
      t518 = t517 * t17
      t519 = t474 * t506
      t520 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t521 = t519 * t520
      t526 = t8 * t17
      t529 = 0.90D2 * t7 * (t475 * t506 * t507 - t518 * t521) - 0.180D3 
     #* t526 * t521
      t533 = FJET(XB1, XB2, s, -t435, -t458, -t460, t465, t471, t529 * t
     #79 * t98 / 0.2880D4)
      t536 = t79 * t97 * t59
      t539 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t542 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t543 = t519 * t542
      t550 = 0.90D2 * t7 * (t475 * t506 * t539 - t518 * t543) - 0.180D3 
     #* t526 * t543
      t554 = FJET(XB1, XB2, s, -t458, -t435, t465, -t460, t471, t550 * t
     #79 * t98 / 0.2880D4)
      t559 = t2 * t472 * t18
      t561 = t123 * s * t388
      t564 = t15 * t20 * t18 * t148
      t567 = log(-0.4D1 * t179 * t564)
      t568 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t570 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t581 = log(-0.4D1 * t101 * t564)
      t586 = t581 ** 2
      t598 = (0.90D2 * t7 * (t567 * t568 - t570) + 0.180D3 * t8 * t568) 
     #* t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t570 + t581 * t568) + 
     #0.90D2 * t7 * (-t586 * t568 / 0.2D1 + t581 * t570) - t39 * t568) *
     # t97 * t59 / 0.2880D4
      t599 = FJET(XB1, XB2, s, -t559, 0.0D0, -t561, -t5, t471, t598)
      t601 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t603 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t613 = t39 * t601
      t662 = (-0.180D3 * t8 * (-t241 * t601 + t603) + 0.90D2 * t7 * (t25
     #0 * t601 / 0.2D1 - t241 * t603) + t613) * t79 * t59 / 0.2880D4 + (
     #0.90D2 * t7 * (t603 - t264 * t601) - 0.180D3 * t8 * t601) * t79 * 
     #t98 / 0.2880D4 + (-0.180D3 * t8 * (-t277 * t601 + t603) + 0.90D2 *
     # t7 * (t282 * t601 / 0.2D1 - t277 * t603) + t613) * t79 * t97 / 0.
     #5760D4 - (-0.180D3 * t8 * (-t297 * t601 / 0.2D1 + t296 * t603) + t
     #39 * (t296 * t601 - t603) + 0.90D2 * t7 * (-t297 * t603 / 0.2D1 + 
     #t310 * t601 / 0.6D1) - t56 * t601) * t79 / 0.5760D4
      t663 = FJET(XB1, XB2, s, 0.0D0, t236, 0.0D0, -t235, 0.0D0, t662)
      t665 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t666 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t671 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t242, x4)
      t678 = t39 * t666
      t728 = (-0.180D3 * t8 * (t665 - t241 * t666) + 0.90D2 * t7 * (t671
     # + t250 * t666 / 0.2D1 - t241 * t665) + t678) * t79 * t59 / 0.2880
     #D4 + (0.90D2 * t7 * (t665 - t264 * t666) - 0.180D3 * t8 * t666) * 
     #t79 * t98 / 0.2880D4 - (-0.180D3 * t8 * (-t297 * t666 / 0.2D1 - t6
     #71 + t296 * t665) + t39 * (-t665 + t296 * t666) + 0.90D2 * t7 * (t
     #296 * t671 - t297 * t665 / 0.2D1 + t310 * t666 / 0.6D1) - t56 * t6
     #66) * t79 / 0.5760D4 + (-0.180D3 * t8 * (t665 - t277 * t666) + 0.9
     #0D2 * t7 * (-t277 * t665 + t671 + t282 * t666 / 0.2D1) + t678) * t
     #79 * t97 / 0.5760D4
      t729 = FJET(XB1, XB2, s, t236, 0.0D0, -t235, 0.0D0, 0.0D0, t728)
      t731 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t734 = bggbH53J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t735 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t764 = t39 * t731
      t791 = (-0.180D3 * t8 * (t27 * t731 / 0.2D1 + t734 - t24 * t735) +
     # t39 * (t735 - t24 * t731) + 0.90D2 * t7 * (-t24 * t734 + t27 * t7
     #35 / 0.2D1 - t45 * t731 / 0.6D1) + t56 * t731) * t59 / 0.2880D4 + 
     #(-0.180D3 * t8 * (-t65 * t731 + t735) + 0.90D2 * t7 * (t71 * t731 
     #/ 0.2D1 - t65 * t735 + t734) + t764) * t79 * t59 / 0.2880D4 + (0.9
     #0D2 * t7 * (t735 - t88 * t731) - 0.180D3 * t8 * t731) * t79 * t98 
     #/ 0.2880D4 + (-0.180D3 * t8 * (-t104 * t731 + t735) + 0.90D2 * t7 
     #* (t734 - t104 * t735 + t110 * t731 / 0.2D1) + t764) * t97 * t59 /
     # 0.2880D4
      t792 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t5, t3, 0.0D0, t791)
      t794 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t796 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t820 = (-0.180D3 * t8 * (t400 * t794 - t796) + 0.90D2 * t7 * (-t40
     #7 * t794 / 0.2D1 + t400 * t796) - t39 * t794) * t79 * t59 / 0.2880
     #D4 + (0.90D2 * t7 * (-t796 + t421 * t794) + 0.180D3 * t8 * t794) *
     # t79 * t98 / 0.2880D4
      t821 = FJET(XB1, XB2, s, t393, -t395, -t389, t391, 0.0D0, t820)
      t823 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t824 = t141 * t823
      t826 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t827 = t142 * t826
      t831 = t142 * t823
      t854 = (0.90D2 * t7 * (-t185 * t824 + t827) - 0.180D3 * t8 * t831)
     # * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t157 * t824 + t827) +
     # 0.90D2 * t7 * (-t157 * t141 * t826 + t165 * t824 / 0.2D1) + t39 *
     # t831) * t79 * t97 / 0.5760D4
      t855 = FJET(XB1, XB2, s, t132, 0.0D0, t129, 0.0D0, 0.0D0, t854)
      t857 = t121 * t120 + t197 * t196 + t233 * t232 + t321 * t320 + t38
     #5 * t384 + t433 * t432 + t533 * t529 * t536 / 0.2880D4 + t554 * t5
     #50 * t536 / 0.2880D4 + t599 * t598 + t663 * t662 + t729 * t728 + t
     #792 * t791 + t821 * t820 + t855 * t854
      t858 = t13 * t146
      t860 = log(0.4D1 * t858)
      t861 = t860 ** 2
      t862 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t865 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t867 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t875 = t861 * t860
      t883 = t56 * t862
      t889 = log(0.4D1 * t61 * t237)
      t894 = t889 ** 2
      t906 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t909 = log(0.4D1 * t125 * t858)
      t911 = t237 * t148
      t914 = log(0.4D1 * t82 * t911)
      t915 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t920 = t915 - t862
      t929 = log(0.4D1 * t100 * t237)
      t933 = log(0.4D1 * t100 * t911)
      t940 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t941 = t933 ** 2
      t944 = t929 ** 2
      t950 = t39 * t920
      t956 = log(0.4D1 * t237)
      t957 = t956 ** 2
      t958 = t957 ** 2
      t961 = t956 * t7
      t964 = t36 ** 2
      t965 = t34 ** 2
      t971 = t957 * t7
      t975 = t957 * t956 * t7
      t978 = 0.15D2 / 0.4D1 * t958 * t7 - t961 * t55 + t7 * (0.576987313
     #5166051D3 * lh + t964 + 0.60D2 * t965 - 0.60D2 * t34 * t36) + t971
     # * t38 / 0.2D1 + 0.30D2 * t975 * lh
      t984 = 0.180D3 * t961 * lh + t39 + 0.45D2 * t971
      t991 = -t961 * t38 - 0.90D2 * t971 * lh + t56 - 0.15D2 * t975
      t994 = x2 * t146
      t997 = log(0.4D1 * t994 * t12)
      t1001 = log(0.4D1 * t994 * t149)
      t1003 = t997 ** 2
      t1006 = t1001 ** 2
      t1021 = t1003 * t997
      t1024 = t1006 * t1001
      t1037 = log(0.4D1 * t125 * t911)
      t1041 = log(0.4D1 * t125 * t237)
      t1046 = t1037 ** 2
      t1050 = t1041 ** 2
      t1064 = log(0.4D1 * x3 * t146 * t12)
      t1065 = t1064 ** 2
      t1076 = t1065 * t1064
      t1087 = (-0.180D3 * t8 * (-t861 * t862 / 0.2D1 + t860 * t865 - t86
     #7) + t39 * (-t865 + t860 * t862) + 0.90D2 * t7 * (t860 * t867 + t8
     #75 * t862 / 0.6D1 - t861 * t865 / 0.2D1) - t883) * t59 / 0.2880D4 
     #+ (-0.180D3 * t8 * (t889 * t862 - t865) + 0.90D2 * t7 * (-t894 * t
     #862 / 0.2D1 - t867 + t889 * t865) - t39 * t862) * t79 * t59 / 0.28
     #80D4 + (0.90D2 * t7 * (t906 - t865 + t909 * t862 - t914 * t915) - 
     #0.180D3 * t8 * t920) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (t9
     #29 * t862 + t906 - t933 * t915 - t865) + 0.90D2 * t7 * (-t867 - t9
     #33 * t906 + t929 * t865 + t940 + t941 * t915 / 0.2D1 - t944 * t862
     # / 0.2D1) + t950) * t97 * t59 / 0.2880D4 - t978 * t862 / 0.5760D4 
     #- t984 * t867 / 0.5760D4 - t991 * t865 / 0.5760D4 + (-0.180D3 * t8
     # * (-t867 + t997 * t865 + t940 - t1001 * t906 - t1003 * t862 / 0.2
     #D1 + t1006 * t915 / 0.2D1) + t39 * (t906 - t865 + t997 * t862 - t1
     #001 * t915) + 0.90D2 * t7 * (t997 * t867 - t1003 * t865 / 0.2D1 + 
     #t1006 * t906 / 0.2D1 + t1021 * t862 / 0.6D1 - t1024 * t915 / 0.6D1
     # - t1001 * t940) + t56 * t920) * t97 / 0.5760D4 + (-0.180D3 * t8 *
     # (-t1037 * t915 + t906 - t865 + t1041 * t862) + 0.90D2 * t7 * (t10
     #46 * t915 / 0.2D1 - t1037 * t906 - t867 - t1050 * t862 / 0.2D1 + t
     #940 + t1041 * t865) + t950) * t79 * t97 / 0.5760D4 - (-0.180D3 * t
     #8 * (t1065 * t862 / 0.2D1 - t1064 * t865 + t867) + t39 * (t865 - t
     #1064 * t862) + 0.90D2 * t7 * (-t1064 * t867 - t1076 * t862 / 0.6D1
     # + t1065 * t865 / 0.2D1) + t883) * t79 / 0.5760D4
      t1088 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1087)
      t1090 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1092 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1107 = bggbH51J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1117 = (0.90D2 * t7 * (t567 * t1090 - t1092) + 0.180D3 * t8 * t10
     #90) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (t581 * t1090 - t109
     #2) + 0.90D2 * t7 * (-t586 * t1090 / 0.2D1 - t1107 + t581 * t1092) 
     #- t39 * t1090) * t97 * t59 / 0.2880D4
      t1118 = FJET(XB1, XB2, s, -t561, -t5, -t559, 0.0D0, t471, t1117)
      t1120 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1122 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1136 = bggbH53J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1147 = (0.90D2 * t7 * (t567 * t1120 - t1122) + 0.180D3 * t8 * t11
     #20) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t1122 + t581 * t11
     #20) + 0.90D2 * t7 * (t581 * t1122 - t1136 - t586 * t1120 / 0.2D1) 
     #- t39 * t1120) * t97 * t59 / 0.2880D4
      t1148 = FJET(XB1, XB2, s, 0.0D0, -t559, -t5, -t561, t471, t1147)
      t1150 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1152 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1176 = (0.90D2 * t7 * (t567 * t1150 - t1152) + 0.180D3 * t8 * t11
     #50) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t1152 + t581 * t11
     #50) + 0.90D2 * t7 * (-t586 * t1150 / 0.2D1 + t581 * t1152) - t39 *
     # t1150) * t97 * t59 / 0.2880D4
      t1177 = FJET(XB1, XB2, s, -t5, -t561, 0.0D0, -t559, t471, t1176)
      t1179 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1182 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1187 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1189 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1209 = t1182 - t1179
      t1228 = t39 * t1209
      t1249 = t56 * t1179
      t1318 = -t978 * t1179 / 0.5760D4 + (-0.180D3 * t8 * (t1006 * t1182
     # / 0.2D1 - t1003 * t1179 / 0.2D1 - t1001 * t1187 + t997 * t1189) +
     # t39 * (-t1001 * t1182 + t997 * t1179 - t1189 + t1187) + 0.90D2 * 
     #t7 * (-t1003 * t1189 / 0.2D1 + t1006 * t1187 / 0.2D1 + t1021 * t11
     #79 / 0.6D1 - t1024 * t1182 / 0.6D1) + t56 * t1209) * t97 / 0.5760D
     #4 + (-0.180D3 * t8 * (-t1189 + t1187 - t1037 * t1182 + t1041 * t11
     #79) + 0.90D2 * t7 * (-t1050 * t1179 / 0.2D1 + t1046 * t1182 / 0.2D
     #1 + t1041 * t1189 - t1037 * t1187) + t1228) * t79 * t97 / 0.5760D4
     # + (-0.180D3 * t8 * (t860 * t1189 - t861 * t1179 / 0.2D1) + t39 * 
     #(-t1189 + t860 * t1179) + 0.90D2 * t7 * (-t861 * t1189 / 0.2D1 + t
     #875 * t1179 / 0.6D1) - t1249) * t59 / 0.2880D4 + (-0.180D3 * t8 * 
     #(t889 * t1179 - t1189) + 0.90D2 * t7 * (t889 * t1189 - t894 * t117
     #9 / 0.2D1) - t39 * t1179) * t79 * t59 / 0.2880D4 + (0.90D2 * t7 * 
     #(t1187 - t914 * t1182 - t1189 + t909 * t1179) - 0.180D3 * t8 * t12
     #09) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (t1187 + t929 * t117
     #9 - t933 * t1182 - t1189) + 0.90D2 * t7 * (t929 * t1189 + t941 * t
     #1182 / 0.2D1 - t933 * t1187 - t944 * t1179 / 0.2D1) + t1228) * t97
     # * t59 / 0.2880D4 - (-0.180D3 * t8 * (t1065 * t1179 / 0.2D1 - t106
     #4 * t1189) + t39 * (t1189 - t1064 * t1179) + 0.90D2 * t7 * (t1065 
     #* t1189 / 0.2D1 - t1076 * t1179 / 0.6D1) + t1249) * t79 / 0.5760D4
     # - t991 * t1189 / 0.5760D4
      t1319 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1318)
      t1321 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t1322 = t141 * t1321
      t1324 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t1325 = t142 * t1324
      t1329 = t142 * t1321
      t1340 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t131, x4)
      t1354 = (0.90D2 * t7 * (-t185 * t1322 + t1325) - 0.180D3 * t8 * t1
     #329) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t157 * t1322 + t1
     #325) + 0.90D2 * t7 * (t142 * t1340 - t157 * t141 * t1324 + t165 * 
     #t1322 / 0.2D1) + t39 * t1329) * t79 * t97 / 0.5760D4
      t1355 = FJET(XB1, XB2, s, t129, 0.0D0, t132, 0.0D0, 0.0D0, t1354)
      t1357 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1360 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1379 = t56 * t1360
      t1418 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1419 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1424 = t1419 - t1360
      t1445 = t39 * t1424
      t1496 = -t991 * t1357 / 0.5760D4 - t978 * t1360 / 0.5760D4 - (-0.1
     #80D3 * t8 * (-t1064 * t1357 + t1065 * t1360 / 0.2D1) + t39 * (t135
     #7 - t1064 * t1360) + 0.90D2 * t7 * (-t1076 * t1360 / 0.6D1 + t1065
     # * t1357 / 0.2D1) + t1379) * t79 / 0.5760D4 + (-0.180D3 * t8 * (t8
     #60 * t1357 - t861 * t1360 / 0.2D1) + t39 * (-t1357 + t860 * t1360)
     # + 0.90D2 * t7 * (t875 * t1360 / 0.6D1 - t861 * t1357 / 0.2D1) - t
     #1379) * t59 / 0.2880D4 + (-0.180D3 * t8 * (t889 * t1360 - t1357) +
     # 0.90D2 * t7 * (t889 * t1357 - t894 * t1360 / 0.2D1) - t39 * t1360
     #) * t79 * t59 / 0.2880D4 + (0.90D2 * t7 * (t909 * t1360 - t1357 + 
     #t1418 - t914 * t1419) - 0.180D3 * t8 * t1424) * t79 * t98 / 0.2880
     #D4 + (-0.180D3 * t8 * (t929 * t1360 - t933 * t1419 + t1418 - t1357
     #) + 0.90D2 * t7 * (t929 * t1357 - t933 * t1418 - t944 * t1360 / 0.
     #2D1 + t941 * t1419 / 0.2D1) + t1445) * t97 * t59 / 0.2880D4 + (-0.
     #180D3 * t8 * (-t1003 * t1360 / 0.2D1 + t997 * t1357 - t1001 * t141
     #8 + t1006 * t1419 / 0.2D1) + t39 * (-t1357 - t1001 * t1419 + t1418
     # + t997 * t1360) + 0.90D2 * t7 * (-t1003 * t1357 / 0.2D1 + t1006 *
     # t1418 / 0.2D1 + t1021 * t1360 / 0.6D1 - t1024 * t1419 / 0.6D1) + 
     #t56 * t1424) * t97 / 0.5760D4 + (-0.180D3 * t8 * (t1418 - t1357 - 
     #t1037 * t1419 + t1041 * t1360) + 0.90D2 * t7 * (t1046 * t1419 / 0.
     #2D1 - t1037 * t1418 + t1041 * t1357 - t1050 * t1360 / 0.2D1) + t14
     #45) * t79 * t97 / 0.5760D4
      t1497 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1496)
      t1499 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t1501 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t1506 = bggbH53J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t1526 = (-0.180D3 * t8 * (t400 * t1499 - t1501) + 0.90D2 * t7 * (t
     #400 * t1501 - t1506 - t407 * t1499 / 0.2D1) - t39 * t1499) * t79 *
     # t59 / 0.2880D4 + (0.90D2 * t7 * (t421 * t1499 - t1501) + 0.180D3 
     #* t8 * t1499) * t79 * t98 / 0.2880D4
      t1527 = FJET(XB1, XB2, s, -t395, t393, t391, -t389, 0.0D0, t1526)
      t1529 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t1531 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t242, x4)
      t1555 = (-0.180D3 * t8 * (t400 * t1529 - t1531) + 0.90D2 * t7 * (t
     #400 * t1531 - t407 * t1529 / 0.2D1) - t39 * t1529) * t79 * t59 / 0
     #.2880D4 + (0.90D2 * t7 * (t421 * t1529 - t1531) + 0.180D3 * t8 * t
     #1529) * t79 * t98 / 0.2880D4
      t1556 = FJET(XB1, XB2, s, t391, -t389, -t395, t393, 0.0D0, t1555)
      t1558 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1560 = bggbH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1561 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1591 = t39 * t1561
      t1618 = (-0.180D3 * t8 * (-t24 * t1558 + t1560 + t27 * t1561 / 0.2
     #D1) + t39 * (-t24 * t1561 + t1558) + 0.90D2 * t7 * (-t45 * t1561 /
     # 0.6D1 + t27 * t1558 / 0.2D1 - t24 * t1560) + t56 * t1561) * t59 /
     # 0.2880D4 + (-0.180D3 * t8 * (t1558 - t65 * t1561) + 0.90D2 * t7 *
     # (-t65 * t1558 + t71 * t1561 / 0.2D1 + t1560) + t1591) * t79 * t59
     # / 0.2880D4 + (0.90D2 * t7 * (-t88 * t1561 + t1558) - 0.180D3 * t8
     # * t1561) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (t1558 - t104 
     #* t1561) + 0.90D2 * t7 * (t1560 - t104 * t1558 + t110 * t1561 / 0.
     #2D1) + t1591) * t97 * t59 / 0.2880D4
      t1619 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t1618)
      t1621 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1623 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1652 = t39 * t1623
      t1679 = (-0.180D3 * t8 * (-t24 * t1621 + t27 * t1623 / 0.2D1) + t3
     #9 * (-t24 * t1623 + t1621) + 0.90D2 * t7 * (-t45 * t1623 / 0.6D1 +
     # t27 * t1621 / 0.2D1) + t56 * t1623) * t59 / 0.2880D4 + (-0.180D3 
     #* t8 * (-t65 * t1623 + t1621) + 0.90D2 * t7 * (-t65 * t1621 + t71 
     #* t1623 / 0.2D1) + t1652) * t79 * t59 / 0.2880D4 + (0.90D2 * t7 * 
     #(t1621 - t88 * t1623) - 0.180D3 * t8 * t1623) * t79 * t98 / 0.2880
     #D4 + (-0.180D3 * t8 * (t1621 - t104 * t1623) + 0.90D2 * t7 * (-t10
     #4 * t1621 + t110 * t1623 / 0.2D1) + t1652) * t97 * t59 / 0.2880D4
      t1680 = FJET(XB1, XB2, s, -t5, t3, 0.0D0, 0.0D0, 0.0D0, t1679)
      t1682 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t1683 = t519 * t1682
      t1685 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t1693 = 0.90D2 * t7 * (-t518 * t1683 + t475 * t506 * t1685) - 0.18
     #0D3 * t526 * t1683
      t1697 = FJET(XB1, XB2, s, t465, -t460, -t458, -t435, t471, t1693 *
     # t79 * t98 / 0.2880D4)
      t1701 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t1704 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, t131, x4)
      t1705 = t519 * t1704
      t1712 = 0.90D2 * t7 * (t475 * t506 * t1701 - t518 * t1705) - 0.180
     #D3 * t526 * t1705
      t1716 = FJET(XB1, XB2, s, -t460, t465, -t435, -t458, t471, t1712 *
     # t79 * t98 / 0.2880D4)
      t1720 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1723 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1725 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1742 = t56 * t1725
      t1762 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1763 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1768 = t1763 - t1725
      t1780 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1790 = t39 * t1768
      t1867 = -t991 * t1720 / 0.5760D4 + (-0.180D3 * t8 * (-t1723 + t860
     # * t1720 - t861 * t1725 / 0.2D1) + t39 * (-t1720 + t860 * t1725) +
     # 0.90D2 * t7 * (t860 * t1723 - t861 * t1720 / 0.2D1 + t875 * t1725
     # / 0.6D1) - t1742) * t59 / 0.2880D4 + (-0.180D3 * t8 * (-t1720 + t
     #889 * t1725) + 0.90D2 * t7 * (t889 * t1720 - t894 * t1725 / 0.2D1 
     #- t1723) - t39 * t1725) * t79 * t59 / 0.2880D4 + (0.90D2 * t7 * (-
     #t1720 + t909 * t1725 + t1762 - t914 * t1763) - 0.180D3 * t8 * t176
     #8) * t79 * t98 / 0.2880D4 + (-0.180D3 * t8 * (-t933 * t1763 - t172
     #0 + t929 * t1725 + t1762) + 0.90D2 * t7 * (t1780 + t929 * t1720 - 
     #t1723 - t933 * t1762 - t944 * t1725 / 0.2D1 + t941 * t1763 / 0.2D1
     #) + t1790) * t97 * t59 / 0.2880D4 - t978 * t1725 / 0.5760D4 - t984
     # * t1723 / 0.5760D4 + (-0.180D3 * t8 * (-t1723 + t1780 - t1003 * t
     #1725 / 0.2D1 - t1001 * t1762 + t997 * t1720 + t1006 * t1763 / 0.2D
     #1) + t39 * (-t1001 * t1763 - t1720 + t1762 + t997 * t1725) + 0.90D
     #2 * t7 * (t997 * t1723 - t1003 * t1720 / 0.2D1 + t1006 * t1762 / 0
     #.2D1 + t1021 * t1725 / 0.6D1 - t1024 * t1763 / 0.6D1 - t1001 * t17
     #80) + t56 * t1768) * t97 / 0.5760D4 + (-0.180D3 * t8 * (t1041 * t1
     #725 - t1720 - t1037 * t1763 + t1762) + 0.90D2 * t7 * (-t1723 - t10
     #37 * t1762 + t1041 * t1720 + t1046 * t1763 / 0.2D1 - t1050 * t1725
     # / 0.2D1 + t1780) + t1790) * t79 * t97 / 0.5760D4 - (-0.180D3 * t8
     # * (t1723 + t1065 * t1725 / 0.2D1 - t1064 * t1720) + t39 * (t1720 
     #- t1064 * t1725) + 0.90D2 * t7 * (-t1064 * t1723 + t1065 * t1720 /
     # 0.2D1 - t1076 * t1725 / 0.6D1) + t1742) * t79 / 0.5760D4
      t1868 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1867)
      t1870 = t1088 * t1087 + t1118 * t1117 + t1148 * t1147 + t1177 * t1
     #176 + t1319 * t1318 + t1355 * t1354 + t1497 * t1496 + t1527 * t152
     #6 + t1556 * t1555 + t1619 * t1618 + t1680 * t1679 + t1697 * t1693 
     #* t536 / 0.2880D4 + t1716 * t1712 * t536 / 0.2880D4 + t1868 * t186
     #7
      bggbH5n4e1 = t857 + t1870

      end function



      doubleprecision function bggbH5n4e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = -0.1D1 + x2
      t7 = x2 * x3
      t8 = t7 - 0.1D1
      t9 = 0.1D1 / t8
      t10 = x3 * t5 * t9
      t11 = t4 * t10
      t12 = t2 * x1
      t13 = x3 * z
      t14 = x1 * x3
      t15 = x4 * 0.3141592653589793D1
      t16 = cos(t15)
      t17 = x1 * z
      t18 = -z - x1 + t17
      t20 = -0.1D1 + x3
      t21 = x2 * t20
      t23 = Sqrt(x3 * t18 * t21)
      t24 = t16 * t23
      t25 = 0.2D1 * t24
      t26 = x2 ** 2
      t27 = t26 * x3
      t28 = t27 * x1
      t30 = t14 * z
      t31 = t7 * z
      t33 = t7 * x1
      t37 = t27 * t17
      t40 = -t13 - t14 + t7 - t25 - x2 - t28 - t27 * z + t30 + 0.2D1 * t
     #31 + 0.2D1 * t33 + 0.2D1 * t24 * x2 + t37 - 0.2D1 * t7 * t17
      t41 = 0.1D1 / t18
      t44 = t12 * t40 * t41 * t9
      t45 = t20 * s
      t46 = t1 * t3
      t48 = t45 * t46 * t9
      t53 = t12 * t5 * (-t7 - z + t13 - x1 + t14 + t17 - t30 + t25) * t4
     #1 * t9
      t54 = t1 ** 2
      t59 = s * t54 * x2 * x1 * t3 * t41
      t60 = s ** 2
      t61 = 0.1D1 / t60
      t63 = x2 * x1
      t64 = t63 * z
      t65 = z + x1 - t17 - t63 + t64
      t70 = x1 ** 2
      t72 = t70 * x3
      t76 = z ** 2
      t78 = x2 * t70
      t81 = 0.2D1 * t24 * z + 0.2D1 * t24 * x1 - t70 - 0.2D1 * t17 - t72
     # * x2 + t28 + t64 - t31 - t33 + 0.2D1 * t24 * t64 - t63 * t76 - 0.
     #2D1 * t78 * z
      t99 = t78 * t76 - 0.2D1 * t24 * t63 - 0.2D1 * t24 * t17 + 0.2D1 * 
     #x1 * t76 + 0.2D1 * t70 * z - t70 * t76 + t78 - t76 + x3 * t76 * t6
     #3 + 0.2D1 * t72 * x2 * z - t72 * t76 * x2 - t37
      t101 = 0.1D1 / (t81 + t99)
      t103 = t61 * t18 * t65 * t101
      t104 = t20 * t9
      t105 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, t104, x4)
      t106 = 0.1D1 / x3
      t108 = 0.1D1 / x2
      t109 = 0.1D1 / x1
      t110 = t108 * t109
      t114 = FJET(XB1, XB2, s, -t11, -t44, -t48, t53, t59, t103 * t105 *
     # t106 * t110 / 0.32D2)
      t116 = t18 * t65
      t120 = t106 * t108 * t109
      t124 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, t104, x4)
      t129 = FJET(XB1, XB2, s, t53, -t48, -t44, -t11, t59, t103 * t124 *
     # t106 * t110 / 0.32D2)
      t137 = 0.1D1 / t76 / z
      t138 = Sin(t15)
      t139 = t138 ** 2
      t140 = t137 * t139
      t143 = log(0.4D1 * t72 * t140)
      t144 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t146 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t150 = t61 * lh
      t157 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t158 = t157 - t144
      t164 = log(0.4D1 * t78 * t140)
      t166 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t167 = t5 ** 2
      t168 = t140 * t167
      t171 = log(0.4D1 * t78 * t168)
      t177 = 0.180D3 * t150 * t158
      t182 = t70 * t139
      t185 = log(0.4D1 * t182 * t137)
      t190 = t185 ** 2
      t194 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t198 = lh ** 2
      t200 = 0.3141592653589793D1 ** 2
      t202 = 0.180D3 * t198 - 0.30D2 * t200
      t203 = t61 * t202
      t204 = t203 * t144
      t209 = log(0.4D1 * t140)
      t210 = t209 * t61
      t212 = t209 ** 2
      t213 = t212 * t61
      t225 = -t210 * t202 - 0.90D2 * t213 * lh + t61 * (-0.2884936567583
     #026D3 - 0.120D3 * t198 * lh + 0.60D2 * lh * t200) - 0.15D2 * t212 
     #* t209 * t61
      t231 = log(0.4D1 * x3 * t137 * t139)
      t236 = t231 ** 2
      t249 = 0.180D3 * t210 * lh + t203 + 0.45D2 * t213
      t254 = log(0.4D1 * t7 * t168)
      t258 = log(0.4D1 * t7 * t140)
      t267 = x2 * t137
      t270 = log(0.4D1 * t267 * t139)
      t272 = t139 * t167
      t275 = log(0.4D1 * t267 * t272)
      t281 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t283 = t270 ** 2
      t286 = t275 ** 2
      t298 = -0.180D3 * t150 - 0.90D2 * t210
      t301 = (0.90D2 * t61 * (t143 * t144 - t146) + 0.180D3 * t150 * t14
     #4) * t106 * t109 / 0.2880D4 + t61 * t158 * t120 / 0.32D2 + (0.90D2
     # * t61 * (t164 * t144 + t166 - t171 * t157 - t146) - t177) * t108 
     #* t109 / 0.2880D4 + (-0.180D3 * t150 * (-t146 + t185 * t144) + 0.9
     #0D2 * t61 * (-t190 * t144 / 0.2D1 + t185 * t146 - t194) - t204) * 
     #t109 / 0.2880D4 - t225 * t144 / 0.5760D4 - (-0.180D3 * t150 * (t14
     #6 - t231 * t144) + 0.90D2 * t61 * (t236 * t144 / 0.2D1 - t231 * t1
     #46 + t194) + t204) * t106 / 0.5760D4 - t249 * t146 / 0.5760D4 + (0
     #.90D2 * t61 * (-t254 * t157 + t166 - t146 + t258 * t144) - t177) *
     # t106 * t108 / 0.5760D4 + (-0.180D3 * t150 * (t166 - t146 + t270 *
     # t144 - t275 * t157) + 0.90D2 * t61 * (-t194 + t270 * t146 + t281 
     #- t275 * t166 - t283 * t144 / 0.2D1 + t286 * t157 / 0.2D1) + t203 
     #* t158) * t108 / 0.5760D4 - t298 * t194 / 0.5760D4
      t302 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t301)
      t304 = t2 * t10
      t305 = t2 * t104
      t308 = Sqrt(-t13 * t21)
      t312 = 0.1D1 / (-z - t7 + 0.2D1 * t16 * t308)
      t313 = t61 * z * t312
      t314 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t319 = z * t312
      t320 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t323 = t8 ** 2
      t329 = log(-0.4D1 * t7 * t137 * t272 * t20 / t323)
      t330 = t329 * z
      t343 = t313 * t314 * t106 * t110 / 0.32D2 + (0.90D2 * t61 * (t319 
     #* t320 - t330 * t312 * t314) - 0.180D3 * t150 * t319 * t314) * t10
     #6 * t108 / 0.5760D4
      t344 = FJET(XB1, XB2, s, 0.0D0, t304, 0.0D0, t305, 0.0D0, t343)
      t347 = t2 * t63 * t41
      t349 = t1 * x1
      t350 = t5 * s * t349
      t351 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t355 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t356 = t78 * t139
      t358 = 0.1D1 / t76 * t41
      t359 = t3 ** 2
      t364 = log(-0.4D1 * t356 * t358 * t359 * t167)
      t375 = -t61 * t351 * t120 / 0.32D2 + (0.90D2 * t61 * (-t355 + t364
     # * t351) + 0.180D3 * t150 * t351) * t108 * t109 / 0.2880D4
      t376 = FJET(XB1, XB2, s, -t347, 0.0D0, -t350, -t4, t59, t375)
      t378 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t383 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t393 = -t61 * t378 * t120 / 0.32D2 + (0.90D2 * t61 * (t364 * t378 
     #- t383) + 0.180D3 * t150 * t378) * t108 * t109 / 0.2880D4
      t394 = FJET(XB1, XB2, s, -t350, -t4, -t347, 0.0D0, t59, t393)
      t396 = t2 * t20
      t397 = t2 * x3
      t398 = t140 * t20
      t401 = log(-0.4D1 * t72 * t398)
      t402 = -t20
      t403 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t405 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t410 = 0.180D3 * t150 * t403
      t420 = log(-0.4D1 * t7 * t398)
      t432 = log(-0.4D1 * t140 * x3 * t20)
      t437 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t438 = t432 ** 2
      t449 = (0.90D2 * t61 * (-t401 * t403 + t405) - t410) * t106 * t109
     # / 0.2880D4 + t61 * t403 * t120 / 0.32D2 + (0.90D2 * t61 * (-t420 
     #* t403 + t405) - t410) * t106 * t108 / 0.5760D4 - (-0.180D3 * t150
     # * (-t405 + t432 * t403) + 0.90D2 * t61 * (-t437 - t438 * t403 / 0
     #.2D1 + t432 * t405) - t203 * t403) * t106 / 0.5760D4
      t450 = FJET(XB1, XB2, s, 0.0D0, -t396, 0.0D0, t397, 0.0D0, t449)
      t452 = t45 * t349
      t453 = t45 * t46
      t454 = t2 * t14
      t456 = t2 * t3 * x3
      t457 = t72 * t139
      t462 = log(0.4D1 * t457 * t358 * t359 * t20)
      t463 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t465 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t478 = (0.90D2 * t61 * (t462 * t463 - t465) + 0.180D3 * t150 * t46
     #3) * t106 * t109 / 0.2880D4 - t61 * t463 * t120 / 0.32D2
      t479 = FJET(XB1, XB2, s, -t452, t453, t454, -t456, 0.0D0, t478)
      t481 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, t104, x4)
      t486 = FJET(XB1, XB2, s, -t48, t53, -t11, -t44, t59, t103 * t481 *
     # t106 * t110 / 0.32D2)
      t493 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t495 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t508 = (0.90D2 * t61 * (t462 * t493 - t495) + 0.180D3 * t150 * t49
     #3) * t106 * t109 / 0.2880D4 - t61 * t493 * t120 / 0.32D2
      t509 = FJET(XB1, XB2, s, t454, -t456, -t452, t453, 0.0D0, t508)
      t511 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t513 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t518 = 0.180D3 * t150 * t511
      t548 = (0.90D2 * t61 * (-t401 * t511 + t513) - t518) * t106 * t109
     # / 0.2880D4 + t61 * t511 * t120 / 0.32D2 - (-0.180D3 * t150 * (-t5
     #13 + t432 * t511) + 0.90D2 * t61 * (-t438 * t511 / 0.2D1 + t432 * 
     #t513) - t203 * t511) * t106 / 0.5760D4 + (0.90D2 * t61 * (-t420 * 
     #t511 + t513) - t518) * t106 * t108 / 0.5760D4
      t549 = FJET(XB1, XB2, s, -t396, 0.0D0, t397, 0.0D0, 0.0D0, t548)
      t551 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t552 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t563 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t564 = t563 - t552
      t570 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t575 = 0.180D3 * t150 * t564
      t584 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t591 = t203 * t552
      t611 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t642 = (0.90D2 * t61 * (-t551 + t143 * t552) + 0.180D3 * t150 * t5
     #52) * t106 * t109 / 0.2880D4 + t61 * t564 * t120 / 0.32D2 + (0.90D
     #2 * t61 * (-t171 * t563 - t551 + t164 * t552 + t570) - t575) * t10
     #8 * t109 / 0.2880D4 + (-0.180D3 * t150 * (-t551 + t185 * t552) + 0
     #.90D2 * t61 * (-t584 + t185 * t551 - t190 * t552 / 0.2D1) - t591) 
     #* t109 / 0.2880D4 - t249 * t551 / 0.5760D4 + (0.90D2 * t61 * (t258
     # * t552 - t551 - t254 * t563 + t570) - t575) * t106 * t108 / 0.576
     #0D4 + (-0.180D3 * t150 * (-t275 * t563 - t551 + t570 + t270 * t552
     #) + 0.90D2 * t61 * (-t584 + t611 - t283 * t552 / 0.2D1 - t275 * t5
     #70 + t270 * t551 + t286 * t563 / 0.2D1) + t203 * t564) * t108 / 0.
     #5760D4 - t225 * t552 / 0.5760D4 - (-0.180D3 * t150 * (t551 - t231 
     #* t552) + 0.90D2 * t61 * (t584 + t236 * t552 / 0.2D1 - t231 * t551
     #) + t591) * t106 / 0.5760D4 - t298 * t584 / 0.5760D4
      t643 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t642)
      t645 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t650 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t664 = t313 * t645 * t106 * t110 / 0.32D2 + (0.90D2 * t61 * (t319 
     #* t650 - t330 * t312 * t645) - 0.180D3 * t150 * t319 * t645) * t10
     #6 * t108 / 0.5760D4
      t665 = FJET(XB1, XB2, s, 0.0D0, t305, 0.0D0, t304, 0.0D0, t664)
      t667 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t669 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t682 = (0.90D2 * t61 * (t462 * t667 - t669) + 0.180D3 * t150 * t66
     #7) * t106 * t109 / 0.2880D4 - t61 * t667 * t120 / 0.32D2
      t683 = FJET(XB1, XB2, s, -t456, t454, t453, -t452, 0.0D0, t682)
      t685 = t114 * t61 * t116 * t101 * t105 * t120 / 0.32D2 + t129 * t6
     #1 * t116 * t101 * t124 * t120 / 0.32D2 + t302 * t301 + t344 * t343
     # + t376 * t375 + t394 * t393 + t450 * t449 + t479 * t478 + t486 * 
     #t61 * t116 * t101 * t481 * t120 / 0.32D2 + t509 * t508 + t549 * t5
     #48 + t643 * t642 + t665 * t664 + t683 * t682
      t686 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t688 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t698 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t699 = t698 - t686
      t705 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t710 = 0.180D3 * t150 * t699
      t725 = t203 * t686
      t773 = (0.90D2 * t61 * (t143 * t686 - t688) + 0.180D3 * t150 * t68
     #6) * t106 * t109 / 0.2880D4 + t61 * t699 * t120 / 0.32D2 + (0.90D2
     # * t61 * (t164 * t686 - t171 * t698 + t705 - t688) - t710) * t108 
     #* t109 / 0.2880D4 + (-0.180D3 * t150 * (-t688 + t185 * t686) + 0.9
     #0D2 * t61 * (t185 * t688 - t190 * t686 / 0.2D1) - t725) * t109 / 0
     #.2880D4 - (-0.180D3 * t150 * (t688 - t231 * t686) + 0.90D2 * t61 *
     # (-t231 * t688 + t236 * t686 / 0.2D1) + t725) * t106 / 0.5760D4 - 
     #t249 * t688 / 0.5760D4 + (0.90D2 * t61 * (t705 - t688 - t254 * t69
     #8 + t258 * t686) - t710) * t106 * t108 / 0.5760D4 + (-0.180D3 * t1
     #50 * (-t688 - t275 * t698 + t705 + t270 * t686) + 0.90D2 * t61 * (
     #-t283 * t686 / 0.2D1 + t270 * t688 - t275 * t705 + t286 * t698 / 0
     #.2D1) + t203 * t699) * t108 / 0.5760D4 - t225 * t686 / 0.5760D4
      t774 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t773)
      t776 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t780 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t791 = -t61 * t776 * t120 / 0.32D2 + (0.90D2 * t61 * (-t780 + t364
     # * t776) + 0.180D3 * t150 * t776) * t108 * t109 / 0.2880D4
      t792 = FJET(XB1, XB2, s, 0.0D0, -t347, -t4, -t350, t59, t791)
      t794 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t796 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t801 = 0.180D3 * t150 * t794
      t831 = (0.90D2 * t61 * (-t420 * t794 + t796) - t801) * t106 * t108
     # / 0.5760D4 + (0.90D2 * t61 * (-t401 * t794 + t796) - t801) * t106
     # * t109 / 0.2880D4 + t61 * t794 * t120 / 0.32D2 - (-0.180D3 * t150
     # * (t432 * t794 - t796) + 0.90D2 * t61 * (-t438 * t794 / 0.2D1 + t
     #432 * t796) - t203 * t794) * t106 / 0.5760D4
      t832 = FJET(XB1, XB2, s, 0.0D0, t397, 0.0D0, -t396, 0.0D0, t831)
      t834 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t841 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t853 = t313 * t834 * t106 * t110 / 0.32D2 + (0.90D2 * t61 * (-t330
     # * t312 * t834 + t319 * t841) - 0.180D3 * t150 * t319 * t834) * t1
     #06 * t108 / 0.5760D4
      t854 = FJET(XB1, XB2, s, t305, 0.0D0, t304, 0.0D0, 0.0D0, t853)
      t856 = t358 * t359
      t859 = log(-0.4D1 * t457 * t856)
      t860 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t862 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t867 = 0.180D3 * t150 * t860
      t877 = log(-0.4D1 * t356 * t856)
      t888 = log(-0.4D1 * t182 * t856)
      t893 = t888 ** 2
      t896 = bggbH53J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t905 = (0.90D2 * t61 * (-t859 * t860 + t862) - t867) * t106 * t109
     # / 0.2880D4 + t61 * t860 * t120 / 0.32D2 + (0.90D2 * t61 * (-t877 
     #* t860 + t862) - t867) * t108 * t109 / 0.2880D4 + (-0.180D3 * t150
     # * (t862 - t888 * t860) + 0.90D2 * t61 * (t893 * t860 / 0.2D1 + t8
     #96 - t888 * t862) + t203 * t860) * t109 / 0.2880D4
      t906 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t12, 0.0D0, t905)
      t908 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t915 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t104, x4)
      t927 = t313 * t908 * t106 * t110 / 0.32D2 + (0.90D2 * t61 * (-t330
     # * t312 * t908 + t319 * t915) - 0.180D3 * t150 * t319 * t908) * t1
     #06 * t108 / 0.5760D4
      t928 = FJET(XB1, XB2, s, t304, 0.0D0, t305, 0.0D0, 0.0D0, t927)
      t930 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t931 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t937 = 0.180D3 * t150 * t931
      t958 = bggbH51J3(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t968 = (0.90D2 * t61 * (t930 - t859 * t931) - t937) * t106 * t109 
     #/ 0.2880D4 + t61 * t931 * t120 / 0.32D2 + (0.90D2 * t61 * (t930 - 
     #t877 * t931) - t937) * t108 * t109 / 0.2880D4 + (-0.180D3 * t150 *
     # (-t888 * t931 + t930) + 0.90D2 * t61 * (-t888 * t930 + t958 + t89
     #3 * t931 / 0.2D1) + t203 * t931) * t109 / 0.2880D4
      t969 = FJET(XB1, XB2, s, t12, -t4, 0.0D0, 0.0D0, 0.0D0, t968)
      t971 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t973 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t402, x4)
      t986 = (0.90D2 * t61 * (t462 * t971 - t973) + 0.180D3 * t150 * t97
     #1) * t106 * t109 / 0.2880D4 - t61 * t971 * t120 / 0.32D2
      t987 = FJET(XB1, XB2, s, t453, -t452, -t456, t454, 0.0D0, t986)
      t989 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t991 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t996 = 0.180D3 * t150 * t989
      t1026 = (0.90D2 * t61 * (-t859 * t989 + t991) - t996) * t106 * t10
     #9 / 0.2880D4 + t61 * t989 * t120 / 0.32D2 + (0.90D2 * t61 * (t991 
     #- t877 * t989) - t996) * t108 * t109 / 0.2880D4 + (-0.180D3 * t150
     # * (-t888 * t989 + t991) + 0.90D2 * t61 * (-t888 * t991 + t893 * t
     #989 / 0.2D1) + t203 * t989) * t109 / 0.2880D4
      t1027 = FJET(XB1, XB2, s, -t4, t12, 0.0D0, 0.0D0, 0.0D0, t1026)
      t1029 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t1030 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t1036 = 0.180D3 * t150 * t1030
      t1050 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t402, x4)
      t1067 = (0.90D2 * t61 * (t1029 - t401 * t1030) - t1036) * t106 * t
     #109 / 0.2880D4 + t61 * t1030 * t120 / 0.32D2 - (-0.180D3 * t150 * 
     #(-t1029 + t432 * t1030) + 0.90D2 * t61 * (-t438 * t1030 / 0.2D1 - 
     #t1050 + t432 * t1029) - t203 * t1030) * t106 / 0.5760D4 + (0.90D2 
     #* t61 * (t1029 - t420 * t1030) - t1036) * t106 * t108 / 0.5760D4
      t1068 = FJET(XB1, XB2, s, t397, 0.0D0, -t396, 0.0D0, 0.0D0, t1067)
      t1070 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1071 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t1077 = 0.180D3 * t150 * t1071
      t1107 = (0.90D2 * t61 * (t1070 - t859 * t1071) - t1077) * t106 * t
     #109 / 0.2880D4 + t61 * t1071 * t120 / 0.32D2 + (0.90D2 * t61 * (-t
     #877 * t1071 + t1070) - t1077) * t108 * t109 / 0.2880D4 + (-0.180D3
     # * t150 * (-t888 * t1071 + t1070) + 0.90D2 * t61 * (-t888 * t1070 
     #+ t893 * t1071 / 0.2D1) + t203 * t1071) * t109 / 0.2880D4
      t1108 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t12, -t4, 0.0D0, t1107)
      t1110 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1114 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t1125 = -t61 * t1110 * t120 / 0.32D2 + (0.90D2 * t61 * (-t1114 + t
     #364 * t1110) + 0.180D3 * t150 * t1110) * t108 * t109 / 0.2880D4
      t1126 = FJET(XB1, XB2, s, -t4, -t350, 0.0D0, -t347, t59, t1125)
      t1128 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, t104, x4)
      t1133 = FJET(XB1, XB2, s, -t44, -t11, t53, -t48, t59, t103 * t1128
     # * t106 * t110 / 0.32D2)
      t1140 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1142 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4
     #)
      t1152 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1153 = t1152 - t1140
      t1157 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t1164 = 0.180D3 * t150 * t1153
      t1179 = t203 * t1140
      t1227 = (0.90D2 * t61 * (t143 * t1140 - t1142) + 0.180D3 * t150 * 
     #t1140) * t106 * t109 / 0.2880D4 + t61 * t1153 * t120 / 0.32D2 + (0
     #.90D2 * t61 * (t1157 + t164 * t1140 - t171 * t1152 - t1142) - t116
     #4) * t108 * t109 / 0.2880D4 + (-0.180D3 * t150 * (-t1142 + t185 * 
     #t1140) + 0.90D2 * t61 * (t185 * t1142 - t190 * t1140 / 0.2D1) - t1
     #179) * t109 / 0.2880D4 - (-0.180D3 * t150 * (t1142 - t231 * t1140)
     # + 0.90D2 * t61 * (t236 * t1140 / 0.2D1 - t231 * t1142) + t1179) *
     # t106 / 0.5760D4 - t225 * t1140 / 0.5760D4 - t249 * t1142 / 0.5760
     #D4 + (0.90D2 * t61 * (-t1142 + t1157 - t254 * t1152 + t258 * t1140
     #) - t1164) * t106 * t108 / 0.5760D4 + (-0.180D3 * t150 * (-t275 * 
     #t1152 + t270 * t1140 - t1142 + t1157) + 0.90D2 * t61 * (t286 * t11
     #52 / 0.2D1 - t283 * t1140 / 0.2D1 - t275 * t1157 + t270 * t1142) +
     # t203 * t1153) * t108 / 0.5760D4
      t1228 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1227)
      t1230 = t774 * t773 + t792 * t791 + t832 * t831 + t854 * t853 + t9
     #06 * t905 + t928 * t927 + t969 * t968 + t987 * t986 + t1027 * t102
     #6 + t1068 * t1067 + t1108 * t1107 + t1126 * t1125 + t1133 * t61 * 
     #t116 * t101 * t1128 * t120 / 0.32D2 + t1228 * t1227
      bggbH5n4e0 = t685 + t1230

      end function



      doubleprecision function bggbH5n4em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * lh
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t15 = log(0.4D1 * t13)
      t16 = t15 * t4
      t18 = -0.180D3 * t5 - 0.90D2 * t16
      t19 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t22 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t23 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t24 = t22 - t23
      t25 = t4 * t24
      t26 = 0.1D1 / x3
      t27 = 0.1D1 / x2
      t28 = t26 * t27
      t31 = x2 * t9
      t32 = -0.1D1 + x2
      t33 = t32 ** 2
      t37 = log(0.4D1 * t31 * t12 * t33)
      t39 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t42 = log(0.4D1 * t31 * t12)
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t60 = t15 ** 2
      t63 = 0.180D3 * t16 * lh + t4 * (0.180D3 * t54 - 0.30D2 * t56) + 0
     #.45D2 * t60 * t4
      t66 = x1 ** 2
      t67 = t66 * t12
      t70 = log(0.4D1 * t67 * t9)
      t76 = 0.180D3 * t5 * t23
      t78 = 0.1D1 / x1
      t82 = t26 * t78
      t85 = t27 * t78
      t91 = log(0.4D1 * x3 * t9 * t12)
      t99 = -t18 * t19 / 0.5760D4 + t25 * t28 / 0.64D2 + (0.90D2 * t4 * 
     #(-t19 - t37 * t22 + t39 + t42 * t23) - 0.180D3 * t5 * t24) * t27 /
     # 0.5760D4 - t63 * t23 / 0.5760D4 + (0.90D2 * t4 * (-t19 + t70 * t2
     #3) + t76) * t78 / 0.2880D4 - t4 * t23 * t82 / 0.32D2 + t25 * t85 /
     # 0.32D2 - (0.90D2 * t4 * (t19 - t91 * t23) - t76) * t26 / 0.5760D4
      t100 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t99)
      t102 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t103 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t109 = 0.180D3 * t5 * t103
      t116 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t117 = t116 - t103
      t118 = t4 * t117
      t125 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t138 = bggbH53J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t148 = (0.90D2 * t4 * (-t102 + t70 * t103) + t109) * t78 / 0.2880D
     #4 - t4 * t103 * t82 / 0.32D2 + t118 * t85 / 0.32D2 - t18 * t102 / 
     #0.5760D4 + t118 * t28 / 0.64D2 + (0.90D2 * t4 * (t125 - t102 + t42
     # * t103 - t37 * t116) - 0.180D3 * t5 * t117) * t27 / 0.5760D4 - t6
     #3 * t103 / 0.5760D4 - t4 * t138 / 0.64D2 - (0.90D2 * t4 * (t102 - 
     #t91 * t103) - t109) * t26 / 0.5760D4
      t149 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t148)
      t151 = t2 * x1
      t152 = -0.1D1 + x1
      t153 = t2 * t152
      t157 = 0.1D1 / (-z - x1 + x1 * z)
      t159 = t152 ** 2
      t163 = log(-0.4D1 * t67 / t7 * t157 * t159)
      t164 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t166 = bggbH52J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t175 = t4 * t164
      t180 = (0.90D2 * t4 * (-t163 * t164 + t166) - 0.180D3 * t5 * t164)
     # * t78 / 0.2880D4 + t175 * t82 / 0.32D2 + t175 * t85 / 0.32D2
      t181 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t151, -t153, 0.0D0, t180)
      t183 = bggbH53J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t184 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t194 = t4 * t184
      t199 = (0.90D2 * t4 * (t183 - t163 * t184) - 0.180D3 * t5 * t184) 
     #* t78 / 0.2880D4 + t194 * t82 / 0.32D2 + t194 * t85 / 0.32D2
      t200 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t153, t151, 0.0D0, t199)
      t202 = bggbH51J3(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t205 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t208 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t211 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t212 = t211 - t208
      t213 = t4 * t212
      t217 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t232 = 0.180D3 * t5 * t208
      t248 = -t4 * t202 / 0.64D2 - t18 * t205 / 0.5760D4 - t63 * t208 / 
     #0.5760D4 + t213 * t28 / 0.64D2 + (0.90D2 * t4 * (-t37 * t211 - t20
     #5 + t217 + t42 * t208) - 0.180D3 * t5 * t212) * t27 / 0.5760D4 + (
     #0.90D2 * t4 * (-t205 + t70 * t208) + t232) * t78 / 0.2880D4 - t4 *
     # t208 * t82 / 0.32D2 + t213 * t85 / 0.32D2 - (0.90D2 * t4 * (t205 
     #- t91 * t208) - t232) * t26 / 0.5760D4
      t249 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t248)
      t251 = t2 * x3
      t252 = -0.1D1 + x3
      t253 = t2 * t252
      t254 = -t252
      t255 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t256 = t4 * t255
      t264 = log(-0.4D1 * t13 * x3 * t252)
      t266 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t275 = t256 * t82 / 0.32D2 + t256 * t28 / 0.64D2 - (0.90D2 * t4 * 
     #(t264 * t255 - t266) + 0.180D3 * t5 * t255) * t26 / 0.5760D4
      t276 = FJET(XB1, XB2, s, 0.0D0, t251, 0.0D0, -t253, 0.0D0, t275)
      t278 = x2 * x3
      t280 = 0.1D1 / (t278 - 0.1D1)
      t281 = t252 * t280
      t282 = t2 * t281
      t285 = t2 * x3 * t32 * t280
      t287 = cos(t10)
      t291 = Sqrt(-x3 * z * x2 * t252)
      t295 = 0.1D1 / (-z - t278 + 0.2D1 * t287 * t291)
      t296 = t4 * z * t295
      t297 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t281, x4)
      t302 = FJET(XB1, XB2, s, 0.0D0, t282, 0.0D0, t285, 0.0D0, t296 * t
     #297 * t26 * t27 / 0.64D2)
      t309 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t281, x4)
      t314 = FJET(XB1, XB2, s, 0.0D0, t285, 0.0D0, t282, 0.0D0, t296 * t
     #309 * t26 * t27 / 0.64D2)
      t321 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t322 = t4 * t321
      t327 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t337 = t322 * t82 / 0.32D2 + t322 * t28 / 0.64D2 - (0.90D2 * t4 * 
     #(-t327 + t264 * t321) + 0.180D3 * t5 * t321) * t26 / 0.5760D4
      t338 = FJET(XB1, XB2, s, 0.0D0, -t253, 0.0D0, t251, 0.0D0, t337)
      t342 = t2 * x1 * x2 * t157
      t344 = t1 * x1
      t345 = t32 * s * t344
      t346 = t1 ** 2
      t351 = s * t346 * x2 * x1 * t152 * t157
      t352 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t356 = FJET(XB1, XB2, s, 0.0D0, -t342, -t153, -t345, t351, -t4 * t
     #352 * t85 / 0.32D2)
      t362 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t363 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t369 = 0.180D3 * t5 * t363
      t376 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t377 = t376 - t363
      t378 = t4 * t377
      t396 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t405 = (0.90D2 * t4 * (-t362 + t70 * t363) + t369) * t78 / 0.2880D
     #4 - t4 * t363 * t82 / 0.32D2 + t378 * t85 / 0.32D2 - (0.90D2 * t4 
     #* (t362 - t91 * t363) - t369) * t26 / 0.5760D4 - t18 * t362 / 0.57
     #60D4 - t63 * t363 / 0.5760D4 + t378 * t28 / 0.64D2 + (0.90D2 * t4 
     #* (-t37 * t376 + t42 * t363 - t362 + t396) - 0.180D3 * t5 * t377) 
     #* t27 / 0.5760D4
      t406 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t405)
      t408 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t410 = bggbH51J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t419 = t4 * t408
      t424 = (0.90D2 * t4 * (-t163 * t408 + t410) - 0.180D3 * t5 * t408)
     # * t78 / 0.2880D4 + t419 * t82 / 0.32D2 + t419 * t85 / 0.32D2
      t425 = FJET(XB1, XB2, s, t151, -t153, 0.0D0, 0.0D0, 0.0D0, t424)
      t427 = t100 * t99 + t149 * t148 + t181 * t180 + t200 * t199 + t249
     # * t248 + t276 * t275 + t302 * t4 * z * t295 * t297 * t28 / 0.64D2
     # + t314 * t4 * z * t295 * t309 * t28 / 0.64D2 + t338 * t337 - t356
     # * t4 * t352 * t27 * t78 / 0.32D2 + t406 * t405 + t425 * t424
      t428 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t429 = t4 * t428
      t432 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t444 = t429 * t82 / 0.32D2 - (0.90D2 * t4 * (-t432 + t264 * t428) 
     #+ 0.180D3 * t5 * t428) * t26 / 0.5760D4 + t429 * t28 / 0.64D2
      t445 = FJET(XB1, XB2, s, t251, 0.0D0, -t253, 0.0D0, 0.0D0, t444)
      t448 = t2 * x1 * x3
      t450 = t2 * t152 * x3
      t451 = t252 * s
      t452 = t451 * t344
      t454 = t451 * t1 * t152
      t455 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t254, x4)
      t459 = FJET(XB1, XB2, s, t448, -t450, -t452, t454, 0.0D0, -t4 * t4
     #55 * t82 / 0.32D2)
      t465 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t281, x4)
      t470 = FJET(XB1, XB2, s, t282, 0.0D0, t285, 0.0D0, 0.0D0, t296 * t
     #465 * t26 * t27 / 0.64D2)
      t477 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t254, x4)
      t481 = FJET(XB1, XB2, s, t454, -t452, -t450, t448, 0.0D0, -t4 * t4
     #77 * t82 / 0.32D2)
      t487 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t281, x4)
      t492 = FJET(XB1, XB2, s, t285, 0.0D0, t282, 0.0D0, 0.0D0, t296 * t
     #487 * t26 * t27 / 0.64D2)
      t499 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t501 = bggbH54J2(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t510 = t4 * t499
      t515 = (0.90D2 * t4 * (-t163 * t499 + t501) - 0.180D3 * t5 * t499)
     # * t78 / 0.2880D4 + t510 * t82 / 0.32D2 + t510 * t85 / 0.32D2
      t516 = FJET(XB1, XB2, s, -t153, t151, 0.0D0, 0.0D0, 0.0D0, t515)
      t518 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t522 = FJET(XB1, XB2, s, -t153, -t345, 0.0D0, -t342, t351, -t4 * t
     #518 * t85 / 0.32D2)
      t528 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t529 = t4 * t528
      t532 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t254, x4)
      t544 = t529 * t82 / 0.32D2 - (0.90D2 * t4 * (-t532 + t264 * t528) 
     #+ 0.180D3 * t5 * t528) * t26 / 0.5760D4 + t529 * t28 / 0.64D2
      t545 = FJET(XB1, XB2, s, -t253, 0.0D0, t251, 0.0D0, 0.0D0, t544)
      t547 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t254, x4)
      t551 = FJET(XB1, XB2, s, -t450, t448, t454, -t452, 0.0D0, -t4 * t5
     #47 * t82 / 0.32D2)
      t557 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t561 = FJET(XB1, XB2, s, -t345, -t153, -t342, 0.0D0, t351, -t4 * t
     #557 * t85 / 0.32D2)
      t567 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, t254, x4)
      t571 = FJET(XB1, XB2, s, -t452, t454, t448, -t450, 0.0D0, -t4 * t5
     #67 * t82 / 0.32D2)
      t577 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t581 = FJET(XB1, XB2, s, -t342, 0.0D0, -t345, -t153, t351, -t4 * t
     #577 * t85 / 0.32D2)
      t587 = t445 * t444 - t459 * t4 * t455 * t26 * t78 / 0.32D2 + t470 
     #* t4 * z * t295 * t465 * t28 / 0.64D2 - t481 * t4 * t477 * t26 * t
     #78 / 0.32D2 + t492 * t4 * z * t295 * t487 * t28 / 0.64D2 + t516 * 
     #t515 - t522 * t4 * t518 * t27 * t78 / 0.32D2 + t545 * t544 - t551 
     #* t4 * t547 * t26 * t78 / 0.32D2 - t561 * t4 * t557 * t27 * t78 / 
     #0.32D2 - t571 * t4 * t567 * t26 * t78 / 0.32D2 - t581 * t4 * t577 
     #* t27 * t78 / 0.32D2
      bggbH5n4em1 = t427 + t587

      end function



      doubleprecision function bggbH5n4em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t6 = t4 * t5
      t7 = 0.1D1 / x1
      t10 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t13 = 0.1D1 / x2
      t16 = bggbH52J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t21 = z ** 2
      t25 = Sin(x4 * 0.3141592653589793D1)
      t26 = t25 ** 2
      t29 = log(0.4D1 / t21 / z * t26)
      t32 = -0.180D3 * t4 * lh - 0.90D2 * t29 * t4
      t35 = 0.1D1 / x3
      t38 = -t6 * t7 / 0.32D2 + t4 * (t10 - t5) * t13 / 0.64D2 - t4 * t1
     #6 / 0.64D2 - t32 * t5 / 0.5760D4 - t6 * t35 / 0.64D2
      t39 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t38)
      t41 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t42 = t4 * t41
      t45 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t50 = bggbH53J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t57 = -t42 * t7 / 0.32D2 + t4 * (t45 - t41) * t13 / 0.64D2 - t4 * 
     #t50 / 0.64D2 - t32 * t41 / 0.5760D4 - t42 * t35 / 0.64D2
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t57)
      t60 = t2 * x1
      t62 = t2 * (-0.1D1 + x1)
      t63 = bggbH52J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t67 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t60, -t62, 0.0D0, t4 * t63 *
     # t7 / 0.32D2)
      t72 = bggbH53J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t76 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t62, t60, 0.0D0, t4 * t72 *
     # t7 / 0.32D2)
      t81 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t82 = t4 * t81
      t89 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t94 = bggbH51J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t97 = -t82 * t7 / 0.32D2 - t32 * t81 / 0.5760D4 - t82 * t35 / 0.64
     #D2 + t4 * (t89 - t81) * t13 / 0.64D2 - t4 * t94 / 0.64D2
      t98 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t97)
      t100 = t2 * x3
      t101 = -0.1D1 + x3
      t102 = t2 * t101
      t103 = -t101
      t104 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t103, x4)
      t108 = FJET(XB1, XB2, s, 0.0D0, t100, 0.0D0, -t102, 0.0D0, t4 * t1
     #04 * t35 / 0.64D2)
      t113 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t103, x4)
      t117 = FJET(XB1, XB2, s, 0.0D0, -t102, 0.0D0, t100, 0.0D0, t4 * t1
     #13 * t35 / 0.64D2)
      t122 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t123 = t4 * t122
      t130 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t135 = bggbH54J2(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t138 = -t123 * t7 / 0.32D2 - t32 * t122 / 0.5760D4 - t123 * t35 / 
     #0.64D2 + t4 * (t130 - t122) * t13 / 0.64D2 - t4 * t135 / 0.64D2
      t139 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t138)
      t141 = bggbH51J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t145 = FJET(XB1, XB2, s, t60, -t62, 0.0D0, 0.0D0, 0.0D0, t4 * t141
     # * t7 / 0.32D2)
      t150 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t103, x4)
      t154 = FJET(XB1, XB2, s, t100, 0.0D0, -t102, 0.0D0, 0.0D0, t4 * t1
     #50 * t35 / 0.64D2)
      t159 = bggbH54J1(s, XB1, XB2, z, lh, wd, x1, 0.0D0, 0.10D1, x4)
      t163 = FJET(XB1, XB2, s, -t62, t60, 0.0D0, 0.0D0, 0.0D0, t4 * t159
     # * t7 / 0.32D2)
      t168 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, t103, x4)
      t172 = FJET(XB1, XB2, s, -t102, 0.0D0, t100, 0.0D0, 0.0D0, t4 * t1
     #68 * t35 / 0.64D2)
      bggbH5n4em2 = t39 * t38 + t58 * t57 + t67 * t4 * t63 * t7 / 0.32D2
     # + t76 * t4 * t72 * t7 / 0.32D2 + t98 * t97 + t108 * t4 * t104 * t
     #35 / 0.64D2 + t117 * t4 * t113 * t35 / 0.64D2 + t139 * t138 + t145
     # * t4 * t141 * t7 / 0.32D2 + t154 * t4 * t150 * t35 / 0.64D2 + t16
     #3 * t4 * t159 * t7 / 0.32D2 + t172 * t4 * t168 * t35 / 0.64D2

      end function



      doubleprecision function bggbH5n4em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = bggbH52J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t8 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -t4 * t5 / 
     #0.64D2)
      t11 = bggbH53J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -t4 * t11 
     #/ 0.64D2)
      t17 = bggbH51J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t20 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -t4 * t17 
     #/ 0.64D2)
      t23 = bggbH54J1(s, XB1, XB2, z, lh, wd, 0.0D0, 0.0D0, 0.10D1, x4)
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -t4 * t23 
     #/ 0.64D2)
      bggbH5n4em3 = -t8 * t4 * t5 / 0.64D2 - t14 * t4 * t11 / 0.64D2 - t
     #20 * t4 * t17 / 0.64D2 - t26 * t4 * t23 / 0.64D2

      end function



      doubleprecision function bggbH5n4em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bggbH51J1
      doubleprecision bggbH51J2
      doubleprecision bggbH51J3
      doubleprecision bggbH52J1
      doubleprecision bggbH52J2
      doubleprecision bggbH53J1
      doubleprecision bggbH53J2
      doubleprecision bggbH53J3
      doubleprecision bggbH54J1
      doubleprecision bggbH54J2
      bggbH5n4em4 = 0.0D0

      end function
  
 

      doubleprecision function bggbH51J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = t8 * t7
      t10 = 0.1D1 - x3
      t11 = t10 ** 2
      t12 = t11 * t10
      t15 = s * t1
      t16 = t15 * t5
      t18 = z + x1 * t3
      t19 = 0.1D1 / t18
      t20 = x1 * t19
      t23 = x3 * (0.1D1 - x2)
      t27 = cos(x4 * 0.3141592653589793D1)
      t31 = Sqrt(t23 * t18 * x2 * t10)
      t34 = t23 * t18 + x2 * t10 - 0.2D1 * t27 * t31
      t35 = s * t3
      t38 = t7 * x3
      t40 = s - t35 * t20 * t34 - t35 * t38
      t41 = t34 * t40
      t42 = x3 ** 2
      t48 = t9 * t10 * t42
      t51 = t7 * t10
      t52 = z ** 2
      t53 = t51 * t52
      t56 = t2 * t4
      t58 = t8 * t11 * z
      t62 = t9 * t11 * x3
      t64 = t40 * t15
      t65 = t3 * t7
      t69 = t65 * x3
      t72 = t5 * t9
      t79 = t15 * t3 * x1
      t80 = t19 * t34
      t81 = t40 * z
      t85 = t15 * t4
      t86 = x1 ** 2
      t87 = t85 * t86
      t88 = t18 ** 2
      t89 = 0.1D1 / t88
      t90 = t34 ** 2
      t91 = t89 * t90
      t99 = t64 * t4
      t100 = t8 * t10
      t110 = t64 * z
      t111 = t4 * t8
      t112 = t111 * t42
      t115 = t64 * t5
      t118 = t6 * t9 * t12 + 0.3D1 * t16 * t20 * t41 * t8 * t42 + t6 * t
     #48 + 0.4D1 * t2 * t3 * t53 + 0.3D1 * t56 * t58 - t6 * t62 + 0.6D1 
     #* t64 * t65 * t10 + 0.3D1 * t64 * t69 + t64 * t72 * t42 * x3 + t64
     # * t72 * t12 - 0.6D1 * t79 * t80 * t81 - 0.3D1 * t87 * t91 * t40 +
     # 0.4D1 * t79 * t80 * t40 * t52 + 0.6D1 * t99 * t100 * z * x3 + 0.3
     #D1 * t87 * t91 * t81 + 0.3D1 * t99 * t58 + 0.3D1 * t110 * t112 + 0
     #.3D1 * t115 * t62
      t121 = t85 * t20
      t133 = t64 * t3
      t178 = 0.3D1 * t115 * t48 - 0.6D1 * t121 * t41 * t38 + t16 * t86 *
     # x1 / t88 / t18 * t90 * t34 * t40 + 0.4D1 * t133 * t38 * t52 + 0.4
     #D1 * t133 * t53 - 0.4D1 * t64 * t52 - t64 + 0.3D1 * t79 * t80 * t4
     #0 - 0.6D1 * t99 * t100 * x3 - 0.12D2 * t133 * t51 * z - 0.6D1 * t1
     #10 * t69 + 0.3D1 * t16 * t86 * t89 * t90 * t40 * t38 - 0.3D1 * t56
     # * t8 * t10 * z * x3 + 0.2D1 * t15 * t52 * z * t40 + 0.3D1 * t110 
     #+ 0.6D1 * t121 * t41 * z * t7 * x3 - 0.3D1 * t64 * t112 - 0.6D1 * 
     #t64 * t111 * t11
      bggbH51J1 = -0.16D2 / 0.3D1 * wd * (t118 + t178) / t40 / s

      end function
  
   
 

      doubleprecision function bggbH51J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t20 = t4 * t8 + x2 * t10 - 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = s ** 2
      t28 = t27 * s
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t29 * t30
      t32 = t23 ** 2
      t33 = t32 * t10
      t38 = t27 ** 2
      t39 = t38 * t30
      t44 = t29 * z
      t45 = t30 * t1
      t46 = t28 * t45
      t47 = x1 ** 2
      t48 = t4 ** 2
      t49 = 0.1D1 / t48
      t52 = t20 ** 2
      t57 = t1 * t23
      t61 = t57 * x3
      t64 = t32 * t23
      t65 = t45 * t64
      t66 = x3 ** 2
      t70 = t10 ** 2
      t71 = t70 * t10
      t74 = t30 * t32
      t75 = t74 * t66
      t82 = t28 * t1 * x1
      t83 = t5 * t20
      t84 = t26 * z
      t87 = t28 * t30
      t88 = t87 * t47
      t89 = t49 * t52
      t93 = t20 * t26
      t98 = t87 * t6
      t107 = -0.2D1 * t31 * t33 * z * x3 + t39 * t32 * t10 * z * x3 - t4
     #4 - 0.3D1 * t46 * t47 * t49 * t52 * t26 * t24 - 0.6D1 * t29 * t57 
     #* t10 - 0.3D1 * t29 * t61 - t29 * t65 * t66 * x3 - t29 * t65 * t71
     # + 0.3D1 * t29 * t75 + 0.6D1 * t29 * t74 * t70 + t82 * t83 * t84 -
     # t88 * t89 * t84 - 0.3D1 * t46 * t6 * t93 * t32 * t66 - 0.2D1 * t9
     #8 * t93 * z * t23 * x3 + 0.5D1 * t98 * t93 * t24
      t108 = t38 * t45
      t110 = t64 * t10 * t66
      t113 = t32 * t70 * z
      t116 = t64 * t70 * x3
      t128 = t29 * t45
      t151 = t29 - t108 * t110 - t39 * t113 + t108 * t116 - t46 * t47 * 
     #x1 / t48 / t4 * t52 * t20 * t26 - t31 * t113 - t44 * t75 - 0.3D1 *
     # t128 * t116 - 0.3D1 * t128 * t110 + 0.2D1 * t88 * t89 * t26 - 0.2
     #D1 * t82 * t83 * t26 + 0.6D1 * t31 * t33 * x3 + 0.4D1 * t29 * t1 *
     # t23 * t10 * z + 0.2D1 * t44 * t61 - t108 * t64 * t71
      bggbH51J2 = -0.16D2 / 0.3D1 * wd * (t107 + t151) / t26 / s

      end function
  
   
 

      doubleprecision function bggbH51J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t7 = z + x1 * t3
      t8 = 0.1D1 / t7
      t9 = x1 * t8
      t10 = t5 * t9
      t12 = x3 * (0.1D1 - x2)
      t14 = 0.1D1 - x3
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t24 = t12 * t7 + x2 * t14 - 0.2D1 * t17 * t21
      t25 = s * t3
      t28 = 0.1D1 - x1
      t29 = t28 * x3
      t31 = s - t25 * t9 * t24 - t25 * t29
      t32 = t24 * t31
      t37 = t31 * t2
      t38 = t37 * z
      t40 = t3 * t28 * x3
      t42 = t4 * t3
      t43 = t37 * t42
      t44 = t28 ** 2
      t45 = t44 * t28
      t46 = t14 ** 2
      t48 = t45 * t46 * x3
      t50 = t1 ** 2
      t51 = t50 * t42
      t53 = x3 ** 2
      t54 = t45 * t14 * t53
      t66 = t2 * t42
      t76 = x1 ** 2
      t77 = t7 ** 2
      t78 = 0.1D1 / t77
      t81 = t24 ** 2
      t93 = t2 * t3 * x1
      t94 = t8 * t24
      t101 = t4 * t44 * t53
      t109 = -t10 * t32 * z * t28 * x3 + t38 * t40 - t43 * t48 - t51 * t
     #54 - t37 * t4 * t44 * t14 * z * x3 + t50 * t4 * t44 * t14 * z * x3
     # - 0.2D1 * t66 * t9 * t32 * t44 * t53 - t37 * t42 * t45 * t53 * x3
     # - t66 * t76 * t78 * t81 * t31 * t29 + t51 * t48 + t5 * t76 * t78 
     #* t81 * t31 - 0.2D1 * t43 * t54 + t93 * t94 * t31 * z - t93 * t94 
     #* t31 - t38 * t101 - t37 * t40 + 0.2D1 * t37 * t101 + 0.3D1 * t10 
     #* t32 * t29
      bggbH51J3 = -0.16D2 / 0.3D1 * wd * t109 / t31 / s

      end function
  
   
 

      doubleprecision function bggbH52J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * t6 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t32 = t31 * t1
      t34 = x1 ** 2
      t36 = t4 ** 2
      t42 = t14 * t4 + x2 * t7 - t20
      t43 = t21 ** 2
      t50 = t5 * t42
      t51 = t24 ** 2
      t52 = x3 ** 2
      t54 = t50 * t51 * t52
      t57 = t28 ** 2
      t63 = 0.1D1 / t36
      t69 = t57 * t1
      t70 = t69 * x1
      t71 = z ** 2
      t75 = t57 * t31
      t77 = t42 * z
      t78 = t24 * x3
      t83 = t30 * t31 * x1
      t84 = t50 * t78
      t87 = t30 * t1
      t88 = t6 * t42
      t93 = t63 * t42
      t124 = 0.9D1 * t30 * t32 * t34 * x1 / t36 / t4 * t42 * t43 + 0.9D1
     # * t30 * t32 * x1 * t54 + 0.9D1 * t57 * t32 * x1 * t54 - 0.18D2 * 
     #t30 * t31 * t34 * t63 * t42 * t21 + 0.18D2 * t70 * t50 * t71 + 0.1
     #8D2 * t75 * t6 * t77 * t78 - 0.18D2 * t83 * t84 + 0.9D1 * t87 * t8
     #8 + 0.18D2 * t30 * t32 * t34 * t93 * t78 * t21 - 0.18D2 * t87 * t6
     # * t77 + 0.9D1 * t69 * t88 - 0.18D2 * t70 * t50 * z + 0.18D2 * t87
     # * t6 * t42 * t71 - 0.18D2 * t75 * x1 * t84 + 0.18D2 * t30 * t31 *
     # t34 * t93 * z * t21 + 0.18D2 * t83 * t50 * z * t24 * x3
      bggbH52J1 = -0.16D2 / 0.3D1 * wd * t124 / t27 / s

      end function
  
   
 

      doubleprecision function bggbH52J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t24 = 0.1D1 - x1
      t27 = s - t2 * t6 * t21 - t2 * t24 * t7
      t28 = s ** 2
      t30 = t27 * t28 * s
      t31 = t1 ** 2
      t36 = t14 * t4 + x2 * t7 - t20
      t37 = t5 * t36
      t38 = t24 * x3
      t39 = t37 * t38
      t42 = t31 * t1
      t45 = t24 ** 2
      t46 = x3 ** 2
      t48 = t37 * t45 * t46
      t51 = t28 ** 2
      t56 = x1 ** 2
      t59 = t4 ** 2
      t60 = 0.1D1 / t59
      t67 = t6 * t36
      t84 = t21 ** 2
      bggbH52J2 = -0.16D2 / 0.3D1 * wd * (0.9D1 * t30 * t31 * x1 * t39 -
     # 0.9D1 * t30 * t42 * x1 * t48 - 0.9D1 * t51 * t42 * x1 * t48 - 0.2
     #7D2 * t30 * t42 * t56 * t60 * t36 * t38 * t21 - 0.9D1 * t51 * t1 *
     # t67 - 0.9D1 * t30 * t1 * t67 + 0.18D2 * t30 * t31 * t56 * t60 * t
     #36 * t21 - 0.9D1 * t30 * t42 * t56 * x1 / t59 / t4 * t36 * t84 + 0
     #.27D2 * t51 * t31 * x1 * t39) / t27 / s

      end function
  
   
 

      doubleprecision function bggbH53J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 * t3
      t6 = t2 * t5
      t7 = 0.1D1 - x1
      t8 = t7 ** 2
      t9 = 0.1D1 - x3
      t10 = t9 ** 2
      t11 = t8 * t10
      t13 = s * t3
      t14 = x1 * t3
      t15 = z + t14
      t16 = 0.1D1 / t15
      t17 = x1 * t16
      t18 = 0.1D1 - x2
      t23 = cos(x4 * 0.3141592653589793D1)
      t24 = x3 * t18
      t28 = Sqrt(t24 * t15 * x2 * t9)
      t30 = 0.2D1 * t23 * t28
      t31 = t9 * t18 * t15 + x2 * x3 + t30
      t32 = t17 * t31
      t34 = t7 * t9
      t36 = s - t13 * t32 - t13 * t34
      t38 = t16 * t31
      t39 = t36 * x1 * t38
      t42 = t36 * t2
      t43 = t42 * t5
      t44 = x1 ** 2
      t45 = t44 * x1
      t46 = t15 ** 2
      t48 = 0.1D1 / t46 / t15
      t49 = t45 * t48
      t52 = t24 * t15 + x2 * t9 - t30
      t53 = t52 ** 2
      t58 = t42 * t3
      t59 = z ** 2
      t64 = t42 * z
      t76 = t1 ** 2
      t78 = t76 * t4 * t44
      t79 = 0.1D1 / t46
      t82 = t79 * t52 * z * t31
      t85 = t42 * t4
      t86 = t44 * t79
      t91 = t4 * t44
      t92 = t31 ** 2
      t93 = t79 * t92
      t120 = 0.3D1 * t6 * t11 * t39 + 0.3D1 * t43 * t49 * t53 * t31 + 0.
     #4D1 * t58 * t17 * t31 * t59 - 0.6D1 * t64 * t14 * t38 + 0.4D1 * t5
     #8 * t17 * t52 * t59 - 0.12D2 * t58 * t17 * t52 * z - 0.3D1 * t78 *
     # t82 + 0.3D1 * t85 * t86 * t53 * z + 0.3D1 * t64 * t91 * t93 - 0.6
     #D1 * t85 * t86 * t52 * t31 + 0.3D1 * t43 * t49 * t52 * t92 + 0.3D1
     # * t64 + 0.2D1 * t42 * t59 * z - 0.4D1 * t42 * t59 - t42 + 0.6D1 *
     # t42 * t91 * t82 - 0.3D1 * t85 * t86 * t92 + 0.6D1 * t58 * t17 * t
     #52
      t121 = t76 * t5
      t122 = t121 * t45
      t126 = t2 * t3
      t131 = t49 * t53 * t52
      t133 = t2 * t4
      t156 = t126 * t7
      t157 = t9 * t36
      t175 = t133 * t34
      t189 = t122 * t48 * t52 * t92 + 0.3D1 * t126 * t34 * t36 + t121 * 
     #t131 - 0.3D1 * t133 * t11 * t36 + 0.3D1 * t6 * t34 * t36 * t44 * t
     #93 + t6 * t8 * t7 * t10 * t9 * t36 - t122 * t48 * t53 * t31 + 0.4D
     #1 * t76 * t3 * x1 * t16 * t52 * t59 - 0.6D1 * t156 * t157 * z + 0.
     #3D1 * t58 * t32 + 0.4D1 * t156 * t157 * t59 + 0.3D1 * t78 * t79 * 
     #t53 * z + 0.3D1 * t133 * t8 * t10 * t36 * z + 0.6D1 * t175 * t36 *
     # z * t32 + t43 * t131 - 0.6D1 * t85 * t86 * t53 - 0.6D1 * t175 * t
     #39 + t43 * t49 * t92 * t31
      bggbH53J1 = -0.16D2 / 0.3D1 * wd * (t120 + t189) / t36 / s

      end function
  
   
 

      doubleprecision function bggbH53J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - x1
      t7 = t6 ** 2
      t9 = 0.1D1 - x3
      t10 = t9 ** 2
      t11 = s * t3
      t12 = x1 * t3
      t13 = z + t12
      t14 = 0.1D1 / t13
      t15 = x1 * t14
      t16 = 0.1D1 - x2
      t21 = cos(x4 * 0.3141592653589793D1)
      t22 = x3 * t16
      t26 = Sqrt(t22 * t13 * x2 * t9)
      t28 = 0.2D1 * t21 * t26
      t29 = t9 * t16 * t13 + x2 * x3 + t28
      t30 = t15 * t29
      t32 = t6 * t9
      t34 = s - t11 * t30 - t11 * t32
      t38 = t4 * t3
      t39 = t2 * t38
      t41 = x1 ** 2
      t43 = t13 ** 2
      t44 = 0.1D1 / t43
      t45 = t29 ** 2
      t46 = t44 * t45
      t50 = t7 * t10
      t53 = t14 * t29
      t54 = t34 * x1 * t53
      t57 = t34 * t2
      t58 = t57 * t38
      t59 = t41 * x1
      t61 = 0.1D1 / t43 / t13
      t62 = t59 * t61
      t65 = t22 * t13 + x2 * t9 - t28
      t66 = t65 ** 2
      t68 = t62 * t66 * t65
      t70 = t57 * t4
      t71 = t41 * t44
      t75 = t57 * z
      t79 = t57 * t3
      t84 = t1 ** 2
      t86 = t84 * t4 * t41
      t89 = t44 * t65 * z * t29
      t94 = t4 * t41
      t110 = t84 * t38
      t115 = -t5 * t7 * t10 * t34 * z - 0.3D1 * t39 * t32 * t34 * t41 * 
     #t46 - 0.3D1 * t39 * t50 * t54 - t58 * t68 + 0.6D1 * t70 * t71 * t6
     #6 + 0.2D1 * t75 * t12 * t53 + 0.4D1 * t79 * t15 * t65 * z + t86 * 
     #t89 - t70 * t71 * t66 * z - t75 * t94 * t46 + 0.6D1 * t70 * t71 * 
     #t65 * t29 - 0.3D1 * t58 * t62 * t65 * t45 - t39 * t7 * t6 * t10 * 
     #t9 * t34 - t110 * t68 + 0.2D1 * t5 * t50 * t34
      t116 = t2 * t3
      t120 = t5 * t32
      t130 = t110 * t59
      t159 = -0.2D1 * t116 * t32 * t34 + t57 - 0.2D1 * t120 * t34 * z * 
     #t30 - 0.2D1 * t57 * t94 * t89 + 0.5D1 * t120 * t54 - t75 - t130 * 
     #t61 * t65 * t45 - 0.3D1 * t79 * t30 + t116 * t6 * t9 * t34 * z + t
     #130 * t61 * t66 * t29 + 0.3D1 * t70 * t71 * t45 - 0.6D1 * t79 * t1
     #5 * t65 - 0.3D1 * t58 * t62 * t66 * t29 - t58 * t62 * t45 * t29 - 
     #t86 * t44 * t66 * z
      bggbH53J2 = -0.16D2 / 0.3D1 * wd * (t115 + t159) / t34 / s

      end function
  
   
 

      doubleprecision function bggbH53J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = t1 * x1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x3
      t8 = 0.1D1 - x2
      t13 = cos(x4 * 0.3141592653589793D1)
      t14 = x3 * t8
      t18 = Sqrt(t14 * t4 * x2 * t7)
      t20 = 0.2D1 * t13 * t18
      t21 = t7 * t8 * t4 + x2 * x3 + t20
      t22 = x1 * t5 * t21
      t24 = 0.1D1 - x1
      t25 = t24 * t7
      t27 = s - t2 * t22 - t2 * t25
      t28 = s ** 2
      t29 = t28 * s
      t30 = t27 * t29
      t31 = t30 * z
      t32 = t1 ** 2
      t33 = x1 ** 2
      t34 = t32 * t33
      t35 = t4 ** 2
      t36 = 0.1D1 / t35
      t37 = t21 ** 2
      t38 = t36 * t37
      t41 = t28 ** 2
      t42 = t32 * t1
      t44 = t33 * x1
      t45 = t41 * t42 * t44
      t47 = 0.1D1 / t35 / t4
      t50 = t14 * t4 + x2 * t7 - t20
      t54 = t29 * t1
      t59 = t29 * t32
      t60 = t24 ** 2
      t61 = t7 ** 2
      t62 = t60 * t61
      t67 = t29 * t42
      t73 = t50 ** 2
      t77 = t59 * t25
      t79 = t5 * t21
      t80 = t27 * x1 * t79
      t89 = t36 * t50 * z * t21
      t91 = t30 * t42
      t92 = t44 * t47
      t117 = -t31 * t34 * t38 - t45 * t47 * t50 * t37 + t54 * t24 * t7 *
     # t27 * z + t59 * t62 * t27 - t54 * t25 * t27 - 0.2D1 * t67 * t25 *
     # t27 * t33 * t38 + t45 * t47 * t73 * t21 + 0.3D1 * t77 * t80 - t30
     # * t1 * t22 + t41 * t32 * t33 * t89 - t91 * t92 * t73 * t21 - 0.2D
     #1 * t91 * t92 * t50 * t37 - t30 * t34 * t89 - t91 * t92 * t37 * t2
     #1 - t77 * t27 * z * t22 - t67 * t62 * t80 + 0.2D1 * t30 * t32 * t3
     #3 * t36 * t37 + t31 * t3 * t79
      bggbH53J3 = -0.16D2 / 0.3D1 * wd * t117 / t27 / s

      end function
  
   
 

      doubleprecision function bggbH54J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t26 = s - t2 * t6 * (t8 * t4 + x2 * t10 - t19) - t2 * t23 * x3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t31 = t29 * t30
      t32 = t23 ** 2
      t33 = t32 * t10
      t45 = t29 * t1
      t46 = t23 * t10
      t47 = t46 * z
      t51 = t29 * t30 * t23
      t52 = t10 * x1
      t56 = t10 * t7 * t4 + x2 * x3 + t19
      t57 = t5 * t56
      t58 = t52 * t57
      t61 = t27 ** 2
      t62 = t61 * t30
      t71 = t30 * t1
      t78 = z ** 2
      t79 = t46 * t78
      t84 = x1 ** 2
      t86 = t4 ** 2
      t88 = t56 ** 2
      t90 = t10 * t84 / t86 * t88
      t93 = t61 * t1
      t110 = x3 ** 2
      t116 = 0.18D2 * t31 * t33 * z * x3 + 0.9D1 * t29 * t1 * t23 * t10 
     #- 0.18D2 * t31 * t33 * x3 - 0.18D2 * t45 * t47 - 0.18D2 * t51 * t5
     #8 - 0.18D2 * t62 * t23 * t58 + 0.18D2 * t51 * t10 * z * t6 * t56 +
     # 0.18D2 * t29 * t71 * t32 * t52 * t57 * x3 + 0.18D2 * t45 * t79 + 
     #0.9D1 * t29 * t71 * t23 * t90 + 0.9D1 * t93 * t46 + 0.18D2 * t62 *
     # t46 * z * x1 * t57 + 0.18D2 * t93 * t79 + 0.9D1 * t61 * t71 * t23
     # * t90 + 0.9D1 * t29 * t71 * t32 * t23 * t10 * t110 - 0.18D2 * t93
     # * t47
      bggbH54J1 = -0.16D2 / 0.3D1 * wd * t116 / t26 / s

      end function
  
   
 

      doubleprecision function bggbH54J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + t1 * x1
      t5 = 0.1D1 / t4
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t23 = 0.1D1 - x1
      t26 = s - t2 * x1 * t5 * (t8 * t4 + x2 * t10 - t19) - t2 * t23 * x
     #3
      t27 = s ** 2
      t29 = t26 * t27 * s
      t30 = t1 ** 2
      t33 = t10 * x1
      t37 = t10 * t7 * t4 + x2 * x3 + t19
      t38 = t5 * t37
      t39 = t33 * t38
      t43 = t23 ** 2
      t48 = t27 ** 2
      t53 = t30 * t1
      t57 = x3 ** 2
      t67 = x1 ** 2
      t69 = t4 ** 2
      t71 = t37 ** 2
      t73 = t10 * t67 / t69 * t71
      bggbH54J2 = -0.16D2 / 0.3D1 * wd * (0.9D1 * t29 * t30 * t23 * t39 
     #+ 0.18D2 * t29 * t30 * t43 * t10 * x3 + 0.27D2 * t48 * t30 * t23 *
     # t39 - 0.9D1 * t29 * t53 * t43 * t23 * t10 * t57 - 0.9D1 * t48 * t
     #1 * t23 * t10 - 0.9D1 * t48 * t53 * t23 * t73 - 0.9D1 * t29 * t1 *
     # t23 * t10 - 0.9D1 * t29 * t53 * t23 * t73 - 0.27D2 * t29 * t53 * 
     #t43 * t33 * t38 * x3) / t26 / s

      end function
  
 