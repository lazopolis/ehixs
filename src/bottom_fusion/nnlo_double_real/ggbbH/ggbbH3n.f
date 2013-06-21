  
      subroutine ggbbH3n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision ggbbH31J1  
      doubleprecision ggbbH31J2  
      doubleprecision ggbbH31J3  
      doubleprecision ggbbH32J1  
      doubleprecision ggbbH32J2  
      doubleprecision ggbbH32J3  
      doubleprecision ggbbH3n1e1  
      doubleprecision ggbbH3n1e0  
      doubleprecision ggbbH3n1em1  
      doubleprecision ggbbH3n1em2  
      doubleprecision ggbbH3n1em3  
      doubleprecision ggbbH3n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=ggbbH3n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=ggbbH3n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=ggbbH3n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=ggbbH3n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=ggbbH3n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=ggbbH3n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function ggbbH3n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH31J1
      doubleprecision ggbbH31J2
      doubleprecision ggbbH31J3
      doubleprecision ggbbH32J1
      doubleprecision ggbbH32J2
      doubleprecision ggbbH32J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = x2 * 0.3141592653589793D1
      t17 = sin(t16)
      t18 = t17 ** 2
      t19 = z ** 2
      t20 = 0.1D1 / t19
      t21 = t18 * t20
      t22 = x1 ** 2
      t23 = t6 ** 2
      t24 = t22 * t23
      t25 = t9 ** 2
      t26 = t24 * t25
      t29 = log(0.4D1 * t21 * t26)
      t33 = 0.1D1 / s
      t34 = (-0.180D3 * lh - 0.90D2 * t29) * t11 * t33
      t36 = 0.1D1 / (-0.2D1 + t1)
      t37 = t9 * t36
      t38 = ggbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t39 = t37 * t38
      t41 = lh ** 2
      t42 = 0.180D3 * t41
      t43 = 0.3141592653589793D1 ** 2
      t44 = 0.30D2 * t43
      t47 = t29 ** 2
      t51 = (t42 - t44 + 0.180D3 * t29 * lh + 0.45D2 * t47) * t11 * t33
      t52 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t53 = t37 * t52
      t59 = t42 - t44
      t67 = (-0.2884936567583026D3 - 0.120D3 * t41 * lh + 0.60D2 * lh * 
     #t43 - t29 * t59 - 0.90D2 * t47 * lh - 0.15D2 * t47 * t29) * t11 * 
     #t33
      t68 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t69 = t37 * t68
      t71 = lh * t11
      t72 = t21 * t22
      t73 = t23 * x4
      t77 = log(0.4D1 * t72 * t73 * t25)
      t78 = t77 * t9
      t79 = t36 * t68
      t85 = t11 * t33
      t86 = t77 ** 2
      t87 = t86 * t9
      t90 = t36 * t52
      t96 = t59 * t11 * t33
      t97 = t96 * t69
      t99 = 0.1D1 / x4
      t101 = x3 * t18
      t102 = t101 * t20
      t107 = log(0.4D1 * t102 * t24 * x4 * t25)
      t108 = t107 * t9
      t113 = t71 * t33
      t117 = 0.1D1 / x3
      t122 = log(0.4D1 * t102 * t26)
      t123 = t122 * t9
      t130 = t122 ** 2
      t131 = t130 * t9
      t140 = -t34 * t39 / 0.2880D4 - t51 * t53 / 0.2880D4 - t67 * t69 / 
     #0.2880D4 + (-0.180D3 * t71 * t33 * (-t53 + t78 * t79) + 0.90D2 * t
     #85 * (-t39 - t87 * t79 / 0.2D1 + t78 * t90) - t97) * t99 / 0.2880D
     #4 - (0.90D2 * t85 * (-t108 * t79 + t53) - 0.180D3 * t113 * t69) * 
     #t117 * t99 / 0.2880D4 + (-0.180D3 * t71 * t33 * (t123 * t79 - t53)
     # + 0.90D2 * t85 * (-t39 + t123 * t90 - t131 * t79 / 0.2D1) - t97) 
     #* t117 / 0.2880D4
      t141 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t140)
      t143 = 0.1D1 - x4
      t144 = KAPPA2(x1, x2, 0.0D0, t143, z)
      t145 = s * t144
      t146 = t145 * t4
      t147 = -t143
      t148 = t7 * t147
      t149 = t145 * t148
      t150 = t7 * x4
      t151 = t145 * t150
      t152 = t144 ** 2
      t155 = x1 * t6
      t157 = s * t152 * t11 * t155 * t147
      t158 = t152 ** 2
      t160 = t73 * t147 * t158
      t163 = log(-0.4D1 * t72 * t160)
      t164 = t163 * t152
      t166 = 0.1D1 / (-0.2D1 + t144)
      t167 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t143)
      t168 = t166 * t167
      t170 = t152 * t166
      t171 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t143)
      t172 = t170 * t171
      t177 = ggbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t143)
      t181 = t163 ** 2
      t182 = t181 * t152
      t188 = t170 * t167
      t193 = t101 * t20 * t22
      t196 = log(-0.4D1 * t193 * t160)
      t197 = t196 * t152
      t208 = (-0.180D3 * t71 * t33 * (-t164 * t168 + t172) + 0.90D2 * t8
     #5 * (t170 * t177 - t164 * t166 * t171 + t182 * t168 / 0.2D1) + t96
     # * t188) * t99 / 0.2880D4 - (0.90D2 * t85 * (t197 * t168 - t172) +
     # 0.180D3 * t113 * t188) * t117 * t99 / 0.2880D4
      t209 = FJET(XB1, XB2, s, 0.0D0, t146, t149, -t151, t157, t208)
      t211 = ggbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t212 = t37 * t211
      t214 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t215 = t37 * t214
      t217 = t36 * t214
      t219 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t220 = t37 * t219
      t227 = t36 * t219
      t232 = t96 * t215
      t259 = -t34 * t212 / 0.2880D4 - t67 * t215 / 0.2880D4 + (-0.180D3 
     #* t71 * t33 * (t78 * t217 - t220) + 0.90D2 * t85 * (-t212 - t87 * 
     #t217 / 0.2D1 + t78 * t227) - t232) * t99 / 0.2880D4 - t51 * t220 /
     # 0.2880D4 - (0.90D2 * t85 * (t220 - t108 * t217) - 0.180D3 * t113 
     #* t215) * t117 * t99 / 0.2880D4 + (-0.180D3 * t71 * t33 * (-t220 +
     # t123 * t217) + 0.90D2 * t85 * (-t212 + t123 * t227 - t131 * t217 
     #/ 0.2D1) - t232) * t117 / 0.2880D4
      t260 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t259)
      t262 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t143)
      t263 = t166 * t262
      t265 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t143)
      t266 = t170 * t265
      t270 = t170 * t262
      t281 = ggbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t143)
      t294 = -(0.90D2 * t85 * (t197 * t263 - t266) + 0.180D3 * t113 * t2
     #70) * t117 * t99 / 0.2880D4 + (-0.180D3 * t71 * t33 * (t266 - t164
     # * t263) + 0.90D2 * t85 * (t170 * t281 - t164 * t166 * t265 + t182
     # * t263 / 0.2D1) + t96 * t270) * t99 / 0.2880D4
      t295 = FJET(XB1, XB2, s, t146, 0.0D0, -t151, t149, t157, t294)
      t297 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t298 = s * t297
      t299 = t4 * x3
      t300 = t298 * t299
      t301 = -0.1D1 + x3
      t302 = t4 * t301
      t303 = t298 * t302
      t304 = t298 * t7
      t305 = t297 ** 2
      t309 = s * t305 * t11 * t155 * t301
      t311 = 0.1D1 / (-0.2D1 + t297)
      t312 = t305 * t311
      t313 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t314 = t312 * t313
      t315 = t23 * t301
      t316 = t305 ** 2
      t321 = log(-0.4D1 * t193 * t315 * x4 * t316)
      t322 = t321 * t305
      t323 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t324 = t311 * t323
      t329 = t312 * t323
      t339 = log(-0.4D1 * t102 * t24 * t301 * t316)
      t340 = t339 * t305
      t346 = ggbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t350 = t339 ** 2
      t351 = t350 * t305
      t361 = -(0.90D2 * t85 * (-t314 + t322 * t324) + 0.180D3 * t113 * t
     #329) * t117 * t99 / 0.2880D4 + (-0.180D3 * t71 * t33 * (-t340 * t3
     #24 + t314) + 0.90D2 * t85 * (t312 * t346 - t340 * t311 * t313 + t3
     #51 * t324 / 0.2D1) + t96 * t329) * t117 / 0.2880D4
      t362 = FJET(XB1, XB2, s, t300, -t303, -t304, 0.0D0, t309, t361)
      t364 = KAPPA2(x1, x2, x3, t143, z)
      t365 = s * t364
      t366 = t365 * t299
      t367 = t365 * t302
      t368 = t365 * t148
      t369 = t365 * t150
      t370 = t364 ** 2
      t375 = cos(t16)
      t377 = x4 * t147
      t379 = sqrt(x3 * t301 * t377)
      t384 = s * t370 * t11 * t155 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t375 * t379)
      t385 = t370 ** 2
      t390 = log(0.4D1 * t193 * t315 * t377 * t385)
      t391 = t390 * t370
      t393 = 0.1D1 / (-0.2D1 + t364)
      t394 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t143)
      t397 = t370 * t393
      t398 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, t143)
      t406 = 0.90D2 * t85 * (-t391 * t393 * t394 + t397 * t398) - 0.180D
     #3 * t113 * t397 * t394
      t410 = FJET(XB1, XB2, s, t366, -t367, t368, -t369, t384, -t406 * t
     #117 * t99 / 0.2880D4)
      t412 = t117 * t99
      t415 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t416 = t311 * t415
      t418 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t419 = t312 * t418
      t423 = t312 * t415
      t434 = ggbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t447 = -(0.90D2 * t85 * (t322 * t416 - t419) + 0.180D3 * t113 * t4
     #23) * t117 * t99 / 0.2880D4 + (-0.180D3 * t71 * t33 * (-t340 * t41
     #6 + t419) + 0.90D2 * t85 * (t312 * t434 - t340 * t311 * t418 + t35
     #1 * t416 / 0.2D1) + t96 * t423) * t117 / 0.2880D4
      t448 = FJET(XB1, XB2, s, -t303, t300, 0.0D0, -t304, t309, t447)
      t450 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, t143)
      t452 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t143)
      t461 = 0.90D2 * t85 * (t397 * t450 - t391 * t393 * t452) - 0.180D3
     # * t113 * t397 * t452
      t465 = FJET(XB1, XB2, s, -t367, t366, -t369, t368, t384, -t461 * t
     #117 * t99 / 0.2880D4)
      ggbbH3n1e1 = t141 * t140 + t209 * t208 + t260 * t259 + t295 * t294
     # + t362 * t361 - t410 * t406 * t412 / 0.2880D4 + t448 * t447 - t46
     #5 * t461 * t412 / 0.2880D4

      end function



      doubleprecision function ggbbH3n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH31J1
      doubleprecision ggbbH31J2
      doubleprecision ggbbH31J3
      doubleprecision ggbbH32J1
      doubleprecision ggbbH32J2
      doubleprecision ggbbH32J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = 0.1D1 / s
      t16 = t11 * t15
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t21 = t19 * t20
      t22 = x2 * 0.3141592653589793D1
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = x1 ** 2
      t29 = t27 * t28
      t30 = t6 ** 2
      t31 = t30 * x4
      t32 = t9 ** 2
      t36 = log(0.4D1 * t29 * t31 * t32)
      t37 = t9 * t36
      t38 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t39 = t18 * t38
      t45 = lh * t11 * t15
      t46 = t19 * t38
      t48 = 0.180D3 * t45 * t46
      t50 = 0.1D1 / x4
      t53 = t16 * t9
      t54 = 0.1D1 / x3
      t55 = t54 * t50
      t60 = x3 * t24 * t26
      t61 = t28 * t30
      t62 = t61 * t32
      t65 = log(0.4D1 * t60 * t62)
      t66 = t65 * t9
      t74 = ggbbH31J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t81 = log(0.4D1 * t27 * t62)
      t85 = (-0.180D3 * lh - 0.90D2 * t81) * t11 * t15
      t88 = lh ** 2
      t90 = 0.3141592653589793D1 ** 2
      t94 = t81 ** 2
      t98 = (0.180D3 * t88 - 0.30D2 * t90 + 0.180D3 * t81 * lh + 0.45D2 
     #* t94) * t11 * t15
      t101 = (0.90D2 * t16 * (-t21 + t37 * t39) + t48) * t50 / 0.2880D4 
     #- t53 * t39 * t55 / 0.32D2 + (0.90D2 * t16 * (t66 * t39 - t21) + t
     #48) * t54 / 0.2880D4 - t16 * t19 * t74 / 0.32D2 - t85 * t21 / 0.28
     #80D4 - t98 * t46 / 0.2880D4
      t102 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t101)
      t104 = 0.1D1 - x4
      t105 = KAPPA2(x1, x2, 0.0D0, t104, z)
      t106 = s * t105
      t107 = t106 * t4
      t108 = -t104
      t109 = t7 * t108
      t110 = t106 * t109
      t111 = t7 * x4
      t112 = t106 * t111
      t113 = t105 ** 2
      t116 = x1 * t6
      t118 = s * t113 * t11 * t116 * t108
      t119 = t16 * t113
      t121 = 0.1D1 / (-0.2D1 + t105)
      t122 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t104)
      t123 = t121 * t122
      t127 = t113 ** 2
      t132 = log(-0.4D1 * t29 * t31 * t108 * t127)
      t133 = t132 * t113
      t135 = t113 * t121
      t136 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t104)
      t147 = t119 * t123 * t55 / 0.32D2 + (0.90D2 * t16 * (-t133 * t123 
     #+ t135 * t136) - 0.180D3 * t45 * t135 * t122) * t50 / 0.2880D4
      t148 = FJET(XB1, XB2, s, 0.0D0, t107, t110, -t112, t118, t147)
      t150 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t151 = t18 * t150
      t153 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t154 = t19 * t153
      t158 = t19 * t150
      t160 = 0.180D3 * t45 * t158
      t176 = ggbbH32J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t182 = (0.90D2 * t16 * (t37 * t151 - t154) + t160) * t50 / 0.2880D
     #4 - t85 * t154 / 0.2880D4 - t53 * t151 * t55 / 0.32D2 + (0.90D2 * 
     #t16 * (-t154 + t66 * t151) + t160) * t54 / 0.2880D4 - t16 * t19 * 
     #t176 / 0.32D2 - t98 * t158 / 0.2880D4
      t183 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t182)
      t185 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t104)
      t187 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t104)
      t188 = t121 * t187
      t202 = (0.90D2 * t16 * (t135 * t185 - t133 * t188) - 0.180D3 * t45
     # * t135 * t187) * t50 / 0.2880D4 + t119 * t188 * t55 / 0.32D2
      t203 = FJET(XB1, XB2, s, t107, 0.0D0, -t112, t110, t118, t202)
      t205 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t206 = s * t205
      t207 = t4 * x3
      t208 = t206 * t207
      t209 = -0.1D1 + x3
      t210 = t4 * t209
      t211 = t206 * t210
      t212 = t206 * t7
      t213 = t205 ** 2
      t217 = s * t213 * t11 * t116 * t209
      t218 = t16 * t213
      t220 = 0.1D1 / (-0.2D1 + t205)
      t221 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t222 = t220 * t221
      t226 = t213 ** 2
      t231 = log(-0.4D1 * t60 * t61 * t209 * t226)
      t232 = t231 * t213
      t234 = t213 * t220
      t235 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t246 = t218 * t222 * t55 / 0.32D2 + (0.90D2 * t16 * (-t232 * t222 
     #+ t234 * t235) - 0.180D3 * t45 * t234 * t221) * t54 / 0.2880D4
      t247 = FJET(XB1, XB2, s, t208, -t211, -t212, 0.0D0, t217, t246)
      t249 = KAPPA2(x1, x2, x3, t104, z)
      t250 = s * t249
      t251 = t250 * t207
      t252 = t250 * t210
      t253 = t250 * t109
      t254 = t250 * t111
      t255 = t249 ** 2
      t260 = cos(t22)
      t264 = sqrt(x3 * t209 * x4 * t108)
      t269 = s * t255 * t11 * t116 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t260 * t264)
      t270 = t16 * t255
      t272 = 0.1D1 / (-0.2D1 + t249)
      t273 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t104)
      t275 = t272 * t273 * t55
      t278 = FJET(XB1, XB2, s, t251, -t252, t253, -t254, t269, -t270 * t
     #275 / 0.32D2)
      t280 = t15 * t255
      t284 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t285 = t220 * t284
      t290 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t301 = t218 * t285 * t55 / 0.32D2 + (0.90D2 * t16 * (-t232 * t285 
     #+ t234 * t290) - 0.180D3 * t45 * t234 * t284) * t54 / 0.2880D4
      t302 = FJET(XB1, XB2, s, -t211, t208, 0.0D0, -t212, t217, t301)
      t304 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, t104)
      t306 = t272 * t304 * t55
      t309 = FJET(XB1, XB2, s, -t252, t251, -t254, t253, t269, -t270 * t
     #306 / 0.32D2)
      ggbbH3n1e0 = t102 * t101 + t148 * t147 + t183 * t182 + t203 * t202
     # + t247 * t246 - t278 * t11 * t280 * t275 / 0.32D2 + t302 * t301 -
     # t309 * t11 * t280 * t306 / 0.32D2

      end function



      doubleprecision function ggbbH3n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH31J1
      doubleprecision ggbbH31J2
      doubleprecision ggbbH31J3
      doubleprecision ggbbH32J1
      doubleprecision ggbbH32J2
      doubleprecision ggbbH32J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = 0.1D1 / s
      t16 = t11 * t15
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = ggbbH31J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t26 = sin(x2 * 0.3141592653589793D1)
      t27 = t26 ** 2
      t28 = z ** 2
      t31 = x1 ** 2
      t32 = t6 ** 2
      t34 = t9 ** 2
      t38 = log(0.4D1 * t27 / t28 * t31 * t32 * t34)
      t42 = (-0.180D3 * lh - 0.90D2 * t38) * t11 * t15
      t43 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t47 = t16 * t9
      t48 = t18 * t43
      t49 = 0.1D1 / x4
      t53 = 0.1D1 / x3
      t57 = -t16 * t19 * t20 / 0.32D2 - t42 * t19 * t43 / 0.2880D4 - t47
     # * t48 * t49 / 0.32D2 - t47 * t48 * t53 / 0.32D2
      t58 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t57)
      t60 = 0.1D1 - x4
      t61 = KAPPA2(x1, x2, 0.0D0, t60, z)
      t62 = s * t61
      t63 = t62 * t4
      t64 = -t60
      t66 = t62 * t7 * t64
      t68 = t62 * t7 * x4
      t69 = t61 ** 2
      t72 = x1 * t6
      t74 = s * t69 * t11 * t72 * t64
      t75 = t16 * t69
      t77 = 0.1D1 / (-0.2D1 + t61)
      t78 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t60)
      t83 = FJET(XB1, XB2, s, 0.0D0, t63, t66, -t68, t74, t75 * t77 * t7
     #8 * t49 / 0.32D2)
      t86 = t69 * t77
      t91 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t95 = t18 * t91
      t99 = ggbbH32J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t106 = -t42 * t19 * t91 / 0.2880D4 - t47 * t95 * t49 / 0.32D2 - t1
     #6 * t19 * t99 / 0.32D2 - t47 * t95 * t53 / 0.32D2
      t107 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t106)
      t109 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, t60)
      t114 = FJET(XB1, XB2, s, t63, 0.0D0, -t68, t66, t74, t75 * t77 * t
     #109 * t49 / 0.32D2)
      t121 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t122 = s * t121
      t124 = t122 * t4 * x3
      t125 = -0.1D1 + x3
      t127 = t122 * t4 * t125
      t128 = t122 * t7
      t129 = t121 ** 2
      t133 = s * t129 * t11 * t72 * t125
      t134 = t16 * t129
      t136 = 0.1D1 / (-0.2D1 + t121)
      t137 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t142 = FJET(XB1, XB2, s, t124, -t127, -t128, 0.0D0, t133, t134 * t
     #136 * t137 * t53 / 0.32D2)
      t145 = t129 * t136
      t150 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, 0.10D1)
      t155 = FJET(XB1, XB2, s, -t127, t124, 0.0D0, -t128, t133, t134 * t
     #136 * t150 * t53 / 0.32D2)
      ggbbH3n1em1 = t58 * t57 + t83 * t11 * t15 * t86 * t78 * t49 / 0.32
     #D2 + t107 * t106 + t114 * t11 * t15 * t86 * t109 * t49 / 0.32D2 + 
     #t142 * t11 * t15 * t145 * t137 * t53 / 0.32D2 + t155 * t11 * t15 *
     # t145 * t150 * t53 / 0.32D2

      end function



      doubleprecision function ggbbH3n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH31J1
      doubleprecision ggbbH31J2
      doubleprecision ggbbH31J3
      doubleprecision ggbbH32J1
      doubleprecision ggbbH32J2
      doubleprecision ggbbH32J3
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = 0.1D1 / s
      t16 = t11 * t15
      t19 = t9 / (-0.2D1 + t1)
      t20 = ggbbH31J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t21 = t19 * t20
      t24 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, -t16 * t21 / 
     #0.32D2)
      t28 = ggbbH32J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, 0.10D1)
      t29 = t19 * t28
      t32 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, -t16 * t29 / 
     #0.32D2)
      ggbbH3n1em2 = -t24 * t11 * t15 * t21 / 0.32D2 - t32 * t11 * t15 * 
     #t29 / 0.32D2

      end function



      doubleprecision function ggbbH3n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH31J1
      doubleprecision ggbbH31J2
      doubleprecision ggbbH31J3
      doubleprecision ggbbH32J1
      doubleprecision ggbbH32J2
      doubleprecision ggbbH32J3
      ggbbH3n1em3 = 0.0D0

      end function



      doubleprecision function ggbbH3n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH31J1
      doubleprecision ggbbH31J2
      doubleprecision ggbbH31J3
      doubleprecision ggbbH32J1
      doubleprecision ggbbH32J2
      doubleprecision ggbbH32J3
      ggbbH3n1em4 = 0.0D0

      end function
  
 

      doubleprecision function ggbbH31J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t11 = s - t2 * t5 * x4 - t2 * t5 * (0.1D1 - x4)
      t12 = s ** 2
      t13 = t11 * t12
      t14 = t3 * x1
      t17 = 0.1D1 - x3
      t20 = s - t2 * t14 * x3 - t2 * t14 * t17
      t21 = t13 * t20
      t22 = t1 * t3
      t24 = t22 * x1 * t17
      t28 = t13 * t20 * z
      t30 = t11 * t20
      t31 = z ** 2
      t35 = t1 ** 2
      t36 = t3 ** 2
      t37 = t35 * t36
      t38 = x1 ** 2
      t39 = t17 ** 2
      t45 = t22 * t4 * x4
      t60 = t12 * s
      t61 = t60 * z
      t62 = t61 * t20
      t65 = t61 * t11
      t69 = t12 ** 2
      t73 = t60 * t31
      t76 = t4 ** 2
      t77 = x4 ** 2
      t87 = 0.21D2 * t21 * t24 + 0.16D2 * t28 - 0.18D2 * t30 * t12 * t31
     # - 0.9D1 * t21 * t37 * t38 * t39 - 0.18D2 * t28 * t45 + 0.21D2 * t
     #21 * t45 - 0.18D2 * t30 * t12 * t35 * t36 * t4 * x4 * x1 * t17 - 0
     #.18D2 * t28 * t24 + 0.8D1 * t62 * t24 - 0.8D1 * t65 - 0.8D1 * t62 
     #* t45 - 0.16D2 * t69 * t31 * z + 0.16D2 * t73 * t11 - 0.9D1 * t21 
     #* t37 * t76 * t77 - 0.15D2 * t21 + 0.16D2 * t73 * t20 + 0.8D1 * t6
     #5 * t45
      ggbbH31J1 = -0.16D2 / 0.3D1 * wd * t87 / t11 / s / t20

      end function
  
   
 

      doubleprecision function ggbbH31J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t11 = s - t2 * t5 * x4 - t2 * t5 * (0.1D1 - x4)
      t12 = s ** 2
      t13 = t11 * t12
      t14 = t3 * x1
      t17 = 0.1D1 - x3
      t20 = s - t2 * t14 * x3 - t2 * t14 * t17
      t21 = t13 * t20
      t22 = t1 * t3
      t24 = t22 * t4 * x4
      t28 = t12 * s * z
      t29 = t28 * t20
      t32 = t28 * t11
      t34 = t1 ** 2
      t35 = t3 ** 2
      t36 = t34 * t35
      t37 = t4 ** 2
      t38 = x4 ** 2
      t53 = t22 * x1 * t17
      t61 = x1 ** 2
      t62 = t17 ** 2
      t70 = -0.22D2 * t21 * t24 + 0.16D2 * t29 * t24 + 0.16D2 * t32 + 0.
     #9D1 * t21 * t36 * t37 * t38 + 0.27D2 * t11 * t20 * t12 * t34 * t35
     # * t4 * x4 * x1 * t17 - 0.8D1 * t29 * t53 + 0.16D2 * t21 - 0.8D1 *
     # t32 * t24 - 0.22D2 * t21 * t53 + 0.9D1 * t21 * t36 * t61 * t62 - 
     #0.11D2 * t13 * t20 * z
      ggbbH31J2 = -0.16D2 / 0.3D1 * wd * t70 / t11 / s / t20

      end function
  
   
 

      doubleprecision function ggbbH31J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t3 = t1 * s * z
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = s * t4
      t6 = 0.1D1 - z
      t7 = x1 * t6
      t10 = 0.1D1 - x3
      t13 = s - t5 * t7 * x3 - t5 * t7 * t10
      t15 = t4 * t6
      t16 = 0.1D1 - x1
      t18 = t15 * t16 * x4
      t21 = t6 * t16
      t27 = s - t5 * t21 * x4 - t5 * t21 * (0.1D1 - x4)
      t28 = t27 * t1
      t29 = t28 * t13
      ggbbH31J3 = -0.16D2 / 0.3D1 * wd * (-0.8D1 * t3 * t13 * t18 + 0.11
     #D2 * t29 - 0.8D1 * t3 * t27 - 0.8D1 * t28 * t13 * z - 0.9D1 * t29 
     #* t18 - 0.9D1 * t29 * t15 * x1 * t10) / t27 / s / t13

      end function
  
   
 

      doubleprecision function ggbbH32J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = z ** 2
      t4 = t2 * t3
      t5 = kappa2(x1, x2, x3, x4, z)
      t6 = s * t5
      t7 = 0.1D1 - z
      t8 = 0.1D1 - x1
      t9 = t7 * t8
      t15 = s - t6 * t9 * x4 - t6 * t9 * (0.1D1 - x4)
      t18 = t15 * t1
      t19 = t7 * x1
      t22 = 0.1D1 - x3
      t25 = s - t6 * t19 * x3 - t6 * t19 * t22
      t26 = t18 * t25
      t27 = t5 * t7
      t29 = t27 * x1 * t22
      t33 = t18 * t25 * z
      t35 = t5 ** 2
      t36 = t7 ** 2
      t37 = t35 * t36
      t38 = x1 ** 2
      t39 = t22 ** 2
      t44 = t8 ** 2
      t45 = x4 ** 2
      t51 = t27 * t8 * x4
      t54 = t2 * z
      t55 = t54 * t15
      t58 = t15 * t25
      t69 = t54 * t25
      t75 = t1 ** 2
      t87 = 0.16D2 * t4 * t15 + 0.21D2 * t26 * t29 + 0.16D2 * t33 - 0.9D
     #1 * t26 * t37 * t38 * t39 - 0.9D1 * t26 * t37 * t44 * t45 - 0.18D2
     # * t33 * t51 + 0.8D1 * t55 * t51 - 0.18D2 * t58 * t1 * t35 * t36 *
     # t8 * x4 * x1 * t22 - 0.18D2 * t33 * t29 + 0.8D1 * t69 * t29 - 0.1
     #5D2 * t26 + 0.21D2 * t26 * t51 - 0.16D2 * t75 * t3 * z - 0.8D1 * t
     #69 - 0.8D1 * t55 * t29 - 0.18D2 * t58 * t1 * t3 + 0.16D2 * t4 * t2
     #5
      ggbbH32J1 = -0.16D2 / 0.3D1 * wd * t87 / t15 / s / t25

      end function
  
   
 

      doubleprecision function ggbbH32J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t11 = s - t2 * t5 * x4 - t2 * t5 * (0.1D1 - x4)
      t12 = s ** 2
      t13 = t11 * t12
      t14 = t3 * x1
      t17 = 0.1D1 - x3
      t20 = s - t2 * t14 * x3 - t2 * t14 * t17
      t21 = t13 * t20
      t22 = t1 * t3
      t24 = t22 * t4 * x4
      t28 = t12 * s * z
      t29 = t28 * t20
      t32 = t22 * x1 * t17
      t35 = t28 * t11
      t39 = t1 ** 2
      t42 = t3 ** 2
      t51 = t39 * t42
      t52 = x1 ** 2
      t53 = t17 ** 2
      t61 = t4 ** 2
      t62 = x4 ** 2
      t70 = -0.22D2 * t21 * t24 + 0.16D2 * t29 - 0.8D1 * t29 * t32 - 0.8
     #D1 * t35 * t24 + 0.27D2 * t11 * t20 * t12 * t39 * t42 * t4 * x4 * 
     #x1 * t17 + 0.16D2 * t35 * t32 + 0.9D1 * t21 * t51 * t52 * t53 + 0.
     #16D2 * t21 - 0.22D2 * t21 * t32 + 0.9D1 * t21 * t51 * t61 * t62 - 
     #0.11D2 * t13 * t20 * z
      ggbbH32J2 = -0.16D2 / 0.3D1 * wd * t70 / t11 / s / t20

      end function
  
   
 

      doubleprecision function ggbbH32J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t11 = s - t2 * t5 * x4 - t2 * t5 * (0.1D1 - x4)
      t12 = s ** 2
      t13 = t11 * t12
      t14 = t3 * x1
      t17 = 0.1D1 - x3
      t20 = s - t2 * t14 * x3 - t2 * t14 * t17
      t21 = t13 * t20
      t23 = t1 * t3
      t25 = t23 * x1 * t17
      t32 = t12 * s * z
      ggbbH32J3 = -0.16D2 / 0.3D1 * wd * (0.11D2 * t21 - 0.9D1 * t21 * t
     #25 - 0.8D1 * t13 * t20 * z - 0.8D1 * t32 * t20 - 0.9D1 * t21 * t23
     # * t4 * x4 - 0.8D1 * t32 * t11 * t25) / t11 / s / t20

      end function
  
 