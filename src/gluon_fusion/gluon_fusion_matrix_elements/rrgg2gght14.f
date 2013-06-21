  
      subroutine rrgg2gght14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght14s1e1  
      doubleprecision rrgg2gght14s1e0  
      doubleprecision rrgg2gght14s1em1  
      doubleprecision rrgg2gght14s1em2  
      doubleprecision rrgg2gght14s1em3  
      doubleprecision rrgg2gght14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght14s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = t17 ** 2
      t20 = cos(t8)
      t21 = t20 ** 2
      t22 = t4 * x2
      t23 = Sqrt(-t22)
      t24 = t23 ** 2
      t26 = x2 * z
      t28 = (0.1D1 - x2 + t26) ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / t29
      t31 = t1 ** 2
      t32 = t31 ** 2
      t34 = t21 * t24 * t30 * t32
      t40 = (0.180D3 + 0.180D3 * lh) * z
      t41 = t40 * wd
      t42 = -0.180D3 * t6 + t41
      t45 = t24 * t30
      t46 = t45 * t32
      t51 = 0.360D3 * lh
      t52 = lh ** 2
      t53 = 0.180D3 * t52
      t54 = 0.3141592653589793D1 ** 2
      t55 = 0.30D2 * t54
      t57 = (-0.90D2 - t51 - t53 + t55) * z
      t59 = -0.270D3 * t6 + 0.2D1 * t41 + t57 * wd
      t64 = 0.1D1 / x3
      t67 = z * t21
      t69 = wd * t24 * t30
      t72 = 0.180D3 * lh
      t74 = t12 * t4
      t77 = log(-0.4D1 * x2 * t10 * t74)
      t78 = 0.90D2 * t77
      t85 = t77 * lh
      t87 = t77 ** 2
      t108 = t72 + t78 + 0.360D3 * t85 + 0.90D2 * t87 + 0.360D3 * t52 - 
     #0.60D2 * t54 + 0.90D2 * t87 * lh - 0.60D2 * lh * t54 + 0.288493656
     #7583026D3 + 0.120D3 * t52 * lh + 0.15D2 * t87 * t77 - t77 * (-t53 
     #+ t55)
      t115 = x1 ** 2
      t116 = x3 * t115
      t117 = t116 * x2
      t120 = log(-0.4D1 * t117 * t14)
      t129 = 0.1D1 / x1
      t132 = t67 * wd
      t133 = x2 * t115
      t136 = log(-0.4D1 * t133 * t14)
      t137 = t136 ** 2
      t143 = t21 * wd
      t144 = t40 * t143
      t146 = (-0.180D3 * t132 + t144) * t32
      t155 = (-0.270D3 * t132 + 0.2D1 * t144 + t57 * t143) * t32
      t161 = (0.720D3 * t6 * t18 * t34 + 0.16D2 * t42 * t17 * t21 * t46 
     #- 0.16D2 * t59 * t21 * t46) * t64 / 0.320D3 - (-0.360D3 * t67 * t6
     #9 + 0.3D1 * (0.180D3 + t72 + t78) * z * t21 * t69 + 0.2D1 * (-0.90
     #D2 - t51 - 0.180D3 * t77 - 0.180D3 * t85 - 0.45D2 * t87 - t53 + t5
     #5) * z * t21 * t69 + t108 * z * t21 * t69) * t32 / 0.20D2 + (-0.14
     #40D4 * t6 * t120 * t34 - 0.16D2 * t42 * t21 * t46) * t64 * t129 / 
     #0.160D3 + (0.720D3 * t132 * t32 * t137 * t45 + 0.16D2 * t146 * t13
     #6 * t24 * t30 - 0.16D2 * t155 * t45) * t129 / 0.160D3
      t162 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t161)
      t164 = 0.2D1 * t7
      t165 = sqrt(x3)
      t166 = t20 * t165
      t167 = -0.1D1 + t165
      t168 = t165 + 0.1D1
      t171 = Sqrt(t22 * t167 * t168)
      t173 = 0.2D1 * t166 * t171
      t175 = t2 * (0.1D1 - x2 - x3 + t164 + t173)
      t177 = t2 * (-x3 + t164 - x2 + t173)
      t181 = t12 * t167 * t7
      t184 = log(0.4D1 * t10 * t168 * t4 * t181)
      t185 = t184 ** 2
      t187 = x3 * z
      t189 = 0.2D1 * t7 * z
      t190 = z * t20
      t195 = (-t164 - t187 - t26 + x3 - 0.1D1 + x2 + t189 - t173 + 0.2D1
     # * t190 * t165 * t171) ** 2
      t196 = 0.1D1 / t195
      t197 = t165 * x3
      t198 = t197 * z
      t200 = 0.2D1 * t165 * z
      t201 = t165 * x2
      t202 = 0.5D1 * t201
      t203 = t197 * x2
      t204 = 0.2D1 * t203
      t210 = 0.3D1 * t165
      t212 = 0.5D1 * t201 * z
      t213 = t20 * x3
      t219 = 0.2D1 * t203 * z
      t220 = t197 - t198 + t200 + t202 - t204 + 0.4D1 * t20 * t171 + 0.2
     #D1 * t190 * x3 * t171 - t210 - t212 - 0.2D1 * t213 * t171 - 0.4D1 
     #* t190 * t171 + t219
      t221 = t220 ** 2
      t223 = 0.1D1 / t28
      t225 = t196 * t221 * t223 * t31
      t231 = t221 * t223 * t31
      t238 = t10 * t115
      t243 = log(0.4D1 * t238 * t168 * t4 * t181)
      t253 = (-0.45D2 * t6 * t185 * t225 - t42 * t184 * t196 * t231 + t5
     #9 * t196 * t231) * t64 / 0.320D3 + (0.90D2 * t6 * t243 * t225 + t4
     #2 * t196 * t231) * t64 * t129 / 0.160D3
      t254 = FJET(XB1, XB2, s, 0.0D0, t175, 0.0D0, -t177, 0.0D0, t253)
      t256 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t161)
      t258 = FJET(XB1, XB2, s, 0.0D0, -t177, 0.0D0, t175, 0.0D0, t253)
      t260 = -0.1D1 + x1
      t262 = x1 * z
      t263 = 0.1D1 - x1 + t262
      t264 = 0.1D1 / t263
      t266 = t2 * t260 * x2 * t264
      t267 = t2 * x1
      t270 = t4 * s * t1 * t260
      t275 = s * t31 * x2 * x1 * t260 * t264
      t278 = t260 ** 2
      t280 = t12 * t264 * t278 * t4
      t283 = log(-0.4D1 * t7 * t238 * t280)
      t286 = t4 * t263
      t288 = Sqrt(-t286 * x2)
      t289 = t288 ** 2
      t290 = x1 * x2
      t291 = t290 * z
      t293 = (-0.1D1 + x1 - t262 + x2 - t290 - t26 + t291) ** 2
      t294 = t293 ** 2
      t295 = 0.1D1 / t294
      t296 = t289 * t295
      t297 = t278 * t260
      t305 = t295 * t297
      t318 = log(-0.4D1 * t133 * t10 * t280)
      t319 = t318 ** 2
      t321 = t296 * t297
      t336 = (-0.1440D4 * t6 * t283 * t263 * t296 * t297 * t21 * t32 - 0
     #.16D2 * t42 * t263 * t289 * t305 * t21 * t32) * t64 * t129 / 0.160
     #D3 + (0.720D3 * t67 * wd * t32 * t319 * t263 * t321 + 0.16D2 * t14
     #6 * t318 * t263 * t289 * t305 - 0.16D2 * t155 * t263 * t321) * t12
     #9 / 0.160D3
      t337 = FJET(XB1, XB2, s, 0.0D0, -t266, t267, t270, -t275, t336)
      t339 = FJET(XB1, XB2, s, t267, t270, 0.0D0, -t266, -t275, t336)
      t341 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t161)
      t343 = FJET(XB1, XB2, s, t175, 0.0D0, -t177, 0.0D0, 0.0D0, t253)
      t345 = x3 * x1
      t346 = t2 * t345
      t347 = t345 * z
      t348 = t345 * x2
      t349 = t345 * t26
      t350 = x2 * t167
      t353 = Sqrt(t286 * t350 * t168)
      t355 = 0.2D1 * t166 * t353
      t359 = t2 * t260 * (-x3 + t345 - t347 + t164 - t348 + t349 - x2 + 
     #t355) * t264
      t362 = t2 * x1 * t167 * t168
      t363 = 0.1D1 - x1 + t262 - x2 + t290 - t291 - x3 + t345 - t347 + t
     #164 - t348 + t349 + t355
      t366 = t2 * t260 * t363 * t264
      t375 = log(0.4D1 * t10 * t278 * t115 * t264 * t168 * t74 * t350 * 
     #x3)
      t379 = t115 * t11
      t383 = x2 * t11
      t385 = 0.1D1 - 0.2D1 * t347 - t189 - 0.3D1 * t348 - t291 + 0.4D1 *
     # t349 + t379 * t7 - 0.2D1 * t116 * t26 - t345 * t383 + t164 + t262
     # - x1
      t386 = t165 * t353
      t392 = t353 * x1
      t396 = t187 + t345 + t26 + t290 - 0.2D1 * t190 * t386 + 0.2D1 * t1
     #90 * t386 * x1 - 0.2D1 * t166 * t392 - x2 - x3 + t345 * t11 + t117
     # + t355
      t398 = (t385 + t396) ** 2
      t399 = 0.1D1 / t398
      t402 = x3 * t353
      t413 = t20 * t353
      t416 = t165 * x1
      t421 = x1 * t197
      t427 = t197 * t115
      t429 = t165 * t115
      t435 = 0.2D1 * t190 * t402 * x1 + 0.2D1 * t213 * t353 + 0.4D1 * t1
     #90 * t353 - 0.2D1 * t165 * t11 * x1 + 0.4D1 * t413 * x1 + 0.6D1 * 
     #t416 * z + 0.8D1 * t416 * x2 - 0.3D1 * t421 * x2 - 0.2D1 * t421 * 
     #z + t421 * t11 + t427 * x2 - 0.2D1 * t429 * z - 0.3D1 * t429 * x2 
     #+ t379 * t165 + t198 - t200 - t202 + t204
      t458 = t421 + t429 - 0.4D1 * t416 - 0.4D1 * t413 - 0.2D1 * t213 * 
     #t392 - 0.4D1 * t190 * t392 + 0.4D1 * t203 * t262 - 0.2D1 * t190 * 
     #t402 - 0.11D2 * t416 * t26 - 0.3D1 * t379 * t201 + 0.6D1 * t429 * 
     #t26 + 0.3D1 * t416 * t383 - 0.2D1 * t427 * t26 - t421 * t383 + t42
     #7 * t383 + t210 - t197 + t212 - t219
      t460 = (t435 + t458) ** 2
      t461 = 0.1D1 / t293
      t474 = 0.90D2 * t6 * t375 * t399 * t460 * t461 * t260 * t263 * t31
     # + t42 * t399 * t460 * t461 * t260 * t263 * t31
      t477 = t474 * t64 * t129 / 0.160D3
      t478 = FJET(XB1, XB2, s, t346, t359, -t362, -t366, -t275, t477)
      t480 = t64 * t129
      t483 = FJET(XB1, XB2, s, t270, t267, -t266, 0.0D0, -t275, t336)
      t485 = FJET(XB1, XB2, s, t359, t346, -t366, -t362, -t275, t477)
      t489 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t161)
      t491 = FJET(XB1, XB2, s, -t177, 0.0D0, t175, 0.0D0, 0.0D0, t253)
      t493 = FJET(XB1, XB2, s, -t362, -t366, t346, t359, -t275, t477)
      t497 = FJET(XB1, XB2, s, -t266, 0.0D0, t270, t267, -t275, t336)
      t499 = FJET(XB1, XB2, s, -t366, -t362, t359, t346, -t275, t477)
      rrgg2gght14s1e1 = t162 * t161 + t254 * t253 + t256 * t161 + t258 *
     # t253 + t337 * t336 + t339 * t336 + t341 * t161 + t343 * t253 + t4
     #78 * t474 * t480 / 0.160D3 + t483 * t336 + t485 * t474 * t480 / 0.
     #160D3 + t489 * t161 + t491 * t253 + t493 * t474 * t480 / 0.160D3 +
     # t497 * t336 + t499 * t474 * t480 / 0.160D3

      end function



      doubleprecision function rrgg2gght14s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t19 = cos(t8)
      t20 = t19 ** 2
      t21 = t4 * x2
      t22 = Sqrt(-t21)
      t23 = t22 ** 2
      t24 = t20 * t23
      t25 = x2 * z
      t27 = (0.1D1 - x2 + t25) ** 2
      t28 = t27 ** 2
      t29 = 0.1D1 / t28
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t39 = (0.180D3 + 0.180D3 * lh) * z
      t41 = -0.180D3 * t6 + t39 * wd
      t43 = t23 * t29
      t48 = 0.1D1 / x3
      t52 = 0.1D1 / x1
      t53 = t48 * t52
      t57 = z * t20
      t58 = t57 * wd
      t59 = x1 ** 2
      t60 = x2 * t59
      t63 = log(-0.4D1 * t60 * t14)
      t72 = (-0.180D3 * t58 + t39 * t20 * wd) * t31
      t79 = wd * t23 * t29
      t87 = log(-0.4D1 * x2 * t10 * t12 * t4)
      t98 = t87 ** 2
      t100 = lh ** 2
      t102 = 0.3141592653589793D1 ** 2
      t111 = (-0.1440D4 * t6 * t17 * t24 * t32 - 0.16D2 * t41 * t20 * t4
     #3 * t31) * t48 / 0.320D3 + 0.9D1 * t6 * t24 * t32 * t53 + (-0.1440
     #D4 * t58 * t31 * t63 * t43 - 0.16D2 * t72 * t43) * t52 / 0.160D3 -
     # (-0.270D3 * t57 * t79 + 0.2D1 * (0.180D3 + 0.180D3 * lh + 0.90D2 
     #* t87) * z * t20 * t79 + (-0.90D2 - 0.360D3 * lh - 0.180D3 * t87 -
     # 0.180D3 * t87 * lh - 0.45D2 * t98 - 0.180D3 * t100 + 0.30D2 * t10
     #2) * z * t20 * t79) * t31 / 0.20D2
      t112 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t111)
      t114 = 0.2D1 * t7
      t115 = sqrt(x3)
      t116 = t19 * t115
      t117 = -0.1D1 + t115
      t118 = t115 + 0.1D1
      t121 = Sqrt(t21 * t117 * t118)
      t123 = 0.2D1 * t116 * t121
      t125 = t2 * (0.1D1 - x2 - x3 + t114 + t123)
      t127 = t2 * (-x3 + t114 - x2 + t123)
      t134 = log(0.4D1 * t10 * t118 * t4 * t12 * t117 * t7)
      t136 = x3 * z
      t138 = 0.2D1 * t7 * z
      t139 = z * t19
      t144 = (-t114 - t136 - t25 + x3 - 0.1D1 + x2 + t138 - t123 + 0.2D1
     # * t139 * t115 * t121) ** 2
      t145 = 0.1D1 / t144
      t146 = x3 * t115
      t147 = t146 * z
      t149 = 0.2D1 * t115 * z
      t150 = x2 * t115
      t151 = 0.5D1 * t150
      t152 = t146 * x2
      t153 = 0.2D1 * t152
      t159 = 0.3D1 * t115
      t161 = 0.5D1 * t150 * z
      t162 = t19 * x3
      t168 = 0.2D1 * t152 * z
      t169 = t146 - t147 + t149 + t151 - t153 + 0.4D1 * t19 * t121 + 0.2
     #D1 * t139 * x3 * t121 - t159 - t161 - 0.2D1 * t162 * t121 - 0.4D1 
     #* t139 * t121 + t168
      t170 = t169 ** 2
      t171 = t145 * t170
      t172 = 0.1D1 / t27
      t173 = t172 * t30
      t188 = (0.90D2 * t6 * t134 * t171 * t173 + t41 * t145 * t170 * t17
     #2 * t30) * t48 / 0.320D3 - 0.9D1 / 0.16D2 * t6 * t171 * t173 * t53
      t189 = FJET(XB1, XB2, s, 0.0D0, t125, 0.0D0, -t127, 0.0D0, t188)
      t191 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t111)
      t193 = FJET(XB1, XB2, s, 0.0D0, -t127, 0.0D0, t125, 0.0D0, t188)
      t195 = -0.1D1 + x1
      t197 = x1 * z
      t198 = 0.1D1 - x1 + t197
      t199 = 0.1D1 / t198
      t201 = t2 * t195 * x2 * t199
      t202 = t2 * x1
      t205 = t4 * s * t1 * t195
      t210 = s * t30 * x2 * x1 * t195 * t199
      t211 = t4 * t198
      t213 = Sqrt(-t211 * x2)
      t214 = t213 ** 2
      t216 = x1 * x2
      t217 = t216 * z
      t219 = (-0.1D1 + x1 - t197 + x2 - t216 - t25 + t217) ** 2
      t220 = t219 ** 2
      t221 = 0.1D1 / t220
      t224 = t195 ** 2
      t225 = t224 * t195
      t240 = log(-0.4D1 * t60 * t10 * t12 * t199 * t224 * t4)
      t243 = t214 * t221 * t225
      t253 = 0.9D1 * t6 * t198 * t214 * t221 * t225 * t20 * t31 * t48 * 
     #t52 + (-0.1440D4 * t57 * wd * t31 * t240 * t198 * t243 - 0.16D2 * 
     #t72 * t198 * t243) * t52 / 0.160D3
      t254 = FJET(XB1, XB2, s, 0.0D0, -t201, t202, t205, -t210, t253)
      t256 = FJET(XB1, XB2, s, t202, t205, 0.0D0, -t201, -t210, t253)
      t258 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t111)
      t260 = FJET(XB1, XB2, s, t125, 0.0D0, -t127, 0.0D0, 0.0D0, t188)
      t262 = x3 * x1
      t263 = t2 * t262
      t264 = t262 * z
      t265 = t262 * x2
      t266 = t262 * t25
      t270 = Sqrt(t211 * x2 * t117 * t118)
      t272 = 0.2D1 * t116 * t270
      t276 = t2 * t195 * (-x3 + t262 - t264 + t114 - t265 + t266 - x2 + 
     #t272) * t199
      t279 = t2 * x1 * t117 * t118
      t280 = 0.1D1 - x1 + t197 - x2 + t216 - t217 - x3 + t262 - t264 + t
     #114 - t265 + t266 + t272
      t283 = t2 * t195 * t280 * t199
      t287 = t59 * t11
      t289 = x3 * t59
      t292 = x2 * t11
      t294 = 0.1D1 - 0.2D1 * t264 - t138 - 0.3D1 * t265 - t217 + 0.4D1 *
     # t266 + t287 * t7 - 0.2D1 * t289 * t25 - t262 * t292 + t114 + t197
     # - x1
      t295 = t115 * t270
      t301 = t270 * x1
      t306 = t136 + t262 + t25 + t216 - 0.2D1 * t139 * t295 + 0.2D1 * t1
     #39 * t295 * x1 - 0.2D1 * t116 * t301 - x2 - x3 + t262 * t11 + t289
     # * x2 + t272
      t308 = (t294 + t306) ** 2
      t309 = 0.1D1 / t308
      t310 = x3 * t270
      t321 = t19 * t270
      t332 = t115 * x1
      t337 = t115 * t59
      t340 = t147 - t149 - t151 + t153 - t146 + t159 + 0.2D1 * t139 * t3
     #10 * x1 + 0.2D1 * t162 * t270 + 0.4D1 * t139 * t270 - 0.2D1 * t115
     # * t11 * x1 + 0.4D1 * t321 * x1 - 0.2D1 * t162 * t301 - 0.4D1 * t1
     #39 * t301 + 0.4D1 * t152 * t197 - 0.2D1 * t139 * t310 - 0.11D2 * t
     #332 * t25 - 0.3D1 * t287 * t150 + 0.6D1 * t337 * t25
      t343 = t59 * t146
      t346 = x1 * t146
      t366 = 0.3D1 * t332 * t292 - 0.2D1 * t343 * t25 - t346 * t292 + t3
     #43 * t292 + t161 - t168 + 0.6D1 * t332 * z + 0.8D1 * t332 * x2 - 0
     #.3D1 * t346 * x2 - 0.2D1 * t346 * z + t346 * t11 + t343 * x2 - 0.2
     #D1 * t337 * z - 0.3D1 * t337 * x2 + t287 * t115 + t346 + t337 - 0.
     #4D1 * t332 - 0.4D1 * t321
      t368 = (t340 + t366) ** 2
      t370 = 0.1D1 / t219
      t375 = t30 * t48 * t52
      t378 = 0.9D1 / 0.16D2 * t6 * t309 * t368 * t370 * t195 * t198 * t3
     #75
      t379 = FJET(XB1, XB2, s, t263, t276, -t279, -t283, -t210, -t378)
      t382 = wd * t309 * t368
      t386 = t370 * t195 * t198 * t375
      t389 = FJET(XB1, XB2, s, t205, t202, -t201, 0.0D0, -t210, t253)
      t391 = FJET(XB1, XB2, s, t276, t263, -t283, -t279, -t210, -t378)
      t396 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t111)
      t398 = FJET(XB1, XB2, s, -t127, 0.0D0, t125, 0.0D0, 0.0D0, t188)
      t400 = FJET(XB1, XB2, s, -t279, -t283, t263, t276, -t210, -t378)
      t405 = FJET(XB1, XB2, s, -t201, 0.0D0, t205, t202, -t210, t253)
      t407 = FJET(XB1, XB2, s, -t283, -t279, t276, t263, -t210, -t378)
      rrgg2gght14s1e0 = t112 * t111 + t189 * t188 + t191 * t111 + t193 *
     # t188 + t254 * t253 + t256 * t253 + t258 * t111 + t260 * t188 - 0.
     #9D1 / 0.16D2 * t379 * z * t382 * t386 + t389 * t253 - 0.9D1 / 0.16
     #D2 * t391 * z * t382 * t386 + t396 * t111 + t398 * t188 - 0.9D1 / 
     #0.16D2 * t400 * z * t382 * t386 + t405 * t253 - 0.9D1 / 0.16D2 * t
     #407 * z * t382 * t386

      end function



      doubleprecision function rrgg2gght14s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = z * wd
      t7 = x4 * 0.3141592653589793D1
      t8 = cos(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = t4 * x2
      t12 = Sqrt(-t11)
      t13 = t12 ** 2
      t14 = x2 * z
      t16 = (0.1D1 - x2 + t14) ** 2
      t17 = t16 ** 2
      t18 = 0.1D1 / t17
      t19 = t13 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = 0.1D1 / x1
      t29 = wd * t13 * t18
      t33 = Sin(t7)
      t34 = t33 ** 2
      t36 = z ** 2
      t41 = log(-0.4D1 * x2 * t34 / t36 * t4)
      t50 = 0.1D1 / x3
      t55 = 0.9D1 * t10 * t19 * t21 * t22 - (-0.180D3 * z * t9 * t29 + (
     #0.180D3 + 0.180D3 * lh + 0.90D2 * t41) * z * t9 * t29) * t21 / 0.2
     #0D2 + 0.9D1 / 0.2D1 * t10 * t19 * t21 * t50
      t56 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t55)
      t58 = x2 * x3
      t59 = 0.2D1 * t58
      t60 = sqrt(x3)
      t66 = Sqrt(t11 * (-0.1D1 + t60) * (t60 + 0.1D1))
      t68 = 0.2D1 * t8 * t60 * t66
      t70 = t2 * (0.1D1 - x2 - x3 + t59 + t68)
      t72 = t2 * (-x3 + t59 - x2 + t68)
      t76 = z * t8
      t81 = (-t59 - x3 * z - t14 + x3 - 0.1D1 + x2 + 0.2D1 * t58 * z - t
     #68 + 0.2D1 * t76 * t60 * t66) ** 2
      t82 = 0.1D1 / t81
      t84 = t60 * x3
      t88 = t60 * x2
      t90 = t84 * x2
      t107 = t84 - t84 * z + 0.2D1 * t60 * z + 0.5D1 * t88 - 0.2D1 * t90
     # + 0.4D1 * t8 * t66 + 0.2D1 * t76 * x3 * t66 - 0.3D1 * t60 - 0.5D1
     # * t88 * z - 0.2D1 * t8 * x3 * t66 - 0.4D1 * t76 * t66 + 0.2D1 * t
     #90 * z
      t108 = t107 ** 2
      t112 = t108 / t16 * t20 * t50
      t114 = 0.9D1 / 0.32D2 * t6 * t82 * t112
      t115 = FJET(XB1, XB2, s, 0.0D0, t70, 0.0D0, -t72, 0.0D0, -t114)
      t117 = wd * t82
      t121 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t55)
      t123 = FJET(XB1, XB2, s, 0.0D0, -t72, 0.0D0, t70, 0.0D0, -t114)
      t128 = -0.1D1 + x1
      t130 = x1 * z
      t131 = 0.1D1 - x1 + t130
      t132 = 0.1D1 / t131
      t134 = t2 * t128 * x2 * t132
      t135 = t2 * x1
      t138 = t4 * s * t1 * t128
      t143 = s * t20 * x2 * x1 * t128 * t132
      t146 = Sqrt(-t4 * t131 * x2)
      t147 = t146 ** 2
      t150 = x1 * x2
      t153 = (-0.1D1 + x1 - t130 + x2 - t150 - t14 + t150 * z) ** 2
      t154 = t153 ** 2
      t156 = t128 ** 2
      t161 = 0.1D1 / t154 * t156 * t128 * t9 * t21 * t22
      t163 = 0.9D1 * t6 * t131 * t147 * t161
      t164 = FJET(XB1, XB2, s, 0.0D0, -t134, t135, t138, -t143, t163)
      t167 = wd * t131 * t147
      t171 = FJET(XB1, XB2, s, t135, t138, 0.0D0, -t134, -t143, t163)
      t176 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t55)
      t178 = FJET(XB1, XB2, s, t70, 0.0D0, -t72, 0.0D0, 0.0D0, -t114)
      t183 = FJET(XB1, XB2, s, t138, t135, -t134, 0.0D0, -t143, t163)
      t188 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t55)
      t190 = FJET(XB1, XB2, s, -t134, 0.0D0, t138, t135, -t143, t163)
      t195 = FJET(XB1, XB2, s, -t72, 0.0D0, t70, 0.0D0, 0.0D0, -t114)
      rrgg2gght14s1em1 = t56 * t55 - 0.9D1 / 0.32D2 * t115 * z * t117 * 
     #t112 + t121 * t55 - 0.9D1 / 0.32D2 * t123 * z * t117 * t112 + 0.9D
     #1 * t164 * z * t167 * t161 + 0.9D1 * t171 * z * t167 * t161 + t176
     # * t55 - 0.9D1 / 0.32D2 * t178 * z * t117 * t112 + 0.9D1 * t183 * 
     #z * t167 * t161 + t188 * t55 + 0.9D1 * t190 * z * t167 * t161 - 0.
     #9D1 / 0.32D2 * t195 * z * t117 * t112

      end function



      doubleprecision function rrgg2gght14s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t8 = cos(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t12 = Sqrt(-t4 * x2)
      t13 = t12 ** 2
      t16 = (0.1D1 - x2 + x2 * z) ** 2
      t17 = t16 ** 2
      t18 = 0.1D1 / t17
      t20 = t1 ** 2
      t21 = t20 ** 2
      t24 = 0.9D1 / 0.2D1 * z * wd * t9 * t13 * t18 * t21
      t25 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t24)
      t30 = t9 * t13 * t18 * t21
      t32 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t24)
      t36 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t24)
      t40 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t24)
      rrgg2gght14s1em2 = 0.9D1 / 0.2D1 * t25 * z * wd * t30 + 0.9D1 / 0.
     #2D1 * t32 * z * wd * t30 + 0.9D1 / 0.2D1 * t36 * z * wd * t30 + 0.
     #9D1 / 0.2D1 * t40 * z * wd * t30

      end function



      doubleprecision function rrgg2gght14s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght14s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght14s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght14s1em4 = 0.0D0

      end function
