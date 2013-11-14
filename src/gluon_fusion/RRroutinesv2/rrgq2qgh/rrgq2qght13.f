  
      subroutine rrgq2qght13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght13s1e1  
      doubleprecision rrgq2qght13s1e0  
      doubleprecision rrgq2qght13s1em1  
      doubleprecision rrgq2qght13s1em2  
      doubleprecision rrgq2qght13s1em3  
      doubleprecision rrgq2qght13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght13s1e1
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
      t4 = -0.1D1 + x2
      t6 = x4 * pi
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t1 ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t14 * t4
      t18 = log(-0.4D1 * x2 * t8 * t15)
      t19 = t18 * wd
      t21 = x2 * z
      t23 = (0.1D1 - x2 + t21) ** 2
      t24 = t23 ** 2
      t25 = 0.1D1 / t24
      t27 = (wd - t19) * t25 * z
      t28 = t12 * t1
      t29 = cos(t6)
      t30 = t29 ** 2
      t31 = t28 * t30
      t32 = t4 * x2
      t33 = Sqrt(-t32)
      t34 = t33 ** 2
      t36 = t31 * t34 * lh
      t39 = t18 ** 2
      t41 = t39 * wd / 0.2D1
      t44 = (wd - t19 + t41) * t25 * z
      t45 = t31 * t34
      t48 = wd * z
      t49 = lh ** 2
      t51 = pi ** 2
      t53 = -0.180D3 * t49 + 0.30D2 * t51
      t54 = t48 * t53
      t57 = t25 * t28 * t30 * t34
      t58 = t54 * t57
      t60 = x2 * x3
      t64 = log(-0.4D1 * t60 * t8 * t15)
      t65 = t64 ** 2
      t73 = 0.64D2 * t58
      t75 = 0.1D1 / x3
      t104 = x1 ** 2
      t105 = x3 * t104
      t106 = t105 * x2
      t107 = t8 * t11
      t112 = log(-0.4D1 * t106 * t107 * t13 * t4)
      t122 = 0.90D2 * t48 + (0.180D3 * wd * lh - 0.90D2 * wd) * z
      t128 = 0.1D1 / x1
      t132 = x2 * t104
      t136 = log(-0.4D1 * t132 * t8 * t15)
      t137 = t136 ** 2
      t139 = t25 * t34
      t143 = t48 * t31
      t151 = -0.8D1 * t27 * t36 + 0.4D1 * t44 * t45 - 0.2D1 / 0.45D2 * t
     #58 - (0.2880D4 * t48 * t65 * t57 + 0.11520D5 * t48 * lh * t64 * t5
     #7 - t73) * t75 / 0.1440D4 + 0.2D1 / 0.45D2 * wd * t25 * z * t31 * 
     #t34 * (-0.60D2 * lh * t51 + 0.240D3 * zeta3 + 0.120D3 * t49 * lh) 
     #- 0.4D1 * (wd - t19 + t41 - t39 * t18 * wd / 0.6D1) * t25 * z * t4
     #5 + 0.2D1 / 0.45D2 * t27 * t31 * t34 * t53 + 0.8D1 * t44 * t36 - (
     #-0.5760D4 * t48 * t112 * t57 - 0.64D2 * t122 * t25 * t45) * t75 * 
     #t128 / 0.720D3 - (0.2880D4 * t48 * t28 * t30 * t137 * t139 + 0.115
     #20D5 * t143 * lh * t136 * t139 - t73) * t128 / 0.720D3
      t152 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #151)
      t154 = -0.1D1 + x1
      t155 = x3 * x1
      t156 = t155 * z
      t157 = 0.2D1 * t60
      t158 = t60 * x1
      t159 = x1 * z
      t160 = t60 * t159
      t161 = sqrt(x3)
      t162 = t29 * t161
      t163 = 0.1D1 - x1 + t159
      t164 = t4 * t163
      t165 = -0.1D1 + t161
      t167 = t161 + 0.1D1
      t170 = Sqrt(t164 * x2 * t165 * t167)
      t172 = 0.2D1 * t162 * t170
      t175 = 0.1D1 / t163
      t179 = x2 * x1
      t180 = t179 * z
      t181 = 0.1D1 - x1 + t159 - x2 + t179 - t180 - x3 + t155 - t156 + t
     #157 - t158 + t160 + t172
      t192 = s * t12 * x2 * x1 * t154 * t175
      t193 = t165 * t167
      t194 = t154 ** 2
      t198 = t4 * x3
      t205 = log(0.4D1 * t193 * t194 * t104 * x2 * t198 * t8 * t11 * t17
     #5 * t13)
      t208 = t161 * x3
      t209 = t208 * z
      t210 = x1 * t208
      t211 = t161 * t104
      t213 = 0.2D1 * t161 * z
      t214 = t161 * x1
      t216 = t208 * x2
      t217 = 0.2D1 * t216
      t218 = t161 * x2
      t219 = 0.5D1 * t218
      t220 = t29 * t170
      t225 = x2 * t10
      t229 = t161 * t10
      t238 = t29 * x3
      t239 = t170 * x1
      t242 = z * t29
      t243 = x3 * t170
      t246 = -t208 + t209 + t210 + t211 - t213 - 0.4D1 * t214 + t217 - t
     #219 - 0.4D1 * t220 - 0.2D1 * t104 * t208 * t21 - t210 * t225 - 0.1
     #1D2 * t218 * t159 + 0.3D1 * t229 * t179 + 0.6D1 * t211 * t21 - 0.3
     #D1 * t211 * t225 - 0.4D1 * t220 * t159 - 0.2D1 * t238 * t239 - 0.2
     #D1 * t242 * t243
      t249 = 0.3D1 * t161
      t258 = 0.2D1 * t216 * z
      t266 = 0.5D1 * t218 * z
      t282 = t216 * t104 * t10 + t249 + 0.2D1 * t242 * t243 * x1 + 0.4D1
     # * t216 * t159 - 0.2D1 * t229 * x1 - t258 + 0.2D1 * t238 * t170 + 
     #0.4D1 * t220 * x1 + 0.4D1 * t220 * z + t266 - 0.2D1 * t210 * z + t
     #210 * t10 + t216 * t104 - 0.3D1 * t210 * x2 + 0.6D1 * t214 * z + 0
     #.8D1 * t218 * x1 - 0.2D1 * t211 * z - 0.3D1 * t211 * x2 + t211 * t
     #10
      t284 = (t246 + t282) ** 2
      t287 = (-0.1D1 + x1 - t159 + x2 - t179 - t21 + t180) ** 2
      t288 = 0.1D1 / t287
      t291 = x3 * t10
      t298 = t170 * z
      t301 = 0.1D1 + 0.4D1 * t160 - t291 * t179 - 0.2D1 * t105 * t21 + t
     #105 * t225 + t172 - 0.2D1 * t162 * t239 - x1 - x2 - x3 + t159 - 0.
     #2D1 * t162 * t298
      t307 = 0.2D1 * t60 * z
      t310 = x3 * z
      t311 = 0.2D1 * t162 * t298 * x1 - t180 - 0.2D1 * t156 - t307 - 0.3
     #D1 * t158 + t291 * x1 + t157 + t21 + t179 + t310 + t155 + t106
      t313 = (t301 + t311) ** 2
      t314 = 0.1D1 / t313
      t326 = 0.360D3 * t48 * t205 * t154 * t284 * t163 * t1 * t288 * t31
     #4 + 0.4D1 * t122 * t154 * t284 * t163 * t1 * t288 * t314
      t330 = FJET(XB1, XB2, s, t2 * t154 * (-x3 + t155 - t156 + t157 - t
     #158 + t160 - x2 + t172) * t175, t2 * t155, -t2 * t154 * t181 * t17
     #5, -t2 * x1 * t165 * t167, -t192, -t326 * t75 * t128 / 0.720D3)
      t335 = t32 * t193
      t336 = Sqrt(t335)
      t338 = 0.2D1 * t162 * t336
      t347 = log(0.4D1 * t335 * x3 * t8 * t14)
      t348 = t347 ** 2
      t350 = 0.1D1 / t23
      t351 = t29 * t336
      t360 = -t249 + t258 + t208 - t266 - 0.4D1 * t351 * z - 0.2D1 * t23
     #8 * t336 + 0.2D1 * t242 * x3 * t336 - t209 + t213 - t217 + 0.4D1 *
     # t351 + t219
      t361 = t360 ** 2
      t367 = (-t338 - 0.1D1 - t310 + x3 - t157 + 0.2D1 * t162 * t336 * z
     # + t307 - t21 + x2) ** 2
      t368 = 0.1D1 / t367
      t370 = t350 * t361 * t368 * t1
      t387 = log(0.4D1 * t193 * t132 * t198 * t107 * t13)
      t400 = -(-0.180D3 * t48 * t348 * t370 - 0.720D3 * t48 * lh * t347 
     #* t370 + 0.4D1 * t54 * t370) * t75 / 0.1440D4 - (0.360D3 * t48 * t
     #387 * t370 + 0.4D1 * t122 * t350 * t361 * t368 * t1) * t75 * t128 
     #/ 0.720D3
      t401 = FJET(XB1, XB2, s, -t2 * (-x3 + t157 - x2 + t338), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t157 + t338), 0.0D0, 0.0D0, t400)
      t417 = log(-0.4D1 * t60 * t104 * t8 * t14 * t175 * t194 * t4)
      t420 = t287 ** 2
      t421 = 0.1D1 / t420
      t423 = Sqrt(-t164 * x2)
      t424 = t423 ** 2
      t427 = t194 * t154
      t434 = t424 * t163
      t448 = log(-0.4D1 * t132 * t107 * t13 * t175 * t194 * t4)
      t449 = t448 ** 2
      t451 = t434 * t427
      t468 = -(-0.5760D4 * t48 * t417 * t28 * t421 * t424 * t163 * t30 *
     # t427 - 0.64D2 * t122 * t28 * t421 * t434 * t30 * t427) * t75 * t1
     #28 / 0.720D3 - (0.2880D4 * t143 * t449 * t421 * t451 + 0.11520D5 *
     # t48 * t31 * lh * t448 * t421 * t451 - 0.64D2 * t143 * t53 * t421 
     #* t451) * t128 / 0.720D3
      t469 = FJET(XB1, XB2, s, -t2 * t154 * x2 * t175, 0.0D0, t4 * s * t
     #1 * t154, t2 * x1, -t192, t468)
      rrgq2qght13s1e1 = t152 * t151 - t330 * t326 * t75 * t128 / 0.720D3
     # + t401 * t400 + t469 * t468

      end function



      doubleprecision function rrgq2qght13s1e0
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
      t4 = -0.1D1 + x2
      t6 = wd * z
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t17 = t16 * t4
      t20 = log(-0.4D1 * t7 * t10 * t17)
      t22 = x2 * z
      t24 = (0.1D1 - x2 + t22) ** 2
      t25 = t24 ** 2
      t26 = 0.1D1 / t25
      t27 = t14 * t1
      t28 = t26 * t27
      t29 = cos(t8)
      t30 = t29 ** 2
      t31 = t4 * x2
      t32 = Sqrt(-t31)
      t33 = t32 ** 2
      t34 = t30 * t33
      t35 = t28 * t34
      t38 = t6 * lh
      t39 = t38 * t35
      t40 = 0.11520D5 * t39
      t42 = 0.1D1 / x3
      t46 = 0.1D1 / x1
      t47 = t42 * t46
      t52 = x1 ** 2
      t53 = x2 * t52
      t57 = log(-0.4D1 * t53 * t10 * t17)
      t70 = log(-0.4D1 * x2 * t10 * t17)
      t71 = t70 * wd
      t74 = (wd - t71) * t26 * z
      t75 = t27 * t30
      t76 = t75 * t33
      t83 = t70 ** 2
      t91 = lh ** 2
      t93 = pi ** 2
      t99 = -(-0.5760D4 * t6 * t20 * t35 - t40) * t42 / 0.1440D4 - 0.8D1
     # * t6 * t28 * t34 * t47 - (-0.5760D4 * t6 * t27 * t30 * t57 * t26 
     #* t33 - t40) * t46 / 0.720D3 - 0.8D1 * t39 + 0.4D1 * t74 * t76 + 0
     #.8D1 * t74 * t75 * t33 * lh - 0.4D1 * (wd - t71 + t83 * wd / 0.2D1
     #) * t26 * z * t76 + 0.2D1 / 0.45D2 * t6 * (-0.180D3 * t91 + 0.30D2
     # * t93) * t35
      t100 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #99)
      t102 = -0.1D1 + x1
      t103 = x3 * x1
      t104 = t103 * z
      t105 = 0.2D1 * t7
      t106 = t7 * x1
      t107 = x1 * z
      t108 = t7 * t107
      t109 = sqrt(x3)
      t110 = t29 * t109
      t111 = 0.1D1 - x1 + t107
      t112 = t4 * t111
      t113 = -0.1D1 + t109
      t115 = t109 + 0.1D1
      t118 = Sqrt(t112 * x2 * t113 * t115)
      t120 = 0.2D1 * t110 * t118
      t123 = 0.1D1 / t111
      t127 = x2 * x1
      t128 = t127 * z
      t129 = 0.1D1 - x1 + t107 - x2 + t127 - t128 - x3 + t103 - t104 + t
     #105 - t106 + t108 + t120
      t140 = s * t14 * x2 * x1 * t102 * t123
      t141 = t109 * t12
      t144 = t109 * x3
      t145 = t144 * x2
      t147 = 0.2D1 * t145 * z
      t148 = t29 * x3
      t151 = t29 * t118
      t156 = t109 * x2
      t158 = 0.5D1 * t156 * z
      t159 = t144 * x1
      t166 = t109 * x1
      t171 = t109 * t52
      t182 = x2 * t12
      t184 = -0.2D1 * t141 * x1 - t147 + 0.2D1 * t148 * t118 + 0.4D1 * t
     #151 * x1 + 0.4D1 * t151 * z + t158 - 0.2D1 * t159 * z + t159 * t12
     # + t145 * t52 - 0.3D1 * t159 * x2 + 0.6D1 * t166 * z + 0.8D1 * t15
     #6 * x1 - 0.2D1 * t171 * z - 0.3D1 * t171 * x2 + t171 * t12 + 0.4D1
     # * t145 * t107 - 0.2D1 * t52 * t144 * t22 - t159 * t182
      t195 = t118 * x1
      t198 = z * t29
      t199 = x3 * t118
      t204 = 0.3D1 * t109
      t206 = 0.2D1 * t109 * z
      t208 = 0.2D1 * t145
      t209 = 0.5D1 * t156
      t211 = t144 * z
      t215 = -0.11D2 * t156 * t107 + 0.3D1 * t141 * t127 + 0.6D1 * t171 
     #* t22 - 0.3D1 * t171 * t182 - 0.4D1 * t151 * t107 - 0.2D1 * t148 *
     # t195 - 0.2D1 * t198 * t199 + t145 * t52 * t12 + t204 + t159 + t17
     #1 - t206 - 0.4D1 * t166 + t208 - t209 - 0.4D1 * t151 + t211 + 0.2D
     #1 * t198 * t199 * x1 - t144
      t217 = (t184 + t215) ** 2
      t222 = (-0.1D1 + x1 - t107 + x2 - t127 - t22 + t128) ** 2
      t223 = 0.1D1 / t222
      t226 = x3 * t12
      t228 = x3 * t52
      t234 = t118 * z
      t237 = 0.1D1 + 0.4D1 * t108 - t226 * t127 - 0.2D1 * t228 * t22 + t
     #228 * t182 + t120 - 0.2D1 * t110 * t195 - x1 - x2 - x3 + t107 - 0.
     #2D1 * t110 * t234
      t243 = 0.2D1 * t7 * z
      t246 = x3 * z
      t248 = 0.2D1 * t110 * t234 * x1 - t128 - 0.2D1 * t104 - t243 - 0.3
     #D1 * t106 + t226 * x1 + t105 + t22 + t127 + t246 + t103 + t228 * x
     #2
      t250 = (t237 + t248) ** 2
      t253 = 0.1D1 / t250 * t42 * t46
      t257 = FJET(XB1, XB2, s, t2 * t102 * (-x3 + t103 - t104 + t105 - t
     #106 + t108 - x2 + t120) * t123, t2 * t103, -t2 * t102 * t129 * t12
     #3, -t2 * x1 * t113 * t115, -t140, t6 * t102 * t217 * t111 * t1 * t
     #223 * t253 / 0.2D1)
      t268 = t31 * t113 * t115
      t269 = Sqrt(t268)
      t271 = 0.2D1 * t110 * t269
      t280 = log(0.4D1 * t268 * x3 * t10 * t16)
      t283 = t29 * t269
      t292 = -t204 + t147 + t144 - t158 - 0.4D1 * t283 * z - 0.2D1 * t14
     #8 * t269 + 0.2D1 * t198 * x3 * t269 - t211 + t206 - t208 + 0.4D1 *
     # t283 + t209
      t293 = t292 ** 2
      t294 = 0.1D1 / t24 * t293
      t299 = (-t271 - 0.1D1 - t246 + x3 - t105 + 0.2D1 * t110 * t269 * z
     # + t243 - t22 + x2) ** 2
      t301 = 0.1D1 / t299 * t1
      t302 = t294 * t301
      t314 = -(0.360D3 * t6 * t280 * t302 + 0.720D3 * t38 * t302) * t42 
     #/ 0.1440D4 + t6 * t294 * t301 * t47 / 0.2D1
      t315 = FJET(XB1, XB2, s, -t2 * (-x3 + t105 - x2 + t271), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t105 + t271), 0.0D0, 0.0D0, t314)
      t324 = t222 ** 2
      t325 = 0.1D1 / t324
      t328 = Sqrt(-t112 * x2)
      t329 = t328 ** 2
      t333 = t102 ** 2
      t334 = t333 * t102
      t340 = t6 * t75
      t348 = log(-0.4D1 * t53 * t10 * t13 * t15 * t123 * t333 * t4)
      t351 = t329 * t111 * t334
      t362 = -0.8D1 * t6 * t27 * t325 * t329 * t111 * t30 * t334 * t42 *
     # t46 - (-0.5760D4 * t340 * t348 * t325 * t351 - 0.11520D5 * t340 *
     # lh * t325 * t351) * t46 / 0.720D3
      t363 = FJET(XB1, XB2, s, -t2 * t102 * x2 * t123, 0.0D0, t4 * s * t
     #1 * t102, t2 * x1, -t140, t362)
      rrgq2qght13s1e0 = t100 * t99 + t257 * wd * z * t102 * t217 * t111 
     #* t1 * t223 * t253 / 0.2D1 + t315 * t314 + t363 * t362

      end function



      doubleprecision function rrgq2qght13s1em1
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
      t4 = -0.1D1 + x2
      t6 = wd * z
      t7 = x2 * z
      t9 = (0.1D1 - x2 + t7) ** 2
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t12 = t6 * t11
      t13 = t1 ** 2
      t14 = t13 * t1
      t15 = x4 * pi
      t16 = cos(t15)
      t17 = t16 ** 2
      t18 = t14 * t17
      t19 = t4 * x2
      t20 = Sqrt(-t19)
      t21 = t20 ** 2
      t22 = t18 * t21
      t31 = Sin(t15)
      t32 = t31 ** 2
      t34 = z ** 2
      t36 = t13 ** 2
      t41 = log(-0.4D1 * x2 * t32 / t34 * t36 * t4)
      t48 = 0.1D1 / x1
      t53 = 0.1D1 / x3
      t58 = 0.4D1 * t12 * t22 + 0.8D1 * t6 * lh * t11 * t14 * t17 * t21 
     #- 0.4D1 * (wd - t41 * wd) * t11 * z * t22 - 0.8D1 * t12 * t18 * t2
     #1 * t48 - 0.4D1 * t12 * t18 * t21 * t53
      t59 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t5
     #8)
      t61 = -0.1D1 + x1
      t63 = x1 * z
      t64 = 0.1D1 - x1 + t63
      t65 = 0.1D1 / t64
      t77 = x2 * x1
      t80 = (-0.1D1 + x1 - t63 + x2 - t77 - t7 + t77 * z) ** 2
      t81 = t80 ** 2
      t82 = 0.1D1 / t81
      t87 = Sqrt(-t4 * t64 * x2)
      t88 = t87 ** 2
      t90 = t61 ** 2
      t94 = t88 * t64 * t17 * t90 * t61 * t48
      t97 = FJET(XB1, XB2, s, -t2 * t61 * x2 * t65, 0.0D0, t4 * s * t1 *
     # t61, t2 * x1, -s * t13 * x2 * x1 * t61 * t65, -0.8D1 * t6 * t14 *
     # t82 * t94)
      t104 = x2 * x3
      t105 = 0.2D1 * t104
      t106 = sqrt(x3)
      t107 = t16 * t106
      t112 = Sqrt(t19 * (-0.1D1 + t106) * (t106 + 0.1D1))
      t114 = 0.2D1 * t107 * t112
      t119 = 0.1D1 / t9
      t122 = t106 * x3
      t123 = t122 * x2
      t126 = t106 * x2
      t129 = t16 * t112
      t145 = -0.3D1 * t106 + 0.2D1 * t123 * z + t122 - 0.5D1 * t126 * z 
     #- 0.4D1 * t129 * z - 0.2D1 * t16 * x3 * t112 + 0.2D1 * z * t16 * x
     #3 * t112 - t122 * z + 0.2D1 * t106 * z - 0.2D1 * t123 + 0.4D1 * t1
     #29 + 0.5D1 * t126
      t146 = t145 ** 2
      t154 = (-t114 - 0.1D1 - x3 * z + x3 - t105 + 0.2D1 * t107 * t112 *
     # z + 0.2D1 * t104 * z - t7 + x2) ** 2
      t158 = t146 / t154 * t1 * t53
      t161 = FJET(XB1, XB2, s, -t2 * (-x3 + t105 - x2 + t114), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t105 + t114), 0.0D0, 0.0D0, t6 * t119 * t158
     # / 0.4D1)
      rrgq2qght13s1em1 = t59 * t58 - 0.8D1 * t97 * wd * z * t14 * t82 * 
     #t94 + t161 * wd * z * t119 * t158 / 0.4D1

      end function



      doubleprecision function rrgq2qght13s1em2
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
      t4 = -0.1D1 + x2
      t9 = (0.1D1 - x2 + x2 * z) ** 2
      t10 = t9 ** 2
      t11 = 0.1D1 / t10
      t13 = t1 ** 2
      t14 = t13 * t1
      t16 = cos(x4 * pi)
      t17 = t16 ** 2
      t20 = Sqrt(-t4 * x2)
      t21 = t20 ** 2
      t25 = FJET(XB1, XB2, s, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.4D1 * wd * z * t11 * t14 * t17 * t21)
      rrgq2qght13s1em2 = -0.4D1 * t25 * wd * z * t11 * t14 * t17 * t21

      end function



      doubleprecision function rrgq2qght13s1em3
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght13s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght13s1em4
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght13s1em4 = 0.0D0

      end function
