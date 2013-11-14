  
      subroutine rrqg2qght13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght13s1e1  
      doubleprecision rrqg2qght13s1e0  
      doubleprecision rrqg2qght13s1em1  
      doubleprecision rrqg2qght13s1em2  
      doubleprecision rrqg2qght13s1em3  
      doubleprecision rrqg2qght13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght13s1e1
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
      t6 = z * wd
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
      t21 = t20 ** 2
      t23 = cos(t8)
      t24 = t23 ** 2
      t25 = t4 * x2
      t26 = Sqrt(-t25)
      t27 = t26 ** 2
      t28 = t24 * t27
      t29 = t14 * t1
      t30 = x2 * z
      t32 = (t30 - x2 + 0.1D1) ** 2
      t33 = t32 ** 2
      t34 = 0.1D1 / t33
      t36 = t28 * t29 * t34
      t43 = lh ** 2
      t45 = pi ** 2
      t47 = 0.180D3 * t43 - 0.30D2 * t45
      t48 = t6 * t47
      t49 = t48 * t36
      t52 = 0.1D1 / x3
      t56 = z * t34
      t67 = x2 * t10
      t70 = log(-0.4D1 * t67 * t17)
      t71 = t70 ** 2
      t81 = t27 * t29
      t93 = x1 ** 2
      t94 = x3 * t93
      t95 = t94 * x2
      t96 = t10 * t13
      t97 = t15 * t4
      t101 = log(-0.4D1 * t95 * t96 * t97)
      t105 = t6 * lh
      t110 = 0.1D1 / x1
      t113 = t24 * t29
      t115 = x2 * t93
      t116 = t115 * t10
      t119 = log(-0.4D1 * t116 * t17)
      t120 = t119 ** 2
      t122 = t27 * t34
      t126 = t113 * t6
      t135 = (-0.720D3 * t6 * t21 * t36 - 0.2880D4 * t6 * lh * t20 * t36
     # - 0.16D2 * t49) * t52 / 0.360D3 - 0.2D1 / 0.45D2 * t28 * t29 * t5
     #6 * wd * (0.60D2 * lh * t45 - 0.240D3 * zeta3 - 0.120D3 * t43 * lh
     #) + 0.2D1 / 0.3D1 * t71 * t70 * t24 * t27 * t29 * z * t34 * wd + 0
     #.2D1 / 0.45D2 * t70 * t24 * t81 * t56 * wd * t47 + 0.4D1 * t71 * t
     #24 * t81 * t56 * wd * lh - (-0.5760D4 * t6 * t101 * t36 - 0.11520D
     #5 * t105 * t36) * t52 * t110 / 0.720D3 + (-0.2880D4 * t113 * z * w
     #d * t120 * t122 - 0.11520D5 * t126 * lh * t119 * t122 - 0.64D2 * t
     #49) * t110 / 0.720D3
      t136 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t
     #135)
      t138 = 0.2D1 * t7
      t139 = sqrt(x3)
      t140 = t23 * t139
      t141 = -0.1D1 + t139
      t142 = t139 + 0.1D1
      t143 = t141 * t142
      t145 = Sqrt(t25 * t143)
      t147 = 0.2D1 * t140 * t145
      t154 = t15 * t141 * t142 * t13
      t155 = t4 * x3
      t159 = log(0.4D1 * t154 * t67 * t155)
      t160 = t159 ** 2
      t163 = 0.2D1 * t7 * z
      t164 = z * t23
      t168 = x3 * z
      t170 = (-t30 - 0.1D1 + t163 - t147 + x2 + 0.2D1 * t164 * t139 * t1
     #45 - t168 + x3 - t138) ** 2
      t175 = 0.2D1 * t139 * z
      t176 = t139 * x3
      t177 = t176 * z
      t178 = 0.3D1 * t139
      t179 = t139 * x2
      t180 = 0.5D1 * t179
      t181 = t176 * x2
      t182 = 0.2D1 * t181
      t186 = 0.5D1 * t179 * z
      t188 = 0.2D1 * t181 * z
      t191 = t23 * x3
      t197 = t175 - t177 - t178 + t176 + t180 - t182 + 0.4D1 * t23 * t14
     #5 - t186 + t188 - 0.4D1 * t164 * t145 - 0.2D1 * t191 * t145 + 0.2D
     #1 * t164 * x3 * t145
      t198 = t197 ** 2
      t200 = 0.1D1 / t170 * t1 / t32 * t198
      t211 = t93 * t10
      t216 = log(0.4D1 * t154 * t211 * t25 * x3)
      t226 = (0.45D2 * t6 * t160 * t200 + 0.180D3 * t6 * lh * t159 * t20
     #0 + t48 * t200) * t52 / 0.360D3 - (0.360D3 * t6 * t216 * t200 + 0.
     #720D3 * t105 * t200) * t52 * t110 / 0.720D3
      t227 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t138 - x2 + t147), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t138 + t147), 0.0D0, t226)
      t229 = -0.1D1 + x1
      t231 = x1 * z
      t232 = 0.1D1 - x1 + t231
      t233 = 0.1D1 / t232
      t238 = t1 * t229
      t244 = s * t14 * x2 * t229 * x1 * t233
      t246 = t229 ** 2
      t247 = t233 * t246
      t252 = log(-0.4D1 * t7 * t211 * t16 * t247 * t4)
      t255 = x2 * x1
      t256 = t255 * z
      t258 = (-t30 + t256 - t231 + x2 - t255 - 0.1D1 + x1) ** 2
      t259 = t258 ** 2
      t260 = 0.1D1 / t259
      t262 = t4 * t232
      t264 = Sqrt(-t262 * x2)
      t265 = t264 ** 2
      t266 = t246 * t229
      t269 = t24 * t260 * t265 * t266 * t29
      t273 = t6 * lh * t232
      t283 = log(-0.4D1 * t115 * t96 * t97 * t247)
      t284 = t283 ** 2
      t287 = t260 * t265 * t266
      t303 = -(-0.5760D4 * t6 * t252 * t232 * t269 - 0.11520D5 * t273 * 
     #t269) * t52 * t110 / 0.720D3 + (-0.2880D4 * t126 * t284 * t232 * t
     #287 - 0.11520D5 * t113 * t105 * t283 * t232 * t287 - 0.64D2 * t126
     # * t47 * t232 * t287) * t110 / 0.720D3
      t304 = FJET(XB1, XB2, s, 0.0D0, -t2 * t229 * x2 * t233, t2 * x1, t
     #4 * s * t238, -t244, t303)
      t306 = x3 * x1
      t308 = t306 * z
      t309 = t7 * x1
      t310 = t7 * t231
      t314 = Sqrt(t262 * x2 * t141 * t142)
      t316 = 0.2D1 * t140 * t314
      t324 = 0.1D1 - x1 + t231 - x2 + t255 - t256 - x3 + t306 - t308 + t
     #138 - t309 + t310 + t316
      t335 = log(0.4D1 * t233 * t15 * t143 * t13 * t116 * t155 * t246)
      t342 = t139 * t12
      t345 = t139 * t93
      t348 = t12 * x2
      t354 = x1 * t176
      t358 = t23 * t314
      t360 = t139 * x1
      t362 = -t175 + t177 - t180 + t182 + t181 * t93 * t12 - 0.11D2 * t1
     #79 * t231 + 0.3D1 * t342 * t255 + 0.6D1 * t345 * t30 - 0.3D1 * t34
     #5 * t348 - 0.2D1 * t93 * t176 * t30 - t354 * t348 + 0.4D1 * t181 *
     # t231 + t186 - t188 - 0.4D1 * t358 + t345 - 0.4D1 * t360 + t354
      t363 = x3 * t314
      t367 = t314 * x1
      t397 = 0.2D1 * t164 * t363 * x1 - t176 - 0.4D1 * t164 * t367 - 0.2
     #D1 * t191 * t367 - 0.2D1 * t164 * t363 + t178 + 0.4D1 * t358 * x1 
     #+ 0.4D1 * t164 * t314 - 0.2D1 * t342 * x1 + 0.6D1 * t360 * z + 0.8
     #D1 * t179 * x1 - 0.3D1 * t345 * x2 - 0.2D1 * t345 * z + t345 * t12
     # + 0.2D1 * t191 * t314 - 0.2D1 * t354 * z + t354 * t12 + t181 * t9
     #3 - 0.3D1 * t354 * x2
      t399 = (t362 + t397) ** 2
      t404 = x3 * t12
      t406 = 0.1D1 - t256 - 0.2D1 * t308 - t163 - 0.3D1 * t309 + t95 - x
     #1 + t138 + t231 + t404 * x1 + t30 + t255
      t414 = t139 * t314
      t420 = t168 + t306 - x2 - x3 + t316 - 0.2D1 * t140 * t367 + 0.4D1 
     #* t310 - t404 * t255 - 0.2D1 * t94 * t30 + t94 * t348 - 0.2D1 * t1
     #64 * t414 + 0.2D1 * t164 * t414 * x1
      t422 = (t406 + t420) ** 2
      t425 = t399 / t258 * t238 / t422
      t430 = 0.360D3 * t6 * t335 * t232 * t425 + 0.720D3 * t273 * t425
      t434 = FJET(XB1, XB2, s, t2 * t306, t2 * t229 * (-x3 + t306 - t308
     # + t138 - t309 + t310 - x2 + t316) * t233, -t2 * x1 * t141 * t142,
     # -t2 * t229 * t324 * t233, -t244, -t430 * t52 * t110 / 0.720D3)
      rrqg2qght13s1e1 = t136 * t135 + t227 * t226 + t304 * t303 - t434 *
     # t430 * t52 * t110 / 0.720D3

      end function



      doubleprecision function rrqg2qght13s1e0
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
      t6 = z * wd
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t17 = t13 * t15 * t4
      t20 = log(-0.4D1 * t7 * t10 * t17)
      t22 = cos(t8)
      t23 = t22 ** 2
      t24 = t4 * x2
      t25 = Sqrt(-t24)
      t26 = t25 ** 2
      t27 = t23 * t26
      t28 = t14 * t1
      t29 = x2 * z
      t31 = (t29 - x2 + 0.1D1) ** 2
      t32 = t31 ** 2
      t33 = 0.1D1 / t32
      t34 = t28 * t33
      t35 = t27 * t34
      t38 = t6 * lh
      t39 = t38 * t35
      t42 = 0.1D1 / x3
      t46 = 0.1D1 / x1
      t47 = t42 * t46
      t51 = t23 * t28
      t53 = x1 ** 2
      t54 = x2 * t53
      t58 = log(-0.4D1 * t54 * t10 * t17)
      t68 = x2 * t10
      t71 = log(-0.4D1 * t68 * t17)
      t80 = t71 ** 2
      t88 = lh ** 2
      t90 = pi ** 2
      t96 = (0.1440D4 * t6 * t20 * t35 + 0.2880D4 * t39) * t42 / 0.360D3
     # - 0.8D1 * t6 * t27 * t34 * t47 + (0.5760D4 * t51 * z * wd * t58 *
     # t26 * t33 + 0.11520D5 * t39) * t46 / 0.720D3 - 0.8D1 * t71 * t23 
     #* t26 * t28 * z * t33 * wd * lh - 0.2D1 * t80 * t23 * t26 * t28 * 
     #z * t33 * wd - 0.2D1 / 0.45D2 * t6 * (0.180D3 * t88 - 0.30D2 * t90
     #) * t35
      t97 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t9
     #6)
      t99 = 0.2D1 * t7
      t100 = sqrt(x3)
      t101 = t22 * t100
      t102 = -0.1D1 + t100
      t103 = t100 + 0.1D1
      t106 = Sqrt(t24 * t102 * t103)
      t108 = 0.2D1 * t101 * t106
      t120 = log(0.4D1 * t15 * t102 * t103 * t13 * t68 * t4 * x3)
      t123 = 0.2D1 * t7 * z
      t124 = z * t22
      t128 = x3 * z
      t130 = (-t29 - 0.1D1 + t123 - t108 + x2 + 0.2D1 * t124 * t100 * t1
     #06 - t128 + x3 - t99) ** 2
      t132 = 0.1D1 / t130 * t1
      t135 = 0.2D1 * t100 * z
      t136 = t100 * x3
      t137 = t136 * z
      t138 = 0.3D1 * t100
      t139 = t100 * x2
      t140 = 0.5D1 * t139
      t141 = t136 * x2
      t142 = 0.2D1 * t141
      t146 = 0.5D1 * t139 * z
      t148 = 0.2D1 * t141 * z
      t151 = t22 * x3
      t157 = t135 - t137 - t138 + t136 + t140 - t142 + 0.4D1 * t22 * t10
     #6 - t146 + t148 - 0.4D1 * t124 * t106 - 0.2D1 * t151 * t106 + 0.2D
     #1 * t124 * x3 * t106
      t158 = t157 ** 2
      t159 = 0.1D1 / t31 * t158
      t160 = t132 * t159
      t172 = (-0.90D2 * t6 * t120 * t160 - 0.180D3 * t38 * t160) * t42 /
     # 0.360D3 + t6 * t132 * t159 * t47 / 0.2D1
      t173 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t99 - x2 + t108), 0.0
     #D0, t2 * (0.1D1 - x2 - x3 + t99 + t108), 0.0D0, t172)
      t175 = -0.1D1 + x1
      t177 = x1 * z
      t178 = 0.1D1 - x1 + t177
      t179 = 0.1D1 / t178
      t184 = t1 * t175
      t190 = s * t14 * x2 * t175 * x1 * t179
      t192 = x2 * x1
      t193 = t192 * z
      t195 = (-t29 + t193 - t177 + x2 - t192 - 0.1D1 + x1) ** 2
      t196 = t195 ** 2
      t197 = 0.1D1 / t196
      t200 = t4 * t178
      t202 = Sqrt(-t200 * x2)
      t203 = t202 ** 2
      t204 = t175 ** 2
      t205 = t204 * t175
      t206 = t203 * t205
      t220 = log(-0.4D1 * t54 * t10 * t13 * t15 * t4 * t204 * t179)
      t237 = -0.8D1 * t6 * t178 * t23 * t197 * t206 * t28 * t42 * t46 + 
     #(0.5760D4 * t51 * t6 * t220 * t178 * t197 * t203 * t205 + 0.11520D
     #5 * t6 * lh * t178 * t23 * t197 * t206 * t28) * t46 / 0.720D3
      t238 = FJET(XB1, XB2, s, 0.0D0, -t2 * t175 * x2 * t179, t2 * x1, t
     #4 * s * t184, -t190, t237)
      t240 = x3 * x1
      t242 = t240 * z
      t243 = t7 * x1
      t244 = t7 * t177
      t248 = Sqrt(t200 * x2 * t102 * t103)
      t250 = 0.2D1 * t101 * t248
      t258 = 0.1D1 - x1 + t177 - x2 + t192 - t193 - x3 + t240 - t242 + t
     #99 - t243 + t244 + t250
      t264 = t22 * t248
      t269 = t100 * t12
      t272 = t100 * x1
      t277 = t100 * t53
      t283 = t136 * x1
      t292 = 0.2D1 * t151 * t248 + 0.4D1 * t264 * x1 + 0.4D1 * t124 * t2
     #48 - 0.2D1 * t269 * x1 + 0.6D1 * t272 * z + 0.8D1 * t139 * x1 - 0.
     #3D1 * t277 * x2 - 0.2D1 * t277 * z + t277 * t12 - 0.2D1 * t283 * z
     # + t283 * t12 + t141 * t53 - 0.3D1 * t283 * x2 + t146 - t148 - 0.4
     #D1 * t264 + t277 - 0.4D1 * t272
      t293 = x3 * t248
      t305 = t12 * x2
      t314 = t248 * x1
      t321 = t283 - t135 + t137 - t140 + t142 + 0.2D1 * t124 * t293 * x1
     # - t136 + t138 + t141 * t53 * t12 - 0.11D2 * t139 * t177 + 0.3D1 *
     # t269 * t192 + 0.6D1 * t277 * t29 - 0.3D1 * t277 * t305 - 0.2D1 * 
     #t53 * t136 * t29 - t283 * t305 + 0.4D1 * t141 * t177 - 0.4D1 * t12
     #4 * t314 - 0.2D1 * t151 * t314 - 0.2D1 * t124 * t293
      t323 = (t292 + t321) ** 2
      t325 = 0.1D1 / t195
      t330 = x3 * t53
      t332 = x3 * t12
      t334 = 0.1D1 - t193 - 0.2D1 * t242 - t123 - 0.3D1 * t243 + t330 * 
     #x2 - x1 + t99 + t177 + t332 * x1 + t29 + t192
      t342 = t100 * t248
      t348 = t128 + t240 - x2 - x3 + t250 - 0.2D1 * t101 * t314 + 0.4D1 
     #* t244 - t332 * t192 - 0.2D1 * t330 * t29 + t330 * t305 - 0.2D1 * 
     #t124 * t342 + 0.2D1 * t124 * t342 * x1
      t350 = (t334 + t348) ** 2
      t353 = 0.1D1 / t350 * t42 * t46
      t357 = FJET(XB1, XB2, s, t2 * t240, t2 * t175 * (-x3 + t240 - t242
     # + t99 - t243 + t244 - x2 + t250) * t179, -t2 * x1 * t102 * t103, 
     #-t2 * t175 * t258 * t179, -t190, t6 * t178 * t323 * t325 * t184 * 
     #t353 / 0.2D1)
      rrqg2qght13s1e0 = t97 * t96 + t173 * t172 + t238 * t237 + t357 * z
     # * wd * t178 * t323 * t325 * t1 * t175 * t353 / 0.2D1

      end function



      doubleprecision function rrqg2qght13s1em1
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
      t6 = z * wd
      t7 = x4 * pi
      t8 = cos(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = t4 * x2
      t12 = Sqrt(-t11)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 * t1
      t16 = t13 * t15
      t17 = x2 * z
      t19 = (t17 - x2 + 0.1D1) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 / x1
      t33 = Sin(t7)
      t34 = t33 ** 2
      t36 = z ** 2
      t38 = t14 ** 2
      t43 = log(-0.4D1 * x2 * t34 / t36 * t38 * t4)
      t46 = t15 * z
      t51 = 0.1D1 / x3
      t56 = -0.8D1 * t10 * t16 * t21 * t22 + 0.8D1 * t6 * lh * t9 * t13 
     #* t15 * t21 + 0.4D1 * t43 * t9 * t13 * t46 * t21 * wd - 0.4D1 * t1
     #0 * t16 * t21 * t51
      t57 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t5
     #6)
      t59 = -0.1D1 + x1
      t61 = x1 * z
      t62 = 0.1D1 - x1 + t61
      t63 = 0.1D1 / t62
      t77 = x2 * x1
      t80 = (-t17 + t77 * z - t61 + x2 - t77 - 0.1D1 + x1) ** 2
      t81 = t80 ** 2
      t86 = Sqrt(-t4 * t62 * x2)
      t87 = t86 ** 2
      t88 = t59 ** 2
      t92 = t62 / t81 * t87 * t88 * t59 * t22
      t95 = FJET(XB1, XB2, s, 0.0D0, -t2 * t59 * x2 * t63, t2 * x1, t4 *
     # s * t1 * t59, -s * t14 * x2 * t59 * x1 * t63, -0.8D1 * t9 * t15 *
     # t6 * t92)
      t101 = x2 * x3
      t102 = 0.2D1 * t101
      t103 = sqrt(x3)
      t109 = Sqrt(t11 * (-0.1D1 + t103) * (t103 + 0.1D1))
      t111 = 0.2D1 * t8 * t103 * t109
      t118 = z * t8
      t124 = (-t17 - 0.1D1 + 0.2D1 * t101 * z - t111 + x2 + 0.2D1 * t118
     # * t103 * t109 - x3 * z + x3 - t102) ** 2
      t125 = 0.1D1 / t124
      t131 = t103 * x3
      t134 = t103 * x2
      t136 = t131 * x2
      t152 = 0.2D1 * t103 * z - t131 * z - 0.3D1 * t103 + t131 + 0.5D1 *
     # t134 - 0.2D1 * t136 + 0.4D1 * t8 * t109 - 0.5D1 * t134 * z + 0.2D
     #1 * t136 * z - 0.4D1 * t118 * t109 - 0.2D1 * t8 * x3 * t109 + 0.2D
     #1 * t118 * x3 * t109
      t153 = t152 ** 2
      t155 = t1 / t19 * t153 * t51
      t158 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t102 - x2 + t111), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t102 + t111), 0.0D0, t6 * t125 * t155
     # / 0.4D1)
      rrqg2qght13s1em1 = t57 * t56 - 0.8D1 * t95 * t9 * t46 * wd * t92 +
     # t158 * z * wd * t125 * t155 / 0.4D1

      end function



      doubleprecision function rrqg2qght13s1em2
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
      t8 = cos(x4 * pi)
      t9 = t8 ** 2
      t12 = Sqrt(-t4 * x2)
      t13 = t12 ** 2
      t14 = t1 ** 2
      t15 = t14 * t1
      t19 = (x2 * z - x2 + 0.1D1) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t25 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, -0
     #.4D1 * z * wd * t9 * t13 * t15 * t21)
      rrqg2qght13s1em2 = -0.4D1 * t25 * z * wd * t9 * t13 * t15 * t21

      end function



      doubleprecision function rrqg2qght13s1em3
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
      rrqg2qght13s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght13s1em4
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
      rrqg2qght13s1em4 = 0.0D0

      end function
