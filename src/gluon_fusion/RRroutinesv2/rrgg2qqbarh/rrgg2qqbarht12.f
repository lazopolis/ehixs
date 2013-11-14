  
      subroutine rrgg2qqbarht12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarht12s1e1  
      doubleprecision rrgg2qqbarht12s1e0  
      doubleprecision rrgg2qqbarht12s1em1  
      doubleprecision rrgg2qqbarht12s1em2  
      doubleprecision rrgg2qqbarht12s1em3  
      doubleprecision rrgg2qqbarht12s1em4  
      doubleprecision rrgg2qqbarht12s2e1  
      doubleprecision rrgg2qqbarht12s2e0  
      doubleprecision rrgg2qqbarht12s2em1  
      doubleprecision rrgg2qqbarht12s2em2  
      doubleprecision rrgg2qqbarht12s2em3  
      doubleprecision rrgg2qqbarht12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht12s1e1
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
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = t6 * z
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t15 = t13 * x3 * t4
      t18 = log(-0.4D1 * x2 * t10 * t15)
      t19 = t18 ** 2
      t20 = cos(t8)
      t21 = t20 ** 2
      t23 = x3 * t4
      t24 = Sqrt(-t23)
      t25 = t24 ** 2
      t30 = t6 * lh
      t31 = 0.180D3 * t30
      t34 = (-t31 - 0.90D2 * t6) * z
      t35 = 0.180D3 * t7 + t34
      t37 = t21 * t25
      t42 = lh ** 2
      t44 = pi ** 2
      t46 = 0.180D3 * t42 - 0.30D2 * t44
      t47 = t6 * t46
      t50 = 0.270D3 * t7 + 0.2D1 * t34 + (t31 + t47) * z
      t55 = 0.1D1 / x2
      t60 = t37 * t6
      t62 = t10 * t13
      t65 = log(-0.4D1 * t62 * t23)
      t68 = t25 * wd * nf
      t69 = t65 * t21 * t68
      t70 = 0.90D2 * t69
      t74 = -t60 - t69
      t77 = t65 ** 2
      t79 = t77 * t21 * t68
      t80 = 0.45D2 * t79
      t107 = x1 ** 2
      t108 = x2 * t107
      t112 = log(-0.4D1 * t108 * t10 * t15)
      t122 = 0.1D1 / x1
      t125 = t21 * wd
      t126 = t125 * nf
      t127 = x3 * t107
      t131 = log(-0.4D1 * t127 * t62 * t4)
      t132 = t131 ** 2
      t138 = t125 * nf * z
      t142 = 0.180D3 * t125 * nf * lh
      t145 = (-t142 - 0.90D2 * t126) * z
      t146 = 0.180D3 * t138 + t145
      t155 = 0.270D3 * t138 + 0.2D1 * t145 + (t142 + t125 * nf * t46) * 
     #z
      t160 = -(0.720D3 * t7 * t19 * t21 * t25 - 0.16D2 * t35 * t18 * t37
     # + 0.16D2 * t50 * t21 * t25) * t55 / 0.960D3 - (-0.180D3 * t37 * t
     #30 - 0.90D2 * t60 - t70) * z / 0.20D2 - (-0.180D3 * t74 * lh + t70
     # + t80 + t37 * t47) * z / 0.30D2 - (-0.180D3 * (t69 + t79 / 0.2D1)
     # * lh + t37 * t6 * (0.60D2 * lh * t44 - 0.240D3 * zeta3 - 0.120D3 
     #* t42 * lh) - t80 - 0.15D2 * t77 * t65 * t21 * t68 + t74 * t46) * 
     #z / 0.60D2 - 0.6D1 * t37 * t7 - (-0.1440D4 * t7 * t112 * t21 * t25
     # + 0.16D2 * t35 * t21 * t25) * t55 * t122 / 0.480D3 - (0.45D2 * t1
     #26 * z * t132 * t25 - t146 * t131 * t25 + t155 * t25) * t122 / 0.3
     #0D2
      t161 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #160)
      t163 = -0.1D1 + x1
      t164 = x3 * x1
      t165 = t164 * z
      t166 = x2 * x3
      t167 = 0.2D1 * t166
      t168 = t166 * x1
      t169 = x1 * z
      t170 = t166 * t169
      t171 = sqrt(x2)
      t172 = t20 * t171
      t173 = -0.1D1 + t171
      t174 = x3 * t173
      t175 = t171 + 0.1D1
      t176 = 0.1D1 - x1 + t169
      t180 = Sqrt(t174 * t175 * t176 * t4)
      t182 = 0.2D1 * t172 * t180
      t185 = 0.1D1 / t176
      t188 = t2 * t164
      t189 = x2 * x1
      t190 = t189 * z
      t191 = 0.1D1 - x1 + t169 - x2 + t189 - t190 - x3 + t164 - t165 + t
     #167 - t168 + t170 + t182
      t195 = t4 * s
      t197 = t195 * t1 * x1
      t198 = t1 ** 2
      t204 = t163 ** 2
      t215 = log(0.4D1 * t204 * t107 * t185 * x2 * t13 * t10 * t173 * t1
     #75 * x3 * t4)
      t217 = x2 * z
      t219 = (-t217 + t190 - t169 - 0.1D1 + x1 + x2 - t189) ** 2
      t221 = t20 * x2
      t222 = t180 * z
      t226 = 0.3D1 * t171
      t227 = t12 * t171
      t230 = t171 * x2
      t231 = t230 * x3
      t238 = t12 * t107
      t244 = t20 * t180
      t251 = x3 * t171
      t254 = t171 * z
      t258 = t171 * x1
      t262 = 0.4D1 * t231 * z
      t264 = t107 * t171
      t271 = t107 * t230
      t274 = 0.4D1 * t221 * t222 * x1 + t226 - 0.4D1 * t127 * t227 - 0.2
     #D1 * t231 * t12 * x1 - 0.4D1 * t231 * t107 * z + 0.2D1 * t231 * t2
     #38 - 0.4D1 * t221 * t180 * x1 - 0.4D1 * t244 * t169 - 0.4D1 * t221
     # * t222 + 0.8D1 * t231 * t169 - 0.12D2 * t251 * t169 + 0.8D1 * t12
     #7 * t254 + 0.2D1 * x3 * t12 * t258 - t262 - t227 * x1 - 0.4D1 * t2
     #64 * z + 0.2D1 * t231 * t107 - 0.6D1 * t231 * x1 + 0.2D1 * t271 * 
     #z
      t275 = x1 * t230
      t285 = 0.2D1 * t251 * z
      t288 = t230 * z
      t295 = 0.2D1 * t230
      t298 = 0.4D1 * t231
      t299 = 0.2D1 * t288
      t300 = 0.6D1 * t251
      t303 = t275 * t12 - t271 * t12 + 0.2D1 * t238 * t171 - 0.4D1 * t12
     #7 * t171 + 0.6D1 * t258 * z + t285 + 0.10D2 * t251 * x1 - 0.4D1 * 
     #t288 * x1 + 0.4D1 * t244 * x1 + 0.4D1 * t221 * t180 - t295 + 0.2D1
     # * t264 - 0.4D1 * t244 + t298 + t299 - t300 + 0.3D1 * t275 - t271 
     #- 0.5D1 * t258 - t254
      t305 = (t274 + t303) ** 2
      t306 = 0.1D1 / t219 * t305
      t312 = -0.90D2 * t7 * t215 * t185 * t306 + t35 * t185 * t306
      t316 = FJET(XB1, XB2, s, t2 * t163 * (-x3 + t164 - t165 + t167 - t
     #168 + t170 - x2 + t182) * t185, t188, -t2 * t163 * t191 * t185, -t
     #197, -s * t198 * x2 * x1 * t163 * t185, -t312 * t55 * t122 / 0.480
     #D3)
      t322 = t174 * t175 * t4
      t323 = Sqrt(t322)
      t325 = 0.2D1 * t172 * t323
      t334 = log(0.4D1 * x2 * t13 * t10 * t322)
      t335 = t334 ** 2
      t343 = t262 - 0.4D1 * t221 * t323 - t285 - t226 + t295 - t298 - t2
     #99 + 0.4D1 * t20 * t323 + t300 + t254 + 0.4D1 * t221 * t323 * z
      t344 = t343 ** 2
      t347 = (t217 + 0.1D1 - x2) ** 2
      t348 = 0.1D1 / t347
      t360 = t108 * t62
      t363 = log(0.4D1 * t360 * t322)
      t374 = -(-0.45D2 * t7 * t335 * t344 * t348 + t35 * t334 * t344 * t
     #348 - t50 * t344 * t348) * t55 / 0.960D3 - (0.90D2 * t7 * t363 * t
     #344 * t348 - t35 * t344 * t348) * t55 * t122 / 0.480D3
      t375 = FJET(XB1, XB2, s, -t2 * (-x3 + t167 - x2 + t325), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t167 + t325), 0.0D0, 0.0D0, t374)
      t378 = t1 * t163
      t382 = t23 * t185 * t204
      t385 = log(-0.4D1 * t360 * t382)
      t388 = Sqrt(-x3 * t176 * t4)
      t389 = t388 ** 2
      t391 = t21 * t185
      t406 = log(-0.4D1 * t107 * t10 * t13 * t382)
      t407 = t406 ** 2
      t409 = t389 * t185
      t420 = -(0.1440D4 * t7 * t385 * t389 * t391 - 0.16D2 * t35 * t389 
     #* t391) * t55 * t122 / 0.480D3 - (-0.45D2 * t126 * z * t407 * t409
     # + t146 * t406 * t409 - t155 * t389 * t185) * t122 / 0.30D2
      t421 = FJET(XB1, XB2, s, -x3 * s * t378, t188, t195 * t378, -t197,
     # 0.0D0, t420)
      rrgg2qqbarht12s1e1 = t161 * t160 - t316 * t312 * t55 * t122 / 0.48
     #0D3 + t375 * t374 + t421 * t420

      end function



      doubleprecision function rrgg2qqbarht12s1e0
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
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = t6 * z
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t18 = log(-0.4D1 * x2 * t10 * t13 * x3 * t4)
      t19 = cos(t8)
      t20 = t19 ** 2
      t22 = x3 * t4
      t23 = Sqrt(-t22)
      t24 = t23 ** 2
      t29 = t6 * lh
      t34 = 0.180D3 * t7 + (-0.180D3 * t29 - 0.90D2 * t6) * z
      t39 = 0.1D1 / x2
      t42 = t20 * t24
      t44 = nf * z
      t45 = 0.1D1 / x1
      t46 = t39 * t45
      t50 = t20 * wd
      t51 = t50 * nf
      t52 = x1 ** 2
      t53 = x3 * t52
      t54 = t10 * t13
      t58 = log(-0.4D1 * t53 * t54 * t4)
      t71 = 0.180D3 * t50 * t44 + (-0.180D3 * t50 * nf * lh - 0.90D2 * t
     #51) * z
      t80 = t42 * t6
      t84 = log(-0.4D1 * t54 * t22)
      t87 = t24 * wd * nf
      t88 = t84 * t20 * t87
      t89 = 0.90D2 * t88
      t96 = t84 ** 2
      t100 = lh ** 2
      t102 = pi ** 2
      t110 = -(-0.1440D4 * t7 * t18 * t20 * t24 + 0.16D2 * t34 * t20 * t
     #24) * t39 / 0.960D3 - 0.3D1 * t42 * wd * t44 * t46 - (-0.90D2 * t5
     #1 * z * t58 * t24 + t71 * t24) * t45 / 0.30D2 - 0.9D1 / 0.2D1 * t4
     #2 * t7 - (-0.180D3 * t42 * t29 - 0.90D2 * t80 - t89) * z / 0.30D2 
     #- (-0.180D3 * (-t80 - t88) * lh + t89 + 0.45D2 * t96 * t20 * t87 +
     # t42 * t6 * (0.180D3 * t100 - 0.30D2 * t102)) * z / 0.60D2
      t111 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #110)
      t113 = -0.1D1 + x1
      t114 = x3 * x1
      t115 = t114 * z
      t116 = x2 * x3
      t117 = 0.2D1 * t116
      t118 = t116 * x1
      t119 = x1 * z
      t120 = t116 * t119
      t121 = sqrt(x2)
      t122 = t19 * t121
      t124 = x3 * (-0.1D1 + t121)
      t125 = t121 + 0.1D1
      t126 = 0.1D1 - x1 + t119
      t130 = Sqrt(t124 * t125 * t126 * t4)
      t132 = 0.2D1 * t122 * t130
      t135 = 0.1D1 / t126
      t138 = t2 * t114
      t139 = x2 * x1
      t140 = t139 * z
      t141 = 0.1D1 - x1 + t119 - x2 + t139 - t140 - x3 + t114 - t115 + t
     #117 - t118 + t120 + t132
      t145 = t4 * s
      t147 = t145 * t1 * x1
      t148 = t1 ** 2
      t156 = x2 * z
      t158 = (-t156 + t140 - t119 - 0.1D1 + x1 + x2 - t139) ** 2
      t159 = 0.1D1 / t158
      t160 = t121 * x2
      t161 = t160 * x3
      t163 = 0.4D1 * t161 * z
      t164 = t12 * t121
      t166 = t52 * t121
      t173 = t52 * t160
      t176 = x1 * t160
      t179 = t52 * t12
      t184 = t121 * x1
      t187 = x3 * t121
      t189 = 0.2D1 * t187 * z
      t192 = t160 * z
      t195 = t19 * t130
      t198 = t19 * x2
      t201 = 0.2D1 * t160
      t202 = t130 * z
      t207 = -t163 - t164 * x1 - 0.4D1 * t166 * z + 0.2D1 * t161 * t52 -
     # 0.6D1 * t161 * x1 + 0.2D1 * t173 * z + t176 * t12 - t173 * t12 + 
     #0.2D1 * t179 * t121 - 0.4D1 * t53 * t121 + 0.6D1 * t184 * z + t189
     # + 0.10D2 * t187 * x1 - 0.4D1 * t192 * x1 + 0.4D1 * t195 * x1 + 0.
     #4D1 * t198 * t130 - t201 + 0.4D1 * t198 * t202 * x1 + 0.2D1 * t166
      t209 = 0.4D1 * t161
      t210 = 0.2D1 * t192
      t211 = 0.6D1 * t187
      t214 = t121 * z
      t241 = 0.3D1 * t121
      t242 = -0.4D1 * t195 + t209 + t210 - t211 + 0.3D1 * t176 - t173 - 
     #0.5D1 * t184 - t214 - 0.12D2 * t187 * t119 + 0.8D1 * t53 * t214 + 
     #0.2D1 * x3 * t12 * t184 - 0.4D1 * t53 * t164 - 0.2D1 * t161 * t12 
     #* x1 - 0.4D1 * t161 * t52 * z + 0.2D1 * t161 * t179 - 0.4D1 * t198
     # * t130 * x1 - 0.4D1 * t195 * t119 - 0.4D1 * t198 * t202 + 0.8D1 *
     # t161 * t119 + t241
      t244 = (t207 + t242) ** 2
      t249 = FJET(XB1, XB2, s, t2 * t113 * (-x3 + t114 - t115 + t117 - t
     #118 + t120 - x2 + t132) * t135, t138, -t2 * t113 * t141 * t135, -t
     #147, -s * t148 * x2 * x1 * t113 * t135, -0.3D1 / 0.16D2 * t6 * z *
     # t135 * t159 * t244 * t46)
      t259 = t124 * t125 * t4
      t260 = Sqrt(t259)
      t262 = 0.2D1 * t122 * t260
      t271 = log(0.4D1 * x2 * t13 * t10 * t259)
      t279 = t163 - 0.4D1 * t198 * t260 - t189 - t241 + t201 - t209 - t2
     #10 + 0.4D1 * t19 * t260 + t211 + t214 + 0.4D1 * t198 * t260 * z
      t280 = t279 ** 2
      t283 = (t156 + 0.1D1 - x2) ** 2
      t284 = 0.1D1 / t283
      t297 = -(0.90D2 * t7 * t271 * t280 * t284 - t34 * t280 * t284) * t
     #39 / 0.960D3 + 0.3D1 / 0.16D2 * t7 * t280 * t284 * t46
      t298 = FJET(XB1, XB2, s, -t2 * (-x3 + t117 - x2 + t262), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t117 + t262), 0.0D0, 0.0D0, t297)
      t301 = t1 * t113
      t306 = Sqrt(-x3 * t126 * t4)
      t307 = t306 ** 2
      t316 = t113 ** 2
      t321 = log(-0.4D1 * t52 * t10 * t13 * t135 * t316 * t22)
      t332 = 0.3D1 * t6 * z * t307 * t20 * t135 * t46 - (0.90D2 * t51 * 
     #z * t321 * t307 * t135 - t71 * t307 * t135) * t45 / 0.30D2
      t333 = FJET(XB1, XB2, s, -x3 * s * t301, t138, t145 * t301, -t147,
     # 0.0D0, t332)
      rrgg2qqbarht12s1e0 = t111 * t110 - 0.3D1 / 0.16D2 * t249 * wd * t4
     #4 * t135 * t159 * t244 * t39 * t45 + t298 * t297 + t333 * t332

      end function



      doubleprecision function rrgg2qqbarht12s1em1
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
      t4 = -0.1D1 + x3
      t6 = x4 * pi
      t7 = cos(t6)
      t8 = t7 ** 2
      t9 = x3 * t4
      t10 = Sqrt(-t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = t12 * wd
      t14 = nf * z
      t15 = 0.1D1 / x1
      t19 = wd * nf
      t20 = t19 * z
      t28 = Sin(t6)
      t29 = t28 ** 2
      t30 = z ** 2
      t35 = log(-0.4D1 * t29 / t30 * t9)
      t44 = 0.1D1 / x2
      t48 = -0.3D1 * t13 * t14 * t15 - 0.3D1 * t12 * t20 - (-0.180D3 * t
     #12 * t19 * lh - 0.90D2 * t12 * t19 - 0.90D2 * t35 * t8 * t11 * wd 
     #* nf) * z / 0.60D2 - 0.3D1 / 0.2D1 * t13 * t14 * t44
      t49 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #8)
      t53 = t1 * (-0.1D1 + x1)
      t57 = t4 * s
      t62 = 0.1D1 - x1 + x1 * z
      t65 = Sqrt(-x3 * t62 * t4)
      t66 = t65 ** 2
      t70 = t66 * t8 / t62 * t15
      t73 = FJET(XB1, XB2, s, -x3 * s * t53, t2 * x1 * x3, t57 * t53, -t
     #57 * t1 * x1, 0.0D0, 0.3D1 * t20 * t70)
      t79 = 0.2D1 * x2 * x3
      t80 = sqrt(x2)
      t87 = Sqrt(x3 * (-0.1D1 + t80) * (t80 + 0.1D1) * t4)
      t89 = 0.2D1 * t7 * t80 * t87
      t94 = t80 * x2
      t95 = t94 * x3
      t98 = t7 * x2
      t101 = x3 * t80
      t116 = 0.4D1 * t95 * z - 0.4D1 * t98 * t87 - 0.2D1 * t101 * z - 0.
     #3D1 * t80 + 0.2D1 * t94 - 0.4D1 * t95 - 0.2D1 * t94 * z + 0.4D1 * 
     #t7 * t87 + 0.6D1 * t101 + t80 * z + 0.4D1 * t98 * t87 * z
      t117 = t116 ** 2
      t120 = (x2 * z + 0.1D1 - x2) ** 2
      t121 = 0.1D1 / t120
      t126 = FJET(XB1, XB2, s, -t2 * (-x3 + t79 - x2 + t89), 0.0D0, t2 *
     # (0.1D1 - x2 - x3 + t79 + t89), 0.0D0, 0.0D0, 0.3D1 / 0.32D2 * t20
     # * t117 * t121 * t44)
      rrgg2qqbarht12s1em1 = t49 * t48 + 0.3D1 * t73 * wd * t14 * t70 + 0
     #.3D1 / 0.32D2 * t126 * wd * nf * z * t117 * t121 * t44

      end function



      doubleprecision function rrgg2qqbarht12s1em2
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
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t7 = cos(x4 * pi)
      t8 = t7 ** 2
      t10 = Sqrt(-x3 * t4)
      t11 = t10 ** 2
      t14 = wd * nf * z
      t17 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.3D1 / 0.2D1 * t8 * t11 * t14)
      rrgg2qqbarht12s1em2 = -0.3D1 / 0.2D1 * t17 * t8 * t11 * t14

      end function



      doubleprecision function rrgg2qqbarht12s1em3
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
      rrgg2qqbarht12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht12s1em4
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
      rrgg2qqbarht12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht12s2e1
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
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x2 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = sqrt(x2)
      t16 = -0.1D1 + t15
      t17 = t15 + 0.1D1
      t18 = t16 * t17
      t19 = x3 * t4
      t20 = t18 * t19
      t23 = log(0.4D1 * t10 * t13 * t20)
      t24 = t23 ** 2
      t25 = t15 * z
      t26 = x3 * t15
      t28 = 0.2D1 * t26 * z
      t30 = x3 * t16
      t34 = Sqrt(t30 * t17 * z * t4)
      t35 = cos(t11)
      t39 = (-t25 + t28 - t15 + 0.2D1 * t26 + 0.4D1 * t34 * t35) ** 2
      t43 = t13 * x3 * t4
      t46 = log(-0.4D1 * t10 * t43)
      t47 = t46 ** 2
      t48 = t35 ** 2
      t50 = x3 * z
      t52 = Sqrt(-t50 * t4)
      t53 = t52 ** 2
      t59 = 0.90D2 * t6
      t61 = 0.180D3 * t6 * lh
      t62 = t59 - t61
      t69 = lh ** 2
      t71 = pi ** 2
      t73 = 0.180D3 * t69 - 0.30D2 * t71
      t78 = t39 - 0.16D2 * t48 * t53
      t81 = 0.1D1 / x2
      t84 = t48 * wd
      t85 = t84 * nf
      t86 = 0.90D2 * t85
      t88 = t84 * nf * lh
      t90 = x3 * t9
      t94 = log(-0.4D1 * t90 * t13 * t4)
      t96 = t94 * t48 * t6
      t98 = -t85 - t96
      t101 = t94 ** 2
      t103 = t101 * t48 * t6
      t106 = t84 * nf * t73
      t128 = x1 ** 2
      t129 = x2 * t128
      t134 = log(-0.4D1 * t129 * t13 * t90 * t4)
      t138 = t9 * t13
      t142 = log(0.4D1 * t129 * t138 * t20)
      t151 = 0.1D1 / x1
      t154 = x3 * t128
      t158 = log(-0.4D1 * t154 * t138 * t4)
      t159 = t158 ** 2
      t164 = 0.180D3 * t88
      t165 = t86 - t164
      t169 = t86 - t164 + t106
      t175 = (0.90D2 * t6 * (t24 * t39 / 0.2D1 - 0.8D1 * t47 * t48 * t53
     #) + t62 * (-t23 * t39 + 0.16D2 * t46 * t48 * t53) + (t59 - t61 + t
     #6 * t73) * t78) * t81 / 0.960D3 - (t86 - 0.540D3 * t88 - 0.90D2 * 
     #t96 - 0.360D3 * t98 * lh + 0.45D2 * t103 + 0.2D1 * t106 - 0.180D3 
     #* (t96 + t103 / 0.2D1) * lh + t84 * nf * (0.60D2 * lh * t71 - 0.24
     #0D3 * zeta3 - 0.120D3 * t69 * lh) - 0.15D2 * t101 * t94 * t48 * t6
     # + t98 * t73) * t53 / 0.60D2 - (0.90D2 * t6 * (-0.16D2 * t134 * t4
     #8 * t53 + t142 * t39) - t62 * t78) * t81 * t151 / 0.480D3 + (-0.72
     #0D3 * t84 * nf * t159 * t53 + 0.16D2 * t165 * t158 * t53 - 0.16D2 
     #* t169 * t53) * t151 / 0.480D3
      t176 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #175)
      t178 = x3 * x1
      t181 = -0.1D1 + x1
      t182 = t1 * t181
      t183 = x3 * s * t182
      t184 = t4 * s
      t187 = t184 * t182
      t188 = x1 * z
      t189 = -z - x1 + t188
      t190 = 0.1D1 / t189
      t192 = 0.1D1 / t7
      t195 = t181 ** 2
      t200 = log(0.4D1 * t190 * t128 * t13 * t192 * t195 * x2 * t19)
      t205 = Sqrt(x3 * t189 * t4)
      t206 = t205 ** 2
      t207 = z * t206
      t212 = t190 * z
      t213 = t212 * t206
      t225 = log(0.4D1 * t128 * t13 * t192 * t19 * t190 * t195)
      t226 = t225 ** 2
      t240 = -(-0.1440D4 * t6 * t200 * t48 * t190 * t207 + 0.16D2 * t62 
     #* t48 * t213) * t81 * t151 / 0.480D3 + (-0.720D3 * t85 * t226 * t1
     #90 * t207 + 0.16D2 * t165 * t225 * t213 - 0.16D2 * t169 * t190 * t
     #207) * t151 / 0.480D3
      t241 = FJET(XB1, XB2, s, t2 * t178, -t183, -t184 * t1 * x1, t187, 
     #0.0D0, t240)
      t243 = t178 * z
      t244 = x2 * x3
      t245 = t244 * z
      t246 = t244 * x1
      t247 = t244 * t188
      t252 = Sqrt(-t30 * t17 * t189 * t4)
      t254 = 0.2D1 * t35 * t15 * t252
      t260 = x2 * x1
      t261 = t260 * z
      t262 = z + x1 - t188 - x2 * z - t260 + t261 - t50 - t178 + t243 + 
     #t245 + t246 - t247 + t244 + t254
      t266 = t1 ** 2
      t279 = log(-0.4D1 * t192 * t195 * t18 * t128 * x2 * t190 * t43)
      t282 = (z + t261 - t188 + x1 - t260) ** 2
      t283 = 0.1D1 / t282
      t289 = t15 * x1
      t292 = t15 * t7
      t295 = t15 * x2
      t296 = t295 * x3
      t303 = t7 * t128
      t306 = t35 * x2
      t307 = t252 * x1
      t310 = t35 * t252
      t313 = t128 * t15
      t315 = t295 * x1
      t316 = t128 * t295
      t320 = -0.4D1 * t26 * t188 + 0.8D1 * t154 * t25 + 0.6D1 * x3 * t7 
     #* t289 - 0.4D1 * t154 * t292 - 0.2D1 * t296 * t7 * x1 - 0.4D1 * t2
     #96 * t128 * z + 0.2D1 * t296 * t303 + 0.4D1 * t306 * t307 + 0.4D1 
     #* t310 * t188 + 0.2D1 * t313 - t315 + t292 - t316 + t289 - 0.4D1 *
     # t306 * t307 * z
      t347 = -0.3D1 * t292 * x1 - 0.4D1 * t313 * z - 0.2D1 * t26 * t7 + 
     #0.2D1 * t296 * t128 + 0.2D1 * t296 * x1 + 0.2D1 * t316 * z + t315 
     #* t7 - t316 * t7 + 0.2D1 * t303 * t15 - 0.4D1 * t154 * t15 + 0.2D1
     # * t289 * z - 0.2D1 * t26 * x1 - 0.4D1 * t310 * z - 0.4D1 * t310 *
     # x1 - t28 + t25
      t349 = (t320 + t347) ** 2
      t358 = 0.90D2 * t6 * t279 * t283 * t349 * t212 - t62 * t283 * t349
     # * t190 * z
      t362 = FJET(XB1, XB2, s, t2 * x1 * (-t50 - t178 + t243 + t245 + t2
     #46 - t247 - x2 + t244 + t254) * t190, -t183, -t2 * x1 * t262 * t19
     #0, t187, s * t266 * x2 * x1 * t181 * t190, -t358 * t81 * t151 / 0.
     #480D3)
      rrgg2qqbarht12s2e1 = t176 * t175 + t241 * t240 - t362 * t358 * t81
     # * t151 / 0.480D3

      end function



      doubleprecision function rrgg2qqbarht12s2e0
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
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = x2 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = sqrt(x2)
      t16 = -0.1D1 + t15
      t17 = t15 + 0.1D1
      t19 = x3 * t4
      t23 = log(0.4D1 * t10 * t13 * t16 * t17 * t19)
      t24 = t15 * z
      t25 = x3 * t15
      t27 = 0.2D1 * t25 * z
      t29 = x3 * t16
      t33 = Sqrt(t29 * t17 * z * t4)
      t34 = cos(t11)
      t38 = (-t24 + t27 - t15 + 0.2D1 * t25 + 0.4D1 * t33 * t34) ** 2
      t44 = log(-0.4D1 * t10 * t13 * x3 * t4)
      t45 = t34 ** 2
      t47 = x3 * z
      t49 = Sqrt(-t47 * t4)
      t50 = t49 ** 2
      t62 = t38 - 0.16D2 * t45 * t50
      t65 = 0.1D1 / x2
      t70 = 0.1D1 / x1
      t74 = t45 * wd
      t75 = x1 ** 2
      t76 = x3 * t75
      t81 = log(-0.4D1 * t76 * t13 * t9 * t4)
      t86 = t74 * nf
      t87 = 0.90D2 * t86
      t89 = t74 * nf * lh
      t91 = t87 - 0.180D3 * t89
      t102 = log(-0.4D1 * x3 * t9 * t13 * t4)
      t104 = t102 * t45 * t6
      t109 = t102 ** 2
      t113 = lh ** 2
      t115 = pi ** 2
      t123 = (0.90D2 * t6 * (-t23 * t38 + 0.16D2 * t44 * t45 * t50) + (0
     #.90D2 * t6 - 0.180D3 * t6 * lh) * t62) * t65 / 0.960D3 + 0.3D1 / 0
     #.16D2 * t6 * t62 * t65 * t70 + (0.1440D4 * t74 * nf * t81 * t50 - 
     #0.16D2 * t91 * t50) * t70 / 0.480D3 - (t87 - 0.360D3 * t89 - 0.90D
     #2 * t104 - 0.180D3 * (-t86 - t104) * lh + 0.45D2 * t109 * t45 * t6
     # + t74 * nf * (0.180D3 * t113 - 0.30D2 * t115)) * t50 / 0.60D2
      t124 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #123)
      t126 = x3 * x1
      t129 = -0.1D1 + x1
      t130 = t1 * t129
      t131 = x3 * s * t130
      t132 = t4 * s
      t135 = t132 * t130
      t136 = x1 * z
      t137 = -z - x1 + t136
      t138 = 0.1D1 / t137
      t143 = Sqrt(x3 * t137 * t4)
      t144 = t143 ** 2
      t145 = z * t144
      t146 = t65 * t70
      t153 = t129 ** 2
      t158 = log(0.4D1 * t75 * t13 / t7 * t19 * t138 * t153)
      t169 = -0.3D1 * t6 * t45 * t138 * t145 * t146 + (0.1440D4 * t86 * 
     #t158 * t138 * t145 - 0.16D2 * t91 * t138 * t145) * t70 / 0.480D3
      t170 = FJET(XB1, XB2, s, t2 * t126, -t131, -t132 * t1 * x1, t135, 
     #0.0D0, t169)
      t172 = t126 * z
      t173 = x2 * x3
      t174 = t173 * z
      t175 = t173 * x1
      t176 = t173 * t136
      t181 = Sqrt(-t29 * t17 * t137 * t4)
      t183 = 0.2D1 * t34 * t15 * t181
      t189 = x2 * x1
      t190 = t189 * z
      t191 = z + x1 - t136 - x2 * z - t189 + t190 - t47 - t126 + t172 + 
     #t174 + t175 - t176 + t173 + t183
      t195 = t1 ** 2
      t202 = (z + t190 - t136 + x1 - t189) ** 2
      t203 = 0.1D1 / t202
      t209 = t15 * x1
      t212 = t15 * t7
      t215 = t15 * x2
      t216 = t215 * x3
      t223 = t7 * t75
      t226 = t34 * x2
      t227 = t181 * x1
      t230 = t34 * t181
      t233 = t75 * t15
      t235 = t215 * x1
      t236 = t75 * t215
      t237 = -0.4D1 * t25 * t136 + 0.8D1 * t76 * t24 + 0.6D1 * x3 * t7 *
     # t209 - 0.4D1 * t76 * t212 - 0.2D1 * t216 * t7 * x1 - 0.4D1 * t216
     # * t75 * z + 0.2D1 * t216 * t223 + 0.4D1 * t226 * t227 + 0.4D1 * t
     #230 * t136 + t24 + 0.2D1 * t233 - t235 + t212 - t236 + t209
      t267 = -0.4D1 * t226 * t227 * z - t27 - 0.3D1 * t212 * x1 - 0.4D1 
     #* t233 * z - 0.2D1 * t25 * t7 + 0.2D1 * t216 * t75 + 0.2D1 * t216 
     #* x1 + 0.2D1 * t236 * z + t235 * t7 - t236 * t7 + 0.2D1 * t223 * t
     #15 - 0.4D1 * t76 * t15 + 0.2D1 * t209 * z - 0.2D1 * t25 * x1 - 0.4
     #D1 * t230 * z - 0.4D1 * x1 * t230
      t269 = (t237 + t267) ** 2
      t276 = FJET(XB1, XB2, s, t2 * x1 * (-t47 - t126 + t172 + t174 + t1
     #75 - t176 - x2 + t173 + t183) * t138, -t131, -t2 * x1 * t191 * t13
     #8, t135, s * t195 * x2 * x1 * t129 * t138, 0.3D1 / 0.16D2 * t6 * t
     #203 * t269 * t138 * z * t146)
      rrgg2qqbarht12s2e0 = t124 * t123 + t170 * t169 + 0.3D1 / 0.16D2 * 
     #t276 * wd * nf * t203 * t269 * t138 * z * t65 * t70

      end function



      doubleprecision function rrgg2qqbarht12s2em1
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
      t4 = -0.1D1 + x3
      t6 = x4 * pi
      t7 = cos(t6)
      t8 = t7 ** 2
      t9 = t8 * wd
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t15 = 0.1D1 / x1
      t19 = t9 * nf
      t24 = z ** 2
      t28 = Sin(t6)
      t29 = t28 ** 2
      t33 = log(-0.4D1 * x3 / t24 / z * t29 * t4)
      t35 = wd * nf
      t41 = sqrt(x2)
      t43 = x3 * t41
      t53 = Sqrt(x3 * (-0.1D1 + t41) * (t41 + 0.1D1) * z * t4)
      t57 = (-t41 * z + 0.2D1 * t43 * z - t41 + 0.2D1 * t43 + 0.4D1 * t5
     #3 * t7) ** 2
      t65 = -0.3D1 * t9 * nf * t13 * t15 - (0.90D2 * t19 - 0.180D3 * t9 
     #* nf * lh - 0.90D2 * t33 * t8 * t35) * t13 / 0.60D2 + 0.3D1 / 0.32
     #D2 * t35 * (t57 - 0.16D2 * t8 * t13) / x2
      t66 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t6
     #5)
      t72 = t1 * (-0.1D1 + x1)
      t74 = t4 * s
      t79 = -z - x1 + x1 * z
      t84 = Sqrt(x3 * t79 * t4)
      t85 = t84 ** 2
      t87 = 0.1D1 / t79 * z * t85 * t15
      t90 = FJET(XB1, XB2, s, t2 * x1 * x3, -x3 * s * t72, -t74 * t1 * x
     #1, t74 * t72, 0.0D0, -0.3D1 * t19 * t87)
      rrgg2qqbarht12s2em1 = t66 * t65 - 0.3D1 * t90 * t8 * t35 * t87

      end function



      doubleprecision function rrgg2qqbarht12s2em2
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
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t7 = cos(x4 * pi)
      t8 = t7 ** 2
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t17 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, -0
     #.3D1 / 0.2D1 * t8 * wd * nf * t13)
      rrgg2qqbarht12s2em2 = -0.3D1 / 0.2D1 * t17 * t8 * wd * nf * t13

      end function



      doubleprecision function rrgg2qqbarht12s2em3
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
      rrgg2qqbarht12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht12s2em4
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
      rrgg2qqbarht12s2em4 = 0.0D0

      end function
