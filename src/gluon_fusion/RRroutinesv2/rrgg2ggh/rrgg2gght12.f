      subroutine rrgg2gght12
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt12
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt12
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhardt12s1e1  
      doubleprecision rrgg2gghhardt12s1e0  
      doubleprecision rrgg2gghhardt12s1em1  
      doubleprecision rrgg2gghhardt12s1em2  
      doubleprecision rrgg2gghhardt12s1em3  
      doubleprecision rrgg2gghhardt12s1em4  
      doubleprecision rrgg2gghhardt12s2e1  
      doubleprecision rrgg2gghhardt12s2e0  
      doubleprecision rrgg2gghhardt12s2em1  
      doubleprecision rrgg2gghhardt12s2em2  
      doubleprecision rrgg2gghhardt12s2em3  
      doubleprecision rrgg2gghhardt12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt12s1e1
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
      t6 = z * wd
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x2 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t12 * x3
      t15 = t10 * t13 * t4
      t17 = log(-0.4D1 * t15)
      t18 = t17 ** 2
      t19 = cos(t7)
      t20 = t19 ** 2
      t23 = Sqrt(-x3 * t4)
      t24 = t23 ** 2
      t32 = (-0.180D3 - 0.180D3 * lh) * z * wd
      t33 = 0.180D3 * t6 + t32
      t35 = t20 * t24
      t41 = pi ** 2
      t42 = 0.30D2 * t41
      t43 = lh ** 2
      t44 = 0.180D3 * t43
      t48 = 0.270D3 * t6 + 0.2D1 * t32 + (0.90D2 + 0.360D3 * lh - t42 + 
     #t44) * z * wd
      t53 = 0.1D1 / x2
      t57 = t35 * lh
      t58 = 0.180D3 * t57
      t63 = log(-0.4D1 * x3 * t9 * t12 * t4)
      t64 = t63 * t20
      t65 = t64 * t24
      t66 = 0.90D2 * t65
      t74 = t24 * lh
      t75 = t64 * t74
      t77 = t63 ** 2
      t78 = t77 * t20
      t79 = t78 * t24
      t81 = -t42 + t44
      t82 = t35 * t81
      t111 = x2 * x3
      t112 = x1 ** 2
      t114 = t9 * t12
      t115 = t114 * t4
      t118 = log(-0.4D1 * t111 * t112 * t115)
      t123 = t33 * t20
      t127 = 0.1D1 / x1
      t131 = t20 * z
      t132 = x3 * t112
      t135 = log(-0.4D1 * t132 * t115)
      t136 = t135 ** 2
      t141 = t131 * wd
      t143 = t20 * lh
      t147 = (-0.180D3 * t20 - 0.180D3 * t143) * z * wd
      t148 = 0.180D3 * t141 + t147
      t159 = 0.270D3 * t141 + 0.2D1 * t147 + (0.90D2 * t20 + 0.360D3 * t
     #143 + t20 * t81) * z * wd
      t164 = (0.720D3 * t6 * t18 * t20 * t24 - 0.16D2 * t33 * t17 * t35 
     #+ 0.16D2 * t48 * t20 * t24) * t53 / 0.320D3 + 0.3D1 / 0.20D2 * (-0
     #.180D3 * t35 - t58 - t66) * z * wd + (0.90D2 * t35 + 0.360D3 * t57
     # + 0.180D3 * t65 + 0.180D3 * t75 + 0.45D2 * t79 + t82) * z * wd / 
     #0.10D2 + (-t58 - t66 - 0.360D3 * t75 - 0.90D2 * t79 - 0.2D1 * t82 
     #- 0.90D2 * t78 * t74 + t35 * (-0.240D3 * zeta3 - 0.120D3 * t43 * l
     #h + 0.60D2 * lh * t41) - 0.15D2 * t77 * t63 * t20 * t24 - t64 * t2
     #4 * t81) * z * wd / 0.20D2 + 0.18D2 * t35 * t6 + (-0.1440D4 * t6 *
     # t118 * t20 * t24 + 0.16D2 * t123 * t24) * t127 * t53 / 0.160D3 + 
     #(0.45D2 * t131 * wd * t136 * t24 - t148 * t135 * t24 + t159 * t24)
     # * t127 / 0.10D2
      t165 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #164)
      t167 = -0.1D1 + x1
      t168 = x3 * x1
      t169 = t168 * z
      t170 = 0.2D1 * t111
      t171 = t168 * x2
      t172 = x2 * z
      t173 = t168 * t172
      t174 = sqrt(x2)
      t175 = t19 * t174
      t176 = -0.1D1 + t174
      t177 = x3 * t176
      t178 = t174 + 0.1D1
      t179 = x1 * z
      t180 = 0.1D1 - x1 + t179
      t184 = Sqrt(t177 * t178 * t180 * t4)
      t186 = 0.2D1 * t175 * t184
      t189 = 0.1D1 / t180
      t192 = t2 * t168
      t193 = x1 * x2
      t194 = t193 * z
      t195 = 0.1D1 - x1 + t179 - x2 + t193 - t194 - x3 + t168 - t169 + t
     #170 - t171 + t173 + t186
      t199 = t4 * s
      t201 = t199 * t1 * x1
      t202 = t1 ** 2
      t208 = t167 ** 2
      t215 = log(0.4D1 * t15 * t208 * t189 * t112 * t176 * t178)
      t217 = t174 * x2
      t218 = t217 * z
      t221 = t217 * t112
      t225 = t217 * x1
      t226 = t11 * x3
      t231 = t174 * z
      t239 = t112 * t11
      t240 = t174 * x3
      t243 = t19 * t184
      t246 = x2 * t19
      t247 = t184 * x1
      t253 = 0.3D1 * t174
      t255 = 0.4D1 * t217 * x3
      t256 = t112 * t174
      t259 = x1 * t174
      t262 = 0.8D1 * t218 * t168 - 0.4D1 * t221 * z * x3 - 0.2D1 * t225 
     #* t226 + 0.2D1 * t221 * t226 - 0.12D2 * t168 * t231 + 0.2D1 * t168
     # * t174 * t11 + 0.8D1 * t132 * t231 - 0.4D1 * t239 * t240 - 0.4D1 
     #* t172 * t243 - 0.4D1 * t246 * t247 - 0.4D1 * z * t19 * t247 + t25
     #3 + t255 + 0.2D1 * t256 + 0.3D1 * t225 - t221 - t231 - 0.5D1 * t25
     #9 - 0.4D1 * t243
      t263 = 0.6D1 * t240
      t264 = 0.2D1 * t218
      t266 = 0.4D1 * t218 * x3
      t269 = t243 * x1
      t289 = 0.2D1 * t240 * z
      t296 = 0.2D1 * t217
      t297 = -t263 + t264 - t266 - 0.4D1 * t218 * x1 + 0.4D1 * t269 + 0.
     #4D1 * t246 * t184 - 0.4D1 * t256 * z - t259 * t11 + 0.2D1 * t239 *
     # t174 + 0.2D1 * t221 * x3 - 0.6D1 * t225 * x3 + 0.2D1 * t221 * z +
     # t225 * t11 - t221 * t11 + 0.6D1 * t259 * z + t289 + 0.10D2 * t168
     # * t174 - 0.4D1 * t132 * t174 + 0.4D1 * t172 * t269 - t296
      t299 = (t262 + t297) ** 2
      t301 = (-0.1D1 + x1 - t179 + x2 - t193 - t172 + t194) ** 2
      t302 = 0.1D1 / t301
      t310 = -0.90D2 * t6 * t215 * t299 * t302 * t189 + t33 * t299 * t30
     #2 * t189
      t314 = FJET(XB1, XB2, s, t2 * t167 * (-x3 + t168 - t169 + t170 - t
     #171 + t173 - x2 + t186) * t189, t192, -t2 * t167 * t195 * t189, -t
     #201, -s * t202 * x2 * t167 * x1 * t189, t310 * t127 * t53 / 0.160D
     #3)
      t320 = t177 * t178 * t4
      t321 = Sqrt(t320)
      t323 = 0.2D1 * t175 * t321
      t331 = log(0.4D1 * t10 * t12 * t320)
      t332 = t331 ** 2
      t334 = (0.1D1 - x2 + t172) ** 2
      t335 = 0.1D1 / t334
      t337 = t19 * t321
      t343 = t266 + 0.4D1 * t172 * t337 - 0.4D1 * t246 * t321 - t255 + 0
     #.4D1 * t337 - t253 + t231 + t296 + t263 - t289 - t264
      t344 = t343 ** 2
      t362 = log(0.4D1 * t10 * t13 * t4 * t112 * t176 * t178)
      t373 = (-0.45D2 * t6 * t332 * t335 * t344 + t33 * t331 * t335 * t3
     #44 - t48 * t335 * t344) * t53 / 0.320D3 + (0.90D2 * t6 * t362 * t3
     #35 * t344 - t33 * t335 * t344) * t127 * t53 / 0.160D3
      t374 = FJET(XB1, XB2, s, -t2 * (-x3 + t170 - x2 + t323), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t170 + t323), 0.0D0, 0.0D0, t373)
      t377 = t1 * t167
      t380 = t112 * t189
      t384 = x3 * t208
      t388 = log(-0.4D1 * t380 * t4 * t9 * t12 * x2 * t384)
      t392 = Sqrt(-x3 * t180 * t4)
      t393 = t392 ** 2
      t398 = t393 * t189
      t409 = log(-0.4D1 * t380 * t4 * t114 * t384)
      t410 = t409 ** 2
      t422 = (0.1440D4 * t6 * t388 * t20 * t393 * t189 - 0.16D2 * t123 *
     # t398) * t127 * t53 / 0.160D3 + (-0.45D2 * t141 * t410 * t393 * t1
     #89 + t148 * t409 * t398 - t159 * t393 * t189) * t127 / 0.10D2
      t423 = FJET(XB1, XB2, s, -x3 * s * t377, t192, t199 * t377, -t201,
     # 0.0D0, t422)
      rrgg2gghhardt12s1e1 = t165 * t164 + t314 * t310 * t127 * t53 / 0.1
     #60D3 + t374 * t373 + t423 * t422

      end function



      doubleprecision function rrgg2gghhardt12s1e0
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
      t6 = z * wd
      t7 = x4 * pi
      t8 = Sin(t7)
      t9 = t8 ** 2
      t10 = x2 * t9
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t17 = log(-0.4D1 * t10 * t12 * x3 * t4)
      t18 = cos(t7)
      t19 = t18 ** 2
      t22 = Sqrt(-x3 * t4)
      t23 = t22 ** 2
      t32 = 0.180D3 * t6 + (-0.180D3 - 0.180D3 * lh) * z * wd
      t37 = 0.1D1 / x2
      t40 = t19 * t23
      t42 = 0.1D1 / x1
      t47 = t19 * z
      t48 = x1 ** 2
      t49 = x3 * t48
      t50 = t9 * t12
      t54 = log(-0.4D1 * t49 * t50 * t4)
      t59 = t47 * wd
      t66 = 0.180D3 * t59 + (-0.180D3 * t19 - 0.180D3 * t19 * lh) * z * 
     #wd
      t74 = t40 * lh
      t80 = log(-0.4D1 * x3 * t9 * t12 * t4)
      t81 = t80 * t19
      t82 = t81 * t23
      t94 = t80 ** 2
      t98 = pi ** 2
      t100 = lh ** 2
      t108 = (-0.1440D4 * t6 * t17 * t19 * t23 + 0.16D2 * t32 * t19 * t2
     #3) * t37 / 0.320D3 + 0.9D1 * t40 * z * wd * t42 * t37 + (-0.90D2 *
     # t47 * wd * t54 * t23 + t66 * t23) * t42 / 0.10D2 + 0.27D2 / 0.2D1
     # * t40 * t6 + (-0.180D3 * t40 - 0.180D3 * t74 - 0.90D2 * t82) * z 
     #* wd / 0.10D2 + (0.90D2 * t40 + 0.360D3 * t74 + 0.180D3 * t82 + 0.
     #180D3 * t81 * t23 * lh + 0.45D2 * t94 * t19 * t23 + t40 * (-0.30D2
     # * t98 + 0.180D3 * t100)) * z * wd / 0.20D2
      t109 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #108)
      t111 = -0.1D1 + x1
      t112 = x3 * x1
      t113 = t112 * z
      t115 = 0.2D1 * x2 * x3
      t116 = t112 * x2
      t117 = x2 * z
      t118 = t112 * t117
      t119 = sqrt(x2)
      t120 = t18 * t119
      t122 = x3 * (-0.1D1 + t119)
      t123 = t119 + 0.1D1
      t124 = x1 * z
      t125 = 0.1D1 - x1 + t124
      t129 = Sqrt(t122 * t123 * t125 * t4)
      t131 = 0.2D1 * t120 * t129
      t134 = 0.1D1 / t125
      t137 = t2 * t112
      t138 = x1 * x2
      t139 = t138 * z
      t140 = 0.1D1 - x1 + t124 - x2 + t138 - t139 - x3 + t112 - t113 + t
     #115 - t116 + t118 + t131
      t144 = t4 * s
      t146 = t144 * t1 * x1
      t147 = t1 ** 2
      t153 = t119 * x2
      t154 = t153 * z
      t157 = t153 * t48
      t161 = t153 * x1
      t162 = t11 * x3
      t167 = t119 * z
      t175 = t48 * t11
      t176 = t119 * x3
      t179 = t18 * t129
      t182 = x2 * t18
      t183 = t129 * x1
      t190 = 0.4D1 * t153 * x3
      t191 = t48 * t119
      t194 = x1 * t119
      t197 = 0.6D1 * t176
      t198 = 0.8D1 * t154 * t112 - 0.4D1 * t157 * z * x3 - 0.2D1 * t161 
     #* t162 + 0.2D1 * t157 * t162 - 0.12D2 * t112 * t167 + 0.2D1 * t112
     # * t119 * t11 + 0.8D1 * t49 * t167 - 0.4D1 * t175 * t176 - 0.4D1 *
     # t117 * t179 - 0.4D1 * t182 * t183 - 0.4D1 * z * t18 * t183 + t190
     # + 0.2D1 * t191 + 0.3D1 * t161 - t157 - t167 - 0.5D1 * t194 - 0.4D
     #1 * t179 - t197
      t199 = 0.2D1 * t154
      t200 = t179 * x1
      t203 = 0.3D1 * t119
      t204 = 0.2D1 * t153
      t206 = 0.4D1 * t154 * x3
      t228 = 0.2D1 * t176 * z
      t233 = t199 + 0.4D1 * t117 * t200 + t203 - t204 - t206 - 0.4D1 * t
     #154 * x1 + 0.4D1 * t200 + 0.4D1 * t182 * t129 - 0.4D1 * t191 * z -
     # t194 * t11 + 0.2D1 * t175 * t119 + 0.2D1 * t157 * x3 - 0.6D1 * t1
     #61 * x3 + 0.2D1 * t157 * z + t161 * t11 - t157 * t11 + 0.6D1 * t19
     #4 * z + t228 + 0.10D2 * t112 * t119 - 0.4D1 * t49 * t119
      t235 = (t198 + t233) ** 2
      t238 = (-0.1D1 + x1 - t124 + x2 - t138 - t117 + t139) ** 2
      t241 = t42 * t37
      t242 = 0.1D1 / t238 * t134 * t241
      t245 = FJET(XB1, XB2, s, t2 * t111 * (-x3 + t112 - t113 + t115 - t
     #116 + t118 - x2 + t131) * t134, t137, -t2 * t111 * t140 * t134, -t
     #146, -s * t147 * x2 * t111 * x1 * t134, 0.9D1 / 0.16D2 * t6 * t235
     # * t242)
      t252 = t122 * t123 * t4
      t253 = Sqrt(t252)
      t255 = 0.2D1 * t120 * t253
      t263 = log(0.4D1 * t10 * t12 * t252)
      t265 = (0.1D1 - x2 + t117) ** 2
      t266 = 0.1D1 / t265
      t268 = t18 * t253
      t274 = t206 + 0.4D1 * t117 * t268 - 0.4D1 * t182 * t253 - t190 + 0
     #.4D1 * t268 - t203 + t167 + t204 + t197 - t228 - t199
      t275 = t274 ** 2
      t289 = (0.90D2 * t6 * t263 * t266 * t275 - t32 * t266 * t275) * t3
     #7 / 0.320D3 - 0.9D1 / 0.16D2 * t6 * t266 * t275 * t42 * t37
      t290 = FJET(XB1, XB2, s, -t2 * (-x3 + t115 - x2 + t255), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t115 + t255), 0.0D0, 0.0D0, t289)
      t293 = t1 * t111
      t298 = Sqrt(-x3 * t125 * t4)
      t299 = t298 ** 2
      t306 = t111 ** 2
      t311 = log(-0.4D1 * t48 * t134 * t4 * t50 * x3 * t306)
      t321 = -0.9D1 * t59 * t299 * t134 * t241 + (0.90D2 * t59 * t311 * 
     #t299 * t134 - t66 * t299 * t134) * t42 / 0.10D2
      t322 = FJET(XB1, XB2, s, -x3 * s * t293, t137, t144 * t293, -t146,
     # 0.0D0, t321)
      rrgg2gghhardt12s1e0 = t109 * t108 + 0.9D1 / 0.16D2 * t245 * z * wd
     # * t235 * t242 + t290 * t289 + t322 * t321

      end function



      doubleprecision function rrgg2gghhardt12s1em1
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
      t10 = Sqrt(-x3 * t4)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z * wd
      t14 = 0.1D1 / x1
      t23 = Sin(t6)
      t24 = t23 ** 2
      t26 = z ** 2
      t31 = log(-0.4D1 * x3 * t24 / t26 * t4)
      t39 = 0.1D1 / x2
      t43 = 0.9D1 * t12 * t13 * t14 + 0.9D1 * t12 * t13 + (-0.180D3 * t1
     #2 - 0.180D3 * t12 * lh - 0.90D2 * t31 * t8 * t11) * z * wd / 0.20D
     #2 + 0.9D1 / 0.2D1 * t12 * t13 * t39
      t44 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #3)
      t48 = t1 * (-0.1D1 + x1)
      t52 = t4 * s
      t58 = 0.1D1 - x1 + x1 * z
      t61 = Sqrt(-x3 * t58 * t4)
      t62 = t61 ** 2
      t63 = 0.1D1 / t58
      t68 = FJET(XB1, XB2, s, -x3 * s * t48, t2 * x1 * x3, t52 * t48, -t
     #52 * t1 * x1, 0.0D0, -0.9D1 * t13 * t8 * t62 * t63 * t14)
      t77 = 0.2D1 * x2 * x3
      t78 = sqrt(x2)
      t85 = Sqrt(x3 * (-0.1D1 + t78) * (t78 + 0.1D1) * t4)
      t87 = 0.2D1 * t7 * t78 * t85
      t92 = x2 * z
      t94 = (0.1D1 - x2 + t92) ** 2
      t96 = t78 * x2
      t97 = t96 * z
      t100 = t7 * t85
      t112 = t78 * x3
      t117 = 0.4D1 * t97 * x3 + 0.4D1 * t92 * t100 - 0.4D1 * x2 * t7 * t
     #85 - 0.4D1 * t96 * x3 + 0.4D1 * t100 - 0.3D1 * t78 + t78 * z + 0.2
     #D1 * t96 + 0.6D1 * t112 - 0.2D1 * t112 * z - 0.2D1 * t97
      t118 = t117 ** 2
      t120 = 0.1D1 / t94 * t118 * t39
      t123 = FJET(XB1, XB2, s, -t2 * (-x3 + t77 - x2 + t87), 0.0D0, t2 *
     # (0.1D1 - x2 - x3 + t77 + t87), 0.0D0, 0.0D0, -0.9D1 / 0.32D2 * t1
     #3 * t120)
      rrgg2gghhardt12s1em1 = t44 * t43 - 0.9D1 * t68 * z * wd * t8 * t62
     # * t63 * t14 - 0.9D1 / 0.32D2 * t123 * z * wd * t120

      end function



      doubleprecision function rrgg2gghhardt12s1em2
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
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t7 = cos(x4 * pi)
      t8 = t7 ** 2
      t10 = Sqrt(-x3 * t4)
      t11 = t10 ** 2
      t16 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, 0.
     #9D1 / 0.2D1 * t8 * t11 * z * wd)
      rrgg2gghhardt12s1em2 = 0.9D1 / 0.2D1 * t16 * t8 * t11 * z * wd

      end function



      doubleprecision function rrgg2gghhardt12s1em3
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
      rrgg2gghhardt12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt12s1em4
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
      rrgg2gghhardt12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt12s2e1
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
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = t8 * x2
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t14 = x3 * t4
      t15 = sqrt(x2)
      t16 = -0.1D1 + t15
      t17 = t15 + 0.1D1
      t22 = log(0.4D1 * t9 * t12 * t14 * t16 * t17)
      t23 = t22 ** 2
      t24 = x3 * t15
      t26 = 0.2D1 * t24 * z
      t27 = t15 * z
      t28 = cos(t6)
      t29 = x3 * t16
      t33 = Sqrt(t29 * t17 * z * t4)
      t38 = (t26 - t27 + 0.4D1 * t28 * t33 - t15 + 0.2D1 * t24) ** 2
      t41 = x2 * x3
      t43 = t12 * t8 * t4
      t46 = log(-0.4D1 * t41 * t43)
      t47 = t46 ** 2
      t48 = t28 ** 2
      t50 = x3 * z
      t52 = Sqrt(-t50 * t4)
      t53 = t52 ** 2
      t62 = (-0.180D3 - 0.180D3 * lh) * wd
      t63 = 0.180D3 * wd + t62
      t73 = pi ** 2
      t74 = 0.30D2 * t73
      t75 = lh ** 2
      t76 = 0.180D3 * t75
      t80 = t48 * t53
      t82 = t38 - 0.16D2 * t80
      t85 = 0.1D1 / x2
      t89 = t48 * lh
      t90 = 0.180D3 * t89
      t95 = log(-0.4D1 * x3 * t12 * t8 * t4)
      t96 = t95 * t48
      t97 = 0.90D2 * t96
      t102 = 0.90D2 * t48
      t103 = 0.360D3 * t89
      t105 = t96 * lh
      t107 = t95 ** 2
      t108 = t107 * t48
      t110 = -t74 + t76
      t111 = t48 * t110
      t138 = x1 ** 2
      t142 = log(-0.4D1 * t41 * t138 * t43)
      t152 = log(0.4D1 * t9 * t14 * t138 * t16 * t17 * t12)
      t159 = 0.1D1 / x1
      t163 = t48 * wd
      t164 = x3 * t138
      t167 = log(-0.4D1 * t164 * t43)
      t168 = t167 ** 2
      t175 = (-0.180D3 * t48 - 0.180D3 * t89) * wd
      t176 = 0.180D3 * t163 + t175
      t184 = 0.270D3 * t163 + 0.2D1 * t175 + (t102 + t103 + t111) * wd
      t190 = -(0.90D2 * wd * (t23 * t38 / 0.2D1 - 0.8D1 * t47 * t48 * t5
     #3) + t63 * (-t22 * t38 + 0.16D2 * t46 * t48 * t53) + (0.270D3 * wd
     # + 0.2D1 * t62 + (0.90D2 + 0.360D3 * lh - t74 + t76) * wd) * t82) 
     #* t85 / 0.320D3 + 0.3D1 / 0.20D2 * (-0.180D3 * t48 - t90 - t97) * 
     #t53 * wd + (t102 + t103 + 0.180D3 * t96 + 0.180D3 * t105 + 0.45D2 
     #* t108 + t111) * t53 * wd / 0.10D2 + (-t90 - t97 - 0.360D3 * t105 
     #- 0.90D2 * t108 - 0.2D1 * t111 - 0.90D2 * t108 * lh + t48 * (-0.24
     #0D3 * zeta3 - 0.120D3 * t75 * lh + 0.60D2 * lh * t73) - 0.15D2 * t
     #107 * t95 * t48 - t96 * t110) * t53 * wd / 0.20D2 + 0.18D2 * t80 *
     # wd - (0.90D2 * wd * (0.16D2 * t142 * t48 * t53 - t152 * t38) + t6
     #3 * t82) * t159 * t85 / 0.160D3 + (0.720D3 * t163 * t168 * t53 - 0
     #.16D2 * t176 * t167 * t53 + 0.16D2 * t184 * t53) * t159 / 0.160D3
      t191 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #190)
      t193 = x3 * x1
      t196 = -0.1D1 + x1
      t197 = t1 * t196
      t198 = x3 * s * t197
      t199 = t4 * s
      t202 = t199 * t197
      t204 = 0.1D1 / t10
      t207 = x1 * z
      t208 = -z - x1 + t207
      t209 = 0.1D1 / t208
      t211 = t196 ** 2
      t212 = t211 * t4
      t216 = log(0.4D1 * t138 * t8 * t204 * x2 * x3 * t209 * t212)
      t221 = Sqrt(x3 * t208 * t4)
      t222 = t221 ** 2
      t224 = t222 * z * t209
      t238 = log(0.4D1 * t164 * t8 * t204 * t209 * t212)
      t239 = t238 ** 2
      t253 = -(0.1440D4 * wd * t216 * t48 * t224 - 0.16D2 * t63 * t48 * 
     #t224) * t159 * t85 / 0.160D3 + (0.720D3 * t163 * t239 * t224 - 0.1
     #6D2 * t176 * t238 * t224 + 0.16D2 * t184 * t222 * z * t209) * t159
     # / 0.160D3
      t254 = FJET(XB1, XB2, s, t2 * t193, -t198, -t199 * t1 * x1, t202, 
     #0.0D0, t253)
      t256 = t193 * z
      t257 = t41 * z
      t258 = t193 * x2
      t259 = x2 * z
      t260 = t193 * t259
      t265 = Sqrt(-t29 * t17 * t208 * t4)
      t267 = 0.2D1 * t28 * t15 * t265
      t272 = x1 * x2
      t273 = t272 * z
      t274 = z + x1 - t207 - t259 - t272 + t273 - t50 - t193 + t256 + t2
     #57 + t258 - t260 + t41 + t267
      t278 = t1 ** 2
      t292 = log(-0.4D1 * t9 * t14 * t138 * t211 * t209 * t204 * t16 * t
     #17)
      t295 = z * t28 * t265
      t298 = t138 * t15
      t300 = t15 * x2
      t301 = t300 * x1
      t302 = t15 * t10
      t303 = t300 * t138
      t304 = x1 * t15
      t305 = t28 * t265
      t312 = t10 * x3
      t321 = 0.4D1 * t272 * t295 + t26 - t27 - 0.2D1 * t298 + t301 - t30
     #2 + t303 - t304 - 0.4D1 * t272 * t305 - 0.4D1 * t207 * t305 + 0.4D
     #1 * t303 * t50 + 0.2D1 * t301 * t312 - 0.2D1 * t303 * t312 + 0.4D1
     # * t193 * t27 - 0.6D1 * t193 * t302
      t324 = t138 * t10
      t353 = -0.8D1 * t164 * t27 + 0.4D1 * t324 * t24 + 0.4D1 * t298 * z
     # + 0.3D1 * t304 * t10 - 0.2D1 * t324 * t15 + 0.2D1 * t302 * x3 - 0
     #.2D1 * t303 * x3 - 0.2D1 * t301 * x3 - 0.2D1 * t303 * z - t301 * t
     #10 + t303 * t10 - 0.2D1 * t304 * z + 0.2D1 * t193 * t15 + 0.4D1 * 
     #t164 * t15 + 0.4D1 * t295 + 0.4D1 * x1 * t28 * t265
      t355 = (t321 + t353) ** 2
      t358 = (z + x1 - t207 - t272 + t273) ** 2
      t361 = z / t358 * t209
      t366 = -0.90D2 * wd * t292 * t355 * t361 + t63 * t355 * t361
      t370 = FJET(XB1, XB2, s, t2 * x1 * (-t50 - t193 + t256 + t257 + t2
     #58 - t260 - x2 + t41 + t267) * t209, -t198, -t2 * x1 * t274 * t209
     #, t202, s * t278 * x2 * x1 * t196 * t209, -t366 * t159 * t85 / 0.1
     #60D3)
      rrgg2gghhardt12s2e1 = t191 * t190 + t254 * t253 - t370 * t366 * t1
     #59 * t85 / 0.160D3

      end function



      doubleprecision function rrgg2gghhardt12s2e0
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
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t15 = sqrt(x2)
      t16 = -0.1D1 + t15
      t17 = t15 + 0.1D1
      t22 = log(0.4D1 * t8 * x2 * t12 * x3 * t4 * t16 * t17)
      t23 = x3 * t15
      t25 = 0.2D1 * t23 * z
      t26 = t15 * z
      t27 = cos(t6)
      t28 = x3 * t16
      t32 = Sqrt(t28 * t17 * z * t4)
      t37 = (t25 - t26 + 0.4D1 * t27 * t32 - t15 + 0.2D1 * t23) ** 2
      t39 = x2 * x3
      t41 = t12 * t8 * t4
      t44 = log(-0.4D1 * t39 * t41)
      t45 = t27 ** 2
      t47 = x3 * z
      t49 = Sqrt(-t47 * t4)
      t50 = t49 ** 2
      t61 = t45 * t50
      t63 = t37 - 0.16D2 * t61
      t66 = 0.1D1 / x2
      t70 = 0.1D1 / x1
      t71 = t70 * t66
      t74 = t45 * wd
      t75 = x1 ** 2
      t76 = x3 * t75
      t79 = log(-0.4D1 * t76 * t41)
      t84 = t45 * lh
      t88 = 0.180D3 * t74 + (-0.180D3 * t45 - 0.180D3 * t84) * wd
      t102 = log(-0.4D1 * x3 * t12 * t8 * t4)
      t103 = t102 * t45
      t114 = t102 ** 2
      t117 = pi ** 2
      t119 = lh ** 2
      t127 = -(0.90D2 * wd * (-t22 * t37 + 0.16D2 * t44 * t45 * t50) + (
     #0.180D3 * wd + (-0.180D3 - 0.180D3 * lh) * wd) * t63) * t66 / 0.32
     #0D3 - 0.9D1 / 0.16D2 * wd * t63 * t71 + (-0.1440D4 * t74 * t79 * t
     #50 + 0.16D2 * t88 * t50) * t70 / 0.160D3 + 0.27D2 / 0.2D1 * t61 * 
     #wd + (-0.180D3 * t45 - 0.180D3 * t84 - 0.90D2 * t103) * t50 * wd /
     # 0.10D2 + (0.90D2 * t45 + 0.360D3 * t84 + 0.180D3 * t103 + 0.180D3
     # * t103 * lh + 0.45D2 * t114 * t45 + t45 * (-0.30D2 * t117 + 0.180
     #D3 * t119)) * t50 * wd / 0.20D2
      t128 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #127)
      t130 = x3 * x1
      t133 = -0.1D1 + x1
      t134 = t1 * t133
      t135 = x3 * s * t134
      t136 = t4 * s
      t139 = t136 * t134
      t140 = x1 * z
      t141 = -z - x1 + t140
      t144 = Sqrt(x3 * t141 * t4)
      t145 = t144 ** 2
      t147 = 0.1D1 / t141
      t148 = z * t147
      t155 = t133 ** 2
      t160 = log(0.4D1 * t76 * t8 / t10 * t147 * t155 * t4)
      t172 = 0.9D1 * t74 * t145 * t148 * t71 + (-0.1440D4 * t74 * t160 *
     # t145 * z * t147 + 0.16D2 * t88 * t145 * t148) * t70 / 0.160D3
      t173 = FJET(XB1, XB2, s, t2 * t130, -t135, -t136 * t1 * x1, t139, 
     #0.0D0, t172)
      t175 = t130 * z
      t176 = t39 * z
      t177 = t130 * x2
      t178 = x2 * z
      t179 = t130 * t178
      t184 = Sqrt(-t28 * t17 * t141 * t4)
      t186 = 0.2D1 * t27 * t15 * t184
      t191 = x1 * x2
      t192 = t191 * z
      t193 = z + x1 - t140 - t178 - t191 + t192 - t47 - t130 + t175 + t1
     #76 + t177 - t179 + t39 + t186
      t197 = t1 ** 2
      t203 = t75 * t15
      t205 = t15 * x2
      t206 = t205 * x1
      t207 = t15 * t10
      t208 = t205 * t75
      t209 = x1 * t15
      t211 = z * t27 * t184
      t214 = t27 * t184
      t221 = t10 * x3
      t232 = -t26 - 0.2D1 * t203 + t206 - t207 + t208 - t209 + 0.4D1 * t
     #191 * t211 - 0.4D1 * t191 * t214 - 0.4D1 * t140 * t214 + 0.4D1 * t
     #208 * t47 + 0.2D1 * t206 * t221 - 0.2D1 * t208 * t221 + 0.4D1 * t1
     #30 * t26 - 0.6D1 * t130 * t207 - 0.8D1 * t76 * t26
      t233 = t75 * t10
      t262 = 0.4D1 * t233 * t23 + t25 + 0.4D1 * t203 * z + 0.3D1 * t209 
     #* t10 - 0.2D1 * t233 * t15 + 0.2D1 * t207 * x3 - 0.2D1 * x3 * t208
     # - 0.2D1 * t206 * x3 - 0.2D1 * t208 * z - t206 * t10 + t208 * t10 
     #- 0.2D1 * t209 * z + 0.2D1 * t130 * t15 + 0.4D1 * t76 * t15 + 0.4D
     #1 * t211 + 0.4D1 * x1 * t27 * t184
      t264 = (t232 + t262) ** 2
      t268 = (z + x1 - t140 - t191 + t192) ** 2
      t271 = 0.1D1 / t268 * t147 * t71
      t274 = FJET(XB1, XB2, s, t2 * x1 * (-t47 - t130 + t175 + t176 + t1
     #77 - t179 - x2 + t39 + t186) * t147, -t135, -t2 * x1 * t193 * t147
     #, t139, s * t197 * x2 * x1 * t133 * t147, -0.9D1 / 0.16D2 * wd * t
     #264 * z * t271)
      rrgg2gghhardt12s2e0 = t128 * t127 + t173 * t172 - 0.9D1 / 0.16D2 *
     # t274 * wd * t264 * z * t271

      end function



      doubleprecision function rrgg2gghhardt12s2em1
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
      t11 = Sqrt(-x3 * z * t4)
      t12 = t11 ** 2
      t13 = t8 * t12
      t14 = 0.1D1 / x1
      t23 = z ** 2
      t27 = Sin(t6)
      t28 = t27 ** 2
      t32 = log(-0.4D1 * x3 / t23 / z * t28 * t4)
      t39 = sqrt(x2)
      t40 = x3 * t39
      t50 = Sqrt(x3 * (-0.1D1 + t39) * (t39 + 0.1D1) * z * t4)
      t55 = (0.2D1 * t40 * z - t39 * z + 0.4D1 * t7 * t50 - t39 + 0.2D1 
     #* t40) ** 2
      t62 = 0.9D1 * t13 * wd * t14 + 0.9D1 * t13 * wd + (-0.180D3 * t8 -
     # 0.180D3 * t8 * lh - 0.90D2 * t32 * t8) * t12 * wd / 0.20D2 - 0.9D
     #1 / 0.32D2 * wd * (t55 - 0.16D2 * t13) / x2
      t63 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t6
     #2)
      t69 = t1 * (-0.1D1 + x1)
      t71 = t4 * s
      t77 = -z - x1 + x1 * z
      t80 = Sqrt(x3 * t77 * t4)
      t81 = t80 ** 2
      t83 = 0.1D1 / t77
      t88 = FJET(XB1, XB2, s, t2 * x1 * x3, -x3 * s * t69, -t71 * t1 * x
     #1, t71 * t69, 0.0D0, 0.9D1 * t8 * wd * t81 * z * t83 * t14)
      rrgg2gghhardt12s2em1 = t63 * t62 + 0.9D1 * t88 * t8 * wd * t81 * z
     # * t83 * t14

      end function



      doubleprecision function rrgg2gghhardt12s2em2
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
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t7 = cos(x4 * pi)
      t8 = t7 ** 2
      t11 = Sqrt(-x3 * z * t4)
      t12 = t11 ** 2
      t16 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.
     #9D1 / 0.2D1 * t8 * t12 * wd)
      rrgg2gghhardt12s2em2 = 0.9D1 / 0.2D1 * t16 * t8 * t12 * wd

      end function



      doubleprecision function rrgg2gghhardt12s2em3
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
      rrgg2gghhardt12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt12s2em4
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
      rrgg2gghhardt12s2em4 = 0.0D0

      end function
  
      subroutine rrgg2gghsoftt12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt12s1e1  
      doubleprecision rrgg2gghsoftt12s1e0  
      doubleprecision rrgg2gghsoftt12s1em1  
      doubleprecision rrgg2gghsoftt12s1em2  
      doubleprecision rrgg2gghsoftt12s1em3  
      doubleprecision rrgg2gghsoftt12s1em4  
      doubleprecision rrgg2gghsoftt12s2e1  
      doubleprecision rrgg2gghsoftt12s2e0  
      doubleprecision rrgg2gghsoftt12s2em1  
      doubleprecision rrgg2gghsoftt12s2em2  
      doubleprecision rrgg2gghsoftt12s2em3  
      doubleprecision rrgg2gghsoftt12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt12s1e1
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
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t6 = -0.1D1 + x3
      t7 = sqrt(x2)
      t8 = -0.1D1 + t7
      t10 = t7 + 0.1D1
      t14 = log(0.4D1 * x2 * t3 * x3 * t6 * t8 * t10)
      t15 = t14 ** 2
      t18 = cos(t1)
      t21 = x3 * t8 * t10 * t6
      t22 = Sqrt(t21)
      t26 = (-t7 + 0.2D1 * t7 * x3 + 0.2D1 * t18 * t22) ** 2
      t28 = x2 * x3
      t29 = t3 * t6
      t32 = log(-0.4D1 * t28 * t29)
      t33 = t32 ** 2
      t34 = t18 ** 2
      t37 = Sqrt(-x3 * t6)
      t38 = t37 ** 2
      t44 = wd * lh
      t53 = lh ** 2
      t55 = pi ** 2
      t57 = -0.180D3 * t53 + 0.30D2 * t55
      t60 = t34 * t38
      t65 = 0.1D1 / x2
      t68 = x3 * t3
      t71 = log(-0.4D1 * t68 * t6)
      t72 = t71 ** 2
      t74 = t38 * wd
      t95 = x1 ** 2
      t96 = t95 * t3
      t100 = log(-0.4D1 * t28 * t96 * t6)
      t105 = (-0.1D1 + x1) ** 2
      t106 = x2 * t105
      t110 = log(0.4D1 * t106 * t96 * t21)
      t117 = log(0.4D1 * x2 * t95 * t3 * t21)
      t124 = log(-0.4D1 * t68 * t95 * t106 * t6)
      t130 = 0.1D1 / x1
      t138 = log(-0.4D1 * t68 * t95 * t105 * t6)
      t139 = t138 ** 2
      t143 = log(-0.4D1 * x3 * t95 * t29)
      t144 = t143 ** 2
      t157 = (-0.90D2 * wd * (t15 * t26 - 0.4D1 * t33 * t34 * t38) + 0.1
     #80D3 * t44 * (-0.2D1 * t14 * t26 + 0.8D1 * t32 * t34 * t38) + wd *
     # t57 * (0.2D1 * t26 - 0.8D1 * t60)) * t65 / 0.160D3 - 0.9D1 / 0.2D
     #1 * t72 * t34 * t74 * lh - t60 * wd * (-0.60D2 * lh * t55 + 0.240D
     #3 * zeta3 + 0.120D3 * t53 * lh) / 0.20D2 - 0.3D1 / 0.4D1 * t72 * t
     #71 * t34 * t74 + t71 * t34 * t74 * t57 / 0.20D2 - 0.9D1 / 0.8D1 * 
     #wd * (0.8D1 * t100 * t34 * t38 + 0.2D1 * t110 * t26 - 0.2D1 * t117
     # * t26 - 0.8D1 * t124 * t34 * t38) * t130 * t65 - (-0.90D2 * t60 *
     # wd * (-t139 / 0.2D1 + t144 / 0.2D1) + 0.180D3 * t60 * t44 * (t138
     # - t143)) * t130 / 0.10D2
      t158 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t157)
      rrgg2gghsoftt12s1e1 = t158 * t157

      end function



      doubleprecision function rrgg2gghsoftt12s1e0
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
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t6 = -0.1D1 + x3
      t7 = sqrt(x2)
      t8 = -0.1D1 + t7
      t10 = t7 + 0.1D1
      t14 = log(0.4D1 * x2 * t3 * x3 * t6 * t8 * t10)
      t17 = cos(t1)
      t21 = Sqrt(x3 * t8 * t10 * t6)
      t25 = (-t7 + 0.2D1 * t7 * x3 + 0.2D1 * t17 * t21) ** 2
      t29 = t3 * t6
      t32 = log(-0.4D1 * x3 * x2 * t29)
      t33 = t17 ** 2
      t36 = Sqrt(-x3 * t6)
      t37 = t36 ** 2
      t45 = t33 * t37
      t54 = x3 * t3
      t55 = x1 ** 2
      t57 = (-0.1D1 + x1) ** 2
      t62 = log(-0.4D1 * t54 * t55 * t57 * t6)
      t66 = log(-0.4D1 * x3 * t55 * t29)
      t75 = log(-0.4D1 * t54 * t6)
      t77 = t37 * wd
      t81 = t75 ** 2
      t85 = lh ** 2
      t87 = pi ** 2
      t93 = (-0.90D2 * wd * (-0.2D1 * t14 * t25 + 0.8D1 * t32 * t33 * t3
     #7) + 0.180D3 * wd * lh * (0.2D1 * t25 - 0.8D1 * t45)) / x2 / 0.160
     #D3 + 0.9D1 * t45 * wd * (t62 - t66) / x1 + 0.9D1 * t75 * t33 * t77
     # * lh + 0.9D1 / 0.4D1 * t81 * t33 * t77 - t45 * wd * (-0.180D3 * t
     #85 + 0.30D2 * t87) / 0.20D2
      t94 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t93)
      rrgg2gghsoftt12s1e0 = t94 * t93

      end function



      doubleprecision function rrgg2gghsoftt12s1em1
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
      t1 = x4 * pi
      t2 = cos(t1)
      t3 = t2 ** 2
      t4 = -0.1D1 + x3
      t6 = Sqrt(-x3 * t4)
      t7 = t6 ** 2
      t8 = t3 * t7
      t12 = Sin(t1)
      t13 = t12 ** 2
      t17 = log(-0.4D1 * x3 * t13 * t4)
      t22 = sqrt(x2)
      t30 = Sqrt(x3 * (-0.1D1 + t22) * (t22 + 0.1D1) * t4)
      t34 = (-t22 + 0.2D1 * t22 * x3 + 0.2D1 * t2 * t30) ** 2
      t42 = -0.9D1 * t8 * wd * lh - 0.9D1 / 0.2D1 * t17 * t3 * t7 * wd -
     # 0.9D1 / 0.16D2 * wd * (0.2D1 * t34 - 0.8D1 * t8) / x2
      t43 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t42)
      rrgg2gghsoftt12s1em1 = t43 * t42

      end function



      doubleprecision function rrgg2gghsoftt12s1em2
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
      t2 = cos(x4 * pi)
      t3 = t2 ** 2
      t6 = Sqrt(-x3 * (-0.1D1 + x3))
      t7 = t6 ** 2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.9D1 /
     # 0.2D1 * t3 * t7 * wd)
      rrgg2gghsoftt12s1em2 = 0.9D1 / 0.2D1 * t11 * t3 * t7 * wd

      end function



      doubleprecision function rrgg2gghsoftt12s1em3
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
      rrgg2gghsoftt12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt12s1em4
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
      rrgg2gghsoftt12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt12s2e1
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
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t6 = -0.1D1 + x3
      t7 = sqrt(x2)
      t8 = -0.1D1 + t7
      t10 = t7 + 0.1D1
      t14 = log(0.4D1 * x2 * t3 * x3 * t6 * t8 * t10)
      t15 = t14 ** 2
      t18 = cos(t1)
      t21 = x3 * t8 * t10 * t6
      t22 = Sqrt(t21)
      t26 = (-t7 + 0.2D1 * t7 * x3 + 0.2D1 * t18 * t22) ** 2
      t28 = x2 * x3
      t29 = t3 * t6
      t32 = log(-0.4D1 * t28 * t29)
      t33 = t32 ** 2
      t34 = t18 ** 2
      t37 = Sqrt(-x3 * t6)
      t38 = t37 ** 2
      t44 = wd * lh
      t53 = lh ** 2
      t55 = pi ** 2
      t57 = -0.180D3 * t53 + 0.30D2 * t55
      t60 = t34 * t38
      t65 = 0.1D1 / x2
      t68 = x3 * t3
      t71 = log(-0.4D1 * t68 * t6)
      t72 = t71 ** 2
      t92 = x1 ** 2
      t93 = t92 * t3
      t97 = log(-0.4D1 * t28 * t93 * t6)
      t102 = (-0.1D1 + x1) ** 2
      t103 = x2 * t102
      t107 = log(0.4D1 * t103 * t93 * t21)
      t114 = log(0.4D1 * x2 * t92 * t3 * t21)
      t121 = log(-0.4D1 * t68 * t92 * t103 * t6)
      t127 = 0.1D1 / x1
      t135 = log(-0.4D1 * t68 * t92 * t102 * t6)
      t136 = t135 ** 2
      t140 = log(-0.4D1 * x3 * t92 * t29)
      t141 = t140 ** 2
      t154 = (-0.90D2 * wd * (t15 * t26 - 0.4D1 * t33 * t34 * t38) + 0.1
     #80D3 * t44 * (-0.2D1 * t14 * t26 + 0.8D1 * t32 * t34 * t38) + wd *
     # t57 * (0.2D1 * t26 - 0.8D1 * t60)) * t65 / 0.160D3 - (0.90D2 * t7
     #2 * wd * lh + wd * (-0.60D2 * lh * t55 + 0.240D3 * zeta3 + 0.120D3
     # * t53 * lh) + 0.15D2 * t72 * t71 * wd - t71 * wd * t57) * t34 * t
     #38 / 0.20D2 - 0.9D1 / 0.8D1 * wd * (0.8D1 * t97 * t34 * t38 + 0.2D
     #1 * t107 * t26 - 0.2D1 * t114 * t26 - 0.8D1 * t121 * t34 * t38) * 
     #t127 * t65 - (-0.90D2 * t60 * wd * (-t136 / 0.2D1 + t141 / 0.2D1) 
     #+ 0.180D3 * t60 * t44 * (t135 - t140)) * t127 / 0.10D2
      t155 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t154)
      rrgg2gghsoftt12s2e1 = t155 * t154

      end function



      doubleprecision function rrgg2gghsoftt12s2e0
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
      t1 = x4 * pi
      t2 = Sin(t1)
      t3 = t2 ** 2
      t6 = -0.1D1 + x3
      t7 = sqrt(x2)
      t8 = -0.1D1 + t7
      t10 = t7 + 0.1D1
      t14 = log(0.4D1 * x2 * t3 * x3 * t6 * t8 * t10)
      t17 = cos(t1)
      t21 = Sqrt(x3 * t8 * t10 * t6)
      t25 = (-t7 + 0.2D1 * t7 * x3 + 0.2D1 * t17 * t21) ** 2
      t29 = t3 * t6
      t32 = log(-0.4D1 * x3 * x2 * t29)
      t33 = t17 ** 2
      t36 = Sqrt(-x3 * t6)
      t37 = t36 ** 2
      t45 = t33 * t37
      t54 = x3 * t3
      t55 = x1 ** 2
      t57 = (-0.1D1 + x1) ** 2
      t62 = log(-0.4D1 * t54 * t55 * t57 * t6)
      t66 = log(-0.4D1 * x3 * t55 * t29)
      t75 = log(-0.4D1 * t54 * t6)
      t79 = t75 ** 2
      t82 = lh ** 2
      t84 = pi ** 2
      t92 = (-0.90D2 * wd * (-0.2D1 * t14 * t25 + 0.8D1 * t32 * t33 * t3
     #7) + 0.180D3 * wd * lh * (0.2D1 * t25 - 0.8D1 * t45)) / x2 / 0.160
     #D3 + 0.9D1 * t45 * wd * (t62 - t66) / x1 - (-0.180D3 * t75 * wd * 
     #lh - 0.45D2 * t79 * wd + wd * (-0.180D3 * t82 + 0.30D2 * t84)) * t
     #33 * t37 / 0.20D2
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t92)
      rrgg2gghsoftt12s2e0 = t93 * t92

      end function



      doubleprecision function rrgg2gghsoftt12s2em1
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
      t3 = x4 * pi
      t4 = Sin(t3)
      t5 = t4 ** 2
      t7 = -0.1D1 + x3
      t10 = log(-0.4D1 * x3 * t5 * t7)
      t14 = cos(t3)
      t15 = t14 ** 2
      t18 = Sqrt(-t7 * x3)
      t19 = t18 ** 2
      t22 = sqrt(x2)
      t30 = Sqrt(x3 * (-0.1D1 + t22) * (t22 + 0.1D1) * t7)
      t34 = (-t22 + 0.2D1 * t22 * x3 + 0.2D1 * t14 * t30) ** 2
      t43 = -(0.180D3 * wd * lh + 0.90D2 * t10 * wd) * t15 * t19 / 0.20D
     #2 - 0.9D1 / 0.16D2 * wd * (0.2D1 * t34 - 0.8D1 * t15 * t19) / x2
      t44 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t43)
      rrgg2gghsoftt12s2em1 = t44 * t43

      end function



      doubleprecision function rrgg2gghsoftt12s2em2
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
      t2 = cos(x4 * pi)
      t3 = t2 ** 2
      t6 = Sqrt(-x3 * (-0.1D1 + x3))
      t7 = t6 ** 2
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.9D1 /
     # 0.2D1 * t3 * t7 * wd)
      rrgg2gghsoftt12s2em2 = 0.9D1 / 0.2D1 * t11 * t3 * t7 * wd

      end function



      doubleprecision function rrgg2gghsoftt12s2em3
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
      rrgg2gghsoftt12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt12s2em4
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
      rrgg2gghsoftt12s2em4 = 0.0D0

      end function
