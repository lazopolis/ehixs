  
      subroutine rrqg2qght14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght14s1e1  
      doubleprecision rrqg2qght14s1e0  
      doubleprecision rrqg2qght14s1em1  
      doubleprecision rrqg2qght14s1em2  
      doubleprecision rrqg2qght14s1em3  
      doubleprecision rrqg2qght14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght14s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = x2 * 0.3141592653589793D1
      t16 = cos(t15)
      t17 = t16 ** 2
      t18 = wd * t17
      t19 = t11 * t3
      t20 = t6 * t19
      t21 = t18 * t20
      t22 = 0.1D1 / z
      t23 = x1 * t22
      t24 = sin(t15)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t29 = t11 ** 2
      t30 = t25 * t27 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t36 = t33 * x4 * t34
      t39 = log(0.4D1 * t30 * t36)
      t40 = t39 ** 2
      t43 = 0.1D1 / (-0.2D1 + t1)
      t49 = t18 * t20 * x1
      t50 = t22 * lh
      t56 = lh ** 2
      t58 = 0.3141592653589793D1 ** 2
      t60 = 0.180D3 * t56 - 0.30D2 * t58
      t64 = t21 * t23 * t60 * t34 * t43
      t66 = 0.1D1 / x4
      t72 = log(0.4D1 * t30 * t33 * t34)
      t73 = t72 * wd
      t75 = (wd - t73) * t17
      t78 = t19 * x1
      t80 = t34 * t43
      t81 = t22 * t60
      t85 = t72 ** 2
      t87 = t85 * wd / 0.2D1
      t89 = (wd - t73 + t87) * t17
      t104 = x1 * t34
      t105 = t43 * t22
      t118 = wd * t6
      t119 = t118 * t78
      t120 = x3 * t25
      t125 = log(0.4D1 * t120 * t27 * t29 * t36)
      t128 = t17 * t34 * t43
      t136 = 0.1D1 / x3
      t141 = t29 * t31
      t146 = log(0.4D1 * t120 * t27 * t141 * t32 * t34)
      t147 = t146 ** 2
      t161 = 0.4D1 / 0.45D2 * (-0.45D2 * t21 * t23 * t40 * t34 * t43 - 0
     #.180D3 * t49 * t50 * t39 * t34 * t43 - t64) * t66 - 0.4D1 / 0.45D2
     # * (-t18 + t75) * t6 * t78 * t80 * t81 + 0.16D2 * (-t75 + t89) * t
     #6 * t78 * t80 * t50 - 0.8D1 * (-t89 + (wd - t73 + t87 - t85 * t72 
     #* wd / 0.6D1) * t17) * t6 * t19 * t104 * t105 - 0.4D1 / 0.45D2 * t
     #21 * t104 * t105 * (0.60D2 * lh * t58 - 0.2884936567583026D3 - 0.1
     #20D3 * t56 * lh) - (-0.360D3 * t119 * t22 * t125 * t128 - 0.720D3 
     #* t119 * t50 * t128) * t136 * t66 / 0.45D2 + 0.4D1 / 0.45D2 * (-0.
     #45D2 * t21 * t23 * t147 * t34 * t43 - 0.180D3 * t49 * t50 * t146 *
     # t34 * t43 - t64) * t136
      t162 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t161)
      t164 = sqrt(x4)
      t165 = -0.1D1 + t164
      t166 = t164 + 0.1D1
      t167 = t165 * t166
      t168 = KAPPA2(x1, x2, 0.0D0, -t167, z)
      t169 = s * t168
      t173 = t6 * t165 * t166
      t175 = t7 * x4
      t177 = t168 ** 2
      t180 = x1 * t6
      t184 = t177 ** 2
      t187 = x4 * t27
      t189 = t187 * t167 * t25
      t192 = log(-0.4D1 * t184 * t32 * t141 * t189)
      t193 = t192 ** 2
      t195 = Sqrt(-t167)
      t196 = t195 ** 2
      t199 = 0.1D1 / (-0.2D1 + t168)
      t200 = t196 * t184 * t199
      t214 = t118 * t78 * t22
      t217 = t32 * t29 * t31
      t221 = log(-0.4D1 * t184 * x3 * t217 * t189)
      t224 = t17 * t184 * t199
      t236 = 0.4D1 / 0.45D2 * (0.45D2 * t49 * t22 * t193 * t200 + 0.180D
     #3 * t49 * t50 * t192 * t200 + t49 * t81 * t200) * t66 - (0.360D3 *
     # t214 * t221 * t196 * t224 + 0.720D3 * t214 * lh * t196 * t224) * 
     #t136 * t66 / 0.45D2
      t237 = FJET(XB1, XB2, s, 0.0D0, t169 * t4, t169 * t3 * t173, -t169
     # * t175, s * t177 * t11 * t180 * (-0.1D1 + x4), t236)
      t239 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t240 = s * t239
      t241 = t4 * x3
      t244 = sqrt(x3)
      t245 = -0.1D1 + t244
      t247 = t244 + 0.1D1
      t248 = x1 * t245 * t247
      t251 = t239 ** 2
      t257 = t245 * t247
      t258 = x3 * t32
      t262 = t251 ** 2
      t264 = t262 * t27 * t25
      t268 = log(-0.4D1 * t257 * t258 * t29 * t31 * x4 * t264)
      t270 = Sqrt(-t257)
      t271 = t270 ** 2
      t275 = t271 * t262 / (-0.2D1 + t239)
      t291 = log(-0.4D1 * t257 * t258 * t141 * t264)
      t292 = t291 ** 2
      t306 = -(0.360D3 * t214 * t268 * t17 * t275 + 0.720D3 * t214 * lh 
     #* t17 * t275) * t136 * t66 / 0.45D2 + 0.4D1 / 0.45D2 * (0.45D2 * t
     #49 * t22 * t292 * t275 + 0.180D3 * t49 * t50 * t291 * t275 + t49 *
     # t81 * t275) * t136
      t307 = FJET(XB1, XB2, s, t240 * t241, -t240 * t3 * t248, -t240 * t
     #7, 0.0D0, s * t251 * t11 * t180 * (-0.1D1 + x3), t306)
      t309 = KAPPA2(x1, x2, x3, -t167, z)
      t310 = s * t309
      t312 = t310 * t3
      t316 = t309 ** 2
      t323 = Sqrt(t257 * t167)
      t333 = t316 ** 2
      t339 = log(0.4D1 * t257 * x3 * t217 * t187 * t165 * t166 * t333 * 
     #t25)
      t345 = (t244 * t164 - 0.2D1 * t16 * t323) ** 2
      t349 = t333 * t345 / (-0.2D1 + t309)
      t356 = -0.90D2 * t119 * t22 * t339 * t349 - 0.180D3 * t119 * t50 *
     # t349
      t360 = FJET(XB1, XB2, s, t310 * t241, -t312 * t248, t312 * t173, -
     #t310 * t175, s * t316 * t11 * t180 * (x3 - 0.1D1 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t16 * t244 * t164 * t323), -t356 * t136 * t66 / 0.
     #45D2)
      rrqg2qght14s1e1 = t162 * t161 + t237 * t236 + t307 * t306 - t360 *
     # t356 * t136 * t66 / 0.45D2

      end function



      doubleprecision function rrqg2qght14s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = x2 * 0.3141592653589793D1
      t16 = cos(t15)
      t17 = t16 ** 2
      t18 = wd * t17
      t19 = t11 * t3
      t20 = t6 * t19
      t21 = t18 * t20
      t22 = 0.1D1 / z
      t23 = x1 * t22
      t24 = sin(t15)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t29 = t11 ** 2
      t30 = t25 * t27 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t39 = log(0.4D1 * t30 * t33 * x4 * t34)
      t42 = 0.1D1 / (-0.2D1 + t1)
      t47 = wd * t6
      t48 = t19 * x1
      t50 = t22 * lh
      t51 = t17 * t34
      t55 = 0.180D3 * t47 * t48 * t50 * t51 * t42
      t57 = 0.1D1 / x4
      t61 = t47 * t48 * t22
      t62 = 0.1D1 / x3
      t70 = t29 * t31
      t75 = log(0.4D1 * x3 * t25 * t27 * t70 * t32 * t34)
      t84 = lh ** 2
      t86 = 0.3141592653589793D1 ** 2
      t97 = log(0.4D1 * t30 * t33 * t34)
      t98 = t97 * wd
      t100 = (wd - t98) * t17
      t108 = t97 ** 2
      t121 = 0.4D1 / 0.45D2 * (0.90D2 * t21 * t23 * t39 * t34 * t42 + t5
     #5) * t57 - 0.8D1 * t61 * t51 * t42 * t62 * t57 + 0.4D1 / 0.45D2 * 
     #(0.90D2 * t21 * t23 * t75 * t34 * t42 + t55) * t62 - 0.4D1 / 0.45D
     #2 * t21 * t23 * (0.180D3 * t84 - 0.30D2 * t86) * t34 * t42 + 0.16D
     #2 * (-t18 + t100) * t6 * t48 * t34 * t42 * t50 - 0.8D1 * (-t100 + 
     #(wd - t98 + t108 * wd / 0.2D1) * t17) * t6 * t19 * x1 * t34 * t42 
     #* t22
      t122 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t121)
      t124 = sqrt(x4)
      t125 = -0.1D1 + t124
      t126 = t124 + 0.1D1
      t127 = t125 * t126
      t128 = KAPPA2(x1, x2, 0.0D0, -t127, z)
      t129 = s * t128
      t133 = t6 * t125 * t126
      t135 = t7 * x4
      t137 = t128 ** 2
      t140 = x1 * t6
      t144 = t20 * x1
      t145 = t18 * t144
      t146 = t137 ** 2
      t154 = log(-0.4D1 * t146 * t32 * t70 * x4 * t27 * t127 * t25)
      t156 = Sqrt(-t127)
      t157 = t156 ** 2
      t160 = 0.1D1 / (-0.2D1 + t128)
      t181 = 0.4D1 / 0.45D2 * (-0.90D2 * t145 * t22 * t154 * t157 * t146
     # * t160 - 0.180D3 * t61 * lh * t157 * t17 * t146 * t160) * t57 + 0
     #.8D1 * t61 * t157 * t17 * t146 * t160 * t62 * t57
      t182 = FJET(XB1, XB2, s, 0.0D0, t129 * t4, t129 * t3 * t133, -t129
     # * t135, s * t137 * t11 * t140 * (-0.1D1 + x4), t181)
      t184 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t185 = s * t184
      t186 = t4 * x3
      t189 = sqrt(x3)
      t190 = -0.1D1 + t189
      t192 = t189 + 0.1D1
      t193 = x1 * t190 * t192
      t196 = t184 ** 2
      t202 = t190 * t192
      t203 = Sqrt(-t202)
      t204 = t203 ** 2
      t206 = t196 ** 2
      t209 = 0.1D1 / (-0.2D1 + t184)
      t222 = log(-0.4D1 * t202 * x3 * t32 * t70 * t206 * t27 * t25)
      t225 = t204 * t206 * t209
      t236 = 0.8D1 * t61 * t17 * t204 * t206 * t209 * t62 * t57 + 0.4D1 
     #/ 0.45D2 * (-0.90D2 * t145 * t22 * t222 * t225 - 0.180D3 * t61 * l
     #h * t17 * t225) * t62
      t237 = FJET(XB1, XB2, s, t185 * t186, -t185 * t3 * t193, -t185 * t
     #7, 0.0D0, s * t196 * t11 * t140 * (-0.1D1 + x3), t236)
      t239 = KAPPA2(x1, x2, x3, -t127, z)
      t240 = s * t239
      t242 = t240 * t3
      t246 = t239 ** 2
      t253 = Sqrt(t202 * t127)
      t260 = t246 ** 2
      t265 = (t189 * t124 - 0.2D1 * t16 * t253) ** 2
      t270 = 0.1D1 / (-0.2D1 + t239) * t62 * t57
      t274 = FJET(XB1, XB2, s, t240 * t186, -t242 * t193, t242 * t133, -
     #t240 * t135, s * t246 * t11 * t140 * (x3 - 0.1D1 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t16 * t189 * t124 * t253), -0.2D1 * t61 * t260 * t
     #265 * t270)
      rrqg2qght14s1e0 = t122 * t121 + t182 * t181 + t237 * t236 - 0.2D1 
     #* t274 * wd * t144 * t22 * t260 * t265 * t270

      end function



      doubleprecision function rrqg2qght14s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = t11 * t3
      t18 = wd * t6 * t16 * x1
      t19 = 0.1D1 / z
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = t19 * t22
      t24 = t9 ** 2
      t26 = 0.1D1 / (-0.2D1 + t1)
      t27 = t24 * t26
      t28 = 0.1D1 / x3
      t39 = wd * t22
      t40 = sin(t20)
      t41 = t40 ** 2
      t42 = z ** 2
      t45 = t11 ** 2
      t47 = x1 ** 2
      t48 = t6 ** 2
      t53 = log(0.4D1 * t41 / t42 * t45 * t47 * t48 * t24)
      t65 = 0.1D1 / x4
      t70 = -0.8D1 * t18 * t23 * t27 * t28 + 0.16D2 * t18 * t19 * lh * t
     #22 * t24 * t26 - 0.8D1 * (-t39 + (wd - t53 * wd) * t22) * t6 * t16
     # * x1 * t24 * t26 * t19 - 0.8D1 * t18 * t23 * t27 * t65
      t71 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 *
     # t11 * x1 * t6, t70)
      t73 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t74 = s * t73
      t78 = sqrt(x3)
      t79 = -0.1D1 + t78
      t81 = t78 + 0.1D1
      t85 = t73 ** 2
      t88 = x1 * t6
      t94 = t39 * t6 * t16 * x1
      t96 = Sqrt(-t79 * t81)
      t97 = t96 ** 2
      t99 = t85 ** 2
      t103 = t99 / (-0.2D1 + t73) * t28
      t107 = FJET(XB1, XB2, s, t74 * t4 * x3, -t74 * t3 * x1 * t79 * t81
     #, -t74 * t7, 0.0D0, s * t85 * t11 * t88 * (-0.1D1 + x3), 0.8D1 * t
     #94 * t19 * t97 * t103)
      t110 = t22 * t6 * t16
      t112 = t19 * x1
      t117 = sqrt(x4)
      t118 = -0.1D1 + t117
      t119 = t117 + 0.1D1
      t120 = t118 * t119
      t121 = KAPPA2(x1, x2, 0.0D0, -t120, z)
      t122 = s * t121
      t130 = t121 ** 2
      t136 = Sqrt(-t120)
      t137 = t136 ** 2
      t139 = t130 ** 2
      t143 = t139 / (-0.2D1 + t121) * t65
      t147 = FJET(XB1, XB2, s, 0.0D0, t122 * t4, t122 * t3 * t6 * t118 *
     # t119, -t122 * t7 * x4, s * t130 * t11 * t88 * (-0.1D1 + x4), 0.8D
     #1 * t94 * t19 * t137 * t143)
      rrqg2qght14s1em1 = t71 * t70 + 0.8D1 * t107 * wd * t110 * t112 * t
     #97 * t103 + 0.8D1 * t147 * wd * t110 * t112 * t137 * t143

      end function



      doubleprecision function rrqg2qght14s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = t11 * t3
      t19 = 0.1D1 / z
      t21 = cos(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t24 = t9 ** 2
      t26 = 0.1D1 / (-0.2D1 + t1)
      t31 = FJET(XB1, XB2, s, 0.0D0, t2 * t3 * x1, -t2 * t3 * t6, 0.0D0,
     # -s * t9 * t11 * x1 * t6, -0.8D1 * wd * t6 * t16 * x1 * t19 * t22 
     #* t24 * t26)
      rrqg2qght14s1em2 = -0.8D1 * t31 * wd * t6 * t16 * x1 * t19 * t22 *
     # t24 * t26

      end function



      doubleprecision function rrqg2qght14s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght14s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght14s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght14s1em4 = 0.0D0

      end function
