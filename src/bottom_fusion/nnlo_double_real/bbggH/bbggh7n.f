  
      subroutine bbggh7n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bbggh7n1e1  
      doubleprecision bbggh7n1e0  
      doubleprecision bbggh7n1em1  
      doubleprecision bbggh7n1em2  
      doubleprecision bbggh7n1em3  
      doubleprecision bbggh7n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bbggh7n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bbggh7n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bbggh7n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bbggh7n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bbggh7n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bbggh7n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bbggh7n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t15 = log(-0.4D1 * x3 * t8 * t11 * t4)
      t16 = t15 * z
      t19 = lh ** 2
      t21 = 0.3141592653589793D1 ** 2
      t23 = 0.180D3 * t19 - 0.30D2 * t21
      t24 = z * t23
      t25 = t15 ** 2
      t26 = t25 * z
      t30 = t4 * wd
      t49 = t1 * z
      t50 = t49 * wd
      t52 = z * lh
      t53 = t1 * wd
      t55 = 0.180D3 * t52 * t53
      t56 = -0.90D2 * t50 - t55
      t57 = x2 * x3
      t58 = t8 * t11
      t59 = t58 * t4
      t62 = log(-0.4D1 * t57 * t59)
      t66 = t62 ** 2
      t72 = t55 + t24 * t53
      t73 = 0.18D2 * t4
      t76 = 0.1D1 / x2
      t79 = x1 ** 2
      t80 = t57 * t79
      t83 = log(-0.4D1 * t80 * t59)
      t91 = 0.1D1 / x1
      t97 = t1 * t4 * wd
      t99 = 0.180D3 * t52 * t97
      t100 = -0.90D2 * t49 * t30 - t99
      t101 = x3 * t79
      t104 = log(-0.4D1 * t101 * t59)
      t106 = t104 ** 2
      t110 = t24 * t97
      t114 = (0.180D3 * t16 * lh + t24 + 0.45D2 * t26) * t1 * t30 / 0.30
     #D2 - (-t16 * t23 - 0.90D2 * t26 * lh + z * (-0.2884936567583026D3 
     #- 0.120D3 * t19 * lh + 0.60D2 * lh * t21) - 0.15D2 * t25 * t15 * z
     #) * t1 * t30 / 0.30D2 - (-0.18D2 * t56 * t62 * t4 + 0.810D3 * t49 
     #* wd * t66 * t4 + t72 * t73) * t76 / 0.540D3 - (-0.1620D4 * t49 * 
     #wd * t83 * t4 + t56 * t73) * t76 * t91 / 0.270D3 - (-t100 * t104 +
     # 0.45D2 * t49 * t30 * t106 + t99 + t110) * t91 / 0.15D2
      t115 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t114)
      t117 = 0.2D1 * t57
      t118 = cos(t6)
      t119 = -0.1D1 + x2
      t121 = x3 * t4
      t123 = Sqrt(x2 * t119 * t121)
      t125 = 0.2D1 * t118 * t123
      t127 = t2 * (-x3 + t117 - x2 + t125)
      t129 = t2 * (0.1D1 - x2 - x3 + t117 + t125)
      t130 = x2 * z
      t132 = (0.1D1 - x2 + t130) ** 2
      t133 = 0.1D1 / t132
      t135 = 0.5D1 * x2 - 0.5D1 * t130
      t136 = t133 * t135
      t142 = log(0.4D1 * x2 * t8 * t11 * t121 * t119)
      t143 = t142 * t133
      t144 = 0.18D2 * x3
      t145 = 0.13D2 * x2
      t146 = 0.13D2 * t130
      t147 = 0.18D2 - t144 - t145 + t146
      t152 = t142 ** 2
      t169 = log(0.4D1 * t80 * t58 * t119 * t4)
      t182 = -(t56 * (t136 - t143 * t147) + 0.90D2 * t49 * wd * (-t143 *
     # t135 + t152 * t133 * t147 / 0.2D1) + t72 * t133 * t147) * t76 / 0
     #.540D3 - (0.90D2 * t49 * wd * (t136 - t169 * t133 * t147) + t56 * 
     #t133 * t147) * t76 * t91 / 0.270D3
      t183 = FJET(XB1, XB2, s, 0.0D0, -t127, 0.0D0, t129, 0.0D0, t182)
      t185 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t114)
      t187 = x1 * x3
      t188 = t2 * t187
      t189 = -0.1D1 + x1
      t190 = t187 * z
      t191 = t57 * x1
      t192 = x1 * z
      t193 = t57 * t192
      t195 = 0.1D1 - x1 + t192
      t199 = Sqrt(x3 * t119 * t195 * x2 * t4)
      t201 = 0.2D1 * t118 * t199
      t204 = 0.1D1 / t195
      t206 = t2 * t189 * (-x3 + t187 - t190 + t117 - t191 + t193 - x2 + 
     #t201) * t204
      t207 = t4 * s
      t209 = t207 * t1 * x1
      t210 = x2 * x1
      t211 = t210 * z
      t212 = 0.1D1 - x1 + t192 - x2 + t210 - t211 - x3 + t187 - t190 + t
     #117 - t191 + t193 + t201
      t215 = t2 * t189 * t212 * t204
      t216 = t1 ** 2
      t221 = s * t216 * x2 * x1 * t189 * t204
      t227 = t57 * t79 * t8
      t228 = t11 * t204
      t229 = t189 ** 2
      t235 = log(0.4D1 * t227 * t228 * t229 * t119 * t4)
      t242 = 0.18D2 * t190 - 0.13D2 * t210 - t146 + 0.13D2 * t211 + t145
     # + 0.18D2 * x1 - 0.18D2 * t192 + t144 - 0.18D2 - 0.18D2 * t187
      t247 = (-0.1D1 + x1 - t192 + x2 - t210 - t130 + t211) ** 2
      t248 = 0.1D1 / t247
      t255 = 0.90D2 * t50 * (-0.5D1 * x2 + 0.5D1 * t210 + 0.5D1 * t130 -
     # 0.5D1 * t211 - t235 * t242) * t195 * t248 + t56 * t242 * t195 * t
     #248
      t258 = t255 * t76 * t91 / 0.270D3
      t259 = FJET(XB1, XB2, s, t188, t206, -t209, -t215, -t221, -t258)
      t261 = t76 * t91
      t265 = t2 * t189 * x3
      t267 = t207 * t1 * t189
      t269 = t228 * t229 * t4
      t272 = log(-0.4D1 * t227 * t269)
      t276 = 0.1620D4 * t49 * wd * t272 * t4
      t286 = log(-0.4D1 * t101 * t8 * t269)
      t288 = t286 ** 2
      t294 = (t100 * t286 - 0.45D2 * t49 * t30 * t288 - t99 - t110) * t9
     #1 / 0.15D2
      t295 = -(t276 - t56 * t73) * t76 * t91 / 0.270D3 - t294
      t296 = FJET(XB1, XB2, s, t188, -t265, -t209, t267, 0.0D0, t295)
      t298 = FJET(XB1, XB2, s, t206, t188, -t215, -t209, -t221, -t258)
      t302 = FJET(XB1, XB2, s, -t127, 0.0D0, t129, 0.0D0, 0.0D0, t182)
      t310 = -(t276 - 0.18D2 * t56 * t4) * t76 * t91 / 0.270D3 - t294
      t311 = FJET(XB1, XB2, s, -t265, t188, t267, -t209, 0.0D0, t310)
      bbggh7n1e1 = t115 * t114 + t183 * t182 + t185 * t114 - t259 * t255
     # * t261 / 0.270D3 + t296 * t295 - t298 * t255 * t261 / 0.270D3 + t
     #302 * t182 + t311 * t310

      end function



      doubleprecision function bbggh7n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = t1 * z
      t7 = x2 * x3
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t22 = t6 * wd
      t24 = z * lh
      t25 = t1 * wd
      t28 = -0.90D2 * t22 - 0.180D3 * t24 * t25
      t29 = 0.18D2 * t4
      t32 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t40 = t4 * wd
      t41 = x1 ** 2
      t42 = x3 * t41
      t45 = log(-0.4D1 * t42 * t14)
      t50 = 0.90D2 * t6 * t40
      t54 = 0.180D3 * t24 * t1 * t4 * wd
      t63 = log(-0.4D1 * x3 * t10 * t12 * t4)
      t64 = t63 * z
      t72 = lh ** 2
      t74 = 0.3141592653589793D1 ** 2
      t78 = t63 ** 2
      t85 = -(-0.1620D4 * t6 * wd * t17 * t4 + t28 * t29) * t32 / 0.540D
     #3 - t22 * t29 * t32 * t36 / 0.3D1 - (-0.90D2 * t6 * t40 * t45 - t5
     #0 - t54) * t36 / 0.15D2 + (-0.180D3 * t24 - 0.90D2 * t64) * t1 * t
     #40 / 0.30D2 - (0.180D3 * t64 * lh + z * (0.180D3 * t72 - 0.30D2 * 
     #t74) + 0.45D2 * t78 * z) * t1 * t40 / 0.30D2
      t86 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t85)
      t88 = 0.2D1 * t7
      t89 = cos(t8)
      t90 = -0.1D1 + x2
      t92 = x3 * t4
      t94 = Sqrt(x2 * t90 * t92)
      t96 = 0.2D1 * t89 * t94
      t98 = t2 * (-x3 + t88 - x2 + t96)
      t100 = t2 * (0.1D1 - x2 - x3 + t88 + t96)
      t101 = x2 * z
      t103 = (0.1D1 - x2 + t101) ** 2
      t104 = 0.1D1 / t103
      t113 = log(0.4D1 * x2 * t10 * t12 * t92 * t90)
      t115 = 0.18D2 * x3
      t116 = 0.13D2 * x2
      t117 = 0.13D2 * t101
      t118 = 0.18D2 - t115 - t116 + t117
      t130 = t32 * t36
      t134 = -(0.90D2 * t6 * wd * (t104 * (0.5D1 * x2 - 0.5D1 * t101) - 
     #t113 * t104 * t118) + t28 * t104 * t118) * t32 / 0.540D3 - t22 * t
     #104 * t118 * t130 / 0.3D1
      t135 = FJET(XB1, XB2, s, 0.0D0, -t98, 0.0D0, t100, 0.0D0, t134)
      t137 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t85)
      t139 = x1 * x3
      t140 = t2 * t139
      t141 = -0.1D1 + x1
      t142 = t139 * z
      t143 = t7 * x1
      t144 = x1 * z
      t145 = t7 * t144
      t147 = 0.1D1 - x1 + t144
      t151 = Sqrt(x3 * t90 * t147 * x2 * t4)
      t153 = 0.2D1 * t89 * t151
      t156 = 0.1D1 / t147
      t158 = t2 * t141 * (-x3 + t139 - t142 + t88 - t143 + t145 - x2 + t
     #153) * t156
      t159 = t4 * s
      t161 = t159 * t1 * x1
      t162 = x2 * x1
      t163 = t162 * z
      t164 = 0.1D1 - x1 + t144 - x2 + t162 - t163 - x3 + t139 - t142 + t
     #88 - t143 + t145 + t153
      t167 = t2 * t141 * t164 * t156
      t168 = t1 ** 2
      t173 = s * t168 * x2 * x1 * t141 * t156
      t180 = 0.18D2 * t142 - 0.13D2 * t162 - t117 + 0.13D2 * t163 + t116
     # + 0.18D2 * x1 - 0.18D2 * t144 + t115 - 0.18D2 - 0.18D2 * t139
      t184 = (-0.1D1 + x1 - t144 + x2 - t162 - t101 + t163) ** 2
      t185 = 0.1D1 / t184
      t189 = t6 * wd * t180 * t147 * t185 * t130 / 0.3D1
      t190 = FJET(XB1, XB2, s, t140, t158, -t161, -t167, -t173, -t189)
      t196 = t180 * t147 * t185 * t32 * t36
      t200 = t2 * t141 * x3
      t202 = t159 * t1 * t141
      t210 = t141 ** 2
      t215 = log(-0.4D1 * t42 * t10 * t12 * t156 * t210 * t4)
      t221 = (0.90D2 * t6 * t40 * t215 + t50 + t54) * t36 / 0.15D2
      t222 = t22 * t29 * t32 * t36 / 0.3D1 - t221
      t223 = FJET(XB1, XB2, s, t140, -t200, -t161, t202, 0.0D0, t222)
      t225 = FJET(XB1, XB2, s, t158, t140, -t167, -t161, -t173, -t189)
      t230 = FJET(XB1, XB2, s, -t98, 0.0D0, t100, 0.0D0, 0.0D0, t134)
      t236 = 0.6D1 * t22 * t4 * t32 * t36 - t221
      t237 = FJET(XB1, XB2, s, -t200, t140, t202, -t161, 0.0D0, t236)
      bbggh7n1e0 = t86 * t85 + t135 * t134 + t137 * t85 - t190 * z * t25
     # * t196 / 0.3D1 + t223 * t222 - t225 * z * t25 * t196 / 0.3D1 + t2
     #30 * t134 + t237 * t236

      end function



      doubleprecision function bbggh7n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = t1 * z
      t7 = t4 * wd
      t12 = x4 * 0.3141592653589793D1
      t13 = Sin(t12)
      t14 = t13 ** 2
      t16 = z ** 2
      t21 = log(-0.4D1 * x3 * t14 / t16 * t4)
      t30 = 0.1D1 / x2
      t35 = t7 / x1
      t37 = 0.6D1 * t6 * t35
      t38 = 0.3D1 * t6 * t7 - (-0.180D3 * z * lh - 0.90D2 * t21 * z) * t
     #1 * t7 / 0.30D2 - 0.3D1 * t6 * t4 * wd * t30 - t37
      t39 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t38)
      t42 = 0.2D1 * x2 * x3
      t43 = cos(t12)
      t48 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t50 = 0.2D1 * t43 * t48
      t52 = t2 * (-x3 + t42 - x2 + t50)
      t54 = t2 * (0.1D1 - x2 - x3 + t42 + t50)
      t56 = x2 * z
      t58 = (0.1D1 - x2 + t56) ** 2
      t59 = 0.1D1 / t58
      t63 = 0.18D2 - 0.18D2 * x3 - 0.13D2 * x2 + 0.13D2 * t56
      t67 = t6 * wd * t59 * t63 * t30 / 0.6D1
      t68 = FJET(XB1, XB2, s, 0.0D0, -t52, 0.0D0, t54, 0.0D0, -t67)
      t73 = wd * t59 * t63 * t30
      t76 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t38)
      t79 = t2 * x1 * x3
      t80 = -0.1D1 + x1
      t82 = t2 * t80 * x3
      t83 = t4 * s
      t85 = t83 * t1 * x1
      t87 = t83 * t1 * t80
      t88 = FJET(XB1, XB2, s, t79, -t82, -t85, t87, 0.0D0, t37)
      t93 = FJET(XB1, XB2, s, -t52, 0.0D0, t54, 0.0D0, 0.0D0, -t67)
      t98 = FJET(XB1, XB2, s, -t82, t79, t87, -t85, 0.0D0, t37)
      bbggh7n1em1 = t39 * t38 - t68 * z * t1 * t73 / 0.6D1 + t76 * t38 +
     # 0.6D1 * t88 * z * t1 * t35 - t93 * z * t1 * t73 / 0.6D1 + 0.6D1 *
     # t98 * z * t1 * t35

      end function



      doubleprecision function bbggh7n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t9 = 0.3D1 * z * t1 * t4 * wd
      t10 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t9)
      t13 = t1 * t4 * wd
      t15 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t9)
      bbggh7n1em2 = -0.3D1 * t10 * z * t13 - 0.3D1 * t15 * z * t13

      end function



      doubleprecision function bbggh7n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh7n1em3 = 0.0D0

      end function



      doubleprecision function bbggh7n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      bbggh7n1em4 = 0.0D0

      end function
