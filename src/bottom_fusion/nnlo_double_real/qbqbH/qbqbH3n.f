  
      subroutine qbqbH3n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision qbqbH31J1  
      doubleprecision qbqbH31J2  
      doubleprecision qbqbH3n1e1  
      doubleprecision qbqbH3n1e0  
      doubleprecision qbqbH3n1em1  
      doubleprecision qbqbH3n1em2  
      doubleprecision qbqbH3n1em3  
      doubleprecision qbqbH3n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=qbqbH3n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=qbqbH3n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=qbqbH3n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=qbqbH3n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=qbqbH3n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=qbqbH3n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function qbqbH3n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH31J1
      doubleprecision qbqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = x4 * 0.3141592653589793D1
      t7 = Sin(t6)
      t8 = t7 ** 2
      t9 = z ** 2
      t10 = 0.1D1 / t9
      t11 = t8 * t10
      t12 = x2 * t4
      t15 = log(-0.4D1 * t11 * t12)
      t18 = lh ** 2
      t19 = 0.180D3 * t18
      t20 = 0.3141592653589793D1 ** 2
      t21 = 0.30D2 * t20
      t22 = t15 ** 2
      t25 = t1 ** 2
      t27 = 0.1D1 / s
      t28 = qbqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t31 = -t19 + t21
      t43 = qbqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t47 = x2 * z
      t49 = 0.1D1 / (0.1D1 - x2 + t47)
      t52 = t49 * lh
      t53 = t25 * t27
      t54 = x3 * t8
      t55 = t10 * x2
      t56 = t55 * t4
      t59 = log(-0.4D1 * t54 * t56)
      t65 = t49 * t25
      t67 = t59 ** 2
      t74 = t49 * t31
      t76 = t74 * t53 * t43
      t78 = 0.1D1 / x3
      t82 = x1 ** 2
      t83 = x3 * t82
      t84 = t83 * t8
      t87 = log(-0.4D1 * t84 * t56)
      t93 = lh * t25
      t94 = t27 * t49
      t100 = 0.1D1 / x1
      t103 = t82 * t8
      t106 = log(-0.4D1 * t103 * t56)
      t114 = t106 ** 2
      t124 = -((-0.180D3 * t15 * lh - t19 + t21 - 0.45D2 * t22) * t25 * 
     #t27 * t28 + (-t15 * t31 + 0.90D2 * t22 * lh - 0.60D2 * lh * t20 + 
     #0.2884936567583026D3 + 0.120D3 * t18 * lh + 0.15D2 * t22 * t15) * 
     #t25 * t27 * t43) * t49 / 0.5760D4 + (0.180D3 * t52 * t53 * (-t28 +
     # t59 * t43) - 0.90D2 * t65 * t27 * (t59 * t28 - t67 * t43 / 0.2D1)
     # - t76) * t78 / 0.5760D4 + (-0.90D2 * t53 * (-t49 * t28 + t87 * t4
     #9 * t43) - 0.180D3 * t93 * t94 * t43) * t78 * t100 / 0.2880D4 - (0
     #.180D3 * t93 * t27 * (t28 - t106 * t43) * t49 - 0.90D2 * t53 * (-t
     #106 * t28 + t114 * t43 / 0.2D1) * t49 + t76) * t100 / 0.2880D4
      t125 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t
     #124)
      t128 = 0.2D1 * x2 * x3
      t129 = cos(t6)
      t130 = -0.1D1 + x3
      t133 = Sqrt(t12 * x3 * t130)
      t135 = 0.2D1 * t129 * t133
      t140 = qbqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t145 = log(0.4D1 * t54 * t10 * t12 * t130)
      t146 = qbqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t153 = t145 ** 2
      t170 = log(0.4D1 * t84 * t55 * t4 * t130)
      t183 = (0.180D3 * t52 * t53 * (t140 - t145 * t146) - 0.90D2 * t65 
     #* t27 * (-t145 * t140 + t153 * t146 / 0.2D1) + t74 * t53 * t146) *
     # t78 / 0.5760D4 + (-0.90D2 * t53 * (t49 * t140 - t170 * t49 * t146
     #) + 0.180D3 * t93 * t94 * t146) * t78 * t100 / 0.2880D4
      t184 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t128 - x2 + t135), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t128 + t135), 0.0D0, t183)
      t186 = -0.1D1 + x1
      t188 = x1 * z
      t189 = 0.1D1 - x1 + t188
      t190 = 0.1D1 / t189
      t201 = s * t25 * x2 * x1 * t186 * t190
      t202 = x1 * x2
      t203 = t202 * z
      t205 = 0.1D1 / (-0.1D1 + x1 - t188 + x2 - t202 - t47 + t203)
      t206 = t186 * t205
      t207 = -t186
      t208 = qbqbH31J2(s, XB1, XB2, z, lh, wd, t207, x2, 0.0D0, x4)
      t209 = t206 * t208
      t210 = t83 * t11
      t211 = t186 ** 2
      t212 = t190 * t211
      t213 = t12 * t212
      t216 = log(-0.4D1 * t210 * t213)
      t218 = qbqbH31J1(s, XB1, XB2, z, lh, wd, t207, x2, 0.0D0, x4)
      t219 = t205 * t218
      t224 = t93 * t27
      t225 = t206 * t218
      t234 = log(-0.4D1 * t103 * t10 * t213)
      t235 = t234 * t186
      t243 = t234 ** 2
      t256 = (-0.90D2 * t53 * (t209 - t216 * t186 * t219) + 0.180D3 * t2
     #24 * t225) * t78 * t100 / 0.2880D4 - (-0.180D3 * t93 * t27 * (t209
     # - t235 * t219) + 0.90D2 * t53 * (-t235 * t205 * t208 + t243 * t18
     #6 * t219 / 0.2D1) - t31 * t25 * t27 * t225) * t100 / 0.2880D4
      t257 = FJET(XB1, XB2, s, 0.0D0, -t2 * t186 * x2 * t190, x1 * t1 * 
     #s, t4 * t186 * t2, -t201, t256)
      t259 = x3 * x1
      t261 = t259 * z
      t262 = t259 * x2
      t263 = t259 * t47
      t268 = Sqrt(x3 * t4 * t189 * x2 * t130)
      t270 = 0.2D1 * t129 * t268
      t277 = 0.1D1 - x1 + t188 - x2 + t202 - t203 - x3 + t259 - t261 + t
     #128 - t262 + t263 + t270
      t281 = qbqbH31J2(s, XB1, XB2, z, lh, wd, t207, x2, x3, x4)
      t287 = log(0.4D1 * t210 * t12 * t212 * t130)
      t289 = qbqbH31J1(s, XB1, XB2, z, lh, wd, t207, x2, x3, x4)
      t298 = 0.90D2 * t53 * (t206 * t281 - t287 * t186 * t205 * t289) - 
     #0.180D3 * t224 * t206 * t289
      t302 = FJET(XB1, XB2, s, t2 * t259, t2 * t186 * (-x3 + t259 - t261
     # + t128 - t262 + t263 - x2 + t270) * t190, -t2 * x1 * t130, -t2 * 
     #t186 * t277 * t190, -t201, t298 * t78 * t100 / 0.2880D4)
      qbqbH3n1e1 = t125 * t124 + t184 * t183 + t257 * t256 + t302 * t298
     # * t78 * t100 / 0.2880D4

      end function



      doubleprecision function qbqbH3n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH31J1
      doubleprecision qbqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = x2 * z
      t8 = 0.1D1 / (0.1D1 - x2 + t6)
      t9 = t1 ** 2
      t10 = t8 * t9
      t11 = 0.1D1 / s
      t12 = qbqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = x3 * t15
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t18 * x2 * t4
      t23 = log(-0.4D1 * t16 * t20)
      t24 = qbqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t30 = lh * t9
      t31 = t11 * t8
      t34 = 0.180D3 * t30 * t31 * t24
      t36 = 0.1D1 / x3
      t39 = t9 * t11
      t40 = t39 * t8
      t42 = 0.1D1 / x1
      t46 = x1 ** 2
      t47 = t46 * t15
      t50 = log(-0.4D1 * t47 * t20)
      t61 = x2 * t4
      t64 = log(-0.4D1 * t15 * t18 * t61)
      t72 = lh ** 2
      t74 = 0.3141592653589793D1 ** 2
      t76 = t64 ** 2
      t85 = (-0.90D2 * t10 * t11 * (-t12 + t23 * t24) - t34) * t36 / 0.5
     #760D4 + t40 * t24 * t36 * t42 / 0.32D2 - (-0.90D2 * t39 * (t12 - t
     #50 * t24) * t8 + t34) * t42 / 0.2880D4 - ((0.180D3 * lh + 0.90D2 *
     # t64) * t9 * t11 * t12 + (-0.180D3 * t64 * lh - 0.180D3 * t72 + 0.
     #30D2 * t74 - 0.45D2 * t76) * t9 * t11 * t24) * t8 / 0.5760D4
      t86 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t8
     #5)
      t89 = 0.2D1 * x2 * x3
      t90 = cos(t13)
      t91 = -0.1D1 + x3
      t94 = Sqrt(t61 * x3 * t91)
      t96 = 0.2D1 * t90 * t94
      t101 = qbqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t106 = log(0.4D1 * t16 * t18 * t61 * t91)
      t107 = qbqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t123 = (-0.90D2 * t10 * t11 * (t101 - t106 * t107) + 0.180D3 * t30
     # * t31 * t107) * t36 / 0.5760D4 - t40 * t107 * t36 * t42 / 0.32D2
      t124 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t89 - x2 + t96), 0.0D
     #0, t2 * (0.1D1 - x2 - x3 + t89 + t96), 0.0D0, t123)
      t126 = -0.1D1 + x1
      t128 = x1 * z
      t129 = 0.1D1 - x1 + t128
      t130 = 0.1D1 / t129
      t141 = s * t9 * x2 * x1 * t126 * t130
      t142 = t39 * t126
      t143 = x1 * x2
      t144 = t143 * z
      t146 = 0.1D1 / (-0.1D1 + x1 - t128 + x2 - t143 - t6 + t144)
      t147 = -t126
      t148 = qbqbH31J1(s, XB1, XB2, z, lh, wd, t147, x2, 0.0D0, x4)
      t149 = t146 * t148
      t150 = t36 * t42
      t154 = t126 * t146
      t155 = qbqbH31J2(s, XB1, XB2, z, lh, wd, t147, x2, 0.0D0, x4)
      t158 = t126 ** 2
      t163 = log(-0.4D1 * t47 * t18 * t61 * t130 * t158)
      t176 = -t142 * t149 * t150 / 0.32D2 - (0.90D2 * t39 * (t154 * t155
     # - t163 * t126 * t149) - 0.180D3 * t30 * t11 * t154 * t148) * t42 
     #/ 0.2880D4
      t177 = FJET(XB1, XB2, s, 0.0D0, -t2 * t126 * x2 * t130, x1 * t1 * 
     #s, t4 * t126 * t2, -t141, t176)
      t179 = x3 * x1
      t181 = t179 * z
      t182 = t179 * x2
      t183 = t179 * t6
      t188 = Sqrt(x3 * t4 * t129 * x2 * t91)
      t190 = 0.2D1 * t90 * t188
      t197 = 0.1D1 - x1 + t128 - x2 + t143 - t144 - x3 + t179 - t181 + t
     #89 - t182 + t183 + t190
      t201 = qbqbH31J1(s, XB1, XB2, z, lh, wd, t147, x2, x3, x4)
      t203 = t146 * t201 * t150
      t206 = FJET(XB1, XB2, s, t2 * t179, t2 * t126 * (-x3 + t179 - t181
     # + t89 - t182 + t183 - x2 + t190) * t130, -t2 * x1 * t91, -t2 * t1
     #26 * t197 * t130, -t141, t142 * t203 / 0.32D2)
      qbqbH3n1e0 = t86 * t85 + t124 * t123 + t177 * t176 + t206 * t9 * t
     #11 * t126 * t203 / 0.32D2

      end function



      doubleprecision function qbqbH3n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH31J1
      doubleprecision qbqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = qbqbH31J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = z ** 2
      t19 = x2 * t4
      t22 = log(-0.4D1 * t15 / t16 * t19)
      t26 = qbqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t30 = x2 * z
      t32 = 0.1D1 / (0.1D1 - x2 + t30)
      t35 = t32 * t26
      t36 = 0.1D1 / x3
      t40 = 0.1D1 / x1
      t44 = -(-0.90D2 * t8 * t9 + (0.180D3 * lh + 0.90D2 * t22) * t6 * t
     #7 * t26) * t32 / 0.5760D4 + t8 * t35 * t36 / 0.64D2 + t8 * t35 * t
     #40 / 0.32D2
      t45 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t4
     #4)
      t48 = 0.2D1 * x2 * x3
      t49 = cos(t13)
      t53 = Sqrt(t19 * x3 * (-0.1D1 + x3))
      t55 = 0.2D1 * t49 * t53
      t61 = qbqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t63 = t7 * t61 * t36
      t66 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t48 - x2 + t55), 0.0D0
     #, t2 * (0.1D1 - x2 - x3 + t48 + t55), 0.0D0, -t32 * t6 * t63 / 0.6
     #4D2)
      t71 = -0.1D1 + x1
      t73 = x1 * z
      t75 = 0.1D1 / (0.1D1 - x1 + t73)
      t88 = x1 * x2
      t91 = 0.1D1 / (-0.1D1 + x1 - t73 + x2 - t88 - t30 + t88 * z)
      t93 = qbqbH31J1(s, XB1, XB2, z, lh, wd, -t71, x2, 0.0D0, x4)
      t98 = FJET(XB1, XB2, s, 0.0D0, -t2 * t71 * x2 * t75, x1 * t1 * s, 
     #t4 * t71 * t2, -s * t6 * x2 * x1 * t71 * t75, -t8 * t71 * t91 * t9
     #3 * t40 / 0.32D2)
      qbqbH3n1em1 = t45 * t44 - t66 * t32 * t6 * t63 / 0.64D2 - t98 * t6
     # * t7 * t71 * t91 * t93 * t40 / 0.32D2

      end function



      doubleprecision function qbqbH3n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH31J1
      doubleprecision qbqbH31J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t11 = 0.1D1 / (0.1D1 - x2 + x2 * z)
      t12 = qbqbH31J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, 0.0D0, x4)
      t16 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * (-0.1D1 + x2)
     #, 0.0D0, t6 * t7 * t11 * t12 / 0.64D2)
      qbqbH3n1em2 = t16 * t6 * t7 * t11 * t12 / 0.64D2

      end function



      doubleprecision function qbqbH3n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH31J1
      doubleprecision qbqbH31J2
      qbqbH3n1em3 = 0.0D0

      end function



      doubleprecision function qbqbH3n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH31J1
      doubleprecision qbqbH31J2
      qbqbH3n1em4 = 0.0D0

      end function
  
 

      doubleprecision function qbqbH31J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t20 = t8 * t4 + t10 * x2 - 0.2D1 * t13 * t17
      t21 = x1 * t5 * t20
      t23 = 0.1D1 - x1
      t26 = s - t2 * t21 - t2 * t23 * x3
      t27 = t26 * s
      t28 = t1 ** 2
      t32 = x2 * x1 * t23 * t5
      t37 = s ** 2
      t38 = t37 * z
      t44 = z ** 2
      t50 = t28 ** 2
      t52 = x2 ** 2
      t54 = x1 ** 2
      t55 = t23 ** 2
      t57 = t4 ** 2
      t58 = 0.1D1 / t57
      t72 = t20 ** 2
      qbqbH31J1 = -0.16D2 * wd * (0.2D1 * t27 * t28 * t32 + 0.2D1 * t27 
     #* z + 0.2D1 * t38 * t1 * t21 - t27 * t1 * t21 - t27 - 0.2D1 * t37 
     #* t44 - 0.2D1 * t38 * t28 * t32 - 0.2D1 * t37 * t50 * t52 * t54 * 
     #t55 * t58 + 0.4D1 * t37 * t28 * t1 * t54 * t58 * t20 * x2 * t23 - 
     #0.2D1 * t37 * t28 * t54 * t58 * t72) / t26

      end function
  
   
 

      doubleprecision function qbqbH31J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t21 = x1 / t4 * (t8 * t4 + t10 * x2 - 0.2D1 * t13 * t17)
      t26 = s - t2 * t21 - t2 * (0.1D1 - x1) * x3
      t27 = t26 * s
      qbqbH31J2 = -0.16D2 * wd * (-t27 * t1 * t21 + t27) / t26

      end function
  
 