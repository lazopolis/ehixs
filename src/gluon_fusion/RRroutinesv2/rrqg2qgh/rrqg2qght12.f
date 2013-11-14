  
      subroutine rrqg2qght12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght12s1e1  
      doubleprecision rrqg2qght12s1e0  
      doubleprecision rrqg2qght12s1em1  
      doubleprecision rrqg2qght12s1em2  
      doubleprecision rrqg2qght12s1em3  
      doubleprecision rrqg2qght12s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght12s1e1
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
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = t1 * z
      t20 = x2 * z
      t22 = (t20 - x2 + 0.1D1) ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t25 = Sin(t5)
      t26 = t25 ** 2
      t27 = t3 * t26
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t34 = t32 * t7 * t9
      t37 = log(0.4D1 * t27 * t34)
      t38 = t37 ** 2
      t40 = -x2 + t20 - 0.1D1 + x3
      t44 = t23 * wd
      t47 = 0.180D3 * t19 * t44 * lh
      t50 = -t47 - 0.90D2 * t19 * t44
      t53 = lh ** 2
      t55 = pi ** 2
      t57 = 0.180D3 * t53 - 0.30D2 * t55
      t60 = t47 + t19 * t44 * t57
      t63 = 0.1D1 / x3
      t66 = t19 * wd
      t67 = x1 ** 2
      t68 = t67 * t26
      t69 = t3 * t68
      t72 = log(0.4D1 * t69 * t34)
      t79 = 0.180D3 * t19 * wd * lh
      t81 = -t79 - 0.90D2 * t66
      t86 = 0.1D1 / x1
      t89 = -(0.45D2 * t24 * wd * t38 * t40 - t50 * t37 * t40 + t60 * t4
     #0) * t63 / 0.810D3 + (0.90D2 * t66 * t72 * t40 * t23 - t81 * t40 *
     # t23) * t63 * t86 / 0.405D3
      t90 = FJET(XB1, XB2, s, 0.0D0, t2 * (0.1D1 - x2 - x3 + t4 + t14), 
     #0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, t89)
      t94 = t32 * t7
      t97 = log(-0.4D1 * t27 * t94)
      t98 = t97 ** 2
      t100 = -x2 + t20 - 0.1D1
      t110 = t100 * t23
      t112 = t19 * t110 * wd
      t113 = t19 * t100
      t118 = log(-0.4D1 * x2 * t26 * t94)
      t120 = z * t100
      t121 = t118 * t1 * t120
      t124 = (-0.2D1 * t113 - t121) * t23 * wd
      t126 = t118 ** 2
      t128 = t126 * t1 * t120
      t132 = (t113 + 0.2D1 * t121 + t128 / 0.2D1) * t23 * wd
      t160 = t26 * t29
      t161 = t31 * t7
      t165 = log(-0.4D1 * t3 * t67 * t160 * t161)
      t176 = x2 * t67
      t180 = log(-0.4D1 * t176 * t26 * t94)
      t181 = t180 ** 2
      t190 = t79 + t19 * wd * t57
      t196 = -(-0.45D2 * t24 * wd * t98 * t100 + t50 * t97 * t100 - t60 
     #* t100) * t63 / 0.810D3 - 0.2D1 / 0.9D1 * (t112 + t124 + t132) * l
     #h + t113 * t44 * (0.60D2 * lh * t55 - 0.240D3 * zeta3 - 0.120D3 * 
     #t53 * lh) / 0.810D3 + t112 / 0.9D1 + t124 / 0.9D1 + t132 / 0.9D1 +
     # (-t121 - t128 - t126 * t118 * t1 * t120 / 0.6D1) * t23 * wd / 0.9
     #D1 + (t112 + t124) * t57 / 0.810D3 + (-0.90D2 * t66 * t165 * t100 
     #* t23 + t81 * t100 * t23) * t63 * t86 / 0.405D3 + (0.45D2 * t66 * 
     #t181 * t100 * t23 - t81 * t180 * t110 + t190 * t100 * t23) * t86 /
     # 0.405D3
      t197 = FJET(XB1, XB2, s, 0.0D0, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, t
     #196)
      t201 = -0.1D1 + x1
      t205 = x1 * z
      t206 = 0.1D1 - x1 + t205
      t207 = 0.1D1 / t206
      t214 = s * t30 * x2 * t201 * x1 * t207
      t215 = t201 ** 2
      t216 = t207 * t215
      t221 = log(-0.4D1 * t69 * t32 * t216 * t7)
      t222 = x2 * x1
      t223 = t222 * z
      t225 = (-t20 + t223 - t205 + x2 - t222 - 0.1D1 + x1) ** 2
      t226 = 0.1D1 / t225
      t228 = x2 - t222 - t20 + t223 + 0.1D1 - x1 + t205
      t229 = t201 * t228
      t233 = t81 * t226
      t242 = log(-0.4D1 * t176 * t160 * t161 * t216)
      t243 = t242 ** 2
      t257 = (0.90D2 * t66 * t221 * t226 * t229 - t233 * t229) * t63 * t
     #86 / 0.405D3 + (-0.45D2 * t66 * t243 * t226 * t229 + t81 * t242 * 
     #t226 * t201 * t228 - t190 * t226 * t229) * t86 / 0.405D3
      t258 = FJET(XB1, XB2, s, t2 * x1, t7 * s * t1 * t201, 0.0D0, -t2 *
     # t201 * x2 * t207, -t214, t257)
      t263 = x3 * x1
      t264 = t263 * z
      t265 = t3 * x1
      t266 = t3 * t205
      t271 = Sqrt(x3 * t7 * t206 * x2 * t9)
      t273 = 0.2D1 * t6 * t271
      t274 = 0.1D1 - x1 + t205 - x2 + t222 - t223 - x3 + t263 - t264 + t
     #4 - t265 + t266 + t273
      t291 = log(0.4D1 * t3 * t68 * t29 * t31 * t207 * t215 * t7 * t9)
      t294 = (x2 - t222 - t20 + t223 + 0.1D1 - x1 + t205 - x3 + t263 - t
     #264) * t201
      t299 = -0.90D2 * t66 * t291 * t226 * t294 + t233 * t294
      t303 = FJET(XB1, XB2, s, -t9 * s * t1 * x1, -t2 * t201 * t274 * t2
     #07, t2 * t263, t2 * t201 * (-x3 + t263 - t264 + t4 - t265 + t266 -
     # x2 + t273) * t207, -t214, t299 * t63 * t86 / 0.405D3)
      rrqg2qght12s1e1 = t90 * t89 + t197 * t196 + t258 * t257 + t303 * t
     #299 * t63 * t86 / 0.405D3

      end function



      doubleprecision function rrqg2qght12s1e0
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
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = t1 * z
      t20 = x2 * z
      t22 = (t20 - x2 + 0.1D1) ** 2
      t23 = 0.1D1 / t22
      t24 = t19 * t23
      t25 = Sin(t5)
      t26 = t25 ** 2
      t27 = t3 * t26
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t37 = log(0.4D1 * t27 * t32 * t7 * t9)
      t39 = -x2 + t20 - 0.1D1 + x3
      t43 = t23 * wd
      t49 = -0.180D3 * t19 * t43 * lh - 0.90D2 * t19 * t43
      t52 = 0.1D1 / x3
      t55 = t19 * wd
      t57 = 0.1D1 / x1
      t58 = t52 * t57
      t62 = -(-0.90D2 * t24 * wd * t37 * t39 + t49 * t39) * t52 / 0.810D
     #3 - 0.2D1 / 0.9D1 * t55 * t39 * t23 * t58
      t63 = FJET(XB1, XB2, s, 0.0D0, t2 * (0.1D1 - x2 - x3 + t4 + t14), 
     #0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, t62)
      t67 = t32 * t7
      t70 = log(-0.4D1 * t27 * t67)
      t72 = -x2 + t20 - 0.1D1
      t80 = t19 * t72
      t84 = x1 ** 2
      t85 = x2 * t84
      t89 = log(-0.4D1 * t85 * t26 * t67)
      t98 = -0.180D3 * t19 * wd * lh - 0.90D2 * t55
      t106 = t19 * t72 * t23 * wd
      t111 = log(-0.4D1 * x2 * t26 * t67)
      t113 = z * t72
      t114 = t111 * t1 * t113
      t117 = (-0.2D1 * t80 - t114) * t23 * wd
      t124 = t111 ** 2
      t132 = lh ** 2
      t134 = pi ** 2
      t140 = -(0.90D2 * t24 * wd * t70 * t72 - t49 * t72) * t52 / 0.810D
     #3 + 0.2D1 / 0.9D1 * t80 * t43 * t58 + (-0.90D2 * t55 * t89 * t72 *
     # t23 + t98 * t72 * t23) * t57 / 0.405D3 - 0.2D1 / 0.9D1 * (t106 + 
     #t117) * lh + t106 / 0.9D1 + t117 / 0.9D1 + (t80 + 0.2D1 * t114 + t
     #124 * t1 * t113 / 0.2D1) * t23 * wd / 0.9D1 + t80 * t43 * (0.180D3
     # * t132 - 0.30D2 * t134) / 0.810D3
      t141 = FJET(XB1, XB2, s, 0.0D0, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, t
     #140)
      t145 = -0.1D1 + x1
      t149 = x1 * z
      t150 = 0.1D1 - x1 + t149
      t151 = 0.1D1 / t150
      t158 = s * t30 * x2 * t145 * x1 * t151
      t159 = x2 * x1
      t160 = t159 * z
      t162 = (-t20 + t160 - t149 + x2 - t159 - 0.1D1 + x1) ** 2
      t163 = 0.1D1 / t162
      t165 = t19 * wd * t163
      t167 = t145 * (x2 - t159 - t20 + t160 + 0.1D1 - x1 + t149)
      t174 = t145 ** 2
      t179 = log(-0.4D1 * t85 * t26 * t29 * t31 * t7 * t174 * t151)
      t189 = -0.2D1 / 0.9D1 * t165 * t167 * t58 + (0.90D2 * t55 * t179 *
     # t163 * t167 - t98 * t163 * t167) * t57 / 0.405D3
      t190 = FJET(XB1, XB2, s, t2 * x1, t7 * s * t1 * t145, 0.0D0, -t2 *
     # t145 * x2 * t151, -t158, t189)
      t195 = x3 * x1
      t196 = t195 * z
      t197 = t3 * x1
      t198 = t3 * t149
      t203 = Sqrt(x3 * t7 * t150 * x2 * t9)
      t205 = 0.2D1 * t6 * t203
      t206 = 0.1D1 - x1 + t149 - x2 + t159 - t160 - x3 + t195 - t196 + t
     #4 - t197 + t198 + t205
      t215 = x2 - t159 - t20 + t160 + 0.1D1 - x1 + t149 - x3 + t195 - t1
     #96
      t220 = FJET(XB1, XB2, s, -t9 * s * t1 * x1, -t2 * t145 * t206 * t1
     #51, t2 * t195, t2 * t145 * (-x3 + t195 - t196 + t4 - t197 + t198 -
     # x2 + t205) * t151, -t158, 0.2D1 / 0.9D1 * t165 * t215 * t145 * t5
     #8)
      rrqg2qght12s1e0 = t63 * t62 + t141 * t140 + t190 * t189 + 0.2D1 / 
     #0.9D1 * t220 * t1 * z * wd * t163 * t215 * t145 * t52 * t57

      end function



      doubleprecision function rrqg2qght12s1em1
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
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * pi
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t12 = Sqrt(x2 * t7 * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t19 = t1 * z
      t20 = t19 * wd
      t21 = x2 * z
      t22 = -x2 + t21 - 0.1D1 + x3
      t24 = (t21 - x2 + 0.1D1) ** 2
      t25 = 0.1D1 / t24
      t27 = 0.1D1 / x3
      t31 = FJET(XB1, XB2, s, 0.0D0, t2 * (0.1D1 - x2 - x3 + t4 + t14), 
     #0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0, -t20 * t22 * t25 * t27 
     #/ 0.9D1)
      t41 = -x2 + t21 - 0.1D1
      t42 = t19 * t41
      t43 = t25 * wd
      t44 = 0.1D1 / x1
      t56 = Sin(t5)
      t57 = t56 ** 2
      t59 = z ** 2
      t61 = t1 ** 2
      t62 = t61 ** 2
      t67 = log(-0.4D1 * x2 * t57 / t59 * t62 * t7)
      t78 = 0.2D1 / 0.9D1 * t42 * t43 * t44 - 0.2D1 / 0.9D1 * t42 * t43 
     #* lh + t19 * t41 * t25 * wd / 0.9D1 + (-0.2D1 * t42 - t67 * t1 * z
     # * t41) * t25 * wd / 0.9D1 + t42 * t43 * t27 / 0.9D1
      t79 = FJET(XB1, XB2, s, 0.0D0, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, t7
     #8)
      t83 = -0.1D1 + x1
      t87 = x1 * z
      t89 = 0.1D1 / (0.1D1 - x1 + t87)
      t97 = x2 * x1
      t98 = t97 * z
      t100 = (-t21 + t98 - t87 + x2 - t97 - 0.1D1 + x1) ** 2
      t105 = 0.1D1 / t100 * t83 * (x2 - t97 - t21 + t98 + 0.1D1 - x1 + t
     #87) * t44
      t108 = FJET(XB1, XB2, s, t2 * x1, t7 * s * t1 * t83, 0.0D0, -t2 * 
     #t83 * x2 * t89, -s * t61 * x2 * t83 * x1 * t89, -0.2D1 / 0.9D1 * t
     #20 * t105)
      rrqg2qght12s1em1 = -t31 * t1 * z * wd * t22 * t25 * t27 / 0.9D1 + 
     #t79 * t78 - 0.2D1 / 0.9D1 * t108 * t1 * z * wd * t105

      end function



      doubleprecision function rrqg2qght12s1em2
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
      t7 = x2 * z
      t10 = (t7 - x2 + 0.1D1) ** 2
      t13 = (-x2 + t7 - 0.1D1) / t10 * wd
      t16 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-0.1D1 + x2), 0.0D0, t2 * x2
     #, 0.0D0, t1 * z * t13 / 0.9D1)
      rrqg2qght12s1em2 = t16 * t1 * z * t13 / 0.9D1

      end function



      doubleprecision function rrqg2qght12s1em3
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
      rrqg2qght12s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght12s1em4
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
      rrqg2qght12s1em4 = 0.0D0

      end function
