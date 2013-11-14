  
      subroutine rrgq2qght12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght12s1e1  
      doubleprecision rrgq2qght12s1e0  
      doubleprecision rrgq2qght12s1em1  
      doubleprecision rrgq2qght12s1em2  
      doubleprecision rrgq2qght12s1em3  
      doubleprecision rrgq2qght12s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght12s1e1
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
      t19 = x2 * z
      t21 = (0.1D1 - x2 + t19) ** 2
      t22 = 0.1D1 / t21
      t23 = wd * t22
      t24 = t23 * z
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
      t40 = -x2 + t19 - 0.1D1 + x3
      t45 = 0.180D3 * t23 * lh
      t48 = (-t45 - 0.90D2 * t23) * z
      t52 = lh ** 2
      t54 = pi ** 2
      t56 = 0.180D3 * t52 - 0.30D2 * t54
      t59 = (t45 + t23 * t56) * z
      t63 = 0.1D1 / x3
      t67 = wd * z * t1
      t68 = x1 ** 2
      t69 = t68 * t26
      t70 = t3 * t69
      t73 = log(0.4D1 * t70 * t34)
      t79 = 0.180D3 * wd * lh
      t82 = (-t79 - 0.90D2 * wd) * z
      t83 = t1 * t22
      t88 = 0.1D1 / x1
      t91 = -(0.45D2 * t24 * t1 * t38 * t40 - t48 * t1 * t37 * t40 + t59
     # * t1 * t40) * t63 / 0.810D3 - (-0.90D2 * t67 * t73 * t22 * t40 + 
     #t82 * t83 * t40) * t63 * t88 / 0.405D3
      t92 = FJET(XB1, XB2, s, t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 
     #-t2 * (-x3 + t4 - x2 + t14), 0.0D0, 0.0D0, t91)
      t95 = -0.1D1 + x1
      t100 = x1 * z
      t101 = 0.1D1 - x1 + t100
      t102 = 0.1D1 / t101
      t109 = s * t30 * x2 * x1 * t95 * t102
      t110 = t95 ** 2
      t116 = log(-0.4D1 * t70 * t32 * t102 * t110 * t7)
      t118 = x2 * x1
      t119 = t118 * z
      t120 = x2 - t118 - t19 + t119 + 0.1D1 - x1 + t100
      t122 = (-0.1D1 + x1 - t100 + x2 - t118 - t19 + t119) ** 2
      t123 = 0.1D1 / t122
      t124 = t120 * t123
      t128 = t82 * t1
      t130 = t95 * t120 * t123
      t135 = x2 * t68
      t136 = t26 * t29
      t138 = t31 * t102
      t139 = t110 * t7
      t143 = log(-0.4D1 * t135 * t136 * t138 * t139)
      t144 = t143 ** 2
      t154 = (t79 + wd * t56) * z
      t160 = -(-0.90D2 * t67 * t116 * t95 * t124 + t128 * t130) * t63 * 
     #t88 / 0.405D3 + (-0.45D2 * t67 * t144 * t95 * t124 + t128 * t143 *
     # t95 * t124 - t154 * t1 * t130) * t88 / 0.405D3
      t161 = FJET(XB1, XB2, s, t7 * s * t1 * t95, t2 * x1, -t2 * t95 * x
     #2 * t102, 0.0D0, -t109, t160)
      t165 = t32 * t7
      t168 = log(-0.4D1 * t27 * t165)
      t169 = t168 ** 2
      t171 = -x2 + t19 - 0.1D1
      t178 = t1 * t171
      t186 = log(-0.4D1 * x2 * t26 * t165)
      t187 = t186 * wd
      t188 = t186 ** 2
      t190 = t188 * wd / 0.2D1
      t220 = log(-0.4D1 * t3 * t68 * t136 * t31 * t7)
      t225 = t83 * t171
      t234 = log(-0.4D1 * t135 * t26 * t165)
      t235 = t234 ** 2
      t247 = -(-0.45D2 * t24 * t1 * t169 * t171 + t48 * t1 * t168 * t171
     # - t59 * t178) * t63 / 0.810D3 + (-0.180D3 * (t187 + t190) * t22 *
     # lh + t23 * (0.60D2 * lh * t54 - 0.240D3 * zeta3 - 0.120D3 * t52 *
     # lh) + 0.90D2 * (-t190 - t188 * t186 * wd / 0.6D1) * t22 + (-wd - 
     #t187) * t22 * t56) * z * t178 / 0.810D3 - (0.90D2 * t67 * t220 * t
     #22 * t171 - t82 * t225) * t63 * t88 / 0.405D3 + (0.45D2 * t67 * t2
     #35 * t22 * t171 - t128 * t234 * t22 * t171 + t154 * t225) * t88 / 
     #0.405D3
      t248 = FJET(XB1, XB2, s, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, 0.0D0, t
     #247)
      t250 = x3 * x1
      t251 = t250 * z
      t252 = t3 * x1
      t253 = t3 * t100
      t258 = Sqrt(x3 * t7 * t101 * x2 * t9)
      t260 = 0.2D1 * t6 * t258
      t261 = 0.1D1 - x1 + t100 - x2 + t118 - t119 - x3 + t250 - t251 + t
     #4 - t252 + t253 + t260
      t279 = log(0.4D1 * t3 * t69 * t29 * t138 * t139 * t9)
      t281 = x2 - t118 - t19 + t119 + 0.1D1 - x1 + t100 - x3 + t250 - t2
     #51
      t289 = 0.90D2 * t67 * t279 * t95 * t281 * t123 - t128 * t95 * t281
     # * t123
      t293 = FJET(XB1, XB2, s, -t2 * t95 * t261 * t102, -t9 * s * t1 * x
     #1, t2 * t95 * (-x3 + t250 - t251 + t4 - t252 + t253 - x2 + t260) *
     # t102, t2 * t250, -t109, -t289 * t63 * t88 / 0.405D3)
      rrgq2qght12s1e1 = t92 * t91 + t161 * t160 + t248 * t247 - t293 * t
     #289 * t63 * t88 / 0.405D3

      end function



      doubleprecision function rrgq2qght12s1e0
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
      t19 = x2 * z
      t21 = (0.1D1 - x2 + t19) ** 2
      t22 = 0.1D1 / t21
      t23 = wd * t22
      t24 = t23 * z
      t25 = Sin(t5)
      t26 = t25 ** 2
      t27 = t3 * t26
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t1 ** 2
      t31 = t30 ** 2
      t32 = t29 * t31
      t37 = log(0.4D1 * t27 * t32 * t7 * t9)
      t39 = -x2 + t19 - 0.1D1 + x3
      t47 = (-0.180D3 * t23 * lh - 0.90D2 * t23) * z
      t51 = 0.1D1 / x3
      t54 = wd * z
      t55 = t54 * t1
      t57 = 0.1D1 / x1
      t58 = t51 * t57
      t62 = -(-0.90D2 * t24 * t1 * t37 * t39 + t47 * t1 * t39) * t51 / 0
     #.810D3 - 0.2D1 / 0.9D1 * t55 * t22 * t39 * t58
      t63 = FJET(XB1, XB2, s, t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 
     #-t2 * (-x3 + t4 - x2 + t14), 0.0D0, 0.0D0, t62)
      t66 = -0.1D1 + x1
      t67 = t1 * t66
      t71 = x1 * z
      t72 = 0.1D1 - x1 + t71
      t73 = 0.1D1 / t72
      t80 = s * t30 * x2 * x1 * t66 * t73
      t81 = t54 * t67
      t82 = x2 * x1
      t83 = t82 * z
      t84 = x2 - t82 - t19 + t83 + 0.1D1 - x1 + t71
      t86 = (-0.1D1 + x1 - t71 + x2 - t82 - t19 + t83) ** 2
      t87 = 0.1D1 / t86
      t88 = t84 * t87
      t92 = x1 ** 2
      t93 = x2 * t92
      t97 = t66 ** 2
      t102 = log(-0.4D1 * t93 * t26 * t29 * t31 * t73 * t97 * t7)
      t111 = (-0.180D3 * wd * lh - 0.90D2 * wd) * z
      t119 = -0.2D1 / 0.9D1 * t81 * t88 * t58 + (0.90D2 * t55 * t102 * t
     #66 * t88 - t111 * t1 * t66 * t84 * t87) * t57 / 0.405D3
      t120 = FJET(XB1, XB2, s, t7 * s * t67, t2 * x1, -t2 * t66 * x2 * t
     #73, 0.0D0, -t80, t119)
      t124 = t32 * t7
      t127 = log(-0.4D1 * t27 * t124)
      t129 = -x2 + t19 - 0.1D1
      t133 = t1 * t129
      t145 = log(-0.4D1 * t93 * t26 * t124)
      t159 = log(-0.4D1 * x2 * t26 * t124)
      t160 = t159 * wd
      t165 = t159 ** 2
      t171 = lh ** 2
      t173 = pi ** 2
      t181 = -(0.90D2 * t24 * t1 * t127 * t129 - t47 * t133) * t51 / 0.8
     #10D3 + 0.2D1 / 0.9D1 * t55 * t22 * t129 * t58 + (-0.90D2 * t55 * t
     #145 * t22 * t129 + t111 * t1 * t22 * t129) * t57 / 0.405D3 + (-0.1
     #80D3 * (-wd - t160) * t22 * lh + 0.90D2 * (t160 + t165 * wd / 0.2D
     #1) * t22 + t23 * (0.180D3 * t171 - 0.30D2 * t173)) * z * t133 / 0.
     #810D3
      t182 = FJET(XB1, XB2, s, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, 0.0D0, t
     #181)
      t184 = x3 * x1
      t185 = t184 * z
      t186 = t3 * x1
      t187 = t3 * t71
      t192 = Sqrt(x3 * t7 * t72 * x2 * t9)
      t194 = 0.2D1 * t6 * t192
      t195 = 0.1D1 - x1 + t71 - x2 + t82 - t83 - x3 + t184 - t185 + t4 -
     # t186 + t187 + t194
      t207 = x2 - t82 - t19 + t83 + 0.1D1 - x1 + t71 - x3 + t184 - t185
      t212 = FJET(XB1, XB2, s, -t2 * t66 * t195 * t73, -t9 * s * t1 * x1
     #, t2 * t66 * (-x3 + t184 - t185 + t4 - t186 + t187 - x2 + t194) * 
     #t73, t2 * t184, -t80, 0.2D1 / 0.9D1 * t81 * t207 * t87 * t58)
      rrgq2qght12s1e0 = t63 * t62 + t120 * t119 + t182 * t181 + 0.2D1 / 
     #0.9D1 * t212 * wd * z * t1 * t66 * t207 * t87 * t51 * t57

      end function



      doubleprecision function rrgq2qght12s1em1
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
      t20 = wd * z * t1
      t21 = x2 * z
      t23 = (0.1D1 - x2 + t21) ** 2
      t24 = 0.1D1 / t23
      t25 = -x2 + t21 - 0.1D1 + x3
      t27 = 0.1D1 / x3
      t31 = FJET(XB1, XB2, s, t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, 
     #-t2 * (-x3 + t4 - x2 + t14), 0.0D0, 0.0D0, -t20 * t24 * t25 * t27 
     #/ 0.9D1)
      t40 = -0.1D1 + x1
      t45 = x1 * z
      t47 = 0.1D1 / (0.1D1 - x1 + t45)
      t50 = t1 ** 2
      t56 = x2 * x1
      t57 = t56 * z
      t61 = (-0.1D1 + x1 - t45 + x2 - t56 - t21 + t57) ** 2
      t63 = 0.1D1 / x1
      t65 = t40 * (x2 - t56 - t21 + t57 + 0.1D1 - x1 + t45) / t61 * t63
      t68 = FJET(XB1, XB2, s, t7 * s * t1 * t40, t2 * x1, -t2 * t40 * x2
     # * t47, 0.0D0, -s * t50 * x2 * x1 * t40 * t47, -0.2D1 / 0.9D1 * t2
     #0 * t65)
      t76 = wd * t24
      t78 = -x2 + t21 - 0.1D1
      t79 = t1 * t78
      t85 = Sin(t5)
      t86 = t85 ** 2
      t88 = z ** 2
      t90 = t50 ** 2
      t95 = log(-0.4D1 * x2 * t86 / t88 * t90 * t7)
      t108 = 0.2D1 / 0.9D1 * t76 * z * t79 * t63 + (-0.180D3 * t76 * lh 
     #+ 0.90D2 * (-wd - t95 * wd) * t24) * z * t79 / 0.810D3 + t20 * t24
     # * t78 * t27 / 0.9D1
      t109 = FJET(XB1, XB2, s, -t2 * t7, 0.0D0, t2 * x2, 0.0D0, 0.0D0, t
     #108)
      rrgq2qght12s1em1 = -t31 * wd * z * t1 * t24 * t25 * t27 / 0.9D1 - 
     #0.2D1 / 0.9D1 * t68 * wd * z * t1 * t65 + t109 * t108

      end function



      doubleprecision function rrgq2qght12s1em2
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
      t9 = (0.1D1 - x2 + t7) ** 2
      t13 = t1 / t9 * (-x2 + t7 - 0.1D1)
      t16 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x2), 0.0D0, t2 * x2, 0.0D0
     #, 0.0D0, wd * z * t13 / 0.9D1)
      rrgq2qght12s1em2 = t16 * wd * z * t13 / 0.9D1

      end function



      doubleprecision function rrgq2qght12s1em3
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
      rrgq2qght12s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght12s1em4
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
      rrgq2qght12s1em4 = 0.0D0

      end function
