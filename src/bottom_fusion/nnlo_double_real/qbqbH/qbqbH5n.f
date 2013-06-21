  
      subroutine qbqbH5n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision qbqbH5n1e1  
      doubleprecision qbqbH5n1e0  
      doubleprecision qbqbH5n1em1  
      doubleprecision qbqbH5n1em2  
      doubleprecision qbqbH5n1em3  
      doubleprecision qbqbH5n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=qbqbH5n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=qbqbH5n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=qbqbH5n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=qbqbH5n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=qbqbH5n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=qbqbH5n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function qbqbH5n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
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
      t17 = lh ** 2
      t19 = 0.3141592653589793D1 ** 2
      t21 = 0.180D3 * t17 - 0.30D2 * t19
      t23 = t15 ** 2
      t37 = t1 ** 2
      t38 = t37 * t1
      t40 = x2 * wd
      t41 = x2 * z
      t43 = (0.1D1 - x2 + t41) ** 2
      t44 = 0.1D1 / t43
      t45 = t40 * t44
      t48 = z * lh
      t49 = t48 * t38
      t50 = x3 * t8
      t51 = t10 * x2
      t52 = t51 * t4
      t55 = log(-0.4D1 * t50 * t52)
      t61 = z * t38 * x2
      t62 = wd * t44
      t63 = t55 ** 2
      t68 = z * t21 * t38
      t69 = t68 * t45
      t71 = 0.1D1 / x3
      t74 = x1 ** 2
      t75 = x3 * t74
      t76 = t75 * t8
      t79 = log(-0.4D1 * t76 * t52)
      t85 = 0.180D3 * t49 * t45
      t88 = 0.1D1 / x1
      t91 = t74 * t8
      t94 = log(-0.4D1 * t91 * t52)
      t99 = t94 ** 2
      t107 = -(-t15 * z * t21 - 0.90D2 * t23 * z * lh + z * (0.60D2 * lh
     # * t19 - 0.2884936567583026D3 - 0.120D3 * t17 * lh) - 0.15D2 * t23
     # * t15 * z) * t38 * t45 / 0.180D3 + (-0.180D3 * t49 * t40 * t44 * 
     #t55 - 0.45D2 * t61 * t62 * t63 - t69) * t71 / 0.180D3 + (0.90D2 * 
     #t61 * wd * t79 * t44 + t85) * t71 * t88 / 0.90D2 - (0.180D3 * t49 
     #* t40 * t94 * t44 + 0.45D2 * t61 * wd * t99 * t44 + t69) * t88 / 0
     #.90D2
      t108 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t
     #107)
      t111 = 0.2D1 * x2 * x3
      t112 = cos(t6)
      t113 = -0.1D1 + x3
      t116 = Sqrt(t12 * x3 * t113)
      t118 = 0.2D1 * t112 * t116
      t127 = log(0.4D1 * t50 * t10 * t12 * t113)
      t132 = t127 ** 2
      t143 = log(0.4D1 * t76 * t51 * t4 * t113)
      t152 = (0.180D3 * t49 * t40 * t44 * t127 + 0.45D2 * t61 * t62 * t1
     #32 + t69) * t71 / 0.180D3 + (-0.90D2 * t61 * wd * t143 * t44 - t85
     #) * t71 * t88 / 0.90D2
      t153 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t111 - x2 + t118), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t111 + t118), 0.0D0, t152)
      t155 = -0.1D1 + x1
      t157 = x1 * z
      t158 = 0.1D1 - x1 + t157
      t159 = 0.1D1 / t158
      t170 = s * t37 * x2 * x1 * t155 * t159
      t171 = t75 * t11
      t172 = t155 ** 2
      t173 = t159 * t172
      t174 = t12 * t173
      t177 = log(-0.4D1 * t171 * t174)
      t179 = x1 * x2
      t180 = t179 * z
      t182 = (-0.1D1 + x1 - t157 + x2 - t179 - t41 + t180) ** 2
      t184 = t172 / t182
      t188 = t40 * t184
      t190 = 0.180D3 * t49 * t188
      t199 = log(-0.4D1 * t91 * t10 * t174)
      t204 = t199 ** 2
      t213 = (-0.90D2 * t61 * wd * t177 * t184 - t190) * t71 * t88 / 0.9
     #0D2 - (-0.180D3 * t48 * t38 * x2 * wd * t199 * t184 - 0.45D2 * t61
     # * wd * t204 * t184 - t68 * t188) * t88 / 0.90D2
      t214 = FJET(XB1, XB2, s, 0.0D0, -t2 * t155 * x2 * t159, x1 * t1 * 
     #s, t4 * t155 * t2, -t170, t213)
      t216 = x3 * x1
      t218 = t216 * z
      t219 = t216 * x2
      t220 = t216 * t41
      t225 = Sqrt(x3 * t4 * t158 * x2 * t113)
      t227 = 0.2D1 * t112 * t225
      t234 = 0.1D1 - x1 + t157 - x2 + t179 - t180 - x3 + t216 - t218 + t
     #111 - t219 + t220 + t227
      t242 = log(0.4D1 * t171 * t12 * t173 * t113)
      t247 = 0.90D2 * t61 * wd * t242 * t184 + t190
      t251 = FJET(XB1, XB2, s, t2 * t216, t2 * t155 * (-x3 + t216 - t218
     # + t111 - t219 + t220 - x2 + t227) * t159, -t2 * x1 * t113, -t2 * 
     #t155 * t234 * t159, -t170, t247 * t71 * t88 / 0.90D2)
      qbqbH5n1e1 = t108 * t107 + t153 * t152 + t214 * t213 + t251 * t247
     # * t71 * t88 / 0.90D2

      end function



      doubleprecision function qbqbH5n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t6 = t1 ** 2
      t7 = t6 * t1
      t8 = z * t7
      t9 = t8 * x2
      t10 = x2 * z
      t12 = (0.1D1 - x2 + t10) ** 2
      t13 = 0.1D1 / t12
      t14 = wd * t13
      t15 = x4 * 0.3141592653589793D1
      t16 = Sin(t15)
      t17 = t16 ** 2
      t18 = x3 * t17
      t19 = z ** 2
      t20 = 0.1D1 / t19
      t22 = t20 * x2 * t4
      t25 = log(-0.4D1 * t18 * t22)
      t30 = z * lh * t7
      t31 = x2 * wd
      t32 = t31 * t13
      t34 = 0.180D3 * t30 * t32
      t36 = 0.1D1 / x3
      t39 = 0.1D1 / x1
      t40 = t36 * t39
      t42 = t9 * t14 * t40
      t43 = x1 ** 2
      t44 = t43 * t17
      t47 = log(-0.4D1 * t44 * t22)
      t56 = x2 * t4
      t59 = log(-0.4D1 * t17 * t20 * t56)
      t63 = lh ** 2
      t65 = 0.3141592653589793D1 ** 2
      t69 = t59 ** 2
      t76 = (0.90D2 * t9 * t14 * t25 + t34) * t36 / 0.180D3 - t42 - (-0.
     #90D2 * t9 * wd * t47 * t13 - t34) * t39 / 0.90D2 - (0.180D3 * t59 
     #* z * lh + z * (0.180D3 * t63 - 0.30D2 * t65) + 0.45D2 * t69 * z) 
     #* t7 * t32 / 0.180D3
      t77 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t7
     #6)
      t80 = 0.2D1 * x2 * x3
      t81 = cos(t15)
      t82 = -0.1D1 + x3
      t85 = Sqrt(t56 * x3 * t82)
      t87 = 0.2D1 * t81 * t85
      t96 = log(0.4D1 * t18 * t20 * t56 * t82)
      t103 = (-0.90D2 * t9 * t14 * t96 - t34) * t36 / 0.180D3 + t42
      t104 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t80 - x2 + t87), 0.0D
     #0, t2 * (0.1D1 - x2 - x3 + t80 + t87), 0.0D0, t103)
      t106 = -0.1D1 + x1
      t108 = x1 * z
      t109 = 0.1D1 - x1 + t108
      t110 = 0.1D1 / t109
      t121 = s * t6 * x2 * x1 * t106 * t110
      t123 = t106 ** 2
      t124 = x1 * x2
      t125 = t124 * z
      t127 = (-0.1D1 + x1 - t108 + x2 - t124 - t10 + t125) ** 2
      t128 = 0.1D1 / t127
      t129 = t123 * t128
      t131 = t8 * t31 * t129 * t40
      t137 = log(-0.4D1 * t44 * t20 * t56 * t110 * t123)
      t148 = t131 - (0.90D2 * t9 * wd * t137 * t129 + 0.180D3 * t30 * t3
     #1 * t129) * t39 / 0.90D2
      t149 = FJET(XB1, XB2, s, 0.0D0, -t2 * t106 * x2 * t110, x1 * t1 * 
     #s, t4 * t106 * t2, -t121, t148)
      t151 = x3 * x1
      t153 = t151 * z
      t154 = t151 * x2
      t155 = t151 * t10
      t160 = Sqrt(x3 * t4 * t109 * x2 * t82)
      t162 = 0.2D1 * t81 * t160
      t169 = 0.1D1 - x1 + t108 - x2 + t124 - t125 - x3 + t151 - t153 + t
     #80 - t154 + t155 + t162
      t173 = FJET(XB1, XB2, s, t2 * t151, t2 * t106 * (-x3 + t151 - t153
     # + t80 - t154 + t155 - x2 + t162) * t110, -t2 * x1 * t82, -t2 * t1
     #06 * t169 * t110, -t121, -t131)
      qbqbH5n1e0 = t77 * t76 + t104 * t103 + t149 * t148 - t173 * z * t7
     # * x2 * wd * t123 * t128 * t36 * t39

      end function



      doubleprecision function qbqbH5n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x2
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t14 = x2 * t4
      t17 = log(-0.4D1 * t10 / t11 * t14)
      t21 = t1 ** 2
      t22 = t21 * t1
      t24 = x2 * wd
      t25 = x2 * z
      t27 = (0.1D1 - x2 + t25) ** 2
      t28 = 0.1D1 / t27
      t33 = z * t22 * x2
      t34 = wd * t28
      t35 = 0.1D1 / x3
      t38 = t33 * t34 * t35 / 0.2D1
      t39 = 0.1D1 / x1
      t42 = -(-0.180D3 * z * lh - 0.90D2 * t17 * z) * t22 * t24 * t28 / 
     #0.180D3 - t38 - t33 * t34 * t39
      t43 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * t4, 0.0D0, t4
     #2)
      t46 = 0.2D1 * x2 * x3
      t47 = cos(t8)
      t51 = Sqrt(t14 * x3 * (-0.1D1 + x3))
      t53 = 0.2D1 * t47 * t51
      t58 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t46 - x2 + t53), 0.0D0
     #, t2 * (0.1D1 - x2 - x3 + t46 + t53), 0.0D0, t38)
      t65 = -0.1D1 + x1
      t67 = x1 * z
      t69 = 0.1D1 / (0.1D1 - x1 + t67)
      t81 = t65 ** 2
      t83 = x1 * x2
      t86 = (-0.1D1 + x1 - t67 + x2 - t83 - t25 + t83 * z) ** 2
      t89 = wd * t81 / t86 * t39
      t91 = FJET(XB1, XB2, s, 0.0D0, -t2 * t65 * x2 * t69, x1 * t1 * s, 
     #t4 * t65 * t2, -s * t21 * x2 * x1 * t65 * t69, t33 * t89)
      qbqbH5n1em1 = t43 * t42 + t58 * z * t22 * t24 * t28 * t35 / 0.2D1 
     #+ t91 * z * t22 * x2 * t89

      end function



      doubleprecision function qbqbH5n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t6 = t1 ** 2
      t7 = t6 * t1
      t12 = (0.1D1 - x2 + x2 * z) ** 2
      t14 = x2 * wd / t12
      t17 = FJET(XB1, XB2, s, 0.0D0, t2 * x2, 0.0D0, -t2 * (-0.1D1 + x2)
     #, 0.0D0, -z * t7 * t14 / 0.2D1)
      qbqbH5n1em2 = -t17 * z * t7 * t14 / 0.2D1

      end function



      doubleprecision function qbqbH5n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      qbqbH5n1em3 = 0.0D0

      end function



      doubleprecision function qbqbH5n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      qbqbH5n1em4 = 0.0D0

      end function
