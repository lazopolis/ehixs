  
      subroutine rrgq2qght11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qght11s1e1  
      doubleprecision rrgq2qght11s1e0  
      doubleprecision rrgq2qght11s1em1  
      doubleprecision rrgq2qght11s1em2  
      doubleprecision rrgq2qght11s1em3  
      doubleprecision rrgq2qght11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght11s1e1
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
      t7 = 0.2D1 * x3 - 0.1D1
      t8 = t7 * wd
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x3
      t22 = log(-0.4D1 * x2 * t11 * t14 * t18 * t4)
      t23 = t22 ** 2
      t28 = 0.180D3 * t8 * lh
      t31 = (-t28 - 0.90D2 * t8) * z
      t33 = lh ** 2
      t35 = pi ** 2
      t37 = 0.180D3 * t33 - 0.30D2 * t35
      t40 = (t28 + t8 * t37) * z
      t42 = 0.1D1 / x2
      t45 = t11 * t14
      t46 = t45 * t4
      t49 = log(-0.4D1 * t18 * t46)
      t51 = t49 * t7 * wd
      t52 = t49 ** 2
      t54 = t52 * t7 * wd
      t76 = 0.180D3 * lh
      t77 = x2 * x3
      t78 = x1 ** 2
      t85 = log(-0.4D1 * t77 * t78 * t17 * t11 * t14 * t4)
      t90 = t8 * z
      t91 = 0.90D2 * t90
      t94 = 0.1D1 / x1
      t101 = log(-0.4D1 * x3 * t78 * t17 * t46)
      t102 = t101 ** 2
      t110 = (-0.45D2 * t8 * z * t23 + t31 * t22 - t40) * t42 / 0.810D3 
     #- (-0.180D3 * (t51 + t54 / 0.2D1) * lh + t8 * (0.60D2 * lh * t35 -
     # 0.240D3 * zeta3 - 0.120D3 * t33 * lh) - 0.45D2 * t54 - 0.15D2 * t
     #52 * t49 * t7 * wd + (-t8 - t51) * t37) * z / 0.810D3 - (t8 * z * 
     #(-t76 - 0.90D2 * t85) - t91) * t42 * t94 / 0.405D3 + (-0.45D2 * t8
     # * z * t102 + t31 * t101 - t40) * t94 / 0.405D3
      t111 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #110)
      t113 = -0.1D1 + x1
      t114 = x3 * x1
      t115 = t114 * z
      t116 = 0.2D1 * t77
      t117 = t77 * x1
      t118 = x1 * z
      t119 = t77 * t118
      t120 = cos(t9)
      t121 = -0.1D1 + x2
      t123 = 0.1D1 - x1 + t118
      t127 = Sqrt(x3 * t121 * t123 * x2 * t4)
      t129 = 0.2D1 * t120 * t127
      t132 = 0.1D1 / t123
      t135 = t2 * t114
      t136 = x2 * x1
      t137 = t136 * z
      t138 = 0.1D1 - x1 + t118 - x2 + t136 - t137 - x3 + t114 - t115 + t
     #116 - t117 + t119 + t129
      t142 = t4 * s
      t144 = t142 * t1 * x1
      t150 = t78 * t11
      t154 = t113 ** 2
      t160 = log(0.4D1 * t77 * t150 * t14 * t17 * t132 * t154 * t121 * t
     #4)
      t163 = t123 ** 2
      t165 = x2 * z
      t167 = (-0.1D1 + x1 - t118 + x2 - t136 - t165 + t137) ** 2
      t168 = 0.1D1 / t167
      t175 = t90 * (-t76 - 0.90D2 * t160) * t163 * t168 - 0.90D2 * t8 * 
     #z * t163 * t168
      t179 = FJET(XB1, XB2, s, t2 * t113 * (-x3 + t114 - t115 + t116 - t
     #117 + t119 - x2 + t129) * t132, t135, -t2 * t113 * t138 * t132, -t
     #144, -s * t16 * x2 * x1 * t113 * t132, -t175 * t42 * t94 / 0.405D3
     #)
      t185 = x3 * t4
      t187 = Sqrt(x2 * t121 * t185)
      t189 = 0.2D1 * t120 * t187
      t195 = t14 * t17
      t197 = t195 * t121 * t4
      t200 = log(0.4D1 * t77 * t11 * t197)
      t201 = t200 ** 2
      t204 = (0.1D1 - x2 + t165) ** 2
      t205 = 0.1D1 / t204
      t220 = log(0.4D1 * t77 * t150 * t197)
      t233 = (0.45D2 * t8 * z * t201 * t205 - t31 * t200 * t205 + t40 * 
     #t205) * t42 / 0.810D3 - (t8 * z * (0.180D3 * t205 * lh + 0.90D2 * 
     #t220 * t205) + 0.90D2 * t8 * z * t205) * t42 * t94 / 0.405D3
      t234 = FJET(XB1, XB2, s, -t2 * (-x3 + t116 - x2 + t189), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t116 + t189), 0.0D0, 0.0D0, t233)
      t237 = t1 * t113
      t242 = t154 * t132
      t247 = log(-0.4D1 * t77 * t78 * t17 * t45 * t242 * t4)
      t259 = log(-0.4D1 * t150 * t195 * t185 * t242)
      t260 = t259 ** 2
      t268 = -(-t8 * z * (-t76 - 0.90D2 * t247) + t91) * t42 * t94 / 0.4
     #05D3 + (0.45D2 * t8 * z * t260 - t31 * t259 + t40) * t94 / 0.405D3
      t269 = FJET(XB1, XB2, s, -x3 * s * t237, t135, t142 * t237, -t144,
     # 0.0D0, t268)
      rrgq2qght11s1e1 = t111 * t110 - t179 * t175 * t42 * t94 / 0.405D3 
     #+ t234 * t233 + t269 * t268

      end function



      doubleprecision function rrgq2qght11s1e0
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
      t7 = 0.2D1 * x3 - 0.1D1
      t8 = t7 * wd
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x3
      t22 = log(-0.4D1 * x2 * t11 * t14 * t18 * t4)
      t30 = (-0.180D3 * t8 * lh - 0.90D2 * t8) * z
      t32 = 0.1D1 / x2
      t36 = 0.1D1 / x1
      t39 = 0.2D1 / 0.9D1 * t8 * z * t32 * t36
      t40 = x1 ** 2
      t44 = t11 * t14 * t4
      t47 = log(-0.4D1 * x3 * t40 * t17 * t44)
      t56 = log(-0.4D1 * t18 * t44)
      t58 = t56 * t7 * wd
      t63 = t56 ** 2
      t67 = lh ** 2
      t69 = pi ** 2
      t76 = (0.90D2 * t8 * z * t22 - t30) * t32 / 0.810D3 - t39 + (0.90D
     #2 * t8 * z * t47 - t30) * t36 / 0.405D3 - (-0.180D3 * (-t8 - t58) 
     #* lh + 0.90D2 * t58 + 0.45D2 * t63 * t7 * wd + t8 * (0.180D3 * t67
     # - 0.30D2 * t69)) * z / 0.810D3
      t77 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t7
     #6)
      t79 = -0.1D1 + x1
      t80 = x3 * x1
      t81 = t80 * z
      t82 = x2 * x3
      t83 = 0.2D1 * t82
      t84 = t82 * x1
      t85 = x1 * z
      t86 = t82 * t85
      t87 = cos(t9)
      t88 = -0.1D1 + x2
      t90 = 0.1D1 - x1 + t85
      t94 = Sqrt(x3 * t88 * t90 * x2 * t4)
      t96 = 0.2D1 * t87 * t94
      t99 = 0.1D1 / t90
      t102 = t2 * t80
      t103 = x2 * x1
      t104 = t103 * z
      t105 = 0.1D1 - x1 + t85 - x2 + t103 - t104 - x3 + t80 - t81 + t83 
     #- t84 + t86 + t96
      t109 = t4 * s
      t111 = t109 * t1 * x1
      t117 = t8 * z
      t118 = t90 ** 2
      t119 = x2 * z
      t121 = (-0.1D1 + x1 - t85 + x2 - t103 - t119 + t104) ** 2
      t125 = t118 / t121 * t32 * t36
      t128 = FJET(XB1, XB2, s, t2 * t79 * (-x3 + t80 - t81 + t83 - t84 +
     # t86 - x2 + t96) * t99, t102, -t2 * t79 * t105 * t99, -t111, -s * 
     #t16 * x2 * x1 * t79 * t99, -0.2D1 / 0.9D1 * t117 * t125)
      t135 = x3 * t4
      t137 = Sqrt(x2 * t88 * t135)
      t139 = 0.2D1 * t87 * t137
      t145 = t14 * t17
      t150 = log(0.4D1 * t82 * t11 * t145 * t88 * t4)
      t153 = (0.1D1 - x2 + t119) ** 2
      t154 = 0.1D1 / t153
      t166 = (-0.90D2 * t8 * z * t150 * t154 + t30 * t154) * t32 / 0.810
     #D3 + 0.2D1 / 0.9D1 * t117 * t154 * t32 * t36
      t167 = FJET(XB1, XB2, s, -t2 * (-x3 + t83 - x2 + t139), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t83 + t139), 0.0D0, 0.0D0, t166)
      t170 = t1 * t79
      t175 = t79 ** 2
      t180 = log(-0.4D1 * t40 * t11 * t145 * t135 * t99 * t175)
      t187 = t39 + (-0.90D2 * t8 * z * t180 + t30) * t36 / 0.405D3
      t188 = FJET(XB1, XB2, s, -x3 * s * t170, t102, t109 * t170, -t111,
     # 0.0D0, t187)
      rrgq2qght11s1e0 = t77 * t76 - 0.2D1 / 0.9D1 * t128 * t7 * wd * z *
     # t125 + t167 * t166 + t188 * t187

      end function



      doubleprecision function rrgq2qght11s1em1
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
      t7 = 0.2D1 * x3 - 0.1D1
      t8 = t7 * wd
      t9 = 0.1D1 / x1
      t12 = 0.2D1 / 0.9D1 * t8 * z * t9
      t16 = t1 ** 2
      t17 = t16 ** 2
      t19 = x4 * pi
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t28 = log(-0.4D1 * x3 * t17 * t21 / t22 * t4)
      t35 = 0.1D1 / x2
      t39 = -t12 - (-0.180D3 * t8 * lh - 0.90D2 * t8 - 0.90D2 * t28 * t7
     # * wd) * z / 0.810D3 - t8 * z * t35 / 0.9D1
      t40 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #9)
      t44 = t1 * (-0.1D1 + x1)
      t48 = t4 * s
      t52 = FJET(XB1, XB2, s, -x3 * s * t44, t2 * x1 * x3, t48 * t44, -t
     #48 * t1 * x1, 0.0D0, t12)
      t59 = 0.2D1 * x2 * x3
      t60 = cos(t19)
      t65 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t67 = 0.2D1 * t60 * t65
      t74 = (0.1D1 - x2 + x2 * z) ** 2
      t77 = z / t74 * t35
      t80 = FJET(XB1, XB2, s, -t2 * (-x3 + t59 - x2 + t67), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t59 + t67), 0.0D0, 0.0D0, t8 * t77 / 0.9D1)
      rrgq2qght11s1em1 = t40 * t39 + 0.2D1 / 0.9D1 * t52 * t7 * wd * z *
     # t9 + t80 * t7 * wd * t77 / 0.9D1

      end function



      doubleprecision function rrgq2qght11s1em2
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
      t7 = 0.2D1 * x3 - 0.1D1
      t11 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, -t7 * wd * z / 0.9D1)
      rrgq2qght11s1em2 = -t11 * t7 * wd * z / 0.9D1

      end function



      doubleprecision function rrgq2qght11s1em3
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
      rrgq2qght11s1em3 = 0.0D0

      end function



      doubleprecision function rrgq2qght11s1em4
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
      rrgq2qght11s1em4 = 0.0D0

      end function
