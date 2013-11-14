  
      subroutine rrqg2qght11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght11s1e1  
      doubleprecision rrqg2qght11s1e0  
      doubleprecision rrqg2qght11s1em1  
      doubleprecision rrqg2qght11s1em2  
      doubleprecision rrqg2qght11s1em3  
      doubleprecision rrqg2qght11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght11s1e1
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
      t8 = z * t7
      t9 = x2 * x3
      t10 = t1 ** 2
      t11 = t10 ** 2
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t18 = t15 * t17
      t19 = t18 * t4
      t22 = log(-0.4D1 * t9 * t11 * t19)
      t23 = t22 ** 2
      t27 = t8 * wd
      t28 = 0.90D2 * t27
      t30 = t7 * wd
      t32 = 0.180D3 * lh * z * t30
      t33 = -t28 - t32
      t35 = lh ** 2
      t36 = 0.180D3 * t35
      t37 = pi ** 2
      t38 = 0.30D2 * t37
      t39 = t36 - t38
      t41 = t39 * z * t30
      t43 = 0.1D1 / x2
      t49 = log(-0.4D1 * x3 * t11 * t19)
      t52 = t49 ** 2
      t72 = x1 ** 2
      t74 = t11 * t15
      t79 = log(-0.4D1 * t9 * t72 * t74 * t17 * t4)
      t85 = 0.1D1 / x1
      t88 = x3 * t72
      t92 = log(-0.4D1 * t88 * t11 * t19)
      t93 = t92 ** 2
      t101 = (-0.45D2 * t8 * wd * t23 + t33 * t22 - t32 - t41) * t43 / 0
     #.810D3 + (0.180D3 * t49 * lh + 0.45D2 * t52 + t36 - t38) * z * t30
     # / 0.810D3 - (-0.90D2 * t52 * lh + 0.60D2 * lh * t37 - 0.240D3 * z
     #eta3 - 0.120D3 * t35 * lh - 0.15D2 * t52 * t49 - t49 * t39) * z * 
     #t30 / 0.810D3 + (0.90D2 * t8 * wd * t79 + t28 + t32) * t43 * t85 /
     # 0.405D3 - (0.45D2 * t8 * wd * t93 - t33 * t92 + t32 + t41) * t85 
     #/ 0.405D3
      t102 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #101)
      t104 = 0.2D1 * t9
      t105 = cos(t13)
      t106 = -0.1D1 + x2
      t110 = Sqrt(x2 * t106 * x3 * t4)
      t112 = 0.2D1 * t105 * t110
      t120 = t17 * t11 * t106 * t4
      t123 = log(0.4D1 * t9 * t15 * t120)
      t124 = t123 ** 2
      t126 = x2 * z
      t128 = (t126 - x2 + 0.1D1) ** 2
      t129 = 0.1D1 / t128
      t140 = t72 * t15
      t144 = log(0.4D1 * t9 * t140 * t120)
      t154 = (0.45D2 * t8 * wd * t124 * t129 - t33 * t123 * t129 + (t32 
     #+ t41) * t129) * t43 / 0.810D3 + (-0.90D2 * t8 * wd * t144 * t129 
     #+ t33 * t129) * t43 * t85 / 0.405D3
      t155 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t104 - x2 + t112), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t104 + t112), 0.0D0, t154)
      t157 = x3 * x1
      t158 = t2 * t157
      t159 = -0.1D1 + x1
      t160 = t157 * z
      t161 = t9 * x1
      t162 = x1 * z
      t163 = t9 * t162
      t165 = 0.1D1 - x1 + t162
      t169 = Sqrt(x3 * t106 * t165 * x2 * t4)
      t171 = 0.2D1 * t105 * t169
      t174 = 0.1D1 / t165
      t177 = t4 * s
      t179 = t177 * t1 * x1
      t180 = x2 * x1
      t181 = t180 * z
      t182 = 0.1D1 - x1 + t162 - x2 + t180 - t181 - x3 + t157 - t160 + t
     #104 - t161 + t163 + t171
      t194 = t159 ** 2
      t200 = log(0.4D1 * t9 * t140 * t17 * t11 * t174 * t194 * t106 * t4
     #)
      t202 = (-t126 + t181 - t162 + x2 - t180 - 0.1D1 + x1) ** 2
      t203 = 0.1D1 / t202
      t205 = t165 ** 2
      t211 = 0.90D2 * t27 * t200 * t203 * t205 - t33 * t203 * t205
      t215 = FJET(XB1, XB2, s, t158, t2 * t159 * (-x3 + t157 - t160 + t1
     #04 - t161 + t163 - x2 + t171) * t174, -t179, -t2 * t159 * t182 * t
     #174, -s * t10 * x2 * t159 * x1 * t174, t211 * t43 * t85 / 0.405D3)
      t221 = t1 * t159
      t231 = log(-0.4D1 * t9 * t72 * t11 * t18 * t194 * t174 * t4)
      t244 = log(-0.4D1 * t88 * t74 * t17 * t174 * t194 * t4)
      t245 = t244 ** 2
      t253 = (-0.90D2 * t8 * wd * t231 - t28 - t32) * t43 * t85 / 0.405D
     #3 - (-0.45D2 * t8 * wd * t245 + t33 * t244 - t32 - t41) * t85 / 0.
     #405D3
      t254 = FJET(XB1, XB2, s, t158, -x3 * s * t221, -t179, t177 * t221,
     # 0.0D0, t253)
      rrqg2qght11s1e1 = t102 * t101 + t155 * t154 + t215 * t211 * t43 * 
     #t85 / 0.405D3 + t254 * t253

      end function



      doubleprecision function rrqg2qght11s1e0
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
      t8 = z * t7
      t9 = x2 * x3
      t10 = t1 ** 2
      t11 = t10 ** 2
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = z ** 2
      t17 = 0.1D1 / t16
      t19 = t15 * t17 * t4
      t22 = log(-0.4D1 * t9 * t11 * t19)
      t26 = t8 * wd
      t27 = 0.90D2 * t26
      t29 = t7 * wd
      t31 = 0.180D3 * lh * z * t29
      t33 = 0.1D1 / x2
      t37 = 0.1D1 / x1
      t40 = 0.2D1 / 0.9D1 * t8 * wd * t33 * t37
      t41 = x1 ** 2
      t42 = x3 * t41
      t46 = log(-0.4D1 * t42 * t11 * t19)
      t57 = log(-0.4D1 * x3 * t11 * t19)
      t65 = t57 ** 2
      t67 = lh ** 2
      t69 = pi ** 2
      t75 = (0.90D2 * t8 * wd * t22 + t27 + t31) * t33 / 0.810D3 - t40 -
     # (-0.90D2 * t8 * wd * t46 - t27 - t31) * t37 / 0.405D3 + (-0.180D3
     # * lh - 0.90D2 * t57) * z * t29 / 0.810D3 - (0.180D3 * t57 * lh + 
     #0.45D2 * t65 + 0.180D3 * t67 - 0.30D2 * t69) * z * t29 / 0.810D3
      t76 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t7
     #5)
      t78 = 0.2D1 * t9
      t79 = cos(t13)
      t80 = -0.1D1 + x2
      t84 = Sqrt(x2 * t80 * x3 * t4)
      t86 = 0.2D1 * t79 * t84
      t97 = log(0.4D1 * t9 * t15 * t17 * t11 * t80 * t4)
      t99 = x2 * z
      t101 = (t99 - x2 + 0.1D1) ** 2
      t102 = 0.1D1 / t101
      t115 = (-0.90D2 * t8 * wd * t97 * t102 + (-t27 - t31) * t102) * t3
     #3 / 0.810D3 + 0.2D1 / 0.9D1 * t26 * t102 * t33 * t37
      t116 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t78 - x2 + t86), 0.0D
     #0, t2 * (0.1D1 - x2 - x3 + t78 + t86), 0.0D0, t115)
      t118 = x3 * x1
      t119 = t2 * t118
      t120 = -0.1D1 + x1
      t121 = t118 * z
      t122 = t9 * x1
      t123 = x1 * z
      t124 = t9 * t123
      t126 = 0.1D1 - x1 + t123
      t130 = Sqrt(x3 * t80 * t126 * x2 * t4)
      t132 = 0.2D1 * t79 * t130
      t135 = 0.1D1 / t126
      t138 = t4 * s
      t140 = t138 * t1 * x1
      t141 = x2 * x1
      t142 = t141 * z
      t143 = 0.1D1 - x1 + t123 - x2 + t141 - t142 - x3 + t118 - t121 + t
     #78 - t122 + t124 + t132
      t153 = (-t99 + t142 - t123 + x2 - t141 - 0.1D1 + x1) ** 2
      t155 = t126 ** 2
      t158 = 0.1D1 / t153 * t155 * t33 * t37
      t161 = FJET(XB1, XB2, s, t119, t2 * t120 * (-x3 + t118 - t121 + t7
     #8 - t122 + t124 - x2 + t132) * t135, -t140, -t2 * t120 * t143 * t1
     #35, -s * t10 * x2 * t120 * x1 * t135, -0.2D1 / 0.9D1 * t26 * t158)
      t167 = t1 * t120
      t173 = t120 ** 2
      t178 = log(-0.4D1 * t42 * t11 * t15 * t17 * t135 * t173 * t4)
      t185 = t40 - (0.90D2 * t8 * wd * t178 + t27 + t31) * t37 / 0.405D3
      t186 = FJET(XB1, XB2, s, t119, -x3 * s * t167, -t140, t138 * t167,
     # 0.0D0, t185)
      rrqg2qght11s1e0 = t76 * t75 + t116 * t115 - 0.2D1 / 0.9D1 * t161 *
     # z * t29 * t158 + t186 * t185

      end function



      doubleprecision function rrqg2qght11s1em1
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
      t8 = z * t7
      t9 = 0.1D1 / x1
      t12 = 0.2D1 / 0.9D1 * t8 * wd * t9
      t16 = t1 ** 2
      t17 = t16 ** 2
      t19 = x4 * pi
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t28 = log(-0.4D1 * x3 * t17 * t21 / t22 * t4)
      t32 = t7 * wd
      t35 = 0.1D1 / x2
      t39 = -t12 + t8 * wd / 0.9D1 - (-0.180D3 * lh - 0.90D2 * t28) * z 
     #* t32 / 0.810D3 - t8 * wd * t35 / 0.9D1
      t40 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t3
     #9)
      t46 = t1 * (-0.1D1 + x1)
      t48 = t4 * s
      t52 = FJET(XB1, XB2, s, t2 * x1 * x3, -x3 * s * t46, -t48 * t1 * x
     #1, t48 * t46, 0.0D0, t12)
      t58 = 0.2D1 * x2 * x3
      t59 = cos(t19)
      t64 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t66 = 0.2D1 * t59 * t64
      t73 = (x2 * z - x2 + 0.1D1) ** 2
      t76 = wd / t73 * t35
      t79 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t58 - x2 + t66), 0.0D0
     #, t2 * (0.1D1 - x2 - x3 + t58 + t66), 0.0D0, t8 * t76 / 0.9D1)
      rrqg2qght11s1em1 = t40 * t39 + 0.2D1 / 0.9D1 * t52 * z * t32 * t9 
     #+ t79 * z * t7 * t76 / 0.9D1

      end function



      doubleprecision function rrqg2qght11s1em2
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
      t11 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, -z * t7 * wd / 0.9D1)
      rrqg2qght11s1em2 = -t11 * z * t7 * wd / 0.9D1

      end function



      doubleprecision function rrqg2qght11s1em3
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
      rrqg2qght11s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght11s1em4
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
      rrqg2qght11s1em4 = 0.0D0

      end function
