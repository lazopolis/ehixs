  
      subroutine rrqg2qght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh11J1  
      doubleprecision rrqg2qgh11J2  
      doubleprecision rrqg2qgh11J3  
      doubleprecision rrqg2qgh11J4  
      doubleprecision rrqg2qgh11J5  
      doubleprecision rrqg2qgh11J6  
      doubleprecision rrqg2qgh11J7  
      doubleprecision rrqg2qght1s1e1  
      doubleprecision rrqg2qght1s1e0  
      doubleprecision rrqg2qght1s1em1  
      doubleprecision rrqg2qght1s1em2  
      doubleprecision rrqg2qght1s1em3  
      doubleprecision rrqg2qght1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.1D1 / s
      t7 = 0.3141592653589793D1 * t6
      t8 = rrqg2qgh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = x2 * t11 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x3
      t20 = -0.1D1 + x2
      t22 = t19 * t4 * t20
      t25 = log(0.4D1 * t16 * t22)
      t26 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t28 = t25 ** 2
      t29 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t32 = rrqg2qgh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t33 = t19 * t4
      t36 = log(-0.4D1 * t16 * t33)
      t37 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t39 = t36 ** 2
      t40 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t46 = 0.3141592653589793D1 * lh
      t53 = lh ** 2
      t55 = 0.3141592653589793D1 ** 2
      t57 = 0.180D3 * t53 - 0.30D2 * t55
      t58 = 0.3141592653589793D1 * t57
      t59 = t29 - t40
      t63 = 0.1D1 / x2
      t66 = rrqg2qgh11J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t69 = t11 * t15
      t72 = log(-0.4D1 * t69 * t33)
      t73 = t72 ** 2
      t74 = t73 * 0.3141592653589793D1
      t86 = t72 * 0.3141592653589793D1
      t105 = x1 ** 2
      t106 = x2 * t105
      t110 = log(0.4D1 * t106 * t69 * t22)
      t114 = x3 * t4
      t118 = log(-0.4D1 * t106 * t15 * t11 * t18 * t114)
      t129 = 0.1D1 / x1
      t132 = t105 * t15
      t136 = log(-0.4D1 * t132 * t11 * t33)
      t138 = t136 ** 2
      t154 = (0.90D2 * t7 * (t8 - t25 * t26 + t28 * t29 / 0.2D1 - t32 + 
     #t36 * t37 - t39 * t40 / 0.2D1) - 0.180D3 * t46 * t6 * (t26 - t25 *
     # t29 - t37 + t36 * t40) + t58 * t6 * t59) * t63 / 0.1440D4 - t7 * 
     #t66 / 0.16D2 - (-0.90D2 * t74 * lh + 0.3141592653589793D1 * (0.60D
     #2 * lh * t55 - 0.2884936567583026D3 - 0.120D3 * t53 * lh) - 0.15D2
     # * t73 * t72 * 0.3141592653589793D1 - t86 * t57) * t6 * t40 / 0.14
     #40D4 - (-0.180D3 * t46 - 0.90D2 * t86) * t6 * t32 / 0.1440D4 - (0.
     #180D3 * t86 * lh + 0.45D2 * t74 + t58) * t6 * t37 / 0.1440D4 - (0.
     #90D2 * t7 * (t110 * t29 + t37 - t118 * t40 - t26) + 0.180D3 * t46 
     #* t6 * t59) * t63 * t129 / 0.720D3 - (0.90D2 * t7 * (t32 - t136 * 
     #t37 + t138 * t40 / 0.2D1) - 0.180D3 * t46 * t6 * (t37 - t136 * t40
     #) + t58 * t6 * t40) * t129 / 0.720D3
      t155 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #154)
      t157 = x3 * x1
      t159 = -0.1D1 + x1
      t161 = t2 * t159 * x3
      t165 = t2 * t159 * t4
      t166 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t167 = 0.1D1 / t9
      t168 = t15 * t167
      t170 = x1 * z
      t171 = -z - x1 + t170
      t172 = 0.1D1 / t171
      t174 = t159 ** 2
      t179 = log(0.4D1 * t106 * t168 * t19 * t4 * t172 * t174)
      t180 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t185 = t6 * t180
      t191 = rrqg2qgh11J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t194 = t172 * t174
      t198 = log(0.4D1 * t132 * t167 * t18 * t114 * t194)
      t200 = t198 ** 2
      t215 = -(0.90D2 * t7 * (-t166 + t179 * t180) + 0.180D3 * t46 * t18
     #5) * t63 * t129 / 0.720D3 - (-0.90D2 * t7 * (t191 - t198 * t166 + 
     #t200 * t180 / 0.2D1) + 0.180D3 * t46 * t6 * (t166 - t198 * t180) -
     # t58 * t185) * t129 / 0.720D3
      t216 = FJET(XB1, XB2, s, t157 * t2, -t161, -t4 * x1 * t2, t165, 0.
     #0D0, t215)
      t218 = x3 * z
      t219 = t157 * z
      t220 = x2 * x3
      t221 = t220 * z
      t222 = t220 * x1
      t223 = t220 * t170
      t224 = cos(t13)
      t229 = Sqrt(-x3 * t20 * t171 * x2 * t4)
      t231 = 0.2D1 * t224 * t229
      t237 = x2 * x1
      t239 = z + x1 - t170 - x2 * z - t237 + t237 * z - t218 - t157 + t2
     #19 + t221 + t222 - t223 + t220 + t231
      t248 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t255 = log(-0.4D1 * t106 * t168 * t18 * t114 * t194 * t20)
      t256 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t264 = 0.90D2 * t7 * (t248 - t255 * t256) - 0.180D3 * t46 * t6 * t
     #256
      t268 = FJET(XB1, XB2, s, t2 * x1 * (-t218 - t157 + t219 + t221 + t
     #222 - t223 - x2 + t220 + t231) * t172, -t161, -t2 * x1 * t239 * t1
     #72, t165, s * t17 * x2 * x1 * t159 * t172, -t264 * t63 * t129 / 0.
     #720D3)
      rrqg2qght1s1e1 = t155 * t154 + t216 * t215 - t268 * t264 * t63 * t
     #129 / 0.720D3

      end function



      doubleprecision function rrqg2qght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.1D1 / s
      t7 = 0.3141592653589793D1 * t6
      t8 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t13 = x4 * 0.3141592653589793D1
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = x2 * t11 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x3
      t20 = -0.1D1 + x2
      t25 = log(0.4D1 * t16 * t19 * t4 * t20)
      t26 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t28 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t29 = t19 * t4
      t32 = log(-0.4D1 * t16 * t29)
      t33 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t38 = 0.3141592653589793D1 * lh
      t39 = t26 - t33
      t44 = 0.1D1 / x2
      t49 = 0.1D1 / x1
      t53 = x1 ** 2
      t54 = t53 * t15
      t58 = log(-0.4D1 * t54 * t11 * t29)
      t69 = rrqg2qgh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t76 = log(-0.4D1 * t11 * t15 * t29)
      t77 = t76 * 0.3141592653589793D1
      t85 = t76 ** 2
      t88 = lh ** 2
      t90 = 0.3141592653589793D1 ** 2
      t98 = (0.90D2 * t7 * (t8 - t25 * t26 - t28 + t32 * t33) - 0.180D3 
     #* t38 * t6 * t39) * t44 / 0.1440D4 + t7 * t39 * t44 * t49 / 0.8D1 
     #- (0.90D2 * t7 * (t28 - t58 * t33) - 0.180D3 * t38 * t6 * t33) * t
     #49 / 0.720D3 - t7 * t69 / 0.16D2 - (-0.180D3 * t38 - 0.90D2 * t77)
     # * t6 * t28 / 0.1440D4 - (0.180D3 * t77 * lh + 0.45D2 * t85 * 0.31
     #41592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t88 - 0.30D2 
     #* t90)) * t6 * t33 / 0.1440D4
      t99 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t9
     #8)
      t101 = x3 * x1
      t103 = -0.1D1 + x1
      t105 = t2 * t103 * x3
      t109 = t2 * t103 * t4
      t110 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t115 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t120 = x1 * z
      t121 = -z - x1 + t120
      t122 = 0.1D1 / t121
      t123 = t103 ** 2
      t128 = log(0.4D1 * t54 / t9 * t18 * x3 * t4 * t122 * t123)
      t139 = t7 * t110 * t44 * t49 / 0.8D1 - (-0.90D2 * t7 * (t115 - t12
     #8 * t110) + 0.180D3 * t38 * t6 * t110) * t49 / 0.720D3
      t140 = FJET(XB1, XB2, s, t101 * t2, -t105, -t4 * x1 * t2, t109, 0.
     #0D0, t139)
      t142 = x3 * z
      t143 = t101 * z
      t144 = x2 * x3
      t145 = t144 * z
      t146 = t144 * x1
      t147 = t144 * t120
      t148 = cos(t13)
      t153 = Sqrt(-x3 * t20 * t121 * x2 * t4)
      t155 = 0.2D1 * t148 * t153
      t161 = x2 * x1
      t163 = z + x1 - t120 - x2 * z - t161 + t161 * z - t142 - t101 + t1
     #43 + t145 + t146 - t147 + t144 + t155
      t172 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t174 = t172 * t44 * t49
      t177 = FJET(XB1, XB2, s, t2 * x1 * (-t142 - t101 + t143 + t145 + t
     #146 - t147 - x2 + t144 + t155) * t122, -t105, -t2 * x1 * t163 * t1
     #22, t109, s * t17 * x2 * x1 * t103 * t122, -t7 * t174 / 0.8D1)
      rrqg2qght1s1e0 = t99 * t98 + t140 * t139 - t177 * 0.31415926535897
     #93D1 * t6 * t174 / 0.8D1

      end function



      doubleprecision function rrqg2qght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = 0.1D1 / s
      t7 = 0.3141592653589793D1 * t6
      t8 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x4
     #)
      t9 = 0.1D1 / x1
      t13 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t18 = z ** 2
      t22 = Sin(x4 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t25 = t1 ** 2
      t26 = t25 ** 2
      t31 = log(-0.4D1 / t18 / z * t23 * t26 * x3 * t4)
      t38 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t44 = -t7 * t8 * t9 / 0.8D1 - t7 * t13 / 0.16D2 - (-0.180D3 * 0.31
     #41592653589793D1 * lh - 0.90D2 * t31 * 0.3141592653589793D1) * t6 
     #* t8 / 0.1440D4 + t7 * (t38 - t8) / x2 / 0.16D2
      t45 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t4
     #4)
      t49 = -0.1D1 + x1
      t56 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t60 = FJET(XB1, XB2, s, x3 * x1 * t2, -t2 * t49 * x3, -t4 * x1 * t
     #2, t2 * t49 * t4, 0.0D0, t7 * t56 * t9 / 0.8D1)
      rrqg2qght1s1em1 = t45 * t44 + t60 * 0.3141592653589793D1 * t6 * t5
     #6 * t9 / 0.8D1

      end function



      doubleprecision function rrqg2qght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7
      t2 = s * (-0.1D1 + z)
      t6 = 0.1D1 / s
      t8 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x4
     #)
      t11 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, -0.3141592653589793D1 * t6 * t8 / 0.16D2)
      rrqg2qght1s1em2 = -t11 * 0.3141592653589793D1 * t6 * t8 / 0.16D2

      end function



      doubleprecision function rrqg2qght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7
      rrqg2qght1s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7
      rrqg2qght1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh11J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t5 * t3
      t7 = t2 * t6
      t8 = x1 ** 2
      t9 = t8 * x1
      t10 = x2 * t9
      t11 = t7 * t10
      t12 = 0.1D1 - x1
      t13 = t12 ** 2
      t14 = x1 * t3
      t15 = z + t14
      t16 = t15 ** 2
      t17 = 0.1D1 / t16
      t18 = t13 * t17
      t19 = s * t3
      t20 = 0.1D1 / t15
      t21 = x1 * t20
      t22 = 0.1D1 - x2
      t23 = x3 * t22
      t25 = 0.1D1 - x3
      t28 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(t23 * t15 * x2 * t25)
      t34 = 0.2D1 * t28 * t32
      t35 = t23 * t15 + x2 * t25 - t34
      t36 = t21 * t35
      t38 = t12 * x3
      t40 = s - t19 * t36 - t19 * t38
      t44 = t25 * t22 * t15 + x2 * x3 + t34
      t45 = t40 * t44
      t50 = s * t1
      t51 = t50 * t5
      t52 = x2 ** 2
      t54 = t8 * t13
      t55 = t40 ** 2
      t56 = t17 * t55
      t60 = t40 * t2
      t61 = t4 * t3
      t62 = t60 * t61
      t63 = t8 * x2
      t64 = t12 * t20
      t68 = t2 * s
      t70 = t68 * t5 * x2
      t71 = t20 * x3
      t72 = t54 * t71
      t75 = t5 * t4
      t77 = t68 * t75 * t52
      t78 = t13 * t12
      t79 = t9 * t78
      t80 = t17 * x3
      t84 = t50 * t4
      t86 = x1 * t12
      t87 = t20 * t55
      t91 = t55 * t50
      t92 = t91 * t4
      t93 = t8 * t17
      t94 = t35 * t44
      t99 = t52 * x2
      t100 = t99 * t9
      t102 = 0.1D1 / t16 / t15
      t103 = t78 * t102
      t107 = t5 ** 2
      t109 = t52 ** 2
      t111 = t8 ** 2
      t112 = t13 ** 2
      t114 = t16 ** 2
      t115 = 0.1D1 / t114
      t120 = t5 * t61
      t121 = t2 * t120
      t123 = t111 * t78
      t124 = t102 * t40
      t134 = t9 * t12
      t135 = t17 * t35
      t136 = t134 * t135
      t139 = t111 * t13
      t144 = t50 * t6
      t146 = t9 * t13
      t150 = t68 * t6
      t151 = t150 * x2
      t152 = t111 * t12
      t153 = t35 ** 2
      t154 = t102 * t153
      t158 = t17 * t44
      t162 = x1 * t13
      t163 = x3 * t25
      t172 = t50 * t61
      t174 = t8 * t12
      t178 = 0.96D2 * t11 * t18 * t45 * x3 - 0.3523D4 * t51 * t52 * t54 
     #* t56 + 0.3523D4 * t62 * t63 * t64 + 0.752D3 * t70 * t72 - 0.1088D
     #4 * t77 * t79 * t80 + 0.1244D4 * t84 * x2 * t86 * t87 + 0.94D2 * t
     #92 * t93 * t94 - 0.576D3 * t60 * t75 * t100 * t103 + 0.36D2 * t2 *
     # t107 * t109 * t111 * t112 * t115 * t40 - 0.12D2 * t121 * t99 * t1
     #23 * t124 - 0.10D2 * t50 * t75 * t99 * t79 * t102 * t55 + 0.848D3 
     #* t70 * t136 - 0.1088D4 * t77 * t139 * t102 * t35 - 0.122D3 * t144
     # * t52 * t146 * t56 - 0.472D3 * t151 * t152 * t154 - 0.1088D4 * t7
     #0 * t134 * t158 + 0.584D3 * t62 * t162 * t163 - 0.1244D4 * t7 * t9
     # * t40 * t52 * t18 - 0.7896D4 * t172 * x2 * t174 * t87
      t179 = t91 * z
      t180 = t20 * t44
      t184 = t20 * t35
      t189 = t52 * t8
      t193 = t60 * t4
      t194 = x2 * x1
      t198 = t102 * t44
      t202 = t44 ** 2
      t203 = t102 * t202
      t207 = t20 * t25
      t215 = t8 * t78
      t216 = x3 ** 2
      t217 = t20 * t216
      t221 = t25 ** 2
      t222 = t20 * t221
      t228 = t40 * t25
      t232 = t51 * t10
      t233 = t12 * t102
      t238 = t52 * t9
      t239 = t144 * t238
      t240 = t13 * t102
      t241 = t55 * t44
      t245 = t86 * t25
      t249 = t13 * x3 * t25
      t255 = t93 * t153
      t264 = t91 * t3
      t267 = -0.128D3 * t179 * t14 * t180 + 0.128D3 * t179 * t14 * t184 
     #+ 0.864D3 * t60 * t5 * t189 * t18 - 0.576D3 * t193 * t194 * t64 + 
     #0.544D3 * t77 * t139 * t198 - 0.272D3 * t151 * t152 * t203 - 0.108
     #8D4 * t70 * t54 * t207 + 0.544D3 * t77 * t79 * t17 * t25 - 0.376D3
     # * t151 * t215 * t217 - 0.272D3 * t151 * t215 * t222 + 0.144D3 * t
     #121 * t100 * t112 * t102 * t228 - 0.430D3 * t232 * t233 * t55 * t2
     #02 + 0.328D3 * t239 * t240 * t241 + 0.528D3 * t92 * t245 - 0.880D3
     # * t92 * t249 - 0.47D2 * t92 * t93 * t202 - 0.47D2 * t92 * t255 - 
     #0.328D3 * t193 * t245 + 0.192D3 * t62 * t9 * t17 * t153 - 0.148D3 
     #* t264 * t36
      t270 = t153 * t35
      t284 = t8 * t20
      t302 = t61 * t8
      t303 = t91 * t302
      t304 = t180 * t38
      t307 = t150 * t10
      t313 = t121 * t99 * t111
      t314 = t78 * t115
      t315 = t40 * t35
      t319 = t2 * t5
      t321 = t12 * t17
      t322 = t40 * z
      t327 = t5 * x2
      t328 = t60 * t327
      t333 = t60 * t61 * x2
      t344 = t7 * t63
      t345 = t78 * t20
      t346 = t40 * x3
      t355 = t78 * t17
      t356 = t40 * t221
      t361 = t4 * t12
      t364 = x3 * x1 * t184
      t367 = t4 * x1
      t368 = t91 * t367
      t369 = t12 * t25
      t370 = t184 * t369
      t373 = -0.28D2 * t62 * t9 * t102 * t270 + 0.56D2 * t62 * t162 * t2
     #21 + 0.216D3 * t62 * t78 * t221 * x3 - 0.28D2 * t60 * t3 * t36 - 0
     #.17D2 * t92 * t284 * t44 + 0.1088D4 * t150 * t52 * t146 * t17 - 0.
     #272D3 * t68 * t120 * t99 * t123 * t102 - 0.376D3 * t68 * t61 * x2 
     #* t174 * t20 - 0.128D3 * t303 * t304 + 0.1088D4 * t307 * t18 * t35
     # * t25 - 0.28D2 * t313 * t314 * t315 - 0.128D3 * t319 * t10 * t321
     # * t322 * t35 - 0.576D3 * t328 * t134 * t203 + 0.1728D4 * t333 * t
     #174 * t17 * z * t35 + 0.1728D4 * t84 * t194 * t64 * t55 * z + 0.12
     #8D3 * t344 * t345 * t346 * t25 + 0.1088D4 * t150 * t63 * t345 * t1
     #63 + 0.216D3 * t344 * t355 * t356 * t44 - 0.56D2 * t60 * t361 * t3
     #64 + 0.128D3 * t368 * t370
      t379 = t51 * t63
      t380 = t13 * t20
      t382 = t380 * t55 * x3
      t389 = t60 * t302
      t393 = t2 * t75
      t394 = t393 * t238
      t395 = t315 * t25
      t400 = t393 * t52 * t111
      t401 = t13 * t115
      t402 = t40 * t202
      t410 = t319 * t8 * t40
      t411 = x2 * t13
      t418 = t17 * t153
      t430 = t55 * t25
      t431 = t380 * t430
      t437 = t40 * t153
      t447 = t172 * t63
      t448 = t321 * t241
      t451 = x2 * t111
      t452 = t7 * t451
      t456 = t172 * t194
      t459 = -0.112D3 * t60 * t367 * t370 + 0.128D3 * t303 * t370 - 0.17
     #D2 * t379 * t382 - 0.848D3 * t307 * t18 * x3 * t35 + 0.530D3 * t38
     #9 * t38 * t184 - 0.112D3 * t394 * t103 * t395 + 0.108D3 * t400 * t
     #401 * t402 + 0.108D3 * t313 * t314 * t45 - 0.3523D4 * t410 * t411 
     #* t71 + 0.36D2 * t60 * t61 * t12 * x3 * t8 * t418 + 0.864D3 * t328
     # * t54 * t158 * x3 + 0.1088D4 * t307 * t18 * t44 * x3 + 0.528D3 * 
     #t379 * t431 + 0.312D3 * t344 * t345 * t356 + 0.36D2 * t400 * t401 
     #* t437 - 0.148D3 * t394 * t355 * t346 - 0.16D2 * t400 * t240 * t45
     # + 0.3523D4 * t447 * t448 + 0.748D3 * t452 * t233 * t402 - 0.1632D
     #4 * t456 * t431
      t462 = t55 * t35
      t470 = x2 * t12
      t475 = z ** 2
      t477 = t68 * t475 * t327
      t480 = t228 * t44
      t485 = t319 * t9 * t40
      t493 = t321 * t462
      t507 = t12 * t115
      t520 = t91 * t361
      t533 = t25 * t44
      t537 = -0.400D3 * t379 * t18 * t462 * t25 + 0.240D3 * t394 * t355 
     #* t228 - 0.1728D4 * t389 * t470 * t20 * z + 0.64D2 * t477 * t136 +
     # 0.880D3 * t11 * t18 * t480 + 0.1244D4 * t485 * t470 * t158 + 0.10
     #88D4 * t150 * t451 * t233 * t94 + 0.3466D4 * t447 * t493 - 0.1244D
     #4 * t456 * t382 + 0.144D3 * t11 * t240 * t228 * t202 + 0.128D3 * t
     #319 * t63 * t380 * t322 * x3 - 0.28D2 * t452 * t507 * t40 * t270 +
     # 0.12D2 * t91 + 0.36D2 * t60 + 0.148D3 * t400 * t240 * t315 - 0.12
     #8D3 * t11 * t18 * t395 + 0.96D2 * t520 * t364 - 0.1728D4 * t60 * t
     #4 * x2 * t86 * t20 * t475 - 0.128D3 * t520 * t25 * x1 * t180 - 0.5
     #44D3 * t307 * t18 * t533
      t546 = t60 * t61 * t13
      t556 = t315 * t44
      t560 = x1 * t78
      t567 = t4 * t13
      t568 = t567 * t216
      t573 = t567 * t221
      t578 = t61 * t78
      t579 = t221 * t25
      t583 = t3 * t12
      t584 = t583 * x3
      t593 = t583 * t25
      t604 = 0.36D2 * t452 * t507 * t40 * t202 * t44 + 0.275D3 * t232 * 
     #t493 - 0.112D3 * t546 * t163 * t36 + 0.306D3 * t232 * t448 - 0.584
     #D3 * t379 * t18 * t430 * t44 - 0.96D2 * t452 * t233 * t556 - 0.576
     #D3 * t328 * t560 * t217 - 0.576D3 * t328 * t134 * t154 + 0.108D3 *
     # t60 * t568 - 0.748D3 * t91 * t568 - 0.312D3 * t91 * t573 + 0.216D
     #3 * t60 * t573 + 0.144D3 * t60 * t578 * t579 + 0.16D2 * t91 * t584
     # + 0.36D2 * t60 * t578 * t216 * x3 + 0.108D3 * t60 * t584 + 0.144D
     #3 * t60 * t593 - 0.240D3 * t91 * t593 + 0.36D2 * t452 * t507 * t45
     # * t153 - 0.64D2 * t477 * t72
      t636 = t60 * t6 * t52
      t661 = t284 * t35
      t669 = t86 * x3
      t672 = -0.28D2 * t546 * t216 * x1 * t184 - 0.56D2 * t400 * t401 * 
     #t556 - 0.28D2 * t452 * t507 * t402 * t35 + 0.10D2 * t60 * t14 - 0.
     #122D3 * t91 * t14 - 0.168D3 * t344 * t355 * t315 * t221 - 0.128D3 
     #* t328 * t54 * t135 * t25 - 0.192D3 * t232 * t233 * t55 * t153 - 0
     #.96D2 * t368 * t304 - 0.1152D4 * t636 * t146 * t198 - 0.3466D4 * t
     #485 * t470 * t135 + 0.400D3 * t389 * t370 - 0.530D3 * t232 * t233 
     #* t462 * t44 + 0.288D3 * t394 * t103 * t480 - 0.112D3 * t7 * t10 *
     # t13 * t124 * t533 * t35 + 0.430D3 * t62 * t162 * t216 + 0.275D3 *
     # t92 * t661 - 0.182D3 * t193 * t661 + 0.148D3 * t264 * t21 * t44 +
     # 0.306D3 * t92 * t669
      t745 = 0.36D2 * t193 * t255 + 0.288D3 * t193 * t249 - 0.328D3 * t1
     #93 * t669 + 0.144D3 * t62 * t78 * t25 * t216 + 0.864D3 * t333 * t1
     #74 * t158 + 0.328D3 * t144 * t189 * t355 * t430 + 0.224D3 * t328 *
     # t560 * t222 + 0.47D2 * t452 * t233 * t437 + 0.47D2 * t344 * t345 
     #* t40 * t216 + 0.144D3 * t11 * t240 * t437 * t25 + 0.144D3 * t389 
     #* t418 * t369 + 0.1632D4 * t410 * t411 * t207 - 0.94D2 * t11 * t18
     # * t346 * t35 - 0.168D3 * t60 * t61 * x1 * t184 * t13 * t221 + 0.1
     #44D3 * t7 * t194 * t112 * t20 * t40 * t579 + 0.216D3 * t393 * t189
     # * t112 * t17 * t356 + 0.182D3 * t239 * t240 * t462 - 0.1152D4 * t
     #333 * t162 * t71 + 0.864D3 * t636 * t215 * t80 - 0.56D2 * t51 * t1
     #94 * t345 * t55 * t221
      rrqg2qgh11J1 = wd * (t178 + t267 + t373 + t459 + t537 + t604 + t67
     #2 + t745) / t1 / t55 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh11J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t21 - t2 * t24
      t27 = s ** 2
      t28 = t27 ** 2
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = t23 ** 2
      t34 = x1 * t33
      t35 = x3 ** 2
      t36 = t34 * t35
      t37 = t32 * t36
      t39 = t27 * s
      t40 = t26 ** 2
      t41 = t39 * t40
      t42 = t41 * t30
      t43 = x1 ** 2
      t44 = t43 * t5
      t45 = t20 * t44
      t46 = t42 * t45
      t48 = t29 * t30
      t49 = t48 * t45
      t51 = t41 * t1
      t55 = t10 * t7 * t4 + x2 * x3 + t19
      t56 = t6 * t55
      t57 = t51 * t56
      t59 = x1 * t23
      t60 = t59 * x3
      t61 = t42 * t60
      t63 = t4 ** 2
      t64 = 0.1D1 / t63
      t65 = t43 * t64
      t66 = t20 ** 2
      t67 = t65 * t66
      t68 = t48 * t67
      t71 = t33 * x3 * t10
      t74 = t48 * t60
      t76 = t33 * t23
      t81 = t59 * t10
      t82 = t42 * t81
      t84 = t42 * t71
      t86 = t55 ** 2
      t88 = t42 * t65 * t86
      t90 = t42 * t67
      t92 = t48 * t81
      t94 = t43 * x1
      t95 = t94 * t64
      t97 = t32 * t95 * t66
      t99 = t51 * t21
      t102 = 0.1D1 / t63 / t4
      t104 = t66 * t20
      t106 = t32 * t94 * t102 * t104
      t108 = t10 ** 2
      t109 = t34 * t108
      t110 = t32 * t109
      t114 = t32 * t76 * t108 * x3
      t116 = 0.430D3 * t37 + 0.275D3 * t46 - 0.182D3 * t49 + 0.148D3 * t
     #57 + 0.306D3 * t61 + 0.36D2 * t68 + 0.288D3 * t48 * t71 - 0.328D3 
     #* t74 + 0.144D3 * t32 * t76 * t10 * t35 + 0.528D3 * t82 - 0.880D3 
     #* t84 - 0.47D2 * t88 - 0.47D2 * t90 - 0.328D3 * t92 + 0.192D3 * t9
     #7 - 0.148D3 * t99 - 0.28D2 * t106 + 0.56D2 * t110 + 0.216D3 * t114
      t120 = t55 * t44
      t121 = t42 * t120
      t123 = t28 * s
      t124 = t30 ** 2
      t125 = t124 * t1
      t126 = t123 * t125
      t127 = x2 ** 2
      t129 = t94 * t33
      t131 = t126 * t127 * t129 * t64
      t133 = t124 * t31
      t135 = t127 * x2
      t137 = t43 ** 2
      t138 = t137 * t76
      t140 = t123 * t133 * t135 * t138 * t102
      t144 = t43 * t23
      t146 = t123 * t31 * x2 * t144 * t5
      t148 = t39 * t124
      t150 = t43 * t33
      t151 = t64 * t40
      t153 = t148 * t127 * t150 * t151
      t155 = t43 * x2
      t156 = t23 * t5
      t158 = t32 * t155 * t156
      t161 = t123 * t124 * x2
      t162 = t5 * x3
      t163 = t150 * t162
      t164 = t161 * t163
      t166 = t124 * t30
      t168 = t123 * t166 * t127
      t169 = t94 * t76
      t170 = t64 * x3
      t172 = t168 * t169 * t170
      t174 = t39 * t30
      t176 = t5 * t40
      t178 = t174 * x2 * t59 * t176
      t180 = t20 * t55
      t182 = t42 * t65 * t180
      t185 = t135 * t94
      t186 = t76 * t102
      t190 = t124 ** 2
      t192 = t127 ** 2
      t194 = t33 ** 2
      t196 = t63 ** 2
      t197 = 0.1D1 / t196
      t202 = t28 * t133
      t204 = t102 * t26
      t206 = t202 * t135 * t138 * t204
      t212 = t39 * t166 * t135 * t169 * t102 * t40
      t214 = t94 * t23
      t215 = t64 * t20
      t216 = t214 * t215
      t217 = t161 * t216
      t219 = t137 * t33
      t222 = t168 * t219 * t102 * t20
      t224 = t39 * t125
      t227 = t224 * t127 * t129 * t151
      t229 = t126 * x2
      t230 = t137 * t23
      t231 = t102 * t66
      t233 = t229 * t230 * t231
      t235 = t64 * t55
      t237 = t161 * t214 * t235
      t239 = -0.28D2 * t29 * t1 * t21 - 0.17D2 * t121 + 0.1088D4 * t131 
     #- 0.272D3 * t140 - 0.376D3 * t146 - 0.3523D4 * t153 + 0.3523D4 * t
     #158 + 0.752D3 * t164 - 0.1088D4 * t172 + 0.1244D4 * t178 + 0.94D2 
     #* t182 - 0.576D3 * t29 * t166 * t185 * t186 + 0.36D2 * t28 * t190 
     #* t192 * t137 * t194 * t197 * t26 - 0.12D2 * t206 - 0.10D2 * t212 
     #+ 0.848D3 * t217 - 0.1088D4 * t222 - 0.122D3 * t227 - 0.472D3 * t2
     #33 - 0.1088D4 * t237
      t241 = x3 * t10
      t243 = t32 * t34 * t241
      t245 = t28 * t125
      t248 = t33 * t64
      t250 = t245 * t94 * t26 * t127 * t248
      t252 = t39 * t31
      t255 = t252 * x2 * t144 * t176
      t257 = t41 * z
      t258 = t5 * t55
      t260 = t257 * t3 * t258
      t262 = t5 * t20
      t264 = t257 * t3 * t262
      t266 = t29 * t124
      t267 = t127 * t43
      t271 = x2 * x1
      t275 = t102 * t55
      t277 = t168 * t219 * t275
      t279 = t102 * t86
      t281 = t229 * t230 * t279
      t283 = t5 * t10
      t285 = t161 * t150 * t283
      t289 = t168 * t169 * t64 * t10
      t291 = t29 * t3
      t293 = t41 * t3
      t295 = t31 * t76
      t300 = t1 * t23
      t301 = t300 * x3
      t304 = t300 * t10
      t307 = t41 * t304
      t309 = t30 * t33
      t310 = t309 * t35
      t313 = t41 * t310
      t315 = t309 * t108
      t316 = t41 * t315
      t318 = 0.584D3 * t243 - 0.1244D4 * t250 - 0.7896D4 * t255 - 0.128D
     #3 * t260 + 0.128D3 * t264 + 0.864D3 * t266 * t267 * t248 - 0.576D3
     # * t48 * t271 * t156 + 0.544D3 * t277 - 0.272D3 * t281 - 0.1088D4 
     #* t285 + 0.544D3 * t289 + 0.10D2 * t291 - 0.122D3 * t293 + 0.36D2 
     #* t29 * t295 * t35 * x3 + 0.108D3 * t29 * t301 + 0.144D3 * t29 * t
     #304 - 0.240D3 * t307 + 0.108D3 * t29 * t310 - 0.748D3 * t313 - 0.3
     #12D3 * t316
      t319 = t29 * t315
      t321 = t108 * t10
      t324 = 0.144D3 * t29 * t295 * t321
      t325 = t41 * t301
      t327 = t43 * t76
      t328 = t5 * t35
      t330 = t229 * t327 * t328
      t332 = t5 * t108
      t334 = t229 * t327 * t332
      t336 = t30 * t23
      t337 = t41 * t336
      t340 = t337 * t10 * x1 * t258
      t342 = x2 * t94
      t343 = t126 * t342
      t344 = t10 * t55
      t346 = t343 * t248 * t344
      t348 = x2 * t137
      t349 = t245 * t348
      t350 = t23 * t197
      t356 = t148 * t342
      t357 = t23 * t102
      t360 = t356 * t357 * t40 * t86
      t362 = t127 * t94
      t363 = t224 * t362
      t364 = t33 * t102
      t365 = t40 * t55
      t367 = t363 * t364 * t365
      t370 = t29 * t31 * x2
      t375 = t29 * t125 * t127
      t379 = t28 * t124
      t381 = t379 * t94 * t26
      t382 = x2 * t23
      t384 = t381 * t382 * t215
      t386 = t31 * t43
      t387 = t29 * t386
      t388 = t23 * t10
      t389 = t262 * t388
      t390 = t387 * t389
      t392 = t124 * x2
      t393 = t29 * t392
      t394 = x1 * t76
      t399 = t393 * t214 * t231
      t401 = t245 * t155
      t402 = t76 * t64
      t403 = t26 * t108
      t406 = t401 * t402 * t403 * t55
      t408 = t245 * t342
      t409 = t26 * t10
      t414 = t28 * t166
      t415 = t414 * t362
      t416 = t409 * t55
      t424 = t245 * t342 * t33 * t204 * t344 * t20
      t426 = 0.216D3 * t319 + t324 + 0.16D2 * t325 - 0.376D3 * t330 - 0.
     #272D3 * t334 - 0.128D3 * t340 - 0.544D3 * t346 + 0.36D2 * t349 * t
     #350 * t26 * t86 * t55 - 0.430D3 * t360 + 0.328D3 * t367 + 0.864D3 
     #* t370 * t144 * t235 - 0.1152D4 * t375 * t129 * t275 - 0.3466D4 * 
     #t384 + 0.400D3 * t390 - 0.576D3 * t393 * t394 * t328 - 0.576D3 * t
     #399 + 0.216D3 * t406 + 0.144D3 * t408 * t364 * t409 * t86 + 0.288D
     #3 * t415 * t186 * t416 - 0.112D3 * t424
      t429 = t148 * t155
      t430 = t40 * t10
      t433 = t429 * t248 * t430 * t55
      t435 = t26 * t20
      t436 = t435 * t55
      t438 = t349 * t357 * t436
      t440 = t26 * t55
      t443 = t408 * t248 * t440 * x3
      t445 = t31 * x1
      t448 = t262 * t33 * t108
      t449 = t29 * t445 * t448
      t456 = 0.144D3 * t245 * t271 * t194 * t5 * t26 * t321
      t460 = t414 * t267 * t194 * t64 * t403
      t462 = t148 * t271
      t463 = t76 * t5
      t466 = t462 * t463 * t40 * t108
      t468 = t224 * t267
      t470 = t468 * t402 * t430
      t473 = t393 * t394 * t332
      t475 = t26 * t66
      t477 = t349 * t357 * t475
      t481 = t401 * t463 * t26 * t35
      t483 = t252 * t271
      t484 = t33 * t5
      t485 = t484 * t430
      t486 = t483 * t485
      t488 = t40 * t20
      t491 = t429 * t248 * t488 * t10
      t494 = t415 * t402 * t409
      t496 = t429 * t485
      t499 = t401 * t463 * t403
      t502 = t379 * t43 * t26
      t503 = x2 * t33
      t505 = t502 * t503 * t162
      t510 = t64 * t66
      t512 = t29 * t31 * t23 * x3 * t43 * t510
      t515 = x3 * x1 * t262
      t516 = t337 * t515
      t520 = z ** 2
      t524 = 0.1728D4 * t29 * t30 * x2 * t59 * t5 * t520
      t525 = -0.584D3 * t433 - 0.96D2 * t438 + 0.96D2 * t443 - 0.168D3 *
     # t449 + t456 + 0.216D3 * t460 - 0.56D2 * t466 + 0.328D3 * t470 + 0
     #.224D3 * t473 + 0.47D2 * t477 + 0.47D2 * t481 - 0.1632D4 * t486 - 
     #0.400D3 * t491 + 0.240D3 * t494 + 0.528D3 * t496 + 0.312D3 * t499 
     #- 0.3523D4 * t505 + 0.36D2 * t512 + 0.96D2 * t516 - t524
      t527 = t26 * z
      t530 = t379 * t155 * t484 * t527 * x3
      t535 = 0.1728D4 * t387 * t382 * t5 * z
      t540 = 0.1728D4 * t370 * t144 * t64 * z * t20
      t545 = 0.1728D4 * t174 * t271 * t156 * t40 * z
      t546 = t26 * x3
      t549 = t401 * t463 * t546 * t10
      t553 = t126 * t155 * t463 * t241
      t556 = t123 * t520 * t392
      t557 = t556 * t163
      t561 = t356 * t357 * t488 * t55
      t564 = t414 * t127 * t137
      t566 = t564 * t364 * t440
      t568 = t41 * t386
      t569 = t258 * t24
      t574 = t343 * t248 * t20 * t10
      t577 = t202 * t135 * t137
      t578 = t76 * t197
      t582 = t31 * t33
      t583 = t29 * t582
      t584 = t35 * x1
      t585 = t584 * t262
      t590 = t408 * t364 * t475 * t10
      t594 = t401 * t402 * t435 * t108
      t596 = t435 * t10
      t598 = t415 * t186 * t596
      t600 = t33 * t197
      t601 = t26 * t86
      t611 = t30 * x1
      t612 = t41 * t611
      t613 = t612 * t389
      t615 = 0.128D3 * t530 - t535 + t540 + t545 + 0.128D3 * t549 + 0.10
     #88D4 * t553 - 0.64D2 * t557 - 0.530D3 * t561 - 0.16D2 * t566 - 0.1
     #28D3 * t568 * t569 + 0.1088D4 * t574 - 0.28D2 * t577 * t578 * t435
     # - 0.28D2 * t583 * t585 + 0.144D3 * t590 - 0.168D3 * t594 - 0.112D
     #3 * t598 + 0.108D3 * t564 * t600 * t601 + 0.108D3 * t577 * t578 * 
     #t440 - 0.56D2 * t29 * t336 * t515 + 0.128D3 * t613
      t618 = t29 * t611 * t389
      t623 = t387 * t510 * t388
      t626 = t502 * t503 * t283
      t630 = t408 * t248 * t546 * t20
      t632 = t23 * t64
      t633 = t632 * t488
      t634 = t356 * t633
      t636 = t40 * x3
      t637 = t484 * t636
      t638 = t429 * t637
      t642 = t343 * t248 * x3 * t20
      t645 = t387 * t24 * t262
      t647 = t556 * t216
      t650 = t408 * t248 * t416
      t652 = t612 * t569
      t655 = t381 * t382 * t235
      t659 = t126 * t348 * t357 * t180
      t661 = t252 * t155
      t662 = t661 * t633
      t664 = t483 * t637
      t667 = t564 * t600 * t475
      t670 = t415 * t402 * t546
      t673 = t564 * t364 * t435
      t676 = t408 * t248 * t596
      t678 = -0.112D3 * t618 + 0.128D3 * t568 * t389 + 0.144D3 * t623 + 
     #0.1632D4 * t626 - 0.94D2 * t630 + 0.275D3 * t634 - 0.17D2 * t638 -
     # 0.848D3 * t642 + 0.530D3 * t645 + 0.64D2 * t647 + 0.880D3 * t650 
     #- 0.96D2 * t652 + 0.1244D4 * t655 + 0.1088D4 * t659 + 0.3466D4 * t
     #662 - 0.1244D4 * t664 + 0.36D2 * t667 - 0.148D3 * t670 + 0.148D3 *
     # t673 - 0.128D3 * t676
      t684 = t241 * t21
      t685 = t583 * t684
      t687 = t632 * t365
      t688 = t356 * t687
      t696 = t343 * t248 * t55 * x3
      t701 = t379 * t342 * t632 * t527 * t20
      t715 = t349 * t350 * t440 * t66
      t719 = t393 * t150 * t215 * t10
      t723 = t356 * t357 * t40 * t66
      t727 = t349 * t350 * t26 * t104
      t730 = t363 * t364 * t488
      t738 = t661 * t687
      t743 = t349 * t357 * t601
      t745 = 0.144D3 * t202 * t185 * t194 * t102 * t409 - 0.112D3 * t685
     # + 0.306D3 * t688 + 0.864D3 * t393 * t150 * t235 * x3 + 0.1088D4 *
     # t696 - 0.128D3 * t701 - 0.576D3 * t393 * t214 * t279 - 0.56D2 * t
     #564 * t600 * t436 - 0.28D2 * t349 * t350 * t601 * t20 + 0.36D2 * t
     #715 - 0.128D3 * t719 - 0.192D3 * t723 - 0.28D2 * t727 + 0.182D3 * 
     #t730 - 0.1152D4 * t370 * t34 * t162 + 0.864D3 * t375 * t327 * t170
     # + 0.3523D4 * t738 + 0.12D2 * t41 + 0.36D2 * t29 + 0.748D3 * t743
      t769 = -0.542D3 * t37 - 0.156D3 * t46 + 0.108D3 * t49 - 0.216D3 * 
     #t57 - 0.190D3 * t61 - 0.72D2 * t68 + 0.604D3 * t74 - 0.752D3 * t82
     # + 0.640D3 * t84 + 0.54D2 * t88 + 0.54D2 * t90 + 0.256D3 * t92 - 0
     #.30D2 * t97 + 0.216D3 * t99 + 0.56D2 * t106 + 0.24D2 * t110 - 0.72
     #D2 * t114 + 0.490D3 * t121 - 0.1728D4 * t131
      t789 = 0.432D3 * t140 + 0.584D3 * t146 - 0.5266D4 * t153 + 0.5266D
     #4 * t158 - 0.1168D4 * t164 + 0.1728D4 * t172 - 0.3776D4 * t178 - 0
     #.108D3 * t182 - 0.16D2 * t206 + 0.62D2 * t212 - 0.1360D4 * t217 + 
     #0.1728D4 * t222 + 0.134D3 * t227 + 0.776D3 * t233 + 0.1728D4 * t23
     #7 - 0.544D3 * t243 + 0.3776D4 * t250 - 0.9344D4 * t255 + 0.256D3 *
     # t260
      t818 = -0.256D3 * t264 - 0.864D3 * t277 + 0.432D3 * t281 + 0.1728D
     #4 * t285 - 0.864D3 * t289 - 0.62D2 * t291 + 0.134D3 * t293 - 0.64D
     #2 * t307 + 0.592D3 * t313 + 0.264D3 * t316 - 0.72D2 * t319 - t324 
     #- 0.608D3 * t325 + 0.584D3 * t330 + 0.432D3 * t334 + 0.256D3 * t32
     # * t95 * t180 - 0.64D2 * t266 * t137 * t102 * t86 * t20 + 0.64D2 *
     # t266 * t394 * t108 * x3 + 0.496D3 * t340
      t837 = 0.864D3 * t346 + 0.542D3 * t360 - 0.604D3 * t367 - 0.4940D4
     # * t384 - 0.288D3 * t390 + 0.352D3 * t399 - 0.72D2 * t406 + 0.144D
     #3 * t424 + 0.544D3 * t433 + 0.640D3 * t438 - 0.640D3 * t443 + 0.34
     #4D3 * t449 - t456 - 0.72D2 * t460 - 0.24D2 * t466 - 0.256D3 * t470
     # - 0.1376D4 * t473 - 0.54D2 * t477 - 0.54D2 * t481
      t855 = 0.4816D4 * t486 + 0.288D3 * t491 + 0.64D2 * t494 - 0.752D3 
     #* t496 - 0.264D3 * t499 - 0.5266D4 * t505 - 0.72D2 * t512 - 0.640D
     #3 * t516 + t524 - 0.256D3 * t530 + t535 - t540 - t545 - 0.496D3 * 
     #t549 - 0.1728D4 * t553 + 0.128D3 * t557 + 0.444D3 * t561 + 0.608D3
     # * t566 - 0.1728D4 * t574
      t875 = -0.256D3 * t590 + 0.344D3 * t594 + 0.144D3 * t598 - 0.496D3
     # * t613 + 0.144D3 * t618 - 0.256D3 * t623 - 0.4816D4 * t626 + 0.10
     #8D3 * t630 - 0.156D3 * t634 + 0.490D3 * t638 + 0.1360D4 * t642 - 0
     #.444D3 * t645 - 0.128D3 * t647 - 0.640D3 * t650 + 0.640D3 * t652 -
     # 0.3776D4 * t655 - 0.1728D4 * t659 + 0.4940D4 * t662 + 0.3776D4 * 
     #t664
      t893 = t41 * t31
      t898 = t95 * t86
      t901 = -0.72D2 * t667 + 0.216D3 * t670 - 0.216D3 * t673 + 0.496D3 
     #* t676 + 0.144D3 * t685 - 0.190D3 * t688 - 0.1728D4 * t696 + 0.256
     #D3 * t701 - 0.72D2 * t715 + 0.1024D4 * t719 + 0.30D2 * t723 + 0.56
     #D2 * t727 - 0.108D3 * t730 + 0.5266D4 * t738 + 0.16D2 * t41 - 0.59
     #2D3 * t743 + 0.128D3 * t893 * t109 - 0.64D2 * t893 * t36 - 0.64D2 
     #* t893 * t898
      t912 = t41 * t582
      t963 = 0.136D3 * t32 * t898 - 0.256D3 * t48 * t120 + 0.256D3 * t38
     #7 * t569 + 0.64D2 * t381 * t24 * t64 * t86 + 0.64D2 * t912 * t585 
     #+ 0.128D3 * t912 * t684 + 0.64D2 * t41 * t445 * t448 - 0.64D2 * t5
     #02 * t448 - 0.216D3 * t462 * t463 * t636 * t10 - 0.136D3 * t462 * 
     #t463 * t40 * t35 + 0.256D3 * t468 * t402 * t636 - 0.256D3 * t429 *
     # t248 * t636 * t20 - 0.64D2 * t912 * t584 * t258 - 0.64D2 * t912 *
     # t108 * x1 * t258 + 0.216D3 * t387 * t388 * t258 - 0.128D3 * t381 
     #* t388 * t235 * t20 + 0.128D3 * t502 * t33 * t10 * t162 * t55 - 0.
     #256D3 * t429 * t248 * t365 * x3 - 0.128D3 * t912 * t241 * t56
      rrqg2qgh11J2 = (wd * (t116 + t239 + t318 + t426 + t525 + t615 + t6
     #78 + t745) + wd * (t769 + t789 + t818 + t837 + t855 + t875 + t901 
     #+ t963)) / t27 / t40 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh11J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t5 * t4
      t7 = t2 * t6
      t8 = x2 ** 2
      t9 = x1 ** 2
      t10 = t9 * x1
      t11 = t8 * t10
      t12 = t7 * t11
      t13 = 0.1D1 - x1
      t14 = t13 ** 2
      t15 = t14 * t13
      t16 = x1 * t3
      t17 = z + t16
      t18 = t17 ** 2
      t20 = 0.1D1 / t18 / t17
      t21 = t15 * t20
      t22 = s * t3
      t23 = 0.1D1 / t17
      t24 = x1 * t23
      t25 = 0.1D1 - x2
      t26 = x3 * t25
      t28 = 0.1D1 - x3
      t31 = cos(x4 * 0.3141592653589793D1)
      t35 = Sqrt(t26 * t17 * x2 * t28)
      t37 = 0.2D1 * t31 * t35
      t38 = t26 * t17 + x2 * t28 - t37
      t39 = t24 * t38
      t41 = t13 * x3
      t43 = s - t22 * t39 - t22 * t41
      t44 = t43 * t38
      t45 = t44 * t28
      t47 = t12 * t21 * t45
      t49 = t9 ** 2
      t51 = t7 * t8 * t49
      t52 = t18 ** 2
      t53 = 0.1D1 / t52
      t54 = t14 * t53
      t58 = t28 * t25 * t17 + x2 * x3 + t37
      t59 = t58 ** 2
      t60 = t43 * t59
      t64 = t4 * t3
      t65 = t5 * t64
      t66 = t2 * t65
      t67 = t8 * x2
      t69 = t66 * t67 * t49
      t70 = t15 * t53
      t71 = t43 * t58
      t75 = t43 * t2
      t76 = t4 * t13
      t79 = t23 * t38
      t80 = x3 * x1 * t79
      t83 = t43 ** 2
      t84 = s * t1
      t85 = t83 * t84
      t86 = t4 * x1
      t87 = t85 * t86
      t88 = t13 * t28
      t89 = t79 * t88
      t90 = t87 * t89
      t93 = t75 * t86 * t89
      t95 = t85 * t76
      t96 = t95 * t80
      t100 = x1 * t13
      t101 = z ** 2
      t105 = 0.1728D4 * t75 * t4 * x2 * t100 * t23 * t101
      t106 = t2 * t5
      t107 = x2 * t9
      t109 = t14 * t23
      t110 = t43 * z
      t113 = t106 * t107 * t109 * t110 * x3
      t114 = 0.128D3 * t113
      t116 = t9 * t43
      t117 = t2 * t64 * t116
      t118 = x2 * t13
      t122 = 0.1728D4 * t117 * t118 * t23 * z
      t124 = t75 * t64 * x2
      t125 = t9 * t13
      t126 = 0.1D1 / t18
      t131 = 0.1728D4 * t124 * t125 * t126 * z * t38
      t132 = t84 * t4
      t133 = x2 * x1
      t135 = t13 * t23
      t139 = 0.1728D4 * t132 * t133 * t135 * t83 * z
      t140 = t5 * t3
      t141 = t2 * t140
      t142 = t141 * t107
      t143 = t15 * t23
      t144 = t43 * x3
      t147 = t142 * t143 * t144 * t28
      t149 = t2 * s
      t150 = t149 * t140
      t152 = x3 * t28
      t154 = t150 * t107 * t143 * t152
      t156 = x2 * t49
      t157 = t141 * t156
      t158 = t13 * t20
      t159 = t44 * t58
      t161 = t157 * t158 * t159
      t163 = x2 * t10
      t164 = t141 * t163
      t165 = t14 * t126
      t168 = t164 * t165 * t71 * x3
      t170 = t64 * x1
      t172 = t28 ** 2
      t174 = t79 * t14 * t172
      t175 = t75 * t170 * t174
      t178 = t14 ** 2
      t180 = t172 * t28
      t184 = 0.144D3 * t141 * t133 * t178 * t23 * t43 * t180
      t185 = t8 * t9
      t188 = t43 * t172
      t190 = t7 * t185 * t178 * t126 * t188
      t192 = -0.112D3 * t47 + 0.108D3 * t51 * t54 * t60 + 0.108D3 * t69 
     #* t70 * t71 - 0.56D2 * t75 * t76 * t80 + 0.128D3 * t90 - 0.112D3 *
     # t93 + 0.96D2 * t96 - t105 + t114 - t122 + t131 + t139 + 0.128D3 *
     # t147 + 0.1088D4 * t154 - 0.96D2 * t161 + 0.96D2 * t168 - 0.168D3 
     #* t175 + t184 + 0.216D3 * t190
      t193 = t84 * t5
      t194 = t193 * t133
      t197 = t194 * t143 * t83 * t172
      t199 = t13 * t53
      t200 = t38 ** 2
      t203 = t157 * t199 * t71 * t200
      t205 = t5 * x2
      t206 = t75 * t205
      t207 = t9 * t14
      t208 = t126 * t38
      t211 = t206 * t207 * t208 * t28
      t213 = t193 * t163
      t216 = t213 * t158 * t83 * t200
      t219 = t106 * t10 * t43
      t220 = t126 * t58
      t222 = t219 * t118 * t220
      t225 = t38 * t58
      t227 = t150 * t156 * t158 * t225
      t229 = t84 * t64
      t230 = t229 * t107
      t231 = t13 * t126
      t232 = t83 * t38
      t233 = t231 * t232
      t234 = t230 * t233
      t236 = t229 * t133
      t237 = t83 * x3
      t238 = t109 * t237
      t239 = t236 * t238
      t241 = t43 * t200
      t243 = t51 * t54 * t241
      t245 = t15 * t126
      t247 = t12 * t245 * t144
      t249 = t14 * t20
      t251 = t51 * t249 * t44
      t255 = t213 * t158 * t83 * t59
      t257 = t84 * t140
      t258 = t257 * t11
      t259 = t83 * t58
      t261 = t258 * t249 * t259
      t263 = t4 * t14
      t264 = x3 ** 2
      t265 = t263 * t264
      t268 = t85 * t265
      t270 = t263 * t172
      t271 = t85 * t270
      t273 = t75 * t270
      t275 = t64 * t15
      t278 = 0.144D3 * t75 * t275 * t180
      t279 = t3 * t13
      t280 = t279 * x3
      t281 = t85 * t280
      t283 = t264 * x3
      t287 = -0.56D2 * t197 + 0.36D2 * t203 - 0.128D3 * t211 - 0.192D3 *
     # t216 + 0.1244D4 * t222 + 0.1088D4 * t227 + 0.3466D4 * t234 - 0.12
     #44D4 * t239 + 0.36D2 * t243 - 0.148D3 * t247 + 0.148D3 * t251 - 0.
     #430D3 * t255 + 0.328D3 * t261 + 0.108D3 * t75 * t265 - 0.748D3 * t
     #268 - 0.312D3 * t271 + 0.216D3 * t273 + t278 + 0.16D2 * t281 + 0.3
     #6D2 * t75 * t275 * t283
      t291 = t279 * t28
      t294 = t85 * t291
      t303 = t150 * t163
      t306 = t303 * t165 * t58 * x3
      t311 = t106 * t163 * t231 * t110 * t38
      t313 = t10 * t13
      t314 = t20 * t59
      t326 = t85 * t64 * t9
      t327 = t326 * t89
      t329 = t126 * t200
      t330 = t329 * t88
      t331 = t117 * t330
      t333 = t106 * t116
      t334 = x2 * t14
      t335 = t23 * t28
      t337 = t333 * t334 * t335
      t341 = t164 * t165 * t144 * t38
      t343 = t213 * t233
      t345 = t193 * t107
      t346 = t345 * t238
      t350 = t303 * t165 * x3 * t38
      t352 = t41 * t79
      t353 = t117 * t352
      t355 = t231 * t259
      t356 = t230 * t355
      t359 = t157 * t158 * t60
      t361 = 0.108D3 * t75 * t280 + 0.144D3 * t75 * t291 - 0.240D3 * t29
     #4 + 0.864D3 * t124 * t125 * t220 + 0.864D3 * t206 * t207 * t220 * 
     #x3 + 0.1088D4 * t306 - 0.128D3 * t311 - 0.576D3 * t206 * t313 * t3
     #14 - 0.56D2 * t51 * t54 * t159 - 0.28D2 * t157 * t199 * t60 * t38 
     #+ 0.128D3 * t327 + 0.144D3 * t331 + 0.1632D4 * t337 - 0.94D2 * t34
     #1 + 0.275D3 * t343 - 0.17D2 * t346 - 0.848D3 * t350 + 0.530D3 * t3
     #53 + 0.3523D4 * t356 + 0.748D3 * t359
      t363 = t149 * t101 * t205
      t364 = t313 * t208
      t365 = t363 * t364
      t366 = 0.64D2 * t365
      t367 = t43 * t28
      t368 = t367 * t58
      t370 = t164 * t165 * t368
      t372 = t23 * t58
      t373 = t372 * t41
      t374 = t87 * t373
      t376 = t75 * t16
      t378 = t85 * t16
      t382 = t95 * t28 * x1 * t372
      t384 = t28 * t58
      t386 = t303 * t165 * t384
      t388 = t59 * t58
      t394 = t164 * t165 * t45
      t396 = t67 * t10
      t402 = t64 * t14
      t403 = t75 * t402
      t404 = t152 * t39
      t405 = t403 * t404
      t407 = t213 * t355
      t409 = t23 * x3
      t410 = t207 * t409
      t411 = t363 * t410
      t412 = 0.64D2 * t411
      t415 = t213 * t158 * t232 * t58
      t418 = t51 * t249 * t71
      t420 = t326 * t373
      t424 = t303 * t165 * t38 * t28
      t429 = t264 * x1
      t430 = t429 * t79
      t435 = t164 * t249 * t241 * t28
      t437 = t366 + 0.880D3 * t370 - 0.96D2 * t374 + 0.10D2 * t376 - 0.1
     #22D3 * t378 - 0.128D3 * t382 - 0.544D3 * t386 + 0.36D2 * t157 * t1
     #99 * t43 * t388 - 0.128D3 * t394 + 0.144D3 * t66 * t396 * t178 * t
     #20 * t367 - 0.112D3 * t405 + 0.306D3 * t407 - t412 - 0.530D3 * t41
     #5 - 0.16D2 * t418 - 0.128D3 * t420 + 0.1088D4 * t424 - 0.28D2 * t6
     #9 * t70 * t44 - 0.28D2 * t403 * t430 + 0.144D3 * t435
      t442 = t142 * t245 * t44 * t172
      t445 = t75 * t140 * t8
      t446 = t10 * t14
      t447 = t20 * t58
      t452 = t219 * t118 * t208
      t454 = t117 * t89
      t456 = x1 * t15
      t457 = t23 * t264
      t461 = t20 * t200
      t463 = t206 * t313 * t461
      t464 = 0.576D3 * t463
      t467 = t142 * t245 * t188 * t58
      t476 = t200 * t38
      t479 = t157 * t199 * t43 * t476
      t484 = t258 * t249 * t232
      t486 = x1 * t14
      t490 = t9 * t15
      t491 = t126 * x3
      t495 = t257 * t185
      t496 = t83 * t28
      t498 = t495 * t245 * t496
      t500 = t23 * t172
      t502 = t206 * t456 * t500
      t505 = t157 * t158 * t241
      t509 = t142 * t143 * t43 * t264
      t511 = t109 * t496
      t512 = t236 * t511
      t514 = -0.168D3 * t442 - 0.1152D4 * t445 * t446 * t447 - 0.3466D4 
     #* t452 + 0.400D3 * t454 - 0.576D3 * t206 * t456 * t457 - t464 + 0.
     #216D3 * t467 + 0.144D3 * t164 * t249 * t367 * t59 + 0.288D3 * t12 
     #* t21 * t368 - 0.28D2 * t479 + 0.12D2 * t85 + 0.36D2 * t75 + 0.182
     #D3 * t484 - 0.1152D4 * t124 * t486 * t409 + 0.864D3 * t445 * t490 
     #* t491 + 0.328D3 * t498 + 0.224D3 * t502 + 0.47D2 * t505 + 0.47D2 
     #* t509 - 0.1632D4 * t512
      t516 = t126 * t83
      t518 = t193 * t8 * t207 * t516
      t520 = t75 * t64
      t522 = t520 * t107 * t135
      t525 = t149 * t5 * x2
      t526 = t525 * t410
      t529 = t149 * t6 * t8
      t530 = t10 * t15
      t532 = t529 * t530 * t491
      t535 = t23 * t83
      t537 = t132 * x2 * t100 * t535
      t539 = t85 * t4
      t540 = t9 * t126
      t542 = t539 * t540 * t225
      t548 = t5 ** 2
      t550 = t8 ** 2
      t558 = t49 * t15
      t559 = t20 * t43
      t561 = t66 * t67 * t558 * t559
      t567 = t84 * t6 * t67 * t530 * t20 * t83
      t569 = t525 * t364
      t571 = t49 * t14
      t574 = t529 * t571 * t20 * t38
      t578 = t257 * t8 * t446 * t516
      t580 = t150 * x2
      t581 = t49 * t13
      t583 = t580 * t581 * t461
      t586 = t525 * t313 * t220
      t588 = t486 * t152
      t589 = t520 * t588
      t594 = t141 * t10 * t43 * t8 * t165
      t598 = t229 * x2 * t125 * t535
      t600 = t85 * z
      t602 = t600 * t16 * t372
      t603 = 0.128D3 * t602
      t605 = t600 * t16 * t79
      t607 = -0.3523D4 * t518 + 0.3523D4 * t522 + 0.752D3 * t526 - 0.108
     #8D4 * t532 + 0.1244D4 * t537 + 0.94D2 * t542 - 0.576D3 * t75 * t6 
     #* t396 * t21 + 0.36D2 * t2 * t548 * t550 * t49 * t178 * t53 * t43 
     #- 0.12D2 * t561 - 0.10D2 * t567 + 0.848D3 * t569 - 0.1088D4 * t574
     # - 0.122D3 * t578 - 0.472D3 * t583 - 0.1088D4 * t586 + 0.584D3 * t
     #589 - 0.1244D4 * t594 - 0.7896D4 * t598 - t603 + 0.128D3 * t605
      t609 = t75 * t5
      t613 = t75 * t4
      t618 = t529 * t571 * t447
      t621 = t580 * t581 * t314
      t624 = t525 * t207 * t335
      t628 = t529 * t530 * t126 * t28
      t631 = t580 * t490 * t457
      t634 = t580 * t490 * t500
      t638 = t345 * t165 * t232 * t28
      t641 = t12 * t245 * t367
      t643 = t345 * t511
      t646 = t142 * t143 * t188
      t649 = t333 * t334 * t409
      t651 = t64 * t13
      t653 = x3 * t9
      t654 = t653 * t329
      t655 = t75 * t651 * t654
      t661 = t141 * t163 * t14 * t559 * t384 * t38
      t665 = t345 * t165 * t496 * t58
      t667 = t486 * t264
      t668 = t520 * t667
      t670 = t9 * t23
      t671 = t670 * t38
      t672 = t539 * t671
      t674 = t613 * t671
      t676 = t85 * t3
      t677 = t24 * t58
      t678 = t676 * t677
      t680 = 0.864D3 * t609 * t185 * t165 - 0.576D3 * t613 * t133 * t135
     # + 0.544D3 * t618 - 0.272D3 * t621 - 0.1088D4 * t624 + 0.544D3 * t
     #628 - 0.376D3 * t631 - 0.272D3 * t634 - 0.400D3 * t638 + 0.240D3 *
     # t641 + 0.528D3 * t643 + 0.312D3 * t646 - 0.3523D4 * t649 + 0.36D2
     # * t655 - 0.112D3 * t661 - 0.584D3 * t665 + 0.430D3 * t668 + 0.275
     #D3 * t672 - 0.182D3 * t674 + 0.148D3 * t678
      t681 = t100 * x3
      t682 = t539 * t681
      t684 = t540 * t200
      t685 = t613 * t684
      t688 = t14 * x3 * t28
      t691 = t613 * t681
      t697 = t100 * t28
      t698 = t539 * t697
      t700 = t539 * t688
      t703 = t539 * t540 * t59
      t705 = t539 * t684
      t707 = t613 * t697
      t709 = t10 * t126
      t710 = t709 * t200
      t711 = t520 * t710
      t713 = t676 * t39
      t715 = t10 * t20
      t716 = t715 * t476
      t717 = t520 * t716
      t719 = t486 * t172
      t720 = t520 * t719
      t724 = t520 * t15 * t172 * x3
      t729 = t670 * t58
      t730 = t539 * t729
      t734 = t150 * t8 * t446 * t126
      t739 = t149 * t65 * t67 * t558 * t20
      t744 = t149 * t64 * x2 * t125 * t23
      t746 = 0.306D3 * t682 + 0.36D2 * t685 + 0.288D3 * t613 * t688 - 0.
     #328D3 * t691 + 0.144D3 * t520 * t15 * t28 * t264 + 0.528D3 * t698 
     #- 0.880D3 * t700 - 0.47D2 * t703 - 0.47D2 * t705 - 0.328D3 * t707 
     #+ 0.192D3 * t711 - 0.148D3 * t713 - 0.28D2 * t717 + 0.56D2 * t720 
     #+ 0.216D3 * t724 - 0.28D2 * t75 * t3 * t39 - 0.17D2 * t730 + 0.108
     #8D4 * t734 - 0.272D3 * t739 - 0.376D3 * t744
      t751 = t126 * t59
      t755 = t85 * t402
      t756 = t755 * t430
      t758 = t755 * t404
      t761 = t85 * t170 * t174
      t763 = t333 * t174
      t767 = t194 * t143 * t237 * t28
      t774 = t495 * t245 * t237
      t778 = t345 * t165 * t237 * t38
      t785 = t755 * t172 * x1 * t372
      t787 = t88 * t372
      t788 = t117 * t787
      t790 = t220 * t38
      t792 = t219 * t88 * t790
      t794 = t14 * t28
      t797 = t333 * t794 * t409 * t58
      t801 = t345 * t165 * t259 * x3
      t804 = t755 * t152 * t677
      t809 = 0.64D2 * t219 * t41 * t751 + 0.64D2 * t756 + 0.128D3 * t758
     # + 0.64D2 * t761 - 0.64D2 * t763 - 0.216D3 * t767 - 0.136D3 * t194
     # * t143 * t83 * t264 + 0.256D3 * t774 - 0.256D3 * t778 - 0.64D2 * 
     #t755 * t429 * t372 - 0.64D2 * t785 + 0.216D3 * t788 - 0.128D3 * t7
     #92 + 0.128D3 * t797 - 0.256D3 * t801 - 0.128D3 * t804 + 0.144D3 * 
     #t47 - 0.496D3 * t90 + 0.144D3 * t93
      t814 = t117 * t373
      t825 = -0.640D3 * t96 + t105 - 0.256D3 * t113 + t122 - t131 - t139
     # - 0.496D3 * t147 - 0.1728D4 * t154 + 0.256D3 * t814 + 0.640D3 * t
     #161 - 0.640D3 * t168 + 0.344D3 * t175 - t184 - 0.72D2 * t190 - 0.2
     #4D2 * t197 - 0.72D2 * t203 + 0.1024D4 * t211 + 0.30D2 * t216 - 0.3
     #776D4 * t222
      t845 = -0.1728D4 * t227 + 0.4940D4 * t234 + 0.3776D4 * t239 - 0.72
     #D2 * t243 + 0.216D3 * t247 - 0.216D3 * t251 + 0.542D3 * t255 - 0.6
     #04D3 * t261 + 0.592D3 * t268 + 0.264D3 * t271 - 0.72D2 * t273 - t2
     #78 - 0.608D3 * t281 - 0.64D2 * t294 - 0.1728D4 * t306 + 0.256D3 * 
     #t311 - 0.256D3 * t331 - 0.4816D4 * t337 + 0.108D3 * t341
      t865 = -0.156D3 * t343 + 0.490D3 * t346 + 0.1360D4 * t350 - 0.444D
     #3 * t353 + 0.5266D4 * t356 - 0.592D3 * t359 - 0.128D3 * t365 - 0.6
     #40D3 * t370 + 0.640D3 * t374 - 0.62D2 * t376 + 0.134D3 * t378 + 0.
     #496D3 * t382 + 0.864D3 * t386 + 0.496D3 * t394 + 0.144D3 * t405 - 
     #0.190D3 * t407 + 0.128D3 * t411 + 0.444D3 * t415 + 0.608D3 * t418
      t887 = -0.1728D4 * t424 - 0.256D3 * t435 + 0.344D3 * t442 - 0.4940
     #D4 * t452 - 0.288D3 * t454 + 0.352D3 * t463 - 0.72D2 * t467 + 0.56
     #D2 * t479 + 0.16D2 * t85 - 0.108D3 * t484 - 0.256D3 * t498 - 0.137
     #6D4 * t502 - 0.54D2 * t505 - 0.54D2 * t509 + 0.4816D4 * t512 - 0.5
     #266D4 * t518 + 0.5266D4 * t522 - 0.1168D4 * t526 + 0.1728D4 * t532
      t907 = -0.3776D4 * t537 - 0.108D3 * t542 - 0.16D2 * t561 + 0.62D2 
     #* t567 - 0.1360D4 * t569 + 0.1728D4 * t574 + 0.134D3 * t578 + 0.77
     #6D3 * t583 + 0.1728D4 * t586 - 0.544D3 * t589 + 0.3776D4 * t594 - 
     #0.9344D4 * t598 + 0.256D3 * t602 - 0.256D3 * t605 - 0.864D3 * t618
     # + 0.432D3 * t621 + 0.1728D4 * t624 - 0.864D3 * t628 + 0.584D3 * t
     #631
      t910 = t709 * t225
      t911 = t520 * t910
      t913 = t49 * t20
      t914 = t59 * t38
      t916 = t609 * t913 * t914
      t920 = t609 * t456 * t172 * x3
      t937 = 0.432D3 * t634 + 0.256D3 * t911 - 0.64D2 * t916 + 0.64D2 * 
     #t920 + 0.288D3 * t638 + 0.64D2 * t641 - 0.752D3 * t643 - 0.264D3 *
     # t646 - 0.5266D4 * t649 - 0.72D2 * t655 + 0.144D3 * t661 + 0.544D3
     # * t665 - 0.542D3 * t668 - 0.156D3 * t672 + 0.108D3 * t674 - 0.216
     #D3 * t678 - 0.190D3 * t682 - 0.72D2 * t685 + 0.604D3 * t691
      t952 = t85 * t64
      t954 = 0.128D3 * t952 * t719
      t955 = t952 * t667
      t957 = t709 * t59
      t958 = t952 * t957
      t962 = t613 * t729
      t964 = -0.752D3 * t698 + 0.640D3 * t700 + 0.54D2 * t703 + 0.54D2 *
     # t705 + 0.256D3 * t707 - 0.30D2 * t711 + 0.216D3 * t713 + 0.56D2 *
     # t717 + 0.24D2 * t720 - 0.72D2 * t724 + 0.490D3 * t730 - 0.1728D4 
     #* t734 + 0.432D3 * t739 + 0.584D3 * t744 + t954 - 0.64D2 * t955 - 
     #0.64D2 * t958 + 0.136D3 * t520 * t957 - 0.256D3 * t962
      t996 = t14 * t264
      t1000 = -0.128D3 * t756 + 0.64D2 * t758 + 0.128D3 * t761 - 0.128D3
     # * t763 - 0.80D2 * t767 + 0.25296D5 * t774 - 0.25248D5 * t778 - 0.
     #128D3 * t785 + 0.80D2 * t788 - 0.64D2 * t792 + 0.64D2 * t797 - 0.2
     #5264D5 * t801 - 0.64D2 * t804 + 0.8D1 * t90 - 0.672D3 * t952 * t71
     #0 + 0.16D2 * t609 * t456 * t283 + 0.112D3 * t609 * t913 * t476 - 0
     #.112D3 * t952 * t716 - 0.16D2 * t952 * t715 * t388 + 0.32D2 * t333
     # * t996 * t372
      t1001 = t85 * t651
      t1005 = t28 * t9
      t1030 = t208 * t41 * t58
      t1053 = -0.32D2 * t1001 * t653 * t751 - 0.96D2 * t1001 * t1005 * t
     #751 - 0.384D3 * t87 * t79 * z * t13 * x3 + 0.384D3 * t75 * t64 * t
     #10 * t208 * z * t58 - 0.192D3 * t333 * t794 * t409 * t38 - 0.416D3
     # * t326 * t352 + 0.48D2 * t219 * t41 * t329 + 0.64D2 * t326 * t103
     #0 - 0.48D2 * t333 * t996 * t79 - 0.288D3 * t1001 * t654 - 0.96D2 *
     # t326 * t330 + 0.256D3 * t96 + t114 + 0.8D1 * t147 + 0.640D3 * t15
     #4 + 0.96D2 * t219 * t330 - 0.64D2 * t219 * t1030 + 0.192D3 * t1001
     # * t1005 * t790 + 0.32D2 * t326 * t787 + 0.25264D5 * t814
      t1055 = t200 * z
      t1066 = t38 * t101
      t1086 = t58 * t200
      t1106 = -0.384D3 * t539 * t540 * t1055 + 0.384D3 * t520 * t709 * t
     #1055 - 0.768D3 * t539 * t9 * z * t79 + 0.384D3 * t613 * t670 * t10
     #66 + 0.128D3 * t2 * t3 * x1 * t79 * t101 * z * t43 - 0.384D3 * t67
     #6 * t24 * t1066 + 0.96D2 * t609 * t456 * t28 * t264 + 0.32D2 * t95
     #2 * t588 - 0.48D2 * t952 * t715 * t1086 + 0.288D3 * t609 * t913 * 
     #t1086 - 0.416D3 * t952 * t910 + 0.48D2 * t952 * t715 * t914 - 0.25
     #6D3 * t161 - 0.144D3 * t175 - 0.32D2 * t197 - 0.1152D4 * t211 + 0.
     #28792D5 * t216 + 0.640D3 * t222 + 0.640D3 * t227 - 0.424D3 * t234
      t1127 = -0.640D3 * t239 + 0.128D3 * t251 - 0.28208D5 * t255 + 0.27
     #920D5 * t261 - 0.64D2 * t268 - 0.64D2 * t271 + 0.128D3 * t281 + 0.
     #128D3 * t294 + 0.640D3 * t306 - 0.512D3 * t311 - 0.192D3 * t327 + 
     #0.288D3 * t331 + 0.912D3 * t337 - 0.8D1 * t341 - 0.28372D5 * t343 
     #+ 0.25220D5 * t346 - 0.512D3 * t350 - 0.536D3 * t353 - 0.780D3 * t
     #356 + 0.64D2 * t359
      t1147 = t366 + 0.128D3 * t370 - 0.304D3 * t376 + 0.304D3 * t378 - 
     #0.8D1 * t382 - 0.320D3 * t386 - 0.8D1 * t394 + 0.28224D5 * t407 - 
     #t412 + 0.536D3 * t415 - 0.128D3 * t418 - 0.64D2 * t420 + 0.640D3 *
     # t424 + 0.288D3 * t435 - 0.144D3 * t442 + 0.424D3 * t452 + 0.74288
     #D5 * t454 + t464 - 0.144D3 * t479 - 0.64D2 * t85
      t1168 = -0.29128D5 * t484 + 0.74320D5 * t498 + 0.576D3 * t502 - 0.
     #252D3 * t505 + 0.4D1 * t509 - 0.912D3 * t512 + 0.780D3 * t518 - 0.
     #780D3 * t522 + 0.416D3 * t526 - 0.640D3 * t532 + 0.640D3 * t537 + 
     #0.8D1 * t542 + 0.64D2 * t561 + 0.304D3 * t567 + 0.512D3 * t569 - 0
     #.640D3 * t574 + 0.304D3 * t578 - 0.304D3 * t583 - 0.640D3 * t586 +
     # 0.74208D5 * t589
      t1189 = -0.640D3 * t594 + 0.1728D4 * t598 - t603 + 0.512D3 * t605 
     #+ 0.320D3 * t618 - 0.160D3 * t621 - 0.640D3 * t624 + 0.320D3 * t62
     #8 - 0.208D3 * t631 - 0.160D3 * t634 + 0.25248D5 * t911 + 0.128D3 *
     # t916 + 0.128D3 * t920 - 0.74288D5 * t638 - 0.128D3 * t641 + 0.742
     #96D5 * t643 + 0.64D2 * t646 + 0.780D3 * t649 - 0.74208D5 * t665 + 
     #0.28208D5 * t668
      t1211 = -0.128D3 * t713 - 0.144D3 * t717 + 0.32D2 * t720 + 0.25220
     #D5 * t730 + 0.640D3 * t734 - 0.160D3 * t739 - 0.208D3 * t744 + t95
     #4 + 0.16D2 * t955 + 0.16D2 * t958 - 0.25296D5 * t962
      rrqg2qgh11J3 = (wd * (t192 + t287 + t361 + t437 + t514 + t607 + t6
     #80 + t746) + wd * (t809 + t825 + t845 + t865 + t887 + t907 + t937 
     #+ t964) + wd * (t1000 + t1053 + t1106 + t1127 + t1147 + t1168 + t1
     #189 - 0.28372D5 * t672 + 0.29128D5 * t674 + 0.28224D5 * t682 - 0.2
     #7920D5 * t691 + 0.74296D5 * t698 - 0.128D3 * t700 - 0.4D1 * t703 +
     # 0.252D3 * t705 - 0.74320D5 * t707 - 0.28792D5 * t711 + t1211)) / 
     #t1 / t83 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh11J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t2 * t5
      t7 = x1 ** 2
      t8 = t7 * x1
      t9 = t8 * x2
      t11 = 0.1D1 - x1
      t12 = x1 * t3
      t13 = z + t12
      t14 = t13 ** 2
      t15 = 0.1D1 / t14
      t16 = t11 * t15
      t17 = s * t3
      t18 = 0.1D1 / t13
      t19 = x1 * t18
      t20 = 0.1D1 - x2
      t21 = x3 * t20
      t23 = 0.1D1 - x3
      t26 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(t21 * t13 * x2 * t23)
      t32 = 0.2D1 * t26 * t30
      t33 = t21 * t13 + x2 * t23 - t32
      t34 = t19 * t33
      t36 = t11 * x3
      t38 = s - t17 * t34 - t17 * t36
      t39 = t38 * z
      t42 = t6 * t9 * t16 * t39 * t33
      t44 = t38 * t2
      t45 = t5 * x2
      t46 = t44 * t45
      t47 = t8 * t11
      t49 = 0.1D1 / t14 / t13
      t53 = t23 * t20 * t13 + x2 * x3 + t32
      t54 = t53 ** 2
      t55 = t49 * t54
      t59 = t5 * t4
      t60 = t2 * t59
      t61 = x2 ** 2
      t62 = t7 ** 2
      t64 = t60 * t61 * t62
      t65 = t11 ** 2
      t66 = t14 ** 2
      t67 = 0.1D1 / t66
      t68 = t65 * t67
      t69 = t38 * t33
      t70 = t69 * t53
      t74 = t38 ** 2
      t75 = s * t1
      t76 = t74 * t75
      t77 = t4 * x1
      t78 = t76 * t77
      t79 = t18 * t53
      t80 = t79 * t36
      t81 = t78 * t80
      t84 = t6 * t8 * t38
      t85 = x2 * t11
      t86 = t15 * t53
      t88 = t84 * t85 * t86
      t90 = t2 * s
      t91 = t5 * t3
      t92 = t90 * t91
      t93 = x2 * t62
      t95 = t11 * t49
      t96 = t33 * t53
      t98 = t92 * t93 * t95 * t96
      t100 = t4 * t3
      t101 = t75 * t100
      t102 = x2 * t7
      t103 = t101 * t102
      t104 = t74 * t33
      t105 = t16 * t104
      t106 = t103 * t105
      t108 = t2 * t91
      t109 = t108 * t93
      t110 = t33 ** 2
      t111 = t38 * t110
      t113 = t109 * t95 * t111
      t115 = t108 * t102
      t116 = t65 * t11
      t117 = t116 * t18
      t118 = x3 ** 2
      t121 = t115 * t117 * t38 * t118
      t123 = x2 * x1
      t124 = t101 * t123
      t125 = t65 * t18
      t126 = t74 * t23
      t127 = t125 * t126
      t128 = t124 * t127
      t130 = t75 * t5
      t131 = t130 * t102
      t132 = t65 * t15
      t135 = t131 * t132 * t104 * t23
      t137 = t108 * t9
      t138 = t38 * t53
      t141 = t137 * t132 * t138 * x3
      t143 = t100 * x1
      t145 = t18 * t33
      t146 = t23 ** 2
      t148 = t145 * t65 * t146
      t149 = t44 * t143 * t148
      t152 = t65 ** 2
      t154 = t146 * t23
      t158 = 0.144D3 * t108 * t123 * t152 * t18 * t38 * t154
      t159 = t61 * t7
      t162 = t38 * t146
      t164 = t60 * t159 * t152 * t15 * t162
      t166 = t130 * t123
      t169 = t166 * t117 * t74 * t146
      t171 = t75 * t91
      t172 = t171 * t159
      t173 = t116 * t15
      t175 = t172 * t173 * t126
      t177 = x1 * t116
      t178 = t18 * t146
      t180 = t46 * t177 * t178
      t182 = t100 * t7
      t183 = t44 * t182
      t184 = t11 * t23
      t185 = t145 * t184
      t186 = t183 * t185
      t188 = -0.128D3 * t42 - 0.576D3 * t46 * t47 * t55 - 0.56D2 * t64 *
     # t68 * t70 - 0.96D2 * t81 + 0.1244D4 * t88 + 0.1088D4 * t98 + 0.34
     #66D4 * t106 + 0.47D2 * t113 + 0.47D2 * t121 - 0.1632D4 * t128 - 0.
     #400D3 * t135 + 0.96D2 * t141 - 0.168D3 * t149 + t158 + 0.216D3 * t
     #164 - 0.56D2 * t169 + 0.328D3 * t175 + 0.224D3 * t180 + 0.400D3 * 
     #t186
      t189 = t18 * t118
      t193 = t74 * x3
      t194 = t125 * t193
      t195 = t124 * t194
      t198 = t64 * t68 * t111
      t200 = t61 * t8
      t201 = t60 * t200
      t202 = t38 * x3
      t204 = t201 * t173 * t202
      t207 = t44 * t91 * t61
      t208 = t7 * t116
      t209 = t15 * x3
      t213 = t74 * t53
      t214 = t16 * t213
      t215 = t103 * t214
      t217 = t38 * t54
      t219 = t109 * t95 * t217
      t221 = t65 * t49
      t222 = t23 * t38
      t227 = t116 * t49
      t228 = t222 * t53
      t234 = t49 * t38
      t235 = t23 * t53
      t238 = t108 * t9 * t65 * t234 * t235 * t33
      t240 = t11 * t67
      t243 = t109 * t240 * t138 * t110
      t245 = t7 * t65
      t246 = t15 * t33
      t249 = t46 * t245 * t246 * t23
      t254 = t64 * t221 * t69
      t256 = t69 * t23
      t258 = t137 * t132 * t256
      t260 = t5 * t100
      t261 = t2 * t260
      t262 = t61 * x2
      t263 = t262 * t8
      t269 = t100 * t65
      t270 = t44 * t269
      t271 = t23 * x3
      t272 = t271 * t34
      t273 = t270 * t272
      t277 = t137 * t221 * t111 * t23
      t279 = t4 * t65
      t280 = t279 * t118
      t283 = -0.576D3 * t46 * t177 * t189 - 0.1244D4 * t195 + 0.36D2 * t
     #198 - 0.148D3 * t204 + 0.864D3 * t207 * t208 * t209 + 0.3523D4 * t
     #215 + 0.748D3 * t219 + 0.144D3 * t137 * t221 * t222 * t54 + 0.288D
     #3 * t201 * t227 * t228 - 0.112D3 * t238 + 0.36D2 * t243 - 0.128D3 
     #* t249 + 0.12D2 * t76 + 0.36D2 * t44 + 0.148D3 * t254 - 0.128D3 * 
     #t258 + 0.144D3 * t261 * t263 * t152 * t49 * t222 - 0.112D3 * t273 
     #+ 0.144D3 * t277 + 0.108D3 * t44 * t280
      t285 = t76 * t280
      t287 = t279 * t146
      t288 = t76 * t287
      t290 = t44 * t287
      t292 = t100 * t116
      t295 = 0.144D3 * t44 * t292 * t154
      t296 = t3 * t11
      t297 = t296 * x3
      t298 = t76 * t297
      t300 = t118 * x3
      t306 = t296 * t23
      t309 = t76 * t306
      t312 = t64 * t221 * t138
      t314 = t76 * t182
      t315 = t314 * t80
      t317 = t92 * t9
      t320 = t317 * t132 * t33 * t23
      t323 = t261 * t262 * t62
      t324 = t116 * t67
      t328 = t118 * x1
      t329 = t328 * t145
      t334 = t115 * t117 * t202 * t23
      t338 = t92 * t102 * t117 * t271
      t340 = t130 * t9
      t343 = t340 * t95 * t74 * t110
      t345 = t110 * t33
      t348 = t109 * t240 * t38 * t345
      t350 = t171 * t200
      t352 = t350 * t221 * t104
      t355 = t44 * t100 * x2
      t356 = x1 * t65
      t357 = t18 * x3
      t361 = -0.748D3 * t285 - 0.312D3 * t288 + 0.216D3 * t290 + t295 + 
     #0.16D2 * t298 + 0.36D2 * t44 * t292 * t300 + 0.108D3 * t44 * t297 
     #+ 0.144D3 * t44 * t306 - 0.240D3 * t309 - 0.16D2 * t312 - 0.128D3 
     #* t315 + 0.1088D4 * t320 - 0.28D2 * t323 * t324 * t69 - 0.28D2 * t
     #270 * t329 + 0.128D3 * t334 + 0.1088D4 * t338 - 0.192D3 * t343 - 0
     #.28D2 * t348 + 0.182D3 * t352 - 0.1152D4 * t355 * t356 * t357
      t364 = t131 * t132 * t126 * t53
      t367 = t109 * t95 * t70
      t371 = t115 * t173 * t162 * t53
      t373 = t44 * t100
      t374 = t356 * t118
      t375 = t373 * t374
      t377 = t76 * t4
      t378 = t7 * t18
      t379 = t378 * t33
      t380 = t377 * t379
      t382 = t44 * t4
      t383 = t382 * t379
      t385 = t76 * t3
      t386 = t19 * t53
      t387 = t385 * t386
      t389 = x1 * t11
      t390 = t389 * x3
      t391 = t377 * t390
      t393 = t7 * t15
      t394 = t393 * t110
      t395 = t382 * t394
      t398 = t65 * x3 * t23
      t401 = t382 * t390
      t407 = t389 * t23
      t408 = t377 * t407
      t410 = t377 * t398
      t413 = t377 * t393 * t54
      t415 = t377 * t394
      t417 = t382 * t407
      t419 = t8 * t15
      t420 = t419 * t110
      t421 = t373 * t420
      t423 = t385 * t34
      t425 = t8 * t49
      t426 = t425 * t345
      t427 = t373 * t426
      t429 = -0.584D3 * t364 - 0.96D2 * t367 + 0.216D3 * t371 + 0.430D3 
     #* t375 + 0.275D3 * t380 - 0.182D3 * t383 + 0.148D3 * t387 + 0.306D
     #3 * t391 + 0.36D2 * t395 + 0.288D3 * t382 * t398 - 0.328D3 * t401 
     #+ 0.144D3 * t373 * t116 * t23 * t118 + 0.528D3 * t408 - 0.880D3 * 
     #t410 - 0.47D2 * t413 - 0.47D2 * t415 - 0.328D3 * t417 + 0.192D3 * 
     #t421 - 0.148D3 * t423 - 0.28D2 * t427
      t432 = t356 * t146
      t433 = t373 * t432
      t437 = t373 * t116 * t146 * x3
      t442 = t378 * t53
      t443 = t377 * t442
      t446 = t8 * t65
      t448 = t92 * t61 * t446 * t15
      t452 = t62 * t116
      t454 = t90 * t260 * t262 * t452 * t49
      t458 = t7 * t11
      t460 = t90 * t100 * x2 * t458 * t18
      t463 = t350 * t221 * t213
      t468 = t49 * t53
      t473 = t84 * t85 * t246
      t475 = t4 * t11
      t476 = t76 * t475
      t479 = t476 * t23 * x1 * t79
      t482 = t317 * t132 * t235
      t484 = t54 * t53
      t491 = t340 * t95 * t74 * t54
      t493 = t340 * t214
      t501 = t317 * t132 * t53 * x3
      t503 = t44 * t12
      t505 = t76 * t12
      t507 = 0.56D2 * t433 + 0.216D3 * t437 - 0.28D2 * t44 * t3 * t34 - 
     #0.17D2 * t443 + 0.1088D4 * t448 - 0.272D3 * t454 - 0.376D3 * t460 
     #+ 0.328D3 * t463 + 0.864D3 * t355 * t458 * t86 - 0.1152D4 * t207 *
     # t446 * t468 - 0.3466D4 * t473 - 0.128D3 * t479 - 0.544D3 * t482 +
     # 0.36D2 * t109 * t240 * t38 * t484 - 0.430D3 * t491 + 0.306D3 * t4
     #93 + 0.864D3 * t46 * t245 * t86 * x3 + 0.1088D4 * t501 + 0.10D2 * 
     #t503 - 0.122D3 * t505
      t509 = t15 * t74
      t511 = t130 * t61 * t245 * t509
      t513 = t11 * t18
      t515 = t373 * t102 * t513
      t518 = t90 * t5 * x2
      t519 = t245 * t357
      t520 = t518 * t519
      t523 = t90 * t59 * t61
      t524 = t8 * t116
      t526 = t523 * t524 * t209
      t528 = t75 * t4
      t530 = t18 * t74
      t532 = t528 * x2 * t389 * t530
      t535 = t377 * t393 * t96
      t541 = t5 ** 2
      t543 = t61 ** 2
      t552 = t261 * t262 * t452 * t234
      t558 = t75 * t59 * t262 * t524 * t49 * t74
      t560 = t47 * t246
      t561 = t518 * t560
      t563 = t62 * t65
      t566 = t523 * t563 * t49 * t33
      t570 = t171 * t61 * t446 * t509
      t572 = t92 * x2
      t573 = t62 * t11
      t574 = t49 * t110
      t576 = t572 * t573 * t574
      t579 = t518 * t47 * t86
      t581 = t356 * t271
      t582 = t373 * t581
      t587 = t108 * t8 * t38 * t61 * t132
      t591 = t101 * x2 * t458 * t530
      t593 = t76 * z
      t595 = t593 * t12 * t79
      t596 = 0.128D3 * t595
      t598 = t593 * t12 * t145
      t600 = -0.3523D4 * t511 + 0.3523D4 * t515 + 0.752D3 * t520 - 0.108
     #8D4 * t526 + 0.1244D4 * t532 + 0.94D2 * t535 - 0.576D3 * t44 * t59
     # * t263 * t227 + 0.36D2 * t2 * t541 * t543 * t62 * t152 * t67 * t3
     #8 - 0.12D2 * t552 - 0.10D2 * t558 + 0.848D3 * t561 - 0.1088D4 * t5
     #66 - 0.122D3 * t570 - 0.472D3 * t576 - 0.1088D4 * t579 + 0.584D3 *
     # t582 - 0.1244D4 * t587 - 0.7896D4 * t591 - t596 + 0.128D3 * t598
      t602 = t44 * t5
      t610 = t523 * t563 * t468
      t613 = t572 * t573 * t55
      t615 = t18 * t23
      t617 = t518 * t245 * t615
      t621 = t523 * t524 * t15 * t23
      t624 = t572 * t208 * t189
      t627 = t572 * t208 * t178
      t630 = t46 * t47 * t574
      t631 = 0.576D3 * t630
      t633 = t201 * t173 * t222
      t635 = t131 * t127
      t638 = t115 * t117 * t162
      t641 = t6 * t7 * t38
      t642 = x2 * t65
      t644 = t641 * t642 * t357
      t646 = t36 * t145
      t647 = t183 * t646
      t649 = z ** 2
      t651 = t90 * t649 * t45
      t652 = t651 * t560
      t653 = 0.64D2 * t652
      t655 = t137 * t132 * t228
      t660 = t6 * t102 * t125 * t39 * x3
      t661 = 0.128D3 * t660
      t665 = 0.1728D4 * t183 * t85 * t18 * z
      t670 = 0.1728D4 * t355 * t458 * t15 * z * t33
      t675 = 0.1728D4 * t528 * t123 * t513 * t74 * z
      t676 = 0.864D3 * t602 * t159 * t132 - 0.576D3 * t382 * t123 * t513
     # + 0.544D3 * t610 - 0.272D3 * t613 - 0.1088D4 * t617 + 0.544D3 * t
     #621 - 0.376D3 * t624 - 0.272D3 * t627 - t631 + 0.240D3 * t633 + 0.
     #528D3 * t635 + 0.312D3 * t638 - 0.3523D4 * t644 + 0.530D3 * t647 +
     # t653 + 0.880D3 * t655 + t661 - t665 + t670 + t675
      t677 = t100 * t11
      t679 = x3 * t7
      t680 = t15 * t110
      t681 = t679 * t680
      t682 = t44 * t677 * t681
      t685 = x3 * x1 * t145
      t686 = t476 * t685
      t693 = 0.1728D4 * t44 * t4 * x2 * t389 * t18 * t649
      t696 = t137 * t132 * t202 * t33
      t698 = t340 * t105
      t700 = t131 * t194
      t704 = t317 * t132 * x3 * t33
      t706 = t314 * t185
      t708 = t680 * t184
      t709 = t183 * t708
      t712 = t641 * t642 * t615
      t714 = t651 * t519
      t715 = 0.64D2 * t714
      t718 = t340 * t95 * t104 * t53
      t730 = t78 * t185
      t733 = t44 * t77 * t185
      t737 = t115 * t173 * t69 * t146
      t740 = t201 * t227 * t256
      t745 = 0.36D2 * t682 + 0.96D2 * t686 - t693 - 0.94D2 * t696 + 0.27
     #5D3 * t698 - 0.17D2 * t700 - 0.848D3 * t704 + 0.128D3 * t706 + 0.1
     #44D3 * t709 + 0.1632D4 * t712 - t715 - 0.530D3 * t718 - 0.28D2 * t
     #109 * t240 * t217 * t33 + 0.108D3 * t323 * t324 * t138 - 0.56D2 * 
     #t44 * t475 * t685 + 0.128D3 * t730 - 0.112D3 * t733 - 0.168D3 * t7
     #37 - 0.112D3 * t740 + 0.108D3 * t64 * t68 * t217
      t750 = t76 * t269
      t751 = t750 * t329
      t753 = t750 * t272
      t765 = t172 * t173 * t193
      t769 = t131 * t132 * t193 * t33
      t776 = t750 * t146 * x1 * t79
      t781 = 0.64D2 * t751 + 0.128D3 * t753 + 0.256D3 * t42 + 0.640D3 * 
     #t81 - 0.3776D4 * t88 - 0.1728D4 * t98 + 0.4940D4 * t106 - 0.54D2 *
     # t113 - 0.54D2 * t121 + 0.4816D4 * t128 + 0.288D3 * t135 + 0.256D3
     # * t765 - 0.256D3 * t769 - 0.64D2 * t750 * t328 * t79 - 0.64D2 * t
     #776 - 0.640D3 * t141 + 0.344D3 * t149 - t158 - 0.72D2 * t164
      t786 = t65 * t23
      t789 = t641 * t786 * t357 * t53
      t793 = t131 * t132 * t213 * x3
      t796 = t750 * t271 * t386
      t798 = t183 * t80
      t800 = t15 * t54
      t805 = t76 * t143 * t148
      t807 = t641 * t148
      t811 = t166 * t117 * t193 * t23
      t823 = -0.24D2 * t169 - 0.256D3 * t175 - 0.1376D4 * t180 - 0.288D3
     # * t186 + 0.128D3 * t789 - 0.256D3 * t793 - 0.128D3 * t796 + 0.256
     #D3 * t798 + 0.64D2 * t84 * t36 * t800 + 0.64D2 * t805 - 0.64D2 * t
     #807 - 0.216D3 * t811 - 0.136D3 * t166 * t117 * t74 * t118 + 0.3776
     #D4 * t195 - 0.72D2 * t198 + 0.216D3 * t204 + 0.5266D4 * t215 - 0.5
     #92D3 * t219 + 0.144D3 * t238
      t843 = -0.72D2 * t243 + 0.1024D4 * t249 + 0.16D2 * t76 - 0.216D3 *
     # t254 + 0.496D3 * t258 + 0.144D3 * t273 - 0.256D3 * t277 + 0.592D3
     # * t285 + 0.264D3 * t288 - 0.72D2 * t290 - t295 - 0.608D3 * t298 -
     # 0.64D2 * t309 + 0.608D3 * t312 - 0.1728D4 * t320 - 0.496D3 * t334
     # - 0.1728D4 * t338 + 0.30D2 * t343 + 0.56D2 * t348
      t863 = -0.108D3 * t352 + 0.544D3 * t364 + 0.640D3 * t367 - 0.72D2 
     #* t371 - 0.542D3 * t375 - 0.156D3 * t380 + 0.108D3 * t383 - 0.216D
     #3 * t387 - 0.190D3 * t391 - 0.72D2 * t395 + 0.604D3 * t401 - 0.752
     #D3 * t408 + 0.640D3 * t410 + 0.54D2 * t413 + 0.54D2 * t415 + 0.256
     #D3 * t417 - 0.30D2 * t421 + 0.216D3 * t423 + 0.56D2 * t427
      t872 = t76 * t100
      t874 = 0.128D3 * t872 * t432
      t875 = t872 * t374
      t877 = t419 * t54
      t878 = t872 * t877
      t882 = t382 * t442
      t892 = 0.24D2 * t433 - 0.72D2 * t437 + 0.490D3 * t443 - 0.1728D4 *
     # t448 + 0.432D3 * t454 + 0.584D3 * t460 + t874 - 0.64D2 * t875 - 0
     #.64D2 * t878 + 0.136D3 * t373 * t877 - 0.256D3 * t882 - 0.604D3 * 
     #t463 - 0.4940D4 * t473 + 0.496D3 * t479 + 0.864D3 * t482 + 0.542D3
     # * t491 - 0.190D3 * t493 - 0.1728D4 * t501 - 0.62D2 * t503
      t912 = 0.134D3 * t505 - 0.5266D4 * t511 + 0.5266D4 * t515 - 0.1168
     #D4 * t520 + 0.1728D4 * t526 - 0.3776D4 * t532 - 0.108D3 * t535 - 0
     #.16D2 * t552 + 0.62D2 * t558 - 0.1360D4 * t561 + 0.1728D4 * t566 +
     # 0.134D3 * t570 + 0.776D3 * t576 + 0.1728D4 * t579 - 0.544D3 * t58
     #2 + 0.3776D4 * t587 - 0.9344D4 * t591 + 0.256D3 * t595 - 0.256D3 *
     # t598
      t920 = t419 * t96
      t921 = t373 * t920
      t923 = t62 * t49
      t924 = t54 * t33
      t926 = t602 * t923 * t924
      t930 = t602 * t177 * t146 * x3
      t941 = -0.864D3 * t610 + 0.432D3 * t613 + 0.1728D4 * t617 - 0.864D
     #3 * t621 + 0.584D3 * t624 + 0.432D3 * t627 + 0.256D3 * t921 - 0.64
     #D2 * t926 + 0.64D2 * t930 + 0.352D3 * t630 + 0.64D2 * t633 - 0.752
     #D3 * t635 - 0.264D3 * t638 - 0.5266D4 * t644 - 0.444D3 * t647 - 0.
     #128D3 * t652 - 0.640D3 * t655 - 0.256D3 * t660 + t665
      t942 = t184 * t79
      t943 = t183 * t942
      t945 = t86 * t33
      t947 = t84 * t184 * t945
      t963 = -t670 - t675 + 0.216D3 * t943 - 0.128D3 * t947 - 0.72D2 * t
     #682 - 0.640D3 * t686 + t693 + 0.108D3 * t696 - 0.156D3 * t698 + 0.
     #490D3 * t700 + 0.1360D4 * t704 - 0.256D3 * t709 - 0.4816D4 * t712 
     #+ 0.128D3 * t714 + 0.444D3 * t718 - 0.496D3 * t730 + 0.144D3 * t73
     #3 + 0.344D3 * t737 + 0.144D3 * t740
      t999 = -0.128D3 * t751 + 0.64D2 * t753 - 0.512D3 * t42 + 0.640D3 *
     # t88 + 0.640D3 * t98 - 0.424D3 * t106 - 0.252D3 * t113 + 0.4D1 * t
     #121 - 0.912D3 * t128 - 0.74288D5 * t135 + 0.25296D5 * t765 - 0.252
     #48D5 * t769 - 0.128D3 * t776 - 0.144D3 * t149 + 0.384D3 * t44 * t1
     #00 * t8 * t246 * z * t53 - 0.192D3 * t641 * t786 * t357 * t33 - 0.
     #416D3 * t314 * t646 + 0.48D2 * t84 * t36 * t680 - 0.32D2 * t169 + 
     #0.74320D5 * t175
      t1014 = t246 * t36 * t53
      t1017 = t76 * t677
      t1018 = t23 * t7
      t1025 = t65 * t118
      t1040 = 0.576D3 * t180 + 0.74288D5 * t186 + 0.64D2 * t789 - 0.2526
     #4D5 * t793 - 0.64D2 * t796 + 0.25264D5 * t798 + 0.128D3 * t805 - 0
     #.128D3 * t807 - 0.80D2 * t811 - 0.96D2 * t314 * t708 + 0.96D2 * t8
     #4 * t708 - 0.64D2 * t84 * t1014 + 0.192D3 * t1017 * t1018 * t945 -
     # 0.640D3 * t195 - 0.780D3 * t215 + 0.64D2 * t219 + 0.32D2 * t641 *
     # t1025 * t79 - 0.32D2 * t1017 * t679 * t800 - 0.96D2 * t1017 * t10
     #18 * t800 - 0.384D3 * t78 * t145 * z * t11 * x3
      t1071 = -0.1152D4 * t249 - 0.64D2 * t76 + 0.128D3 * t254 - 0.8D1 *
     # t258 + 0.288D3 * t277 - 0.64D2 * t285 - 0.64D2 * t288 + 0.128D3 *
     # t298 + 0.128D3 * t309 + 0.32D2 * t314 * t942 - 0.128D3 * t312 - 0
     #.64D2 * t315 + 0.640D3 * t320 - 0.672D3 * t872 * t420 + 0.16D2 * t
     #602 * t177 * t300 + 0.112D3 * t602 * t923 * t345 - 0.112D3 * t872 
     #* t426 - 0.16D2 * t872 * t425 * t484 + 0.8D1 * t334 + 0.640D3 * t3
     #38
      t1092 = 0.28792D5 * t343 - 0.144D3 * t348 - 0.29128D5 * t352 - 0.7
     #4208D5 * t364 - 0.256D3 * t367 + 0.28208D5 * t375 - 0.28372D5 * t3
     #80 + 0.29128D5 * t383 + 0.28224D5 * t391 - 0.27920D5 * t401 + 0.74
     #296D5 * t408 - 0.128D3 * t410 - 0.4D1 * t413 + 0.252D3 * t415 - 0.
     #74320D5 * t417 - 0.28792D5 * t421 - 0.128D3 * t423 - 0.144D3 * t42
     #7 + 0.32D2 * t433 + 0.25220D5 * t443
      t1114 = 0.640D3 * t448 - 0.160D3 * t454 - 0.208D3 * t460 + t874 + 
     #0.16D2 * t875 + 0.16D2 * t878 - 0.25296D5 * t882 + 0.27920D5 * t46
     #3 + 0.424D3 * t473 - 0.8D1 * t479 - 0.320D3 * t482 - 0.28208D5 * t
     #491 + 0.28224D5 * t493 + 0.640D3 * t501 - 0.304D3 * t503 + 0.304D3
     # * t505 + 0.780D3 * t511 - 0.780D3 * t515 + 0.416D3 * t520 - 0.640
     #D3 * t526
      t1134 = 0.640D3 * t532 + 0.8D1 * t535 + 0.64D2 * t552 + 0.304D3 * 
     #t558 + 0.512D3 * t561 - 0.640D3 * t566 + 0.304D3 * t570 - 0.304D3 
     #* t576 - 0.640D3 * t579 + 0.74208D5 * t582 - 0.640D3 * t587 + 0.17
     #28D4 * t591 - t596 + 0.512D3 * t598 + 0.320D3 * t610 - 0.160D3 * t
     #613 - 0.640D3 * t617 + 0.320D3 * t621 - 0.208D3 * t624 - 0.160D3 *
     # t627
      t1139 = t110 * z
      t1150 = t33 * t649
      t1170 = t53 * t110
      t1186 = 0.25248D5 * t921 + 0.128D3 * t926 + 0.128D3 * t930 - 0.384
     #D3 * t377 * t393 * t1139 + 0.384D3 * t373 * t419 * t1139 - 0.768D3
     # * t377 * t7 * z * t145 + 0.384D3 * t382 * t378 * t1150 + 0.128D3 
     #* t2 * t3 * x1 * t145 * t649 * z * t38 - 0.384D3 * t385 * t19 * t1
     #150 + 0.96D2 * t602 * t177 * t23 * t118 + 0.32D2 * t872 * t581 - 0
     #.48D2 * t872 * t425 * t1170 + 0.288D3 * t602 * t923 * t1170 - 0.41
     #6D3 * t872 * t920 + 0.48D2 * t872 * t425 * t924 + t631 - 0.128D3 *
     # t633 + 0.74296D5 * t635 + 0.64D2 * t638 + 0.780D3 * t644
      t1210 = -0.8D1 * t696 - 0.28372D5 * t698 + 0.25220D5 * t700 - 0.51
     #2D3 * t704 - 0.192D3 * t706 + 0.288D3 * t709 + 0.912D3 * t712 - t7
     #15 + 0.536D3 * t718 + 0.8D1 * t730 - 0.144D3 * t737
      rrqg2qgh11J4 = (wd * (t188 + t283 + t361 + t429 + t507 + t600 + t6
     #76 + t745) + wd * (t781 + t823 + t843 + t863 + t892 + t912 + t941 
     #+ t963) + wd * (t999 + t1040 + t1071 + t1092 + t1114 + t1134 + t11
     #86 - 0.536D3 * t647 + t653 + 0.128D3 * t655 + t661 + 0.64D2 * t314
     # * t1014 - 0.48D2 * t641 * t1025 * t145 - 0.288D3 * t1017 * t681 +
     # 0.80D2 * t943 - 0.64D2 * t947 + 0.256D3 * t686 + t1210)) / t1 / t
     #74 / z / 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh11J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t5 = t4 ** 2
      t6 = t5 * t3
      t7 = t2 * t6
      t8 = x1 ** 2
      t9 = t8 ** 2
      t10 = x2 * t9
      t11 = t7 * t10
      t12 = 0.1D1 - x1
      t13 = x1 * t3
      t14 = z + t13
      t15 = t14 ** 2
      t16 = t15 ** 2
      t17 = 0.1D1 / t16
      t18 = t12 * t17
      t19 = s * t3
      t20 = 0.1D1 / t14
      t21 = x1 * t20
      t22 = 0.1D1 - x2
      t23 = x3 * t22
      t25 = 0.1D1 - x3
      t28 = cos(x4 * 0.3141592653589793D1)
      t32 = Sqrt(t23 * t14 * x2 * t25)
      t34 = 0.2D1 * t28 * t32
      t35 = t23 * t14 + x2 * t25 - t34
      t36 = t21 * t35
      t38 = t12 * x3
      t40 = s - t19 * t36 - t19 * t38
      t44 = t25 * t22 * t14 + x2 * x3 + t34
      t45 = t40 * t44
      t46 = t35 ** 2
      t49 = t11 * t18 * t45 * t46
      t51 = s * t1
      t52 = t4 * t3
      t53 = t51 * t52
      t54 = x2 * x1
      t55 = t54 * t53
      t56 = t12 ** 2
      t57 = t56 * t20
      t58 = t40 ** 2
      t59 = t58 * t25
      t60 = t57 * t59
      t61 = t55 * t60
      t63 = t51 * t5
      t64 = t8 * x2
      t65 = t63 * t64
      t66 = 0.1D1 / t15
      t67 = t56 * t66
      t68 = t58 * t35
      t71 = t65 * t67 * t68 * t25
      t73 = t8 * x1
      t74 = x2 * t73
      t75 = t7 * t74
      t76 = t40 * t35
      t77 = t76 * t25
      t79 = t75 * t67 * t77
      t81 = t58 * t51
      t82 = t52 * t8
      t83 = t81 * t82
      t84 = t20 * t35
      t85 = t12 * t25
      t86 = t84 * t85
      t87 = t83 * t86
      t89 = t40 * t2
      t90 = t89 * t82
      t91 = t66 * t46
      t92 = t91 * t85
      t93 = t90 * t92
      t95 = t2 * t5
      t97 = t95 * t8 * t40
      t98 = x2 * t56
      t99 = t20 * t25
      t101 = t97 * t98 * t99
      t103 = t52 * t12
      t105 = x3 * t8
      t106 = t105 * t91
      t107 = t89 * t103 * t106
      t109 = t4 * t12
      t110 = t81 * t109
      t112 = x3 * x1 * t84
      t113 = t110 * t112
      t117 = x1 * t12
      t118 = z ** 2
      t122 = 0.1728D4 * t89 * t4 * x2 * t117 * t20 * t118
      t123 = t40 * x3
      t126 = t75 * t67 * t123 * t35
      t128 = t63 * t74
      t129 = t12 * t66
      t130 = t129 * t68
      t131 = t128 * t130
      t133 = t5 * t52
      t134 = t2 * t133
      t135 = x2 ** 2
      t136 = t135 * x2
      t137 = t136 * t73
      t139 = t56 ** 2
      t141 = 0.1D1 / t15 / t14
      t143 = t40 * t25
      t147 = t5 * x2
      t148 = t89 * t147
      t149 = t56 * t12
      t150 = x1 * t149
      t151 = x3 ** 2
      t152 = t20 * t151
      t156 = t73 * t12
      t157 = t141 * t46
      t159 = t148 * t156 * t157
      t160 = 0.576D3 * t159
      t161 = t7 * t64
      t162 = t149 * t66
      t163 = t25 ** 2
      t164 = t40 * t163
      t167 = t161 * t162 * t164 * t44
      t169 = t56 * t141
      t170 = t44 ** 2
      t175 = t5 * t4
      t176 = t2 * t175
      t178 = t176 * t135 * t9
      t179 = t56 * t17
      t180 = t40 * t46
      t182 = t178 * t179 * t180
      t184 = t135 * t73
      t185 = t176 * t184
      t187 = t185 * t162 * t123
      t189 = 0.36D2 * t49 - 0.1632D4 * t61 - 0.400D3 * t71 - 0.128D3 * t
     #79 + 0.128D3 * t87 + 0.144D3 * t93 + 0.1632D4 * t101 + 0.36D2 * t1
     #07 + 0.96D2 * t113 - t122 - 0.94D2 * t126 + 0.275D3 * t131 + 0.144
     #D3 * t134 * t137 * t139 * t141 * t143 - 0.576D3 * t148 * t150 * t1
     #52 - t160 + 0.216D3 * t167 + 0.144D3 * t75 * t169 * t143 * t170 + 
     #0.36D2 * t182 - 0.148D3 * t187
      t191 = t178 * t169 * t76
      t194 = t134 * t136 * t9
      t195 = t149 * t17
      t199 = t2 * s
      t201 = t199 * t118 * t147
      t202 = t8 * t56
      t203 = x3 * t20
      t204 = t202 * t203
      t205 = t201 * t204
      t206 = 0.64D2 * t205
      t207 = t12 * t141
      t210 = t128 * t207 * t68 * t44
      t213 = t40 * z
      t216 = t95 * t74 * t129 * t213 * t35
      t218 = t89 * t13
      t220 = t81 * t13
      t222 = t40 * t170
      t229 = t95 * t64 * t57 * t213 * x3
      t230 = 0.128D3 * t229
      t231 = x2 * t12
      t235 = 0.1728D4 * t90 * t231 * t20 * z
      t237 = t89 * t52 * x2
      t238 = t8 * t12
      t243 = 0.1728D4 * t237 * t238 * t66 * z * t35
      t244 = t51 * t4
      t246 = t12 * t20
      t250 = 0.1728D4 * t244 * t54 * t246 * t58 * z
      t252 = t185 * t162 * t143
      t254 = t65 * t60
      t259 = t4 * x1
      t260 = t81 * t259
      t261 = t260 * t86
      t264 = t89 * t259 * t86
      t268 = t128 * t207 * t58 * t46
      t270 = t46 * t35
      t273 = t11 * t18 * t40 * t270
      t275 = t51 * t6
      t276 = t275 * t184
      t278 = t276 * t169 * t68
      t280 = 0.148D3 * t191 + 0.108D3 * t194 * t195 * t45 - t206 - 0.530
     #D3 * t210 - 0.128D3 * t216 + 0.10D2 * t218 - 0.122D3 * t220 + 0.10
     #8D3 * t178 * t179 * t222 + t230 - t235 + t243 + t250 + 0.240D3 * t
     #252 + 0.528D3 * t254 - 0.56D2 * t89 * t109 * t112 + 0.128D3 * t261
     # - 0.112D3 * t264 - 0.192D3 * t268 - 0.28D2 * t273 + 0.182D3 * t27
     #8
      t282 = x1 * t56
      t286 = t58 * x3
      t287 = t57 * t286
      t288 = t65 * t287
      t290 = t199 * t6
      t291 = t290 * t74
      t294 = t291 * t67 * x3 * t35
      t296 = t149 * t20
      t298 = t161 * t296 * t164
      t301 = t97 * t98 * t203
      t303 = t38 * t84
      t304 = t90 * t303
      t307 = t89 * t6 * t135
      t308 = t73 * t56
      t309 = t141 * t44
      t314 = t95 * t73 * t40
      t315 = t66 * t35
      t317 = t314 * t231 * t315
      t319 = t149 * t141
      t320 = t143 * t44
      t325 = t178 * t169 * t45
      t327 = t76 * t44
      t329 = t11 * t207 * t327
      t331 = t66 * t44
      t335 = t4 * t56
      t336 = t335 * t151
      t339 = t81 * t336
      t341 = t335 * t163
      t342 = t81 * t341
      t344 = t89 * t341
      t346 = t52 * t149
      t347 = t163 * t25
      t350 = 0.144D3 * t89 * t346 * t347
      t351 = t3 * t12
      t352 = t351 * x3
      t353 = t81 * t352
      t355 = t151 * x3
      t361 = -0.1152D4 * t237 * t282 * t203 - 0.17D2 * t288 - 0.848D3 * 
     #t294 + 0.312D3 * t298 - 0.3523D4 * t301 + 0.530D3 * t304 - 0.1152D
     #4 * t307 * t308 * t309 - 0.3466D4 * t317 + 0.288D3 * t185 * t319 *
     # t320 - 0.16D2 * t325 - 0.96D2 * t329 + 0.864D3 * t237 * t238 * t3
     #31 + 0.108D3 * t89 * t336 - 0.748D3 * t339 - 0.312D3 * t342 + 0.21
     #6D3 * t344 + t350 + 0.16D2 * t353 + 0.36D2 * t89 * t346 * t355 + 0
     #.108D3 * t89 * t352
      t362 = t351 * t25
      t365 = t81 * t362
      t368 = t66 * t58
      t370 = t63 * t135 * t202 * t368
      t372 = t89 * t52
      t374 = t372 * t64 * t246
      t377 = t199 * t5 * x2
      t378 = t377 * t204
      t381 = t199 * t175 * t135
      t382 = t73 * t149
      t383 = t66 * x3
      t385 = t381 * t382 * t383
      t388 = t20 * t58
      t390 = t244 * x2 * t117 * t388
      t392 = t81 * t4
      t393 = t8 * t66
      t394 = t35 * t44
      t396 = t392 * t393 * t394
      t402 = t5 ** 2
      t404 = t135 ** 2
      t412 = t9 * t149
      t413 = t141 * t40
      t415 = t134 * t136 * t412 * t413
      t421 = t51 * t175 * t136 * t382 * t141 * t58
      t423 = t156 * t315
      t424 = t377 * t423
      t426 = t9 * t56
      t429 = t381 * t426 * t141 * t35
      t433 = t275 * t135 * t308 * t368
      t435 = t290 * x2
      t436 = t9 * t12
      t438 = t435 * t436 * t157
      t441 = t377 * t156 * t331
      t443 = x3 * t25
      t444 = t282 * t443
      t445 = t372 * t444
      t450 = t7 * t73 * t40 * t135 * t67
      t454 = t53 * x2 * t238 * t388
      t456 = 0.144D3 * t89 * t362 - 0.240D3 * t365 - 0.3523D4 * t370 + 0
     #.3523D4 * t374 + 0.752D3 * t378 - 0.1088D4 * t385 + 0.1244D4 * t39
     #0 + 0.94D2 * t396 - 0.576D3 * t89 * t175 * t137 * t319 + 0.36D2 * 
     #t2 * t402 * t404 * t9 * t139 * t17 * t40 - 0.12D2 * t415 - 0.10D2 
     #* t421 + 0.848D3 * t424 - 0.1088D4 * t429 - 0.122D3 * t433 - 0.472
     #D3 * t438 - 0.1088D4 * t441 + 0.584D3 * t445 - 0.1244D4 * t450 - 0
     #.7896D4 * t454
      t459 = t81 * z
      t460 = t20 * t44
      t462 = t459 * t13 * t460
      t463 = 0.128D3 * t462
      t465 = t459 * t13 * t84
      t467 = t89 * t5
      t468 = t135 * t8
      t472 = t89 * t4
      t477 = t381 * t426 * t309
      t479 = t141 * t170
      t481 = t435 * t436 * t479
      t484 = t377 * t202 * t99
      t488 = t381 * t382 * t66 * t25
      t490 = t8 * t149
      t492 = t435 * t490 * t152
      t494 = t20 * t163
      t496 = t435 * t490 * t494
      t498 = t58 * t44
      t499 = t129 * t498
      t500 = t128 * t499
      t508 = t291 * t67 * t44 * x3
      t510 = t63 * t54
      t513 = t510 * t296 * t58 * t163
      t515 = t275 * t468
      t517 = t515 * t162 * t59
      t520 = t148 * t150 * t494
      t523 = t11 * t207 * t180
      t526 = t11 * t207 * t222
      t529 = t276 * t169 * t498
      t531 = t55 * t287
      t533 = -t463 + 0.128D3 * t465 + 0.864D3 * t467 * t468 * t67 - 0.57
     #6D3 * t472 * t54 * t246 + 0.544D3 * t477 - 0.272D3 * t481 - 0.1088
     #D4 * t484 + 0.544D3 * t488 - 0.376D3 * t492 - 0.272D3 * t496 + 0.3
     #06D3 * t500 + 0.864D3 * t148 * t202 * t331 * x3 + 0.1088D4 * t508 
     #- 0.56D2 * t513 + 0.328D3 * t517 + 0.224D3 * t520 + 0.47D2 * t523 
     #+ 0.748D3 * t526 + 0.328D3 * t529 - 0.1244D4 * t531
      t534 = t201 * t423
      t535 = 0.64D2 * t534
      t537 = t75 * t67 * t320
      t539 = t460 * t38
      t540 = t260 * t539
      t544 = t110 * t25 * x1 * t460
      t546 = t25 * t44
      t548 = t291 * t67 * t546
      t550 = t282 * t151
      t551 = t372 * t550
      t553 = t8 * t20
      t554 = t553 * t35
      t555 = t392 * t554
      t557 = t472 * t554
      t559 = t81 * t3
      t560 = t21 * t44
      t561 = t559 * t560
      t563 = t117 * x3
      t564 = t392 * t563
      t566 = t393 * t46
      t567 = t472 * t566
      t570 = t56 * x3 * t25
      t573 = t472 * t563
      t579 = t117 * t25
      t580 = t392 * t579
      t582 = t392 * t570
      t585 = t392 * t393 * t170
      t587 = t392 * t566
      t589 = t472 * t579
      t591 = t73 * t66
      t592 = t591 * t46
      t593 = t372 * t592
      t595 = t535 + 0.880D3 * t537 - 0.96D2 * t540 - 0.128D3 * t544 - 0.
     #544D3 * t548 + 0.430D3 * t551 + 0.275D3 * t555 - 0.182D3 * t557 + 
     #0.148D3 * t561 + 0.306D3 * t564 + 0.36D2 * t567 + 0.288D3 * t472 *
     # t570 - 0.328D3 * t573 + 0.144D3 * t372 * t149 * t25 * t151 + 0.52
     #8D3 * t580 - 0.880D3 * t582 - 0.47D2 * t585 - 0.47D2 * t587 - 0.32
     #8D3 * t589 + 0.192D3 * t593
      t597 = t559 * t36
      t599 = t73 * t141
      t600 = t599 * t270
      t601 = t372 * t600
      t603 = t282 * t163
      t604 = t372 * t603
      t608 = t372 * t149 * t163 * x3
      t613 = t553 * t44
      t614 = t392 * t613
      t618 = t290 * t135 * t308 * t66
      t623 = t199 * t133 * t136 * t412 * t141
      t628 = t199 * t52 * x2 * t238 * t20
      t630 = t52 * t56
      t631 = t89 * t630
      t632 = t443 * t36
      t633 = t631 * t632
      t637 = t75 * t67 * t45 * x3
      t639 = t52 * x1
      t642 = t84 * t56 * t163
      t643 = t89 * t639 * t642
      t650 = 0.144D3 * t7 * t54 * t139 * t20 * t40 * t347
      t654 = t176 * t468 * t139 * t66 * t164
      t658 = t161 * t296 * t40 * t151
      t663 = t53 * t64
      t664 = t663 * t499
      t668 = t161 * t296 * t123 * t25
      t672 = t161 * t162 * t76 * t163
      t675 = t185 * t319 * t77
      t677 = -0.148D3 * t597 - 0.28D2 * t601 + 0.56D2 * t604 + 0.216D3 *
     # t608 - 0.28D2 * t89 * t3 * t36 - 0.17D2 * t614 + 0.1088D4 * t618 
     #- 0.272D3 * t623 - 0.376D3 * t628 - 0.112D3 * t633 + 0.96D2 * t637
     # - 0.168D3 * t643 + t650 + 0.216D3 * t654 + 0.47D2 * t658 + 0.864D
     #3 * t307 * t490 * t383 + 0.3523D4 * t664 + 0.128D3 * t668 - 0.168D
     #3 * t672 - 0.112D3 * t675
      t680 = t148 * t202 * t315 * t25
      t682 = t170 * t44
      t689 = t128 * t207 * t58 * t170
      t691 = t83 * t539
      t695 = t291 * t67 * t35 * t25
      t700 = t151 * x1
      t701 = t700 * t84
      t706 = t75 * t169 * t180 * t25
      t712 = t7 * t74 * t56 * t413 * t546 * t35
      t716 = t65 * t67 * t59 * t44
      t718 = t90 * t86
      t721 = t314 * t231 * t331
      t725 = t290 * t10 * t207 * t394
      t727 = t663 * t130
      t733 = t290 * t64 * t296 * t443
      t745 = -0.128D3 * t680 + 0.36D2 * t11 * t18 * t40 * t682 - 0.430D3
     # * t689 - 0.128D3 * t691 + 0.1088D4 * t695 - 0.28D2 * t194 * t195 
     #* t76 - 0.28D2 * t631 * t701 + 0.144D3 * t706 - 0.112D3 * t712 - 0
     #.584D3 * t716 + 0.400D3 * t718 + 0.1244D4 * t721 + 0.1088D4 * t725
     # + 0.3466D4 * t727 + 0.12D2 * t81 + 0.36D2 * t89 + 0.1088D4 * t733
     # - 0.576D3 * t148 * t156 * t479 - 0.56D2 * t178 * t179 * t327 - 0.
     #28D2 * t11 * t18 * t222 * t35
      t753 = t331 * t35
      t755 = t314 * t85 * t753
      t771 = -0.72D2 * t49 + 0.4816D4 * t61 + 0.288D3 * t71 - 0.128D3 * 
     #t755 + 0.496D3 * t79 - 0.256D3 * t93 - 0.4816D4 * t101 - 0.72D2 * 
     #t107 - 0.640D3 * t113 + t122 + 0.108D3 * t126 - 0.156D3 * t131 + 0
     #.352D3 * t159 - 0.72D2 * t167 - 0.72D2 * t182 + 0.216D3 * t187 - 0
     #.216D3 * t191 + 0.128D3 * t205 + 0.444D3 * t210
      t777 = t81 * t639 * t642
      t779 = t97 * t642
      t791 = 0.256D3 * t216 - 0.62D2 * t218 + 0.134D3 * t220 - 0.256D3 *
     # t229 + t235 - t243 - t250 + 0.64D2 * t777 - 0.64D2 * t779 + 0.64D
     #2 * t252 - 0.752D3 * t254 - 0.496D3 * t261 + 0.144D3 * t264 + 0.30
     #D2 * t268 + 0.56D2 * t273 - 0.108D3 * t278 + 0.490D3 * t288 + 0.13
     #60D4 * t294 - 0.264D3 * t298
      t797 = t81 * t52
      t799 = 0.128D3 * t797 * t603
      t800 = t797 * t550
      t802 = t591 * t170
      t803 = t797 * t802
      t807 = t472 * t613
      t815 = t81 * t630
      t816 = t815 * t701
      t818 = t815 * t632
      t820 = t56 * t25
      t823 = t97 * t820 * t203 * t44
      t825 = -0.5266D4 * t301 - 0.444D3 * t304 - 0.4940D4 * t317 + 0.608
     #D3 * t325 + t799 - 0.64D2 * t800 - 0.64D2 * t803 + 0.136D3 * t372 
     #* t802 - 0.256D3 * t807 + 0.640D3 * t329 + 0.592D3 * t339 + 0.264D
     #3 * t342 - 0.72D2 * t344 - t350 - 0.608D3 * t353 - 0.64D2 * t365 +
     # 0.64D2 * t816 + 0.128D3 * t818 + 0.128D3 * t823
      t828 = t65 * t67 * t498 * x3
      t831 = t815 * t443 * t560
      t850 = -0.256D3 * t828 - 0.128D3 * t831 - 0.5266D4 * t370 + 0.5266
     #D4 * t374 - 0.1168D4 * t378 + 0.1728D4 * t385 - 0.3776D4 * t390 - 
     #0.108D3 * t396 - 0.16D2 * t415 + 0.62D2 * t421 - 0.1360D4 * t424 +
     # 0.1728D4 * t429 + 0.134D3 * t433 + 0.776D3 * t438 + 0.1728D4 * t4
     #41 - 0.544D3 * t445 + 0.3776D4 * t450 - 0.9344D4 * t454 + 0.256D3 
     #* t462
      t860 = t591 * t394
      t861 = t372 * t860
      t863 = t9 * t141
      t864 = t170 * t35
      t866 = t467 * t863 * t864
      t870 = t467 * t150 * t163 * x3
      t880 = t510 * t296 * t286 * t25
      t887 = -0.256D3 * t465 - 0.864D3 * t477 + 0.432D3 * t481 + 0.1728D
     #4 * t484 - 0.864D3 * t488 + 0.584D3 * t492 + 0.432D3 * t496 + 0.25
     #6D3 * t861 - 0.64D2 * t866 + 0.64D2 * t870 - 0.190D3 * t500 - 0.17
     #28D4 * t508 - 0.24D2 * t513 - 0.256D3 * t517 - 0.1376D4 * t520 - 0
     #.54D2 * t523 - 0.216D3 * t880 - 0.136D3 * t510 * t296 * t58 * t151
     # - 0.592D3 * t526
      t892 = t65 * t67 * t286 * t35
      t899 = t815 * t163 * x1 * t460
      t901 = t85 * t460
      t902 = t90 * t901
      t917 = -0.604D3 * t529 + 0.3776D4 * t531 - 0.256D3 * t892 - 0.64D2
     # * t815 * t700 * t460 - 0.64D2 * t899 + 0.216D3 * t902 - 0.128D3 *
     # t534 - 0.640D3 * t537 + 0.640D3 * t540 + 0.496D3 * t544 + 0.864D3
     # * t548 - 0.542D3 * t551 - 0.156D3 * t555 + 0.108D3 * t557 - 0.216
     #D3 * t561 - 0.190D3 * t564 - 0.72D2 * t567 + 0.604D3 * t573 - 0.75
     #2D3 * t580
      t937 = 0.640D3 * t582 + 0.54D2 * t585 + 0.54D2 * t587 + 0.256D3 * 
     #t589 - 0.30D2 * t593 + 0.216D3 * t597 + 0.56D2 * t601 + 0.24D2 * t
     #604 - 0.72D2 * t608 + 0.490D3 * t614 - 0.1728D4 * t618 + 0.432D3 *
     # t623 + 0.584D3 * t628 + 0.144D3 * t633 - 0.640D3 * t637 + 0.344D3
     # * t643 - t650 - 0.72D2 * t654 - 0.54D2 * t658
      t938 = t90 * t539
      t940 = t66 * t170
      t951 = t515 * t162 * t286
      t963 = 0.256D3 * t938 + 0.64D2 * t314 * t38 * t940 + 0.5266D4 * t6
     #64 - 0.496D3 * t668 + 0.344D3 * t672 + 0.144D3 * t675 + 0.1024D4 *
     # t680 + 0.542D3 * t689 + 0.256D3 * t951 - 0.1728D4 * t695 - 0.256D
     #3 * t706 + 0.144D3 * t712 + 0.544D3 * t716 - 0.288D3 * t718 - 0.37
     #76D4 * t721 - 0.1728D4 * t725 + 0.4940D4 * t727 + 0.16D2 * t81 - 0
     #.1728D4 * t733
      t985 = -0.912D3 * t61 - 0.74288D5 * t71 - 0.64D2 * t755 - 0.8D1 * 
     #t79 - 0.192D3 * t87 + 0.288D3 * t93 + 0.912D3 * t101 + 0.256D3 * t
     #113 - 0.8D1 * t126 - 0.28372D5 * t131 + t160 + 0.128D3 * t191 - t2
     #06 + 0.536D3 * t210 - 0.512D3 * t216 - 0.304D3 * t218 + 0.304D3 * 
     #t220 + t230 + 0.128D3 * t777 - 0.128D3 * t779
      t1007 = -0.128D3 * t252 + 0.74296D5 * t254 + 0.8D1 * t261 + 0.2879
     #2D5 * t268 - 0.144D3 * t273 - 0.29128D5 * t278 + 0.25220D5 * t288 
     #- 0.512D3 * t294 + 0.64D2 * t298 + 0.780D3 * t301 - 0.536D3 * t304
     # + 0.424D3 * t317 - 0.128D3 * t325 + 0.48D2 * t314 * t38 * t91 + t
     #799 + 0.16D2 * t800 + 0.16D2 * t803 - 0.25296D5 * t807 - 0.256D3 *
     # t329 - 0.64D2 * t339
      t1017 = t46 * z
      t1028 = t35 * t118
      t1048 = t44 * t46
      t1060 = -0.64D2 * t342 + 0.128D3 * t353 + 0.128D3 * t365 - 0.128D3
     # * t816 + 0.64D2 * t818 + 0.64D2 * t823 - 0.25264D5 * t828 - 0.64D
     #2 * t831 - 0.384D3 * t392 * t393 * t1017 + 0.384D3 * t372 * t591 *
     # t1017 - 0.768D3 * t392 * t8 * z * t84 + 0.384D3 * t472 * t553 * t
     #1028 + 0.128D3 * t2 * t3 * x1 * t84 * t118 * z * t40 - 0.384D3 * t
     #559 * t21 * t1028 + 0.96D2 * t467 * t150 * t25 * t151 + 0.32D2 * t
     #797 * t444 - 0.48D2 * t797 * t599 * t1048 + 0.288D3 * t467 * t863 
     #* t1048 - 0.416D3 * t797 * t860 + 0.48D2 * t797 * t599 * t864
      t1080 = 0.780D3 * t370 - 0.780D3 * t374 + 0.416D3 * t378 - 0.640D3
     # * t385 + 0.640D3 * t390 + 0.8D1 * t396 + 0.64D2 * t415 + 0.304D3 
     #* t421 + 0.512D3 * t424 - 0.640D3 * t429 + 0.304D3 * t433 - 0.304D
     #3 * t438 - 0.640D3 * t441 + 0.74208D5 * t445 - 0.640D3 * t450 + 0.
     #1728D4 * t454 - t463 + 0.512D3 * t465 + 0.320D3 * t477 - 0.160D3 *
     # t481
      t1103 = -0.640D3 * t484 + 0.320D3 * t488 - 0.208D3 * t492 - 0.160D
     #3 * t496 + 0.25248D5 * t861 + 0.128D3 * t866 + 0.128D3 * t870 + 0.
     #28224D5 * t500 + 0.640D3 * t508 - 0.32D2 * t513 + 0.74320D5 * t517
     # + 0.576D3 * t520 - 0.252D3 * t523 - 0.80D2 * t880 + 0.64D2 * t526
     # + 0.27920D5 * t529 - 0.640D3 * t531 - 0.25248D5 * t892 - 0.128D3 
     #* t899 + 0.80D2 * t902
      t1107 = t81 * t103
      t1108 = t25 * t8
      t1131 = t535 + 0.128D3 * t537 - 0.8D1 * t544 - 0.320D3 * t548 - 0.
     #96D2 * t1107 * t1108 * t940 - 0.384D3 * t260 * t84 * z * t12 * x3 
     #+ 0.28208D5 * t551 - 0.28372D5 * t555 + 0.29128D5 * t557 + 0.28224
     #D5 * t564 - 0.27920D5 * t573 + 0.74296D5 * t580 - 0.128D3 * t582 -
     # 0.4D1 * t585 + 0.252D3 * t587 - 0.74320D5 * t589 - 0.28792D5 * t5
     #93 - 0.128D3 * t597 - 0.144D3 * t601 + 0.32D2 * t604
      t1137 = t56 * t151
      t1167 = t315 * t38 * t44
      t1177 = 0.25220D5 * t614 + 0.640D3 * t618 - 0.160D3 * t623 - 0.208
     #D3 * t628 + 0.32D2 * t97 * t1137 * t460 - 0.32D2 * t1107 * t105 * 
     #t940 + 0.384D3 * t89 * t52 * t73 * t315 * z * t44 - 0.192D3 * t97 
     #* t820 * t203 * t35 - 0.416D3 * t83 * t303 - 0.144D3 * t643 + 0.4D
     #1 * t658 + 0.25264D5 * t938 + 0.192D3 * t1107 * t1108 * t753 + 0.3
     #2D2 * t83 * t901 - 0.780D3 * t664 + 0.8D1 * t668 + 0.64D2 * t83 * 
     #t1167 - 0.48D2 * t97 * t1137 * t84 - 0.288D3 * t1107 * t106 - 0.96
     #D2 * t83 * t92
      t1210 = -0.16D2 * t797 * t599 * t682 - 0.64D2 * t691 + 0.640D3 * t
     #695 + 0.288D3 * t706 - 0.74208D5 * t716 + 0.74288D5 * t718 + 0.640
     #D3 * t721 + 0.640D3 * t725 - 0.424D3 * t727 - 0.64D2 * t81 + 0.640
     #D3 * t733
      rrqg2qgh11J5 = (wd * (t189 + t280 + t361 + t456 + t533 + t595 + t6
     #77 + t745) + wd * (t771 + t791 + t825 + t850 + t887 + t917 + t937 
     #+ t963) + wd * (t985 + t1007 + t1060 + t1080 + t1103 + t1131 + t11
     #77 - 0.144D3 * t672 - 0.1152D4 * t680 + 0.96D2 * t314 * t92 - 0.64
     #D2 * t314 * t1167 - 0.28208D5 * t689 + 0.25296D5 * t951 - 0.672D3 
     #* t797 * t592 + 0.16D2 * t467 * t150 * t355 + 0.112D3 * t467 * t86
     #3 * t270 - 0.112D3 * t797 * t600 + t1210)) / t1 / t58 / z / 0.3141
     #592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh11J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t6 * t4
      t8 = t3 * t7
      t9 = x1 ** 2
      t10 = t9 * x1
      t11 = x2 * t10
      t12 = t8 * t11
      t13 = 0.1D1 - x1
      t14 = t13 ** 2
      t15 = t4 * x1
      t16 = z + t15
      t17 = t16 ** 2
      t18 = 0.1D1 / t17
      t19 = t14 * t18
      t20 = 0.1D1 - x2
      t21 = x3 * t20
      t23 = 0.1D1 - x3
      t26 = cos(x4 * 0.3141592653589793D1)
      t30 = Sqrt(t21 * t16 * x2 * t23)
      t32 = 0.2D1 * t26 * t30
      t33 = t21 * t16 + x2 * t23 - t32
      t36 = t12 * t19 * x3 * t33
      t38 = s * t4
      t39 = 0.1D1 / t16
      t40 = x1 * t39
      t41 = t40 * t33
      t43 = t13 * x3
      t45 = s - t38 * t41 - t38 * t43
      t46 = t45 * t2
      t47 = t5 * t4
      t48 = t47 * t9
      t49 = t46 * t48
      t50 = t39 * t33
      t51 = t43 * t50
      t52 = t49 * t51
      t54 = z ** 2
      t56 = t6 * x2
      t57 = t3 * t54 * t56
      t58 = t10 * t13
      t59 = t18 * t33
      t60 = t58 * t59
      t61 = t57 * t60
      t63 = s * t1
      t64 = t63 * t47
      t65 = x2 * x1
      t66 = t64 * t65
      t67 = t14 * t39
      t68 = t45 ** 2
      t69 = t68 * x3
      t70 = t67 * t69
      t71 = t66 * t70
      t73 = t6 * t5
      t74 = t2 * t73
      t75 = x2 ** 2
      t76 = t9 ** 2
      t78 = t74 * t75 * t76
      t79 = t17 ** 2
      t80 = 0.1D1 / t79
      t82 = t33 ** 2
      t83 = t45 * t82
      t87 = t68 * t63
      t89 = t63 * t6
      t90 = t89 * t11
      t92 = 0.1D1 / t17 / t16
      t93 = t13 * t92
      t94 = t68 * t33
      t98 = t23 * t20 * t16 + x2 * x3 + t32
      t101 = t90 * t93 * t94 * t98
      t103 = t87 * t47
      t104 = x1 * t14
      t105 = t23 ** 2
      t106 = t104 * t105
      t108 = 0.128D3 * t103 * t106
      t109 = x3 ** 2
      t110 = t104 * t109
      t111 = t103 * t110
      t113 = t10 * t18
      t114 = t98 ** 2
      t115 = t113 * t114
      t116 = t103 * t115
      t118 = t46 * t47
      t121 = t46 * t5
      t122 = t9 * t39
      t123 = t122 * t98
      t124 = t121 * t123
      t126 = t118 * t110
      t128 = t87 * t5
      t129 = t122 * t33
      t130 = t128 * t129
      t132 = t121 * t129
      t134 = t87 * t4
      t135 = t40 * t98
      t138 = x1 * t13
      t139 = t138 * x3
      t140 = t128 * t139
      t142 = t9 * t18
      t143 = t142 * t82
      t146 = t121 * t139
      t148 = 0.1360D4 * t36 - 0.444D3 * t52 - 0.128D3 * t61 + 0.3776D4 *
     # t71 - 0.72D2 * t78 * t14 * t80 * t83 + 0.16D2 * t87 + 0.444D3 * t
     #101 + t108 - 0.64D2 * t111 - 0.64D2 * t116 + 0.136D3 * t118 * t115
     # - 0.256D3 * t124 - 0.542D3 * t126 - 0.156D3 * t130 + 0.108D3 * t1
     #32 - 0.216D3 * t134 * t135 - 0.190D3 * t140 - 0.72D2 * t121 * t143
     # + 0.604D3 * t146
      t149 = t138 * t23
      t150 = t128 * t149
      t154 = t128 * t14 * x3 * t23
      t157 = t128 * t142 * t114
      t159 = t128 * t143
      t161 = t121 * t149
      t163 = t113 * t82
      t164 = t118 * t163
      t166 = t134 * t41
      t168 = t10 * t92
      t169 = t82 * t33
      t170 = t168 * t169
      t171 = t118 * t170
      t173 = t118 * t106
      t175 = t14 * t13
      t180 = t128 * t123
      t182 = x2 * t9
      t183 = t89 * t182
      t184 = t68 * t23
      t185 = t67 * t184
      t186 = t183 * t185
      t188 = t2 * t7
      t189 = t188 * t182
      t190 = t175 * t39
      t191 = t45 * t105
      t193 = t189 * t190 * t191
      t195 = t89 * t65
      t200 = t63 * t7
      t201 = t75 * t9
      t202 = t200 * t201
      t203 = t175 * t18
      t205 = t202 * t203 * t69
      t209 = t183 * t19 * t69 * t33
      t211 = t47 * x1
      t214 = t50 * t14 * t105
      t215 = t87 * t211 * t214
      t218 = t46 * t6 * t9
      t219 = t218 * t214
      t223 = t195 * t190 * t69 * t23
      t225 = -0.752D3 * t150 + 0.640D3 * t154 + 0.54D2 * t157 + 0.54D2 *
     # t159 + 0.256D3 * t161 - 0.30D2 * t164 + 0.216D3 * t166 + 0.56D2 *
     # t171 + 0.24D2 * t173 - 0.72D2 * t118 * t175 * t105 * x3 + 0.490D3
     # * t180 - 0.752D3 * t186 - 0.264D3 * t193 - 0.136D3 * t195 * t190 
     #* t68 * t109 + 0.256D3 * t205 - 0.256D3 * t209 + 0.64D2 * t215 - 0
     #.64D2 * t219 - 0.216D3 * t223
      t227 = t75 * t10
      t228 = t200 * t227
      t229 = t14 * t92
      t231 = t228 * t229 * t94
      t233 = t64 * t182
      t234 = t13 * t18
      t235 = t68 * t98
      t236 = t234 * t235
      t237 = t233 * t236
      t239 = t5 * x1
      t240 = t87 * t239
      t241 = t39 * t98
      t242 = t241 * t43
      t245 = t2 * t6
      t247 = t245 * t10 * t45
      t248 = x2 * t13
      t249 = t18 * t98
      t251 = t247 * t248 * t249
      t253 = x2 * t76
      t255 = t33 * t98
      t257 = t8 * t253 * t93 * t255
      t259 = t188 * t253
      t261 = t259 * t93 * t83
      t265 = t189 * t190 * t45 * t109
      t267 = t66 * t185
      t270 = t14 ** 2
      t272 = t105 * t23
      t277 = t188 * t11
      t278 = t45 * t23
      t281 = t277 * t19 * t278 * t98
      t283 = x2 * t14
      t284 = t39 * t23
      t286 = t218 * t283 * t284
      t288 = t45 * t33
      t291 = t259 * t93 * t288 * t98
      t293 = t45 * t98
      t299 = t46 * t211 * t214
      t301 = t46 * t56
      t302 = x1 * t175
      t303 = t39 * t105
      t305 = t301 * t302 * t303
      t314 = t45 * z
      t317 = t245 * t182 * t67 * t314 * x3
      t325 = t9 * t13
      t331 = -0.108D3 * t231 + 0.5266D4 * t237 + 0.640D3 * t240 * t242 -
     # 0.3776D4 * t251 - 0.1728D4 * t257 - 0.54D2 * t261 - 0.54D2 * t265
     # + 0.4816D4 * t267 - 0.144D3 * t188 * t65 * t270 * t39 * t45 * t27
     #2 - 0.640D3 * t281 - 0.4816D4 * t286 + 0.640D3 * t291 - 0.640D3 * 
     #t277 * t19 * t293 * x3 + 0.344D3 * t299 - 0.1376D4 * t305 + 0.1728
     #D4 * t46 * t5 * x2 * t138 * t39 * t54 - 0.256D3 * t317 + 0.1728D4 
     #* t49 * t248 * t39 * z - 0.1728D4 * t46 * t47 * x2 * t325 * t18 * 
     #z * t33
      t332 = t45 * x3
      t335 = t189 * t190 * t332 * t23
      t338 = x3 * t23
      t340 = t8 * t182 * t190 * t338
      t342 = t9 * t14
      t343 = t39 * x3
      t344 = t342 * t343
      t345 = t57 * t344
      t347 = t74 * t227
      t349 = t347 * t203 * t278
      t353 = t301 * t342 * t59 * t23
      t357 = t90 * t93 * t68 * t82
      t361 = t183 * t19 * t235 * x3
      t363 = t47 * t14
      t364 = t87 * t363
      t366 = t364 * t338 * t135
      t368 = t23 * t98
      t370 = t12 * t19 * t368
      t374 = t90 * t93 * t68 * t114
      t377 = t228 * t229 * t235
      t380 = t247 * t248 * t59
      t384 = t277 * t229 * t83 * t23
      t386 = t5 * t14
      t388 = t87 * t386 * t109
      t390 = t386 * t105
      t391 = t87 * t390
      t399 = t4 * t13
      t401 = t87 * t399 * x3
      t404 = t87 * t399 * t23
      t406 = -0.496D3 * t335 - 0.1728D4 * t340 + 0.128D3 * t345 + 0.64D2
     # * t349 + 0.1024D4 * t353 + 0.30D2 * t357 - 0.256D3 * t361 - 0.128
     #D3 * t366 + 0.864D3 * t370 + 0.542D3 * t374 - 0.604D3 * t377 - 0.4
     #940D4 * t380 - 0.256D3 * t384 + 0.592D3 * t388 + 0.264D3 * t391 - 
     #0.72D2 * t46 * t390 - 0.144D3 * t46 * t47 * t175 * t272 - 0.608D3 
     #* t401 - 0.64D2 * t404
      t411 = t259 * t93 * t45 * t114
      t414 = t3 * t73 * t75
      t415 = t10 * t175
      t418 = t414 * t415 * t18 * t23
      t420 = t8 * x2
      t421 = t9 * t175
      t424 = t420 * t421 * t39 * t109
      t427 = t420 * t421 * t303
      t429 = t288 * t23
      t431 = t277 * t19 * t429
      t434 = t338 * t41
      t437 = t234 * t94
      t438 = t233 * t437
      t441 = t78 * t229 * t293
      t446 = t245 * t11 * t234 * t314 * t33
      t448 = t13 * t80
      t453 = t63 * t5
      t455 = t13 * t39
      t460 = t23 * t14
      t463 = t218 * t460 * t343 * t98
      t466 = t87 * t5 * t13
      t469 = t466 * x3 * x1 * t50
      t471 = t109 * x1
      t473 = t364 * t471 * t50
      t475 = t364 * t434
      t477 = t49 * t242
      t479 = t18 * t114
      t485 = t466 * t23 * x1 * t241
      t487 = t13 * t23
      t488 = t50 * t487
      t489 = t49 * t488
      t491 = -0.592D3 * t411 - 0.864D3 * t418 + 0.584D3 * t424 + 0.432D3
     # * t427 + 0.496D3 * t431 + 0.144D3 * t46 * t363 * t434 + 0.4940D4 
     #* t438 + 0.608D3 * t441 + 0.256D3 * t446 - 0.72D2 * t259 * t448 * 
     #t293 * t82 - 0.1728D4 * t453 * t65 * t455 * t68 * z + 0.128D3 * t4
     #63 - 0.640D3 * t469 + 0.64D2 * t473 + 0.128D3 * t475 + 0.256D3 * t
     #477 + 0.64D2 * t247 * t43 * t479 + 0.496D3 * t485 - 0.288D3 * t489
      t492 = t92 * t82
      t494 = t301 * t58 * t492
      t500 = t113 * t255
      t501 = t118 * t500
      t504 = t3 * t6 * x2
      t506 = t504 * t342 * t284
      t509 = t10 * t14
      t511 = t8 * t75 * t509 * t18
      t513 = t6 * t47
      t515 = t75 * x2
      t517 = t76 * t175
      t519 = t3 * t513 * t515 * t517 * t92
      t524 = t3 * t47 * x2 * t325 * t39
      t527 = t218 * t283 * t343
      t529 = t47 * t13
      t531 = x3 * t9
      t532 = t18 * t82
      t533 = t531 * t532
      t538 = t364 * t105 * x1 * t241
      t540 = t487 * t241
      t541 = t49 * t540
      t543 = t249 * t33
      t545 = t247 * t487 * t543
      t549 = t92 * t45
      t555 = t78 * t229 * t288
      t557 = t90 * t236
      t561 = t12 * t19 * t98 * x3
      t570 = t195 * t190 * t68 * t105
      t573 = t202 * t203 * t184
      t575 = 0.352D3 * t494 - 0.72D2 * t189 * t203 * t191 * t98 + 0.256D
     #3 * t501 + 0.1728D4 * t506 - 0.1728D4 * t511 + 0.432D3 * t519 + 0.
     #584D3 * t524 - 0.5266D4 * t527 - 0.72D2 * t46 * t529 * t533 - 0.64
     #D2 * t538 + 0.216D3 * t541 - 0.128D3 * t545 + 0.144D3 * t188 * t11
     # * t14 * t549 * t368 * t33 - 0.216D3 * t555 - 0.190D3 * t557 - 0.1
     #728D4 * t561 - 0.72D2 * t74 * t201 * t270 * t18 * t191 - 0.24D2 * 
     #t570 - 0.256D3 * t573
      t579 = t183 * t19 * t184 * t98
      t589 = t259 * t448 * t45 * t169
      t593 = t189 * t203 * t288 * t105
      t599 = t240 * t488
      t604 = t532 * t487
      t605 = t49 * t604
      t609 = t277 * t19 * t332 * t33
      t611 = t90 * t437
      t613 = t183 * t70
      t617 = t414 * t415 * t18 * x3
      t620 = t39 * t68
      t622 = t453 * x2 * t138 * t620
      t625 = t128 * t142 * t255
      t630 = t2 * t513 * t515 * t517 * t549
      t636 = t63 * t73 * t515 * t415 * t92 * t68
      t638 = t504 * t60
      t640 = t76 * t14
      t643 = t414 * t640 * t92 * t33
      t645 = 0.544D3 * t579 + 0.216D3 * t347 * t203 * t332 - 0.64D2 * t3
     #64 * t471 * t241 + 0.56D2 * t589 + 0.344D3 * t593 + 0.144D3 * t347
     # * t175 * t92 * t429 - 0.496D3 * t599 + 0.144D3 * t46 * t239 * t48
     #8 - 0.256D3 * t605 + 0.108D3 * t609 - 0.156D3 * t611 + 0.490D3 * t
     #613 + 0.1728D4 * t617 - 0.3776D4 * t622 - 0.108D3 * t625 - 0.16D2 
     #* t630 + 0.62D2 * t636 - 0.1360D4 * t638 + 0.1728D4 * t643
      t647 = t18 * t68
      t649 = t200 * t75 * t509 * t647
      t651 = t76 * t13
      t653 = t420 * t651 * t492
      t656 = t504 * t58 * t249
      t658 = t104 * t338
      t659 = t118 * t658
      t664 = t188 * t10 * t45 * t75 * t19
      t668 = t64 * x2 * t325 * t620
      t670 = t87 * z
      t672 = t670 * t15 * t241
      t675 = t670 * t15 * t50
      t679 = t414 * t640 * t92 * t98
      t683 = t420 * t651 * t92 * t114
      t685 = t46 * t6
      t686 = t76 * t92
      t687 = t114 * t33
      t689 = t685 * t686 * t687
      t693 = t685 * t302 * t105 * x3
      t697 = t89 * t75 * t342 * t647
      t700 = t118 * t182 * t455
      t702 = t504 * t344
      t704 = t46 * t15
      t706 = t87 * t15
      t710 = t12 * t19 * t33 * t23
      t714 = t183 * t19 * t94 * t23
      t716 = 0.134D3 * t649 + 0.776D3 * t653 + 0.1728D4 * t656 - 0.544D3
     # * t659 + 0.3776D4 * t664 - 0.9344D4 * t668 + 0.256D3 * t672 - 0.2
     #56D3 * t675 - 0.864D3 * t679 + 0.432D3 * t683 - 0.64D2 * t689 + 0.
     #64D2 * t693 - 0.5266D4 * t697 + 0.5266D4 * t700 - 0.1168D4 * t702 
     #- 0.62D2 * t704 + 0.134D3 * t706 - 0.1728D4 * t710 + 0.288D3 * t71
     #4
      t740 = -0.512D3 * t36 - 0.536D3 * t52 + 0.64D2 * t61 - 0.640D3 * t
     #71 - 0.64D2 * t87 + 0.536D3 * t101 + t108 + 0.16D2 * t111 + 0.16D2
     # * t116 - 0.25296D5 * t124 + 0.28208D5 * t126 - 0.28372D5 * t130 +
     # 0.29128D5 * t132 + 0.28224D5 * t140 - 0.27920D5 * t146 + 0.74296D
     #5 * t150 - 0.128D3 * t154 - 0.4D1 * t157 + 0.252D3 * t159 - 0.7432
     #0D5 * t161
      t760 = t87 * t48
      t763 = -0.28792D5 * t164 - 0.128D3 * t166 - 0.144D3 * t171 + 0.32D
     #2 * t173 + 0.25220D5 * t180 + 0.74296D5 * t186 + 0.64D2 * t193 + 0
     #.25296D5 * t205 - 0.25248D5 * t209 + 0.128D3 * t215 - 0.128D3 * t2
     #19 - 0.80D2 * t223 - 0.29128D5 * t231 - 0.780D3 * t237 + 0.640D3 *
     # t251 + 0.640D3 * t257 - 0.252D3 * t261 + 0.4D1 * t265 - 0.912D3 *
     # t267 - 0.64D2 * t760 * t242
      t775 = t87 * t529
      t776 = t23 * t9
      t808 = 0.384D3 * t46 * t47 * t10 * t59 * z * t98 - 0.192D3 * t218 
     #* t460 * t343 * t33 + 0.192D3 * t775 * t776 * t543 - 0.96D2 * t775
     # * t776 * t479 + 0.128D3 * t281 + 0.912D3 * t286 - 0.256D3 * t291 
     #- 0.144D3 * t299 + 0.576D3 * t305 - 0.384D3 * t240 * t50 * z * t13
     # * x3 - 0.32D2 * t775 * t531 * t479 + 0.128D3 * t317 + 0.8D1 * t33
     #5 + 0.640D3 * t340 - 0.64D2 * t345 - 0.128D3 * t349 - 0.288D3 * t7
     #75 * t533 - 0.96D2 * t760 * t604 + 0.96D2 * t247 * t604 - 0.1152D4
     # * t353
      t810 = t14 * t109
      t833 = t82 * z
      t837 = 0.28792D5 * t357 + 0.32D2 * t218 * t810 * t241 + 0.32D2 * t
     #760 * t540 - 0.25264D5 * t361 - 0.64D2 * t366 - 0.320D3 * t370 - 0
     #.28208D5 * t374 + 0.27920D5 * t377 + 0.424D3 * t380 + 0.288D3 * t3
     #84 - 0.192D3 * t760 * t488 - 0.64D2 * t388 - 0.64D2 * t391 + 0.128
     #D3 * t401 + 0.128D3 * t404 + 0.64D2 * t411 + 0.320D3 * t418 - 0.20
     #8D3 * t424 - 0.160D3 * t427 - 0.384D3 * t128 * t142 * t833
      t847 = t33 * t54
      t867 = t98 * t82
      t882 = t59 * t43 * t98
      t891 = 0.384D3 * t118 * t113 * t833 - 0.768D3 * t128 * t9 * z * t5
     #0 + 0.384D3 * t121 * t122 * t847 + 0.128D3 * t2 * t4 * x1 * t50 * 
     #t54 * z * t45 - 0.384D3 * t134 * t40 * t847 + 0.96D2 * t685 * t302
     # * t23 * t109 + 0.32D2 * t103 * t658 - 0.48D2 * t103 * t168 * t867
     # + 0.288D3 * t685 * t686 * t867 - 0.416D3 * t103 * t500 + 0.48D2 *
     # t103 * t168 * t687 - 0.8D1 * t431 - 0.424D3 * t438 - 0.64D2 * t24
     #7 * t882 - 0.128D3 * t441 - 0.512D3 * t446 + 0.64D2 * t463 + 0.256
     #D3 * t469 - 0.128D3 * t473 + 0.64D2 * t475
      t922 = 0.25264D5 * t477 - 0.8D1 * t485 + 0.74288D5 * t489 + 0.576D
     #3 * t494 + 0.25248D5 * t501 - 0.640D3 * t506 + 0.640D3 * t511 - 0.
     #160D3 * t519 - 0.208D3 * t524 - 0.672D3 * t103 * t163 + 0.16D2 * t
     #685 * t302 * t109 * x3 + 0.112D3 * t685 * t686 * t169 - 0.112D3 * 
     #t103 * t170 - 0.16D2 * t103 * t168 * t114 * t98 + 0.780D3 * t527 -
     # 0.128D3 * t538 + 0.80D2 * t541 - 0.64D2 * t545 + 0.128D3 * t555 +
     # 0.28224D5 * t557
      t950 = 0.640D3 * t561 - 0.32D2 * t570 + 0.74320D5 * t573 - 0.74208
     #D5 * t579 - 0.416D3 * t760 * t51 + 0.48D2 * t247 * t43 * t532 - 0.
     #144D3 * t589 - 0.144D3 * t593 + 0.64D2 * t760 * t882 - 0.48D2 * t2
     #18 * t810 * t50 + 0.8D1 * t599 + 0.288D3 * t605 - 0.8D1 * t609 - 0
     #.28372D5 * t611 + 0.25220D5 * t613 - 0.640D3 * t617 + 0.640D3 * t6
     #22 + 0.8D1 * t625 + 0.64D2 * t630 + 0.304D3 * t636
      t973 = 0.320D3 * t679 - 0.160D3 * t683 + 0.128D3 * t689 + 0.128D3 
     #* t693 + 0.780D3 * t697 - 0.780D3 * t700 + 0.416D3 * t702 - 0.304D
     #3 * t704 + 0.304D3 * t706 + 0.640D3 * t710 - 0.74288D5 * t714
      rrqg2qgh11J6 = (wd * (t148 + t225 + t331 + t406 + t491 + t575 + t6
     #45 + t716) + wd * (t740 + t763 + t808 + t837 + t891 + t922 + t950 
     #+ 0.512D3 * t638 - 0.640D3 * t643 + 0.304D3 * t649 - 0.304D3 * t65
     #3 - 0.640D3 * t656 + 0.74208D5 * t659 - 0.640D3 * t664 + 0.1728D4 
     #* t668 - 0.128D3 * t672 + 0.512D3 * t675 + t973)) / t1 / t68 / z /
     # 0.3141592653589793D1 / 0.36D2

      end function
  
   
 

      doubleprecision function rrqg2qgh11J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t21 = t6 * t20
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t21 - t2 * t24
      t27 = s ** 2
      t28 = t27 ** 2
      t29 = t26 * t28
      t30 = t1 ** 2
      t31 = t30 * t1
      t32 = t29 * t31
      t33 = x1 ** 2
      t34 = t33 * x1
      t35 = t4 ** 2
      t36 = 0.1D1 / t35
      t37 = t34 * t36
      t41 = t10 * t7 * t4 + x2 * x3 + t19
      t42 = t20 * t41
      t43 = t37 * t42
      t46 = t26 ** 2
      t47 = t27 * s
      t48 = t46 * t47
      t49 = t48 * z
      t50 = t5 * t20
      t54 = t5 * t41
      t58 = t47 * t31
      t60 = t33 * t23
      t61 = t5 * t46
      t65 = t30 ** 2
      t66 = t65 * t1
      t67 = t28 * t66
      t69 = x2 ** 2
      t71 = t23 ** 2
      t72 = t71 * t36
      t76 = x1 * t71
      t77 = x3 * t10
      t78 = t76 * t77
      t81 = t28 * s
      t83 = t81 * t65 * x2
      t84 = t34 * t23
      t85 = t36 * t41
      t89 = t81 * t66
      t90 = t89 * x2
      t91 = t33 ** 2
      t92 = t91 * t23
      t94 = 0.1D1 / t35 / t4
      t95 = t20 ** 2
      t96 = t94 * t95
      t100 = t47 * t66
      t102 = t34 * t71
      t103 = t36 * t46
      t107 = t65 * t30
      t109 = t81 * t107 * t69
      t110 = t91 * t71
      t115 = t36 * t20
      t116 = t84 * t115
      t120 = t69 * x2
      t122 = t71 * t23
      t123 = t34 * t122
      t128 = t1 * t23
      t135 = x2 * t34
      t136 = t67 * t135
      t137 = t26 * t10
      t143 = t30 * t71
      t144 = x3 ** 2
      t148 = t10 ** 2
      t152 = t41 ** 2
      t161 = 0.25248D5 * t32 * t43 + 0.512D3 * t49 * t3 * t50 - 0.128D3 
     #* t49 * t3 * t54 + 0.1728D4 * t58 * x2 * t60 * t61 - 0.640D3 * t67
     # * t34 * t26 * t69 * t72 + 0.74208D5 * t32 * t78 - 0.640D3 * t83 *
     # t84 * t85 - 0.304D3 * t90 * t92 * t96 + 0.304D3 * t100 * t69 * t1
     #02 * t103 - 0.640D3 * t109 * t110 * t94 * t20 + 0.512D3 * t83 * t1
     #16 + 0.304D3 * t47 * t107 * t120 * t123 * t94 * t46 + 0.128D3 * t4
     #8 * t128 * x3 + 0.128D3 * t48 * t128 * t10 + 0.128D3 * t136 * t72 
     #* t137 * t41 - 0.64D2 * t48 - 0.64D2 * t48 * t143 * t144 - 0.64D2 
     #* t48 * t143 * t148 - 0.160D3 * t90 * t92 * t94 * t152 + 0.320D3 *
     # t109 * t110 * t94 * t41
      t162 = t28 * t107
      t164 = t162 * t69 * t91
      t165 = t71 * t94
      t166 = t26 * t20
      t170 = x2 * t91
      t172 = t23 * t94
      t176 = t28 * t65
      t178 = t176 * t34 * t26
      t179 = x2 * t23
      t183 = x2 * t33
      t184 = t58 * t183
      t185 = t23 * t36
      t186 = t46 * t20
      t187 = t185 * t186
      t190 = x2 * x1
      t191 = t58 * t190
      t192 = t71 * t5
      t193 = t46 * x3
      t194 = t192 * t193
      t201 = t89 * t135
      t206 = t47 * t65
      t207 = t206 * t135
      t208 = t46 * t41
      t209 = t185 * t208
      t212 = t48 * t31
      t213 = t37 * t95
      t216 = t29 * t65
      t217 = x1 * t122
      t222 = t91 * t94
      t223 = t95 * t20
      t227 = t34 * t94
      t228 = t227 * t223
      t235 = t76 * t148
      t238 = t76 * t144
      t244 = t29 * t30
      t245 = t33 * t5
      t246 = t245 * t41
      t251 = x1 * t23
      t259 = t33 * t71
      t260 = t5 * t10
      t264 = 0.128D3 * t164 * t165 * t166 + 0.640D3 * t89 * t170 * t172 
     #* t42 + 0.640D3 * t178 * t179 * t85 - 0.424D3 * t184 * t187 - 0.64
     #0D3 * t191 * t194 - 0.8D1 * t136 * t72 * t166 * t10 + 0.640D3 * t2
     #01 * t72 * t41 * x3 + 0.28224D5 * t207 * t209 - 0.672D3 * t212 * t
     #213 + 0.16D2 * t216 * t217 * t144 * x3 + 0.112D3 * t216 * t222 * t
     #223 - 0.112D3 * t212 * t228 - 0.16D2 * t212 * t227 * t152 * t41 + 
     #0.128D3 * t212 * t235 + 0.16D2 * t212 * t238 + 0.16D2 * t212 * t37
     # * t152 - 0.25296D5 * t244 * t246 + 0.640D3 * t47 * t30 * x2 * t25
     #1 * t61 - 0.640D3 * t109 * t123 * t36 * x3 - 0.640D3 * t83 * t259 
     #* t260
      t266 = t33 * t122
      t271 = t5 * t148
      t279 = t65 * x2
      t280 = t29 * t279
      t286 = t26 * z
      t295 = z ** 2
      t296 = t20 * t295
      t311 = t48 * t1
      t315 = t48 * t30
      t316 = t245 * t20
      t330 = t65 * t31
      t333 = t91 * t122
      t345 = -0.208D3 * t90 * t266 * t5 * t144 - 0.160D3 * t90 * t266 * 
     #t271 + 0.28792D5 * t207 * t172 * t46 * t95 - 0.1152D4 * t280 * t25
     #9 * t115 * t10 - 0.512D3 * t176 * t135 * t185 * t286 * t20 - 0.304
     #D3 * t29 * t3 + 0.304D3 * t48 * t3 + 0.384D3 * t244 * t245 * t296 
     #+ 0.128D3 * t28 * t1 * x1 * t50 * t295 * z * t26 + 0.96D2 * t216 *
     # t217 * t10 * t144 - 0.384D3 * t311 * t6 * t296 - 0.28372D5 * t315
     # * t316 + 0.28208D5 * t32 * t238 + 0.32D2 * t212 * t78 + 0.29128D5
     # * t244 * t316 - 0.208D3 * t81 * t31 * x2 * t60 * t5 - 0.160D3 * t
     #81 * t330 * t120 * t333 * t94 + 0.640D3 * t89 * t69 * t102 * t36 +
     # 0.25220D5 * t315 * t246 + 0.32D2 * t32 * t235
      t352 = t251 * t10
      t355 = t33 * t36
      t368 = t251 * x3
      t375 = t41 * t95
      t382 = t152 * t20
      t386 = t67 * t170
      t387 = t35 ** 2
      t404 = t69 * t34
      t405 = t100 * t404
      t410 = t176 * t33 * t26
      t411 = x2 * t71
      t415 = -0.144D3 * t32 * t228 - 0.128D3 * t311 * t21 - 0.28792D5 * 
     #t32 * t213 - 0.74320D5 * t244 * t352 + 0.252D3 * t315 * t355 * t95
     # - 0.4D1 * t315 * t355 * t152 - 0.128D3 * t315 * t71 * x3 * t10 + 
     #0.74296D5 * t315 * t352 - 0.27920D5 * t244 * t368 + 0.28224D5 * t3
     #15 * t368 - 0.416D3 * t212 * t43 + 0.288D3 * t216 * t222 * t375 - 
     #0.48D2 * t212 * t227 * t375 + 0.48D2 * t212 * t227 * t382 - 0.144D
     #3 * t386 * t23 / t387 * t26 * t223 + 0.64D2 * t386 * t172 * t26 * 
     #t152 - 0.780D3 * t184 * t209 + 0.320D3 * t109 * t123 * t36 * t10 -
     # 0.29128D5 * t405 * t165 * t186 + 0.912D3 * t410 * t411 * t260
      t418 = t26 * x3
      t425 = t206 * t183
      t432 = t31 * t33
      t433 = t29 * t432
      t434 = t54 * t24
      t438 = t48 * t30 * t23
      t457 = t23 * t10
      t458 = t50 * t457
      t464 = t46 * t10
      t473 = t31 * x1
      t476 = t50 * t71 * t148
      t479 = t206 * t190
      t480 = t122 * t5
      t486 = t100 * t69 * t33
      t487 = t122 * t36
      t494 = t26 * t95
      t498 = t67 * t183
      t503 = -0.8D1 * t136 * t72 * t418 * t20 - 0.28372D5 * t207 * t187 
     #+ 0.25220D5 * t425 * t194 - 0.512D3 * t201 * t72 * x3 * t20 + 0.25
     #264D5 * t433 * t434 - 0.8D1 * t438 * t10 * x1 * t54 - 0.320D3 * t2
     #01 * t72 * t10 * t41 - 0.28208D5 * t207 * t172 * t46 * t152 + 0.27
     #920D5 * t405 * t165 * t208 + 0.424D3 * t178 * t179 * t115 + 0.7428
     #8D5 * t433 * t458 + 0.576D3 * t280 * t84 * t96 - 0.74208D5 * t425 
     #* t72 * t464 * t41 - 0.256D3 * t386 * t172 * t166 * t41 - 0.144D3 
     #* t29 * t473 * t476 - 0.32D2 * t479 * t480 * t46 * t148 + 0.74320D
     #5 * t486 * t487 * t464 + 0.576D3 * t280 * t217 * t271 - 0.252D3 * 
     #t386 * t172 * t494 + 0.4D1 * t498 * t480 * t26 * t144
      t504 = t192 * t464
      t521 = t95 * z
      t532 = t5 * x3
      t554 = t81 * t295 * t279
      t555 = t259 * t532
      t559 = t48 * t31 * t71
      t579 = -0.912D3 * t191 * t504 - 0.74288D5 * t425 * t72 * t186 * t1
     #0 - 0.128D3 * t162 * t404 * t487 * t137 + 0.74296D5 * t425 * t504 
     #- 0.768D3 * t315 * t33 * z * t50 + 0.384D3 * t32 * t37 * t521 - 0.
     #384D3 * t315 * t355 * t521 + 0.64D2 * t498 * t480 * t26 * t148 + 0
     #.780D3 * t410 * t411 * t532 + 0.256D3 * t438 * x3 * x1 * t50 + 0.1
     #28D3 * t176 * t183 * t192 * t286 * x3 + 0.8D1 * t498 * t480 * t418
     # * t10 + 0.640D3 * t89 * t183 * t480 * t77 - 0.64D2 * t554 * t555 
     #- 0.128D3 * t559 * t144 * x1 * t50 + 0.64D2 * t559 * t77 * t21 + 0
     #.128D3 * t48 * t473 * t476 - 0.128D3 * t410 * t476 - 0.80D2 * t479
     # * t480 * t193 * t10 + 0.25296D5 * t486 * t487 * t193
      t589 = t457 * t54
      t592 = t85 * t20
      t596 = t71 * t10
      t609 = t71 * t144
      t614 = t48 * t31 * t23
      t615 = x3 * t33
      t616 = t36 * t152
      t620 = t10 * t33
      t625 = t48 * t30 * x1
      t641 = t48 * t432
      t642 = t24 * t50
      t645 = t36 * t95
      t650 = t115 * t24 * t41
      t659 = t645 * t457
      t664 = -0.25248D5 * t425 * t72 * t193 * t20 - 0.128D3 * t559 * t14
     #8 * x1 * t54 + 0.80D2 * t433 * t589 - 0.64D2 * t178 * t457 * t592 
     #+ 0.64D2 * t410 * t596 * t532 * t41 - 0.25264D5 * t425 * t72 * t20
     #8 * x3 - 0.64D2 * t559 * t77 * t6 * t41 + 0.32D2 * t410 * t609 * t
     #54 - 0.32D2 * t614 * t615 * t616 - 0.96D2 * t614 * t620 * t616 - 0
     #.384D3 * t625 * t50 * z * t23 * x3 + 0.384D3 * t29 * t31 * t34 * t
     #115 * z * t41 - 0.192D3 * t410 * t596 * t532 * t20 - 0.416D3 * t64
     #1 * t642 + 0.48D2 * t178 * t24 * t645 + 0.64D2 * t641 * t650 - 0.4
     #8D2 * t410 * t609 * t50 - 0.288D3 * t614 * t615 * t645 - 0.96D2 * 
     #t641 * t659 + 0.96D2 * t178 * t659
      t731 = -0.192D3 * t641 * t458 + 0.288D3 * t433 * t659 + 0.64D2 * t
     #28 * t330 * t120 * t333 * t94 * t26 + 0.8D1 * t315 * t355 * t42 + 
     #0.416D3 * t83 * t555 + 0.64D2 * t554 * t116 - 0.536D3 * t433 * t64
     #2 - 0.780D3 * t32 * t183 * t23 * t5 + 0.780D3 * t206 * t69 * t259 
     #* t103 + 0.128D3 * t216 * t217 * t148 * x3 + 0.128D3 * t216 * t222
     # * t382
      rrqg2qgh11J7 = wd * (t161 + t264 + t345 + t415 + t503 + t579 + t66
     #4 - 0.64D2 * t178 * t650 + 0.192D3 * t614 * t620 * t592 + 0.32D2 *
     # t641 * t589 + 0.536D3 * t207 * t172 * t186 * t41 - 0.128D3 * t164
     # * t165 * t26 * t41 - 0.64D2 * t641 * t434 + 0.640D3 * t201 * t72 
     #* t20 * t10 + 0.288D3 * t136 * t165 * t494 * t10 - 0.144D3 * t498 
     #* t487 * t166 * t148 + 0.8D1 * t625 * t458 + t731) / t27 / t46 / z
     # / 0.3141592653589793D1 / 0.36D2

      end function
  
 