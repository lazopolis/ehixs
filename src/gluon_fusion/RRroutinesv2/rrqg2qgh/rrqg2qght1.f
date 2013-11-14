  
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
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = 0.1D1 / s
      t7 = pi * t6
      t8 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = x2 * t11 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x3
      t20 = -0.1D1 + x2
      t22 = t19 * t4 * t20
      t25 = log(0.4D1 * t16 * t22)
      t26 = t25 ** 2
      t27 = t19 * t4
      t30 = log(-0.4D1 * t16 * t27)
      t31 = t30 ** 2
      t37 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t40 = pi * lh
      t41 = t6 * t8
      t48 = 0.1D1 / x2
      t51 = t11 * t15
      t54 = log(-0.4D1 * t51 * t27)
      t55 = t54 * pi
      t58 = t54 ** 2
      t59 = t58 * pi
      t61 = lh ** 2
      t63 = pi ** 2
      t65 = 0.180D3 * t61 - 0.30D2 * t63
      t66 = pi * t65
      t71 = rrqg2qgh11J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t95 = rrqg2qgh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t98 = x1 ** 2
      t99 = x2 * t98
      t103 = log(0.4D1 * t99 * t51 * t22)
      t107 = x3 * t4
      t111 = log(-0.4D1 * t99 * t15 * t11 * t18 * t107)
      t115 = 0.1D1 / x1
      t119 = t98 * t15
      t123 = log(-0.4D1 * t119 * t11 * t27)
      t125 = t123 ** 2
      t140 = (0.90D2 * t7 * t8 * (t26 / 0.2D1 - t31 / 0.2D1) + (0.90D2 *
     # t7 * t37 - 0.180D3 * t40 * t41) * (-t25 + t30)) * t48 / 0.1440D4 
     #- (0.180D3 * t55 * lh + 0.45D2 * t59 + t66) * t6 * t37 / 0.1440D4 
     #- t7 * t71 / 0.16D2 - (-0.90D2 * t59 * lh + pi * (0.60D2 * lh * t6
     #3 - 0.240D3 * zeta3 - 0.120D3 * t61 * lh) - 0.15D2 * t58 * t54 * p
     #i - t55 * t65) * t6 * t8 / 0.1440D4 - (-0.180D3 * t40 - 0.90D2 * t
     #55) * t6 * t95 / 0.1440D4 - t7 * (t103 * t8 - t111 * t8) * t48 * t
     #115 / 0.8D1 - (0.90D2 * t7 * (t95 - t123 * t37 + t125 * t8 / 0.2D1
     #) - 0.180D3 * t40 * t6 * (t37 - t123 * t8) + t66 * t41) * t115 / 0
     #.720D3
      t141 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t140)
      t143 = x3 * x1
      t144 = t143 * t2
      t145 = -0.1D1 + x1
      t147 = t2 * t145 * x3
      t149 = t4 * x1 * t2
      t151 = t2 * t145 * t4
      t152 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, t144, -t149, -t
     #147, t151, 0.0D0)
      t153 = 0.1D1 / t9
      t154 = t15 * t153
      t156 = x1 * z
      t157 = -z - x1 + t156
      t158 = 0.1D1 / t157
      t160 = t145 ** 2
      t165 = log(0.4D1 * t99 * t154 * t19 * t4 * t158 * t160)
      t166 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t149, -t
     #147, t151, 0.0D0)
      t171 = t6 * t166
      t177 = rrqg2qgh11J3(s, XB1, XB2, z, lh, wd, nf, s, t144, -t149, -t
     #147, t151, 0.0D0)
      t180 = t158 * t160
      t184 = log(0.4D1 * t119 * t153 * t18 * t107 * t180)
      t186 = t184 ** 2
      t201 = -(0.90D2 * t7 * (-t152 + t165 * t166) + 0.180D3 * t40 * t17
     #1) * t48 * t115 / 0.720D3 - (-0.90D2 * t7 * (t177 - t184 * t152 + 
     #t186 * t166 / 0.2D1) + 0.180D3 * t40 * t6 * (t152 - t184 * t166) -
     # t66 * t171) * t115 / 0.720D3
      t202 = FJET(XB1, XB2, s, t144, -t147, -t149, t151, 0.0D0, t201)
      t204 = x3 * z
      t205 = t143 * z
      t206 = x2 * x3
      t207 = t206 * z
      t208 = t206 * x1
      t209 = t206 * t156
      t210 = cos(t13)
      t215 = Sqrt(-x3 * t20 * t157 * x2 * t4)
      t217 = 0.2D1 * t210 * t215
      t221 = t2 * x1 * (-t204 - t143 + t205 + t207 + t208 - t209 - x2 + 
     #t206 + t217) * t158
      t223 = x2 * x1
      t225 = z + x1 - t156 - x2 * z - t223 + t223 * z - t204 - t143 + t2
     #05 + t207 + t208 - t209 + t206 + t217
      t228 = t2 * x1 * t225 * t158
      t233 = s * t17 * x2 * x1 * t145 * t158
      t234 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, t221, -t228, -t
     #147, t151, t233)
      t241 = log(-0.4D1 * t99 * t154 * t18 * t107 * t180 * t20)
      t242 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t221, -t228, -t
     #147, t151, t233)
      t250 = 0.90D2 * t7 * (t234 - t241 * t242) - 0.180D3 * t40 * t6 * t
     #242
      t254 = FJET(XB1, XB2, s, t221, -t147, -t228, t151, t233, -t250 * t
     #48 * t115 / 0.720D3)
      rrqg2qght1s1e1 = t141 * t140 + t202 * t201 - t254 * t250 * t48 * t
     #115 / 0.720D3

      end function



      doubleprecision function rrqg2qght1s1e0
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
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = 0.1D1 / s
      t7 = pi * t6
      t8 = rrqg2qgh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t11 = pi * lh
      t13 = z ** 2
      t15 = 0.1D1 / t13 / z
      t16 = x4 * pi
      t17 = Sin(t16)
      t18 = t17 ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t21 * x3
      t23 = t22 * t4
      t26 = log(-0.4D1 * t15 * t18 * t23)
      t27 = t26 * pi
      t31 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t36 = t26 ** 2
      t39 = lh ** 2
      t41 = pi ** 2
      t47 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t51 = x2 * t15 * t18
      t52 = -0.1D1 + x2
      t57 = log(0.4D1 * t51 * t22 * t4 * t52)
      t60 = log(-0.4D1 * t51 * t23)
      t63 = 0.1D1 / x2
      t67 = x1 ** 2
      t68 = t67 * t18
      t72 = log(-0.4D1 * t68 * t15 * t23)
      t81 = 0.1D1 / x1
      t84 = -t7 * t8 / 0.16D2 - (-0.180D3 * t11 - 0.90D2 * t27) * t6 * t
     #31 / 0.1440D4 - (0.180D3 * t27 * lh + 0.45D2 * t36 * pi + pi * (0.
     #180D3 * t39 - 0.30D2 * t41)) * t6 * t47 / 0.1440D4 + t7 * t47 * (-
     #t57 + t60) * t63 / 0.16D2 - (0.90D2 * t7 * (t31 - t72 * t47) - 0.1
     #80D3 * t11 * t6 * t47) * t81 / 0.720D3
      t85 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t84)
      t87 = x3 * x1
      t88 = t87 * t2
      t89 = -0.1D1 + x1
      t91 = t2 * t89 * x3
      t93 = t4 * x1 * t2
      t95 = t2 * t89 * t4
      t96 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, -t91,
     # t95, 0.0D0)
      t101 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, -t91
     #, t95, 0.0D0)
      t106 = x1 * z
      t107 = -z - x1 + t106
      t108 = 0.1D1 / t107
      t109 = t89 ** 2
      t114 = log(0.4D1 * t68 / t13 * t21 * x3 * t4 * t108 * t109)
      t125 = t7 * t96 * t63 * t81 / 0.8D1 - (-0.90D2 * t7 * (t101 - t114
     # * t96) + 0.180D3 * t11 * t6 * t96) * t81 / 0.720D3
      t126 = FJET(XB1, XB2, s, t88, -t91, -t93, t95, 0.0D0, t125)
      t128 = x3 * z
      t129 = t87 * z
      t130 = x2 * x3
      t131 = t130 * z
      t132 = t130 * x1
      t133 = t130 * t106
      t134 = cos(t16)
      t139 = Sqrt(-x3 * t52 * t107 * x2 * t4)
      t141 = 0.2D1 * t134 * t139
      t145 = t2 * x1 * (-t128 - t87 + t129 + t131 + t132 - t133 - x2 + t
     #130 + t141) * t108
      t147 = x2 * x1
      t149 = z + x1 - t106 - x2 * z - t147 + t147 * z - t128 - t87 + t12
     #9 + t131 + t132 - t133 + t130 + t141
      t152 = t2 * x1 * t149 * t108
      t157 = s * t20 * x2 * x1 * t89 * t108
      t158 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t145, -t152, -t
     #91, t95, t157)
      t160 = t158 * t63 * t81
      t163 = FJET(XB1, XB2, s, t145, -t91, -t152, t95, t157, -t7 * t160 
     #/ 0.8D1)
      rrqg2qght1s1e0 = t85 * t84 + t126 * t125 - t163 * pi * t6 * t160 /
     # 0.8D1

      end function



      doubleprecision function rrqg2qght1s1em1
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
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = 0.1D1 / s
      t7 = pi * t6
      t8 = rrqg2qgh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t13 = z ** 2
      t17 = Sin(x4 * pi)
      t18 = t17 ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t26 = log(-0.4D1 / t13 / z * t18 * t21 * x3 * t4)
      t31 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t34 = 0.1D1 / x1
      t38 = -t7 * t8 / 0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t26 * pi)
     # * t6 * t31 / 0.1440D4 - t7 * t31 * t34 / 0.8D1
      t39 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t38)
      t42 = x3 * x1 * t2
      t43 = -0.1D1 + x1
      t45 = t2 * t43 * x3
      t47 = t4 * x1 * t2
      t49 = t2 * t43 * t4
      t50 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, t42, -t47, -t45,
     # t49, 0.0D0)
      t54 = FJET(XB1, XB2, s, t42, -t45, -t47, t49, 0.0D0, t7 * t50 * t3
     #4 / 0.8D1)
      rrqg2qght1s1em1 = t39 * t38 + t54 * pi * t6 * t50 * t34 / 0.8D1

      end function



      doubleprecision function rrqg2qght1s1em2
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
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = t2 * x3
      t5 = t2 * (-0.1D1 + x3)
      t6 = 0.1D1 / s
      t8 = rrqg2qgh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -pi * t6 * t
     #8 / 0.16D2)
      rrqg2qght1s1em2 = -t11 * pi * t6 * t8 / 0.16D2

      end function



      doubleprecision function rrqg2qght1s1em3
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
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght1s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght1s1em4
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
      doubleprecision rrqg2qgh11J1
      doubleprecision rrqg2qgh11J2
      doubleprecision rrqg2qgh11J3
      doubleprecision rrqg2qgh11J4
      doubleprecision rrqg2qgh11J5
      doubleprecision rrqg2qgh11J6
      doubleprecision rrqg2qgh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh11J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

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
      t1 = S12 + S13 + S23
      t2 = 0.1D1 / t1
      t4 = 0.1D1 / S12
      t7 = -S23 + S13
      t9 = t1 ** 2
      t10 = 0.1D1 / t9
      t12 = S13 + S14 + S34
      t14 = S12 ** 2
      t15 = 0.1D1 / t14
      t16 = t12 * S34 * t15
      t19 = s ** 2
      t21 = z ** 2
      t23 = t2 * t12
      t40 = t10 * t12
      t42 = 0.16D2 * t2
      t46 = 0.4D1 * S24
      t48 = 0.7D1 / 0.9D1 * S13
      t56 = S34 ** 2
      t73 = 0.82D2 / 0.9D1 * S24
      t75 = 0.91D2 / 0.18D2 * S13
      t82 = 0.37D2 / 0.9D1 * S13
      t83 = 0.20D2 / 0.3D1 * S24
      t84 = S13 * S24
      t85 = 0.28D2 / 0.9D1 * t84
      t86 = S23 * S24
      t88 = S24 ** 2
      t89 = 0.6D1 * t88
      t90 = S13 ** 2
      t91 = S23 * S13
      t93 = S23 ** 2
      t99 = t56 * S34
      t116 = 0.136D3 / 0.3D1 * S24
      t119 = 0.1733D4 / 0.18D2 * S13
      t122 = S24 * S14
      t129 = S14 * S23
      t131 = S14 ** 2
      t134 = S13 * S14
      t144 = 0.32D2 / 0.9D1 * t84
      t152 = 0.44D2 / 0.3D1 * S24
      t153 = 0.275D3 / 0.36D2 * S13
      t156 = 0.100D3 / 0.9D1 * t84
      t157 = 0.14D2 / 0.9D1 * t88
      t161 = 0.16D2 / 0.3D1 * t90
      t170 = 0.47D2 / 0.36D2 * t90
      t173 = 0.8D1 / 0.3D1 * t129
      t174 = 0.26D2 / 0.3D1 * t88
      t176 = 0.4D1 * t90 * S24
      t179 = 0.4D1 * t88 * S24
      t188 = 0.7D1 / 0.9D1 * t90 * S13
      t192 = 0.14D2 / 0.3D1 * S13 * t88
      t195 = (0.68D2 / 0.9D1 * t40 - t42) * t99 + ((0.311D3 / 0.9D1 * t2
     # + (0.136D3 / 0.9D1 * S24 - 0.272D3 / 0.9D1 * S23 - 0.272D3 / 0.9D
     #1 * S13 + 0.136D3 / 0.9D1 * S14) * t10) * t12 - 0.3523D4 / 0.36D2 
     #+ (0.32D2 * S14 - 0.24D2 * S23) * t2) * t56 + ((0.658D3 / 0.3D1 + 
     #(t116 - 0.3523D4 / 0.36D2 * S23 + 0.311D3 / 0.9D1 * S14 - t119) * 
     #t2 + (0.136D3 / 0.9D1 * t122 + 0.94D2 / 0.9D1 * t93 + 0.118D3 / 0.
     #9D1 * t90 + 0.212D3 / 0.9D1 * t91 - 0.272D3 / 0.9D1 * t84 - 0.272D
     #3 / 0.9D1 * t86 - 0.272D3 / 0.9D1 * t129 + 0.68D2 / 0.9D1 * t131 +
     # 0.68D2 / 0.9D1 * t88 - 0.272D3 / 0.9D1 * t134) * t10) * t12 + t11
     #6 - 0.3523D4 / 0.36D2 * S14 + 0.311D3 / 0.9D1 * S23 - t119 + (-0.1
     #6D2 * t93 + 0.56D2 / 0.9D1 * t88 - t144 - 0.16D2 * t90 + 0.24D2 * 
     #t129 - 0.16D2 * t131) * t2) * S34 + (t152 + t153 - 0.17D2 / 0.36D2
     # * S14 + 0.17D2 / 0.2D1 * S23 + (-t156 - t157 - 0.146D3 / 0.9D1 * 
     #t86 - 0.265D3 / 0.18D2 * t91 - 0.215D3 / 0.18D2 * t93 - t161) * t2
     #) * t12 - 0.32D2 / 0.9D1 * t122 + 0.47D2 / 0.18D2 * t134 + 0.8D1 /
     # 0.3D1 * t91 - 0.47D2 / 0.36D2 * t131 - t170 - 0.220D3 / 0.9D1 * t
     #86 - 0.187D3 / 0.9D1 * t93 + t144 - t173 - t174 + (-t176 - t93 * S
     #23 - t179 + 0.28D2 / 0.9D1 * t86 * S13 - 0.4D1 * S24 * t93 - S23 *
     # t90 + 0.7D1 / 0.9D1 * t93 * S13 + t188 - 0.6D1 * t88 * S23 + t192
     #) * t2
      t197 = t56 ** 2
      t253 = (-0.48D2 * S34 * t2 * t4 + 0.16D2 / 0.9D1 * t7 * t10 * t16)
     # * t19 * t21 + (((0.48D2 * t23 + 0.48D2 - 0.48D2 * S13 * t2) * S34
     # + 0.32D2 / 0.9D1 * S14 - 0.32D2 / 0.9D1 * S13) * t4 - 0.32D2 / 0.
     #9D1 * t7 * t2 * t16) * s * z + t2 * t14 + ((0.94D2 / 0.9D1 * t40 -
     # t42) * S34 - 0.5D1 / 0.18D2 * t23 + 0.1D1 / 0.3D1 + (-t46 - 0.3D1
     # * S23 + t48) * t2) * S12 + (-0.272D3 / 0.9D1 * t40 + 0.24D2 * t2)
     # * t56 + ((-0.3523D4 / 0.36D2 * t2 + (0.188D3 / 0.9D1 * S23 + 0.21
     #2D3 / 0.9D1 * S13 - 0.272D3 / 0.9D1 * S14 - 0.272D3 / 0.9D1 * S24)
     # * t10) * t12 + 0.311D3 / 0.9D1 + (0.32D2 * S23 - 0.24D2 * S14) * 
     #t2) * S34 + (0.61D2 / 0.18D2 + (-t73 - 0.82D2 / 0.9D1 * S23 - t75)
     # * t2) * t12 - 0.4D1 / 0.9D1 * S23 - 0.37D2 / 0.9D1 * S14 + t82 + 
     #t83 + (-t85 + 0.8D1 * t86 + t89 + t90 - 0.14D2 / 0.9D1 * t91 + 0.3
     #D1 * t93) * t2 + t195 * t4 + (t2 * t197 + (t23 / 0.3D1 - 0.5D1 / 0
     #.18D2 + (-t46 + t48 - 0.3D1 * S14) * t2) * t99 + ((0.61D2 / 0.18D2
     # + (-0.37D2 / 0.9D1 * S23 - 0.4D1 / 0.9D1 * S14 + t83 + t82) * t2)
     # * t12 - t75 - t73 - 0.82D2 / 0.9D1 * S14 + (-0.14D2 / 0.9D1 * t13
     #4 - t85 + t90 + 0.3D1 * t131 + t89 + 0.8D1 * t122) * t2) * t56 + (
     #(0.17D2 / 0.2D1 * S14 + t152 + t153 - 0.17D2 / 0.36D2 * S23 + (0.8
     #D1 / 0.3D1 * t134 - 0.220D3 / 0.9D1 * t122 - t174 - 0.32D2 / 0.9D1
     # * t86 - t173 + t144 + 0.47D2 / 0.18D2 * t91 - t170 - 0.47D2 / 0.3
     #6D2 * t93 - 0.187D3 / 0.9D1 * t131) * t2) * t12 - t156 - 0.146D3 /
     # 0.9D1 * t122 - 0.215D3 / 0.18D2 * t131 - 0.265D3 / 0.18D2 * t134 
     #- t157 - t161 + (-0.6D1 * t88 * S14 - 0.4D1 * S24 * t131 - t131 * 
     #S14 + 0.28D2 / 0.9D1 * t122 * S13 - t176 + t192 + t188 + 0.7D1 / 0
     #.9D1 * t131 * S13 - S14 * t90 - t179) * t2) * S34 + (0.32D2 / 0.9D
     #1 * t129 - 0.32D2 / 0.9D1 * t84) * t12) * t15
      rrqg2qgh11J1 = t253 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh11J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

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
      t1 = -S13 + S23
      t3 = S12 + S13 + S23
      t4 = t3 ** 2
      t5 = 0.1D1 / t4
      t7 = S13 + S14 + S34
      t9 = S12 ** 2
      t10 = 0.1D1 / t9
      t12 = s ** 2
      t13 = z ** 2
      t19 = 0.1D1 / S12
      t23 = 0.1D1 / t3
      t32 = t5 * t7
      t34 = 0.16D2 * t23
      t37 = t23 * t7
      t39 = 0.4D1 * S24
      t41 = 0.7D1 / 0.9D1 * S13
      t49 = S34 ** 2
      t66 = 0.2D1 * S24
      t68 = 0.37D2 / 0.18D2 * S13
      t75 = 0.17D2 / 0.9D1 * S13
      t76 = 0.76D2 / 0.9D1 * S24
      t78 = S23 * S24
      t80 = S23 ** 2
      t82 = S24 ** 2
      t83 = 0.4D1 * t82
      t84 = S13 ** 2
      t85 = S13 * S24
      t86 = 0.8D1 / 0.9D1 * t85
      t87 = S23 * S13
      t93 = t49 * S34
      t110 = 0.796D3 / 0.9D1 * S24
      t113 = 0.467D3 / 0.2D1 * S13
      t116 = S24 * S14
      t123 = S14 * S23
      t125 = S14 ** 2
      t128 = S13 * S14
      t147 = 0.56D2 / 0.9D1 * S24
      t148 = 0.119D3 / 0.36D2 * S13
      t150 = 0.28D2 / 0.9D1 * t85
      t153 = 0.20D2 / 0.9D1 * t82
      t154 = 0.64D2 / 0.9D1 * t123
      t155 = 0.9D1 / 0.2D1 * t84
      t164 = 0.136D3 / 0.9D1 * t123
      t166 = 0.92D2 / 0.9D1 * t85
      t172 = 0.7D1 / 0.36D2 * t84
      t173 = 0.4D1 / 0.3D1 * t82
      t174 = S13 * t82
      t175 = 0.44D2 / 0.9D1 * t174
      t177 = 0.7D1 / 0.9D1 * t84 * S13
      t178 = t78 * S13
      t181 = 0.28D2 / 0.9D1 * t84 * S24
      t183 = t80 * S13
      t186 = t82 * S23
      t192 = (-0.40D2 / 0.9D1 * t32 - t34) * t93 + ((-0.211D3 / 0.3D1 * 
     #t23 + (-0.80D2 / 0.9D1 * S24 + 0.160D3 / 0.9D1 * S23 + 0.160D3 / 0
     #.9D1 * S13 - 0.80D2 / 0.9D1 * S14) * t5) * t7 - 0.8789D4 / 0.36D2 
     #+ (0.32D2 * S14 - 0.24D2 * S23) * t23) * t49 + ((0.4310D4 / 0.9D1 
     #+ (-t110 - 0.8789D4 / 0.36D2 * S23 - 0.211D3 / 0.3D1 * S14 - t113)
     # * t23 + (-0.80D2 / 0.9D1 * t116 - 0.52D2 / 0.9D1 * t80 - 0.76D2 /
     # 0.9D1 * t84 - 0.128D3 / 0.9D1 * t87 + 0.160D3 / 0.9D1 * t85 + 0.1
     #60D3 / 0.9D1 * t78 + 0.160D3 / 0.9D1 * t123 - 0.40D2 / 0.9D1 * t12
     #5 - 0.40D2 / 0.9D1 * t82 + 0.160D3 / 0.9D1 * t128) * t5) * t7 - t1
     #10 - 0.8789D4 / 0.36D2 * S14 - 0.211D3 / 0.3D1 * S23 - t113 + (-0.
     #16D2 * t80 - 0.32D2 * t82 + 0.224D3 / 0.9D1 * t85 - 0.56D2 / 0.9D1
     # * t84 + 0.24D2 * t123 - 0.16D2 * t125) * t23) * S34 + (0.29D2 / 0
     #.9D1 * S23 - t147 + t148 + 0.473D3 / 0.36D2 * S14 + (-t150 - 0.10D
     #2 / 0.9D1 * t78 - 0.64D2 / 0.9D1 * t128 - t153 - t154 - t155 - 0.4
     #3D2 / 0.18D2 * t87 + 0.28D2 / 0.9D1 * t80 - 0.6D1 * t116 - 0.34D2 
     #/ 0.9D1 * t125) * t23) * t7 + t164 - 0.136D3 / 0.9D1 * t87 - t166 
     #+ 0.92D2 / 0.9D1 * t116 - 0.20D2 / 0.3D1 * t78 - 0.7D1 / 0.18D2 * 
     #t128 - 0.13D2 / 0.3D1 * t80 + 0.7D1 / 0.36D2 * t125 + t172 - t173 
     #+ (-t175 - t177 - 0.8D1 / 0.9D1 * t178 + t181 - t80 * S23 + 0.7D1 
     #/ 0.9D1 * t183 + S23 * t84 - 0.4D1 * t186 - 0.4D1 * S24 * t80) * t
     #23
      t194 = t49 ** 2
      t236 = t125 * S13
      t238 = t116 * S13
      t243 = t82 * S14
      t247 = (0.29D2 / 0.9D1 * S14 - t147 + t148 + 0.473D3 / 0.36D2 * S2
     #3 + (-0.7D1 / 0.18D2 * t87 + t172 - 0.136D3 / 0.9D1 * t128 - t173 
     #- t166 - 0.20D2 / 0.3D1 * t116 + 0.92D2 / 0.9D1 * t78 + 0.7D1 / 0.
     #36D2 * t80 + t164 - 0.13D2 / 0.3D1 * t125) * t23) * t7 - t150 - 0.
     #43D2 / 0.18D2 * t128 - 0.34D2 / 0.9D1 * t80 - t154 + 0.28D2 / 0.9D
     #1 * t125 - 0.6D1 * t78 - 0.64D2 / 0.9D1 * t87 - 0.10D2 / 0.9D1 * t
     #116 - t153 - t155 + (-t177 - t125 * S14 + 0.7D1 / 0.9D1 * t236 - 0
     #.8D1 / 0.9D1 * t238 - 0.4D1 * S24 * t125 + t181 + S14 * t84 - 0.4D
     #1 * t243 - t175) * t23
      t254 = 0.16D2 / 0.9D1 * t174
      t257 = 0.32D2 / 0.9D1 * t78 * S14
      t271 = t23 * t194 + (0.7D1 / 0.9D1 * t37 + 0.13D2 / 0.9D1 + (-t39 
     #+ t41 - 0.3D1 * S14) * t23) * t93 + ((-0.1D1 / 0.3D1 + (-t75 + t76
     # + 0.17D2 / 0.9D1 * S23 + 0.148D3 / 0.9D1 * S14) * t23) * t7 - 0.6
     #4D2 / 0.9D1 * S23 - t68 + 0.23D2 / 0.3D1 * S14 - t66 + (-0.14D2 / 
     #0.9D1 * t128 - t84 + 0.3D1 * t125 + t83 + t86 + 0.8D1 * t116) * t2
     #3) * t49 + t247 * S34 + (0.16D2 / 0.9D1 * t80 - 0.32D2 / 0.9D1 * t
     #85 + 0.32D2 / 0.9D1 * t123 + 0.16D2 / 0.9D1 * t125 - 0.32D2 / 0.9D
     #1 * t82 + (-t254 - 0.32D2 / 0.9D1 * t238 + t257 + 0.16D2 / 0.9D1 *
     # t186 - 0.16D2 / 0.9D1 * t236 + 0.16D2 / 0.9D1 * S23 * t125) * t23
     #) * t7 + t257 - 0.32D2 / 0.9D1 * t178 - t254 + 0.16D2 / 0.9D1 * t2
     #43 - 0.16D2 / 0.9D1 * t183 + 0.16D2 / 0.9D1 * t80 * S14
      t273 = 0.16D2 / 0.9D1 * t1 * t5 * t7 * S34 * t10 * t12 * t13 + ((-
     #0.32D2 / 0.9D1 * S14 + 0.32D2 / 0.9D1 * S13) * t19 - 0.32D2 / 0.9D
     #1 * t1 * t23 * t7 * S34 * t10) * s * z + t23 * t9 + ((-0.52D2 / 0.
     #9D1 * t32 - t34) * S34 + 0.13D2 / 0.9D1 * t37 + 0.7D1 / 0.9D1 + (-
     #t39 - 0.3D1 * S23 + t41) * t23) * S12 + (0.160D3 / 0.9D1 * t32 + 0
     #.24D2 * t23) * t49 + ((-0.8789D4 / 0.36D2 * t23 + (0.160D3 / 0.9D1
     # * S14 - 0.104D3 / 0.9D1 * S23 + 0.160D3 / 0.9D1 * S24 - 0.128D3 /
     # 0.9D1 * S13) * t5) * t7 - 0.211D3 / 0.3D1 + (0.32D2 * S23 - 0.24D
     #2 * S14) * t23) * S34 + (-0.1D1 / 0.3D1 + (-t66 + 0.23D2 / 0.3D1 *
     # S23 - t68 - 0.64D2 / 0.9D1 * S14) * t23) * t7 + 0.17D2 / 0.9D1 * 
     #S14 - t75 + t76 + 0.148D3 / 0.9D1 * S23 + (0.8D1 * t78 + 0.3D1 * t
     #80 + t83 - t84 + t86 - 0.14D2 / 0.9D1 * t87) * t23 + t192 * t19 + 
     #t271 * t10
      rrqg2qgh11J2 = t273 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh11J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S13 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t14 = S13 + S14 + S34
      t21 = 0.1D1 / S12
      t28 = S13 ** 2
      t29 = S13 * S14
      t35 = S23 * S13
      t46 = t2 * t14
      t48 = 0.4D1 * S24
      t50 = 0.7D1 / 0.9D1 * S13
      t55 = S34 ** 2
      t65 = 0.14527D5 / 0.18D2 * S13
      t66 = 0.18598D5 / 0.9D1 * S24
      t73 = 0.5D1 / 0.3D1 * S13
      t74 = 0.44D2 / 0.9D1 * S24
      t77 = S23 * S24
      t79 = S23 ** 2
      t81 = S24 ** 2
      t82 = 0.4D1 * t81
      t83 = S13 * S24
      t84 = 0.8D1 / 0.9D1 * t83
      t88 = t55 * S34
      t99 = 0.3991D4 / 0.18D2 * S13
      t100 = 0.568D3 / 0.9D1 * S24
      t111 = S14 * S23
      t113 = S14 ** 2
      t121 = 0.18518D5 / 0.9D1 * S24
      t122 = 0.28253D5 / 0.36D2 * S13
      t124 = 0.6200D4 / 0.3D1 * t83
      t127 = 0.28D2 / 0.9D1 * t81
      t128 = 0.6380D4 / 0.9D1 * t111
      t129 = 0.14315D5 / 0.18D2 * t28
      t132 = S24 * S14
      t139 = 0.136D3 / 0.9D1 * t111
      t141 = 0.10D2 * t83
      t147 = 0.259D3 / 0.36D2 * t28
      t148 = S13 * t81
      t149 = 0.8D1 / 0.9D1 * t148
      t150 = t28 * S13
      t151 = 0.29D2 / 0.9D1 * t150
      t152 = t77 * S13
      t154 = t28 * S24
      t155 = 0.44D2 / 0.9D1 * t154
      t156 = t79 * S23
      t157 = t79 * S13
      t159 = S23 * t28
      t160 = t81 * S23
      t162 = S24 * t79
      t166 = -0.16D2 * t2 * t88 + (-0.473D3 / 0.9D1 * t46 - 0.8009D4 / 0
     #.36D2 + (0.32D2 * S14 - 0.24D2 * S23) * t2) * t55 + ((0.3878D4 / 0
     #.9D1 + (-0.8009D4 / 0.36D2 * S23 - t99 - t100 - 0.473D3 / 0.9D1 * 
     #S14) * t2) * t14 - 0.473D3 / 0.9D1 * S23 - 0.8009D4 / 0.36D2 * S14
     # - t100 - t99 + (-0.16D2 * t79 - 0.64D2 / 0.9D1 * t83 - 0.16D2 * t
     #81 + 0.24D2 * t111 - 0.16D2 * t113 + 0.88D2 / 0.9D1 * t28) * t2) *
     # S34 + (0.7085D4 / 0.9D1 * S23 + t121 - t122 + 0.25693D5 / 0.36D2 
     #* S14 + (-t124 - 0.18562D5 / 0.9D1 * t77 - 0.6376D4 / 0.9D1 * t29 
     #- t127 - t128 + t129 + 0.25D2 / 0.2D1 * t35 - 0.7024D4 / 0.9D1 * t
     #79 - 0.74D2 / 0.9D1 * t132 - 0.34D2 / 0.9D1 * t113) * t2) * t14 + 
     #t139 - 0.8D1 * t35 - t141 + 0.10D2 * t132 - 0.92D2 / 0.9D1 * t77 -
     # t29 / 0.6D1 - 0.55D2 / 0.9D1 * t79 + t113 / 0.12D2 + t147 - t127 
     #+ (-t149 + t151 - 0.8D1 / 0.9D1 * t152 - t155 - t156 + 0.7D1 / 0.9
     #D1 * t157 + t159 - 0.4D1 * t160 - 0.4D1 * t162) * t2
      t168 = t55 ** 2
      t208 = t113 * S14
      t209 = t113 * S13
      t211 = t132 * S13
      t213 = S24 * t113
      t215 = S14 * t28
      t216 = t81 * S14
      t220 = (0.7085D4 / 0.9D1 * S14 + t121 - t122 + 0.25693D5 / 0.36D2 
     #* S23 + (-t35 / 0.6D1 + t147 - 0.8D1 * t29 - t127 - t141 - 0.92D2 
     #/ 0.9D1 * t132 + 0.10D2 * t77 + t79 / 0.12D2 + t139 - 0.55D2 / 0.9
     #D1 * t113) * t2) * t14 - t124 + 0.25D2 / 0.2D1 * t29 - 0.34D2 / 0.
     #9D1 * t79 - t128 - 0.7024D4 / 0.9D1 * t113 - 0.74D2 / 0.9D1 * t77 
     #- 0.6376D4 / 0.9D1 * t35 - 0.18562D5 / 0.9D1 * t132 - t127 + t129 
     #+ (t151 - t208 + 0.7D1 / 0.9D1 * t209 - 0.8D1 / 0.9D1 * t211 - 0.4
     #D1 * t213 - t155 + t215 - 0.4D1 * t216 - t149) * t2
      t233 = 0.16D2 / 0.9D1 * t35 * S14
      t234 = 0.16D2 / 0.3D1 * t211
      t235 = t79 * S14
      t238 = 0.16D2 / 0.3D1 * t77 * S14
      t239 = 0.16D2 / 0.3D1 * t152
      t240 = 0.28D2 / 0.9D1 * t150
      t241 = S23 * t113
      t243 = 0.8D1 / 0.3D1 * t154
      t248 = 0.16D2 / 0.3D1 * t148
      t252 = -t233 - t234 + 0.8D1 / 0.9D1 * t235 + t238 - t239 + t240 + 
     #0.16D2 / 0.9D1 * t241 + t243 + 0.4D1 / 0.9D1 * t156 - 0.4D1 / 0.3D
     #1 * t157 + 0.4D1 / 0.3D1 * t159 + 0.16D2 / 0.3D1 * t160 - t248 + 0
     #.8D1 * t215 + 0.8D1 / 0.3D1 * t162 + 0.16D2 / 0.9D1 * t209
      t254 = 0.16D2 / 0.3D1 * t111 + 0.104D3 / 0.9D1 * t29 + 0.16D2 / 0.
     #9D1 * t83 - 0.64D2 / 0.9D1 * t81 + 0.4D1 / 0.3D1 * t79 + 0.56D2 / 
     #0.3D1 * t28 - 0.8D1 / 0.9D1 * t77 + 0.104D3 / 0.9D1 * t35 + 0.4D1 
     #/ 0.3D1 * t113 - 0.8D1 / 0.9D1 * t132 + t252 * t2
      t266 = 0.16D2 / 0.9D1 * t157 + 0.8D1 / 0.3D1 * t213 - t248 + 0.16D
     #2 / 0.3D1 * t216 - 0.4D1 / 0.3D1 * t209 + 0.8D1 * t159 + 0.8D1 / 0
     #.9D1 * t241 + 0.4D1 / 0.3D1 * t215 - t239 + t243 + t238
      t269 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.32D2 / 0.3
     #D1 * t3 * t14 + 0.32D2 / 0.3D1 * S13) * t5 * t6 * t9 + (-0.32D2 / 
     #0.3D1 * S13 * t21 + (-0.32D2 / 0.3D1 * t3 * t14 * S34 + (-0.64D2 /
     # 0.3D1 * S13 + (-0.32D2 / 0.3D1 * t28 - 0.32D2 / 0.3D1 * t29) * t2
     #) * t14 - 0.32D2 / 0.3D1 * t35 - 0.32D2 / 0.3D1 * t28) * t5) * s *
     # z + t2 * t4 + (-0.16D2 * S34 * t2 + 0.89D2 / 0.9D1 * t46 - 0.1D1 
     #+ (-t48 - 0.3D1 * S23 + t50) * t2) * S12 + 0.24D2 * t2 * t55 + (-0
     #.8009D4 / 0.36D2 * t46 - 0.473D3 / 0.9D1 + (0.32D2 * S23 - 0.24D2 
     #* S14) * t2) * S34 + (-0.79D2 / 0.9D1 + (t65 - t66 - 0.6911D4 / 0.
     #9D1 * S23 - 0.6388D4 / 0.9D1 * S14) * t2) * t14 + t73 + t74 + 0.17
     #D2 / 0.9D1 * S14 + 0.116D3 / 0.9D1 * S23 + (0.8D1 * t77 + 0.3D1 * 
     #t79 + t82 - t28 + t84 - 0.14D2 / 0.9D1 * t35) * t2 + t166 * t21 + 
     #(t2 * t168 + (-t46 + 0.89D2 / 0.9D1 + (-t48 + t50 - 0.3D1 * S14) *
     # t2) * t88 + ((-0.79D2 / 0.9D1 + (0.116D3 / 0.9D1 * S14 + t74 + 0.
     #17D2 / 0.9D1 * S23 + t73) * t2) * t14 + t65 - 0.6388D4 / 0.9D1 * S
     #23 - t66 - 0.6911D4 / 0.9D1 * S14 + (-0.14D2 / 0.9D1 * t29 - t28 +
     # 0.3D1 * t113 + t82 + t84 + 0.8D1 * t132) * t2) * t55 + t220 * S34
     # + t254 * t14 + t240 + 0.4D1 / 0.9D1 * t208 - t234 - t233 + 0.16D2
     # / 0.9D1 * t235 + t266) * t5
      rrqg2qgh11J3 = t269 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh11J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S13 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t14 = S13 + S14 + S34
      t21 = 0.1D1 / S12
      t28 = S13 ** 2
      t29 = S13 * S14
      t35 = S23 * S13
      t46 = t2 * t14
      t48 = 0.4D1 * S24
      t50 = 0.7D1 / 0.9D1 * S13
      t55 = S34 ** 2
      t65 = 0.14527D5 / 0.18D2 * S13
      t66 = 0.18598D5 / 0.9D1 * S24
      t73 = 0.5D1 / 0.3D1 * S13
      t74 = 0.44D2 / 0.9D1 * S24
      t77 = S23 * S24
      t79 = S23 ** 2
      t81 = S24 ** 2
      t82 = 0.4D1 * t81
      t83 = S13 * S24
      t84 = 0.8D1 / 0.9D1 * t83
      t88 = t55 * S34
      t99 = 0.3991D4 / 0.18D2 * S13
      t100 = 0.568D3 / 0.9D1 * S24
      t111 = S14 * S23
      t113 = S14 ** 2
      t121 = 0.18518D5 / 0.9D1 * S24
      t122 = 0.28253D5 / 0.36D2 * S13
      t124 = 0.6200D4 / 0.3D1 * t83
      t127 = 0.28D2 / 0.9D1 * t81
      t128 = 0.6380D4 / 0.9D1 * t111
      t129 = 0.14315D5 / 0.18D2 * t28
      t132 = S24 * S14
      t139 = 0.136D3 / 0.9D1 * t111
      t141 = 0.10D2 * t83
      t147 = 0.259D3 / 0.36D2 * t28
      t148 = S13 * t81
      t149 = 0.8D1 / 0.9D1 * t148
      t150 = t28 * S13
      t151 = 0.29D2 / 0.9D1 * t150
      t152 = t77 * S13
      t154 = t28 * S24
      t155 = 0.44D2 / 0.9D1 * t154
      t156 = t79 * S23
      t157 = t79 * S13
      t159 = S23 * t28
      t160 = t81 * S23
      t162 = S24 * t79
      t166 = -0.16D2 * t2 * t88 + (-0.473D3 / 0.9D1 * t46 - 0.8009D4 / 0
     #.36D2 + (0.32D2 * S14 - 0.24D2 * S23) * t2) * t55 + ((0.3878D4 / 0
     #.9D1 + (-0.8009D4 / 0.36D2 * S23 - t99 - t100 - 0.473D3 / 0.9D1 * 
     #S14) * t2) * t14 - 0.473D3 / 0.9D1 * S23 - 0.8009D4 / 0.36D2 * S14
     # - t100 - t99 + (-0.16D2 * t79 - 0.64D2 / 0.9D1 * t83 - 0.16D2 * t
     #81 + 0.24D2 * t111 - 0.16D2 * t113 + 0.88D2 / 0.9D1 * t28) * t2) *
     # S34 + (0.7085D4 / 0.9D1 * S23 + t121 - t122 + 0.25693D5 / 0.36D2 
     #* S14 + (-t124 - 0.18562D5 / 0.9D1 * t77 - 0.6376D4 / 0.9D1 * t29 
     #- t127 - t128 + t129 + 0.25D2 / 0.2D1 * t35 - 0.7024D4 / 0.9D1 * t
     #79 - 0.74D2 / 0.9D1 * t132 - 0.34D2 / 0.9D1 * t113) * t2) * t14 + 
     #t139 - 0.8D1 * t35 - t141 + 0.10D2 * t132 - 0.92D2 / 0.9D1 * t77 -
     # t29 / 0.6D1 - 0.55D2 / 0.9D1 * t79 + t113 / 0.12D2 + t147 - t127 
     #+ (-t149 + t151 - 0.8D1 / 0.9D1 * t152 - t155 - t156 + 0.7D1 / 0.9
     #D1 * t157 + t159 - 0.4D1 * t160 - 0.4D1 * t162) * t2
      t168 = t55 ** 2
      t208 = t113 * S14
      t209 = t113 * S13
      t211 = t132 * S13
      t213 = S24 * t113
      t215 = S14 * t28
      t216 = t81 * S14
      t220 = (0.7085D4 / 0.9D1 * S14 + t121 - t122 + 0.25693D5 / 0.36D2 
     #* S23 + (-t35 / 0.6D1 + t147 - 0.8D1 * t29 - t127 - t141 - 0.92D2 
     #/ 0.9D1 * t132 + 0.10D2 * t77 + t79 / 0.12D2 + t139 - 0.55D2 / 0.9
     #D1 * t113) * t2) * t14 - t124 + 0.25D2 / 0.2D1 * t29 - 0.34D2 / 0.
     #9D1 * t79 - t128 - 0.7024D4 / 0.9D1 * t113 - 0.74D2 / 0.9D1 * t77 
     #- 0.6376D4 / 0.9D1 * t35 - 0.18562D5 / 0.9D1 * t132 - t127 + t129 
     #+ (t151 - t208 + 0.7D1 / 0.9D1 * t209 - 0.8D1 / 0.9D1 * t211 - 0.4
     #D1 * t213 - t155 + t215 - 0.4D1 * t216 - t149) * t2
      t233 = 0.16D2 / 0.9D1 * t35 * S14
      t234 = 0.16D2 / 0.3D1 * t211
      t235 = t79 * S14
      t238 = 0.16D2 / 0.3D1 * t77 * S14
      t239 = 0.16D2 / 0.3D1 * t152
      t240 = 0.28D2 / 0.9D1 * t150
      t241 = S23 * t113
      t243 = 0.8D1 / 0.3D1 * t154
      t248 = 0.16D2 / 0.3D1 * t148
      t252 = -t233 - t234 + 0.8D1 / 0.9D1 * t235 + t238 - t239 + t240 + 
     #0.16D2 / 0.9D1 * t241 + t243 + 0.4D1 / 0.9D1 * t156 - 0.4D1 / 0.3D
     #1 * t157 + 0.4D1 / 0.3D1 * t159 + 0.16D2 / 0.3D1 * t160 - t248 + 0
     #.8D1 * t215 + 0.8D1 / 0.3D1 * t162 + 0.16D2 / 0.9D1 * t209
      t254 = 0.16D2 / 0.3D1 * t111 + 0.104D3 / 0.9D1 * t29 + 0.16D2 / 0.
     #9D1 * t83 - 0.64D2 / 0.9D1 * t81 + 0.4D1 / 0.3D1 * t79 + 0.56D2 / 
     #0.3D1 * t28 - 0.8D1 / 0.9D1 * t77 + 0.104D3 / 0.9D1 * t35 + 0.4D1 
     #/ 0.3D1 * t113 - 0.8D1 / 0.9D1 * t132 + t252 * t2
      t266 = 0.16D2 / 0.9D1 * t157 + 0.8D1 / 0.3D1 * t213 - t248 + 0.16D
     #2 / 0.3D1 * t216 - 0.4D1 / 0.3D1 * t209 + 0.8D1 * t159 + 0.8D1 / 0
     #.9D1 * t241 + 0.4D1 / 0.3D1 * t215 - t239 + t243 + t238
      t269 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.32D2 / 0.3
     #D1 * t3 * t14 + 0.32D2 / 0.3D1 * S13) * t5 * t6 * t9 + (-0.32D2 / 
     #0.3D1 * S13 * t21 + (-0.32D2 / 0.3D1 * t3 * t14 * S34 + (-0.64D2 /
     # 0.3D1 * S13 + (-0.32D2 / 0.3D1 * t28 - 0.32D2 / 0.3D1 * t29) * t2
     #) * t14 - 0.32D2 / 0.3D1 * t35 - 0.32D2 / 0.3D1 * t28) * t5) * s *
     # z + t2 * t4 + (-0.16D2 * S34 * t2 + 0.89D2 / 0.9D1 * t46 - 0.1D1 
     #+ (-t48 - 0.3D1 * S23 + t50) * t2) * S12 + 0.24D2 * t2 * t55 + (-0
     #.8009D4 / 0.36D2 * t46 - 0.473D3 / 0.9D1 + (0.32D2 * S23 - 0.24D2 
     #* S14) * t2) * S34 + (-0.79D2 / 0.9D1 + (t65 - t66 - 0.6911D4 / 0.
     #9D1 * S23 - 0.6388D4 / 0.9D1 * S14) * t2) * t14 + t73 + t74 + 0.17
     #D2 / 0.9D1 * S14 + 0.116D3 / 0.9D1 * S23 + (0.8D1 * t77 + 0.3D1 * 
     #t79 + t82 - t28 + t84 - 0.14D2 / 0.9D1 * t35) * t2 + t166 * t21 + 
     #(t2 * t168 + (-t46 + 0.89D2 / 0.9D1 + (-t48 + t50 - 0.3D1 * S14) *
     # t2) * t88 + ((-0.79D2 / 0.9D1 + (0.116D3 / 0.9D1 * S14 + t74 + 0.
     #17D2 / 0.9D1 * S23 + t73) * t2) * t14 + t65 - 0.6388D4 / 0.9D1 * S
     #23 - t66 - 0.6911D4 / 0.9D1 * S14 + (-0.14D2 / 0.9D1 * t29 - t28 +
     # 0.3D1 * t113 + t82 + t84 + 0.8D1 * t132) * t2) * t55 + t220 * S34
     # + t254 * t14 + t240 + 0.4D1 / 0.9D1 * t208 - t234 - t233 + 0.16D2
     # / 0.9D1 * t235 + t266) * t5
      rrqg2qgh11J4 = t269 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh11J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t3 = S13 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t14 = S13 + S14 + S34
      t21 = 0.1D1 / S12
      t28 = S13 ** 2
      t29 = S13 * S14
      t35 = S23 * S13
      t46 = t2 * t14
      t48 = 0.4D1 * S24
      t50 = 0.7D1 / 0.9D1 * S13
      t55 = S34 ** 2
      t65 = 0.14527D5 / 0.18D2 * S13
      t66 = 0.18598D5 / 0.9D1 * S24
      t73 = 0.5D1 / 0.3D1 * S13
      t74 = 0.44D2 / 0.9D1 * S24
      t77 = S23 * S24
      t79 = S23 ** 2
      t81 = S24 ** 2
      t82 = 0.4D1 * t81
      t83 = S13 * S24
      t84 = 0.8D1 / 0.9D1 * t83
      t88 = t55 * S34
      t99 = 0.3991D4 / 0.18D2 * S13
      t100 = 0.568D3 / 0.9D1 * S24
      t111 = S14 * S23
      t113 = S14 ** 2
      t121 = 0.18518D5 / 0.9D1 * S24
      t122 = 0.28253D5 / 0.36D2 * S13
      t124 = 0.6200D4 / 0.3D1 * t83
      t127 = 0.28D2 / 0.9D1 * t81
      t128 = 0.6380D4 / 0.9D1 * t111
      t129 = 0.14315D5 / 0.18D2 * t28
      t132 = S24 * S14
      t139 = 0.136D3 / 0.9D1 * t111
      t141 = 0.10D2 * t83
      t147 = 0.259D3 / 0.36D2 * t28
      t148 = S13 * t81
      t149 = 0.8D1 / 0.9D1 * t148
      t150 = t28 * S13
      t151 = 0.29D2 / 0.9D1 * t150
      t152 = t77 * S13
      t154 = t28 * S24
      t155 = 0.44D2 / 0.9D1 * t154
      t156 = t79 * S23
      t157 = t79 * S13
      t159 = S23 * t28
      t160 = t81 * S23
      t162 = S24 * t79
      t166 = -0.16D2 * t2 * t88 + (-0.473D3 / 0.9D1 * t46 - 0.8009D4 / 0
     #.36D2 + (0.32D2 * S14 - 0.24D2 * S23) * t2) * t55 + ((0.3878D4 / 0
     #.9D1 + (-0.8009D4 / 0.36D2 * S23 - t99 - t100 - 0.473D3 / 0.9D1 * 
     #S14) * t2) * t14 - 0.473D3 / 0.9D1 * S23 - 0.8009D4 / 0.36D2 * S14
     # - t100 - t99 + (-0.16D2 * t79 - 0.64D2 / 0.9D1 * t83 - 0.16D2 * t
     #81 + 0.24D2 * t111 - 0.16D2 * t113 + 0.88D2 / 0.9D1 * t28) * t2) *
     # S34 + (0.7085D4 / 0.9D1 * S23 + t121 - t122 + 0.25693D5 / 0.36D2 
     #* S14 + (-t124 - 0.18562D5 / 0.9D1 * t77 - 0.6376D4 / 0.9D1 * t29 
     #- t127 - t128 + t129 + 0.25D2 / 0.2D1 * t35 - 0.7024D4 / 0.9D1 * t
     #79 - 0.74D2 / 0.9D1 * t132 - 0.34D2 / 0.9D1 * t113) * t2) * t14 + 
     #t139 - 0.8D1 * t35 - t141 + 0.10D2 * t132 - 0.92D2 / 0.9D1 * t77 -
     # t29 / 0.6D1 - 0.55D2 / 0.9D1 * t79 + t113 / 0.12D2 + t147 - t127 
     #+ (-t149 + t151 - 0.8D1 / 0.9D1 * t152 - t155 - t156 + 0.7D1 / 0.9
     #D1 * t157 + t159 - 0.4D1 * t160 - 0.4D1 * t162) * t2
      t168 = t55 ** 2
      t208 = t113 * S14
      t209 = t113 * S13
      t211 = t132 * S13
      t213 = S24 * t113
      t215 = S14 * t28
      t216 = t81 * S14
      t220 = (0.7085D4 / 0.9D1 * S14 + t121 - t122 + 0.25693D5 / 0.36D2 
     #* S23 + (-t35 / 0.6D1 + t147 - 0.8D1 * t29 - t127 - t141 - 0.92D2 
     #/ 0.9D1 * t132 + 0.10D2 * t77 + t79 / 0.12D2 + t139 - 0.55D2 / 0.9
     #D1 * t113) * t2) * t14 - t124 + 0.25D2 / 0.2D1 * t29 - 0.34D2 / 0.
     #9D1 * t79 - t128 - 0.7024D4 / 0.9D1 * t113 - 0.74D2 / 0.9D1 * t77 
     #- 0.6376D4 / 0.9D1 * t35 - 0.18562D5 / 0.9D1 * t132 - t127 + t129 
     #+ (t151 - t208 + 0.7D1 / 0.9D1 * t209 - 0.8D1 / 0.9D1 * t211 - 0.4
     #D1 * t213 - t155 + t215 - 0.4D1 * t216 - t149) * t2
      t233 = 0.16D2 / 0.9D1 * t35 * S14
      t234 = 0.16D2 / 0.3D1 * t211
      t235 = t79 * S14
      t238 = 0.16D2 / 0.3D1 * t77 * S14
      t239 = 0.16D2 / 0.3D1 * t152
      t240 = 0.28D2 / 0.9D1 * t150
      t241 = S23 * t113
      t243 = 0.8D1 / 0.3D1 * t154
      t248 = 0.16D2 / 0.3D1 * t148
      t252 = -t233 - t234 + 0.8D1 / 0.9D1 * t235 + t238 - t239 + t240 + 
     #0.16D2 / 0.9D1 * t241 + t243 + 0.4D1 / 0.9D1 * t156 - 0.4D1 / 0.3D
     #1 * t157 + 0.4D1 / 0.3D1 * t159 + 0.16D2 / 0.3D1 * t160 - t248 + 0
     #.8D1 * t215 + 0.8D1 / 0.3D1 * t162 + 0.16D2 / 0.9D1 * t209
      t254 = 0.16D2 / 0.3D1 * t111 + 0.104D3 / 0.9D1 * t29 + 0.16D2 / 0.
     #9D1 * t83 - 0.64D2 / 0.9D1 * t81 + 0.4D1 / 0.3D1 * t79 + 0.56D2 / 
     #0.3D1 * t28 - 0.8D1 / 0.9D1 * t77 + 0.104D3 / 0.9D1 * t35 + 0.4D1 
     #/ 0.3D1 * t113 - 0.8D1 / 0.9D1 * t132 + t252 * t2
      t266 = 0.16D2 / 0.9D1 * t157 + 0.8D1 / 0.3D1 * t213 - t248 + 0.16D
     #2 / 0.3D1 * t216 - 0.4D1 / 0.3D1 * t209 + 0.8D1 * t159 + 0.8D1 / 0
     #.9D1 * t241 + 0.4D1 / 0.3D1 * t215 - t239 + t243 + t238
      t269 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.32D2 / 0.3
     #D1 * t3 * t14 + 0.32D2 / 0.3D1 * S13) * t5 * t6 * t9 + (-0.32D2 / 
     #0.3D1 * S13 * t21 + (-0.32D2 / 0.3D1 * t3 * t14 * S34 + (-0.64D2 /
     # 0.3D1 * S13 + (-0.32D2 / 0.3D1 * t28 - 0.32D2 / 0.3D1 * t29) * t2
     #) * t14 - 0.32D2 / 0.3D1 * t35 - 0.32D2 / 0.3D1 * t28) * t5) * s *
     # z + t2 * t4 + (-0.16D2 * S34 * t2 + 0.89D2 / 0.9D1 * t46 - 0.1D1 
     #+ (-t48 - 0.3D1 * S23 + t50) * t2) * S12 + 0.24D2 * t2 * t55 + (-0
     #.8009D4 / 0.36D2 * t46 - 0.473D3 / 0.9D1 + (0.32D2 * S23 - 0.24D2 
     #* S14) * t2) * S34 + (-0.79D2 / 0.9D1 + (t65 - t66 - 0.6911D4 / 0.
     #9D1 * S23 - 0.6388D4 / 0.9D1 * S14) * t2) * t14 + t73 + t74 + 0.17
     #D2 / 0.9D1 * S14 + 0.116D3 / 0.9D1 * S23 + (0.8D1 * t77 + 0.3D1 * 
     #t79 + t82 - t28 + t84 - 0.14D2 / 0.9D1 * t35) * t2 + t166 * t21 + 
     #(t2 * t168 + (-t46 + 0.89D2 / 0.9D1 + (-t48 + t50 - 0.3D1 * S14) *
     # t2) * t88 + ((-0.79D2 / 0.9D1 + (0.116D3 / 0.9D1 * S14 + t74 + 0.
     #17D2 / 0.9D1 * S23 + t73) * t2) * t14 + t65 - 0.6388D4 / 0.9D1 * S
     #23 - t66 - 0.6911D4 / 0.9D1 * S14 + (-0.14D2 / 0.9D1 * t29 - t28 +
     # 0.3D1 * t113 + t82 + t84 + 0.8D1 * t132) * t2) * t55 + t220 * S34
     # + t254 * t14 + t240 + 0.4D1 / 0.9D1 * t208 - t234 - t233 + 0.16D2
     # / 0.9D1 * t235 + t266) * t5
      rrqg2qgh11J5 = t269 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh11J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

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
      t1 = S12 + S13 + S23
      t2 = 0.1D1 / t1
      t3 = S13 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t15 = 0.1D1 / S12
      t20 = t1 ** 2
      t21 = 0.1D1 / t20
      t23 = S13 + S14 + S34
      t24 = t23 * S34
      t34 = t2 * t23
      t39 = 0.64D2 / 0.9D1 * S13
      t47 = S13 ** 2
      t48 = S13 * S14
      t54 = S23 * S13
      t55 = 0.32D2 / 0.3D1 * t54
      t62 = t21 * t23
      t68 = S34 ** 2
      t82 = 0.6172D4 / 0.3D1 * S24
      t83 = 0.7309D4 / 0.9D1 * S13
      t90 = 0.16D2 / 0.9D1 * S24
      t93 = 0.22D2 / 0.9D1 * S13
      t94 = S13 * S24
      t96 = S24 ** 2
      t100 = (0.4D1 * t94 - 0.2D1 * t96 - 0.2D1 * t47) * t2
      t101 = t68 * S34
      t115 = 0.976D3 / 0.9D1 * S24
      t116 = 0.1129D4 / 0.9D1 * S13
      t123 = S24 * S14
      t125 = S23 * S24
      t128 = S14 * S23
      t131 = S23 ** 2
      t134 = S14 ** 2
      t150 = 0.7132D4 / 0.9D1 * S13
      t152 = 0.18386D5 / 0.9D1 * S24
      t154 = 0.18500D5 / 0.9D1 * t94
      t155 = 0.6380D4 / 0.9D1 * t128
      t158 = 0.14D2 / 0.9D1 * t96
      t159 = 0.14411D5 / 0.18D2 * t47
      t167 = 0.122D3 / 0.9D1 * t94
      t171 = 0.17D2 / 0.2D1 * t47
      t172 = 0.160D3 / 0.9D1 * t128
      t173 = 0.50D2 / 0.9D1 * t96
      t176 = t47 * S24
      t177 = 0.8D1 / 0.9D1 * t176
      t178 = S23 * t47
      t181 = 0.4D1 * t96 * S24
      t182 = t47 * S13
      t183 = 0.22D2 / 0.9D1 * t182
      t184 = t96 * S23
      t186 = S13 * t96
      t187 = 0.50D2 / 0.9D1 * t186
      t188 = t125 * S13
      t192 = -0.68D2 / 0.9D1 * t62 * t101 + (-0.2243D4 / 0.18D2 + (-0.78
     #4D3 / 0.9D1 * t2 + (-0.136D3 / 0.9D1 * S24 + 0.272D3 / 0.9D1 * S23
     # + 0.272D3 / 0.9D1 * S13 - 0.136D3 / 0.9D1 * S14) * t21) * t23) * 
     #t68 + ((0.1904D4 / 0.9D1 + (-t115 - t116 - 0.2243D4 / 0.18D2 * S23
     # - 0.784D3 / 0.9D1 * S14) * t2 + (0.272D3 / 0.9D1 * t94 - 0.118D3 
     #/ 0.9D1 * t47 - 0.136D3 / 0.9D1 * t123 + 0.272D3 / 0.9D1 * t125 - 
     #0.68D2 / 0.9D1 * t96 + 0.272D3 / 0.9D1 * t128 - 0.212D3 / 0.9D1 * 
     #t54 - 0.94D2 / 0.9D1 * t131 + 0.272D3 / 0.9D1 * t48 - 0.68D2 / 0.9
     #D1 * t134) * t21) * t23 - 0.784D3 / 0.9D1 * S23 - t116 - 0.2243D4 
     #/ 0.18D2 * S14 - t115 + (0.232D3 / 0.9D1 * t47 - 0.32D2 / 0.9D1 * 
     #t94 - 0.200D3 / 0.9D1 * t96) * t2) * S34 + (0.4285D4 / 0.6D1 * S14
     # - t150 + 0.14017D5 / 0.18D2 * S23 + t152 + (0.245D3 / 0.9D1 * t54
     # - t154 - t155 - 0.18416D5 / 0.9D1 * t125 - 0.6376D4 / 0.9D1 * t48
     # - t158 + t159 - 0.1537D4 / 0.2D1 * t131 - 0.74D2 / 0.9D1 * t123 -
     # 0.34D2 / 0.9D1 * t134) * t2) * t23 - t167 + 0.44D2 / 0.3D1 * t131
     # + 0.128D3 / 0.9D1 * t125 - 0.25D2 / 0.9D1 * t48 + t171 + t172 + t
     #173 + 0.25D2 / 0.18D2 * t134 - t55 + 0.122D3 / 0.9D1 * t123 + (-t1
     #77 + 0.2D1 * t178 + t181 + t183 + 0.2D1 * t184 - t187 - 0.4D1 * t1
     #88) * t2
      t225 = t96 * S14
      t227 = t123 * S13
      t229 = S14 * t47
      t233 = (0.14017D5 / 0.18D2 * S14 + 0.4285D4 / 0.6D1 * S23 + t152 -
     # t150 + (-t167 - 0.25D2 / 0.9D1 * t54 + t172 + t171 - 0.32D2 / 0.3
     #D1 * t48 + t173 + 0.128D3 / 0.9D1 * t123 + 0.122D3 / 0.9D1 * t125 
     #+ 0.25D2 / 0.18D2 * t131 + 0.44D2 / 0.3D1 * t134) * t2) * t23 - t1
     #54 - t155 + t159 + 0.245D3 / 0.9D1 * t48 - 0.34D2 / 0.9D1 * t131 -
     # 0.18416D5 / 0.9D1 * t123 - 0.1537D4 / 0.2D1 * t134 - 0.74D2 / 0.9
     #D1 * t125 - 0.6376D4 / 0.9D1 * t54 - t158 + (t181 + 0.2D1 * t225 -
     # t177 - 0.4D1 * t227 + 0.2D1 * t229 - t187 + t183) * t2
      t246 = 0.16D2 / 0.9D1 * t54 * S14
      t247 = 0.16D2 / 0.3D1 * t227
      t248 = t131 * S14
      t251 = 0.16D2 / 0.3D1 * t125 * S14
      t252 = 0.16D2 / 0.3D1 * t188
      t253 = 0.28D2 / 0.9D1 * t182
      t254 = S23 * t134
      t256 = 0.8D1 / 0.3D1 * t176
      t259 = t131 * S13
      t263 = 0.16D2 / 0.3D1 * t186
      t267 = t134 * S13
      t269 = -t246 - t247 + 0.8D1 / 0.9D1 * t248 + t251 - t252 + t253 + 
     #0.16D2 / 0.9D1 * t254 + t256 + 0.4D1 / 0.9D1 * t131 * S23 - 0.4D1 
     #/ 0.3D1 * t259 + 0.4D1 / 0.3D1 * t178 + 0.16D2 / 0.3D1 * t184 - t2
     #63 + 0.8D1 * t229 + 0.8D1 / 0.3D1 * S24 * t131 + 0.16D2 / 0.9D1 * 
     #t267
      t271 = 0.16D2 / 0.9D1 * t128 + 0.104D3 / 0.9D1 * t48 + 0.16D2 / 0.
     #3D1 * t94 - 0.64D2 / 0.9D1 * t96 + 0.4D1 / 0.3D1 * t131 + 0.56D2 /
     # 0.3D1 * t47 - 0.8D1 / 0.9D1 * t125 + 0.104D3 / 0.9D1 * t54 + 0.4D
     #1 / 0.3D1 * t134 - 0.8D1 / 0.9D1 * t123 + t269 * t2
      t284 = (0.61D2 / 0.6D1 - 0.4D1 / 0.3D1 * t34) * t101 + ((-0.73D2 /
     # 0.6D1 + (0.40D2 / 0.3D1 * S14 + 0.6D1 * S23 - t93 - t90) * t2) * 
     #t23 - 0.6388D4 / 0.9D1 * S23 + t83 - 0.6829D4 / 0.9D1 * S14 - t82 
     #+ t100) * t68 + t233 * S34 + t271 * t23 + t253 + 0.4D1 / 0.9D1 * t
     #134 * S14 - t247 - t246 + 0.16D2 / 0.9D1 * t248 + 0.16D2 / 0.9D1 *
     # t259 + 0.8D1 / 0.3D1 * S24 * t134 - t263 + 0.16D2 / 0.3D1 * t225 
     #- 0.4D1 / 0.3D1 * t267 + 0.8D1 * t178 + 0.8D1 / 0.9D1 * t254 + 0.4
     #D1 / 0.3D1 * t229 - t252 + t256 + t251
      t286 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + (0.48D2 * S34
     # * t2 * t15 + ((-0.16D2 / 0.9D1 * S13 + 0.16D2 / 0.9D1 * S23) * t2
     #1 * t24 + 0.32D2 / 0.3D1 * t3 * t23 + 0.32D2 / 0.3D1 * S13) * t5) 
     #* t6 * t9 + (((-0.48D2 * t34 - 0.48D2 + 0.48D2 * t3) * S34 - 0.32D
     #2 / 0.9D1 * S14 - t39) * t15 + ((-0.32D2 / 0.9D1 * S23 - t39) * t2
     # * t24 + (-0.64D2 / 0.3D1 * S13 + (-0.32D2 / 0.3D1 * t47 - 0.32D2 
     #/ 0.3D1 * t48) * t2) * t23 - t55 - 0.32D2 / 0.3D1 * t47) * t5) * s
     # * z + (-0.94D2 / 0.9D1 * t62 * S34 - 0.4D1 / 0.3D1 + 0.61D2 / 0.6
     #D1 * t34) * S12 + 0.272D3 / 0.9D1 * t62 * t68 + (-0.784D3 / 0.9D1 
     #+ (-0.2243D4 / 0.18D2 * t2 + (-0.212D3 / 0.9D1 * S13 - 0.188D3 / 0
     #.9D1 * S23 + 0.272D3 / 0.9D1 * S24 + 0.272D3 / 0.9D1 * S14) * t21)
     # * t23) * S34 + (-0.73D2 / 0.6D1 + (-t82 + t83 - 0.6829D4 / 0.9D1 
     #* S23 - 0.6388D4 / 0.9D1 * S14) * t2) * t23 - t90 + 0.6D1 * S14 + 
     #0.40D2 / 0.3D1 * S23 - t93 + t100 + t192 * t15 + t284 * t5
      rrqg2qgh11J6 = t286 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh11J7
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

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
      t1 = S12 + S13 + S23
      t2 = 0.1D1 / t1
      t3 = S13 * t2
      t4 = S12 ** 2
      t5 = 0.1D1 / t4
      t6 = s ** 2
      t9 = z ** 2
      t16 = t1 ** 2
      t17 = 0.1D1 / t16
      t19 = S13 + S14 + S34
      t20 = t19 * S34
      t30 = 0.128D3 / 0.9D1 * S13
      t32 = 0.1D1 / S12
      t34 = 0.32D2 / 0.9D1 * S23
      t39 = S13 ** 2
      t40 = S13 * S14
      t46 = S23 * S13
      t54 = t17 * t19
      t57 = t2 * t19
      t61 = S34 ** 2
      t65 = 0.160D3 / 0.9D1 * S14
      t76 = 0.7282D4 / 0.9D1 * S13
      t77 = 0.18580D5 / 0.9D1 * S24
      t84 = t61 * S34
      t89 = 0.160D3 / 0.9D1 * S23
      t98 = 0.106D3 / 0.9D1 * S13
      t99 = 0.76D2 / 0.3D1 * S24
      t104 = S23 * S24
      t106 = S24 ** 2
      t110 = S13 * S24
      t112 = S23 ** 2
      t114 = S14 ** 2
      t116 = S14 * S23
      t118 = S24 * S14
      t132 = 0.18574D5 / 0.9D1 * S24
      t134 = 0.7093D4 / 0.9D1 * S13
      t138 = 0.6316D4 / 0.9D1 * t116
      t140 = 0.8D1 / 0.9D1 * t106
      t142 = 0.18572D5 / 0.9D1 * t110
      t144 = 0.7198D4 / 0.9D1 * t39
      t152 = 0.2D1 / 0.9D1 * t110
      t153 = 0.7D1 * t39
      t155 = 0.16D2 / 0.9D1 * t106
      t158 = t39 * S24
      t160 = S13 * t106
      t162 = t39 * S13
      t165 = (-0.8D1 * t158 + 0.4D1 * t160 + 0.4D1 * t162) * t2
      t166 = 0.40D2 / 0.9D1 * t54 * t84 + (0.65D2 / 0.3D1 + (0.160D3 / 0
     #.9D1 * t2 + (0.80D2 / 0.9D1 * S14 - t89 - 0.160D3 / 0.9D1 * S13 + 
     #0.80D2 / 0.9D1 * S24) * t17) * t19) * t61 + ((-0.48D2 + (t98 + t99
     # + t65 + 0.65D2 / 0.3D1 * S23) * t2 + (0.76D2 / 0.9D1 * t39 - 0.16
     #0D3 / 0.9D1 * t104 + 0.40D2 / 0.9D1 * t106 - 0.160D3 / 0.9D1 * t40
     # + 0.128D3 / 0.9D1 * t46 - 0.160D3 / 0.9D1 * t110 + 0.52D2 / 0.9D1
     # * t112 + 0.40D2 / 0.9D1 * t114 - 0.160D3 / 0.9D1 * t116 + 0.80D2 
     #/ 0.9D1 * t118) * t17) * t19 + t89 + t99 + t98 + 0.65D2 / 0.3D1 * 
     #S14 + (0.16D2 * t39 + 0.16D2 * t106 - 0.32D2 * t110) * t2) * S34 +
     # (t132 + 0.784D3 * S23 - t134 + 0.6305D4 / 0.9D1 * S14 + (-0.20D2 
     #/ 0.9D1 * t118 + 0.134D3 / 0.9D1 * t46 - t138 - 0.6184D4 / 0.3D1 *
     # t104 - t140 - 0.2104D4 / 0.3D1 * t40 - t142 - 0.7052D4 / 0.9D1 * 
     #t112 + t144) * t2) * t19 + 0.64D2 / 0.9D1 * t46 + 0.2D1 / 0.9D1 * 
     #t40 - 0.16D2 / 0.9D1 * t112 + t152 + t153 - 0.32D2 / 0.9D1 * t104 
     #- t155 - 0.2D1 / 0.9D1 * t118 - t114 / 0.9D1 + t165
      t197 = (0.6305D4 / 0.9D1 * S23 + t132 - t134 + 0.784D3 * S14 + (t1
     #52 - 0.32D2 / 0.9D1 * t118 - 0.2D1 / 0.9D1 * t104 + 0.2D1 / 0.9D1 
     #* t46 - t155 + 0.64D2 / 0.9D1 * t40 + t153 - t112 / 0.9D1 - 0.16D2
     # / 0.9D1 * t114) * t2) * t19 - 0.2104D4 / 0.3D1 * t46 - t142 + 0.1
     #34D3 / 0.9D1 * t40 - 0.7052D4 / 0.9D1 * t114 - t138 - 0.20D2 / 0.9
     #D1 * t104 - t140 - 0.6184D4 / 0.3D1 * t118 + t144 + t165
      t210 = 0.16D2 / 0.9D1 * t46 * S14
      t211 = t118 * S13
      t216 = 0.16D2 / 0.9D1 * t104 * S14
      t217 = t104 * S13
      t219 = 0.28D2 / 0.9D1 * t162
      t220 = t114 * S13
      t222 = 0.8D1 / 0.3D1 * t158
      t225 = t112 * S13
      t227 = S23 * t39
      t231 = 0.32D2 / 0.9D1 * t160
      t234 = S14 * t39
      t236 = -t210 - 0.16D2 / 0.9D1 * t211 + 0.8D1 / 0.9D1 * t112 * S14 
     #+ t216 - 0.16D2 / 0.3D1 * t217 + t219 + 0.32D2 / 0.9D1 * t220 + t2
     #22 + 0.4D1 / 0.9D1 * t112 * S23 - 0.4D1 / 0.3D1 * t225 + 0.4D1 / 0
     #.3D1 * t227 + 0.32D2 / 0.9D1 * t106 * S23 - t231 + 0.8D1 / 0.3D1 *
     # S24 * t112 + 0.8D1 * t234
      t238 = 0.16D2 / 0.3D1 * t110 + 0.16D2 / 0.9D1 * t116 + 0.104D3 / 0
     #.9D1 * t40 + 0.56D2 / 0.3D1 * t39 - 0.32D2 / 0.9D1 * t106 - 0.4D1 
     #/ 0.9D1 * t112 - 0.8D1 / 0.9D1 * t118 - 0.8D1 / 0.9D1 * t104 + 0.1
     #04D3 / 0.9D1 * t46 - 0.4D1 / 0.9D1 * t114 + t236 * t2
      t254 = (0.76D2 / 0.9D1 - 0.16D2 / 0.9D1 * t57) * t84 + ((-0.76D2 /
     # 0.9D1 + (-0.32D2 / 0.9D1 * S24 - 0.32D2 / 0.9D1 * S14 + 0.32D2 / 
     #0.9D1 * S13) * t2) * t19 - 0.2108D4 / 0.3D1 * S23 - 0.6980D4 / 0.9
     #D1 * S14 - t77 + t76) * t61 + t197 * S34 + t238 * t19 + t219 + 0.4
     #D1 / 0.9D1 * t114 * S14 - t210 + 0.32D2 / 0.9D1 * t106 * S14 + 0.3
     #2D2 / 0.9D1 * t225 + 0.8D1 / 0.3D1 * S24 * t114 - t231 + 0.8D1 / 0
     #.9D1 * S23 * t114 - 0.4D1 / 0.3D1 * t220 + 0.8D1 * t227 + t222 + 0
     #.4D1 / 0.3D1 * t234 - 0.16D2 / 0.3D1 * t211 + t216 - 0.16D2 / 0.9D
     #1 * t217
      t256 = -0.32D2 / 0.9D1 * t3 * t5 * t6 * s * t9 * z + ((-0.16D2 / 0
     #.9D1 * S23 + 0.16D2 / 0.9D1 * S13) * t17 * t20 + 0.32D2 / 0.3D1 * 
     #t3 * t19 + 0.32D2 / 0.3D1 * S13) * t5 * t6 * t9 + ((0.32D2 / 0.9D1
     # * S14 - t30) * t32 + ((-t30 + t34) * t2 * t20 + (-0.64D2 / 0.3D1 
     #* S13 + (-0.32D2 / 0.3D1 * t39 - 0.32D2 / 0.3D1 * t40) * t2) * t19
     # - 0.32D2 / 0.3D1 * t46 - 0.32D2 / 0.3D1 * t39) * t5) * s * z + (0
     #.52D2 / 0.9D1 * t54 * S34 - 0.16D2 / 0.9D1 + 0.76D2 / 0.9D1 * t57)
     # * S12 - 0.160D3 / 0.9D1 * t54 * t61 + (0.160D3 / 0.9D1 + (0.65D2 
     #/ 0.3D1 * t2 + (t30 - t65 + 0.104D3 / 0.9D1 * S23 - 0.160D3 / 0.9D
     #1 * S24) * t17) * t19) * S34 + (-0.76D2 / 0.9D1 + (-0.6980D4 / 0.9
     #D1 * S23 - 0.2108D4 / 0.3D1 * S14 + t76 - t77) * t2) * t19 - t34 +
     # 0.32D2 / 0.9D1 * S13 - 0.32D2 / 0.9D1 * S24 + t166 * t32 + t254 *
     # t5
      rrqg2qgh11J7 = t256 / pi * wd / z

      end function
  
 