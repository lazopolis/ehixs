  
      subroutine rrqg2qght10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qgh101J1  
      doubleprecision rrqg2qgh101J2  
      doubleprecision rrqg2qgh101J3  
      doubleprecision rrqg2qgh101J4  
      doubleprecision rrqg2qgh101J5  
      doubleprecision rrqg2qgh101J6  
      doubleprecision rrqg2qgh101J7  
      doubleprecision rrqg2qght10s1e1  
      doubleprecision rrqg2qght10s1e0  
      doubleprecision rrqg2qght10s1em1  
      doubleprecision rrqg2qght10s1em2  
      doubleprecision rrqg2qght10s1em3  
      doubleprecision rrqg2qght10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght10s1e1
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
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7

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
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = pi * t1
      t20 = 0.1D1 / s
      t21 = x2 * z
      t23 = (t21 - x2 + 0.1D1) ** 2
      t24 = 0.1D1 / t23
      t25 = rrqg2qgh101J2(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.0D
     #0, 0.0D0, 0.0D0)
      t27 = x1 ** 2
      t28 = Sin(t5)
      t29 = t28 ** 2
      t30 = t27 * t29
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t34 = t1 ** 2
      t35 = t34 ** 2
      t38 = t33 * t35 * t7 * t9
      t41 = log(0.4D1 * t3 * t30 * t38)
      t43 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.0D
     #0, 0.0D0, 0.0D0)
      t49 = pi * lh
      t51 = t20 * t24
      t52 = t51 * t43
      t56 = 0.1D1 / x1
      t59 = rrqg2qgh101J3(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.0D
     #0, 0.0D0, 0.0D0)
      t67 = log(0.4D1 * t3 * t29 * t38)
      t68 = t67 * pi
      t77 = t67 ** 2
      t80 = lh ** 2
      t82 = pi ** 2
      t90 = -(0.90D2 * t19 * t20 * (-t24 * t25 + t41 * t24 * t43) + 0.18
     #0D3 * t49 * t1 * t52) * t56 / 0.720D3 + t19 * t51 * t59 / 0.16D2 +
     # (-0.180D3 * t49 - 0.90D2 * t68) * t1 * t51 * t25 / 0.1440D4 + (0.
     #180D3 * t68 * lh + 0.45D2 * t77 * pi + pi * (0.180D3 * t80 - 0.30D
     #2 * t82)) * t1 * t52 / 0.1440D4
      t91 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t90)
      t93 = x3 * x1
      t94 = t2 * t93
      t95 = -0.1D1 + x1
      t96 = t93 * z
      t97 = t3 * x1
      t98 = x1 * z
      t99 = t3 * t98
      t101 = 0.1D1 - x1 + t98
      t105 = Sqrt(x3 * t7 * t101 * x2 * t9)
      t107 = 0.2D1 * t6 * t105
      t110 = 0.1D1 / t101
      t112 = t2 * t95 * (-x3 + t93 - t96 + t4 - t97 + t99 - x2 + t107) *
     # t110
      t115 = t9 * s * t1 * x1
      t116 = x2 * x1
      t117 = t116 * z
      t118 = 0.1D1 - x1 + t98 - x2 + t116 - t117 - x3 + t93 - t96 + t4 -
     # t97 + t99 + t107
      t121 = t2 * t95 * t118 * t110
      t126 = s * t34 * x2 * t95 * x1 * t110
      t127 = t101 * t95
      t129 = (-t21 + t117 - t98 + x2 - t116 - 0.1D1 + x1) ** 2
      t130 = 0.1D1 / t129
      t131 = rrqg2qgh101J2(s, XB1, XB2, z, lh, wd, nf, s, t112, -t121, t
     #94, -t115, -t126)
      t137 = t95 ** 2
      t143 = log(0.4D1 * t3 * t30 * t33 * t35 * t110 * t137 * t7 * t9)
      t146 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, t112, -t121, t
     #94, -t115, -t126)
      t159 = -0.90D2 * t19 * t20 * (t127 * t130 * t131 - t143 * t101 * t
     #95 * t130 * t146) + 0.180D3 * t49 * t1 * t20 * t127 * t130 * t146
      t162 = FJET(XB1, XB2, s, t94, t112, -t115, -t121, -t126, -t159 * t
     #56 / 0.720D3)
      rrqg2qght10s1e1 = t91 * t90 - t162 * t159 * t56 / 0.720D3

      end function



      doubleprecision function rrqg2qght10s1e0
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
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7

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
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = pi * t1
      t20 = 0.1D1 / s
      t22 = x2 * z
      t24 = (t22 - x2 + 0.1D1) ** 2
      t25 = 0.1D1 / t24
      t26 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.0D
     #0, 0.0D0, 0.0D0)
      t28 = 0.1D1 / x1
      t32 = t20 * t25
      t33 = rrqg2qgh101J2(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.0D
     #0, 0.0D0, 0.0D0)
      t39 = Sin(t5)
      t40 = t39 ** 2
      t42 = z ** 2
      t44 = t1 ** 2
      t45 = t44 ** 2
      t51 = log(0.4D1 * t3 * t40 / t42 * t45 * t7 * t9)
      t59 = t19 * t20 * t25 * t26 * t28 / 0.8D1 + t19 * t32 * t33 / 0.16
     #D2 + (-0.180D3 * pi * lh - 0.90D2 * t51 * pi) * t1 * t32 * t26 / 0
     #.1440D4
      t60 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t59)
      t62 = x3 * x1
      t63 = t2 * t62
      t64 = -0.1D1 + x1
      t65 = t62 * z
      t66 = t3 * x1
      t67 = x1 * z
      t68 = t3 * t67
      t70 = 0.1D1 - x1 + t67
      t74 = Sqrt(x3 * t7 * t70 * x2 * t9)
      t76 = 0.2D1 * t6 * t74
      t79 = 0.1D1 / t70
      t81 = t2 * t64 * (-x3 + t62 - t65 + t4 - t66 + t68 - x2 + t76) * t
     #79
      t84 = t9 * s * t1 * x1
      t85 = x2 * x1
      t86 = t85 * z
      t87 = 0.1D1 - x1 + t67 - x2 + t85 - t86 - x3 + t62 - t65 + t4 - t6
     #6 + t68 + t76
      t90 = t2 * t64 * t87 * t79
      t95 = s * t44 * x2 * t64 * x1 * t79
      t99 = (-t22 + t86 - t67 + x2 - t85 - 0.1D1 + x1) ** 2
      t100 = 0.1D1 / t99
      t102 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, t81, -t90, t63
     #, -t84, -t95)
      t107 = FJET(XB1, XB2, s, t63, t81, -t84, -t90, -t95, t19 * t20 * t
     #70 * t64 * t100 * t102 * t28 / 0.8D1)
      rrqg2qght10s1e0 = t60 * t59 + t107 * pi * t1 * t20 * t70 * t64 * t
     #100 * t102 * t28 / 0.8D1

      end function



      doubleprecision function rrqg2qght10s1em1
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
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7

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
      t6 = cos(x4 * pi)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t23 = (x2 * z - x2 + 0.1D1) ** 2
      t26 = rrqg2qgh101J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.0D
     #0, 0.0D0, 0.0D0)
      t27 = 0.1D1 / s / t23 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, pi * t1 * 
     #t27 / 0.16D2)
      rrqg2qght10s1em1 = t30 * pi * t1 * t27 / 0.16D2

      end function



      doubleprecision function rrqg2qght10s1em2
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
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght10s1em2 = 0.0D0

      end function



      doubleprecision function rrqg2qght10s1em3
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
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght10s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght10s1em4
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
      doubleprecision rrqg2qgh101J1
      doubleprecision rrqg2qgh101J2
      doubleprecision rrqg2qgh101J3
      doubleprecision rrqg2qgh101J4
      doubleprecision rrqg2qgh101J5
      doubleprecision rrqg2qgh101J6
      doubleprecision rrqg2qgh101J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqg2qght10s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqg2qgh101J1
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
      t7 = S34 ** 2
      t15 = S13 ** 2
      t17 = S24 ** 2
      t23 = S23 ** 2
      t25 = S14 ** 2
      t35 = 0.94D2 / 0.9D1 * t7 + (-0.272D3 / 0.9D1 * S13 + 0.212D3 / 0.
     #9D1 * S23 - 0.272D3 / 0.9D1 * S14 + 0.188D3 / 0.9D1 * S24) * S34 +
     # 0.68D2 / 0.9D1 * t15 + 0.94D2 / 0.9D1 * t17 - 0.272D3 / 0.9D1 * S
     #24 * S14 + 0.212D3 / 0.9D1 * S23 * S24 + 0.118D3 / 0.9D1 * t23 + 0
     #.68D2 / 0.9D1 * t25 + 0.136D3 / 0.9D1 * S13 * S14 - 0.272D3 / 0.9D
     #1 * S14 * S23 - 0.272D3 / 0.9D1 * S23 * S13 - 0.272D3 / 0.9D1 * S1
     #3 * S24
      rrqg2qgh101J1 = (0.68D2 / 0.9D1 * S12 + 0.136D3 / 0.9D1 * S13 - 0.
     #272D3 / 0.9D1 * S24 + 0.136D3 / 0.9D1 * S14 - 0.272D3 / 0.9D1 * S3
     #4 - 0.272D3 / 0.9D1 * S23 + t35 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh101J2
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
      t7 = S34 ** 2
      t15 = S13 ** 2
      t17 = S24 ** 2
      t23 = S23 ** 2
      t25 = S14 ** 2
      t35 = -0.52D2 / 0.9D1 * t7 + (0.160D3 / 0.9D1 * S13 - 0.128D3 / 0.
     #9D1 * S23 + 0.160D3 / 0.9D1 * S14 - 0.104D3 / 0.9D1 * S24) * S34 -
     # 0.40D2 / 0.9D1 * t15 - 0.52D2 / 0.9D1 * t17 + 0.160D3 / 0.9D1 * S
     #24 * S14 - 0.128D3 / 0.9D1 * S23 * S24 - 0.76D2 / 0.9D1 * t23 - 0.
     #40D2 / 0.9D1 * t25 - 0.80D2 / 0.9D1 * S13 * S14 + 0.160D3 / 0.9D1 
     #* S14 * S23 + 0.160D3 / 0.9D1 * S23 * S13 + 0.160D3 / 0.9D1 * S13 
     #* S24
      rrqg2qgh101J2 = (-0.40D2 / 0.9D1 * S12 - 0.80D2 / 0.9D1 * S13 + 0.
     #160D3 / 0.9D1 * S24 - 0.80D2 / 0.9D1 * S14 + 0.160D3 / 0.9D1 * S34
     # + 0.160D3 / 0.9D1 * S23 + t35 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh101J3
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
      rrqg2qgh101J3 = 0.0D0

      end function
  
   
 

      doubleprecision function rrqg2qgh101J4
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
      rrqg2qgh101J4 = 0.0D0

      end function
  
   
 

      doubleprecision function rrqg2qgh101J5
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
      rrqg2qgh101J5 = 0.0D0

      end function
  
   
 

      doubleprecision function rrqg2qgh101J6
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
      t7 = S34 ** 2
      t15 = S13 ** 2
      t17 = S24 ** 2
      t23 = S23 ** 2
      t25 = S14 ** 2
      t35 = -0.94D2 / 0.9D1 * t7 + (0.272D3 / 0.9D1 * S13 - 0.212D3 / 0.
     #9D1 * S23 + 0.272D3 / 0.9D1 * S14 - 0.188D3 / 0.9D1 * S24) * S34 -
     # 0.68D2 / 0.9D1 * t15 - 0.94D2 / 0.9D1 * t17 + 0.272D3 / 0.9D1 * S
     #24 * S14 - 0.212D3 / 0.9D1 * S23 * S24 - 0.118D3 / 0.9D1 * t23 - 0
     #.68D2 / 0.9D1 * t25 - 0.136D3 / 0.9D1 * S13 * S14 + 0.272D3 / 0.9D
     #1 * S14 * S23 + 0.272D3 / 0.9D1 * S23 * S13 + 0.272D3 / 0.9D1 * S1
     #3 * S24
      rrqg2qgh101J6 = (-0.68D2 / 0.9D1 * S12 - 0.136D3 / 0.9D1 * S13 + 0
     #.272D3 / 0.9D1 * S24 - 0.136D3 / 0.9D1 * S14 + 0.272D3 / 0.9D1 * S
     #34 + 0.272D3 / 0.9D1 * S23 + t35 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqg2qgh101J7
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
      t7 = S34 ** 2
      t15 = S14 ** 2
      t19 = S24 ** 2
      t25 = S13 ** 2
      t31 = S23 ** 2
      t35 = 0.52D2 / 0.9D1 * t7 + (-0.160D3 / 0.9D1 * S14 - 0.160D3 / 0.
     #9D1 * S13 + 0.128D3 / 0.9D1 * S23 + 0.104D3 / 0.9D1 * S24) * S34 +
     # 0.40D2 / 0.9D1 * t15 - 0.160D3 / 0.9D1 * S14 * S23 + 0.52D2 / 0.9
     #D1 * t19 + 0.128D3 / 0.9D1 * S23 * S24 - 0.160D3 / 0.9D1 * S13 * S
     #24 + 0.40D2 / 0.9D1 * t25 - 0.160D3 / 0.9D1 * S23 * S13 + 0.80D2 /
     # 0.9D1 * S13 * S14 + 0.76D2 / 0.9D1 * t31 - 0.160D3 / 0.9D1 * S24 
     #* S14
      rrqg2qgh101J7 = (0.40D2 / 0.9D1 * S12 - 0.160D3 / 0.9D1 * S34 + 0.
     #80D2 / 0.9D1 * S13 - 0.160D3 / 0.9D1 * S24 + 0.80D2 / 0.9D1 * S14 
     #- 0.160D3 / 0.9D1 * S23 + t35 / S12) / pi * wd / z

      end function
  
 