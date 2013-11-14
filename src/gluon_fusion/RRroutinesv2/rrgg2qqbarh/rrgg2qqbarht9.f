  
      subroutine rrgg2qqbarht9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh91J1  
      doubleprecision rrgg2qqbarh91J2  
      doubleprecision rrgg2qqbarh91J3  
      doubleprecision rrgg2qqbarh91J4  
      doubleprecision rrgg2qqbarh91J5  
      doubleprecision rrgg2qqbarh91J6  
      doubleprecision rrgg2qqbarh91J7  
      doubleprecision rrgg2qqbarht9s1e1  
      doubleprecision rrgg2qqbarht9s1e0  
      doubleprecision rrgg2qqbarht9s1em1  
      doubleprecision rrgg2qqbarht9s1em2  
      doubleprecision rrgg2qqbarht9s1em3  
      doubleprecision rrgg2qqbarht9s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht9s1e1
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
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7

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
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = pi * t19
      t21 = 0.1D1 / s
      t22 = x2 * z
      t24 = (t22 + 0.1D1 - x2) ** 2
      t25 = 0.1D1 / t24
      t26 = rrgg2qqbarh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t28 = x1 ** 2
      t30 = Sin(t5)
      t31 = t30 ** 2
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t39 = log(0.4D1 * t3 * t28 * t31 * t33 * t7 * t9)
      t41 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t43 = -t25 * t26 + t39 * t25 * t41
      t47 = pi * lh
      t49 = t21 * t25
      t50 = t49 * t41
      t52 = 0.180D3 * t47 * t19 * t50
      t54 = 0.1D1 / x1
      t57 = rrgg2qqbarh91J3(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t60 = t20 * t49 * t57 / 0.16D2
      t67 = log(0.4D1 * x2 * t31 * t33 * t10 * t7)
      t68 = t67 * pi
      t74 = (-0.180D3 * t47 - 0.90D2 * t68) * t19 * t49 * t26 / 0.1440D4
      t77 = t67 ** 2
      t80 = lh ** 2
      t82 = pi ** 2
      t89 = (0.180D3 * t68 * lh + 0.45D2 * t77 * pi + pi * (0.180D3 * t8
     #0 - 0.30D2 * t82)) * t19 * t50 / 0.1440D4
      t90 = (0.90D2 * t20 * t21 * t43 + t52) * t54 / 0.720D3 - t60 - t74
     # - t89
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
      t126 = s * t19 * x2 * x1 * t95 * t110
      t127 = t101 * t95
      t129 = (-t22 + t117 - t98 - 0.1D1 + x1 + x2 - t116) ** 2
      t130 = 0.1D1 / t129
      t131 = rrgg2qqbarh91J2(s, XB1, XB2, z, lh, wd, nf, s, t112, -t121,
     # t94, -t115, -t126)
      t137 = t95 ** 2
      t143 = log(0.4D1 * t3 * t28 * t31 * t33 * t110 * t137 * t7 * t9)
      t146 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, s, t112, -t121,
     # t94, -t115, -t126)
      t159 = 0.90D2 * t20 * t21 * (-t127 * t130 * t131 + t143 * t101 * t
     #95 * t130 * t146) + 0.180D3 * t47 * t19 * t21 * t127 * t130 * t146
      t161 = t159 * t54 / 0.720D3
      t162 = FJET(XB1, XB2, s, t94, t112, -t115, -t121, -t126, t161)
      t166 = FJET(XB1, XB2, s, t112, t94, -t121, -t115, -t126, t161)
      t177 = (0.90D2 * t20 * t21 * t43 + t52) * t54 / 0.720D3 - t60 - t7
     #4 - t89
      t178 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t177)
      rrgg2qqbarht9s1e1 = t91 * t90 + t162 * t159 * t54 / 0.720D3 + t166
     # * t159 * t54 / 0.720D3 + t178 * t177

      end function



      doubleprecision function rrgg2qqbarht9s1e0
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
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7

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
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = pi * t19
      t21 = 0.1D1 / s
      t23 = x2 * z
      t25 = (t23 + 0.1D1 - x2) ** 2
      t26 = 0.1D1 / t25
      t27 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t29 = 0.1D1 / x1
      t33 = t21 * t26
      t34 = rrgg2qqbarh91J2(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t40 = Sin(t5)
      t41 = t40 ** 2
      t43 = z ** 2
      t49 = log(0.4D1 * x2 * t41 / t43 * t10 * t7)
      t57 = -t20 * t21 * t26 * t27 * t29 / 0.8D1 - t20 * t33 * t34 / 0.1
     #6D2 - (-0.180D3 * pi * lh - 0.90D2 * t49 * pi) * t19 * t33 * t27 /
     # 0.1440D4
      t58 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t57)
      t60 = x3 * x1
      t61 = t2 * t60
      t62 = -0.1D1 + x1
      t63 = t60 * z
      t64 = t3 * x1
      t65 = x1 * z
      t66 = t3 * t65
      t68 = 0.1D1 - x1 + t65
      t72 = Sqrt(x3 * t7 * t68 * x2 * t9)
      t74 = 0.2D1 * t6 * t72
      t77 = 0.1D1 / t68
      t79 = t2 * t62 * (-x3 + t60 - t63 + t4 - t64 + t66 - x2 + t74) * t
     #77
      t82 = t9 * s * t1 * x1
      t83 = x2 * x1
      t84 = t83 * z
      t85 = 0.1D1 - x1 + t65 - x2 + t83 - t84 - x3 + t60 - t63 + t4 - t6
     #4 + t66 + t74
      t88 = t2 * t62 * t85 * t77
      t93 = s * t19 * x2 * x1 * t62 * t77
      t97 = (-t23 + t84 - t65 - 0.1D1 + x1 + x2 - t83) ** 2
      t98 = 0.1D1 / t97
      t100 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, s, t79, -t88, t
     #61, -t82, -t93)
      t104 = t20 * t21 * t68 * t62 * t98 * t100 * t29 / 0.8D1
      t105 = FJET(XB1, XB2, s, t61, t79, -t82, -t88, -t93, -t104)
      t107 = t19 * t21
      t112 = t68 * t62 * t98 * t100 * t29
      t115 = FJET(XB1, XB2, s, t79, t61, -t88, -t82, -t93, -t104)
      t120 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t57)
      rrgg2qqbarht9s1e0 = t58 * t57 - t105 * pi * t107 * t112 / 0.8D1 - 
     #t115 * pi * t107 * t112 / 0.8D1 + t120 * t57

      end function



      doubleprecision function rrgg2qqbarht9s1em1
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
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7

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
      t19 = t1 ** 2
      t24 = (x2 * z + 0.1D1 - x2) ** 2
      t27 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, s, -t16, t18, 0.
     #0D0, 0.0D0, 0.0D0)
      t28 = 0.1D1 / s / t24 * t27
      t30 = pi * t19 * t28 / 0.16D2
      t31 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, -t30)
      t35 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, -t30)
      rrgg2qqbarht9s1em1 = -t31 * pi * t19 * t28 / 0.16D2 - t35 * pi * t
     #19 * t28 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht9s1em2
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
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht9s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht9s1em3
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
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht9s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht9s1em4
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
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht9s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh91J1
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
      t12 = S34 ** 2
      t23 = S13 ** 2
      t25 = S24 ** 2
      t31 = S23 ** 2
      t33 = S14 ** 2
      rrgg2qqbarh91J1 = (-0.17D2 / 0.6D1 * nf * S12 + 0.34D2 / 0.3D1 * n
     #f * S34 + (0.136D3 * S23 - 0.68D2 * S14 - 0.68D2 * S13 + 0.136D3 *
     # S24) * nf / 0.12D2 + (-0.59D2 / 0.12D2 * nf * t12 + (-0.106D3 * S
     #24 + 0.136D3 * S13 + 0.136D3 * S14 - 0.106D3 * S23) * nf * S34 / 0
     #.12D2 + (-0.34D2 * t23 - 0.47D2 * t25 - 0.94D2 * S23 * S24 + 0.136
     #D3 * S14 * S23 - 0.47D2 * t31 - 0.34D2 * t33 + 0.136D3 * S24 * S14
     # + 0.136D3 * S23 * S13 - 0.68D2 * S13 * S14 + 0.136D3 * S13 * S24)
     # * nf / 0.12D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J2
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
      t12 = S34 ** 2
      t23 = S13 ** 2
      t25 = S24 ** 2
      t31 = S23 ** 2
      t33 = S14 ** 2
      rrgg2qqbarh91J2 = (-0.7D1 / 0.6D1 * nf * S12 + 0.14D2 / 0.3D1 * nf
     # * S34 - (-0.56D2 * S23 + 0.28D2 * S14 + 0.28D2 * S13 - 0.56D2 * S
     #24) * nf / 0.12D2 + (-0.7D1 / 0.4D1 * nf * t12 - (0.42D2 * S24 - 0
     #.56D2 * S13 - 0.56D2 * S14 + 0.42D2 * S23) * nf * S34 / 0.12D2 - (
     #0.14D2 * t23 + 0.21D2 * t25 + 0.42D2 * S23 * S24 - 0.56D2 * S14 * 
     #S23 + 0.21D2 * t31 + 0.14D2 * t33 - 0.56D2 * S24 * S14 - 0.56D2 * 
     #S23 * S13 + 0.28D2 * S13 * S14 - 0.56D2 * S13 * S24) * nf / 0.12D2
     #) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J3
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
      t12 = S34 ** 2
      t23 = S13 ** 2
      t25 = S24 ** 2
      t31 = S23 ** 2
      t33 = S14 ** 2
      rrgg2qqbarh91J3 = (-0.7D1 / 0.6D1 * nf * S12 + 0.14D2 / 0.3D1 * nf
     # * S34 - (-0.56D2 * S23 + 0.28D2 * S14 + 0.28D2 * S13 - 0.56D2 * S
     #24) * nf / 0.12D2 + (-0.7D1 / 0.4D1 * nf * t12 - (0.42D2 * S24 - 0
     #.56D2 * S13 - 0.56D2 * S14 + 0.42D2 * S23) * nf * S34 / 0.12D2 - (
     #0.14D2 * t23 + 0.21D2 * t25 + 0.42D2 * S23 * S24 - 0.56D2 * S14 * 
     #S23 + 0.21D2 * t31 + 0.14D2 * t33 - 0.56D2 * S24 * S14 - 0.56D2 * 
     #S23 * S13 + 0.28D2 * S13 * S14 - 0.56D2 * S13 * S24) * nf / 0.12D2
     #) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J4
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
      t12 = S34 ** 2
      t23 = S13 ** 2
      t25 = S24 ** 2
      t31 = S23 ** 2
      t33 = S14 ** 2
      rrgg2qqbarh91J4 = (-0.7D1 / 0.6D1 * nf * S12 + 0.14D2 / 0.3D1 * nf
     # * S34 - (-0.56D2 * S23 + 0.28D2 * S14 + 0.28D2 * S13 - 0.56D2 * S
     #24) * nf / 0.12D2 + (-0.7D1 / 0.4D1 * nf * t12 - (0.42D2 * S24 - 0
     #.56D2 * S13 - 0.56D2 * S14 + 0.42D2 * S23) * nf * S34 / 0.12D2 - (
     #0.14D2 * t23 + 0.21D2 * t25 + 0.42D2 * S23 * S24 - 0.56D2 * S14 * 
     #S23 + 0.21D2 * t31 + 0.14D2 * t33 - 0.56D2 * S24 * S14 - 0.56D2 * 
     #S23 * S13 + 0.28D2 * S13 * S14 - 0.56D2 * S13 * S24) * nf / 0.12D2
     #) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J5
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
      t12 = S34 ** 2
      t23 = S13 ** 2
      t25 = S24 ** 2
      t31 = S23 ** 2
      t33 = S14 ** 2
      rrgg2qqbarh91J5 = (-0.7D1 / 0.6D1 * nf * S12 + 0.14D2 / 0.3D1 * nf
     # * S34 - (-0.56D2 * S23 + 0.28D2 * S14 + 0.28D2 * S13 - 0.56D2 * S
     #24) * nf / 0.12D2 + (-0.7D1 / 0.4D1 * nf * t12 - (0.42D2 * S24 - 0
     #.56D2 * S13 - 0.56D2 * S14 + 0.42D2 * S23) * nf * S34 / 0.12D2 - (
     #0.14D2 * t23 + 0.21D2 * t25 + 0.42D2 * S23 * S24 - 0.56D2 * S14 * 
     #S23 + 0.21D2 * t31 + 0.14D2 * t33 - 0.56D2 * S24 * S14 - 0.56D2 * 
     #S23 * S13 + 0.28D2 * S13 * S14 - 0.56D2 * S13 * S24) * nf / 0.12D2
     #) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J6
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
      t12 = S34 ** 2
      t23 = S13 ** 2
      t25 = S24 ** 2
      t31 = S23 ** 2
      t33 = S14 ** 2
      rrgg2qqbarh91J6 = (0.95D2 / 0.6D1 * nf * S12 - 0.190D3 / 0.3D1 * n
     #f * S34 - (0.760D3 * S23 - 0.380D3 * S14 - 0.380D3 * S13 + 0.760D3
     # * S24) * nf / 0.12D2 + (0.111D3 / 0.4D1 * nf * t12 - (-0.594D3 * 
     #S24 + 0.760D3 * S13 + 0.760D3 * S14 - 0.594D3 * S23) * nf * S34 / 
     #0.12D2 - (-0.190D3 * t23 - 0.261D3 * t25 - 0.522D3 * S23 * S24 + 0
     #.760D3 * S14 * S23 - 0.261D3 * t31 - 0.190D3 * t33 + 0.760D3 * S24
     # * S14 + 0.760D3 * S23 * S13 - 0.380D3 * S13 * S14 + 0.760D3 * S13
     # * S24) * nf / 0.12D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J7
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
      t12 = S34 ** 2
      t25 = S14 ** 2
      t29 = S24 ** 2
      t31 = S13 ** 2
      t41 = S23 ** 2
      rrgg2qqbarh91J7 = (-0.25D2 / 0.3D1 * nf * S12 + 0.100D3 / 0.3D1 * 
     #nf * S34 - 0.5D1 / 0.12D2 * (0.40D2 * S13 - 0.80D2 * S24 + 0.40D2 
     #* S14 - 0.80D2 * S23) * nf + (-0.95D2 / 0.6D1 * nf * t12 - 0.5D1 /
     # 0.12D2 * (0.64D2 * S24 - 0.80D2 * S14 - 0.80D2 * S13 + 0.64D2 * S
     #23) * nf * S34 - 0.5D1 / 0.12D2 * (0.52D2 * S23 * S24 + 0.20D2 * t
     #25 - 0.80D2 * S23 * S13 + 0.26D2 * t29 + 0.20D2 * t31 - 0.80D2 * S
     #14 * S23 - 0.80D2 * S24 * S14 + 0.40D2 * S13 * S14 - 0.80D2 * S13 
     #* S24 + 0.26D2 * t41) * nf) / S12) / pi * wd / z

      end function
  
 