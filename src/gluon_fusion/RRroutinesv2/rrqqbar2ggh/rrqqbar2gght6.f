  
      subroutine rrqqbar2gght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2gght6s1e1  
      doubleprecision rrqqbar2gght6s1e0  
      doubleprecision rrqqbar2gght6s1em1  
      doubleprecision rrqqbar2gght6s1em2  
      doubleprecision rrqqbar2gght6s1em3  
      doubleprecision rrqqbar2gght6s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght6s1e1
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
      t3 = x3 * x1
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t8 = x3 * s * t7
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t13 = t10 * t7
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t1 ** 2
      t18 = t17 * t1
      t19 = t16 * t18
      t20 = x1 * t6
      t21 = t19 * t20
      t22 = 0.1D1 / z
      t23 = t22 * wd
      t25 = x4 * pi
      t26 = Sin(t25)
      t27 = t26 ** 2
      t28 = z ** 2
      t29 = 0.1D1 / t28
      t30 = t27 * t29
      t32 = t17 ** 2
      t33 = x1 ** 2
      t35 = t6 ** 2
      t41 = log(0.4D1 * x2 * t16 * t30 * t32 * t33 * t35 * x3 * t9)
      t42 = cos(t25)
      t43 = t42 ** 2
      t47 = Sqrt(x3 * t15 * t9)
      t48 = t47 ** 2
      t58 = t16 * lh
      t63 = -0.90D2 * t19 * x1 * t6 * t22 * wd - 0.180D3 * t58 * t18 * t
     #20 * t23
      t68 = 0.1D1 / x2
      t75 = t33 * t35
      t80 = log(0.4D1 * t16 * t27 * t29 * t32 * t75 * x3 * t9)
      t81 = t80 * t16
      t87 = t23 * t43 * t48
      t92 = t80 ** 2
      t95 = lh ** 2
      t97 = pi ** 2
      t106 = -0.4D1 / 0.135D3 * (-0.360D3 * t21 * t23 * t41 * t43 * t48 
     #+ 0.4D1 * t63 * t43 * t48) * t68 + 0.16D2 / 0.135D3 * (-0.180D3 * 
     #t58 - 0.90D2 * t81) * t18 * t20 * t87 - 0.16D2 / 0.135D3 * (0.180D
     #3 * t81 * lh + 0.45D2 * t92 * t16 + t16 * (0.180D3 * t95 - 0.30D2 
     #* t97)) * t18 * t20 * t87
      t107 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0,
     # t106)
      t109 = x3 * z
      t110 = t3 * z
      t111 = x2 * x3
      t112 = t111 * z
      t113 = t111 * x1
      t114 = t111 * t14
      t115 = sqrt(x2)
      t117 = -0.1D1 + t115
      t119 = t115 + 0.1D1
      t123 = Sqrt(-x3 * t117 * t119 * t15 * t9)
      t125 = 0.2D1 * t42 * t115 * t123
      t131 = x2 * x1
      t133 = z + x1 - t14 - x2 * z - t131 + t131 * z - t109 - t3 + t110 
     #+ t112 + t113 - t114 + t111 + t125
      t150 = log(-0.4D1 * t16 * t32 * t75 * x3 * t9 * t117 * t119 * t30 
     #* x2)
      t156 = (-t115 + 0.2D1 * t115 * x3 + 0.2D1 * t42 * t123) ** 2
      t162 = 0.90D2 * t21 * t23 * t150 * t156 - t63 * t156
      t165 = FJET(XB1, XB2, s, t2 * x1 * (-t109 - t3 + t110 + t112 + t11
     #3 - t114 - x2 + t111 + t125) * t16, -t8, -t2 * x1 * t133 * t16, t1
     #3, s * t17 * x2 * t20 * t16, -0.4D1 / 0.135D3 * t162 * t68)
      rrqqbar2gght6s1e1 = t107 * t106 - 0.4D1 / 0.135D3 * t165 * t162 * 
     #t68

      end function



      doubleprecision function rrqqbar2gght6s1e0
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
      t3 = x3 * x1
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t8 = x3 * s * t7
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t13 = t10 * t7
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t1 ** 2
      t18 = t17 * t1
      t20 = x1 * t6
      t21 = t16 * t18 * t20
      t22 = 0.1D1 / z
      t23 = t22 * wd
      t24 = x4 * pi
      t25 = cos(t24)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t15 * t9)
      t30 = t29 ** 2
      t31 = t26 * t30
      t32 = 0.1D1 / x2
      t37 = t23 * t31
      t42 = Sin(t24)
      t43 = t42 ** 2
      t45 = z ** 2
      t47 = t17 ** 2
      t50 = x1 ** 2
      t51 = t6 ** 2
      t57 = log(0.4D1 * t16 * t43 / t45 * t47 * t50 * t51 * x3 * t9)
      t65 = -0.32D2 / 0.3D1 * t21 * t23 * t31 * t32 + 0.32D2 / 0.3D1 * t
     #21 * t37 - 0.16D2 / 0.135D3 * (-0.180D3 * t16 * lh - 0.90D2 * t57 
     #* t16) * t18 * t20 * t37
      t66 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t65)
      t68 = x3 * z
      t69 = t3 * z
      t70 = x2 * x3
      t71 = t70 * z
      t72 = t70 * x1
      t73 = t70 * t14
      t74 = sqrt(x2)
      t82 = Sqrt(-x3 * (-0.1D1 + t74) * (t74 + 0.1D1) * t15 * t9)
      t84 = 0.2D1 * t25 * t74 * t82
      t90 = x2 * x1
      t92 = z + x1 - t14 - x2 * z - t90 + t90 * z - t68 - t3 + t69 + t71
     # + t72 - t73 + t70 + t84
      t105 = (-t74 + 0.2D1 * t74 * x3 + 0.2D1 * t25 * t82) ** 2
      t110 = FJET(XB1, XB2, s, t2 * x1 * (-t68 - t3 + t69 + t71 + t72 - 
     #t73 - x2 + t70 + t84) * t16, -t8, -t2 * x1 * t92 * t16, t13, s * t
     #17 * x2 * t20 * t16, 0.8D1 / 0.3D1 * t21 * t23 * t105 * t32)
      rrqqbar2gght6s1e0 = t66 * t65 + 0.8D1 / 0.3D1 * t110 * t16 * t18 *
     # x1 * t6 * t22 * wd * t105 * t32

      end function



      doubleprecision function rrqqbar2gght6s1em1
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
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t15 = -z - x1 + x1 * z
      t16 = 0.1D1 / t15
      t17 = t1 ** 2
      t18 = t17 * t1
      t22 = 0.1D1 / z
      t25 = cos(x4 * pi)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t15 * t9)
      t30 = t29 ** 2
      t35 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, -0.32D2 / 0.3D1 * t16 * t18 * x1 * t6 * t22
     # * wd * t26 * t30)
      rrqqbar2gght6s1em1 = -0.32D2 / 0.3D1 * t35 * t16 * t18 * x1 * t6 *
     # t22 * wd * t26 * t30

      end function



      doubleprecision function rrqqbar2gght6s1em2
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
      rrqqbar2gght6s1em2 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght6s1em3
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
      rrqqbar2gght6s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght6s1em4
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
      rrqqbar2gght6s1em4 = 0.0D0

      end function
