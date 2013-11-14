  
      subroutine rrgg2qqbarht13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarht13s1e1  
      doubleprecision rrgg2qqbarht13s1e0  
      doubleprecision rrgg2qqbarht13s1em1  
      doubleprecision rrgg2qqbarht13s1em2  
      doubleprecision rrgg2qqbarht13s1em3  
      doubleprecision rrgg2qqbarht13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht13s1e1
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
      t15 = t1 ** 2
      t16 = wd * nf * t15
      t17 = x4 * pi
      t18 = cos(t17)
      t19 = t18 ** 2
      t20 = x1 * z
      t21 = -z - x1 + t20
      t22 = 0.1D1 / t21
      t26 = Sqrt(x3 * t21 * t9)
      t27 = t26 ** 2
      t28 = 0.1D1 / x2
      t33 = t19 * wd
      t34 = nf * t15
      t41 = x1 ** 2
      t42 = Sin(t17)
      t43 = t42 ** 2
      t45 = z ** 2
      t49 = t6 ** 2
      t54 = log(0.4D1 * t41 * t43 / t45 * x3 * t9 * t22 * t49)
      t63 = 0.6D1 * t16 * t19 * t22 * t27 * t28 + (0.180D3 * t33 * t34 *
     # t22 + (-0.180D3 * t33 * t34 * lh - 0.90D2 * t54 * t19 * t16) * t2
     #2) * t27 / 0.15D2
      t64 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t63)
      t66 = x3 * z
      t67 = t3 * z
      t68 = x2 * x3
      t69 = t68 * z
      t70 = t68 * x1
      t71 = t68 * t20
      t72 = sqrt(x2)
      t80 = Sqrt(-x3 * (-0.1D1 + t72) * (t72 + 0.1D1) * t21 * t9)
      t82 = 0.2D1 * t18 * t72 * t80
      t88 = x2 * x1
      t89 = t88 * z
      t90 = z + x1 - t20 - x2 * z - t88 + t89 - t66 - t3 + t67 + t69 + t
     #70 - t71 + t68 + t82
      t99 = t18 * t80
      t104 = t45 * t72
      t107 = t41 * t72
      t110 = t72 * x3
      t113 = t72 * x2
      t114 = t113 * x3
      t119 = t41 * t113
      t122 = x1 * t113
      t125 = t41 * t45
      t128 = x3 * t41
      t131 = t72 * x1
      t138 = 0.4D1 * t99 * z + 0.4D1 * t99 * x1 + 0.3D1 * t104 * x1 + 0.
     #4D1 * t107 * z + 0.2D1 * t110 * t45 - 0.2D1 * t114 * t41 - 0.2D1 *
     # t114 * x1 - 0.2D1 * t119 * z - t122 * t45 + t119 * t45 - 0.2D1 * 
     #t125 * t72 + 0.4D1 * t128 * t72 - 0.2D1 * t131 * z + 0.2D1 * t110 
     #* z + 0.2D1 * t110 * x1
      t139 = t18 * x2
      t140 = t80 * x1
      t144 = t72 * z
      t167 = 0.4D1 * t139 * t140 * z - 0.8D1 * t128 * t144 + 0.4D1 * t11
     #0 * t20 + 0.2D1 * t114 * t45 * x1 + 0.4D1 * t128 * t104 - 0.6D1 * 
     #x3 * t45 * t131 - 0.2D1 * t114 * t125 + 0.4D1 * t114 * t41 * z - 0
     #.4D1 * t99 * t20 - 0.4D1 * t139 * t140 - 0.2D1 * t107 + t122 - t10
     #4 + t119 - t131 - t144
      t169 = (t138 + t167) ** 2
      t170 = t21 ** 2
      t176 = t169 / t170 / (z + t89 - t20 + x1 - t88) * t28
      t179 = FJET(XB1, XB2, s, t2 * x1 * (-t66 - t3 + t67 + t69 + t70 - 
     #t71 - x2 + t68 + t82) * t22, -t8, -t2 * x1 * t90 * t22, t13, s * t
     #15 * x2 * x1 * t6 * t22, 0.3D1 / 0.8D1 * t16 * t176)
      rrgg2qqbarht13s1e1 = t64 * t63 + 0.3D1 / 0.8D1 * t179 * wd * t34 *
     # t176

      end function



      doubleprecision function rrgg2qqbarht13s1e0
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
      t7 = t1 * (-0.1D1 + x1)
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t15 = t1 ** 2
      t18 = cos(x4 * pi)
      t19 = t18 ** 2
      t21 = -z - x1 + x1 * z
      t22 = 0.1D1 / t21
      t26 = Sqrt(x3 * t21 * t9)
      t27 = t26 ** 2
      t31 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, 0.6D1 * wd * nf * t15 * t19 * t22 * t27)
      rrgg2qqbarht13s1e0 = 0.6D1 * t31 * wd * nf * t15 * t19 * t22 * t27

      end function



      doubleprecision function rrgg2qqbarht13s1em1
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
      rrgg2qqbarht13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht13s1em2
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
      rrgg2qqbarht13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht13s1em3
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
      rrgg2qqbarht13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht13s1em4
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
      rrgg2qqbarht13s1em4 = 0.0D0

      end function
