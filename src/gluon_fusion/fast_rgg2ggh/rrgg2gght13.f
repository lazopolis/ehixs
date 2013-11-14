  
      subroutine rrgg2gght13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght13s1e1  
      doubleprecision rrgg2gght13s1e0  
      doubleprecision rrgg2gght13s1em1  
      doubleprecision rrgg2gght13s1em2  
      doubleprecision rrgg2gght13s1em3  
      doubleprecision rrgg2gght13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght13s1e1
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
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t13 = t9 * t1 * t5
      t14 = t1 ** 2
      t15 = wd * t14
      t16 = x1 * z
      t17 = -z - x1 + t16
      t20 = Sqrt(x3 * t17 * t8)
      t21 = t20 ** 2
      t23 = x4 * pi
      t24 = cos(t23)
      t25 = t24 ** 2
      t26 = 0.1D1 / t17
      t27 = t25 * t26
      t28 = 0.1D1 / x2
      t32 = wd * t21
      t40 = x1 ** 2
      t41 = x3 * t40
      t42 = Sin(t23)
      t43 = t42 ** 2
      t45 = z ** 2
      t48 = t5 ** 2
      t53 = log(0.4D1 * t41 * t43 / t45 * t26 * t48 * t8)
      t63 = -0.18D2 * t15 * t21 * t27 * t28 - (-0.180D3 * t32 * t27 * lh
     # - 0.90D2 * t32 * t27 + 0.90D2 * (0.2D1 * t32 * t25 - t53 * wd * t
     #21 * t25) * t26) * t14 / 0.5D1
      t64 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, t
     #63)
      t66 = x3 * z
      t67 = t3 * z
      t68 = x2 * x3
      t69 = t68 * z
      t70 = t3 * x2
      t71 = x2 * z
      t72 = t3 * t71
      t73 = sqrt(x2)
      t81 = Sqrt(-x3 * (-0.1D1 + t73) * (t73 + 0.1D1) * t17 * t8)
      t83 = 0.2D1 * t24 * t73 * t81
      t88 = x1 * x2
      t89 = t88 * z
      t90 = z + x1 - t16 - t71 - t88 + t89 - t66 - t3 + t67 + t69 + t70 
     #- t72 + t68 + t83
      t100 = z * t24 * t81
      t104 = x3 * t73
      t109 = t73 * x2
      t110 = t40 * t109
      t113 = t109 * x1
      t116 = x1 * t73
      t121 = t73 * t40
      t130 = t45 * t73
      t136 = -0.4D1 * t100 - 0.4D1 * t41 * t73 - 0.2D1 * t104 * z - 0.2D
     #1 * t3 * t73 + 0.2D1 * x3 * t110 + 0.2D1 * t113 * x3 + 0.2D1 * t11
     #6 * z - 0.3D1 * t116 * t45 - 0.4D1 * t121 * z + 0.2D1 * t121 * t45
     # + 0.2D1 * t110 * z + t113 * t45 - t110 * t45 - 0.2D1 * t130 * x3 
     #- 0.4D1 * x1 * t24 * t81
      t141 = t73 * z
      t149 = x3 * t45
      t154 = t24 * t81
      t162 = -0.4D1 * t88 * t100 + 0.6D1 * t3 * t130 + 0.8D1 * t41 * t14
     #1 - 0.4D1 * t40 * t45 * t104 - 0.4D1 * t110 * t66 - 0.2D1 * t113 *
     # t149 + 0.2D1 * t110 * t149 + 0.4D1 * t16 * t154 + 0.4D1 * t88 * t
     #154 - 0.4D1 * t3 * t141 + t141 + t116 - t113 + t130 - t110 + 0.2D1
     # * t121
      t164 = (t136 + t162) ** 2
      t167 = 0.1D1 / (z - t88 + x1 + t89 - t16)
      t168 = t17 ** 2
      t169 = 0.1D1 / t168
      t174 = FJET(XB1, XB2, s, t2 * x1 * (-t66 - t3 + t67 + t69 + t70 - 
     #t72 - x2 + t68 + t83) * t26, -t7, -t2 * x1 * t90 * t26, t13, s * t
     #14 * x2 * x1 * t5 * t26, -0.9D1 / 0.8D1 * t15 * t164 * t167 * t169
     # * t28)
      rrgg2gght13s1e1 = t64 * t63 - 0.9D1 / 0.8D1 * t174 * wd * t14 * t1
     #64 * t167 * t169 * t28

      end function



      doubleprecision function rrgg2gght13s1e0
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
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t14 = t1 ** 2
      t17 = -z - x1 + x1 * z
      t20 = Sqrt(x3 * t17 * t8)
      t21 = t20 ** 2
      t23 = cos(x4 * pi)
      t24 = t23 ** 2
      t27 = t21 * t24 / t17
      t30 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t5 * x3, -t9 * t1 * x1
     #, t9 * t1 * t5, 0.0D0, -0.18D2 * wd * t14 * t27)
      rrgg2gght13s1e0 = -0.18D2 * t30 * wd * t14 * t27

      end function



      doubleprecision function rrgg2gght13s1em1
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
      rrgg2gght13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gght13s1em2
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
      rrgg2gght13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gght13s1em3
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
      rrgg2gght13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght13s1em4
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
      rrgg2gght13s1em4 = 0.0D0

      end function
