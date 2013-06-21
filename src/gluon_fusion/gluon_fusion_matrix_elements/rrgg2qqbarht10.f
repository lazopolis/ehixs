  
      subroutine rrgg2qqbarht10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarht10s1e1  
      doubleprecision rrgg2qqbarht10s1e0  
      doubleprecision rrgg2qqbarht10s1em1  
      doubleprecision rrgg2qqbarht10s1em2  
      doubleprecision rrgg2qqbarht10s1em3  
      doubleprecision rrgg2qqbarht10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht10s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * x1
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t13 = t9 * t1 * t5
      t14 = wd * nf
      t15 = 0.1D1 / z
      t16 = t15 * x1
      t18 = t14 * t16 * t5
      t19 = t1 ** 2
      t20 = t19 ** 2
      t21 = x1 * z
      t22 = -z - x1 + t21
      t23 = 0.1D1 / t22
      t24 = t20 * t23
      t25 = x1 ** 2
      t27 = x4 * 0.3141592653589793D1
      t28 = Sin(t27)
      t29 = t28 ** 2
      t30 = z ** 2
      t31 = 0.1D1 / t30
      t34 = x3 * t8
      t35 = t5 ** 2
      t37 = t34 * t23 * t35
      t40 = log(0.4D1 * x2 * t25 * t29 * t31 * t37)
      t41 = cos(t27)
      t42 = t41 ** 2
      t46 = Sqrt(x3 * t22 * t8)
      t47 = t46 ** 2
      t52 = t14 * t16
      t53 = t5 * t20
      t59 = x1 * t5
      t63 = -0.180D3 * t52 * t53 * t23 * lh + 0.90D2 * t14 * t15 * t59 *
     # t24
      t68 = 0.1D1 / x2
      t80 = log(0.4D1 * t25 * t29 * t31 * t37)
      t82 = t80 * wd * nf
      t86 = t20 * t42
      t87 = t47 * t23
      t88 = t86 * t87
      t89 = (0.2D1 * t14 - t82) * t15 * t59 * t88
      t96 = t80 ** 2
      t105 = lh ** 2
      t107 = 0.3141592653589793D1 ** 2
      t114 = (-0.360D3 * t18 * t24 * t40 * t42 * t47 + 0.4D1 * t63 * t42
     # * t47) * t68 / 0.120D3 - 0.6D1 * (-t52 * t53 * t42 * t47 * t23 + 
     #t89) * lh - 0.3D1 * t89 + 0.3D1 * (0.3D1 * t14 - 0.2D1 * t82 + t96
     # * wd * nf / 0.2D1) * t15 * t59 * t88 + t18 * t86 * t87 * (0.180D3
     # * t105 - 0.30D2 * t107) / 0.30D2
      t115 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, 
     #t114)
      t117 = x3 * z
      t118 = t3 * z
      t119 = x2 * x3
      t120 = t119 * z
      t121 = t119 * x1
      t122 = t119 * t21
      t123 = sqrt(x2)
      t125 = -0.1D1 + t123
      t127 = t123 + 0.1D1
      t131 = Sqrt(-x3 * t125 * t127 * t22 * t8)
      t133 = 0.2D1 * t41 * t123 * t131
      t139 = x2 * x1
      t141 = z + x1 - t21 - x2 * z - t139 + t139 * z - t117 - t3 + t118 
     #+ t120 + t121 - t122 + t119 + t133
      t158 = log(-0.4D1 * t125 * t127 * t34 * t31 * t25 * t35 * t29 * t2
     #3 * x2)
      t165 = (-t123 + 0.2D1 * t41 * t131 + 0.2D1 * t123 * x3) ** 2
      t171 = 0.90D2 * t52 * t53 * t23 * t158 * t165 - t63 * t165
      t174 = FJET(XB1, XB2, s, t2 * x1 * (-t117 - t3 + t118 + t120 + t12
     #1 - t122 - x2 + t119 + t133) * t23, -t7, -t2 * x1 * t141 * t23, t1
     #3, s * t19 * x2 * t59 * t23, t171 * t68 / 0.120D3)
      rrgg2qqbarht10s1e1 = t115 * t114 + t174 * t171 * t68 / 0.120D3

      end function



      doubleprecision function rrgg2qqbarht10s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * x1
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t13 = t9 * t1 * t5
      t14 = wd * nf
      t15 = 0.1D1 / z
      t16 = t15 * x1
      t18 = t14 * t16 * t5
      t19 = t1 ** 2
      t20 = t19 ** 2
      t21 = x4 * 0.3141592653589793D1
      t22 = cos(t21)
      t23 = t22 ** 2
      t24 = t20 * t23
      t25 = x1 * z
      t26 = -z - x1 + t25
      t29 = Sqrt(x3 * t26 * t8)
      t30 = t29 ** 2
      t31 = 0.1D1 / t26
      t32 = t30 * t31
      t33 = 0.1D1 / x2
      t42 = t14 * t16
      t43 = t5 * t20
      t50 = x1 ** 2
      t51 = Sin(t21)
      t52 = t51 ** 2
      t54 = z ** 2
      t58 = t5 ** 2
      t63 = log(0.4D1 * t50 * t52 / t54 * x3 * t8 * t31 * t58)
      t68 = x1 * t5
      t73 = 0.3D1 * t18 * t24 * t32 * t33 - 0.6D1 * t18 * t24 * t32 * lh
     # - 0.3D1 * t42 * t43 * t23 * t30 * t31 + 0.3D1 * (0.2D1 * t14 - t6
     #3 * wd * nf) * t15 * t68 * t24 * t32
      t74 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, t
     #73)
      t76 = x3 * z
      t77 = t3 * z
      t78 = x2 * x3
      t79 = t78 * z
      t80 = t78 * x1
      t81 = t78 * t25
      t82 = sqrt(x2)
      t90 = Sqrt(-x3 * (-0.1D1 + t82) * (t82 + 0.1D1) * t26 * t8)
      t92 = 0.2D1 * t22 * t82 * t90
      t98 = x2 * x1
      t100 = z + x1 - t25 - x2 * z - t98 + t98 * z - t76 - t3 + t77 + t7
     #9 + t80 - t81 + t78 + t92
      t113 = (-t82 + 0.2D1 * t22 * t90 + 0.2D1 * t82 * x3) ** 2
      t116 = t43 * t31 * t113 * t33
      t119 = FJET(XB1, XB2, s, t2 * x1 * (-t76 - t3 + t77 + t79 + t80 - 
     #t81 - x2 + t78 + t92) * t31, -t7, -t2 * x1 * t100 * t31, t13, s * 
     #t19 * x2 * t68 * t31, -0.3D1 / 0.4D1 * t42 * t116)
      rrgg2qqbarht10s1e0 = t74 * t73 - 0.3D1 / 0.4D1 * t119 * wd * nf * 
     #t15 * x1 * t116

      end function



      doubleprecision function rrgg2qqbarht10s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t15 = 0.1D1 / z
      t18 = t1 ** 2
      t19 = t18 ** 2
      t22 = cos(x4 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t25 = -z - x1 + x1 * z
      t28 = Sqrt(x3 * t25 * t8)
      t29 = t28 ** 2
      t33 = t5 * t19 * t23 * t29 / t25
      t36 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t5 * x3, -t9 * t1 * x1
     #, t9 * t1 * t5, 0.0D0, 0.3D1 * wd * nf * t15 * x1 * t33)
      rrgg2qqbarht10s1em1 = 0.3D1 * t36 * wd * nf * t15 * x1 * t33

      end function



      doubleprecision function rrgg2qqbarht10s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht10s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht10s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht10s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht10s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht10s1em4 = 0.0D0

      end function
