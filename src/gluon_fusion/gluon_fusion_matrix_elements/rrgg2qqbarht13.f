  
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
      t15 = t1 ** 2
      t16 = wd * nf * t15
      t17 = x1 * z
      t18 = -z - x1 + t17
      t21 = Sqrt(x3 * t18 * t8)
      t22 = t21 ** 2
      t23 = x4 * 0.3141592653589793D1
      t24 = cos(t23)
      t25 = t24 ** 2
      t26 = t22 * t25
      t27 = 0.1D1 / t18
      t28 = 0.1D1 / x2
      t38 = x1 ** 2
      t39 = Sin(t23)
      t40 = t39 ** 2
      t42 = z ** 2
      t46 = t5 ** 2
      t51 = log(0.4D1 * t38 * t40 / t42 * x3 * t8 * t27 * t46)
      t58 = nf * t15
      t61 = 0.6D1 * t16 * t26 * t27 * t28 + (-0.180D3 * t26 * t27 * lh +
     # 0.180D3 * t26 * t27 - 0.90D2 * t51 * t22 * t25 * t27) * wd * t58 
     #/ 0.15D2
      t62 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, t
     #61)
      t64 = x3 * z
      t65 = t3 * z
      t66 = x2 * x3
      t67 = t66 * z
      t68 = t66 * x1
      t69 = t66 * t17
      t70 = sqrt(x2)
      t78 = Sqrt(-x3 * (-0.1D1 + t70) * (t70 + 0.1D1) * t18 * t8)
      t80 = 0.2D1 * t24 * t70 * t78
      t86 = x2 * x1
      t87 = t86 * z
      t88 = z + x1 - t17 - x2 * z - t86 + t87 - t64 - t3 + t65 + t67 + t
     #68 - t69 + t66 + t80
      t99 = t70 * t38
      t101 = t70 * x2
      t102 = t101 * x1
      t103 = t70 * t42
      t104 = t38 * t101
      t105 = t70 * z
      t106 = t70 * x1
      t107 = t70 * x3
      t110 = t101 * x3
      t119 = t24 * t78
      t126 = -0.2D1 * t99 + t102 - t103 + t104 - t105 - t106 + 0.2D1 * t
     #107 * t42 - 0.2D1 * t110 * t38 - 0.2D1 * t110 * x1 - 0.2D1 * t104 
     #* z - t102 * t42 + t104 * t42 + 0.4D1 * t119 * z + 0.4D1 * t119 * 
     #x1 - 0.2D1 * t106 * z
      t131 = x3 * t38
      t140 = t24 * x2
      t141 = t78 * x1
      t167 = 0.2D1 * t107 * z + 0.2D1 * t107 * x1 + 0.4D1 * t131 * t70 +
     # 0.3D1 * t106 * t42 + 0.4D1 * t99 * z - 0.2D1 * t99 * t42 + 0.4D1 
     #* t140 * t141 * z - 0.4D1 * t140 * t141 - 0.4D1 * t119 * t17 + 0.2
     #D1 * t110 * t42 * x1 + 0.4D1 * t110 * t38 * z - 0.2D1 * t110 * t42
     # * t38 + 0.4D1 * t107 * t17 - 0.6D1 * x3 * t42 * t106 - 0.8D1 * t1
     #31 * t105 + 0.4D1 * t131 * t103
      t169 = (t126 + t167) ** 2
      t171 = t18 ** 2
      t174 = 0.1D1 / (z + x1 + t87 - t17 - t86) * t169 / t171 * t28
      t177 = FJET(XB1, XB2, s, t2 * x1 * (-t64 - t3 + t65 + t67 + t68 - 
     #t69 - x2 + t66 + t80) * t27, -t7, -t2 * x1 * t88 * t27, t13, s * t
     #15 * x2 * x1 * t5 * t27, 0.3D1 / 0.8D1 * t16 * t174)
      rrgg2qqbarht13s1e1 = t62 * t61 + 0.3D1 / 0.8D1 * t177 * wd * t58 *
     # t174

      end function



      doubleprecision function rrgg2qqbarht13s1e0
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
      t15 = t1 ** 2
      t18 = -z - x1 + x1 * z
      t21 = Sqrt(x3 * t18 * t8)
      t22 = t21 ** 2
      t24 = cos(x4 * 0.3141592653589793D1)
      t25 = t24 ** 2
      t27 = 0.1D1 / t18
      t31 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t5 * x3, -t9 * t1 * x1
     #, t9 * t1 * t5, 0.0D0, 0.6D1 * wd * nf * t15 * t22 * t25 * t27)
      rrgg2qqbarht13s1e0 = 0.6D1 * t31 * wd * nf * t15 * t22 * t25 * t27

      end function



      doubleprecision function rrgg2qqbarht13s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht13s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht13s1em4 = 0.0D0

      end function
