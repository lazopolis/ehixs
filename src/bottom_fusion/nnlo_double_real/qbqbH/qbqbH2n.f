  
      subroutine qbqbH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision qbqbH21J1  
      doubleprecision qbqbH21J2  
      doubleprecision qbqbH2n1e1  
      doubleprecision qbqbH2n1e0  
      doubleprecision qbqbH2n1em1  
      doubleprecision qbqbH2n1em2  
      doubleprecision qbqbH2n1em3  
      doubleprecision qbqbH2n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=qbqbH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=qbqbH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=qbqbH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=qbqbH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=qbqbH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=qbqbH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function qbqbH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH21J1
      doubleprecision qbqbH21J2
      t1 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t7 * x4
      t10 = -0.1D1 + x4
      t11 = t7 * t10
      t13 = t1 ** 2
      t15 = t3 ** 2
      t17 = x1 * t6
      t20 = t15 * t3
      t21 = t20 * t6
      t22 = t13 * t1
      t24 = 0.1D1 / (-0.2D1 + t1)
      t25 = t22 * t24
      t26 = qbqbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t27 = t25 * t26
      t28 = x2 * 0.3141592653589793D1
      t29 = sin(t28)
      t30 = t29 ** 2
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t34 = x1 ** 2
      t36 = x3 * t30 * t33 * t34
      t37 = t6 ** 2
      t38 = t37 * x4
      t39 = t13 ** 2
      t44 = log(-0.4D1 * t36 * t38 * t10 * t39)
      t46 = qbqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t53 = lh * t20 * t6
      t54 = t25 * t46
      t58 = 0.1D1 / x3
      t64 = x4 * t10
      t68 = log(-0.4D1 * t39 * t30 * t33 * t34 * t37 * t64)
      t74 = lh ** 2
      t76 = 0.3141592653589793D1 ** 2
      t80 = t68 ** 2
      t87 = (0.90D2 * t21 * (-t27 + t44 * t22 * t24 * t46) + 0.180D3 * t
     #53 * t54) * t58 / 0.2880D4 - (-0.180D3 * lh - 0.90D2 * t68) * t20 
     #* t6 * t27 / 0.2880D4 - (0.180D3 * t74 - 0.30D2 * t76 + 0.180D3 * 
     #t68 * lh + 0.45D2 * t80) * t20 * t6 * t54 / 0.2880D4
      t88 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t8, t2 * t11, -s * t
     #13 * t15 * t17 * x4, t87)
      t90 = KAPPA2(x1, x2, x3, x4, z)
      t91 = s * t90
      t94 = -0.1D1 + x3
      t99 = t90 ** 2
      t104 = cos(t28)
      t107 = sqrt(x3 * t94 * t64)
      t113 = t99 * t90
      t115 = 0.1D1 / (-0.2D1 + t90)
      t116 = t113 * t115
      t117 = qbqbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t119 = t99 ** 2
      t125 = log(0.4D1 * t36 * t38 * t10 * t119 * t94)
      t127 = qbqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t136 = 0.90D2 * t21 * (t116 * t117 - t125 * t113 * t115 * t127) - 
     #0.180D3 * t53 * t116 * t127
      t139 = FJET(XB1, XB2, s, t91 * t4 * x3, -t91 * t4 * t94, -t91 * t8
     #, t91 * t11, s * t99 * t15 * t17 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0
     #.2D1 * t104 * t107), t136 * t58 / 0.2880D4)
      qbqbH2n1e1 = t88 * t87 + t139 * t136 * t58 / 0.2880D4

      end function



      doubleprecision function qbqbH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH21J1
      doubleprecision qbqbH21J2
      t1 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t7 * x4
      t10 = -0.1D1 + x4
      t11 = t7 * t10
      t13 = t1 ** 2
      t15 = t3 ** 2
      t17 = x1 * t6
      t20 = t15 * t3
      t21 = t20 * t6
      t22 = t13 * t1
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = qbqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t28 = 0.1D1 / x3
      t32 = t22 * t25
      t33 = qbqbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t38 = t13 ** 2
      t39 = x2 * 0.3141592653589793D1
      t40 = sin(t39)
      t41 = t40 ** 2
      t43 = z ** 2
      t46 = x1 ** 2
      t47 = t6 ** 2
      t49 = x4 * t10
      t53 = log(-0.4D1 * t38 * t41 / t43 * t46 * t47 * t49)
      t61 = -t21 * t22 * t25 * t26 * t28 / 0.32D2 - t21 * t32 * t33 / 0.
     #32D2 - (-0.180D3 * lh - 0.90D2 * t53) * t20 * t6 * t32 * t26 / 0.2
     #880D4
      t62 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t8, t2 * t11, -s * t
     #13 * t15 * t17 * x4, t61)
      t64 = KAPPA2(x1, x2, x3, x4, z)
      t65 = s * t64
      t68 = -0.1D1 + x3
      t73 = t64 ** 2
      t78 = cos(t39)
      t81 = sqrt(x3 * t68 * t49)
      t87 = t73 * t64
      t90 = 0.1D1 / (-0.2D1 + t64)
      t91 = qbqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t96 = FJET(XB1, XB2, s, t65 * t4 * x3, -t65 * t4 * t68, -t65 * t8,
     # t65 * t11, s * t73 * t15 * t17 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.
     #2D1 * t78 * t81), t21 * t87 * t90 * t91 * t28 / 0.32D2)
      qbqbH2n1e0 = t62 * t61 + t96 * t20 * t6 * t87 * t90 * t91 * t28 / 
     #0.32D2

      end function



      doubleprecision function qbqbH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH21J1
      doubleprecision qbqbH21J2
      t1 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t13 = t1 ** 2
      t15 = t3 ** 2
      t20 = t15 * t3
      t26 = qbqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t27 = t13 * t1 / (-0.2D1 + t1) * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2 * t3 * x1, -t2 * t7 * x4, t2 * t
     #7 * (-0.1D1 + x4), -s * t13 * t15 * x1 * t6 * x4, -t20 * t6 * t27 
     #/ 0.32D2)
      qbqbH2n1em1 = -t30 * t20 * t6 * t27 / 0.32D2

      end function



      doubleprecision function qbqbH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH21J1
      doubleprecision qbqbH21J2
      qbqbH2n1em2 = 0.0D0

      end function



      doubleprecision function qbqbH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH21J1
      doubleprecision qbqbH21J2
      qbqbH2n1em3 = 0.0D0

      end function



      doubleprecision function qbqbH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH21J1
      doubleprecision qbqbH21J2
      qbqbH2n1em4 = 0.0D0

      end function
  
 

      doubleprecision function qbqbH21J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t4 * x3 - t2 * t8
      t11 = t10 ** 2
      t13 = t10 * s
      t14 = t1 ** 2
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t27 = sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      t34 = t13 * t1
      t39 = s ** 2
      qbqbH21J1 = -0.16D2 * wd * (-0.2D1 * t11 + t13 * t14 * t16 * x1 * 
     #t18 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27) - 0.2D1 * t3
     #4 * t8 + 0.2D1 * t13 * z - 0.2D1 * t39 * z * t1 * t8 + t34 * t3 * 
     #t18 * x4) / t11

      end function
  
   
 

      doubleprecision function qbqbH21J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = t10 ** 2
      t13 = t10 * s
      t14 = t1 ** 2
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t27 = sqrt(x3 * t7 * x4 * (0.1D1 - x4))
      qbqbH21J2 = -0.16D2 * wd * (0.2D1 * t11 + t13 * t14 * t16 * x1 * t
     #18 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27) - t13 * t1 * 
     #t3 * t18 * x4) / t11

      end function
  
 