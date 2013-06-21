  
      subroutine RVbbargH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision polylog  
      doubleprecision Log  
      doubleprecision RVbbargH2n1e1  
      doubleprecision RVbbargH2n1e0  
      doubleprecision RVbbargH2n1em1  
      doubleprecision RVbbargH2n1em2  
      doubleprecision RVbbargH2n1em3  
      doubleprecision RVbbargH2n1em4  
      doubleprecision RVbbargH2n2e1  
      doubleprecision RVbbargH2n2e0  
      doubleprecision RVbbargH2n2em1  
      doubleprecision RVbbargH2n2em2  
      doubleprecision RVbbargH2n2em3  
      doubleprecision RVbbargH2n2em4  
      doubleprecision RVbbargH2n3e1  
      doubleprecision RVbbargH2n3e0  
      doubleprecision RVbbargH2n3em1  
      doubleprecision RVbbargH2n3em2  
      doubleprecision RVbbargH2n3em3  
      doubleprecision RVbbargH2n3em4  
      doubleprecision RVbbargH2n4e1  
      doubleprecision RVbbargH2n4e0  
      doubleprecision RVbbargH2n4em1  
      doubleprecision RVbbargH2n4em2  
      doubleprecision RVbbargH2n4em3  
      doubleprecision RVbbargH2n4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=RVbbargH2n1e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH2n2e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH2n3e1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH2n4e1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=RVbbargH2n1e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH2n2e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH2n3e0(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH2n4e0(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=RVbbargH2n1em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH2n2em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH2n3em1(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH2n4em1(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=RVbbargH2n1em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH2n2em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH2n3em2(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH2n4em2(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=RVbbargH2n1em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH2n2em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH2n3em3(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH2n4em3(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=RVbbargH2n1em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.2)then  
         fff=RVbbargH2n2em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.3)then  
         fff=RVbbargH2n3em4(s, XB1, XB2, z, lh, wd, x1)  
      else if(sector.eq.4)then  
         fff=RVbbargH2n4em4(s, XB1, XB2, z, lh, wd, x1)  
      end if  
      end if  
      end subroutine

      doubleprecision function RVbbargH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t8 = z ** 2
      t9 = 0.1D1 - 0.2D1 * z + t8
      t14 = log(z)
      t16 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t14
      t17 = 0.3141592653589793D1 * t16
      t18 = log(x1)
      t19 = t18 * 0.3141592653589793D1
      t22 = (0.240D3 * 0.3141592653589793D1 + t17 + 0.240D3 * t19) * wd
      t23 = -0.1D1 - t8
      t27 = x1 * z
      t28 = -z - x1 + t27
      t30 = z * t3 / t28
      t31 = polylog(2, t30)
      t32 = 0.1D1 / t3
      t34 = log(t28 * t32)
      t35 = t34 ** 2
      t37 = -t3
      t38 = polylog(2, t37)
      t40 = log(-t32 * z)
      t41 = t40 ** 2
      t53 = 0.3141592653589793D1 ** 2
      t56 = t14 ** 2
      t60 = lh ** 2
      t69 = t18 ** 2
      t80 = log(t37)
      t84 = polylog(3, x1)
      t85 = polylog(3, t37)
      t89 = 0.1D1 - t30
      t90 = log(t89)
      t93 = t90 ** 2
      t94 = log(t30)
      t98 = polylog(3, t89)
      t99 = polylog(3, t30)
      t103 = -t53 * t18 / 0.6D1 + t69 * t80 / 0.2D1 + t18 * t38 + t84 + 
     #t85 + t40 * t38 + t41 * t40 / 0.6D1 + t53 * t90 / 0.6D1 - t93 * t9
     #4 / 0.2D1 - t90 * t31 - t98 - t99 - t34 * t31 - t35 * t34 / 0.6D1
      t107 = (-0.120D3 * t6 * t9 + t22 * t23) * (-t31 - t35 / 0.2D1 + t3
     #8 + t41 / 0.2D1) + (-0.120D3 * t6 * (x1 - 0.2D1 * t27 + x1 * t8) +
     # t22 * t9 + (-0.2D1 * t17 - 0.480D3 * t19 + 0.60D2 * t53 * 0.31415
     #92653589793D1 + 0.3141592653589793D1 * (-0.60D2 * t56 + 0.480D3 * 
     #lh - 0.240D3 * t14 - 0.240D3 * t60 + 0.240D3 * t14 * lh + 0.20D2 *
     # t53 - 0.480D3) - 0.2D1 * t19 * t16 - 0.240D3 * t69 * 0.3141592653
     #589793D1) * wd * t23) * (-t34 + t40) - 0.120D3 * t6 * t23 * t103
      t108 = 0.1D1 / x1
      t111 = FJET(XB1, XB2, s, -t2 * t3, t2 * x1, 0.0D0, 0.0D0, 0.0D0, -
     #t107 * t108 / 0.360D3)
      RVbbargH2n1e1 = -t111 * t107 * t108 / 0.360D3

      end function



      doubleprecision function RVbbargH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t7 = z ** 2
      t8 = -0.1D1 - t7
      t11 = -z - x1 + x1 * z
      t14 = polylog(2, z * t3 / t11)
      t15 = 0.1D1 / t3
      t17 = log(t11 * t15)
      t18 = t17 ** 2
      t21 = polylog(2, -t3)
      t23 = log(-t15 * z)
      t24 = t23 ** 2
      t36 = log(z)
      t40 = log(x1)
      t49 = -0.120D3 * t6 * t8 * (-t14 - t18 / 0.2D1 + t21 + t24 / 0.2D1
     #) + (-0.120D3 * t6 * (0.1D1 - 0.2D1 * z + t7) + (0.240D3 * 0.31415
     #92653589793D1 + 0.3141592653589793D1 * (-0.240D3 + 0.240D3 * lh - 
     #0.120D3 * t36) + 0.240D3 * t40 * 0.3141592653589793D1) * wd * t8) 
     #* (-t17 + t23)
      t50 = 0.1D1 / x1
      t53 = FJET(XB1, XB2, s, -t2 * t3, t2 * x1, 0.0D0, 0.0D0, 0.0D0, -t
     #49 * t50 / 0.360D3)
      RVbbargH2n1e0 = -t53 * t49 * t50 / 0.360D3

      end function



      doubleprecision function RVbbargH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = -0.1D1 + x1
      t7 = z ** 2
      t11 = 0.1D1 / t3
      t13 = log((-z - x1 + x1 * z) * t11)
      t15 = log(-t11 * z)
      t19 = (-0.1D1 - t7) * (-t13 + t15) / x1
      t22 = FJET(XB1, XB2, s, -t2 * t3, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 0.
     #3141592653589793D1 * wd * t19 / 0.3D1)
      RVbbargH2n1em1 = t22 * 0.3141592653589793D1 * wd * t19 / 0.3D1

      end function



      doubleprecision function RVbbargH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n1em2 = 0.0D0

      end function



      doubleprecision function RVbbargH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n1em3 = 0.0D0

      end function



      doubleprecision function RVbbargH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n1em4 = 0.0D0

      end function


      doubleprecision function RVbbargH2n2e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = log(z)
      t6 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t4
      t7 = 0.3141592653589793D1 * t6
      t8 = log(x1)
      t9 = t8 * 0.3141592653589793D1
      t11 = t7 + 0.120D3 * t9
      t12 = t11 * wd
      t14 = z ** 2
      t15 = 0.24D2 * t14
      t16 = -0.24D2 + 0.32D2 * z - t15
      t18 = 0.3141592653589793D1 * wd
      t19 = 0.16D2 * z
      t21 = 0.8D1 - t19 + 0.8D1 * t14
      t24 = t4 ** 2
      t25 = t24 * t4
      t27 = t24 * lh
      t29 = 0.3141592653589793D1 ** 2
      t32 = t4 * lh
      t34 = lh ** 2
      t35 = t4 * t34
      t37 = t34 * lh
      t40 = t29 * t4
      t43 = t29 * lh
      t46 = -0.20D2 * t25 - 0.5753417909889299D3 + 0.120D3 * t27 + 0.40D
     #2 * t29 - 0.120D3 * t24 + 0.480D3 * t32 - 0.240D3 * t35 + 0.160D3 
     #* t37 + 0.960D3 * lh + 0.20D2 * t40 - 0.480D3 * t4 - 0.40D2 * t43 
     #- 0.480D3 * t34
      t47 = 0.3141592653589793D1 * t46
      t54 = -0.60D2 * t24 + 0.480D3 * lh - 0.240D3 * t4 - 0.240D3 * t34 
     #+ 0.240D3 * t32 + 0.20D2 * t29 - 0.480D3
      t56 = t8 ** 2
      t57 = t56 * 0.3141592653589793D1
      t66 = (t47 - t9 * t54 + t57 * t6 / 0.2D1 + 0.20D2 * t56 * t8 * 0.3
     #141592653589793D1 - t11 * t29 / 0.2D1) * wd
      t68 = -0.8D1 - 0.8D1 * t14
      t70 = t29 * 0.3141592653589793D1
      t71 = 0.60D2 * t70
      t72 = 0.3141592653589793D1 * t54
      t76 = (t71 + t72 - t6 * t9 - 0.60D2 * t57) * wd
      t77 = 0.24D2 - t19 + t15
      t80 = 0.1D1 / x1
      t92 = t29 ** 2
      t101 = t24 ** 2
      t105 = t34 ** 2
      t109 = 0.1150683581977860D4 * lh + 0.80D2 * t29 - 0.960D3 * t34 - 
     #0.5753417909889299D3 * t4 - 0.240D3 * t24 - 0.40D2 * t25 - 0.5D1 *
     # t101 + 0.3D1 * t92 + 0.320D3 * t37 - 0.80D2 * t105 - 0.40D2 * t40
     # * lh
      t125 = -0.1150683581977860D4 + 0.40D2 * t29 * t34 + 0.10D2 * t29 *
     # t24 + 0.960D3 * t32 + 0.160D3 * t4 * t37 - 0.120D3 * t24 * t34 + 
     #0.40D2 * t25 * lh + 0.240D3 * t27 - 0.480D3 * t35 + 0.40D2 * t40 -
     # 0.80D2 * t43
      t136 = -(t12 * t16 - 0.120D3 * t18 * t21 + t66 * t68 + t76 * t77) 
     #* t80 / 0.360D3 + (-t7 * t21 - (t71 + t72) * t16 - (t47 - t70 * t6
     # / 0.2D1) * t77 - (-0.5D1 * t92 * 0.3141592653589793D1 + 0.3141592
     #653589793D1 * (t109 + t125) - t70 * t54 / 0.2D1) * t68) * wd / 0.3
     #60D3
      t137 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t136)
      t140 = -0.1D1 + x1
      t142 = x1 * t14
      t144 = x1 * z
      t146 = t14 * z
      t147 = t146 * x1
      t149 = x1 ** 2
      t150 = t149 * z
      t152 = t149 * t14
      t154 = t149 * t146
      t156 = t14 ** 2
      t157 = t156 * x1
      t158 = 0.24D2 * t157
      t159 = t156 * t149
      t161 = t149 * x1
      t162 = t161 * z
      t164 = t161 * t14
      t166 = t161 * t146
      t168 = t161 * t156
      t170 = t149 ** 2
      t171 = t170 * z
      t173 = t170 * t14
      t175 = t170 * t146
      t177 = t170 * t156
      t179 = 0.84D2 * t142 - 0.54D2 * t144 - 0.70D2 * t147 - 0.18D2 * t1
     #50 + 0.24D2 * t152 - 0.2D1 * t154 + t158 - 0.6D1 * t159 + 0.144D3 
     #* t162 - 0.216D3 * t164 + 0.144D3 * t166 - 0.36D2 * t168 - 0.72D2 
     #* t171 + 0.108D3 * t173 - 0.72D2 * t175 + 0.18D2 * t177
      t180 = -x1 + t144 + 0.1D1
      t181 = t140 ** 2
      t182 = 0.1D1 / t181
      t184 = log(t180 * t182)
      t185 = t184 * t149
      t188 = log(t182 * z)
      t189 = t188 * t146
      t191 = t188 * t149
      t193 = t184 * x1
      t195 = t184 * t146
      t197 = t188 * x1
      t199 = t184 * z
      t201 = t188 * z
      t203 = 0.1D1 / t140
      t205 = log(-t203 * z)
      t206 = t205 * x1
      t208 = t205 * t149
      t210 = log(-t203)
      t211 = t210 * t149
      t213 = t210 * x1
      t215 = t205 * z
      t217 = t210 * z
      t219 = t205 * t146
      t221 = t210 * t146
      t223 = 0.1D1 / t180
      t224 = t144 * t223
      t225 = polylog(2, t224)
      t226 = t184 ** 2
      t228 = t225 + t226 / 0.2D1
      t229 = t228 * x1
      t230 = 0.3D1 * t185 + 0.3D1 * t189 - 0.3D1 * t191 - 0.3D1 * t193 -
     # 0.3D1 * t195 + 0.3D1 * t197 - 0.3D1 * t199 + 0.3D1 * t201 - 0.3D1
     # * t206 + 0.3D1 * t208 + 0.12D2 * t211 - 0.21D2 * t213 - 0.3D1 * t
     #215 - 0.21D2 * t217 - 0.3D1 * t219 - 0.21D2 * t221 + t229
      t232 = t228 * t146
      t233 = polylog(2, x1)
      t234 = t188 ** 2
      t236 = t233 + t234 / 0.2D1
      t237 = t236 * x1
      t238 = t228 * t149
      t239 = t228 * z
      t241 = 0.2D1 * t184 * t14
      t242 = t236 * t146
      t243 = t236 * t149
      t244 = t236 * z
      t246 = 0.2D1 * t188 * t14
      t247 = t205 ** 2
      t248 = t247 * x1
      t250 = t247 * t149
      t252 = t210 ** 2
      t253 = t252 * t149
      t255 = t252 * x1
      t257 = t210 * t170
      t259 = t210 * t161
      t261 = t247 * z
      t263 = t232 - t237 - t238 + t239 + t241 - t242 + t243 - t244 - t24
     #6 + t248 / 0.2D1 - t250 / 0.2D1 - 0.7D1 / 0.2D1 * t253 + 0.7D1 / 0
     #.2D1 * t255 - 0.9D1 * t257 + 0.18D2 * t259 + t261 / 0.2D1
      t264 = t252 * z
      t266 = t247 * t146
      t268 = t252 * t146
      t271 = 0.2D1 * t205 * t14
      t272 = t210 * t14
      t275 = 0.24D2 * z
      t277 = t188 * t156
      t278 = t277 * x1
      t280 = t193 * z
      t282 = t193 * t14
      t284 = t195 * x1
      t286 = t184 * t156
      t287 = t286 * x1
      t289 = t197 * t14
      t291 = t197 * z
      t293 = t185 * z
      t295 = t185 * t14
      t297 = 0.7D1 / 0.2D1 * t264 + t266 / 0.2D1 + 0.7D1 / 0.2D1 * t268 
     #+ t271 + 0.14D2 * t272 - 0.32D2 * t14 + t275 + 0.16D2 * x1 + 0.3D1
     # * t278 + 0.8D1 * t280 - 0.10D2 * t282 + 0.8D1 * t284 - 0.3D1 * t2
     #87 + 0.10D2 * t289 - 0.8D1 * t291 - 0.8D1 * t293 + 0.10D2 * t295
      t300 = t185 * t146
      t302 = t286 * t149
      t304 = t189 * x1
      t306 = t191 * z
      t308 = t191 * t14
      t310 = t191 * t146
      t312 = t277 * t149
      t314 = t206 * t14
      t316 = t208 * z
      t318 = t208 * t14
      t320 = t211 * z
      t322 = t211 * t14
      t324 = t213 * z
      t326 = t213 * t14
      t328 = t206 * t146
      t330 = t206 * t156
      t332 = -0.8D1 * t300 + 0.3D1 * t302 - 0.8D1 * t304 + 0.8D1 * t306 
     #- 0.10D2 * t308 + 0.8D1 * t310 - 0.3D1 * t312 - 0.10D2 * t314 - 0.
     #8D1 * t316 + 0.10D2 * t318 - 0.11D2 * t320 - 0.2D1 * t322 + 0.47D2
     # * t324 - 0.52D2 * t326 + 0.8D1 * t328 - 0.3D1 * t330
      t333 = t208 * t146
      t335 = t208 * t156
      t337 = t213 * t146
      t339 = t213 * t156
      t341 = t211 * t146
      t343 = t211 * t156
      t345 = t206 * z
      t347 = t236 * t156
      t348 = t347 * x1
      t349 = t229 * z
      t351 = t229 * t14
      t353 = t232 * x1
      t355 = t228 * t156
      t356 = t355 * x1
      t357 = t237 * t14
      t359 = t237 * z
      t361 = t238 * z
      t363 = t238 * t14
      t365 = t238 * t146
      t367 = -0.8D1 * t333 + 0.3D1 * t335 + 0.47D2 * t337 - 0.21D2 * t33
     #9 - 0.11D2 * t341 + 0.12D2 * t343 + 0.8D1 * t345 - t348 - 0.2D1 * 
     #t349 + 0.2D1 * t351 - 0.2D1 * t353 + t356 - 0.2D1 * t357 + 0.2D1 *
     # t359 + 0.2D1 * t361 - 0.2D1 * t363 + 0.2D1 * t365
      t369 = t355 * t149
      t370 = t242 * x1
      t372 = t243 * z
      t374 = t243 * t14
      t376 = t243 * t146
      t378 = t347 * t149
      t379 = t248 * t14
      t380 = t250 * z
      t381 = t250 * t14
      t382 = t253 * z
      t384 = t253 * t14
      t386 = t255 * z
      t388 = t255 * t14
      t390 = t248 * t146
      t391 = t248 * t156
      t393 = t250 * t146
      t394 = t250 * t156
      t396 = -t369 + 0.2D1 * t370 - 0.2D1 * t372 + 0.2D1 * t374 - 0.2D1 
     #* t376 + t378 + t379 + t380 - t381 + 0.7D1 * t382 - 0.7D1 * t384 -
     # 0.7D1 * t386 + 0.7D1 * t388 - t390 + t391 / 0.2D1 + t393 - t394 /
     # 0.2D1
      t397 = t257 * z
      t399 = t257 * t14
      t401 = t257 * t146
      t403 = t257 * t156
      t405 = t255 * t146
      t407 = t255 * t156
      t409 = t253 * t146
      t411 = t253 * t156
      t413 = t259 * z
      t415 = t259 * t14
      t417 = t259 * t146
      t419 = t259 * t156
      t421 = t248 * z
      t425 = 0.24D2 * t146
      t426 = 0.36D2 * t397 - 0.54D2 * t399 + 0.36D2 * t401 - 0.9D1 * t40
     #3 - 0.7D1 * t405 + 0.7D1 / 0.2D1 * t407 + 0.7D1 * t409 - 0.7D1 / 0
     #.2D1 * t411 - 0.72D2 * t413 + 0.108D3 * t415 - 0.72D2 * t417 + 0.1
     #8D2 * t419 - t421 + 0.2D1 * t149 + 0.18D2 * t170 - 0.36D2 * t161 +
     # t425
      t432 = 0.1D1 / (-z - x1 + t144)
      t436 = 0.16D2 * t144
      t438 = 0.16D2 * t150
      t443 = -0.48D2 * t142 + t436 + 0.48D2 * t147 - t438 + 0.48D2 * t15
     #2 - 0.48D2 * t154 - 0.16D2 * t157 + 0.16D2 * t159 - t189 + t193 + 
     #t195
      t448 = -t197 + t199 - t201 + t206 - t208 + 0.3D1 * t211 + 0.15D2 *
     # t213 + t215 + 0.23D2 * t217 + t219 + 0.23D2 * t221
      t459 = -0.3D1 * t229 - 0.3D1 * t232 + 0.3D1 * t237 + 0.3D1 * t238 
     #- 0.3D1 * t239 - t241 + 0.3D1 * t242 - 0.3D1 * t243 + 0.3D1 * t244
     # + t246 - 0.3D1 / 0.2D1 * t248
      t470 = 0.16D2 * t14
      t471 = 0.3D1 / 0.2D1 * t250 + 0.6D1 * t253 - 0.21D2 / 0.2D1 * t255
     # + 0.18D2 * t257 - 0.36D2 * t259 - 0.3D1 / 0.2D1 * t261 - 0.21D2 /
     # 0.2D1 * t264 - 0.3D1 / 0.2D1 * t266 - 0.21D2 / 0.2D1 * t268 - t27
     #1 - 0.30D2 * t272 + t470
      t474 = 0.8D1 * z
      t483 = -t474 - t278 - 0.3D1 * t280 + 0.4D1 * t282 - 0.3D1 * t284 +
     # t287 - 0.4D1 * t289 + 0.3D1 * t291 + 0.3D1 * t304 + 0.6D1 * t314 
     #+ 0.4D1 * t316
      t494 = -0.6D1 * t318 - 0.22D2 * t320 + 0.30D2 * t322 - 0.50D2 * t3
     #24 + 0.78D2 * t326 - 0.4D1 * t328 + t330 + 0.4D1 * t333 - t335 - 0
     #.66D2 * t337 + 0.23D2 * t339 - 0.6D1 * t341
      t507 = -0.5D1 * t343 - 0.4D1 * t345 + 0.3D1 * t348 + 0.8D1 * t349 
     #- 0.10D2 * t351 + 0.8D1 * t353 - 0.3D1 * t356 + 0.10D2 * t357 - 0.
     #8D1 * t359 - 0.8D1 * t361 + 0.10D2 * t363
      t519 = -0.8D1 * t365 + 0.3D1 * t369 - 0.8D1 * t370 + 0.8D1 * t372 
     #- 0.10D2 * t374 + 0.8D1 * t376 - 0.3D1 * t378 - 0.5D1 * t379 - 0.4
     #D1 * t380 + 0.5D1 * t381 - 0.11D2 / 0.2D1 * t382 - t384
      t534 = 0.47D2 / 0.2D1 * t386 - 0.26D2 * t388 + 0.4D1 * t390 - 0.3D
     #1 / 0.2D1 * t391 - 0.4D1 * t393 + 0.3D1 / 0.2D1 * t394 - 0.72D2 * 
     #t397 + 0.108D3 * t399 - 0.72D2 * t401 + 0.18D2 * t403 + 0.47D2 / 0
     #.2D1 * t405
      t543 = t188 * t161
      t545 = t184 * t161
      t552 = -0.21D2 / 0.2D1 * t407 - 0.11D2 / 0.2D1 * t409 + 0.6D1 * t4
     #11 + 0.144D3 * t413 - 0.216D3 * t415 + 0.144D3 * t417 - 0.36D2 * t
     #419 + 0.4D1 * t421 + t543 * t156 + 0.4D1 * t545 * z - 0.6D1 * t545
     # * t14 + 0.4D1 * t545 * t146
      t561 = t247 * t205
      t562 = t561 * x1
      t565 = t561 * t149
      t570 = t252 * t210
      t571 = t570 * t149
      t576 = t570 * x1
      t581 = -t545 * t156 - 0.4D1 * t543 * z + 0.6D1 * t543 * t14 - 0.4D
     #1 * t543 * t146 + t562 * t14 / 0.3D1 + t565 * z / 0.3D1 - t565 * t
     #14 / 0.3D1 + 0.7D1 / 0.3D1 * t571 * z - 0.7D1 / 0.3D1 * t571 * t14
     # - 0.7D1 / 0.3D1 * t576 * z + 0.7D1 / 0.3D1 * t576 * t14
      t590 = t252 * t170
      t607 = -t562 * t146 / 0.3D1 + t562 * t156 / 0.6D1 + t565 * t146 / 
     #0.3D1 - t565 * t156 / 0.6D1 + 0.18D2 * t590 * z - 0.27D2 * t590 * 
     #t14 + 0.18D2 * t590 * t146 - 0.9D1 / 0.2D1 * t590 * t156 - 0.7D1 /
     # 0.3D1 * t576 * t146 + 0.7D1 / 0.6D1 * t576 * t156 + 0.7D1 / 0.3D1
     # * t571 * t146 - 0.7D1 / 0.6D1 * t571 * t156
      t610 = t252 * t161
      t621 = -t140
      t622 = log(t621)
      t625 = t622 ** 2
      t629 = polylog(3, t621)
      t630 = polylog(3, x1)
      t634 = -t29 * t622 / 0.6D1 + t625 * t8 / 0.2D1 + t622 * t233 + t62
     #9 + t630 - 0.1202056903159594D1 + t188 * t233 + t234 * t188 / 0.6D
     #1
      t635 = t634 * t156
      t637 = 0.1D1 - t224
      t638 = log(t637)
      t641 = t638 ** 2
      t642 = log(t224)
      t646 = polylog(3, t637)
      t647 = polylog(3, t224)
      t651 = -t29 * t638 / 0.6D1 + t641 * t642 / 0.2D1 + t638 * t225 + t
     #646 + t647 - 0.1202056903159594D1 + t184 * t225 + t226 * t184 / 0.
     #6D1
      t652 = t651 * x1
      t657 = t651 * t146
      t660 = t651 * t156
      t662 = t634 * x1
      t665 = -0.36D2 * t610 * z + 0.54D2 * t610 * t14 - 0.36D2 * t610 * 
     #t146 + 0.9D1 * t610 * t156 - t562 * z / 0.3D1 - t635 * x1 - 0.2D1 
     #* t652 * z + 0.2D1 * t652 * t14 - 0.2D1 * t657 * x1 + t660 * x1 - 
     #0.2D1 * t662 * t14
      t668 = t651 * t149
      t677 = t634 * t146
      t680 = t634 * t149
      t689 = 0.8D1 * t146
      t690 = 0.2D1 * t662 * z + 0.2D1 * t668 * z - 0.2D1 * t651 * t14 * 
     #t149 + 0.2D1 * t657 * t149 - t660 * t149 + 0.2D1 * t677 * x1 - 0.2
     #D1 * t680 * z + 0.2D1 * t634 * t14 * t149 - 0.2D1 * t677 * t149 + 
     #t635 * t149 - t689 + t543
      t706 = -t545 + 0.2D1 * t228 * t14 - 0.2D1 * t236 * t14 + t562 / 0.
     #6D1 - t565 / 0.6D1 - 0.7D1 / 0.6D1 * t571 + 0.7D1 / 0.6D1 * t576 -
     # 0.9D1 / 0.2D1 * t590 + 0.9D1 * t610 + t561 * z / 0.6D1 + 0.7D1 / 
     #0.6D1 * t570 * z
      t716 = t561 * t146 / 0.6D1 + 0.7D1 / 0.6D1 * t570 * t146 + t247 * 
     #t14 + 0.7D1 * t252 * t14 + t652 + t657 - t662 + t651 * z - t668 - 
     #t677 - t634 * z + t680
      t733 = t474 + t689 - t436 + 0.8D1 * x1 + 0.8D1 * t157 + 0.16D2 * t
     #142 - 0.16D2 * t147 + t438 - 0.16D2 * t152 + 0.16D2 * t154 - 0.8D1
     # * t159 - 0.8D1 * t149
      t752 = -0.62D2 * t142 + 0.55D2 * t144 + 0.55D2 * t147 - 0.19D2 * t
     #150 + 0.8D1 * t152 - 0.19D2 * t154 - t158 + 0.15D2 * t159 - 0.72D2
     # * t162 + 0.108D3 * t164 - 0.72D2 * t166 + 0.18D2 * t168 + 0.36D2 
     #* t171 - 0.54D2 * t173 + 0.36D2 * t175 - 0.9D1 * t177 - t185
      t758 = -t189 + t191 + t193 + t195 - t197 + t199 - t201 + t206 - t2
     #08 - 0.7D1 * t211 + 0.7D1 * t213 + t215 + 0.7D1 * t217 + t219 + 0.
     #7D1 * t221 + t470 - t275 - 0.24D2 * x1
      t774 = -t278 - 0.2D1 * t280 + 0.2D1 * t282 - 0.2D1 * t284 + t287 -
     # 0.2D1 * t289 + 0.2D1 * t291 + 0.2D1 * t293 - 0.2D1 * t295 + 0.2D1
     # * t300 - t302 + 0.2D1 * t304 - 0.2D1 * t306 + 0.2D1 * t308 - 0.2D
     #1 * t310 + t312 + 0.2D1 * t314 + 0.2D1 * t316
      t790 = -0.2D1 * t318 + 0.14D2 * t320 - 0.14D2 * t322 - 0.14D2 * t3
     #24 + 0.14D2 * t326 - 0.2D1 * t328 + t330 + 0.2D1 * t333 - t335 - 0
     #.14D2 * t337 + 0.7D1 * t339 + 0.14D2 * t341 - 0.7D1 * t343 - 0.2D1
     # * t345 + 0.15D2 * t149 - 0.9D1 * t170 + 0.18D2 * t161 - t425
      t796 = -t12 * (t179 + t230 + t263 + t297 + t332 + t367 + t396 + t4
     #26) * t223 * t432 + 0.120D3 * t18 * (t443 + t448 + t459 + t471 + t
     #483 + t494 + t507 + t519 + t534 + t552 + t581 + t607 + t665 + t690
     # + t706 + t716) * t223 * t432 - t66 * t733 * t223 * t432 - t76 * (
     #t752 + t758 + t774 + t790) * t223 * t432
      t799 = FJET(XB1, XB2, s, t2 * x1, -t2 * t140, 0.0D0, 0.0D0, 0.0D0,
     # -t796 * t80 / 0.360D3)
      RVbbargH2n2e1 = t137 * t136 - t799 * t796 * t80 / 0.360D3

      end function



      doubleprecision function RVbbargH2n2e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 * wd
      t5 = z ** 2
      t6 = 0.24D2 * t5
      t7 = -0.24D2 + 0.32D2 * z - t6
      t11 = log(z)
      t13 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t11
      t14 = 0.3141592653589793D1 * t13
      t15 = log(x1)
      t16 = t15 * 0.3141592653589793D1
      t19 = (t14 + 0.120D3 * t16) * wd
      t20 = 0.16D2 * z
      t21 = 0.24D2 - t20 + t6
      t23 = 0.3141592653589793D1 ** 2
      t24 = t23 * 0.3141592653589793D1
      t25 = 0.60D2 * t24
      t26 = t11 ** 2
      t30 = lh ** 2
      t32 = t11 * lh
      t36 = 0.3141592653589793D1 * (-0.60D2 * t26 + 0.480D3 * lh - 0.240
     #D3 * t11 - 0.240D3 * t30 + 0.240D3 * t32 + 0.20D2 * t23 - 0.480D3)
      t38 = t15 ** 2
      t42 = (t25 + t36 - t16 * t13 - 0.60D2 * t38 * 0.3141592653589793D1
     #) * wd
      t44 = -0.8D1 - 0.8D1 * t5
      t47 = 0.1D1 / x1
      t76 = -0.20D2 * t26 * t11 - 0.5753417909889299D3 + 0.120D3 * t26 *
     # lh + 0.40D2 * t23 - 0.120D3 * t26 + 0.480D3 * t32 - 0.240D3 * t11
     # * t30 + 0.160D3 * t30 * lh + 0.960D3 * lh + 0.20D2 * t23 * t11 - 
     #0.480D3 * t11 - 0.40D2 * t23 * lh - 0.480D3 * t30
      t86 = -(-0.120D3 * t3 * t7 + t19 * t21 + t42 * t44) * t47 / 0.360D
     #3 + (-0.120D3 * 0.3141592653589793D1 * (-0.8D1 + t20 - 0.8D1 * t5)
     # - t14 * t7 - (t25 + t36) * t21 - (0.3141592653589793D1 * t76 - t2
     #4 * t13 / 0.2D1) * t44) * wd / 0.360D3
      t87 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t86)
      t90 = -0.1D1 + x1
      t92 = x1 ** 2
      t94 = t92 ** 2
      t96 = t92 * x1
      t98 = t5 * z
      t99 = 0.24D2 * t98
      t100 = 0.24D2 * z
      t102 = t90 ** 2
      t103 = 0.1D1 / t102
      t105 = log(t103 * z)
      t106 = t5 ** 2
      t107 = t105 * t106
      t108 = t107 * x1
      t110 = x1 * z
      t111 = -x1 + t110 + 0.1D1
      t113 = log(t111 * t103)
      t114 = t113 * x1
      t115 = t114 * z
      t117 = t114 * t5
      t119 = t113 * t98
      t120 = t119 * x1
      t122 = t113 * t106
      t123 = t122 * x1
      t125 = t105 * x1
      t126 = t125 * t5
      t128 = t125 * z
      t130 = t113 * t92
      t131 = t130 * z
      t133 = t130 * t5
      t135 = t130 * t98
      t137 = 0.2D1 * t92 + 0.18D2 * t94 - 0.36D2 * t96 + t99 + t100 - 0.
     #32D2 * t5 + 0.3D1 * t108 + 0.8D1 * t115 - 0.10D2 * t117 + 0.8D1 * 
     #t120 - 0.3D1 * t123 + 0.10D2 * t126 - 0.8D1 * t128 - 0.8D1 * t131 
     #+ 0.10D2 * t133 - 0.8D1 * t135
      t138 = t122 * t92
      t140 = t105 * t98
      t141 = t140 * x1
      t143 = t105 * t92
      t144 = t143 * z
      t146 = t143 * t5
      t148 = t143 * t98
      t150 = t107 * t92
      t152 = 0.1D1 / t90
      t154 = log(-t152 * z)
      t155 = t154 * x1
      t156 = t155 * t5
      t158 = t154 * t92
      t159 = t158 * z
      t161 = t158 * t5
      t163 = log(-t152)
      t164 = t163 * t92
      t165 = t164 * z
      t167 = t164 * t5
      t169 = t163 * x1
      t170 = t169 * z
      t172 = t169 * t5
      t174 = t155 * t98
      t176 = t155 * t106
      t178 = t158 * t98
      t180 = t158 * t106
      t182 = 0.3D1 * t138 - 0.8D1 * t141 + 0.8D1 * t144 - 0.10D2 * t146 
     #+ 0.8D1 * t148 - 0.3D1 * t150 - 0.10D2 * t156 - 0.8D1 * t159 + 0.1
     #0D2 * t161 - 0.11D2 * t165 - 0.2D1 * t167 + 0.47D2 * t170 - 0.52D2
     # * t172 + 0.8D1 * t174 - 0.3D1 * t176 - 0.8D1 * t178 + 0.3D1 * t18
     #0
      t184 = t169 * t98
      t186 = t169 * t106
      t188 = t164 * t98
      t190 = t164 * t106
      t192 = t155 * z
      t194 = polylog(2, x1)
      t195 = t105 ** 2
      t197 = t194 + t195 / 0.2D1
      t198 = t197 * t106
      t200 = 0.1D1 / t111
      t202 = polylog(2, t110 * t200)
      t203 = t113 ** 2
      t205 = t202 + t203 / 0.2D1
      t206 = t205 * x1
      t211 = t205 * t98
      t214 = t205 * t106
      t216 = t197 * x1
      t221 = t205 * t92
      t229 = 0.47D2 * t184 - 0.21D2 * t186 - 0.11D2 * t188 + 0.12D2 * t1
     #90 + 0.8D1 * t192 - t198 * x1 - 0.2D1 * t206 * z + 0.2D1 * t206 * 
     #t5 - 0.2D1 * t211 * x1 + t214 * x1 - 0.2D1 * t216 * t5 + 0.2D1 * t
     #216 * z + 0.2D1 * t221 * z - 0.2D1 * t221 * t5 + 0.2D1 * t221 * t9
     #8 - t214 * t92
      t230 = t197 * t98
      t233 = t197 * t92
      t241 = t154 ** 2
      t242 = t241 * x1
      t244 = t241 * t92
      t247 = t163 ** 2
      t248 = t247 * t92
      t253 = t247 * x1
      t264 = t163 * t94
      t267 = 0.2D1 * t230 * x1 - 0.2D1 * t233 * z + 0.2D1 * t233 * t5 - 
     #0.2D1 * t233 * t98 + t198 * t92 + t242 * t5 + t244 * z - t244 * t5
     # + 0.7D1 * t248 * z - 0.7D1 * t248 * t5 - 0.7D1 * t253 * z + 0.7D1
     # * t253 * t5 - t242 * t98 + t242 * t106 / 0.2D1 + t244 * t98 - t24
     #4 * t106 / 0.2D1 + 0.36D2 * t264 * z
      t284 = t163 * t96
      t294 = x1 * t5
      t297 = t98 * x1
      t299 = t92 * z
      t301 = -0.54D2 * t264 * t5 + 0.36D2 * t264 * t98 - 0.9D1 * t264 * 
     #t106 - 0.7D1 * t253 * t98 + 0.7D1 / 0.2D1 * t253 * t106 + 0.7D1 * 
     #t248 * t98 - 0.7D1 / 0.2D1 * t248 * t106 - 0.72D2 * t284 * z + 0.1
     #08D3 * t284 * t5 - 0.72D2 * t284 * t98 + 0.18D2 * t284 * t106 - t2
     #42 * z + 0.84D2 * t294 - 0.54D2 * t110 - 0.70D2 * t297 - 0.18D2 * 
     #t299
      t302 = t92 * t5
      t304 = t92 * t98
      t306 = t106 * x1
      t307 = 0.24D2 * t306
      t308 = t106 * t92
      t310 = t96 * z
      t312 = t96 * t5
      t314 = t96 * t98
      t316 = t96 * t106
      t318 = t94 * z
      t320 = t94 * t5
      t322 = t94 * t98
      t324 = t94 * t106
      t331 = 0.24D2 * t302 - 0.2D1 * t304 + t307 - 0.6D1 * t308 + 0.144D
     #3 * t310 - 0.216D3 * t312 + 0.144D3 * t314 - 0.36D2 * t316 - 0.72D
     #2 * t318 + 0.108D3 * t320 - 0.72D2 * t322 + 0.18D2 * t324 + 0.3D1 
     #* t130 + 0.3D1 * t140 - 0.3D1 * t143 - 0.3D1 * t114 - 0.3D1 * t119
      t334 = t113 * z
      t336 = t105 * z
      t342 = t154 * z
      t344 = t163 * z
      t346 = t154 * t98
      t348 = t163 * t98
      t353 = 0.3D1 * t125 - 0.3D1 * t334 + 0.3D1 * t336 - 0.3D1 * t155 +
     # 0.3D1 * t158 + 0.12D2 * t164 - 0.21D2 * t169 - 0.3D1 * t342 - 0.2
     #1D2 * t344 - 0.3D1 * t346 - 0.21D2 * t348 + t206 + t211 - t216 - t
     #221 + t205 * z + 0.2D1 * t113 * t5
      t376 = -t230 + t233 - t197 * z - 0.2D1 * t105 * t5 + t242 / 0.2D1 
     #- t244 / 0.2D1 - 0.7D1 / 0.2D1 * t248 + 0.7D1 / 0.2D1 * t253 - 0.9
     #D1 * t264 + 0.18D2 * t284 + t241 * z / 0.2D1 + 0.7D1 / 0.2D1 * t24
     #7 * z + t241 * t98 / 0.2D1 + 0.7D1 / 0.2D1 * t247 * t98 + 0.2D1 * 
     #t154 * t5 + 0.14D2 * t163 * t5 + 0.16D2 * x1
      t382 = 0.1D1 / (-z - x1 + t110)
      t398 = 0.15D2 * t92 - 0.9D1 * t94 + 0.18D2 * t96 - t99 - t100 + 0.
     #16D2 * t5 - t108 - 0.2D1 * t115 + 0.2D1 * t117 - 0.2D1 * t120 + t1
     #23 - 0.2D1 * t126 + 0.2D1 * t128 + 0.2D1 * t131 - 0.2D1 * t133 + 0
     #.2D1 * t135 - t138
      t414 = 0.2D1 * t141 - 0.2D1 * t144 + 0.2D1 * t146 - 0.2D1 * t148 +
     # t150 + 0.2D1 * t156 + 0.2D1 * t159 - 0.2D1 * t161 + 0.14D2 * t165
     # - 0.14D2 * t167 - 0.14D2 * t170 + 0.14D2 * t172 - 0.2D1 * t174 + 
     #t176 + 0.2D1 * t178 - t180 - 0.14D2 * t184 + 0.7D1 * t186
      t433 = 0.14D2 * t188 - 0.7D1 * t190 - 0.2D1 * t192 - 0.62D2 * t294
     # + 0.55D2 * t110 + 0.55D2 * t297 - 0.19D2 * t299 + 0.8D1 * t302 - 
     #0.19D2 * t304 - t307 + 0.15D2 * t308 - 0.72D2 * t310 + 0.108D3 * t
     #312 - 0.72D2 * t314 + 0.18D2 * t316 + 0.36D2 * t318 - 0.54D2 * t32
     #0 + 0.36D2 * t322
      t440 = -0.9D1 * t324 - t130 - t140 + t143 + t114 + t119 - t125 + t
     #334 - t336 + t155 - t158 - 0.7D1 * t164 + 0.7D1 * t169 + t342 + 0.
     #7D1 * t344 + t346 + 0.7D1 * t348 - 0.24D2 * x1
      t458 = 0.8D1 * z + 0.8D1 * t98 - 0.16D2 * t110 + 0.8D1 * x1 + 0.8D
     #1 * t306 + 0.16D2 * t294 - 0.16D2 * t297 + 0.16D2 * t299 - 0.16D2 
     #* t302 + 0.16D2 * t304 - 0.8D1 * t308 - 0.8D1 * t92
      t462 = 0.120D3 * t3 * (t137 + t182 + t229 + t267 + t301 + t331 + t
     #353 + t376) * t200 * t382 - t19 * (t398 + t414 + t433 + t440) * t2
     #00 * t382 - t42 * t458 * t200 * t382
      t465 = FJET(XB1, XB2, s, t2 * x1, -t2 * t90, 0.0D0, 0.0D0, 0.0D0, 
     #-t462 * t47 / 0.360D3)
      RVbbargH2n2e0 = t87 * t86 - t465 * t462 * t47 / 0.360D3

      end function



      doubleprecision function RVbbargH2n2em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 * wd
      t5 = z ** 2
      t6 = 0.24D2 * t5
      t7 = 0.24D2 - 0.16D2 * z + t6
      t11 = log(z)
      t14 = 0.3141592653589793D1 * (-0.240D3 + 0.240D3 * lh - 0.120D3 * 
     #t11)
      t15 = log(x1)
      t19 = (t14 + 0.120D3 * t15 * 0.3141592653589793D1) * wd
      t21 = -0.8D1 - 0.8D1 * t5
      t24 = 0.1D1 / x1
      t32 = 0.3141592653589793D1 ** 2
      t35 = t11 ** 2
      t39 = lh ** 2
      t52 = -(-0.120D3 * t3 * t7 + t19 * t21) * t24 / 0.360D3 + (-0.120D
     #3 * 0.3141592653589793D1 * (0.24D2 - 0.32D2 * z + t6) - t14 * t7 -
     # (0.60D2 * t32 * 0.3141592653589793D1 + 0.3141592653589793D1 * (-0
     #.60D2 * t35 + 0.480D3 * lh - 0.240D3 * t11 - 0.240D3 * t39 + 0.240
     #D3 * t11 * lh + 0.20D2 * t32 - 0.480D3)) * t21) * wd / 0.360D3
      t53 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t52)
      t56 = -0.1D1 + x1
      t58 = x1 * t5
      t60 = x1 * z
      t62 = t5 * z
      t63 = t62 * x1
      t65 = x1 ** 2
      t66 = t65 * z
      t68 = t65 * t5
      t70 = t65 * t62
      t72 = t5 ** 2
      t73 = t72 * x1
      t75 = t72 * t65
      t77 = t65 * x1
      t86 = t65 ** 2
      t95 = -x1 + t60 + 0.1D1
      t96 = t56 ** 2
      t97 = 0.1D1 / t96
      t99 = log(t95 * t97)
      t100 = t99 * t65
      t101 = -0.62D2 * t58 + 0.55D2 * t60 + 0.55D2 * t63 - 0.19D2 * t66 
     #+ 0.8D1 * t68 - 0.19D2 * t70 - 0.24D2 * t73 + 0.15D2 * t75 - 0.72D
     #2 * t77 * z + 0.108D3 * t77 * t5 - 0.72D2 * t77 * t62 + 0.18D2 * t
     #77 * t72 + 0.36D2 * t86 * z - 0.54D2 * t86 * t5 + 0.36D2 * t86 * t
     #62 - 0.9D1 * t86 * t72 - t100
      t103 = log(t97 * z)
      t104 = t103 * t62
      t105 = t103 * t65
      t106 = t99 * x1
      t107 = t99 * t62
      t108 = t103 * x1
      t111 = 0.1D1 / t56
      t113 = log(-t111 * z)
      t114 = t113 * x1
      t115 = t113 * t65
      t116 = log(-t111)
      t117 = t116 * t65
      t119 = t116 * x1
      t127 = t103 * t72
      t133 = -t104 + t105 + t106 + t107 - t108 + t99 * z - t103 * z + t1
     #14 - t115 - 0.7D1 * t117 + 0.7D1 * t119 + t113 * z + 0.7D1 * t116 
     #* z + t113 * t62 + 0.7D1 * t116 * t62 - t127 * x1 - 0.2D1 * t106 *
     # z + 0.2D1 * t106 * t5
      t137 = t99 * t72
      t169 = -0.2D1 * t107 * x1 + t137 * x1 - 0.2D1 * t108 * t5 + 0.2D1 
     #* t108 * z + 0.2D1 * t100 * z - 0.2D1 * t100 * t5 + 0.2D1 * t100 *
     # t62 - t137 * t65 + 0.2D1 * t104 * x1 - 0.2D1 * t105 * z + 0.2D1 *
     # t105 * t5 - 0.2D1 * t105 * t62 + t127 * t65 + 0.2D1 * t114 * t5 +
     # 0.2D1 * t115 * z - 0.2D1 * t115 * t5 + 0.14D2 * t117 * z - 0.14D2
     # * t117 * t5
      t197 = -0.14D2 * t119 * z + 0.14D2 * t119 * t5 - 0.2D1 * t114 * t6
     #2 + t114 * t72 + 0.2D1 * t115 * t62 - t115 * t72 - 0.14D2 * t119 *
     # t62 + 0.7D1 * t119 * t72 + 0.14D2 * t117 * t62 - 0.7D1 * t117 * t
     #72 - 0.2D1 * t114 * z - 0.24D2 * z - 0.24D2 * x1 - 0.24D2 * t62 + 
     #0.16D2 * t5 + 0.15D2 * t65 - 0.9D1 * t86 + 0.18D2 * t77
      t200 = 0.1D1 / t95
      t203 = 0.1D1 / (-z - x1 + t60)
      t219 = 0.8D1 * z + 0.8D1 * t62 - 0.16D2 * t60 + 0.8D1 * x1 + 0.8D1
     # * t73 + 0.16D2 * t58 - 0.16D2 * t63 + 0.16D2 * t66 - 0.16D2 * t68
     # + 0.16D2 * t70 - 0.8D1 * t75 - 0.8D1 * t65
      t223 = 0.120D3 * t3 * (t101 + t133 + t169 + t197) * t200 * t203 - 
     #t19 * t219 * t200 * t203
      t226 = FJET(XB1, XB2, s, t2 * x1, -t2 * t56, 0.0D0, 0.0D0, 0.0D0, 
     #-t223 * t24 / 0.360D3)
      RVbbargH2n2em1 = t53 * t52 - t226 * t223 * t24 / 0.360D3

      end function



      doubleprecision function RVbbargH2n2em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 * wd
      t4 = z ** 2
      t6 = -0.8D1 - 0.8D1 * t4
      t7 = 0.1D1 / x1
      t17 = log(z)
      t26 = t3 * t6 * t7 / 0.3D1 + (-0.120D3 * 0.3141592653589793D1 * (-
     #0.24D2 + 0.16D2 * z - 0.24D2 * t4) - 0.3141592653589793D1 * (-0.24
     #0D3 + 0.240D3 * lh - 0.120D3 * t17) * t6) * wd / 0.360D3
      t27 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t26)
      t33 = t4 * z
      t35 = x1 * z
      t38 = t4 ** 2
      t45 = x1 ** 2
      t55 = 0.8D1 * z + 0.8D1 * t33 - 0.16D2 * t35 + 0.8D1 * x1 + 0.8D1 
     #* t38 * x1 + 0.16D2 * x1 * t4 - 0.16D2 * t33 * x1 + 0.16D2 * t45 *
     # z - 0.16D2 * t45 * t4 + 0.16D2 * t45 * t33 - 0.8D1 * t38 * t45 - 
     #0.8D1 * t45
      t58 = 0.1D1 / (-x1 + t35 + 0.1D1)
      t60 = 0.1D1 / (-z - x1 + t35)
      t65 = FJET(XB1, XB2, s, t2 * x1, -t2 * (-0.1D1 + x1), 0.0D0, 0.0D0
     #, 0.0D0, -t3 * t55 * t58 * t60 * t7 / 0.3D1)
      RVbbargH2n2em2 = t27 * t26 - t65 * 0.3141592653589793D1 * wd * t55
     # * t58 * t60 * t7 / 0.3D1

      end function



      doubleprecision function RVbbargH2n2em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = z ** 2
      t5 = 0.8D1 + 0.8D1 * t3
      t9 = FJET(XB1, XB2, s, 0.0D0, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D
     #0, -0.3141592653589793D1 * t5 * wd / 0.3D1)
      RVbbargH2n2em3 = -t9 * 0.3141592653589793D1 * t5 * wd / 0.3D1

      end function



      doubleprecision function RVbbargH2n2em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n2em4 = 0.0D0

      end function


      doubleprecision function RVbbargH2n3e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = log(z)
      t6 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t4
      t7 = 0.3141592653589793D1 * t6
      t8 = log(x1)
      t9 = t8 * 0.3141592653589793D1
      t11 = t7 + 0.120D3 * t9
      t12 = t11 * wd
      t14 = z ** 2
      t15 = 0.24D2 * t14
      t16 = 0.24D2 - 0.32D2 * z + t15
      t18 = 0.3141592653589793D1 * wd
      t19 = 0.16D2 * z
      t21 = -0.8D1 + t19 - 0.8D1 * t14
      t24 = t4 ** 2
      t25 = t24 * t4
      t27 = t24 * lh
      t29 = 0.3141592653589793D1 ** 2
      t32 = t4 * lh
      t34 = lh ** 2
      t35 = t4 * t34
      t37 = t34 * lh
      t40 = t29 * t4
      t43 = t29 * lh
      t46 = -0.20D2 * t25 - 0.5753417909889299D3 + 0.120D3 * t27 + 0.40D
     #2 * t29 - 0.120D3 * t24 + 0.480D3 * t32 - 0.240D3 * t35 + 0.160D3 
     #* t37 + 0.960D3 * lh + 0.20D2 * t40 - 0.480D3 * t4 - 0.40D2 * t43 
     #- 0.480D3 * t34
      t47 = 0.3141592653589793D1 * t46
      t54 = -0.60D2 * t24 + 0.480D3 * lh - 0.240D3 * t4 - 0.240D3 * t34 
     #+ 0.240D3 * t32 + 0.20D2 * t29 - 0.480D3
      t56 = t8 ** 2
      t57 = t56 * 0.3141592653589793D1
      t66 = (t47 - t9 * t54 + t57 * t6 / 0.2D1 + 0.20D2 * t56 * t8 * 0.3
     #141592653589793D1 - t11 * t29 / 0.2D1) * wd
      t68 = 0.8D1 + 0.8D1 * t14
      t70 = t29 * 0.3141592653589793D1
      t71 = 0.60D2 * t70
      t72 = 0.3141592653589793D1 * t54
      t76 = (t71 + t72 - t6 * t9 - 0.60D2 * t57) * wd
      t77 = -0.24D2 + t19 - t15
      t80 = 0.1D1 / x1
      t89 = t29 ** 2
      t98 = t24 ** 2
      t102 = t34 ** 2
      t106 = 0.1150683581977860D4 * lh + 0.80D2 * t29 - 0.960D3 * t34 - 
     #0.5753417909889299D3 * t4 - 0.240D3 * t24 - 0.40D2 * t25 - 0.5D1 *
     # t98 + 0.3D1 * t89 + 0.320D3 * t37 - 0.80D2 * t102 - 0.40D2 * t40 
     #* lh
      t122 = -0.1150683581977860D4 + 0.40D2 * t29 * t34 + 0.10D2 * t29 *
     # t24 + 0.960D3 * t32 + 0.160D3 * t4 * t37 - 0.120D3 * t24 * t34 + 
     #0.40D2 * t25 * lh + 0.240D3 * t27 - 0.480D3 * t35 + 0.40D2 * t40 -
     # 0.80D2 * t43
      t132 = (t12 * t16 - 0.120D3 * t18 * t21 + t66 * t68 + t76 * t77) *
     # t80 / 0.360D3 + (t7 * t21 + (t71 + t72) * t16 + (t47 - t70 * t6 /
     # 0.2D1) * t77 + (-0.5D1 * t89 * 0.3141592653589793D1 + 0.314159265
     #3589793D1 * (t106 + t122) - t70 * t54 / 0.2D1) * t68) * wd / 0.360
     #D3
      t133 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t132)
      t135 = -0.1D1 + x1
      t138 = x1 * t14
      t140 = x1 * z
      t142 = t14 * z
      t143 = t142 * x1
      t145 = x1 ** 2
      t146 = t145 * z
      t148 = t145 * t14
      t150 = t145 * t142
      t152 = t14 ** 2
      t153 = t152 * x1
      t154 = 0.24D2 * t153
      t155 = t152 * t145
      t157 = t145 * x1
      t158 = t157 * z
      t160 = t157 * t14
      t162 = t157 * t142
      t164 = t157 * t152
      t166 = t145 ** 2
      t167 = t166 * z
      t169 = t166 * t14
      t171 = t166 * t142
      t173 = t166 * t152
      t175 = 0.84D2 * t138 - 0.54D2 * t140 - 0.70D2 * t143 - 0.18D2 * t1
     #46 + 0.24D2 * t148 - 0.2D1 * t150 + t154 - 0.6D1 * t155 + 0.144D3 
     #* t158 - 0.216D3 * t160 + 0.144D3 * t162 - 0.36D2 * t164 - 0.72D2 
     #* t167 + 0.108D3 * t169 - 0.72D2 * t171 + 0.18D2 * t173
      t179 = 0.24D2 * t142
      t180 = t135 ** 2
      t181 = 0.1D1 / t180
      t183 = log(t181 * z)
      t184 = t183 * t152
      t185 = t184 * x1
      t187 = -x1 + t140 + 0.1D1
      t189 = log(t187 * t181)
      t190 = t189 * x1
      t191 = t190 * z
      t193 = t190 * t14
      t195 = t189 * t142
      t196 = t195 * x1
      t198 = t189 * t152
      t199 = t198 * x1
      t201 = t183 * x1
      t202 = t201 * t14
      t204 = t201 * z
      t206 = t189 * t145
      t207 = t206 * z
      t209 = t206 * t14
      t211 = t206 * t142
      t213 = t198 * t145
      t215 = t183 * t142
      t216 = t215 * x1
      t218 = t183 * t145
      t219 = t218 * z
      t221 = 0.2D1 * t145 + 0.18D2 * t166 - 0.36D2 * t157 + t179 + 0.3D1
     # * t185 + 0.8D1 * t191 - 0.10D2 * t193 + 0.8D1 * t196 - 0.3D1 * t1
     #99 + 0.10D2 * t202 - 0.8D1 * t204 - 0.8D1 * t207 + 0.10D2 * t209 -
     # 0.8D1 * t211 + 0.3D1 * t213 - 0.8D1 * t216 + 0.8D1 * t219
      t223 = t218 * t14
      t225 = t218 * t142
      t227 = t184 * t145
      t229 = 0.1D1 / t135
      t231 = log(-t229 * z)
      t232 = t231 * x1
      t233 = t232 * t14
      t235 = t231 * t145
      t236 = t235 * z
      t238 = t235 * t14
      t240 = log(-t229)
      t241 = t240 * t145
      t242 = t241 * z
      t244 = t241 * t14
      t246 = t240 * x1
      t247 = t246 * z
      t249 = t246 * t14
      t251 = t232 * t142
      t253 = t232 * t152
      t255 = t235 * t142
      t257 = t235 * t152
      t259 = t246 * t142
      t261 = t246 * t152
      t263 = -0.10D2 * t223 + 0.8D1 * t225 - 0.3D1 * t227 - 0.10D2 * t23
     #3 - 0.8D1 * t236 + 0.10D2 * t238 - 0.11D2 * t242 - 0.2D1 * t244 + 
     #0.47D2 * t247 - 0.52D2 * t249 + 0.8D1 * t251 - 0.3D1 * t253 - 0.8D
     #1 * t255 + 0.3D1 * t257 + 0.47D2 * t259 - 0.21D2 * t261
      t264 = t241 * t142
      t266 = t241 * t152
      t268 = t232 * z
      t270 = polylog(2, x1)
      t271 = t183 ** 2
      t273 = t270 + t271 / 0.2D1
      t274 = t273 * t152
      t275 = t274 * x1
      t276 = 0.1D1 / t187
      t277 = t140 * t276
      t278 = polylog(2, t277)
      t279 = t189 ** 2
      t281 = t278 + t279 / 0.2D1
      t282 = t281 * x1
      t283 = t282 * z
      t285 = t282 * t14
      t287 = t281 * t142
      t288 = t287 * x1
      t290 = t281 * t152
      t291 = t290 * x1
      t292 = t273 * x1
      t293 = t292 * t14
      t295 = t292 * z
      t297 = t281 * t145
      t298 = t297 * z
      t300 = t297 * t14
      t302 = t297 * t142
      t304 = t290 * t145
      t305 = t273 * t142
      t306 = t305 * x1
      t308 = t273 * t145
      t309 = t308 * z
      t311 = t308 * t14
      t313 = -0.11D2 * t264 + 0.12D2 * t266 + 0.8D1 * t268 - t275 - 0.2D
     #1 * t283 + 0.2D1 * t285 - 0.2D1 * t288 + t291 - 0.2D1 * t293 + 0.2
     #D1 * t295 + 0.2D1 * t298 - 0.2D1 * t300 + 0.2D1 * t302 - t304 + 0.
     #2D1 * t306 - 0.2D1 * t309 + 0.2D1 * t311
      t316 = t308 * t142
      t318 = t274 * t145
      t319 = t231 ** 2
      t320 = t319 * x1
      t321 = t320 * t14
      t322 = t319 * t145
      t323 = t322 * z
      t324 = t322 * t14
      t325 = t240 ** 2
      t326 = t325 * t145
      t327 = t326 * z
      t329 = t326 * t14
      t331 = t325 * x1
      t332 = t331 * z
      t334 = t331 * t14
      t336 = t320 * t142
      t337 = t320 * t152
      t339 = t322 * t142
      t340 = t322 * t152
      t342 = t240 * t166
      t343 = t342 * z
      t345 = t342 * t14
      t347 = t342 * t142
      t349 = -0.2D1 * t316 + t318 + t321 + t323 - t324 + 0.7D1 * t327 - 
     #0.7D1 * t329 - 0.7D1 * t332 + 0.7D1 * t334 - t336 + t337 / 0.2D1 +
     # t339 - t340 / 0.2D1 + 0.36D2 * t343 - 0.54D2 * t345 + 0.36D2 * t3
     #47
      t350 = t342 * t152
      t352 = t331 * t142
      t354 = t331 * t152
      t356 = t326 * t142
      t358 = t326 * t152
      t360 = t240 * t157
      t361 = t360 * z
      t363 = t360 * t14
      t365 = t360 * t142
      t367 = t360 * t152
      t369 = t320 * z
      t370 = 0.24D2 * z
      t377 = -0.9D1 * t350 - 0.7D1 * t352 + 0.7D1 / 0.2D1 * t354 + 0.7D1
     # * t356 - 0.7D1 / 0.2D1 * t358 - 0.72D2 * t361 + 0.108D3 * t363 - 
     #0.72D2 * t365 + 0.18D2 * t367 - t369 + t370 + 0.16D2 * x1 - 0.32D2
     # * t14 + 0.3D1 * t206 + 0.3D1 * t215 - 0.3D1 * t218 - 0.3D1 * t190
      t381 = t189 * z
      t383 = t183 * z
      t389 = t231 * z
      t391 = t240 * z
      t393 = t231 * t142
      t395 = t240 * t142
      t397 = t281 * z
      t398 = -0.3D1 * t195 + 0.3D1 * t201 - 0.3D1 * t381 + 0.3D1 * t383 
     #- 0.3D1 * t232 + 0.3D1 * t235 + 0.12D2 * t241 - 0.21D2 * t246 - 0.
     #3D1 * t389 - 0.21D2 * t391 - 0.3D1 * t393 - 0.21D2 * t395 + t282 +
     # t287 - t292 - t297 + t397
      t400 = 0.2D1 * t189 * t14
      t401 = t273 * z
      t403 = 0.2D1 * t183 * t14
      t410 = t319 * z
      t412 = t325 * z
      t414 = t319 * t142
      t416 = t325 * t142
      t419 = 0.2D1 * t231 * t14
      t420 = t240 * t14
      t422 = t400 - t305 + t308 - t401 - t403 + t320 / 0.2D1 - t322 / 0.
     #2D1 - 0.7D1 / 0.2D1 * t326 + 0.7D1 / 0.2D1 * t331 - 0.9D1 * t342 +
     # 0.18D2 * t360 + t410 / 0.2D1 + 0.7D1 / 0.2D1 * t412 + t414 / 0.2D
     #1 + 0.7D1 / 0.2D1 * t416 + t419 + 0.14D2 * t420
      t428 = 0.1D1 / (-z - x1 + t140)
      t432 = 0.16D2 * t140
      t434 = 0.16D2 * t146
      t439 = 0.8D1 * t142
      t441 = -0.48D2 * t138 + t432 + 0.48D2 * t143 - t434 + 0.48D2 * t14
     #8 - 0.48D2 * t150 - 0.16D2 * t153 + 0.16D2 * t155 - t439 - t185 - 
     #0.3D1 * t191
      t452 = 0.4D1 * t193 - 0.3D1 * t196 + t199 - 0.4D1 * t202 + 0.3D1 *
     # t204 + 0.3D1 * t216 + 0.6D1 * t233 + 0.4D1 * t236 - 0.6D1 * t238 
     #- 0.22D2 * t242 + 0.30D2 * t244
      t463 = -0.50D2 * t247 + 0.78D2 * t249 - 0.4D1 * t251 + t253 + 0.4D
     #1 * t255 - t257 - 0.66D2 * t259 + 0.23D2 * t261 - 0.6D1 * t264 - 0
     #.5D1 * t266 - 0.4D1 * t268
      t476 = 0.3D1 * t275 + 0.8D1 * t283 - 0.10D2 * t285 + 0.8D1 * t288 
     #- 0.3D1 * t291 + 0.10D2 * t293 - 0.8D1 * t295 - 0.8D1 * t298 + 0.1
     #0D2 * t300 - 0.8D1 * t302 + 0.3D1 * t304 - 0.8D1 * t306
      t489 = 0.8D1 * t309 - 0.10D2 * t311 + 0.8D1 * t316 - 0.3D1 * t318 
     #- 0.5D1 * t321 - 0.4D1 * t323 + 0.5D1 * t324 - 0.11D2 / 0.2D1 * t3
     #27 - t329 + 0.47D2 / 0.2D1 * t332 - 0.26D2 * t334
      t502 = 0.4D1 * t336 - 0.3D1 / 0.2D1 * t337 - 0.4D1 * t339 + 0.3D1 
     #/ 0.2D1 * t340 - 0.72D2 * t343 + 0.108D3 * t345 - 0.72D2 * t347 + 
     #0.18D2 * t350 + 0.47D2 / 0.2D1 * t352 - 0.21D2 / 0.2D1 * t354 - 0.
     #11D2 / 0.2D1 * t356 + 0.6D1 * t358
      t509 = t183 * t157
      t511 = t189 * t157
      t521 = 0.144D3 * t361 - 0.216D3 * t363 + 0.144D3 * t365 - 0.36D2 *
     # t367 + 0.4D1 * t369 + t509 * t152 + 0.4D1 * t511 * z - 0.6D1 * t5
     #11 * t14 + 0.4D1 * t511 * t142 - t511 * t152 - 0.4D1 * t509 * z
      t526 = t319 * t231
      t527 = t526 * x1
      t530 = t526 * t145
      t535 = t325 * t240
      t536 = t535 * t145
      t541 = t535 * x1
      t552 = 0.6D1 * t509 * t14 - 0.4D1 * t509 * t142 + t527 * t14 / 0.3
     #D1 + t530 * z / 0.3D1 - t530 * t14 / 0.3D1 + 0.7D1 / 0.3D1 * t536 
     #* z - 0.7D1 / 0.3D1 * t536 * t14 - 0.7D1 / 0.3D1 * t541 * z + 0.7D
     #1 / 0.3D1 * t541 * t14 - t527 * t142 / 0.3D1 + t527 * t152 / 0.6D1
     # + t530 * t142 / 0.3D1
      t558 = t325 * t166
      t575 = t325 * t157
      t580 = -t530 * t152 / 0.6D1 + 0.18D2 * t558 * z - 0.27D2 * t558 * 
     #t14 + 0.18D2 * t558 * t142 - 0.9D1 / 0.2D1 * t558 * t152 - 0.7D1 /
     # 0.3D1 * t541 * t142 + 0.7D1 / 0.6D1 * t541 * t152 + 0.7D1 / 0.3D1
     # * t536 * t142 - 0.7D1 / 0.6D1 * t536 * t152 - 0.36D2 * t575 * z +
     # 0.54D2 * t575 * t14
      t587 = -t135
      t588 = log(t587)
      t591 = t588 ** 2
      t595 = polylog(3, t587)
      t596 = polylog(3, x1)
      t600 = -t29 * t588 / 0.6D1 + t591 * t8 / 0.2D1 + t588 * t270 + t59
     #5 + t596 - 0.1202056903159594D1 + t183 * t270 + t271 * t183 / 0.6D
     #1
      t601 = t600 * t152
      t603 = 0.1D1 - t277
      t604 = log(t603)
      t607 = t604 ** 2
      t608 = log(t277)
      t612 = polylog(3, t603)
      t613 = polylog(3, t277)
      t617 = -t29 * t604 / 0.6D1 + t607 * t608 / 0.2D1 + t604 * t278 + t
     #612 + t613 - 0.1202056903159594D1 + t189 * t278 + t279 * t189 / 0.
     #6D1
      t618 = t617 * x1
      t623 = t617 * t142
      t626 = t617 * t152
      t628 = t600 * x1
      t633 = t617 * t145
      t639 = -0.36D2 * t575 * t142 + 0.9D1 * t575 * t152 - t527 * z / 0.
     #3D1 - t601 * x1 - 0.2D1 * t618 * z + 0.2D1 * t618 * t14 - 0.2D1 * 
     #t623 * x1 + t626 * x1 - 0.2D1 * t628 * t14 + 0.2D1 * t628 * z + 0.
     #2D1 * t633 * z - 0.2D1 * t617 * t14 * t145
      t644 = t600 * t142
      t647 = t600 * t145
      t656 = 0.8D1 * z
      t657 = 0.16D2 * t14
      t658 = 0.2D1 * t623 * t145 - t626 * t145 + 0.2D1 * t644 * x1 - 0.2
     #D1 * t647 * z + 0.2D1 * t600 * t14 * t145 - 0.2D1 * t644 * t145 + 
     #t601 * t145 - t656 + t657 - t215 + t190
      t663 = t195 - t201 + t381 - t383 + t232 - t235 + 0.3D1 * t241 + 0.
     #15D2 * t246 + t389 + 0.23D2 * t391 + t393 + 0.23D2 * t395
      t675 = -0.3D1 * t282 - 0.3D1 * t287 + 0.3D1 * t292 + 0.3D1 * t297 
     #- 0.3D1 * t397 - t400 + 0.3D1 * t305 - 0.3D1 * t308 + 0.3D1 * t401
     # + t403 - 0.3D1 / 0.2D1 * t320
      t686 = 0.3D1 / 0.2D1 * t322 + 0.6D1 * t326 - 0.21D2 / 0.2D1 * t331
     # + 0.18D2 * t342 - 0.36D2 * t360 - 0.3D1 / 0.2D1 * t410 - 0.21D2 /
     # 0.2D1 * t412 - 0.3D1 / 0.2D1 * t414 - 0.21D2 / 0.2D1 * t416 - t41
     #9 - 0.30D2 * t420 + t509
      t702 = -t511 + 0.2D1 * t281 * t14 - 0.2D1 * t273 * t14 + t527 / 0.
     #6D1 - t530 / 0.6D1 - 0.7D1 / 0.6D1 * t536 + 0.7D1 / 0.6D1 * t541 -
     # 0.9D1 / 0.2D1 * t558 + 0.9D1 * t575 + t526 * z / 0.6D1 + 0.7D1 / 
     #0.6D1 * t535 * z
      t712 = t526 * t142 / 0.6D1 + 0.7D1 / 0.6D1 * t535 * t142 + t319 * 
     #t14 + 0.7D1 * t325 * t14 + t618 + t623 - t628 + t617 * z - t633 - 
     #t644 - t600 * z + t647
      t729 = t656 + t439 - t432 + 0.8D1 * x1 + 0.8D1 * t153 + 0.16D2 * t
     #138 - 0.16D2 * t143 + t434 - 0.16D2 * t148 + 0.16D2 * t150 - 0.8D1
     # * t155 - 0.8D1 * t145
      t749 = -0.62D2 * t138 + 0.55D2 * t140 + 0.55D2 * t143 - 0.19D2 * t
     #146 + 0.8D1 * t148 - 0.19D2 * t150 - t154 + 0.15D2 * t155 - 0.72D2
     # * t158 + 0.108D3 * t160 - 0.72D2 * t162 + 0.18D2 * t164 + 0.36D2 
     #* t167 - 0.54D2 * t169 + 0.36D2 * t171 - 0.9D1 * t173 + 0.15D2 * t
     #145
      t764 = -0.9D1 * t166 + 0.18D2 * t157 - t179 - t185 - 0.2D1 * t191 
     #+ 0.2D1 * t193 - 0.2D1 * t196 + t199 - 0.2D1 * t202 + 0.2D1 * t204
     # + 0.2D1 * t207 - 0.2D1 * t209 + 0.2D1 * t211 - t213 + 0.2D1 * t21
     #6 - 0.2D1 * t219 + 0.2D1 * t223 - 0.2D1 * t225
      t780 = t227 + 0.2D1 * t233 + 0.2D1 * t236 - 0.2D1 * t238 + 0.14D2 
     #* t242 - 0.14D2 * t244 - 0.14D2 * t247 + 0.14D2 * t249 - 0.2D1 * t
     #251 + t253 + 0.2D1 * t255 - t257 - 0.14D2 * t259 + 0.7D1 * t261 + 
     #0.14D2 * t264 - 0.7D1 * t266 - 0.2D1 * t268 - t370
      t786 = -0.24D2 * x1 + t657 - t206 - t215 + t218 + t190 + t195 - t2
     #01 + t381 - t383 + t232 - t235 - 0.7D1 * t241 + 0.7D1 * t246 + t38
     #9 + 0.7D1 * t391 + t393 + 0.7D1 * t395
      t792 = t12 * (t175 + t221 + t263 + t313 + t349 + t377 + t398 + t42
     #2) * t276 * t428 - 0.120D3 * t18 * (t441 + t452 + t463 + t476 + t4
     #89 + t502 + t521 + t552 + t580 + t639 + t658 + t663 + t675 + t686 
     #+ t702 + t712) * t276 * t428 + t66 * t729 * t276 * t428 + t76 * (t
     #749 + t764 + t780 + t786) * t276 * t428
      t795 = FJET(XB1, XB2, s, -t2 * t135, t2 * x1, 0.0D0, 0.0D0, 0.0D0,
     # t792 * t80 / 0.360D3)
      RVbbargH2n3e1 = t133 * t132 + t795 * t792 * t80 / 0.360D3

      end function



      doubleprecision function RVbbargH2n3e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 * wd
      t5 = z ** 2
      t6 = 0.24D2 * t5
      t7 = 0.24D2 - 0.32D2 * z + t6
      t11 = log(z)
      t13 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t11
      t14 = 0.3141592653589793D1 * t13
      t15 = log(x1)
      t16 = t15 * 0.3141592653589793D1
      t19 = (t14 + 0.120D3 * t16) * wd
      t20 = 0.16D2 * z
      t21 = -0.24D2 + t20 - t6
      t23 = 0.3141592653589793D1 ** 2
      t24 = t23 * 0.3141592653589793D1
      t25 = 0.60D2 * t24
      t26 = t11 ** 2
      t30 = lh ** 2
      t32 = t11 * lh
      t36 = 0.3141592653589793D1 * (-0.60D2 * t26 + 0.480D3 * lh - 0.240
     #D3 * t11 - 0.240D3 * t30 + 0.240D3 * t32 + 0.20D2 * t23 - 0.480D3)
      t38 = t15 ** 2
      t42 = (t25 + t36 - t16 * t13 - 0.60D2 * t38 * 0.3141592653589793D1
     #) * wd
      t44 = 0.8D1 + 0.8D1 * t5
      t47 = 0.1D1 / x1
      t74 = -0.20D2 * t26 * t11 - 0.5753417909889299D3 + 0.120D3 * t26 *
     # lh + 0.40D2 * t23 - 0.120D3 * t26 + 0.480D3 * t32 - 0.240D3 * t11
     # * t30 + 0.160D3 * t30 * lh + 0.960D3 * lh + 0.20D2 * t23 * t11 - 
     #0.480D3 * t11 - 0.40D2 * t23 * lh - 0.480D3 * t30
      t83 = (-0.120D3 * t3 * t7 + t19 * t21 + t42 * t44) * t47 / 0.360D3
     # + (-0.120D3 * 0.3141592653589793D1 * (-0.8D1 + t20 - 0.8D1 * t5) 
     #+ t14 * t7 + (t25 + t36) * t21 + (0.3141592653589793D1 * t74 - t24
     # * t13 / 0.2D1) * t44) * wd / 0.360D3
      t84 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t83)
      t86 = -0.1D1 + x1
      t89 = t86 ** 2
      t90 = 0.1D1 / t89
      t92 = log(t90 * z)
      t93 = t5 ** 2
      t94 = t92 * t93
      t95 = t94 * x1
      t97 = x1 * z
      t98 = -x1 + t97 + 0.1D1
      t100 = log(t98 * t90)
      t101 = t100 * x1
      t102 = t101 * z
      t104 = t101 * t5
      t106 = t5 * z
      t107 = t100 * t106
      t108 = t107 * x1
      t110 = t100 * t93
      t111 = t110 * x1
      t113 = t92 * x1
      t114 = t113 * t5
      t116 = t113 * z
      t118 = x1 ** 2
      t119 = t100 * t118
      t120 = t119 * z
      t122 = t119 * t5
      t124 = t119 * t106
      t126 = t110 * t118
      t128 = t92 * t106
      t129 = t128 * x1
      t131 = t92 * t118
      t132 = t131 * z
      t134 = t131 * t5
      t136 = t131 * t106
      t138 = t94 * t118
      t140 = 0.3D1 * t95 + 0.8D1 * t102 - 0.10D2 * t104 + 0.8D1 * t108 -
     # 0.3D1 * t111 + 0.10D2 * t114 - 0.8D1 * t116 - 0.8D1 * t120 + 0.10
     #D2 * t122 - 0.8D1 * t124 + 0.3D1 * t126 - 0.8D1 * t129 + 0.8D1 * t
     #132 - 0.10D2 * t134 + 0.8D1 * t136 - 0.3D1 * t138
      t141 = 0.1D1 / t86
      t143 = log(-t141 * z)
      t144 = t143 * x1
      t145 = t144 * t5
      t147 = t143 * t118
      t148 = t147 * z
      t150 = t147 * t5
      t152 = log(-t141)
      t153 = t152 * t118
      t154 = t153 * z
      t156 = t153 * t5
      t158 = t152 * x1
      t159 = t158 * z
      t161 = t158 * t5
      t163 = t144 * t106
      t165 = t144 * t93
      t167 = t147 * t106
      t169 = t147 * t93
      t171 = t158 * t106
      t173 = t158 * t93
      t175 = t153 * t106
      t177 = t153 * t93
      t179 = t144 * z
      t181 = polylog(2, x1)
      t182 = t92 ** 2
      t184 = t181 + t182 / 0.2D1
      t185 = t184 * t93
      t187 = -0.10D2 * t145 - 0.8D1 * t148 + 0.10D2 * t150 - 0.11D2 * t1
     #54 - 0.2D1 * t156 + 0.47D2 * t159 - 0.52D2 * t161 + 0.8D1 * t163 -
     # 0.3D1 * t165 - 0.8D1 * t167 + 0.3D1 * t169 + 0.47D2 * t171 - 0.21
     #D2 * t173 - 0.11D2 * t175 + 0.12D2 * t177 + 0.8D1 * t179 - t185 * 
     #x1
      t189 = 0.1D1 / t98
      t191 = polylog(2, t97 * t189)
      t192 = t100 ** 2
      t194 = t191 + t192 / 0.2D1
      t195 = t194 * x1
      t200 = t194 * t106
      t203 = t194 * t93
      t205 = t184 * x1
      t210 = t194 * t118
      t218 = t184 * t106
      t221 = t184 * t118
      t229 = t143 ** 2
      t230 = t229 * x1
      t232 = -0.2D1 * t195 * z + 0.2D1 * t195 * t5 - 0.2D1 * t200 * x1 +
     # t203 * x1 - 0.2D1 * t205 * t5 + 0.2D1 * t205 * z + 0.2D1 * t210 *
     # z - 0.2D1 * t210 * t5 + 0.2D1 * t210 * t106 - t203 * t118 + 0.2D1
     # * t218 * x1 - 0.2D1 * t221 * z + 0.2D1 * t221 * t5 - 0.2D1 * t221
     # * t106 + t185 * t118 + t230 * t5
      t233 = t229 * t118
      t236 = t152 ** 2
      t237 = t236 * t118
      t242 = t236 * x1
      t253 = t118 ** 2
      t254 = t152 * t253
      t269 = t233 * z - t233 * t5 + 0.7D1 * t237 * z - 0.7D1 * t237 * t5
     # - 0.7D1 * t242 * z + 0.7D1 * t242 * t5 - t230 * t106 + t230 * t93
     # / 0.2D1 + t233 * t106 - t233 * t93 / 0.2D1 + 0.36D2 * t254 * z - 
     #0.54D2 * t254 * t5 + 0.36D2 * t254 * t106 - 0.9D1 * t254 * t93 - 0
     #.7D1 * t242 * t106 + 0.7D1 / 0.2D1 * t242 * t93 + 0.7D1 * t237 * t
     #106
      t274 = t118 * x1
      t275 = t152 * t274
      t286 = x1 * t5
      t289 = t106 * x1
      t291 = t118 * z
      t293 = t118 * t5
      t295 = t118 * t106
      t297 = t93 * x1
      t298 = 0.24D2 * t297
      t299 = t93 * t118
      t301 = t274 * z
      t303 = -0.7D1 / 0.2D1 * t237 * t93 - 0.72D2 * t275 * z + 0.108D3 *
     # t275 * t5 - 0.72D2 * t275 * t106 + 0.18D2 * t275 * t93 - t230 * z
     # - 0.32D2 * t5 + 0.84D2 * t286 - 0.54D2 * t97 - 0.70D2 * t289 - 0.
     #18D2 * t291 + 0.24D2 * t293 - 0.2D1 * t295 + t298 - 0.6D1 * t299 +
     # 0.144D3 * t301
      t304 = t274 * t5
      t306 = t274 * t106
      t308 = t274 * t93
      t310 = t253 * z
      t312 = t253 * t5
      t314 = t253 * t106
      t316 = t253 * t93
      t324 = t100 * z
      t326 = t92 * z
      t330 = -0.216D3 * t304 + 0.144D3 * t306 - 0.36D2 * t308 - 0.72D2 *
     # t310 + 0.108D3 * t312 - 0.72D2 * t314 + 0.18D2 * t316 + 0.3D1 * t
     #119 + 0.3D1 * t128 - 0.3D1 * t131 - 0.3D1 * t101 - 0.3D1 * t107 + 
     #0.3D1 * t113 - 0.3D1 * t324 + 0.3D1 * t326 - 0.3D1 * t144 + 0.3D1 
     #* t147
      t334 = t143 * z
      t336 = t152 * z
      t338 = t143 * t106
      t340 = t152 * t106
      t348 = 0.12D2 * t153 - 0.21D2 * t158 - 0.3D1 * t334 - 0.21D2 * t33
     #6 - 0.3D1 * t338 - 0.21D2 * t340 + t195 + t200 - t205 - t210 + t19
     #4 * z + 0.2D1 * t100 * t5 - t218 + t221 - t184 * z + 0.2D1 * t118 
     #+ 0.18D2 * t253
      t350 = 0.24D2 * t106
      t351 = 0.24D2 * z
      t373 = -0.36D2 * t274 + t350 + t351 + 0.16D2 * x1 - 0.2D1 * t92 * 
     #t5 + t230 / 0.2D1 - t233 / 0.2D1 - 0.7D1 / 0.2D1 * t237 + 0.7D1 / 
     #0.2D1 * t242 - 0.9D1 * t254 + 0.18D2 * t275 + t229 * z / 0.2D1 + 0
     #.7D1 / 0.2D1 * t236 * z + t229 * t106 / 0.2D1 + 0.7D1 / 0.2D1 * t2
     #36 * t106 + 0.2D1 * t143 * t5 + 0.14D2 * t152 * t5
      t379 = 0.1D1 / (-z - x1 + t97)
      t396 = -t95 - 0.2D1 * t102 + 0.2D1 * t104 - 0.2D1 * t108 + t111 - 
     #0.2D1 * t114 + 0.2D1 * t116 + 0.2D1 * t120 - 0.2D1 * t122 + 0.2D1 
     #* t124 - t126 + 0.2D1 * t129 - 0.2D1 * t132 + 0.2D1 * t134 - 0.2D1
     # * t136 + t138 + 0.2D1 * t145
      t413 = 0.2D1 * t148 - 0.2D1 * t150 + 0.14D2 * t154 - 0.14D2 * t156
     # - 0.14D2 * t159 + 0.14D2 * t161 - 0.2D1 * t163 + t165 + 0.2D1 * t
     #167 - t169 - 0.14D2 * t171 + 0.7D1 * t173 + 0.14D2 * t175 - 0.7D1 
     #* t177 - 0.2D1 * t179 + 0.16D2 * t5 - 0.62D2 * t286 + 0.55D2 * t97
      t428 = 0.55D2 * t289 - 0.19D2 * t291 + 0.8D1 * t293 - 0.19D2 * t29
     #5 - t298 + 0.15D2 * t299 - 0.72D2 * t301 + 0.108D3 * t304 - 0.72D2
     # * t306 + 0.18D2 * t308 + 0.36D2 * t310 - 0.54D2 * t312 + 0.36D2 *
     # t314 - 0.9D1 * t316 - t119 - t128 + t131 + t101
      t437 = t107 - t113 + t324 - t326 + t144 - t147 - 0.7D1 * t153 + 0.
     #7D1 * t158 + t334 + 0.7D1 * t336 + t338 + 0.7D1 * t340 + 0.15D2 * 
     #t118 - 0.9D1 * t253 + 0.18D2 * t274 - t350 - t351 - 0.24D2 * x1
      t455 = 0.8D1 * z + 0.8D1 * t106 - 0.16D2 * t97 + 0.8D1 * x1 + 0.8D
     #1 * t297 + 0.16D2 * t286 - 0.16D2 * t289 + 0.16D2 * t291 - 0.16D2 
     #* t293 + 0.16D2 * t295 - 0.8D1 * t299 - 0.8D1 * t118
      t459 = -0.120D3 * t3 * (t140 + t187 + t232 + t269 + t303 + t330 + 
     #t348 + t373) * t189 * t379 + t19 * (t396 + t413 + t428 + t437) * t
     #189 * t379 + t42 * t455 * t189 * t379
      t462 = FJET(XB1, XB2, s, -t2 * t86, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #t459 * t47 / 0.360D3)
      RVbbargH2n3e0 = t84 * t83 + t462 * t459 * t47 / 0.360D3

      end function



      doubleprecision function RVbbargH2n3em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 * wd
      t5 = z ** 2
      t6 = 0.24D2 * t5
      t7 = -0.24D2 + 0.16D2 * z - t6
      t11 = log(z)
      t14 = 0.3141592653589793D1 * (-0.240D3 + 0.240D3 * lh - 0.120D3 * 
     #t11)
      t15 = log(x1)
      t19 = (t14 + 0.120D3 * t15 * 0.3141592653589793D1) * wd
      t21 = 0.8D1 + 0.8D1 * t5
      t24 = 0.1D1 / x1
      t31 = 0.3141592653589793D1 ** 2
      t34 = t11 ** 2
      t38 = lh ** 2
      t50 = (-0.120D3 * t3 * t7 + t19 * t21) * t24 / 0.360D3 + (-0.120D3
     # * 0.3141592653589793D1 * (0.24D2 - 0.32D2 * z + t6) + t14 * t7 + 
     #(0.60D2 * t31 * 0.3141592653589793D1 + 0.3141592653589793D1 * (-0.
     #60D2 * t34 + 0.480D3 * lh - 0.240D3 * t11 - 0.240D3 * t38 + 0.240D
     #3 * t11 * lh + 0.20D2 * t31 - 0.480D3)) * t21) * wd / 0.360D3
      t51 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t50)
      t53 = -0.1D1 + x1
      t56 = x1 * t5
      t58 = x1 * z
      t60 = t5 * z
      t61 = t60 * x1
      t63 = x1 ** 2
      t64 = t63 * z
      t66 = t63 * t5
      t68 = t63 * t60
      t70 = t5 ** 2
      t71 = t70 * x1
      t73 = t70 * t63
      t75 = t63 * x1
      t84 = t63 ** 2
      t93 = -x1 + t58 + 0.1D1
      t94 = t53 ** 2
      t95 = 0.1D1 / t94
      t97 = log(t93 * t95)
      t98 = t97 * t63
      t99 = -0.62D2 * t56 + 0.55D2 * t58 + 0.55D2 * t61 - 0.19D2 * t64 +
     # 0.8D1 * t66 - 0.19D2 * t68 - 0.24D2 * t71 + 0.15D2 * t73 - 0.72D2
     # * t75 * z + 0.108D3 * t75 * t5 - 0.72D2 * t75 * t60 + 0.18D2 * t7
     #5 * t70 + 0.36D2 * t84 * z - 0.54D2 * t84 * t5 + 0.36D2 * t84 * t6
     #0 - 0.9D1 * t84 * t70 - t98
      t101 = log(t95 * z)
      t102 = t101 * t60
      t103 = t101 * t63
      t104 = t97 * x1
      t105 = t97 * t60
      t106 = t101 * x1
      t109 = 0.1D1 / t53
      t111 = log(-t109 * z)
      t112 = t111 * x1
      t113 = t111 * t63
      t114 = log(-t109)
      t115 = t114 * t63
      t117 = t114 * x1
      t128 = -t102 + t103 + t104 + t105 - t106 + t97 * z - t101 * z + t1
     #12 - t113 - 0.7D1 * t115 + 0.7D1 * t117 + t111 * z + 0.7D1 * t114 
     #* z + t111 * t60 + 0.7D1 * t114 * t60 + 0.15D2 * t63 - 0.9D1 * t84
     # + 0.18D2 * t75
      t131 = t101 * t70
      t139 = t97 * t70
      t163 = -0.24D2 * t60 - t131 * x1 - 0.2D1 * t104 * z + 0.2D1 * t104
     # * t5 - 0.2D1 * t105 * x1 + t139 * x1 - 0.2D1 * t106 * t5 + 0.2D1 
     #* t106 * z + 0.2D1 * t98 * z - 0.2D1 * t98 * t5 + 0.2D1 * t98 * t6
     #0 - t139 * t63 + 0.2D1 * t102 * x1 - 0.2D1 * t103 * z + 0.2D1 * t1
     #03 * t5 - 0.2D1 * t103 * t60 + t131 * t63 + 0.2D1 * t112 * t5
      t195 = 0.2D1 * t113 * z - 0.2D1 * t113 * t5 + 0.14D2 * t115 * z - 
     #0.14D2 * t115 * t5 - 0.14D2 * t117 * z + 0.14D2 * t117 * t5 - 0.2D
     #1 * t112 * t60 + t112 * t70 + 0.2D1 * t113 * t60 - t113 * t70 - 0.
     #14D2 * t117 * t60 + 0.7D1 * t117 * t70 + 0.14D2 * t115 * t60 - 0.7
     #D1 * t115 * t70 - 0.2D1 * t112 * z + 0.16D2 * t5 - 0.24D2 * z - 0.
     #24D2 * x1
      t198 = 0.1D1 / t93
      t201 = 0.1D1 / (-z - x1 + t58)
      t217 = 0.8D1 * z + 0.8D1 * t60 - 0.16D2 * t58 + 0.8D1 * x1 + 0.8D1
     # * t71 + 0.16D2 * t56 - 0.16D2 * t61 + 0.16D2 * t64 - 0.16D2 * t66
     # + 0.16D2 * t68 - 0.8D1 * t73 - 0.8D1 * t63
      t221 = -0.120D3 * t3 * (t99 + t128 + t163 + t195) * t198 * t201 + 
     #t19 * t217 * t198 * t201
      t224 = FJET(XB1, XB2, s, -t2 * t53, t2 * x1, 0.0D0, 0.0D0, 0.0D0, 
     #t221 * t24 / 0.360D3)
      RVbbargH2n3em1 = t51 * t50 + t224 * t221 * t24 / 0.360D3

      end function



      doubleprecision function RVbbargH2n3em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t3 = 0.3141592653589793D1 * wd
      t4 = z ** 2
      t6 = 0.8D1 + 0.8D1 * t4
      t7 = 0.1D1 / x1
      t17 = log(z)
      t25 = -t3 * t6 * t7 / 0.3D1 + (-0.120D3 * 0.3141592653589793D1 * (
     #-0.24D2 + 0.16D2 * z - 0.24D2 * t4) + 0.3141592653589793D1 * (-0.2
     #40D3 + 0.240D3 * lh - 0.120D3 * t17) * t6) * wd / 0.360D3
      t26 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t25)
      t32 = t4 * z
      t34 = x1 * z
      t37 = t4 ** 2
      t44 = x1 ** 2
      t54 = 0.8D1 * z + 0.8D1 * t32 - 0.16D2 * t34 + 0.8D1 * x1 + 0.8D1 
     #* t37 * x1 + 0.16D2 * x1 * t4 - 0.16D2 * t32 * x1 + 0.16D2 * t44 *
     # z - 0.16D2 * t44 * t4 + 0.16D2 * t44 * t32 - 0.8D1 * t37 * t44 - 
     #0.8D1 * t44
      t57 = 0.1D1 / (-x1 + t34 + 0.1D1)
      t59 = 0.1D1 / (-z - x1 + t34)
      t64 = FJET(XB1, XB2, s, -t2 * (-0.1D1 + x1), t2 * x1, 0.0D0, 0.0D0
     #, 0.0D0, -t3 * t54 * t57 * t59 * t7 / 0.3D1)
      RVbbargH2n3em2 = t26 * t25 - t64 * 0.3141592653589793D1 * wd * t54
     # * t57 * t59 * t7 / 0.3D1

      end function



      doubleprecision function RVbbargH2n3em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t3 = z ** 2
      t5 = 0.8D1 + 0.8D1 * t3
      t9 = FJET(XB1, XB2, s, s * (-0.1D1 + z), 0.0D0, 0.0D0, 0.0D0, 0.0D
     #0, -0.3141592653589793D1 * t5 * wd / 0.3D1)
      RVbbargH2n3em3 = -t9 * 0.3141592653589793D1 * t5 * wd / 0.3D1

      end function



      doubleprecision function RVbbargH2n3em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n3em4 = 0.0D0

      end function


      doubleprecision function RVbbargH2n4e1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t8 = z ** 2
      t9 = 0.1D1 - 0.2D1 * z + t8
      t14 = log(z)
      t16 = -0.240D3 + 0.240D3 * lh - 0.120D3 * t14
      t17 = 0.3141592653589793D1 * t16
      t18 = log(x1)
      t19 = t18 * 0.3141592653589793D1
      t22 = (0.240D3 * 0.3141592653589793D1 + t17 + 0.240D3 * t19) * wd
      t23 = -0.1D1 - t8
      t27 = x1 * z
      t28 = -z - x1 + t27
      t30 = z * t4 / t28
      t31 = polylog(2, t30)
      t32 = 0.1D1 / t4
      t34 = log(t28 * t32)
      t35 = t34 ** 2
      t37 = -t4
      t38 = polylog(2, t37)
      t40 = log(-t32 * z)
      t41 = t40 ** 2
      t53 = 0.3141592653589793D1 ** 2
      t56 = t14 ** 2
      t60 = lh ** 2
      t69 = t18 ** 2
      t80 = log(t37)
      t84 = polylog(3, x1)
      t85 = polylog(3, t37)
      t89 = 0.1D1 - t30
      t90 = log(t89)
      t93 = t90 ** 2
      t94 = log(t30)
      t98 = polylog(3, t89)
      t99 = polylog(3, t30)
      t103 = -t53 * t18 / 0.6D1 + t69 * t80 / 0.2D1 + t18 * t38 + t84 + 
     #t85 + t40 * t38 + t41 * t40 / 0.6D1 + t53 * t90 / 0.6D1 - t93 * t9
     #4 / 0.2D1 - t90 * t31 - t98 - t99 - t34 * t31 - t35 * t34 / 0.6D1
      t107 = (-0.120D3 * t6 * t9 + t22 * t23) * (-t31 - t35 / 0.2D1 + t3
     #8 + t41 / 0.2D1) + (-0.120D3 * t6 * (x1 - 0.2D1 * t27 + x1 * t8) +
     # t22 * t9 + (-0.2D1 * t17 - 0.480D3 * t19 + 0.60D2 * t53 * 0.31415
     #92653589793D1 + 0.3141592653589793D1 * (-0.60D2 * t56 + 0.480D3 * 
     #lh - 0.240D3 * t14 - 0.240D3 * t60 + 0.240D3 * t14 * lh + 0.20D2 *
     # t53 - 0.480D3) - 0.2D1 * t19 * t16 - 0.240D3 * t69 * 0.3141592653
     #589793D1) * wd * t23) * (-t34 + t40) - 0.120D3 * t6 * t23 * t103
      t108 = 0.1D1 / x1
      t111 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, -
     #t107 * t108 / 0.360D3)
      RVbbargH2n4e1 = -t111 * t107 * t108 / 0.360D3

      end function



      doubleprecision function RVbbargH2n4e0
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x1
      t6 = 0.3141592653589793D1 * wd
      t7 = z ** 2
      t8 = -0.1D1 - t7
      t11 = -z - x1 + x1 * z
      t14 = polylog(2, z * t4 / t11)
      t15 = 0.1D1 / t4
      t17 = log(t11 * t15)
      t18 = t17 ** 2
      t21 = polylog(2, -t4)
      t23 = log(-t15 * z)
      t24 = t23 ** 2
      t36 = log(z)
      t40 = log(x1)
      t49 = -0.120D3 * t6 * t8 * (-t14 - t18 / 0.2D1 + t21 + t24 / 0.2D1
     #) + (-0.120D3 * t6 * (0.1D1 - 0.2D1 * z + t7) + (0.240D3 * 0.31415
     #92653589793D1 + 0.3141592653589793D1 * (-0.240D3 + 0.240D3 * lh - 
     #0.120D3 * t36) + 0.240D3 * t40 * 0.3141592653589793D1) * wd * t8) 
     #* (-t17 + t23)
      t50 = 0.1D1 / x1
      t53 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, -t
     #49 * t50 / 0.360D3)
      RVbbargH2n4e0 = -t53 * t49 * t50 / 0.360D3

      end function



      doubleprecision function RVbbargH2n4em1
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x1
      t7 = z ** 2
      t11 = 0.1D1 / t4
      t13 = log((-z - x1 + x1 * z) * t11)
      t15 = log(-t11 * z)
      t19 = (-0.1D1 - t7) * (-t13 + t15) / x1
      t22 = FJET(XB1, XB2, s, t2 * x1, -t2 * t4, 0.0D0, 0.0D0, 0.0D0, 0.
     #3141592653589793D1 * wd * t19 / 0.3D1)
      RVbbargH2n4em1 = t22 * 0.3141592653589793D1 * wd * t19 / 0.3D1

      end function



      doubleprecision function RVbbargH2n4em2
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n4em2 = 0.0D0

      end function



      doubleprecision function RVbbargH2n4em3
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n4em3 = 0.0D0

      end function



      doubleprecision function RVbbargH2n4em4
     &(s, XB1, XB2, z, lh, wd, x1) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision polylog
      doubleprecision Log
      RVbbargH2n4em4 = 0.0D0

      end function
