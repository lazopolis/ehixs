  
      subroutine rrgg2gght12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght12s1e1  
      doubleprecision rrgg2gght12s1e0  
      doubleprecision rrgg2gght12s1em1  
      doubleprecision rrgg2gght12s1em2  
      doubleprecision rrgg2gght12s1em3  
      doubleprecision rrgg2gght12s1em4  
      doubleprecision rrgg2gght12s2e1  
      doubleprecision rrgg2gght12s2e0  
      doubleprecision rrgg2gght12s2em1  
      doubleprecision rrgg2gght12s2em2  
      doubleprecision rrgg2gght12s2em3  
      doubleprecision rrgg2gght12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght12s1e1
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
      t4 = -0.1D1 + x3
      t6 = wd * z
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = t13 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = t17 ** 2
      t19 = cos(t8)
      t20 = t19 ** 2
      t22 = x3 * t4
      t23 = Sqrt(-t22)
      t24 = t23 ** 2
      t28 = t6 * lh
      t33 = pi ** 2
      t35 = lh ** 2
      t37 = -0.30D2 * t33 + 0.180D3 * t35
      t40 = t6 * t37 * t20 * t24
      t43 = 0.1D1 / x2
      t46 = t20 * wd
      t47 = t46 * z
      t50 = x3 * t10
      t51 = t12 * t4
      t54 = log(-0.4D1 * t50 * t51)
      t56 = t54 * t20 * wd
      t58 = (-0.2D1 * t46 - t56) * z
      t61 = t54 ** 2
      t63 = t61 * t20 * wd
      t66 = (t46 + 0.2D1 * t56 + t63 / 0.2D1) * z
      t94 = x1 ** 2
      t98 = log(-0.4D1 * t7 * t94 * t14)
      t108 = 0.1D1 / x1
      t112 = x3 * t94
      t115 = log(-0.4D1 * t112 * t14)
      t116 = t115 ** 2
      t128 = -(-0.720D3 * t6 * t18 * t20 * t24 - 0.2880D4 * t28 * t17 * 
     #t20 * t24 - 0.16D2 * t40) * t43 / 0.320D3 + (-0.180D3 * (0.3D1 * t
     #47 + 0.2D1 * t58 + t66) * lh + t46 * z * (-0.240D3 * zeta3 - 0.120
     #D3 * t35 * lh + 0.60D2 * lh * t33) + 0.360D3 * t47 + 0.270D3 * t58
     # + 0.180D3 * t66 + 0.90D2 * (-t56 - t63 - t61 * t54 * t20 * wd / 0
     #.6D1) * z + (0.2D1 * t47 + t58) * t37) * t24 / 0.20D2 - (0.1440D4 
     #* t6 * t98 * t20 * t24 + 0.2880D4 * t6 * lh * t20 * t24) * t108 * 
     #t43 / 0.160D3 + (0.45D2 * t46 * z * t116 * t24 + 0.180D3 * t47 * l
     #h * t115 * t24 + t40) * t108 / 0.10D2
      t129 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #128)
      t131 = -0.1D1 + x1
      t132 = x3 * x1
      t133 = t132 * z
      t134 = 0.2D1 * t7
      t135 = t132 * x2
      t136 = x2 * z
      t137 = t132 * t136
      t138 = sqrt(x2)
      t139 = t19 * t138
      t140 = -0.1D1 + t138
      t141 = x3 * t140
      t142 = t138 + 0.1D1
      t143 = x1 * z
      t144 = 0.1D1 - x1 + t143
      t148 = Sqrt(t141 * t142 * t144 * t4)
      t150 = 0.2D1 * t139 * t148
      t153 = 0.1D1 / t144
      t156 = t2 * t132
      t157 = x1 * x2
      t158 = t157 * z
      t159 = 0.1D1 - x1 + t143 - x2 + t157 - t158 - x3 + t132 - t133 + t
     #134 - t135 + t137 + t150
      t163 = t4 * s
      t165 = t163 * t1 * x1
      t166 = t1 ** 2
      t172 = t131 ** 2
      t176 = t94 * t12
      t177 = t140 * t142
      t182 = log(0.4D1 * x2 * t172 * t50 * t4 * t176 * t177 * t153)
      t185 = (x2 - t157 - 0.1D1 + x1 - t136 + t158 - t143) ** 2
      t187 = t138 * x3
      t188 = 0.6D1 * t187
      t189 = t138 * x2
      t191 = 0.4D1 * t189 * x3
      t192 = t189 * z
      t193 = 0.2D1 * t192
      t194 = t138 * z
      t195 = x1 * t138
      t197 = x1 * t189
      t199 = t94 * t189
      t200 = t138 * t94
      t202 = t19 * t148
      t204 = t202 * x1
      t207 = 0.3D1 * t138
      t208 = 0.2D1 * t189
      t210 = 0.4D1 * t192 * x3
      t216 = 0.2D1 * t187 * z
      t223 = -t188 + t191 + t193 - t194 - 0.5D1 * t195 + 0.3D1 * t197 - 
     #t199 + 0.2D1 * t200 - 0.4D1 * t202 + 0.4D1 * t136 * t204 + t207 - 
     #t208 - t210 - 0.4D1 * t192 * x1 - 0.4D1 * t112 * t138 + t216 + 0.1
     #0D2 * t132 * t138 + 0.2D1 * t199 * x3 - 0.6D1 * t197 * x3
      t235 = x2 * t19
      t241 = t148 * x1
      t260 = t11 * x3
      t267 = 0.6D1 * t195 * z - t195 * t11 - 0.4D1 * t200 * z + 0.2D1 * 
     #t200 * t11 + 0.2D1 * t199 * z + t197 * t11 - t199 * t11 + 0.4D1 * 
     #t235 * t148 + 0.4D1 * t204 + 0.8D1 * t192 * t132 - 0.4D1 * t235 * 
     #t241 - 0.4D1 * z * t19 * t241 - 0.4D1 * t136 * t202 + 0.2D1 * t132
     # * t138 * t11 + 0.8D1 * t112 * t194 - 0.4D1 * t94 * t11 * t187 - 0
     #.4D1 * t199 * z * x3 - 0.2D1 * t197 * t260 + 0.2D1 * t199 * t260 -
     # 0.12D2 * t132 * t194
      t269 = (t223 + t267) ** 2
      t271 = 0.1D1 / t185 * t269 * t153
      t276 = 0.90D2 * t6 * t182 * t271 + 0.180D3 * t28 * t271
      t280 = FJET(XB1, XB2, s, t2 * t131 * (-x3 + t132 - t133 + t134 - t
     #135 + t137 - x2 + t150) * t153, t156, -t2 * t131 * t159 * t153, -t
     #165, -s * t166 * x2 * x1 * t131 * t153, -t276 * t108 * t43 / 0.160
     #D3)
      t287 = Sqrt(t141 * t142 * t4)
      t289 = 0.2D1 * t139 * t287
      t294 = x2 * t10
      t299 = log(0.4D1 * t294 * x3 * t51 * t177)
      t300 = t299 ** 2
      t302 = (-x2 + 0.1D1 + t136) ** 2
      t303 = 0.1D1 / t302
      t305 = t19 * t287
      t311 = t188 - t191 - t193 + 0.4D1 * t305 + t208 + 0.4D1 * t136 * t
     #305 + t210 - 0.4D1 * t235 * t287 + t194 - t207 - t216
      t312 = t311 ** 2
      t330 = log(0.4D1 * t294 * t22 * t176 * t177)
      t343 = -(0.45D2 * t6 * t300 * t303 * t312 + 0.180D3 * t28 * t299 *
     # t303 * t312 + t6 * t37 * t303 * t312) * t43 / 0.320D3 - (-0.90D2 
     #* t6 * t330 * t303 * t312 - 0.180D3 * t6 * lh * t303 * t312) * t10
     #8 * t43 / 0.160D3
      t344 = FJET(XB1, XB2, s, -t2 * (-x3 + t134 - x2 + t289), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t134 + t289), 0.0D0, 0.0D0, t343)
      t356 = log(-0.4D1 * t153 * t172 * t4 * t94 * t13 * t7)
      t360 = Sqrt(-x3 * t144 * t4)
      t361 = t360 ** 2
      t363 = t20 * t361 * t153
      t378 = log(-0.4D1 * t112 * t10 * t12 * t153 * t172 * t4)
      t379 = t378 ** 2
      t395 = -(-0.1440D4 * t6 * t356 * t363 - 0.2880D4 * t28 * t363) * t
     #108 * t43 / 0.160D3 + (-0.45D2 * t47 * t379 * t361 * t153 - 0.180D
     #3 * t47 * lh * t378 * t361 * t153 - t47 * t37 * t361 * t153) * t10
     #8 / 0.10D2
      t396 = FJET(XB1, XB2, s, -t2 * t131 * x3, t156, t163 * t1 * t131, 
     #-t165, 0.0D0, t395)
      rrgg2gght12s1e1 = t129 * t128 - t280 * t276 * t108 * t43 / 0.160D3
     # + t344 * t343 + t396 * t395

      end function



      doubleprecision function rrgg2gght12s1e0
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
      t4 = -0.1D1 + x3
      t6 = wd * z
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = cos(t8)
      t19 = t18 ** 2
      t22 = Sqrt(-x3 * t4)
      t23 = t22 ** 2
      t29 = t6 * lh * t19 * t23
      t32 = 0.1D1 / x2
      t35 = t19 * wd
      t36 = t35 * z
      t37 = 0.1D1 / x1
      t42 = x1 ** 2
      t43 = x3 * t42
      t46 = log(-0.4D1 * t43 * t14)
      t58 = t12 * t4
      t61 = log(-0.4D1 * x3 * t10 * t58)
      t63 = t61 * t19 * wd
      t65 = (-0.2D1 * t35 - t63) * z
      t72 = t61 ** 2
      t79 = pi ** 2
      t81 = lh ** 2
      t89 = -(0.1440D4 * t6 * t17 * t19 * t23 + 0.2880D4 * t29) * t32 / 
     #0.320D3 + 0.9D1 * t36 * t23 * t37 * t32 + (-0.90D2 * t35 * z * t46
     # * t23 - 0.180D3 * t29) * t37 / 0.10D2 + (-0.180D3 * (0.2D1 * t36 
     #+ t65) * lh + 0.270D3 * t36 + 0.180D3 * t65 + 0.90D2 * (t35 + 0.2D
     #1 * t63 + t72 * t19 * wd / 0.2D1) * z + t35 * z * (-0.30D2 * t79 +
     # 0.180D3 * t81)) * t23 / 0.20D2
      t90 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t8
     #9)
      t92 = -0.1D1 + x1
      t93 = x3 * x1
      t94 = t93 * z
      t95 = 0.2D1 * t7
      t96 = t93 * x2
      t97 = x2 * z
      t98 = t93 * t97
      t99 = sqrt(x2)
      t100 = t18 * t99
      t101 = -0.1D1 + t99
      t102 = x3 * t101
      t103 = t99 + 0.1D1
      t104 = x1 * z
      t105 = 0.1D1 - x1 + t104
      t109 = Sqrt(t102 * t103 * t105 * t4)
      t111 = 0.2D1 * t100 * t109
      t114 = 0.1D1 / t105
      t117 = t2 * t93
      t118 = x1 * x2
      t119 = t118 * z
      t120 = 0.1D1 - x1 + t104 - x2 + t118 - t119 - x3 + t93 - t94 + t95
     # - t96 + t98 + t111
      t124 = t4 * s
      t126 = t124 * t1 * x1
      t127 = t1 ** 2
      t134 = (x2 - t118 - 0.1D1 + x1 - t97 + t119 - t104) ** 2
      t135 = 0.1D1 / t134
      t137 = t99 * x2
      t138 = t137 * z
      t140 = 0.4D1 * t138 * x3
      t145 = x3 * t99
      t147 = 0.2D1 * t145 * z
      t150 = t42 * t137
      t153 = x1 * t137
      t156 = x1 * t99
      t160 = t99 * t42
      t169 = x2 * t18
      t172 = t18 * t109
      t173 = t172 * x1
      t175 = 0.3D1 * t99
      t176 = 0.2D1 * t137
      t179 = -t140 - 0.4D1 * x1 * t138 - 0.4D1 * t43 * t99 + t147 + 0.10
     #D2 * t93 * t99 + 0.2D1 * t150 * x3 - 0.6D1 * t153 * x3 + 0.6D1 * t
     #156 * z - t156 * t11 - 0.4D1 * t160 * z + 0.2D1 * t160 * t11 + 0.2
     #D1 * t150 * z + t153 * t11 - t150 * t11 + 0.4D1 * t169 * t109 + 0.
     #4D1 * t173 + t175 - t176 + 0.4D1 * t97 * t173
      t182 = t109 * x1
      t193 = t99 * z
      t202 = t11 * x3
      t209 = 0.6D1 * t145
      t211 = 0.4D1 * t137 * x3
      t212 = 0.2D1 * t138
      t217 = 0.8D1 * t138 * t93 - 0.4D1 * t169 * t182 - 0.4D1 * z * t18 
     #* t182 - 0.4D1 * t97 * t172 + 0.2D1 * t93 * t99 * t11 + 0.8D1 * t4
     #3 * t193 - 0.4D1 * t42 * t11 * t145 - 0.4D1 * t150 * z * x3 - 0.2D
     #1 * t153 * t202 + 0.2D1 * t150 * t202 - 0.12D2 * t93 * t193 - t209
     # + t211 + t212 - t193 - 0.5D1 * t156 + 0.3D1 * t153 - t150 + 0.2D1
     # * t160 - 0.4D1 * t172
      t219 = (t179 + t217) ** 2
      t221 = t37 * t32
      t222 = t219 * t114 * t221
      t225 = FJET(XB1, XB2, s, t2 * t92 * (-x3 + t93 - t94 + t95 - t96 +
     # t98 - x2 + t111) * t114, t117, -t2 * t92 * t120 * t114, -t126, -s
     # * t127 * x2 * x1 * t92 * t114, 0.9D1 / 0.16D2 * t6 * t135 * t222)
      t233 = Sqrt(t102 * t103 * t4)
      t235 = 0.2D1 * t100 * t233
      t246 = log(0.4D1 * x2 * t10 * x3 * t58 * t101 * t103)
      t248 = (-x2 + 0.1D1 + t97) ** 2
      t249 = 0.1D1 / t248
      t251 = t18 * t233
      t257 = t209 - t211 - t212 + 0.4D1 * t251 + t176 + 0.4D1 * t97 * t2
     #51 + t140 - 0.4D1 * t169 * t233 + t193 - t175 - t147
      t258 = t257 ** 2
      t274 = -(-0.90D2 * t6 * t246 * t249 * t258 - 0.180D3 * t6 * lh * t
     #249 * t258) * t32 / 0.320D3 - 0.9D1 / 0.16D2 * t6 * t249 * t258 * 
     #t37 * t32
      t275 = FJET(XB1, XB2, s, -t2 * (-x3 + t95 - x2 + t235), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t95 + t235), 0.0D0, 0.0D0, t274)
      t283 = Sqrt(-x3 * t105 * t4)
      t284 = t283 ** 2
      t291 = t92 ** 2
      t296 = log(-0.4D1 * t43 * t10 * t12 * t114 * t291 * t4)
      t309 = -0.9D1 * t36 * t284 * t114 * t221 + (0.90D2 * t36 * t296 * 
     #t284 * t114 + 0.180D3 * t6 * lh * t19 * t284 * t114) * t37 / 0.10D
     #2
      t310 = FJET(XB1, XB2, s, -t2 * t92 * x3, t117, t124 * t1 * t92, -t
     #126, 0.0D0, t309)
      rrgg2gght12s1e0 = t90 * t89 + 0.9D1 / 0.16D2 * t225 * wd * z * t13
     #5 * t222 + t275 * t274 + t310 * t309

      end function



      doubleprecision function rrgg2gght12s1em1
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
      t4 = -0.1D1 + x3
      t6 = wd * z
      t7 = x4 * pi
      t8 = cos(t7)
      t9 = t8 ** 2
      t11 = Sqrt(-x3 * t4)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = 0.1D1 / x1
      t18 = t9 * wd
      t22 = t18 * z
      t25 = Sin(t7)
      t26 = t25 ** 2
      t28 = z ** 2
      t33 = log(-0.4D1 * x3 * t26 / t28 * t4)
      t42 = 0.1D1 / x2
      t46 = 0.9D1 * t6 * t13 * t14 + (-0.180D3 * t18 * z * lh + 0.180D3 
     #* t22 + 0.90D2 * (-0.2D1 * t18 - t33 * t9 * wd) * z) * t12 / 0.20D
     #2 + 0.9D1 / 0.2D1 * t6 * t13 * t42
      t47 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #6)
      t49 = -0.1D1 + x1
      t54 = t4 * s
      t60 = 0.1D1 - x1 + x1 * z
      t63 = Sqrt(-x3 * t60 * t4)
      t64 = t63 ** 2
      t65 = 0.1D1 / t60
      t70 = FJET(XB1, XB2, s, -t2 * t49 * x3, t2 * x1 * x3, t54 * t1 * t
     #49, -t54 * t1 * x1, 0.0D0, -0.9D1 * t22 * t64 * t65 * t14)
      t79 = 0.2D1 * x2 * x3
      t80 = sqrt(x2)
      t87 = Sqrt(x3 * (-0.1D1 + t80) * (t80 + 0.1D1) * t4)
      t89 = 0.2D1 * t8 * t80 * t87
      t94 = x2 * z
      t96 = (-x2 + 0.1D1 + t94) ** 2
      t98 = t80 * x3
      t100 = t80 * x2
      t103 = t100 * z
      t105 = t8 * t87
      t119 = 0.6D1 * t98 - 0.4D1 * t100 * x3 - 0.2D1 * t103 + 0.4D1 * t1
     #05 + 0.2D1 * t100 + 0.4D1 * t94 * t105 + 0.4D1 * t103 * x3 - 0.4D1
     # * x2 * t8 * t87 + t80 * z - 0.3D1 * t80 - 0.2D1 * t98 * z
      t120 = t119 ** 2
      t122 = 0.1D1 / t96 * t120 * t42
      t125 = FJET(XB1, XB2, s, -t2 * (-x3 + t79 - x2 + t89), 0.0D0, t2 *
     # (0.1D1 - x2 - x3 + t79 + t89), 0.0D0, 0.0D0, -0.9D1 / 0.32D2 * t6
     # * t122)
      rrgg2gght12s1em1 = t47 * t46 - 0.9D1 * t70 * wd * z * t9 * t64 * t
     #65 * t14 - 0.9D1 / 0.32D2 * t125 * wd * z * t122

      end function



      doubleprecision function rrgg2gght12s1em2
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
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t8 = cos(x4 * pi)
      t9 = t8 ** 2
      t11 = Sqrt(-x3 * t4)
      t12 = t11 ** 2
      t16 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, 0.
     #9D1 / 0.2D1 * wd * z * t9 * t12)
      rrgg2gght12s1em2 = 0.9D1 / 0.2D1 * t16 * wd * z * t9 * t12

      end function



      doubleprecision function rrgg2gght12s1em3
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
      rrgg2gght12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght12s1em4
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
      rrgg2gght12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght12s2e1
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
      t4 = -0.1D1 + x3
      t6 = z ** 2
      t8 = 0.1D1 / t6 / z
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t16 = log(-0.4D1 * x3 * t8 * t12 * t4)
      t17 = t16 ** 2
      t18 = x3 * z
      t20 = Sqrt(-t18 * t4)
      t21 = t20 ** 2
      t23 = cos(t10)
      t24 = t23 ** 2
      t25 = t24 * wd
      t26 = t25 * lh
      t29 = t21 * t24
      t31 = lh ** 2
      t34 = pi ** 2
      t41 = x2 * t12
      t44 = sqrt(x2)
      t45 = -0.1D1 + t44
      t46 = t44 + 0.1D1
      t47 = t45 * t46
      t51 = log(0.4D1 * t41 * x3 * t4 * t8 * t47)
      t52 = t51 ** 2
      t53 = t44 * z
      t54 = x3 * t44
      t56 = 0.2D1 * t54 * z
      t57 = x3 * t45
      t61 = Sqrt(t57 * t46 * z * t4)
      t66 = (-t53 + t56 + 0.4D1 * t61 * t23 - t44 + 0.2D1 * t54) ** 2
      t74 = log(-0.4D1 * x2 * t8 * t12 * x3 * t4)
      t75 = t74 ** 2
      t82 = wd * lh
      t92 = -0.30D2 * t34 + 0.180D3 * t31
      t95 = -t66 + 0.16D2 * t29
      t98 = 0.1D1 / x2
      t106 = t25 * t92
      t109 = x2 * x3
      t110 = x1 ** 2
      t113 = t12 * t8 * t4
      t116 = log(-0.4D1 * t109 * t110 * t113)
      t120 = x3 * t4
      t127 = log(0.4D1 * t41 * t120 * t110 * t45 * t46 * t8)
      t135 = 0.1D1 / x1
      t139 = x3 * t110
      t142 = log(-0.4D1 * t139 * t113)
      t143 = t142 ** 2
      t157 = -0.9D1 / 0.2D1 * t17 * t21 * t26 + t29 * wd * (-0.240D3 * z
     #eta3 - 0.120D3 * t31 * lh + 0.60D2 * lh * t34) / 0.20D2 + (0.90D2 
     #* wd * (-t52 * t66 / 0.2D1 + 0.8D1 * t75 * t21 * t24) - 0.180D3 * 
     #t82 * (t51 * t66 - 0.16D2 * t74 * t21 * t24) + wd * t92 * t95) * t
     #98 / 0.320D3 - 0.3D1 / 0.4D1 * t17 * t16 * t21 * t25 - t16 * t21 *
     # t106 / 0.20D2 + (0.90D2 * wd * (-0.16D2 * t116 * t21 * t24 + t127
     # * t66) - 0.180D3 * t82 * t95) * t135 * t98 / 0.160D3 + (0.720D3 *
     # t25 * t143 * t21 + 0.2880D4 * t25 * lh * t142 * t21 + 0.16D2 * t2
     #5 * t92 * t21) * t135 / 0.160D3
      t158 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #157)
      t160 = x3 * x1
      t162 = -0.1D1 + x1
      t164 = t2 * t162 * x3
      t165 = t4 * s
      t169 = t165 * t1 * t162
      t173 = t162 ** 2
      t174 = 0.1D1 / t6 * t173
      t175 = x1 * z
      t176 = -z - x1 + t175
      t177 = 0.1D1 / t176
      t179 = t174 * t177 * t4
      t182 = log(0.4D1 * t109 * t110 * t12 * t179)
      t187 = Sqrt(x3 * t176 * t4)
      t188 = t187 ** 2
      t190 = t188 * z * t177
      t201 = log(0.4D1 * t139 * t12 * t179)
      t202 = t201 ** 2
      t207 = z * t177
      t216 = (-0.1440D4 * wd * t182 * t24 * t190 - 0.2880D4 * t26 * t190
     #) * t135 * t98 / 0.160D3 + (0.720D3 * t25 * t202 * t190 + 0.2880D4
     # * t26 * t201 * t188 * t207 + 0.16D2 * t106 * t190) * t135 / 0.160
     #D3
      t217 = FJET(XB1, XB2, s, t2 * t160, -t164, -t165 * t1 * x1, t169, 
     #0.0D0, t216)
      t219 = t160 * z
      t220 = t109 * z
      t221 = t160 * x2
      t222 = x2 * z
      t223 = t160 * t222
      t228 = Sqrt(-t57 * t46 * t176 * t4)
      t230 = 0.2D1 * t23 * t44 * t228
      t235 = x1 * x2
      t236 = t235 * z
      t237 = z + x1 - t175 - t222 - t235 + t236 - t18 - t160 + t219 + t2
     #20 + t221 - t223 + t109 + t230
      t241 = t1 ** 2
      t253 = log(-0.4D1 * t41 * t120 * t110 * t174 * t47 * t177)
      t255 = t44 * x2
      t256 = x1 * t255
      t257 = t44 * t6
      t258 = t110 * t255
      t259 = t44 * t110
      t261 = x1 * t44
      t273 = t6 * x3
      t278 = t23 * t228
      t283 = -t256 + t257 - t258 + 0.2D1 * t259 + t261 + t53 - 0.4D1 * t
     #160 * t53 + 0.6D1 * t160 * t257 + 0.8D1 * t139 * t53 - 0.4D1 * t11
     #0 * t6 * t54 - 0.4D1 * t258 * t18 - 0.2D1 * t256 * t273 + 0.2D1 * 
     #t258 * t273 + 0.4D1 * t175 * t278 + 0.4D1 * t235 * t278
      t285 = z * t23 * t228
      t314 = -0.4D1 * t285 - 0.4D1 * t139 * t44 - 0.2D1 * t160 * t44 + 0
     #.2D1 * t258 * x3 + 0.2D1 * t256 * x3 + 0.2D1 * t261 * z - 0.3D1 * 
     #t261 * t6 - 0.4D1 * t259 * z + 0.2D1 * t259 * t6 + 0.2D1 * t258 * 
     #z + t256 * t6 - t258 * t6 - 0.2D1 * t257 * x3 - 0.4D1 * x1 * t23 *
     # t228 - t56 - 0.4D1 * t235 * t285
      t316 = (t283 + t314) ** 2
      t319 = (z - t235 + x1 + t236 - t175) ** 2
      t321 = t207 / t319
      t327 = 0.90D2 * wd * t253 * t316 * t321 + 0.180D3 * t82 * t316 * t
     #321
      t331 = FJET(XB1, XB2, s, t2 * x1 * (-t18 - t160 + t219 + t220 + t2
     #21 - t223 - x2 + t109 + t230) * t177, -t164, -t2 * x1 * t237 * t17
     #7, t169, s * t241 * x2 * x1 * t162 * t177, t327 * t135 * t98 / 0.1
     #60D3)
      rrgg2gght12s2e1 = t158 * t157 + t217 * t216 + t331 * t327 * t135 *
     # t98 / 0.160D3

      end function



      doubleprecision function rrgg2gght12s2e0
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
      t4 = -0.1D1 + x3
      t6 = x4 * pi
      t7 = Sin(t6)
      t8 = t7 ** 2
      t11 = z ** 2
      t13 = 0.1D1 / t11 / z
      t15 = sqrt(x2)
      t16 = -0.1D1 + t15
      t17 = t15 + 0.1D1
      t22 = log(0.4D1 * x2 * t8 * x3 * t4 * t13 * t16 * t17)
      t23 = t15 * z
      t24 = x3 * t15
      t26 = 0.2D1 * t24 * z
      t27 = x3 * t16
      t31 = Sqrt(t27 * t17 * z * t4)
      t32 = cos(t6)
      t37 = (-t23 + t26 + 0.4D1 * t31 * t32 - t15 + 0.2D1 * t24) ** 2
      t44 = log(-0.4D1 * x2 * t13 * t8 * x3 * t4)
      t45 = x3 * z
      t47 = Sqrt(-t45 * t4)
      t48 = t47 ** 2
      t50 = t32 ** 2
      t59 = -t37 + 0.16D2 * t48 * t50
      t63 = 0.1D1 / x2
      t67 = 0.1D1 / x1
      t68 = t67 * t63
      t71 = t50 * wd
      t72 = x1 ** 2
      t73 = x3 * t72
      t78 = log(-0.4D1 * t73 * t8 * t13 * t4)
      t92 = log(-0.4D1 * x3 * t13 * t8 * t4)
      t94 = t71 * lh
      t97 = t92 ** 2
      t101 = pi ** 2
      t103 = lh ** 2
      t109 = (0.90D2 * wd * (t22 * t37 - 0.16D2 * t44 * t48 * t50) - 0.1
     #80D3 * wd * lh * t59) * t63 / 0.320D3 + 0.9D1 / 0.16D2 * wd * t59 
     #* t68 + (-0.1440D4 * t71 * t78 * t48 - 0.2880D4 * t71 * lh * t48) 
     #* t67 / 0.160D3 + 0.9D1 * t92 * t48 * t94 + 0.9D1 / 0.4D1 * t97 * 
     #t48 * t71 + t71 * (-0.30D2 * t101 + 0.180D3 * t103) * t48 / 0.20D2
      t110 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #109)
      t112 = x3 * x1
      t114 = -0.1D1 + x1
      t116 = t2 * t114 * x3
      t117 = t4 * s
      t121 = t117 * t1 * t114
      t122 = x1 * z
      t123 = -z - x1 + t122
      t126 = Sqrt(x3 * t123 * t4)
      t127 = t126 ** 2
      t129 = 0.1D1 / t123
      t137 = t114 ** 2
      t142 = log(0.4D1 * t73 * t8 / t11 * t129 * t137 * t4)
      t145 = t127 * z * t129
      t153 = 0.9D1 * t71 * t127 * z * t129 * t68 + (-0.1440D4 * t71 * t1
     #42 * t145 - 0.2880D4 * t94 * t145) * t67 / 0.160D3
      t154 = FJET(XB1, XB2, s, t2 * t112, -t116, -t117 * t1 * x1, t121, 
     #0.0D0, t153)
      t156 = t112 * z
      t157 = x2 * x3
      t158 = t157 * z
      t159 = t112 * x2
      t160 = x2 * z
      t161 = t112 * t160
      t166 = Sqrt(-t27 * t17 * t123 * t4)
      t168 = 0.2D1 * t32 * t15 * t166
      t173 = x1 * x2
      t174 = t173 * z
      t175 = z + x1 - t122 - t160 - t173 + t174 - t45 - t112 + t156 + t1
     #58 + t159 - t161 + t157 + t168
      t179 = t1 ** 2
      t186 = z * t32 * t166
      t189 = x1 * t15
      t190 = t15 * x2
      t191 = x1 * t190
      t192 = t15 * t11
      t193 = t72 * t190
      t194 = t15 * t72
      t211 = -0.4D1 * t173 * t186 + t189 - t191 + t192 - t193 + 0.2D1 * 
     #t194 - 0.4D1 * t186 - 0.4D1 * t73 * t15 - 0.2D1 * t112 * t15 + 0.2
     #D1 * t193 * x3 + 0.2D1 * t191 * x3 + 0.2D1 * t189 * z + t23 + 0.6D
     #1 * t112 * t192 + 0.8D1 * t73 * t23
      t217 = t11 * x3
      t222 = t32 * t166
      t244 = -0.4D1 * t72 * t11 * t24 - 0.4D1 * t193 * t45 - 0.2D1 * t19
     #1 * t217 + 0.2D1 * t193 * t217 + 0.4D1 * t122 * t222 + 0.4D1 * t17
     #3 * t222 - 0.4D1 * t112 * t23 - t26 - 0.3D1 * t189 * t11 - 0.4D1 *
     # t194 * z + 0.2D1 * t194 * t11 + 0.2D1 * t193 * z + t191 * t11 - t
     #193 * t11 - 0.2D1 * t192 * x3 - 0.4D1 * x1 * t32 * t166
      t246 = (t211 + t244) ** 2
      t250 = (z - t173 + x1 + t174 - t122) ** 2
      t253 = t129 / t250 * t68
      t256 = FJET(XB1, XB2, s, t2 * x1 * (-t45 - t112 + t156 + t158 + t1
     #59 - t161 - x2 + t157 + t168) * t129, -t116, -t2 * x1 * t175 * t12
     #9, t121, s * t179 * x2 * x1 * t114 * t129, -0.9D1 / 0.16D2 * wd * 
     #t246 * z * t253)
      rrgg2gght12s2e0 = t110 * t109 + t154 * t153 - 0.9D1 / 0.16D2 * t25
     #6 * wd * t246 * z * t253

      end function



      doubleprecision function rrgg2gght12s2em1
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
      t4 = -0.1D1 + x3
      t8 = Sqrt(-x3 * z * t4)
      t9 = t8 ** 2
      t10 = x4 * pi
      t11 = cos(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = 0.1D1 / x1
      t18 = t12 * wd
      t22 = z ** 2
      t26 = Sin(t10)
      t27 = t26 ** 2
      t31 = log(-0.4D1 * x3 / t22 / z * t27 * t4)
      t35 = sqrt(x2)
      t37 = x3 * t35
      t46 = Sqrt(x3 * (-0.1D1 + t35) * (t35 + 0.1D1) * z * t4)
      t51 = (-t35 * z + 0.2D1 * t37 * z + 0.4D1 * t46 * t11 - t35 + 0.2D
     #1 * t37) ** 2
      t58 = 0.9D1 * t13 * wd * t14 - 0.9D1 * t18 * lh * t9 - 0.9D1 / 0.2
     #D1 * t31 * t9 * t18 + 0.9D1 / 0.32D2 * wd * (-t51 + 0.16D2 * t13) 
     #/ x2
      t59 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t5
     #8)
      t63 = -0.1D1 + x1
      t66 = t4 * s
      t72 = -z - x1 + x1 * z
      t75 = Sqrt(x3 * t72 * t4)
      t76 = t75 ** 2
      t78 = 0.1D1 / t72
      t83 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t63 * x3, -t66 * t1 * 
     #x1, t66 * t1 * t63, 0.0D0, 0.9D1 * t18 * t76 * z * t78 * t14)
      rrgg2gght12s2em1 = t59 * t58 + 0.9D1 * t83 * t12 * wd * t76 * z * 
     #t78 * t14

      end function



      doubleprecision function rrgg2gght12s2em2
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
      t2 = s * (-0.1D1 + z)
      t4 = -0.1D1 + x3
      t8 = Sqrt(-x3 * z * t4)
      t9 = t8 ** 2
      t11 = cos(x4 * pi)
      t12 = t11 ** 2
      t16 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.
     #9D1 / 0.2D1 * t9 * t12 * wd)
      rrgg2gght12s2em2 = 0.9D1 / 0.2D1 * t16 * t9 * t12 * wd

      end function



      doubleprecision function rrgg2gght12s2em3
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
      rrgg2gght12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght12s2em4
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
      rrgg2gght12s2em4 = 0.0D0

      end function
