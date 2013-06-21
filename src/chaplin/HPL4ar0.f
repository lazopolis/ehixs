      double complex function HPL4ar0(n1,n2,n3,n4,x)
      implicit none
      integer n1,n2,n3,n4,j,bcflag
      double complex x,ris,myi,cli4pt5,cli4,llx
      double precision pi, zeta2, zeta3,zeta4,ll2,xre

      pi=3.1415926535897932385D0
      zeta3=1.20205690315959428539973816151d0
      zeta2=pi**2/6d0
      zeta4=pi**4/90d0
      myi = dcmplx(0d0,1d0)
      bcflag = 0

      ll2 = dlog(2d0)
      cli4pt5 = cli4(dcmplx(0.5d0,0d0))

      j=1+(n4+1)*1+(n3+1)*3+(n2+1)*9+(n1+1)*27
      ris = dcmplx(0d0,0d0)

c---  +i*epsilon to get branch cuts right ---
      if (dimag(x).eq.0d0) then
         x = x + dcmplx(0d0,1d-60)
         bcflag = 1
      endif
c---  
      select case (j)
c This was file contains the Taylor 
c expansions around x = 0

         case(1)            !-1-1-1-1


            ris = (x**4)/24d0 - (x**5)/12d0 + (17d0*x**
     &6)/144d0 - (7d0*x**7)/48d0 + (967d0*x**8)/5760d0 - (89d
     &0*x**9)/480d0 + (4523d0*x**10)/22680d0

         case(2)            !-1-1-10

            llx = log(x)

            ris = -((11d0*x**3)/36d0) + (19d0*x**4)/48d
     &0 - (599d0*x**5)/1440d0 + (79d0*x**6)/192d0 - (3343d0*x
     &**7)/8400d0 + (21977d0*x**8)/57600d0 - (83359739d0*x**9
     &)/228614400d0 + (3538531d0*x**10)/10160640d0 + (x**3*ll
     &x)/6d0 - (x**4*llx)/4d0 + (7d0*x**5*llx)/24d0 - (5d0*x*
     &*6*llx)/16d0 + (29d0*x**7*llx)/90d0 - (469d0*x**8*llx)/
     &1440d0 + (29531d0*x**9*llx)/90720d0 - (1303d0*x**10*llx
     &)/4032d0

         case(3)            !-1-1-11


            ris = (x**4)/24d0 - (x**5)/15d0 + (61d0*x**
     &6)/720d0 - (97d0*x**7)/1008d0 + (467d0*x**8)/4480d0 - (
     &9931d0*x**9)/90720d0 + (1831d0*x**10)/16200d0

         case(4)            !-1-10-1


            ris = (x**3)/6d0 - (11d0*x**4)/48d0 + (181d
     &0*x**5)/720d0 - (37d0*x**6)/144d0 + (38569d0*x**7)/1512
     &00d0 - (43171d0*x**8)/172800d0 + (9261559d0*x**9)/38102
     &400d0 - (1197607d0*x**10)/5080320d0

         case(5)            !-1-100

            llx = log(x)

            ris = (7d0*x**2)/8d0 - (41d0*x**3)/72d0 + (
     &1397d0*x**4)/3456d0 - (2671d0*x**5)/8640d0 + (322493d0*
     &x**6)/1296000d0 - (104641d0*x**7)/504000d0 + (140539517
     &d0*x**8)/790272000d0 - (2486560891d0*x**9)/16003008000d
     &0 + (22064922487d0*x**10)/160030080000d0 - (3d0*x**2*ll
     &x)/4d0 + (7d0*x**3*llx)/12d0 - (131d0*x**4*llx)/288d0 +
     & (53d0*x**5*llx)/144d0 - (2213d0*x**6*llx)/7200d0 + (94
     &7d0*x**7*llx)/3600d0 - (647707d0*x**8*llx)/2822400d0 + 
     &(1290829d0*x**9*llx)/6350400d0 - (11574649d0*x**10*llx)
     &/63504000d0 + (x**2*llx**2)/4d0 - (x**3*llx**2)/4d0 + (
     &11d0*x**4*llx**2)/48d0 - (5d0*x**5*llx**2)/24d0 + (137d
     &0*x**6*llx**2)/720d0 - (7d0*x**7*llx**2)/40d0 + (363d0*
     &x**8*llx**2)/2240d0 - (761d0*x**9*llx**2)/5040d0 + (712
     &9d0*x**10*llx**2)/50400d0

         case(6)            !-1-101


            ris = (x**3)/6d0 - (3d0*x**4)/16d0 + (139d0
     &*x**5)/720d0 - (3d0*x**6)/16d0 + (27319d0*x**7)/151200d
     &0 - (29821d0*x**8)/172800d0 + (6284809d0*x**9)/38102400
     &d0 - (88913d0*x**10)/564480d0

         case(7)            !-1-11-1


            ris = (x**4)/24d0 - (x**5)/20d0 + (43d0*x**
     &6)/720d0 - (103d0*x**7)/1680d0 + (2563d0*x**8)/40320d0 
     &- (127d0*x**9)/2016d0 + (3569d0*x**10)/56700d0

         case(8)            !-1-110

            llx = log(x)

            ris = -((11d0*x**3)/36d0) + (11d0*x**4)/36d
     &0 - (719d0*x**5)/2400d0 + (1337d0*x**6)/4800d0 - (46061
     &d0*x**7)/176400d0 + (27479d0*x**8)/112896d0 - (52243973
     &d0*x**9)/228614400d0 + (163666943d0*x**10)/762048000d0 
     &+ (x**3*llx)/6d0 - (x**4*llx)/6d0 + (7d0*x**5*llx)/40d0
     & - (119d0*x**6*llx)/720d0 + (101d0*x**7*llx)/630d0 - (3
     &05d0*x**8*llx)/2016d0 + (13157d0*x**9*llx)/90720d0 - (4
     &1603d0*x**10*llx)/302400d0

         case(9)            !-1-111


            ris = (x**4)/24d0 - (x**5)/30d0 + (31d0*x**
     &6)/720d0 - (191d0*x**7)/5040d0 + (547d0*x**8)/13440d0 -
     & (3373d0*x**9)/90720d0 + (2147d0*x**10)/56700d0

         case(10)            !-10-1-1


            ris = (x**3)/12d0 - (5d0*x**4)/48d0 + (17d0
     &*x**5)/160d0 - (59d0*x**6)/576d0 + (2929d0*x**7)/30240d
     &0 - (629d0*x**8)/6912d0 + (185921d0*x**9)/2177280d0 - (
     &116423d0*x**10)/1451520d0

         case(11)            !-10-10

            llx = log(x)

            ris = -((5d0*x**2)/4d0) + (8d0*x**3)/9d0 - 
     &(1151d0*x**4)/1728d0 + (2281d0*x**5)/4320d0 - (17653d0*
     &x**6)/40500d0 + (93371d0*x**7)/252000d0 - (127203607d0*
     &x**8)/395136000d0 + (2276013631d0*x**9)/8001504000d0 - 
     &(4076031341d0*x**10)/16003008000d0 + (x**2*llx)/2d0 - (
     &5d0*x**3*llx)/12d0 + (49d0*x**4*llx)/144d0 - (41d0*x**5
     &*llx)/144d0 + (5269d0*x**6*llx)/21600d0 - (767d0*x**7*l
     &lx)/3600d0 + (266681d0*x**8*llx)/1411200d0 - (1077749d0
     &*x**9*llx)/6350400d0 + (9778141d0*x**10*llx)/63504000d0

         case(12)            !-10-11


            ris = (x**3)/12d0 - (11d0*x**4)/144d0 + (10
     &3d0*x**5)/1440d0 - (2743d0*x**6)/43200d0 + (8699d0*x**7
     &)/151200d0 - (439571d0*x**8)/8467200d0 + (3617053d0*x**
     &9)/76204800d0 - (33150437d0*x**10)/762048000d0

         case(13)            !-100-1


            ris = (x**2)/2d0 - (3d0*x**3)/8d0 + (251d0*
     &x**4)/864d0 - (407d0*x**5)/1728d0 + (256103d0*x**6)/129
     &6000d0 - (4081d0*x**7)/24000d0 + (9822481d0*x**8)/65856
     &000d0 - (78708473d0*x**9)/592704000d0 + (19148110939d0*
     &x**10)/160030080000d0

         case(14)            !-1000

            llx = log(x)

            ris = -x + (x**2)/16d0 - (x**3)/81d0 + (x**
     &4)/256d0 - (x**5)/625d0 + (x**6)/1296d0 - (x**7)/2401d0
     & + (x**8)/4096d0 - (x**9)/6561d0 + (x**10)/10000d0 + x*
     &llx - (x**2*llx)/8d0 + (x**3*llx)/27d0 - (x**4*llx)/64d
     &0 + (x**5*llx)/125d0 - (x**6*llx)/216d0 + (x**7*llx)/34
     &3d0 - (x**8*llx)/512d0 + (x**9*llx)/729d0 - (x**10*llx)
     &/1000d0 - (x*llx**2)/2d0 + (x**2*llx**2)/8d0 - (x**3*ll
     &x**2)/18d0 + (x**4*llx**2)/32d0 - (x**5*llx**2)/50d0 + 
     &(x**6*llx**2)/72d0 - (x**7*llx**2)/98d0 + (x**8*llx**2)
     &/128d0 - (x**9*llx**2)/162d0 + (x**10*llx**2)/200d0 + (
     &x*llx**3)/6d0 - (x**2*llx**3)/12d0 + (x**3*llx**3)/18d0
     & - (x**4*llx**3)/24d0 + (x**5*llx**3)/30d0 - (x**6*llx*
     &*3)/36d0 + (x**7*llx**3)/42d0 - (x**8*llx**3)/48d0 + (x
     &**9*llx**3)/54d0 - (x**10*llx**3)/60d0

         case(15)            !-1001


            ris = (x**2)/2d0 - (7d0*x**3)/24d0 + (197d0
     &*x**4)/864d0 - (1549d0*x**5)/8640d0 + (195353d0*x**6)/1
     &296000d0 - (194353d0*x**7)/1512000d0 + (66879079d0*x**8
     &)/592704000d0 - (533875007d0*x**9)/5334336000d0 + (1443
     &6577189d0*x**10)/160030080000d0

         case(16)            !-101-1


            ris = (x**3)/12d0 - (7d0*x**4)/144d0 + (71d
     &0*x**5)/1440d0 - (1607d0*x**6)/43200d0 + (5291d0*x**7)/
     &151200d0 - (245939d0*x**8)/8467200d0 + (2067997d0*x**9)
     &/76204800d0 - (18015013d0*x**10)/762048000d0

         case(17)            !-1010

            llx = log(x)

            ris = -((5d0*x**2)/4d0) + (2d0*x**3)/3d0 - 
     &(881d0*x**4)/1728d0 + (1687d0*x**5)/4320d0 - (13153d0*x
     &**6)/40500d0 + (206863d0*x**7)/756000d0 - (282912571d0*
     &x**8)/1185408000d0 + (560731627d0*x**9)/2667168000d0 - 
     &(3019814291d0*x**10)/16003008000d0 + (x**2*llx)/2d0 - (
     &x**3*llx)/4d0 + (31d0*x**4*llx)/144d0 - (23d0*x**5*llx)
     &/144d0 + (3019d0*x**6*llx)/21600d0 - (139d0*x**7*llx)/1
     &200d0 + (48877d0*x**8*llx)/470400d0 - (191833d0*x**9*ll
     &x)/2116800d0 + (5257891d0*x**10*llx)/63504000d0

         case(18)            !-1011


            ris = (x**3)/12d0 - (x**4)/48d0 + (19d0*x**
     &5)/480d0 - (11d0*x**6)/576d0 + (769d0*x**7)/30240d0 - (
     &553d0*x**8)/34560d0 + (40769d0*x**9)/2177280d0 - (19591
     &d0*x**10)/1451520d0

         case(19)            !-11-1-1


            ris = (x**4)/24d0 - (x**5)/30d0 + (31d0*x**
     &6)/720d0 - (181d0*x**7)/5040d0 + (1571d0*x**8)/40320d0 
     &- (113d0*x**9)/3360d0 + (15727d0*x**10)/453600d0

         case(20)            !-11-10

            llx = log(x)

            ris = -((11d0*x**3)/36d0) + (19d0*x**4)/144
     &d0 - (1181d0*x**5)/7200d0 + (1439d0*x**6)/14400d0 - (43
     &1d0*x**7)/3920d0 + (218839d0*x**8)/2822400d0 - (1880026
     &1d0*x**9)/228614400d0 + (54725d0*x**10)/870912d0 + (x**
     &3*llx)/6d0 - (x**4*llx)/12d0 + (13d0*x**5*llx)/120d0 - 
     &(17d0*x**6*llx)/240d0 + (5d0*x**7*llx)/63d0 - (589d0*x*
     &*8*llx)/10080d0 + (5669d0*x**9*llx)/90720d0 - (85d0*x**
     &10*llx)/1728d0

         case(21)            !-11-11


            ris = (x**4)/24d0 - (x**5)/60d0 + (23d0*x**
     &6)/720d0 - (29d0*x**7)/1680d0 + (1009d0*x**8)/40320d0 -
     & (1429d0*x**9)/90720d0 + (1853d0*x**10)/90720d0

         case(22)            !-110-1


            ris = (x**3)/6d0 - (x**4)/16d0 + (67d0*x**5
     &)/720d0 - (11d0*x**6)/216d0 + (9619d0*x**7)/151200d0 - 
     &(7117d0*x**8)/172800d0 + (73393d0*x**9)/1524096d0 - (14
     &51159d0*x**10)/42336000d0

         case(23)            !-1100

            llx = log(x)

            ris = (7d0*x**2)/8d0 - (85d0*x**3)/216d0 + 
     &(1019d0*x**4)/3456d0 - (46633d0*x**5)/216000d0 + (23024
     &3d0*x**6)/1296000d0 - (10882477d0*x**7)/74088000d0 + (3
     &01825801d0*x**8)/2370816000d0 - (5330081423d0*x**9)/480
     &09024000d0 + (15880889737d0*x**10)/160030080000d0 - (3d
     &0*x**2*llx)/4d0 + (11d0*x**3*llx)/36d0 - (77d0*x**4*llx
     &)/288d0 + (659d0*x**5*llx)/3600d0 - (1163d0*x**6*llx)/7
     &200d0 + (2517d0*x**7*llx)/19600d0 - (108919d0*x**8*llx)
     &/940800d0 + (1875737d0*x**9*llx)/19051200d0 - (5731399d
     &0*x**10*llx)/63504000d0 + (x**2*llx**2)/4d0 - (x**3*llx
     &**2)/12d0 + (5d0*x**4*llx**2)/48d0 - (7d0*x**5*llx**2)/
     &120d0 + (47d0*x**6*llx**2)/720d0 - (37d0*x**7*llx**2)/8
     &40d0 + (319d0*x**8*llx**2)/6720d0 - (533d0*x**9*llx**2)
     &/15120d0 + (1879d0*x**10*llx**2)/50400d0

         case(24)            !-1101


            ris = (x**3)/6d0 - (x**4)/48d0 + (61d0*x**5
     &)/720d0 - (5d0*x**6)/216d0 + (8269d0*x**7)/151200d0 - (
     &3667d0*x**8)/172800d0 + (60751d0*x**9)/1524096d0 - (240
     &0827d0*x**10)/127008000d0

         case(25)            !-111-1


            ris = (x**4)/24d0 + (7d0*x**6)/240d0 - (x**
     &7)/720d0 + (857d0*x**8)/40320d0 - (x**9)/480d0 + (7429d
     &0*x**10)/453600d0

         case(26)            !-1110

            llx = log(x)

            ris = -((11d0*x**3)/36d0) + (x**4)/24d0 - (
     &1027d0*x**5)/7200d0 + (25d0*x**6)/576d0 - (15653d0*x**7
     &)/176400d0 + (2209d0*x**8)/57600d0 - (2902399d0*x**9)/4
     &5722880d0 + (8469731d0*x**10)/254016000d0 + (x**3*llx)/
     &6d0 + (11d0*x**5*llx)/120d0 - (x**6*llx)/144d0 + (19d0*
     &x**7*llx)/315d0 - (13d0*x**8*llx)/1440d0 + (799d0*x**9*
     &llx)/18144d0 - (317d0*x**10*llx)/33600d0

         case(27)            !-1111


            ris = (x**4)/24d0 + (x**5)/60d0 + (5d0*x**6
     &)/144d0 + (5d0*x**7)/336d0 + (157d0*x**8)/5760d0 + (31d
     &0*x**9)/2592d0 + (9883d0*x**10)/453600d0

         case(28)            !0-1-1-1


            ris = (x**3)/18d0 - (x**4)/16d0 + (7d0*x**5
     &)/120d0 - (5d0*x**6)/96d0 + (29d0*x**7)/630d0 - (469d0*
     &x**8)/11520d0 + (29531d0*x**9)/816480d0 - (1303d0*x**10
     &)/40320d0

         case(29)            !0-1-10

            llx = log(x)

            ris = -((x**2)/2d0) + (x**3)/4d0 - (41d0*x*
     &*4)/288d0 + (13d0*x**5)/144d0 - (8009d0*x**6)/129600d0 
     &+ (161d0*x**7)/3600d0 - (190513d0*x**8)/5644800d0 + (16
     &7101d0*x**9)/6350400d0 - (13371157d0*x**10)/635040000d0
     & + (x**2*llx)/4d0 - (x**3*llx)/6d0 + (11d0*x**4*llx)/96
     &d0 - (x**5*llx)/12d0 + (137d0*x**6*llx)/2160d0 - (x**7*
     &llx)/20d0 + (363d0*x**8*llx)/8960d0 - (761d0*x**9*llx)/
     &22680d0 + (7129d0*x**10*llx)/252000d0

         case(30)            !0-1-11


            ris = (x**3)/18d0 - (x**4)/24d0 + (7d0*x**5
     &)/200d0 - (119d0*x**6)/4320d0 + (101d0*x**7)/4410d0 - (
     &305d0*x**8)/16128d0 + (13157d0*x**9)/816480d0 - (41603d
     &0*x**10)/3024000d0

         case(31)            !0-10-1


            ris = (x**2)/4d0 - (5d0*x**3)/36d0 + (49d0*
     &x**4)/576d0 - (41d0*x**5)/720d0 + (5269d0*x**6)/129600d
     &0 - (767d0*x**7)/25200d0 + (266681d0*x**8)/11289600d0 -
     & (1077749d0*x**9)/57153600d0 + (9778141d0*x**10)/635040
     &000d0

         case(32)            !0-100

            llx = log(x)

            ris = 3*x - (3d0*x**2)/16d0 + (x**3)/27d0 -
     & (3d0*x**4)/256d0 + (3d0*x**5)/625d0 - (x**6)/432d0 + (
     &3d0*x**7)/2401d0 - (3d0*x**8)/4096d0 + (x**9)/2187d0 - 
     &(3d0*x**10)/10000d0 - 2*x*llx + (x**2*llx)/4d0 - (2d0*x
     &**3*llx)/27d0 + (x**4*llx)/32d0 - (2d0*x**5*llx)/125d0 
     &+ (x**6*llx)/108d0 - (2d0*x**7*llx)/343d0 + (x**8*llx)/
     &256d0 - (2d0*x**9*llx)/729d0 + (x**10*llx)/500d0 + (x*l
     &lx**2)/2d0 - (x**2*llx**2)/8d0 + (x**3*llx**2)/18d0 - (
     &x**4*llx**2)/32d0 + (x**5*llx**2)/50d0 - (x**6*llx**2)/
     &72d0 + (x**7*llx**2)/98d0 - (x**8*llx**2)/128d0 + (x**9
     &*llx**2)/162d0 - (x**10*llx**2)/200d0

         case(33)            !0-101


            ris = (x**2)/4d0 - (x**3)/12d0 + (31d0*x**4
     &)/576d0 - (23d0*x**5)/720d0 + (3019d0*x**6)/129600d0 - 
     &(139d0*x**7)/8400d0 + (48877d0*x**8)/3763200d0 - (19183
     &3d0*x**9)/19051200d0 + (5257891d0*x**10)/635040000d0

         case(34)            !0-11-1


            ris = (x**3)/18d0 - (x**4)/48d0 + (13d0*x**
     &5)/600d0 - (17d0*x**6)/1440d0 + (5d0*x**7)/441d0 - (589
     &d0*x**8)/80640d0 + (5669d0*x**9)/816480d0 - (17d0*x**10
     &)/3456d0

         case(35)            !0-110

            llx = log(x)

            ris = -((x**2)/2d0) + (13d0*x**3)/108d0 - (
     &23d0*x**4)/288d0 + (743d0*x**5)/18000d0 - (3959d0*x**6)
     &/129600d0 + (8291d0*x**7)/411600d0 - (10007d0*x**8)/627
     &200d0 + (2024977d0*x**9)/171460800d0 - (6204907d0*x**10
     &)/635040000d0 + (x**2*llx)/4d0 - (x**3*llx)/18d0 + (5d0
     &*x**4*llx)/96d0 - (7d0*x**5*llx)/300d0 + (47d0*x**6*llx
     &)/2160d0 - (37d0*x**7*llx)/2940d0 + (319d0*x**8*llx)/26
     &880d0 - (533d0*x**9*llx)/68040d0 + (1879d0*x**10*llx)/2
     &52000d0

         case(36)            !0-111


            ris = (x**3)/18d0 + (11d0*x**5)/600d0 - (x*
     &*6)/864d0 + (19d0*x**7)/2205d0 - (13d0*x**8)/11520d0 + 
     &(799d0*x**9)/163296d0 - (317d0*x**10)/336000d0

         case(37)            !00-1-1


            ris = (x**2)/8d0 - (x**3)/18d0 + (11d0*x**4
     &)/384d0 - (x**5)/60d0 + (137d0*x**6)/12960d0 - (x**7)/1
     &40d0 + (363d0*x**8)/71680d0 - (761d0*x**9)/204120d0 + (
     &7129d0*x**10)/2520000d0

         case(38)            !00-10

            llx = log(x)

            ris = -3*x + (3d0*x**2)/16d0 - (x**3)/27d0 
     &+ (3d0*x**4)/256d0 - (3d0*x**5)/625d0 + (x**6)/432d0 - 
     &(3d0*x**7)/2401d0 + (3d0*x**8)/4096d0 - (x**9)/2187d0 +
     & (3d0*x**10)/10000d0 + x*llx - (x**2*llx)/8d0 + (x**3*l
     &lx)/27d0 - (x**4*llx)/64d0 + (x**5*llx)/125d0 - (x**6*l
     &lx)/216d0 + (x**7*llx)/343d0 - (x**8*llx)/512d0 + (x**9
     &*llx)/729d0 - (x**10*llx)/1000d0

         case(39)            !00-11


            ris = (x**2)/8d0 - (x**3)/54d0 + (5d0*x**4)
     &/384d0 - (7d0*x**5)/1500d0 + (47d0*x**6)/12960d0 - (37d
     &0*x**7)/20580d0 + (319d0*x**8)/215040d0 - (533d0*x**9)/
     &612360d0 + (1879d0*x**10)/2520000d0

         case(40)            !000-1


            ris = x - (x**2)/16d0 + (x**3)/81d0 - (x**4
     &)/256d0 + (x**5)/625d0 - (x**6)/1296d0 + (x**7)/2401d0 
     &- (x**8)/4096d0 + (x**9)/6561d0 - (x**10)/10000d0

         case(41)            !0000

            llx = log(x)

            ris = (llx**4)/24d0

         case(42)            !0001


            ris = x + (x**2)/16d0 + (x**3)/81d0 + (x**4
     &)/256d0 + (x**5)/625d0 + (x**6)/1296d0 + (x**7)/2401d0 
     &+ (x**8)/4096d0 + (x**9)/6561d0 + (x**10)/10000d0

         case(43)            !001-1


            ris = (x**2)/8d0 + (x**3)/54d0 + (5d0*x**4)
     &/384d0 + (7d0*x**5)/1500d0 + (47d0*x**6)/12960d0 + (37d
     &0*x**7)/20580d0 + (319d0*x**8)/215040d0 + (533d0*x**9)/
     &612360d0 + (1879d0*x**10)/2520000d0

         case(44)            !0010

            llx = log(x)

            ris = -3*x - (3d0*x**2)/16d0 - (x**3)/27d0 
     &- (3d0*x**4)/256d0 - (3d0*x**5)/625d0 - (x**6)/432d0 - 
     &(3d0*x**7)/2401d0 - (3d0*x**8)/4096d0 - (x**9)/2187d0 -
     & (3d0*x**10)/10000d0 + x*llx + (x**2*llx)/8d0 + (x**3*l
     &lx)/27d0 + (x**4*llx)/64d0 + (x**5*llx)/125d0 + (x**6*l
     &lx)/216d0 + (x**7*llx)/343d0 + (x**8*llx)/512d0 + (x**9
     &*llx)/729d0 + (x**10*llx)/1000d0

         case(45)            !0011


            ris = (x**2)/8d0 + (x**3)/18d0 + (11d0*x**4
     &)/384d0 + (x**5)/60d0 + (137d0*x**6)/12960d0 + (x**7)/1
     &40d0 + (363d0*x**8)/71680d0 + (761d0*x**9)/204120d0 + (
     &7129d0*x**10)/2520000d0

         case(46)            !01-1-1


            ris = (x**3)/18d0 + (11d0*x**5)/600d0 + (x*
     &*6)/864d0 + (19d0*x**7)/2205d0 + (13d0*x**8)/11520d0 + 
     &(799d0*x**9)/163296d0 + (317d0*x**10)/336000d0

         case(47)            !01-10

            llx = log(x)

            ris = -((x**2)/2d0) - (13d0*x**3)/108d0 - (
     &23d0*x**4)/288d0 - (743d0*x**5)/18000d0 - (3959d0*x**6)
     &/129600d0 - (8291d0*x**7)/411600d0 - (10007d0*x**8)/627
     &200d0 - (2024977d0*x**9)/171460800d0 - (6204907d0*x**10
     &)/635040000d0 + (x**2*llx)/4d0 + (x**3*llx)/18d0 + (5d0
     &*x**4*llx)/96d0 + (7d0*x**5*llx)/300d0 + (47d0*x**6*llx
     &)/2160d0 + (37d0*x**7*llx)/2940d0 + (319d0*x**8*llx)/26
     &880d0 + (533d0*x**9*llx)/68040d0 + (1879d0*x**10*llx)/2
     &52000d0

         case(48)            !01-11


            ris = (x**3)/18d0 + (x**4)/48d0 + (13d0*x**
     &5)/600d0 + (17d0*x**6)/1440d0 + (5d0*x**7)/441d0 + (589
     &d0*x**8)/80640d0 + (5669d0*x**9)/816480d0 + (17d0*x**10
     &)/3456d0

         case(49)            !010-1


            ris = (x**2)/4d0 + (x**3)/12d0 + (31d0*x**4
     &)/576d0 + (23d0*x**5)/720d0 + (3019d0*x**6)/129600d0 + 
     &(139d0*x**7)/8400d0 + (48877d0*x**8)/3763200d0 + (19183
     &3d0*x**9)/19051200d0 + (5257891d0*x**10)/635040000d0

         case(50)            !0100

            llx = log(x)

            ris = 3*x + (3d0*x**2)/16d0 + (x**3)/27d0 +
     & (3d0*x**4)/256d0 + (3d0*x**5)/625d0 + (x**6)/432d0 + (
     &3d0*x**7)/2401d0 + (3d0*x**8)/4096d0 + (x**9)/2187d0 + 
     &(3d0*x**10)/10000d0 - 2*x*llx - (x**2*llx)/4d0 - (2d0*x
     &**3*llx)/27d0 - (x**4*llx)/32d0 - (2d0*x**5*llx)/125d0 
     &- (x**6*llx)/108d0 - (2d0*x**7*llx)/343d0 - (x**8*llx)/
     &256d0 - (2d0*x**9*llx)/729d0 - (x**10*llx)/500d0 + (x*l
     &lx**2)/2d0 + (x**2*llx**2)/8d0 + (x**3*llx**2)/18d0 + (
     &x**4*llx**2)/32d0 + (x**5*llx**2)/50d0 + (x**6*llx**2)/
     &72d0 + (x**7*llx**2)/98d0 + (x**8*llx**2)/128d0 + (x**9
     &*llx**2)/162d0 + (x**10*llx**2)/200d0

         case(51)            !0101


            ris = (x**2)/4d0 + (5d0*x**3)/36d0 + (49d0*
     &x**4)/576d0 + (41d0*x**5)/720d0 + (5269d0*x**6)/129600d
     &0 + (767d0*x**7)/25200d0 + (266681d0*x**8)/11289600d0 +
     & (1077749d0*x**9)/57153600d0 + (9778141d0*x**10)/635040
     &000d0

         case(52)            !011-1


            ris = (x**3)/18d0 + (x**4)/24d0 + (7d0*x**5
     &)/200d0 + (119d0*x**6)/4320d0 + (101d0*x**7)/4410d0 + (
     &305d0*x**8)/16128d0 + (13157d0*x**9)/816480d0 + (41603d
     &0*x**10)/3024000d0

         case(53)            !0110

            llx = log(x)

            ris = -((x**2)/2d0) - (x**3)/4d0 - (41d0*x*
     &*4)/288d0 - (13d0*x**5)/144d0 - (8009d0*x**6)/129600d0 
     &- (161d0*x**7)/3600d0 - (190513d0*x**8)/5644800d0 - (16
     &7101d0*x**9)/6350400d0 - (13371157d0*x**10)/635040000d0
     & + (x**2*llx)/4d0 + (x**3*llx)/6d0 + (11d0*x**4*llx)/96
     &d0 + (x**5*llx)/12d0 + (137d0*x**6*llx)/2160d0 + (x**7*
     &llx)/20d0 + (363d0*x**8*llx)/8960d0 + (761d0*x**9*llx)/
     &22680d0 + (7129d0*x**10*llx)/252000d0

         case(54)            !0111


            ris = (x**3)/18d0 + (x**4)/16d0 + (7d0*x**5
     &)/120d0 + (5d0*x**6)/96d0 + (29d0*x**7)/630d0 + (469d0*
     &x**8)/11520d0 + (29531d0*x**9)/816480d0 + (1303d0*x**10
     &)/40320d0

         case(55)            !1-1-1-1


            ris = (x**4)/24d0 - (x**5)/60d0 + (5d0*x**6
     &)/144d0 - (5d0*x**7)/336d0 + (157d0*x**8)/5760d0 - (31d
     &0*x**9)/2592d0 + (9883d0*x**10)/453600d0

         case(56)            !1-1-10

            llx = log(x)

            ris = -((11d0*x**3)/36d0) - (x**4)/24d0 - (
     &1027d0*x**5)/7200d0 - (25d0*x**6)/576d0 - (15653d0*x**7
     &)/176400d0 - (2209d0*x**8)/57600d0 - (2902399d0*x**9)/4
     &5722880d0 - (8469731d0*x**10)/254016000d0 + (x**3*llx)/
     &6d0 + (11d0*x**5*llx)/120d0 + (x**6*llx)/144d0 + (19d0*
     &x**7*llx)/315d0 + (13d0*x**8*llx)/1440d0 + (799d0*x**9*
     &llx)/18144d0 + (317d0*x**10*llx)/33600d0

         case(57)            !1-1-11


            ris = (x**4)/24d0 + (7d0*x**6)/240d0 + (x**
     &7)/720d0 + (857d0*x**8)/40320d0 + (x**9)/480d0 + (7429d
     &0*x**10)/453600d0

         case(58)            !1-10-1


            ris = (x**3)/6d0 + (x**4)/48d0 + (61d0*x**5
     &)/720d0 + (5d0*x**6)/216d0 + (8269d0*x**7)/151200d0 + (
     &3667d0*x**8)/172800d0 + (60751d0*x**9)/1524096d0 + (240
     &0827d0*x**10)/127008000d0

         case(59)            !1-100

            llx = log(x)

            ris = (7d0*x**2)/8d0 + (85d0*x**3)/216d0 + 
     &(1019d0*x**4)/3456d0 + (46633d0*x**5)/216000d0 + (23024
     &3d0*x**6)/1296000d0 + (10882477d0*x**7)/74088000d0 + (3
     &01825801d0*x**8)/2370816000d0 + (5330081423d0*x**9)/480
     &09024000d0 + (15880889737d0*x**10)/160030080000d0 - (3d
     &0*x**2*llx)/4d0 - (11d0*x**3*llx)/36d0 - (77d0*x**4*llx
     &)/288d0 - (659d0*x**5*llx)/3600d0 - (1163d0*x**6*llx)/7
     &200d0 - (2517d0*x**7*llx)/19600d0 - (108919d0*x**8*llx)
     &/940800d0 - (1875737d0*x**9*llx)/19051200d0 - (5731399d
     &0*x**10*llx)/63504000d0 + (x**2*llx**2)/4d0 + (x**3*llx
     &**2)/12d0 + (5d0*x**4*llx**2)/48d0 + (7d0*x**5*llx**2)/
     &120d0 + (47d0*x**6*llx**2)/720d0 + (37d0*x**7*llx**2)/8
     &40d0 + (319d0*x**8*llx**2)/6720d0 + (533d0*x**9*llx**2)
     &/15120d0 + (1879d0*x**10*llx**2)/50400d0

         case(60)            !1-101


            ris = (x**3)/6d0 + (x**4)/16d0 + (67d0*x**5
     &)/720d0 + (11d0*x**6)/216d0 + (9619d0*x**7)/151200d0 + 
     &(7117d0*x**8)/172800d0 + (73393d0*x**9)/1524096d0 + (14
     &51159d0*x**10)/42336000d0

         case(61)            !1-11-1


            ris = (x**4)/24d0 + (x**5)/60d0 + (23d0*x**
     &6)/720d0 + (29d0*x**7)/1680d0 + (1009d0*x**8)/40320d0 +
     & (1429d0*x**9)/90720d0 + (1853d0*x**10)/90720d0

         case(62)            !1-110

            llx = log(x)

            ris = -((11d0*x**3)/36d0) - (19d0*x**4)/144
     &d0 - (1181d0*x**5)/7200d0 - (1439d0*x**6)/14400d0 - (43
     &1d0*x**7)/3920d0 - (218839d0*x**8)/2822400d0 - (1880026
     &1d0*x**9)/228614400d0 - (54725d0*x**10)/870912d0 + (x**
     &3*llx)/6d0 + (x**4*llx)/12d0 + (13d0*x**5*llx)/120d0 + 
     &(17d0*x**6*llx)/240d0 + (5d0*x**7*llx)/63d0 + (589d0*x*
     &*8*llx)/10080d0 + (5669d0*x**9*llx)/90720d0 + (85d0*x**
     &10*llx)/1728d0

         case(63)            !1-111


            ris = (x**4)/24d0 + (x**5)/30d0 + (31d0*x**
     &6)/720d0 + (181d0*x**7)/5040d0 + (1571d0*x**8)/40320d0 
     &+ (113d0*x**9)/3360d0 + (15727d0*x**10)/453600d0

         case(64)            !10-1-1


            ris = (x**3)/12d0 + (x**4)/48d0 + (19d0*x**
     &5)/480d0 + (11d0*x**6)/576d0 + (769d0*x**7)/30240d0 + (
     &553d0*x**8)/34560d0 + (40769d0*x**9)/2177280d0 + (19591
     &d0*x**10)/1451520d0

         case(65)            !10-10

            llx = log(x)

            ris = -((5d0*x**2)/4d0) - (2d0*x**3)/3d0 - 
     &(881d0*x**4)/1728d0 - (1687d0*x**5)/4320d0 - (13153d0*x
     &**6)/40500d0 - (206863d0*x**7)/756000d0 - (282912571d0*
     &x**8)/1185408000d0 - (560731627d0*x**9)/2667168000d0 - 
     &(3019814291d0*x**10)/16003008000d0 + (x**2*llx)/2d0 + (
     &x**3*llx)/4d0 + (31d0*x**4*llx)/144d0 + (23d0*x**5*llx)
     &/144d0 + (3019d0*x**6*llx)/21600d0 + (139d0*x**7*llx)/1
     &200d0 + (48877d0*x**8*llx)/470400d0 + (191833d0*x**9*ll
     &x)/2116800d0 + (5257891d0*x**10*llx)/63504000d0

         case(66)            !10-11


            ris = (x**3)/12d0 + (7d0*x**4)/144d0 + (71d
     &0*x**5)/1440d0 + (1607d0*x**6)/43200d0 + (5291d0*x**7)/
     &151200d0 + (245939d0*x**8)/8467200d0 + (2067997d0*x**9)
     &/76204800d0 + (18015013d0*x**10)/762048000d0

         case(67)            !100-1


            ris = (x**2)/2d0 + (7d0*x**3)/24d0 + (197d0
     &*x**4)/864d0 + (1549d0*x**5)/8640d0 + (195353d0*x**6)/1
     &296000d0 + (194353d0*x**7)/1512000d0 + (66879079d0*x**8
     &)/592704000d0 + (533875007d0*x**9)/5334336000d0 + (1443
     &6577189d0*x**10)/160030080000d0

         case(68)            !1000

            llx = log(x)

            ris = -x - (x**2)/16d0 - (x**3)/81d0 - (x**
     &4)/256d0 - (x**5)/625d0 - (x**6)/1296d0 - (x**7)/2401d0
     & - (x**8)/4096d0 - (x**9)/6561d0 - (x**10)/10000d0 + x*
     &llx + (x**2*llx)/8d0 + (x**3*llx)/27d0 + (x**4*llx)/64d
     &0 + (x**5*llx)/125d0 + (x**6*llx)/216d0 + (x**7*llx)/34
     &3d0 + (x**8*llx)/512d0 + (x**9*llx)/729d0 + (x**10*llx)
     &/1000d0 - (x*llx**2)/2d0 - (x**2*llx**2)/8d0 - (x**3*ll
     &x**2)/18d0 - (x**4*llx**2)/32d0 - (x**5*llx**2)/50d0 - 
     &(x**6*llx**2)/72d0 - (x**7*llx**2)/98d0 - (x**8*llx**2)
     &/128d0 - (x**9*llx**2)/162d0 - (x**10*llx**2)/200d0 + (
     &x*llx**3)/6d0 + (x**2*llx**3)/12d0 + (x**3*llx**3)/18d0
     & + (x**4*llx**3)/24d0 + (x**5*llx**3)/30d0 + (x**6*llx*
     &*3)/36d0 + (x**7*llx**3)/42d0 + (x**8*llx**3)/48d0 + (x
     &**9*llx**3)/54d0 + (x**10*llx**3)/60d0

         case(69)            !1001


            ris = (x**2)/2d0 + (3d0*x**3)/8d0 + (251d0*
     &x**4)/864d0 + (407d0*x**5)/1728d0 + (256103d0*x**6)/129
     &6000d0 + (4081d0*x**7)/24000d0 + (9822481d0*x**8)/65856
     &000d0 + (78708473d0*x**9)/592704000d0 + (19148110939d0*
     &x**10)/160030080000d0

         case(70)            !101-1


            ris = (x**3)/12d0 + (11d0*x**4)/144d0 + (10
     &3d0*x**5)/1440d0 + (2743d0*x**6)/43200d0 + (8699d0*x**7
     &)/151200d0 + (439571d0*x**8)/8467200d0 + (3617053d0*x**
     &9)/76204800d0 + (33150437d0*x**10)/762048000d0

         case(71)            !1010

            llx = log(x)

            ris = -((5d0*x**2)/4d0) - (8d0*x**3)/9d0 - 
     &(1151d0*x**4)/1728d0 - (2281d0*x**5)/4320d0 - (17653d0*
     &x**6)/40500d0 - (93371d0*x**7)/252000d0 - (127203607d0*
     &x**8)/395136000d0 - (2276013631d0*x**9)/8001504000d0 - 
     &(4076031341d0*x**10)/16003008000d0 + (x**2*llx)/2d0 + (
     &5d0*x**3*llx)/12d0 + (49d0*x**4*llx)/144d0 + (41d0*x**5
     &*llx)/144d0 + (5269d0*x**6*llx)/21600d0 + (767d0*x**7*l
     &lx)/3600d0 + (266681d0*x**8*llx)/1411200d0 + (1077749d0
     &*x**9*llx)/6350400d0 + (9778141d0*x**10*llx)/63504000d0

         case(72)            !1011


            ris = (x**3)/12d0 + (5d0*x**4)/48d0 + (17d0
     &*x**5)/160d0 + (59d0*x**6)/576d0 + (2929d0*x**7)/30240d
     &0 + (629d0*x**8)/6912d0 + (185921d0*x**9)/2177280d0 + (
     &116423d0*x**10)/1451520d0

         case(73)            !11-1-1


            ris = (x**4)/24d0 + (x**5)/30d0 + (31d0*x**
     &6)/720d0 + (191d0*x**7)/5040d0 + (547d0*x**8)/13440d0 +
     & (3373d0*x**9)/90720d0 + (2147d0*x**10)/56700d0

         case(74)            !11-10

            llx = log(x)

            ris = -((11d0*x**3)/36d0) - (11d0*x**4)/36d
     &0 - (719d0*x**5)/2400d0 - (1337d0*x**6)/4800d0 - (46061
     &d0*x**7)/176400d0 - (27479d0*x**8)/112896d0 - (52243973
     &d0*x**9)/228614400d0 - (163666943d0*x**10)/762048000d0 
     &+ (x**3*llx)/6d0 + (x**4*llx)/6d0 + (7d0*x**5*llx)/40d0
     & + (119d0*x**6*llx)/720d0 + (101d0*x**7*llx)/630d0 + (3
     &05d0*x**8*llx)/2016d0 + (13157d0*x**9*llx)/90720d0 + (4
     &1603d0*x**10*llx)/302400d0

         case(75)            !11-11


            ris = (x**4)/24d0 + (x**5)/20d0 + (43d0*x**
     &6)/720d0 + (103d0*x**7)/1680d0 + (2563d0*x**8)/40320d0 
     &+ (127d0*x**9)/2016d0 + (3569d0*x**10)/56700d0

         case(76)            !110-1


            ris = (x**3)/6d0 + (3d0*x**4)/16d0 + (139d0
     &*x**5)/720d0 + (3d0*x**6)/16d0 + (27319d0*x**7)/151200d
     &0 + (29821d0*x**8)/172800d0 + (6284809d0*x**9)/38102400
     &d0 + (88913d0*x**10)/564480d0

         case(77)            !1100

            llx = log(x)

            ris = (7d0*x**2)/8d0 + (41d0*x**3)/72d0 + (
     &1397d0*x**4)/3456d0 + (2671d0*x**5)/8640d0 + (322493d0*
     &x**6)/1296000d0 + (104641d0*x**7)/504000d0 + (140539517
     &d0*x**8)/790272000d0 + (2486560891d0*x**9)/16003008000d
     &0 + (22064922487d0*x**10)/160030080000d0 - (3d0*x**2*ll
     &x)/4d0 - (7d0*x**3*llx)/12d0 - (131d0*x**4*llx)/288d0 -
     & (53d0*x**5*llx)/144d0 - (2213d0*x**6*llx)/7200d0 - (94
     &7d0*x**7*llx)/3600d0 - (647707d0*x**8*llx)/2822400d0 - 
     &(1290829d0*x**9*llx)/6350400d0 - (11574649d0*x**10*llx)
     &/63504000d0 + (x**2*llx**2)/4d0 + (x**3*llx**2)/4d0 + (
     &11d0*x**4*llx**2)/48d0 + (5d0*x**5*llx**2)/24d0 + (137d
     &0*x**6*llx**2)/720d0 + (7d0*x**7*llx**2)/40d0 + (363d0*
     &x**8*llx**2)/2240d0 + (761d0*x**9*llx**2)/5040d0 + (712
     &9d0*x**10*llx**2)/50400d0

         case(78)            !1101


            ris = (x**3)/6d0 + (11d0*x**4)/48d0 + (181d
     &0*x**5)/720d0 + (37d0*x**6)/144d0 + (38569d0*x**7)/1512
     &00d0 + (43171d0*x**8)/172800d0 + (9261559d0*x**9)/38102
     &400d0 + (1197607d0*x**10)/5080320d0

         case(79)            !111-1


            ris = (x**4)/24d0 + (x**5)/15d0 + (61d0*x**
     &6)/720d0 + (97d0*x**7)/1008d0 + (467d0*x**8)/4480d0 + (
     &9931d0*x**9)/90720d0 + (1831d0*x**10)/16200d0

         case(80)            !1110

            llx = log(x)

            ris = -((11d0*x**3)/36d0) - (19d0*x**4)/48d
     &0 - (599d0*x**5)/1440d0 - (79d0*x**6)/192d0 - (3343d0*x
     &**7)/8400d0 - (21977d0*x**8)/57600d0 - (83359739d0*x**9
     &)/228614400d0 - (3538531d0*x**10)/10160640d0 + (x**3*ll
     &x)/6d0 + (x**4*llx)/4d0 + (7d0*x**5*llx)/24d0 + (5d0*x*
     &*6*llx)/16d0 + (29d0*x**7*llx)/90d0 + (469d0*x**8*llx)/
     &1440d0 + (29531d0*x**9*llx)/90720d0 + (1303d0*x**10*llx
     &)/4032d0

         case(81)            !1111


            ris = (x**4)/24d0 + (x**5)/12d0 + (17d0*x**
     &6)/144d0 + (7d0*x**7)/48d0 + (967d0*x**8)/5760d0 + (89d
     &0*x**9)/480d0 + (4523d0*x**10)/22680d0
c End of expansions around x = 0

      end select
c --- set the imaginary part back to zero if it has been modified to
c --- get the branch cuts right (and should be zero).
      if (bcflag.eq.1) then
         xre = dreal(x)

         if (n4.eq.0.and.xre.gt.0d0) then
            if (xre.lt.1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
c
         else if (n4.eq.1.and.xre.lt.1d0) then
            if (n1.ne.-1.and.n2.ne.-1.and.n3.ne.-1) then
               ris = dcmplx(dreal(ris),0d0)
            else if (xre.gt.-1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
c            
         else if (n4.eq.-1.and.xre.gt.-1d0) then
            if (n1.ne.1.and.n2.ne.1.and.n3.ne.1) then
               ris = dcmplx(dreal(ris),0d0)
            else if (xre.lt.1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
            
         endif
      endif

      HPL4ar0=ris
      return
      end function
