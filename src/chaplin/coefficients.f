C     u/(e^u-1) =  sum_\{n=0\}^\{infty\} Bern(n)/n! u^n   
C--- fbern(n) = Bern(n)/n!, as produced by Maple  for n, n =0..60 
      
      double precision function fbern(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)
      if (n.gt.60) then
         print*, "Error in fbern: argument out of range!"
      else
         select case (n)
         case(0) 
            ris = 1.D0 
         case(1) 
            ris = -0.5D0 
         case(2) 
            ris = 0.8333333333333333D-1 
         case(3) 
            ris = 0.D0 
         case(4) 
            ris = -0.1388888888888889D-2 
         case(5) 
            ris = 0.D0 
         case(6) 
            ris = 0.3306878306878307D-4 
         case(7) 
            ris = 0.D0 
         case(8) 
            ris = -0.8267195767195767D-6 
         case(9) 
            ris = 0.D0 
         case(10) 
            ris = 0.208767569878681D-7 
         case(11) 
            ris = 0.D0 
         case(12) 
            ris = -0.5284190138687493D-9 
         case(13) 
            ris = 0.D0 
         case(14) 
            ris = 0.1338253653068468D-10 
         case(15) 
            ris = 0.D0 
         case(16) 
            ris = -0.3389680296322583D-12 
         case(17) 
            ris = 0.D0 
         case(18) 
            ris = 0.8586062056277845D-14 
         case(19) 
            ris = 0.D0 
         case(20) 
            ris = -0.2174868698558062D-15 
         case(21) 
            ris = 0.D0 
         case(22) 
            ris = 0.550900282836023D-17 
         case(23) 
            ris = 0.D0 
         case(24) 
            ris = -0.1395446468581252D-18 
         case(25) 
            ris = 0.D0 
         case(26) 
            ris = 0.3534707039629467D-20 
         case(27) 
            ris = 0.D0 
         case(28) 
            ris = -0.8953517427037547D-22 
         case(29) 
            ris = 0.D0 
         case(30) 
            ris = 0.2267952452337683D-23 
         case(31) 
            ris = 0.D0 
         case(32) 
            ris = -0.5744790668872202D-25 
         case(33) 
            ris = 0.D0 
         case(34) 
            ris = 0.1455172475614865D-26 
         case(35) 
            ris = 0.D0 
         case(36) 
            ris = -0.368599494066531D-28 
         case(37) 
            ris = 0.D0 
         case(38) 
            ris = 0.9336734257095045D-30 
         case(39) 
            ris = 0.D0 
         case(40) 
            ris = -0.236502241570063D-31 
         case(41) 
            ris = 0.D0 
         case(42) 
            ris = 0.5990671762482134D-33 
         case(43) 
            ris = 0.D0 
         case(44) 
            ris = -0.151745488446829D-34 
         case(45) 
            ris = 0.D0 
         case(46) 
            ris = 0.3843758125454188D-36 
         case(47) 
            ris = 0.D0 
         case(48) 
            ris = -0.9736353072646691D-38 
         case(49) 
            ris = 0.D0 
         case(50) 
            ris = 0.2466247044200681D-39 
         case(51) 
            ris = 0.D0 
         case(52) 
            ris = -0.6247076741820744D-41 
         case(53) 
            ris = 0.D0 
         case(54) 
            ris = 0.1582403024464491D-42 
         case(55) 
            ris = 0.D0 
         case(56) 
            ris = -0.4008273685948936D-44 
         case(57) 
            ris = 0.D0 
         case(58) 
            ris = 0.1015307585556956D-45 
         case(59) 
            ris = 0.D0 
         case(60) 
            ris = -0.2571804158241872D-47
         end select
      endif
      fbern = ris
      return
      end

c     zetaval4 (n) = Zeta(4-n)/(n!) for n !=3

      double precision function zetaval4(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)   
      if (n.gt.60) then
         print*, "Error in zetaval4: argument out of range!"
      else
         select case (n)
         case(0)
            ris =  1.082323233711138d0
         case(1)
            ris =  1.202056903159594d0
         case(2)
            ris =  0.8224670334241131d0
         case(3)
            print*, "Error in zetaval4: illegal argument n=3!"
         case(4)
            ris =  -0.02083333333333333d0
         case(5)
            ris =  -0.0006944444444444444d0
         case(6)
            ris =  0d0
         case(7)
            ris =  0.000001653439153439153d0
         case(8)
            ris =  0d0
         case(9)
            ris =  -1.093544413650234d-8
         case(10)
            ris =  0d0
         case(11)
            ris =  1.043837849393405d-10
         case(12)
            ris =  0d0
         case(13)
            ris =  -1.216594230062244d-12
         case(14)
            ris =  0d0
         case(15)
            ris =  1.613000652835010d-14
         case(16)
            ris =  0d0
         case(17)
            ris =  -2.342881045287934d-16
         case(18)
            ris =  0d0
         case(19)
            ris =  3.643877167529436d-18
         case(20)
            ris =  0d0
         case(21)
            ris =  -5.977486811666558d-20 
         case(22)
            ris =  0d0
         case(23)
            ris =  1.023371305551507d-21
         case(24)
            ris =  0d0
         case(25)
            ris =  -1.814559561383475d-23
         case(26)
            ris =  0d0
         case(27)
            ris =  3.313025803849127d-25
         case(28)
            ris =  0d0
         case(29)
            ris =  -6.200979326536194d-27
         case(30)
            ris =  0d0
         case(31)
            ris =  1.185645085417335d-28
         case(32)
            ris =  0d0
         case(33)
            ris =  -2.309335748959029d-30
         case(34)
            ris =  0d0
         case(35)
            ris =  4.571548469627103d-32
         case(36)
            ris =  0d0
         case(37)
            ris =  -9.180435533946961d-34
         case(38)
            ris =  0d0
         case(39)
            ris =  1.867249304296863d-35
         case(40)
            ris =  0d0
         case(41)
            ris =  -3.841518653556106d-37
         case(42)
            ris =  0d0
         case(43)
            ris =  7.984976959257184d-39
         case(44)
            ris =  0d0
         case(45)
            ris =  -1.675299999575527d-40
         case(46)
            ris =  0d0
         case(47)
            ris =  3.544825882479490d-42
         case(48)
            ris =  0d0
         case(49)
            ris =  -7.558977352819157d-44
         case(50)
            ris =  0d0
         case(51)
            ris =  1.623374862052603d-45
         case(52)
            ris =  0d0
         case(53)
            ris =  -3.509273235152795d-47
         case(54)
            ris =  0d0
         case(55)
            ris =  7.632049500594654d-49
         case(56)
            ris =  0d0
         case(57)
            ris =  -1.669159245403588d-50
         case(58)
            ris =  0d0
         case(59)
            ris =  3.669564111503313d-52
         case(60)
            ris =  0d0
         end select
      endif
      zetaval4 = ris
      return
      end

c     zetaval3 (n) = Zeta(3-n)/(n!) for n !=2

      double precision function zetaval3(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)      
      if (n.gt.60) then
         print*, "Error in zetaval3: argument out of range!"
      else
         select case (n)
         case(0)
            ris =  1.202056903159594d0
         case(1)
            ris =  1.644934066848226d0
         case(2)
            print*, "Error in zetaval3: illegal argument n=2!"
         case(3)
            ris = -0.08333333333333333d0
         case(4)
            ris = -0.003472222222222222d0
         case(5)
            ris = 0d0
         case(6)
            ris = 0.00001157407407407407d0
         case(7)
            ris = 0d0
         case(8)
            ris = -9.841899722852104d-8
         case(9)
            ris = 0d0
         case(10)
            ris = 1.148221634332745d-9
         case(11)
            ris = 0d0
         case(12)
            ris = -1.581572499080917d-11
         case(13)
            ris = 0d0
         case(14)
            ris = 2.419500979252515d-13
         case(15)
            ris = 0d0
         case(16)
            ris = -3.982897776989488d-15
         case(17)
            ris = 0d0
         case(18)
            ris = 6.923366618305929d-17
         case(19)
            ris = 0d0
         case(20)
            ris = -1.255272230449977d-18 
         case(21)
            ris = 0d0
         case(22)
            ris = 2.353754002768465d-20
         case(23)
            ris = 0d0
         case(24)
            ris = -4.536398903458687d-22
         case(25)
            ris = 0d0
         case(26)
            ris = 8.945169670392643d-24
         case(27)
            ris = 0d0
         case(28)
            ris = -1.798284004695496d-25
         case(29)
            ris = 0d0
         case(30)
            ris = 3.675499764793738d-27
         case(31)
            ris = 0d0
         case(32)
            ris = -7.620807971564795d-29
         case(33)
            ris = 0d0
         case(34)
            ris = 1.600041964369486d-30
         case(35)
            ris = 0d0
         case(36)
            ris = -3.396761147560376d-32
         case(37)
            ris = 0d0
         case(38)
            ris = 7.282272286757765d-34
         case(39)
            ris = 0d0
         case(40)
            ris = -1.575022647958003d-35
         case(41)
            ris = 0d0
         case(42)
            ris = 3.433540092480589d-37
         case(43)
            ris = 0d0
         case(44)
            ris = -7.538849998089870d-39
         case(45)
            ris = 0d0
         case(46)
            ris = 1.666068164765360d-40
         case(47)
            ris = 0d0
         case(48)
            ris = -3.703898902881387d-42
         case(49)
            ris = 0d0
         case(50)
            ris = 8.279211796468275d-44
         case(51)
            ris = 0d0
         case(52)
            ris = -1.859914814630981d-45
         case(53)
            ris = 0d0
         case(54)
            ris = 4.197627225327060d-47
         case(55)
            ris = 0d0
         case(56)
            ris = -9.514207698800454d-49
         case(57)
            ris = 0d0
         case(58)
            ris = 2.165042825786954d-50
         case(59)
            ris = 0d0
         case(60)
            ris = -4.945000903745158d-52
         end select
      endif
      zetaval3 = ris
      return
      end

c     zetaval2 (n) = Zeta(2-n)/(n!) for n !=1

      double precision function zetaval2(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)      
      if (n.gt.60) then
         print*, "Error in zetaval2: argument out of range!"
      else
         select case (n)
         case(0)
            ris =  1.644934066848226d0
         case(1)
            print*, "Error in zetaval2: illegal argument n=1!"
         case(2)
            ris = -0.2500000000000000d0
         case(3)
            ris = -0.01388888888888889d0
         case(4)
            ris = 0d0
         case(5)
            ris = 0.00006944444444444444d0
         case(6)
            ris = 0d0
         case(7)
            ris = -7.873519778281683d-7
         case(8)
            ris = 0d0
         case(9)
            ris = 1.148221634332745d-8
         case(10)
            ris = 0d0
         case(11)
            ris = -1.897886998897100d-10   
         case(12)
            ris = 0d0
         case(13)
            ris = 3.387301370953521d-12
         case(14)
            ris = 0d0
         case(15)
            ris = -6.372636443183180d-14
         case(16)
            ris = 0d0
         case(17)
            ris = 1.246205991295067d-15   
         case(18)
            ris = 0d0
         case(19)
            ris = -2.510544460899955d-17
         case(20)
            ris = 0d0
         case(21)
            ris = 5.178258806090624d-19 
         case(22)
            ris = 0d0
         case(23)
            ris = -1.088735736830085d-20 
         case(24)
            ris = 0d0
         case(25)
            ris = 2.325744114302087d-22
         case(26)
            ris = 0d0
         case(27)
            ris = -5.035195213147390d-24
         case(28)
            ris = 0d0
         case(29)
            ris = 1.102649929438122d-25
         case(30)
            ris = 0d0
         case(31)
            ris = -2.438658550900734d-27
         case(32)
            ris = 0d0
         case(33)
            ris = 5.440142678856252d-29 
         case(34)
            ris = 0d0
         case(35)
            ris = -1.222834013121735d-30
         case(36)
            ris = 0d0
         case(37)
            ris = 2.767263468967951d-32
         case(38)
            ris = 0d0
         case(39)
            ris = -6.300090591832014d-34
         case(40)
            ris = 0d0
         case(41)
            ris = 1.442086838841848d-35
         case(42)
            ris = 0d0
         case(43)
            ris = -3.317093999159543d-37
         case(44)
            ris = 0d0
         case(45)
            ris = 7.663913557920658d-39 
         case(46)
            ris = 0d0
         case(47)
            ris = -1.777871473383066d-40
         case(48)
            ris = 0d0
         case(49)
            ris = 4.139605898234137d-42 
         case(50)
            ris = 0d0
         case(51)
            ris = -9.671557036081102d-44 
         case(52)
            ris = 0d0
         case(53)
            ris = 2.266718701676613d-45 
         case(54)
            ris = 0d0
         case(55)
            ris = -5.327956311328254d-47
         case(56)
            ris = 0d0
         case(57)
            ris = 1.255724838956433d-48 
         case(58)
            ris = 0d0
         case(59)
            ris = -2.967000542247094d-50 
         case(60)
            ris = 0d0
         end select
      endif
      zetaval2 = ris
      return
      end

C--- fbern3(n) = Sum(Bern(n-k)*Bern(k)/(k+1)!(n-k)!,k=0..n) as produced by Maple  for n, n =0..60 
c--- for the expansion of bsli3_inside
      
      double precision function fbern3(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)
      if (n.gt.60) then
         print*, "Error in fbern3: argument out of range!"
      else
         select case (n)
         case(0) 
            ris = 1d0 
         case(1) 
            ris = -0.7500000000000000d0
         case(2)
            ris = 0.2361111111111111d0
         case(3)
            ris = -0.03472222222222222d0
         case(4)
            ris = 0.0006481481481481481d0
         case(5)
            ris = 0.0004861111111111111d0
         case(6)
            ris = -0.00002393550012597632d0
         case(7)
            ris = -0.00001062925170068027d0
         case(8)
            ris = 7.794784580498866d-7
         case(9)
            ris = 2.526087595532040d-7 
         case(10)
            ris = -2.359163915200471d-8
         case(11)
            ris = -6.168132746415575d-9  
         case(12)
            ris = 6.824456748981078d-10  
         case(13)
            ris = 1.524285616929085d-10  
         case(14)
            ris = -1.916909414174054d-11   
         case(15)
            ris = -3.791718683693992d-12  
         case(16)
            ris = 5.277408409541286d-13 
         case(17)
            ris = 9.471165533842511d-13  
         case(18)
            ris = -1.432311114490360d-14   
         case(19)
            ris = -2.372464515550457d-15  
         case(20)
            ris = 3.846565792753191d-16 
         case(21)
            ris = 5.954997627004217d-17  
         case(22)
            ris = -1.024749796500817d-17   
         case(23)
            ris = -1.497011638141367d-18  
         case(24)
            ris = 2.712880383718634d-19 
         case(25)
            ris = 3.767705465169381d-20  
         case(26)
            ris = -7.145911733703927d-21   
         case(27)
            ris = -9.491342976782829d-22  
         case(28)
            ris = 1.874573842744960d-22 
         case(29)
            ris = 2.392750346880724d-23  
         case(30)
            ris = -4.900845424989120d-24   
         case(31)
            ris = -6.035679913479318d-25  
         case(32)
            ris = 1.277612709067810d-25 
         case(33)
            ris = 1.523239950079751d-26  
         case(34)
            ris = -3.322561855169143d-27   
         case(35)
            ris = -3.845812971267857d-28  
         case(36)
            ris = 8.622657206358129d-29 
         case(37)
            ris = 9.713094776077507d-30  
         case(38)
            ris = -2.233694426393531d-30   
         case(39)
            ris = -2.453885285518569d-31
         case(40)
            ris = 5.777231527668267d-32
         case(41)
            ris = 6.200973407019944d-33
         case(42)
            ris = -1.492145220906824d-33   
         case(43)
            ris = -1.567326914602884d-34  
         case(44)
            ris = 3.849163455706614d-35 
         case(45)
            ris = 3.962243309444980d-36  
         case(46)
            ris = -9.918444388268698d-37   
         case(47)
            ris = -1.001830575251358d-37  
         case(48)
            ris = 2.553242918658000d-38 
         case(49)
            ris = 2.533438809719292d-39  
         case(50)
            ris = -6.566810518040945d-40   
         case(51)
            ris = -6.407406536403730d-41  
         case(52)
            ris = 1.687591198065752d-41 
         case(53)
            ris = 1.620703871698778d-42  
         case(54)
            ris = -4.333734365550847d-43   
         case(55)
            ris = -4.099862381567091d-44  
         case(56)
            ris = 1.112160432685329d-44 
         case(57)
            ris = 1.037228716978014d-45  
         case(58)
            ris = -2.852378663608893d-46   
         case(59)
            ris = -2.624311979617555d-47  
         case(60)
            ris = 7.311443261888958d-48 
         end select
      endif
      fbern3 = ris
      return
      end

C--- fbern4(n) = Sum(Sum(Bern(n-k1)*Bern(k2)*Bern(k1-k2)/(k1+1)(k2+1)!(n-k1)!(k1-k2)!,k2=0..k1),k1=0..n) as produced by Maple  for n, n =0..60 
c--- for the expansion of bsli4_inside
      
      double precision function fbern4(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)
      if (n.gt.60) then
         print*, "Error in fbern4: argument out of range!"
      else
         select case (n)
         case(0)
            ris =  1d0
         case(1)
            ris =  -0.8750000000000000d0
         case(2)
            ris =  0.3495370370370370d0
         case(3)
            ris =  -0.07928240740740741d0
         case(4)
            ris =  0.009639660493827160d0
         case(5)
            ris =  -0.0001863425925925926d0
         case(6)
            ris =  -0.0001093680638040048d0
         case(7)
            ris =  0.000006788098837418565d0
         case(8)
            ris =  0.000002061865494287074d0
         case(9)
            ris =  -2.183261421852692d-7
         case(10)
            ris =  -4.271107367089217d-8
         case(11)
            ris =  6.535550523864399d-9
         case(12)
            ris =  9.049046773887543d-10
         case(13)
            ris =  -1.872603276102330d-10
         case(14)
            ris =  -1.917727902789986d-11
         case(15)
            ris =  5.216900572839828d-12
         case(16)
            ris =  4.020087098665104d-13
         case(17)
            ris =  -1.426164321965609d-13
         case(18)
            ris =  -8.256053984896996d-15
         case(19)
            ris =  3.847254012507184d-15 
         case(20)
            ris =  1.640607009971150d-16
         case(21)
            ris =  -1.027796845856804d-16 
         case(22)
            ris =  -3.090129195856955d-18
         case(23)
            ris =  2.725638444323362d-18 
         case(24)
            ris =  5.288189050608147d-20
         case(25)
            ris =  -7.186926847034095d-20
         case(26)
            ris =  -7.308407684224817d-22
         case(27)
            ris =  1.886417255201600d-21
         case(28)
            ris =  3.853306982438308d-24
         case(29)
            ris =  -4.933131916910348d-23
         case(30)
            ris =  2.567917828979520d-25 
         case(31)
            ris =  1.286107151842219d-24
         case(32)
            ris =  -1.509985568680010d-26
         case(33)
            ris =  -3.344379709216914d-26
         case(34)
            ris =  5.915118844613248d-28
         case(35)
            ris =  8.677730002701531d-28
         case(36)
            ris =  -2.007846701098425d-29
         case(37)
            ris =  -2.247412324372187d-29
         case(38)
            ris =  6.330523251424075d-31
         case(39)
            ris =  5.811038175096110d-31
         case(40)
            ris =  -1.908098741710552d-32
         case(41)
            ris =  -1.500402230226536d-32
         case(42)
            ris =  5.579753739853343d-34 
         case(43)
            ris =  3.869172594205331d-34
         case(44)
            ris =  -1.596600909217081d-35
         case(45)
            ris =  -9.966584965646120d-36
         case(46)
            ris =  4.494598066447527d-37
         case(47)
            ris =  2.564746205249869d-37
         case(48)
            ris =  -1.249320845166970d-38
         case(49)
            ris =  -6.594112459106036d-39
         case(50)
            ris =  3.437590743407587d-40 
         case(51)
            ris =  1.694026261214745d-40
         case(52)
            ris =  -9.380808471597297d-42
         case(53)
            ris =  -4.348790717341230d-42
         case(54)
            ris =  2.542378503354191d-43 
         case(55)
            ris =  1.115655021429451d-43
         case(56)
            ris =  -6.850498271628611d-45
         case(57)
            ris =  -2.860416527479943d-45
         case(58)
            ris =  1.836767217486433d-46
         case(59)
            ris =  7.329738001420199d-47
         case(60)
            ris =  -4.903773548311542d-48
         end select
      endif
      fbern4 = ris
      return
      end

C--- bsh2m2_inside_coeff = Convulute[BernoulliB,Car[Convolute[l,b]]][k]/(k+1)! as produced by Mathematica for k =0..50 
c--- for the expansion of bsli221m1_inside (see Claude's Mathematica notebook for details)

      double precision function bsh2m2_inside_coeff(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)
      if (n.gt.60) then
         print*, "Error in bsh2m2_inside_coeff: argument out
     &of range!"
      else
         select case (n)
         case(0)
            ris = 0d0
         case(1)
            ris = 0.25d0
         case(2)
            ris = -0.33333333333333333333333333333333333333d0
         case(3)
            ris = 0.32465277777777777777777777777777777778d0
         case(4)
            ris = -0.30625d0
         case(5)
            ris = 0.30379243827160493827160493827160493827d0
         case(6)
            ris = -0.31872354497354497354497354497354497354d0
         case(7)
            ris = 0.34942753606072058453010833963214915596d0
         case(8)
            ris = -0.39610441205803308977912152515327118502d0
         case(9)
            ris = 0.46088839082315024775342235659695977156d0
         case(10)
            ris = -0.54763208903089855470807851760232712614d0
         case(11)
            ris = 0.66200911111953893439383258206410362896d0
         case(12)
            ris = -0.81188925838826538531925737312942700148d0
         case(13)
            ris = 1.00795643362229816216687401687838388955d0
         case(14)
            ris = -1.26459964909127263321239539025280620546d0
         case(15)
            ris = 1.60114980282579045421193214977913524517d0
         case(16)
            ris = -2.04357458264112382930398068660055661539d0
         case(17)
            ris = 2.62679146785147108346626455025786802269d0
         case(18)
            ris = -3.3978212662345306192108849116821050901d0
         case(19)
            ris = 4.42008916747513891593795341308266195422d0
         case(20)
            ris = -5.77929605453662821472964164616057986534d0
         case(21)
            ris = 7.5914420822867649412108703407882562631d0
         case(22)
            ris = -10.01380417551731684404860747031673848837d0
         case(23)
            ris = 13.2599724342706003994272786362279595029d0
         case(24)
            ris = -17.62046979451679106230099139996092532498d0
         case(25)
            ris = 23.49105957126703545228140735739792837063d0
         case(26)
            ris = -31.41164910680603666263381982990994751513d0
         case(27)
            ris = 42.11981144677754360869223477309392899981d0
         case(28)
            ris = -56.62449156061816696374541803214426506275d0
         case(29)
            ris = 76.30760732211123301650777535989971597022d0
         case(30)
            ris = -103.06423265661701646097356382586770596248d0
         case(31)
            ris = 139.49618771598667273816520701579496493537d0
         case(32)
            ris = -189.17961447585572571919597597471778982985d0
         case(33)
            ris = 257.03512190956252314912186733651735036023d0
         case(34)
            ris = -349.84023114411608509348802046227076258748d0
         case(35)
            ris = 476.93937865631952298174763732794670642248d0
         case(36)
            ris = -651.22837899795528361666190839729500360734d0
         case(37)
            ris = 890.52043294417179905969366275719729891096d0
         case(38)
            ris = -1219.44288503829704492628403792914586291004d0
         case(39)
            ris = 1672.07273451094160412392293512612616442755d0
         case(40)
            ris = -2295.60103372615062888300528763041578834624d0
         case(41)
            ris = 3155.43108102646886271018804417076202656023d0
         case(42)
            ris = -4342.27577947660636901810844191849977089553d0
         case(43)
            ris = 5982.04398273096741701289935738449147760094d0
         case(44)
            ris = -8249.61973814509238262853195875556444226902d0
         case(45)
            ris = 11388.07805564752005631223807277962536950224d0
         case(46)
            ris = -15735.49668840664380274564847089543627521088d0
         case(47)
            ris = 21762.38631954750795838858738525192422342085d0
         case(48)
            ris = -30123.97109708559602826046920301590224663195d0
         case(49)
            ris = 41733.24755820664669516069768729157065842151d0
         case(50)
            ris = -57863.12925898186469926250043797718540000443d0
         case(51)
            ris = 80289.32329366449d0
         case(52)
            ris = -111490.2720614106d0
         case(53)
            ris = 154927.0759149843d0
         case(54)
            ris = -215435.5591115531d0
         case(55)
            ris = 299775.6357407621d0
         case(56)
            ris = -417401.3987681358d0
         case(57)
            ris = 581541.0413407259d0
         case(58)
            ris = -810711.850057715d0
         case(59)
            ris = 1.130846347545417d6
         case(60)
            ris = -1.578277214325144d6 
         end select
      endif
      bsh2m2_inside_coeff = ris
      return
      end

C--- bsh2m2_outside_coeff = Convulute[Car[del],Bar[BernoulliB]][k]/(k+2)! as produced by Mathematica for k =0..50 
c--- for the expansion of bsh2m2_outside (see Claude's Mathematica notebook for details)

      double precision function bsh2m2_outside_coeff(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)     
      if (n.gt.60) then
         print*, "Error in bsh2m2_outside_coeff: argument out
     &of range!"
      else
         select case (n)
         case(0)
            ris = -0.34657359027997265468724018278035003959d0
         case(1)
            ris = -0.09942893171332877578120669713005833993d0
         case(2)
            ris = -0.01870241097611073131510055809417152833d0
         case(3)
            ris = -0.00208333333333333333333333333333333333d0
         case(4)
            ris = -0.0000489283712703729023437740571499675889260269565d0
         case(5)
            ris = 0.00002066798941798941798941798941798941798941798941d0
         case(6)
            ris = 1.2884145995370693326161656949400627839308240490217d-6
         case(7)
            ris = -4.592886537330981775426219870664315108759553203997d-7
         case(8)
            ris = -3.841355103101167227573412362616934818518123182212d-8
         case(9)
            ris = 1.5187840708674042007375340708674042007375340708674d-8
         case(10)
            ris = 1.3118120316900653818632021704955809220831299106123d-9
         case(11)
            ris = -6.55744290003549262808522067781327040586299845559d-10
         case(12)
            ris = -5.1539458301686469734396605655212348946725440934d-11
         case(13)
            ris = 3.36552586214024863558751187876912240731842646682d-11
         case(14)
            ris = 2.3077743717648296479607815279472789099531259751d-12
         case(15)
            ris = -1.9314711337353727643176756155280898511524095091d-12
         case(16)
            ris = -1.15513935092623822093656707083318507733733184d-13
         case(17)
            ris = 1.196485287344128071506308917799396598670260918d-13
         case(18)
            ris = 6.3245372250613078406062697399862401724401848d-15
         case(19)
            ris = -7.8398923775775080722526660481862571568365291d-15
         case(20)
            ris = -3.716509741676115597607681248818613057099225d-16
         case(21)
            ris = 5.366482373464184362690209868728159007613545d-16
         case(22)
            ris = 2.30981644289788787941730304712493565335811d-17
         case(23)
            ris = -3.80563584777921127707022030905576525221732d-17
         case(24)
            ris = -1.5019169864365238057633778251380945723184d-18
         case(25)
            ris = 2.779217005973289381963121045947532848022d-18
         case(26)
            ris = 1.013616876604427059783958923968945174938d-19
         case(27)
            ris = -2.080714243170466623018503720296429725011d-19
         case(28)
            ris = -7.0577011987439886644765373870035563284d-21
         case(29)
            ris = 1.59134829672210430115185071099233178351d-20
         case(30)
            ris = 5.046832822906558403400194919312393048d-22
         case(31)
            ris = -1.2398157254237306318207896697863693177d-21
         case(32)
            ris = -3.69292819414198754324245418851581713d-23
         case(33)
            ris = 9.81732678916664963774441840178628279d-23
         case(34)
            ris = 2.757132928501794394186871428724018582559368058864d-24
         case(35)
            ris = -7.88593434892663445855262182065822929462112827603d-24
         case(36)
            ris = -2.09530892978319292794475415724407233432261031336d-25
         case(37)
            ris = 6.415819818433061571040126269943549904812367719067d-25
         case(38)
            ris = 1.617656421209199638990888867241030108178649331311d-26
         case(39)
            ris = -5.27974304909279650804125265776728275000693304122d-26
         case(40)
            ris = -1.26664166057107543507584763138568240226796931858d-27
         case(41)
            ris = 4.389787510383208946659969425947524976493828718066d-27
         case(42)
            ris = 1.004478513658036808806221219351475682910401269132d-28
         case(43)
            ris = -3.68402365984839917328041850801567942311124408227d-28
         case(44)
            ris = -8.05795104864683113893564373654148312536013899404d-30
         case(45)
            ris = 3.11806182115730762940319268428443509041923236979d-29
         case(46)
            ris = 6.5321288579505704995572158842477318308471281315d-31
         case(47)
            ris = -2.65957871796725401706805913133661826034197023975d-30
         case(48)
            ris = -5.346122089889279322484472674434465600440162554d-32
         case(49)
            ris = 2.284697007454141289587948162164613354844912161d-31
         case(50)
            ris = 4.41401503099778189467559272097237769583880453d-33
         case(51)
            ris = -1.975545204274158d-32
         case(52)
            ris = -3.673985220125961d-34
         case(53)
            ris = 1.718584764348089d-33
         case(54)
            ris = 3.080932930073462d-35
         case(55)
            ris = -1.503444991124448d-34
         case(56)
            ris = -2.601539822964359d-36
         case(57)
            ris = 1.322099805214306d-35
         case(58)
            ris = 2.210902587052212d-37
         case(59)
            ris = -1.168278254554942d-36
         case(60)
            ris = -1.890205353734545d-38
         end select
      endif
      bsh2m2_outside_coeff = ris
      return
      end    


C--- bsh21m1_inside_coeff = Convolute[Car[L},BernoulliB][k]/(k+1)! as produced by Mathematica for k =0..50 
c--- for the expansion of bsh21m1_inside (see Claude's Mathematica notebook for details)

      double precision function bsh21m1_inside_coeff(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)
      if (n.gt.60) then
         print*, "Error in bsh21m1_inside_coeff: argument out
     &of range!"
      else
         select case (n)
         case(0)
            ris = 0d0
         case(1)
            ris = 0d0
         case(2)
            ris = 0.05555555555555555555555555555555555555d0
         case(3)
            ris = -0.04166666666666666666666666666666666667d0
         case(4)
            ris = 0.02111111111111111111111111111111111111d0
         case(5)
            ris = -0.01134259259259259259259259259259259259d0
         case(6)
            ris = 0.00739323507180650037792894935752078609d0
         case(7)
            ris = -0.00557622354497354497354497354497354497d0
         case(8)
            ris = 0.0046271188516558886929257299627669998d0
         case(9)
            ris = -0.00411593364197530864197530864197530864d0
         case(10)
            ris = 0.00386294059826256795953765650735347705d0
         case(11)
            ris = -0.00378350754291147809666328184846703365d0
         case(12)
            ris = 0.00383692432871436737940604444470948338d0
         case(13)
            ris = -0.00400544521509982347094969921878463073d0
         case(14)
            ris = 0.00428509853072908812179829228624055197d0
         case(15)
            ris = -0.00468163170895672859131853840848549843d0
         case(16)
            ris = 0.00520894450884518419736482996645449426d0
         case(17)
            ris = -0.00588892214032736337256782266061697999d0
         case(18)
            ris = 0.00675222085677558979005270347976165983d0
         case(19)
            ris = -0.00783985524185440691698570025881023921d0
         case(20)
            ris = 0.00920559839097260642645948076369668916d0
         case(21)
            ris = -0.0109193160805888610150623659673927008d0
         case(22)
            ris = 0.01307145143184244713642707831306743478d0
         case(23)
            ris = -0.01577897791569898089500149194951736475d0
         case(24)
            ris = 0.01919326000798685425750467950010886339d0
         case(25)
            ris = -0.02351041535618228273234241152537828282d0
         case(26)
            ris = 0.0289849744599237780775774668569192097d0
         case(27)
            ris = -0.03594790161587331235625197336866535179d0
         case(28)
            ris = 0.04483039773763080821524708127824947767d0
         case(29)
            ris = -0.05619538290928166440848974708400538306d0
         case(30)
            ris = 0.07077919615385316369497980154571334018d0
         case(31)
            ris = -0.08954690849500889592607257636718821149d0
         case(32)
            ris = 0.11376579943143369596714529802167748344d0
         case(33)
            ris = -0.1451030998955813485736868287081959981d0
         case(34)
            ris = 0.18575619678426856983449590403023339146d0
         case(35)
            ris = -0.23862631525569750772708854743417161154d0
         case(36)
            ris = 0.30755050306441750138054992981413281864d0
         case(37)
            ris = -0.39761188650052154382264838479391395438d0
         case(38)
            ris = 0.51555512609759617684200377763623527295d0
         case(39)
            ris = -0.67034341981460265331158331009443544253d0
         case(40)
            ris = 0.87390616355980603103871544224890727124d0
         case(41)
            ris = -1.14214368489797724295212437888874165919d0
         case(42)
            ris = 1.4962789532328810275473804352692022646d0
         case(43)
            ris = -1.96467807302034533319402088475784359455d0
         case(44)
            ris = 2.58530473614794932408116808366503279294d0
         case(45)
            ris = -3.40903281315012497383638972221323051052d0
         case(46)
            ris = 4.5041215980597537113658301939101194455d0
         case(47)
            ris = -5.96226768284192323289120314071164150324d0
         case(48)
            ris = 7.9067966940867543775459861615050017829d0
         case(49)
            ris = -10.50376179135037068467037092166076121372d0
         case(50)
            ris = 13.97699393313140871740204966813643099381d0
         case(51)
            ris = -18.62852893150884d0
         case(52)
            ris = 24.86635593168189d0
         case(53)
            ris = -33.24214291737328d0
         case(54)
            ris = 44.50256824694052d0
         case(55)
            ris = -59.6592207797288d0
         case(56)
            ris = 80.0838592330268d0
         case(57)
            ris = -107.6383289103828d0
         case(58)
            ris = 144.8518753970011d0
         case(59)
            ris = -195.1633208123604d0
         case(60)
            ris = 263.2520618632051d0
         end select
      endif
      bsh21m1_inside_coeff = ris
      return
      end    


c--- bsh21m1_outside_1_coeff = Convolute[Car[Convolute[Car[eta],Bar[BernoulliB]]],Bar[BernoulliB]][k]/(k+2)! as produced by Mathematica for k =0..50 
c--- for the expansion of bsh21m1_outside_1 (see Claude's Mathematica notebook for details)

      double precision function bsh21m1_outside_1_coeff(n)
      implicit none
      double precision ris, x
      integer n
      x=0d0
      ris=dlog(x)      
      if (n.gt.60) then
         print*, "Error in bsh21m1_outside_1_coeff: argument out
     &of range!"
      else
         select case (n)
         case(0)
            ris = 0.25d0
         case(1)
            ris = 0.07291666666666666666666666666666666667d0
         case(2)
            ris = 0.01417824074074074074074074074074074074d0
         case(3)
            ris = 0.00171440972222222222222222222222222222d0
         case(4)
            ris = 0.00007301311728395061728395061728395061728395061728d0
         case(5)
            ris = -0.0000125041335978835978835978835978835978835978835d0
         case(6)
            ris = -1.450098475236655508764352301767267753662311485440d-6
         case(7)
            ris = 2.2362128927574116066179558243050306542370034433526d-7
         case(8)
            ris = 3.6408877787226001511715797430083144368858654572940d-8
         case(9)
            ris = -6.755588081542942654053765164876275987387098498209d-9
         case(10)
            ris = -1.109970772828323351593571833489043103222414371422d-9
         case(11)
            ris = 2.920729093179508793986908465022943137421251899366d-10
         case(12)
            ris = 4.07040664729507986893565053606845255125652751942d-11
         case(13)
            ris = -1.54232004675897065473978860416056377061861876871d-11
         case(14)
            ris = -1.7586804691217876825258861212325737218833540501d-12
         case(15)
            ris = 9.08205825364570960972137183362414195488165817d-13
         case(16)
            ris = 8.66969407829228852133340343965747389582501868d-14
         case(17)
            ris = -5.72693997823042031508939689225652685429186075d-14
         case(18)
            ris = -4.722657643647692753376839132260669346681035d-15
         case(19)
            ris = 3.7961006754346316575043025796936606378810691d-15
         case(20)
            ris = 2.77257823938750085713719228626169406095171d-16
         case(21)
            ris = -2.618185561274727099420529912925874442552533d-16
         case(22)
            ris = -1.72389951229310029134042145312489110687666d-17
         case(23)
            ris = 1.86623221122622925298184145645356459070021d-17
         case(24)
            ris = 1.1217493834274488344169131569830710245067d-18
         case(25)
            ris = -1.3678444862720544794089398871076698236317d-18
         case(26)
            ris = -7.5756515622019988494441902137486118554d-20
         case(27)
            ris = 1.026798362516310598835429129742185643616d-19
         case(28)
            ris = 5.27780444854412377545177391084561763d-21
         case(29)
            ris = -7.8689667849455907082448641721941430713d-21
         case(30)
            ris = -3.775745032462589788957464895007771073d-22
         case(31)
            ris = 6.140384723588674603038884693244599927d-22
         case(32)
            ris = 2.7638095480009859372217068951712052d-23
         case(33)
            ris = -4.86830965140488328582095855194144745d-23
         case(34)
            ris = -2.06403852393105725162797358912842602551673375243d-24
         case(35)
            ris = 3.914544875722016734185580275369864471898481889177d-24
         case(36)
            ris = 1.568945242864306075290133668541159017241982855506d-25
         case(37)
            ris = -3.18746217044925224339822041955991933581145950268d-25
         case(38)
            ris = -1.21151253199583864898257078920642860579884917706d-26
         case(39)
            ris = 2.624881660360950866206049570667699593436409964141d-26
         case(40)
            ris = 9.487757321884177131918607162923106001622006559783d-28
         case(41)
            ris = -2.18371877690768050829616893289950202832191081510d-27
         case(42)
            ris = -7.52502306611488092771581412176004304821436910272d-29
         case(43)
            ris = 1.833554008456269328977582847463506471990654538906d-28
         case(44)
            ris = 6.03726988240999908603463469087821103347341708425d-30
         case(45)
            ris = -1.55254146773965411841604406785622060642534787771d-29
         case(46)
            ris = -4.8945474465226378082227353296153123751377171356d-31
         case(47)
            ris = 1.32474791719770610002463760569579814683029165664d-30
         case(48)
            ris = 4.006201871062892644520914908220239685492560877d-32
         case(49)
            ris = -1.1383872450203677137446779268432892918307379484d-31
         case(50)
            ris = -3.30795228544275656590407577478078560216736902d-33
         case(51)
            ris = 9.8462750253901d-33
         case(52)
            ris = 2.753533737449097d-34
         case(53)
            ris = -8.56771228709784d-34
         case(54)
            ris = -2.309189265362991d-35
         case(55)
            ris = 7.496835279005074d-35
         case(56)
            ris = 1.949976046480221d-36
         case(57)
            ris = -6.59387311821575d-36
         case(58)
            ris = -1.657248185332491d-37
         case(59)
            ris = 5.827730590842361d-37
         case(60)
            ris = 1.4169158195933d-38
         end select
      endif
      bsh21m1_outside_1_coeff = ris
      return
      end    

c--- bsh21m1_outside_2_coeff(k,1) = Convolute[Car[gam],Bar[g]][k]/(k+2)! as produced by Mathematica for k =0..50
c--- bsh21m1_outside_2_coeff(k,2) = Convolute[Car[Car[gam]],Bar[g]][k]/(k+2)! as produced by Mathematica for k =0..50
c--- bsh21m1_outside_2_coeff(k,3) = Convolute[Car[Convolute[Car[zeta0],Bar[g]]],Bar[g]][k]/(k+2)! as produced by Mathematica for k =0..50
c--- for the expansion of bsh21m1_outside_2 (see Claude's Mathematica notebook for details)

      double precision function bsh21m1_outside_2_coeff(n,subsum)
      implicit none
      double precision ris, x
      integer n,subsum
      x=0d0
      ris=dlog(x)      
      if (n.gt.60) then
         print*, "Error in bsh21m1_outside_2_coeff: argument out
     &of range!"
      else
         if (subsum.eq.1) then
            select case (n)
            case(0)
               ris = 0d0
            case(1)
               ris = 0.16666666666666666666666666666666666667d0
            case(2)
               ris = 0.0625d0
            case(3)
               ris = 0.00625d0
            case(4)
               ris = -0.00173611111111111111111111111111111111d0
            case(5)
               ris = -0.0003720238095238095238095238095238095238095238d0
            case(6)
               ris = 0.00008680555555555555555555555555555555555555555d0
            case(7)
               ris = 0.00002531828703703703703703703703703703703703703d0
            case(8)
               ris = -5.270337301587301587301587301587301587301587301d-6
            case(9)
               ris = -1.855421777296777296777296777296777296777296777d-6
            case(10)
               ris = 3.5594870664315108759553203997648442092886537330d-7
            case(11)
               ris = 1.4292047169651336318002984669651336318002984669d-7
            case(12)
               ris = -2.576042692610152927613245073562533879994197454d-8
            case(13)
               ris = -1.141697647774036662925551814440703329592218481d-8
            case(14)
               ris = 1.9575544284125633331982538331744680951030157379d-9
            case(15)
               ris = 9.377652132955751799122480728456452079234525546d-10
            case(16)
               ris = -1.54265762652430880678970038170508482089904782d-10
            case(17)
               ris = -7.87378930516677589800211168446571769694391062d-11
            case(18)
               ris = 1.250431147566023893097932590272736207653384400d-11
            case(19)
               ris = 6.729838798725773123942885674738909860118837537d-12
            case(20)
               ris = -1.03659679345023624023841276552509311901686017d-12
            case(21)
               ris = -5.83732280713435942521425063171880270333065017d-13
            case(22)
               ris = 8.75243450378780141526629978995324028067404164d-14
            case(23)
               ris = 5.12613042150025261109974926881767196801198667d-14
            case(24)
               ris = -7.5037517385828254416529200193827226939886356d-15
            case(25)
               ris = -4.5492301052925288773317081500542044501364159d-15
            case(26)
               ris = 6.51676292493487646485761946725077479067199d-16
            case(27)
               ris = 4.074067333402776821589306077188521872096776d-16
            case(28)
               ris = -5.72247981994584093064676643420415091783503d-17
            case(29)
               ris = -3.67749637059494004367499747413198233056543d-17
            case(30)
               ris = 5.0733237513549675432802066525322949695602d-18
            case(31)
               ris = 3.3426753131313663405218780083867215612587d-18
            case(32)
               ris = -4.535604419012368389159448673072010907877d-19
            case(33)
               ris = -3.057098810704015299977460499947598834653d-19
            case(34)
               ris = 4.08491385126889485364061486610298956767d-20
            case(35)
               ris = 2.81131456247794519348890218538578088722d-20
            case(36)
               ris = -3.7032111633096124007473933586098507533d-21
            case(37)
               ris = -2.5980640881031781230925588756627899703d-21
            case(38)
               ris = 3.376923645096436745875516644676212773d-22
            case(39)
               ris = 2.411694256167375948472748428878393445d-22
            case(40)
               ris = -3.09567815001352659100066216972712216d-23
            case(41)
               ris = -2.24774596499909962351377422893932130063330474d-23
            case(42)
               ris = 2.851434312137208946967380799976008071071636359d-24
            case(43)
               ris = 2.102661345997803300418010124529386908178361026d-24
            case(44)
               ris = -2.63788030055063558099450739834596558407312199d-25
            case(45)
               ris = -1.97357187099294818117806748388968264959552358d-25
            case(46)
               ris = 2.450003914953797602211357145346712756286076962d-26
            case(47)
               ris = 1.858142696663828879342589539376359097343693842d-26
            case(48)
               ris = -2.28378312864161800706091922568518524064930875d-27
            case(49)
               ris = -1.75445675011862810331494999376396148044626397d-27
            case(50)
               ris = 2.135959474858800782975074228996498601145096651d-28
            case(51)
               ris = 1.660927324677313d-28
            case(52)
               ris = -2.003869835229258d-29
            case(53)
               ris = -1.576231413170608d-29
            case(54)
               ris = 1.885320018869902d-30
            case(55)
               ris = 1.499258317962172d-30
            case(56)
               ris = -1.778488657974245d-31
            case(57)
               ris = -1.429069647639787d-31
            case(58)
               ris = 1.681853375257285d-32
            case(59)
               ris = 1.364867893550398d-32
            case(60)
               ris = -1.594133505201271d-33
            end select
         else if (subsum.eq.2) then
            select case (n)
            case(0)
               ris = 0d0
            case(1)
               ris = 0.16666666666666666666666666666666666667d0
            case(2)
               ris = 0.05208333333333333333333333333333333333d0
            case(3)
               ris = 0.003125d0
            case(4)
               ris = -0.00147569444444444444444444444444444444d0
            case(5)
               ris = -0.0001550099206349206349206349206349206349206349d0
            case(6)
               ris = 0.00007647156084656084656084656084656084656084656d0
            case(7)
               ris = 9.5445923353909465020576131687242798353909465020d-6
            case(8)
               ris = -4.757943397266313932980599647266313932980599647d-6
            case(9)
               ris = -6.515505376182459515792849126182459515792849126d-7
            case(10)
               ris = 3.2682563064507508951953396397840842285286729731d-7
            case(11)
               ris = 4.7484208861161986161986161986161986161986161986d-8
            case(12)
               ris = -2.394398656592770349384105997862611619225375839d-8
            case(13)
               ris = -3.625125620086075332988913235826816073729653976d-9
            case(14)
               ris = 1.8363724876060713173336000244541438796918766684d-9
            case(15)
               ris = 2.865944021286524860581798171648926765788325226d-10
            case(16)
               ris = -1.45758459564980648288585661065517205504064445d-10
            case(17)
               ris = -2.32838468413785793540685805150612082698324169d-11
            case(18)
               ris = 1.188275213330285863324058748066196688559502719d-11
            case(19)
               ris = 1.933503822472518429028303905487125760668582984d-12
            case(20)
               ris = -9.89703128984630315084770283275148430299430782d-13
            case(21)
               ris = -1.63466881634295585423700697631072893672432258d-13
            case(22)
               ris = 8.38919117457921677154971422554806627693065255d-14
            case(23)
               ris = 1.4028858625293607647507962957618922789775271d-14
            case(24)
               ris = -7.2161079219371504663895580853063849907190713d-15
            case(25)
               ris = -1.2193541093450470767871940015654146809184328d-15
            case(26)
               ris = 6.284684473192181434057846694200533523197916d-16
            case(27)
               ris = 1.071445713069174104023480777322252788621476d-16
            case(28)
               ris = -5.53220031854370089970161533355943161391686d-17
            case(29)
               ris = -9.5042159953734323082857929926737727332588d-18
            case(30)
               ris = 4.9151233333019631790273830042275244812622d-18
            case(31)
               ris = 8.500762771480414138262263524514820723884d-19
            case(32)
               ris = -4.402456940802724998947381524525389375544d-19
            case(33)
               ris = -7.65903580367993977392334931344651701416d-20
            case(34)
               ris = 3.97163472766227844173293394964803271413d-20
            case(35)
               ris = 6.9456639209031970621928762423928316727d-21
            case(36)
               ris = -3.6059045636730985613884153048926249453d-21
            case(37)
               ris = -6.335500036892307702169596451622714972d-22
            case(38)
               ris = 3.292614485266093858157976755436658203d-22
            case(39)
               ris = 5.80929840384842187451229076089266311d-23
            case(40)
               ris = -3.02206141351930248304393910593483084d-23
            case(41)
               ris = -5.35212354986506242999028503157253106429488924d-24
            case(42)
               ris = 2.786700753556020925469228744162599249967573739d-24
            case(43)
               ris = 4.952238709274166904785963604619273167363155055d-25
            case(44)
               ris = -2.58059300109423288908402062151319966482304914d-25
            case(45)
               ris = -4.60031532388009138943578959371630772679068281d-26
            case(46)
               ris = 2.399009383883991454154228990147544359416107737d-26
            case(47)
               ris = 4.288861757985356960856131542245371812569441915d-27
            case(48)
               ris = -2.23814630591791220504907262551205441313633361d-27
            case(49)
               ris = -4.01180434825882647985651364487700288621966957d-28
            case(50)
               ris = 2.094915547694847356164964959498134510377994794d-28
            case(51)
               ris = 3.764176029800685d-29
            case(52)
               ris = -1.966788063967759d-29
            case(53)
               ris = -3.541882057292559d-30
            case(54)
               ris = 1.851676260957409d-30
            case(55)
               ris = 3.341521729152979d-31
            case(56)
               ris = -1.747844273203386d-31
            case(57)
               ris = -3.160251303589404d-32
            case(58)
               ris = 1.653838868422199d-32
            case(59)
               ris = 2.995677564022023d-33
            case(60)
               ris = -1.568435724652945d-33
            end select
         else if (subsum.eq.3) then
            select case (n)
            case(0)
               ris = 0d0
            case(1)
               ris = 0d0
            case(2)
               ris = -0.02083333333333333333333333333333333333d0
            case(3)
               ris = -0.01111111111111111111111111111111111111d0
            case(4)
               ris = -0.00179398148148148148148148148148148148d0
            case(5)
               ris = 0.00028687169312169312169312169312169312169312169d0
            case(6)
               ris = 0.00011584408068783068783068783068783068783068783d0
            case(7)
               ris = -0.0000133877174841129867584894039920494946949973d0
            case(8)
               ris = -8.385052516376921138825900730662635424540186444d-6
            case(9)
               ris = 7.4501316905830794719683608572497461386350275239d-7
            case(10)
               ris = 6.4183911762656802339342021881704421386961069500d-7
            case(11)
               ris = -4.576472400053260743496433732123967814203504439d-8
            case(12)
               ris = -5.105690052170025156694571935415747688131389086d-8
            case(13)
               ris = 3.0023758536825273788340116485809710868015291399d-9
            case(14)
               ris = 4.1809760192550009724127493483358077317909372979d-9
            case(15)
               ris = -2.06426319018391799471824074257640276640469310d-10
            case(16)
               ris = -3.50259415229121623072591878965810352709889832d-10
            case(17)
               ris = 1.469344724927931546487304672039662495221191836d-11
            case(18)
               ris = 2.988669545365678992904570109241692175989438077d-11
            case(19)
               ris = -1.07359378224693038306181812112937919710256311d-12
            case(20)
               ris = -2.58901433074483585606032473323982865370717711d-12
            case(21)
               ris = 8.00156545941041806452689457392190326768742839d-14
            case(22)
               ris = 2.271399178164114895213405848776281902455207003d-13
            case(23)
               ris = -6.0532170724446121374381236062677816708410459d-15
            case(24)
               ris = -2.01431959644371532643466460029071988794889633d-14
            case(25)
               ris = 4.629135976782943110690327054864037531250861d-16
            case(26)
               ris = 1.802957884202160845758528755227202463027618d-15
            case(27)
               ris = -3.56576419588677669801412997549831738079855d-17
            case(28)
               ris = -1.626819717974281949483392173537926207037832d-16
            case(29)
               ris = 2.7571863907530375010068417470755989870398d-18
            case(30)
               ris = 1.47829444861471280152055322506537180984009d-17
            case(31)
               ris = -2.132695411009186348116202505594154403757d-19
            case(32)
               ris = -1.3517474280571352707296633165059683401818d-18
            case(33)
               ris = 1.64383329268649490260464288560571476757d-20
            case(34)
               ris = 1.242930399302229340876537353880584849734d-19
            case(35)
               ris = -1.2566221655297006520478692416158441111d-21
            case(36)
               ris = -1.14858831088312604531477081363631003668d-20
            case(37)
               ris = 9.46753759912258376860597894139681869d-23
            case(38)
               ris = 1.0661909551242729748619110084637011793d-21
            case(39)
               ris = -6.96525296707289731799476687925463796725269937d-24
            case(40)
               ris = -9.93745968623760522811169894597342198d-23
            case(41)
               ris = 4.928382792373506955739146759522020395868166174d-25
            case(42)
               ris = 9.296660828538463431167223952511904037804936080d-24
            case(43)
               ris = -3.25862002708089170143506485310677173568140210d-26
            case(44)
               ris = -8.72673292743260305949887508731407971170326747d-25
            case(45)
               ris = 1.881229628269446371782189071340383133581359339d-27
            case(46)
               ris = 8.217286161228788424228387941832336681437895452d-26
            case(47)
               ris = -7.38731544757496084633531565673841578260034660d-29
            case(48)
               ris = -7.75979892023397204632046427089567866631152084d-27
            case(49)
               ris = -2.13553768361852035110099179942088715089421855d-30
            case(50)
               ris = 7.347224903745779229377634881087824028494465148d-28
            case(51)
               ris = 1.010693554339312d-30
            case(52)
               ris = -6.973693934846783d-29
            case(53)
               ris = -1.680455908837644d-31
            case(54)
               ris = 6.634281682599491d-30
            case(55)
               ris = 2.245009148220748d-32
            case(56)
               ris = -6.324830655307171d-31
            case(57)
               ris = -2.722184704086999d-33
            case(58)
               ris = 6.041809764469975d-32
            case(59)
               ris = 3.126425681094268d-34
            case(60)
               ris = -5.782203202974365d-33
            end select
         else
            x=0d0
            ris=dlog(x)
            print*, "Error in bsh21m1_outside_2_coeff: 'subsum' out
     &of range!"
         endif
      endif
      bsh21m1_outside_2_coeff = ris
      return
      end    
