		j initialisation

debut:	
		lin $t0 1		//entée 1 : marche/pause
		cbeqi $t0 0
		j marche
		j pause
		

marche:	so $zero 12		//pas de réglage courant en mode marche
		lin $t0 0		//entrée 0 : tics du quartz
		cbeq $t0 $in0
		j incrtics
		j debut
		
incrtics:
		lin $in0 0
		incr $qt $qt
		cbeq $qt $k0
		j debut
		li $qt 0

		//gestion unités secondes
		incr $sc0 $sc0
		cbeqi $sc0 10
		j printsc0debut
		li $sc0 0
		sd $sc0 0
		
		//gestion dizaines secondes
		incr $sc1 $sc1
		cbeqi $sc1 6
		j printsc1debut
		li $sc1 0
		sd $sc1 1
		
		//gestion unités minutes
		incr $mn0 $mn0
		cbeqi $mn0 10
		j printmn0debut
		li $mn0 0
		sd $mn0 2
		
		//gestion dizaines minutes
		incr $mn1 $mn1
		cbeqi $mn1 6
		j printmn1debut
		li $mn1 0
		sd $mn1 3
		
		//gestion heures
		incr $hr0 $hr0
		cbeqi $hr0 4
		j suitehrdebut
		cbeqi $hr1 2
		j suitehrdebut
		li $hr0 0
		li $hr1 0
		sd $hr0 4
		sd $hr1 5
		
		//gestion jours
		cbeq $da $nda
		j suitedadebut
		li $da 1
		li $da0 1
		li $da1 0
		sd $da0 6
		sd $da1 7

		//gestion mois
		cbeqi $mo 12
		j suitemodebut
		li $mo 1
		li $mo0 1
		li $mo1 0
		li $nda 31
		sd $mo0 8
		sd $mo1 9
		
		//gestion unités années
		incr $yr $yr
		incr $yr0 $yr0
		cbeqi $yr0 10
		j printyr0debut
		li $yr0 0
		sd $yr0 10
		
		//gestion dizaines années
		incr $yr1 $yr1
		cbeqi $yr1 10
		j printyr1debut
		li $yr1 0
		li $yr 0
		sd $yr1 11
		
		j debut
		
		
printsc0debut:
		sd $sc0 0
		j debut
		
printsc1debut:
		sd $sc1 1
		j debut
		
printmn0debut:
		sd $mn0 2
		j debut
		
printmn1debut:
		sd $mn1 3
		j debut
		
suitehrdebut:		
		cbeqi $hr0 10
		j printhr0debut
		li $hr0 0
		incr $hr1 $hr1
		sd $hr1 5
printhr0debut:
		sd $hr0 4
		j debut		
		
suitedadebut:
		incr $da $da
		incr $da0 $da0
		cbeqi $da0 10
		j printda0debut
		li $da0 0
		incr $da1 $da1
		sd $da1 7
printda0debut:
		sd $da0 6
		j debut
		
suitemodebut:
		incr $mo $mo
		incr $mo0 $mo0
		cbeqi $mo0 10
		j printmo0
		li $mo0 0
		incr $mo1 $mo1
		sd $mo1 9
printmo0:	
		sd $mo0 8
//actualisation de $nda
		cbeqi $nda 31 
		j set31		//après un mois à <31 jours vient toujours un mois à 31 jours
		//après un mois à 31 jours, vient un à 30 jours, sauf août, février (janvier a déjà été testé)
		cbeqi $mo 8 //août ?
		j pasaout
		j set31
pasaout:
		cbeqi $mo 2 //février ?
		j set30
fevrier:
		li $nda 29
		modf $yr $t0
		cbeq $t0 $zero //année bissextile?
		li $nda 28
		j debut
set31:	li $nda 31
		j debut
set30:	li $nda 30
		j debut
		
printyr0debut:
		sd $yr0 10
		j debut
		
printyr1debut:
		sd $yr1 11
		j debut
		
/////////////

initialisation:
		li $zero 0
		li $in0 0
		li $in1 0
		li $in2 0
		li $in3 0
		li $in4 0
		li $in5 0
		lbi 32		//$k0
reset:	li $qt 0
		li $sc0 0
		li $sc1 0
		li $mn0 0
		li $mn1 0
		li $hr0 0
		li $hr1 0
		li $da0 1
		li $da1 0
		li $da 1
		li $mo0 1
		li $mo1 0
		li $mo 1
		li $yr0 0
		li $yr1 0
		li $yr 0		//initialiser à quelle année ? actellement 2000
		li $nda 31
		li $cr 1
		sd $sc0 0
		sd $sc1 1 
		sd $mn0 2
		sd $mn1 3
		sd $hr0 4
		sd $hr1 5
		sd $da0 6
		sd $da1 7
		sd $mo0 8
		sd $mo1 9
		sd $yr0 10
		sd $yr1 11
		so $cr 12
		j debut
		
/////////////

pause:// réglage possible
		so $cr 12		//pause : affichage du réglage courant
		
		lin $t0 2		//entée 2 : changer réglage courant
		cbeq $t0 $in2
		j changereglage
		
		lin $t0 3		//entée 3 : incrémenter grandeur du réglage courant
		cbeq $t0 $in3
		j incrreglage
		
		lin $t0 4		//entée 4 : reset
		cbeq $t0 $in4
		j reset0
		
		lin $t0 5		//entée 5 : changer vitesse
		cbeq $t0 $in5
		j changevitesse

		j debut		


reset0:	lin $in4 4
		j reset
		
changereglage:
		lin $in2 2
		incr $cr $cr
		cbeqi $cr 7
		j changereglagesuite
		li $cr 1
changereglagesuite:		
		so $cr 12
		j debut
		
incrreglage:
		lin $in3 3
		cbeqi $cr 1
		j crsup2
		j incrsecondes
crsup2:	cbeqi $cr 2
		j crsup3
		j incrminutes
crsup3:	cbeqi $cr 3
		j crsup4
		j incrheures
crsup4:	cbeqi $cr 4
		j crsup5
		j incrannees
crsup5:	cbeqi $cr 5
		j crsup6
		j incrmois
crsup6:	j incrjours

incrsecondes:
		incr $sc0 $sc0
		cbeqi $sc0 10
		j printsc0debut
		li $sc0 0
		sd $sc0 0
		incr $sc1 $sc1
		cbeqi $sc1 6
		j printsc1debut
		li $sc1 0
		sd $sc1 1
		j debut
		
incrminutes:
		incr $mn0 $mn0
		cbeqi $mn0 10
		j printmn0debut
		li $mn0 0
		sd $mn0 2
		incr $mn1 $mn1
		cbeqi $mn1 6
		j printmn1debut
		li $mn1 0
		sd $mn1 3
		j debut
		
incrheures:
		incr $hr0 $hr0
		cbeqi $hr0 4
		j suitehrdebut
		cbeqi $hr1 2
		j suitehrdebut
		li $hr0 0
		li $hr1 0
		sd $hr0 4
		sd $hr1 5
		j debut
		
incrjours:
		cbeq $da $nda
		j suitedadebut
		li $da 1
		li $da0 1
		li $da1 0
		sd $da0 6
		sd $da1 7
		j debut
		
incrmois:
		li $da 1     //on met le jour à 1, pour ne pas avoir de problème si on était le 31/01 par exemple
		li $da0 1
		li $da1 0
		sd $da0 6
		sd $da1 7
		cbeqi $mo 12
		j suitemodebut
		li $mo 1
		li $mo0 1
		li $mo1 0
		li $nda 31
		sd $mo0 8
		sd $mo1 9
		j debut
		
incrannees:
		incr $yr $yr
		incr $yr0 $yr0
		cbeqi $yr0 10
		j printyr0debut
		li $yr0 0
		sd $yr0 10
		incr $yr1 $yr1
		cbeqi $yr1 10
		j printyr1debut
		li $yr1 0
		li $yr 0
		sd $yr1 11
		cbeqi $mo 2
		j debut
//Si on incrémente l'année en mode réglage et qu'on est en février, il faut mettre à jour le nombre de jours du mois courant. Si on était un 29 février, on devient le 28.
		cbeqi $da 29
		j fevrier
		li $da 28
		li $da0 8
		li $da1 2
		sd $da0 6
		sd $da1 7
		j fevrier
		
		
changevitesse:
		lin $in5 5
		lbi 32
		cbeq $in5 $zero
		lbi 1
		li $qt 0
		j debut
