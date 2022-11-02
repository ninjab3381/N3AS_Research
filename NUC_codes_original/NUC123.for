
C                             FILE: NUC123.for

C========================IDENTIFICATION DIVISION=====================        

                              PROGRAM nuc123             
 
*********************************************************************      
*                          Modified by thomas topel                 *  
*                               August, 2003                        *
*								    *
*          MODIFICATIONS.					    *	 	                  							
*      All added comments take the form C//// beginning in          *
*      the left-most column.					    *
*      For lengthier commands, where lines need to be continued,    *
*      'a' denotes line continuation (must appear in column six).   *
*     								    *
*          NOTES ON COMPILING/RUNNING.                              *
*      To compile a single file: g77 filename.for                   *
*      To link and compile all                    		    *
*      four files simultaneously: g77 NU*.for	                    *
*      To run from the command prompt: ./a.out   		    *
*      Before each run, it is helpful to use                        *
*      the command: rm a.out nuc123.dat nucint.dat.                 *
*      This removes the listed files. In particular, nuc123.dat     *
*      and nucint.dat can not exist at the time of compilation      *
*      (there OPEN statements specify status='new').                *
*  								    *
*********************************************************************


C----------LINKAGES.
C     CALLED BY - none
C     CALLS     - [subroutine] help, setcom, setmod, run, output

C----------REMARKS.
C     Control program -
C     Offers user the main menu and channels through to various options
C     Implementation -
C       To run this program, NUC123.FOR must be linked with NUCCOM
C       (containing the computation subroutines), NUCRAT (with the
C       reaction rates), and NUCINT (with an interface subroutine).
C       This program has been written to be compatible with
C       ANSI FORTRAN-77 with the exception of the END DO statement
C       used to limit the number of statement labels.
C       The code was developed on the VAX/VMS system.
C     Notes -
C       The program utilizes Wagoner's code as the core of the 
C       computational routines.
C     Documentation -
C       Kawano, L., 1992, Fermilab preprint FERMILAB-PUB-92/04-A,
C       Kellogg Radiation Lab preprint OAP-714.
C     Copy -
C       Version 4.1 (December 1991) 
C////// as described in LET'S GO: EARLY UNIVERSE II

C----------PARAMETERS.
       PARAMETER (ir=1)             !Input unit number. 'r'=read
       PARAMETER (iw=1)             !Output unit number. 'w'=write
       PARAMETER (nrec=88)          !Number of nuclear reactions.
       PARAMETER (nnuc=26)          !Number of nuclides in calculation.

C----------COMMON AREAS.
       COMMON /recpr0/ reacpr               !Reaction parameter values. 
       COMMON /recpr/  iform,ii,jj,kk,ll    
     a  ,rev,q9                             !Reaction parameter names.
       COMMON /rates/  f,r                  !Reaction rates.
       COMMON /compr0/ cy0,ct0,t9i0,t9f0    
     a ,ytmin0,inc0  			    !Default comp parameters.	
       COMMON /compr/  cy,ct,t9i,t9f,ytmin,inc 
       				            !Computation parameters.
       COMMON /modpr0/ c0,cosmo0,xi0        !Default model parameters.
       COMMON /modpr/  g,tau,xnu,c,cosmo,xi !Model parameters.
       COMMON /varpr0/ dt0,eta0             !Default variationl params.         
       COMMON /varpr/  dt1,eta1             !parameters.
       
       COMMON /checki/  itime               !Computation location. 
                                            
C//////Changed the name of the above list from 'check' to 'checki',
C      as 'check' is the name of a subroutine.
                                           
       
       COMMON /runopt/ irun,isize,jsize     !Run options.
       COMMON /outopt/ nout,outfile         !Output option.


C==========================DECLARATION DIVISION===================

C----------REACTION PARAMETERS FROM BLOCK DATA.
       REAL    reacpr(nrec,8)       !Reaction parameters.

C----------REACTION PARAMETERS.
       INTEGER iform(nrec)          !Reaction type code (1-11).
       INTEGER ii(nrec)             !Incoming nuclide type (1-26).
       INTEGER jj(nrec)             !Incoming light nuclide type (1-6).        
       INTEGER kk(nrec)             !Outgoing light nuclide type (1-6).
       INTEGER ll(nrec)             !Outgoing nuclide type (1-26).
       REAL    rev(nrec)            !Reverse reaction coefficient.
       REAL    q9(nrec)             !Energy released in reaction.

C----------REACTION RATES.
       REAL    f(nrec)              !Forward reaction rate coefficients.        
       REAL    r(nrec)              !Reverse reaction rate coefficients.

C----------DEFAULT COMPUTATION PARAMETERS.
       REAL    cy0                  !Default cy.
       REAL    ct0                  !Default ct.
       REAL    t9i0                 !Default t9i.
       REAL    t9f0                 !Default t9f.
       REAL    ytmin0               !Default ytmin.
       INTEGER inc0                 !Default accumulation increment.

C----------COMPUTATIONAL PARAMETERS.
       REAL    cy          !Time step limiting constant on abundances.
       REAL    ct          !Time step limiting constant on temperature.
       REAL    t9i         !Initial temperature (in 10**9 K).
       REAL    t9f         !Final temperature (in 10**9 k).
       REAL    ytmin       !Smallest abundances allowed.
       INTEGER inc         !Accumulation increment.

C----------DEFAULT MODEL PARAMETERS.
       REAL    c0(3)       !Default c.
       REAL    cosmo0      !Default cosmological constant.
       REAL    xi0(3)      !Default neutrino degeneracy parameters.

C----------EARLY UNIVERSE MODEL PARAMETERS.
       REAL    c(3)        !c(1) is variation of gravitational constant.       
                           !c(2) is neutron lifetime (sec).
                           !c(3) is number of neutrino species.
       REAL    cosmo       !Cosmological constant.
       REAL    xi(3)       !Neutrino degeneracy parameters.

C----------DEFAULT VARIATIONAL PARAMETERS.
       REAL    dt0                  !Default initial time step.
       REAL    eta0                 !Default baryon-to-photon ratio.

C----------VARIATIONAL PARAMETERS.
       REAL    dt1                  !Initial time step.
       REAL    eta1                 !Baryon-to-photon ratio.

C----------COMPUTATION LOCATION.
       INTEGER itime                !Time check.

C----------RUN OPTION.
       INTEGER irun                 !Run network size.
       INTEGER isize                !Number of nuclides in computation.
       INTEGER jsize                !Number of reactions in computation.

C----------OUTPUT FILE STATUS.
       INTEGER nout                 !Number of output requests.
       LOGICAL outfile              !Indicates if output file used.

C----------USER RESPONSE VARIABLES.
       INTEGER inum                 !Selection number.


C===========================PROCEDURE DIVISION========================

C10--------OPEN FILES AND PRINT GREETING------------------------------

C//////OPEN (unit=1, file='SYS$COMMAND', status='old') !User terminal.

C//////This opens an existing file, i.e. one that must exist before compilation     						       
  
C//////    Rather than opening a file 'SYS$COMMAND' to read from/write to,       						      	
C      the code has been altered to use the standard I/O devices, 
C      i.e. keyboard input and screen output. This was implemented on a SUN
C      machine, which has as I/O standards the keyboard and creates an a.out
C      file for output (not unique to the SUN system). The advantage here is
C      that in reading from the a.out file, the <RETURN> button is recognized.
C          Since the code now reads/writes via the terminal, all READ and
C      WRITE statements with reference to SYS$COMMAND (unit 1) have been 
C      changed to the form READ/WRITE(*,___), indicating that lines are 
C      read from/written to the a.out file.

       
       OPEN (unit=2, file='nuc123.dat', status='new')  !Output file.
    
C//////This creates a file 'nuc123.dat' which cannot exist at compilation.       
C     If an output file is requested, the computations are written to this file.
      
       
       itime = 1                    !Time = beginning of program.
       CALL check                   !Check interface subroutine.
       
       
       
       WRITE (*,1000)              
1000   FORMAT (6(/),
     a        2(' ',4x,'NN',6x,'NN  UU',6x,'UU',4x,8('C'),6x,'11',8x,
     a        6('2'),6x,6('3'),/),
     a        2(' ',4x,'NN',6x,'NN  UU',6x,'UU  CC',12x,'1111',6x,
     a        '22',6x,'22  33',6x,'33',/),
     a        2(' ',4x,'NNNN    NN  UU',6x,'UU  CC',14x,'11',14x,
     a        '22',10x,'33',/),
     a        2(' ',4x,'NN  NN  NN  UU',6x,'UU  CC',14x,'11',12x,
     a        '22',10x,'33',/),
     a        2(' ',4x,'NN    NNNN  UU',6x,'UU  CC',14x,'11',10x,
     a        '22',14x,'33',/),
     a        2(' ',4x,'NN',6x,'NN  UU',6x,'UU  CC',14x,'11',8x,
     a        '22',8x,'33',6x,'33',/),
     a        2(' ',4x,'NN',6x,'NN  ',10('U'),4x,8('C'),4x,6('1'),4x,
     a        10('2'),4x,6('3'),/),/,
     a        ' ',26x,'WRITTEN BY LAWRENCE KAWANO',///,
     a        ' ','(Press <RETURN> to continue): ',$)
     
            

C20--------INPUT INITIALIZATION INFORMATION AND PAUSE-----------------

       DO i  = 1,nrec
C..........READ IN REACTION PARAMETERS.
         iform(i) = int(reacpr(i,2))!Reaction type.
         ii(i)    = int(reacpr(i,3))!Incoming nuclide type.
         jj(i)    = int(reacpr(i,4))!Incoming nuclide type.
         kk(i)    = int(reacpr(i,5))!Outgoing nuclide type.
         ll(i)    = int(reacpr(i,6))!Outgoing nuclide type.
         rev(i)   = reacpr(i,7)     !Reverse reaction coefficient.
         q9(i)    = reacpr(i,8)     !Energy released.
C..........INITIALIZE REACTION RATES.
         f(i)  = 0.                 !Forward rate coeff.
         r(i)  = 0.                 !Reverse rate coeff.
C..........SET RUN OPTIONS TO DEFAULT.
       END DO
       irun       = 1               !Do full run.
       isize      = nnuc            !Use all 26 nuclides.
       jsize      = nrec            !Use all 88 reactions.
C..........SET OUTPUT OPTION TO DEFAULT.
       nout    = 0                  !No output requests.
       outfile = .false.            !Output file not used.
C..........SET VALUES TO DEFAULT.
       cy    = cy0         !Time step limiting constant on abundances.
       ct    = ct0         !Time step limiting constant on temperature.
       t9i   = t9i0        !Initial temperature.
       t9f   = t9f0        !Final temperature.
       ytmin = ytmin0      !Smallest abundances allowed.
       inc   = inc0        !Accumulation increment.
       c(1)  = c0(1)       !Variation of gravitational constant.
       c(2)  = c0(2)       !Neutron lifetime.
       c(3)  = c0(3)       !Number of neutrino species.
       cosmo = cosmo0      !Cosmological constant.
       xi(1) = xi0(1)      !Electron degeneracy parameter.
       xi(2) = xi0(2)      !Muon degeneray parameter.
       xi(3) = xi0(3)      !Tauon degeneracy parameter.
       dt1   = dt0         !Initial time step.
       eta1  = eta0        !Baryon-to-photon ratio.
       
C..........ACCEPT <RETURN> TO CONTINUE.
       READ (*,*)       !Pause.    
       
C//////This statement reads in <RETURN> from the keyboard          
         
C30--------PRINT MENU AND AWAIT RESPONSE----------------------------

C..........RETURN FROM LOOPING.
300    CONTINUE
C..........DISPLAY MENU.
       
       WRITE (*,3000)                            
3000   FORMAT (8(/),
     a               ' ',32x,'MENU SELECTION',/,
     a          ' ',32x,'---- ---------',//,
     a          ' ',24x,'1. HELP',/,
     a          ' ',24x,'2. SET COMPUTATION PARAMETERS',/,
     a          ' ',24x,'3. SET MODEL PARAMETERS',/,
     a          ' ',24x,'4. RUN',/,
     a          ' ',24x,'5. OUTPUT',/,
     a          ' ',24x,'6. EXIT',8(/),
     a          ' ',24x,'Enter selection (1-6): ',$)
C..........READ IN SELECTION NUMBER.
			
       READ (*,3001) inum                    
3001   FORMAT(i1)

C       END IF

C40--------BRANCH TO APPROPRIATE SECTION-----------------------------

       GO TO (410,420,430,440,450,460),inum
       GO TO 460                !Improper input or <RETURN>.
410    CONTINUE                 !Help section.
         CALL help
         GO TO 500
420    CONTINUE                 !Set computation parameters section.
         CALL setcom
         GO TO 500
430    CONTINUE                 !Set model parameters section.
         CALL setmod
         GO TO 500
440    CONTINUE                 !Run section.
         itime = 2              !Time = beginning of run section.
         CALL check             !Check interface subroutine.
         CALL run
         itime = 9              !Time = end of run section.
         CALL check             !Check interface subroutine.
         GO TO 500
450    CONTINUE                 !Output section.
         CALL output
         GO TO 500
460    CONTINUE                 !Exit section.
         IF (outfile) THEN
           CLOSE (unit=2,status='keep')     !Close output file.
         ELSE
           CLOSE (unit=2,status='delete')   !File not used - dispose.
         END IF
         CLOSE (unit=1)         !End terminal session.
         itime = 10             !Time = end of program.
         CALL check             !Check interface subroutine.
         STOP

C50---------GO BACK TO MENU-------------------------------------------

500    CONTINUE
       GO TO 300

       END



C========================IDENTIFICATION DIVISION=====================

       SUBROUTINE help

C----------LINKAGES.
C     CALLED BY - [program] nuc123
C     CALLS     - none

C----------REMARKS.
C     Displays description and workings of the program.

C----------PARAMETERS.
       PARAMETER (ir=1)             !Input unit number.
       PARAMETER (iw=1)             !Output unit number.


C==========================DECLARATION DIVISION=====================

C----------USER RESPONSE VARIABLES.
       INTEGER inum                 !Selection number.


C===========================PROCEDURE DIVISION===================

C10--------PRINT HELP SELECTION-----------------------------------

C..........RETURN FROM LOOPING.

100    CONTINUE

C..........DISPLAY MENU.
       WRITE (*,1000)			
1000   FORMAT (8(/),
     a          ' ',32x,'HELP SELECTION',/,
     a          ' ',32x,'---- ---------',//,
     a          ' ',24x,'1. INTRODUCTION',/,
     a          ' ',24x,'2. SETTING UP A RUN',/,
     a          ' ',24x,'3. RUNNING THE PROGRAM',/,
     a          ' ',24x,'4. OUTPUT OPTIONS',/,
     a          ' ',24x,'5. GENERAL METHOD OF COMPUTATION',/,
     a          ' ',24x,'6. USING THE INTERFACE SUBROUTINE',/,
     a          ' ',24x,'7. EXIT',7(/),
     a          ' ',24x,'Enter selection (1-7): ',$)
C..........READ IN SELECTION NUMBER.
   
       READ (*,1001) inum				
1001   FORMAT (i1)

C20--------BRANCH TO APPROPRIATE SECTION-------------------------

       GO TO (210,220,230,240,250,260,270),inum
       GO TO 270                    !Improper input or <RETURN>.

C21--------INTRODUCTION SECTION----------------------------------

210    CONTINUE                     !Setting up a run section.
         WRITE (*,2100)		    
2100   FORMAT (/,
     a            ' ',31x,'INTRODUCTION',/,
     a            ' ',31x,'------------',2(/),
     a            ' ','Welcome to the wonderful world of primor',
     a                'dial nucleosynthesis.  NUC123 is a      ',/,
     a            ' ','FORTRAN program designed to provide the ',
     a                'early universe researcher with the tools',/,
     a            ' ','necessary for the investigation of primo',
     a                'rdial nucleosynthesis.  Its menu-driven ',/,
     a            ' ','interface allows the user to first set c',
     a                'omputation parameters (such as the time ',/,
     a            ' ','step) and model parameters (such as the ',
     a                'neutron lifetime and number of neutri-  ',/,
     a            ' ','nos) before doing single runs or multipl',
     a                'e runs (in which desired model parame-  ',/,
     a            ' ','ters are varied over a desired range.)  ',
     a                'After the run, the user can utilize the ',/,
     a            ' ','menu to either produce an output file or',
     a                ' to view the most recent run on the     ',/,
     a            ' ','screen.  The program comes with an empty',
     a                ' subroutine CHECK into which the user   ',/,
     a            ' ','may wish to put additional code to add t',
     a                'o the computation in an original manner.',10(/),        
     a            ' ','(Enter <RETURN> to go back to help menu): ',$)
         READ (*,*)	       							
         GO TO 300             

C22--------SET UP RUN SECTION-----------------------------------
220    CONTINUE                     !Setting up a run section.
         WRITE (*,2200)								
2200   FORMAT (/,
     a          ' ',29x,'SETTING UP A RUN',/,
     a          ' ',29x,'------- -- - ---',2(/),
     a          ' ','I. Setting computation parameters.      ',/,
     a          ' ','   The accuracy of the computation and t',
     a              'he relevant temperature region can be   ',/,
     a          ' ','   set by the following parameters:     ',/,
     a          ' ','    A. Time step limiting constant 1  (d',
     a              'efault value of 0.3)                    ',/,
     a          ' ','    B. Time step limiting constant 2  (d',
     a              'efault value of 0.03)                   ',/,
     a          ' ','    C. Initial time step  (default value',
     a              ' of 10**-4)                             ',/,
     a          ' ','    D. Initial temperature  (default val',
     a              'ue of 10**2)                            ',/,
     a          ' ','       This is the temperature at the be',
     a              'ginning of the run in units of 10**9 K  ',/,
     a          ' ','    E. Final temperature  (default value',
     a              ' of 10**-2)                             ',/,
     a          ' ','       This is the termination temperatu',
     a              're of the run in units of 10**9 K       ',/,
     a          ' ','    F. Smallest abundances allowed  (def',
     a              'ault value of 10**-25)                  ',/,
     a          ' ','       Elemental abundances are not allo',
     a              'wed to drop below this value            ',/,
     a          ' ','    G. # of iterations for each accumula',
     a             'tion  (default value of 30)             ',/,
     a         ' ','       This is the number of iterations ',
     a              'before values are put in an output array',6(/),
     a          ' ','(Enter 1 to continue, <RETURN> to end): ',$)
         READ (*,1001) inum						
         IF (inum.eq.1) THEN
           WRITE (*,2202)						
2202     FORMAT (/,
     a            ' ','II. Setting model parameters.           ',/,          
     a            ' ','   Default values here give what is know',
     a                'n as the standard model with best guess ',/,
     a            ' ','   figure on the neutron lifetime of 888',
     a                '.541 seconds.  Nonstandard scenarios can',/,
     a            ' ','   be investigated by varying the follow',
     a                'ing parameters:                         ',/,
     a            ' ','    A. The gravitational constant       ',/,
     a            ' ','       (The default value of one here gi',
     a                'ves the usual 6.6720e-8 dyne*cm**2/g**2)',/,
     a            ' ','    B. Neutron life-time  (default value',
     a                ' of 888.541 seconds)                    ',/,
     a            ' ','    C. Number of neutrino species  (defa',
     a                'ult value of 3 light neutrinos)         ',/,
     a            ' ','    D. Final baryon-to-photon ratio  (se',
     a                't to log(eta) = -9.5)                   ',/,
     a            ' ','    E. Cosmological constant  (default v',
     a                'alue of 0)                              ',/,
     a            ' ','    F. Neutrino degeneracy parameters  (',
     a                'default values all 0)                   ',/,
     a            ' ','       There are 3 separate parameters f',
     a                'or the electron, muon, and tau neutrinos',11(/),        
     a         ' ','(Enter <RETURN> to go back to help menu): ',$)
           READ (*,*)							
           GO TO 300
         ELSE
           GO TO 300
         END IF !(inum.eq.1)

C23--------RUN PROGRAM SECTION----------------------------------------

230     CONTINUE                     !Running the program section.
         WRITE (*,2300)							
2300     FORMAT (/,
     a          ' ',28x,'RUNNING THE PROGRAM',/,
     a          ' ',28x,'------- --- -------',2(/),
     a          ' ','I. Setting run speed.                   ',/,
     a          ' ','   The code can be run at 3 different se',
     a              'ttings of speed.  The running of the    ',/,
     a          ' ','   code can be speeded up by reducing th',
     a              'e number of nuclides and reactions.  The',/,
     a          ' ','   complete computation takes into accou',
     a              'nt the following nuclides: n, p, d, t,  ',/,
     a          ' ','   He3, He4, Li6, Li7, Be7, Li8, B8, Be9',
     a              ',B10, B11, C11, B12, C12, N12, C13, N13,',/,
     a          ' ','   C14, N14, O14, N15, O15, and O16.    ',/,
     a          ' ','   The given CPU percentages and abundan',
     a              'ce variations are with regard to a      ',/,
     a          ' ','   single run with all default parameter',
     a              ' values.                                ',/,
     a          ' ','    A. 26 nuclides, 88 reactions (defaul',
     a              't)                                      ',/,
     a          ' ','       nuclides from n to O16           ',/,
     a          ' ','    B. 18 nuclides, 60 reactions        ',/,
     a          ' ','       nuclides from n to N12           ',/,
     a          ' ','       (63% CPU time, variation = .1%)  ',/,
     a          ' ','    C.  9 nuclides, 25 reactions        ',/,
     a          ' ','       nuclides from n to Be7           ',/,
     a          ' ','       (20% CPU time, variation = .5%)  ',4(/),        
     a          ' ','(Enter 1 to continue, <RETURN> to end): ',$)
        READ (*,1001) inum							
         IF (inum.eq.1) THEN
           WRITE (*,2302)						
2302       FORMAT (/,
     a            ' ','II. Do single run.                      ',/,
     a            ' ','    A. Interactive.                     ',/,
     a            ' ','       In an interactive session, the us',
     a                'er can readily input the computational  ',/,
     a            ' ','       and model parameters and begin th',
     a                'e computation process.  The run itself  ',/,
     a            ' ','       is commenced when option 2, "GO",',
     a                ' in the "RUN" section is requested.     ',//,
     a            ' ','    B. Batch.                           ',/,
     a            ' ','       To run the program in a batch mod',
     a                'e, it must be altered slightly so that  ',/,
     a            ' ','       the I/O takes place with files in',
     a                'stead of a terminal.  This is done by   ',/,
     a            ' ','       setting different values for the ',
     a                'input and output unit number parameters ',/,
     a            ' ','       "ir" and "iw" and assigning them ',
     a                'to different files in NUC123.  In the   ',/,
     a            ' ','       file assigned the "ir" unit numbe',
     a                'r, one must place the responses to the  ',/,
     a            ' ','       queries of the program.          ',10(/),                 
     a            ' ','(Enter 1 to continue, <RETURN> to end): ',$)
           READ (*,1001) inum						
           IF (inum.eq.1) THEN
             WRITE (*,2304)						
2304         FORMAT (/,
     a              ' ','III. Do multiple runs.                 ',/,
     a              ' ','   A wide range of early universe model',
     a                  's can be covered by doing many runs    ',/,
     a              ' ','   while one or more parameters are var',
     a                  'ied over a range of interest.  The     ',/,
     a              ' ','   parameters that can be varied are th',
     a                  'e following:                           ',/,
     a              ' ','    A. Eta                             ',
     a                  '       - Logrithmic variation          ',/,
     a              ' ','    B. Gravitational constant          ',
     a                  '       - Linear variation              ',/,
     a              ' ','    C. Neutron lifetime                ',
     a                 '       - Linear variation              ',/,
     a              ' ','    D. Number of neutrino species      ',
     a                  '       - Linear variation              ',/,
     a              ' ','    E. Cosmological constant           ',
     a                  '       - Linear variation              ',/,
     a              ' ','    F. Neutrino degeneracy parameters  ',
     a                  '       - Linear variation              ',/,
     a              ' ','        1. Electron neutrino           ',/,
     a              ' ','        2. Muon neutrino               ',/,
     a              ' ','        3. Tauon neutrino              ',/,
     a              ' ','   At most 3 parameters can be varied. ',
     a                  ' The first parameter inputted will be  ',/,
     a              ' ','   will be varied in the outermost loop',
     a                  ' and the third parameter inputted will ',/,
     a              ' ','   be varied in the innermost loop.    ',7(/),
     a              ' ','(Enter <RETURN> to go back to help menu): ',$)        
             READ (*,*)								
             GO TO 300
           ELSE
             GO TO 300
           END IF !(inum.eq.1)
         ELSE
           GO TO 300
         END IF !(inum.eq.1)

C24--------OUTPUT OPTIONS SECTION--------------------------------------        

240    CONTINUE                     !Output options section.
         WRITE (*,2400)							
2400     FORMAT (/,
     a          ' ',30x,'OUTPUT OPTIONS',/,
     a          ' ',30x,'------ -------',2(/),
     a          ' ','I.  Request output file.                ',/,
     a          ' ','   After a run, the user can request the',
     a              ' program to put the resulting numbers   ',/,
     a          ' ','   into an output file.  This can be don',
     a              'e as many times as desired and all the  ',/,
     a          ' ','   information will be put in one new fi',
     a              'le under the name of "NUC123.DAT."  If  ',/,
     a          ' ','   there is no request during the entire',
     a              ' running of the program, this file is   ',/,
     a          ' ','   not created.  If an output file is re',
     a              'quested after a multiple run, only the  ',/,
     a          ' ','   information from the very last run wi',
     a              'll be given.  The output file will give ',/,
     a          ' ','   the computational and model parameter',
     a              's for each run and will contain the     ',/,
     a          ' ','   following information:               ',/,
     a          ' ','    A. Temperatures in decreasing order ',/,
     a          ' ','    B. Abundances for n, p, d, t, He3, H',
     a              'e4, Li6, Li7, Be7, and Li8 & up         ',/,
     a          ' ','       (p and He4 are in mass fraction, ',
     a              'the rest in ratios to the p abundance)  ',/,
     a          ' ','    C. Time, time interval, chemical pot',
     a              'ential of the electron                  ',/,
     a          ' ','    D. Energy densities for photons, ele',
     a              'ctrons, electron neutrinos, and baryons ',/,
     a          ' ','    E. Baryon-to-photon ratio, expansion',
     a              ' rate of the universe                   ',5(/),
     a          ' ','(Enter 1 to continue, <RETURN> to end): ',$)
         READ (*,1001) inum						
         IF (inum.eq.1) THEN
           WRITE (*,2402)						
2402       FORMAT (/,
     a            ' ','II.  Request output on screen.         ',/,
     a            ' ','   Instead of waiting to print out an o',
     a                'utput file, the user can immediately   ',/,
     a            ' ','   access the results of the latest run',
     a                ' by requesting the output on the       ',/,
     a            ' ','   screen.  There are four screens on e',
     a                'ach of which are displayed the         ',/,
     a            ' ','   computational and model parameters a',
     a                'nd the temperature:                    ',/,
     a            ' ','    A. Abundances for d, t, He3, He4, a',
     a                'nd Li7                                 ',/,
     a            ' ','       (He4 in mass fraction, rest as a',
     a                ' ratio with the p abundance)           ',/,
     a            ' ','    B. Abundances for n, p, Li6, Be7, a',
     a                'nd Li8 & up                            ',/,
     a            ' ','       (p in mass fraction, rest as a r',
     a                'atio with the p abundance)             ',/,
     a            ' ','    C. Energy densities for photons, el',
     a                'ectrons, electron neutrinos, & baryons ',/,
     a            ' ','    D. Time, time interval, chemical po',
     a                'tential of the electron,               ',/,
     a            ' ','       baryon-to-photon ratio, and expa',
     a                'nsion rate of the universe             ',11(/),
     a            ' ','(Enter <RETURN> to go back to help menu): ',$)
           READ (*,*)							
           GO TO 300
         ELSE
           GO TO 300
         END IF !(inum.eq.1)

C25--------METHOD OF COMPUTATION SECTION-----------------------------

250    CONTINUE               !General method of computation section.
         WRITE (*,2500)							
2500     FORMAT (/,
     a          ' ',22x,'GENERAL METHOD OF COMPUTATION',/,
     a          ' ',22x,'------- ------ -- -----------',2(/),
     a          ' ','I. Time evolution algorithm.            ',/,
     a          ' ','   The program utilizes a 2-point Runge-',
     a              'Kutta scheme (located in subroutine     ',/,
     a          ' ','   DRIVER) to time-evolve the temperatur',
     a              'e, the quantity hv (the ratio of the    ',/,
     a          ' ','   baryon density to T**3), the chemical',
     a              ' potential of the electron, and the     ',/,
     a          ' ','   nuclide abundances.  In the 2-point R',
     a              'unge-Kutta routine, a variable v at time',/,
     a          ' ','   t0 (= v0) is evolved to a time t1 by ',
     a              'adding to v0 the average of the         ',/,
     a          ' ','   derivatives evaluated at t0 and at t1',
     a              ' multiplied by dt:                      ',/,
     a          ' ','       v1 = v0 + 0.5(dvdt(t0)+dvdt(t1)) ',/,
     a          ' ','   where dvdt(t1) is gotten by first fin',
     a              'ding v1'' = v0 + dvdt(t0).  The         ',/,
     a          ' ','   derivatives of the nuclide abundances',
     a              ' are first computed and these are used  ',/,
     a          ' ','   to find the derivatives of t9, hv, an',
     a              'd phie (this is done in subroutine      ',/,
     a          ' ','   DERIVS).  To compute the time derivat',
     a              'ives of the nuclide abundances, a matrix',/,
     a          ' ','   equation is set up (in subroutine SOL',
     a              ') and is solved (in subroutine EQSLIN)  ',/,
     a          ' ','   by gaussian elimination utilizing imp',
     a              'licit differentiation.                  ',6(/),
     a          ' ','(Enter 1 to continue, <RETURN> to end): ',$)
         READ (*,1001) inum						
         IF (inum.eq.1) THEN
           WRITE (*,2502)						
2502       FORMAT (/
     a            ' ','II. Hierarchy of Subroutines.   ',/,
     a            ' ','    NUC123                       ',
     a                '     Main program (main menu)    ',/,
     a            ' ','        HELP                     ',
     a                '     Help option                 ',/,
     a            ' ','        SETCOM                   ',
     a                '     Set computational parameters',/,
     a            ' ','        SETMOD                   ',
     a                '     Set model parameters        ',/,
     a            ' ','        RUN                      ',
     a                '     Run computation code        ',/,
     a            ' ','            DRIVER               ',
     a                '     Main routine (Runge-Kutta loop)    ',/,
     a            ' ','                START            ',
     a                '     Initialization routine      ',/,
     a            ' ','                    RATE0        ',
     a                '     Computes weak decay rates   ',/,
     a            ' ','                DERIVS           ',
     a                '     Computes time derivatives   ',/,
     a            ' ','                    THERM        ',
     a                '     Computes energy densities   ',/,
     a            ' ','                        BESSEL   ',
     a                '     Gives functions of Kn       ',/,
     a            ' ','                            KNUX ',
     a                '     Computes modified Bessel fcn Kn    ',/,
     a            ' ','                        NUDENS   ',
     a                '     Computes neutrino energy density   ',/,
     a            ' ','                    RATE1-4      ',
     a                '     Computes rates for reactions',/,
     a            ' ','                    SOL          ',
     a                '     Builds A matrix for eqn dy/dt = Ay ',/,
     a            ' ','                        EQSLIN   ',
     a                '     Solves dy/dt=Ay by gaussian elim   ',/,
     a            ' ','                ACCUM            ',
     a               '     Output accumulator          ',/,
     a            ' ','        OUTPUT                   ',
     a                '     Allows user to output result',4(/),
     a            ' ','(Enter <RETURN> to go back to help menu): ',$)         
           READ (*,*)							
           GO TO 300
         ELSE
           GO TO 300
         END IF !(inum.eq.1)

C26--------USING INTERFACE SUBROUTINE SECTION.

260    CONTINUE               !Using the interface subroutine section.        
         WRITE (*,2600)							
2600     FORMAT (/,
     a          ' ',22x,'USING THE INTERFACE SUBROUTINE',/,
     a          ' ',22x,'----- --- --------- ----------',2(/),
     a          ' ','I. Purpose.                             ',/,
     a          ' ','   The interface subroutine CHECK is des',
     a              'igned to be an outlet of the program    ',/,
     a          ' ','   into which alterations can be easily ',
     a              'plugged.  Programs are normally modified',/,
     a          ' ','   by searching through the program, ide',
     a              'ntifying the appropriate areas for      ',/,
     a          ' ','   alterations, and interspersing new co',
     a              'mmands while deleting some old ones.    ',/,
     a          ' ','   This process can get tricky unless on',
     a             'e actively documents the alterations:   ',/,
     a          ' ','   one might lose track of all of the mo',
     a              'difications and deletions.  Thus, it is ',/,
     a          ' ','   worthwhile to put most if not all of ',
     a              'the necessary changes into one          ',/,
     a          ' ','   subroutine which is to be called from',
     a              ' strategic locations in the main        ',/,
     a          ' ','   program.  Furthermore, by putting cha',
     a              'nges into one small subroutine, one need',/,
     a          ' ','   only to compile the subroutine CHECK ',
     a              'each time instead of the entire nucleo- ',/,
     a          ' ','   synthesis code.                      ',8(/),
     a          ' ','(Enter 1 to continue, <RETURN> to end): ',$)
         READ (*,1001) inum						
         IF (inum.eq.1) THEN
           WRITE (*,2602)						
2602       FORMAT (/,
     a            ' ','II. Description.                        ',/,
     a           ' ','   Subroutine CHECK is an empty subrouti',
     a                'ne with a large COMMON area, giving the ',/,
     a            ' ','   user ready access to all of the impor',
     a                'tant variables in the computations.  The',/,
     a            ' ','   routine is called from various locati',
     a                'ons in the main program and the location',/,
     a            ' ','   spot in the program is labeled by the'
     a                ,' flag "itime".  The set call locations  ',/,
     a            ' ','   are given below:                     ',/,
     a            ' ','    A. itime = 1 (NUC123, very beginning',
     a                ' of program run)                        ',/,
     a            ' ','       (appropriate for opening files, i',
     a                'nitializing variables)                  ',/,
     a           ' ','    B. itime = 2 (NUC123, right before g',
     a                'oing into the RUN section)              ',/,
     a            ' ','    C. itime = 3 (RUN, right before goin',
     a                'g into DRIVER to do the computations)   ',/,
     a            ' ','    D. itime = 4 (DRIVER, in 1st R-K loo',
     a                'p after computing derivatives in DERIVS)',/,
     a            ' ','    E. itime = 7 (DRIVER, in 2nd R-K loo',
     a                'p after computing derivatives in DERIVS)',/,
     a            ' ','    F. itime = 8 (RUN, right after comin',
     a                'g back from DRIVER)                     ',/,
     a            ' ','    G. itime = 9 (NUC123, right after co',
     a                'ming back from the RUN section)         ',/,
     a            ' ','    H. itime =10 (NUC123, very end of pr',
     a                'ogram run)                              ',/,
     a            ' ','       (appropriate for closing files)  ',/,
     a            ' ','   The difference between the (2,9) pair',
     a                'ing and the (3,8) pairing is that for a ',/,
     a           ' ','   multiple run, the (3,8) pairing would',
     a                ' be called before and after every run   ',/,
     a            ' ','   but the (2,9) pairing would be called',
     a                ' before and after the entire sequence.  ',4(/),
     a            ' ','(Enter 1 to continue, <RETURN> to end): ',$)
           READ (*,1001) inum						
           IF (inum.eq.1) THEN
             WRITE (*,2604)						
2604         FORMAT (/,
     a              ' ','III. Implementation.                   ',/,        
     a              ' ','   The additional program statements ar',
     a                  'e needed in the subroutine CHECK.  If a',/,
     a              ' ','   particular command is to be executed',
     a                  ' when the computer is at a certain     ',/,
     a              ' ','   location in the program -- say label',
     a                  'ed by itime = 8 -- then in CHECK, one  ',/,
     a              ' ','   must place the command under the sta',
     a                  'tement, IF (itime.eq.8)....  The user  ',/,
     a              ' ','   is at leisure to place his own locat',
     a                  'ion indicators (5,6) and CALL CHECK    ',/,
     a              ' ','   statements anywhere in the program a',
     a                  's long as there is a COMMON /check/    ',/,
     a              ' ','   statement in the particular subrouti',
     a                  'ne to carry the value of itime along.  ',15(/),        
     a              ' ','(Enter <RETURN> to go back to help menu): ',$)
             READ (*,*)							  	
             GO TO 300
           ELSE
             GO TO 300
           END IF !(inum.eq.1)
         ELSE
           GO TO 300
         END IF !(inum.eq.1)

C27--------EXIT SECTION---------------------------------------------

270    CONTINUE                     !Exit section.
         RETURN

C30--------GO BACK TO MAIN MENU-------------------------------------

300    CONTINUE
       GO TO 100

       END



C========================IDENTIFICATION DIVISION======================

       SUBROUTINE setcom

C----------LINKAGES.
C     CALLED BY - [program] nuc123
C     CALLS     - none

C----------REMARKS.
C     Allows resetting of computation parameters.

C----------PARAMETERS.
       PARAMETER (ir=1)             !Input unit number.
       PARAMETER (iw=1)             !Output unit number.

C----------COMMON AREAS.
       COMMON /compr0/ cy0,ct0,     !Default comp parameters.
     a  t9i0,t9f0,ytmin0,inc0  
       COMMON /compr/  cy,ct,t9i,t9f,ytmin,inc         
                                    !Computation parameters.
       COMMON /varpr0/ dt0,eta0     !Default variationl params.      
       COMMON /varpr/  dt1,eta1     !Variational parameters.


C==========================DECLARATION DIVISION=====================

C----------DEFAULT COMPUTATION PARAMETERS.
       REAL    cy0                  !Default cy.
       REAL    ct0                  !Default ct.
       REAL    t9i0                 !Default t9i.
       REAL    t9f0                 !Default t9f.
       REAL    ytmin0               !Default ytmin.
       INTEGER inc0                 !Default accumulation increment.

C----------COMPUTATION PARAMETERS.
       REAL    cy        !Time step limiting constant on abundances.
       REAL    ct        !Time step limiting constant on temperature.
       REAL    t9i       !Initial temperature (in 10**9 K).
       REAL    t9f       !Final temperature (in 10**9 K).
       REAL    ytmin     !Smallest abundances allowed.
       INTEGER inc       !Accumulation increment.

C----------DEFAULT VARIATIONAL  PARAMETERS.
       REAL    dt0                  !Default initial dt.

C----------VARIATIONAL  PARAMETERS.
       REAL    dt1                  !Initial time step.

C----------LOCAL VARIABLES.
       INTEGER inum                 !Selection number.


C===========================PROCEDURE DIVISION================

C10--------PRINT RESET SELECTION AND AWAIT RESPONSE---------

C..........RETURN FROM LOOPING.
100    CONTINUE
C..........DISPLAY RESET SELECTIONS.
       WRITE (*,1000) cy,ct,dt1,t9i,t9f,ytmin,float(inc)   		  
1000   FORMAT (8(/),
     a        ' ',21x,'SET COMPUTATION PARAMETERS SELECTION',/,
     a        ' ',21x,'--- ----------- ---------- ---------',//,
     a        ' ',10x,' 1. CHANGE TIME-STEP LIMITING CONSTANT 1  FROM ',
     a            f5.3,/,
     a        ' ',10x,' 2. CHANGE TIME-STEP LIMITING CONSTANT 2  FROM ',
     a            f5.3,/,
     a        ' ',10x,' 3. CHANGE INITIAL TIME-STEP              FROM ',
     a            1pe8.2,' SECONDS',/,
     a        ' ',10x,' 4. CHANGE INITIAL TEMPERATURE            FROM ',
     a            1pe8.2,' (10**9 K)',/,
     a        ' ',10x,' 5. CHANGE FINAL TEMPERATURE              FROM ',
     a            1pe8.2,' (10**9 K)',/,
     a        ' ',10x,' 6. CHANGE SMALLEST ABUNDANCES ALLOWED    FROM ',
     a            1pe8.2,/,
     a        ' ',10x,' 7. CHANGE ACCUMULATION INCREMENT         FROM ',        
     a            1pe8.2,' ITERATIONS',/,
     a        ' ',10x,' 8. RESET ALL TO DEFAULT VALUES',/,
     a        ' ',10x,' 9. EXIT',5(/),
     a        ' ',10x,'Enter selection (1-9): ',$)
C..........READ IN SELECTION NUMBER.

       READ (*,1001) inum						
1001  FORMAT (i1)

C20--------BRANCH TO APPROPRIATE SECTION-------------------------------

       GO TO (210,220,230,240,250,260,270,280,300),inum
       GO TO 300           !Improper input or <RETURN>.
210    CONTINUE            !Change time step limiting const 1 section.
         WRITE (*,2100)								
2100     FORMAT (' ','Enter value for time step limiting constant 1:',$)
         READ (*,*) cy							
2101     FORMAT (f5.3)     
         GO TO 400
220    CONTINUE            !Change time step limiting const 2 section.
         WRITE (*,2200)							
2200     FORMAT (' ','Enter value for time step limiting constant 2:',$)        
         READ (*,*) ct								
         GO TO 400
230    CONTINUE                     !Change initial time step section.
         WRITE (*,2300)							
2300     FORMAT (' ','Enter value for initial time step: ',$)
         READ (*,*) dt1							
         GO TO 400
240    CONTINUE                     !Change initial temperature section.
         WRITE (*,2400)							
2400    FORMAT (' ','Enter value for initial temperature: ',$)
         READ (*,*) t9i							
         GO TO 400
250    CONTINUE                     !Change final temperature section.
         WRITE (*,2500)							
2500     FORMAT (' ','Enter value for final temperature: ',$)
         READ (*,*) t9f							
         GO TO 400
260    CONTINUE           !Change smallest abundances allowed section.
         WRITE (*,2600)							
2600     FORMAT (' ','Enter value for smallest abundances allowed: ',$)
         READ (*,*) ytmin						
         GO TO 400
270    CONTINUE           !Change accumulation increment section.
         WRITE (*,2700)							
2700     FORMAT (' ','Enter value for accumulation increment: ',$)
         READ (*,*) inc							
         GO TO 400
280    CONTINUE           !Reset all to default values section.
         cy    = cy0      !Time step limiting constant on abundances.
         ct    = ct0      !Time step limiting constant on temperature.
         dt1   = dt0      !Time step.
         t9i   = t9i0     !Initial temperature.
         t9f   = t9f0     !Final temperature.
         ytmin = ytmin0   !Smallest abundances allowed.
         inc   = inc0     !Accumulation increment.
         WRITE (*,2800)							
2800     FORMAT (' ','All values reset to default - Press <RETURN> '
     a               ,'to continue: ',$)
         READ (*,*)							
         GO TO 400
300    CONTINUE                     !Exit section.
         RETURN

C40--------GO BACK TO MENU-------------------------------------

400    CONTINUE
       GO TO 100

       END



C========================IDENTIFICATION DIVISION=============

       SUBROUTINE setmod

C----------LINKAGES.
C     CALLED BY - [program] nuc123
C     CALLS     - none

C----------REMARKS.
C     Allows resetting of model parameters.

C----------PARAMETERS.
       PARAMETER (ir=1)             !Input unit number.
       PARAMETER (iw=1)             !Output unit number.

C----------COMMON AREAS.
       COMMON /modpr0/ c0,cosmo0,xi0        !Default model parameters.
       COMMON /modpr/  g,tau,xnu,c,cosmo,xi !Model parameters.
       COMMON /varpr0/ dt0,eta0             !Default variationl params.
       COMMON /varpr/  dt1,eta1             !Variational parameters.

C==========================DECLARATION DIVISION=====================

C----------DEFAULT MODEL PARAMETERS.
       REAL    c0(3)          !Default c.
       REAL    cosmo0         !Default cosmological constant.
       REAL    xi0(3)         !Default neutrino degeneracy parameters.

C----------EARLY UNIVERSE MODEL PARAMETERS.
       REAL    c(3)      !c(1) is variation of gravitational constant.        
                         !c(2) is neutron lifetime (sec).
                         !c(3) is number of neutrino species.
       REAL    cosmo     !Cosmological constant.
       REAL    xi(3)     !Neutrino degeneracy parameters.

C----------DEFAULT VARIATIONAL PARAMETERS.
       REAL    eta0                 !Default eta.

C----------VARIATIONAL PARAMETERS.
       REAL    eta1                 !Intial baryon-to-photon ratio.

C----------USER RESPONSE VARIABLES.
       INTEGER inum                 !Selection number.


C===========================PROCEDURE DIVISION=====================

C10--------PRINT RESET SELECTION AND AWAIT RESPONSE----------------

C..........RETURN FROM LOOPING.
100    CONTINUE
C..........DISPLAY RESET SELECTIONS.
       WRITE (*,1000) c(1),c(2),c(3),eta1,cosmo,xi(1),xi(2),xi(3)		
1000   FORMAT (8(/),
     a     ' ',24x,'SET MODEL PARAMETERS SELECTION',/,
     a      ' ',24x,'--- ----- ---------- ---------',//,
     a      ' ',10x,' 1. CHANGE GRAVITATIONAL CONSTANT        FROM ',               
     a         1pe10.3,/,
     a       ' ',10x,' 2. CHANGE NEUTRON LIFETIME              FROM ',
     a           1pe10.3,' SECONDS',/,
     a       ' ',10x,' 3. CHANGE NUMBER OF NEUTRINO SPECIES    FROM ',
     a           1pe10.3,/,
     a       ' ',10x,' 4. CHANGE FINAL BARYON-TO-PHOTON RATIO  FROM ',
     a           1pe10.3,/,
     a       ' ',10x,' 5. CHANGE COSMOLOGICAL CONSTANT         FROM ',
     a           1pe10.3,/,
     a       ' ',10x,' 6. CHANGE XI-ELECTRON                   FROM ',
     a           1pe10.3,/,
     a       ' ',10x,' 7. CHANGE XI-MUON                       FROM ',
     a           1pe10.3,/,
     a       ' ',10x,' 8. CHANGE XI-TAUON                      FROM ',                
     a           1pe10.3,/,
     a       ' ',10x,' 9. RESET ALL TO DEFAULT VALUES',/,
     a       ' ',10x,'10. EXIT',4(/),
     a       ' ',10x,' Enter selection (1-10): ',$)
C..........READ IN SELECTION NUMBER.
       READ (*,1001) inum						
1001   FORMAT (i2)

C20--------BRANCH TO APPROPRIATE SECTION--------------------------

       GO TO (210,220,230,240,250,260,270,280,290,300),inum
       GO TO 300             !Improper input or <RETURN>.
210    CONTINUE              !Change gravitational constant section.
         WRITE (*,2100)							
2100     FORMAT (' ','Enter value for variation of gravitational ',
     a              'constant: ',$)
         READ (*,*) c(1)						
         GO TO 400
220    CONTINUE               !Change neutron lifetime section.
         WRITE (*,2200)							
2200     FORMAT (' ','Enter value for neutron lifetime (sec): ',$)
         READ (*,*) c(2)						
         GO TO 400
230    CONTINUE             !Change number of neutrino species section.       
         WRITE (*,2300)							
2300     FORMAT (' ','Enter value for number of neutrino species: ',$)
         READ (*,*) c(3)						
         GO TO 400
240    CONTINUE             !Change baryon-to-photon ratio section.
         WRITE (*,2400)							
2400     FORMAT (' ','Enter value for baryon-to-photon ratio: ',$)
         READ (*,*) eta1						
         GO TO 400
250    CONTINUE             !Change cosmological constant section.
         WRITE (*,2500)
2500     FORMAT (' ','Enter value for cosmological constant: ',$)
         READ (*,*) cosmo
         GO TO 400
260    CONTINUE             !Change neutrino degeneracy section.
         WRITE (*,2600)
2600     FORMAT (' ','Enter value for xi electron: ',$)
         READ (*,*) xi(1)
         GO TO 400
270    CONTINUE             !Change neutrino degeneracy section.
         WRITE (*,2700)
2700     FORMAT (' ','Enter value for xi muon: ',$)
         READ (*,*) xi(2)
         GO TO 400
280    CONTINUE             !Change neutrino degeneracy section.
         WRITE (*,2800)
2800     FORMAT (' ','Enter value for xi tauon: ',$)
         READ (*,*) xi(3)
         IF ((xi(3).ne.0.).and.(c(3).lt.3.)) THEN
           c(3) = 3.
           WRITE (*,2802)
2802       FORMAT (' ','Number of neutrinos set to 3')
           WRITE (*,2804)
2804       FORMAT (' ','Press <RETURN> to continue: ',$)
           READ (*,*)
         END IF
         GO TO 400
290    CONTINUE            !Reset all to default values section.
         c(1)   = c0(1)
         c(2)   = c0(2)
         c(3)   = c0(3)
         cosmo  = cosmo0
         xi(1)  = xi0(1)
         xi(2)  = xi0(2)
         xi(3)  = xi0(3)
         eta1   = eta0
         WRITE (*,2900)
2900     FORMAT (' ','All values reset to default - 
     a            Press <RETURN> ' ,'to continue: ',$)
         READ (*,*)
         GO TO 400
300    CONTINUE                     !Exit section.
         RETURN

C40--------GO BACK TO MENU--------------------------------------

400    CONTINUE
       GO TO 100

       END



C========================IDENTIFICATION DIVISION================

       SUBROUTINE run

C----------LINKAGES.
C     CALLED BY - [program] nuc123
C     CALLS     - [subroutine] driver, check

C----------REMARKS.
C     Activates computation routine.

C----------PARAMETERS.
       PARAMETER (ir=1)    !Input unit number.
       PARAMETER (iw=1)    !Output unit number.
       PARAMETER (nrec=88) !Number of nuclear reactions.
       PARAMETER (lrec=64) !Total # of nuclear reactions for irun = 2.
       PARAMETER (krec=34) !Total # of nuclear reactions for irun = 3.        
       PARAMETER (nnuc=26) !Number of nuclides in calculation.
       PARAMETER (lnuc=18) !Total # of nuclides for irun = 2.
       PARAMETER (knuc=9)  !Total # of nuclides for irun = 3.

C----------COMMON AREAS.
       COMMON /modpr/  g,tau,xnu,c,cosmo,xi  !Model parameters.
       COMMON /varpr/  dt1,eta1              !Variational parameters.
       
       COMMON /checki/  itime                !Computation location.
                                           
C//////Changed the name of the list from 'check' to 'checki',
C      as 'check' is the name of a subroutine.
                                             
                                            
       COMMON /runopt/ irun,isize,jsize      !Run options.


C==========================DECLARATION DIVISION====================

C----------MODEL PARAMETERS.
       REAL    eta1     !Baryon-to-photon ratio.
       REAL    c(3)     !c(1) is variation of gravitational constant.     
                        !c(2) is neutron lifetime (sec).
                        !c(3) is number of neutrino species.
       REAL    cosmo    !Cosmological constant.
       REAL    xi(3)    !Neutrino degeneracy parameters.

C----------RUN OPTION.
       INTEGER irun           !Run network size.
       INTEGER isize          !Number of nuclides in computation.
       INTEGER jsize          !Number of reactions in computation.

C----------USER INTERACTION VARIABLES.
       REAL    rnumb1               !Run parameter for outer loop.
       REAL    rnumb2               !Run parameter for middle loop.
       REAL    rnumb3               !Run parameter for inner loop.
       REAL    rnum1(3)             !Run parameter starting value.
       REAL    rnum2(3)             !Run parameter end value.
       REAL    rnum3(3)             !Run parameter increment.
       INTEGER inumb                !Selection number.
       INTEGER inum(3)              !Selection number.
       INTEGER jnum                 !Number of loopings to be done.
       INTEGER knum                 !Number of loopings rejected.
       INTEGER lnumb1               !Run parameter for outer loop.
       INTEGER lnumb2               !Run parameter for middle loop.
       INTEGER lnumb3               !Run parameter for inner loop.
       INTEGER lnum(3)              !Run parameter end value.
       
       CHARACTER lchose             !User response (alphanumeric).
       				    
C//////Changed 'lchose' from INTEGER to CHARACTER based on the  
C      IF statement following statement label 2301.
       		        

C----------FLAG AND LABELS.
       INTEGER itime            !Computation location.
       CHARACTER*22 vtype(8)    !Label for quantities being varied.

C----------EQUIVALENCE VARIABLE.
       REAL    qvary(7)         !Array set equal to c, cosmo, and xi.

C----------EQUIVALENCE STATEMENTS.
       EQUIVALENCE (qvary(1),c(1)), (qvary(4),cosmo), (qvary(5),xi(1))


C==============================DATA DIVISION======================

C----------LABELS FOR QUANTITIES BEING VARIED.
       DATA vtype /'baryon/photon ratio   ',
     a            'gravitational constant',
     a            'neutron lifetime      ',
     a            '# of neutrino species ',
     a            'cosmological constant ',
     a            'xi-electron           ',
     a            'xi-muon               ',
     a            'xi-tauon              '/


C===========================PROCEDURE DIVISION=====================

C10--------PRINT RUN SELECTION AND AWAIT RESPONSE------------------

C..........RETURN FROM LOOPING.
100    CONTINUE
C..........DISPLAY RUN SELECTIONS.
       WRITE (*,1000)
1000   FORMAT (8(/),
     a        ' ',32x,'RUN SELECTION',/,
     a        ' ',32x,'--- ---------',//,
     a        ' ',27x,' 1. SET RUN NETWORK',/,
     a        ' ',27x,' 2. GO',/,
     a        ' ',27x,' 3. DO MULTIPLE RUNS',/,
     a        ' ',27x,' 4. EXIT',10(/),
     a        ' ',27x,' Enter selection (1-4): ',$)
C..........READ IN SELECTION NUMBER.
C       READ(*,*)      !pause
       READ (*,1001) inumb
1001   FORMAT (i1)

C20--------BRANCH TO APPROPRIATE SECTION---------------------------

       GO TO (210,220,230,240),inumb
       GO TO 240                    !Improper input or <RETURN>.

C21--------SET RUN NETWORK SECTION---------------------------------

210    CONTINUE
         WRITE (*,2100)
2100     FORMAT (' ','Enter network size 
     a           (1-26 nuclides (default); ',  '2-18; 3-9): ',$)
         READ (*,*) inumb          !Read in selection number.
         IF ((inumb.ne.1).and.(inumb.ne.2).and.(inumb.ne.3)) inumb = 1         
         !Default.
         IF (inumb.ne.irun) THEN   !Run network changed from previously.       
           irun = inumb            !Run network size selection.
         END IF
         IF (irun.eq.1) THEN       !Maximal network size.
           isize = nnuc
           jsize = nrec
         ELSE
           IF (irun.eq.2) THEN     !Abridged network size.
             isize = lnuc
             jsize = lrec
           ELSE
             IF (irun.eq.3) THEN   !Minimal network size.
               isize = knuc
               jsize = krec
             END IF
           END IF
         END IF !(irun.eq.1)
         WRITE (*,2104) irun
2104     FORMAT (' ','Run network set to ',i1,' - 
     a            Press <RETURN> ' ,'to continue: ',$)
         READ (*,*)
         GO TO 300

C22--------GO SECTION------------------------------------------

220    CONTINUE
         WRITE (*,2200)
2200     FORMAT (' ','Begin computation run....')
         itime = 3
         CALL check         !Call interface subr before computation.
         CALL driver        !Do nucleosynthesis computation.
         itime = 8
         CALL check         !Call interface subr after computation.
         WRITE (*,2202)
2202     FORMAT (' ','Computation completed - '
     a           ,'Press <RETURN> to continue: ',$)
         READ (*,*)
         GO TO 300

C23--------DO MULTIPLE RUNS SECTION---------------------------------

C..........GET NUMBER OF LOOPINGS.
230    CONTINUE
         WRITE (*,2300)
2300     FORMAT (' ','Enter the number of loopings to be done' 
     a            ,'(1 ','(default); 2; 3): ',$)
         READ (*,*) jnum     !Read in number of loopings to be done.
         IF ((jnum.ne.1).and.(jnum.ne.2).and.(jnum.ne.3)) THEN
           jnum = 1           !Default number of loopings.
         END IF
         knum = 0.            !No loopings rejected for now.
         DO i = 1,3
           IF (i.gt.jnum) THEN
             rnum1(i) = 0.          !Initialize initial parameter.
             rnum2(i) = 0.          !Initialize terminal parameter.
             rnum3(i) = 1.          !Initialize incremental parameter.        
             inum(i)  = 0           !Initialize selection number.
           ELSE
C..........OBTAIN QUANTITY TO VARY.
             WRITE (*,2302)
2302         FORMAT (8(/),
     a                ' ',30x,'QUANTITY TO VARY',/,
     a              ' ',30x,'-------- -- ----',//,
     a                ' ',25x,' 1.  ETA     (LOGRITHMIC VARIATION)',/,
     a                ' ',25x,' 2.  G           (LINEAR VARIATION)',/,        
     a                ' ',25x,' 3.  TAU         (LINEAR VARIATION)',/,
     a                ' ',25x,' 4.  # NEUTRINOS (LINEAR VARIATION)',/,
     a                ' ',25x,' 5.  LAMBDA      (LINEAR VARIATION)',/,
     a                ' ',25x,' 6.  XI-ELECTRON (LINEAR VARIATION)',/,
     a                ' ',25x,' 7.  XI-MUON     (LINEAR VARIATION)',/,
     a                ' ',25x,' 8.  XI-TAUON    (LINEAR VARIATION)',/,
     a                ' ',25x,' 9.  NO SELECTION',5(/),
     a                ' ',25x,' Enter selection (1-9): ',$)
             READ (*,1001) inum(i)
             IF ((inum(i).lt.1).or.(inum(i).gt.8)) THEN 
             !No selection made.     
               WRITE (*,2304)
2304           FORMAT (' ','No selection made - Reduce number of ',
     a                      'loopings by one',/,
     a                  ' ','Press <RETURN> to continue: ',$)
               READ (*,*)
               knum = knum + 1   !Step up number of loopings rejected.
               rnum1(i) = 0.     !Initialize initial parameter.
               rnum2(i) = 0.     !Initialize terminal parameter.
               rnum3(i) = 1.     !Initialize incremental parameter.
               inum(i)  = 0      !Initialize selection number.
             ELSE !((inum(i).ge.1).and.(inum(i).le.8))  
       
C..........INPUT RUN SPECIFICATIONS.
231            CONTINUE
               WRITE (*,2306)
2306           FORMAT (' ','Enter minimum value: ',$)
               READ (*,*) rnum1(i)  !Read in starting value.
               WRITE (*,2308)
2308           FORMAT (' ','Enter maximum value: ',$)
               READ (*,*) rnum2(i)  !Read in terminating value.
232            CONTINUE
               WRITE (*,2310)
2310           FORMAT (' ','Enter increment: ',$)
               READ (*,*) rnum3(i)  !Read in incremental value.
               IF (rnum3(i).eq.0.) THEN 
               !Trouble with 0 division later on.     
                 WRITE (*,2312)
2312             FORMAT (' ','Zero increment not allowed:
     a                    trouble with ','dividing by zero')
                 GO TO 232
               END IF
               WRITE (*,2314) rnum1(i), rnum2(i), rnum3(i)  !Display 
                                                        
2314           FORMAT (' ','Run from ',1pe12.5,' to ',1pe12.5, 
     a                  ' in increments of ',1pe12.5)
               WRITE (*,2316)
2316           FORMAT (' ','Confirm these values (Y or N): ',$)
               READ (*,2301) lchose                !Get confirmation.
2301           FORMAT (a1)
               IF ((lchose.ne.'Y').and.(lchose.ne.'y')) GO TO 231
             END IF !((inum(i).lt.1).or.(inum(i).gt.8))
           END IF !(i.gt.jnum)
         END DO !i = 1,3
         jnum = jnum-knum           !Number of valid loopings.
         IF (jnum.ne.0) THEN        !Run requested.
C..........WRITE OUT QUANTITY TO VARY, RUN SPECIFICATIONS.
           DO l = 1,jnum+knum       !Check all loopings.
             IF (inum(l).ne.0) THEN !Proper selection was made.
               WRITE (*,2318) vtype(inum(l)),rnum1(l), 
     a                rnum2(l), rnum3(l)     
               !Display run params.
                              
2318           FORMAT (' ','Run ',a22,/, '    from ',1pe12.5,' to 
     a                  ',1pe12.5, ' in increments of ',1pe12.5)
C..........GET LOGS OF eta VALUES FOR LOGRITHMIC INCREMENTATION.
               IF (inum(l).eq.1) THEN  
               !Work with exponents for eta increments.
                 rnum1(l) = log10(rnum1(l))
                 rnum2(l) = log10(rnum2(l))
               END IF
             END IF
           END DO
C..........COMPUTE NUMBER OF RUNS FOR EACH LOOPING.
           DO l = 1,3
             lnum(l) = nint((rnum2(l)-rnum1(l)+rnum3(l))/rnum3(l))
           END DO
C..........DO MULTIPLE RUNS.
           WRITE (*,2200)          
           !Inform user of beginning of computation.
           DO lnumb1 = 0,lnum(1)-1  
           !Outer loop.
             rnumb1 = rnum1(1)+float(lnumb1)*rnum3(1)  
             !Value of param for run.
             IF ((inum(1).ge.1).and.(inum(1).le.8)) THEN
               IF (inum(1).eq.1) THEN
                 eta1 = 10**rnumb1    !Vary baryon-to-photon ratio.
               ELSE
                 qvary(inum(1)-1) = rnumb1  !Vary other quantities.
               END IF
             END IF
             DO lnumb2 = 0,lnum(2)-1  !Middle loop.
               rnumb2 = rnum1(2)+float(lnumb2)*rnum3(2)  
               !Value of param for run.
               IF ((inum(2).ge.1).and.(inum(2).le.8)) THEN
                 IF (inum(2).eq.1) THEN
                   eta1 = 10**rnumb2  !Vary baryon-to-photon ratio.
                 ELSE
                   qvary(inum(2)-1) = rnumb2  !Vary other quantities.
                 END IF
               END IF
               DO lnumb3 = 0,lnum(3)-1  !Inner loop.
                 rnumb3 = rnum1(3)+float(lnumb3)*rnum3(3)  
                 !Value of parameter.
                 IF ((inum(3).ge.1).and.(inum(3).le.8)) THEN
                   IF (inum(3).eq.1) THEN
                     eta1 = 10**rnumb3  !Vary baryon-to-photon ratio.
                   ELSE
                     qvary(inum(3)-1) = rnumb3  !Vary other quantities.
                   END IF
                 END IF
                 itime = 3
                 CALL check         
                 !Check interface subr before computation.
                 CALL driver        
                 !Do nucleosynthesis computation.
                 itime = 8
                 CALL check         
                 !Check interface subroutine after computation.
               END DO !lnumb3 = 0,lnum(3)-1
             END DO !lnumb2 = 0,lnum(2)-1
           END DO !lnumb1 = 0,lnum(1)-1
           WRITE (*,2202)          
           !Inform user of completion of computation.
         ELSE !(jnum.eq.0)
           WRITE (*,2320)
2320       FORMAT (' ','No selection made - ',
     a              'Press <RETURN> to continue: ',$)
         END IF !(jnum.ne.0)
         READ (*,*)
         GO TO 300

C24--------EXIT SECTION-----------------------------------------

240    CONTINUE
         RETURN

C30--------GO BACK TO MENU-------------------------------------

300    CONTINUE
       GO TO 100

        END



C========================IDENTIFICATION DIVISION==============

       SUBROUTINE output

C----------LINKAGES.
C     CALLED BY - [program] nuc123
C     CALLS     - none

C----------REMARKS.
C     Outputs computational results either into an output file or onto       
C     the screen

C----------PARAMETERS.
       PARAMETER (ir=1)             !Input unit number.
       PARAMETER (iw=1)             !Output unit number.
       PARAMETER (nnuc=26)          !Number of nuclides in calculation.
       PARAMETER (itmax=40)         !Maximum # of line to be printed.

C----------COMMON AREAS.
       COMMON /compr/  cy,ct,t9i,t9f,ytmin,inc  !Computation parameters.        
       COMMON /modpr/  g,tau,xnu,c,cosmo,xi     !Model parameters.
       COMMON /flags/  ltime,is,ip,it,mbad      !Flags, counters.
       COMMON /outdat/ xout,thmout,t9out,tout,
     a         dtout,etaout,hubout              !Output data. 
       COMMON /outopt/ nout,outfile             !Output option.


C==========================DECLARATION DIVISION==============

C----------COMPUTATION SETTINGS.
       REAL    cy         !Time step limiting constant on abundances.
       REAL    ct         !Time step limiting constant on temperature.
       REAL    t9i        !Initial temperature (in 10**9 K).
       REAL    t9f        !Final temperature (in 10**9 K).
       REAL    ytmin      !Smallest abundances allowed.

C----------EARLY UNIVERSE MODEL PARAMETERS.
       REAL    c(3)     !c(1) is variation of gravitational constant.
                        !c(2) is neutron lifetime (sec).
                        !c(3) is number of neutrino species.
       REAL    cosmo    !Cosmological constant.
       REAL    xi(3)    !Neutrino degeneracy parameters.

C----------COUNTER.
       INTEGER it                !# times accumulated in output buffer.

C----------OUTPUT ARRAYS.
       REAL    xout(itmax,nnuc)  !Nuclide mass fractions.
       REAL    thmout(itmax,6)   !Thermodynamic variables.
       REAL    t9out(itmax)      !Temperature (in units of 10**9 K).
       REAL    tout(itmax)       !Time.
       REAL    dtout(itmax)      !Time step.
       REAL    etaout(itmax)     !Baryon-to-photon ratio.
       REAL    hubout(itmax)     !Expansion rate.
       
C----------OUTPUT FILE STATUS.
       INTEGER nout                 !Number of output requests.
       LOGICAL outfile              !Indicates if output file used.

C----------USER INTERACTION VARIABLES.
       INTEGER inum                 !Selection number.


C===========================PROCEDURE DIVISION==================

C10--------PRINT OUTPUT SELECTION AND AWAIT RESPONSE-----------

C..........RETURN FROM LOOPING.
100    CONTINUE
C..........DISPLAY OUTPUT SELECTIONS.
       WRITE (*,1000)
1000   FORMAT (8(/),
     a        ' ',30x,'OUTPUT SELECTION',/,
     a        ' ',30x,'------ ---------',//,
     a        ' ',25x,' 1. REQUEST OUTPUT FILE',/,
     a        ' ',25x,' 2. REQUEST OUTPUT ON SCREEN',/,
     a        ' ',25x,' 3. EXIT',11(/),
     a        ' ',25x,' Enter selection (1-3): ',$)
C..........READ IN SELECTION NUMBER.
       READ (*,1001) inum
1001   FORMAT (i1)
C..........BRANCH TO APPROPRIATE SECTION.
       GO TO (200,300,400),inum
       GO TO 400                    !Improper input or <RETURN>.

C20--------REQUEST OUTPUT SECTION----------------------------------
200      CONTINUE

C///// To convert temperature to MeV, remove comments below:

c     DO j = 1,it            !Temperature in MeV.
c       t9out(j) = t9out(j)*.08617
c      END DO
c      DO j = 1,it             !Energy density as fraction of total.
c        thmout(j,1) = thmout(j,1)/thmout(j,6)  !Rhog.
c        thmout(j,2) = thmout(j,2)/thmout(j,6)  !Rhoe.
c        thmout(j,3) = thmout(j,3)/thmout(j,6)  !Rhone.
c        thmout(j,4) = thmout(j,4)/thmout(j,6)  !Rhob.
c      END DO
C..........PRINT CAPTION.
         nout = nout + 1     !Keep track of number of output requests.
         

         
C///// WRITE(2,_) statements write out to unit 2, the nuc123.dat file  
      
         
         IF (nout.eq.1) THEN
           WRITE (2,2000)            !This writes to the output file (unit 2) 
2000       FORMAT (54x,'NUCLIDE ABUNDANCE YIELDS',/,
     a            54x,'------- --------- ------',//)
         END IF
         WRITE (2,2002) cy,ct,t9i,t9f,ytmin
2002     FORMAT (' Computational parameters:',/,
     a          '   cy = ',f5.3,'/  ct = ',f5.3,
     a          '/  initial temp = ',1pe8.2,
     a          '/  final temp = ',1pe8.2,
     a          '/  smallest abundances allowed = ',1pe8.2)
         WRITE (2,2004) c(1),c(2),c(3),cosmo,xi(1),xi(2),xi(3)
2004     FORMAT (' Model parameters:',/,
     a          '   g = ',f5.2,'/  tau = ',f6.2,
     a          '/  # nu = ',f5.2,'/  lambda = ',1pe10.3,
     a          '/  xi-e = ',e10.3,'/  xi-m = ',e10.3,
     a          '/  xi-t = ',e10.3,/)
C..........PRINT HEADINGS, ABUNDANCES FOR NEUTRON TO LI8.
         WRITE (2,2006)
2006     FORMAT (4x,'Temp',8x,'N/H',10x,'P',10x,'D/H',9x,'T/H',8x,
     a          'He3/H',8x,'He4',8x,'Li6/H',7x,'Li7/H',7x,
     a          'Be7/H',6x,'Li8/H&up',/,132('-'))
         DO j = 1,it
           WRITE (2,2008) t9out(j),(xout(j,i),i=1,10)
2008       FORMAT (1pe10.3,1p10e12.3)
         END DO
C..........PRINT THERMODYNAMIC QUANTITIES.
         WRITE (2,2010)
2010     FORMAT (' ',/,4x,'Temp',9x,'T',10x,'rhog',8x,'rhoe',7x,
     a              'rhone',8x,'rhob',8x,'phie',9x,'dt',9x,
     a              'eta',10x,'H',/,132('-'))
         DO j = 1,it
           WRITE (2,2012) t9out(j),tout(j),(thmout(j,i),i=1,5),dtout(j),        
     a                   etaout(j),hubout(j)
2012       FORMAT (1pe10.3,9e12.3)
         END DO
         WRITE (2,2014)
2014     FORMAT (///)
         outfile = .true.           !Output file requested.
         WRITE (*,2016)
2016     FORMAT (' ','Output file requested - Press <RETURN> to '
     a              ,'continue: ',$)
         READ (*,*)
         GO TO 500

C30--------REQUEST OUTPUT ON SCREEN SECTION------------------

C..........RETURN FROM LOOPING.
300    CONTINUE
c      DO j = 1,it              !Temperature in MeV.
c        t9out(j) = t9out(j)*.08617
c      END DO
c      DO j = 1,it              !Energy density as fraction of total.
c        thmout(j,1) = thmout(j,1)/thmout(j,6)  !Rhog.
c        thmout(j,2) = thmout(j,2)/thmout(j,6)  !Rhoe.
c        thmout(j,3) = thmout(j,3)/thmout(j,6)  !Rhone.
c        thmout(j,4) = thmout(j,4)/thmout(j,6)  !Rhob.
c      END DO
C..........DISPLAY SCREEN OUTPUT SELECTIONS. 

C//////These are WRITE(*,_) statements, as we're writing out to a.out file.	

         WRITE (*,3000)
3000     FORMAT (8(/),
     a          ' ',26x,'SCREEN OUTPUT SELECTION',/,
     a          ' ',26x,'------ ------ ---------',//,
     a          ' ',25x,' 1. DISPLAY D,T,HE3,HE4,LI7',/,     
     a          ' ',25x,' 2. DISPLAY N,P,LI6,BE7,LI8&UP',/,
     a          ' ',25x,' 3. DISPLAY RHOG,RHOE,RHONE,RHOB',/,
     a          ' ',25x,' 4. DISPLAY T,DT,PHIE,ETA,H',/,
     a          ' ',25x,' 5. EXIT',9(/),
     a          ' ',25x,' Enter selection (1-5): ',$)
C..........READ IN SELECTION NUMBER.
         READ (*,1001) inum
         GO TO (310,320,330,340,350),inum
         GO TO 350                  !Improper input or <RETURN>.
310      CONTINUE                   !Display d,t,he3,he4,li7.
C..........PRINT CAPTION.
           WRITE (*,2014)
           WRITE (*,3100) cy,ct,t9i,t9f,ytmin
3100       FORMAT (' ','Computational parameters:',/,
     a            ' ','   cy = ',f5.3,'/ ct = ',f5.3,
     a                '/ initial temp = ',1pe8.2,
     a                '/ final temp = ',1pe8.2,/,
     a            ' ','   smallest abundances allowed = ',1pe8.2)
           WRITE (*,3102) c(1),c(2),c(3),cosmo,xi(1),xi(2),xi(3)
3102       FORMAT (' ','Model parameters:',/,
     a            ' ','   g = ',f5.2,'/ tau = ',f6.2,
     a                '/ # nu = ',f5.2,'/ lambda = ',1pe10.3,/,
     a            ' ','   xi-e = ',e10.3,'/ xi-m = ',e10.3,
     a                '/ xi-t = ',e10.3,/)
C..........PRINT HEADINGS, ABUNDANCES FOR D,T,HE3,HE4,LI7.
           WRITE (*,3104)
3104       FORMAT (4x,'Temp',8x,'D/H',9x,'T/H',8x,'He3/H',8x,
     a            'He4',8x,'Li7/H',/,' ',79('-'))
           DO j = 1,it
             WRITE (*,3106) t9out(j),(xout(j,i),i=3,6),xout(j,8)
3106         FORMAT (1pe10.3,1p5e12.3)
           END DO
           WRITE (*,2014)
           WRITE (*,3108)
3108       FORMAT (' ','Press <RETURN> to continue: ',$)
           READ (*,*)
           GO TO 360
320      CONTINUE                   !Display n,p,li6,be7,li8&up.
C..........PRINT CAPTION.
           WRITE (*,2014)
           WRITE (*,3100) cy,ct,t9i,t9f,ytmin
           WRITE (*,3102) c(1),c(2),c(3),cosmo,xi(1),xi(2),xi(3)
C..........PRINT HEADINGS, ABUNDANCES FOR N,P,LI6,BE7,LI8&UP.
           WRITE (*,3204)
3204       FORMAT (4x,'Temp',8x,'N/H',10x,'P',9x,
     a            'Li6/H',7x,'Be7/H',6x,'Li8/H&up',/,' ',80('-'))
           DO j = 1,it
             WRITE (*,3106) t9out(j),(xout(j,i),i=1,2),xout(j,7),
     a                      (xout(j,i),i=9,10)
           END DO
           WRITE (*,2014)
           WRITE (*,3108)
           READ (*,*)
           GO TO 360
330      CONTINUE                   !Display rhog,rhoe,rhone,rhob.
C..........PRINT CAPTION.
           WRITE (*,2014)
           WRITE (*,3100) cy,ct,t9i,t9f,ytmin
           WRITE (*,3102) c(1),c(2),c(3),cosmo,xi(1),xi(2),xi(3)
C..........PRINT ENERGY DENSITIES.
           WRITE (*,3304)
3304       FORMAT (4x,'Temp',8x,'rhog',8x,'rhoe',7x,'rhone',8x,'rhob',       
     a            /,' ',80('-'))
           DO j = 1,it
             WRITE (*,3306) t9out(j),(thmout(j,i),i=1,4)
3306         FORMAT (1pe10.3,4e12.3)
           END DO
           WRITE (*,2014)
           WRITE (*,3108)
           READ (*,*)
           GO TO 360
340      CONTINUE                   !Display t,dt,phie,eta,hubcst.
C..........PRINT CAPTION.
           WRITE (*,2014)
           WRITE (*,3100) cy,ct,t9i,t9f,ytmin
           WRITE (*,3102) c(1),c(2),c(3),cosmo,xi(1),xi(2),xi(3)
C..........PRINT THERMODYNAMIC QUANTITIES.
           WRITE (*,3404)
3404       FORMAT (4x,'Temp',8x,'time',8x,'phie',9x,'dt',9x,'eta',10x,
     a            'H',/,' ',80('-'))
           DO j = 1,it
             WRITE (*,3406) t9out(j),tout(j),thmout(j,5),dtout(j),
     a                      etaout(j),hubout(j)
3406         FORMAT (1pe10.3,5e12.3)
           END DO
           WRITE (*,2014)
           WRITE (*,3108)
           READ (*,*)
           GO TO 360
350      CONTINUE                   !Exit.
           GO TO 500
360      CONTINUE
         GO TO 300

C40--------EXIT SECTION---------------------------------------------

400    CONTINUE
         RETURN

C50--------GO BACK TO MENU------------------------------------------

500    CONTINUE
       GO TO 100

       END
