program sigma_v_calculation
! This program calculates sigma_v depending on given sigma or s_factor     
    implicit none
   
    ! Type declarations
       double precision :: E_G, A_1, A_0, A, sigma_v_int_constant, sigma_v_int, sigma, sigma_v, new_sigma_v

       double precision, dimension(13):: S, E
       
       double precision, dimension(60) :: T9
       
       double precision, dimension(6) :: GL_root

       double precision :: PI

       INTEGER :: Z_0,Z_1
       integer :: i, j
       
    ! Executable statements 
       !Get variables for deuterium burning reaction d(p, g)3He
       PI = DACOS(-1.D0)
       Z_0 = 1 !deuterium atomic umber
       Z_1 = 1 !proton atomic number
       S = (/ 0.386, 0.627, 0.850, 0.966, 1.133, 1.223, 1.375, 1.475, 1.648, 1.791, 1.866, 2.073, 2.156 /)
       S = S*10**(-6.0)
       E =  (/ 32.4, 66.7, 99.5, 115.9, 132.9, 149.3, 166.1, 182.7, 199.5, 222.8, 232.9, 252.9, 262.9 /)
       GL_root = (/ 0.22284660417926068946, 1.1889321016726230307, 2.9927363260593140777, & 
       5.7751435691045105018, 9.8374674183825899177, 15.982873980601701783 /)
       E = E*10**(-3.0)
       A_1 = 1.007277 !atomic mass of proton
       A_0 = 2.01410177811 ! atomic mass of deuterium
       A = (A_0*A_1)/(A_0+A_1) !reduced atomic mass

       E_G = (0.98948*Z_0*Z_1*SQRT(A))**2

       ! T9 = (/ 40.0, 20.0, 10.0, 5.0, 2.5, 1.25, 1.0, 0.8, 0.6, 0.4, 0.2, 0.1, 0.005, 0.0025 /)

       T9 = (/0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009,0.010, 0.011, 0.012,0.013, 0.014, & 
       0.015, 0.016, 0.018, 0.020, 0.025, 0.030, 0.040, 0.050, 0.060, 0.070, 0.080, 0.090, 0.100, 0.110, 0.120, 0.130, &
       0.140, 0.150, 0.160, 0.180, 0.200, 0.250, 0.300, 0.350, 0.400, 0.450, 0.500, 0.600, 0.700, 0.800, 0.900, 1.000, &
       1.250, 1.500, 1.750, 2.000, 2.500, 3.000, 3.500, 4.000, 5.000, 6.000, 7.000, 8.000, 9.000, 10.00 /)

      

       do i= 1, size(T9)

         sigma_v_int_constant = 6.1968*(10**(-14.0))*(A**(-1.0/2))*(T9(i)**(-3.0/2))  
         sigma_v_int = 0

         do j = 1, (size(S) - 1)
            
            sigma = (S(j)/E(j))*EXP(-SQRT(E_G/E(j)))
            sigma_v_int = sigma_v_int + sigma*E(j)*EXP(-11.605*E(j)/T9(i)) * (E(j+1) - E(j))


         end do

         sigma_v = sigma_v_int_constant*sigma_v_int
    
         open(1, file = 'sigma_v_d(p,g)3He.dat', status = 'unknown')  
         write(1,*) T9(i), sigma_v

         new_sigma_v = (2.65e+3*(T9(i))**(-(2.0/3.0))*EXP(-3.720/((T9(i))**(1.0/3.0))) &
         *(1.+.112*((T9(i))**(1.0/3.0))+1.99*(T9(i))**(2.0/3.0)+1.56*(T9(i))+.162*((T9(i))**(4.0/3.0))+.324*((T9(i))**(5.0/3.0))))&
         /(6.022*10**(23.0))

         open(2, file = 'new_sigma_v_d(p,g)3He.dat', status = 'unknown')  
         write(2,*) T9(i), new_sigma_v





      end do

 
    
end program sigma_v_calculation