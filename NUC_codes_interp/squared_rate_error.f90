program sigma_v_calculation
    ! This program calculates sigma_v depending on given sigma or s_factor     
        implicit none
       
        ! Type declarations
          !  double precision :: E_G, A_1, A_0, A, sigma_v_int_constant, sigma_v_int, & 
          !  sigma_v, new_sigma_v, E_root, E_root_keV, S_root, f
    
          !  double precision, dimension(13):: S, E
           
          !  double precision, dimension(60) :: T9
           
          !  double precision, dimension(6) :: GL_x_root, GL_w_root
    
          !  double precision :: PI
    
          real*8 :: E_G, A_1, A_0, A, sigma_v_int_constant, sigma_v_int, & 
          sigma_v, new_sigma_v, E_root, E_root_keV, S_root, f, dS_dE_2k, S_root_2k, &
          S_theory_coeff_E_p, S_theory_coeff_E, first_integral, second_integral, f2
    
           real*8, dimension(13):: S, E
           
           real*8, dimension(60) :: T9, squared_err_arr
           
           real*8, dimension(6) :: GL_x_root, GL_w_root

           real*8, dimension(:), allocatable :: S_theory

           !double precision, dimension(:, :), allocatable :: cov_matrix
           real*8, dimension(:, :), allocatable :: cov_matrix

           integer :: nsize
    
           real*8 :: PI

           real*8 :: place_holder
    
    
    
           INTEGER :: Z_0,Z_1
           integer :: i, j, k, reason, E_p_exp, E_exp, m, n
           
        ! Executable statements 
           !Get variables for deuterium burning reaction d(p, g)3He
           PI = ACOS(-1.0)
           Z_0 = 1 !deuterium atomic umber
           Z_1 = 1 !proton atomic number
           S = (/ 0.386, 0.627, 0.850, 0.966, 1.133, 1.223, 1.375, 1.475, 1.648, 1.791, 1.866, 2.073, 2.156 /)
           S = S*10**(-6.0)
           E =  (/ 32.4, 66.7, 99.5, 115.9, 132.9, 149.3, 166.1, 182.7, 199.5, 222.8, 232.9, 252.9, 262.9 /)
           GL_x_root = (/ 0.22284660417926068946, 1.1889321016726230307, 2.9927363260593140777, & 
           5.7751435691045105018, 9.8374674183825899177, 15.982873980601701783 /)
           GL_w_root = (/ 0.45896467394996359357, 0.41700083077212099411, 0.11337338207404497574, & 
           0.010399197453149074899, 0.00026101720281493205948, 8.9854790642962123883e-7 /)
           
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
    
          

        !    S_theory(1) = 0.2121
        !    S_theory(2) = 25.973*10.0**(-3.0)
        !    S_theory(3) = 5.449*10.0**(-6.0)
        !    S_theory(4) = (1.656*10.0**(-9.0))

           open(15, file = 'S_theory_values.dat', status = 'unknown')
            nsize = 0
            do
               read(15,*,iostat=reason) place_holder
               if (.not.(reason==0)) EXIT
               !if (reason==0) EXIT
               nsize= nsize+1


            end do

            allocate(S_theory(nsize))
            allocate(cov_matrix(nsize, nsize))
            rewind(15)

           do i=1, nsize
            read(15, *) S_theory(i)
           end do

           close(15)

           read(*,*)
            
           open(16, file = 'cov_matrix_values_copy.dat', status = 'unknown')

            do i = 1, nsize
                read(16,*) cov_matrix(i, :)
            end do

        !     do i = 1, nsize
        !        write(*, *) S_theory(i)
        !    end do
                
        !     do i = 1, nsize
        !         write(*, *) cov_matrix(i, :)
        !     end do
    
           dS_dE_2k = ((S_theory(2)) + (S_theory(3)*2.0*2000.0) -  & 
           (S_theory(4)*3.0*2000.0**(2.0))) * 10.0**(-6.0)
    
           S_root_2k = ((S_theory(1) + S_theory(2)*2000.0) + (S_theory(3)*2000.0**(2.0)) -  & 
           (S_theory(4)*2000.0**(3.0))) * 10.0**(-6.0)
    
           do i= 1, size(T9)
          
             sigma_v_int_constant = (6.1968*(10**(-14.0))*(A**(-1.0/2))*(T9(i)**(-3.0/2))) * T9(i)/11.605 
             second_integral = 0

                do j = 1, size(GL_x_root)
                    first_integral = 0
                    do k = 1, (size(GL_x_root))
                

                        E_root = GL_x_root(k) * T9(i)/11.605
                        E_root_keV = E_root*10**(3.0)

                        ! IF(E_root_keV <2.0) THEN
                        !     S_root = 0.0
                        ! ELSE IF(E_root_keV>2000) THEN

                        !     S_root = S_root_2k + dS_dE_2k*(E_root_keV-2000.0)

                        ! ELSE
                        !     S_root = (S_theory(1) + (S_theory(2)*E_root_keV) + (S_theory(3)*E_root_keV**(2.0)) -  & 
                        !     (S_theory(4)*E_root_keV**(3.0))) * 10.0**(-6.0)

                        !     write(3, *) "calc", T9(i), E_root_keV, S_root
                        ! END IF
                        f = 0
                        do m = 1, nsize
                            E_p_exp = m - 1
                            do n = 1, nsize
                                E_exp = n - 1
                                f = f + ((T9(i)*10**(3)*GL_x_root(j)/11.605)**E_p_exp)* & 
                                ((T9(i)*10**(3)*GL_x_root(k)/11.605)**E_exp)* cov_matrix(m, n)

                                !if(i == 1) write(*, *) j, k, m, n, f
                            end do
                        end do

                        

                        f = f*10**(-12.0)

                        f = f*EXP(-(((E_G*11.605)/(T9(i)*GL_x_root(k)))**(0.5)))
                        first_integral = first_integral + GL_w_root(k)*f

                        if(i == size(T9)) write(*, *) j, k, f, GL_w_root(k),first_integral

                    
                    end do

                    second_integral = second_integral + GL_w_root(j)*EXP(-(((E_G*11.605)/(T9(i)*GL_x_root(j)))**(0.5)))* & 
                    first_integral

                end do
            
                squared_err_arr(i) = second_integral*(sigma_v_int_constant**2)        
        

             open(7, file = 'squared_err_arr_values_Nature.dat')
             write(7,*) T9(i), squared_err_arr(i),  squared_err_arr(i)**(0.5)

    
            end do
    
     
        
    end program sigma_v_calculation

    !write sigma_v_int_constant