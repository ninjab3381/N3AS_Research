

                                                            PROGRAM kja


               
        double precision T9_arr(60)
        double precision sigma_v_arr(60)
        OPEN(1, FILE="sigma_v_d(p,g)3He.dat", status = 'unknown')

        DO i = 1, 60
            READ(1, *) T9_arr(i), sigma_v_arr(i)

        END DO
                                                         


