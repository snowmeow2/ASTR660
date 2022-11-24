subroutine evolution()
    use Simulation_data
    use IO, only : output
    implicit none

    integer :: n
    real    :: dt, time

    n        = 0
    time     = 0.0

    dt = abs(dx/c)*cfl

    do while(time .le. tend)

        ! reset boundary condition
        call boundary(u)

        ! dump output times with frequency set by io_interval
        if (mod(n,io_interval) .eq. 0) then
            print *, "n =", n ," Time =", time
            call output(n,time)
        endif

        ! update the solution
        call update(time, dt)
        
        n    = n + 1
        time = time + dt
    enddo

end subroutine evolution


subroutine update(time, dt)
    use Simulation_data
    implicit none
    real, intent(in) :: time ,dt
    integer :: i
    real    :: FL, FR

    uold = u

    do i = istart, iend
        call flux(i, dt, FL, FR)
        u(i) = uold(i) - dt/dx*(FR - FL)
    enddo

end subroutine update

!
! Routine to compute the fluxes
!
subroutine flux(i, dt, FL, FR)
    use Simulation_data
    implicit none
    integer, intent(in) :: i
    real, intent(in)    :: dt
    real, intent(out)   :: FL, FR

    ! Arithmetic average method
    FL = 0.5*(uold(i-1) + uold(i))
    FR = 0.5*(uold(i) + uold(i+1))

    ! The Lax-Friedrichs method
    FL = 0.5*(uold(i-1) + uold(i)) - 0.5*dx/dt*(uold(i) - uold(i-1))
    FR = 0.5*(uold(i) + uold(i+1)) - 0.5*dx/dt*(uold(i+1) - uold(i))

    ! The upwind method
    !FL = ! TODO
    !FR = ! TODO

    ! The Lax-Wendroff method
    !FL = ! TODO
    !FR = ! TODO

end subroutine flux