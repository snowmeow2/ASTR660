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

    uold = u

    do i = istart, iend
        ! c > 0
        ! upwind
        u(i) = uold(i) - c*dt/dx*(uold(i)-uold(i-1)) 
        
        ! FTCS
        u(i) = uold(i) - c*dt/2/dx*(uold(i+1)-uold(i-1))
    enddo

end subroutine update

