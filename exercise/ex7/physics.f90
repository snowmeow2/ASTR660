!---------------------------------------------------
! The physics module
!
module physics
    use solver
    implicit none
    contains
        subroutine update(time,dt,x0,y0,vx0,vy0,x,y,vx,vy)
            implicit none
            real, intent(in)  :: time, dt,x0, y0, vx0, vy0
            real, intent(out) :: x,y,vx,vy
            integer,parameter :: n = 4
            real,dimension(n) :: yin, ynext

            ! pack yin
            yin(1) = x0
            yin(2) = y0
            yin(3) = vx0
            yin(4) = vy0

            ! call euler(4,yin,ynext,time,dt, my_func)
            call rk2(4,yin,ynext,time,dt, my_func)
            !call rk4(4,yin,ynext,time,dt, my_func)

            ! unpack ynext
            x  = ynext(1)
            y  = ynext(2)
            vx = ynext(3)
            vy = ynext(4)

            return
        end subroutine update

        subroutine my_func(n,t,yin,k)
            use constants, only : g
            implicit none
            integer, intent(in)  :: n   ! number of ODEs
            real, intent(in)     :: t   ! not used here because dydt is constant
            real,dimension(n),intent(in)  :: yin   ! y
            real,dimension(n),intent(out) :: k     ! dydt


            ! n = 4 for the angry bird problem
            k(1)  =  yin(3) ! vx
            k(2)  =  yin(4) ! vy
            k(3)  =  0 ! ax
            k(4)  =  -g ! ay

            return
        end subroutine my_func

end module physics

