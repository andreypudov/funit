!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Logger) ConsoleLogger

    implicit none

    integer, parameter :: TERMINAL_WIDTH = 80

contains
    module subroutine init_consoleLogger(self)
        class(ConsoleLogger), intent(in out) :: self

        call self%UnitLogger%init()

        self%case   = 0
        self%passed = 0
        self%failed = 0
    end subroutine

    module subroutine clean_consoleLogger(self)
        class(ConsoleLogger), intent(in out) :: self

        call self%UnitLogger%clean()
    end subroutine

    module subroutine log_consoleLogger(self, type, name, details, status)
        class(ConsoleLogger),       intent(in out) :: self
        integer,                    intent(in)     :: type
        character(len=*),           intent(in)     :: name
        character(len=*), optional, intent(in)     :: details
        logical,          optional, intent(in)     :: status

        character(len=16) buffer
        character(len=80) title

        real now

        select case(type)
        case(TYPE_RUNNER)
            self%case   = 0
            self%passed = 0
            self%failed = 0

            print '(A)', name
            call printSeparator()

            call cpu_time(self%finish)
        case(TYPE_SUITE)
            call cpu_time(now)
            self%case = self%case + 1

            write (buffer, '(F12.3)') now - self%finish
            write (title, '(I0,1X,A)') self%case, name
            write (*, '(A72,1X,A,A,A)') adjustl(title), &
                '[', trim(adjustl(buffer)), ']'

            self%finish = now
        case(TYPE_CASE)
            ! TODO add error handling
            if (status) then
                self%passed = self%passed + 1
                buffer      = 'OK'
                title       = name

                ! no output for success cases
                return
            else
                self%failed = self%failed + 1
                buffer      = 'FAILED'
                title       = name

                if (len(trim(details)) > 0) then
                    title  = name // ' [' // trim(adjustl(details)) // ']'
                end if
            end if

            print '(4X,A70,A6)', title, trim(adjustl(buffer))
        case(TYPE_RESULT)
            call cpu_time(self%finish)

            call printSeparator()
            write (buffer, '(F12.3)') self%finish - self%start
            print '(A,A,A,A,I0,A,I0,A,I0)', &
                'Tests completed in ', trim(adjustl(buffer)), ' seconds. ', &
                'Total: ', (self%passed + self%failed), &
                    ', passed: ', self%passed, &
                    ', failed: ',  self%failed
        end select
    end subroutine

    subroutine printSeparator()
        integer index

        do index = 1, TERMINAL_WIDTH
            write (*, '(A)', advance = 'no') '-'
        end do

        print '(A)', ''
    end subroutine
end submodule
