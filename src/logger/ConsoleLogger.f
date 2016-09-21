!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the 'Software'), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!

submodule (Logger) ConsoleLogger

    implicit none

    integer, parameter :: TERMINAL_WIDTH = 80

contains
    module subroutine init_consoleLogger(self)
        class(ConsoleLogger), intent(in out) :: self

        call self%UnitLogger%init()

        self%case    = 0
        self%success = 0
        self%failure = 0
    end subroutine

    module subroutine clean_consoleLogger(self)
        class(ConsoleLogger), intent(in out) :: self

        call self%UnitLogger%clean()
    end subroutine

    module subroutine log_consoleLogger(self, type, message, status)
        class(ConsoleLogger), intent(in out) :: self
        integer,              intent(in)     :: type
        character(len=*),     intent(in)     :: message
        logical, optional,    intent(in)     :: status

        character(len=16)  buffer
        character(len=128) name
        real finish

        select case(type)
        case(TYPE_SUITE)
            self%case    = 0
            self%success = 0
            self%failure = 0

            print '(A)', message
            call printSeparator()
        case(TYPE_CASE)
            self%case = self%case + 1

            print '(I0,1X,A)', self%case, message
        case(TYPE_PROCEDURE)
            ! TODO add error handling
            if (status) then
                self%success = self%success + 1
                buffer = 'OK'
            else
                self%failure = self%failure + 1
                buffer = 'FAILED'
            end if

            name = message

            print '(4X,A70,A6)', name, trim(adjustl(buffer))
        case(TYPE_RESULT)
            call cpu_time(finish)

            call printSeparator()
            write (buffer, '(F12.3)') finish - self%start
            print '(A,A,A,A,I0,A,I0,A,I0)', 'Tests completed in ', trim(adjustl(buffer)), ' seconds. ', &
                'Total: ', (self%success + self%failure), &
                    ', success: ', self%success, &
                    ', failed: ',  self%failure
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
