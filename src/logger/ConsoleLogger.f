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

    type(ConsoleLogger), pointer :: instance

contains
    module subroutine init_consoleLogger(self, suiteName)
        class(ConsoleLogger), intent(in out) :: self
        character(len=*),     intent(in)     :: suiteName

        if (.not. associated(instance)) then
            allocate(instance)
            call instance%UnitLogger%init(suiteName)

            instance%case      = 0
            instance%procedure = 0

            call instance%log(TYPE_SUITE, instance%suiteName)
            call printSeparator()
        end if
    end subroutine

    module subroutine clean_consoleLogger(self)
        class(ConsoleLogger), intent(in out) :: self

        character(len=16) buffer
        real finish

        call cpu_time(finish)

        call printSeparator()
        write (buffer, '(F12.3)') finish - instance%start
        print '(A,A,A)', 'Tests completed in ', trim(adjustl(buffer)), ' seconds.'

        call instance%UnitLogger%clean()

        deallocate(instance)
        instance => null()
    end subroutine

    module subroutine log_consoleLogger(self, type, message)
        class(ConsoleLogger), intent(in) :: self
        integer,              intent(in) :: type
        character(len=*),     intent(in) :: message

        select case(type)
        case(TYPE_SUITE)
            instance%case      = 0
            instance%procedure = 0

            print '(A)', message
        case(TYPE_CASE)
            instance%case      = instance%case + 1
            instance%procedure = 0

            print '(I1,1X,A)', instance%case, message
        case(TYPE_PROCEDURE)
            instance%procedure = instance%procedure + 1

            print '(4X,A)', message
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
