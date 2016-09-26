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

module Logger

    implicit none
    private

    integer, parameter, public :: TYPE_RUNNER    = 0
    integer, parameter, public :: TYPE_SUITE     = 1
    integer, parameter, public :: TYPE_CASE      = 2
    integer, parameter, public :: TYPE_REASON    = 3
    integer, parameter, public :: TYPE_RESULT    = 4

    type, public :: UnitLogger
    private
        integer :: case
        integer :: passed
        integer :: failed

        real :: start = 0.0
    contains
        procedure, pass, public :: init  => init_unitLogger
        procedure, pass, public :: clean => clean_unitLogger

        procedure, pass, public :: log => log_unitLogger
    end type

    type, extends(UnitLogger), public :: ConsoleLogger
    private
        character(len=:), pointer :: reason => null()
    contains
        procedure, pass, public :: init  => init_consoleLogger
        procedure, pass, public :: clean => clean_consoleLogger

        procedure, pass, public :: log => log_consoleLogger
    end type

    type(UnitLogger),    pointer, private :: unitLoggerInstance

    interface
        module subroutine init_unitLogger(self)
            class(UnitLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_unitLogger(self)
            class(UnitLogger), intent(in out) :: self
        end subroutine

        module subroutine log_unitLogger(self, type, message, status)
            class(UnitLogger), intent(in out) :: self
            integer,           intent(in)     :: type
            character(len=*),  intent(in)     :: message
            logical, optional, intent(in)     :: status
        end subroutine
    end interface

    interface
        module subroutine init_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

       module subroutine log_consoleLogger(self, type, message, status)
           class(ConsoleLogger), intent(in out) :: self
           integer,              intent(in)     :: type
           character(len=*),     intent(in)     :: message
           logical, optional,    intent(in)     :: status
       end subroutine
    end interface
end module
