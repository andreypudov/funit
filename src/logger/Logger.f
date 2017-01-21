!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2017 Andrey Pudov
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
    integer, parameter, public :: TYPE_RESULT    = 3

    type, public :: UnitLogger
    private
        integer :: case
        integer :: passed
        integer :: failed

        real :: start  = 0.0 ! suite execution time
        real :: finish = 0.0 ! case execution time
    contains
        procedure, pass, public :: init  => init_unitLogger
        procedure, pass, public :: clean => clean_unitLogger

        procedure, pass, public :: log => log_unitLogger
    end type

    type, extends(UnitLogger), public :: ConsoleLogger
    private
    contains
        procedure, pass, public :: init  => init_consoleLogger
        procedure, pass, public :: clean => clean_consoleLogger

        procedure, pass, public :: log => log_consoleLogger
    end type

    type, extends(UnitLogger), public :: JSONLogger
    private
    contains
        procedure, pass, public :: init  => init_JSONLogger
        procedure, pass, public :: clean => clean_JSONLogger

        procedure, pass, public :: log => log_JSONLogger
    end type

    interface
        module subroutine init_unitLogger(self)
            class(UnitLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_unitLogger(self)
            class(UnitLogger), intent(in out) :: self
        end subroutine

        module subroutine log_unitLogger(self, type, name, details, status)
            class(UnitLogger),          intent(in out) :: self
            integer,                    intent(in)     :: type
            character(len=*),           intent(in)     :: name
            character(len=*), optional, intent(in)     :: details
            logical,          optional, intent(in)     :: status
        end subroutine
    end interface

    interface
        module subroutine init_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

       module subroutine log_consoleLogger(self, type, name, details, status)
           class(ConsoleLogger),       intent(in out) :: self
           integer,                    intent(in)     :: type
           character(len=*),           intent(in)     :: name
           character(len=*), optional, intent(in)     :: details
           logical,          optional, intent(in)     :: status
       end subroutine
   end interface

   interface
        module subroutine init_JSONLogger(self)
            class(JSONLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_JSONLogger(self)
            class(JSONLogger), intent(in out) :: self
        end subroutine

       module subroutine log_JSONLogger(self, type, name, details, status)
           class(JSONLogger),       intent(in out) :: self
           integer,                    intent(in)     :: type
           character(len=*),           intent(in)     :: name
           character(len=*), optional, intent(in)     :: details
           logical,          optional, intent(in)     :: status
       end subroutine
    end interface
end module
