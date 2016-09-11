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

    integer, parameter, public :: OFF     = 10000 ! turn off logging
    integer, parameter, public :: SEVERE  = 1000  ! serious failure
    integer, parameter, public :: WARNING = 900   ! potential problem
    integer, parameter, public :: INFO    = 800   ! informational messages
    integer, parameter, public :: CONFIG  = 700   ! static configuration messages
    integer, parameter, public :: FINE    = 500   ! tracing information
    integer, parameter, public :: FINER   = 400   ! a fairly detailed tracing message
    integer, parameter, public :: FINEST  = 300   ! a highly detailed tracing message
    integer, parameter, public :: ALL     = 0     ! all messages should be logged

    type, private :: DefaultLogger
    private
        integer :: level = INFO
        real    :: start = 0.0
    contains
        procedure, pass, public :: init  => init_defaultLogger
        procedure, pass, public :: clean => clean_defaultLogger

        procedure, pass, public :: getLevel => getLevel_defaultLogger
        procedure, pass, public :: setLevel => setLevel_defaultLogger

        procedure, pass, public :: log => log_defaultLogger
    end type

    type, extends(DefaultLogger), public :: ConsoleLogger
    private
    contains
        procedure, pass, public :: init  => init_consoleLogger
        procedure, pass, public :: clean => clean_consoleLogger

        procedure, pass, public :: log => log_consoleLogger
    end type

    interface
        module subroutine init_defaultLogger(self)
            class(DefaultLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_defaultLogger(self)
            class(DefaultLogger), intent(in out) :: self
        end subroutine

        module function getLevel_defaultLogger(self) result(value)
            class(DefaultLogger), intent(in) :: self

            integer value
        end function

        module subroutine setLevel_defaultLogger(self, level)
            class(DefaultLogger), intent(in out) :: self
            integer, intent(in)           :: level
        end subroutine

        module subroutine log_defaultLogger(self, message, level)
            class(DefaultLogger), intent(in) :: self
            character(len=*),     intent(in) :: message
            integer, optional,    intent(in) :: level
        end subroutine
    end interface

    interface
        module subroutine init_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

        module subroutine clean_consoleLogger(self)
            class(ConsoleLogger), intent(in out) :: self
        end subroutine

        module subroutine log_consoleLogger(self, message, level)
            class(ConsoleLogger), intent(in) :: self
            character(len=*),     intent(in) :: message
            integer, optional,    intent(in) :: level
        end subroutine
    end interface
end module
