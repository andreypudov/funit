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
    public log
    private

    integer, parameter, public :: TYPE_RUNNER = 0
    integer, parameter, public :: TYPE_SUITE  = 1
    integer, parameter, public :: TYPE_CASE   = 2
    integer, parameter, public :: TYPE_RESULT = 3

    integer, parameter, private :: TERMINAL_WIDTH = 80

contains
    subroutine log(type, name, details, status)
        integer,                    intent(in)     :: type
        character(len=*),           intent(in)     :: name
        character(len=*), optional, intent(in)     :: details
        logical,          optional, intent(in)     :: status

        select case(type)
        case(TYPE_CASE)
            print '(A)', name
        end select
    end subroutine
end module
