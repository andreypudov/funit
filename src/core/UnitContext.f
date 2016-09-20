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

submodule (Unit) UnitContext

    implicit none

    type(UnitContext), pointer :: instance

contains
    module subroutine init_context()
        type(ConsoleLogger), pointer :: logger

        if (.not. associated(instance)) then
            allocate(instance)
            allocate(logger)

            call logger%init()
            instance%logger => logger
        end if
    end subroutine

    module subroutine clean_context()
        if (.not. associated(instance)) then
            deallocate(instance%logger)
            deallocate(instance)
        end if
    end subroutine

    module function getLogger_context() result(value)
        class(UnitLogger),  pointer :: value

        call init_context()
        value => instance%logger
    end function

    module function getSuite_context() result(value)
        class(UnitSuite), pointer :: value

        value => instance%suite
    end function

    module function getCase_context() result(value)
        class(UnitCase), pointer :: value

        value => instance%case
    end function

    module function getProcedure_context() result(value)
        class(UnitProcedureEntry), pointer :: value

        value => instance%procedure
    end function

    module subroutine setSuite_context(suite)
        class(UnitSuite), pointer, intent(in) :: suite

        instance%suite => suite
    end subroutine

    module subroutine setCase_context(case)
        class(UnitCase), pointer, intent(in) :: case

        instance%case => case
    end subroutine

    module subroutine setProcedure_context(procedure)
        class(UnitProcedureEntry), pointer, intent(in) :: procedure

        instance%procedure => procedure
    end subroutine
end submodule
