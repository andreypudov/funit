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

submodule (Unit) UnitSuite

    use ifport

    implicit none

contains
    module subroutine init_suite(self, name)
        class(UnitSuite), intent(in out)       :: self
        character(len=*), optional, intent(in) :: name

        self%list => null()
        self%last => null()

        if (present(name)) then
            allocate(character(len(name)) :: self%name)
            self%name = name
        else
            allocate(character(1) :: self%name)
            self%name = ''
        end if

        ! initialize assertion handler
        call setHandler()
    end subroutine

    module subroutine clean_suite(self)
        class(UnitSuite), intent(in out) :: self
        type(UnitCaseEntry), pointer     :: entry

        type(UnitContext) context

        do while (associated(self%list))
            entry => self%list
            self%list => self%list%next

            call entry%case%clean()

            deallocate(entry)
        end do

        deallocate(self%name)
        call context%clean()
    end subroutine

    module subroutine add_suite(self, case)
        class(UnitSuite), intent(in out)     :: self
        class(UnitCase), pointer, intent(in) :: case

        type(UnitCaseEntry), pointer :: entry
        type(UnitCaseEntry), pointer :: previous

        allocate(entry)
        entry%next => null()
        entry%case => case

        call entry%case%init()

        if (.not. associated(self%last)) then
            self%list => entry
        else
            previous      => self%last
            previous%next => entry
        end if

        self%last => entry
    end subroutine

    module subroutine run_suite(self, resume)
        class(UnitSuite), intent(in)  :: self
        logical, optional, intent(in) :: resume

        type(UnitCaseEntry), pointer  :: entry
        class(UnitLogger),   pointer  :: logger
        type(UnitContext) context

        logger => context%getLogger()
        call logger%log(TYPE_SUITE, self%name)

        entry => self%list

        do while (associated(entry))
            call entry%case%run()

            entry => entry%next
        end do
    end subroutine

    subroutine setHandler()
        interface
            subroutine handler(signo, siginfo)
                integer, intent(in) :: signo
                integer, intent(in) :: siginfo
            end subroutine
        end interface

        integer result

        result = ieee_handler('set', 'division', resume)
        if (result /= 0) then
            error stop 'Could not set assertion handler.'
        end if
    end subroutine

    subroutine resume(signo, siginfo)
        integer, intent(in) :: signo
        integer, intent(in) :: siginfo

        print '(A)', 'ASSERTION'
    end subroutine
end submodule
