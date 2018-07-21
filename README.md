# A unit testing library for Fortran

Unit is a lightweight library for writing, administering, and running unit tests in Fortran. It is an instance of the xUnit architecture and provides Fortran programmers a basic testing functionality with a flexible variety of user interfaces.

Unit is built as a dynamic library which is linked with the user's testing code. It uses a lightweight library for building test structures, and provides a rich set of assertions for testing common data types. In addition, several different interfaces are provided for running tests and reporting results.

## Features

* An [XUnit](https://en.wikipedia.org/wiki/XUnit) test library. 
* Test discovery.
* A rich set of assertions.
* Fatal and non-fatal failures.
* Various options for running the tests.
* XML test report generation.

[comment]: # (User-defined assertions.)
[comment]: # (Value-parameterized tests.)
[comment]: # (Type-parameterized tests.)
[comment]: # (Death tests.)

## Requirements

Unit is designed to have fairly minimal requirements to build and use with your projects, but there are some. Currently, Unit requires Intel(R) Parallel Studio XE 2017 Fortran compiler and supports Linux, Windows, and Mac OS X.
