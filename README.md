# eRINA (Embedded Recursive InterNetwork Architecture)

## Overview
eRINA, short for Embedded Recursive InterNetwork Architecture, is an undergraduate senior design project created at PennState under mentorship from [AdaCore](https://www.adacore.com/) that aims to bring the power of Recursive InterNetwork Architecture (RINA) to embedded platforms. In this README, we will introduce you to the project, its goals, and how you can get started.

## What is eRINA?
RINA (Recursive InterNetwork Architecture) is an innovative networking architecture that has gained significant attention in recent years for its potential to revolutionize how we design and manage networks. At its core, RINA is a concept that focuses on the fundamental principles of networking, such as naming, addressing, and routing, in a clean and recursive manner. RINA was designed to address many of the shortcomings presented in the traditional layered TCP/IP architecture and OSI model. More about the philosophy and design descisions of the architecture can be found in the documentation provided under [RINA resources](#rina-resources).

eRINA aims to take the fundamental principles of RINA and implement them as a software stack solution in Ada, specifically designed for embedded platforms. This means that eRINA provides a robust and efficient networking solution for resource-constrained devices, allowing them to benefit from the advantages of RINA's clean, lightweight, and scalable design.

## RINA Resources
[ETSI GR NGP 009 :: Next Generation Protocols (NGP)](https://www.etsi.org/deliver/etsi_gr/NGP/001_099/009/01.01.01_60/gr_ngp009v010101p.pdf) - An example of a non-IP network protocol architecture based on RINA design principles.

[Patterns in Network Architecture (John Day)](https://www.oreilly.com/library/view/patterns-in-network/9780132252423/) - The principles behind RINA were first presented by John Day in his 2008 book "Patterns in Network Architecture: A return to Fundamentals".

[Error and Flow Control Protocol (EFCP) Design and Implementation](https://ieeexplore.ieee.org/document/8685905) - A Data Transfer Protocol for the Recursive InterNetwork Architecture

## Testing Setup
1. Install rlite package on system
2. Ensure `modprobe rlite` and `modprobe rlite-normal` are run to load those kernel modules
3. Run `sudo rlite-uipcps` before running tests
4. Enter `eRINA_Tests` directory with `cd eRINA_Tests`
5. Run tests: `alr build` and `alr run`

## Tests Implemented Progress

| Test Suite | Implementation Progress |
| ---------- | ----------------------- |
| TS-001     |   :white_check_mark:    |