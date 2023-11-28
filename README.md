# eRINA (Embedded Recursive InterNetwork Architecture)

## Overview
eRINA, short for Embedded Recursive InterNetwork Architecture, is an undergraduate senior design project created at PennState under mentorship from [AdaCore](https://www.adacore.com/) that aims to bring the power of Recursive InterNetwork Architecture (RINA) to embedded platforms. In this README, we will introduce you to the project, its goals, and how you can get started.

## What is eRINA?
RINA (Recursive InterNetwork Architecture) is an innovative networking architecture that has gained significant attention in recent years for its potential to revolutionize how we design and manage networks. At its core, RINA is a concept that focuses on the fundamental principles of networking, such as naming, addressing, and routing, in a clean and recursive manner. RINA was designed to address many of the shortcomings presented in the traditional layered TCP/IP architecture and OSI model. More about the philosophy and design descisions of the architecture can be found in the documentation provided under [RINA resources](#rina-resources).

eRINA aims to take the fundamental principles of RINA and implement them as a software stack solution in Ada, specifically designed for embedded platforms. This means that eRINA provides a robust and efficient networking solution for resource-constrained devices, allowing them to benefit from the advantages of RINA's clean, lightweight, and scalable design.

## RINA Resources

## Testing Setup
1. Install rlite package on system
2. Ensure `modprobe rlite` and `modprobe rlite-normal` are run to load those kernel modules
3. Run `sudo rlite-uipcps` before running tests
4. Enter `eRINA_Tests` directory with `cd eRINA_Tests`
5. Run tests: `alr build` and `alr run`
