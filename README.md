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

## Prerequisites

It is recommended to use Ubuntu 22.04.4 LTS (Jammy Jellyfish).

## eRINA_Tests Setup
1. Install [rlite](https://github.com/rlite/rlite) package on system
2. Ensure `modprobe rlite` and `modprobe rlite-normal` are run to load those kernel modules
3. Run `sudo rlite-uipcps` before running tests
4. Enter `eRINA_Tests` directory with `cd eRINA_Tests`
5. Run tests: `alr build` and `alr run`

## eRINA_Linux Setup
1. Instal [rlite](https://github.com/rlite/rlite) package on system
2. Ensure `modprobe rlite` and `modprobe rlite-normal` are run to load those kernel modules
3. Run `sudo rlite-uipcps`
4. Enter `eRINA_Linux` directory with `cd eRINA_Linux`
5. Run: `alr build` and `alr run`

## eRINA_STM32F7 Setup
1. If you’re using a virtual machine with Windows as your host operating system, you will need to get the necessary drivers on your Windows machine first. This is required to get USB passthrough to the VM working. If you’re running natively on Ubuntu, you may ignore this step.

    Head to: 
    [32F746GDISCOVERY - Discovery kit with STM32F746NG MCU, STM32F746G-DISCO, STMicroelectronics](https://www.st.com/en/evaluation-tools/32f746gdiscovery.html#tools-software)

    Download “STSW-LINK009” under “Software Development Tools” (you may need to register for an account).

    Run the executable and install the drivers as prompted.

    Boot up your virtual machine and make sure the “STMicroelectronics STM32 STLink” USB device is enabled.

2. Alire does not natively index the GNAT academic program software. This index includes the dependencies we need. To add it, run the following command in the terminal:

    `alr index --add git+https://github.com/GNAT-Academic-Program/alire-index --name gap`

    Without this, you will not be able to download/update the stm32 package dependencies.

3. OpenOCD (Open On-Chip-Debugger) is a tool that provides a debugging interface which allows developers to interact with and debug embedded systems. OpenOCD is the tool that we will be using to program the flash memory of the microcontroller. Install this package using the command below:

    `sudo apt install openocd`
4. Ensure the working directory is `eRINA_STM32F`. If it is not, from the repository's root, run `cd eRINA_STM32F`.
5. Run `alr build`
6. Connect a STM32F7 to your host machine via USB-A port and connect board to network via RJ45 port. Typically, this means connecting the board via ethernet to a router or access point.
7. Flash the board using the following command:
    
    `openocd -f board/stm32f7discovery.cfg -c 'program bin/demo verify reset exit'`

