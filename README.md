# Car Management System Setup and Execution Guide

The Car Management System is a distributed application designed to simulate a traffic environment. It involves multiple nodes, including a master node, that work together to manage cars, sensors, and traffic dynamics. Follow these steps to set up and run the Car Management System:

## Step 1: Node Setup

1. Open five terminal/command prompt windows, each representing a separate Erlang node.
2. Designate one of these nodes as the master node. The master node will coordinate the simulation.

## Step 2: Navigation

1. In each terminal, navigate to the directory containing the source files of the Car Management System.

## Step 3: Running Erlang Nodes

1. For each of the five nodes, run the Erlang shell using the following command:
   
   ```
   erl -setcookie cooki -name NODEN@HOSTNAME
   ```
   
   Replace `NODEN` with a unique node identifier (e.g., `node1`, `node2`, etc.), and replace `HOSTNAME` with the hostname of the machine.

2. For the master node, use the following command:

   ```
   erl -setcookie cooki -name master@HOSTNAME
   ```

## Step 4: Compilation

1. Inside each Erlang shell, compile all the necessary source files by executing the following commands:

   ```
   c(cars).
   c(server).
   c(main).
   c(graphics).
   c(car_a_s).
   c(car_monitor).
   c(cars_manager).
   c(traffic_light).
   ```

## Step 5: Starting the Simulation

1. In the Erlang shell of the master node, start the simulation by running:

   ```
   main:start().
   ```

## Expected Behavior

The Car Management System simulation will run with the following behaviors:

- **Accidents**: When accidents occur, cars involved will disappear from the map and be removed.
- **Car Speed**: Each car's speed is set to be uniform.
- **Car Following**: When a car approaches another car from behind, it waits for the front car to move before proceeding.
- **Car Generation**: As cars exit, new cars are immediately created and inserted into the map from entrances.
- **Node Resilience**: If one of the nodes is killed, the system remains operational as the nodes backup each other's data.

Please note that this guide assumes that you have the necessary Erlang runtime environment and the source code files in place. Make sure to replace placeholders like `NODEN` and `HOSTNAME` with actual node identifiers and hostnames. Also, ensure that the mentioned behaviors are correctly implemented in your source code.

## Graphics with wxWidgets
In our project, we've chosen to utilize wxWidgets for handling graphics and user interface components. wxWidgets is a powerful and modern C++ framework that provides a comprehensive set of tools for creating visually appealing and interactive applications.

**Why wxWidgets?**
Modern and Cross-Platform: One of the primary reasons we opted for wxWidgets is its ability to create cross-platform applications. Whether our users are on Windows, macOS, or Linux, our graphics and interface will provide a consistent experience.

**Ease of Use**: wxWidgets offers a user-friendly interface, making it an ideal choice for both experienced developers and newcomers. Its intuitive design allows us to focus on crafting engaging graphics rather than getting bogged down in complicated implementation details.

**Rich Library of Widgets**: The framework boasts an extensive collection of widgets and controls that can be easily integrated into our application. This saves us time and effort by providing pre-built components that are customizable to suit our specific needs.

**Customization**: Despite its ease of use, wxWidgets also offers a high level of customization. This enables us to tailor the graphics and interface to align with our project's unique requirements and design aesthetics.

**Community and Support**: With an active and vibrant community, wxWidgets ensures that we have access to ample documentation, tutorials, and forums. This support network is invaluable in overcoming challenges and making the most of the framework's capabilities.

**Open-Source**: As an open-source project, wxWidgets aligns with our values and allows us to contribute to its growth while benefiting from continuous improvements made by the community.
