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
- **Car Speed**: Each car's speed is set to 15 km/h.
- **Car Following**: When a car approaches another car from behind, it waits for the front car to move before proceeding.
- **Car Generation**: As cars exit, new cars are immediately created and inserted into the map from entrances.
- **Node Resilience**: If one of the nodes is killed, the system remains operational as the nodes backup each other's data.

Please note that this guide assumes that you have the necessary Erlang runtime environment and the source code files in place. Make sure to replace placeholders like `NODEN` and `HOSTNAME` with actual node identifiers and hostnames. Also, ensure that the mentioned behaviors are correctly implemented in your source code.
