# Sokoban Editor

The Sokoban Editor is a planning as satisfiability solver for Sokoban. It based on [Snowman-Editor](https://github.com/GerardMT/Snowman-Editor), but much more simpler.

The editor can solve a given level ***optimally*** using the Yices SMT.

## Installation
Download the latest version from the [release page](https://github.com/GerardMT/Sokoban-Editor/releases) or [build](https://github.com/GerardMT/Sokoban-Editor#build) the project.

The editor requires at least Java SE 11. 

## Usage
First execution to generate the config file:

    java -jar sokoban_editor.jar

Once the config file has been generated, set the Yices 2 solver path.

To solve a given level:

    java -jar sokoban_editor.jar smt_reachability ./levels/toy/toy5.lvl

Where:
- **smt_basic** - minimizes character + box movements.
- **smt_reachability** - minimizes box movements guaranteeing valid character movements.
- **smt_reachability_folding** - minimizes change of directions of box movements (box movement of n positions in a straight line has a cost of 1).


## Build
Download the source code:

    git clone https://github.com/GerardMT/Sokoban-Editor
    cd Sokoban-Editor

Download the [Planner](https://github.com/GerardMT/Planner) submodule:

    git submodule update --init --recursive

The project uses the SBT build tool. To compile the project run: 
    
    sbt compile
    sbt package

I personally use IntelliJ IDEA which eases the Scala/Java environment setup.

## Example
Execution of:

    java -jar sokoban_editor. jar smt_reachability ./levels/microban/levels/microban1.lvl

Output:
```
timesteps=1 time=525
timesteps=2 time=276
timesteps=3 time=155
timesteps=4 time=152
timesteps=5 time=107
timesteps=6 time=126
timesteps=7 time=197
timesteps=8 time=242
STATE 0
####xx
# .#xx
#  ###
#*@  #
#  $ #
#  ###
####xx

...

STATE 7
####xx
# .#xx
# $###
#*@  #
#    #
#  ###
####xx
STATE 8
####xx
# *#xx
# @###
#*   #
#    #
#  ###
####xx

plan=dlUrrrdLullddrUluRuulDrddrruLdlUU
lenght=33
box_actions=8
time=2.07
```