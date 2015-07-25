# Middernacht 

Middernacht is torrent tracker written in Erlang. It is a pet project of mine to
understand the bitorrent protocol better. I would like to see this project
mature and will try to do so.

If you want to contribute to this project, you are more than welcome to contact
me via any means. I can be found idling on #erlang, freenode.

# Installation
### Dependencies
There are no dependencies except a working Erlang installation.

### Building
 - Create a directory `ebin` in the root of the project.
 - Make the source files `make all`

### Execution

There is a bash script present to execute the project easily. However, for the time being it does not work.

After building all the files you can start an Erlang shell from the root of the project `erl -pa ebin` which will load all the compiled files into the shell. To start a tracker listening on port `4702` you can evaluate `main:start(4702).`. Debugging information will be displayed inside the shell.

### Usage

The tracker currently accepts all requests on a given port. I have found that not all torrent clients agree with a tracker url in the form of `http://example.com:4702`, so it is suggested to use `http://example.com:4702/foo` instead.


# TODO

 - Introduce peer database
 - Make http part more stable
 - Clean up code w.r.t good style and convention
 - Use reltools
 - Handle compact responses
 - Implement supervisors
 - Invent time-making-machine to make more time
 - Use logging framework

