# erlang-secure-tcp-limiter (STL)

## Goal
This project was designed to limit count of messages sent from incoming connections. 
You can declare a function for calculating block time given the count of messages per second, minute, hour and day.

-----------

## Getting started
* Usage of *STL* is very similar to erlang *SSL* library.
* Install *Riak* database. The *STL* uses it for storing logs.
* You can see everything u need in `examples` directory.
