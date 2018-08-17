

There are two `main`s entry

example.Hello reads data from data file
 which was provided on "https://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt"

example.HelloRandom generates its own data randomly at start

The code runs on a single thread to get its standard speed and also the basic logic.

There are some optimizations to consider.
For example, cleaning short and possibly useless routes of result of the vertical linking of routes would reduce the number of routes to link vertically in the next iteration.
