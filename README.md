# RBaseX
BaseX R Client

## Revision history

Version 1.1.2: 2022-11-04
* Changes in the client protocol made it necessary to add put.R and putBinary.R as replacement for Store.R and Replace.R. In future versions, these programs will be removed.
Version 1.1.1: 2022-03-10
* Bug-fixes
* Added tests
* Renamed 'Execute' command to 'Command' for beter alignment to the server protocol.

Version 1.0.0: 2021-12-10
* Open the socketConnection as non-blocking. Execution time for the tests has reduced from 116 seconds to 2 seconds.

Version 0.9.2: 2021-12-05
* Complete rewrite
* For each command that can be send to the server, the code is clearly separated in a part for pre-handling the inout data and a part for handling the response
* Nearly all responses are returned as a named list

Version 0.3.0 : 2020-04-20

* All methods that read from or write to the socket, have been moved to SocketClass.R.
* The 'Bind' command accepts single values and sequences. Types can be added.
* Results from querys can be transformed to dataframes or tibbles. 

## Implementation

RBaseX is a full implementation of the BaseX server protocol. RBaseX may be used to create, update and query databases.

RBaseX has been developed using the R6-classes. You can call the public methods directly or use the wrapper methods.

## Using RBaseX

This is a development-version of RBaseX. To install, download the latest tar.gz to your filesystem. You can install this tar with "install.packages(file.choose(), repos=NULL)"

The latest official release can be found at https://cran.r-project.org/package=RBaseX
