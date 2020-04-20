# RBaseX
BaseX R Client

## Revision history

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
