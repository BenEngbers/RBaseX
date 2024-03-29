% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RBaseX.R, R/RbaseXClient.R
\docType{package}
\name{RBaseX}
\alias{RBaseX}
\alias{BasexClient}
\title{RBaseX}
\description{
'BaseX' is a robust, high-performance XML database engine and a highly compliant XQuery 3.1 processor
    with full support of the W3C Update and Full Text extensions.

The client can be used in 'standard' mode and in 'query' mode.
    Standard Mode is used for connecting to a server and sending commands.
}
\details{
'RBaseX' was developed using R6. For most of the public methods in the R6-classes, wrapper-functions
    are created. The differences in performance between R6-methods and wrapper-functions are minimal and
    slightly in advantage of the R6-version.

    It is easy to use the R6-calls instead of the wrapper-functions.
    The only important difference is that in order to execute a query, you have to call ExecuteQuery()
    on a queryObject.
}
\examples{
\dontrun{
    Session <- BasexClient$new("localhost", 1984L, username = "<username>", password = "<password>")
    Session$Execute("Check test")
    Session$Execute("delete /")
    # Add resource
    Session$Add("test.xml", "<root/>")

    # Bindings -----
    query_txt <- "declare variable $name external; for $i in 1 to 3 return element { $name } { $i }"
    query_obj <- Session$Query(query_txt)
    query_obj$queryObject$Bind("$name", "number")
    print(query_obj$queryObject$ExecuteQuery())
}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{BasexClient$new()}}
\item \href{#method-Command}{\code{BasexClient$Command()}}
\item \href{#method-Execute}{\code{BasexClient$Execute()}}
\item \href{#method-Query}{\code{BasexClient$Query()}}
\item \href{#method-Create}{\code{BasexClient$Create()}}
\item \href{#method-Add}{\code{BasexClient$Add()}}
\item \href{#method-Replace}{\code{BasexClient$Replace()}}
\item \href{#method-Store}{\code{BasexClient$Store()}}
\item \href{#method-set_intercept}{\code{BasexClient$set_intercept()}}
\item \href{#method-restore_intercept}{\code{BasexClient$restore_intercept()}}
\item \href{#method-get_intercept}{\code{BasexClient$get_intercept()}}
\item \href{#method-get_socket}{\code{BasexClient$get_socket()}}
\item \href{#method-set_success}{\code{BasexClient$set_success()}}
\item \href{#method-get_success}{\code{BasexClient$get_success()}}
\item \href{#method-clone}{\code{BasexClient$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new client-session
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$new(host, port = 1984L, username, password)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{host, port, username, password}}{Host-information and user-credentials}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Command"></a>}}
\if{latex}{\out{\hypertarget{method-Command}{}}}
\subsection{Method \code{Command()}}{
Execute a command
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$Command(command)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{command}}{Command}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
For a list of database commands see \url{https://docs.basex.org/wiki/Commands}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Execute"></a>}}
\if{latex}{\out{\hypertarget{method-Execute}{}}}
\subsection{Method \code{Execute()}}{
Execute a command
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$Execute(command)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{command}}{Command}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
For a list of database commands see \url{https://docs.basex.org/wiki/Commands}.
   This function is replaced by 'Command' and is obsolete.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Query"></a>}}
\if{latex}{\out{\hypertarget{method-Query}{}}}
\subsection{Method \code{Query()}}{
Create a new query-object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$Query(query_string)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{query_string}}{Query-string}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
A query-object has two fields. 'queryObject' is an ID for the new created 'QueryClass'-instance.
    'success' holds the status from the last executed operation on the queryObject.
}

\subsection{Returns}{
ID for the created query-object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Create"></a>}}
\if{latex}{\out{\hypertarget{method-Create}{}}}
\subsection{Method \code{Create()}}{
Create a new database
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$Create(name, input)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{Name}

\item{\code{input}}{Initial content, Optional}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Initial content can be offered as string, URL or file.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Add"></a>}}
\if{latex}{\out{\hypertarget{method-Add}{}}}
\subsection{Method \code{Add()}}{
Add a new resouce at the specified path
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$Add(path, input)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{Path}

\item{\code{input}}{File, directory or XML-string}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Replace"></a>}}
\if{latex}{\out{\hypertarget{method-Replace}{}}}
\subsection{Method \code{Replace()}}{
Replace resource, adressed by path
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$Replace(path, input)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{Path}

\item{\code{input}}{File, directory or XML-string}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Store"></a>}}
\if{latex}{\out{\hypertarget{method-Store}{}}}
\subsection{Method \code{Store()}}{
Store binary content
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$Store(path, input)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{Path}

\item{\code{input}}{File, directory or XML-string}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Binary content can be retrieved by executing a retrieve-command
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_intercept"></a>}}
\if{latex}{\out{\hypertarget{method-set_intercept}{}}}
\subsection{Method \code{set_intercept()}}{
Toggles between using the ´success'-field, returned by the
    Execute-command or using regular error-handling (try-catch).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$set_intercept(Intercept)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{Intercept}}{Boolean}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-restore_intercept"></a>}}
\if{latex}{\out{\hypertarget{method-restore_intercept}{}}}
\subsection{Method \code{restore_intercept()}}{
Restore the Intercept Toggles to the original value
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$restore_intercept()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_intercept"></a>}}
\if{latex}{\out{\hypertarget{method-get_intercept}{}}}
\subsection{Method \code{get_intercept()}}{
Get current Intercept
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$get_intercept()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_socket"></a>}}
\if{latex}{\out{\hypertarget{method-get_socket}{}}}
\subsection{Method \code{get_socket()}}{
Get the socket-ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$get_socket()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Socket-ID,
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_success"></a>}}
\if{latex}{\out{\hypertarget{method-set_success}{}}}
\subsection{Method \code{set_success()}}{
Set the status success-from the last operation on the socket
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$set_success(Success)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{Success}}{Boolean}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
This function is intended to be used by instances from the QueryClass
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_success"></a>}}
\if{latex}{\out{\hypertarget{method-get_success}{}}}
\subsection{Method \code{get_success()}}{
Get the status success-from the last operation on the socket
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$get_success()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Boolean,
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BasexClient$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
