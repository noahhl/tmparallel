This fork provides some additional multicore support by favoring <code>plyr::llply</code> over <code>base::lapply</code> for <code>tm_map.VCorpus</code>. Passing <code>tm_map</code> any of plyr's options for <code>llply</code> is supported.

Benchmark for <code>removeWords</code> on a 10,000 document corpus with and without parallelization (2011 iMac 2.8 GHz Intel Core i5, 8 GB RAM, OS X 10.6.7, R version 2.13.0 Patched (2011-04-23 r55622), with 4 parallel workers). (Not necessarily representative of anything.)

Non-parallel:

<pre>
system.time(tm::tm_map(inboundCorpus[1:10000], removeWords, myStopWords))
elapsed = 100.681  
</pre>

With <code>llply</code>:

<pre>
library(doMC)
registerDoMC(cores=4)
system.time(tmparallel::tm_map(inboundCorpus[1:10000], removeWords, myStopWords, .parallel=T, .progress='text'))  
elapsed = 53.809  
</pre>

With equivalent size <code>snow</code> MPI cluster:

<pre>
library(snow)
makeCluster(4, type="MPI")  
system.time(tm::tm_map(inboundCorpus[1:10000], removeWords, myStopWords))
elapsed = 117.673
</pre>

<hr/>
R-Forge SVN README


(See "http://download.r-forge.r-project.org/manuals/R-Forge_Manual.pdf"
       for detailed information on registering a new project.

1. Introduction
-----------------------------------------------------------------------
R is free software distributed under a GNU-style copyleft. R-Forge is
a central platform for the development of R packages, R-related 
software and further projects. Among many other web-based features it 
provides facilities for collaborative source code management via 
Subversion (SVN).

2. The directory you're in
-----------------------------------------------------------------------
This is the repository of your project. It contains two important
pre-defined directories namely 'www' and 'pkg'. They must not be
deleted otherwise R-Forge's core functionality will not be available
(daily checking and building of your package or the project websites).
These two directories are standardized and therefore are going to be
described in this README. The rest of your repository can be used as
you like.

3. 'pkg' directory
-----------------------------------------------------------------------
To make use of the package building and checking feature the package 
source code has to be put into the 'pkg' directory of your repository 
(i.e., 'pkg/DESCRIPTION', 'pkg/R', 'pkg/man', etc.) or, alternatively,
a subdirectory of 'pkg'. The latter structure allows to have more than
one package in a single project, e.g., if a project consists of the
packages foo and bar then the source code is located in 'pkg/foo' and
'pkg/bar', respectively.

R-Forge automatically examines the 'pkg' directory of every repository 
and builds the package sources as well as the package binaries on a
daily basis for Mac OSX and Windows (if applicable). The package builds
are provided in the 'R Packages' tab for download or can be installed
directly in R from a CRAN-style repository using 
'install.packages("foo", repos="http://R-Forge.R-project.org")'. 
Furthermore, in the 'R Packages' tab developers can examine logs of the 
build and check process on different platforms.

4. 'www' directory
-----------------------------------------------------------------------
Developers may present their work on a subdomain of R-Forge, e.g.,
'http://foo.R-Forge.R-project.org', or via a link to an external
website.

This directory contains the project homepage which gets updated hourly
on R-Forge, so please take into consideration that it will not be 
available right after you commit your changes or updates. 

5. Help
-----------------------------------------------------------------------
If you need help don't hesitate to contact us
(R-Forge@R-project.org)
