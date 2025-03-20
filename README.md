# esutils

Various R tools, for instance for finding git repositories
and doing backups.  Most functions in esutils are at
some point moved to more-appropriate specialized packages.

[ [More] ](https://enricoschumann.net/R/packages/esutils/)

## Installing the package

The latest released version is available from
https://enricoschumann.net. In an R session, just type:

    install.packages('esutils',
                     repos = c('https://enricoschumann.net/R', getOption('repos')))


## System dependencies

Function 'ss2csv' uses 'ssconvert', which is part of Gnumeric.

Function 'pdf2txt' uses 'pdftotext', which is part of Xpdf.
