
# Simple San Simon Functional Web Browser

It is a computer desktop program for rendering web pages.
One nice thing is that it is written with Haskell, a purely funcional programming language.
It supports a generic markup language (like ~ html or xhtml or xml) and part of CSS.

## To install
```
brew install gd
stack build
stack exec 3SFWebBrowser
```

We have a configuration directory ('config' directory) that should be placed
at your home directory: '~/.3SFWebBrowser/'.  Cabal creates it automatically.

You will need to edit '~/.3SFWebBrowser/dbf' and setup the proper path for
'User_Agent_Stylesheet' which is located at the same directory.


For more information
--------------------

Web: http://hsbrowser.wordpress.com

Carlos Gomez
carliros.g@gmail.com

