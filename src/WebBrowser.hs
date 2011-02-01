module WebBrowser where

pageNotAvailable error link 
    = "<html> <head> </head> <body> <h1>This webpage is not available.</h1> <p> Error returned: " ++ error ++ " </p> <p> The webpage at " ++ link ++ " might be temporarily down or it may have moved permanently to a new web address. </p> </body> </html>"
