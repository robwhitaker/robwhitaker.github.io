module Style.Theme where

import Clay

data Theme = Theme
    { bodyBgColor :: Color
    , containerBgColor :: Color
    , linkColor :: Color
    , highlightColor :: Color
    , mainTextColor :: Color
    , dividerColor :: Color
    , codeTheme :: CodeTheme
    }

data CodeTheme = CodeTheme
    { bgColor   :: Color
    , kwColor   :: Color
    , cfColor   :: Color
    , dtColor   :: Color
    , fuColor   :: Color
    , stColor   :: Color
    , dvColor   :: Color
    , otColor   :: Color
    , atColor   :: Color
    , coColor   :: Color
    , vaColor   :: Color
    , textColor :: Color        
    }

lightTheme :: Theme
lightTheme = Theme
    { bodyBgColor      = white
    , containerBgColor = white
    , linkColor        = darkred
    , highlightColor   = cornflowerblue
    , mainTextColor    = black
    , dividerColor     = rgb 175 175 175
    , codeTheme = CodeTheme
        { bgColor   = rgb 50 50 50
        , kwColor   = deeppink
        , cfColor   = deeppink
        , dtColor   = skyblue
        , fuColor   = cornflowerblue
        , stColor   = khaki
        , dvColor   = fuchsia
        , otColor   = springgreen
        , atColor   = springgreen
        , coColor   = gray
        , vaColor   = springgreen
        , textColor = white
        }
    }