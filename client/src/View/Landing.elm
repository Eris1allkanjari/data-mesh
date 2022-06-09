module View.Landing exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Markdown
import Types exposing (..)


view : StaticImages -> Html Msg
view images =
    div [ class "landing-pane" ]
        [ Markdown.toHtml [] landingIntro
        , img
            [ class "landing-diagram"
            , src images.landingImage1Path
            ]
            []
        ]


landingIntro : String
landingIntro =
    """
### Data Mesh


Ky aplikacion tregon nje Data Mesh UI, e cila mund te perdoret si nga prodhuesit ose konsumuesit e produktit te te dhenave.

Aplikacioni eshte i organizuar ne hapa te cilat mund te ndiqen per te konsumuar dhe krijuar produkte te dhenash:

* Tab 1 Eshte per konsumatoret e produktit te te dhenave.Aty mund te eksplorohen produktet e ndryshme 
duke perfshire dhe metadata pershkruese per to

* Tab 2 Eshte per zhvilluesit e aplikacioneve , te cilet mund te perdorin 
produkte te dhenash te disponueshme per te ndertuar aplikacione te reja
* Tab 3 Eshte per pronaret e produktit te te dhenave, te cilet mund te menaxhojne produktet e te dhenave dhe metadatat e tyre
"""
