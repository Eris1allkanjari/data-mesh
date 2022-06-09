module View.Create exposing (view)

import GenericDict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (routeToString)
import Types exposing (..)
import UIKit
import View.Common exposing (errorView, loadingWheel, selectable, webDataView)
import View.Tooltips exposing (tooltip)


view : Maybe UseCaseName -> Model -> Html Msg
view activeUseCaseKey model =
    webDataView
        (\useCases ->
            let
                activeUseCase =
                    Maybe.andThen (\k -> Dict.get unUseCaseName k useCases) activeUseCaseKey
            in
            div [ class "create-pane" ]
                [ header []
                    [ Markdown.toHtml [] dataProductCreationIntro ]
                , div [ class "create-use-cases" ]
                    [ useCasesView activeUseCaseKey
                        useCases
                    ]
                , div [ class "create-use-detail" ]
                    [ useCasesDetail
                        activeUseCase
                        model.executeUseCaseResult
                    ]
                , footer []
                    [ Markdown.toHtml [] dataProductCreationOutro ]
                ]
        )
        model.useCases


dataProductCreationIntro : String
dataProductCreationIntro =
    """
### Krijo nje aplikacion duke perdorur produktet e te dhenave

*Roli: Zhvillues aplikacioni*

Pas identifikimit te produkteve te dhenave te nevojshem ,mund te ndertojme nje aplikacion i cili do konsumoj evente nga secili produkt te dhenash i regjistruar,performoj logjiken e nevojshme te biznesit, dhe nese eshte e nevojshme te krijoje evente te reja.

Ky tab permban disa use-case per biznesin qe demostrojne konsumimin dhe perdorimin e produkteve te dhenave te pa publikuar.
"""

dataProductCreationOutro : String
dataProductCreationOutro =
    """
Shenime:
- ksqlDB pedoret per demostrim , por mund te perdoret cdo gjuhe e afte per te konsumuar produkte te dhenash
- Cdo event i krijuar nga aplikacioni do qendroje brenda domain te aplikacionit
    """


useCasesView : Maybe UseCaseName -> Dict UseCaseName UseCase -> Html Msg
useCasesView activeUseCaseKey useCases =
    div []
        [ h4 []
            [ text "Shembuj te use-cases te biznesit"
            , tooltip "Keto use-cases tregojne konsumimin e produkteve te dhenave dhe event streams"
            ]
        , table
            [ UIKit.table
            , UIKit.tableDivider
            , UIKit.tableStriped
            , UIKit.tableSmall
            ]
            [ tbody []
                (useCases
                    |> Dict.values
                    |> List.indexedMap
                        (\index useCase ->
                            tr
                                ((if activeUseCaseKey == Just useCase.name then
                                    [ UIKit.active, onClick (ChangeView (Create Nothing)) ]

                                  else
                                    [ onClick (ChangeView (Create (Just useCase.name))) ]
                                 )
                                    ++ [ selectable ]
                                )
                                [ td [ UIKit.button, UIKit.buttonLink ]
                                    [ text (String.fromInt (index + 1) ++ ": " ++ useCase.title) ]
                                ]
                        )
                )
            ]
        ]


useCasesDetail : Maybe UseCase -> WebData UseCaseName -> Html Msg
useCasesDetail mUseCase executeUseCaseResult =
    div []
        [ h4 []
            [ text "Informacion per aplikacionin"
            , tooltip "ksqlDB pedoret per demostrim , por mund te perdoret cdo gjuhe e afte per te konsumuar produkte te dhenash"
            ]
        , case mUseCase of
            Nothing ->
                i [] [ text "Zgjidh nje use-case nga tabela ne te majte." ]

            Just useCase ->
                table
                    [ UIKit.table
                    , UIKit.tableDivider
                    , class "table-horizontal"
                    ]
                    [ tbody []
                        (List.map
                            (\( title, content ) ->
                                tr []
                                    [ th [] [ text title ]
                                    , td [] [ content ]
                                    ]
                            )
                            [ ( "Titulli", text useCase.title )
                            , ( "Pershkrimi", text useCase.description )
                            , ( "Emri", text (unUseCaseName useCase.name) )
                            , ( "Inputet", text useCase.inputs )
                            , ( "Output"
                              , text useCase.outputTopic
                              )
                            , ( "Query ksqlDB"
                              , pre [ style "max-height" "300px" ]
                                    [ code [] [ text useCase.ksqlDbCommand ] ]
                              )
                            ]
                        )
                    ]
        , case ( mUseCase, executeUseCaseResult ) of
            ( Nothing, _ ) ->
                span [] []

            ( Just _, Loading ) ->
                loadingWheel

            ( Just _, Failure err ) ->
                errorView err

            ( Just _, Success _ ) ->
                div []
                    [ text "Stream u krijua."
                    , text " "
                    , a [ href (routeToString Manage) ] [ text "Shko ketu." ]
                    , text " per te pare dhe publikuar produktin e te dhenave."
                    ]

            ( Just useCase, _ ) ->
                button
                    [ UIKit.button
                    , UIKit.buttonPrimary
                    , UIKit.marginBottom
                    , onClick (ExecuteUseCase useCase.name)
                    ]
                    [ text "Ekzekuto query e ksqlDB"
                    ]
        ]
